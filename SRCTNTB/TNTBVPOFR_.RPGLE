000100060502     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200060427      *------------------------------------------------------------------------*
000300131015      * Gestione tabella "VPO" = ABILITAZIONI FILIALI                          *
000400060427      *------------------------------------------------------------------------*
000500060505     ftntbe01l  uf a e           k disk
000600130926     ftabel00f  if   e           k disk
000700130926     fazorg01l  if   e           k disk
000800130924     ftntbvpofd cf   e             workstn sfile(tbfls02:s02nrr)
000900060428
001000060428      *------------------------------------------------------------------------*
001100060428      * riepilogo indicatori
001200060428      *------------------------------------------------------------------------*
001300120621      * 02 - visualizzazione
001400060508      * 03 - proteggo campi video 01
001500120621      * 04 - manutenzione
001600120704      * 06 - dicitura "Annullamento" a video
001700060508      * 20 - gestione subfile 01
001800060508      * 21 - gestione subfile 01
001900060508      * 22 - gestione subfile 01
002000060508      * 23 - gestione subfile 01
002100120704      * 24 - gestione subfile 02
002200120704      * 25 - gestione subfile 02
002300120704      * 26 - gestione subfile 02
002400120704      * 27 - gestione subfile 02
002500060428      * 28 - messaggio errore
002600120704      * 40/42Errori campi subfile/control
002700060502
002800060502      *------------------------------------------------------------------------*
002900060502      * schiere
003000060502      *------------------------------------------------------------------------*
003100131015     d msg             s             78    dim(17) ctdata perrcd(1)
003200130924     d fil_ab          s              3    dim(85)
003300060427
003400060428      *------------------------------------------------------------------------*
003500060428      * campi di work
003600060428      *------------------------------------------------------------------------*
003700090428     d data_eur        s               d   datfmt(*eur)
003800060428     d blanks          s                   like(d1descopz)
003900090427     d comand          s              1
004000060428     d len             s              5u 0
004100120704     d s02nrr          s              4  0
004200130926     d savrcdnbr       s                   like(c2rcdnbr)
004300120704     d sv2rcdnbr       s                   like(c2rcdnbr)
004400120620     d kfil            s                   like(orgfil)
004500060505     d wrkcarw01       s              1
004600130926     d WATB            s              1
004700060504     d $video          s             10
004800120620     d i               s              3  0 inz
004900120620     d w0030           s              3  0 inz
005000120706     d ix              s              3  0 inz
005100130926     d YY              s              3  0 inz
005200130926     d XX              s              3  0 inz
005201131016     d winkq           s              1
005202131015
005203131015     d $Fine           s               n   inz(*off)
005300120705
005400120705       // - Parametri per TNSD19R
005500120705     d SD19cod         s              3    inz
005600120705     d SD19tip         s              1    inz
005700120705     d SD19des         s             25    inz
005800060428
005900060428      *------------------------------------------------------------------------*
006000060428      * ds interne/esterne
006100060428      *------------------------------------------------------------------------*
006200120620     d campolp         ds           255
006300120620     D  Slp                    1    255    dim(85)
006400120620     d
006401131015     d TIBS02ds      e ds                  inz
006500060502     d kpjba         e ds
006600060503     d tibs34ds      e ds                  inz
006700060428     d §azute        e ds                  extname(azute00f)
006800060428     d                                     dtaara
006900060428     d §datiute      e ds                  extname(ddatiute)
007000060428     d                                     dtaara
007100120706     d trul06ds      e ds
007200120706     D  LIN                    1     90    dim(30)
007300010503
007400060428     d psds           sds
007500060428     d  pgmname          *proc
007600060428
007700090424      // ? PROTOTIPI ?
007800090424      /copy gaitrasrc/srcprotopr,tibs34r
007900120706      /copy gaitrasrc/srcprotopr,trul06r
007901131015      /copy gaitrasrc/srcProtoPR,TIBS02R
008000130926
008100120705       // - Ricerca codici Organigramma
008200120705     d tnsd19r         pr                  extpgm('TNSD19R')
008300120705     d  D19cod                             like(SD19cod)
008400120705     d  D19tip                             like(SD19tip)
008500120705     d  D19des                             like(SD19des)
008600060428
008700060428      *------------------------------------------------------------------------*
008800060428      * costanti
008900060428      *------------------------------------------------------------------------*
009000060428     d errore          c                   '1'
009100060428     d eseguito        c                   '0'
009200010503
009300060428      *------------------------------------------------------------------------*
009400120621
009500120704      /free
009501131015
009502131015       $video='D1';
009503131015
009504131015       DOW $Fine = *off;
009505131015         SELECT;
009506131015           WHEN $Video = 'D1';
009507131015              exsr sr_d01;
009508131015           WHEN $Video = 'C2';
009600131015              exsr sr_c02;
009701131015           OTHER;
009702131015             $Fine = *on;
009703131015         endsl;
009704131015       enddo;
009705131015
009706131015       exsr sr_uscita;
009707131015       // ------------------------------------------------------------------------
009708131015       // gestione video richiesta codice tabella abilitazione filiali
009709131015       // ------------------------------------------------------------------------
009710131015       begsr sr_d01;
009711131015
009712131015       d01ke1=*blanks;
009713131015       dow $video='D1';
009714131015          exfmt tbfld01;
009715131015          *in28=*off;
009716131015          clear d01msg;
009717131015          select;
009718131015          when *inkc=*on;
009719131015             $video=*blanks;
009720131015          other;
009721131015             exsr sr_cntd01;
009722131015             if not *in90;
009723131015                $video='C2';
009724131015             endif;
009725131015          endsl;
009726131015       enddo;
009727131015       endsr;
009728131015       // ------------------------------------------------------------------------
009729131015       // Controllo video richiesta codice tabella abilitazione filiali
009730131015       // ------------------------------------------------------------------------
009731131015       begsr sr_cntd01;
009732131015          *in90=*off;
009733131015 1        if %scan('?':d01ke1)>0;
009734131015             clear d01ke1;
009735131015             clear tibs02ds;
009736131015             t02mod='R';
009737131015             t02sif=knsif;
009738131015             t02cod='VPO';
009739131015             t02ke1='DECOFI';
009740131015             TNTBE_RicercaControllo (kpjba:tibs02ds);
009741131015             if t02err=*blanks and %subst(t02ke1:1:6)='DECOFI';
009742131017                d01ke1=%subst(t02ke1:7:9);
009743131015             endif;
009744131015             *in90=*on;
009745131015             *in48=*on;
009746131015             leavesr;
009747131015          endif;
009748131015          if d01ke1=*blanks;
009749131015             *in90=*on;
009750131015             *in28=*on;
009751131015             *in48=*on;
009752131015             d01msg  = Msg(9);
009753131015             leavesr;
009754131015          endif;
009755131015       endsr;
009800130926
009801131015       // ------------------------------------------------------------------------
009900060504       // gestione video dati di trasmissione
010000060504       // ------------------------------------------------------------------------
010100060504       begsr sr_w01;
010200060504
010300060504       // imposto i dati a video
010400060504        exsr sr_carw01;
010500060504
010600060505       // fino a che la variabile resta settata come 'W01'
010700060504        dou $video <> 'W01';
010800060504
010900060504       // reset indicatore generico di errore
011000060504         *in28 = *off;
011100060504
011200060504       // emetto il video
011300120618          exfmt tbflw01;
011400060504
011500060504       // controllo tasti funzionali del video
011600060504         select;
011700060504
011800060504       // F6=Conferma
011900060504          when *inkf;
012000060504       // controllo i dati del video
012100060504           exsr sr_ctrw01;
012200130926
012300060504       // se non riscontrati errori aggiorno il record corrente
012400060504           if not *in28;
012500060504            exsr sr_aggiorna;
012600130926
012700131015              $video = 'D1 ';
012800131015       //     exsr sr_cars02;
012900060504           endif;
013000060504
013100060504       // F12=Ritorno
013200060504          when *inkl;
013300120704           $video = 'C02';
013400060504
013500060504       // Invio
013600060504          other;
013700060504           exsr sr_ctrw01;
013800060504
013900060504         endsl;
014000060504
014100060504       // fine gestione 'W01'
014200060504        enddo;
014300060504
014400060504       endsr;
014500060504
014600060504       // ------------------------------------------------------------------------
014700060504       // imposto i dati di trasmissione
014800060504       // ------------------------------------------------------------------------
014900060504       begsr sr_carw01;
015000060504
015100060504       // se pulisco i campi
015200060504         clear w1ftt;
015300060504         clear w1flt;
015400060504         clear w1ftr;
015500060504         clear w1dtr;
015600060504
015700060504       // se non immissione imposto i campi del file
015800090424        if comand <> 'J';
015900060504         w1ftt = tbeftt;
016000060504         w1flt = tbeflt;
016100060504         w1ftr = tbeftr;
016200060504       // imposto la data
016300060504         if tbedtr <> *zeros;
016400090428          data_eur=%date(tbedtr:*iso);
016500090428          w1dtr=%dec(data_eur);
016600060504         endif;
016700060504
016800060504        endif;
016900060504
017000060504       endsr;
017100060504
017200060504       // ------------------------------------------------------------------------
017300060504       // controllo i dati di trasmissione
017400060504       // ------------------------------------------------------------------------
017500060504       begsr sr_ctrw01;
017600060504
017700060504       endsr;
017800060502
017900060502       // ------------------------------------------------------------------------
018000060502       // aggiorno tabella
018100060502       // ------------------------------------------------------------------------
018200060502       begsr sr_aggiorna;
018300060502
018400130926         tbecod = 'VPO';
018500131015         tbeke1 = c2cke1             ;
018600130926         chain (tbecod:tbeke1) tntbe01l;
018700130926
0188001309261        if  not %found(tntbe01l);
018900130926          clear tntbe000  ;
0190001309261        endif  ;
019100130926
019200060502       // imposto campi del file
019300060502
019400130926        tbeatb = watb  ;
019500130926        tbeftt = w1ftt  ;
019600060504        tbeflt = w1flt;
019700060502        clear tbeftr;
019800060502        clear tbedtr;
019801131016        if winkq='0';
019900131016           tbeuni=campolp;
019901131016        endif;
020000060502
0201001309261        if  not %found(tntbe01l);
020101131015          tbecod = 'VPO';
020200131015          tbeke1 = c2cke1             ;
020300090427          clear tbelin;
020400090428          clear tbeatb;
020500060508          write tntbe000;
020600130926          else    ;
020700060502
020800060502          update tntbe000;
0209001309261         endif  ;
021000010430
021100060502       endsr;
021200120704       // ------------------------------------------------------------------------
021300130926       // gestione subfile
021400120704       // ------------------------------------------------------------------------
021500130926       BEGSR sr_c02;
021600130926
021700120704        $video = 'C02';
021800130926
021900120704       // caricamento subfile
022000130926        exsr sr_cars02;
022100130926
022200120704       // inizio elaborazione subfile
0223001309261        dou  $video <> 'C02';
022400130926
022500120704       // c2csrrrn contiene il numero di riga del subfile su cui era posizionato il cursore
022600120704       // impostando c2rcdnbr visualizzo la pagina che vedeva l'utente quando ha premuto
022700120704       // l'ultimo tasto
0228001309262         if c2csrrrn > 0;
022900120704           c2rcdnbr = c2csrrrn;
023000120704          else;
023100120704           c2rcdnbr = sv2rcdnbr;
0232001309262         endif;
023300120704       // se non so quale pagina visualizzare forzo pagina 1
0234001309262         if c2rcdnbr < 1;
023500120704           eval c2rcdnbr = 1;
0236001309262         endif;
023700120704
023800120704       // salvo il record number del subfile
023900120704          sv2rcdnbr = c2Rcdnbr;
024000120704
024100120704       // Emissione del subfile
024200120704          write tbflp02;
024300120704          exfmt tbflc02;
024400130926
024500130926          clear  Watb  ;
024501131016          Winkq = '0'  ;
024600120704
024700120704       // controllo tasti funzionali del subfile
0248001309262         select;
024900120704       // F3=Fine
0250001309262          when *inkc;
025100120704            $video = *blanks;
025200120704            exsr sr_uscita;
025201131015       // F12=Ritorno
0252021310152          when *inkl;
025203131015            $video = 'D1';
025300130926
0254001309262      // F6=Conferma
025500120704           when *inkf;
025600120704            // controllo e decodifico i dati del video
025700120704            exsr sr_ctrs02;
025800130926
025900130926       // se non riscontrati errori emetto la finestra con i dati per la trasmissione
026000130926 3          if not *in28;
026100130926             wrkcarw01 = *on;
026200130926             $video = 'W01';
026300130926             exsr sr_w01;
026400130926 3          endif;
026500130926
026600130926       // F16=Annullamento tabella
026700130926 2         when *inkq  ;
026701131016           winkq='1';
026800130926           if tbeatb=' '  ;
026900130926           watb='A'  ;
027000130926           else  ;
027100130926           watb=' '  ;
027200130926           endif  ;
027300130926
027400120704
027500120704       // se non riscontrati errori emetto la finestra con i dati per la trasmissione
027600130926 3          if not *in28;
027700120704             wrkcarw01 = *on;
027800120704             $video = 'W01';
027900120704             exsr sr_w01;
028000130926 3          endif;
028100130926
028200120705       // F14=Inserimento per terminal di partenza
028300130926 2         when *inkn;
028400120706              exsr sr_ctrs02;
028500130926 3            if not *in28;
028600120706                 exsr sr_w02;
028700130926 3               *inkl=*off;
028800120706              endif;
028900120704
029000120704       // Invio
029100130926 x2        other;
029200120704            if not *in03;
029300120704             exsr sr_ctrs02;
029400120704            endif;
029500130926 2        endsl;
029600130926
029700120704       // fine elaborazione 'C02'
029800130926 1       enddo;
029900120704       endsr;
030000120704
030100120704       // ------------------------------------------------------------------------
030200120704       // caricamento subfile2 - dettaglio per lna/lna forzata
030300120704       // ------------------------------------------------------------------------
030400130926       begsr sr_cars02;
030500120704
030600130924       // Inizializzo il subfile
030700120704         s02nrr = 0;
030800120704         *in24 = *on;
030900120704         write tbflc02;
031000120704         *in24 = *off;
031100130926         *in25 = *on ;
031200120704         *in26 = *off;
031300120704         *in27 = *off;
031400120705         clear campolp;
031500130924
031600130924       // chain      file
031700130924         tbecod = 'VPO';
031800131015         tbeke1 = 'DECOFI' + d01ke1  ;
031801131015       // campo a video
031802131015         c2cke1 = tbeke1;
031900130924         chain (tbecod:tbeke1) tntbe01l;
032000130926
0321001309261        if  %found(tntbe01l);
032200130924
0323001309262        if tbeatb<>' '  ;
032400130924         *in06 =*on  ;
032500130924         else  ;
032600130924         *in06 = *off ;
0327001309262        endif  ;
032800130924
032900130924        // Riempo la schiera delle filiali e poi la emetto a video
033000130924        xx= 1  ;
033100130924        yy= 1  ;
033200130924
033300130926 2      dow  xx < 256    ;
033400130924
033500130924        fil_ab(yy) = %subst(tbeuni:xx:3)  ;
033600130926
033700130924        clear s02dlp      ;
033800130924        s02lp=fil_ab(yy)  ;
033900130926 3      if s02lp='999'   ;
034000130924        s02dlp='Tutte'   ;
034100130926 x3     else  ;
034200130924
034300130926  4     if s02lp>*zeros  ;
034400130924        w0030 =%int(s02lp)    ;
034500130924        chain w0030  azorg01l   ;
034600130926  5      if %found(azorg01l)  ;
034700130926         s02dlp=orgde5  ;
034800130926  6       if s02dlp=*blanks  ;
034900130924           s02dlp=orgdes  ;
035000130926  6       endif  ;
035100130926  5      endif  ;
035200130926  4     endif  ;
035300130926  3     endif  ;
035400130924
035500130924        s02nrr=s02nrr+  1 ;
035600130924        write tbfls02  ;
035700130924
035800130924        xx= xx+ 3  ;
035900130926        yy= yy+ 1  ;
036000130924
036100130926  2     enddo;
036200130926  x1    else  ;
036300130926
036400130926        dow s02nrr  <  85   ;
036500130926        clear s02lp  ;
036600130926        clear s02dlp  ;
036700130926        s02nrr=s02nrr+  1 ;
036800130926        write tbfls02  ;
036900130926        enddo  ;
037000130926
037100130926  1     endif   ;
037200120704
037300120704       endsr;
037400120704       // ------------------------------------------------------------------------
037500120704       // controllo subfile 2 (dettaglio per linea di partenza)
037600120704       // ------------------------------------------------------------------------
037700120704       begsr sr_ctrs02;
037800120704
037900120704        *in28 = *off;
038000120704        *in42 = *off;
038100120704        clear vc2msg;
038200120704
038300120704        clear slp;
038400120706        *in26=*on;
038500130926        s02nrr=1  ;
038600130926
038700130926 1      chain s02nrr  tbfls02;
038800130926            dow %found   ;
038900120705              clear s02dlp;
039000130926 2            if s02lp<>*zeros and s02lp<>*blanks and s02lp<>'999';
039100120704      /end-free
039200120704     C                   testn                   s02lp                30
039300130926    3c                   if             *in30  = *off
039400120704     c                             or  %subst(s02lp: 3: 1) < *zero
039500120704     c                   eval      vc2msg = msg(02)
039600120704     c                   eval      *in28 = *on
039700120704     c                   eval      *in42 = *on
039800120704     c                   update    tbfls02
039900120705     c                   eval      c2csrrrn=s02nrr
040000120704     c                   leavesr
040100130926    3c                   endif
040200120704      /free
040300120704              w0030=%dec(s02lp:3:0);
040400120704              chain w0030 azorg01l;
040500130926 3            if not %found (azorg01l) or (orgfag <>'A' and orgfag<>'F');
040600120704                 vc2msg = msg(02);
040700120704                 *in28 = *on;
040800120704                 *in42 = *on;
040900120704                 update tbfls02;
041000120705                 c2csrrrn=s02nrr;
041100120704                 leavesr;
041200130926 3            endif;
041300120704                // decodifico la linea di partenza
041400120704                s02dlp=orgde5;
041500130926 3              if s02dlp=*blanks;
041600120704                   s02dlp=orgdes;
041700130926 3              endif;
041800120704
041900130926              else  ;
042000130926 3            if s02lp='999'  ;
042100130926                   s02dlp='Tutte'   ;
042200130926 3            endif;
042300130926 2            endif;
042400130926
042500130926 2            if s02lp=*zeros;
042600120704                 clear s02lp;
042700120704                 clear s02dlp;
042800130926 2            endif;
042900130926 2            if s02lp<>*blanks;
043000120705                 // errore se linea di partenza ripetuta all'interno del sfl
043100130926 3               if %lookup(s02lp:slp)>0;
043200120705                    vc2msg = msg(03);
043300120705                    *in28 = *on;
043400120705                    *in42 = *on;
043500120705                    update tbfls02;
043600120705                    c2csrrrn=s02nrr;
043700120705                    leavesr;
043800130926 x3              else;
043900120705                 // se linea di partenza non ripetuta memorizzo in schiera
044000120705                    i=%lookup('   ':slp);
044100130926 4                  if i>0;
044200120705                       slp(i)=s02lp;
044300130926 4                  endif;
044400130926 3               endif;
044500130926 2            endif;
044600130926
044700130926              update tbfls02;
044800130926              s02nrr=s02nrr+1  ;
044900130926              chain s02nrr  tbfls02;
045000130926 1          enddo;
045100120704       endsr;
045200060505
045300120705       // ------------------------------------------------------------------------
045400130926       // finestra di richiesta terminal di partenza/ codice area
045500120705       // ------------------------------------------------------------------------
045600130926       BEGSR sr_w02;
045700130926
045800120705          clear w02tfp;
045900120705          clear w02tfpd;
046000130926          clear w02are;
046100130926          clear w02ared;
046200120706          $video='W02';
046300130926
046400120706       // fino a che la variabile resta settata come 'W02'
046500120706        dou $video <> 'W02';
046600120705          exfmt TBFLW02;
046700120706          *in29=*off;
046800120705          *in90=*off;
046900120705          select;
047000120705          when *inkl;
047100120705             $video = 'C02';
047200130926
047300120705          when *inkf;
047400120705             exsr sr_ctrw02;
047500120705             if not *in90;
047600120705                exsr aggc02;
047700120706             $video = 'C02';
047800120705             endif;
047900130926
048000120705          other;
048100120705             exsr sr_ctrw02;
048200120705          endsl;
048300120705        enddo;
048400120705       endsr;
048500120705       // ------------------------------------------------------------------------
048600130926       //   controllo terminal di paretenza/area
048700120705       // ------------------------------------------------------------------------
048800120705       begsr sr_ctrw02;
048900120705          clear w02tfpd;
049000130926          clear w02ared;
049100130926
049200130926 1        if %scan('?':w02tfp)>0;
049300120705             clear  SD19cod;
049400120705             clear  SD19tip;
049500120705             clear  SD19des;
049600120705             tnsd19r(SD19cod : SD19tip : SD19des );
049700120705             w02tfp= SD19cod;
049800120705             w02tfpd= SD19des;
049900120705             *in90=*on;
050000130926             *in46=*on;
050100120705             leavesr;
050200130926 1        endif;
050300130926
050400130926
050500130926 1        if %scan('?':w02are)>0;
050600130926      /end-free
050700130926     C                   MOVEL     '05'          COD               2
050800130926     C                   CLEAR                   §KEY              8
050900130926     C                   CALL      'X§TABER'
051000130926     C                   PARM      1             CODUT             1 0
051100130926     C                   PARM                    COD
051200130926     C                   PARM                    §KEY
051300130926     C                   PARM                    §DES             25
051400130926     c                   movel     §key          w02are
051500130926     c                   movel     §des          w02ared
051600130926      /free
051700130926
051800130926             *in90=*on;
051900130926             *in47=*on;
052000130926             leavesr;
052100130926 1        endif  ;
052200130926
052300130926          // errore se non immesso il terminal di partenza o area
052400130926 1        if (w02tfp = *blanks or w02tfp=*zeros) and
052500130926              w02are =*blanks  ;
052600120705             *in90=*on;
052700120706             *in29=*on;
052800130926             eval w02msg=msg(6);
052900120705             leavesr;
053000130926 1        endif;
053100130926
053200130926          // errore se immessi entrambi
053300130926 1        if  w02tfp <>*blanks and w02tfp<>*zeros  and
053400130926              w02are<>*blanks  ;
053500130926             *in90=*on;
053600130926             *in29=*on;
053700130926             eval w02msg=msg(6);
053800130926             leavesr;
053900130926 1        endif;
054000130926
054100130926 1        if  w02tfp <>*blanks and w02tfp<>*zeros          ;
054200130926      /end-free
054300120705     C                   testn                   w02tfp               30
054400130926 2  2c                   if             *in30  = *off
054500120705     c                             or  %subst(w02tfp: 3: 1) < *zero
054600120705     c                   eval      *in90=*on
054700120706     c                   eval      *in29=*on
054800130926     c                   eval      w02msg=msg(8)
054900120705     c                   leavesr
055000130926 2   c                   endif
055100120705      /free
055200120705          // errore se non è un terminal di partenza
055300120705          kfil=%dec(w02tfp:3:0);
055400120705          chain (kfil) azorg01l;
055500130926 2        if not %found(azorg01l) or (orgfag<>'A' and orgfag<>'F')
055600120706                                  or orgfva<>' ' ;
055700120705             *in90=*on;
055800120706             *in29=*on;
055900130926             eval w02msg=msg(8);
056000120705             leavesr;
056100130926 2        endif;
056200130926
056300120706          W02TFPD=orgdes;
056400130926
056500120705          // verifico se è un terminal di partenza e determino l'elenco delle
056600120706          // filiali del terminal richiamando il trul06r con £1
056700120706          clear trul06ds;
056800120706          d06cod='£1';
056900120706          d06key=w02tfp;
057000120706          d06tla='L';
057100120706          kpjbu=trul06ds;
057200120706          trul06r(kpjba);
057300120706          trul06ds=kpjbu;
057400130926 2        if d06err<>' ';
057500120706             *in90=*on;
057600120706             *in29=*on;
057700130926             eval w02msg=msg(8);
057800120706             leavesr;
057900130926 2        endif;
058000130926 1        endif;
058100130926
058200130926          // Controllo cod area
058300130926      /end-free
058400130926     c                   if        w02are<>*blanks
058500130926     c                   z-add     1             codut
058600130926     C                   MOVEL     '05'          COD
058700130926     C                   MOVEL(P)  w02are        KEY
058800130926     C     KTAB2         CHAIN     TABEL                              30
058900130926     C  N30TBLFLG        IFNE      ' '
059000130926     C                   SETON                                        30
059100130926     C                   END
059200130926     c                   if        *in30
059300130926     c                   seton                                        904729
059400130926     c                   eval      w02msg=msg(4)
059500130926     c                   leavesr
059600130926     c                   endif
059700130926     c                   movel     tbluni        w02ared
059800130926     c                   endif
059900130926      /free
060000130926
060100120705       endsr;
060200120706       // ------------------------------------------------------------------------
060300120706       // Scrittura delle filiali dalla schiera lin al subfile
060400120706       // ------------------------------------------------------------------------
060500120706       begsr aggc02;
060600120706          s02nrr=1;
060700120706          *in26=*on;
060800130926
060900130926 1        if w02tfp>*zeros  ;
061000130926
061100120706          Ix=1;
061200120706          chain s02nrr tbfls02;
061300130926 2        dow lin(Ix) > *zeros and s02nrr<=85;
061400120706           // elemento da caricare già presente nel sfl
061500130926 3            if %lookup(lin(Ix):slp)>0;
061600120706                 ix+=1;
061700120706                 iter;
061800130926 3            endif;
061900130926
062000130926 3            if s02lp=*blanks or s02lp=*zeros;
062100120706                 s02lp=lin(Ix);
062200120706                 Ix+=1;
062300130926 3            endif;
062400120706                 update tbfls02;
062500120706                 s02nrr+=1;
062600120706              chain s02nrr tbfls02;
062700130926 2         enddo;
062800130926
062900120706           // Se non caricati tutte le linee di partenza per mancanza
063000120706           // di spazio emetto messaggio a video
063100130926 2         if lin(IX)> *zeros;
063200120706              vc2msg=msg(07);
063300120706              *in28=*on;
063400130926 2         endif;
063500130926
063600130926 x1    else  ;
063700130926
063800130926       setll *loval  azorg01l    ;
063900130926       read          azorg01l    ;
064000130926
064100130926 2     dow  not %eof(azorg01l)  ;
064200130926
064300130926 3     if orgfva=' ' and %editc(orgcar:'X') = w02are  ;
064400130926
064500130926           // elemento da caricare già presente nel sfl
064600130926 4            if %lookup(%editc(orgfil:'X'):slp)=0;
064700130926
064800130926              chain s02nrr tbfls02;
064900130926
065000130926 5            dow s02lp<>*blanks and s02lp<>*zeros
065100130926                                      and s02nrr<=85    ;
065200130926                 s02nrr+=1;
065300130926              chain s02nrr tbfls02;
065400130926 5            enddo ;
065500130926
065600130926 5            if s02nrr<=85  ;
065700130926                 s02lp=%editc(orgfil:'X')  ;
065800130926                 s02dlp=orgde5  ;
065900130926                 if s02dlp=*blanks  ;
066000130926                  s02dlp=orgdes  ;
066100130926                 endif  ;
066200130926
066300130926                 update tbfls02;
066400130926                 s02nrr+=1;
066500130926
066600130926 x5           else ;
066700130926              vc2msg=msg(07);
066800130926              *in28=*on;
066900130926              leavesr  ;
067000130926 5          endif  ;
067100130926 4          endif  ;
067200130926 3          endif  ;
067300130926
067400130926       read          azorg01l    ;
067500130926 2     enddo  ;
067600130926
067700130926 1     endif  ;
067800130926
067900120706       endsr;
068000060502       // ------------------------------------------------------------------------
068100060502       // routine iniziale
068200060502       // ------------------------------------------------------------------------
068300060428         begsr *inzsr;
068400060428
068500060428      /end-free
068600060428
068700130926     C     KTAB2         KLIST
068800130926     C                   KFLD                    CODUT
068900130926     C                   KFLD                    COD               2
069000130926     C                   KFLD                    KEY               8
069100060428     c     *entry        plist
069200060428     c                   parm                    kpjba
069300131015     c*****              movel     kpjbu         c2cke1
069400060428
069500060428      /free
069600060428         in(e) §azute;
069700060428         if not %error;
069800060428          in(e) §datiute;
069900060428         endif;
070000060502         if %error or rsut = *blanks;
070100060428          tibs34r(tibs34ds);
070200060428          in §azute;
070300060428          in §datiute;
070400060428         endif;
070500060428
070600060428         endsr;
070700060502
070800060502       // ------------------------------------------------------------------------
070900060502       // uscita dal programma
071000060502       // ------------------------------------------------------------------------
071100060502         begsr sr_uscita;
071200060502
071300060502          *inlr = *on;
071400060502          return;
071500060502
071600060502         endsr;
071700060502
071800060502      /end-free
071900060502
072000060502** -MSG-                                                                     *
072100120620Linea di arrivo mancante o errata                                              01
072200120704Linea di partenza mancante o errata                                            02
072300120705Linea di partenza già indicata                                                 03
072400130926Codice area errato                                                             04
072500120709Chiave già presente in tabella                                                 05
072600130926Immettere il terminal di partenza o il codice Area                             06
072700120706Attenzione! Non inserite tutte le linee per mancanza di spazio                 07
072800130926Terminal di partenza errato                                                    08
072900131016Immettere un codice                                                            09
