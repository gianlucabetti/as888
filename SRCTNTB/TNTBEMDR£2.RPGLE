000100060502     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200140401     h dftactgrp(*no)
000300140401     h bnddir('UBBNDDIR')
000400060427      *------------------------------------------------------------------------*
000500151106      * Gestione tabella "EMD" = alert affidamento spedizione posticipato in chiusura FGV
000600060427      *------------------------------------------------------------------------*
000700140331     ftntbe01l  uf a e           k disk    extfile(wlibfile1) usropn
000800130926     ftabel00f  if   e           k disk
000900130926     fazorg01l  if   e           k disk
001000151106     ftntbemdd  cf   e             workstn sfile(tbems02:s02nrr)
001100151106     f                                     sfile(tbems04:s04nrr)
001200060428
001300060428      *------------------------------------------------------------------------*
001400060428      * riepilogo indicatori
001500060428      *------------------------------------------------------------------------*
001600060508      * 20 - gestione subfile 01
001700060508      * 21 - gestione subfile 01
001800060508      * 22 - gestione subfile 01
001900060508      * 23 - gestione subfile 01
002000120704      * 24 - gestione subfile 02
002100120704      * 25 - gestione subfile 02
002200120704      * 26 - gestione subfile 02
002300120704      * 27 - gestione subfile 02
002400060428      * 28 - messaggio errore
002500151106      * 40/43Errori campi subfile/control
002600060502
002700140331       // -?Costanti per la definizione delle schiere con i nomi?
002800140331       //  ?degli iSeries da elaborare e delle relative librerie?
002900140331     d c_NrSyst        c                   const(2)
003000140331     d c_NrLibr        c                   const(2)
003100060502      *------------------------------------------------------------------------*
003200060502      * schiere
003300060502      *------------------------------------------------------------------------*
003400140331       // -?iSeries  &  Librerie con entrambi i file tabelle?
003500140331     d $iSystem        s                   like(currSysNetA)
003600140331     d                                     dim(c_NrSyst)
003700140331     d                                     ctdata   perrcd( 1)
003800140331     d $Libraries      s                   like(ds_Libr)
003900140331     d                                     dim(c_NrSyst)
004000140331     d                                     alt($iSystem)
004100131015     d msg             s             78    dim(17) ctdata perrcd(1)
004200060427
004300060428      *------------------------------------------------------------------------*
004400060428      * campi di work
004500060428      *------------------------------------------------------------------------*
004600090428     d data_eur        s               d   datfmt(*eur)
004700060428     d blanks          s                   like(d1descopz)
004800090427     d comand          s              1
004900060428     d len             s              5u 0
005000120704     d s02nrr          s              4  0
005100151106     d s04nrr          s              4  0
005200130926     d savrcdnbr       s                   like(c2rcdnbr)
005300120704     d sv2rcdnbr       s                   like(c2rcdnbr)
005400151109     d sv4rcdnbr       s                   like(c2rcdnbr)
005500120620     d kfil            s                   like(orgfil)
005600151106     d alfatfp         s              3
005700151106     d alfatfa         s              3
005800151106     d w006a           s              6
005900151106     d wrkcarw01       s              1
006000130926     d WATB            s              1
006100060504     d $video          s             10
006200120620     d i               s              3  0 inz
006300120620     d w0030           s              3  0 inz
006400120706     d ix              s              3  0 inz
006500130926     d YY              s              3  0 inz
006600130926     d XX              s              3  0 inz
006700140314     d X               s              3  0 inz
006800131016     d winkq           s              1
006900131015
007000131015     d $Fine           s               n   inz(*off)
007100120705
007200120705       // - Parametri per TNSD19R
007300120705     d SD19cod         s              3    inz
007400120705     d SD19tip         s              1    inz
007500120705     d SD19des         s             25    inz
007600060428
007700060428      *------------------------------------------------------------------------*
007800060428      * ds interne/esterne
007900060428      *------------------------------------------------------------------------*
008000140331       // -?Ridefinizione elenco librerie in elaborare le tabelle?
008100140331     d ds_Libr         ds                  inz
008200140331     d   $Libr                       10    dim(c_NrLibr) inz
008300140331
008400151106     D  Slp            S              6    dim(250)
008500151109     D  ktsp           S              1    dim(10)
008600120620     d
008700131015     d TIBS02ds      e ds                  inz
008800151106     d fnlv55ds      e ds                  inz
008900060502     d kpjba         e ds
009000060503     d tibs34ds      e ds                  inz
009100060428     d §azute        e ds                  extname(azute00f)
009200060428     d                                     dtaara
009300060428     d §datiute      e ds                  extname(ddatiute)
009400060428     d                                     dtaara
009500120706     d trul06ds      e ds
009600120706     D  LIN                    1     90    dim(30)
009700010503
009800140331       // -?Nome del sistema?
009900140331     d currSysNeta     s              8a   inz
010000140331
010100140331       // -?Nome esteso Libreria/File dei 2 file tabella?
010200140331     d wLibFile1       s             21a   inz
010300140331     d w_iSystem       s              1  0 inz
010400140331     d w_SisInf        s              3  0 inz
010500060428     d psds           sds
010600060428     d  pgmname          *proc
010700060428
010800090424      // ? PROTOTIPI ?
010900090424      /copy gaitrasrc/srcprotopr,tibs34r
011000151106      /copy gaitrasrc/srcprotopr,fnlv55r
011100131015      /copy gaitrasrc/srcProtoPR,TIBS02R
011200140331       // -?Reperimento NETA sistema AS/400 corrente?
011300140331      /copy gaitrasrc/srcProtoPR,UBRTVNETA
011400130926
011500120705       // - Ricerca codici Organigramma
011600120705     d tnsd19r         pr                  extpgm('TNSD19R')
011700120705     d  D19cod                             like(SD19cod)
011800120705     d  D19tip                             like(SD19tip)
011900120705     d  D19des                             like(SD19des)
012000060428
012100060428      *------------------------------------------------------------------------*
012200060428      * costanti
012300060428      *------------------------------------------------------------------------*
012400060428     d errore          c                   '1'
012500060428     d eseguito        c                   '0'
012600010503
012700060428      *------------------------------------------------------------------------*
012800120621
012900120704      /free
013000131015
013100140331          $video='D1';
013200131015
013300131015       DOW $Fine = *off;
013400131015         SELECT;
013500131015           WHEN $Video = 'D1';
013600131015              exsr sr_d01;
013700131015           WHEN $Video = 'C2';
013800131015              exsr sr_c02;
013900151106           WHEN $Video = 'C4';
014000151106              exsr sr_c04;
014100131015           OTHER;
014200131015             $Fine = *on;
014300131015         endsl;
014400131015       enddo;
014500131015
014600131015       exsr sr_uscita;
014700131015       // ------------------------------------------------------------------------
014800131015       // gestione video richiesta codice tabella abilitazione filiali
014900131015       // ------------------------------------------------------------------------
015000131015       begsr sr_d01;
015100131015
015200151109       clear v1ctfpa ;
015300151109       clear v1ctsp  ;
0154001511061      dow $video='D1';
015500151106          exfmt tbemd01;
015600131015          *in28=*off;
015700131015          clear d01msg;
0158001511062         select;
0159001511062         when *inkc=*on;
016000131015             $video=*blanks;
0161001511062         other;
016200131015             exsr sr_cntd01;
0163001511063            if not *in90;
016400151109             select  ;
0165001511094             when v1ctfpa='S' ;
016600131015                $video='C2';
0167001511094             when v1ctsp ='S' ;
016800151106                $video='C4';
0169001511094             endsl;
0170001511063            endif;
0171001511062         endsl;
017200131015       enddo;
017300131015       endsr;
017400131015       // ------------------------------------------------------------------------
017500131015       // Controllo video richiesta codice tabella abilitazione filiali
017600131015       // ------------------------------------------------------------------------
017700131015       begsr sr_cntd01;
017800151106       *in90=*off  ;
017900131015       endsr;
018000130926
018100060502       // ------------------------------------------------------------------------
018200151106       // aggiorno tabella  EMD tfptfa
018300060502       // ------------------------------------------------------------------------
018400060502       begsr sr_aggiorna;
018500060502
018600151106         tbecod = 'EMD';
018700151106         tbeke1 = 'TFPTFA'           ;
018800151106         // deleto e riscrivo tutto
018900140314         setll (tbecod:tbeke1) tntbe01l;
019000140314         reade (tbecod:tbeke1) tntbe01l;
019100151106         dow    not %eof(tntbe01l)  ;
019200151106           delete tntbe000  ;
019300151106           reade (tbecod:tbeke1) tntbe01l;
019400151106         enddo  ;
019500151106
019600140314            exsr sr_impoupd;
019700010430
019800060502       endsr;
019900151106       // ------------------------------------------------------------------------
020000151106       // aggiorno tabella  EMD tipo servizio
020100151106       // ------------------------------------------------------------------------
020200151106       begsr sr_aggiornatsp ;
020300151106
020400151106         tbecod = 'EMD';
020500151106         tbeke1 = 'TSP'           ;
020600151106         // deleto e riscrivo tutto
020700151106         setll (tbecod:tbeke1) tntbe01l;
020800151106         reade (tbecod:tbeke1) tntbe01l;
020900151106         dow    not %eof(tntbe01l)  ;
021000151106           delete tntbe000  ;
021100151106           reade (tbecod:tbeke1) tntbe01l;
021200151106         enddo  ;
021300151106
021400151106            exsr sr_impoupdTSP;
021500151106
021600151106       endsr;
021700140314       // ------------------------------------------------------------------------
021800151106       // Campi in update    TFPTFA
021900140314       // ------------------------------------------------------------------------
022000140314       BEGSR sr_impoupd;
022100151106        clear tbeatb  ;
022200151109        tbeftt = 'S'    ;
022300151109        tbeflt = 000  ;
022400151109         if  w_SisInf = 1;
022500151109           TBEftr = 'T';
022600151109         else;
022700151109           TBEftr = 'R';
022800151109         endif;
022900151109         TBEdtr = %dec(*date);
023000151106        s02nrr=1  ;
023100151106 1      chain s02nrr  tbems02;
023200151106            dow %found   ;
023300151106          if s02tfp>*zeros  ;
023400151106          tbeke2=s02tfp+s02tfa  ;
023500151106          write tntbe000        ;
023600151106          endif   ;
023700151106              s02nrr=s02nrr+1  ;
023800151106              chain s02nrr  tbems02;
023900151106 1          enddo;
024000151106
024100140314       endsr;
024200151106       // ------------------------------------------------------------------------
024300151106       // Campi in update  TSP
024400151106       // ------------------------------------------------------------------------
024500151106       BEGSR sr_impoupdTSP;
024600151106        clear tbeatb  ;
024700151109        tbeftt = 'S'    ;
024800151109        tbeflt = 000   ;
024900151109         if  w_SisInf = 1;
025000151109           TBEftr = 'T';
025100151109         else;
025200151109           TBEftr = 'R';
025300151109         endif;
025400151109         TBEdtr = %dec(*date)  ;
025500151106        s04nrr=1  ;
025600151106 1      chain s04nrr  tbems04;
025700151106            dow %found   ;
025800151109          if s04tsp<>*blanks  ;
025900151106          tbeke2=s04tsp         ;
026000151106          write tntbe000        ;
026100151106          endif   ;
026200151106              s04nrr=s04nrr+1  ;
026300151106              chain s04nrr  tbems04;
026400151106 1          enddo;
026500151106
026600151106       endsr;
026700120704       // ------------------------------------------------------------------------
026800151106       // gestione subfile per terminal partenza/arrivo
026900120704       // ------------------------------------------------------------------------
027000130926       BEGSR sr_c02;
027100130926
027200120704        $video = 'C02';
027300130926
027400120704       // caricamento subfile
027500130926        exsr sr_cars02;
027600130926
027700120704       // inizio elaborazione subfile
0278001309261        dou  $video <> 'C02';
027900130926
028000120704       // c2csrrrn contiene il numero di riga del subfile su cui era posizionato il cursore
028100120704       // impostando c2rcdnbr visualizzo la pagina che vedeva l'utente quando ha premuto
028200120704       // l'ultimo tasto
0283001309262         if c2csrrrn > 0;
028400120704           c2rcdnbr = c2csrrrn;
028500120704          else;
028600120704           c2rcdnbr = sv2rcdnbr;
0287001309262         endif;
028800120704       // se non so quale pagina visualizzare forzo pagina 1
0289001309262         if c2rcdnbr < 1;
029000120704           eval c2rcdnbr = 1;
0291001309262         endif;
029200120704
029300120704       // salvo il record number del subfile
029400120704          sv2rcdnbr = c2Rcdnbr;
029500120704
029600120704       // Emissione del subfile
029700151106          write tbemp02;
029800151106          exfmt tbemc02;
029900120704
030000120704       // controllo tasti funzionali del subfile
0301001309262         select;
030200120704       // F3=Fine
0303001309262          when *inkc;
030400120704            $video = *blanks;
030500151109       if v1ctsp='S'  ;
030600151109            $video = 'C4';
030700151109            else  ;
030800120704            exsr sr_uscita;
030900151109       endif  ;
031000151109
031100131015       // F12=Ritorno
0312001310152          when *inkl;
031300151109       if v1ctsp='S'  ;
031400151109            $video = 'C4';
031500151109            else  ;
031600131015            $video = 'D1';
031700151109            endif  ;
031800130926
0319001309262      // F6=Conferma
032000120704           when *inkf;
032100120704            // controllo e decodifico i dati del video
032200120704            exsr sr_ctrs02;
032300130926
032400130926 3          if not *in28;
032500140401                exsr sr_UpdateAll;
032600151109       if v1ctsp='S'  ;
032700151109            $video = 'C4';
032800151110          else  ;
032900151110            $video = 'D1';
033000151109       endif  ;
033100130926 3          endif;
033200130926
033300120704       // Invio
033400130926 x2        other;
033500120704             exsr sr_ctrs02;
033600130926 2        endsl;
033700130926
033800120704       // fine elaborazione 'C02'
033900130926 1       enddo;
034000151109
034100120704       endsr;
034200120704
034300120704       // ------------------------------------------------------------------------
034400151106       // caricamento subfile2 - dettaglio per tfp/tfa
034500120704       // ------------------------------------------------------------------------
034600130926       begsr sr_cars02;
034700120704
034800130924       // Inizializzo il subfile
034900120704         s02nrr = 0;
035000120704         *in24 = *on;
035100151106         write tbemc02;
035200120704         *in24 = *off;
035300130926         *in25 = *on ;
035400120704         *in26 = *off;
035500120704         *in27 = *off;
035600151110
035700151110         // eseguo apertura a tntbe puntando all'ambiente di sede
035800151110         w_SisInf = 1;
035900151110         // -?Apertura file tabelle                                 ?
036000151110         exsr sr_OpenFileTab;
036100130924
036200130924       // chain      file
036300151106         tbecod = 'EMD';
036400151106         tbeke1 = 'TFPTFA'           ;
036500140314         setll (tbecod:tbeke1) tntbe01l;
036600140314         reade (tbecod:tbeke1) tntbe01l;
036700140314         // Riempio la schiera delle filiali e poi la emetto a video
036800151106
036900151106 1     dow not %eof(tntbe01l);
037000151106 1a      if tbeatb=' ' ;
037100130924
037200151106        alfatfp=%subst(tbeke2:1:3)  ;
037300151106        alfatfa=%subst(tbeke2:4:3)  ;
037400130926
037500151106        clear s02dtfp     ;
037600151106        clear s02dtfa     ;
037700151106        clear s02tfp      ;
037800151106        clear s02tfa      ;
037900151106  3     if alfatfp>*zeros  ;
038000151106        s02tfp=alfatfp  ;
038100130924
038200151106        w0030 =%int(s02tfp)    ;
038300130924        chain w0030  azorg01l   ;
038400151106  4      if %found(azorg01l)  ;
038500151106         s02dtfp=orgde5  ;
038600151106  5       if s02dtfp=*blanks  ;
038700151106           s02dtfp=orgdes  ;
038800151106  5       endif  ;
038900130926  4     endif  ;
039000151106  3     endif  ;
039100151106
039200151106  3     if alfatfa>*zeros  ;
039300151106        s02tfa=alfatfa  ;
039400151106
039500151106        w0030 =%int(s02tfa)    ;
039600151106        chain w0030  azorg01l   ;
039700151106  4      if %found(azorg01l)  ;
039800151106         s02dtfa=orgde5  ;
039900151106  5       if s02dtfa=*blanks  ;
040000151106           s02dtfa=orgdes  ;
040100151106  5       endif  ;
040200151106  4     endif  ;
040300130926  3     endif  ;
040400130924
040500130924        s02nrr=s02nrr+  1 ;
040600151106        write tbems02  ;
040700130924
040800151106  1a    endif;
040900151106
041000140314         reade (tbecod:tbeke1) tntbe01l;
041100151106  1     enddo;
041200130926
041300140314        dow s02nrr  <  250  ;
041400151106        clear s02tfp ;
041500151106        clear s02dtfp ;
041600151106        clear s02tfa ;
041700151106        clear s02dtfa ;
041800130926        s02nrr=s02nrr+  1 ;
041900151106        write tbems02  ;
042000130926        enddo  ;
042100130926
042200120704
042300120704       endsr;
042400120704       // ------------------------------------------------------------------------
042500151106       // controllo subfile 2 (dettaglio per tfp/tfa
042600120704       // ------------------------------------------------------------------------
042700120704       begsr sr_ctrs02;
042800120704
042900120704        *in28 = *off;
043000120704        *in42 = *off;
043100151106        *in43 = *off;
043200120704        clear vc2msg;
043300120704
043400120704        clear slp;
043500120706        *in26=*on;
043600130926        s02nrr=1  ;
043700130926
043800151106 1      chain s02nrr  tbems02;
043900130926            dow %found   ;
044000151106              clear s02dtfp ;
044100151106              clear s02dtfa ;
044200151106 2            if s02tfp<>*zeros and s02tfp<>*blanks;
044300120704      /end-free
044400151106     C                   testn                   s02tfp               30
044500130926    3c                   if             *in30  = *off
044600151106     c                             or  %subst(s02tfp: 3: 1) < *zero
044700120704     c                   eval      vc2msg = msg(02)
044800120704     c                   eval      *in28 = *on
044900120704     c                   eval      *in42 = *on
045000151106     c                   update    tbems02
045100120705     c                   eval      c2csrrrn=s02nrr
045200120704     c                   leavesr
045300130926    3c                   endif
045400120704      /free
045500151106              w0030=%dec(s02tfp:3:0);
045600120704              chain w0030 azorg01l;
045700130926 3            if not %found (azorg01l) or (orgfag <>'A' and orgfag<>'F');
045800120704                 vc2msg = msg(02);
045900120704                 *in28 = *on;
046000120704                 *in42 = *on;
046100151106                 update tbems02;
046200120705                 c2csrrrn=s02nrr;
046300120704                 leavesr;
046400130926 3            endif;
046500120704                // decodifico la linea di partenza
046600151106                s02dtfp=orgde5;
046700151106 3              if s02dtfp=*blanks;
046800151106                   s02dtfp=orgdes;
046900130926 3              endif;
047000151109            // verifico se è un terminal di partenza
047100151106            clear fnlv55ds   ;
047200151106            d55tpt='P'       ;
047300151109            d55lin=w0030          ;
047400151109            d55drf= %dec(*date)  ;
047500151106            callp FNLV55R (fnlv55ds)  ;
047600151106 3          if  d55tfp<>d55lin or d55err<>' ' ;
047700151106                 vc2msg = msg(02);
047800151106                 *in28 = *on;
047900151106                 *in42 = *on;
048000151106                 update tbems02;
048100151106                 c2csrrrn=s02nrr;
048200151106                 leavesr;
048300151106 3            endif;
048400151106 2            endif;
048500130926
048600151106 2            if s02tfp=*zeros;
048700151106                 clear s02tfp;
048800151106                 clear s02dtfp;
048900130926 2            endif;
049000151106            // Terminal di arrivo
049100151106 2            if s02tfa<>*zeros and s02tfa<>*blanks;
049200151106      /end-free
049300151106     C                   testn                   s02tfa               30
049400151106    3c                   if             *in30  = *off
049500151106     c                             or  %subst(s02tfa: 3: 1) < *zero
049600151106     c                   eval      vc2msg = msg(02)
049700151106     c                   eval      *in28 = *on
049800151106     c                   eval      *in43 = *on
049900151106     c                   update    tbems02
050000151106     c                   eval      c2csrrrn=s02nrr
050100151106     c                   leavesr
050200151106    3c                   endif
050300151106      /free
050400151106              w0030=%dec(s02tfa:3:0);
050500151106              chain w0030 azorg01l;
050600151106 3            if not %found (azorg01l) or (orgfag <>'A' and orgfag<>'F');
050700151106                 vc2msg = msg(02);
050800151106                 *in28 = *on;
050900151106                 *in43 = *on;
051000151106                 update tbems02;
051100151106                 c2csrrrn=s02nrr;
051200151106                 leavesr;
051300151106 3            endif;
051400151106                // decodifico la linea di partenza
051500151106                s02dtfa=orgde5;
051600151106 3              if s02dtfa=*blanks;
051700151106                   s02dtfa=orgdes;
051800151106 3              endif;
051900151109            // verifico se è un terminal di arrivo
052000151109            clear fnlv55ds   ;
052100151109            d55tpt='A'       ;
052200151109            d55lin=w0030          ;
052300151109            d55drf= %dec(*date)  ;
052400151109            callp FNLV55R (fnlv55ds)  ;
052500151109 3          if  d55tfa<>d55lin or d55err<>' ' ;
052600151109                 vc2msg = msg(02);
052700151109                 *in28 = *on;
052800151109                 *in43 = *on;
052900151109                 update tbems02;
053000151109                 c2csrrrn=s02nrr;
053100151109                 leavesr;
053200151109 3            endif;
053300151109 2            endif;
053400151106
053500151106 2            if s02tfa=*zeros;
053600151106                 clear s02tfa;
053700151106                 clear s02dtfa;
053800151106 2            endif;
053900151106
054000151106              // o vuoti entrami o pieni entrami
054100151106              if (s02tfp >*zeros and s02tfa<=*zeros) or
054200151106                 (s02tfp <=*zeros and s02tfa >*zeros)     ;
054300151106                 vc2msg = msg(01);
054400151106                 *in28 = *on;
054500151106                 *in43 = *on;
054600151106                 update tbems02;
054700151106                 c2csrrrn=s02nrr;
054800151106                 leavesr;
054900151106              endif                                ;
055000151106
055100151106 2            if s02tfp>*blanks;
055200120705                 // errore se linea di partenza ripetuta all'interno del sfl
055300151106                 w006a=s02tfp+s02tfa  ;
055400151106 3               if %lookup(w006a:slp)>0;
055500120705                    vc2msg = msg(03);
055600120705                    *in28 = *on;
055700120705                    *in42 = *on;
055800151106                    update tbems02;
055900120705                    c2csrrrn=s02nrr;
056000120705                    leavesr;
056100130926 x3              else;
056200120705                 // se linea di partenza non ripetuta memorizzo in schiera
056300151106                    i=%lookup('      ':slp);
056400130926 4                  if i>0;
056500151106                       slp(i)=w006a ;
056600130926 4                  endif;
056700130926 3               endif;
056800130926 2            endif;
056900130926
057000151106              update tbems02;
057100130926              s02nrr=s02nrr+1  ;
057200151106              chain s02nrr  tbems02;
057300130926 1          enddo;
057400120704       endsr;
057500060505
057600151106       // ------------------------------------------------------------------------
057700151106       // gestione subfile per tipo servizio
057800151106       // ------------------------------------------------------------------------
057900151106       BEGSR sr_c04;
058000151106
058100151106        $video = 'C04';
058200151106
058300151110        *inkl=*off ;
058400151110        *inkc=*off ;
058500151106       // caricamento subfile
058600151106        exsr sr_cars04;
058700151106
058800151106       // inizio elaborazione subfile
0589001511061        dou  $video <> 'C04';
059000151106
059100151106       // c4csrrrn contiene il numero di riga del subfile su cui era posizionato il cursore
059200151106       // impostando c4rcdnbr visualizzo la pagina che vedeva l'utente quando ha premuto
059300151106       // l'ultimo tasto
0594001511062         if c4csrrrn > 0;
059500151106           c4rcdnbr = c4csrrrn;
059600151106          else;
059700151106           c4rcdnbr = sv4rcdnbr;
0598001511062         endif;
059900151106       // se non so quale pagina visualizzare forzo pagina 1
0600001511062         if c4rcdnbr < 1;
060100151106           eval c4rcdnbr = 1;
0602001511062         endif;
060300151106
060400151106       // salvo il record number del subfile
060500151106          sv2rcdnbr = c4Rcdnbr;
060600151106
060700151106       // Emissione del subfile
060800151106          write tbemp02;
060900151106          exfmt tbemc04;
061000151106
061100151106       // controllo tasti funzionali del subfile
0612001511062         select;
061300151106       // F3=Fine
0614001511062          when *inkc;
061500151106            $video = *blanks;
061600151106            exsr sr_uscita;
061700151106       // F12=Ritorno
0618001511062          when *inkl;
061900151106            $video = 'D1';
062000151106
0621001511062      // F6=Conferma
062200151106           when *inkf;
062300151106            // controllo e decodifico i dati del video
062400151106            exsr sr_ctrs04;
062500151106
062600151106       // se non riscontrati errori emetto la finestra con i dati per la trasmissione
062700151106       // se programma con chiamata non diretta  altrimenti non emetto la finestra
062800151106       // perchè in questo caso il pgm aggiorna direttamente sia ambiente di sede che
062900151106       // ambiente di filiale
063000151106 3          if not *in28;
063100151106                exsr sr_UpdateTSP;
063200151110                $video = 'D1';
063300151106                leavesr;
063400151106 3          endif;
063500151106
063600151106       // Invio
063700151106 x2        other;
063800151106             exsr sr_ctrs04;
063900151106 2        endsl;
064000151106
064100151106       // fine elaborazione 'C04'
064200151106 1       enddo;
064300151106       endsr;
064400151106
064500151106       // ------------------------------------------------------------------------
064600151106       // caricamento subfile4 - dettaglio per tipo servizio
064700151106       // ------------------------------------------------------------------------
064800151106       begsr sr_cars04;
064900151106
065000151106       // Inizializzo il subfile
065100151106         s04nrr = 0;
065200151106         *in24 = *on;
065300151106         write tbemc04;
065400151106         *in24 = *off;
065500151106         *in25 = *on ;
065600151106         *in26 = *off;
065700151106         *in27 = *off;
065800151110
065900151110         // eseguo apertura a tntbe puntando all'ambiente di sede
066000151110         w_SisInf = 1;
066100151110         // -?Apertura file tabelle                                 ?
066200151110         exsr sr_OpenFileTab;
066300151110
066400151106       // chain      file
066500151106         tbecod = 'EMD';
066600151106         tbeke1 = 'TSP'           ;
066700151106         setll (tbecod:tbeke1) tntbe01l;
066800151106         reade (tbecod:tbeke1) tntbe01l;
066900151106         // Riempio la schiera delle filiali e poi la emetto a video
067000151106
067100151106 1     dow not %eof(tntbe01l);
067200151106 1a      if tbeatb=' ' ;
067300151106
067400151106        clear s04dtsp     ;
067500151106        clear s04tsp      ;
067600151106  3     if tbeke2 <>' '    ;
067700151106        s04tsp=tbeke2   ;
067800151106
067900151109        tblkey=s04tsp  ;
068000151109        chain (1:'5E':tblkey) tabel00f  ;
068100151109        if %found(tabel00f)  ;
068200151106        s04dtsp=tbluni  ;
068300151106        endif  ;
068400151106  3     endif  ;
068500151106
068600151106
068700151106        s04nrr=s04nrr+  1 ;
068800151106        write tbems04  ;
068900151106
069000151106  1a    endif;
069100151106
069200151106         reade (tbecod:tbeke1) tntbe01l;
069300151106  1     enddo;
069400151106
069500151106        dow s04nrr  <  10   ;
069600151106        clear s04tsp ;
069700151106        clear s04dtsp ;
069800151106        s04nrr=s04nrr+  1 ;
069900151106        write tbems04  ;
070000151106        enddo  ;
070100151106
070200151106
070300151106       endsr;
070400151106       // ------------------------------------------------------------------------
070500151106       // controllo subfile 4 (dettaglio per tipo aservizio
070600151106       // ------------------------------------------------------------------------
070700151106       begsr sr_ctrs04;
070800151106
070900151106        *in28 = *off;
071000151106        *in44 = *off;
071100151106        clear vc2msg;
071200151106
071300151106        clear ktsp ;
071400151106        *in26=*on;
071500151106        s04nrr=1  ;
071600151106
071700151106 1      chain s04nrr  tbems04;
071800151106            dow %found   ;
071900151106              clear s04dtsp ;
072000151106 2            if s04tsp<>' '  ;
072100151106              tblkey=s04tsp  ;
072200151109              chain (1:'5E':tblkey) tabel00f  ;
072300151109 3            if not %found (tabel00f) ;
072400151106                 vc2msg = msg(04);
072500151106                 *in28 = *on;
072600151106                 *in44 = *on;
072700151106                 update tbems04;
072800151106                 c4csrrrn=s04nrr;
072900151106                 leavesr;
073000151106 3            endif;
073100151106                // decodifico la linea di partenza
073200151106                s04dtsp=tbluni;
073300151106 2            endif;
073400151106
073500151106 2            if s04tsp=' '   ;
073600151106                 clear s04tsp;
073700151106                 clear s04dtsp;
073800151106 2            endif;
073900151109 2            if s04tsp<>*blanks;
074000151109                 // errore se tsp ripetuto
074100151106 3               if %lookup(s04tsp:ktsp)>0;
074200151106                    vc2msg = msg(04);
074300151106                    *in28 = *on;
074400151106                    *in44 = *on;
074500151106                    update tbems04;
074600151106                    c4csrrrn=s04nrr;
074700151106                    leavesr;
074800151106 x3              else;
074900151109                 // se  non ripetuto memorizzo in schiera
075000151106                    i=%lookup(' ':ktsp);
075100151106 4                  if i>0;
075200151106                       ktsp(i)=s04tsp;
075300151106 4                  endif;
075400151106 3               endif;
075500151106 2            endif;
075600151106
075700151106              update tbems04;
075800151106              s04nrr=s04nrr+1  ;
075900151106              chain s04nrr  tbems04;
076000151106 1          enddo;
076100151106       endsr;
076200151106
076300140401       //--------------------------------------------------------------
076400140401       //?Apertura dei files tabelle nel sistema informativo impostato.?
076500140401       //--------------------------------------------------------------
076600140401       BEGSR  sr_OpenFileTab;
076700140401
076800140401         // -?Chiusura (eventuale) archivi?
076900140401         if  %open(Tntbe01l);
077000140401           close  Tntbe01l;
077100140401         endif;
077200140401
077300140401         // -?Apertura archivi?
077400140401         ds_Libr  = $Libraries(w_iSystem);
077500140401         wLibFile1 = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
077600140401         open  TNTBE01L;
077700140401
077800140401       ENDSR;
077900140401       //--------------------------------------------------------------
078000140401       //?Aggiornam. tab. nei file di tutti i sistemi informativi?
078100140401       //--------------------------------------------------------------
078200140401       BEGSR  sr_UpdateAll;
078300140401
078400140401         // -?Ciclo di elaborazione per ogni sistema informativo?
078500140401         For  w_SisInf = 1  To  %elem($Libr);
078600140401
078700140401           // -?Apertura degli archivi?
078800140401           if  w_SisInf > 1;
078900140401             exsr  sr_OpenFileTab;
079000140401           endif;
079100140401
079200140401           exsr  sr_aggiorna;
079300140401
079400140401         EndFor;
079500140401
079600140401       ENDSR;
079700151106       //--------------------------------------------------------------
079800151106       //?Aggiornam. tab. nei file di tutti i sistemi informativi?
079900151106       //--------------------------------------------------------------
080000151106       BEGSR  sr_Updatetsp;
080100151106
080200151106         // -?Ciclo di elaborazione per ogni sistema informativo?
080300151106         For  w_SisInf = 1  To  %elem($Libr);
080400151106
080500151106           // -?Apertura degli archivi?
080600151106           if  w_SisInf > 1;
080700151106             exsr  sr_OpenFileTab;
080800151106           endif;
080900151106
081000151106           exsr  sr_aggiornatsp;
081100151106
081200151106         EndFor;
081300151106
081400151106       ENDSR;
081500060502       // ------------------------------------------------------------------------
081600060502       // routine iniziale
081700060502       // ------------------------------------------------------------------------
081800060428         begsr *inzsr;
081900060428
082000060428      /end-free
082100060428
082200060428     c     *entry        plist
082300060428     c                   parm                    kpjba
082400140331     c                   movel     kpjbu         PCodVpo           9
082500140331
082600060428      /free
082700140401         // -?Verifica del sistema AS/400 corrente?
082800140401         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
082900140401           $Fine = *on;
083000140401           leavesr;
083100140401         endif;
083200140401
083300140401         // -?Impostazione elenco librerie in cui gestire le tabelle?
083400140401         //  ?(a seconda del sistema in cui si stà lavorando)?
083500140401         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
083600140401         if  w_iSystem = *zero;
083700140401           $Fine = *on;
083800140401           leavesr;
083900140401         endif;
084000140401
084100060428         in(e) §azute;
084200060428         if not %error;
084300060428          in(e) §datiute;
084400060428         endif;
084500060502         if %error or rsut = *blanks;
084600060428          tibs34r(tibs34ds);
084700060428          in §azute;
084800060428          in §datiute;
084900060428         endif;
085000060428
085100060428         endsr;
085200060502
085300060502       // ------------------------------------------------------------------------
085400060502       // uscita dal programma
085500060502       // ------------------------------------------------------------------------
085600060502         begsr sr_uscita;
085700060502
085800060502          *inlr = *on;
085900060502          return;
086000060502
086100060502         endsr;
086200060502
086300060502      /end-free
086400060502
086500140401** - $iSystem / $Libraries:?Sistemi AS/400 & Librerie con entrambi i file?
086600140401SETRAS  GAITRAGRU FILTRAGRU
086700140401AS888   GAITRAGRPSFILTRAGRPF
086800060502** -MSG-                                                                     *
086900151106Immettere sia terminal di partenza che terminal di arrivo                      01
087000151106Terminal mancante o errato                                                     02
087100151106Combinazione temrinal partenza/arrivo già indicata                             03
087200151106Tipo servizio errato o già presente                                            04
087300151106La linea indicata non è un terminal di partenza                                05
087400151106La linea indicata non è un terminal di arrivo                                  06
