000100130911      //---------------------------------------------------------------
000200140227      //?TRULORSR - Visualizza Orari di Servizio
000300130911      //---------------------------------------------------------------
000400130911     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000500130911     h dftactgrp(*no) actgrp(*caller)
000600130911
000700130911      //---------------------------------------------------------------
000800130911      //?Dichiarazione file.
000900130911      //---------------------------------------------------------------
001000130911
001100130911       // -?File video
001200140306     fTRULORSD  cf   e             workstn usropn
001300140314     ftntbe01l  if   e           k disk
001400130911
001500130911      //---------------------------------------------------------------
001600130911      //?Definizione costanti.
001700130911      //---------------------------------------------------------------
001800130911
001900130911      //---------------------------------------------------------------
002000130911      //?Definizione schiere.
002100130911      //---------------------------------------------------------------
002200130911
002300130911      //---------------------------------------------------------------
002400130911      //?Definizione aree dati.
002500130911      //---------------------------------------------------------------
002600130911
002700130911      //---------------------------------------------------------------
002800130911      //?Definizione strutture dati.
002900130911      //---------------------------------------------------------------
003000130911
003100130911      // - Parametri ricevuti
003200130911     d KPJBA         e ds
003300140109     d TRULORSds     e ds
003400140307     d TRULOR2ds     e ds
003500140514     d TRULOR3ds     e ds
003600140314     d
003700140314     d TIBS02DS      e ds
003800130911
003900130911      // - Ricerca orari di servizio
004000140109     d TISIO9ds      e ds
004100140514     d
004200140514     d* Altri dati :   località normalizzata
004300140514     d tisio9ds2       ds
004400140514     D  osio9loc               1     35
004500140514     D  osio9norm             36     36
004600150728     c* ore teoriche trazine cliente e ore teoriche consegna cliente
004700150728     D  osio9trazC            37     39
004800150728     D  osio9consC            40     42
004900150728     D  osio9TR_TC            37     42
005000150728     D  osio9libero           43    100
005100130911
005200140314     d DVPOOraris    e ds
005300140318     d
005400150723     d Diore01       e ds
005500150505      //d TRULVPODS     e ds
005600150505      //d  fil_ab                16    765    dim(250)
005700130911      //---------------------------------------------------------------
005800130911      //?Definizione variabili globali.
005900130911      //---------------------------------------------------------------
006000130911
006100130911      // - Flags booleani
006200130911     d wFine           s               n   inz(*off)
006300140307     d wNOwin          s               n   inz(*off)
006400131108     d wEntrambi       s               n   inz(*off)
006500130925
006600130925      // - Campi di comodo
006700131015     d wMintrzs        s              2  0
006800131015     d wMinuti         s              5  0
006900131105     d wMinuti1        s              5  0
007000130925     d wOra            s              6  0
007100140303     d wOrap           s              6  0
007200130925     d wOraSys         s              6  0
007300130925     d wOraISO         s               t   timfmt(*iso)
007400131015     d wOraISO1        s               t   timfmt(*iso)
007500131010     d wOratrzs        s              2  0
007600140325     d wOUTmsg1        s             40
007700140325     d wOUTmsg2        s             40
007800131108     d wPOS            s              2  0
007900140314     d Indx            s              4  0
008000140314     d XX              s              4  0
008100140314     d YY              s              4  0
008200140314     d W003a           s              3
008300150505     d Wcarica         s              1
008400140307     d W_stfsw         s                   like(do0ostfs)
008500150714     d W_stisw         s                   like(do0ostis)
008600130911
008700130911      //---------------------------------------------------------------
008800130911      //?Definizione procedure esterne.
008900130911      //---------------------------------------------------------------
009000130911
009100130911      // - Ricerca orari di servizio
009200140109     d tisio9r         pr                  extpgm('TISIO9R')
009300130911     d  kpjba                              likeds(KPJBA)
009400140109     d  tisio9ds                           likeds(TISIO9DS)
009500140514     d  tisio9ds2                          likeds(TISIO9DS2) options(*nopass)
009600130911
009700130911      //---------------------------------------------------------------
009800130911      //?Definizione prototipi.
009900130911      //---------------------------------------------------------------
010000140314      /copy gaitrasrc/srcprotopr,tibs02r
010100130911
010200130911      //---------------------------------------------------------------
010300130911      //?Definizione key-list.
010400130911      //---------------------------------------------------------------
010500131108
010600131108      //---------------------------------------------------------------
010700131108      //?Definizione indicatori.
010800131108      //---------------------------------------------------------------
010900131108
011000131108      // 40 Bolla in consegna
011100131108      // 41 Riconsegna in giornata non possibile
011200140307      // 50 ORM per località in PIKUP 02
011300131108
011400130911      //---------------------------------------------------------------
011500130911      //?Main.
011600130911      //---------------------------------------------------------------
011700130911
011800130911     c     *Entry        plist
011900130911     c                   parm                    KPJBA
012000140109     c                   parm                    trulorsds
012100140307     c                   parm                    trulor2ds
012200140514     c                   parm                    trulor3ds
012300130911
012400130911      /free
012500130911
012600130911       //?Operazioni iniziali
012700130911       exsr RoutInz;
012800130911
012900130911       //?Elaboro
013000130911       DOW  wFine = *off;
013100130911         exsr Elabora;
013200130911       ENDDO;
013300130911
013400130911       //?Operazioni finali
013500130911       exsr RoutEnd;
013600130911
013700130911       //--------------------------------------------------------------
013800130911       //?Operazioni iniziali.
013900130911       //--------------------------------------------------------------
014000130911       BEGSR RoutInz;
014100130911
014200130911         clear OOREerr;
014300130911         clear OOREmsg;
014400140307         wNOwin = *off;
014500131108         wEntrambi = *off;
014600140307
014700140307       //?Controllo se passata anche la seconda DS
014800140307       //?in questo caso vuol dire che non devo aprire il file video
014900140307       //?perchè il pgm è stato richiamato SOLO per tornare i dati
015000140307         IF  %parms > 2;
015100140307           clear OOR2stis;
015200140307           clear OOR2stfs;
015300140307           clear OOR2miis;
015400140307           clear OOR2mxfs;
015500140307           clear OOR2rids;
015600140307           clear OOR2rias;
015700140310           clear OOR2fric ;
015800140307           clear OOR2presd ;
015900140307           clear OOR2presa ;
016000140310           clear OOR2lrsc;
016100140310           clear OOR2lrnc;
016200140310           clear OOR2lrnk;
016300140310           clear OOR2orif;
016400140310           clear OOR2err;
016500140307           clear OOR2msg;
016600140415           clear OOR2FCSGL;
016700140307           wNOwin = *on;
016800140514         IF  %parms > 3;
016900150505         // non pulisco più la localtà normalizzata: se passata significa che non la devo
017000150505         //  ricalcolare ma la tengo come buona
017100150505         //  clear OOR3loc_n ;
017200150505         //  clear OOR3norm ;
017300140514           clear OOR3libero ;
017400140307         ENDIF;
017500140514         ENDIF;
017600140307
017700140314       //?Carico filiale abilitate  per l'orario stimato da visualizzare
017800150505 1     //if fil_ab(1)='   ';
017900140318
018000150505       //  clear trulvpods  ;
018100150505       //  Ivpoke1='DECOFI816ORS'    ;
018200150505       //  TRULVPOR (trulvpods);
018300140314
018400150505 2     //  if fil_ab(1)='   ';
018500150505       //   eval fil_ab(1)='000' ;
018600150505 2     //  endif ;
018700140314
018800150505         if  wcarica=' ' ;
018900140314         // carico tabella variabili orari servizi
019000140314         clear TIBS02ds;
019100140314         T02Mod = 'C';
019200140314         T02sif = knsif;
019300140314         T02cod = 'VPO';
019400140314         t02tla = 'L'   ;
019500140314         T02ke1 ='ORARISER' ;
019600140314         TNTBE_RicercaControllo  (kpjba : tibs02ds);
019700140314
019800140314 2       if  T02err  = *blank;
019900140314          dvpoORARIS=t02uni  ;
020000140314 2       endif;
020100150505
020200150505         wcarica='S'  ;
020300140314 1       endif;
020400140314
020500130911       //?Se non ho la Linea di riferimento esco dal pgm con errore
020600130911         IF  IOREfil = *zeros;
020700130911           OOREerr = '1';
020800130911           OOREmsg = 'Manca la filiale di riferimento';
020900130911           wFine = *on;
021000130911           leavesr;
021100130911         ENDIF;
021200130911
021300130911       //?Se non ho il CAP esco dal pgm con errore
021400130911         IF  IOREcap = *blanks;
021500130911           OOREerr = '1';
021600130911           OOREmsg = 'Manca il CAP';
021700130911           wFine = *on;
021800130911           leavesr;
021900130911         ENDIF;
022000130911
022100130911       //?Se non ho la Località esco dal pgm con errore
022200130911         IF  IOREloc = *blanks;
022300130911           OOREerr = '1';
022400130911           OOREmsg = 'Manca la Località';
022500130911           wFine = *on;
022600130911           leavesr;
022700130911         ENDIF;
022800130911
022900130911       //?Se non ho la Data di riferimento imposto la data del giorno
023000130911         IF  IOREdta = *zeros;
023100130911           IOREdta = %dec(%date());
023200130911         ENDIF;
023300130911
023400130911       //?Se non ho il tipo servizio imposto Consegna
023500130911         IF  IOREtser = *blanks;
023600130911           IOREtser = 'C';
023700130911         ENDIF;
023800131108
023900131108       //?Se richiesto tipo servizio ENTRAMBI forzo tipo servizio CONSEGNA
024000131108         IF  IOREtser = 'E';
024100131108           IOREtser = 'C';
024200131108           wEntrambi = *on;
024300131108         ENDIF;
024400150723
024500150723       //?Se richiesto tipo servizio ENTRAMBI forzo tipo servizio CONSEGNA
024600150723             diore01  = ioreflo   ;
024700130912
024800131016       //?Se non ho riga posizionamento WIN imposto riga 13
024900131016       //?per CONSEGNE
025000140317         IF  (IORErig = *zeros or iorerig>11) and IOREtser = 'C';
025100140325           IORErig = 12;
025200130912         ENDIF;
025300131108       //?Se non ho riga posizionamento WIN imposto riga 13
025400131016       //?per RITIRI
025500140317         IF  (IORErig = *zeros or iorerig>10) and IOREtser = 'R';
025600140317           IORErig = 10;
025700131016         ENDIF;
025800131016       //?Se non ho riga posizionamento WIN imposto riga 09
025900131016       //?per ENTRAMBI
026000140317         IF  (IORErig = *zeros or iorerig>09) and IOREtser = 'E';
026100131016           IORErig = 09;
026200131016         ENDIF;
026300131016       //?Se non ho colonna posizionamento WIN imposto colonna 2
026400130912         IF  IOREcol = *zeros;
026500130912           IOREcol = 02;
026600130912         ENDIF;
026700130925
026800130925       //?Imposto ora del sistema
026900130925         wOraSys = %dec(%time());
027000130911
027100130911       ENDSR;
027200130911
027300130911       //--------------------------------------------------------------
027400130911       //?Elabora.
027500130911       //--------------------------------------------------------------
027600130911       BEGSR Elabora;
027700130911
027800130911         wFine = *on;
027900131024
028000140307         SELECT;
028100140307       //?Elaborazione con Window
028200140307           WHEN  not wNOwin;
028300140307             exsr  elaWIN;
028400140307       //?Elaborazione senza Window e ritorna solo i dati
028500140307           WHEN  wNOwin;
028600140307             exsr  elaNOWIN;
028700140307         ENDSL;
028800140307
028900131024
029000131024       ENDSR;
029100131024
029200131024       //--------------------------------------------------------------
029300131024       //?Elaborazione con WINDOW;
029400131024       //--------------------------------------------------------------
029500131024       BEGSR elaWIN;
029600131024
029700140306         IF  not %open(TRULORsD);
029800140306           open TRULORsD;
029900131024         ENDIF;
030000130911
030100131011         clear ULOREW01C;
030200140307         clear ULOREW03C;
030300131011         clear ULOREW01R;
030400130920         clear ULOREW02;
030500140307
030600140307
030700140307       //?Costante tipo servizio
030800140307         SELECT;
030900140307           WHEN  Wentrambi= *on;
031000140307             OREDTSERW = 'ENTRAMBI';
031100140307           WHEN  IOREtser = 'C';
031200140307             OREDTSERW = 'CONSEGNA';
031300140307           WHEN  IOREtser = 'R';
031400140307             OREDTSERW = 'RITIRI';
031500140307         ENDSL;
031600140307
031700140307         W1rig = IORErig;
031800140307         W1col = IOREcol;
031900140307
032000140307       //?chiamata al tisio9r
032100140307         exsr CallTISI  ;
032200130920
032300130920         IF  DO0err <> *blanks;
032400131017         //?Se tipo servizio ENTRAMBI emetto WIN con messaggio
032500131017         //?personalizzato
032600131108           IF  wEntrambi = *on;
032700131017             W02msg1 = 'Orari servizio non inseriti';
032800131017             exfmt  ULOREW02E;
032900131017             leavesr;
033000131017           ENDIF;
033100131017         //?Se tipo servizio CONSEGNA/RITIRI WIN con messaggio da TISI
033200140325           wOUTmsg1 = %subst(DO0msg:1:40);
033300140325           wOUTmsg2 = %subst(DO0msg:41:78-40);
033400140325           IF  %subst(wOUTmsg1:40:1) <> ' ' and wOUTmsg2 <> *blanks;
033500140325             FOR  wPOS by 1 to 40;
033600140325               IF  %subst(wOUTmsg1:40-wPOS:1) = ' ';
033700140325                 wPOS = 40 - wPOS;
033800131108                 leave;
033900131108               ENDIF;
034000131108             ENDFOR;
034100131108             W02msg1 = %subst(wOUTmsg1:1:wPOS-1);
034200140325             W02msg2 = %trim(%subst(wOUTmsg1:wPOS+1:40-wPOS)) +
034300131108                       %trim(wOUTmsg2);
034400131108           ELSE;
034500131108             W02msg1 = wOUTmsg1;
034600131108             W02msg2 = wOUTmsg2;
034700131108           ENDIF;
034800130920           exfmt  ULOREW02;
034900130920           leavesr;
035000130920         ENDIF;
035100130911
035200130927         OREdrifw = '(' + DO0rif + ')';
035300130927         IF  DO0rif <> 'L' and DO0rif <> 'C' and
035400130927             DO0rif <> 'F';
035500130927           OREdrifw = '(S)';
035600130927         ENDIF;
035700130927
035800131011         ORElrsc = DO0olrsc;
035900131011         ORElrnc = DO0olrnc;
036000130911
036100130911         OREstisw = DO0ostis;
036200140307         W_stfsw  = DO0ostfs;
036300150714         W_stisw  = DO0ostis;
036400140307         exsr SistOFin;
036500140307         OREstfsw = W_stfsw ;
036600161026         // in caso di ritiri, se richiesto, visualizzo min e max
036700161026         IF  WEntrambi =*off
036800161026            and §IORER_MNX='S' ;
036900161026          OREstisw = DO0omiis;
037000161026          OREstfsw = DO0omxfs;
037100161026         endif  ;
037200140307
037300131016         OREmiisw = DO0omiis;
037400131016         OREmxfsw = DO0omxfs;
037500130925
037600130930       //?Imposto gli orari di Riconsegna in giornata
037700131024         exsr riconsegna;
037800140224
037900140303       //?Orario presunto ritiro/consegna
038000140314          exsr OrStimata ;
038100131016
038200131016       //?Se tipo servizio ENTRAMBI faccio secondo richiamo con 'R'
038300140307         IF  WEntrambi =*on  ;
038400131016           DO0tser = 'R';
038500140109           tisio9r (kpjba:tisio9ds);
038600131016           OREstiswR = DO0ostis;
038700131016           OREstfswR = DO0ostfs;
038800131016           ORElrsc = DO0olrsc;
038900131016           ORElrnc = DO0olrnc;
039000161026            // if  §IORER_MNX='S' ;
039100161026           // OREstiswR = DO0omiis;
039200161026           // OREstfswR = DO0omxfs;
039300161026           // endif  ;
039400131016         ENDIF;
039500131108
039600131108       //?Se tipo servizio RITIRI controllo la PIKUP
039700140307         IF  IOREtser = 'R' and IOREgf2 <> '01' and
039800140307             IOREgf2 <> *blanks;
039900131108           *in50 = *on;
040000131108         ELSE;
040100131108           *in50 = *off;
040200131108         ENDIF;
040300130911
040400131016         SELECT;
040500161026           WHEN  IOREtser = 'C' and wentrambi=*off ;
040600140310           //  if *in42 and (%subst(knmus:1:3)='EDP')   ;
040700140314             //if *in42 ;
040800140314
040900140307             exfmt  ULOREW03C;
041000140314
041100140314             //else  ;
041200140314
041300140314             if *in42 and not *in42  ;
041400140307             exfmt  ULOREW01C;
041500140307             endif  ;
041600140307
041700131016           WHEN  IOREtser = 'R';
041800140314
041900140314             exfmt  ULOREW03R;
042000140314
042100140314             if *in42 and not *in42  ;
042200140314             exfmt  ULOREW01R;
042300140314             endif ;
042400140314
042500131016           OTHER;
042600131016             exfmt  ULOREW01E;
042700131016         ENDSL;
042800130911
042900130911       ENDSR;
043000140307       //--------------------------------------------------------------
043100140307       //?Chiamata ai TISIO9R
043200140307       //--------------------------------------------------------------
043300140307         BEGSR  CallTISI  ;
043400140307
043500140307         clear TISIO9ds;
043600140307
043700140307         DO0dri = IOREdta;
043800140307         DO0lna = IOREfil;
043900140307         DO0cap = IOREcap;
044000140307         DO0loc = IOREloc;
044100140307         DO0nar = IOREnar;
044200140307         DO0tser = IOREtser;
044300140409         // if IOREcons='S'  ;
044400140526         // la data distinta la passo sempre perchè serve per determinare l'orario in soglia P o " "
044500140409         //  sulle bolle consegnate
044600140526          if iorendc>0   ;
044700140307          do0ddc=IOREddc ;
044800140526           endif  ;
044900140307         DO0tfp  = IOREtfp ;
045000140307         DO0tfa  = IOREtfa ;
045100140307         DO0dti  = IOREdti ;
045200140307         DO0hti  = IOREhti ;
045300140307         DO0dcr  = IOREdcr ;
045400140307         DO0hcr  = IOREhcr ;
045500140307         DO0tcr  = IOREtcr ;
045600140307         DO0dei  = IOREdei ;
045700140307         DO0oei  = IOREoei ;
045800140307
045900140307       //?Se tipo servizio ENTRAMBI faccio 1 richiamo con 'C'
046000140307         IF  IOREtser = 'E';
046100140307           DO0tser = 'C';
046200140307         ENDIF;
046300140307
046400140514         clear tisio9ds2    ;
046500150723         if  %parms>3 ;
046600150505         // se passata la località normalizzata non la faccio calcolare
046700150505         if oor3loc_n<>*blanks  ;
046800150505         osio9loc=oor3loc_n  ;
046900150505         osio9norm=oor3norm   ;
047000150723         endif  ;
047100150728         if §ioretrazC>*zeros   ;
047200150728         osio9trazC=§ioretrazC   ;
047300150728         if §ioreconsC>*zeros   ;
047400150728         osio9consC=§ioreconsC   ;
047500150728         endif  ;
047600150728         endif  ;
047700150723         endif  ;
047800150728
047900150728         if §ioretrazC>*zeros and %parms<=3 ;
048000150728         osio9trazC=§ioretrazC   ;
048100150728         if §ioreconsC>*zeros  ;
048200150728         osio9consC=§ioreconsC   ;
048300150728         endif  ;
048400150728         endif  ;
048500150723
048600150505
048700150728         if §ioretrazC>*zeros or  %parms> 3 ;
048800140514         tisio9r (kpjba:tisio9ds:tisio9ds2);
048900140514         else  ;
049000140514         tisio9r (kpjba:tisio9ds);
049100140514         endif  ;
049200140307
049300140307         ENDSR   ;
049400140307       //--------------------------------------------------------------
049500140307       //?sistema orario fine consegna
049600140307       //--------------------------------------------------------------
049700140307         BEGSR   SistOFin  ;
049800140307       //?Per consegne controllo anche il tipo servizio bolla
049900140307       //?in modo da impostare l'orario di fine servizio = al limite
050000140307       //?massimo del tipo servizio bolla 10:30 o 13:00
050100140307         IF  IOREtser <> 'R' and IOREtsp = 'H';
050200140307           W_stfsw = 1030;
050300140307         ENDIF;
050400150714
050500150714         // Imposto 13 solo se l'orario di presunta consegna è entro le 12,00
050600150714         //  per evitare errori di visualizzazione in caso di orari diversi tra interni ed esterni,
050700150714         //  controllo comunque che non sia < dell'orario DAL
050800150714         IF  IOREtser <> 'R' and IOREtsp = 'E' and ioreoei<=1200
050900150714             and w_stisw<=1230  ;
051000140307           W_stfsw = 1300;
051100140307         ENDIF;
051200140307         ENDSR ;
051300140307
051400140307       //--------------------------------------------------------------
051500140307       //?calcolo ora stimata consegna DAL /AL
051600140307       //--------------------------------------------------------------
051700140307       BEGSR OrStimata  ;
051800140307         clear orepresd ;
051900140307         clear orepresa ;
052000140307         *in42 = *off;
052100140307
052200140307       // non presente se orario richiesto per entrambi
052300140307         IF  WEntrambi=*off  and ioreorp>0 ;
052400140307
052500140307         if  %dec(%subst(%editc(ioreorp:'X'):1:2):2:0) <=24 and
052600140307             %dec(%subst(%editc(ioreorp:'X'):3:2):2:0) >=00 and
052700140307             %dec(%subst(%editc(ioreorp:'X'):3:2):2:0) <=59    ;
052800140307           wora = ioreorp*100  ;
052900140307           woraiso = %time(wora) ;
053000140314           wOraISO = wOraISO - %minutes(§VPORST_M) ;
053100140307           Worap   = %dec(wOraISO);
053200140307           OREpresd= %dec(%subst(%editc(worap:'X'):1:4):4:0)  ;
053300140307           woraiso = %time(wora) ;
053400140314           wOraISO = wOraISO + %minutes(§VPORST_p) ;
053500140307           worap   = %dec(wOraISO);
053600140307           OREpresa= %dec(%subst(%editc(worap:'X'):1:4):4:0)  ;
053700140307           *in42=*on  ;
053800140307         endif  ;
053900140307         endif  ;
054000140314
054100140307         ENDSR ;
054200140307       //--------------------------------------------------------------
054300140307       //?Elaborazione senza WINDOW;
054400140307       //--------------------------------------------------------------
054500140307       BEGSR elaNOWIN;
054600140307
054700140307       //?chiamata al tisio9r
054800140307         exsr CallTISI  ;
054900140307
055000140307         IF  DO0err <> *blanks;
055100140307           OOR2err = '1';
055200140307           OOR2msg = 'Orari servizio non inseriti';
055300140307           leavesr;
055400140307         ENDIF;
055500140307
055600140307         OOR2stis = DO0ostis;
055700140307         OOR2stfs = DO0ostfs;
055800150714         W_stisw  = DO0ostis;
055900140307         W_stfsw  = DO0ostfs;
056000140307         exsr SistOFin;
056100140307         OOR2stfs = W_stfsw ;
056200140307         OOR2miis = DO0omiis;
056300140307         OOR2mxfs = DO0omxfs;
056400140307         OOR2orif = DO0rif  ;
056500140307         OOR2lrsc = DO0olrsc;
056600140307         OOR2lrnc = DO0olrnc;
056700140307         OOR2lrnk = DO0olrnk;
056800140415         OOR2fcsgl= DO0fcsgl;
056900140307
057000140307       //?Calcolo gli orari di Riconsegna in giornata
057100140307         exsr riconsegna;
057200140307
057300140310       //?Se riconsegna non possibile passo flag
057400140310         IF  *in41 or not *in40 ;
057500140307           OOR2FRIC='N'   ;
057600140307           else  ;
057700140307           OOR2FRIC='S'   ;
057800140307         ENDIF;
057900140307
058000140307       //?Ritorno i dati della riconsegna in giornata
058100140307         OOR2rids = OREda;
058200140307         OOR2rias = OREa;
058300140307
058400140307       //?Orario presunto ritiro/consegna
058500140307       exsr OrStimata ;
058600140307
058700140307       OOR2presd=OREpresd    ;
058800140307       OOR2presa=OREpresa    ;
058900140514
059000140514       //?Se passata la terza DS restituisco i dati della località normalizzata
059100140514       if %parms>3  ;
059200140514       OOR3loc_n=osio9loc    ;
059300140514       OOR3norm =osio9norm   ;
059400140514       endif  ;
059500140307
059600140307       ENDSR;
059700131024
059800131024       //--------------------------------------------------------------
059900131024       //?Calcolo la riconsegna.
060000131024       //--------------------------------------------------------------
060100131024       BEGSR riconsegna;
060200131024
060300131024         *in40 = *off;
060400131024         *in41 = *off;
060500140307         clear oreDA  ;
060600140307         clear oreA   ;
060700131024
060800131024         wOratrzs = %dec(%subst(%editc(DO0trzs:'X'):1:2):2:0);
060900131024         wMintrzs = %dec(%subst(%editc(DO0trzs:'X'):3:2):2:0);
061000131024
061100131024         IF  IOREtser = 'C' and IOREcons = 'S';
061200131024           *in40 = *on;
061300131024           wOraISO = %time();
061400131024           wOraISO = wOraISO + %hours(wOratrzs);
061500131105           wOraISO = wOraISO + %minutes(wMintrzs);
061600131024           wOra = %dec(wOraISO);
061700131024
061800131024         //?Imposto ora dalle
061900131024           SELECT;
062000131024             WHEN  wOra < wOraSys;
062100131024               OREda = DO0ostis;
062200131024             WHEN  wOra > DO0ostfs * 100;
062300131024               *in41 = *on;
062400131024             OTHER;
062500131024               OREda = wOra / 100;
062600131024           ENDSL;
062700131024
062800131024         //?Se posso fare la riconsegna
062900131024           IF  not *in41;
063000131105           //?arrotondo ai 5 minuti
063100131105             wOra = OREda * 100;
063200131105             wOraISO = %time(wOra);
063300131105             wMinuti = %dec(%subst(%editc(OREda:'X'):4:1):1:0);
063400131105             IF  wMinuti < 5;
063500131105               wMinuti1 = 5 - wMinuti;
063600131105               wOraISO = wOraISO + %minutes(wMinuti1);
063700131024             ENDIF;
063800131105             IF  wMinuti > 5;
063900131105               wMinuti1 = 10 - wMinuti;
064000131105               wOraISO = wOraISO + %minutes(wMinuti1);
064100131024             ENDIF;
064200131105             wOra = %dec(wOraISO);
064300131105             OREDA = wOra / 100;
064400131024           //?Imposto ora alle
064500131024             OREa = DO0ostfs;
064600131024           //?Se tra l'ora dalle e l'ora alle c'è meno di 30 minuti
064700131024           //?non posso fare la riconsegna
064800131024             wOra = OREda * 100;
064900131024             wOraISO = %time(wOra);
065000131024             wOra = OREa * 100;
065100131024             wOraISO1 = %time(wOra);
065200131024             wMinuti = %diff(wOraISO1 : wOraISo : *minutes);
065300131024             IF  wMinuti < 30;
065400131024               *in41 = *on;
065500131024             ENDIF;
065600131024           ENDIF;
065700131024         ENDIF;
065800131024
065900131024       ENDSR;
066000130911
066100130911       //--------------------------------------------------------------
066200130911       //?Operazioni finali.
066300130911       //--------------------------------------------------------------
066400130911       BEGSR RoutEnd;
066500131024
066600140306         IF  %open(TRULORsD);
066700140306           close TRULORsD;
066800131024         ENDIF;
066900130911
067000130911         *inLR = *on;
067100130911         return;
067200130911
067300130911       ENDSR;
067400130911
067500130911      /end-free
