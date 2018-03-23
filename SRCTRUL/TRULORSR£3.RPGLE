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
004600140514     D  osio9libero           37    100
004700130911
004800140314     d DVPOOraris    e ds
004900140318     d
005000150505      //d TRULVPODS     e ds
005100150505      //d  fil_ab                16    765    dim(250)
005200130911      //---------------------------------------------------------------
005300130911      //?Definizione variabili globali.
005400130911      //---------------------------------------------------------------
005500130911
005600130911      // - Flags booleani
005700130911     d wFine           s               n   inz(*off)
005800140307     d wNOwin          s               n   inz(*off)
005900131108     d wEntrambi       s               n   inz(*off)
006000130925
006100130925      // - Campi di comodo
006200131015     d wMintrzs        s              2  0
006300131015     d wMinuti         s              5  0
006400131105     d wMinuti1        s              5  0
006500130925     d wOra            s              6  0
006600140303     d wOrap           s              6  0
006700130925     d wOraSys         s              6  0
006800130925     d wOraISO         s               t   timfmt(*iso)
006900131015     d wOraISO1        s               t   timfmt(*iso)
007000131010     d wOratrzs        s              2  0
007100140325     d wOUTmsg1        s             40
007200140325     d wOUTmsg2        s             40
007300131108     d wPOS            s              2  0
007400140314     d Indx            s              4  0
007500140314     d XX              s              4  0
007600140314     d YY              s              4  0
007700140314     d W003a           s              3
007800150505     d Wcarica         s              1
007900140307     d W_stfsw         s                   like(do0ostfs)
008000130911
008100130911      //---------------------------------------------------------------
008200130911      //?Definizione procedure esterne.
008300130911      //---------------------------------------------------------------
008400130911
008500130911      // - Ricerca orari di servizio
008600140109     d tisio9r         pr                  extpgm('TISIO9R')
008700130911     d  kpjba                              likeds(KPJBA)
008800140109     d  tisio9ds                           likeds(TISIO9DS)
008900140514     d  tisio9ds2                          likeds(TISIO9DS2) options(*nopass)
009000130911
009100130911      //---------------------------------------------------------------
009200130911      //?Definizione prototipi.
009300130911      //---------------------------------------------------------------
009400140314      /copy gaitrasrc/srcprotopr,tibs02r
009500130911
009600130911      //---------------------------------------------------------------
009700130911      //?Definizione key-list.
009800130911      //---------------------------------------------------------------
009900131108
010000131108      //---------------------------------------------------------------
010100131108      //?Definizione indicatori.
010200131108      //---------------------------------------------------------------
010300131108
010400131108      // 40 Bolla in consegna
010500131108      // 41 Riconsegna in giornata non possibile
010600140307      // 50 ORM per località in PIKUP 02
010700131108
010800130911      //---------------------------------------------------------------
010900130911      //?Main.
011000130911      //---------------------------------------------------------------
011100130911
011200130911     c     *Entry        plist
011300130911     c                   parm                    KPJBA
011400140109     c                   parm                    trulorsds
011500140307     c                   parm                    trulor2ds
011600140514     c                   parm                    trulor3ds
011700130911
011800130911      /free
011900130911
012000130911       //?Operazioni iniziali
012100130911       exsr RoutInz;
012200130911
012300130911       //?Elaboro
012400130911       DOW  wFine = *off;
012500130911         exsr Elabora;
012600130911       ENDDO;
012700130911
012800130911       //?Operazioni finali
012900130911       exsr RoutEnd;
013000130911
013100130911       //--------------------------------------------------------------
013200130911       //?Operazioni iniziali.
013300130911       //--------------------------------------------------------------
013400130911       BEGSR RoutInz;
013500130911
013600130911         clear OOREerr;
013700130911         clear OOREmsg;
013800140307         wNOwin = *off;
013900131108         wEntrambi = *off;
014000140307
014100140307       //?Controllo se passata anche la seconda DS
014200140307       //?in questo caso vuol dire che non devo aprire il file video
014300140307       //?perchè il pgm è stato richiamato SOLO per tornare i dati
014400140307         IF  %parms > 2;
014500140307           clear OOR2stis;
014600140307           clear OOR2stfs;
014700140307           clear OOR2miis;
014800140307           clear OOR2mxfs;
014900140307           clear OOR2rids;
015000140307           clear OOR2rias;
015100140310           clear OOR2fric ;
015200140307           clear OOR2presd ;
015300140307           clear OOR2presa ;
015400140310           clear OOR2lrsc;
015500140310           clear OOR2lrnc;
015600140310           clear OOR2lrnk;
015700140310           clear OOR2orif;
015800140310           clear OOR2err;
015900140307           clear OOR2msg;
016000140415           clear OOR2FCSGL;
016100140307           wNOwin = *on;
016200140514         IF  %parms > 3;
016300150505         // non pulisco più la localtà normalizzata: se passata significa che non la devo
016400150505         //  ricalcolare ma la tengo come buona
016500150505         //  clear OOR3loc_n ;
016600150505         //  clear OOR3norm ;
016700140514           clear OOR3libero ;
016800140307         ENDIF;
016900140514         ENDIF;
017000140307
017100140314       //?Carico filiale abilitate  per l'orario stimato da visualizzare
017200150505 1     //if fil_ab(1)='   ';
017300140318
017400150505       //  clear trulvpods  ;
017500150505       //  Ivpoke1='DECOFI816ORS'    ;
017600150505       //  TRULVPOR (trulvpods);
017700140314
017800150505 2     //  if fil_ab(1)='   ';
017900150505       //   eval fil_ab(1)='000' ;
018000150505 2     //  endif ;
018100140314
018200150505         if  wcarica=' ' ;
018300140314         // carico tabella variabili orari servizi
018400140314         clear TIBS02ds;
018500140314         T02Mod = 'C';
018600140314         T02sif = knsif;
018700140314         T02cod = 'VPO';
018800140314         t02tla = 'L'   ;
018900140314         T02ke1 ='ORARISER' ;
019000140314         TNTBE_RicercaControllo  (kpjba : tibs02ds);
019100140314
019200140314 2       if  T02err  = *blank;
019300140314          dvpoORARIS=t02uni  ;
019400140314 2       endif;
019500150505
019600150505         wcarica='S'  ;
019700140314 1       endif;
019800140314
019900130911       //?Se non ho la Linea di riferimento esco dal pgm con errore
020000130911         IF  IOREfil = *zeros;
020100130911           OOREerr = '1';
020200130911           OOREmsg = 'Manca la filiale di riferimento';
020300130911           wFine = *on;
020400130911           leavesr;
020500130911         ENDIF;
020600130911
020700130911       //?Se non ho il CAP esco dal pgm con errore
020800130911         IF  IOREcap = *blanks;
020900130911           OOREerr = '1';
021000130911           OOREmsg = 'Manca il CAP';
021100130911           wFine = *on;
021200130911           leavesr;
021300130911         ENDIF;
021400130911
021500130911       //?Se non ho la Località esco dal pgm con errore
021600130911         IF  IOREloc = *blanks;
021700130911           OOREerr = '1';
021800130911           OOREmsg = 'Manca la Località';
021900130911           wFine = *on;
022000130911           leavesr;
022100130911         ENDIF;
022200130911
022300130911       //?Se non ho la Data di riferimento imposto la data del giorno
022400130911         IF  IOREdta = *zeros;
022500130911           IOREdta = %dec(%date());
022600130911         ENDIF;
022700130911
022800130911       //?Se non ho il tipo servizio imposto Consegna
022900130911         IF  IOREtser = *blanks;
023000130911           IOREtser = 'C';
023100130911         ENDIF;
023200131108
023300131108       //?Se richiesto tipo servizio ENTRAMBI forzo tipo servizio CONSEGNA
023400131108         IF  IOREtser = 'E';
023500131108           IOREtser = 'C';
023600131108           wEntrambi = *on;
023700131108         ENDIF;
023800130912
023900131016       //?Se non ho riga posizionamento WIN imposto riga 13
024000131016       //?per CONSEGNE
024100140317         IF  (IORErig = *zeros or iorerig>11) and IOREtser = 'C';
024200140325           IORErig = 12;
024300130912         ENDIF;
024400131108       //?Se non ho riga posizionamento WIN imposto riga 13
024500131016       //?per RITIRI
024600140317         IF  (IORErig = *zeros or iorerig>10) and IOREtser = 'R';
024700140317           IORErig = 10;
024800131016         ENDIF;
024900131016       //?Se non ho riga posizionamento WIN imposto riga 09
025000131016       //?per ENTRAMBI
025100140317         IF  (IORErig = *zeros or iorerig>09) and IOREtser = 'E';
025200131016           IORErig = 09;
025300131016         ENDIF;
025400131016       //?Se non ho colonna posizionamento WIN imposto colonna 2
025500130912         IF  IOREcol = *zeros;
025600130912           IOREcol = 02;
025700130912         ENDIF;
025800130925
025900130925       //?Imposto ora del sistema
026000130925         wOraSys = %dec(%time());
026100130911
026200130911       ENDSR;
026300130911
026400130911       //--------------------------------------------------------------
026500130911       //?Elabora.
026600130911       //--------------------------------------------------------------
026700130911       BEGSR Elabora;
026800130911
026900130911         wFine = *on;
027000131024
027100140307         SELECT;
027200140307       //?Elaborazione con Window
027300140307           WHEN  not wNOwin;
027400140307             exsr  elaWIN;
027500140307       //?Elaborazione senza Window e ritorna solo i dati
027600140307           WHEN  wNOwin;
027700140307             exsr  elaNOWIN;
027800140307         ENDSL;
027900140307
028000131024
028100131024       ENDSR;
028200131024
028300131024       //--------------------------------------------------------------
028400131024       //?Elaborazione con WINDOW;
028500131024       //--------------------------------------------------------------
028600131024       BEGSR elaWIN;
028700131024
028800140306         IF  not %open(TRULORsD);
028900140306           open TRULORsD;
029000131024         ENDIF;
029100130911
029200131011         clear ULOREW01C;
029300140307         clear ULOREW03C;
029400131011         clear ULOREW01R;
029500130920         clear ULOREW02;
029600140307
029700140307
029800140307       //?Costante tipo servizio
029900140307         SELECT;
030000140307           WHEN  Wentrambi= *on;
030100140307             OREDTSERW = 'ENTRAMBI';
030200140307           WHEN  IOREtser = 'C';
030300140307             OREDTSERW = 'CONSEGNA';
030400140307           WHEN  IOREtser = 'R';
030500140307             OREDTSERW = 'RITIRI';
030600140307         ENDSL;
030700140307
030800140307         W1rig = IORErig;
030900140307         W1col = IOREcol;
031000140307
031100140307       //?chiamata al tisio9r
031200140307         exsr CallTISI  ;
031300130920
031400130920         IF  DO0err <> *blanks;
031500131017         //?Se tipo servizio ENTRAMBI emetto WIN con messaggio
031600131017         //?personalizzato
031700131108           IF  wEntrambi = *on;
031800131017             W02msg1 = 'Orari servizio non inseriti';
031900131017             exfmt  ULOREW02E;
032000131017             leavesr;
032100131017           ENDIF;
032200131017         //?Se tipo servizio CONSEGNA/RITIRI WIN con messaggio da TISI
032300140325           wOUTmsg1 = %subst(DO0msg:1:40);
032400140325           wOUTmsg2 = %subst(DO0msg:41:78-40);
032500140325           IF  %subst(wOUTmsg1:40:1) <> ' ' and wOUTmsg2 <> *blanks;
032600140325             FOR  wPOS by 1 to 40;
032700140325               IF  %subst(wOUTmsg1:40-wPOS:1) = ' ';
032800140325                 wPOS = 40 - wPOS;
032900131108                 leave;
033000131108               ENDIF;
033100131108             ENDFOR;
033200131108             W02msg1 = %subst(wOUTmsg1:1:wPOS-1);
033300140325             W02msg2 = %trim(%subst(wOUTmsg1:wPOS+1:40-wPOS)) +
033400131108                       %trim(wOUTmsg2);
033500131108           ELSE;
033600131108             W02msg1 = wOUTmsg1;
033700131108             W02msg2 = wOUTmsg2;
033800131108           ENDIF;
033900130920           exfmt  ULOREW02;
034000130920           leavesr;
034100130920         ENDIF;
034200130911
034300130927         OREdrifw = '(' + DO0rif + ')';
034400130927         IF  DO0rif <> 'L' and DO0rif <> 'C' and
034500130927             DO0rif <> 'F';
034600130927           OREdrifw = '(S)';
034700130927         ENDIF;
034800130927
034900131011         ORElrsc = DO0olrsc;
035000131011         ORElrnc = DO0olrnc;
035100130911
035200130911         OREstisw = DO0ostis;
035300140307         W_stfsw  = DO0ostfs;
035400140307         exsr SistOFin;
035500140307         OREstfsw = W_stfsw ;
035600140307
035700131016         OREmiisw = DO0omiis;
035800131016         OREmxfsw = DO0omxfs;
035900130925
036000130930       //?Imposto gli orari di Riconsegna in giornata
036100131024         exsr riconsegna;
036200140224
036300140303       //?Orario presunto ritiro/consegna
036400140314          exsr OrStimata ;
036500131016
036600131016       //?Se tipo servizio ENTRAMBI faccio secondo richiamo con 'R'
036700140307         IF  WEntrambi =*on  ;
036800131016           DO0tser = 'R';
036900140109           tisio9r (kpjba:tisio9ds);
037000131016           OREstiswR = DO0ostis;
037100131016           OREstfswR = DO0ostfs;
037200131016           ORElrsc = DO0olrsc;
037300131016           ORElrnc = DO0olrnc;
037400131016         ENDIF;
037500131108
037600131108       //?Se tipo servizio RITIRI controllo la PIKUP
037700140307         IF  IOREtser = 'R' and IOREgf2 <> '01' and
037800140307             IOREgf2 <> *blanks;
037900131108           *in50 = *on;
038000131108         ELSE;
038100131108           *in50 = *off;
038200131108         ENDIF;
038300130911
038400131016         SELECT;
038500131016           WHEN  IOREtser = 'C';
038600140310           //  if *in42 and (%subst(knmus:1:3)='EDP')   ;
038700140314             //if *in42 ;
038800140314
038900140307             exfmt  ULOREW03C;
039000140314
039100140314             //else  ;
039200140314
039300140314             if *in42 and not *in42  ;
039400140307             exfmt  ULOREW01C;
039500140307             endif  ;
039600140307
039700131016           WHEN  IOREtser = 'R';
039800140314
039900140314             exfmt  ULOREW03R;
040000140314
040100140314             if *in42 and not *in42  ;
040200140314             exfmt  ULOREW01R;
040300140314             endif ;
040400140314
040500131016           OTHER;
040600131016             exfmt  ULOREW01E;
040700131016         ENDSL;
040800130911
040900130911       ENDSR;
041000140307       //--------------------------------------------------------------
041100140307       //?Chiamata ai TISIO9R
041200140307       //--------------------------------------------------------------
041300140307         BEGSR  CallTISI  ;
041400140307
041500140307         clear TISIO9ds;
041600140307
041700140307         DO0dri = IOREdta;
041800140307         DO0lna = IOREfil;
041900140307         DO0cap = IOREcap;
042000140307         DO0loc = IOREloc;
042100140307         DO0nar = IOREnar;
042200140307         DO0tser = IOREtser;
042300140409         // if IOREcons='S'  ;
042400140526         // la data distinta la passo sempre perchè serve per determinare l'orario in soglia P o " "
042500140409         //  sulle bolle consegnate
042600140526          if iorendc>0   ;
042700140307          do0ddc=IOREddc ;
042800140526           endif  ;
042900140307         DO0tfp  = IOREtfp ;
043000140307         DO0tfa  = IOREtfa ;
043100140307         DO0dti  = IOREdti ;
043200140307         DO0hti  = IOREhti ;
043300140307         DO0dcr  = IOREdcr ;
043400140307         DO0hcr  = IOREhcr ;
043500140307         DO0tcr  = IOREtcr ;
043600140307         DO0dei  = IOREdei ;
043700140307         DO0oei  = IOREoei ;
043800140307
043900140307       //?Se tipo servizio ENTRAMBI faccio 1 richiamo con 'C'
044000140307         IF  IOREtser = 'E';
044100140307           DO0tser = 'C';
044200140307         ENDIF;
044300140307
044400140514         if  %parms>3 ;
044500140514         clear tisio9ds2    ;
044600150505         // se passata la località normalizzata non la faccio calcolare
044700150505         if oor3loc_n<>*blanks  ;
044800150505         osio9loc=oor3loc_n  ;
044900150505         osio9norm=oor3norm   ;
045000150505         endif  ;
045100150505
045200140514         tisio9r (kpjba:tisio9ds:tisio9ds2);
045300140514         else  ;
045400140514         tisio9r (kpjba:tisio9ds);
045500140514         endif  ;
045600140307
045700140307         ENDSR   ;
045800140307       //--------------------------------------------------------------
045900140307       //?sistema orario fine consegna
046000140307       //--------------------------------------------------------------
046100140307         BEGSR   SistOFin  ;
046200140307       //?Per consegne controllo anche il tipo servizio bolla
046300140307       //?in modo da impostare l'orario di fine servizio = al limite
046400140307       //?massimo del tipo servizio bolla 10:30 o 13:00
046500140307         IF  IOREtser <> 'R' and IOREtsp = 'H';
046600140307           W_stfsw = 1030;
046700140307         ENDIF;
046800140307         // imposto 13 solo se l'orario di presunta consegna è entro le 12,00
046900140307         IF  IOREtser <> 'R' and IOREtsp = 'E' and ioreoei<=1200 ;
047000140307           W_stfsw = 1300;
047100140307         ENDIF;
047200140307         ENDSR ;
047300140307
047400140307       //--------------------------------------------------------------
047500140307       //?calcolo ora stimata consegna DAL /AL
047600140307       //--------------------------------------------------------------
047700140307       BEGSR OrStimata  ;
047800140307         clear orepresd ;
047900140307         clear orepresa ;
048000140307         *in42 = *off;
048100140307
048200140307       // non presente se orario richiesto per entrambi
048300140307         IF  WEntrambi=*off  and ioreorp>0 ;
048400140307
048500140307         if  %dec(%subst(%editc(ioreorp:'X'):1:2):2:0) <=24 and
048600140307             %dec(%subst(%editc(ioreorp:'X'):3:2):2:0) >=00 and
048700140307             %dec(%subst(%editc(ioreorp:'X'):3:2):2:0) <=59    ;
048800140307           wora = ioreorp*100  ;
048900140307           woraiso = %time(wora) ;
049000140314           wOraISO = wOraISO - %minutes(§VPORST_M) ;
049100140307           Worap   = %dec(wOraISO);
049200140307           OREpresd= %dec(%subst(%editc(worap:'X'):1:4):4:0)  ;
049300140307           woraiso = %time(wora) ;
049400140314           wOraISO = wOraISO + %minutes(§VPORST_p) ;
049500140307           worap   = %dec(wOraISO);
049600140307           OREpresa= %dec(%subst(%editc(worap:'X'):1:4):4:0)  ;
049700140307           *in42=*on  ;
049800140307         endif  ;
049900140307         endif  ;
050000140314
050100140307         ENDSR ;
050200140307       //--------------------------------------------------------------
050300140307       //?Elaborazione senza WINDOW;
050400140307       //--------------------------------------------------------------
050500140307       BEGSR elaNOWIN;
050600140307
050700140307       //?chiamata al tisio9r
050800140307         exsr CallTISI  ;
050900140307
051000140307         IF  DO0err <> *blanks;
051100140307           OOR2err = '1';
051200140307           OOR2msg = 'Orari servizio non inseriti';
051300140307           leavesr;
051400140307         ENDIF;
051500140307
051600140307         OOR2stis = DO0ostis;
051700140307         OOR2stfs = DO0ostfs;
051800140307         W_stfsw  = DO0ostfs;
051900140307         exsr SistOFin;
052000140307         OOR2stfs = W_stfsw ;
052100140307         OOR2miis = DO0omiis;
052200140307         OOR2mxfs = DO0omxfs;
052300140307         OOR2orif = DO0rif  ;
052400140307         OOR2lrsc = DO0olrsc;
052500140307         OOR2lrnc = DO0olrnc;
052600140307         OOR2lrnk = DO0olrnk;
052700140415         OOR2fcsgl= DO0fcsgl;
052800140307
052900140307       //?Calcolo gli orari di Riconsegna in giornata
053000140307         exsr riconsegna;
053100140307
053200140310       //?Se riconsegna non possibile passo flag
053300140310         IF  *in41 or not *in40 ;
053400140307           OOR2FRIC='N'   ;
053500140307           else  ;
053600140307           OOR2FRIC='S'   ;
053700140307         ENDIF;
053800140307
053900140307       //?Ritorno i dati della riconsegna in giornata
054000140307         OOR2rids = OREda;
054100140307         OOR2rias = OREa;
054200140307
054300140307       //?Orario presunto ritiro/consegna
054400140307       exsr OrStimata ;
054500140307
054600140307       OOR2presd=OREpresd    ;
054700140307       OOR2presa=OREpresa    ;
054800140514
054900140514       //?Se passata la terza DS restituisco i dati della località normalizzata
055000140514       if %parms>3  ;
055100140514       OOR3loc_n=osio9loc    ;
055200140514       OOR3norm =osio9norm   ;
055300140514       endif  ;
055400140307
055500140307       ENDSR;
055600131024
055700131024       //--------------------------------------------------------------
055800131024       //?Calcolo la riconsegna.
055900131024       //--------------------------------------------------------------
056000131024       BEGSR riconsegna;
056100131024
056200131024         *in40 = *off;
056300131024         *in41 = *off;
056400140307         clear oreDA  ;
056500140307         clear oreA   ;
056600131024
056700131024         wOratrzs = %dec(%subst(%editc(DO0trzs:'X'):1:2):2:0);
056800131024         wMintrzs = %dec(%subst(%editc(DO0trzs:'X'):3:2):2:0);
056900131024
057000131024         IF  IOREtser = 'C' and IOREcons = 'S';
057100131024           *in40 = *on;
057200131024           wOraISO = %time();
057300131024           wOraISO = wOraISO + %hours(wOratrzs);
057400131105           wOraISO = wOraISO + %minutes(wMintrzs);
057500131024           wOra = %dec(wOraISO);
057600131024
057700131024         //?Imposto ora dalle
057800131024           SELECT;
057900131024             WHEN  wOra < wOraSys;
058000131024               OREda = DO0ostis;
058100131024             WHEN  wOra > DO0ostfs * 100;
058200131024               *in41 = *on;
058300131024             OTHER;
058400131024               OREda = wOra / 100;
058500131024           ENDSL;
058600131024
058700131024         //?Se posso fare la riconsegna
058800131024           IF  not *in41;
058900131105           //?arrotondo ai 5 minuti
059000131105             wOra = OREda * 100;
059100131105             wOraISO = %time(wOra);
059200131105             wMinuti = %dec(%subst(%editc(OREda:'X'):4:1):1:0);
059300131105             IF  wMinuti < 5;
059400131105               wMinuti1 = 5 - wMinuti;
059500131105               wOraISO = wOraISO + %minutes(wMinuti1);
059600131024             ENDIF;
059700131105             IF  wMinuti > 5;
059800131105               wMinuti1 = 10 - wMinuti;
059900131105               wOraISO = wOraISO + %minutes(wMinuti1);
060000131024             ENDIF;
060100131105             wOra = %dec(wOraISO);
060200131105             OREDA = wOra / 100;
060300131024           //?Imposto ora alle
060400131024             OREa = DO0ostfs;
060500131024           //?Se tra l'ora dalle e l'ora alle c'è meno di 30 minuti
060600131024           //?non posso fare la riconsegna
060700131024             wOra = OREda * 100;
060800131024             wOraISO = %time(wOra);
060900131024             wOra = OREa * 100;
061000131024             wOraISO1 = %time(wOra);
061100131024             wMinuti = %diff(wOraISO1 : wOraISo : *minutes);
061200131024             IF  wMinuti < 30;
061300131024               *in41 = *on;
061400131024             ENDIF;
061500131024           ENDIF;
061600131024         ENDIF;
061700131024
061800131024       ENDSR;
061900130911
062000130911       //--------------------------------------------------------------
062100130911       //?Operazioni finali.
062200130911       //--------------------------------------------------------------
062300130911       BEGSR RoutEnd;
062400131024
062500140306         IF  %open(TRULORsD);
062600140306           close TRULORsD;
062700131024         ENDIF;
062800130911
062900130911         *inLR = *on;
063000130911         return;
063100130911
063200130911       ENDSR;
063300130911
063400130911      /end-free
