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
005000140318     d TRULVPODS     e ds
005100140319     d  fil_ab                16    765    dim(250)
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
007800140307     d W_stfsw         s                   like(do0ostfs)
007900130911
008000130911      //---------------------------------------------------------------
008100130911      //?Definizione procedure esterne.
008200130911      //---------------------------------------------------------------
008300130911
008400130911      // - Ricerca orari di servizio
008500140109     d tisio9r         pr                  extpgm('TISIO9R')
008600130911     d  kpjba                              likeds(KPJBA)
008700140109     d  tisio9ds                           likeds(TISIO9DS)
008800140514     d  tisio9ds2                          likeds(TISIO9DS2) options(*nopass)
008900130911
009000130911      //---------------------------------------------------------------
009100130911      //?Definizione prototipi.
009200130911      //---------------------------------------------------------------
009300140314      /copy gaitrasrc/srcprotopr,tibs02r
009400140318      /copy gaitrasrc/srcprotopr,trulvpor
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
016300140514           clear OOR3loc_n ;
016400140514           clear OOR3norm ;
016500140514           clear OOR3libero ;
016600140307         ENDIF;
016700140514         ENDIF;
016800140307
016900140314       //?Carico filiale abilitate  per l'orario stimato da visualizzare
017000140314 1     if fil_ab(1)='   ';
017100140318
017200140318         clear trulvpods  ;
017300140325         Ivpoke1='DECOFI816ORS'    ;
017400140318         TRULVPOR (trulvpods);
017500140314
017600140314 2       if fil_ab(1)='   ';
017700140314          eval fil_ab(1)='000' ;
017800140314 2       endif ;
017900140314
018000140314         // carico tabella variabili orari servizi
018100140314         clear TIBS02ds;
018200140314         T02Mod = 'C';
018300140314         T02sif = knsif;
018400140314         T02cod = 'VPO';
018500140314         t02tla = 'L'   ;
018600140314         T02ke1 ='ORARISER' ;
018700140314         TNTBE_RicercaControllo  (kpjba : tibs02ds);
018800140314
018900140314 2       if  T02err  = *blank;
019000140314          dvpoORARIS=t02uni  ;
019100140314 2       endif;
019200140314 1       endif;
019300140314
019400130911       //?Se non ho la Linea di riferimento esco dal pgm con errore
019500130911         IF  IOREfil = *zeros;
019600130911           OOREerr = '1';
019700130911           OOREmsg = 'Manca la filiale di riferimento';
019800130911           wFine = *on;
019900130911           leavesr;
020000130911         ENDIF;
020100130911
020200130911       //?Se non ho il CAP esco dal pgm con errore
020300130911         IF  IOREcap = *blanks;
020400130911           OOREerr = '1';
020500130911           OOREmsg = 'Manca il CAP';
020600130911           wFine = *on;
020700130911           leavesr;
020800130911         ENDIF;
020900130911
021000130911       //?Se non ho la Località esco dal pgm con errore
021100130911         IF  IOREloc = *blanks;
021200130911           OOREerr = '1';
021300130911           OOREmsg = 'Manca la Località';
021400130911           wFine = *on;
021500130911           leavesr;
021600130911         ENDIF;
021700130911
021800130911       //?Se non ho la Data di riferimento imposto la data del giorno
021900130911         IF  IOREdta = *zeros;
022000130911           IOREdta = %dec(%date());
022100130911         ENDIF;
022200130911
022300130911       //?Se non ho il tipo servizio imposto Consegna
022400130911         IF  IOREtser = *blanks;
022500130911           IOREtser = 'C';
022600130911         ENDIF;
022700131108
022800131108       //?Se richiesto tipo servizio ENTRAMBI forzo tipo servizio CONSEGNA
022900131108         IF  IOREtser = 'E';
023000131108           IOREtser = 'C';
023100131108           wEntrambi = *on;
023200131108         ENDIF;
023300130912
023400131016       //?Se non ho riga posizionamento WIN imposto riga 13
023500131016       //?per CONSEGNE
023600140317         IF  (IORErig = *zeros or iorerig>11) and IOREtser = 'C';
023700140325           IORErig = 12;
023800130912         ENDIF;
023900131108       //?Se non ho riga posizionamento WIN imposto riga 13
024000131016       //?per RITIRI
024100140317         IF  (IORErig = *zeros or iorerig>10) and IOREtser = 'R';
024200140317           IORErig = 10;
024300131016         ENDIF;
024400131016       //?Se non ho riga posizionamento WIN imposto riga 09
024500131016       //?per ENTRAMBI
024600140317         IF  (IORErig = *zeros or iorerig>09) and IOREtser = 'E';
024700131016           IORErig = 09;
024800131016         ENDIF;
024900131016       //?Se non ho colonna posizionamento WIN imposto colonna 2
025000130912         IF  IOREcol = *zeros;
025100130912           IOREcol = 02;
025200130912         ENDIF;
025300130925
025400130925       //?Imposto ora del sistema
025500130925         wOraSys = %dec(%time());
025600130911
025700130911       ENDSR;
025800130911
025900130911       //--------------------------------------------------------------
026000130911       //?Elabora.
026100130911       //--------------------------------------------------------------
026200130911       BEGSR Elabora;
026300130911
026400130911         wFine = *on;
026500131024
026600140307         SELECT;
026700140307       //?Elaborazione con Window
026800140307           WHEN  not wNOwin;
026900140307             exsr  elaWIN;
027000140307       //?Elaborazione senza Window e ritorna solo i dati
027100140307           WHEN  wNOwin;
027200140307             exsr  elaNOWIN;
027300140307         ENDSL;
027400140307
027500131024
027600131024       ENDSR;
027700131024
027800131024       //--------------------------------------------------------------
027900131024       //?Elaborazione con WINDOW;
028000131024       //--------------------------------------------------------------
028100131024       BEGSR elaWIN;
028200131024
028300140306         IF  not %open(TRULORsD);
028400140306           open TRULORsD;
028500131024         ENDIF;
028600130911
028700131011         clear ULOREW01C;
028800140307         clear ULOREW03C;
028900131011         clear ULOREW01R;
029000130920         clear ULOREW02;
029100140307
029200140307
029300140307       //?Costante tipo servizio
029400140307         SELECT;
029500140307           WHEN  Wentrambi= *on;
029600140307             OREDTSERW = 'ENTRAMBI';
029700140307           WHEN  IOREtser = 'C';
029800140307             OREDTSERW = 'CONSEGNA';
029900140307           WHEN  IOREtser = 'R';
030000140307             OREDTSERW = 'RITIRI';
030100140307         ENDSL;
030200140307
030300140307         W1rig = IORErig;
030400140307         W1col = IOREcol;
030500140307
030600140307       //?chiamata al tisio9r
030700140307         exsr CallTISI  ;
030800130920
030900130920         IF  DO0err <> *blanks;
031000131017         //?Se tipo servizio ENTRAMBI emetto WIN con messaggio
031100131017         //?personalizzato
031200131108           IF  wEntrambi = *on;
031300131017             W02msg1 = 'Orari servizio non inseriti';
031400131017             exfmt  ULOREW02E;
031500131017             leavesr;
031600131017           ENDIF;
031700131017         //?Se tipo servizio CONSEGNA/RITIRI WIN con messaggio da TISI
031800140325           wOUTmsg1 = %subst(DO0msg:1:40);
031900140325           wOUTmsg2 = %subst(DO0msg:41:78-40);
032000140325           IF  %subst(wOUTmsg1:40:1) <> ' ' and wOUTmsg2 <> *blanks;
032100140325             FOR  wPOS by 1 to 40;
032200140325               IF  %subst(wOUTmsg1:40-wPOS:1) = ' ';
032300140325                 wPOS = 40 - wPOS;
032400131108                 leave;
032500131108               ENDIF;
032600131108             ENDFOR;
032700131108             W02msg1 = %subst(wOUTmsg1:1:wPOS-1);
032800140325             W02msg2 = %trim(%subst(wOUTmsg1:wPOS+1:40-wPOS)) +
032900131108                       %trim(wOUTmsg2);
033000131108           ELSE;
033100131108             W02msg1 = wOUTmsg1;
033200131108             W02msg2 = wOUTmsg2;
033300131108           ENDIF;
033400130920           exfmt  ULOREW02;
033500130920           leavesr;
033600130920         ENDIF;
033700130911
033800130927         OREdrifw = '(' + DO0rif + ')';
033900130927         IF  DO0rif <> 'L' and DO0rif <> 'C' and
034000130927             DO0rif <> 'F';
034100130927           OREdrifw = '(S)';
034200130927         ENDIF;
034300130927
034400131011         ORElrsc = DO0olrsc;
034500131011         ORElrnc = DO0olrnc;
034600130911
034700130911         OREstisw = DO0ostis;
034800140307         W_stfsw  = DO0ostfs;
034900140307         exsr SistOFin;
035000140307         OREstfsw = W_stfsw ;
035100140307
035200131016         OREmiisw = DO0omiis;
035300131016         OREmxfsw = DO0omxfs;
035400130925
035500130930       //?Imposto gli orari di Riconsegna in giornata
035600131024         exsr riconsegna;
035700140224
035800140303       //?Orario presunto ritiro/consegna
035900140314          exsr OrStimata ;
036000131016
036100131016       //?Se tipo servizio ENTRAMBI faccio secondo richiamo con 'R'
036200140307         IF  WEntrambi =*on  ;
036300131016           DO0tser = 'R';
036400140109           tisio9r (kpjba:tisio9ds);
036500131016           OREstiswR = DO0ostis;
036600131016           OREstfswR = DO0ostfs;
036700131016           ORElrsc = DO0olrsc;
036800131016           ORElrnc = DO0olrnc;
036900131016         ENDIF;
037000131108
037100131108       //?Se tipo servizio RITIRI controllo la PIKUP
037200140307         IF  IOREtser = 'R' and IOREgf2 <> '01' and
037300140307             IOREgf2 <> *blanks;
037400131108           *in50 = *on;
037500131108         ELSE;
037600131108           *in50 = *off;
037700131108         ENDIF;
037800130911
037900131016         SELECT;
038000131016           WHEN  IOREtser = 'C';
038100140310           //  if *in42 and (%subst(knmus:1:3)='EDP')   ;
038200140314             //if *in42 ;
038300140314
038400140307             exfmt  ULOREW03C;
038500140314
038600140314             //else  ;
038700140314
038800140314             if *in42 and not *in42  ;
038900140307             exfmt  ULOREW01C;
039000140307             endif  ;
039100140307
039200131016           WHEN  IOREtser = 'R';
039300140314
039400140314             exfmt  ULOREW03R;
039500140314
039600140314             if *in42 and not *in42  ;
039700140314             exfmt  ULOREW01R;
039800140314             endif ;
039900140314
040000131016           OTHER;
040100131016             exfmt  ULOREW01E;
040200131016         ENDSL;
040300130911
040400130911       ENDSR;
040500140307       //--------------------------------------------------------------
040600140307       //?Chiamata ai TISIO9R
040700140307       //--------------------------------------------------------------
040800140307         BEGSR  CallTISI  ;
040900140307
041000140307         clear TISIO9ds;
041100140307
041200140307         DO0dri = IOREdta;
041300140307         DO0lna = IOREfil;
041400140307         DO0cap = IOREcap;
041500140307         DO0loc = IOREloc;
041600140307         DO0nar = IOREnar;
041700140307         DO0tser = IOREtser;
041800140409         // if IOREcons='S'  ;
041900140409         // la data distinta la passo sempre erchè serve per determinare l'orario in soglia P o " "
042000140409         //  sulle bolle consegnate
042100140307          do0ddc=IOREddc ;
042200140409         //endif  ;
042300140307         DO0tfp  = IOREtfp ;
042400140307         DO0tfa  = IOREtfa ;
042500140307         DO0dti  = IOREdti ;
042600140307         DO0hti  = IOREhti ;
042700140307         DO0dcr  = IOREdcr ;
042800140307         DO0hcr  = IOREhcr ;
042900140307         DO0tcr  = IOREtcr ;
043000140307         DO0dei  = IOREdei ;
043100140307         DO0oei  = IOREoei ;
043200140307
043300140307       //?Se tipo servizio ENTRAMBI faccio 1 richiamo con 'C'
043400140307         IF  IOREtser = 'E';
043500140307           DO0tser = 'C';
043600140307         ENDIF;
043700140307
043800140514         if  %parms>3 ;
043900140514         clear tisio9ds2    ;
044000140514         tisio9r (kpjba:tisio9ds:tisio9ds2);
044100140514         else  ;
044200140514         tisio9r (kpjba:tisio9ds);
044300140514         endif  ;
044400140307
044500140307         ENDSR   ;
044600140307       //--------------------------------------------------------------
044700140307       //?sistema orario fine consegna
044800140307       //--------------------------------------------------------------
044900140307         BEGSR   SistOFin  ;
045000140307       //?Per consegne controllo anche il tipo servizio bolla
045100140307       //?in modo da impostare l'orario di fine servizio = al limite
045200140307       //?massimo del tipo servizio bolla 10:30 o 13:00
045300140307         IF  IOREtser <> 'R' and IOREtsp = 'H';
045400140307           W_stfsw = 1030;
045500140307         ENDIF;
045600140307         // imposto 13 solo se l'orario di presunta consegna è entro le 12,00
045700140307         IF  IOREtser <> 'R' and IOREtsp = 'E' and ioreoei<=1200 ;
045800140307           W_stfsw = 1300;
045900140307         ENDIF;
046000140307         ENDSR ;
046100140307
046200140307       //--------------------------------------------------------------
046300140307       //?calcolo ora stimata consegna DAL /AL
046400140307       //--------------------------------------------------------------
046500140307       BEGSR OrStimata  ;
046600140307         clear orepresd ;
046700140307         clear orepresa ;
046800140307         *in42 = *off;
046900140314
047000140314       // solo se filiale abilitata
047100140314       w003a=%editc(iorefil:'X')  ;
047200140314         Indx=%lookup(w003a:fil_ab)      ;
047300140314         if indx=0  ;
047400140314          w003a='999' ;
047500140314          Indx=%lookup(w003a:fil_ab)      ;
047600140314         endif  ;
047700140314         if Indx>0  ;
047800140307
047900140307       // non presente se orario richiesto per entrambi
048000140307         IF  WEntrambi=*off  and ioreorp>0 ;
048100140307
048200140307         if  %dec(%subst(%editc(ioreorp:'X'):1:2):2:0) <=24 and
048300140307             %dec(%subst(%editc(ioreorp:'X'):3:2):2:0) >=00 and
048400140307             %dec(%subst(%editc(ioreorp:'X'):3:2):2:0) <=59    ;
048500140307           wora = ioreorp*100  ;
048600140307           woraiso = %time(wora) ;
048700140314           wOraISO = wOraISO - %minutes(§VPORST_M) ;
048800140307           Worap   = %dec(wOraISO);
048900140307           OREpresd= %dec(%subst(%editc(worap:'X'):1:4):4:0)  ;
049000140307           woraiso = %time(wora) ;
049100140314           wOraISO = wOraISO + %minutes(§VPORST_p) ;
049200140307           worap   = %dec(wOraISO);
049300140307           OREpresa= %dec(%subst(%editc(worap:'X'):1:4):4:0)  ;
049400140307           *in42=*on  ;
049500140307         endif  ;
049600140307         endif  ;
049700140307
049800140314         endif  ;
049900140314
050000140307         ENDSR ;
050100140307       //--------------------------------------------------------------
050200140307       //?Elaborazione senza WINDOW;
050300140307       //--------------------------------------------------------------
050400140307       BEGSR elaNOWIN;
050500140307
050600140307       //?chiamata al tisio9r
050700140307         exsr CallTISI  ;
050800140307
050900140307         IF  DO0err <> *blanks;
051000140307           OOR2err = '1';
051100140307           OOR2msg = 'Orari servizio non inseriti';
051200140307           leavesr;
051300140307         ENDIF;
051400140307
051500140307         OOR2stis = DO0ostis;
051600140307         OOR2stfs = DO0ostfs;
051700140307         W_stfsw  = DO0ostfs;
051800140307         exsr SistOFin;
051900140307         OOR2stfs = W_stfsw ;
052000140307         OOR2miis = DO0omiis;
052100140307         OOR2mxfs = DO0omxfs;
052200140307         OOR2orif = DO0rif  ;
052300140307         OOR2lrsc = DO0olrsc;
052400140307         OOR2lrnc = DO0olrnc;
052500140307         OOR2lrnk = DO0olrnk;
052600140415         OOR2fcsgl= DO0fcsgl;
052700140307
052800140307       //?Calcolo gli orari di Riconsegna in giornata
052900140307         exsr riconsegna;
053000140307
053100140310       //?Se riconsegna non possibile passo flag
053200140310         IF  *in41 or not *in40 ;
053300140307           OOR2FRIC='N'   ;
053400140307           else  ;
053500140307           OOR2FRIC='S'   ;
053600140307         ENDIF;
053700140307
053800140307       //?Ritorno i dati della riconsegna in giornata
053900140307         OOR2rids = OREda;
054000140307         OOR2rias = OREa;
054100140307
054200140307       //?Orario presunto ritiro/consegna
054300140307       exsr OrStimata ;
054400140307
054500140307       OOR2presd=OREpresd    ;
054600140307       OOR2presa=OREpresa    ;
054700140514
054800140514       //?Se passata la terza DS restituisco i dati della località normalizzata
054900140514       if %parms>3  ;
055000140514       OOR3loc_n=osio9loc    ;
055100140514       OOR3norm =osio9norm   ;
055200140514       endif  ;
055300140307
055400140307       ENDSR;
055500131024
055600131024       //--------------------------------------------------------------
055700131024       //?Calcolo la riconsegna.
055800131024       //--------------------------------------------------------------
055900131024       BEGSR riconsegna;
056000131024
056100131024         *in40 = *off;
056200131024         *in41 = *off;
056300140307         clear oreDA  ;
056400140307         clear oreA   ;
056500131024
056600131024         wOratrzs = %dec(%subst(%editc(DO0trzs:'X'):1:2):2:0);
056700131024         wMintrzs = %dec(%subst(%editc(DO0trzs:'X'):3:2):2:0);
056800131024
056900131024         IF  IOREtser = 'C' and IOREcons = 'S';
057000131024           *in40 = *on;
057100131024           wOraISO = %time();
057200131024           wOraISO = wOraISO + %hours(wOratrzs);
057300131105           wOraISO = wOraISO + %minutes(wMintrzs);
057400131024           wOra = %dec(wOraISO);
057500131024
057600131024         //?Imposto ora dalle
057700131024           SELECT;
057800131024             WHEN  wOra < wOraSys;
057900131024               OREda = DO0ostis;
058000131024             WHEN  wOra > DO0ostfs * 100;
058100131024               *in41 = *on;
058200131024             OTHER;
058300131024               OREda = wOra / 100;
058400131024           ENDSL;
058500131024
058600131024         //?Se posso fare la riconsegna
058700131024           IF  not *in41;
058800131105           //?arrotondo ai 5 minuti
058900131105             wOra = OREda * 100;
059000131105             wOraISO = %time(wOra);
059100131105             wMinuti = %dec(%subst(%editc(OREda:'X'):4:1):1:0);
059200131105             IF  wMinuti < 5;
059300131105               wMinuti1 = 5 - wMinuti;
059400131105               wOraISO = wOraISO + %minutes(wMinuti1);
059500131024             ENDIF;
059600131105             IF  wMinuti > 5;
059700131105               wMinuti1 = 10 - wMinuti;
059800131105               wOraISO = wOraISO + %minutes(wMinuti1);
059900131024             ENDIF;
060000131105             wOra = %dec(wOraISO);
060100131105             OREDA = wOra / 100;
060200131024           //?Imposto ora alle
060300131024             OREa = DO0ostfs;
060400131024           //?Se tra l'ora dalle e l'ora alle c'è meno di 30 minuti
060500131024           //?non posso fare la riconsegna
060600131024             wOra = OREda * 100;
060700131024             wOraISO = %time(wOra);
060800131024             wOra = OREa * 100;
060900131024             wOraISO1 = %time(wOra);
061000131024             wMinuti = %diff(wOraISO1 : wOraISo : *minutes);
061100131024             IF  wMinuti < 30;
061200131024               *in41 = *on;
061300131024             ENDIF;
061400131024           ENDIF;
061500131024         ENDIF;
061600131024
061700131024       ENDSR;
061800130911
061900130911       //--------------------------------------------------------------
062000130911       //?Operazioni finali.
062100130911       //--------------------------------------------------------------
062200130911       BEGSR RoutEnd;
062300131024
062400140306         IF  %open(TRULORsD);
062500140306           close TRULORsD;
062600131024         ENDIF;
062700130911
062800130911         *inLR = *on;
062900130911         return;
063000130911
063100130911       ENDSR;
063200130911
063300130911      /end-free
