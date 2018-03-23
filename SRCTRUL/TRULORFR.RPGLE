000100130911      //---------------------------------------------------------------
000200131108      //?TRULORFR - Visualizza Orari di Filiale
000300130911      //---------------------------------------------------------------
000400130911     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000500130911     h dftactgrp(*no) actgrp(*caller)
000600130911
000700130911      //---------------------------------------------------------------
000800130911      //?Dichiarazione file.
000900130911      //---------------------------------------------------------------
001000130911
001100131112     fazorg01l  if   e           k disk
001200140926     fazlin03l  if   e           k disk    usropn
001300140930     ftntbe11l  if   e           k disk    usropn
001400130911       // -?File video
001500140922     fTRULORFD  cf   e             workstn usropn
001600130911
001700130911      //---------------------------------------------------------------
001800130911      //?Definizione costanti.
001900130911      //---------------------------------------------------------------
002000130911
002100130911      //---------------------------------------------------------------
002200130911      //?Definizione schiere.
002300130911      //---------------------------------------------------------------
002400130911
002500130911      //---------------------------------------------------------------
002600130911      //?Definizione aree dati.
002700130911      //---------------------------------------------------------------
002800130911
002900130911      //---------------------------------------------------------------
003000130911      //?Definizione strutture dati.
003100130911      //---------------------------------------------------------------
003200130911
003300130911      // - Parametri ricevuti
003400130911     d KPJBA         e ds
003500131108     d TRULORFds     e ds
003600140922     d TRULORFdsO    e ds
003700131125      * - Reperimento terminal di arrivo/partenza
003800131125     d FNLV55ds      e ds                  inz
003900131112
004000131112     d og129         e ds
004100131112     d og130         e ds
004200130911
004300130911
004400130911      //---------------------------------------------------------------
004500130911      //?Definizione variabili globali.
004600130911      //---------------------------------------------------------------
004700130911
004800131112     d w0040           s              4  0
004900131112     d w0040a          s              4  0
005000131125     d dataiso_cor     s               d   datfmt(*iso)
005100131125     d wdate           s              8  0
005200140929     d klin            s                   like(tbelin)
005300140929     d korflin         s                   like(iorflin)
005400140929     d oorfore         s                   like(oorforef_d)
005500140929     d orforesc        s                   like(oorforsc_d)
005600130911      // - Flags booleani
005700130911     d wFine           s               n   inz(*off)
005800140922     d wNOwin          s               n   inz(*off)
005900130925
006000130911
006100130911      //---------------------------------------------------------------
006200130911      //?Definizione procedure esterne.
006300130911      //---------------------------------------------------------------
006400130911
006500130911
006600130911      //---------------------------------------------------------------
006700130911      //?Definizione prototipi.
006800130911      //---------------------------------------------------------------
006900130911
007000130911      //---------------------------------------------------------------
007100130911      //?Definizione key-list.
007200130911      //---------------------------------------------------------------
007300130911
007400130911      //---------------------------------------------------------------
007500130911      //?Main.
007600130911      //---------------------------------------------------------------
007700130911
007800130911     c     *Entry        plist
007900130911     c                   parm                    KPJBA
008000131108     c                   parm                    trulorfds
008100140922     c                   parm                    trulorfdsO
008200130911
008300130911      /free
008400130911
008500130911       //?Operazioni iniziali
008600130911       exsr RoutInz;
008700130911
008800130911       //?Elaboro
008900130911       DOW  wFine = *off;
009000130911         exsr Elabora;
009100130911       ENDDO;
009200130911
009300130911       //?Operazioni finali
009400130911       exsr RoutEnd;
009500130911
009600130911       //--------------------------------------------------------------
009700130911       //?Operazioni iniziali.
009800130911       //--------------------------------------------------------------
009900130911       BEGSR RoutInz;
010000130911
010100131112         clear OORFerr;
010200131112         clear OORFmsg;
010300140922
010400140922       //?Controllo se passata anche la seconda DS
010500140922       //?in questo caso vuol dire che non devo aprire il file video
010600140922       //?perchè il pgm è stato richiamato SOLO per tornare i dati
010700140922         wNOwin = *off;
010800140930 1       IF  %parms > 2;
010900140922           clear trulorfdso ;
011000140922           wNOwin = *on;
011100140929
011200140929           open azlin03l  ;
011300140930 2         if iorflin=*blanks ;
011400140929            korflin='it' ;
011500140929           else  ;
011600140929            korflin=iorflin ;
011700140930 2         endif  ;
011800140929           chain korflin azlin03l  ;
011900140929           close azlin03l  ;
012000140929
012100140930 2         if %found(azlin03l)  ;
012200140929            klin=lintntbe  ;
012300140929           else ;
012400140929            clear klin  ;
012500140929           OORFerr = '2';
012600140929           OORFmsg = 'Lingua errata';
012700140929           wFine = *on;
012800140929           leavesr;
012900140930 2         endif  ;
013000140930 x1      else  ;
013100140930 2         IF  not %open(TRULORfD);
013200140929             open TRULORfD;
013300140930 2         ENDIF;
013400140929
013500140930 1       endif  ;
013600140930
013700140930       //?Se non ho la Linea di riferimento esco dal pgm con errore
013800140930 1       IF  IORFfil = *zeros;
013900140930           OORFerr = '1';
014000140930           OORFmsg = 'Manca la filiale di riferimento';
014100140930           wFine = *on;
014200140930           leavesr;
014300140930 1       ENDIF;
014400130912
014500140930       //?Se non ho riga posizionamento WIN imposto
014600140930 1       IF  IORFrig = *zeros ;
014700131113           IORFrig = 06;
014800140930 1       ENDIF;
014900140930       //?Se non ho colonna posizionamento WIN imposto
015000140930 1       IF  IORFcol = *zeros;
015100131113           IORFcol = 28;
015200140930 1       ENDIF;
015300131125
015400131125       // Reperisco la data corrente
015500131125         dataiso_cor=(%date());
015600131125         wdate=%dec(dataiso_cor);
015700130925
015800130911
015900130911       ENDSR;
016000130911
016100130911       //--------------------------------------------------------------
016200130911       //?Elabora.
016300130911       //--------------------------------------------------------------
016400130911       BEGSR Elabora;
016500130911
016600130911         wFine = *on;
016700130911
016800140922         if WNOwin=*off ;
016900140922          clear ULORFW01C;
017000140922         endif  ;
017100131015
017200131125
017300131125      /end-free
017400131125      * Reperisce la capofila della Lna
017500131125     c                   clear                   FNLV55ds
017600131125     c                   eval      D55lin   = iorffil
017700131125     c                   eval      D55tpt   = '6'
017800131125     c                   eval      D55drf   = wdate
017900131125     c                   call      'FNLV55R'
018000131125     c                   parm                    FNLV55ds
018100131125      /free
018200131016
018300131125         //  con la filiale chaino azorg
018400131125         chain d55tfa   azorg01l;
018500131112         if %founD(azorg01l) and (orgfag='F' or orgfag='A');
018600131112         // dati di filiale
018700131112            orfdes=orgdes;
018800131112            orfind=orgind;
018900131112            orfloc=%editc(orgcpf:'X') + ' ' + %trim(orgloc)
019000131112            + ' (' + orgpro + ')';
019100131112            og129=orgdc9;
019200131113            if §ogorfmd>*zeros;
019300131113               w0040=%int(§OGORFMd);
019400131113               w0040a=%int(§OGORFMa);
019500131113               orfore = %editW(w0040:'  :  ')  + ' - ' + %editw(w0040a:'  :  ');
019600131113            endif;
019700131113            if §OGORFpD>*zeros;
019800131113               w0040=%int(§OGORFpd);
019900131113               w0040a=%int(§OGORFpa);
020000131113               orfore = %trim(orfore ) + '  ' + %editW(w0040:'  :  ')
020100131113               + ' - ' + %editw(w0040a:'  :  ');
020200131113            endif;
020300131112         // dati servizio clienti
020400131112            orftel=orgtel;
020500131112            og130=orgdd0;
020600131113            if §OGORamD>*zeros;
020700131113               w0040=%int(§OGORaMd);
020800131113               w0040a=%int(§OGOraMa);
020900131113               orforesc=%editW(w0040:'  :  ')  + ' - ' + %editw(w0040a:'  :  ');
021000131113            endif;
021100131113            if §OGORapD>*zeros;
021200131113               w0040=%int(§OGORapd);
021300131113               w0040a=%int(§OGORapa);
021400131113               orforesc = %trim(orforesc) + '  ' + %editW(w0040:'  :  ')
021500131113               + ' - ' + %editw(w0040a:'  :  ');
021600131113            endif;
021700131113            orffax=orgfax;
021800131113            if orgdd2<>*blanks;
021900131113               orfmail=%trim(orgdd2) + '@brt.it'        ;
022000131113            endif;
022100140922
022200140922           if WNOwin=*off ;
022300140922            W1rig = IORFrig;
022400140922            W1col = IORFcol;
022500131113            exfmt  ULORFW01C;
022600140922           else   ;
022700140922
022800140922           // Riempo Ds di output
022900140922           oorfdes=orfdes    ;
023000140922           oorfind=orfind    ;
023100140922           oorfloc =orgloc    ;
023200140922           oorfpro =orgpro    ;
023300140922           oorfcap =%editc(orgcpf:'X') ;
023400140922           oorftel =orftel    ;
023500140922           oorffax =orffax    ;
023600140922           oorfmail=orfmail   ;
023700140929           oorforef_d =orfore    ;
023800140929           oorforsc_d=orforesc   ;
023900140930           if §OGORFMd>*zeros ;
024000140922           oorffmd =%int(§OGORFMd) ;
024100140930           endif  ;
024200140930           if §OGORFMa>*zeros ;
024300140922           oorffma =%int(§OGORFMa) ;
024400140930           endif  ;
024500140930           if §OGORFpd>*zeros ;
024600140922           oorffpd =%int(§OGORFpd) ;
024700140930           endif  ;
024800140930           if §OGORFpa>*zeros ;
024900140922           oorffpa =%int(§OGORFpa) ;
025000140930           endif  ;
025100140930           if §OGORamd>*zeros ;
025200140922           oorfamd =%int(§OGORaMd) ;
025300140930           endif  ;
025400140930           if §OGORama>*zeros ;
025500140922           oorfama =%int(§OGORaMa) ;
025600140930           endif  ;
025700140930           if §OGORapd>*zeros ;
025800140922           oorfapd =%int(§OGORapd) ;
025900140930           endif  ;
026000140930           if §OGORapa>*zeros ;
026100140922           oorfapa =%int(§OGORapa) ;
026200140930           endif  ;
026300140922
026400140922           // con il codice lingua chaino la tabella UFI per reperire  URL
026500140930           open tntbe11l  ;
026600140926
026700140922           clear tbeke2  ;
026800140922           tbeke1=%editc(iorffil:'X')  ;
026900140930           chain (klin:'UFI': tbeke1:tbeke2) tntbe11l ;
027000140930             if %found(tntbe11l)  ;
027100140922              oorfurl=tbeuni  ;
027200140922             endif  ;
027300140930           close tntbe11l  ;
027400140922
027500140922           endif  ;
027600140922
027700131112         else;
027800131113           OORFerr = '1';
027900131113           OORFmsg = 'Il Codice ricevuto non è di filiale';
028000131112         endif;
028100130925
028200130911       ENDSR;
028300130911
028400130911       //--------------------------------------------------------------
028500130911       //?Operazioni finali.
028600130911       //--------------------------------------------------------------
028700130911       BEGSR RoutEnd;
028800130911
028900131125      /end-free
029000131125      *
029100131125     c                   clear                   FNLV55ds
029200131125     c                   eval      d55tla='C'
029300131125     c                   call      'FNLV55R'
029400131125     c                   parm                    FNLV55ds
029500131125      /free
029600140929         if WNOwin=*off ;
029700131120         WRITE ULORFASS     ;
029800140929         endif  ;
029900130911         *inLR = *on;
030000130911         return;
030100130911
030200130911       ENDSR;
030300130911
030400130911      /end-free
