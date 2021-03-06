000100100413      //---------------------------------------------------------------
000200100422      //?TNVRCRM5 - Note clienti INTERNE
000300100413      //---------------------------------------------------------------
000400100413
000500100413     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600100413
000700100413      //---------------------------------------------------------------
000800100413      //?Dichiarazione file.
000900100413      //---------------------------------------------------------------
001000100422     fTICPN00F  o    e             disk
001100100414     fTNVRCRMP  o    e             printer oflind(*in99)
001200100413
001300100413      //---------------------------------------------------------------
001400100413      //?Definizione costanti.
001500100413      //---------------------------------------------------------------
001600100413
001700100413      //---------------------------------------------------------------
001800100413      //?Definizione schiere.
001900100413      //---------------------------------------------------------------
002000090715
002100100413      //---------------------------------------------------------------
002200100413      //?Definizione aree dati.
002300100413      //---------------------------------------------------------------
002400100413
002500100413      //---------------------------------------------------------------
002600100413      //?Definizione strutture dati.
002700100413      //---------------------------------------------------------------
002800100422
002900100422       // - Reperimento dati anagrafici
003000100422     d TIBS69ds      e ds
003100100422     d DS_cnaco      e ds                  inz extname(CNACO00F)
003200100422     d DS_cnind      e ds                  inz extname(CNIND00F)
003300100422     d DS_cnclp      e ds                  inz extname(CNCLP00F)
003400100422     d DS_fncls      e ds                  inz extname(FNCLS00F)
003500100413
003600100413      // - Reperimento filiali
003700100413     d TRUL31DS      e ds
003800100413     d pog                    10    759  0 dim(250)
003900100422
004000100422      // Note
004100100422     d TFNTCds       e ds                  extname(TFNTC00F)
004200100413
004300100413      //---------------------------------------------------------------
004400100413      //?Definizione variabili globali.
004500100413      //---------------------------------------------------------------
004600100413
004700100413      // - Flags booleani
004800100413     d $End            s               n   inz(*off)
004900100413     d $Fine           s               n   inz(*off)
005000100414     d $Stampa         s               n   inz(*off)
005100100413
005200100413      // - Indici di schiera
005300100413     d xx              s              3  0 inz
005400100422     d yy              s              3  0 inz
005500100413
005600100413      // - Parametro ricevuto
005700100413     d filiale         s              3
005800100422
005900100422       // - Stringa SQL da eseguire
006000100422     d wSQL            s           2048    Varying        inz
006100100413
006200100413      // - Campi di comodo
006300100422     d old_NTCnk1      s                   like(NTCnk1)
006400100422     d old_NTCntr      s                   like(NTCntr)
006500100422     d sav_CPNpno      s                   like(CPNpno)
006600100422     d wData           s              8  0
006700100422     d wRiga           s              2  0
006800100413
006900100413      //---------------------------------------------------------------
007000100413      //?Definizione procedure esterne.
007100100413      //---------------------------------------------------------------
007200100414
007300100413     d KPJBA         e ds
007400100413
007500100413      //---------------------------------------------------------------
007600100413      //?prototipi
007700100413      //---------------------------------------------------------------
007800100414
007900100422      /copy gaitrasrc/srcprotopr,tibs69r
008000100413      /copy gaitrasrc/srcprotopr,trul31r
008100100413
008200100413      //---------------------------------------------------------------
008300100413      //?Definizione key-list.
008400100413      //---------------------------------------------------------------
008500100413
008600100413      //---------------------------------------------------------------
008700100413      //?Riepilogo indicatori.
008800100413      //---------------------------------------------------------------
008900100414
009000100414      // 99    : Salto pagina
009100100413
009200100413      //---------------------------------------------------------------
009300100413      //?M A I N - L I N E
009400100413      //---------------------------------------------------------------
009500100413
009600100413     c     *Entry        plist
009700100413     c                   parm                    filiale
009800100413
009900100413      /free
010000100413
010100100413       //?Operazioni iniziali
010200100413       exsr sr_RoutInz;
010300100422
010400100422       //?Imposto la stringa per SQL
010500100422       exsr sr_PrepSQL;
010600100413
010700100422       //?Scrivo le note INTERNE
010800100413       IF  not $Fine;
010900100422         exsr Elabora;
011000100413       ENDIF;
011100100413
011200100413       //?Operazioni finali
011300100413       exsr sr_RoutEnd;
011400100413
011500100413       //--------------------------------------------------------------
011600100413       //?Operazioni iniziali.
011700100413       //--------------------------------------------------------------
011800100413       BEGSR sr_RoutInz;
011900100413
012000100413       //?Recupero filiali del 'RA' della filiale ricevuta
012100100414         IF  filiale <> *blanks;
012200100414           clear TRUL31DS;
012300100414           I31abi = 'RA';
012400100414           I31cpo = %dec(filiale:3:0);
012500100414           callp trul31r (kpjba : trul31ds);
012600100414           IF  O31pog <= *zeros;
012700100414             $Fine = *on;
012800100414           ENDIF;
012900100414         ENDIF;
013000100422
013100100422       //?Recupero filiali del 'RA' di Bologna
013200100422         IF  filiale = *blanks;
013300100422           clear TRUL31DS;
013400100422           I31abi = 'RA';
013500100422           I31cpo = 001;
013600100422           callp trul31r (kpjba : trul31ds);
013700100422           IF  O31pog <= *zeros;
013800100422             $Fine = *on;
013900100422           ENDIF;
014000100422         ENDIF;
014100100413
014200100413       ENDSR;
014300100422
014400100422       //--------------------------------------------------------------
014500100422       //?Preparazione stringa SQL.
014600100422       //--------------------------------------------------------------
014700100422       BEGSR sr_PrepSQL;
014800100422
014900100422         wSQL  = 'select * from TFNTC00F where NTCapl = ';
015000100422         wSQL += '''' + 'C' + '''';
015100100422
015200100422       //?Se passata la filiale elaboro filiale IN sk pog
015300100422         IF  filiale <> *blanks;
015400100422           wSQL += ' and  substr(NTCnk1, 5, 3) in(';
015500100422
015600100422           yy = 0;
015700100422           xx = 1;
015800100422           FOR xx by 1 to %elem(Pog);
015900100422             IF Pog(xx) <> *zeros;
016000100422               IF yy > 0;
016100100422                 wSQL += ', ';
016200100422               ELSE;
016300100422                 yy = 1;
016400100422               ENDIF;
016500100422               wSQL += %editc(Pog(xx):'X');
016600100422             ENDIF;
016700100422           ENDFOR;
016800100422         ENDIF;
016900100422
017000100422       //?Se NON passata la filiale elaboro filiale NOT IN sk pog
017100100422         IF  filiale = *blanks;
017200100422           wSQL += ' and  substr(NTCnk1, 5, 3) not in(';
017300100422
017400100422           yy = 0;
017500100422           xx = 1;
017600100422           FOR xx by 1 to %elem(Pog);
017700100422             IF Pog(xx) <> *zeros;
017800100422               IF yy > 0;
017900100422                 wSQL += ', ';
018000100422               ELSE;
018100100422                 yy = 1;
018200100422               ENDIF;
018300100422               wSQL += %editc(Pog(xx):'X');
018400100422             ENDIF;
018500100422           ENDFOR;
018600100422         ENDIF;
018700100422
018800100422         wSQL += ') and NTCtnt = ';
018900100422         wSQL += '''' + 'IN' + '''';
019000100422
019100100422         wSQL += ' order by NTCnk1 +
019200100422                   for fetch only';
019300100422
019400100422       ENDSR;
019500100413
019600100413       //--------------------------------------------------------------
019700100422       //?Elaboro.
019800100413       //--------------------------------------------------------------
019900100422       BEGSR Elabora;
020000100414
020100100422         $End = *off;
020200100422
020300100422       //?Preparazione del cursore
020400100422         exec sql PREPARE S1   from :wSQL;
020500100422       //?Dichiarazione del cursore
020600100422         exec sql DECLARE NTC  cursor for S1;
020700100422
020800100422       //?Apertura del cursore
020900100422         exec sql open NTC;
021000100422
021100100422         IF sqlcode < 0;
021200100422           $End = *on;
021300100422         ENDIF;
021400100422
021500100422         DOW not $End;
021600100422
021700100422           exec sql FETCH next from NTC into :TFNTCds;
021800100422
021900100422           IF sqlcod = 100 or sqlcod < 0;
022000100422             $End = *on;
022100100422             leave;
022200100422           ENDIF;
022300100422
022400100422           //?scrivo le note dei clienti
022500100422           exsr Scrivi_Note;
022600100422
022700100422         ENDDO;
022800100422
022900100422         exec sql CLOSE NTC;
023000100414
023100100414       ENDSR;
023200100422
023300100422       //--------------------------------------------------------------
023400100422       //?Scrivo file note.
023500100422       //--------------------------------------------------------------
023600100422       BEGSR Scrivi_Note;
023700100422
023800100422       //?se cliente o giorno memorizzazione diversi azzero progressivo nuove note
023900100422         IF NTCnk1 <> old_NTCnk1 or NTCntr <> old_NTCntr;
024000100422           clear sav_CPNpno;
024100100422           old_NTCnk1 = NTCnk1;
024200100422           old_NTCntr = NTCntr;
024300100422         ENDIF;
024400100422       //?scrivo solo se data > 0
024500100422         IF  NTCntr > 0;
024600100422           exsr Scrivi_TICPN;
024700100422         ENDIF;
024800100422
024900100422       ENDSR;
025000100422
025100100422       //--------------------------------------------------------------
025200100422       //?Scrivo file TICPN.
025300100422       //--------------------------------------------------------------
025400100422       BEGSR Scrivi_TICPN;
025500100422
025600100422       //?Cerco i dati del cliente
025700100422         clear TIBS69DS;
025800100422         I69kac = %dec(%subst(NTCnk1:5:7):7:0);
025900100422         tibs69r (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
026000100422         IF  O69err <> *blanks;
026100100422           wRiga = 08;
026200100422           exsr sr_Stampa;
026300100422           leavesr;
026400100422         ENDIF;
026500100422         IF  ACOlib = *zeros;
026600100422           wRiga = 09;
026700100422           exsr sr_Stampa;
026800100422           leavesr;
026900100422         ENDIF;
027000100422
027100100422         clear TICPN000;
027200100422         CPNcpo = ACOlib;
027300100422         CPNksc = ACOksc;
027400100422       //?Costruisco la data lunga 8 da data lunga 6
027500100422         IF  NTCntr  < 000101;
027600100422           wData = NTCntr + 19000000;
027700100422         ELSE;
027800100422           wData = NTCntr + 20000000;
027900100422         ENDIF;
028000100422         CPNdim = wData;
028100100422         CPNhim = 235900;
028200100422         CPNsns = 'S';
028300100422         CPNnot = NTCrnt;
028400100422         CPNpno = sav_CPNpno + 1;
028500100422         write TICPN000;
028600100422         sav_CPNpno = CPNpno;
028700100422
028800100422       ENDSR;
028900100422
029000100414       //--------------------------------------------------------------
029100100414       //?Stampa errori.
029200100414       //--------------------------------------------------------------
029300100414       BEGSR sr_Stampa;
029400100414
029500100414         IF  not $Stampa;
029600100414           write CRMT01;
029700100414           $Stampa = *on;
029800100426           DESpgm = 'Trascodifica note interne clienti';
029900100426           write CRMT02;
030000100414         ENDIF;
030100100414
030200100422         IF  wRiga = 08;
030300100422           write CRMD08;
030400100422         ENDIF;
030500100422
030600100422         IF  wRiga = 09;
030700100422           write CRMD09;
030800100422         ENDIF;
030900100414
031000100414       ENDSR;
031100100413
031200100413       //--------------------------------------------------------------
031300100413       //?Operazioni finali.
031400100413       //--------------------------------------------------------------
031500100413       BEGSR sr_RoutEnd;
031600100413
031700100413         *inLR = *on;
031800100413         return;
031900100413
032000100413       ENDSR;
032100100413
032200100413      /end-free
