000100130911      //---------------------------------------------------------------
000200130911      //?TRULORER - Visualizza Orari di Servizio
000300130911      //---------------------------------------------------------------
000400130911     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000500130911     h dftactgrp(*no) actgrp(*caller)
000600130911
000700130911      //---------------------------------------------------------------
000800130911      //?Dichiarazione file.
000900130911      //---------------------------------------------------------------
001000130911
001100130911       // -?File video
001200130911     fTRULORED  cf   e             workstn
001300130911
001400130911      //---------------------------------------------------------------
001500130911      //?Definizione costanti.
001600130911      //---------------------------------------------------------------
001700130911
001800130911      //---------------------------------------------------------------
001900130911      //?Definizione schiere.
002000130911      //---------------------------------------------------------------
002100130911
002200130911      //---------------------------------------------------------------
002300130911      //?Definizione aree dati.
002400130911      //---------------------------------------------------------------
002500130911
002600130911      //---------------------------------------------------------------
002700130911      //?Definizione strutture dati.
002800130911      //---------------------------------------------------------------
002900130911
003000130911      // - Parametri ricevuti
003100130911     d KPJBA         e ds
003200130911     d TRULOREds     e ds
003300130911
003400130911      // - Ricerca orari di servizio
003500130911     d TISIO0ds      e ds
003600130911
003700130911      //---------------------------------------------------------------
003800130911      //?Definizione variabili globali.
003900130911      //---------------------------------------------------------------
004000130911
004100130911      // - Flags booleani
004200130911     d wFine           s               n   inz(*off)
004300131108     d wEntrambi       s               n   inz(*off)
004400130925
004500130925      // - Campi di comodo
004600131015     d wMintrzs        s              2  0
004700131015     d wMinuti         s              5  0
004800131105     d wMinuti1        s              5  0
004900130925     d wOra            s              6  0
005000130925     d wOraSys         s              6  0
005100130925     d wOraISO         s               t   timfmt(*iso)
005200131015     d wOraISO1        s               t   timfmt(*iso)
005300131010     d wOratrzs        s              2  0
005400130911
005500130911      //---------------------------------------------------------------
005600130911      //?Definizione procedure esterne.
005700130911      //---------------------------------------------------------------
005800130911
005900130911      // - Ricerca orari di servizio
006000130911     d tisio1r         pr                  extpgm('TISIO1R')
006100130911     d  kpjba                              likeds(KPJBA)
006200130911     d  tisio0ds                           likeds(TISIO0DS)
006300130911
006400130911      //---------------------------------------------------------------
006500130911      //?Definizione prototipi.
006600130911      //---------------------------------------------------------------
006700130911
006800130911      //---------------------------------------------------------------
006900130911      //?Definizione key-list.
007000130911      //---------------------------------------------------------------
007100130911
007200130911      //---------------------------------------------------------------
007300130911      //?Main.
007400130911      //---------------------------------------------------------------
007500130911
007600130911     c     *Entry        plist
007700130911     c                   parm                    KPJBA
007800130911     c                   parm                    truloreds
007900130911
008000130911      /free
008100130911
008200130911       //?Operazioni iniziali
008300130911       exsr RoutInz;
008400130911
008500130911       //?Elaboro
008600130911       DOW  wFine = *off;
008700130911         exsr Elabora;
008800130911       ENDDO;
008900130911
009000130911       //?Operazioni finali
009100130911       exsr RoutEnd;
009200130911
009300130911       //--------------------------------------------------------------
009400130911       //?Operazioni iniziali.
009500130911       //--------------------------------------------------------------
009600130911       BEGSR RoutInz;
009700130911
009800130911         clear OOREerr;
009900130911         clear OOREmsg;
010000131108         wEntrambi = *off;
010100130911
010200130911       //?Se non ho la Linea di riferimento esco dal pgm con errore
010300130911         IF  IOREfil = *zeros;
010400130911           OOREerr = '1';
010500130911           OOREmsg = 'Manca la filiale di riferimento';
010600130911           wFine = *on;
010700130911           leavesr;
010800130911         ENDIF;
010900130911
011000130911       //?Se non ho il CAP esco dal pgm con errore
011100130911         IF  IOREcap = *blanks;
011200130911           OOREerr = '1';
011300130911           OOREmsg = 'Manca il CAP';
011400130911           wFine = *on;
011500130911           leavesr;
011600130911         ENDIF;
011700130911
011800130911       //?Se non ho la Località esco dal pgm con errore
011900130911         IF  IOREloc = *blanks;
012000130911           OOREerr = '1';
012100130911           OOREmsg = 'Manca la Località';
012200130911           wFine = *on;
012300130911           leavesr;
012400130911         ENDIF;
012500130911
012600130911       //?Se non ho la Data di riferimento imposto la data del giorno
012700130911         IF  IOREdta = *zeros;
012800130911           IOREdta = %dec(%date());
012900130911         ENDIF;
013000130911
013100130911       //?Se non ho il tipo servizio imposto Consegna
013200130911         IF  IOREtser = *blanks;
013300130911           IOREtser = 'C';
013400130911         ENDIF;
013500131108
013600131108       //?Se richiesto tipo servizio ENTRAMBI forzo tipo servizio CONSEGNA
013700131108         IF  IOREtser = 'E';
013800131108           IOREtser = 'C';
013900131108           wEntrambi = *on;
014000131108         ENDIF;
014100130912
014200131016       //?Se non ho riga posizionamento WIN imposto riga 13
014300131016       //?per CONSEGNE
014400131016         IF  IORErig = *zeros and IOREtser = 'C';
014500131016           IORErig = 13;
014600130912         ENDIF;
014700131016       //?Se non ho riga posizionamento WIN imposto riga 15
014800131016       //?per RITIRI
014900131016         IF  IORErig = *zeros and IOREtser = 'R';
015000131016           IORErig = 15;
015100131016         ENDIF;
015200131016       //?Se non ho riga posizionamento WIN imposto riga 09
015300131016       //?per ENTRAMBI
015400131016         IF  IORErig = *zeros and IOREtser = 'E';
015500131016           IORErig = 09;
015600131016         ENDIF;
015700131016       //?Se non ho colonna posizionamento WIN imposto colonna 2
015800130912         IF  IOREcol = *zeros;
015900130912           IOREcol = 02;
016000130912         ENDIF;
016100130925
016200130925       //?Imposto ora del sistema
016300130925         wOraSys = %dec(%time());
016400130911
016500130911       ENDSR;
016600130911
016700130911       //--------------------------------------------------------------
016800130911       //?Elabora.
016900130911       //--------------------------------------------------------------
017000130911       BEGSR Elabora;
017100130911
017200130911         wFine = *on;
017300130911
017400131011         clear ULOREW01C;
017500131011         clear ULOREW01R;
017600130920         clear ULOREW02;
017700130911         clear TISIO0ds;
017800131015
017900131015       //?Costante tipo servizio
018000131015         SELECT;
018100131015           WHEN  IOREtser = 'C';
018200131015             OREDTSERW = 'CONSEGNA';
018300131015           WHEN  IOREtser = 'R';
018400131015             OREDTSERW = 'RITIRI';
018500131015           OTHER;
018600131015             OREDTSERW = 'ENTRAMBI';
018700131015         ENDSL;
018800130912
018900130912         W1rig = IORErig;
019000130912         W1col = IOREcol;
019100131016
019200130911         DO0dri = IOREdta;
019300130911         DO0lna = IOREfil;
019400130911         DO0cap = IOREcap;
019500130911         DO0loc = IOREloc;
019600130920         DO0nar = IOREnar;
019700131016         DO0tser = IOREtser;
019800131016
019900131016       //?Se tipo servizio ENTRAMBI faccio 1 richiamo con 'C'
020000131016         IF  IOREtser = 'E';
020100131016           DO0tser = 'C';
020200131016         ENDIF;
020300131016
020400130911         tisio1r (kpjba:tisio0ds);
020500130920
020600130920         IF  DO0err <> *blanks;
020700131017         //?Se tipo servizio ENTRAMBI emetto WIN con messaggio
020800131017         //?personalizzato
020900131108           //IF  IOREtser = 'E';
021000131108           IF  wEntrambi = *on;
021100131017             W02msg1 = 'Orari servizio non inseriti';
021200131017             W1rig = 19;
021300131017             exfmt  ULOREW02E;
021400131017             leavesr;
021500131017           ENDIF;
021600131017         //?Se tipo servizio CONSEGNA/RITIRI WIN con messaggio da TISI
021700130920           W02msg1 = %subst(DO0msg:1:39);
021800130920           W02msg2 = %subst(DO0msg:40:39);
021900130920           exfmt  ULOREW02;
022000130920           leavesr;
022100130920         ENDIF;
022200130911
022300130927         OREdrifw = '(' + DO0rif + ')';
022400130927         IF  DO0rif <> 'L' and DO0rif <> 'C' and
022500130927             DO0rif <> 'F';
022600130927           OREdrifw = '(S)';
022700130927         ENDIF;
022800130927
022900131011         ORElrsc = DO0olrsc;
023000131011         ORElrnc = DO0olrnc;
023100130911
023200130911         OREstisw = DO0ostis;
023300130911         OREstfsw = DO0ostfs;
023400131106       //?Per consegne controllo anche il tipo servizio bolla
023500131106       //?in modo da impostare l'orario di fine servizio = al limite
023600131106       //?massimo del tipo servizio bolla 10:30 o 13:00
023700131106         IF  IOREtser <> 'R' and IOREtsp = 'H';
023800131106           OREstfsw = 1030;
023900131106         ENDIF;
024000131106         IF  IOREtser <> 'R' and IOREtsp = 'E';
024100131106           OREstfsw = 1300;
024200131106         ENDIF;
024300131106
024400131016         OREmiisw = DO0omiis;
024500131016         OREmxfsw = DO0omxfs;
024600131106
024700131010         wOratrzs = %dec(%subst(%editc(DO0trzs:'X'):1:2):2:0);
024800131010         wMintrzs = %dec(%subst(%editc(DO0trzs:'X'):3:2):2:0);
024900130925
025000130930       //?Imposto gli orari di Riconsegna in giornata
025100131011         *in40 = *off;
025200131011         *in41 = *off;
025300131011         IF  IOREtser = 'C' and IOREcons = 'S';
025400131011           *in40 = *on;
025500130930           wOraISO = %time();
025600131010           wOraISO = wOraISO + %hours(wOratrzs);
025700131105           wOraISO = wOraISO + %minutes(wMintrzs);
025800130930           wOra = %dec(wOraISO);
025900130925
026000130930         //?Imposto ora dalle
026100130930           SELECT;
026200130930             WHEN  wOra < wOraSys;
026300130930               OREda = DO0ostis;
026400130930             WHEN  wOra > DO0ostfs * 100;
026500131011               *in41 = *on;
026600130930             OTHER;
026700130930               OREda = wOra / 100;
026800130930           ENDSL;
026900131010
027000131015         //?Se posso fare la riconsegna
027100131014           IF  not *in41;
027200131105           //?arrotondo a 5 minuti
027300131105             wOra = OREda * 100;
027400131105             wOraISO = %time(wOra);
027500131105             wMinuti = %dec(%subst(%editc(OREda:'X'):4:1):1:0);
027600131105             IF  wMinuti < 5;
027700131105               wMinuti1 = 5 - wMinuti;
027800131105               wOraISO = wOraISO + %minutes(wMinuti1);
027900131014             ENDIF;
028000131105             IF  wMinuti > 5;
028100131105               wMinuti1 = 10 - wMinuti;
028200131105               wOraISO = wOraISO + %minutes(wMinuti1);
028300131014             ENDIF;
028400131105             wOra = %dec(wOraISO);
028500131105             OREDA = wOra / 100;
028600131014           //?Imposto ora alle
028700131014             OREa = DO0ostfs;
028800131015           //?Se tra l'ora dalle e l'ora alle c'è meno di 30 minuti
028900131015           //?non posso fare la riconsegna
029000131015             wOra = OREda * 100;
029100131015             wOraISO = %time(wOra);
029200131015             wOra = OREa * 100;
029300131015             wOraISO1 = %time(wOra);
029400131015             wMinuti = %diff(wOraISO1 : wOraISo : *minutes);
029500131015             IF  wMinuti < 30;
029600131015               *in41 = *on;
029700131015             ENDIF;
029800131014           ENDIF;
029900131014         ENDIF;
030000131016
030100131016       //?Se tipo servizio ENTRAMBI faccio secondo richiamo con 'R'
030200131016         IF  IOREtser = 'E';
030300131016           DO0tser = 'R';
030400131016           tisio1r (kpjba:tisio0ds);
030500131016           OREstiswR = DO0ostis;
030600131016           OREstfswR = DO0ostfs;
030700131016           ORElrsc = DO0olrsc;
030800131016           ORElrnc = DO0olrnc;
030900131016         ENDIF;
031000130911
031100131016         SELECT;
031200131016           WHEN  IOREtser = 'C';
031300131016             exfmt  ULOREW01C;
031400131016           WHEN  IOREtser = 'R';
031500131016             exfmt  ULOREW01R;
031600131016           OTHER;
031700131016             exfmt  ULOREW01E;
031800131016         ENDSL;
031900130911
032000130911       ENDSR;
032100130911
032200130911       //--------------------------------------------------------------
032300130911       //?Operazioni finali.
032400130911       //--------------------------------------------------------------
032500130911       BEGSR RoutEnd;
032600130911
032700130911         *inLR = *on;
032800130911         return;
032900130911
033000130911       ENDSR;
033100130911
033200130911      /end-free
