000100100413      //---------------------------------------------------------------
000200100420      //?TNVRCRM3 - Da TNCPS a TICPS stati potenziali
000300100413      //---------------------------------------------------------------
000400100413
000500100413     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600100413
000700100413      //---------------------------------------------------------------
000800100413      //?Dichiarazione file.
000900100413      //---------------------------------------------------------------
001000100415     fTABEL00F  if   e           k disk
001100100420     fTFNTC01L  if   e           k disk
001200100413     fTICPN00F  o    e             disk
001300100415     fTICPS00F  o    e             disk
001400100421     fTNCPS00F  uf   e             disk    prefix(s_)
001500100414     fTNVRCRMP  o    e             printer oflind(*in99)
001600100413
001700100413      //---------------------------------------------------------------
001800100413      //?Definizione costanti.
001900100413      //---------------------------------------------------------------
002000100413
002100100413      //---------------------------------------------------------------
002200100413      //?Definizione schiere.
002300100413      //---------------------------------------------------------------
002400090715
002500100413      //---------------------------------------------------------------
002600100413      //?Definizione aree dati.
002700100413      //---------------------------------------------------------------
002800100413
002900100413      //---------------------------------------------------------------
003000100413      //?Definizione strutture dati.
003100100413      //---------------------------------------------------------------
003200100415
003300100415     d KPJBA         e ds
003400100415
003500100415      // - Reperimento filiali
003600100415     d TRUL31DS      e ds
003700100415     d pog                    10    759  0 dim(250)
003800100415
003900100415      // - Aggiunge/Toglie gg/mm dalla data
004000100415     d XGIOLAVds     e ds                  inz
004100100421
004200100421      // Anagrafica potenziali
004300100421     d TNCPOds       e ds                  extname(TNCPO00F)
004400100413
004500100421      // Stati potenziali
004600100421     d TNCPSds       e ds                  extname(TNCPS00F)
004700100421     d                                     prefix(w_)
004800100415
004900100415      // - Tabella 01 = Codici Commerciali
005000100415     d ds01          e ds                  inz
005100100415
005200100413      //---------------------------------------------------------------
005300100413      //?Definizione variabili globali.
005400100413      //---------------------------------------------------------------
005500100413
005600100413      // - Flags booleani
005700100413     d $End            s               n   inz(*off)
005800100422     d $EndCps         s               n   inz(*off)
005900100413     d $Fine           s               n   inz(*off)
006000100414     d $Stampa         s               n   inz(*off)
006100100421
006200100421      // - Indici di schiera
006300100421     d xx              s              3  0 inz
006400100421     d yy              s              3  0 inz
006500100413
006600100413      // - Parametro ricevuto
006700100413     d filiale         s              3
006800100420
006900100420       // - Stringa SQL da eseguire
007000100420     d wSQL            s           2048    Varying        inz
007100100413
007200100413      // - Campi di comodo
007300100421     d old_NTCntr      s                   like(NTCntr)
007400100421     d rrncps          s              9  0
007500100415     d sav_CPNpno      s                   like(CPNpno)
007600100416     d wData           s              8  0
007700100416     d wDataMax        s              8  0
007800100415     d wDataOggi       s              8  0
007900100414     d wData_ISO       s               d   datfmt(*iso)
008000100421     d wNr             s              2  0
008100100415     d wRiga           s              2  0
008200100413
008300100413      //---------------------------------------------------------------
008400100413      //?Definizione procedure esterne.
008500100413      //---------------------------------------------------------------
008600100413
008700100413      //---------------------------------------------------------------
008800100413      //?prototipi
008900100413      //---------------------------------------------------------------
009000100414
009100100413      /copy gaitrasrc/srcprotopr,trul31r
009200100413
009300100413      //---------------------------------------------------------------
009400100413      //?Definizione key-list.
009500100413      //---------------------------------------------------------------
009600100415
009700100415       // - File TABEL00F
009800100415     d k03tabel      e ds                  extname(TABEL00F:*key)
009900100415     d                                     prefix(k_)
010000100413
010100100413      //---------------------------------------------------------------
010200100413      //?Riepilogo indicatori.
010300100413      //---------------------------------------------------------------
010400100414
010500100414      // 99    : Salto pagina
010600100413
010700100413      //---------------------------------------------------------------
010800100413      //?M A I N - L I N E
010900100413      //---------------------------------------------------------------
011000100413
011100100413     c     *Entry        plist
011200100413     c                   parm                    filiale
011300100413
011400100413      /free
011500100413
011600100413       //?Operazioni iniziali
011700100413       exsr sr_RoutInz;
011800100420
011900100420       //?Imposto la stringa per SQL
012000100420       exsr sr_PrepSQL;
012100100413
012200100413       //?Scrivo i nuovi file per le trattative
012300100413       IF  not $Fine;
012400100413         exsr sr_Elabora;
012500100413       ENDIF;
012600100413
012700100413       //?Operazioni finali
012800100413       exsr sr_RoutEnd;
012900100413
013000100413       //--------------------------------------------------------------
013100100413       //?Operazioni iniziali.
013200100413       //--------------------------------------------------------------
013300100413       BEGSR sr_RoutInz;
013400100413
013500100413       //?Recupero filiali del 'RA' della filiale ricevuta
013600100414         IF  filiale <> *blanks;
013700100414           clear TRUL31DS;
013800100414           I31abi = 'RA';
013900100414           I31cpo = %dec(filiale:3:0);
014000100414           callp trul31r (kpjba : trul31ds);
014100100414           IF  O31pog <= *zeros;
014200100414             $Fine = *on;
014300100414           ENDIF;
014400100414         ENDIF;
014500100422
014600100422       //?Recupero filiali del 'RA' di Bologna
014700100422         IF  filiale = *blanks;
014800100422           clear TRUL31DS;
014900100422           I31abi = 'RA';
015000100422           I31cpo = 001;
015100100422           callp trul31r (kpjba : trul31ds);
015200100422           IF  O31pog <= *zeros;
015300100422             $Fine = *on;
015400100422           ENDIF;
015500100422         ENDIF;
015600100414
015700100414         wData_ISO = %date();
015800100415         wDataOggi = %dec(wData_ISO);
015900100420       //?Calcolo data limite massimo stati
016000100420         wDataMax = 20111231;
016100100413
016200100413         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
016300100416
016400100416       //?Impostazione campi "fissi"
016500100416         k_TBLkut = 1;
016600100413
016700100413       ENDSR;
016800100420
016900100420       //--------------------------------------------------------------
017000100420       //?Preparazione stringa SQL.
017100100420       //--------------------------------------------------------------
017200100420       BEGSR sr_PrepSQL;
017300100420
017400100420         wSQL  = 'select * from TNCPO00F where CPOatb = ';
017500100420         wSQL += '''' + ' ' + '''';
017600100420
017700100420       //?Se passata la filiale elaboro filiale IN sk pog
017800100420         IF  filiale <> *blanks;
017900100420           wSQL += ' and  CPOflt in(';
018000100420
018100100420           yy = 0;
018200100420           xx = 1;
018300100420           FOR xx by 1 to %elem(Pog);
018400100420             IF Pog(xx) <> *zeros;
018500100420               IF yy > 0;
018600100420                 wSQL += ', ';
018700100420               ELSE;
018800100420                 yy = 1;
018900100420               ENDIF;
019000100420               wSQL += %editc(Pog(xx):'X');
019100100420             ENDIF;
019200100420           ENDFOR;
019300100420         ENDIF;
019400100420
019500100420       //?Se NON passata la filiale elaboro filiale NOT IN sk pog
019600100420         IF  filiale = *blanks;
019700100420           wSQL += ' and  CPOflt not in(';
019800100420
019900100420           yy = 0;
020000100420           xx = 1;
020100100420           FOR xx by 1 to %elem(Pog);
020200100420             IF Pog(xx) <> *zeros;
020300100420               IF yy > 0;
020400100420                 wSQL += ', ';
020500100420               ELSE;
020600100420                 yy = 1;
020700100420               ENDIF;
020800100420               wSQL += %editc(Pog(xx):'X');
020900100420             ENDIF;
021000100420           ENDFOR;
021100100420         ENDIF;
021200100420
021300100421         wSQL += ') order by CPOcpo +
021400100420                   for fetch only';
021500100420
021600100420       ENDSR;
021700100413
021800100413       //--------------------------------------------------------------
021900100422       //?Elaboro.
022000100413       //--------------------------------------------------------------
022100100413       BEGSR sr_Elabora;
022200100413
022300100413         $End = *off;
022400100420
022500100420       //?Preparazione del cursore
022600100420         exec sql PREPARE S1   from :wSQL;
022700100420       //?Dichiarazione del cursore
022800100420         exec sql DECLARE CPO  cursor for S1;
022900100420
023000100420       //?Apertura del cursore
023100100420         exec sql open CPO;
023200100420
023300100420         IF sqlcode < 0;
023400100420           $End = *on;
023500100420         ENDIF;
023600100420
023700100420         DOW not $End;
023800100420
023900100420           exec sql FETCH next from CPO into :TNCPOds;
024000100420
024100100420           IF sqlcod = 100 or sqlcod < 0;
024200100420             $End = *on;
024300100420             leave;
024400100420           ENDIF;
024500100421
024600100421           //?scrivo le note dei potenziali
024700100421           exsr Scrivi_Note;
024800100420
024900100421           //?scrivo il nuovo file degli stati potenziali
025000100421           exsr Scrivi;
025100100413
025200100413         ENDDO;
025300100413
025400100420         exec sql CLOSE CPO;
025500100413
025600100413       ENDSR;
025700100414
025800100414       //--------------------------------------------------------------
025900100414       //?Scrivo file note.
026000100414       //--------------------------------------------------------------
026100100414       BEGSR Scrivi_Note;
026200100421
026300100421         clear old_NTCntr;
026400100421         clear sav_CPNpno;
026500100415
026600100421         NTCapl = 'P';
026700100421         NTCnk1 = %editc(CPOcpo:'X');
026800100421         clear NTCnk2;
026900100421         NTCtnt = 'IN';
027000100421         setll  (NTCapl : NTCnk1 : NTCnk2 : NTCtnt) TFNTC01L;
027100100421         reade  (NTCapl : NTCnk1 : NTCnk2 : NTCtnt) TFNTC01L;
027200100414         DOW  not %eof(TFNTC01L);
027300100421         //?se giorno memorizzazione diverso azzero progressivo nuove note
027400100421           IF NTCntr <> old_NTCntr;
027500100421             clear sav_CPNpno;
027600100421             old_NTCntr = NTCntr;
027700100421           ENDIF;
027800100421         //?scrivo solo se data > 0
027900100421           IF  NTCntr > 0;
028000100421             exsr Scrivi_TICPN;
028100100421           ENDIF;
028200100421
028300100421           reade  (NTCapl : NTCnk1 : NTCnk2 : NTCtnt) TFNTC01L;
028400100414
028500100414         ENDDO;
028600100414
028700100414       ENDSR;
028800100421
028900100421       //--------------------------------------------------------------
029000100421       //?Scrivo il nuovo file stati.
029100100421       //--------------------------------------------------------------
029200100421       BEGSR Scrivi;
029300100421
029400100422         $EndCps = *off;
029500100421         //?contatore per aggiungere giorni a data massima
029600100421         clear wNr;
029700100421
029800100421         exec sql
029900100421         DECLARE CPS cursor for
030000100421         SELECT  rrn(TNCPS00F), TNCPS00F.*
030100100421         FROM TNCPS00F
030200100421         WHERE CPScpo = :CPOcpo and CPSatb = ' '
030300100421         ORDER by CPSdst;
030400100421
030500100421         exec sql OPEN CPS;
030600100421
030700100422         DOW  not $EndCps;
030800100421           exec sql FETCH next from CPS into :rrncps, :TNCPSds;
030900100421           IF  sqlcod = 100 or sqlcod < 0;
031000100422             $EndCps = *on;
031100100421             leave;
031200100421           ENDIF;
031300100421         //?scrivo il nuovo file
031400100421           exsr Scrivi_TICPS;
031500100421         //?fleggo il vecchio file come già codificato
031600100421           exsr Agg_TNCPS;
031700100421
031800100421         ENDDO;
031900100421
032000100421         exec sql CLOSE CPS;
032100100421
032200100421       ENDSR;
032300100421
032400100421       //--------------------------------------------------------------
032500100421       //?Scrivo file TICPN.
032600100421       //--------------------------------------------------------------
032700100421       BEGSR Scrivi_TICPN;
032800100421
032900100421         clear TICPN000;
033000100421         CPNcpo = CPOcpo;
033100100421       //?Costruisco la data lunga 8 da data lunga 6
033200100421         IF  NTCntr  < 000101;
033300100421           wData = NTCntr + 19000000;
033400100421         ELSE;
033500100421           wData = NTCntr + 20000000;
033600100421         ENDIF;
033700100421         CPNdim = wData;
033800100421         CPNhim = 235800;
033900100421         CPNsns = 'S';
034000100421         CPNnot = NTCrnt;
034100100421         CPNpno = sav_CPNpno + 1;
034200100503         CPNpos = 046;
034300100503         CPNpru = 'BATCH';
034400100421         write TICPN000;
034500100421         sav_CPNpno = CPNpno;
034600100421
034700100421       ENDSR;
034800100415
034900100415       //--------------------------------------------------------------
035000100415       //?Scrivo file TICPS.
035100100415       //--------------------------------------------------------------
035200100415       BEGSR Scrivi_TICPS;
035300100415
035400100415         clear TICPS000;
035500100421         CPScpo = w_CPScpo;
035600100421         CPScmm = w_CPScmm;
035700100415         clear ds01;
035800100419         k_TBLcod = '01';
035900100415         k_TBLkey = %editc(CPScmm:'X');
036000100415         chain  %kds(K03tabel) TABEL00F;
036100100415         IF  %found(TABEL00F);
036200100415           ds01 = TBLuni;
036300100415         ENDIF;
036400100415         CPScmd = §01age;
036500100415         CPScmmi = CPScmm;
036600100415         CPScmdi = CPScmd;
036700100421         CPSfst = 'A' + %subst(w_CPSfst:1:1);
036800100421
036900100421       //?se data stato > data massima aggiungo 1 alla data massima
037000100421         IF  w_CPSdst > wDataMax;
037100100421           wData_ISO = %date(wDataMax:*iso);
037200100421           wData_ISO = wData_ISO + %days(1 + wNr);
037300100421           CPSdst = %dec(wData_ISO);
037400100421           wNr += 1;
037500100421         ELSE;
037600100421           CPSdst = w_CPSdst;
037700100421         ENDIF;
037800100421
037900100421         CPSclt = w_CPSclt;
038000100421         CPScnc = w_CPScnc;
038100100421         CPSksc = w_CPSksc;
038200100426         CPSnrv = w_CPSnrv;
038300100421         CPSdim = w_CPSdim;
038400100421         CPShim = w_CPShim;
038500100421         CPSpru = w_CPSpru;
038600100421         CPSfle = w_CPSfle;
038700100422         IF  filiale <> *blanks;
038800100422           CPSflo = 'B';
038900100422         ELSE;
039000100422           CPSflo = 'T';
039100100422         ENDIF;
039200100415         write TICPS000;
039300100415
039400100415       ENDSR;
039500100421
039600100421       //--------------------------------------------------------------
039700100421       //?Aggiorno vecchio file.
039800100421       //--------------------------------------------------------------
039900100421       BEGSR Agg_TNCPS;
040000100421
040100100421         chain rrncps TNCPS00F;
040200100421         IF  %found(TNCPS00F);
040300100421           s_CPSftr = 'S';
040400100421           update TNCPS000;
040500100421         ENDIF;
040600100421
040700100421       ENDSR;
040800100414
040900100414       //--------------------------------------------------------------
041000100414       //?Stampa errori.
041100100414       //--------------------------------------------------------------
041200100414       BEGSR sr_Stampa;
041300100414
041400100414         IF  not $Stampa;
041500100414           write CRMT01;
041600100414           $Stampa = *on;
041700100426           DESpgm = 'Scrive nuovi stati da vecchio TNCPS';
041800100426           write CRMT02;
041900100414         ENDIF;
042000100414
042100100414       ENDSR;
042200100413
042300100413       //--------------------------------------------------------------
042400100413       //?Operazioni finali.
042500100413       //--------------------------------------------------------------
042600100413       BEGSR sr_RoutEnd;
042700100413
042800100413         *inLR = *on;
042900100413         return;
043000100413
043100100413       ENDSR;
043200100413
043300100413      /end-free
