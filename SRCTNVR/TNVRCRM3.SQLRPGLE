000100100413      //---------------------------------------------------------------
000200100420      //?TNVRCRM3 - Da TNCPS a TICPS stati potenziali
000300100413      //---------------------------------------------------------------
000400100413
000500100413     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600100413
000700100413      //---------------------------------------------------------------
000800100413      //?Dichiarazione file.
000900100413      //---------------------------------------------------------------
001000100514     fCNACO16L  if   e           k disk
001100100415     fTABEL00F  if   e           k disk
001200100420     fTFNTC01L  if   e           k disk
001300100413     fTICPN00F  o    e             disk
001400100415     fTICPS00F  o    e             disk
001500100421     fTNCPS00F  uf   e             disk    prefix(s_)
001600100414     fTNVRCRMP  o    e             printer oflind(*in99)
001700100413
001800100413      //---------------------------------------------------------------
001900100413      //?Definizione costanti.
002000100413      //---------------------------------------------------------------
002100100413
002200100413      //---------------------------------------------------------------
002300100413      //?Definizione schiere.
002400100413      //---------------------------------------------------------------
002500100513
002600100513     d filiali         s              3a   dim(10)
002700100513     d $Pog            s              3  0 dim(2500)
002800090715
002900100413      //---------------------------------------------------------------
003000100413      //?Definizione aree dati.
003100100413      //---------------------------------------------------------------
003200100413
003300100413      //---------------------------------------------------------------
003400100413      //?Definizione strutture dati.
003500100413      //---------------------------------------------------------------
003600100415
003700100415     d KPJBA         e ds
003800100415
003900100415      // - Reperimento filiali
004000100415     d TRUL31DS      e ds
004100100415     d pog                    10    759  0 dim(250)
004200100415
004300100415      // - Aggiunge/Toglie gg/mm dalla data
004400100415     d XGIOLAVds     e ds                  inz
004500100421
004600100421      // Anagrafica potenziali
004700100421     d TNCPOds       e ds                  extname(TNCPO00F)
004800100413
004900100421      // Stati potenziali
005000100421     d TNCPSds       e ds                  extname(TNCPS00F)
005100100421     d                                     prefix(w_)
005200100415
005300100415      // - Tabella 01 = Codici Commerciali
005400100415     d ds01          e ds                  inz
005500100415
005600100413      //---------------------------------------------------------------
005700100413      //?Definizione variabili globali.
005800100413      //---------------------------------------------------------------
005900100413
006000100413      // - Flags booleani
006100100413     d $End            s               n   inz(*off)
006200100422     d $EndCps         s               n   inz(*off)
006300100413     d $Fine           s               n   inz(*off)
006400100517     d $Noaco          s               n   inz(*off)
006500100414     d $Stampa         s               n   inz(*off)
006600100421
006700100421      // - Indici di schiera
006800100513     d xx              s              4  0 inz
006900100513     d yy              s              4  0 inz
007000100513     d zz              s              4  0 inz
007100100420
007200100420       // - Stringa SQL da eseguire
007300100420     d wSQL            s           2048    Varying        inz
007400100413
007500100413      // - Campi di comodo
007600100517     d conta           s              2  0 inz
007700100421     d old_NTCntr      s                   like(NTCntr)
007800100421     d rrncps          s              9  0
007900100415     d sav_CPNpno      s                   like(CPNpno)
008000100514     d TotCod          s              7  0
008100100513     d TotCpo          s              7  0
008200100517     d TotCpsCod       s              7  0
008300100513     d TotCpsN         s              7  0
008400100416     d wData           s              8  0
008500100416     d wDataMax        s              8  0
008600100415     d wDataOggi       s              8  0
008700100414     d wData_ISO       s               d   datfmt(*iso)
008800100421     d wNr             s              2  0
008900100415     d wRiga           s              2  0
009000100413
009100100413      //---------------------------------------------------------------
009200100413      //?Definizione procedure esterne.
009300100413      //---------------------------------------------------------------
009400100413
009500100413      //---------------------------------------------------------------
009600100413      //?prototipi
009700100413      //---------------------------------------------------------------
009800100414
009900100413      /copy gaitrasrc/srcprotopr,trul31r
010000100413
010100100413      //---------------------------------------------------------------
010200100413      //?Definizione key-list.
010300100413      //---------------------------------------------------------------
010400100415
010500100415       // - File TABEL00F
010600100415     d k03tabel      e ds                  extname(TABEL00F:*key)
010700100415     d                                     prefix(k_)
010800100413
010900100413      //---------------------------------------------------------------
011000100413      //?Riepilogo indicatori.
011100100413      //---------------------------------------------------------------
011200100414
011300100414      // 99    : Salto pagina
011400100413
011500100413      //---------------------------------------------------------------
011600100413      //?M A I N - L I N E
011700100413      //---------------------------------------------------------------
011800100413
011900100413     c     *Entry        plist
012000100513     c                   parm                    filiali
012100100413
012200100413      /free
012300100413
012400100413       //?Operazioni iniziali
012500100513       exsr RoutInz;
012600100420
012700100420       //?Imposto la stringa per SQL
012800100513       exsr PrepSQL;
012900100413
013000100413       //?Scrivo i nuovi file per le trattative
013100100413       IF  not $Fine;
013200100513         exsr Elabora;
013300100413       ENDIF;
013400100413
013500100413       //?Operazioni finali
013600100513       exsr RoutEnd;
013700100413
013800100413       //--------------------------------------------------------------
013900100413       //?Operazioni iniziali.
014000100413       //--------------------------------------------------------------
014100100513       BEGSR RoutInz;
014200100513
014300100513         yy = 1;
014400100513
014500100513       //?Ciclo per filiali passate
014600100513         FOR  yy BY 1 to 10;
014700100514           IF  filiali(yy) <> *zeros and filiali(yy) <> *blanks;
014800100413
014900100513         //?Recupero filiali del 'RA' della filiale ricevuta
015000100513             clear TRUL31DS;
015100100513             I31abi = 'RA';
015200100513             I31cpo = %dec(filiali(yy):3:0);
015300100513             callp trul31r (kpjba : trul31ds);
015400100513             IF  O31pog <= *zeros;
015500100513               $Fine = *on;
015600100514               leavesr;
015700100513             ENDIF;
015800100513
015900100513         //?Memorizzo le filiali appena caricate in un'unica schiera
016000100514             zz = %lookup(*zeros:$Pog);
016100100514             %subarr($Pog:zz:250) = pog;
016200100513
016300100513           ENDIF;
016400100513
016500100513         ENDFOR;
016600100414
016700100414         wData_ISO = %date();
016800100415         wDataOggi = %dec(wData_ISO);
016900100420       //?Calcolo data limite massimo stati
017000100420         wDataMax = 20111231;
017100100413
017200100413         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
017300100416
017400100416       //?Impostazione campi "fissi"
017500100416         k_TBLkut = 1;
017600100513
017700100514         clear TotCod;
017800100513         clear TotCpo;
017900100517         clear TotCpsCod;
018000100513         clear TotCpsN;
018100100413
018200100413       ENDSR;
018300100420
018400100420       //--------------------------------------------------------------
018500100420       //?Preparazione stringa SQL.
018600100420       //--------------------------------------------------------------
018700100513       BEGSR PrepSQL;
018800100420
018900100420         wSQL  = 'select * from TNCPO00F where CPOatb = ';
019000100420         wSQL += '''' + ' ' + '''';
019100100513         wSQL += ' and  CPOflt in(';
019200100420
019300100513         yy = 0;
019400100513         xx = 1;
019500100513         FOR xx by 1 to %elem(Pog);
019600100513           IF $Pog(xx) <> *zeros;
019700100513             IF yy > 0;
019800100513               wSQL += ', ';
019900100513             ELSE;
020000100513               yy = 1;
020100100513             ENDIF;
020200100513             wSQL += %editc($Pog(xx):'X');
020300100513           ENDIF;
020400100513         ENDFOR;
020500100420
020600100421         wSQL += ') order by CPOcpo +
020700100420                   for fetch only';
020800100420
020900100420       ENDSR;
021000100413
021100100413       //--------------------------------------------------------------
021200100422       //?Elaboro.
021300100413       //--------------------------------------------------------------
021400100513       BEGSR Elabora;
021500100413
021600100413         $End = *off;
021700100420
021800100420       //?Preparazione del cursore
021900100420         exec sql PREPARE S1   from :wSQL;
022000100420       //?Dichiarazione del cursore
022100100420         exec sql DECLARE CPO  cursor for S1;
022200100420
022300100420       //?Apertura del cursore
022400100420         exec sql open CPO;
022500100420
022600100420         IF sqlcode < 0;
022700100420           $End = *on;
022800100420         ENDIF;
022900100420
023000100420         DOW not $End;
023100100420
023200100420           exec sql FETCH next from CPO into :TNCPOds;
023300100420
023400100420           IF sqlcod = 100 or sqlcod < 0;
023500100420             $End = *on;
023600100420             leave;
023700100420           ENDIF;
023800100513
023900100513           //?conto quanti potenziali ho preso in considerazione
024000100513           TotCpo += 1;
024100100514           //?conto quanti potenziali mai codificati
024200100517           $noaco = *off;
024300100517           chain CPOcpo CNACO16L;
024400100517           IF  not %found(CNACO16L);
024500100517             TotCod += 1;
024600100517             $noaco = *on;
024700100517           ENDIF;
024800100421
024900100421           //?scrivo le note dei potenziali
025000100421           exsr Scrivi_Note;
025100100420
025200100421           //?scrivo il nuovo file degli stati potenziali
025300100421           exsr Scrivi;
025400100413
025500100413         ENDDO;
025600100413
025700100420         exec sql CLOSE CPO;
025800100513
025900100513       //?Stampa totali
026000100513         exsr Stampa;
026100100413
026200100413       ENDSR;
026300100414
026400100414       //--------------------------------------------------------------
026500100414       //?Scrivo file note.
026600100414       //--------------------------------------------------------------
026700100414       BEGSR Scrivi_Note;
026800100421
026900100421         clear old_NTCntr;
027000100421         clear sav_CPNpno;
027100100415
027200100421         NTCapl = 'P';
027300100421         NTCnk1 = %editc(CPOcpo:'X');
027400100421         clear NTCnk2;
027500100421         NTCtnt = 'IN';
027600100421         setll  (NTCapl : NTCnk1 : NTCnk2 : NTCtnt) TFNTC01L;
027700100421         reade  (NTCapl : NTCnk1 : NTCnk2 : NTCtnt) TFNTC01L;
027800100414         DOW  not %eof(TFNTC01L);
027900100421         //?se giorno memorizzazione diverso azzero progressivo nuove note
028000100421           IF NTCntr <> old_NTCntr;
028100100421             clear sav_CPNpno;
028200100421             old_NTCntr = NTCntr;
028300100421           ENDIF;
028400100421         //?scrivo solo se data > 0
028500100421           IF  NTCntr > 0;
028600100421             exsr Scrivi_TICPN;
028700100421           ENDIF;
028800100421
028900100421           reade  (NTCapl : NTCnk1 : NTCnk2 : NTCtnt) TFNTC01L;
029000100414
029100100414         ENDDO;
029200100414
029300100414       ENDSR;
029400100421
029500100421       //--------------------------------------------------------------
029600100421       //?Scrivo il nuovo file stati.
029700100421       //--------------------------------------------------------------
029800100421       BEGSR Scrivi;
029900100421
030000100422         $EndCps = *off;
030100100421         //?contatore per aggiungere giorni a data massima
030200100421         clear wNr;
030300100517         clear conta;
030400100421
030500100421         exec sql
030600100421         DECLARE CPS cursor for
030700100421         SELECT  rrn(TNCPS00F), TNCPS00F.*
030800100421         FROM TNCPS00F
030900100421         WHERE CPScpo = :CPOcpo and CPSatb = ' '
031000100421         ORDER by CPSdst;
031100100421
031200100421         exec sql OPEN CPS;
031300100421
031400100422         DOW  not $EndCps;
031500100421           exec sql FETCH next from CPS into :rrncps, :TNCPSds;
031600100421           IF  sqlcod = 100 or sqlcod < 0;
031700100422             $EndCps = *on;
031800100421             leave;
031900100421           ENDIF;
032000100421         //?scrivo il nuovo file
032100100421           exsr Scrivi_TICPS;
032200100421         //?fleggo il vecchio file come già codificato
032300100421           exsr Agg_TNCPS;
032400100421
032500100421         ENDDO;
032600100421
032700100421         exec sql CLOSE CPS;
032800100421
032900100421       ENDSR;
033000100421
033100100421       //--------------------------------------------------------------
033200100421       //?Scrivo file TICPN.
033300100421       //--------------------------------------------------------------
033400100421       BEGSR Scrivi_TICPN;
033500100421
033600100421         clear TICPN000;
033700100421         CPNcpo = CPOcpo;
033800100421       //?Costruisco la data lunga 8 da data lunga 6
033900100421         IF  NTCntr  < 000101;
034000100421           wData = NTCntr + 19000000;
034100100421         ELSE;
034200100421           wData = NTCntr + 20000000;
034300100421         ENDIF;
034400100421         CPNdim = wData;
034500100421         CPNhim = 235800;
034600100421         CPNsns = 'S';
034700100421         CPNnot = NTCrnt;
034800100421         CPNpno = sav_CPNpno + 1;
034900100503         CPNpos = 046;
035000100503         CPNpru = 'BATCH';
035100100421         write TICPN000;
035200100421         sav_CPNpno = CPNpno;
035300100421
035400100421       ENDSR;
035500100415
035600100415       //--------------------------------------------------------------
035700100415       //?Scrivo file TICPS.
035800100415       //--------------------------------------------------------------
035900100415       BEGSR Scrivi_TICPS;
036000100519
036100100519       //?se commerciale stato a 0 e commerciale potenziale a 0
036200100519       //?non scrivo il nuovo stato
036300100519         IF  w_CPScmm = 0 and CPOcmm = 0;
036400100519           leavesr;
036500100519         ENDIF;
036600100415
036700100415         clear TICPS000;
036800100421         CPScpo = w_CPScpo;
036900100421         CPScmm = w_CPScmm;
037000100519       //?se commerciale stato a 0 e commerciale potenziale > 0
037100100519       //?scrivo il nuovo stato con commerciale potenziale
037200100519         IF  w_CPScmm = 0 and CPOcmm > 0;
037300100519           CPScmm = CPOcmm;
037400100519         ENDIF;
037500100415         clear ds01;
037600100419         k_TBLcod = '01';
037700100415         k_TBLkey = %editc(CPScmm:'X');
037800100415         chain  %kds(K03tabel) TABEL00F;
037900100415         IF  %found(TABEL00F);
038000100415           ds01 = TBLuni;
038100100415         ENDIF;
038200100415         CPScmd = §01age;
038300100415         CPScmmi = CPScmm;
038400100415         CPScmdi = CPScmd;
038500100421         CPSfst = 'A' + %subst(w_CPSfst:1:1);
038600100421
038700100421       //?se data stato > data massima aggiungo 1 alla data massima
038800100421         IF  w_CPSdst > wDataMax;
038900100421           wData_ISO = %date(wDataMax:*iso);
039000100421           wData_ISO = wData_ISO + %days(1 + wNr);
039100100421           CPSdst = %dec(wData_ISO);
039200100421           wNr += 1;
039300100421         ELSE;
039400100421           CPSdst = w_CPSdst;
039500100421         ENDIF;
039600100421
039700100421         CPSclt = w_CPSclt;
039800100421         CPScnc = w_CPScnc;
039900100421         CPSksc = w_CPSksc;
040000100426         CPSnrv = w_CPSnrv;
040100100421         CPSdim = w_CPSdim;
040200100421         CPShim = w_CPShim;
040300100421         CPSpru = w_CPSpru;
040400100421         CPSfle = w_CPSfle;
040500100513         CPSflo = %editc(wDataOggi:'X');
040600100415         write TICPS000;
040700100519
040800100519        //?conto quanti stati ho trascodificato
040900100519         TotCpsN += 1;
041000100519
041100100519        //?conto quanti mai codificati con stati
041200100519         IF  $Noaco and conta = 0;
041300100519           TotCpsCod += 1;
041400100519           conta += 1;
041500100519         ENDIF;
041600100415
041700100415       ENDSR;
041800100421
041900100421       //--------------------------------------------------------------
042000100421       //?Aggiorno vecchio file.
042100100421       //--------------------------------------------------------------
042200100421       BEGSR Agg_TNCPS;
042300100421
042400100421         chain rrncps TNCPS00F;
042500100421         IF  %found(TNCPS00F);
042600100421           s_CPSftr = 'S';
042700100421           update TNCPS000;
042800100421         ENDIF;
042900100421
043000100421       ENDSR;
043100100414
043200100414       //--------------------------------------------------------------
043300100414       //?Stampa errori.
043400100414       //--------------------------------------------------------------
043500100513       BEGSR Stampa;
043600100414
043700100513         write CRMT01;
043800100513         DESpgm = 'Scrive nuovi stati da vecchio TNCPS';
043900100517         fil01 = %dec(Filiali(01):3:0);
044000100517         fil02 = %dec(Filiali(02):3:0);
044100100517         fil03 = %dec(Filiali(03):3:0);
044200100517         fil04 = %dec(Filiali(04):3:0);
044300100517         fil05 = %dec(Filiali(05):3:0);
044400100517         fil06 = %dec(Filiali(06):3:0);
044500100517         fil07 = %dec(Filiali(07):3:0);
044600100517         fil08 = %dec(Filiali(08):3:0);
044700100517         fil09 = %dec(Filiali(09):3:0);
044800100517         fil10 = %dec(Filiali(10):3:0);
044900100513         write CRMT02;
045000100513         write CRMPOT;
045100100414
045200100414       ENDSR;
045300100413
045400100413       //--------------------------------------------------------------
045500100413       //?Operazioni finali.
045600100413       //--------------------------------------------------------------
045700100513       BEGSR RoutEnd;
045800100413
045900100413         *inLR = *on;
046000100413         return;
046100100413
046200100413       ENDSR;
046300100413
046400100413      /end-free
