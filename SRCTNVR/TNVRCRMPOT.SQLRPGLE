000100100413      //---------------------------------------------------------------
000200100422      //?TNVRCRMPOT - Pulizia potenziali
000300100413      //---------------------------------------------------------------
000400100413
000500100413     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600100413
000700100413      //---------------------------------------------------------------
000800100413      //?Dichiarazione file.
000900100413      //---------------------------------------------------------------
001000100422     fCNACO16L  if   e           k disk
001100100423     fTFACO16L  if   e           k disk    rename(CNACO000:TFACO)
001200100423     fTFNTC01L  uf   e           k disk
001300100423     fTFNTC01S  uf a e           k disk    rename(TFNTC:TFNTC0S)
001400100423     fTICPI01L  uf   e           k disk
001500100423     fTICPI01S  uf a e           k disk    rename(TICPI000:TICPI0S)
001600100423     fTNCPO01L  uf   e           k disk
001700100422     fTNCPO01S  uf a e           k disk    rename(TNCPO000:TNCPO0S)
001800100422     fTNCPS11L  uf   e           k disk
001900100422     fTNCPS01S  uf a e           k disk    rename(TNCPS000:TNCPS0S)
002000100422     fTNVIS05L  if   e           k disk    prefix(w_)
002100100428     fTNVRCRMPP o    e             printer oflind(*in99)
002200100413
002300100413      //---------------------------------------------------------------
002400100413      //?Definizione costanti.
002500100413      //---------------------------------------------------------------
002600100413
002700100413      //---------------------------------------------------------------
002800100413      //?Definizione schiere.
002900100413      //---------------------------------------------------------------
003000090715
003100100413      //---------------------------------------------------------------
003200100413      //?Definizione aree dati.
003300100413      //---------------------------------------------------------------
003400100413
003500100413      //---------------------------------------------------------------
003600100413      //?Definizione strutture dati.
003700100413      //---------------------------------------------------------------
003800100415
003900100415     d KPJBA         e ds
004000100423
004100100423      // File trattative
004200100423     d TIVISds       e ds                  extname(TIVIS00F)
004300100423
004400100423      // Anagrafica potenziali
004500100423     d TNCPOds       e ds                  extname(TNCPO00F)
004600100423     d                                     prefix(o_)
004700100423
004800100423      // - Reperimento filiali
004900100423     d TRUL31DS      e ds
005000100423     d pog                    10    759  0 dim(250)
005100100415
005200100413      //---------------------------------------------------------------
005300100413      //?Definizione variabili globali.
005400100413      //---------------------------------------------------------------
005500100413
005600100413      // - Flags booleani
005700100423     d $Del            s               n   inz(*off)
005800100413     d $End            s               n   inz(*off)
005900100423     d $EndTratta      s               n   inz(*off)
006000100413     d $Fine           s               n   inz(*off)
006100100428     d $Stampa         s               n   inz(*off)
006200100423
006300100423      // - Indici di schiera
006400100423     d xx              s              3  0 inz
006500100423     d yy              s              3  0 inz
006600100413
006700100413      // - Parametro ricevuto
006800100413     d filiale         s              3
006900100423
007000100426      // - Stringa SQL da eseguire
007100100423     d wSQL            s           2048    Varying        inz
007200100413
007300100413      // - Campi di comodo
007400100423     d conta           s              5  0
007500100423     d k_NTCapl        s                   like(NTCapl)
007600100423     d k_NTCnk1        s                   like(NTCnk1)
007700100413
007800100413      //---------------------------------------------------------------
007900100413      //?Definizione procedure esterne.
008000100413      //---------------------------------------------------------------
008100100413
008200100413      //---------------------------------------------------------------
008300100413      //?prototipi
008400100413      //---------------------------------------------------------------
008500100423
008600100423      /copy gaitrasrc/srcprotopr,trul31r
008700100413
008800100413      //---------------------------------------------------------------
008900100413      //?Definizione key-list.
009000100413      //---------------------------------------------------------------
009100100413
009200100413      //---------------------------------------------------------------
009300100413      //?Riepilogo indicatori.
009400100413      //---------------------------------------------------------------
009500100414
009600100414      // 99    : Salto pagina
009700100413
009800100413      //---------------------------------------------------------------
009900100413      //?M A I N - L I N E
010000100413      //---------------------------------------------------------------
010100100413
010200100413     c     *Entry        plist
010300100413     c                   parm                    filiale
010400100413
010500100413      /free
010600100413
010700100413       //?Operazioni iniziali
010800100413       exsr sr_RoutInz;
010900100426
011000100426       //?Pulisco campi di stampa
011100100426       clear TOTpot;
011200100428       clear TOTdel;
011300100426       clear TOTaco;
011400100428       clear TOTvis;
011500100426       clear TOTdeb;
011600100428       clear TOTpul;
011700100413
011800100422       //?Pulizia potenziali
011900100423       IF  not $Fine;
012000100423       //?Se ho ricevuto la filiale devo elaborare solo quella
012100100423         IF  filiale <> *blanks;
012200100423           exsr sr_x_Fil;
012300100423       //?Se non ho ricevuto la filiale devo elaborare tutti i potenziali
012400100423         ELSE;
012500100423           exsr sr_x_All;
012600100423         ENDIF;
012700100423       ENDIF;
012800100426
012900100428       //?Stampa totali
013000100428       IF  *in99;
013100100428         write CRMT01P;
013200100428         write CRMT02P;
013300100428         *in99 = *off;
013400100428       ENDIF;
013500100428
013600100428       write CRMTOTP;
013700100413
013800100413       //?Operazioni finali
013900100413       exsr sr_RoutEnd;
014000100413
014100100413       //--------------------------------------------------------------
014200100413       //?Operazioni iniziali.
014300100413       //--------------------------------------------------------------
014400100413       BEGSR sr_RoutInz;
014500100423
014600100423       //?Recupero filiali del 'RA' della filiale ricevuta
014700100423         IF  filiale <> *blanks;
014800100423           clear TRUL31DS;
014900100423           I31abi = 'RA';
015000100423           I31cpo = %dec(filiale:3:0);
015100100423           callp trul31r (kpjba : trul31ds);
015200100423           IF  O31pog <= *zeros;
015300100423             $Fine = *on;
015400100423           ENDIF;
015500100423         ENDIF;
015600100413
015700100413       ENDSR;
015800100423
015900100423       //--------------------------------------------------------------
016000100423       //?Elaboro per filiale.
016100100423       //--------------------------------------------------------------
016200100423       BEGSR sr_x_Fil;
016300100423
016400100423       //?Imposto la stringa per SQL
016500100423         exsr sr_PrepSQL;
016600100423
016700100423         $End = *off;
016800100423
016900100423       //?Preparazione del cursore
017000100423         exec sql PREPARE S1   from :wSQL;
017100100423       //?Dichiarazione del cursore
017200100423         exec sql DECLARE FIL  cursor for S1;
017300100423
017400100423       //?Apertura del cursore
017500100423         exec sql open FIL;
017600100423
017700100423         IF sqlcode < 0;
017800100423           $End = *on;
017900100423         ENDIF;
018000100423
018100100423         DOW not $End;
018200100423
018300100423           exec sql FETCH next from FIL into :TNCPOds;
018400100423
018500100423           IF sqlcod = 100 or sqlcod < 0;
018600100423             $End = *on;
018700100423             leave;
018800100423           ENDIF;
018900100423
019000100423           exsr Elabora;
019100100426
019200100428       //?conto i potenziali elaborati dell'area
019300100426           TOTpot += 1;
019400100423
019500100423         ENDDO;
019600100423
019700100423         exec sql CLOSE FIL;
019800100423
019900100423       ENDSR;
020000100420
020100100413       //--------------------------------------------------------------
020200100422       //?Elaboro.
020300100413       //--------------------------------------------------------------
020400100423       BEGSR sr_x_All;
020500100423
020600100423         $End = *off;
020700100423
020800100423       //?Dichiarazione del cursore
020900100423         exec sql
021000100423         DECLARE ALL cursor for
021100100423         SELECT TNCPO00F.*
021200100423         FROM TNCPO00F
021300100423         WHERE CPOatb = ' '
021400100423         ORDER by CPOcpo;
021500100423
021600100423       //?Apertura del cursore
021700100423         exec sql open ALL;
021800100423
021900100423         IF sqlcode < 0;
022000100423           $End = *on;
022100100423         ENDIF;
022200100423
022300100423         DOW not $End;
022400100423
022500100423           exec sql FETCH next from ALL into :TNCPOds;
022600100423
022700100423           IF sqlcod = 100 or sqlcod < 0;
022800100423             $End = *on;
022900100423             leave;
023000100423           ENDIF;
023100100423
023200100423           exsr Elabora;
023300100426
023400100428       //?conto i potenziali elaborati
023500100426           TOTpot += 1;
023600100423
023700100423         ENDDO;
023800100423
023900100423         exec sql CLOSE ALL;
024000100413
024100100413       ENDSR;
024200100423
024300100423       //--------------------------------------------------------------
024400100423       //?Preparazione stringa SQL.
024500100423       //--------------------------------------------------------------
024600100423       BEGSR sr_PrepSQL;
024700100423
024800100423         wSQL  = 'select * from TNCPO00F where CPOatb = ';
024900100423         wSQL += '''' + ' ' + '''';
025000100423
025100100423       //?Se passata la filiale elaboro filiale IN sk pog
025200100423         wSQL += ' and  CPOflt in(';
025300100423
025400100423         yy = 0;
025500100423         xx = 1;
025600100423         FOR xx by 1 to %elem(Pog);
025700100423           IF Pog(xx) <> *zeros;
025800100423             IF yy > 0;
025900100423               wSQL += ', ';
026000100423             ELSE;
026100100423               yy = 1;
026200100423             ENDIF;
026300100423             wSQL += %editc(Pog(xx):'X');
026400100423           ENDIF;
026500100423         ENDFOR;
026600100423
026700100423         wSQL += ') order by CPOcpo +
026800100423                   for fetch only';
026900100423
027000100423       ENDSR;
027100100423
027200100423       //--------------------------------------------------------------
027300100423       //?Elaboro il record.
027400100423       //--------------------------------------------------------------
027500100423       BEGSR Elabora;
027600100423
027700100423         $Del = *off;
027800100428
027900100428       //?come prima cosa controllo se potenziale pulibile in base allo stato
028000100428         exsr CtrStato;
028100100428       //?non è pulibile esco dal giro
028200100428         IF  not $Del;
028300100428           leavesr;
028400100428         ENDIF;
028500100428
028600100428       //?conto i potenziali pulibili
028700100428         TOTdel += 1;
028800100426
028900100428       //?se potenziale codificato non pulisco e conto
029000100426         chain o_CPOcpo CNACO16L;
029100100426         IF  %found(CNACO16L);
029200100426           TOTaco += 1;
029300100426           leavesr;
029400100426         ENDIF;
029500100423
029600100423       //?se potenziale presente su anagrafica provvisoria
029700100423         setll o_CPOcpo TFACO16L;
029800100423         reade o_CPOcpo TFACO16L;
029900100423         DOW  not %eof(TFACO16L);
030000100428         //?controllo se c'è visita aperta se c'è non pulisco e conto
030100100423           exsr CtrVisita;
030200100423           IF  not $Del;
030300100426             TOTvis += 1;
030400100423             leavesr;
030500100423           ENDIF;
030600100423           reade o_CPOcpo TFACO16L;
030700100423         ENDDO;
030800100423
030900100428       //?controllo se ci sono trattative aperte se ce ne sono non pulisco e conto
031000100423         exsr CtrTrattative;
031100100423         IF  not $Del;
031200100426           TOTvis += 1;
031300100423           leavesr;
031400100423         ENDIF;
031500100428
031600100428       //?se potenziale SEDE D&B non pulisco e conto
031700100428         IF  o_CPOfsf = 'S' and o_CPOduns <> *blanks;
031800100428           TOTdeb += 1;
031900100428           leavesr;
032000100428         ENDIF;
032100100423
032200100428       //?se arrivo qua il potenziale si può cancellare e conto
032300100428       //?stampo il potenziale che andrò a pulire
032400100428         TOTpul += 1;
032500100428         exsr sr_Stampa;
032600100423         exsr DelStorico;
032700100423         exsr Elimina;
032800100423
032900100423       ENDSR;
033000100428
033100100428       //--------------------------------------------------------------
033200100428       //?Controllo l'ultimo stato del potenziale.
033300100428       //--------------------------------------------------------------
033400100428       BEGSR CtrStato;
033500100428
033600100428         $Del = *off;
033700100428
033800100428         chain o_CPOcpo TNCPS11L;
033900100428         IF  not %found(TNCPS11L);
034000100428           leavesr;
034100100428         ENDIF;
034200100428
034300100428         IF  CPSfst = '6 ' and
034400100428            (CPScnc = '01' or CPScnc = '07' or CPScnc = '10' or
034500100428             CPScnc = '13' or CPScnc = '14');
034600100428           $Del = *on;
034700100428         ENDIF;
034800100428
034900100428         IF  CPSfst = '7 ' or CPSfst = '8 ';
035000100428           $Del = *on;
035100100428         ENDIF;
035200100428
035300100428         IF  CPSfst = '9 ' and
035400100428            (CPScnc = '01' or CPScnc = '07' or CPScnc = '10' or
035500100428             CPScnc = '13' or CPScnc = '14');
035600100428           $Del = *on;
035700100428         ENDIF;
035800100428
035900100428       ENDSR;
036000100422
036100100422       //--------------------------------------------------------------
036200100422       //?Controllo se potenziale con visita aperta.
036300100422       //--------------------------------------------------------------
036400100423       BEGSR CtrVisita;
036500100428
036600100428         $Del = *off;
036700100422
036800100422       //?controllo se visita aperta su vecchio archivio
036900100423       //?deve essere dell'area se passata la filiale
037000100423       //?se aperta ma più vecchia del 01/01/2010 è come se fosse chiusa
037100100422         chain ACOksc TNVIS05L;
037200100423       //?non trovo la visita si può pulire il potenziale
037300100423         IF  not %found(TNVIS05L);
037400100423           $Del = *on;
037500100423           leavesr;
037600100423         ENDIF;
037700100423
037800100423       //?se chiusa ok posso pulire il potenzile
037900100423         IF  w_VISfsv = 'C';
038000100423           $Del = *on;
038100100423           leavesr;
038200100423         ENDIF;
038300100428
038400100428       //?richiesta la filiale
038500100428       //?controllo se il commerciale visita fa parte dell'area
038600100428       //?se non fa parte dell'area non pulisco
038700100428         IF  filiale <> *blanks and
038800100428             %lookup(%dec(%subst(%editc(w_VIScmm:'X'):1:3):3:0):pog) = 0;
038900100428           leavesr;
039000100428         ENDIF;
039100100423
039200100423       //?se aperta ma più vecchia del 01/01/2010 è come se fosse chiusa
039300100423         IF  w_VISdvi < 20100101;
039400100423           $Del = *on;
039500100423           leavesr;
039600100423         ENDIF;
039700100422
039800100422       ENDSR;
039900100423
040000100423       //--------------------------------------------------------------
040100100423       //?Controllo se potenziale con trattative aperta.
040200100423       //--------------------------------------------------------------
040300100423       BEGSR CtrTrattative;
040400100423
040500100423         $EndTratta = *off;
040600100423         $Del = *off;
040700100423         clear conta;
040800100423
040900100423       //?conto quante trattative aperte ci sono per il potenziale
041000100423       //?se zero trattative posso pulirlo
041100100423         exec sql
041200100423         SELECT count(*) into :conta
041300100423         FROM TIVIS00F
041400100423         WHERE VIScpo = :o_CPOcpo and VISdch = 0;
041500100423         IF conta = *zeros;
041600100423           $Del = *on;
041700100423           leavesr;
041800100423         ENDIF;
041900100423
042000100423       ENDSR;
042100100428
042200100428       //--------------------------------------------------------------
042300100428       //?Stampa potenziali cancellati
042400100428       //--------------------------------------------------------------
042500100428       BEGSR sr_Stampa;
042600100428
042700100428         IF  not $Stampa or *in99;
042800100428           write CRMT01P;
042900100428           write CRMT02P;
043000100428           $Stampa = *on;
043100100428           *in99 = *off;
043200100428         ENDIF;
043300100428
043400100428         write CRMD01P;
043500100428
043600100428       ENDSR;
043700100428
043800100423       //--------------------------------------------------------------
043900100423       //?Cancello lo storico eventualmente già presente sul potenziale.
044000100423       //--------------------------------------------------------------
044100100423       BEGSR DelStorico;
044200100423
044300100423         setll o_CPOcpo TNCPS01S;
044400100423         reade o_CPOcpo TNCPS01S;
044500100423         DOW  not %eof(TNCPS01S);
044600100423           delete TNCPS0S;
044700100423           reade o_CPOcpo TNCPS01S;
044800100423         ENDDO;
044900100423
045000100423         setll o_CPOcpo TICPI01S;
045100100423         reade o_CPOcpo TICPI01S;
045200100423         DOW  not %eof(TICPI01S);
045300100423           delete TICPI0S;
045400100423           reade o_CPOcpo TICPI01S;
045500100423         ENDDO;
045600100423
045700100423         k_NTCapl = 'P';
045800100423         k_NTCnk1 = %editc(o_CPOcpo:'X');
045900100423         setll (k_NTCapl : k_NTCnk1) TFNTC01S;
046000100423         reade (k_NTCapl : k_NTCnk1) TFNTC01S;
046100100423         DOW  not %eof(TFNTC01S);
046200100423           delete TFNTC0S;
046300100423           reade (k_NTCapl : k_NTCnk1) TFNTC01S;
046400100423         ENDDO;
046500100423
046600100423         chain o_CPOcpo TNCPO01S;
046700100423         IF  %found(TNCPO01S);
046800100423           delete TNCPO0S;
046900100423         ENDIF;
047000100423
047100100423       ENDSR;
047200100423
047300100423       //--------------------------------------------------------------
047400100423       //?Elimina dati potenziale.
047500100423       //--------------------------------------------------------------
047600100423       BEGSR Elimina;
047700100423
047800100423         setll o_CPOcpo TNCPS11L;
047900100423         reade o_CPOcpo TNCPS11L;
048000100423         DOW  not %eof(TNCPS11L);
048100100423           write  TNCPS0S;
048200100423           delete TNCPS000;
048300100423           reade o_CPOcpo TNCPS11L;
048400100423         ENDDO;
048500100423
048600100423         setll o_CPOcpo TICPI01L;
048700100423         reade o_CPOcpo TICPI01L;
048800100423         DOW  not %eof(TICPI01L);
048900100423           write  TICPI0S;
049000100423           delete TICPI000;
049100100423           reade o_CPOcpo TICPI01L;
049200100423         ENDDO;
049300100423
049400100423         k_NTCapl = 'P';
049500100423         k_NTCnk1 = %editc(o_CPOcpo:'X');
049600100423         setll (k_NTCapl : k_NTCnk1) TFNTC01L;
049700100423         reade (k_NTCapl : k_NTCnk1) TFNTC01L;
049800100423         DOW  not %eof(TFNTC01L);
049900100423           write  TFNTC0S;
050000100423           delete TFNTC;
050100100423           reade (k_NTCapl : k_NTCnk1) TFNTC01L;
050200100423         ENDDO;
050300100423
050400100423         chain o_CPOcpo TNCPO01L;
050500100423         IF  %found(TNCPO01L);
050600100423           write  TNCPO0S;
050700100423           delete TNCPO000;
050800100423         ENDIF;
050900100423
051000100423       ENDSR;
051100100426
051200100413       //--------------------------------------------------------------
051300100413       //?Operazioni finali.
051400100413       //--------------------------------------------------------------
051500100413       BEGSR sr_RoutEnd;
051600100413
051700100413         *inLR = *on;
051800100413         return;
051900100413
052000100413       ENDSR;
052100100413
052200100413      /end-free
