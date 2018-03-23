000100080206      //--------------------------------------------------------------
000200140109      //?Reperimento unificazioni file fatture
000300080206      //--------------------------------------------------------------
000400080206
000500090407     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600090601     h dftactgrp(*no) actgrp(*caller)
000700080206
000800080206      //---------------------------------------------------------------
000900080206      //?Dichiarazione file.
001000080206      //---------------------------------------------------------------
001100110802
001200110615      // - File Tabelle
001300140109     fTABEL00F  if   e           k disk
001400080206
001500080206      //---------------------------------------------------------------
001600090406      //?Definizione costanti.
001700080206      //---------------------------------------------------------------
001800080207
001900100211     d Digits          c                   const('0123456789')
002000080206
002100080206      //---------------------------------------------------------------
002200080206      //?Definizione schiere.
002300080206      //---------------------------------------------------------------
002400110615
002500080206
002600080206      //---------------------------------------------------------------
002700080206      //?Definizione aree dati.
002800080206      //---------------------------------------------------------------
002900080206
003000080206
003100080206      //---------------------------------------------------------------
003200080206      //?Definizione strutture dati.
003300080206      //---------------------------------------------------------------
003400080206
003500080206      // - Status
003600080206     d Psds           sds
003700080206     d   SDSpgm          *proc
003800110809
003900110809      // - Tabella 4C
004000110809     d ds4C          e ds                  inz
004100100428
004200080206      //---------------------------------------------------------------
004300080206      //?Definizione variabili globali.
004400080206      //---------------------------------------------------------------
004500140109
004600140109      // - Parametri ricevuti
004700140109     d pIn_CodCli      s              7s 0
004800140109     d pOut_Esito      s              2s 0
004900140109      // legenda flag:
005000140109      //               01 = Cliente non passato
005100140109      //               02 = Cliente passato non trovato in tab. 4C
005200140109      //               03 = Cliente passato trovato in tab. 4C ma annullato
005300140109      //               04 = Cliente passato non unificato
005400140109      //               05 = Errore SQL
005500140109     d pOut_CliUni     s              7s 0
005600140109     d pOut_TotUti     s              5s 0
005700140109     d pOut_ListUti    s              7s 0 dim(500)
005800080206
005900080206      // - Flags booleani
006000140109     d wTest           s               n   inz(*off)
006100080206
006200080206      // - Indici di schiera
006300140109     d wX              s              4  0 inz
006400110802
006500110802      // - Stringa SQL da eseguire
006600110802     d wSQL            s           2048    Varying        inz
006700110808
006800080206      // - Campi di comodo
006900140109     d wOggi           s              8s 0 inz
007000140109     d wTBLKEY         s                   like(TBLKEY)
007100080208
007200090508      //---------------------------------------------------------------
007300090508      //?Definizione procedure usate.
007400090508      //---------------------------------------------------------------
007500080206
007600080626      //---------------------------------------------------------------
007700080626      //?prototipi
007800080626      //---------------------------------------------------------------
007900111027
008000100624
008100080206      //---------------------------------------------------------------
008200080206      //?Definizione key-list.
008300080206      //---------------------------------------------------------------
008400100720
008500100720       // - File TABEL00F
008600100720     d k03tabel      e ds                  extname(TABEL00F:*key)
008700100720     d                                     prefix(k_)
008800080206
008900080206      //---------------------------------------------------------------
009000080206
009100080206      //---------------------------------------------------------------
009200080206      //?M A I N - L I N E
009300080206      //---------------------------------------------------------------
009400080206
009500080206     c     *Entry        plist
009600140109     c                   parm                    pIn_CodCli
009700140109     c                   parm                    pOut_Esito
009800140109     c                   parm                    pOut_CliUni
009900140109     c                   parm                    pOut_TotUti
010000140109     c                   parm                    pOut_ListUti
010100080206
010200080206      /free
010300080206
010400100622       //?Operazioni iniziali
010500080206       exsr RoutInz;
010600080206
010700140109       //?Reperimento dati output
010800140109       exsr RepCliUni;
010900080206
011000100622       //?Operazioni finali
011100080206       exsr RoutEnd;
011200080206
011300080206       //--------------------------------------------------------------
011400080206       //?Operazioni iniziali.
011500080206       //--------------------------------------------------------------
011600080206       BEGSR RoutInz;
011700080206
011800140109       //?Pulisco parametri di output
011900140109         clear pOut_Esito;
012000140109         clear pOut_CliUni;
012100140109         clear pOut_TotUti;
012200140109         clear pOut_ListUti;
012300110411
012400110615       //?Se non ho il codice cliente esco dal pgm con errore al chiamante
012500140109         IF  pIn_CodCli = 0;
012600140109           pOut_Esito = 1;
012700140109           exsr RoutEnd;
012800110411         ENDIF;
012900110809
013000110809       //?Imposto la data del giorno
013100110809         wOggi = %dec(%date());
013200110802
013300110802         exec sql set option dynusrprf = *owner,
013400110802                             closqlcsr = *endmod;
013500090714
013600080206       ENDSR;
013700110809
013800110809       //--------------------------------------------------------------
013900140109       //?Reperisco cliente unificante da tab. 4C
014000110809       //--------------------------------------------------------------
014100140109       BEGSR RepCliUni;
014200110809
014300140109         k_TBLkut = 1;
014400140109         k_TBLcod = '4C';
014500140109         k_TBLkey = %editc(pIn_CodCli:'X');
014600110809         chain %kds(K03tabel) TABEL00F;
014700140109         // cliente non presente in tab. 4C
014800140109         IF  not %found(TABEL00F);
014900140109           pOut_Esito = 2;
015000140109           exsr RoutEnd;
015100110809         ENDIF;
015200140109         // cliente presente in tab. 4C ma annullato
015300140109         IF  TBLflg <> *blanks;
015400140109           pOut_Esito = 3;
015500140109           exsr RoutEnd;
015600140109         ENDIF;
015700110809
015800110809         ds4C = TBLuni;
015900110811
016000140109       //?Reperisco i clienti unificati dall'unificante legato al codice passato
016100140109         // cliente passato non unificato
016200110811         IF  §4Cfks = *zeros;
016300140109           pOut_Esito = 4;
016400140109           exsr RoutEnd;
016500140109         ELSE;
016600140109           pOut_CliUni = §4CFKS;
016700140109           exsr RepCliUti;
016800110811         ENDIF;
016900110809
017000110809       ENDSR ;
017100110809
017200110809       //--------------------------------------------------------------
017300140109       //?Reperisco totale e lista clienti unificati
017400110809       //--------------------------------------------------------------
017500140109       BEGSR RepCliUti;
017600140109
017700140109         // - Creazione stringa SQL da eseguire
017800140109         wSQL = 'select tblkey from tabel00f where tblcod = ''4C'' +
017900140109                 and substr(tbluni , 58 , 7)=' + %editc(pOut_CliUni:'X');
018000110809
018100140109         // - Dichiarazione del cursore
018200140109         exec sql  prepare S1  from :wSQL;
018300140109         exec sql  declare C1  cursor for S1;
018400140109
018500140109         // - Apertura del cursore
018600140109         exec sql   open C1;
018700140109
018800140109         // - Lettura cursore
018900140109         DoU  SQLCode = 100 or SQLCode < 0;
019000140109           exec sql  fetch next  from C1  into :wTBLKEY;
019100140109           select;
019200140109           // errore
019300140109           when SQLCode < 0;
019400140109             pOut_Esito = 5;
019500140109             // - Chiusura cursore (visto che esco con leavesr)
019600140109             exec sql   close C1;
019700140109             leavesr;
019800140109           // nessuna lettura
019900140109           when SQLCode = 100 and pOut_TotUti = 0;
020000140109             // non passo un esito particolare, il tot e la lista sono a 0 (impossibile perché
020100140109             //  c'è almeno il cliente passato)
020200140109           // lettura
020300140109           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
020400140109             pOut_TotUti = pOut_TotUti + 1;
020500140109             pOut_ListUti(pOut_TotUti) = %dec(%subst(wTBLKEY:1:7):7:0);
020600140109           // fine lettura
020700140109           other;
020800140109             // non faccio nulla
020900140109           endsl;
021000140109         EndDo;
021100140109
021200140109         // - Chiusura del cursore
021300140109         exec sql   close C1;
021400110809
021500110809       ENDSR;
021600110809
021700080206       //--------------------------------------------------------------
021800080206       //?Operazioni finali.
021900080206       //--------------------------------------------------------------
022000080206       BEGSR RoutEnd;
022100140109
022200080206         return;
022300080206
022400080206       ENDSR;
022500080206
022600080206      /end-free
022700100624
022800080206       //--------------------------------------------------------------
022900080206       //?Schiere a tempo di compilazione.
023000080206       //--------------------------------------------------------------
023100080206
