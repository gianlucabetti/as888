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
009700140109     c                   parm                    pOut_CliUni
009800140109     c                   parm                    pOut_TotUti
009900140109     c                   parm                    pOut_ListUti
010000080206
010100080206      /free
010200080206
010300100622       //?Operazioni iniziali
010400080206       exsr RoutInz;
010500080206
010600140109       //?Reperimento dati output
010700140109       exsr RepCliUni;
010800080206
010900100622       //?Operazioni finali
011000080206       exsr RoutEnd;
011100080206
011200080206       //--------------------------------------------------------------
011300080206       //?Operazioni iniziali.
011400080206       //--------------------------------------------------------------
011500080206       BEGSR RoutInz;
011600080206
011700140109       //?Pulisco parametri di output
011800140109         clear pOut_CliUni;
011900140109         clear pOut_TotUti;
012000140109         clear pOut_ListUti;
012100110411
012200110615       //?Se non ho il codice cliente esco dal pgm con errore al chiamante
012300140109         IF  pIn_CodCli = 0;
012400140109           pOut_Esito = 1;
012500140109           exsr RoutEnd;
012600110411         ENDIF;
012700110809
012800110809       //?Imposto la data del giorno
012900110809         wOggi = %dec(%date());
013000110802
013100110802         exec sql set option dynusrprf = *owner,
013200110802                             closqlcsr = *endmod;
013300090714
013400080206       ENDSR;
013500110809
013600110809       //--------------------------------------------------------------
013700140109       //?Reperisco cliente unificante da tab. 4C
013800110809       //--------------------------------------------------------------
013900140109       BEGSR RepCliUni;
014000110809
014100140109         k_TBLcod = '4C';
014200140109         k_TBLkey = %editc(pIn_CodCli:'X');
014300110809         chain %kds(K03tabel) TABEL00F;
014400140109         // cliente non presente in tab. 4C
014500140109         IF  not %found(TABEL00F);
014600140109           pOut_Esito = 2;
014700140109           exsr RoutEnd;
014800110809         ENDIF;
014900140109         // cliente presente in tab. 4C ma annullato
015000140109         IF  TBLflg <> *blanks;
015100140109           pOut_Esito = 3;
015200140109           exsr RoutEnd;
015300140109         ENDIF;
015400110809
015500110809         ds4C = TBLuni;
015600110811
015700140109       //?Reperisco i clienti unificati dall'unificante legato al codice passato
015800140109         // cliente passato non unificato
015900110811         IF  §4Cfks = *zeros;
016000140109           pOut_Esito = 4;
016100140109           exsr RoutEnd;
016200140109         ELSE;
016300140109           pOut_CliUni = $4CFKS;
016400140109           exsr RepCliUti;
016500110811         ENDIF;
016600110809
016700110809       ENDSR ;
016800110809
016900110809       //--------------------------------------------------------------
017000140109       //?Reperisco totale e lista clienti unificati
017100110809       //--------------------------------------------------------------
017200140109       BEGSR RepCliUti;
017300140109
017400140109         // - Creazione stringa SQL da eseguire
017500140109         wSQL = 'select tblkey from tabel00f where tblcod = ''4C'' +
017600140109                 and substr(tbluni , 58 , 7)=' + %editc(pOut_CliUni:'X');
017700110809
017800140109         // - Dichiarazione del cursore
017900140109         exec sql  prepare S1  from :wSQL;
018000140109         exec sql  declare C1  cursor for S1;
018100140109
018200140109         // - Apertura del cursore
018300140109         exec sql   open C1;
018400140109
018500140109         // - Lettura cursore
018600140109         DoU  SQLCode = 100 or SQLCode < 0;
018700140109           exec sql  fetch next  from C1  into :wTBLKEY;
018800140109           select;
018900140109           // errore
019000140109           when SQLCode < 0;
019100140109             pOut_Esito = 5;
019200140109             // - Chiusura cursore (visto che esco con leavesr)
019300140109             exec sql   close C1;
019400140109             leavesr;
019500140109           // nessuna lettura
019600140109           when SQLCode = 100 and pOut_TotUti = 0;
019700140109             // non passo un esito particolare, il tot e la lista sono a 0 (impossibile perché
019800140109             //  c'è almeno il cliente passato)
019900140109           // lettura
020000140109           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
020100140109             pOut_TotUti = pOut_TotUti + 1;
020200140109             pOut_CliUti(pOut_TotUti) = %dec(%subst(wTBLKEY:1:7):7:0);
020300140109           // fine lettura
020400140109           other;
020500140109             // non faccio nulla
020600140109           endsl;
020700140109         EndDo;
020800140109
020900140109         // - Chiusura del cursore
021000140109         exec sql   close C1;
021100110809
021200110809       ENDSR;
021300110809
021400080206       //--------------------------------------------------------------
021500080206       //?Operazioni finali.
021600080206       //--------------------------------------------------------------
021700080206       BEGSR RoutEnd;
021800140109
021900080206         return;
022000080206
022100080206       ENDSR;
022200080206
022300080206      /end-free
022400100624
022500080206       //--------------------------------------------------------------
022600080206       //?Schiere a tempo di compilazione.
022700080206       //--------------------------------------------------------------
022800080206
