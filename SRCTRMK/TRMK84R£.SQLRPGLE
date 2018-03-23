000100090708      //---------------------------------------------------------------
000200091008      //?TRMK84R - Controlla agenda
000300090708      //---------------------------------------------------------------
000400090708
000500090708     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600090708
000700090708      //---------------------------------------------------------------
000800090708      //?Dichiarazione file.
000900090708      //---------------------------------------------------------------
001000090708
001100090708      //---------------------------------------------------------------
001200090511      //?Definizione costanti.
001300090511      //---------------------------------------------------------------
001400090511
001500090511      //---------------------------------------------------------------
001600090511      //?Definizione schiere.
001700090511      //---------------------------------------------------------------
001800090708
001900090708      // - Messaggi di errore
002000090708     d $Msg            s             78    dim(10) ctdata perrcd(1)
002100090721
002200090511      //---------------------------------------------------------------
002300090511      //?Definizione aree dati.
002400090511      //---------------------------------------------------------------
002500090511
002600090511      //---------------------------------------------------------------
002700090511      //?Definizione strutture dati.
002800090511      //---------------------------------------------------------------
002900090708
003000090708      // - Parametri ricevuti
003100091008     d TRMK84ds      e ds
003200091009
003300091009      // File tabelle
003400091009     d tabel00f      e ds                  extname(TABEL00F)
003500090709
003600091008      // File attività
003700091008     d tiatc00f      e ds                  extname(TIATC00F)
003800090511
003900090511      //---------------------------------------------------------------
004000090511      //?Definizione variabili globali.
004100090511      //---------------------------------------------------------------
004200090708
004300090708      // - Flags booleani
004400091009     d $Fine           s               n   inz(*off)
004500091009     d $Fine1          s               n   inz(*off)
004600090708
004700090708      // - Campi di comodo
004800091009     d ageuni          s                   like(Imk84Cmm)
004900091009     d $age            s              7
005000090511
005100090511      //---------------------------------------------------------------
005200090511      //?Definizione procedure esterne.
005300090511      //---------------------------------------------------------------
005400090708
005500090511      //---------------------------------------------------------------
005600090511      //?Definizione key-list.
005700090511      //---------------------------------------------------------------
005800090708
005900090708      //---------------------------------------------------------------
006000090708      //?Riepilogo indicatori.
006100090708      //---------------------------------------------------------------
006200090511
006300090511      //---------------------------------------------------------------
006400090708      //?M A I N - L I N E
006500090511      //---------------------------------------------------------------
006600090708
006700090708     c     *Entry        plist
006800091009     c                   parm                    trmk84ds
006900090708
007000090708      /free
007100090708
007200090708       //?Operazioni iniziali
007300090708       exsr sr_RoutInz;
007400091007
007500091007       //?Controllo dati passati
007600091007       exsr sr_CtrDati;
007700091007
007800091008       //?Verifico se ci altri impegni
007900091009       IF  Omk84Err = *blanks;
008000091008         exsr sr_CtrAgenda;
008100091007       ENDIF;
008200090708
008300090708       //?Operazioni finali
008400090708       exsr sr_RoutEnd;
008500090708
008600090708       //--------------------------------------------------------------
008700090708       //?Operazioni iniziali.
008800090708       //--------------------------------------------------------------
008900090708       BEGSR sr_RoutInz;
009000090708
009100091009         clear Omk84Err;
009200091009         clear Omk84Msg;
009300091009
009400090708         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
009500090708
009600090708       ENDSR;
009700090805
009800090805       //--------------------------------------------------------------
009900091007       //?Controllo dati passati.
010000090805       //--------------------------------------------------------------
010100091007       BEGSR sr_CtrDati;
010200090810
010300091009         IF  Imk84Cmm = 0 or Imk84Dad = 0 or Imk84Oii = 0 or
010400091009             Imk84Ofi = 0;
010500091009           Omk84Err = '9';
010600091009           Omk84Msg = $msg(01);
010700091009           leavesr;
010800090810         ENDIF;
010900091009
011000091009         IF  Imk84Tat <> *blanks and (Imk84Atn = 0 or Imk84Atnp = 0);
011100091009           Omk84Err = '9';
011200091009           Omk84Msg = $msg(01);
011300091009           leavesr;
011400091009         ENDIF;
011500091009
011600091009         IF  Imk84Atn > 0 and (Imk84Tat = *blanks or Imk84Atnp = 0);
011700091009           Omk84Err = '9';
011800091009           Omk84Msg = $msg(01);
011900091009           leavesr;
012000091009         ENDIF;
012100091009
012200091009         IF  Imk84Atnp > 0 and (Imk84Tat = *blanks or Imk84Atn = 0);
012300091009           Omk84Err = '9';
012400091009           Omk84Msg = $msg(01);
012500091009           leavesr;
012600091009         ENDIF;
012700090805
012800090805       ENDSR;
012900091007
013000091007       //--------------------------------------------------------------
013100091008       //?Controllo agenda.
013200091007       //--------------------------------------------------------------
013300091008       BEGSR sr_CtrAgenda;
013400091008
013500091009         //?recupero agente unificante del commerciale passato
013600091009         exec sql
013700091009         select substr(tbluni, 26, 7) into :$age from
013800091009         tabel00f where tblcod = '01' and tblkey = :Imk84Cmm;
013900091009         //?non trovo rcd vado a fine
014000091009         IF  sqlcod <> 0;
014100091009           leavesr;
014200091009         ENDIF;
014300091009
014400091009         ageuni = %dec($age:7:0);
014500091008
014600091009         //?Controllo agenda senza attività passata
014700091009         IF  Imk84Tat = *blanks;
014800091008           exec sql
014900091009           declare agenda cursor for
015000100524           select atccmm, atchda, atcoii, atcofi, atccad, atccad, atcest
015100100524           from tiatc00f where atccmm = :ageuni and atcdad = :Imk84dad
015200091014           and atcdco = 0;
015300091009
015400091009           exec sql open agenda;
015500091009
015600091009           DOW  not $Fine;
015700091014             exec sql fetch next from agenda into :atccmm, :atchda,
015800100524                                                  :atcoii, :atcofi,
015900100524                                                  :atccad, :atccac,
016000100524                                                  :atcest;
016100091009             IF  sqlcod = 100 or sqlcod < 0;
016200091009               $Fine = *on;
016300091009               leave;
016400091009             ENDIF;
016500091014
016600091014             //?imposto l'ora inizio e fine impegno se non impostate
016700091014             IF  atcoii = 0;
016800091014               atcoii = atchda / 100;
016900091014             ENDIF;
017000091014             IF  atcofi = 0;
017100091014               atcofi = atchda / 100;
017200091014             ENDIF;
017300100524
017400100524             //?CHIODO
017500100524             //?se l'attività che trovo è
017600100524             //?un appuntamento '91' con causale chiusura attività '24' e esito 'N'
017700100524             //?non devo tener conto dell'impegno perchè di fatto l'impegno non esiste
017800100524             IF  ATCcad = '91' and ATCcac = '24' and ATCest = 'N';
017900100524               iter;
018000100524             ENDIF;
018100100525             //?un affiancamento '92' con causale chiusura attività 'NO' e esito 'N'
018200100525             //?non devo tener conto dell'impegno perchè di fatto l'impegno non esiste
018300100525             IF  ATCcad = '92' and ATCcac = 'NO' and ATCest = 'N';
018400100525               iter;
018500100525             ENDIF;
018600100524             //?un'attività qualsiasi chiusa con attività 'AA'
018700100524             //?non devo tener conto dell'impegno perchè di fatto l'impegno non esiste
018800100524             IF  ATCcac = 'AA';
018900100524               iter;
019000100524             ENDIF;
019100091014
019200091009             //?trovo almeno un impegno ritorno messaggio
019300091014             IF  atcoii >= Imk84Oii and atcofi <= Imk84Ofi;
019400091014               Omk84Err = '1';
019500091014               Omk84Msg = $msg(02);
019600091014               $Fine = *on;
019700091014               leave;
019800091014             ENDIF;
019900091014             IF  Imk84Oii >= atcoii and Imk84Ofi <= atcofi;
020000091014               Omk84Err = '1';
020100091014               Omk84Msg = $msg(02);
020200091014               $Fine = *on;
020300091014               leave;
020400091014             ENDIF;
020500091014
020600091009           ENDDO;
020700091009
020800091009           exec sql close agenda;
020900091008         ENDIF;
021000091008
021100091009         //?Controllo agenda con attività passata
021200091009         IF  Imk84Tat <> *blanks;
021300091008           exec sql
021400091009           declare agenda1 cursor for
021500091014           select atccmm, atchda, atcoii, atcofi from
021600091014           tiatc00f where atccmm = :ageuni and atcdad = :Imk84dad
021700091014           and atcdco = 0 and atctat <> :Imk84Tat
021800091014           and atcatn <> :Imk84Atn and atcatnp <> :Imk84Atnp;
021900091009
022000091009           exec sql open agenda1;
022100091009
022200091009           DOW  not $Fine1;
022300091014             exec sql fetch next from agenda1 into :atccmm, :atchda,
022400091014                                                   :atcoii, :atcofi;
022500091009             IF  sqlcod = 100 or sqlcod < 0;
022600091009               $Fine1 = *on;
022700091009               leave;
022800091009             ENDIF;
022900091014
023000091014             //?imposto l'ora inizio e fine impegno se non impostate
023100091014             IF  atcoii = 0;
023200091014               atcoii = atchda / 100;
023300091014             ENDIF;
023400091014             IF  atcofi = 0;
023500091014               atcofi = atchda / 100;
023600091014             ENDIF;
023700091014
023800091009             //?trovo almeno un impegno ritorno messaggio
023900091014             IF  atcoii >= Imk84Oii and atcofi <= Imk84Ofi;
024000091014               Omk84Err = '1';
024100091014               Omk84Msg = $msg(02);
024200091014               $Fine1 = *on;
024300091014               leave;
024400091014             ENDIF;
024500091014             IF  Imk84Oii >= atcoii and Imk84Ofi <= atcofi;
024600091014               Omk84Err = '1';
024700091014               Omk84Msg = $msg(02);
024800091014               $Fine = *on;
024900091014               leave;
025000091014             ENDIF;
025100091014
025200091009           ENDDO;
025300091009
025400091009           exec sql close agenda1;
025500091008         ENDIF;
025600091007
025700091007       ENDSR;
025800090708
025900090708       //--------------------------------------------------------------
026000090708       //?Operazioni finali.
026100090708       //--------------------------------------------------------------
026200090708       BEGSR sr_RoutEnd;
026300090708
026400090708         *inLR = *on;
026500090708         return;
026600090708
026700090708       ENDSR;
026800090708
026900090708      /end-free
027000090708
027100090708       //--------------------------------------------------------------
027200090708       //?Schiere a tempo di compilazione.
027300090708       //--------------------------------------------------------------
027400090708
027500090708** - $MSG -------------------------------------------------------------------*
027600091014Dati non passati                                                              01
027700100310ATTENZIONE! Altre attività pianificate nel periodo.                           02
