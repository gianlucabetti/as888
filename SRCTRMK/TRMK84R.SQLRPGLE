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
003200090709
003300091008      // File attività
003400091008     d tiatc00f      e ds                  extname(TIATC00F)
003500090511
003600090511      //---------------------------------------------------------------
003700090511      //?Definizione variabili globali.
003800090511      //---------------------------------------------------------------
003900090708
004000090708      // - Flags booleani
004100091009     d $Fine           s               n   inz(*off)
004200091009     d $Fine1          s               n   inz(*off)
004300090708
004400090708      // - Campi di comodo
004500091009     d ageuni          s                   like(Imk84Cmm)
004600090511
004700090511      //---------------------------------------------------------------
004800090511      //?Definizione procedure esterne.
004900090511      //---------------------------------------------------------------
005000090708
005100090511      //---------------------------------------------------------------
005200090511      //?Definizione key-list.
005300090511      //---------------------------------------------------------------
005400090708
005500090708      //---------------------------------------------------------------
005600090708      //?Riepilogo indicatori.
005700090708      //---------------------------------------------------------------
005800090511
005900090511      //---------------------------------------------------------------
006000090708      //?M A I N - L I N E
006100090511      //---------------------------------------------------------------
006200090708
006300090708     c     *Entry        plist
006400091009     c                   parm                    trmk84ds
006500090708
006600090708      /free
006700090708
006800090708       //?Operazioni iniziali
006900090708       exsr sr_RoutInz;
007000091007
007100091007       //?Controllo dati passati
007200091007       exsr sr_CtrDati;
007300091007
007400091008       //?Verifico se ci altri impegni
007500091009       IF  Omk84Err = *blanks;
007600091008         exsr sr_CtrAgenda;
007700091007       ENDIF;
007800090708
007900090708       //?Operazioni finali
008000090708       exsr sr_RoutEnd;
008100090708
008200090708       //--------------------------------------------------------------
008300090708       //?Operazioni iniziali.
008400090708       //--------------------------------------------------------------
008500090708       BEGSR sr_RoutInz;
008600090708
008700091009         clear Omk84Err;
008800091009         clear Omk84Msg;
008900091009
009000090708         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
009100090708
009200090708       ENDSR;
009300090805
009400090805       //--------------------------------------------------------------
009500091007       //?Controllo dati passati.
009600090805       //--------------------------------------------------------------
009700091007       BEGSR sr_CtrDati;
009800090810
009900091009         IF  Imk84Cmm = 0 or Imk84Dad = 0 or Imk84Oii = 0 or
010000091009             Imk84Ofi = 0;
010100091009           Omk84Err = '9';
010200091009           Omk84Msg = $msg(01);
010300091009           leavesr;
010400090810         ENDIF;
010500091009
010600091009         IF  Imk84Tat <> *blanks and (Imk84Atn = 0 or Imk84Atnp = 0);
010700091009           Omk84Err = '9';
010800091009           Omk84Msg = $msg(01);
010900091009           leavesr;
011000091009         ENDIF;
011100091009
011200091009         IF  Imk84Atn > 0 and (Imk84Tat = *blanks or Imk84Atnp = 0);
011300091009           Omk84Err = '9';
011400091009           Omk84Msg = $msg(01);
011500091009           leavesr;
011600091009         ENDIF;
011700091009
011800091009         IF  Imk84Atnp > 0 and (Imk84Tat = *blanks or Imk84Atn = 0);
011900091009           Omk84Err = '9';
012000091009           Omk84Msg = $msg(01);
012100091009           leavesr;
012200091009         ENDIF;
012300090805
012400090805       ENDSR;
012500091007
012600091007       //--------------------------------------------------------------
012700091008       //?Controllo agenda.
012800091007       //--------------------------------------------------------------
012900091008       BEGSR sr_CtrAgenda;
013000091008
013100091009         //?recupero agente unificante del commerciale passato
013200130917         clear  AgeUni;
013300091009         exec sql
013400130917         select CMMuni into :AgeUni
013500130917         from AZCMM00F where CMMcod = :Imk84Cmm;
013600091009         //?non trovo rcd vado a fine
013700091009         IF  sqlcod <> 0;
013800091009           leavesr;
013900091009         ENDIF;
014000091008
014100091009         //?Controllo agenda senza attività passata
014200091009         IF  Imk84Tat = *blanks;
014300091008           exec sql
014400091009           declare agenda cursor for
014500100524           select atccmm, atchda, atcoii, atcofi, atccad, atccad, atcest
014600100524           from tiatc00f where atccmm = :ageuni and atcdad = :Imk84dad
014700091014           and atcdco = 0;
014800091009
014900091009           exec sql open agenda;
015000091009
015100091009           DOW  not $Fine;
015200091014             exec sql fetch next from agenda into :atccmm, :atchda,
015300100524                                                  :atcoii, :atcofi,
015400100524                                                  :atccad, :atccac,
015500100524                                                  :atcest;
015600091009             IF  sqlcod = 100 or sqlcod < 0;
015700091009               $Fine = *on;
015800091009               leave;
015900091009             ENDIF;
016000091014
016100091014             //?imposto l'ora inizio e fine impegno se non impostate
016200091014             IF  atcoii = 0;
016300091014               atcoii = atchda / 100;
016400091014             ENDIF;
016500091014             IF  atcofi = 0;
016600091014               atcofi = atchda / 100;
016700091014             ENDIF;
016800100524
016900100524             //?CHIODO
017000100524             //?se l'attività che trovo è
017100100524             //?un appuntamento '91' con causale chiusura attività '24' e esito 'N'
017200100524             //?non devo tener conto dell'impegno perchè di fatto l'impegno non esiste
017300100524             IF  ATCcad = '91' and ATCcac = '24' and ATCest = 'N';
017400100524               iter;
017500100524             ENDIF;
017600100525             //?un affiancamento '92' con causale chiusura attività 'NO' e esito 'N'
017700100525             //?non devo tener conto dell'impegno perchè di fatto l'impegno non esiste
017800100525             IF  ATCcad = '92' and ATCcac = 'NO' and ATCest = 'N';
017900100525               iter;
018000100525             ENDIF;
018100100524             //?un'attività qualsiasi chiusa con attività 'AA'
018200100524             //?non devo tener conto dell'impegno perchè di fatto l'impegno non esiste
018300100524             IF  ATCcac = 'AA';
018400100524               iter;
018500100524             ENDIF;
018600091014
018700091009             //?trovo almeno un impegno ritorno messaggio
018800091014             IF  atcoii >= Imk84Oii and atcofi <= Imk84Ofi;
018900091014               Omk84Err = '1';
019000091014               Omk84Msg = $msg(02);
019100091014               $Fine = *on;
019200091014               leave;
019300091014             ENDIF;
019400091014             IF  Imk84Oii >= atcoii and Imk84Ofi <= atcofi;
019500091014               Omk84Err = '1';
019600091014               Omk84Msg = $msg(02);
019700091014               $Fine = *on;
019800091014               leave;
019900091014             ENDIF;
020000091014
020100091009           ENDDO;
020200091009
020300091009           exec sql close agenda;
020400091008         ENDIF;
020500091008
020600091009         //?Controllo agenda con attività passata
020700091009         IF  Imk84Tat <> *blanks;
020800091008           exec sql
020900091009           declare agenda1 cursor for
021000091014           select atccmm, atchda, atcoii, atcofi from
021100091014           tiatc00f where atccmm = :ageuni and atcdad = :Imk84dad
021200091014           and atcdco = 0 and atctat <> :Imk84Tat
021300091014           and atcatn <> :Imk84Atn and atcatnp <> :Imk84Atnp;
021400091009
021500091009           exec sql open agenda1;
021600091009
021700091009           DOW  not $Fine1;
021800091014             exec sql fetch next from agenda1 into :atccmm, :atchda,
021900091014                                                   :atcoii, :atcofi;
022000091009             IF  sqlcod = 100 or sqlcod < 0;
022100091009               $Fine1 = *on;
022200091009               leave;
022300091009             ENDIF;
022400091014
022500091014             //?imposto l'ora inizio e fine impegno se non impostate
022600091014             IF  atcoii = 0;
022700091014               atcoii = atchda / 100;
022800091014             ENDIF;
022900091014             IF  atcofi = 0;
023000091014               atcofi = atchda / 100;
023100091014             ENDIF;
023200091014
023300091009             //?trovo almeno un impegno ritorno messaggio
023400091014             IF  atcoii >= Imk84Oii and atcofi <= Imk84Ofi;
023500091014               Omk84Err = '1';
023600091014               Omk84Msg = $msg(02);
023700091014               $Fine1 = *on;
023800091014               leave;
023900091014             ENDIF;
024000091014             IF  Imk84Oii >= atcoii and Imk84Ofi <= atcofi;
024100091014               Omk84Err = '1';
024200091014               Omk84Msg = $msg(02);
024300091014               $Fine = *on;
024400091014               leave;
024500091014             ENDIF;
024600091014
024700091009           ENDDO;
024800091009
024900091009           exec sql close agenda1;
025000091008         ENDIF;
025100091007
025200091007       ENDSR;
025300090708
025400090708       //--------------------------------------------------------------
025500090708       //?Operazioni finali.
025600090708       //--------------------------------------------------------------
025700090708       BEGSR sr_RoutEnd;
025800090708
025900090708         *inLR = *on;
026000090708         return;
026100090708
026200090708       ENDSR;
026300090708
026400090708      /end-free
026500090708
026600090708       //--------------------------------------------------------------
026700090708       //?Schiere a tempo di compilazione.
026800090708       //--------------------------------------------------------------
026900090708
027000090708** - $MSG -------------------------------------------------------------------*
027100091014Dati non passati                                                              01
027200100310ATTENZIONE! Altre attività pianificate nel periodo.                           02
