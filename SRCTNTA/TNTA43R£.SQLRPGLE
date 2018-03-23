000100090708      //---------------------------------------------------------------
000200091008      //?TNTA43R - Controlla se trattativa aperta
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
002500090708
002600090511      //---------------------------------------------------------------
002700090511      //?Definizione strutture dati.
002800090511      //---------------------------------------------------------------
002900090708
003000090708      // - Parametri ricevuti
003100091007     d TNTA43ds      e ds
003200090709
003300091009      // File anagrafica provvisoria
003400091009     d tfaco00f      e ds                  extname(TFACO00F)
003500091009
003600091009      // File trattative
003700091007     d tivis00f      e ds                  extname(TIVIS00F)
003800090511
003900090511      //---------------------------------------------------------------
004000090511      //?Definizione variabili globali.
004100090511      //---------------------------------------------------------------
004200090708
004300090708      // - Flags booleani
004400100118     d $End            s               n   inz(*off)
004500100118     d $Fine           s               n   inz(*off)
004600090511
004700090511      //---------------------------------------------------------------
004800090511      //?Definizione procedure esterne.
004900090511      //---------------------------------------------------------------
005000090511
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
006400091009     c                   parm                    tnta43ds
006500090708
006600090708      /free
006700090708
006800090708       //?Operazioni iniziali
006900090708       exsr sr_RoutInz;
007000091007
007100091007       //?Controllo dati passati
007200091007       exsr sr_CtrDati;
007300091007
007400100118       //?Verifico se esiste una trattativa aperta NON fittizia
007500091009       IF  Ota43Err = *blanks;
007600091008         exsr sr_CtrTrattativa;
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
008700091009         clear Ota43Cmm;
008800091009         clear Ota43Nrv;
008900091009         clear Ota43Err;
009000091009         clear Ota43Msg;
009100090708
009200090708         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
009300090708
009400090708       ENDSR;
009500090805
009600090805       //--------------------------------------------------------------
009700091007       //?Controllo dati passati.
009800090805       //--------------------------------------------------------------
009900091007       BEGSR sr_CtrDati;
010000090810
010100100125         IF  Ita43Cpo = 0 and Ita43ksc = 0 and Ita43cmm = 0;
010200091009           Ota43Err = '9';
010300091009           Ota43Msg = $msg(01);
010400090810         ENDIF;
010500090805
010600090805       ENDSR;
010700091007
010800091007       //--------------------------------------------------------------
010900091008       //?Verifico se esiste la trattativa aperta.
011000091007       //--------------------------------------------------------------
011100091008       BEGSR sr_CtrTrattativa;
011200100118
011300100118         $Fine = *off;
011400100118         $End  = *off;
011500100118
011600100118         //?Controllo se esistono trattative aperte
011700100118         SELECT;
011800100304
011900100304         //?--> A
012000100304         //?Controllo per potenziale cliente e commerciale
012100100304         //?Escludo la trattativa passata
012200100304         WHEN  ITA43cpo > 0 and ITA43ksc > 0 and
012300100304               ITA43cmm > 0 and ITA43nrv > 0;
012400100304           exec sql
012500100304           declare A cursor for select visnrv
012600100304           from tivis00f where viscpo = :ITA43cpo and
012700100304           visksc = :ITA43ksc and viscmm = :ITA43cmm and
012800100304           visnrv <> :ITA43nrv and
012900100304           visffz = ' ' and visdch = 0;
013000100304
013100100304           exec sql open A;
013200100304
013300100304           DOW  not $Fine;
013400100304             exec sql fetch next from A into :visnrv;
013500100304             IF  sqlcod = 100 or sqlcod < 0;
013600100304               $Fine = *on;
013700100304               leave;
013800100304             ENDIF;
013900100304           //?imposto i dati di ritorno con la prima trattativa trovata
014000100304           //?imposto '+' nel campo errore in quanto ci sono + trattative
014100100304           //?a parità di potenziale cliente e commerciale
014200100304             OTA43nrv = Visnrv;
014300100304             OTA43err = '+';
014400100304             OTA43msg = $msg(02);
014500100304             leave;
014600100304           ENDDO;
014700100304
014800100304           exec sql close A;
014900100125
015000100304         //?Controllo per potenziale cliente e commerciale
015100100304         WHEN  ITA43cpo > 0 and ITA43ksc > 0 and
015200100304               ITA43cmm > 0;
015300100304           exec sql
015400100304           declare A1 cursor for select visnrv
015500100304           from tivis00f where viscpo = :ITA43cpo and
015600100304           visksc = :ITA43ksc and viscmm = :ITA43cmm and
015700100304           visffz = ' ' and visdch = 0;
015800100304
015900100304           exec sql open A1;
016000100304
016100100304           DOW  not $Fine;
016200100304             exec sql fetch next from A1 into :visnrv;
016300100304             IF  sqlcod = 100 or sqlcod < 0;
016400100304               $Fine = *on;
016500100304               leave;
016600100304             ENDIF;
016700100304           //?imposto i dati di ritorno con la prima trattativa trovata
016800100304             IF  OTA43nrv = 0;
016900100304               OTA43nrv = visnrv;
017000100304             ELSE;
017100100304           //?imposto '+' nel campo errore in quanto ci sono + trattative
017200100304           //?a parità di potenziale cliente e commerciale
017300100304               OTA43err = '+';
017400100304               $Fine = *on;
017500100304               leave;
017600100304             ENDIF;
017700100304           ENDDO;
017800100304
017900100304           exec sql close A1;
018000100304
018100100304
018200100304         //?--> B
018300100304         //?Controllo per potenziale e codice commerciale
018400100125         //?Escludo la trattativa passata
018500100304         WHEN  ITA43cpo > 0 and ITA43cmm > 0 and ITA43nrv > 0;
018600100304           exec sql
018700100304           declare B cursor for select visnrv
018800100304           from tivis00f where viscpo = :ITA43cpo and
018900100304           viscmm = :ITA43cmm and visnrv <> :ITA43nrv and
019000100304           visffz = ' ' and visdch = 0;
019100100304
019200100304           exec sql open B;
019300100304
019400100304           DOW  not $Fine;
019500100304             exec sql fetch next from B into :visnrv;
019600100304             IF  sqlcod = 100 or sqlcod < 0;
019700100304               $Fine = *on;
019800100304               leave;
019900100304             ENDIF;
020000100304           //?imposto i dati di ritorno con la prima trattativa trovata
020100100304           //?imposto '+' nel campo errore in quanto ci sono + trattative
020200100304           //?a parità di potenziale commerciale
020300100304             OTA43nrv = Visnrv;
020400100304             OTA43err = '+';
020500100304             OTA43msg = $msg(02);
020600100304             leave;
020700100304           ENDDO;
020800100304
020900100304           exec sql close B;
021000100304
021100100304         //?Controllo per potenziale e codice commerciale
021200100304         WHEN  ITA43cmm > 0 and ITA43cpo > 0;
021300100304           exec sql
021400100304           declare B1 cursor for select visnrv
021500100304           from tivis00f where viscpo = :ITA43cpo and
021600100304           viscmm = :ITA43cmm and
021700100304           visffz = ' ' and visdch = 0;
021800100304
021900100304           exec sql open B1;
022000100304
022100100304           DOW  not $Fine;
022200100304             exec sql fetch next from B1 into :visnrv;
022300100304             IF  sqlcod = 100 or sqlcod < 0;
022400100304               $Fine = *on;
022500100304               leave;
022600100304             ENDIF;
022700100304           //?imposto i dati di ritorno con la prima trattativa trovata
022800100304             IF  OTA43nrv = 0;
022900100304               OTA43nrv = visnrv;
023000100304             ELSE;
023100100304           //?imposto '+' nel campo errore in quanto ci sono + trattative
023200100304           //?a parità di potenziale e commerciale
023300100304               OTA43err = '+';
023400100304               $Fine = *on;
023500100304               leave;
023600100304             ENDIF;
023700100304           ENDDO;
023800100304
023900100304           exec sql close B1;
024000100125
024100100118
024200100304         //?--> C
024300100304         //?Controllo per cliente e codice commerciale
024400100118         //?Escludo la trattativa passata
024500100304         WHEN  ITA43ksc > 0 and ITA43cmm > 0 and ITA43nrv > 0;
024600100118           exec sql
024700100304           declare C cursor for select visnrv
024800100118           from tivis00f where visksc = :ITA43ksc and
024900100118           viscmm = :ITA43cmm and visnrv <> :ITA43nrv and
025000100118           visffz = ' ' and visdch = 0;
025100100118
025200100304           exec sql open C;
025300100118
025400100118           DOW  not $Fine;
025500100304             exec sql fetch next from C into :visnrv;
025600100118             IF  sqlcod = 100 or sqlcod < 0;
025700100118               $Fine = *on;
025800100118               leave;
025900100118             ENDIF;
026000100118           //?imposto i dati di ritorno con la prima trattativa trovata
026100100118           //?imposto '+' nel campo errore in quanto ci sono + trattative
026200100304           //?a parità di cliente e commerciale
026300100118             OTA43nrv = Visnrv;
026400100118             OTA43err = '+';
026500100118             OTA43msg = $msg(02);
026600100118             leave;
026700100118           ENDDO;
026800100118
026900100304           exec sql close C;
027000100118
027100100304         //?Controllo per cliente e codice commerciale
027200100304         WHEN  ITA43ksc > 0 and ITA43cmm > 0;
027300100118           exec sql
027400100304           declare C1 cursor for select visnrv
027500100118           from tivis00f where visksc = :ITA43ksc and
027600100118           viscmm = :ITA43cmm and
027700100118           visffz = ' ' and visdch = 0;
027800100118
027900100304           exec sql open C1;
028000100118
028100100118           DOW  not $Fine;
028200100304             exec sql fetch next from C1 into :visnrv;
028300100118             IF  sqlcod = 100 or sqlcod < 0;
028400100118               $Fine = *on;
028500100118               leave;
028600100118             ENDIF;
028700100118           //?imposto i dati di ritorno con la prima trattativa trovata
028800100118             IF  OTA43nrv = 0;
028900100118               OTA43nrv = visnrv;
029000100118             ELSE;
029100100118           //?imposto '+' nel campo errore in quanto ci sono + trattative
029200100304           //?a parità di cliente e commerciale
029300100118               OTA43err = '+';
029400100118               $Fine    = *on;
029500100118               leave;
029600100118             ENDIF;
029700100118           ENDDO;
029800100118
029900100304           exec sql close C1;
030000100118
030100100118
030200100304         //?--> D
030300100118         //?Controllo per cliente
030400100118         //?Escludo la trattativa passata
030500100118         WHEN  ITA43ksc > 0 and ITA43nrv > 0;
030600100118           exec sql
030700100304           declare D cursor for select visnrv
030800100118           from tivis00f where visksc = :ITA43ksc and
030900100118           visnrv <> :ITA43nrv and
031000100118           visffz = ' ' and visdch = 0;
031100100118
031200100304           exec sql open D;
031300100118
031400100118           DOW  not $Fine;
031500100304             exec sql fetch next from D into :visnrv;
031600100118             IF  sqlcod = 100 or sqlcod < 0;
031700100118               $Fine = *on;
031800100118               leave;
031900100118             ENDIF;
032000100118           //?imposto i dati di ritorno con la prima trattativa trovata
032100100118           //?imposto '+' nel campo errore in quanto ci sono + trattative
032200100118           //?a parità di cliente
032300100118             OTA43nrv = Visnrv;
032400100118             OTA43err = '+';
032500100118             OTA43msg = $msg(02);
032600100118             leave;
032700100118           ENDDO;
032800100118
032900100304           exec sql close D;
033000100118
033100100118         //?Controllo per cliente
033200100118         WHEN  ITA43ksc > 0;
033300100118           exec sql
033400100304           declare D1 cursor for select visnrv
033500100118           from tivis00f where visksc = :ITA43ksc and
033600100118           visffz = ' ' and visdch = 0;
033700100118
033800100304           exec sql open D1;
033900100118
034000100118           DOW  not $Fine;
034100100304             exec sql fetch next from D1 into :visnrv;
034200100118             IF  sqlcod = 100 or sqlcod < 0;
034300100118               $Fine = *on;
034400100118               leave;
034500100118             ENDIF;
034600100118           //?imposto i dati di ritorno con la prima trattativa trovata
034700100118             IF  OTA43nrv = 0;
034800100118               OTA43nrv = visnrv;
034900100118             ELSE;
035000100118           //?imposto '+' nel campo errore in quanto ci sono + trattative
035100100304           //?a parità di cliente
035200100118               OTA43err = '+';
035300100118               $Fine    = *on;
035400100118               leave;
035500100118             ENDIF;
035600100118           ENDDO;
035700100118
035800100304           exec sql close D1;
035900100118
036000100304
036100100304         //?--> E
036200100304         //?Controllo per potenziale
036300100304         //?Escludo la trattativa passata
036400100304         WHEN  ITA43cpo > 0 and ITA43nrv > 0;
036500100304           exec sql
036600100304           declare E cursor for select visnrv
036700100304           from tivis00f where viscpo = :ITA43cpo and
036800100304           visnrv <> :ITA43nrv and
036900100304           visffz = ' ' and visdch = 0;
037000100304
037100100304           exec sql open E;
037200100304
037300100304           DOW  not $Fine;
037400100304             exec sql fetch next from E into :visnrv;
037500100304             IF  sqlcod = 100 or sqlcod < 0;
037600100304               $Fine = *on;
037700100304               leave;
037800100304             ENDIF;
037900100304           //?imposto i dati di ritorno con la prima trattativa trovata
038000100304           //?imposto '+' nel campo errore in quanto ci sono + trattative
038100100304           //?a parità di potenziale cliente e commerciale
038200100304             OTA43nrv = Visnrv;
038300100304             OTA43err = '+';
038400100304             OTA43msg = $msg(02);
038500100304             leave;
038600100304           ENDDO;
038700100304
038800100304           exec sql close E;
038900100304
039000100304         //?Controllo per potenziale
039100100304         WHEN  ITA43cpo > 0;
039200100304           exec sql
039300100304           declare E1 cursor for select visnrv
039400100304           from tivis00f where viscpo = :ITA43cpo and
039500100304           visffz = ' ' and visdch = 0;
039600100304
039700100304           exec sql open E1;
039800100304
039900100304           DOW  not $Fine;
040000100304             exec sql fetch next from E1 into :visnrv;
040100100304             IF  sqlcod = 100 or sqlcod < 0;
040200100304               $Fine = *on;
040300100304               leave;
040400100304             ENDIF;
040500100304           //?imposto i dati di ritorno con la prima trattativa trovata
040600100304             IF  OTA43nrv = 0;
040700100304               OTA43nrv = visnrv;
040800100304             ELSE;
040900100304           //?imposto '+' nel campo errore in quanto ci sono + trattative
041000100304           //?a parità di potenziale cliente e commerciale
041100100304               OTA43err = '+';
041200100304               $Fine = *on;
041300100304               leave;
041400100304             ENDIF;
041500100304           ENDDO;
041600100304
041700100304           exec sql close E1;
041800091009
041900100118         ENDSL;
042000091007
042100091007       ENDSR;
042200090708
042300090708       //--------------------------------------------------------------
042400090708       //?Operazioni finali.
042500090708       //--------------------------------------------------------------
042600090708       BEGSR sr_RoutEnd;
042700090708
042800090708         *inLR = *on;
042900090708         return;
043000090708
043100090708       ENDSR;
043200090708
043300090708      /end-free
043400090708
043500090708       //--------------------------------------------------------------
043600090708       //?Schiere a tempo di compilazione.
043700090708       //--------------------------------------------------------------
043800090708
043900090708** - $MSG -------------------------------------------------------------------*
044000091014Dati non passati                                                              01
044100100330Esistono altre trattative aperte con lo stesso commerciale.                   02
