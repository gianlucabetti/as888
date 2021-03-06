000100080206      //--------------------------------------------------------------
000200110201      //?TRMK06R - Aggiorna categoria Potenziali su TNCPO
000300080206      //--------------------------------------------------------------
000400080206
000500100820     h decedit('0,') datedit(*ymd) option(*nodebugio)
000600080206
000700080206      //---------------------------------------------------------------
000800080206      //?Dichiarazione file.
000900080206      //---------------------------------------------------------------
001000080206
001100110201      // - Archivio Potenziali
001200110201     fTNCPO01L  uf   e           k disk
001300080206
001400080206      //---------------------------------------------------------------
001500090406      //?Definizione costanti.
001600080206      //---------------------------------------------------------------
001700050519
001800080206
001900080206      //---------------------------------------------------------------
002000080206      //?Definizione schiere.
002100080206      //---------------------------------------------------------------
002200080206
002300080206
002400080206      //---------------------------------------------------------------
002500080206      //?Definizione aree dati.
002600080206      //---------------------------------------------------------------
002700080206
002800080206
002900080206      //---------------------------------------------------------------
003000080206      //?Definizione strutture dati.
003100080206      //---------------------------------------------------------------
003200080206
003300080206      // - Status
003400080206     d Psds           sds
003500080206     d   SDSpgm          *proc
003600080206
003700080206      // - Parametri ricevuti
003800080206     d KPJBA         e ds
003900090717
004000110201      // - Calcolo categoria potenziale
004100110201     d TRMK05ds      e ds                  inz
004200100719
004300080206      //---------------------------------------------------------------
004400080206      //?Definizione variabili globali.
004500080206      //---------------------------------------------------------------
004600080206
004700080206      // - Flags booleani
004800100312     d $End            s               n   inz(*off)
004900110201     d $EndCpo         s               n   inz(*off)
005000080206
005100090401
005200080206      // - Campi di comodo
005300090717
005400100723     d StringaSql      s            750    Varying       inz
005500080208
005600090508      //---------------------------------------------------------------
005700090508      //?Definizione procedure usate.
005800090508      //---------------------------------------------------------------
005900080206
006000080626      //---------------------------------------------------------------
006100080626      //?prototipi
006200080626      //---------------------------------------------------------------
006300100719
006400110201     d TRMK05R         pr                  extpgm('TRMK05R')
006500100719     d  kpjba                              likeds(kpjba)
006600110201     d  trmk05ds                           likeds(trmk05ds)
006700100820
006800080206      //---------------------------------------------------------------
006900080206      //?Definizione key-list.
007000080206      //---------------------------------------------------------------
007100080206
007200080206
007300080206      //---------------------------------------------------------------
007400080206      //?Riepilogo indicatori.
007500080206      //---------------------------------------------------------------
007600090518      // - Comuni
007700080207      // 99    : Generico di Errore
007800080206      //---------------------------------------------------------------
007900080206
008000080206      //---------------------------------------------------------------
008100080206      //?M A I N - L I N E
008200080206      //---------------------------------------------------------------
008300080206
008400080206     c     *Entry        plist
008500080206     c                   parm                    KPJBA
008600080206
008700080206      /free
008800080206
008900080206       // Operazioni iniziali
009000080206       exsr RoutInz;
009100090601
009200100909       // Esegui
009300100909       exsr Esegui ;
009400080206
009500080206       // Operazioni finali
009600080206       exsr RoutEnd;
009700080206
009800080206       //--------------------------------------------------------------
009900080206       //?Operazioni iniziali.
010000080206       //--------------------------------------------------------------
010100080206       BEGSR RoutInz;
010200100427
010300100427         exec sql  set option  dynusrprf = *owner,
010400100427                               closqlcsr = *endmod;
010500080206
010600090714
010700080206       ENDSR;
010800080206
010900100909       //--------------------------------------------------------------
011000100909       //?Esecuzione lettura Tivis
011100100909       //--------------------------------------------------------------
011200100909       BEGSR Esegui ;
011300100909
011400100927         exec sql
011500110201          declare CPO cursor for
011600110201          select cpocpo, cpofls
011700110201          from tncpo00f  ;
011800100927
011900100927         exec sql
012000110201           open CPO;
012100100927           IF sqlcode < 0;
012200110201             $EndCpo = *on;
012300100927           ENDIF;
012400100927
012500110201         DOW not $EndCpo;
012600100927           exec sql
012700110201             fetch next from CPO into :CpoCpo, :CpoFls ;
012800100927             IF sqlcod = 100 or sqlcod < 0;
012900110201               $EndCpo = *on;
013000100927               leave;
013100100927             ENDIF;
013200110201         // ricalcolo la categoria del potenziale
013300110201             clear trmk05ds ;
013400110201             imk05cpo = CpoCpo ;
013500110201             trmk05r (Kpjba:Trmk05ds) ;
013600110201             If omk05err = ' ' and omk05cat <> cpofls ;
013700110201                Chain(e) CpoCpo tncpo01l ;
013800110201                 If %error ;
013900110304                    //leave ;
014000110304                    iter;
014100100927                 endif ;
014200110201                 cpofls = Omk05cat ;
014300110201                 update tncpo000 ;
014400110201             endif ;
014500100927
014600100927         ENDDO;
014700100927
014800110201         exec sql close CPO ;
014900100927
015000100909       ENDSR;
015100100909
015200080206       //--------------------------------------------------------------
015300080206       //?Operazioni finali.
015400080206       //--------------------------------------------------------------
015500080206       BEGSR RoutEnd;
015600080206
015700080206         *inLR = *on;
015800080206         return;
015900080206
016000080206       ENDSR;
016100080206
016200080206      /end-free
