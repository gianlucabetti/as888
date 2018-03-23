000100101108      //--------------------------------------------------------------
000200101123      //?TRMKQ3R - Creezione file Qlickview - Info Comm. Mensile
000300101108      //--------------------------------------------------------------
000400101108     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500101108
000600101108      //---------------------------------------------------------------
000700101108      //?Dichiarazione file.
000800101108      //---------------------------------------------------------------
000900101108
001000101108      // - File per QL
001100140912     fTICPIM1F  o    e             disk
001200101108      // - Nuove Tabelle
001300101108     fTNTBE01L  if   e           k disk
001400101108
001500101108      //---------------------------------------------------------------
001600101108      //?Definizione costanti.
001700101108      //---------------------------------------------------------------
001800101108
001900101108      //---------------------------------------------------------------
002000101108      //?Definizione schiere.
002100101108      //---------------------------------------------------------------
002200101108
002300101108      //---------------------------------------------------------------
002400101108      //?Definizione aree dati.
002500101108      //---------------------------------------------------------------
002600101108
002700101108      //---------------------------------------------------------------
002800101108      //?Definizione strutture dati.
002900101108      //---------------------------------------------------------------
003000101110
003100101110      // - File Info Commerciali
003200101110     d CPIDS         e ds                  extname(TICPI00F)
003300101111
003400101111      // - IFS motivo non affidamento
003500101111     d dIFS          e ds                  inz
003600101110
003700101110      // - Parametri ricevuti
003800101110     d KPJBA         e ds
003900101108
004000101108      //---------------------------------------------------------------
004100101108      //?Definizione variabili globali.
004200101108      //---------------------------------------------------------------
004300101108
004400101108      // - Flags booleani
004500101110     d $EndCpi         s               n   inz(*off)
004600101111     d $OkInfo         s               n   inz(*off)
004700101108
004800101108      // - Indici di schiera
004900101108
005000101108      // - Campi di comodo
005100101123     d sav_CPIcpo      s                   like(CPIcpo)
005200101108
005300101123      // - Stringhe SQL da eseguire
005400101108
005500101108      //---------------------------------------------------------------
005600101108      //?Definizione procedure usate.
005700101108      //---------------------------------------------------------------
005800101108
005900101108      //---------------------------------------------------------------
006000101108      //?Prototipi usati.
006100101108      //---------------------------------------------------------------
006200101108
006300101108      //---------------------------------------------------------------
006400101108      //?Definizione key-list.
006500101108      //---------------------------------------------------------------
006600101108
006700101108      //---------------------------------------------------------------
006800101108      //?Riepilogo indicatori.
006900101108      //---------------------------------------------------------------
007000101108
007100101108      //---------------------------------------------------------------
007200101108      //?M A I N - L I N E
007300101108      //---------------------------------------------------------------
007400101110
007500101110     c     *Entry        plist
007600101110     c                   parm                    KPJBA
007700101108
007800101108      /free
007900101108
008000101108       //?Operazioni iniziali
008100101108       exsr RoutInz;
008200101108
008300101110       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
008400101123       $EndCpi = *off;
008500101123       $OKinfo = *off;
008600101123       clear sav_CPIcpo;
008700140912       clear TICPIM10;
008800101108
008900101108       //?Preparo il cursore
009000101123       exec sql declare CPI cursor for
009100101123                select * from TICPI00F
009200101123                where CPIatb = ' '
009300101123                order by CPIcpo, CPItpf;
009400101108
009500101108       //?Apro il cursore
009600101123       exec sql open CPI;
009700101108       IF  sqlcode < 0;
009800101123         $EndCpi = *on;
009900101108       ENDIF;
010000101108
010100101123       //?Leggo le info commerciali
010200101123       DOW  not $EndCpi;
010300101123         exec sql fetch next from CPI into :CPIDS;
010400101108         IF  sqlcod = 100 or sqlcod < 0;
010500101123           $EndCPI = *on;
010600101108           leave;
010700101108         ENDIF;
010800101123
010900101123       //?Scrivo il rcd delle info commerciali a cambio potenziale
011000101123         IF  CPIcpo <> sav_CPIcpo;
011100101123           IF  sav_CPIcpo <> 0 and $OKinfo;
011200140912             write  TICPIM10;
011300101123           ENDIF;
011400101123           sav_CPIcpo = CPIcpo;
011500101123           $OKinfo = *off;
011600140912           clear TICPIM10;
011700101123         ENDIF;
011800101108
011900101108         //?Elaboro il record
012000101108         exsr Elabora;
012100101108
012200101108       ENDDO;
012300101108
012400101123       exec sql close CPI;
012500101108
012600101108       //?Operazioni finali
012700101108       exsr RoutEnd;
012800101108
012900101108       //--------------------------------------------------------------
013000101108       //?Operazioni iniziali.
013100101108       //--------------------------------------------------------------
013200101108       BEGSR RoutInz;
013300101108
013400101108       ENDSR;
013500101108
013600101108       //--------------------------------------------------------------
013700101108       //?Elaborazione record.
013800101108       //--------------------------------------------------------------
013900101108       BEGSR Elabora;
014000101110
014100101110       //?Imposto i dati per scrivere il record
014200101123           CPMcpo = CPIcpo;
014300101110
014400101123           IF  CPMdim <= CPIdim;
014500101123             CPMdim = CPIdim;
014600101111           ENDIF;
014700101110
014800101111           SELECT;
014900101110         //?Fatturato/Spedizioni totale
015000101111             WHEN  CPItpf = 'SPT';
015100101112               $OKinfo = *on;
015200101123               CPMfatt = CPIpft;
015300101123               CPMnspt = CPIsna;
015400101110         //?Ripartizioni del fatturato
015500101111             WHEN  CPItpf = 'PRF';
015600101112               $OKinfo = *on;
015700101123               CPMipf5 = CPIval;
015800101111             WHEN  CPItpf = 'PRO';
015900101112               $OKinfo = *on;
016000101123               CPMimes = CPIval;
016100101111             WHEN  CPItpf = 'AER';
016200101112               $OKinfo = *on;
016300101123               CPMeva = CPIval;
016400101111             WHEN  CPItpf = 'CAE';
016500101112               $OKinfo = *on;
016600101123               CPMesc = CPIval;
016700101111             WHEN  CPItpf = 'ATR';
016800101112               $OKinfo = *on;
016900101123               CPMaltro = CPIval;
017000140912         //?Fatturato non affidato a BRT
017100140912             WHEN  CPItpf = 'NRF';
017200140912               $OKinfo = *on;
017300140912               CPMnipf5 = CPIpft;
017400140912             WHEN  CPItpf = 'NRO';
017500140912               $OKinfo = *on;
017600140912               CPMnimes = CPIpft;
017700140912             WHEN  CPItpf = 'NER';
017800140912               $OKinfo = *on;
017900140912               CPMneva = CPIpft;
018000140912             WHEN  CPItpf = 'NAE';
018100140912               $OKinfo = *on;
018200140912               CPMnesc = CPIpft;
018300140912             WHEN  CPItpf = 'NTR';
018400140912               $OKinfo = *on;
018500140912               CPMnaltro = CPIpft;
018600101110         //?Caratteristiche delle spedizioni
018700101111             WHEN  CPItpf = 'PRI';
018800101112               $OKinfo = *on;
018900101123               CPMpriv = CPIval;
019000101111             WHEN  CPItpf = 'ECM';
019100101112               $OKinfo = *on;
019200101123               CPMecomm = CPIval;
019300101111             WHEN  CPItpf = 'GDO';
019400101112               $OKinfo = *on;
019500101123               CPMgdo = CPIval;
019600101111             WHEN  CPItpf = 'HPR';
019700101112               $OKinfo = *on;
019800101123               CPMprior = CPIval;
019900110606             WHEN  CPItpf = 'SBX' and CPIfsn = 'S';
020000110606               $OKinfo = *on;
020100110606               CPMsecbx = CPIfsn;
020200101117             WHEN  CPItpf = 'KMS';
020300101117               $OKinfo = *on;
020400101123               CPMpmas = CPIval;
020500101111         //?Concorrenti
020600101116             WHEN  CPItpf = '10' and CPIspf = '_BAR' and CPIval <> 0;
020700101112               $OKinfo = *on;
020800101123               CPM_bar = CPIval;
020900101116             WHEN  CPItpf = '10' and CPIspf = '_MEP' and CPIval <> 0;
021000101112               $OKinfo = *on;
021100101123               CPM_mep = CPIval;
021200101116             WHEN  CPItpf = '10' and CPIspf = 'ARCO' and CPIval <> 0;
021300101112               $OKinfo = *on;
021400101123               CPMarco = CPIval;
021500101116             WHEN  CPItpf = '10' and CPIspf = 'ARTO' and CPIval <> 0;
021600101112               $OKinfo = *on;
021700101123               CPMarto = CPIval;
021800101116             WHEN  CPItpf = '10' and CPIspf = 'AWS' and CPIval <> 0;
021900101112               $OKinfo = *on;
022000101123               CPMaws  = CPIval;
022100101116             WHEN  CPItpf = '10' and CPIspf = 'BARS' and CPIval <> 0;
022200101112               $OKinfo = *on;
022300101123               CPMbars = CPIval;
022400101116             WHEN  CPItpf = '10' and CPIspf = 'DHL' and CPIval <> 0;
022500101112               $OKinfo = *on;
022600101123               CPMdhl  = CPIval;
022700101116             WHEN  CPItpf = '10' and CPIspf = 'EXEC' and CPIval <> 0;
022800101112               $OKinfo = *on;
022900101123               CPMexec = CPIval;
023000140912             WHEN  CPItpf = '10' and CPIspf = 'FEDE' and CPIval <> 0;
023100140912               $OKinfo = *on;
023200140912               CPMfede = CPIval;
023300101116             WHEN  CPItpf = '10' and CPIspf = 'FERC' and CPIval <> 0;
023400101112               $OKinfo = *on;
023500101123               CPMferc = CPIval;
023600101116             WHEN  CPItpf = '10' and CPIspf = 'MAIL' and CPIval <> 0;
023700101112               $OKinfo = *on;
023800101123               CPMmail = CPIval;
023900101116             WHEN  CPItpf = '10' and CPIspf = 'MTN' and CPIval <> 0;
024000101112               $OKinfo = *on;
024100101123               CPMmtn  = CPIval;
024200101116             WHEN  CPItpf = '10' and CPIspf = 'ONEE' and CPIval <> 0;
024300101112               $OKinfo = *on;
024400101123               CPMonee = CPIval;
024500101116             WHEN  CPItpf = '10' and CPIspf = 'PALL' and CPIval <> 0;
024600101112               $OKinfo = *on;
024700101123               CPMpall = CPIval;
024800101116             WHEN  CPItpf = '10' and CPIspf = 'PALW' and CPIval <> 0;
024900101112               $OKinfo = *on;
025000101123               CPMpalw = CPIval;
025100140912             WHEN  CPItpf = '10' and CPIspf = 'POST' and CPIval <> 0;
025200140912               $OKinfo = *on;
025300140912               CPMpost = CPIval;
025400101116             WHEN  CPItpf = '10' and CPIspf = 'SDA' and CPIval <> 0;
025500101112               $OKinfo = *on;
025600101123               CPMsda  = CPIval;
025700101116             WHEN  CPItpf = '10' and CPIspf = 'SUSA' and CPIval <> 0;
025800101112               $OKinfo = *on;
025900101123               CPMsusa = CPIval;
026000101116             WHEN  CPItpf = '10' and CPIspf = 'TNT' and CPIval <> 0;
026100101112               $OKinfo = *on;
026200101123               CPMtnt  = CPIval;
026300140912             WHEN  CPItpf = '10' and CPIspf = 'TNTP' and CPIval <> 0;
026400140912               $OKinfo = *on;
026500140912               CPMtntp = CPIval;
026600101116             WHEN  CPItpf = '10' and CPIspf = 'UPS' and CPIval <> 0;
026700101112               $OKinfo = *on;
026800101123               CPMups  = CPIval;
026900101116             WHEN  CPItpf = '10' and CPIspf = '8NSC' and CPIval <> 0;
027000101112               $OKinfo = *on;
027100101123               CPM8nsc = CPIval;
027200101116             WHEN  CPItpf = '10' and CPIspf = '9ATR' and CPIval <> 0;
027300101112               $OKinfo = *on;
027400101123               CPM9atr = CPIval;
027500101111         //?Motivo di non affidamento
027600101123             WHEN  CPItpf = 'MOI' and CPIfsn = 'S' and CPMmoi1 = *blanks;
027700101112               $OKinfo = *on;
027800101111               clear dIFS;
027900101111               chain ('IFS' : CPItpf : CPIspf) TNTBE01L;
028000101111               IF  %found(TNTBE01L);
028100101111                 dIFS = TBEuni;
028200101111               ENDIF;
028300101123               CPMmoi1 = CPIspf + '-' + §IFSdes;
028400101123             WHEN  CPItpf = 'MOI' and CPIfsn = 'S' and CPMmoi1 <> *blanks and
028500101123                   CPMmoi2 = *blanks;
028600101112               $OKinfo = *on;
028700101111               clear dIFS;
028800101111               chain ('IFS' : CPItpf : CPIspf) TNTBE01L;
028900101111               IF  %found(TNTBE01L);
029000101111                 dIFS = TBEuni;
029100101111               ENDIF;
029200101123               CPMmoi2 = CPIspf + '-' + §IFSdes;
029300101111         //?Dati relativi alla logistica
029400101111             WHEN  CPItpf = 'LOG' and CPIfsn = 'S';
029500101112               $OKinfo = *on;
029600101123               CPMlog = 'S';
029700101111             WHEN  CPItpf = 'OUT' and CPIfsn = 'S';
029800101112               $OKinfo = *on;
029900101123               CPMout = 'S';
030000140912             WHEN  CPItpf = 'MQL' and CPIval <> 0;
030100140912               $OKinfo = *on;
030200140912               CPMmql = CPIval;
030300140912             WHEN  CPItpf = 'PPL' and CPIval <> 0;
030400140912               $OKinfo = *on;
030500140912               CPMppl = CPIval;
030600101111         //?Operatori di logistica
030700101111             WHEN  CPItpf = '40' and CPIspf = '_BAR';
030800101112               $OKinfo = *on;
030900101123               CPMl_bar = 'S';
031000101111             WHEN  CPItpf = '40' and CPIspf = 'ARTO';
031100101112               $OKinfo = *on;
031200101123               CPMlarto = 'S';
031300101111             WHEN  CPItpf = '40' and CPIspf = 'CEVA';
031400101112               $OKinfo = *on;
031500101123               CPMlceva = 'S';
031600140912             WHEN  CPItpf = '40' and CPIspf = 'CHIA';
031700140912               $OKinfo = *on;
031800140912               CPMlchia = 'S';
031900101111             WHEN  CPItpf = '40' and CPIspf = 'DSV';
032000101112               $OKinfo = *on;
032100101123               CPMldsv  = 'S';
032200101111             WHEN  CPItpf = '40' and CPIspf = 'EXEL';
032300101112               $OKinfo = *on;
032400101123               CPMlexel = 'S';
032500101111             WHEN  CPItpf = '40' and CPIspf = 'FERC';
032600101112               $OKinfo = *on;
032700101123               CPMlferc = 'S';
032800140912             WHEN  CPItpf = '40' and CPIspf = 'FMLO';
032900140912               $OKinfo = *on;
033000140912               CPMlfmlo = 'S';
033100101111             WHEN  CPItpf = '40' and CPIspf = 'GEOD';
033200101112               $OKinfo = *on;
033300101123               CPMlgeod = 'S';
033400101111             WHEN  CPItpf = '40' and CPIspf = 'KUNE';
033500101112               $OKinfo = *on;
033600101123               CPMlkune = 'S';
033700140912             WHEN  CPItpf = '40' and CPIspf = 'MOVI';
033800140912               $OKinfo = *on;
033900140912               CPMlmovi = 'S';
034000101111             WHEN  CPItpf = '40' and CPIspf = 'NORB';
034100101112               $OKinfo = *on;
034200101123               CPMlnorb = 'S';
034300101111             WHEN  CPItpf = '40' and CPIspf = 'NUMB';
034400101112               $OKinfo = *on;
034500101123               CPMlnumb = 'S';
034600101111             WHEN  CPItpf = '40' and CPIspf = 'SIFT';
034700101112               $OKinfo = *on;
034800101123               CPMlsift = 'S';
034900101111             WHEN  CPItpf = '40' and CPIspf = '8NSC';
035000101112               $OKinfo = *on;
035100101123               CPMl8nsc = 'S';
035200101111             WHEN  CPItpf = '40' and CPIspf = '9ATR';
035300101112               $OKinfo = *on;
035400101123               CPMl9atr = 'S';
035500140912         //?Paesi Estero
035600140912             WHEN  CPItpf = '50' and CPIspf = '0005';
035700140912               $OKinfo = *on;
035800140912               CPMpd = 'S';
035900140912             WHEN  CPItpf = '50' and CPIspf = '0010';
036000140912               $OKinfo = *on;
036100140912               CPMpf = 'S';
036200140912             WHEN  CPItpf = '50' and CPIspf = '0015';
036300140912               $OKinfo = *on;
036400140912               CPMpe = 'S';
036500140912             WHEN  CPItpf = '50' and CPIspf = '0020';
036600140912               $OKinfo = *on;
036700140912               CPMpb = 'S';
036800140912             WHEN  CPItpf = '50' and CPIspf = '0025';
036900140912               $OKinfo = *on;
037000140912               CPMpa = 'S';
037100140912             WHEN  CPItpf = '50' and CPIspf = '0030';
037200140912               $OKinfo = *on;
037300140912               CPMpslo = 'S';
037400140912             WHEN  CPItpf = '50' and CPIspf = '0035';
037500140912               $OKinfo = *on;
037600140915               CPMpch = 'S';
037700140912             WHEN  CPItpf = '50' and CPIspf = '0040';
037800140912               $OKinfo = *on;
037900140912               CPMpgb = 'S';
038000140912             WHEN  CPItpf = '50' and CPIspf = '0045';
038100140912               $OKinfo = *on;
038200140912               CPMps = 'S';
038300140912             WHEN  CPItpf = '50' and CPIspf = '0050';
038400140912               $OKinfo = *on;
038500140912               CPMpest = 'S';
038600140912             WHEN  CPItpf = '50' and CPIspf = '0055';
038700140912               $OKinfo = *on;
038800140912               CPMpgr = 'S';
038900140912             WHEN  CPItpf = '50' and CPIspf = '0060';
039000140912               $OKinfo = *on;
039100140912               CPMpru = 'S';
039200140912             WHEN  CPItpf = '50' and CPIspf = '0065';
039300140912               $OKinfo = *on;
039400140912               CPMptr = 'S';
039500140912             WHEN  CPItpf = '50' and CPIspf = '1000';
039600140912               $OKinfo = *on;
039700140912               CPMpanord = 'S';
039800140912             WHEN  CPItpf = '50' and CPIspf = '1010';
039900140912               $OKinfo = *on;
040000140912               CPMpasud = 'S';
040100140912             WHEN  CPItpf = '50' and CPIspf = '2005';
040200140912               $OKinfo = *on;
040300140912               CPMpasia = 'S';
040400140912             WHEN  CPItpf = '50' and CPIspf = '3000';
040500140912               $OKinfo = *on;
040600140912               CPMpother = 'S';
040700101111           ENDSL;
040800101110
040900101110       ENDSR;
041000101108
041100101108       //--------------------------------------------------------------
041200101108       //?Operazioni finali.
041300101108       //--------------------------------------------------------------
041400101108       BEGSR RoutEnd;
041500101108
041600101108         *inLR = *on;
041700101108         return;
041800101108
041900101108       ENDSR;
042000101108
042100101108      /end-free
