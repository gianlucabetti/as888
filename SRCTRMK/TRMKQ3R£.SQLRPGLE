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
001100101123     fTICPIM0F  o    e             disk
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
008700101123       clear TICPIM00;
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
011200101123             write  TICPIM00;
011300101123           ENDIF;
011400101123           sav_CPIcpo = CPIcpo;
011500101123           $OKinfo = *off;
011600101123           clear TICPIM00;
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
016100101111             WHEN  CPItpf = 'MES';
016200101112               $OKinfo = *on;
016300101123               CPMimtr = CPIval;
016400101111             WHEN  CPItpf = 'AER';
016500101112               $OKinfo = *on;
016600101123               CPMeva = CPIval;
016700101111             WHEN  CPItpf = 'CAE';
016800101112               $OKinfo = *on;
016900101123               CPMesc = CPIval;
017000101111             WHEN  CPItpf = 'ATR';
017100101112               $OKinfo = *on;
017200101123               CPMaltro = CPIval;
017300101110         //?Caratteristiche delle spedizioni
017400101111             WHEN  CPItpf = 'PRI';
017500101112               $OKinfo = *on;
017600101123               CPMpriv = CPIval;
017700101111             WHEN  CPItpf = 'ECM';
017800101112               $OKinfo = *on;
017900101123               CPMecomm = CPIval;
018000101111             WHEN  CPItpf = 'GDO';
018100101112               $OKinfo = *on;
018200101123               CPMgdo = CPIval;
018300101111             WHEN  CPItpf = 'HPR';
018400101112               $OKinfo = *on;
018500101123               CPMprior = CPIval;
018600110606             WHEN  CPItpf = 'SBX' and CPIfsn = 'S';
018700110606               $OKinfo = *on;
018800110606               CPMsecbx = CPIfsn;
018900101117             WHEN  CPItpf = 'KMS';
019000101117               $OKinfo = *on;
019100101123               CPMpmas = CPIval;
019200101111             WHEN  CPItpf = 'KPM';
019300101112               $OKinfo = *on;
019400101123               CPMpspec = CPIval;
019500101111         //?Concorrenti
019600101116             WHEN  CPItpf = '10' and CPIspf = '_BAR' and CPIval <> 0;
019700101112               $OKinfo = *on;
019800101123               CPM_bar = CPIval;
019900101116             WHEN  CPItpf = '10' and CPIspf = '_MEP' and CPIval <> 0;
020000101112               $OKinfo = *on;
020100101123               CPM_mep = CPIval;
020200101116             WHEN  CPItpf = '10' and CPIspf = 'ARCO' and CPIval <> 0;
020300101112               $OKinfo = *on;
020400101123               CPMarco = CPIval;
020500101116             WHEN  CPItpf = '10' and CPIspf = 'ARTO' and CPIval <> 0;
020600101112               $OKinfo = *on;
020700101123               CPMarto = CPIval;
020800101116             WHEN  CPItpf = '10' and CPIspf = 'AWS' and CPIval <> 0;
020900101112               $OKinfo = *on;
021000101123               CPMaws  = CPIval;
021100101116             WHEN  CPItpf = '10' and CPIspf = 'BARS' and CPIval <> 0;
021200101112               $OKinfo = *on;
021300101123               CPMbars = CPIval;
021400101116             WHEN  CPItpf = '10' and CPIspf = 'DHL' and CPIval <> 0;
021500101112               $OKinfo = *on;
021600101123               CPMdhl  = CPIval;
021700101116             WHEN  CPItpf = '10' and CPIspf = 'EXEC' and CPIval <> 0;
021800101112               $OKinfo = *on;
021900101123               CPMexec = CPIval;
022000101116             WHEN  CPItpf = '10' and CPIspf = 'FERC' and CPIval <> 0;
022100101112               $OKinfo = *on;
022200101123               CPMferc = CPIval;
022300101116             WHEN  CPItpf = '10' and CPIspf = 'MAIL' and CPIval <> 0;
022400101112               $OKinfo = *on;
022500101123               CPMmail = CPIval;
022600101116             WHEN  CPItpf = '10' and CPIspf = 'MTN' and CPIval <> 0;
022700101112               $OKinfo = *on;
022800101123               CPMmtn  = CPIval;
022900101116             WHEN  CPItpf = '10' and CPIspf = 'ONEE' and CPIval <> 0;
023000101112               $OKinfo = *on;
023100101123               CPMonee = CPIval;
023200101116             WHEN  CPItpf = '10' and CPIspf = 'PALL' and CPIval <> 0;
023300101112               $OKinfo = *on;
023400101123               CPMpall = CPIval;
023500101116             WHEN  CPItpf = '10' and CPIspf = 'PALW' and CPIval <> 0;
023600101112               $OKinfo = *on;
023700101123               CPMpalw = CPIval;
023800101116             WHEN  CPItpf = '10' and CPIspf = 'SDA' and CPIval <> 0;
023900101112               $OKinfo = *on;
024000101123               CPMsda  = CPIval;
024100101116             WHEN  CPItpf = '10' and CPIspf = 'SUSA' and CPIval <> 0;
024200101112               $OKinfo = *on;
024300101123               CPMsusa = CPIval;
024400101116             WHEN  CPItpf = '10' and CPIspf = 'TNT' and CPIval <> 0;
024500101112               $OKinfo = *on;
024600101123               CPMtnt  = CPIval;
024700101116             WHEN  CPItpf = '10' and CPIspf = 'UPS' and CPIval <> 0;
024800101112               $OKinfo = *on;
024900101123               CPMups  = CPIval;
025000101116             WHEN  CPItpf = '10' and CPIspf = '8NSC' and CPIval <> 0;
025100101112               $OKinfo = *on;
025200101123               CPM8nsc = CPIval;
025300101116             WHEN  CPItpf = '10' and CPIspf = '9ATR' and CPIval <> 0;
025400101112               $OKinfo = *on;
025500101123               CPM9atr = CPIval;
025600101111         //?Motivo di non affidamento
025700101123             WHEN  CPItpf = 'MOI' and CPIfsn = 'S' and CPMmoi1 = *blanks;
025800101112               $OKinfo = *on;
025900101111               clear dIFS;
026000101111               chain ('IFS' : CPItpf : CPIspf) TNTBE01L;
026100101111               IF  %found(TNTBE01L);
026200101111                 dIFS = TBEuni;
026300101111               ENDIF;
026400101123               CPMmoi1 = CPIspf + '-' + §IFSdes;
026500101123             WHEN  CPItpf = 'MOI' and CPIfsn = 'S' and CPMmoi1 <> *blanks and
026600101123                   CPMmoi2 = *blanks;
026700101112               $OKinfo = *on;
026800101111               clear dIFS;
026900101111               chain ('IFS' : CPItpf : CPIspf) TNTBE01L;
027000101111               IF  %found(TNTBE01L);
027100101111                 dIFS = TBEuni;
027200101111               ENDIF;
027300101123               CPMmoi2 = CPIspf + '-' + §IFSdes;
027400101111         //?Dati relativi alla logistica
027500101111             WHEN  CPItpf = 'LOG' and CPIfsn = 'S';
027600101112               $OKinfo = *on;
027700101123               CPMlog = 'S';
027800101111             WHEN  CPItpf = 'OUT' and CPIfsn = 'S';
027900101112               $OKinfo = *on;
028000101123               CPMout = 'S';
028100101111         //?Operatori di logistica
028200101111             WHEN  CPItpf = '40' and CPIspf = '_BAR';
028300101112               $OKinfo = *on;
028400101123               CPMl_bar = 'S';
028500101111             WHEN  CPItpf = '40' and CPIspf = 'ARTO';
028600101112               $OKinfo = *on;
028700101123               CPMlarto = 'S';
028800101111             WHEN  CPItpf = '40' and CPIspf = 'CEVA';
028900101112               $OKinfo = *on;
029000101123               CPMlceva = 'S';
029100101111             WHEN  CPItpf = '40' and CPIspf = 'DSV';
029200101112               $OKinfo = *on;
029300101123               CPMldsv  = 'S';
029400101111             WHEN  CPItpf = '40' and CPIspf = 'EXEL';
029500101112               $OKinfo = *on;
029600101123               CPMlexel = 'S';
029700101111             WHEN  CPItpf = '40' and CPIspf = 'FERC';
029800101112               $OKinfo = *on;
029900101123               CPMlferc = 'S';
030000101111             WHEN  CPItpf = '40' and CPIspf = 'GEOD';
030100101112               $OKinfo = *on;
030200101123               CPMlgeod = 'S';
030300101111             WHEN  CPItpf = '40' and CPIspf = 'KUNE';
030400101112               $OKinfo = *on;
030500101123               CPMlkune = 'S';
030600101111             WHEN  CPItpf = '40' and CPIspf = 'NORB';
030700101112               $OKinfo = *on;
030800101123               CPMlnorb = 'S';
030900101111             WHEN  CPItpf = '40' and CPIspf = 'NUMB';
031000101112               $OKinfo = *on;
031100101123               CPMlnumb = 'S';
031200101111             WHEN  CPItpf = '40' and CPIspf = 'SIFT';
031300101112               $OKinfo = *on;
031400101123               CPMlsift = 'S';
031500101111             WHEN  CPItpf = '40' and CPIspf = '8NSC';
031600101112               $OKinfo = *on;
031700101123               CPMl8nsc = 'S';
031800101111             WHEN  CPItpf = '40' and CPIspf = '9ATR';
031900101112               $OKinfo = *on;
032000101123               CPMl9atr = 'S';
032100101111           ENDSL;
032200101110
032300101110       ENDSR;
032400101108
032500101108       //--------------------------------------------------------------
032600101108       //?Operazioni finali.
032700101108       //--------------------------------------------------------------
032800101108       BEGSR RoutEnd;
032900101108
033000101108         *inLR = *on;
033100101108         return;
033200101108
033300101108       ENDSR;
033400101108
033500101108      /end-free
