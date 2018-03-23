000100100322      //--------------------------------------------------------------
000200100322      //?XGIOLAV - ADD/SUB GG/MM ad una data  lavorativi e no
000300100322      //--------------------------------------------------------------
000400100322
000500100322     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600100322     h dftactgrp(*no) actgrp(*caller)
000700100322
000800100322      //---------------------------------------------------------------
000900100322      //?Dichiarazione file.
001000100322      //---------------------------------------------------------------
001100100322
001200100322      // - Calendario
001300100322     fAZCLN01L  if   e           k disk
001400100802
001500100802      // - Tabelle
001600100802     fTABEL00F  if   e           k disk
001700100322
001800100322      //---------------------------------------------------------------
001900100322      //?Definizione costanti.
002000100322      //---------------------------------------------------------------
002100100322
002200100322
002300100322      //---------------------------------------------------------------
002400100322      //?Definizione schiere.
002500100322      //---------------------------------------------------------------
002600100322
002700100322      // - Mattino/Pomeriggio calendario
002800100322     d CLNmat          DS
002900100322     d  mat                    1     31    dim(31)
003000100322     d CLNpom          DS
003100100322     d  pom                    1     31    dim(31)
003200100322
003300100322      //---------------------------------------------------------------
003400100322      //?Definizione aree dati.
003500100322      //---------------------------------------------------------------
003600100322
003700100322
003800100322      //---------------------------------------------------------------
003900100322      //?Definizione strutture dati.
004000100322      //---------------------------------------------------------------
004100100322
004200100802     d ds1Y          e ds
004300100322     d xgiolavds     e ds
004400100322
004500100322      //---------------------------------------------------------------
004600100322      //?Definizione variabili globali.
004700100322      //---------------------------------------------------------------
004800100322
004900100322      // - Flags booleani
005000100420     d $Fine           s               n   inz(*off)
005100100322
005200100322      // - Indici di schiera
005300100322     d xx              s              3  0
005400100322
005500100322      // - Campi di comodo data
005600100322     d data_ISO        s               d   datfmt(*iso)
005700100322     d data_ISOmin     s               d   datfmt(*iso)
005800100322     d data_ISOmax     s               d   datfmt(*iso)
005900100322
006000100322      // - Campi di comodo
006100100322     d wanno           s              4  0
006200100322     d wconta          s              3  0
006300100802     d wfestamat       s              1a
006400100802     d wfestapom       s              1a
006500100322     d wgiorno         s              2  0
006600100802     d wmatpom         s              1a
006700100322     d wmese           s              2  0
006800100322
006900100322      //---------------------------------------------------------------
007000100322      //?Definizione procedure usate.
007100100322      //---------------------------------------------------------------
007200100322
007300100322
007400100322      //---------------------------------------------------------------
007500100322      //?prototipi
007600100322      //---------------------------------------------------------------
007700100322
007800100322
007900100322      //---------------------------------------------------------------
008000100322      //?Definizione key-list.
008100100322      //---------------------------------------------------------------
008200100322
008300100322       // - File AZCLN01L
008400100322     d k04azcln      e ds                  extname(AZCLN01L:*key)
008500100322     d                                     prefix(k_)
008600100322     d                                     inz
008700100802
008800100802       // - File TABEL00F
008900100802     d k03tabel      e ds                  extname(TABEL00F:*key)
009000100802     d                                     prefix(k_)
009100100322
009200100322      //---------------------------------------------------------------
009300100322      //?Riepilogo indicatori.
009400100322      //---------------------------------------------------------------
009500100322
009600100322     c     *Entry        plist
009700100322     c                   parm                    xgiolavds
009800100322
009900100322      /free
010000100322
010100100322       //?Operazioni iniziali
010200100322       exsr RoutInz;
010300100322
010400100420       //?Elaboro
010500100322       IF  OXGLerr = *blanks;
010600100420         data_ISO = (%date(IXGLdata));
010700100420
010800100420       //?Non lavorativo
010900100420         IF  IXGLlav = *blanks;
011000100420           exsr Elabora;
011100100420         ENDIF;
011200100420
011300100420       //?Elaboro lavorativo
011400100420         IF  IXGLlav = 'S';
011500100420           exsr ElaboraLav;
011600100420         ENDIF;
011700100420
011800100420       ENDIF;
011900100322
012000100322       //?Operazioni finali
012100100322       exsr RoutEnd;
012200100322
012300100322       //--------------------------------------------------------------
012400100322       //?Operazioni iniziali.
012500100322       //--------------------------------------------------------------
012600100322       BEGSR RoutInz;
012700100322
012800100322         clear OXGLdata;
012900100322         clear OXGLerr;
013000100322         clear OXGLmsg;
013100100322
013200100322       //?Controllo se passati i dati obbligatori
013300100322         IF  IXGLfil = 0;
013400100322           OXGLerr = '1';
013500100322           OXGLmsg = 'Filiale non passata';
013600100322           leavesr;
013700100322         ENDIF;
013800100322
013900100322         IF  IXGLgg = 0 and IXGLmm = 0;
014000100322           OXGLerr = '1';
014100100322           OXGLmsg = 'Giorni o mesi non passati';
014200100322           leavesr;
014300100322         ENDIF;
014400100322
014500100322         IF  IXGLgg > 0 and IXGLmm > 0;
014600100322           OXGLerr = '1';
014700100322           OXGLmsg = 'Passati sia giorni che mesi';
014800100322           leavesr;
014900100322         ENDIF;
015000100322
015100100322         IF  IXGLadd = *blanks and IXGLsub = *blanks;
015200100322           OXGLerr = '1';
015300100322           OXGLmsg = 'Non passato se in più o in meno';
015400100322           leavesr;
015500100322         ENDIF;
015600100322
015700100322         IF  IXGLadd <> *blanks and IXGLsub <> *blanks;
015800100322           OXGLerr = '1';
015900100322           OXGLmsg = 'Passato sia ADD che SUB';
016000100322           leavesr;
016100100322         ENDIF;
016200100322
016300100322         IF  IXGLmm > 0 and IXGLlav <> *blanks;
016400100322           OXGLerr = '1';
016500100322           OXGLmsg = 'Non previsto calcolo lavorativo per i mesi';
016600100322           leavesr;
016700100322         ENDIF;
016800100322
016900100322       //?Se non passato Partenza/Arrivo imposto Partenza
017000100322         IF  IXGLpa = *blanks;
017100100322           IXGLpa = 'P';
017200100322         ENDIF;
017300100322
017400100322       //?Se non passato Data inizio imposto Oggi
017500100322         IF  IXGLdata = 0;
017600100322           IXGLdata = %dec(%date());
017700100322         ENDIF;
017800100420
017900100420       //?Imposto allcuni campi chiave per il calendario
018000100420         IF  IXGLpa = 'P';
018100100420           clear k_clntfa;
018200100420           k_clntfp = IXGLfil;
018300100420         ENDIF;
018400100420         IF  IXGLpa = 'A';
018500100420           clear k_clntfp;
018600100420           k_clntfa = IXGLfil;
018700100420         ENDIF;
018800100322
018900100322       ENDSR;
019000100322
019100100322       //--------------------------------------------------------------
019200100420       //?Elaboro non lavorativi.
019300100322       //--------------------------------------------------------------
019400100322       BEGSR Elabora;
019500100322
019600100420         SELECT;
019700100322       //?ADD gg
019800100420           WHEN  IXGLadd <> *blanks and IXGLgg > 0;
019900100420             data_ISO = data_ISO + %days(IXGLgg);
020000100322       //?ADD mm
020100100420           WHEN  IXGLadd <> *blanks and IXGLmm > 0;
020200100420             data_ISO = data_ISO + %months(IXGLmm);
020300100322       //?SUB gg
020400100420           WHEN  IXGLsub <> *blanks and IXGLgg > 0;
020500100420             data_ISO = data_ISO - %days(IXGLgg);
020600100322       //?SUB mm
020700100420           WHEN  IXGLsub <> *blanks and IXGLmm > 0;
020800100420             data_ISO = data_ISO - %months(IXGLmm);
020900100420         ENDSL;
021000100420
021100100420       //?controllo se la data calcolata è festivo
021200100420         $Fine = *off;
021300100420         DOW  not $Fine;
021400100802           clear wmatpom;
021500100802           clear wfestamat;
021600100802           clear wfestapom;
021700100420           wanno   = %subdt(data_ISO:*years);
021800100420           wmese   = %subdt(data_ISO:*months);
021900100420           wgiorno = %subdt(data_ISO:*days);
022000100420           k_clnann = wanno;
022100100420           k_clnmes = wmese;
022200100420           chain %kds(K04azcln) AZCLN01L;
022300100701
022400100420           IF  not %found(AZCLN01L);
022500100420             $Fine = *on;
022600100701           else;
022700100420         //?se festivo devo andare avanti di 1 gg
022800100802           IF  mat(wgiorno) <> *blanks;
022900100802             wmatpom = mat(wgiorno);
023000100802             exsr Festivo;
023100100802             wfestamat = §1Yfei;
023200100802           ENDIF;
023300100802           IF  pom(wgiorno) <> *blanks;
023400100802             wmatpom = pom(wgiorno);
023500100802             exsr Festivo;
023600100802             wfestapom = §1Yfei;
023700100802           ENDIF;
023800100802
023900100802           // IF  mat(wgiorno) = 'F'  or
024000100802           //    pom(wgiorno) = 'F';
024100100802           IF  wfestamat = 'S' or wfestapom = 'S';
024200100420             data_ISO = data_ISO + %days(1);
024300100420           ELSE;
024400100420         //?se lavorativo esco
024500100420             $Fine = *on;
024600100420           ENDIF;
024700100701           ENDIF;
024800100420         ENDDO;
024900100420
025000100420       //?imposto la data trovata
025100100420         OXGLdata = %dec(data_ISO);
025200100420
025300100420       ENDSR;
025400100420
025500100420       //--------------------------------------------------------------
025600100420       //?Elaboro lavorativi.
025700100420       //--------------------------------------------------------------
025800100420       BEGSR ElaboraLav;
025900100322
026000100322       //?Richiesto il lavorativo vado sul calendario
026100100322       //?solo per i GG
026200100322         xx = 1;
026300100322
026400100322       //?Dalla data inizio vado avanti o indietro di n gg lavorativi
026500100322         DOW  xx <= IXGLgg;
026600100322       //?ADD
026700100322           IF  IXGLadd <> *blanks;
026800100322             data_ISO = data_ISO + %days(1);
026900100322           ENDIF;
027000100322       //?SUB
027100100322           IF IXGLsub <> *blanks;
027200100322             data_ISO = data_ISO - %days(1);
027300100322           ENDIF;
027400100420
027500100802           clear wmatpom;
027600100802           clear wfestamat;
027700100802           clear wfestapom;
027800100322           wanno   = %subdt(data_ISO:*years);
027900100322           wmese   = %subdt(data_ISO:*months);
028000100322           wgiorno = %subdt(data_ISO:*days);
028100100322           k_clnann = wanno;
028200100322           k_clnmes = wmese;
028300100322           chain %kds(K04azcln) AZCLN01L;
028400100322           IF  %found(AZCLN01L);
028500100322       //?se festivo ritorno su senza andare avanti con il contatore
028600100802             IF  mat(wgiorno) <> *blanks;
028700100802               wmatpom = mat(wgiorno);
028800100802               exsr Festivo;
028900100802               wfestamat = §1Yfei;
029000100802             ENDIF;
029100100802             IF  pom(wgiorno) <> *blanks;
029200100802               wmatpom = pom(wgiorno);
029300100802               exsr Festivo;
029400100802               wfestapom = §1Yfei;
029500100802             ENDIF;
029600100802             // IF  mat(wgiorno) = 'F'  or
029700100802             //    pom(wgiorno) = 'F';
029800100802             IF  wfestamat = 'S' or wfestapom = 'S';
029900100322             ELSE;
030000100322       //?non è festivo vado avanti con il contatore
030100100322               xx += 1;
030200100322             ENDIF;
030300100701           ELSE;
030400100701       //?non trovato calendario -->  vado avanti con il contatore
030500100701               xx += 1;
030600100322           ENDIF;
030700100322         ENDDO;
030800100322
030900100322       //?imposto la data trovata
031000100322         OXGLdata = %dec(data_ISO);
031100100322
031200100322       ENDSR;
031300100802
031400100802       //--------------------------------------------------------------
031500100802       //?Controllo se giorno festivo.
031600100802       //--------------------------------------------------------------
031700100802       BEGSR Festivo;
031800100802
031900100802         k_TBLkut = 1;
032000100802         k_TBLcod = '1Y';
032100100802         clear ds1Y;
032200100802         k_TBLkey = wmatpom;
032300100802         chain  %kds(K03tabel) TABEL00F;
032400100802         IF  %found(TABEL00F);
032500100802           ds1Y = TBLuni;
032600100802         ENDIF;
032700100802
032800100802       ENDSR;
032900100322
033000100322       //--------------------------------------------------------------
033100100322       //?Operazioni finali.
033200100322       //--------------------------------------------------------------
033300100322       BEGSR RoutEnd;
033400100322
033500100322         *inLR = *on;
033600100322         return;
033700100322
033800100322       ENDSR;
