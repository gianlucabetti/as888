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
001400100322
001500100322      //---------------------------------------------------------------
001600100322      //?Definizione costanti.
001700100322      //---------------------------------------------------------------
001800100322
001900100322
002000100322      //---------------------------------------------------------------
002100100322      //?Definizione schiere.
002200100322      //---------------------------------------------------------------
002300100322
002400100322      // - Mattino/Pomeriggio calendario
002500100322     d CLNmat          DS
002600100322     d  mat                    1     31    dim(31)
002700100322     d CLNpom          DS
002800100322     d  pom                    1     31    dim(31)
002900100322
003000100322      //---------------------------------------------------------------
003100100322      //?Definizione aree dati.
003200100322      //---------------------------------------------------------------
003300100322
003400100322
003500100322      //---------------------------------------------------------------
003600100322      //?Definizione strutture dati.
003700100322      //---------------------------------------------------------------
003800100322
003900100322     d xgiolavds     e ds
004000100322
004100100322      //---------------------------------------------------------------
004200100322      //?Definizione variabili globali.
004300100322      //---------------------------------------------------------------
004400100322
004500100322      // - Flags booleani
004600100420     d $Fine           s               n   inz(*off)
004700100322
004800100322      // - Indici di schiera
004900100322     d xx              s              3  0
005000100322
005100100322      // - Campi di comodo data
005200100322     d data_ISO        s               d   datfmt(*iso)
005300100322     d data_ISOmin     s               d   datfmt(*iso)
005400100322     d data_ISOmax     s               d   datfmt(*iso)
005500100322
005600100322      // - Campi di comodo
005700100322     d wanno           s              4  0
005800100322     d wconta          s              3  0
005900100322     d wgiorno         s              2  0
006000100322     d wmese           s              2  0
006100100322
006200100322      //---------------------------------------------------------------
006300100322      //?Definizione procedure usate.
006400100322      //---------------------------------------------------------------
006500100322
006600100322
006700100322      //---------------------------------------------------------------
006800100322      //?prototipi
006900100322      //---------------------------------------------------------------
007000100322
007100100322
007200100322      //---------------------------------------------------------------
007300100322      //?Definizione key-list.
007400100322      //---------------------------------------------------------------
007500100322
007600100322       // - File AZCLN01L
007700100322     d k04azcln      e ds                  extname(AZCLN01L:*key)
007800100322     d                                     prefix(k_)
007900100322     d                                     inz
008000100322
008100100322      //---------------------------------------------------------------
008200100322      //?Riepilogo indicatori.
008300100322      //---------------------------------------------------------------
008400100322
008500100322     c     *Entry        plist
008600100322     c                   parm                    xgiolavds
008700100322
008800100322      /free
008900100322
009000100322       //?Operazioni iniziali
009100100322       exsr RoutInz;
009200100322
009300100420       //?Elaboro
009400100322       IF  OXGLerr = *blanks;
009500100420         data_ISO = (%date(IXGLdata));
009600100420
009700100420       //?Non lavorativo
009800100420         IF  IXGLlav = *blanks;
009900100420           exsr Elabora;
010000100420         ENDIF;
010100100420
010200100420       //?Elaboro lavorativo
010300100420         IF  IXGLlav = 'S';
010400100420           exsr ElaboraLav;
010500100420         ENDIF;
010600100420
010700100420       ENDIF;
010800100322
010900100322       //?Operazioni finali
011000100322       exsr RoutEnd;
011100100322
011200100322       //--------------------------------------------------------------
011300100322       //?Operazioni iniziali.
011400100322       //--------------------------------------------------------------
011500100322       BEGSR RoutInz;
011600100322
011700100322         clear OXGLdata;
011800100322         clear OXGLerr;
011900100322         clear OXGLmsg;
012000100322
012100100322       //?Controllo se passati i dati obbligatori
012200100322         IF  IXGLfil = 0;
012300100322           OXGLerr = '1';
012400100322           OXGLmsg = 'Filiale non passata';
012500100322           leavesr;
012600100322         ENDIF;
012700100322
012800100322         IF  IXGLgg = 0 and IXGLmm = 0;
012900100322           OXGLerr = '1';
013000100322           OXGLmsg = 'Giorni o mesi non passati';
013100100322           leavesr;
013200100322         ENDIF;
013300100322
013400100322         IF  IXGLgg > 0 and IXGLmm > 0;
013500100322           OXGLerr = '1';
013600100322           OXGLmsg = 'Passati sia giorni che mesi';
013700100322           leavesr;
013800100322         ENDIF;
013900100322
014000100322         IF  IXGLadd = *blanks and IXGLsub = *blanks;
014100100322           OXGLerr = '1';
014200100322           OXGLmsg = 'Non passato se in più o in meno';
014300100322           leavesr;
014400100322         ENDIF;
014500100322
014600100322         IF  IXGLadd <> *blanks and IXGLsub <> *blanks;
014700100322           OXGLerr = '1';
014800100322           OXGLmsg = 'Passato sia ADD che SUB';
014900100322           leavesr;
015000100322         ENDIF;
015100100322
015200100322         IF  IXGLmm > 0 and IXGLlav <> *blanks;
015300100322           OXGLerr = '1';
015400100322           OXGLmsg = 'Non previsto calcolo lavorativo per i mesi';
015500100322           leavesr;
015600100322         ENDIF;
015700100322
015800100322       //?Se non passato Partenza/Arrivo imposto Partenza
015900100322         IF  IXGLpa = *blanks;
016000100322           IXGLpa = 'P';
016100100322         ENDIF;
016200100322
016300100322       //?Se non passato Data inizio imposto Oggi
016400100322         IF  IXGLdata = 0;
016500100322           IXGLdata = %dec(%date());
016600100322         ENDIF;
016700100420
016800100420       //?Imposto allcuni campi chiave per il calendario
016900100420         IF  IXGLpa = 'P';
017000100420           clear k_clntfa;
017100100420           k_clntfp = IXGLfil;
017200100420         ENDIF;
017300100420         IF  IXGLpa = 'A';
017400100420           clear k_clntfp;
017500100420           k_clntfa = IXGLfil;
017600100420         ENDIF;
017700100322
017800100322       ENDSR;
017900100322
018000100322       //--------------------------------------------------------------
018100100420       //?Elaboro non lavorativi.
018200100322       //--------------------------------------------------------------
018300100322       BEGSR Elabora;
018400100322
018500100420         SELECT;
018600100322       //?ADD gg
018700100420           WHEN  IXGLadd <> *blanks and IXGLgg > 0;
018800100420             data_ISO = data_ISO + %days(IXGLgg);
018900100322       //?ADD mm
019000100420           WHEN  IXGLadd <> *blanks and IXGLmm > 0;
019100100420             data_ISO = data_ISO + %months(IXGLmm);
019200100322       //?SUB gg
019300100420           WHEN  IXGLsub <> *blanks and IXGLgg > 0;
019400100420             data_ISO = data_ISO - %days(IXGLgg);
019500100322       //?SUB mm
019600100420           WHEN  IXGLsub <> *blanks and IXGLmm > 0;
019700100420             data_ISO = data_ISO - %months(IXGLmm);
019800100420         ENDSL;
019900100420
020000100420       //?controllo se la data calcolata è festivo
020100100420         $Fine = *off;
020200100420         DOW  not $Fine;
020300100420           wanno   = %subdt(data_ISO:*years);
020400100420           wmese   = %subdt(data_ISO:*months);
020500100420           wgiorno = %subdt(data_ISO:*days);
020600100420           k_clnann = wanno;
020700100420           k_clnmes = wmese;
020800100420           chain %kds(K04azcln) AZCLN01L;
020900100420           IF  not %found(AZCLN01L);
021000100420             $Fine = *on;
021100100420           ENDIF;
021200100420         //?se festivo devo andare avanti di 1 gg
021300100420           IF  mat(wgiorno) = 'F'  or
021400100420               pom(wgiorno) = 'F';
021500100420             data_ISO = data_ISO + %days(1);
021600100420           ELSE;
021700100420         //?se lavorativo esco
021800100420             $Fine = *on;
021900100420           ENDIF;
022000100420         ENDDO;
022100100420
022200100420       //?imposto la data trovata
022300100420         OXGLdata = %dec(data_ISO);
022400100420
022500100420       ENDSR;
022600100420
022700100420       //--------------------------------------------------------------
022800100420       //?Elaboro lavorativi.
022900100420       //--------------------------------------------------------------
023000100420       BEGSR ElaboraLav;
023100100322
023200100322       //?Richiesto il lavorativo vado sul calendario
023300100322       //?solo per i GG
023400100322         xx = 1;
023500100322
023600100322       //?Dalla data inizio vado avanti o indietro di n gg lavorativi
023700100322         DOW  xx <= IXGLgg;
023800100322       //?ADD
023900100322           IF  IXGLadd <> *blanks;
024000100322             data_ISO = data_ISO + %days(1);
024100100322           ENDIF;
024200100322       //?SUB
024300100322           IF IXGLsub <> *blanks;
024400100322             data_ISO = data_ISO - %days(1);
024500100322           ENDIF;
024600100420
024700100322           wanno   = %subdt(data_ISO:*years);
024800100322           wmese   = %subdt(data_ISO:*months);
024900100322           wgiorno = %subdt(data_ISO:*days);
025000100322           k_clnann = wanno;
025100100322           k_clnmes = wmese;
025200100322           chain %kds(K04azcln) AZCLN01L;
025300100322           IF  %found(AZCLN01L);
025400100322       //?se festivo ritorno su senza andare avanti con il contatore
025500100322             IF  mat(wgiorno) = 'F'  or
025600100322                 pom(wgiorno) = 'F';
025700100322             ELSE;
025800100322       //?non è festivo vado avanti con il contatore
025900100322               xx += 1;
026000100322             ENDIF;
026100100322           ENDIF;
026200100322         ENDDO;
026300100322
026400100322       //?imposto la data trovata
026500100322         OXGLdata = %dec(data_ISO);
026600100322
026700100322       ENDSR;
026800100322
026900100322       //--------------------------------------------------------------
027000100322       //?Operazioni finali.
027100100322       //--------------------------------------------------------------
027200100322       BEGSR RoutEnd;
027300100322
027400100322         *inLR = *on;
027500100322         return;
027600100322
027700100322       ENDSR;
