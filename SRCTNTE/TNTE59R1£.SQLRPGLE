000100130225       //==============================================================
000200130226       //?TNTE59R1 * File tariffe in QTEMP
000300130225       //==============================================================
000400121025
000500121025       //--------------------------------------------------------------
000600121025       //?Specifiche di controllo.                                     ?
000700121025       //--------------------------------------------------------------
000800121025
000900121025     h decedit('0,') datedit(*ymd/) option(*nodebugio)
001000121025
001100121025       //--------------------------------------------------------------
001200121025       //?Dichiarazione file.                                          ?
001300121025       //--------------------------------------------------------------
001400130225
001500130226       // -?File Tariffe QTEMP
001600130226     fTNTAM00F  o    e             disk
001700130226     fTITPT00F  o    e             disk
001800130226     fTITPD00F  o    e             disk
001900130226     fTITGC00F  o    e             disk
002000130226
002100130226       // -?File Tariffe Cartello
002200130226     fTNTAM00L  if   e           k disk    usropn extfile(wFLib)
002300130226     f                                     rename(TNTAM000:TNTAMCAR)
002400130226     fTITPT01L  if   e           k disk    usropn extfile(wFLib)
002500130226     f                                     rename(TITPT000:TITPTCAR)
002600130226     fTITPD01L  if   e           k disk    usropn extfile(wFLib)
002700130226     f                                     rename(TITPD000:TITPDCAR)
002800130226     fTITGC01L  if   e           k disk    usropn extfile(wFLib)
002900130226     f                                     rename(TITGC000:TITGCCAR)
003000130226
003100130226       // -?File Trattative
003200130226     fTIVIS05L  if   e           k disk
003300121025
003400121025       //--------------------------------------------------------------
003500121025       //?Definizione costanti.                                        ?
003600121025       //--------------------------------------------------------------
003700121025
003800121025       //--------------------------------------------------------------
003900121025       //?Definizione schiere.                                         ?
004000121025       //--------------------------------------------------------------
004100121025
004200121025       //--------------------------------------------------------------
004300121025       //?Definizione aree dati.                                       ?
004400121025       //--------------------------------------------------------------
004500121025
004600121025       //--------------------------------------------------------------
004700121025       //?Definizione strutture dati.                                  ?
004800121025       //--------------------------------------------------------------
004900130225
005000130226       // -?Parametri ricevuti
005100130225     d param           ds                  inz
005200130225     d  parks1                 1      7  0
005300130225     d  parks2                 8     14  0
005400130225     d  parks3                15     21  0
005500130225     d  parks4                22     28  0
005600130225     d  parks5                29     35  0
005700130225     d  parta1                36     38  0
005800130225     d  parta2                39     41  0
005900130225     d  parta3                42     44  0
006000130225     d  parta4                45     47  0
006100130225     d  parta5                48     50  0
006200130228     d  parln1                51     53  0
006300130228     d  parln2                54     56  0
006400130228     d  parln3                57     59  0
006500130228     d  parln4                60     62  0
006600130228     d  parln5                63     65  0
006700130228     d  pards1                66     73  0
006800130228     d  pards2                74     81  0
006900130228     d  parct1                82     83
007000130228     d  parct2                84     85
007100130228     d  parct3                86     87
007200130228     d  parct4                88     89
007300130228     d  parct5                90     91
007400130228     d  parct6                92     93
007500130228     d  parct7                94     95
007600130228     d  parct8                96     97
007700130228     d  parct9                98     99
007800130228     d  parct10              100    101
007900130228     d  parkgd               102    108  1
008000130228     d  parkga               109    115  1
008100130228     d  parcod               116    121  0
008200130228     d  parcoa               122    127  0
008300130228     d  partaof              128    128
008400130228     d  parksc               129    135  0
008500130228     d  parctr               136    138  0
008600130228     d  parprg               139    141  0
008700130228     d  parlnp               142    144  0
008800130228     d  pardtcal             145    145
008900130228     d  pardet               146    146
009000130228     d  parnrv               147    153  0
009100130228     d  parddt               154    161  0
009200121025
009300121025       // -?Parametri ricevuti
009400121025     d KPJBA         e ds
009500130226
009600130226       // -?DS per recupero Cartello
009700130226     d TRUL27DS      e ds
009800121025
009900121025       //--------------------------------------------------------------
010000121025       //?Definizione variabili globali.                               ?
010100121025       //--------------------------------------------------------------
010200121025
010300121025       // -?Flags booleani
010400121024     d wEnd            s               n   inz(*off)
010500121024     d wFine           s               n   inz(*off)
010600130225
010700130225       // -?Campi di Comodo
010800130226     d wKSC            s                   like(TAMksc)
010900130226     d wPRG            s                   like(TAMprg)
011000130226
011100130226       // -?Campi Data
011200130226     d wOggi           s              8s 0 inz
011300130226
011400130226       // -?Nome libreria
011500130226     d wLib            s             10    inz
011600130226
011700130226       // -?Nome libreria + file dei file tariffe di cartello
011800130226     d wFLib           s             21    inz
011900121025
012000121025       //--------------------------------------------------------------
012100121025       //?Definizione procedure esterne.                     ?
012200121025       //--------------------------------------------------------------
012300130226
012400130226       // -?Reperimento Cartello
012500130226     d trul27r         pr                  extpgm('TRUL27R')
012600130226     d  trul27ds                           likeds(trul27ds)
012700121025
012800121024       //--------------------------------------------------------------
012900121024       //?Definizione prototipi.
013000121024       //--------------------------------------------------------------
013100121025
013200121025       //--------------------------------------------------------------
013300121025       //?Definizione key-list.                                        ?
013400121025       //--------------------------------------------------------------
013500121025
013600121025       //--------------------------------------------------------------
013700121025       //?M A I N - L I N E                                            ?
013800121025       //--------------------------------------------------------------
013900121025
014000121025     c     *Entry        plist
014100121025     c                   parm                    KPJBA
014200121025
014300121025      /free
014400121025
014500121024       //?Operazioni iniziali?
014600121025       exsr RoutInz;
014700121025
014800130226       //?Elaborazione?
014900130226       exsr Elabora;
015000121025
015100121024       //?Operazioni finali?
015200121025       exsr RoutEnd;
015300121025
015400121025       //--------------------------------------------------------------
015500121025       //?Operazioni iniziali.                                         ?
015600121025       //--------------------------------------------------------------
015700121024       BEGSR  RoutInz;
015800130226
015900130226         exec sql  set option  dynusrprf = *owner,
016000130226                               closqlcsr = *endmod;
016100130226
016200130226       //?Reperimento della data corrente (in formato aaaa/mm/gg)?
016300130226         wOggi = %dec(%date());
016400130222
016500130226       //?Imposto i dati ricevuti nella Parm
016600130226         param = KPJBU;
016700130226
016800130226       //?Determino la libreria
016900130226         IF  %subst(knsif : 7 : 1) = 'P';
017000130226           wLib = 'UNITRAGRP';
017100130226         ELSE;
017200130226           wLib = 'UNITRAGRU';
017300130226         ENDIF;
017400130226
017500130226       //?Apro i file
017600130226         wFLib = %trim(wLib) + '/TNTAM00L';
017700130226         open  TNTAM00L;
017800130226         wFLib = %trim(wLib) + '/TITPT01L';
017900130226         open  TITPT01L;
018000130226         wFLib = %trim(wLib) + '/TITPD01L';
018100130226         open  TITPD01L;
018200130226         wFLib = %trim(wLib) + '/TITGC01L';
018300130226         open  TITGC01L;
018400121025
018500121025       ENDSR;
018600121024
018700121024       //--------------------------------------------------------------
018800130226       //?Elaborazione.
018900121024       //--------------------------------------------------------------
019000130226       BEGSR  Elabora;
019100130226
019200130226       //?Come prima cosa sistemo i dati presenti sui file in qtemp
019300130226       chain (parksc) TIVIS05L;
019400130226       IF  not %found(TIVIS05L);
019500130226         leavesr;
019600130226       ENDIF;
019700130226       //?Se trattativa su cliente codificato imposto TAMKSC = VISKSC
019800130226       IF  VISksc > 0;
019900130226         wKSC = VISksc;
020000130226       ENDIF;
020100130226       //?altrimenti imposto TAMKSC = filiale commerciale + 1111
020200130226       IF  VISksc = 0;
020300130226         wKSC = %dec(%subst(%editc(VIScmm:'X'):1:3):3:0) * 10000 + 1111;
020400130226       ENDIF;
020500130226
020600130226       exec sql
020700130226         UPDATE TNTAM00F
020800130226         SET TAMksc = :wKSC;
020900130226       exec sql
021000130226         UPDATE TITAD00F
021100130226         SET TADksc = :wKSC;
021200130226       exec sql
021300130226         UPDATE TITPT00F
021400130226         SET TPTksc = :wKSC;
021500130226       exec sql
021600130226         UPDATE TITPD00F
021700130226         SET TPDksc = :wKSC;
021800130226       exec sql
021900130226         UPDATE TITGC00F
022000130226         SET TGCksc = :wKSC;
022100130226
022200130226       //?Imposto il nuovo codice nel campo della PARAM
022300130226         parksc = wKSC;
022400130226
022500130226       //?Devo aggiungere i dati relativi alle cartello
022600130226       //?senza il dettaglio
022700130226
022800130226       //?Cartello Italia/EEX  (per ora la cartello è unica)
022900130226         clear TRUL27DS;
023000130226         I27tla = 'L';
023100130226         I27ntw = 'COR';
023200130226         trul27r (TRUL27DS);
023300130226         IF  O27err <> *blanks;
023400130226           leavesr;
023500130226         ENDIF;
023600130226
023700130226       //?Accodo la cartello
023800130226         exsr CopiaCar;
023900130226
024000130226       //?Cartello DPD
024100130226         clear TRUL27DS;
024200130226         I27tla = 'L';
024300130226         I27ntw = 'DPD';
024400130226         trul27r (TRUL27DS);
024500130226         IF  O27err <> *blanks;
024600130226           leavesr;
024700130226         ENDIF;
024800130226
024900130226       //?Accodo la cartello
025000130226         exsr CopiaCar;
025100130228
025200130228       //?Carico la cartello FedEx x il TNSF20R
025300130228       //?Cartello FED documenti
025400130228         clear TRUL27DS;
025500130228         I27tla = 'L';
025600130228         I27ntw = 'FED';
025700130228         I27tip = 'FD';
025800130228         trul27r (TRUL27DS);
025900130228         IF  O27err <> *blanks;
026000130228           leavesr;
026100130228         ENDIF;
026200130228
026300130228       //?Accodo la cartello
026400130228         exsr CopiaCar;
026500130228
026600130228       //?Cartello FED merci
026700130228         clear TRUL27DS;
026800130228         I27tla = 'L';
026900130228         I27ntw = 'FED';
027000130228         I27tip = 'FM';
027100130228         trul27r (TRUL27DS);
027200130228         IF  O27err <> *blanks;
027300130228           leavesr;
027400130228         ENDIF;
027500130228
027600130228       //?Accodo la cartello
027700130228         exsr CopiaCar;
027800121024
027900121024       ENDSR;
028000130226
028100130226       //--------------------------------------------------------------
028200130226       //?Accodo la cartello.                                          ?
028300130226       //--------------------------------------------------------------
028400130226       BEGSR  CopiaCar;
028500130226
028600130226       //?Cerco l'ultimo progressivo valido
028700130226         setgt (O27ksc:O27ctr) TNTAM00L;
028800130226         readpe (O27ksc:O27ctr) TNTAM00L;
028900130228         DOW  not %eof(TNTAM00L);
029000130228           IF  (pardtcal = 'O' and wOggi >= TAMddt and wOggi <= TAMdst) or
029100130228               (pardtcal = 'S' and pards1 >= TAMddt and pards1 <= TAMdst);
029200130228             wPRG = TAMprg;
029300130228             leave;
029400130228           ELSE;
029500130228             readpe (O27ksc:O27ctr) TNTAM00L;
029600130228           ENDIF;
029700130228         ENDDO;
029800130226
029900130226       //?TNTAM
030000130226         chain (O27ksc:O27ctr:wPRG) TNTAM00L;
030100130226         IF  %found(TNTAM00L);
030200130226           write TNTAM000;
030300130226         ENDIF;
030400130226
030500130226       //?TITPT
030600130226         setll (O27ksc:O27ctr:wPRG) TITPT01L;
030700130226         reade (O27ksc:O27ctr:wPRG) TITPT01L;
030800130226         DOW  not  %eof(TITPT01L);
030900130226           IF  TPTatb = *blanks;
031000130226             write TITPT000;
031100130226           ENDIF;
031200130226           reade (O27ksc:O27ctr:wPRG) TITPT01L;
031300130226         ENDDO;
031400130226
031500130226       //?TITPD
031600130226         setll (O27ksc:O27ctr:wPRG) TITPD01L;
031700130226         reade (O27ksc:O27ctr:wPRG) TITPD01L;
031800130226         DOW  not  %eof(TITPD01L);
031900130226           IF  TPTatb = *blanks;
032000130226             write TITPD000;
032100130226           ENDIF;
032200130226           reade (O27ksc:O27ctr:wPRG) TITPD01L;
032300130226         ENDDO;
032400130226
032500130226       //?TITGC
032600130226         setll (O27ksc:O27ctr:wPRG) TITGC01L;
032700130226         reade (O27ksc:O27ctr:wPRG) TITGC01L;
032800130226         DOW  not  %eof(TITGC01L);
032900130226           IF  TPTatb = *blanks;
033000130226             write TITGC000;
033100130226           ENDIF;
033200130226           reade (O27ksc:O27ctr:wPRG) TITGC01L;
033300130226         ENDDO;
033400130226
033500130226       ENDSR;
033600121025
033700121025       //--------------------------------------------------------------
033800121025       //?Operazioni finali.                                           ?
033900121025       //--------------------------------------------------------------
034000121024       BEGSR  RoutEnd;
034100130228
034200130228       //?Reimposto la kpjbu
034300130228         kpjbu = param;
034400121025
034500121025         *inLR = *on;
034600100507
034700121025       //?Uscita dal pgm
034800121025         return;
034900121025
035000121025       ENDSR;
035100121025
035200121025      /end-free
