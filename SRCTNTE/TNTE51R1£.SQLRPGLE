000100130225       //==============================================================
000200130507       //?TNTE51R1 * File tariffe in QTEMP
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
003300130508
003400130508       // -?File Bolle di sede
003500130508     fTITAS30C  if   e           k disk
003600121025
003700121025       //--------------------------------------------------------------
003800121025       //?Definizione costanti.                                        ?
003900121025       //--------------------------------------------------------------
004000121025
004100121025       //--------------------------------------------------------------
004200121025       //?Definizione schiere.                                         ?
004300121025       //--------------------------------------------------------------
004400121025
004500121025       //--------------------------------------------------------------
004600121025       //?Definizione aree dati.                                       ?
004700121025       //--------------------------------------------------------------
004800121025
004900121025       //--------------------------------------------------------------
005000121025       //?Definizione strutture dati.                                  ?
005100121025       //--------------------------------------------------------------
005200130225
005300130226       // -?Parametri ricevuti
005400130225     d param           ds                  inz
005500130507     d  parksc                 1      7  0
005600130507     d  parctr                 8     10  0
005700130507     d  parprg                11     13  0
005800130508     d  pardtcal              14     14
005900130508     d  paraas                15     18  0
006000130508     d  parlnp                19     21  0
006100130508     d  parnrs                22     23  0
006200130508     d  parnsp                24     30  0
006300130509     d  parclose              31     31
006400121025
006500121025       // -?Parametri ricevuti
006600121025     d KPJBA         e ds
006700130226
006800130226       // -?DS per recupero Cartello
006900130226     d TRUL27DS      e ds
007000121025
007100121025       //--------------------------------------------------------------
007200121025       //?Definizione variabili globali.                               ?
007300121025       //--------------------------------------------------------------
007400121025
007500121025       // -?Flags booleani
007600121024     d wEnd            s               n   inz(*off)
007700121024     d wFine           s               n   inz(*off)
007800130225
007900130225       // -?Campi di Comodo
008000130226     d wKSC            s                   like(TAMksc)
008100130226     d wPRG            s                   like(TAMprg)
008200130226
008300130226       // -?Campi Data
008400130226     d wOggi           s              8s 0 inz
008500130508     d wDtsp           s              8s 0 inz
008600130226
008700130226       // -?Nome libreria
008800130226     d wLib            s             10    inz
008900130226
009000130226       // -?Nome libreria + file dei file tariffe di cartello
009100130226     d wFLib           s             21    inz
009200121025
009300121025       //--------------------------------------------------------------
009400121025       //?Definizione procedure esterne.                     ?
009500121025       //--------------------------------------------------------------
009600130226
009700130226       // -?Reperimento Cartello
009800130226     d trul27r         pr                  extpgm('TRUL27R')
009900130226     d  trul27ds                           likeds(trul27ds)
010000121025
010100121024       //--------------------------------------------------------------
010200121024       //?Definizione prototipi.
010300121024       //--------------------------------------------------------------
010400121025
010500121025       //--------------------------------------------------------------
010600121025       //?Definizione key-list.                                        ?
010700121025       //--------------------------------------------------------------
010800121025
010900121025       //--------------------------------------------------------------
011000121025       //?M A I N - L I N E                                            ?
011100121025       //--------------------------------------------------------------
011200121025
011300121025     c     *Entry        plist
011400121025     c                   parm                    KPJBA
011500121025
011600121025      /free
011700121025
011800121024       //?Operazioni iniziali?
011900121025       exsr RoutInz;
012000121025
012100130226       //?Elaborazione?
012200130226       exsr Elabora;
012300121025
012400121024       //?Operazioni finali?
012500121025       exsr RoutEnd;
012600121025
012700121025       //--------------------------------------------------------------
012800121025       //?Operazioni iniziali.                                         ?
012900121025       //--------------------------------------------------------------
013000121024       BEGSR  RoutInz;
013100130226
013200130226         exec sql  set option  dynusrprf = *owner,
013300130226                               closqlcsr = *endmod;
013400130226
013500130226       //?Reperimento della data corrente (in formato aaaa/mm/gg)?
013600130226         wOggi = %dec(%date());
013700130222
013800130226       //?Imposto i dati ricevuti nella Parm
013900130226         param = KPJBU;
014000130226
014100130226       //?Determino la libreria
014200130226         IF  %subst(knsif : 7 : 1) = 'P';
014300130226           wLib = 'UNITRAGRP';
014400130226         ELSE;
014500130226           wLib = 'UNITRAGRU';
014600130226         ENDIF;
014700130226
014800130226       //?Apro i file
014900130226         wFLib = %trim(wLib) + '/TNTAM00L';
015000130226         open  TNTAM00L;
015100130226         wFLib = %trim(wLib) + '/TITPT01L';
015200130226         open  TITPT01L;
015300130226         wFLib = %trim(wLib) + '/TITPD01L';
015400130226         open  TITPD01L;
015500130226         wFLib = %trim(wLib) + '/TITGC01L';
015600130226         open  TITGC01L;
015700121025
015800121025       ENDSR;
015900121024
016000121024       //--------------------------------------------------------------
016100130226       //?Elaborazione.
016200121024       //--------------------------------------------------------------
016300130226       BEGSR  Elabora;
016400130226
016500130226       //?Come prima cosa sistemo i dati presenti sui file in qtemp
016600130226       chain (parksc) TIVIS05L;
016700130226       IF  not %found(TIVIS05L);
016800130226         leavesr;
016900130226       ENDIF;
017000130226       //?Se trattativa su cliente codificato imposto TAMKSC = VISKSC
017100130226       IF  VISksc > 0;
017200130226         wKSC = VISksc;
017300130226       ENDIF;
017400130226       //?altrimenti imposto TAMKSC = filiale commerciale + 1111
017500130226       IF  VISksc = 0;
017600130226         wKSC = %dec(%subst(%editc(VIScmm:'X'):1:3):3:0) * 10000 + 1111;
017700130226       ENDIF;
017800130226
017900130226       exec sql
018000130226         UPDATE TNTAM00F
018100130226         SET TAMksc = :wKSC;
018200130226       exec sql
018300130226         UPDATE TITAD00F
018400130226         SET TADksc = :wKSC;
018500130226       exec sql
018600130226         UPDATE TITPT00F
018700130226         SET TPTksc = :wKSC;
018800130226       exec sql
018900130226         UPDATE TITPD00F
019000130226         SET TPDksc = :wKSC;
019100130226       exec sql
019200130226         UPDATE TITGC00F
019300130226         SET TGCksc = :wKSC;
019400130226
019500130226       //?Imposto il nuovo codice nel campo della PARAM
019600130226         parksc = wKSC;
019700130508
019800130508       //?Aggancio la bolla per recuperare la data spedizione in caso
019900130508       //?di data calcolo = 'S=Spedizione'
020000130508         IF  pardtcal = 'S';
020100130508           clear wDtsp;
020200130508           chain (paraas:parlnp:parnrs:parnsp) TITAS30C;
020300130508           IF  %found(TITAS30C);
020400130508             wDtsp = TASaas * 10000 + TASmgs;
020500130508           ENDIF;
020600130508         ENDIF;
020700130226
020800130226       //?Devo aggiungere i dati relativi alle cartello
020900130226       //?senza il dettaglio
021000130226
021100130226       //?Cartello Italia/EEX  (per ora la cartello è unica)
021200130226         clear TRUL27DS;
021300130226         I27tla = 'L';
021400130226         I27ntw = 'COR';
021500130226         trul27r (TRUL27DS);
021600130226         IF  O27err <> *blanks;
021700130226           leavesr;
021800130226         ENDIF;
021900130226
022000130226       //?Accodo la cartello
022100130226         exsr CopiaCar;
022200130226
022300130226       //?Cartello DPD
022400130226         clear TRUL27DS;
022500130226         I27tla = 'L';
022600130226         I27ntw = 'DPD';
022700130226         trul27r (TRUL27DS);
022800130226         IF  O27err <> *blanks;
022900130226           leavesr;
023000130226         ENDIF;
023100130226
023200130226       //?Accodo la cartello
023300130226         exsr CopiaCar;
023400130228
023500130228       //?Carico la cartello FedEx x il TNSF20R
023600130228       //?Cartello FED documenti
023700130228         clear TRUL27DS;
023800130228         I27tla = 'L';
023900130228         I27ntw = 'FED';
024000130228         I27tip = 'FD';
024100130228         trul27r (TRUL27DS);
024200130228         IF  O27err <> *blanks;
024300130228           leavesr;
024400130228         ENDIF;
024500130228
024600130228       //?Accodo la cartello
024700130228         exsr CopiaCar;
024800130228
024900130228       //?Cartello FED merci
025000130228         clear TRUL27DS;
025100130228         I27tla = 'L';
025200130228         I27ntw = 'FED';
025300130228         I27tip = 'FM';
025400130228         trul27r (TRUL27DS);
025500130228         IF  O27err <> *blanks;
025600130228           leavesr;
025700130228         ENDIF;
025800130228
025900130228       //?Accodo la cartello
026000130228         exsr CopiaCar;
026100121024
026200121024       ENDSR;
026300130226
026400130226       //--------------------------------------------------------------
026500130226       //?Accodo la cartello.                                          ?
026600130226       //--------------------------------------------------------------
026700130226       BEGSR  CopiaCar;
026800130226
026900130226       //?Cerco l'ultimo progressivo valido
027000130226         setgt (O27ksc:O27ctr) TNTAM00L;
027100130226         readpe (O27ksc:O27ctr) TNTAM00L;
027200130228         DOW  not %eof(TNTAM00L);
027300130228           IF  (pardtcal = 'O' and wOggi >= TAMddt and wOggi <= TAMdst) or
027400130508               (pardtcal = 'S' and wDtsp >= TAMddt and wDtsp <= TAMdst);
027500130228             wPRG = TAMprg;
027600130228             leave;
027700130228           ELSE;
027800130228             readpe (O27ksc:O27ctr) TNTAM00L;
027900130228           ENDIF;
028000130228         ENDDO;
028100130226
028200130226       //?TNTAM
028300130226         chain (O27ksc:O27ctr:wPRG) TNTAM00L;
028400130226         IF  %found(TNTAM00L);
028500130226           write TNTAM000;
028600130509           feod TNTAM00F;
028700130226         ENDIF;
028800130226
028900130226       //?TITPT
029000130226         setll (O27ksc:O27ctr:wPRG) TITPT01L;
029100130226         reade (O27ksc:O27ctr:wPRG) TITPT01L;
029200130226         DOW  not  %eof(TITPT01L);
029300130226           IF  TPTatb = *blanks;
029400130226             write TITPT000;
029500130509             feod TITPT00F;
029600130226           ENDIF;
029700130226           reade (O27ksc:O27ctr:wPRG) TITPT01L;
029800130226         ENDDO;
029900130226
030000130226       //?TITPD
030100130226         setll (O27ksc:O27ctr:wPRG) TITPD01L;
030200130226         reade (O27ksc:O27ctr:wPRG) TITPD01L;
030300130226         DOW  not  %eof(TITPD01L);
030400130226           IF  TPTatb = *blanks;
030500130226             write TITPD000;
030600130509             feod TITPD00F;
030700130226           ENDIF;
030800130226           reade (O27ksc:O27ctr:wPRG) TITPD01L;
030900130226         ENDDO;
031000130226
031100130226       //?TITGC
031200130226         setll (O27ksc:O27ctr:wPRG) TITGC01L;
031300130226         reade (O27ksc:O27ctr:wPRG) TITGC01L;
031400130226         DOW  not  %eof(TITGC01L);
031500130226           IF  TPTatb = *blanks;
031600130226             write TITGC000;
031700130509             feod TITGC00F;
031800130226           ENDIF;
031900130226           reade (O27ksc:O27ctr:wPRG) TITGC01L;
032000130226         ENDDO;
032100130226
032200130226       ENDSR;
032300121025
032400121025       //--------------------------------------------------------------
032500121025       //?Operazioni finali.                                           ?
032600121025       //--------------------------------------------------------------
032700121024       BEGSR  RoutEnd;
032800130509
032900130509       //?Rilascio i file delle tariffe
033000130509         close TNTAM00L;
033100130509         close TITPT01L;
033200130509         close TITPD01L;
033300130509         close TITGC01L;
033400130228
033500130228       //?Reimposto la kpjbu
033600130228         kpjbu = param;
033700121025
033800121025         *inLR = *on;
033900100507
034000121025       //?Uscita dal pgm
034100121025         return;
034200121025
034300121025       ENDSR;
034400121025
034500121025      /end-free
