000100120411      //--------------------------------------------------------------
000200120911      //?TNSB70R1 - Elaborazione Immagini scartate
000300120411      //--------------------------------------------------------------
000400120411
000500120411     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600120411
000700120411      //---------------------------------------------------------------
000800120411      //?Dichiarazione file.
000900120411      //---------------------------------------------------------------
001000040721
001100120411       // -?File bolle di sede
001200120911     fFIAR531C  uf   e           k disk
001300120912     fTITAS30C  if   e           k disk
001400120411
001500120606       // -?File documenti
001600120606     fTIJDC01L  uf a e           k disk
001700120411
001800120411
001900120411      //---------------------------------------------------------------
002000120411      //?Definizione costanti.
002100120411      //---------------------------------------------------------------
002200120411
002300120411      //---------------------------------------------------------------
002400120411      //?Definizione schiere.
002500120411      //---------------------------------------------------------------
002600120411
002700120411      //---------------------------------------------------------------
002800120411      //?Definizione aree dati.
002900120411      //---------------------------------------------------------------
003000120411
003100120411      //---------------------------------------------------------------
003200120411      //?Definizione strutture dati.
003300120411      //---------------------------------------------------------------
003400120411
003500120411       // -?Paremetri ricevuti
003600120411     d KPJBA         e ds
003700120411
003800120606       // -?DS campo AR5 rcd DOC
003900120606     d dAR5doc       e ds
004000120911
004100120911       // -?File TIJDC00F
004200120911     d TIJDCds       e ds                  extname(TIJDC00F)
004300120911     d                                     prefix(JDC:3)
004400120411
004500120411      //---------------------------------------------------------------
004600120411      //?Definizione variabili globali.
004700120411      //---------------------------------------------------------------
004800120411
004900120411       // -?Flags booleani
005000120411     d wEoF            s               n   inz(*off)
005100120411     d wFine           s               n   inz(*off)
005200120412
005300120412       // -?Campi di comodo
005400120911     d chiudi          s              1
005500120606     d kAR5trd         s                   like(AR5trd) inz('DOC')
005600120411
005700120411      //---------------------------------------------------------------
005800120411      //?Definizione procedure esterne.
005900120411      //---------------------------------------------------------------
006000120411
006100120411      //---------------------------------------------------------------
006200120411      //?Definizione prototipi.
006300120411      //---------------------------------------------------------------
006400120411
006500120411      //---------------------------------------------------------------
006600120411      //?Definizione key-list.
006700120411      //---------------------------------------------------------------
006800120411
006900120411      //---------------------------------------------------------------
007000120411      //?Riepilogo indicatori.
007100120411      //---------------------------------------------------------------
007200120411
007300120411      //---------------------------------------------------------------
007400120911
007500120911      //---------------------------------------------------------------
007600120911      //?Indicatori per riconoscere i tipi rcd del file FIAR5
007700120911      //---------------------------------------------------------------
007800120911     iFIAR5000      41
007900120911     iFIAR5P00      43
008000120911
008100120911      //---------------------------------------------------------------
008200120411
008300120411      //---------------------------------------------------------------
008400120411      //?M A I N - L I N E
008500120411      //---------------------------------------------------------------
008600120411
008700120411     c     *Entry        plist
008800120911     c                   parm                    chiudi
008900120411
009000120411      /free
009100120411
009200120411       //?Operazioni iniziali
009300120411       exsr RoutInz;
009400120411
009500120911       //?Elabora i rcd '94' (scan. di 1 sola pag. con CL1)
009600120911       exsr Elabora;
009700120411
009800120411       //?Operazioni finali
009900120411       exsr RoutEnd;
010000120411
010100120411       //--------------------------------------------------------------
010200120411       //?Operazioni iniziali.
010300120411       //--------------------------------------------------------------
010400120411       BEGSR RoutInz;
010500120411
010600120911         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
010700120411
010800120411       ENDSR;
010900120411
011000120411       //--------------------------------------------------------------
011100120911       //?Elaboro i rcd con LACELA = '94'.
011200120411       //--------------------------------------------------------------
011300120911       BEGSR Elabora;
011400120412
011500120911       //?Leggo da TIJDC solo i rcd scartati con 94
011600120911         exec sql
011700120911          DECLARE  JDC94 cursor for
011800120911          SELECT   * from tijdc00f
011900120911          WHERE    LACela = '94'
012000120911          ORDER BY LACaas, LAClnp, LACnrs, LACnsp;
012100120911
012200120911          exec sql
012300120911           OPEN JDC94;
012400120911
012500120911          DOW not wEoF;
012600120911            exec sql
012700120911             FETCH next from JDC94 into: TIJDCds;
012800120911             IF sqlcod = 100 or sqlcod < 0;
012900120911               wEoF = *on;
013000120911               leave;
013100120911             ENDIF;
013200120412
013300120911         //?se scartato --> LACela = 94
013400120911           chain (JDCaas:JDClnp:JDCnrs:JDCnsp:JDCcop) TIJDC01L;
013500120911           IF  %found(TIJDC01L);
013600120911           //?devo passarlo a 00
013700120911             LACela = '00';
013800120911           //?mette CLI al posto di CL1
013900120911             LACcop = 'CLI';
014000120911           //?mettere directory ERR/ davanti alla DIR di TIJDC
014100120911             LACdir = 'ERR/'+ JDCdir;
014200120912           //?metto la LNA nel campo LACutecd (per ora non usato)
014300120912             TASaas = 2000 + %dec(LACaas:2:0);
014400120912             TASlnp = %dec(LAClnp:3:0);
014500120912             TASnrs = %dec(LACnrs:2:0);
014600120912             TASnsp = %dec(LACnsp:7:0);
014700120912             chain (TASaas:TASlnp:TASnrs:TASnsp) TITAS30C;
014800120912             IF  %found(TITAS30C);
014900120912               LACutecd = %editc(TASlna:'X');
015000120912             ENDIF;
015100120911           //?togliere la presenza dell'immagini su FIAR5 rcd DOC
015200120911             AR5aas = 2000 + %dec(LACaas:2:0);
015300120911             AR5lnp = %dec(LAClnp:3:0);
015400120911             AR5nrs = %dec(LACnrs:2:0);
015500120911             AR5nsp = %dec(LACnsp:7:0);
015600120911             chain (AR5aas:AR5lnp:AR5nrs:AR5nsp:kAR5trd) FIAR531C;
015700120911             //?Se esiste FIAR5 aggiorno
015800120911             IF  %found(FIAR531C);
015900120911               dAR5doc = AR5uni;
016000120911               clear �AR5jfid;
016100120911               clear �AR5jdrid;
016200120911               clear �AR5jdiid;
016300120911               AR5uni = dAR5doc;
016400120911           //?Aggiorno FIAR5
016500120911               IF  *in41;
016600120911                 update FIAR5000;
016700120911               ENDIF;
016800120911               IF  *in43;
016900120911                 update FIAR5P00;
017000120911               ENDIF;
017100120911             ENDIF;
017200120911           //?Aggiorno TIJDC
017300120911             UPDATE TIJDC000;
017400120911           ENDIF;
017500120412
017600120911          ENDDO;
017700120911
017800120911        exec sql
017900120911         CLOSE JDC94;
018000120411
018100120411       ENDSR;
018200120411
018300120411
018400120411       //--------------------------------------------------------------
018500120411       //?Operazioni finali.
018600120411       //--------------------------------------------------------------
018700120411       BEGSR RoutEnd;
018800120911
018900120911       //?Testo la chiusura del sottosistema
019000120911         IF  %shtdn;
019100120911           chiudi = 'S';
019200120911         ENDIF;
019300120411
019400120411         *inLR = *on;
019500120411         return;
019600120411
019700120411       ENDSR;
019800120411
019900120411      /end-free
