000100160418      //---------------------------------------------------------------
000200160418      //
000300160418      //?  Controllo clienti con FTP e flag in TITAS ma no TILAC
000400160418      //
000500160418      //---------------------------------------------------------------
000600160418
000700160418     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000800160418
000900160418      //---------------------------------------------------------------
001000160418     fPRTF198   o    f  198        printer oflind(*inof)
001100160418     fTILAC01L  if   e           k disk
001200160418     fTITAS31C  if   e           k disk
001300160418     fTILAG00R  o    e             disk
001400160418
001500160418      //---------------------------------------------------------------
001600160418       // Parametri
001700160418     d kpjba         e ds
001800160418
001900160418       // Campo TASflo
002000160418     d dTASflo       e ds                  inz
002100160418
002200160418       // - Campi di comodo
002300160418     d anno            s              2
002400160418     d cliente         s              7
002500160418     d nrimmagini      s              9  0
002600160418     d savcliente      s              7
002700160418     d tipobolla       s              1
002800160418     d totimmagini     s              9  0
002900160418
003000160418       // - flag booleani
003100160418     d wEoF            s               n
003200160418
003300160418       //--------------------------------------------------------------
003400160418       //?Definizione procedure usate.                                 ?
003500160418       //--------------------------------------------------------------
003600160418
003700160418       //--------------------------------------------------------------
003800160418
003900160418     c     *entry        plist
004000160418     c                   parm                    kpjba
004100160418
004200160418      /free
004300160418
004400160418        wEoF = *off;
004500160418
004600160418       // Leggo tabella LAC solo FTP
004700160418        exec sql
004800160418        DECLARE LAC cursor for
004900160418        SELECT substr(TBEke1, 1, 7), substr(tbeuni, 26, 1)
005000160418               from TNTBE00F
005100160418        WHERE TBEcod = 'LAC' and substr(TBEuni, 66, 1) = 'A'
005200160418        ORDER by substr(TBEke1, 1, 7);
005300160418
005400160418       // Apertura del cursore
005500160418        exec sql
005600160418        OPEN LAC;
005700160418
005800160418        IF  sqlcode < 0;
005900160418          wEoF = *on;
006000160418          exec sql
006100160418          CLOSE LAC;
006200160418        ENDIF;
006300160418
006400160418        DOW  not wEoF;
006500160418          exec sql
006600160418          FETCH NEXT from LAC into :cliente, :tipobolla;
006700160418          IF  sqlcod = 100 or sqlcod < 0;
006800160418            wEoF = *on;
006900160418            leave;
007000160418          ENDIF;
007100160418
007200160418       // Leggo le bolle per cliente
007300160418          IF  cliente <> savcliente;
007400160418
007500160418            TASksc = %dec(cliente:7:0);
007600160418            setll TASksc TITAS31C;
007700160418            reade TASksc TITAS31C;
007800160418            DOW  not %eof (TITAS31C);
007900160418              dTASflo = TASflo;
008000160418              IF  (((tipobolla = 'F' or tipobolla = 'E') and
008100160418                    TAStbl = 'F1') or
008200160418                   ((tipobolla = 'A' or tipobolla = 'E') and
008300160418                     TAStbl = 'A2')) and
008400160418                     §FLOiml <> *blanks and TASdcm > 20160320 and
008500160418                     TAScca = '';
008600160418                LACaas = %subst(%editc(TASaas:'X'):3:2);
008700160418                LAClnp = %editc(TASlnp:'X');
008800160418                LACnrs = %editc(TASnrs:'X');
008900160418                LACnsp = %editc(TASnsp:'X');
009000160418                chain (LACaas:LAClnp:LACnrs:LACnsp) TILAC01L;
009100160418                IF  not %found(TILAC01L);
009200160418                  nrimmagini += 1;
009300160418                  exsr Scrivi;
009400160418                ENDIF;
009500160418              ENDIF;
009600160418              reade TASksc TITAS31C;
009700160418            ENDDO;
009800160418
009900160418            IF  nrimmagini > 0;
010000160418              //exsr Stampa;
010100160418            ENDIF;
010200160418
010300160418            totimmagini += nrimmagini;
010400160418            savcliente = cliente;
010500160418            clear nrimmagini;
010600160418          ENDIF;
010700160418
010800160418        ENDDO;
010900160418
011000160418        //exsr Stampa1;
011100160418
011200160418       // Chiusura del cursore
011300160418        exec sql
011400160418        CLOSE LAC;
011500121218
011600121218        *inLR = *ON;
011700160418
011800160418       //-------------------------------------------------------------**
011900160418       // Scrivo TILAG.
012000160418       //-------------------------------------------------------------**
012100160418        BEGSR Scrivi;
012200160418
012300160418          clear TILAG000;
012400160418          LAGtim = '20160418131000';
012500160418          LAGaas = LACaas;
012600160418          LAGlnp = LAClnp;
012700160418          LAGnrs = LACnrs;
012800160418          LAGnsp = LACnsp;
012900160418          LAGapp = 'IM';
013000160418          write TILAG000;
013100160418
013200160418        ENDSR;
013300140331
013400140331       //-------------------------------------------------------------**
013500160418       // Stampa.
013600140331       //-------------------------------------------------------------**
013700160418        BEGSR Stampa;
013800140331
013900140331          IF  not *in99 or *inof;
014000140331            except testa;
014100140331            *in99 = *on;
014200140331          ENDIF;
014300140331
014400140331          except riga;
014500140331
014600140331        ENDSR;
014700160418
014800160418       //-------------------------------------------------------------**
014900160418       // Stampa totale
015000160418       //-------------------------------------------------------------**
015100160418        BEGSR Stampa1;
015200160418
015300160418          IF  not *in99 or *inof;
015400160418            except testa;
015500160418            *in99 = *on;
015600160418          ENDIF;
015700160418
015800160418          except totale;
015900160418
016000160418        ENDSR;
016100160418
016200121218      /end-free
016300140331
016400140331     oPRTF198   e            testa          2 02
016500141216     o                                           20 'BRT S.p.A.'
016600160418     o                                           60 '**  Immagini non scritte *'
016700140331     o                       udate              110 '  /  /  '
016800160418     o                                          120 'TNVRMB47R'
016900140331     o                                          128 'Pag.'
017000140331     o                       page          Z    132
017100140331     o          e            testa          2
017200140331     o                                            7 'Bolla'
017300140331     o          e            riga        1
017400160418     o                       cliente              7
017500160418     o                       nrimmagini        +  1
017600160418     o          e            totale      1
017700160418     o                                            7 'totale'
017800160418     o                       totimmagini       +  1
