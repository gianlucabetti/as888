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
001300160418
001400160418      //---------------------------------------------------------------
001500160418       // Parametri
001600160418     d kpjba         e ds
001700160420     d  wdata                247    254  0
001800160420     d  wksc                 255    261  0
001900160418
002000160418       // Campo TASflo
002100160418     d dTASflo       e ds                  inz
002200160418
002300160418       // - Campi di comodo
002400160418     d anno            s              2
002500160418     d cliente         s              7
002600160418     d nrimmagini      s              9  0
002700160418     d savcliente      s              7
002800160418     d tipobolla       s              1
002900160418     d totimmagini     s              9  0
003000160418
003100160418       // - flag booleani
003200160418     d wEoF            s               n
003300160418
003400160418       //--------------------------------------------------------------
003500160418       //?Definizione procedure usate.                                 ?
003600160418       //--------------------------------------------------------------
003700160418
003800160418       //--------------------------------------------------------------
003900160418
004000160418     c     *entry        plist
004100160418     c                   parm                    kpjba
004200160418
004300160418      /free
004400160418
004500160418        wEoF = *off;
004600160418
004700160418       // Leggo tabella LAC solo FTP
004800160418        exec sql
004900160418        DECLARE LAC cursor for
005000160418        SELECT substr(TBEke1, 1, 7), substr(tbeuni, 26, 1)
005100160418               from TNTBE00F
005200160418        WHERE TBEcod = 'LAC' and substr(TBEuni, 66, 1) = 'A'
005300160418        ORDER by substr(TBEke1, 1, 7);
005400160418
005500160418       // Apertura del cursore
005600160418        exec sql
005700160418        OPEN LAC;
005800160418
005900160418        IF  sqlcode < 0;
006000160418          wEoF = *on;
006100160418          exec sql
006200160418          CLOSE LAC;
006300160418        ENDIF;
006400160418
006500160418        DOW  not wEoF;
006600160418          exec sql
006700160418          FETCH NEXT from LAC into :cliente, :tipobolla;
006800160418          IF  sqlcod = 100 or sqlcod < 0;
006900160418            wEoF = *on;
007000160418            leave;
007100160418          ENDIF;
007200160420
007300160420          IF  wksc <> *zeros and
007400160420              %editc(wksc:'X') <> cliente;
007500160420            iter;
007600160420          ENDIF;
007700160418
007800160418       // Leggo le bolle per cliente
007900160418          IF  cliente <> savcliente;
008000160418
008100160418            TASksc = %dec(cliente:7:0);
008200160418            setll TASksc TITAS31C;
008300160418            reade TASksc TITAS31C;
008400160418            DOW  not %eof (TITAS31C);
008500160418              dTASflo = TASflo;
008600160418              IF  (((tipobolla = 'F' or tipobolla = 'E') and
008700160418                    TAStbl = 'F1') or
008800160418                   ((tipobolla = 'A' or tipobolla = 'E') and
008900160418                     TAStbl = 'A2')) and
009000160420                     §FLOiml <> *blanks and TASdcm > wdata and
009100160418                     TAScca = '';
009200160418                LACaas = %subst(%editc(TASaas:'X'):3:2);
009300160418                LAClnp = %editc(TASlnp:'X');
009400160418                LACnrs = %editc(TASnrs:'X');
009500160418                LACnsp = %editc(TASnsp:'X');
009600160418                chain (LACaas:LAClnp:LACnrs:LACnsp) TILAC01L;
009700160418                IF  not %found(TILAC01L);
009800160418                  nrimmagini += 1;
009900160420                  IF  wksc <> *zeros;
010000160420                    exsr Stampabolla;
010100160420                  ENDIF;
010200160418                ENDIF;
010300160418              ENDIF;
010400160418              reade TASksc TITAS31C;
010500160418            ENDDO;
010600160418
010700160418            IF  nrimmagini > 0;
010800160420              exsr Stampa;
010900160418            ENDIF;
011000160418
011100160418            totimmagini += nrimmagini;
011200160418            savcliente = cliente;
011300160418            clear nrimmagini;
011400160418          ENDIF;
011500160418
011600160418        ENDDO;
011700160418
011800160420        exsr Stampa1;
011900160418
012000160418       // Chiusura del cursore
012100160418        exec sql
012200160418        CLOSE LAC;
012300121218
012400121218        *inLR = *ON;
012500160420
012600160420       //-------------------------------------------------------------**
012700160420       // Stampa dettaglio bolla.
012800160420       //-------------------------------------------------------------**
012900160420        BEGSR StampaBolla;
013000160420
013100160420          IF  not *in99 or *inof;
013200160420            except testa;
013300160420            *in99 = *on;
013400160420          ENDIF;
013500160420
013600160420          except rigaldv;
013700160420
013800160420        ENDSR;
013900140331
014000140331       //-------------------------------------------------------------**
014100160418       // Stampa.
014200140331       //-------------------------------------------------------------**
014300160418        BEGSR Stampa;
014400140331
014500140331          IF  not *in99 or *inof;
014600140331            except testa;
014700140331            *in99 = *on;
014800140331          ENDIF;
014900140331
015000140331          except riga;
015100140331
015200140331        ENDSR;
015300160418
015400160418       //-------------------------------------------------------------**
015500160418       // Stampa totale
015600160418       //-------------------------------------------------------------**
015700160418        BEGSR Stampa1;
015800160418
015900160418          IF  not *in99 or *inof;
016000160418            except testa;
016100160418            *in99 = *on;
016200160418          ENDIF;
016300160418
016400160418          except totale;
016500160418
016600160418        ENDSR;
016700160418
016800121218      /end-free
016900140331
017000140331     oPRTF198   e            testa          2 02
017100141216     o                                           20 'BRT S.p.A.'
017200160418     o                                           60 '**  Immagini non scritte *'
017300160420     o                       udate              100 '  /  /  '
017400160420     o                                          120 'TNVRMB47R1'
017500140331     o                                          128 'Pag.'
017600140331     o                       page          Z    132
017700140331     o          e            testa          2
017800140331     o                                            7 'Bolla'
017900160420     o          e            rigaldv     1
018000160420     o                       LACaas               2
018100160420     o                       LAClnp            +  1
018200160420     o                       LACnrs            +  1
018300160420     o                       LACnsp            +  1
018400140331     o          e            riga        1
018500160418     o                       cliente              7
018600160418     o                       nrimmagini        +  1
018700160418     o          e            totale      1
018800160418     o                                            7 'totale'
018900160418     o                       totimmagini       +  1
