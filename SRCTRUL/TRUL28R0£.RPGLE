000100140402       //==============================================================
000200140402       //?Stampa Riferimenti & Contatti per cliente                    ?
000300140402       //==============================================================
000400140402
000500140402       //--------------------------------------------------------------
000600140402       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700140402       //--------------------------------------------------------------
000800140402
000900140402     /*PRM  dbgview(*source)
001000140402     /*END
001100140402
001200140402       //--------------------------------------------------------------
001300140402       //?Specifiche di controllo.                                     ?
001400140402       //--------------------------------------------------------------
001500140402
001600140402     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700140402
001800140402       //--------------------------------------------------------------
001900140402       //?Dichiarazione file.                                          ?
002000140402       //--------------------------------------------------------------
002100140402
002200140402
002300140402       //--------------------------------------------------------------
002400140402       //?Definizione costanti.                                        ?
002500140402       //--------------------------------------------------------------
002600140402
002700140402       // -?Costante per controllo "caratteri solo numerici"?
002800140402     d c_Digits        c                   const('0123456789')
002900140402
003000140402       //--------------------------------------------------------------
003100140402       //?Definizione schiere.                                         ?
003200140402       //--------------------------------------------------------------
003300140402
003400140402       // -?Schiera con Spedizione: LNP+NRS+NSP (12 cifre)?
003500140402     d ds_NrSped       ds            12    inz
003600140402     d   sk_NrSped                    1s 0 inz   dim(12)
003700140402
003800140402       //--------------------------------------------------------------
003900140402       //?Definizione aree dati.                                       ?
004000140402       //--------------------------------------------------------------
004100140402
004200140402
004300140402       //--------------------------------------------------------------
004400140402       //?Definizione strutture dati.                                  ?
004500140402       //--------------------------------------------------------------
004600140402
004700140402       // -?Parametri ricevuti?
004800140402     d TRUL28ds0     e ds
004900140402
005000140402       //--------------------------------------------------------------
005100140402       //?Definizione variabili globali.                               ?
005200140402       //--------------------------------------------------------------
005300140402
005400140402       // -?Indici di schiera?
005500140402     d xx              s              3  0 inz
005600140402
005700140402       // -?Lunghezza del codice ricevuto in Input?
005800140402     d wLen            s              3  0 inz
005900140402
006000140402       // -?Anno della spedizione?
006100140402     d wAAS            s              2  0 inz
006200140402
006300140402       // -?Algoritmo "Bocchi" (completo) su n° spedizione (LNP/NRS/NSP)?
006400150324     d wAlg            s              5  0 inz
006500140402     d wAlg_A          s              3    inz
006600140402
006700140402       // -?Campi di comodo per calcolo del check digit?
006800140402     d wRisultato      s              2  0 inz
006900140402     d wSomma          s             20u 0 inz
007000140402     d wCheckDigit     s              1  0 inz
007100140402
007200140402       //--------------------------------------------------------------
007300140402       //?Definizione prototipi procedure.                             ?
007400140402       //--------------------------------------------------------------
007500140402
007600140402
007700140402       //--------------------------------------------------------------
007800140402       //?Definizione key-list.                                        ?
007900140402       //--------------------------------------------------------------
008000140402
008100140402
008200140402       //--------------------------------------------------------------
008300140402       //?M A I N - L I N E                                            ?
008400140402       //--------------------------------------------------------------
008500140402
008600140402     c     *Entry        plist
008700140402     c                   parm                    TRUL28ds0
008800140402
008900140402      /free
009000140402
009100140402       // -?Operazioni iniziali?
009200140402       exsr  sr_RoutInz;
009300140402
009400140403
009500140403       // -?Calcolo 1° check-digit?
009600140403       exsr  sr_ChkDgt;
009700140403
009800140403       // -?Calcolo Algoritmo "Bocchi"?
009900140403       exsr  sr_ChkAlgor;
010000140403
010100140403       // -?Calcolo 2° check-digit?
010200140403       exsr  sr_ChkDgt;
010300140403
010400140402
010500140402       // -?Operazioni finali?
010600140402       exsr  sr_RoutEnd;
010700140402
010800140402       //--------------------------------------------------------------
010900140402       //?Operazioni iniziali.                                         ?
011000140402       //--------------------------------------------------------------
011100140402       BEGSR  sr_RoutInz;
011200140402
011300140402         *inLR = *on;
011400140402
011500140402         // -?Inizializzazione campi di Output?
011600140402         clear  o280cod;
011700140402         o280err = *off;
011800140402
011900140402         // -?Verifica dei parametri ricevuti:?
012000140402         // ·?Codice in input mancante?
012100140402         if  i280cod = *blank  or  i280cod = *zero;
012200140402           o280err = *on;
012300140402           exsr  sr_RoutEnd;
012400140402         endif;
012500140402
012600140402         // -?Calcolo del numero dei caratteri ricevuti?
012700140402         wLen = %len( %trimr( i280cod ) );
012800140402
012900140402         // -?Verifica dei parametri ricevuti:?
013000140402         // ·?I caratteri devono essere 14 (solo n° spedizione)?
013100140402         //  ?                        o 19 (già completo)?
013200140402         if  wLen <> 14  and  wLen <> 19;
013300140402           o280err = *on;
013400140402           exsr  sr_RoutEnd;
013500140402         endif;
013600140402         // ·?Tutti i caratteri devono essere numerici?
013700140402         if  %check( c_Digits : %subst( i280cod : 1 : wLen ) ) > *zero;
013800140402           o280err = *on;
013900140402           exsr  sr_RoutEnd;
014000140402         endif;
014100140403
014200140403         // -?Impostazione iniziale del codice di Output (da "costruire")?
014300140403         o280cod = %subst( i280cod : 1 : 14 );
014400140402
014500140402       ENDSR;
014600140402
014700140402       //--------------------------------------------------------------
014800140403       //?Calcolo del Check-Digit sul codice finora impostato.         ?
014900140402       //--------------------------------------------------------------
015000140402       // L'algoritmo è il seguente:
015100140402       //
015200140403       // - Somma fra loro del doppio delle cifre dispari con eventuale riduzione
015300140403       //         (elaborandole da sinistra a destra)
015400140403       // - Somma fra loro delle cifre pari (elaborandole da sinistra a destra)
015500140403       // - Somma delle due somme
015600140403       // - Resto della divisione intera per 10 della somma delle somme (modulo)
015700140403       // - La sottrazione da 10 del resto (se > 0) è il check digit;
015800140403       //   Zero altrimenti.
015900140402       //
016000140402       // Per "eventuale riduzione" nella moltiplicazione delle cifre dispari
016100140402       // intendo ad esempio una cifra 9 che raddoppiata dà 18, numero che va
016200140402       // ridotto a una cifra sola sommando tra loro le due cifre che compongono
016300140402       // il numero stesso (1 + 8) e si può ottenere anche semplicemente
016400140402       // sottraendo 9 al numero:
016500140402       //
016600140402       // 9 * 2 = 18 --> 1 + 8 = 9 oppure 18 - 9 = 9
016700140402       // 8 * 2 = 16 --> 1 + 6 = 7 oppure 16 - 9 = 7
016800140402       // ...
016900140402       // 5 * 2 = 10 --> 1 + 0 = 1 oppure 10 - 9 = 1
017000140402       //
017100140402       // Esempio cod. 123456-6:
017200140402       //
017300140402       // 6 * 2 + 4 * 2 + 2 * 2 =>
017400140403       //   12  +   8   +   4   =>
017500140403       //   3   +   8   +   4   =
017600140403       //   15 => 6              (somma "ridotta" del doppio delle cifre dispari)
017700140403       // 5 + 3 + 1 = 9          (somma delle cifre pari)
017800140402       // 6 + 9 = 15             (somma delle somme)
017900140402       // 24 / 10 = 2 e resto 4  (modulo 10)
018000140402       // 10 - 4 = 6             (10 - resto = check digit)
018100140403       //
018200140403       // Esempio sped. 14 001 00 0387233 - 0
018300140403       //
018400140403       // 3*2 +3+ 2*2 +7+ 8*2 +3+ 0*2 +0+ 0*2 +1+ 0*2 +0+ + 4*2 +1
018500140403       //   6 +3+   4 +7+  16 +3+   0 +0+   0 +1+   0 +0+ +   8 +1
018600140403       //   6 +3+   4 +7+   7 +3+   0 +0+   0 +1+   0 +0+ +   8 +1
018700140403       //   40                   (somma delle somme)
018800140403       // 40 / 10 = 4 e resto 0  (modulo 10)
018900140403       // 10 - 0 => *ZERO        (unità di 10 - resto = check digit)
019000140402       //--------------------------------------------------------------
019100140402       BEGSR  sr_ChkDgt;
019200140402
019300140403         // -?Pulizia campi di calcolo?
019400140403         clear  wRisultato;
019500140403         clear  wSomma;
019600140402         clear  wCheckDigit;
019700140402
019800140402         // -?Somma fra loro del doppio delle cifre DISPARI (con?
019900140402         //  ?eventuale riduzione) e delle cifre pari?
020000140403         FOR  xx = %len( %trimr( o280cod ) )  BY 1  DOWNTO 1;
020100140403
020200140403           If  %rem( %len( %trimr( o280cod ) ) - xx + 1 : 2 ) > *zero;
020300140403             wRisultato = %dec( %subst( o280cod : xx : 1 ) : 1 : 0 ) * 2;
020400140403             if  wRisultato > 9;
020500140402               wRisultato -= 9;
020600140403             endif;
020700140402           Else;
020800140403             wRisultato = %dec( %subst( o280cod : xx : 1 ) : 1 : 0 );
020900140402           EndIf;
021000140403
021100140402           wSomma += wRisultato;
021200140403
021300140403         ENDFOR;
021400140402
021500140402         // -?Resto della divisione intera per 10 della somma delle somme (modulo)?
021600140403         // -?La sottrazione da 10 del resto (SE > *zero) è il check digit?
021700140403         //  ?(*zero altrimenti)?
021800140403         xx = %rem( wSomma : 10 );
021900140403         if  xx > *zero;
022000140403           wCheckDigit = 10 - xx;
022100140403         endif;
022200140402
022300140402         // -?Impostazione del Check-Digit?
022400140402         o280cod = %trimr( o280cod ) + %editc( wCheckDigit : 'X' );
022500140402
022600140402       ENDSR;
022700140402
022800140402       //--------------------------------------------------------------
022900140402       //?Calcolo / Controllo dell'Algoritmo "Bocchi" sul n° spediz.   ?
023000140402       //--------------------------------------------------------------
023100140403       // L'algoritmo è il seguente:
023200140403       //
023300140403       // - Somma fra loro delle singole cifre della spedizione (senza l'anno)
023400140403       // - Moltiplicazione della somma per l'anno della spedizione (2 cifre)
023500140403       // - Saranno da considerare le ultime 3 cifre (a dx) di questo risultato
023600140403       //
023700140403       // Esempio sped. (14) 277 88 0159664
023800140403       //
023900140403       // 2 + 7 + 7 + 8 + 8 + 0 + 1 + 5 + 9 + 6 + 6 + 4 = 0063
024000140403       // 63 * 14 = 0882 => 882
024100140403       //
024200140403       // Esempio sped. (14) 499 98 9999999
024300140403       //
024400140403       // 4 + 9 + 9 + 9 + 8 + 9 + 9 + 9 + 9 + 9 + 9 + 9 = 0108
024500140403       // 108 * 14 = 1428 => 428
024600140403       //--------------------------------------------------------------
024700140402       BEGSR  sr_ChkAlgor;
024800140402
024900140402         // -?Pulizia algoritmo?
025000140402         clear  wAlg;
025100140402         clear  wAlg_A;
025200140402
025300140402         // -?Reperimento ANNO della spedizione?
025400140402         wAAS = %int( %subst( o280cod : 1 : 2 ) );
025500140402
025600140402         // -?Reperimento NUMERO della spedizione (in schiera)?
025700140402         ds_NrSped = %subst( o280cod : 3 );
025800140402
025900140402         // -?Calcolo algoritmo (ultime 3 cifre della somma delle?
026000140402         //  ?singole cifre moltiplicate per l'anno)?
026100140402         For  xx = 1  To  %elem(sk_NrSped);
026200140402           wAlg += ( sk_NrSped(xx) );
026300140402         EndFor;
026400140403
026500140402         wAlg *= wAAS;
026600140402
026700140402         wAlg_A = %subst( %editc( wAlg : 'X' ) :
026800140402                          %len( wAlg ) - %len( wAlg_A ) + 1 );
026900140402
027000140402         // -?Impostazione del BRT-code (parziale: con le ultime 3?
027100140402         //  ?cifre dell'algoritmo "Bocchi")?
027200140402         o280cod = %trimr( o280cod ) + wAlg_A;
027300140402
027400140402       ENDSR;
027500140402
027600140402       //--------------------------------------------------------------
027700140402       //?Operazioni finali.                                           ?
027800140402       //--------------------------------------------------------------
027900140402       BEGSR  sr_RoutEnd;
028000140402
028100140402         // -?Eventuale restituzione flag di errore SE richiamato per?
028200140402         //  ?controllo e BRTcode ricevuto <> BRTcode calcolato.?
028300140402         if  o280err = *off  and  %len( %trimr( i280cod ) ) > 14  and
028400140402             i280cod <> o280cod;
028500140402           o280err = *on;
028600140402         endif;
028700140402
028800140402         return;
028900140402
029000140402       ENDSR;
