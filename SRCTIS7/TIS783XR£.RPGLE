000100170411      * .......
000200081003     C*
000300081003     C****** OPZ MOVE ******
000400080910     C                   if        ifsOPZ = 'MOVE'
000500080910     C*
000600081003     C* Verifica parametri necessari rispetto all'opzione richiesta
000700081003     C                   if        ifsPTHS <> *blanks AND
000800081003     C                             ifsFILS <> *blanks AND
000900090311     C                             ifsPTHD <> *blanks
001000081003     C*
001100080910     C                   call(e)   'TIS783C1'
001200090311     C                   parm      ifsOPZ        par_OPZ
001300090312     C                   parm                    par_SPTH
001400090311     C                   parm                    par_DPTH
001500090312     C                   parm                    par_SFIL
001600090312     C                   parm                    par_DFIL
001700090311     C                   parm                    par_esito
001800090311     C                   parm                    par_msg
001900081003     C*
002000081003     C                   else
002100090302     C                   eval      par_esito = '2'
002200081003     C                   endif
002300080910     C*
002400081003     C                   endif
002500090302     C************
002600171011     C*
002700171016     C****** OPZ MVNO ****** (MOVE + NO email di errore)
002800171016     C                   if        ifsOPZ = 'MVNO'
002900171011     C*
003000171011     C* Verifica parametri necessari rispetto all'opzione richiesta
003100171011     C                   if        ifsPTHS <> *blanks AND
003200171011     C                             ifsFILS <> *blanks AND
003300171011     C                             ifsPTHD <> *blanks
003400171011     C*
003500171011     C                   call(e)   'TIS783C1'
003600171011     C                   parm      ifsOPZ        par_OPZ
003700171011     C                   parm                    par_SPTH
003800171011     C                   parm                    par_DPTH
003900171011     C                   parm                    par_SFIL
004000171011     C                   parm                    par_DFIL
004100171011     C                   parm                    par_esito
004200171011     C                   parm                    par_msg
004300171011     C*
004400171011     C                   else
004500171011     C                   eval      par_esito = '2'
004600171011     C                   endif
004700171011     C*
004800171011     C                   endif
004900171011     C************
005000090302     C*
005100090302     C****** OPZ MORE (MOVE + RENAME) ******
005200090302     C                   if        ifsOPZ = 'MORE'
005300090302     C*
005400090302     C* Verifica parametri necessari rispetto all'opzione richiesta
005500090302     C                   if        ifsPTHS <> *blanks AND
005600090302     C                             ifsFILS <> *blanks AND
005700090302     C                             ifsPTHD <> *blanks AND
005800090302     C                             ifsFILD <> *blanks
005900090302     C*
006000090302     C                   call(e)   'TIS783C1'
006100090311     C                   parm      ifsOPZ        par_OPZ
006200090312     C                   parm                    par_SPTH
006300090312     C                   parm                    par_DPTH
006400090312     C                   parm                    par_SFIL
006500090312     C                   parm                    par_DFIL
006600090311     C                   parm                    par_esito
006700090311     C                   parm                    par_msg
006800090302     C*
006900090302     C                   else
007000090302     C                   eval      par_esito = '2'
007100090302     C                   endif
007200090302     C*
007300090302     C                   endif
007400090311     C************
007500090311     C*
007600090311     C****** OPZ REMO (RENAME + MOVE) ******
007700090311     C                   if        ifsOPZ = 'REMO'
007800090311     C*
007900090311     C* Verifica parametri necessari rispetto all'opzione richiesta
008000090311     C                   if        ifsPTHS <> *blanks AND
008100090311     C                             ifsFILS <> *blanks AND
008200090311     C                             ifsPTHD <> *blanks AND
008300090311     C                             ifsFILD <> *blanks
008400090311     C*
008500090311     C                   call(e)   'TIS783C1'
008600090311     C                   parm      ifsOPZ        par_OPZ
008700090312     C                   parm                    par_SPTH
008800090312     C                   parm                    par_DPTH
008900090312     C                   parm                    par_SFIL
009000090312     C                   parm                    par_DFIL
009100090311     C                   parm                    par_esito
009200090311     C                   parm                    par_msg
009300090311     C*
009400090311     C                   else
009500090311     C                   eval      par_esito = '2'
009600090311     C                   endif
009700090311     C*
009800090311     C                   endif
009900081003     C************
010000081003     C*
010100081003     C****** OPZ DELE ******
010200081003     C                   if        ifsOPZ = 'DELE'
010300081003     C*
010400081003     C* Verifica parametri necessari rispetto all'opzione richiesta
010500081003     C                   if        ifsPTHS <> *blanks AND
010600081003     C                             ifsFILS <> *blanks
010700081003     C*
010800081003     C                   call(e)   'TIS783C1'
010900090311     C                   parm      ifsOPZ        par_OPZ
011000090312     C                   parm                    par_SPTH
011100090312     C                   parm                    par_DPTH
011200090312     C                   parm                    par_SFIL
011300090312     C                   parm                    par_DFIL
011400090311     C                   parm                    par_esito
011500090311     C                   parm                    par_msg
011600081003     C*
011700081003     C                   else
011800090302     C                   eval      par_esito = '2'
011900081003     C                   endif
012000081003     C*
012100081003     C                   endif
012200130924     C************
012300130924     C*
012400130924     C****** OPZ DELN ****** (DELE + NO email di errore)
012500130924     C                   if        ifsOPZ = 'DELN'
012600130924     C                   setoff                                       55
012700130924     C*
012800130924     C* Verifica parametri necessari rispetto all'opzione richiesta
012900130924     C                   if        ifsPTHS <> *blanks AND
013000130924     C                             ifsFILS <> *blanks
013100130924     C*
013200130924     C                   call(e)   'TIS783C1'
013300130924     C                   parm      'DELE'        par_OPZ
013400130924     C                   parm                    par_SPTH
013500130924     C                   parm                    par_DPTH
013600130924     C                   parm                    par_SFIL
013700130924     C                   parm                    par_DFIL
013800130924     C                   parm                    par_esito
013900130924     C                   parm                    par_msg
014000130924     C*
014100130924     C                   else
014200130924     C                   eval      par_esito = '2'
014300130924     C                   endif
014400130924     C*
014500130924     C                   endif
014600081003     C************
014700140708     C*
014800140708     C****** OPZ RMDN ****** (REMOVE DIR + NO email di errore)
014900140708     C                   if        ifsOPZ = 'RMDN'
015000140708     C*
015100140708     C                   setoff                                       55
015200140708     C*
015300140709     C* Gestisco solo se cartella di primo livello
015400140708     C                   if        %subst(ifsFL1:1:1) = '1'
015500140708     C*
015600140708     C* Verifica parametri necessari rispetto all'opzione richiesta
015700140708     C                   if        ifsPTHS <> *blanks
015800140708     C*
015900140708     C                   call(e)   'TIS783C1'
016000140708     C                   parm      'RMDN'        par_OPZ
016100140708     C                   parm                    par_SPTH
016200140708     C                   parm                    par_DPTH
016300140708     C                   parm                    par_SFIL
016400140708     C                   parm                    par_DFIL
016500140708     C                   parm                    par_esito
016600140708     C                   parm                    par_msg
016700140708     C*
016800140708     C                   else
016900140708     C                   eval      par_esito = '2'
017000140708     C                   endif
017100140708     C                   endif
017200140708     C*
017300140708     C                   endif
017400140708     C************
017500090302     C*
017600090302     C****** OPZ RENA ******
017700090302     C                   if        ifsOPZ = 'RENA'
017800090302     C*
017900090302     C* Verifica parametri necessari rispetto all'opzione richiesta
018000090302     C                   if        ifsPTHD <> *blanks AND
018100090302     C                             ifsFILS <> *blanks AND
018200090302     C                             ifsFILD <> *blanks
018300090302     C*
018400090311     C                   call(e)   'TIS783C1'
018500090311     C                   parm      ifsOPZ        par_OPZ
018600090312     C                   parm                    par_SPTH
018700090312     C                   parm                    par_DPTH
018800090312     C                   parm                    par_SFIL
018900090312     C                   parm                    par_DFIL
019000090311     C                   parm                    par_esito
019100090311     C                   parm                    par_msg
019200090302     C*
019300090302     C                   else
019400090302     C                   eval      par_esito = '2'
019500090302     C                   endif
019600090302     C*
019700090302     C                   endif
019800160310     C************
019900160310     C*
020000160310     C****** OPZ COPY ******
020100160310     C                   if        ifsOPZ = 'COPY'
020200160310     C*
020300160310     C* Verifica parametri necessari rispetto all'opzione richiesta
020400160310     C                   if        ifsPTHS <> *blanks AND
020500160310     C                             ifsFILS <> *blanks AND
020600160310     C                             ifsPTHD <> *blanks
020700160310     C*
020800160310     C                   call(e)   'TIS783C1'
020900160310     C                   parm      ifsOPZ        par_OPZ
021000160310     C                   parm                    par_SPTH
021100160310     C                   parm                    par_DPTH
021200160310     C                   parm                    par_SFIL
021300160310     C                   parm                    par_DFIL
021400160310     C                   parm                    par_esito
021500160310     C                   parm                    par_msg
021600160310     C*
021700160310     C                   else
021800160310     C                   eval      par_esito = '2'
021900160310     C                   endif
022000160310     C*
022100160310     C                   endif
022200160310     C************
022300170926     C*
022400170926     C****** OPZ CORE ******
022500170926     C                   if        ifsOPZ = 'CORE'
022600170926     C*
022700170926     C* Verifica parametri necessari rispetto all'opzione richiesta
022800170926     C                   if        ifsPTHS <> *blanks AND
022900170926     C                             ifsFILS <> *blanks AND
023000170926     C                             ifsPTHD <> *blanks
023100170926     C*
023200170926     C                   call(e)   'TIS783C1'
023300170926     C                   parm      ifsOPZ        par_OPZ
023400170926     C                   parm                    par_SPTH
023500170926     C                   parm                    par_DPTH
023600170926     C                   parm                    par_SFIL
023700170926     C                   parm                    par_DFIL
023800170926     C                   parm                    par_esito
023900170926     C                   parm                    par_msg
024000170926     C*
024100170926     C                   else
024200170926     C                   eval      par_esito = '2'
024300170926     C                   endif
024400170926     C*
024500170926     C                   endif
024600170926     C************
024700130924     C*
024800170411      * .......
