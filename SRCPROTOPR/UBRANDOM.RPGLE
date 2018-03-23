000100090612     P*--------------------------------------------------
000200090612     P* Procedure name: UBRANDOM_Retrieve...
000300090612     P* Purpose:        Genera numeri casuali da <x> a <y>
000400090612     P* Returns:        Integer (Status_Code)
000500090612     P* Parameter:      pInValDa       => Valore minimo "da" (x)
000600090612     P* Parameter:      pInValA        => Valore massimo "a" (y)
000700090612     P* Parameter:      pOutRandom     => Valore casuale reperito
000800090612     P*--------------------------------------------------
000900090612     P*
001000090612     D UBRANDOM_Retrieve...
001100090612     D                 PR            10I 0
001200090612     D   pInValDa                    10I 0 CONST
001300090612     D   pInValA                     10I 0 CONST
001400090612     D   pOutRandom                  10I 0 OPTIONS(*NOPASS)
001500090612     D*
