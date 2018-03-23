000100030407     H DECEDIT('0,') DATEDIT(*DMY.)
000200030407
000300030407      ****************************************************************
000400030407      ** TRUL28R ** Calcolo del CHECK DIGIT :                       **
000500030407      **            M???? - Segnacollo FEDEX MOD 11 & MOD 7         **
000600030407      ****************************************************************
000700030407
000800030407      *
000900030407      * SCHIERE
001000030407      *
001100030407
001200030407      *
001300030407      * DS definite ESTERNAMENTE
001400030407      *
001500030407     D TRUL28DS      e ds
001600030407
001700030407      *
001800030407      * DS definite INTERNAMENTE
001900030407      *
002000030407
002100030407      *
002200030407      * VARIABILI
002300030407      *
002400030407
002500030407      *
002600030407      * COSTANTI
002700030407      *
002800030407
002900030407      ****************************************************************
003000030407
003100030407     C     *ENTRY        plist
003200030407     C                   parm                    TRUL28DS
003300030407      *
003400030407      * Verifica preliminare: se il 1° byte del tracking number è 0
003500030407      * - se il 1° byte del tracking number è <> 0: devo usare
003600030407      *   l'algoritmo di controllo MOD 11;
003700030407      * - se il 2° byte del tracking number è <> 0: devo usare
003800030407      *   l'algoritmo di controllo MOD 7 per tracking number lungo 11;
003900030407      * - se il 2° byte del tracking number è =  0: devo usare
004000030407      *   l'algoritmo di controllo MOD 7 per tracking number lungo 10;
004100030407if  1C                   if        I28mod = 'FED'          and
004200030407     C                             %subst(I28cod:1:1) =  *zero
004300030407     C                   movel     'FE7'         I28mod
004400030407e   1C                   endif
004500030407if  1C                   if        I28mod = 'FE7'          and
004600030407     C                             %subst(I28cod:1:1) <> *zero
004700030407     C                   movel     'FED'         I28mod
004800030407e   1C                   endif
004900030407      *
005000030407      * Richiamo il pgm "specifico"
005100030407sel 1C                   select
005200030407w   1C                   when      I28mod = 'FED'
005300030407     C                   call      'TRUL28R3A'
005400030407     C                   parm                    TRUL28DS
005500030407w   1C                   when      I28mod = 'FE7'
005600030407     C                   call      'TRUL28R3B'
005700030407     C                   parm                    TRUL28DS
005800030407e   1C                   endsl
005900030407      *
006000030407      * Fine
006100030407if  1C                   if        I28tla = *blank
006200030407     C                   movel     *on           *inrt
006300030407x   1C                   else
006400030407     C                   movel     *on           *inlr
006500030407e   1C                   endif
