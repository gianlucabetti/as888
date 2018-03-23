000100151020     D*------------
000200151020     D KPJBA         E DS
000300141028     D*-------------------
000400151020     D UBTTCTLDS       DS
000500151020       //elaboro per oggi: TTDay='0'  oppure ieri: TTDay='-1D'
000600151020     D  TTDay                         3a
000700151021     D  TTRichOra                     5s 0
000800151020     D  IndEMail                    126a
000900120928     D*-------------------
001000120928
001100970526     C*------------------------------------------------------------------------*
001200000303     C* MAIN LINES
001300970526     C*------------------------------------------------------------------------*
001400141027     C*
001500141216     C* da questo pgm si passa un indirizzo email perché il risultato arriva via email
001600151020     C                   RESET                   UBTTCTLDS
001700151020     C                   EVAL      IndEMail = 'luciano.carini@brt.it'
001800151021     C                   EVAL      TTRichOra = 5000
001900151021     C                   EVAL      TTDay = '0'
002000151020     C                   EVAL      KPJBU = UBTTCTLDS
002100151020     C* chiamo interattivo
002200151021     C***                CALL      'UBTTCTLR'
002300151021     c***                parm                    Kpjba
002400140120     C*
002500151020     C* rilancio per ieri
002600151020     C                   RESET                   UBTTCTLDS
002700151020     C                   EVAL      IndEMail = 'luciano.carini@brt.it'
002800151021     C                   EVAL      TTRichOra = 5000
002900151020     C                   EVAL      TTDay = '-1D'
003000151020     C                   EVAL      KPJBU = UBTTCTLDS
003100151020     C* chiamo interattivo
003200170720     C                   CALL      'UBTTCTLR'
003300151020     c                   parm                    Kpjba
003400151020     C*
003500151020     C                   seton                                        lr
