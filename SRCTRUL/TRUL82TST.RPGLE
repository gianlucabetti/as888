000100031002      * DS Esterna x gestione allocazione ed invio messaggi
000200031013     D TRUL82DS      E DS
000300030919
000400030918     C*
000500031002     C* Valorizzo la DS di procedura
000600031013     C                   CLEAR                   TRUL82DS
000700060303     C                   EVAL      UL82§FIL = 'TABEL00F'
000800060303     C                   EVAL      UL82§RRN = 2515
000900060303     C                   EVAL      UL82§WIN = 'S'
001000060303     C***                EVAL      UL82§WIN = 'N'
001100031013     C***                EVAL      UL82§MSS = 'Babbeo muoviti ad uscire da '+
001200031013     C***                                     'questa videata!!!'
001300060303     C***                EVAL      UL82§F5  = 'S'
001400031013     C***                EVAL      UL82§F7  = 'S'
001500031010     C*
001600031013     C***                EVAL      UL82§NUM = 0
001700031013     C***                EVAL      UL82§ATT = 0
001800031002     C*
001900031002     C* Effettuo la chiamata al *PGM d utilità
002000031013     C                   CALL      'TRUL82R'
002100031013     C                   PARM                    TRUL82DS
002200031002     C*
002300031002     C                   SETON                                        LR
