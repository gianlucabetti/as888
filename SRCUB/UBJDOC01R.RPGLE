000100040402      /TITLE Test di TIS183R.
000200040402
000300161128     D iSPED           S             14A
000400161128     D oStato          S              5I 0
000500161128     D oURI_POS        S            128A   INZ
000600161128     D oEsito          S              1A   INZ
000700161128
000800040402     C     *ENTRY        PLIST
000900161128     C                   PARM                    iSPED
001000161128     C                   PARM                    oStato
001100161128     C*
001200161130     C                   CALL      'UBJDOC02R'
001300161128     C                   PARM                    iSPED
001400161128     C                   PARM                    oURI_POS
001500161128     C                   PARM      *zeros        oStato
001600161128     C*
001700161128     C                   IF        oStato = *zeros
001800161130     C                   CALL      'UBJDOC02C'
001900161128     C                   PARM                    oURI_POS
002000161128     C                   PARM                    oEsito
002100161128     C*
002200161128     C                   IF        oEsito = *off
002300161128     C                   ELSE
002400161128     C                   EVAL      oStato = -5                                  * Err visualiz. PC
002500161128     C                   ENDIF
002600161128     C*
002700161125     C                   ENDIF
002800161128     C*
002900040402     C                   EVAL      *INLR = *ON
