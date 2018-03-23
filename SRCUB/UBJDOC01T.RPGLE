000100040402      /TITLE Test di TIS183R.
000200040402
000300161128     D iSPED           S             14A
000400161130     D oStato          S              5I 0
000500161130     D wMsg            S             52A
000600161130
000700161128
000800040402     C     *ENTRY        PLIST
000900161128     C                   PARM                    iSPED
001000161128     C*
001100161128     C                   CALL      'UBJDOC01R'
001200161128     C                   PARM                    iSPED
001300161128     C                   PARM      *zeros        oStato
001400161128     C*
001500161130     C                   SELECT
001600161130     C                   WHEN      oStato = *zeros
001700161130     C                   EVAL      wMsg = 'OK'
001800161130     C                   WHEN      oStato = -1
001900161130     C                   EVAL      wMsg = 'Errore generico'
002000161130     C                   WHEN      oStato = -2
002100161130     C                   EVAL      wMsg = 'Errore: Tabelle mancanti'
002200161130     C                   WHEN      oStato = -3
002300161130     C                   EVAL      wMsg = 'Errore in risposta da JDOC'
002400161130     C                   WHEN      oStato = -4
002500161130     C                   EVAL      wMsg = 'Errore: POD non trovata'
002600161130     C                   WHEN      oStato = -5
002700161130     C                   EVAL      wMsg = 'Errore visualizzazione su PC'
002800161130     C                   ENDSL
002900161130     C*
003000161130     C     wMsg          DSPLY
003100161128     C*
003200040402     C                   EVAL      *INLR = *ON
