000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     C*------------------------------------------------------------------------*
000300000000     C*?MAIN LINE
000400000000     C*------------------------------------------------------------------------*
000500020925     D TIBS34DS      E DS                                                       *Profili utente
000600020925     D DDATIUTE      E DS                                                       *Dati utente
000700020925     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
000800020925     D TRUL26DS      E DS
000900020925     D  skpo                  11    760  0 dim(250)
001000020925     D  skpot                761   1510  0 dim(250)
001100020925     D tibs56ds      E DS
001200020925     D  ski                    7    756  0 dim(250)
001300020925     C*---
001400020925     C* TIBS34R
001500020925     C*---
001600020925     C                   CLEAR                   TIBS34DS
001700020925     C                   CLEAR                   AZUTEDS
001800020925     C                   CLEAR                   DDATIUTE
001900020925     C     *DTAARA       DEFINE    §azute        azuteds
002000020925     C     *DTAARA       DEFINE    §datiute      ddatiute
002100020925     C                   IN(E)     *DTAARA
002200020925if  1C                   IF        %Error
002300020925     c                   EVAL      I34Tla = 'L'
002400020925     C                   CALL      'TIBS34R'
002500020925     C                   PARM                    Tibs34Ds
002600020925     C                   IN        *DTAARA
002700020925e   1C                   ENDIF
002800020925     C*---
002900020925     C* TIBS56R
003000020925     C*---
003100020925     C                   CLEAR                   tibs56ds
003200020925     C                   EVAL      I56PPO = 001
003300020925     C                   EVAL      I56MOD = 'POA'
003400020925     C                   CALL      'TIBS56R'
003500020925     C                   PARM                    tibs56ds
003600020925     C*---
003700020925     C* TRUL26R
003800020925     C*---
003900020925     C                   clear                   trul26ds
004000020925     C                   eval      i26tla = 'L'
004100020925     C                   eval      i26tpt = 'P'
004200020925     C                   call      'TRUL26R'
004300020925     C                   parm                    trul26ds
004400020925     C*
004500020925     c                   seton                                        lr
