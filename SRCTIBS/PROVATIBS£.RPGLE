000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     C*------------------------------------------------------------------------*
000300000000     C*?MAIN LINE
000400000000     C*------------------------------------------------------------------------*
000500020925     D TIBS34DS      E DS                                                       *Profili utente
000600020925     D DDATIUTE      E DS                                                       *Dati utente
000700020925     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
000800020925     D tibs56ds      E DS
000900020925     D  ski                    7    756  0 dim(250)
001000000000     C*
001100020925     C* INIZIALIZZA VARIABILI DI WRK
001200020925     C                   CLEAR                   TIBS34DS
001300020925     C                   CLEAR                   AZUTEDS
001400020925     C                   CLEAR                   DDATIUTE
001500020925     C*
001600020925     C     *DTAARA       DEFINE    §azute        azuteds
001700020925     C     *DTAARA       DEFINE    §datiute      ddatiute
001800020925     C                   IN(E)     *DTAARA
001900020925if  1C                   IF        %Error
002000020925     c                   EVAL      I34Tla = 'L'
002100020925     C                   CALL      'TIBS34R'
002200020925     C                   PARM                    Tibs34Ds
002300020925     C                   IN        *DTAARA
002400020925e   1C                   ENDIF
002500020925     C*
002600020925     C* REPERIMENTO TUTTI P.O.
002700020925     C                   CLEAR                   tibs56ds
002800020925     C                   EVAL      I56PPO = 946
002900020925     C                   EVAL      I56MOD = 'POA'
003000020925     C                   CALL      'TIBS56R'
003100020925     C                   PARM                    tibs56ds
003200020925     C*
003300020925     c                   seton                                        lr
