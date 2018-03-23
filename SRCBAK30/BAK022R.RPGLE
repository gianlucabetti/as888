000100030710     H*PARMS OPTION(*NOXREF) DFTACTGRP(*YES)
000200030710     H*PARMS CVTOPT(*NONE)
000300030710     H DECEDIT('0,') DATEDIT(*DMY.)
000400930713     H*---------------------------------------------------------------*
000500930713     H* BAK022R                                                       *
000600930713     H*                                                               *
000700060912     H* INCREMENTA DI 1 IL NR. INCREMENTALE AZIONE SU KPDT1           *
000800060912X0264H*                                                               *
000900060912  "  H* Se ricevuto in input il flag di errore (param. facoltativo)   *
001000060912  "  H*  vengono gestiti max 5 tentativi di allocazione della data    *
001100060912  "  H*  area KPDT1 dopodichè viene invioato un messagio di errore    *
001200060912  "  H*  alla coda dell'utente e dell'operatore                       *
001300060912  "  H* Se non ricevuto in input il flag di errore i tentativi di     *
001400060912  "  H*  allocazione della data area KPDT1 vengono ripetuti           *
001500060912X0264H*  teoricamente all'infinito                                    *
001600930713     H*                                                               *
001700930713     H*---------------------------------------------------------------*
001800930715     D KPDT1         E DS           512
001900060912X0264D*-------------
002000060912  "  D                SDS
002100060912  "  D  USER                 254    263
002200060912  "  D  JOBNUMA              264    269
002300060912  "  D  JOBNAM               244    253
002400060912  "  D  PARMS            *PARMS
002500060912  "  D*-------------
002600060912  "  D Errore022       S              1A
002700060912X0264D $Tentativi      S              2  0
002800930713     C*---------------------------------------------------------------*
002900930714     C     *ENTRY        PLIST
003000930714     C                   PARM                    WNRAZ             9 0
003100060912X0264C                   PARM                    Errore022
003200930713     C*
003300930713     C     *DTAARA       DEFINE                  KPDT1
003400060912X0264C*
003500060912  "  C                   If        PARMS > 1
003600060912  "  C                   Eval      Errore022 = *Off
003700060912X0264C                   EndIf                                                  nr. azione
003800930714     C*
003900061110X0264C*                  Clear                   $Tentativi
004000061110     C*    *IN99         DOUEQ     *OFF
004100061110X0264C*    $Tentativi    OrEq      5
004200061110X0264C*    PARMS         AndGT     1
004300930713     C     *LOCK         IN        KPDT1                                99
004400061110X0264C*                  Eval      $Tentativi = $Tentativi + 1
004500061110     C*                  ENDDO
004600060912X0264C* allocazione non riuscita
004700060912  "  C                   If        *In99 = *On
004800061110  "  C*                  Eval      Errore022 = *On
004900061110  "  C*                  Clear                   WNRAZ
005000060912  "  C* invio msg a utente e operatore
005100061110  "  C*                  Eval      MsgDta024 = JOBNUMA + USER + JOBNAM
005200061110  "  C*                  Call      'BAK024C'
005300061110  "  C*                  Parm      '11'          Ope024            2
005400061110  "  C*                  Parm      USER          User024          10
005500061110  "  C*                  Parm      'PRO0045'     MsgId24           7
005600061110  "  C*                  Parm      'PROBAS'      MsgF024          10
005700061110  "  C*                  Parm                    MsgDta024       100
005800061110     C                   CALL      'TRULNRAZ0'                                  N. azione di riserva
005900061110     C                   PARM                    wNrAz
006000061110X0264C* allocazione riuscita
006100060912X0264C                   Else
006200930714     C*
006300930713     C                   ADD       1             KP1NRA
006400930713     C                   OUT       KPDT1
006500930714     C*
006600930714     C                   Z-ADD     KP1NRA        WNRAZ
006700060912X0264C                   EndIf
006800930713     C*
006900930713     C                   SETON                                        LR
