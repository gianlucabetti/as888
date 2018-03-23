000100020715     H DATEDIT(*YMD.)
000200000921
000300000921      *------------------------------------------------------------------------*
000400000921      *                                                                        *
000500020715      *  TRASCODIFICA DELL'ULTIMO NUMERO DELLA 1S IN AZNUM                     *
000600000921      *                                                                        *
000700000921      *------------------------------------------------------------------------*
000800000921
000900020722     ftabel00f  if   e           k disk
001000020403
001100020712     d ds1S          e ds
001200020715     d Kpjba         e ds
001300020715     D* DS PER TRUL33R - REPERIMENTO AGGIORNAMENTO NUMERATORI in GRU
001400020715     D TRUL33DS      E DS
001500001009
001600020403     c     ktabel        klist
001700020403     c                   kfld                    codut             1 0
001800020403     c                   kfld                    kcod              2
001900020403
002000020712     c                   eval      kcod = '1S'
002100020403     c                   eval      codut = 1
002200010709
002300020715     c     ktabel        setll     tabel00f
002400020715     c     ktabel        reade     tabel00f
002500020715     C                   dow       NOT  %EOF(TABEL00F)
002600010709
002700020712     c                   movel     tbluni        ds1s
002800010709
002900020715      * Richiamo il trull33 forzando come ultimo numeratore
003000020715      * quello di tabella 1S
003100020715     C                   clear                   Trul33ds
003200020722     C                   movel     tblkey        I33PO1
003300020722     C                   Z-ADD     3             I33OPE
003400020715     C                   Z-ADD     23            I33CNU
003500020715     C                   Z-ADD     §1SCpo        I33UNP
003600020715     C                   movel(p)  trul33ds      kpjbu
003700020715     C                   CALL      'TRUL33R'
003800020715     C                   PARM                    KPJBA
003900020715     C                   eval      trul33ds = kpjbu
004000020403
004100020715     c     ktabel        reade     tabel00f
004200020715     c                   enddo
004300020715
004400020403     c                   eval      *inlr = *on
