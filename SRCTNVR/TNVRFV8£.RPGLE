000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300001127     ffnfvv05l  if   e           k disk    rename(fnfvv000:fnfvv5) prefix(x)
000400001127     ffnfvv01l  uf   e           k disk
000500020828     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000600001127     d knpg            s                   like(fvvnpg) inz(4)
000700001127     d knpg8           s                   like(fvvnpg) inz(8)
000800020828     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
000900020828     D CNCR80        E DS
001000000607      *--------------------------------------------------------------------
001100000608      * reperimento dati foglio
001200001127     c     knpg8         setll     fnfvv01l
001300001127     c                   do        *hival
001400001127     c     knpg8         reade     fnfvv01l                               99
001500001127     c   99              leave
001600001127      * lettura foglio 4 se non trovo chiudo l'8
001700001127     c     kfvv          chain     fnfvv05l                           98
001800020828      *
001900020828     C                   IF        *IN98 = *on
002000020828      *
002100020828     c   of              except    testa
002200020828     c   of              eval      *inof = *off
002300020828     c                   add       1             conta             6 0
002400020828     c                   except    detta
002500020828      *
002600020828     C                   IF        CANCELLA <> *blanks
002700020828     c                   move      'S'           fvvfcf
002800020828     c                   update    fnfvv000
002900020828     C                   ENDIF
003000020828      *
003100020828     C                   ENDIF
003200020828      *
003300001127     c                   enddo
003400020828     c                   eval      *inlr = *on
003500000607      *-----------------------------------------------
003600000607     c     *inzsr        begsr
003700000607      *-----------------------------------------------
003800000607     c
003900020828     c     *entry        plist
004000020828     c                   parm                    cancella          1
004100000607
004200000607     c     kfvv          klist
004300000607     c                   kfld                    knpg
004400001127     c                   kfld                    fvvnfv
004500000607
004600020828     C                   Z-ADD     1             CODUT
004700020828     C                   CALL      'X§PARUT'
004800020828     C                   PARM                    UT§DSE
004900020828     C                   MOVEL     REC80         CNCR80
005000020828     C                   MOVEL     RAGUT         RSUT             20
005100020828
005200020828     c                   eval      *inof = *on
005300020828
005400000607     c                   endsr
005500020828
005600020828     OQSYSPRT   E            TESTA            02
005700020828     O                       RSUT                20
005800020828     O                                         + 20 'F.V. tipo 8 senza corrispo+
005900020828     O                                              ndente F.V. tipo 4'
006000020828     O                                         +  7 '*TNVRFV8*'
006100020828     O                       UDATE         Y   +  5
006200020828     O                                          128 'Pag.'
006300020828     O                       PAGE          Z    132
006400020828     O          E            TESTA          1 03
006500020828     O                                           15 'Num. Foglio'
006600020828     O                                           30 'Contatore'
006700020828     O          E            DETTA       1
006800020828     O                       fvvnfv        Z     12
006900020828     O                       CONTA         2     27
