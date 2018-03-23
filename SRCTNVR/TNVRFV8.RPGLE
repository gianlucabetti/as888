000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300141127     ffnfvv09l  if   e           k disk    rename(fnfvv000:fnfvv9) prefix(x)
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
001600141127      * se foglio chiuso non considera
001700141127     c                   if        fvvfcf = 'S'
001800141127     c                   iter
001900141127     c                   endif
002000141127      * lettura foglio 4
002100141127     c     kfvv          chain     fnfvv09l                           98
002200141127      * se non lo trova oppure se lo trova e il 4 è chiuso
002300141127     C                   IF        *IN98 = *on   or
002400141127     c                             not *in98 and xfvvfcf = 'S'
002500020828      *
002600020828     c   of              except    testa
002700020828     c   of              eval      *inof = *off
002800020828     c                   add       1             conta             6 0
002900020828     c                   except    detta
003000020828      *
003100020828     C                   IF        CANCELLA <> *blanks
003200020828     c                   move      'S'           fvvfcf
003300020828     c                   update    fnfvv000
003400020828     C                   ENDIF
003500020828      *
003600020828     C                   ENDIF
003700020828      *
003800001127     c                   enddo
003900020828     c                   eval      *inlr = *on
004000000607      *-----------------------------------------------
004100000607     c     *inzsr        begsr
004200000607      *-----------------------------------------------
004300000607     c
004400020828     c     *entry        plist
004500020828     c                   parm                    cancella          1
004600000607
004700000607     c     kfvv          klist
004800141127     c                   kfld                    fvvfgs
004900000607     c                   kfld                    knpg
005000001127     c                   kfld                    fvvnfv
005100000607
005200020828     C                   Z-ADD     1             CODUT
005300020828     C                   CALL      'X§PARUT'
005400020828     C                   PARM                    UT§DSE
005500020828     C                   MOVEL     REC80         CNCR80
005600020828     C                   MOVEL     RAGUT         RSUT             20
005700020828
005800020828     c                   eval      *inof = *on
005900020828
006000000607     c                   endsr
006100020828
006200020828     OQSYSPRT   E            TESTA            02
006300020828     O                       RSUT                20
006400020828     O                                         + 20 'F.V. tipo 8 senza corrispo+
006500020828     O                                              ndente F.V. tipo 4'
006600020828     O                                         +  7 '*TNVRFV8*'
006700020828     O                       UDATE         Y   +  5
006800020828     O                                          128 'Pag.'
006900020828     O                       PAGE          Z    132
007000020828     O          E            TESTA          1 03
007100020828     O                                           15 'Num. Foglio'
007200020828     O                                           30 'Contatore'
007300020828     O          E            DETTA       1
007400141127     O                       fvvnpg               2
007500141127     O                       fvvfgs               6
007600141127     O                       fvvnfv        Z     14
007700020828     O                       CONTA         2     27
007800141127     O               98      xfvvnpg             32
007900141127     O               98      xfvvfgs             36
008000141127     O               98      xfvvnfv       Z     44
