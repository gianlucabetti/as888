000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200020409     h*****************************************************************
000300020821     h* Il pgm imposta p.o. dell'utente nel campo ARBIPP
000400020409     h*****************************************************************
000500020821     ffnarb00f  uf   e             disk
000600020821     futent02l  if   e           k disk
000800020821     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
000900020821     D CNCR80        E DS
001000020821     D fnarb01         ds
001100020821     d rrn                            8  0
001200020821     d cuc                                 like(arbcuc)
001300020821     d*
001400020821     d cucs            s                   like(arbcuc)
001500020821     d ipp             s                   LIKE(ARBIPP)
001600020821     d COM03           s              3
001700020702     c*
001800020821     c* leggo tutti i record di FNARB e imposta il p.o. utente nel campo
001900020821     c* ARBIPP
002000020702     c*
002100020702     C/EXEC SQL
002200020821     C+ DECLARE A1 CURSOR FOR SELECT rrn(fnarb00f), arbcuc FROM fnarb00f
002300020821     C+ WHERE arbcuc <> 0 ORDER BY arbcuc
002400020702     C/END-EXEC
002500020702
002600020702     C/EXEC SQL
002700020702     C+ OPEN A1
002800020702     C/END-EXEC
002900020702
003000020702     C                   dou       *inlr
003100020702
003200020702     C/EXEC SQL
003300020821     C+ FETCH NEXT FROM A1 INTO :fnarb01
003400020702     C/END-EXEC
003500020702
003600020702     C                   select
003700020821     C                   when      sqlcod <> 0
003800020702     C                   seton                                        lr
003900020821     C                   other
004000020821     C* reperisco il p.o. utente da UTENT00F
004100020821     c* se REMxxx imposto xxx nel p.o. altrimenti imposto SIMfel
004101020930     c                   if        cuc = 999
004102020930     c                   eval      ipp = SIMfel
004103020930     c                   else
004200020821     c                   if         cuc <> cucs
004300020821     c     cuc           chain     utent02l
004400020821     c                   if        %found(utent02l)
004500020821     c                   if        %subst(utnnam: 1: 3) = 'REM'
004600020821     c                   eval      COM03 = %subst(utnnam: 4: 3)
004700020821     C                   MOVE      COM03         IPP
004800020821     c                   else
004900020821     c                   eval      ipp = SIMfel
005000020821     c                   end
005100020821     c                   else
005101020930     c                   eval      ipp = SIMfel
005300020821     c                   end
005400020821     c                   eval      cucs = cuc
005401020930     c                   end
005500020821     c                   end
005600020821     c* aggiorno FNARB00F con numero relativo di record
005700020821     C     rrn           chain     fnarb00f
005800020821     c                   if        %found(fnarb00f)
005900020821     c                   eval      arbipp = ipp
006000020821     c                   update    fnarb000
006100020821     c                   end
006200020702     C                   endsl
006300020702     C*
006400020702     C                   enddo
006500020702
006600020702     C/EXEC SQL
006700020702     C+ CLOSE A1
006800020702     C/END-EXEC
006900000607      *-----------------------------------------------
007000000607     c     *inzsr        begsr
007100000607      *-----------------------------------------------
007500020402     c*
007600020821     C                   Z-ADD     1             CODUT             1 0
007700020821     C                   CALL      'X§PARUT'
007800020821     C                   PARM                    UTEDSE
007900020821     C                   MOVEL     REC80         CNCR80
008000020402     c                   endsr
