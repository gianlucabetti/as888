000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200020409     h*****************************************************************
000300020821     h* Il pgm imposta p.o. di gestione foglio nel campo ARBIFP
000400020409     h*****************************************************************
000500020821     ffnarb00f  uf   e             disk
000600020923     ffnfvv01l  if   e           k disk
000700020821     fazcae02l  if   e           k disk
000900020821     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
001000020821     D CNCR80        E DS
001100020821     D fnarb01         ds
001200020821     d rrn                            8  0
001300020821     d ndc                                 like(arbndc)
001400020821     d ddc                                 like(arbddc)
001500020821     d lna                                 like(arblna)
001600020821     d*
001700020821     d ecc             s              3  0 dim(100)
001800020821     d dec             s              8  0 dim(100)
001900020821     d sca             s              8  0 dim(100)
002000020821     d*
002100020821     d ifp             s                   like(arbifp)
002200020821     d lnas            s                   like(arblna)
002300020821     d x               s              3  0
002400020821     d y               s              3  0
002500020702     c*
002600020821     c* leggo tutti i record di FNARB e imposta il p.o. di gestione foglio
002700020821     c* nel campo ARBIFP
002800020702     c*
002900020702     C/EXEC SQL
003000020821     C+ DECLARE A1 CURSOR FOR SELECT rrn(fnarb26l), arbndc, arbddc,
003100020923     C+ arblna FROM fnarb26l WHERE arbndc <> 0 and arbifp = 0 ORDER BY
003101020923     C+ arblna
003200020702     C/END-EXEC
003300020702
003400020702     C/EXEC SQL
003500020702     C+ OPEN A1
003600020702     C/END-EXEC
003700020702
003800020702     C                   dou       *inlr
003900020702
004000020702     C/EXEC SQL
004100020821     C+ FETCH NEXT FROM A1 INTO :fnarb01
004200020702     C/END-EXEC
004300020702
004400020702     C                   select
004500020821     C                   when      sqlcod <> 0
004600020702     C                   seton                                        lr
004700020821     C                   other
004800020821     c* controllo se esiste il foglio
004801020903     c                   clear                   ifp
004802020904     c                   if        ndc <> 9999999 and ndc <> 8888888
004900020821     c                   eval      fvvnfv = ndc
005100020923     c     kfvv          chain     fnfvv01l
005101020903     c                   end
005200020923     c                   if        %found(fnfvv01l) and ndc <> 9999999
005201020904     c                             and ndc <> 8888888 and ddc = fvvdfv
005300020821     c                   eval      IFP = fvvfgs
005400020821     c                   else
005500020821     c* carico la azcae per data
005600020821     c                   if        lna <> lnas
005700020821     c                   exsr      sr£6
005800020821     c                   eval      lnas = lna
005900020821     c                   end
006000020821     c* prendo la filiale in gestione in base alle data di validità
006100020821     c                   do        x             y
006200020821     c                   if        ddc >= dec(y) and ddc <= sca(y)
006300020821     c                   move      ecc(y)        ifp
006400020821     c                   leave
006500020821     c                   end
006600020821     c                   enddo
006700020821     c*
006800020821     c                   if        ifp = 0
006900020821     c                   eval      IFP = lna
007000020821     c                   end
007100020821     c                   end
007200020821     c* aggiorno FNARB00F con numero relativo di record
007300020821     C     rrn           chain     fnarb00f
007400020821     c                   if        %found(fnarb00f)
007500020821     c                   eval      arbifp = ifp
007600020821     c                   update    fnarb000
007700020821     c                   end
007800020702     C                   endsl
007900020702     C*
008000020702     C                   enddo
008100020702
008200020702     C/EXEC SQL
008300020702     C+ CLOSE A1
008400020702     C/END-EXEC
008500020821      *-----------------------------------------------
008600020821     c     sr£6          begsr
008700020821      *-----------------------------------------------
008800020821     C                   CLEAR                   ecc
008900020821     C                   CLEAR                   dec
009000020821     C                   CLEAR                   sca
009100020821     C                   CLEAR                   x
009200020821     c     kcae          setll     azcae02l
009300020821     c                   do        *hival
009400020821     c     kcae          reade     azcae02l
009500020821     c                   if        %eof(azcae02l)
009600020821     c                   leave
009700020821     c                   end
009800020821     c                   if        caeatb <> *blanks
009900020821     c                   iter
010000020821     c                   end
010100020821     c                   add       1             x
010200020821     c                   eval      ecc(x) = caetfe
010300020821     c                   eval      dec(x) = caedde
010400020821     c                   eval      sca(x) = caedsc
010500020821     c                   enddo
010600020821     c                   endsr
010700000607      *-----------------------------------------------
010800000607     c     *inzsr        begsr
010900000607      *-----------------------------------------------
011200020402     c*
011300020821     C                   Z-ADD     1             CODUT             1 0
011400020821     C                   CALL      'X§PARUT'
011500020821     C                   PARM                    UTEDSE
011600020821     C                   MOVEL     REC80         CNCR80
011700020821     c*
011800020821     c     kfvv          klist
011900020821     c                   kfld                    fvvnpg
012000020821     c                   kfld                    fvvnfv
012200020821     c                   eval      fvvnpg = 4
012300020821     c*
012400020821     c     kcae          klist
012500020821     c                   kfld                    caeepa
012600020821     c                   kfld                    lna
012700020821     c                   eval      caeepa = '6'
012800020402     c                   endsr
