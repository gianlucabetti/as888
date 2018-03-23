000100940912     H*------------------------------------------------------------------------*
000200111007     H*
000300940912     H*------------------------------------------------------------------------*
000400930128     H DECEDIT('0,') DATEDIT(*DMY.)
000500111010     Ftitas30c  uf   E           k DISK
000600111007     Fqsysprt   O    F  198        PRINTER OFLIND(*INOF)
000700111007     d*------------------------------------------------------------------------*
000800111007     D DSarb         E DS                  EXTNAME(fnarb00f)
000900111010     d
001000111010     ITITAS000      02
001100111010     ITITAS010      03
001200111010     ITITASP00      04
001300940912     C*------------------------------------------------------------------------*
001400111010     c     *entry        plist
001500111010     c                   parm                    AGGIOR            1
001600111010     c
001700111007     C     karb          KLIST
001800111007     C                   KFLD                    ARbAAS                         ANNO SPEDIZIONE
001900111007     C                   KFLD                    ARbLNP                         LINEA DI PARTENZA
002000111007     C                   KFLD                    ARbNRS                         NUMERO SERIE
002100111007     C                   KFLD                    ARbNSP                         NUMERO SPEDIZIONE
002200111007     c
002300111007     c                   clear                   numbol            5 0
002400111007     C                   EXCEPT    testa
002500040304     C/EXEC SQL
002600040304     C+ DECLARE A1 CURSOR FOR
002700111007     C+ SELECT *   FROM fnarb00f JOIN TITAS00F ON
002800111007     C+ arbAAS = TASAAS AND ARbLNP = TASLNP AND ARbNRS = TASNRS AND
002900111007     C+ ARbNSP = TASNSP where arbfin in('T', 'Z') and tasfin not in ('T', 'Z')
003000040304     C/END-EXEC
003100040304     c*
003200040304     c*
003300040304     C/EXEC SQL
003400040304     C+ OPEN A1
003500040304     C/END-EXEC
003600040304
003700040304     C                   do        *hival
003800040304
003900040304     C/EXEC SQL
004000111007     C+ FETCH NEXT FROM A1 INTO : dsarb
004100040304     C/END-EXEC
004200040304
004300040304     C                   select
004400040304     C* FINE FILE
004500040304     C                   when      sqlcod = 100
004600040304     C* stampa riga di fine
004700111007     C                   EXCEPT    fine
004800040304     C                   SETON                                        LR
004900040304     c                   leave
005000111007     c
005100040304     C* DETTAGLIO
005200040304     C                   when      sqlcod = 0
005300111007     c                   setoff                                       020304
005400111007     C     karb          chain     titas30c
005500111007     c                   if        %found(titas30c)
005600111007     C                   if        *in02 and tasfic=' ' and tasfin<>'D' and
005700111007     c                             tasfin<>'I'
005800111007     C                   EXCEPT    bollaag
005900111007     c                   eval      numbol=numbol+1
006000111007     c                   eval      tasfin=arbfin
006100111010     c
006200111010     c                   if        aggior='S'
006300111010     c                   update    titas000
006400111010     c                   endif
006500111010     c
006600111007     c                   endif
006700111007     c                   endif
006800111007     c
006900040304     C                   endsl
007000040304     C*
007100040304     C                   enddo
007200040304
007300040304     C/EXEC SQL
007400040304     C+ CLOSE A1
007500040304     C/END-EXEC
007600111007     c*------------------------------------------------------------------------*
007700040304     C     *INZSR        BEGSR
007800111007     c*------------------------------------------------------------------------*
007900040304     C                   ENDSR
008000111007     Oqsysprt   E            TESTA            02
008100111007     O                                           +0 'TNVRZTL: sistemo flag '
008200111007     O                                           +0 'inoltro in TAS'
008300111007     o
008400111010     O          E            bollaag     1
008500111007     O                       ARbAAS            +  5
008600111007     O                       ARbLNP            +  1
008700111007     O                       ARbNRS            +  1
008800111007     O                       ARbNSP            +  1
008900111010     O                       ARbmgs            +  1 '  /  '
009000111007     O                       ARbfin            +  1
009100111007     O                                         +  1 'TASFIN'
009200111007     O                       tasfin            +  1
009300111010     O          E            fine        1
009400111007     O                                           30 'num aggiornate'
009500111007     O                       NUMbol        2     44
