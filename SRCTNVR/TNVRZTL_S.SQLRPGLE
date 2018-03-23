000100940912     H*------------------------------------------------------------------------*
000200111007     H*
000300940912     H*------------------------------------------------------------------------*
000400930128     H DECEDIT('0,') DATEDIT(*DMY.)
000500111011     Ftitas30c  if   E           k DISK
000600111011     Ffiarbf1c  if   E           k DISK
000700111007     Fqsysprt   O    F  198        PRINTER OFLIND(*INOF)
000800111007     d*------------------------------------------------------------------------*
000900111007     D DSarb         E DS                  EXTNAME(fnarb00f)
001000111011     d Datasys         s               d   datfmt(*iso) inz(*sys)
001100111011     d orasys          s               t   timfmt(*iso) inz(*sys)
001200111011     d oraiso          s               t   timfmt(*iso)
001300111010     d
001400111010     ITITAS000      02
001500111010     ITITAS010      03
001600111010     ITITASP00      04
001700111011     i
001800111011     IFNARBG00      97
001900111011     IFNARBD00      96
002000111011     IFNARBK00      95
002100111011     IFIARBT00      94
002200111011     IFNARBV00      92
002300111011     IFNARBM00      91
002400940912     C*------------------------------------------------------------------------*
002500111010     c
002600111007     C     karb          KLIST
002700111007     C                   KFLD                    ARbAAS                         ANNO SPEDIZIONE
002800111007     C                   KFLD                    ARbLNP                         LINEA DI PARTENZA
002900111007     C                   KFLD                    ARbNRS                         NUMERO SERIE
003000111007     C                   KFLD                    ARbNSP                         NUMERO SPEDIZIONE
003100111011     c*
003200111011     c                   clear                   dateu             8 0
003300111011     c                   clear                   orau              6 0
003400111011     c                   eval      dateu=%dec(datasys)
003500111011     c                   eval      oraiso=orasys
003600111011     c                   subdur    1:*h          oraiso
003700111011     c                   eval      orau= %dec(oraiso)
003800111011     c
003900111007     c
004000111007     c                   clear                   numbol            5 0
004100111007     C                   EXCEPT    testa
004200040304     C/EXEC SQL
004300040304     C+ DECLARE A1 CURSOR FOR
004400111007     C+ SELECT *   FROM fnarb00f JOIN TITAS00F ON
004500111007     C+ arbAAS = TASAAS AND ARbLNP = TASLNP AND ARbNRS = TASNRS AND
004600111011     C+ ARbNSP = TASNSP where arbfin <>tasfin
004700040304     C/END-EXEC
004800040304     c*
004900040304     c*
005000040304     C/EXEC SQL
005100040304     C+ OPEN A1
005200040304     C/END-EXEC
005300040304
005400040304     C                   do        *hival
005500040304
005600040304     C/EXEC SQL
005700111007     C+ FETCH NEXT FROM A1 INTO : dsarb
005800040304     C/END-EXEC
005900040304
006000040304     C                   select
006100040304     C* FINE FILE
006200040304     C                   when      sqlcod = 100
006300040304     C* stampa riga di fine
006400111007     C                   EXCEPT    fine
006500040304     C                   SETON                                        LR
006600040304     c                   leave
006700111007     c
006800040304     C* DETTAGLIO
006900040304     C                   when      sqlcod = 0
007000111011     C* ESCLUDO le bolle in C/S
007100111011     c                   setoff                                       020304
007200111011     C     karb          chain     titas30c
007300111011     c
007400111011     c* Escludo se già contabilizzata in
007500111011    1c                   if        tasfic=' '
007600111011     c* Escludo  se c'e' variazione bolle a udate -  1 ora
007700111011     c
007800111011     c
007900111011     c                   clear                   sii0              1
008000111011     c                   setoff                                       919294
008100111011     c                   setoff                                       959697
008200111011     C     karb          setll     fiarbf1c
008300111011     C     karb          reade     fiarbf1c
008400111011    2c                   dow       not %eof(fiarbf1c)
008500111011     c
008600111011     c* se trovo una variazione I0 fatta ell'ultima orra --> non stampo
008700111011    3c                   if        *in96 and arbcvb='I0'
008800111011    4c                   if        arbdtv=dateu and arborv>=orau
008900111011     c                   eval      siI0='S'
009000111011     c                   leave
009100111011    4c                   endif
009200111011    3c                   endif
009300111011     c
009400111011     c                   setoff                                       919294
009500111011     c                   setoff                                       959697
009600111011     C     karb          reade     fiarbf1c
009700111011    2c                   enddo
009800111011     c
009900111011    2c                   if        sii0=' '
010000111007     C                   EXCEPT    bollaag
010100111007     c                   eval      numbol=numbol+1
010200111011    2c                   endif
010300111011    1c                   endif
010400111007     c
010500040304     C                   endsl
010600040304     C*
010700040304     C                   enddo
010800040304
010900040304     C/EXEC SQL
011000040304     C+ CLOSE A1
011100040304     C/END-EXEC
011200111007     c*------------------------------------------------------------------------*
011300040304     C     *INZSR        BEGSR
011400111007     c*------------------------------------------------------------------------*
011500040304     C                   ENDSR
011600111007     Oqsysprt   E            TESTA            02
011700111011     O                                           +0 'TNVRZTL: falg inoltro '
011800111011     O                                           +0 'in TAS ZTL e non in ARB'
011900111007     o
012000111010     O          E            bollaag     1
012100111007     O                       ARbAAS            +  5
012200111007     O                       ARbLNP            +  1
012300111007     O                       ARbNRS            +  1
012400111007     O                       ARbNSP            +  1
012500111010     O                       ARbmgs            +  1 '  /  '
012600111007     O                       ARbfin            +  1
012700111007     O                                         +  1 'TASFIN'
012800111007     O                       tasfin            +  1
012900111010     O          E            fine        1
013000111011     O                                           30 'num bolle diverse'
013100111007     O                       NUMbol        2     44
