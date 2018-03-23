000100940912     H*------------------------------------------------------------------------*
000200111007     H*
000300940912     H*------------------------------------------------------------------------*
000400930128     H DECEDIT('0,') DATEDIT(*DMY.)
000500111010     FFIARG01L  iF   E           K DISK
000600111007     Fqsysprt   O    F  198        PRINTER OFLIND(*INOF)
000700111007     d*------------------------------------------------------------------------*
000800111007     D DSarb         E DS                  EXTNAME(fnarb00f)
000900111010     D fnlvstds      E DS
001000111010     D fnlv55ds      E DS
001100111010     D kpjba         E DS
001200111011     d Datasys         s               d   datfmt(*iso) inz(*sys)
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
002700111017     C+ SELECT *   FROM fnarb00f
002800111017     C+ where arbfin in('T', 'Z') and arbaas=2011 and arbmgs>1000
002900111017     c+  and (arbfbc<>' ' or (arbncd=0 and arbnrs=0))
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
005300111010     c
005400111010     c* cerco il giro  --> se mancarichiamo fnlvstr
005500111010     C     karb          chain     fiarg01l
005600111010    1c                   if        not %found(fiarg01l)
005700111010     c                             or argcgi=*blanks
005800111010     c                   except    bollaag
005900111010     c                   eval      numbol=numbol+1
006000111010     c
006100111010     c                   clear                   fnlvstds
006200111010     c                   eval      ilvstaas=arbaas
006300111010     c                   eval      ilvstlnp=arblnp
006400111010     c                   eval      ilvstnrs=arbnrs
006500111010     c                   eval      ilvstnsp=arbnsp
006600111010     c                   eval      ilvstmgs=arbmgs
006700111010     c                   eval      ilvstfin=arbfin
006800111010     c* giro vuoto
006900111010     c***                eval      ilvstcgi=argcgi
007000111010     c                   clear                   fnlv55ds
007100111010     C                   MOVEL     '6'           D55TPT
007200111010     C                   MOVEL     ARBLNA        D55LIN
007300111011     c                   if        arbddc>0
007400111011     c                   eval      d55drf =arbddc
007500111011     c                   else
007600111011     c                   eval      d55drf  =%dec(datasys)
007700111011     c                   endif
007800111011     c
007900111010     C                   CALL      'FNLV55R'
008000111010     C                   PARM                    Fnlv55DS
008100111010     c                   eval      ilvstpoc=d55tfa
008200111011     c                   eval      ilvstdat=d55drf
008300111010     c
008400111010     c
008500111010     c                   if        aggior='S'
008600111010     c                   call      'FNLVSTR'
008700111010     c                   parm                    kpjba
008800111010     c                   parm                    fnlvstds
008900111010    1c                   endif
009000111010     c
009100111010    1c                   endif
009200111007     c
009300111010    0C                   endsl
009400040304     C*
009500040304     C                   enddo
009600040304
009700040304     C/EXEC SQL
009800040304     C+ CLOSE A1
009900040304     C/END-EXEC
010000111007     c*------------------------------------------------------------------------*
010100040304     C     *INZSR        BEGSR
010200111007     c*------------------------------------------------------------------------*
010300040304     C                   ENDSR
010400111007     Oqsysprt   E            TESTA            02
010500111010     O                                           +0 'TNVRZTL: sistemo ZTL x'
010600111010     O                                           +0 ' bolle in FFD '
010700111007     o
010800111010     O          E            bollaag     1
010900111007     O                       ARbAAS            +  5
011000111007     O                       ARbLNP            +  1
011100111007     O                       ARbNRS            +  1
011200111007     O                       ARbNSP            +  1
011300111010     O                       ARbmgs            +  1 '  /  '
011400111017     O                       ARbcbo            +  1
011500111017     O                       ARbfin            +  1
011600111010     O                                         +  1 'ARBffd'
011700111010     O                       arbffd            +  1
011800111017     O                                         +  1 'ARBfbc'
011900111017     O                       arbfbc            +  1
012000111010     O          E            fine        1
012100111007     O                                           30 'num aggiornate'
012200111007     O                       NUMbol        2     44
