000100940912     H*------------------------------------------------------------------------*
000200940912     H*------------------------------------------------------------------------*
000300930128     H DECEDIT('0,') DATEDIT(*DMY.)
000400111111     Fazorg01l  if   E           k DISK
000500040303     F                                     INFDS(£DSAR5)
000600931105     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
000700931105     F                                     FORMLEN(66)
000800931105     F                                     FORMOFL(60)
000900940912     L*------------------------------------------------------------------------*
001000940912     D*------------------------------------------------------------------------*
001100931105     D TES             S             66    DIM(15) CTDATA PERRCD(1)
001200940912     C*------------------------------------------------------------------------*
001300040304     C/EXEC SQL
001400040304     C+ DECLARE A1 CURSOR FOR
001500111111     C+ SELECT * from tntam00f where tamatb=' ' and tamdst>20140000
001600111111     C+ order by tamksc, tamctr, tamprg
001700040304     C/END-EXEC
001800040304     c*
001900040304     C* SE TROVA LA BOLLA IN TITASP CANCELLO FIAR500F E LO METTO IN FIAR5P
002000040304     c*
002100040304     C/EXEC SQL
002200040304     C+ OPEN A1
002300040304     C/END-EXEC
002400040304
002500040304     C                   do        *hival
002600040304
002700040304     C/EXEC SQL
002800111111     C+ FETCH NEXT FROM A1 INTO : tntamds
002900040304     C/END-EXEC
003000040304
003100040304     C                   select
003200040304     C* FINE FILE
003300040304     C                   when      sqlcod = 100
003400040304     C* stampa riga di fine
003500040304     C                   EXCEPT    STAFIN
003600040304     C                   SETON                                        LR
003700040304     c                   leave
003800040304     C* DETTAGLIO
003900040304     C                   when      sqlcod = 0
004000040304     C     RRNAR5        chain     FIAR5000                                     *CANCELLO
004100040308     c                   if        %found(fiar500f)
004200040304     C     rrnar5        DELETE    FIAR5000                                     *CANCELLO
004300040304     C                   WRITE     FIAR5P00                             99      *SCRIVO ECC2VP
004400040304     C                   IF        *IN99
004500040304     C                   EXCEPT    ERRAR5
004600040304     C                   SETON                                        LR
004700040305     C                   leave
004800040304     C                   ELSE
004900040304     C                   ADD       1             NUMAR5                         *REC TRASF ECC2V
005000040304     C                   END
005100040308     C                   END
005200040304     C                   endsl
005300040304     C*
005400040304     C                   enddo
005500040304
005600040304     C/EXEC SQL
005700040304     C+ CLOSE A1
005800040304     C/END-EXEC
005900040304     D*------------------------------------------------------------------------*
006000040304     C     *INZSR        BEGSR
006100040304     D*------------------------------------------------------------------------*
006200040303     C     KEYAR5        KLIST
006300040303     C                   KFLD                    AR5AAS                         ANNO SPEDIZIONE
006400040303     C                   KFLD                    AR5LNP                         LINEA DI PARTENZA
006500040303     C                   KFLD                    AR5NRS                         NUMERO SERIE
006600040303     C                   KFLD                    AR5NSP                         NUMERO SPEDIZIONE
006700040303     C                   Z-ADD     *ZEROS        NUMAR5            9 0
006800040304     C* STAMPA RIGA DI TESTATA
006900040304     C                   EXCEPT    TESTA
007000040304     C                   ENDSR
007100940905     OPRTF198   E            TESTA            02
007200040304     O                                           +0 'TNVRAR5R1'
007300040304     O                                           82 'TRASF. FIAR5->FIAR5P    '
007400931105     O                                          190 'Pagina:'
007500931105     O                       PAGE          Z    198
007600931105     O          E            TESTA       1  1
007700931105     O                       TES(2)              66
007800931105     O                       TES(3)             132
007900931105     O                       TES(4)             198
008000931105     O          E            TESTA          1
008100931105     O                       TES(1)              66
008200931105     O                       TES(1)             132
008300931105     O                       TES(1)             198
008400040303     O          E            ERRAR5         1
008500040303     O                       AR5AAS            +  5
008600040303     O                       AR5LNP            +  1
008700040303     O                       AR5NRS            +  1
008800040303     O                       AR5NSP            +  1
008900040303     O                                         +  1 'ERR. WRITE FIAR5   '
009000040303     O                                         +  1 'NR. RELATIO REC.'
009100040303     O                       NRRAR5            +  1
009200040303     O          E            STAFIN         1
009300040303     O                                           30 'N.REC.TRASF. FIAR5'
009400040303     O                       NUMAR5        2     44
009500950308     O          E            STAFIN           60
009600931115     O                                           76 '*** FINE STAMPA ***'
009700940912     O*------------------------------------------------------------------------*
009800931105**
009900931105------------------------------------------------------------------  1
010000931105 DATA    ORA       AA LNP SE N.SPED. TB     ERR  DESCRIZIONE ERROR  2
010100950308E                                                                   3
010200931105                                                                    4
010300931105                                                                    5
010400931105                                                                    6
010500931105                                                                    7
010600931105                                                                    8
010700931105                                                                    9
010800931105                                                                   10
010900931105                                                                   11
011000931105                                                                   12
011100931105                                                                   13
011200931105                                                                   14
011300931105                                                                   15
