000100021127      *PARMS DFTACTGRP(*NO) ACTGRP(*CALLER)
000200021127      *PARMS COMMIT(*NONE) DATFMT(*ISO)
000300021127      *PARMS DYNUSRPRF(*OWNER)
000400030926
000500030929      /TITLE Controllo quadratura riconciliazione.
000600030929
000700030929     H DECEDIT(*JOBRUN)
000800030926
000900030926     D NullPtr         S               *
001000030926     D AnDiz00f      E DS
001100030926     D                                     BASED(NullPtr)
001200030926     D                                     PREFIX(DIZ)
001300030926
001400030926     D Societa         S
001500030926     D                                     LIKE(DizSocieta)
001600030926     D Banca           S
001700030926     D                                     LIKE(DizBanca)
001800030926     D Linea           S
001900030926     D                                     LIKE(DizLinea)
002000030926     D NrTeso          S
002100030926     D                                     LIKE(DizNr)
002200030926
002300030926     D BMVSocieta      S
002400030926     D                                     LIKE(DizSocieta)
002500030926     D BMVBanca        S
002600030926     D                                     LIKE(DizBanca)
002700030926     D BMVLinea        S
002800030926     D                                     LIKE(DizLinea)
002900030926     D BMVNrTeso       S
003000030926     D                                     LIKE(DizNr)
003100030926     D BMVImporto      S
003200030926     D                                     LIKE(DizImporto)
003300030926
003400030926     D BMBSocieta      S
003500030926     D                                     LIKE(DizSocieta)
003600030926     D BMBBanca        S
003700030926     D                                     LIKE(DizBanca)
003800030926     D BMBLinea        S
003900030926     D                                     LIKE(DizLinea)
004000030926     D BMBNrRic        S
004100030926     D                                     LIKE(DizNr)
004200030926     D BMBImporto      S
004300030926     D                                     LIKE(DizImporto)
004400030929
004500030929      /COPY QSYSINC/QRPGLESRC,QUSEC
004600030929
004700030929     D LngTx01         S            512                                         Text string
004800030929     D LngTx02         S             10I 0                                      Length text string
004900030929     D                                     INZ(%SIZE(LngTx01))
005000030929     D LngTx03         S              7                                         Message ID
005100030929     D                                     INZ('BAR0019')
005200030929     D LngTx04         S             20                                         Qualified MSGF
005300030929     D                                     INZ('YBARMSG   *LIBL')
005400030926
005500021127     C     *ENTRY        PLIST
005600030926     C                   PARM                    Societa
005700030926     C                   PARM                    Banca
005800030926     C                   PARM                    Linea
005900030926     C                   PARM                    NrTeso
006000030926
006100030926     C                   CLEAR                   BMVImporto
006200030926     C                   CLEAR                   BMBImporto
006300030926
006400021127     C/EXEC SQL
006500030926     C+ DECLARE NDBMV00F CURSOR FOR SELECT BMVSOCIETA, BMVBANCA,
006600030926     C+ BMVLINEA, BMVNRTESO, SUM(BMVIMPORTO*(BMVDARE-BMVAVERE)) FROM
006700030926     C+ NDBMV00F WHERE BMVSOCIETA = :Societa AND BMVBANCA = :Banca AND
006800030926     C+ BMVLINEA = :Linea AND BMVNRTESO = :NrTeso GROUP BY BMVSOCIETA,
006900030926     C+ BMVBANCA, BMVLINEA, BMVNRTESO
007000030926     C/END-EXEC
007100030926
007200030926     C/EXEC SQL
007300030926     C+ OPEN NDBMV00F
007400030926     C/END-EXEC
007500030926
007600030926     C/EXEC SQL
007700030926     C+ FETCH NEXT FROM NDBMV00F INTO :BMVSocieta, :BMVBanca, :BMVLinea,
007800030926     C+ :BMVNrTeso, :BMVImporto
007900030926     C/END-EXEC
008000030926
008100030926     C/EXEC SQL
008200030926     C+ CLOSE NDBMV00F
008300030926     C/END-EXEC
008400030926
008500030926     C/EXEC SQL
008600030926     C+ DECLARE NDBMB00F CURSOR FOR SELECT BMBSOCIETA, BMBBANCA,
008700030926     C+ BMBLINEA, BMBNRRIC, SUM(BMBIMPORTO*(BMBDARE-BMBAVERE)) FROM
008800030926     C+ NDBMB00F WHERE BMBSOCIETA = :Societa AND BMBBANCA = :Banca AND
008900030926     C+ BMBLINEA = :Linea AND BMBNRRIC = :NrTeso GROUP BY BMBSOCIETA,
009000030926     C+ BMBBANCA, BMBLINEA, BMBNRRIC
009100030926     C/END-EXEC
009200030926
009300030926     C/EXEC SQL
009400030926     C+ OPEN NDBMB00F
009500030926     C/END-EXEC
009600030926
009700030926     C/EXEC SQL
009800030926     C+ FETCH NEXT FROM NDBMB00F INTO :BMBSocieta, :BMBBanca, :BMBLinea,
009900030926     C+ :BMBNrRic, :BMBImporto
010000030926     C/END-EXEC
010100030926
010200030926     C/EXEC SQL
010300030926     C+ CLOSE NDBMB00F
010400030926     C/END-EXEC
010500030926
010600030926     C                   IF        BMVImporto <> BMBImporto
010700030929
010800030929     C                   EVAL      LngTx01 = 'La riconciliazione del blocco '
010900030929     C                             + Societa + ' ' + Banca + ' ' + Linea + ' '
011000030929     C                             + %TRIM(%EDITC(NrTeso:'Z'))
011100030929     C                             + ' non è stata eseguita correttamente-
011200030929     C                              in quanto risulta una squadratura di '
011300030929     C                             + %TRIM(%EDITC(BMVImporto-BMBImporto:'2'))
011400030929     C                             + '. Avvisare il responsabile della-
011500030929     C                              manutenzione del programma senza chiudere-
011600030929     C                              questa finestra.'
011700030929     C
011800030929     C                   CALL      'QUILNGTX'
011900030929     C                   PARM                    LngTx01
012000030929     C                   PARM                    LngTx02
012100030929     C                   PARM                    LngTx03
012200030929     C                   PARM                    LngTx04
012300030929     C                   PARM                    QUSEC
012400030929     C
012500030926     C                   ENDIF
012600030926
012700030926     C                   RETURN
