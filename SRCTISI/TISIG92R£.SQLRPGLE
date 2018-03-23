000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200170424      * ?------------------------------------------------------------------ */
000300161025      * Il pgm scrive i record di valorizzazione autisti per giornata
000400170420      *     IL DETTAGLIO DEI RITIRI
000500170424      * ?------------------------------------------------------------------ */
000600170424     Fwfdep00f  O    E             disk
000700000607     D kpjba         e ds
000800170424      **
000900170424     d sqlDEPOT        s                   like(DEPDEPOT )
001000170424     d sqlNAME         s             35a
001100170424     d sqlNAME1        s             35a
001200170424     d sqlADDR         s             35a
001300170424     d sqlADDR1        s             35a
001400170424     d sqlZIPC         s              9a
001500170424     d sqlTOWN         s                   like(DEPTOWN  )
001600170424     d sqlCOUNT        s                   like(DEPCOUNT )
001700170424     d sqlCCODE        s                   like(DEPCCODE )
001800170424     d sqlCCLI         s                   like(DEPCCLI  )
001900170428     d sqlCFAT         s                   like(DEPCFAT  )
002000170424     d sqlRAGS         s                   like(DEPRAGS  )
002100170424     d sqlVIA          s                   like(DEPVIA   )
002200170424     d sqlCAP          s              5s 0
002300170424     d sqlCIT          s                   like(DEPCIT   )
002400170424     d sqlSTA          s                   like(DEPSTA   )
002500170424     d sqlNOT84        s                   like(DEPNOT84 )
002600170424     d sqlNOT89        s                   like(DEPNOT89 )
002700170420      **
002800170428      **
002900170424      * ?------------------------------------------------------------------ */
003000170424     c     *entry        plist
003100170424     c                   parm                    KPJBA
003200170424     c                   seton                                        LR
003300170424     C*
003400161026     C/EXEC SQL
003500170424     C+ DECLARE a1 CURSOR FOR
003600170424     C+   with
003700170424     C+   NOTA84F as ( select substr(NTCNK1, 1, 11) as key84,
003800170424     C+    NTCTNT as Tnot84, NTCRNT as NOTA84 from tfntc00f
003900170424     C+     where NTCTNT ='84'),
004000170424     C+   NOTA89F as ( select substr(NTCNK1, 1, 11) as key89,
004100170424     C+    NTCTNT as Tnot89, NTCRNT as NOTA89 from tfntc00f
004200170424     C+     where NTCTNT ='89')
004300170424     C+ SELECT pdcDPC, CDPNAM1, CDPNAM2, CDPADR1,
004400170424     C+  CDPADR2, CDPPTC, CDPCTY, CDPISO2, pdcNAR, pdcKSC,
004500170424     C+   IFNULL(ACORAG, ' '),
004600170424     C+   IFNULL(INDVIA, ' '),
004700170424     C+   IFNULL(INDCAP,  0 ),
004800170424     C+   IFNULL(INDCIT, ' '),
004900170424     C+   IFNULL(INDSTA, ' '),
005000170428     C+   IFNULL(CLPSCF,  0 ),
005100170424     C+   IFNULL(NOTA84, ' '),
005200170424     C+   IFNULL(NOTA89, ' ')
005300170424     C+  from (select * from dppdc00f join dpcdp00f
005400170424     C+    on  pdcdpc = cdpdpc
005500170424     C+  where dec(curdate(), 8) between pdcdde  and pdcdsc and
005600170424     C+  cdpver in (select  cvever from dpcve00f where
005700170424     C+  dec(curdate(), 8) between cvedde  and cvedsc) ) a
005800170424     C+   left outer join cnind00f on PDCKSC = INDKSC
005900170424     C+    and indkcc=151 and indksc >0
006000170424     C+   left outer join cnaco00f on PDCKSC = ACOKSC
006100170424     C+    and acokcc=151 and acoksc >0
006200170428     C+   left outer join cnclp00f on clpKSC = ACOKSC
006300170428     C+    and clpkcc=151 and clpksc >0
006400170424     C+   left outer join NOTA84F on
006500170424     C+    digits(acokcc) concat digits(acoksc) =key84
006600170424     C+   left outer join NOTA89F on
006700170424     C+    digits(acokcc) concat digits(acoksc) =key89
006800161026     C/END-EXEC
006900170424      *
007000170424     C/EXEC SQL
007100170424     C+ OPEN a1
007200170424     C/END-EXEC
007300170424      *
007400170424     C                   do        *hival
007500170424
007600170424     C/EXEC SQL
007700170424     C+ FETCH NEXT FROM a1 INTO
007800170424     C+ :sqlDEPOT, :sqlNAME, :sqlNAME1, :sqlADDR, :sqlADDR1,
007900170428     C+ :sqlZIPC,  :sqlTOWN, :sqlCOUNT,
008000170428     C+ :sqlCCODE, :sqlCCLI, :sqlRAGS, :sqlVIA,   :sqlCAP,
008100170428     C+ :sqlCIT,   :sqlSTA,  :sqlCFAT, :sqlNOT84, :sqlNOT89
008200170424     C/END-EXEC
008300170428      **
008400170424     C                   select
008500170424     C                   when      sqlcod = 100
008600170424     c                   leave
008700170424     C                   WHEN      SqlCod < 0
008800170424     c                   if        sqlcod <> -913
008900170424     C                   seton                                        H1
009000170424     c                   end
009100170424     c                   seton                                        lr
009200170424     c                   LEAVE
009300170424     C                   other
009400170424     c                   clear                   wfdep000
009500170424     c* per ritiro non ho il giro
009600170424     c                   eval      DEPDEPOT  = sqlDEPOT
009700170424     c                   eval      DEPNAME   = sqlNAME
009800170424     c                   eval      DEPADDR   = sqlADDR
009900170424     c                   eval      DEPZIPC   = sqlZIPC
010000170424     c                   eval      DEPTOWN   = sqlTOWN
010100170424     c                   eval      DEPCOUNT  = sqlCOUNT
010200170424     c                   eval      DEPCCODE  = sqlCCODE
010300170424     c                   eval      DEPCCLI   = sqlCCLI
010400170428     c                   eval      DEPCFAT   = sqlCFAT
010500170424     c                   eval      DEPRAGS   = sqlRAGS
010600170424     c                   eval      DEPVIA    = sqlVIA
010700170424     c                   movel     sqlCAP        DEPCAP
010800170424     c                   eval      DEPCIT    = sqlCIT
010900170424     c                   eval      DEPSTA    = sqlSTA
011000170424     c                   eval      DEPNOT84  = sqlNOT84
011100170424     c                   eval      DEPNOT89  = sqlNOT89
011200170424     c                   write     wfdep000
011300170424     C                   endsl
011400170424     C                   enddo
011500170424     C/EXEC SQL
011600170424     C+ CLOSE a1
011700170424     C/END-EXEC
011800030429      *
011900170424     c                   RETURN
012000161025      * ?___________________________________________________________________
