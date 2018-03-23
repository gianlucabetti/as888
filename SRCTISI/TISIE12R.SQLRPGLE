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
003300161026     C/EXEC SQL
003400170424     C+ DECLARE a1 CURSOR FOR
003500170424     C+   with
003600170424     C+   NOTA84F as ( select substr(NTCNK1, 1, 11) as key84,
003700170424     C+    NTCTNT as Tnot84, NTCRNT as NOTA84 from tfntc00f
003800170424     C+     where NTCTNT ='84'),
003900170424     C+   NOTA89F as ( select substr(NTCNK1, 1, 11) as key89,
004000170424     C+    NTCTNT as Tnot89, NTCRNT as NOTA89 from tfntc00f
004100170424     C+     where NTCTNT ='89')
004200171114     C+ SELECT legDPC, CDPNAM1, CDPNAM2, CDPADR1,
004300171114     C+  CDPADR2, CDPPTC, CDPCTY, CDPISO2, legNAR, legKSC,
004400170424     C+   IFNULL(ACORAG, ' '),
004500170424     C+   IFNULL(INDVIA, ' '),
004600170424     C+   IFNULL(INDCAP,  0 ),
004700170424     C+   IFNULL(INDCIT, ' '),
004800170424     C+   IFNULL(INDSTA, ' '),
004900170428     C+   IFNULL(CLPSCF,  0 ),
005000170424     C+   IFNULL(NOTA84, ' '),
005100170424     C+   IFNULL(NOTA89, ' ')
005200171114     C+  from (select * from dpleg10f join dpcdp00f
005300171114     C+    on  legDPC = cdpDPC
005400171109     C+  where dec(curdate(), 8) between legdde  and legdsc and
005500171114     C+  cdpver in (select  cvever from dpcve00f where
005600170424     C+  dec(curdate(), 8) between cvedde  and cvedsc) ) a
005700171109     C+   left outer join cnind00f on legKSC = INDKSC
005800170424     C+    and indkcc=151 and indksc >0
005900171109     C+   left outer join cnaco00f on legKSC = ACOKSC
006000170424     C+    and acokcc=151 and acoksc >0
006100170428     C+   left outer join cnclp00f on clpKSC = ACOKSC
006200170428     C+    and clpkcc=151 and clpksc >0
006300170424     C+   left outer join NOTA84F on
006400170424     C+    digits(acokcc) concat digits(acoksc) =key84
006500170424     C+   left outer join NOTA89F on
006600170424     C+    digits(acokcc) concat digits(acoksc) =key89
006700161026     C/END-EXEC
006800170424      *
006900170424     C/EXEC SQL
007000170424     C+ OPEN a1
007100170424     C/END-EXEC
007200171114     c                   if        sqlcod <  0
007300171114     C                   seton                                        H1
007400171114     c                   seton                                        lr
007500171114     c                   return
007600171114     c                   end
007700171114      *
007800170424      *
007900170424     C                   do        *hival
008000170424
008100170424     C/EXEC SQL
008200170424     C+ FETCH NEXT FROM a1 INTO
008300170424     C+ :sqlDEPOT, :sqlNAME, :sqlNAME1, :sqlADDR, :sqlADDR1,
008400170428     C+ :sqlZIPC,  :sqlTOWN, :sqlCOUNT,
008500170428     C+ :sqlCCODE, :sqlCCLI, :sqlRAGS, :sqlVIA,   :sqlCAP,
008600170428     C+ :sqlCIT,   :sqlSTA,  :sqlCFAT, :sqlNOT84, :sqlNOT89
008700170424     C/END-EXEC
008800170428      **
008900170424     C                   select
009000170424     C                   when      sqlcod = 100
009100170424     c                   leave
009200170424     C                   WHEN      SqlCod < 0
009300170424     c                   if        sqlcod <> -913
009400170424     C                   seton                                        H1
009500170424     c                   end
009600170424     c                   seton                                        lr
009700170424     c                   LEAVE
009800170424     C                   other
009900170424     c                   clear                   wfdep000
010000170424     c* per ritiro non ho il giro
010100170424     c                   eval      DEPDEPOT  = sqlDEPOT
010200170424     c                   eval      DEPNAME   = sqlNAME
010300170424     c                   eval      DEPADDR   = sqlADDR
010400170424     c                   eval      DEPZIPC   = sqlZIPC
010500170424     c                   eval      DEPTOWN   = sqlTOWN
010600170424     c                   eval      DEPCOUNT  = sqlCOUNT
010700170424     c                   eval      DEPCCODE  = sqlCCODE
010800170424     c                   eval      DEPCCLI   = sqlCCLI
010900170428     c                   eval      DEPCFAT   = sqlCFAT
011000170424     c                   eval      DEPRAGS   = sqlRAGS
011100170424     c                   eval      DEPVIA    = sqlVIA
011200170424     c                   movel     sqlCAP        DEPCAP
011300170424     c                   eval      DEPCIT    = sqlCIT
011400170424     c                   eval      DEPSTA    = sqlSTA
011500170424     c                   eval      DEPNOT84  = sqlNOT84
011600170424     c                   eval      DEPNOT89  = sqlNOT89
011700170424     c                   write     wfdep000
011800170424     C                   endsl
011900170424     C                   enddo
012000170424     C/EXEC SQL
012100170424     C+ CLOSE a1
012200170424     C/END-EXEC
012300030429      *
012400170424     c                   RETURN
012500161025      * ?___________________________________________________________________
