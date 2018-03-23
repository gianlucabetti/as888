000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200170424      * ?------------------------------------------------------------------ */
000300161025      * Il pgm scrive i record di valorizzazione autisti per giornata
000400170420      *     IL DETTAGLIO DEI RITIRI
000500170424      * ?------------------------------------------------------------------ */
000600171114     Fwfdep10f  O    E             disk
000700000607     D kpjba         e ds
000800170424      **
000900171114     d sqlDEPOT        s              4a
001000171114     d sqlDEP7         s              7a
001100170424     d sqlNAME         s             35a
001200170424     d sqlNAME1        s             35a
001300170424     d sqlADDR         s             35a
001400170424     d sqlADDR1        s             35a
001500170424     d sqlZIPC         s              9a
001600170424     d sqlTOWN         s                   like(DEPTOWN  )
001700170424     d sqlCOUNT        s                   like(DEPCOUNT )
001800170424     d sqlCCODE        s                   like(DEPCCODE )
001900170424     d sqlCCLI         s                   like(DEPCCLI  )
002000170428     d sqlCFAT         s                   like(DEPCFAT  )
002100170424     d sqlRAGS         s                   like(DEPRAGS  )
002200170424     d sqlVIA          s                   like(DEPVIA   )
002300170424     d sqlCAP          s              5s 0
002400170424     d sqlCIT          s                   like(DEPCIT   )
002500170424     d sqlSTA          s                   like(DEPSTA   )
002600170424     d sqlNOT84        s                   like(DEPNOT84 )
002700170424     d sqlNOT89        s                   like(DEPNOT89 )
002800170420      **
002900170428      **
003000170424      * ?------------------------------------------------------------------ */
003100170424     c     *entry        plist
003200170424     c                   parm                    KPJBA
003300170424     c                   seton                                        LR
003400161026     C/EXEC SQL
003500170424     C+ DECLARE a1 CURSOR FOR
003600170424     C+   with
003700170424     C+   NOTA84F as ( select substr(NTCNK1, 1, 11) as key84,
003800170424     C+    NTCTNT as Tnot84, NTCRNT as NOTA84 from tfntc00f
003900170424     C+     where NTCTNT ='84'),
004000170424     C+   NOTA89F as ( select substr(NTCNK1, 1, 11) as key89,
004100170424     C+    NTCTNT as Tnot89, NTCRNT as NOTA89 from tfntc00f
004200170424     C+     where NTCTNT ='89')
004300171114     C+ SELECT legDPC, cdpDPTN, CDPNAME1, CDPNAME2, CDPSTRET,
004400171114     C+      CDPADDR1, CDPPTC, CDPCITY, CDPISO2, legNAR, legKSC,
004500170424     C+   IFNULL(ACORAG, ' '),
004600170424     C+   IFNULL(INDVIA, ' '),
004700170424     C+   IFNULL(INDCAP,  0 ),
004800170424     C+   IFNULL(INDCIT, ' '),
004900170424     C+   IFNULL(INDSTA, ' '),
005000170428     C+   IFNULL(CLPSCF,  0 ),
005100170424     C+   IFNULL(NOTA84, ' '),
005200170424     C+   IFNULL(NOTA89, ' ')
005300171114     C+  from (select * from dpleg10f join dpcdp10f
005400171114     C+    on  legdep7 = cdpDPTN
005500171109     C+  where dec(curdate(), 8) between legdde  and legdsc and
005600171114     C+  cdpver in (select  cvever from dpcve10f where
005700170424     C+  dec(curdate(), 8) between cvedde  and cvedsc) ) a
005800171109     C+   left outer join cnind00f on legKSC = INDKSC
005900170424     C+    and indkcc=151 and indksc >0
006000171109     C+   left outer join cnaco00f on legKSC = ACOKSC
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
007800171114     C+ :sqlDEPOT, :sqlDEP7, :sqlNAME, :sqlNAME1, :sqlADDR,
007900171114     C+ :sqlADDR1, :sqlZIPC,  :sqlTOWN, :sqlCOUNT,
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
009400171114     c                   clear                   wfdep100
009500170424     c* per ritiro non ho il giro
009600170424     c                   eval      DEPDEPOT  = sqlDEPOT
009700171114     c                   eval      DEPDEP7   = sqlDEP7
009800170424     c                   eval      DEPNAME   = sqlNAME
009900170424     c                   eval      DEPADDR   = sqlADDR
010000170424     c                   eval      DEPZIPC   = sqlZIPC
010100170424     c                   eval      DEPTOWN   = sqlTOWN
010200170424     c                   eval      DEPCOUNT  = sqlCOUNT
010300170424     c                   eval      DEPCCODE  = sqlCCODE
010400170424     c                   eval      DEPCCLI   = sqlCCLI
010500170428     c                   eval      DEPCFAT   = sqlCFAT
010600170424     c                   eval      DEPRAGS   = sqlRAGS
010700170424     c                   eval      DEPVIA    = sqlVIA
010800170424     c                   movel     sqlCAP        DEPCAP
010900170424     c                   eval      DEPCIT    = sqlCIT
011000170424     c                   eval      DEPSTA    = sqlSTA
011100170424     c                   eval      DEPNOT84  = sqlNOT84
011200170424     c                   eval      DEPNOT89  = sqlNOT89
011300171114     c                   write     wfdep100
011400170424     C                   endsl
011500170424     C                   enddo
011600170424     C/EXEC SQL
011700170424     C+ CLOSE a1
011800170424     C/END-EXEC
011900030429      *
012000170424     c                   RETURN
012100161025      * ?___________________________________________________________________
