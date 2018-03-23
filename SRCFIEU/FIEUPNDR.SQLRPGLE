000100010514     H DECEDIT('0,') DATEDIT(*DMY.)
000200010514      *----------------------------------------------------*
000300171214      * Allinea B.UNIT e DEPOT di PARTENZA su FIPND00F
000400010514      *----------------------------------------------------*
000500180306     FWFIEUPNDF o  a E             DISK
000600010514     d kpjba         e ds
000700180306
000800180305     D parcel14        s             14
000900180305     D depot7          s              7
001000180305
001100010514      *-------------------------------------------------------------------------*
001200010514     c     *entry        plist
001300010514     c                   parm                    kpjba
001400171221      ***
001500130628     c                   eval      *inlr = *on
001600180305
001700180206      ***************************************************** ******************
001800180307      *****************************************************************
001900180307     C/EXEC SQL
002000180307     C+ DELETE FROM WFIEUPNDF
002100180307     C/END-EXEC
002200180307      *****************************************************************
002300180206      ***************************************************** ******************
002400180206      ***  Se il DEPOT di PARTENZA presente sul FIPND è diverso
002500180206      ***   da quello dello SCAN 05 (TIDP400F)
002600180206      ***   prima sposta il presente depot del campo lungo 7
002700180206      ***    alla DS alla posizione 7 (per non dimenticarsi cosa c'era prima)
002800180206      ***-----------------------
002900180206     C/EXEC SQL
003000180206     c+  update fipnd00f set
003100180206     c+         pndetiu = substr(pndetiu, 1, 6) concat pndidp7
003200180206     c+  where  rrn(fipnd00f) in (SELECT rrn(fipnd00f) FROM
003300180206     c+  fipnd00f JOIN  (select * from tidp400f
003400180206     c+    where rrn(tidp400f) in  (select record from
003500180206     c+    (SELECT b.dp4parcel, min(rrn(b)) record FROM tidp400f b
003600180206     c+ JOIN (SELECT dp4parcel , min(dp4dattim) data FROM tidp400f
003700180302     c+   WHERE dp4SCAN ='05'
003800180302     c+        and dp4parcel not like '0844%' and DP4SERVICE<> 300
003900180302     c+        and CHARACTER_LENGTH(TRIM(DP4DEPCOD))=7
004000180302     c+   GROUP BY dp4parcel) a
004100180206     c+     ON b.dp4parcel= a.dp4parcel and b.dp4dattim= a.data
004200180206     c+     GROUP BY b.dp4parcel ) c ) ) DP4
004300180206     c+  ON pndipn = DP4PARCEL
004400180302     C+ WHERE  DP4SCAN ='05'  and pndisc <> '300'  and pndidp7 <> ' '
004500180302     C+    and pndDGS =0
004600180302     c+    and DP4DEPCOD <> pndidp7
004700180302     c+    and substr(PNDDPE, 1, 2)='DI' )
004800180219     c+   skip locked data
004900180206     C/END-EXEC
005000180206      ***
005100180206      ***-----------------------
005200180206      ***  poi pulisce i campi DEPOT7 e BUNIT
005300180206      ***   SQL seguente aggiorna tutti i records FIPND che non hanno
005400180206      ***    il DEPOT e la B.U con quanto ricevuto sullo SCAN 05
005500180206      ***-----------------------
005600180206     C/EXEC SQL
005700180206     c+  update fipnd00f set pndidp7 =' ', pndbucnp =0
005800180302     c+  where  rrn(fipnd00f) in (SELECT rrn(fipnd00f) FROM
005900180302     c+  fipnd00f JOIN  (select * from tidp400f
006000180302     c+    where rrn(tidp400f) in  (select record from
006100180302     c+    (SELECT b.dp4parcel, min(rrn(b)) record FROM tidp400f b
006200180302     c+ JOIN (SELECT dp4parcel , min(dp4dattim) data FROM tidp400f
006300180302     c+   WHERE dp4SCAN ='05'
006400180302     c+        and dp4parcel not like '0844%' and DP4SERVICE<> 300
006500180302     c+        and CHARACTER_LENGTH(TRIM(DP4DEPCOD))=7
006600180302     c+   GROUP BY dp4parcel) a
006700180302     c+     ON b.dp4parcel= a.dp4parcel and b.dp4dattim= a.data
006800180302     c+     GROUP BY b.dp4parcel ) c ) ) DP4
006900180302     c+  ON pndipn = DP4PARCEL
007000180302     C+ WHERE  DP4SCAN ='05'  and pndisc <> '300'  and pndidp7 <> ' '
007100180302     C+    and pndDGS =0
007200180302     c+    and DP4DEPCOD <> pndidp7
007300180302     c+    and substr(PNDDPE, 1, 2)='DI' )
007400180302     c+   skip locked data
007500180206     C/END-EXEC
007600180206      ***
007700180206      ***-----------------------
007800180206      ***  aggiorna i DEPOT di PARTENZA con gli SCAN 05 (NUOVO TIDP400F)
007900180206      ***-----------------------
008000180305      *-----           *--------------------*
008100180305     C                   GOTO      NONFARE
008200180305      *-----           *--------------------*
008300180306      ***C/EXEC SQL
008400180306      ***C+ update fipnd00f ad SET
008500180306      ***C+   pndidp7 = (select  DP4DEPCOD from tidp400f
008600180306      ***C+              where dp4parcel = ad.PNDIPN and rrn(tidp400f) in
008700180306      ***C+ (select   record from
008800180306      ***C+   (SELECT b.dp4parcel, min(rrn(b)) record FROM tidp400f b JOIN
008900180306      ***C+   (SELECT dp4parcel , min(dp4dattim) data FROM tidp400f
009000180306      ***C+     WHERE dp4SCAN ='05'  and dp4parcel not like '0844%'
009100180306      ***c+       and CHARACTER_LENGTH(TRIM(DP4DEPCOD))=7
009200180306      ***C+           GROUP BY dp4parcel ) a
009300180306      ***C+        ON b.dp4parcel= a.dp4parcel and b.dp4dattim= a.data
009400180306      ***C+  group by b.dp4parcel ) a )        ),
009500180306      ***C+   pndbucnp =(select  substr(DP4DEPCOD, 1, 3) from tidp400f
009600180306      ***C+              where dp4parcel = ad.PNDIPN and rrn(tidp400f) in
009700180306      ***C+ (select   record from
009800180306      ***C+   (SELECT b.dp4parcel, min(rrn(b)) record FROM tidp400f b JOIN
009900180306      ***C+   (SELECT dp4parcel, min(dp4dattim) data FROM tidp400f
010000180306      ***C+     WHERE dp4SCAN ='05'  and dp4parcel not like '0844%'
010100180306      ***c+       and CHARACTER_LENGTH(TRIM(DP4DEPCOD))=7
010200180306      ***C+           GROUP BY dp4parcel ) a
010300180306      ***C+        ON b.dp4parcel= a.dp4parcel and b.dp4dattim= a.data
010400180306      ***C+  group by b.dp4parcel) c )         )
010500180306      ***C+ WHERE pndDGS =0 and pndipn IN
010600180306      ***C+  (select DP4PARCEL from tidp400f where DP4SCAN ='05'
010700180306      ***C+      and DP4PARCEL not like '0844%'
010800180306      ***C+      and DP4SERVICE <> '300'
010900180306      ***C+      and CHARACTER_LENGTH(TRIM(DP4DEPCOD))=7)
011000180306      ***C+      and substr(PNDDPE, 1, 2)='DI'
011100180306      ***C+      and PNDISC <> '300' and pndbucnp=0 and PNDIDP7=' '
011200180306      ***c+   skip locked data
011300180306      ***C/END-EXEC
011400180305      *- *--------------------*
011500180305     c     NONFARE       tag
011600180305      *- *--------------------*
011700180306      ***     RIFATTO con CICLO di SQL pooichè il precedente SQL si piantava
011800180305     C*
011900180305     C/EXEC SQL
012000180305     C+ Declare A1 Cursor for
012100180305     c+  select  dp4parcel, DP4DEPCOD from tidp400f join fipnd00f
012200180305     c+      ON  dp4parcel = pndipn and pndbucnp=0 and PNDIDP7=' '
012300180306     c+          and pnddgs=0
012400180305     c+   where  rrn(tidp400f) in
012500180305     c+ (select record from
012600180305     c+ (SELECT b.dp4parcel, min(rrn(b)) record FROM tidp400f b
012700180305     c+    JOIN
012800180305     c+ (SELECT dp4parcel , min(dp4dattim) data FROM tidp400f
012900180305     c+   WHERE dp4SCAN ='05'  and dp4parcel not like '0844%'
013000180305     c+     and CHARACTER_LENGTH(TRIM(DP4DEPCOD))=7
013100180305     c+     and DP4SERVICE <> '300'
013200180305     c+   GROUP BY dp4parcel ) a
013300180305     c+      ON b.dp4parcel= a.dp4parcel and
013400180305     c+         b.dp4dattim= a.data
013500180305     c+   group by b.dp4parcel ) a )
013600180305     C/END-EXEC
013700180305      *          apertura cursore
013800180305     C/EXEC SQL
013900180305     C+ OPEN A1
014000180305     C/END-EXEC
014100180305     C                   DOU       SqlCod = 100 OR SqlCod < 0
014200180305      *          lettura cursore
014300180305     C/EXEC SQL
014400180305     C+ Fetch Next From A1 Into :parcel14, :depot7
014500180305     C/END-EXEC
014600180305      *
014700180305     C                   SELECT
014800180305     C                   WHEN      SqlCod = 100
014900180306     c                   seton                                        LR
015000180305      *
015100180305     C                   WHEN      SqlCod >= 0
015200180305     C                   Exsr      Update_DEPOT
015300180305      *
015400180305     C                   OTHER
015500180305     C                   EXSR      SqlError
015600180305     C                   ENDSL
015700180305      *
015800180305     C                   ENDDO
015900180305     C/EXEC SQL
016000180305     C+ Close A1
016100180305     C/END-EXEC
016200180305      ***
016300180206     c                   RETURN
016400180305     C*---------------------------------------------------------------*
016500180305     C* Aggiorna il DEPOT e la BUNIT
016600180305     C*---------------------------------------------------------------*
016700180305     C     Update_DEPOT  BEGSR
016800180305      *
016900180305     C/EXEC SQL
017000180305     C+   UPDATE fipnd00f
017100180305     C+      SET pndidp7 = :DEPOT7, pndbucnp = substr(:DEPOT7, 1, 3)
017200180305     C+    WHERE PNDIPN  = :PARCEL14
017300180305     C+      and PNDISC <> '300'
017400180305     C+      and substr(PNDDPE, 1, 2)='DI'
017500180305     C+      and pndbucnp=0 and PNDIDP7=' '
017600180306     C+      and PNDdgs = 0
017700180305     C/END-EXEC
017800180305     C                   if        SQLCOD < 0
017900180305     C                   Exsr      SqlError
018000180306     c                   else
018100180307     c                   time                    datora
018200180306     c                   write     fieupndf
018300180305     C                   END
018400180305      *
018500180305     C                   ENDSR
018600180305     C*---------------------------------------------------------------*
018700180305     C* per emissione errore su SQL
018800180305     C*---------------------------------------------------------------*
018900180305     C     SqlError      BEGSR
019000180305      *
019100180305      * Forzo la stampa del JOBLOG.
019200180305      *   e chiude il pgm forzatamente
019300180305     C                   CALL      'X66CHGJOB'
019400180305     C                   RETURN
019500180305      *
019600180305     C                   ENDSR
019700180305     C*---------------------------------------------------------------*
