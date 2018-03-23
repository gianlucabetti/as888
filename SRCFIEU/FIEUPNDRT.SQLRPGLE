000100010514     H DECEDIT('0,') DATEDIT(*DMY.)
000200010514      *----------------------------------------------------*
000300171214      * Allinea B.UNIT e DEPOT di PARTENZA su FIPND00F
000400010514      *----------------------------------------------------*
000500010514     d kpjba         e ds
000600010514      *-------------------------------------------------------------------------*
000700010514     c     *entry        plist
000800010514     c                   parm                    kpjba
000900130628     c                   eval      *inlr = *on
001000171219      ***
001100171219      *****************************************************
001200171219      ***  QUANDO AVREMO il TTEVENTS gli SCAN 05
001300171219      *****************************************************
001400171219      ***
001500171219      ***-----------------------
001600171219      ***  Se il DEPOT di PARTENZA presente sul FIPND è diverso
001700171219      ***   da quello dello SCAN 05 (TIDP400F)
001800171219      ***   prima sposta il presente depot del campo lungo 7
001900171219      ***    alla DS alla posizione 7 (per non dimenticarsi cosa c'era prima)
002000171219      ***-----------------------
002100171219     C/EXEC SQL
002200171219     c+  update fipnd00f set
002300171219     c+         pndetiu = substr(pndetiu, 1, 6) concat pndidp7
002400180206     c+  where  rrn(fipnd00f) in (SELECT rrn(fipnd00f) FROM
002500180206     c+  fipnd00f JOIN  (select * from tidp400f
002700180206     c+    where rrn(tidp400f) in  (select record from
002900180206     c+    (SELECT b.dp4parcel, min(rrn(b)) record FROM tidp400f b
003100180206     c+ JOIN (SELECT dp4parcel , min(dp4dattim) data FROM tidp400f
003200180206     c+   WHERE dp4SCAN ='05' and CHARACTER_LENGTH(TRIM(DP4DEPCOD))=7
003201180206     c+        and dp4parcel not like '0844%' GROUP BY dp4parcel) a
003400171221     c+     ON b.dp4parcel= a.dp4parcel and b.dp4dattim= a.data
003500171221     c+     GROUP BY b.dp4parcel ) c ) ) DP4
003600171221     c+  ON pndipn = DP4PARCEL
003700180206     C+ WHERE  DP4SCAN ='05' and pndidp7 <> ' '  and pndisc <> '300'
003800171219     C+    and CHARACTER_LENGTH(TRIM(DP4DEPCOD))=7
003900180206     c+    and DP4DEPCOD <> pndidp7  and dp4parcel not like '0844%'
004100171219     c+    and substr(PNDDPE, 1, 2)='DI' and pndDGS =0)
004200171219     C/END-EXEC
004300171219      ***
004400171219      ***-----------------------
004500171219      ***  poi pulisce i campi DEPOT7 e BUNIT
004600171219      ***   SQL seguente aggiorna tutti i records FIPND che non hanno
004700171219      ***    il DEPOT e la B.U con quanto ricevuto sullo SCAN 05
004800171219      ***-----------------------
004900171219     C/EXEC SQL
005000171221     c+  update fipnd00f set pndidp7 =' ', pndbucnp =0
005001180206     c+  where  rrn(fipnd00f) in (SELECT rrn(fipnd00f) FROM
005002180206     c+  fipnd00f JOIN  (select * from tidp400f
005003180206     c+    where rrn(tidp400f) in  (select record from
005004180206     c+    (SELECT b.dp4parcel, min(rrn(b)) record FROM tidp400f b
005005180206     c+ JOIN (SELECT dp4parcel , min(dp4dattim) data FROM tidp400f
005006180206     c+   WHERE dp4SCAN ='05' and CHARACTER_LENGTH(TRIM(DP4DEPCOD))=7
005007180206     c+        and dp4parcel not like '0844%' GROUP BY dp4parcel) a
005008180206     c+     ON b.dp4parcel= a.dp4parcel and b.dp4dattim= a.data
005009180206     c+     GROUP BY b.dp4parcel ) c ) ) DP4
005010180206     c+  ON pndipn = DP4PARCEL
005011180206     C+ WHERE  DP4SCAN ='05' and pndidp7 <> ' '  and pndisc <> '300'
005012180206     C+    and CHARACTER_LENGTH(TRIM(DP4DEPCOD))=7
005013180206     c+    and DP4DEPCOD <> pndidp7  and dp4parcel not like '0844%'
005014180206     c+    and substr(PNDDPE, 1, 2)='DI' and pndDGS =0)
005015180206     C/END-EXEC
007000171219      ***
007100171219      ***-----------------------
007200171219      ***  aggiorna i DEPOT di PARTENZA con gli SCAN 05 (NUOVO TIDP400F)
007300171219      ***-----------------------
007400130628     C/EXEC SQL
007500180206     C+ update fipnd00f ad SET
007600171220     C+   pndidp7 =(select  DP4DEPCOD from tidp400f
007700180206     C+              where dp4parcel = ad.PNDIPN and rrn(tidp400f) in
007800171221     C+ (select record from
007900171221     C+   (SELECT b.dp4parcel, min(rrn(b)) record FROM tidp400f b JOIN
008000171221     C+   (SELECT dp4parcel , min(dp4dattim) data FROM tidp400f
008100171221     C+     WHERE dp4SCAN ='05' and CHARACTER_LENGTH(TRIM(DP4DEPCOD))=7
008200180206     C+           and dp4parcel not like '0844%' GROUP BY dp4parcel ) a
008300171221     C+        ON b.dp4parcel= a.dp4parcel and b.dp4dattim= a.data
008400180206     C+  group by  b.dp4parcel ) a )        ),
008600171214     C+   pndbucnp =(select  substr(DP4DEPCOD, 1, 3) from tidp400f
008700180206     C+              where dp4parcel = ad.PNDIPN and rrn(tidp400f) in
008800171221     C+ (select record from
008900171221     C+   (SELECT b.dp4parcel, min(rrn(b)) record FROM tidp400f b JOIN
009000171221     C+   (SELECT dp4parcel, min(dp4dattim) data FROM tidp400f
009100171221     C+     WHERE dp4SCAN ='05' and CHARACTER_LENGTH(TRIM(DP4DEPCOD))=7
009101180206     C+           and dp4parcel not like '0844%' GROUP BY dp4parcel ) a
009300171221     C+        ON b.dp4parcel= a.dp4parcel and b.dp4dattim= a.data
009400180206     C+  group by  b.dp4parcel) c )         )
009600171219     C+ WHERE  pndipn IN
009700171219     C+  (select DP4PARCEL from tidp400f where DP4SCAN ='05'
009701180206     C+      and DP4PARCEL not like '0844%'
009800180206     C+      and CHARACTER_LENGTH(TRIM(DP4DEPCOD))=7)
009901180206     C+      and substr(PNDDPE, 1, 2)='DI'  and pndDGS =0
010000180206     C+      and PNDISC <> '300' and pndbucnp=0 and PNDIDP7=' '
010100130628     C/END-EXEC
010200171219      ***
010300171219     c                   RETURN
010400171219      ***
010500171219      *****************************************************
