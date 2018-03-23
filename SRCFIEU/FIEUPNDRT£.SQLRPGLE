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
002400171219     c+  where  rrn(fipnd00f) in
002500171219     c+ (select rrn(fipnd00f) from tidp400f join fipnd00f
002600171219     c+      on pndipn = DP4PARCEL
002700171219     C+ WHERE  DP4SCAN ='05'
002800171219     C+    and CHARACTER_LENGTH(TRIM(DP4DEPCOD))=7
002900171219     c+    and DP4DEPCOD <> pndidp7 and pndisc <> '300'
003000171220     c+    and pndidp7 <> ' '
003100171219     c+    and substr(PNDDPE, 1, 2)='DI' and pndDGS =0)
003200171219     C/END-EXEC
003300171219      ***
003400171219      ***-----------------------
003500171219      ***  poi pulisce i campi DEPOT7 e BUNIT
003600171219      ***   SQL seguente aggiorna tutti i records FIPND che non hanno
003700171219      ***    il DEPOT e la B.U con quanto ricevuto sullo SCAN 05
003800171219      ***-----------------------
003900171219     C/EXEC SQL
004000171219     c+  update fipnd00f set
004100171219     c+         pndidp7 = ' ', pndbucnp = 0
004200171219     c+  where  rrn(fipnd00f) in
004300171219     c+ (select rrn(fipnd00f) from tidp400f join fipnd00f
004400171219     c+      on pndipn = DP4PARCEL
004500171219     C+ WHERE  DP4SCAN ='05'
004600171219     C+    and CHARACTER_LENGTH(TRIM(DP4DEPCOD))=7
004700171219     c+    and DP4DEPCOD <> pndidp7 and pndisc <>'300'
004800171220     c+    and pndidp7 <> ' '
004900171219     c+    and substr(PNDDPE, 1, 2)='DI' and pndDGS =0)
005000171219     C/END-EXEC
005100171219      ***
005200171219      ***-----------------------
005300171219      ***  aggiorna i DEPOT di PARTENZA con gli SCAN 05 (NUOVO TIDP400F)
005400171219      ***-----------------------
005500130628     C/EXEC SQL
005600171214     C+ update fipnd00f  set
005700171214     C+   pndidp7  =(select  DP4DEPCOD from tidp400f
005800171214     C+               where DP4PARCEL = pndipn and DP4SCAN ='05'
005900171220     C+                 and CHARACTER_LENGTH(TRIM(DP4DEPCOD))=7
006000171220     c+                 and dp4PARCEL concat digits(dp4DATTIM) in
006100171220     c+             (select dp4PARCEL concat digits(min(dp4DATTIM))
006200171220     c+                from tidp400f where dp4SCAN ='05' and
006300171220     C+                     CHARACTER_LENGTH(TRIM(DP4DEPCOD))=7
006400171220     c+               group by dp4PARCEL) ),
006500171214     C+   pndbucnp =(select  substr(DP4DEPCOD, 1, 3) from tidp400f
006600171214     C+               where DP4PARCEL = pndipn and DP4SCAN ='05'
006700171220     C+                 and CHARACTER_LENGTH(TRIM(DP4DEPCOD))=7
006800171220     c+                 and dp4PARCEL concat digits(dp4DATTIM) in
006900171220     c+             (select dp4PARCEL concat digits(min(dp4DATTIM))
007000171220     c+                from tidp400f where dp4SCAN ='05' and
007100171220     C+                     CHARACTER_LENGTH(TRIM(DP4DEPCOD))=7
007200171220     c+               group by dp4PARCEL) )
007300171219     C+ WHERE  pndipn IN
007400171219     C+  (select DP4PARCEL from tidp400f where DP4SCAN ='05'
007500171219     C+      and CHARACTER_LENGTH(TRIM(DP4DEPCOD))=7)
007600171219     C+   and substr(PNDDPE, 1, 2)='DI'      and pndDGS =0
007700171219     C+   and PNDISC <> '300' and pndbucnp=0 and PNDIDP7=' '
007800130628     C/END-EXEC
007900171219      ***
008000171219     c                   RETURN
008100171219      ***
008200171219      *****************************************************
