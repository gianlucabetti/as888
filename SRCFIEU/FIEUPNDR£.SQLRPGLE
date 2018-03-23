000100010514     H DECEDIT('0,') DATEDIT(*DMY.)
000200010514      *----------------------------------------------------*
000300171214      * Allinea B.UNIT e DEPOT di PARTENZA su FIPND00F
000400010514      *----------------------------------------------------*
000500010514     d kpjba         e ds
000501180305     D parcel14        s             14
000502180305     D depot7          s              7
000503180305
000600010514      *-------------------------------------------------------------------------*
000700010514     c     *entry        plist
000800010514     c                   parm                    kpjba
000900171221      ***
001000130628     c                   eval      *inlr = *on
001001180305
001002180305     c                   goto      AL_TIDP4
001003180305
001004180305      ** SALTA il TIDP2 va al nuovo (da cancellare in seguito)
001005180305
001100171219      ***
001200171221      ***============================================================================
001300171221      ***  Negli SQL si è andati a ricercare il primo SCAN ricevuto temporalmente
001400171221      ***  e qualora fosse già stato decodificato un DEPOT, a parità di SCAN x lo
001500171221      ***  stesso Parcel, viene memorizzato sulla DS il DEPOT che era stato preso
001600171221      ***  in precedenza. Quindi viene impostato il DEPOT dello SCAN più vecchio.
001700171221      **  PER poter fare questo, occorre prendere, a parità di SCAN PARCEL il record
001800171221      **      temporalemente più vecchio e con il num.rel.RECORD agganciare
001900171221      **      il record per poter avere il DEPOT di 7 da aggiornare sul FIPND dove
002000171221      **      fosse differente o non ci fosse.
002100171221      ***============================================================================
002200171221      ***
002300171219      ***-----------------------
002400171219      ***  Se il DEPOT di PARTENZA presente sul FIPND è diverso
002500171219      ***   da quello dello SCAN 05 (VECCHIO TIDP200F)
002600171219      ***   prima sposta il presente depot nel campo lungo 7
002700171219      ***    alla DS alla posizione 7 (per non dimenticarsi cosa c'era prima)
002800171219      ***-----------------------
002900171219     C/EXEC SQL
003000171219     c+  update fipnd00f set
003100171219     c+      pndetiu = substr(pndetiu, 1, 6) concat pndidp7
003200171219     c+  where  rrn(fipnd00f) in
003300171221     c+ (SELECT rrn(fipnd00f) FROM fipnd00f
003400171221     c+  JOIN
003500171221     c+   (select * from tidp200f where rrn(tidp200f) in
003600171221     c+    (select record from
003700171221     c+      (SELECT b.i2parcel, min(rrn(b)) record FROM tidp200f b
003800171221     c+     JOIN
003900171221     c+      (SELECT i2parcel , min(i2dattim) data FROM tidp200f
004000171221     c+       WHERE CHARACTER_LENGTH(TRIM(substr(I2DEPNAME, 1, 7)))=7
004100171221     c+       GROUP BY i2parcel) a
004200171221     c+     ON b.i2parcel= a.i2parcel and b.i2dattim= a.data
004300171221     c+     GROUP BY b.i2parcel ) c ) ) DP4
004400171221     c+  ON pndipn = i2PARCEL
004500171221     C+ WHERE pndidp7 <> ' ' and substr(PNDDPE, 1, 2)='DI' and pndDGS =0
004600171221     C+   and CHARACTER_LENGTH(TRIM(substr(I2DEPNAME, 1, 7)))=7
004700171221     c+   and substr(I2DEPNAME, 1, 7) <> pndidp7 and pndisc <> '300')
004800180219     c+   skip locked data
004900171219     C/END-EXEC
005000171219      ***
005100171219      ***-----------------------
005200171219      ***  poi pulisce i campi DEPOT7 e BUNIT
005300171219      ***   SQL seguente aggiorna tutti i records FIPND che non hanno
005400171219      ***    il DEPOT e la B.U con quanto ricevuto sullo SCAN 05
005500171219      ***-----------------------
005600171219     C/EXEC SQL
005700171219     c+  update fipnd00f set
005800171219     c+      pndidp7 = ' ', pndbucnp = 0
005900171221     c+  where  rrn(fipnd00f) in
006000171221     c+ (SELECT rrn(fipnd00f) FROM fipnd00f
006100171221     c+  JOIN
006200171221     c+   (select * from tidp200f where rrn(tidp200f) in
006300171221     c+    (select record from
006400171221     c+      (SELECT b.i2parcel, min(rrn(b)) record FROM tidp200f b
006500171221     c+     JOIN
006600171221     c+      (SELECT i2parcel , min(i2dattim) data FROM tidp200f
006700171221     c+       WHERE CHARACTER_LENGTH(TRIM(substr(I2DEPNAME, 1, 7)))=7
006800171221     c+       GROUP BY i2parcel) a
006900171221     c+     ON b.i2parcel= a.i2parcel and b.i2dattim= a.data
007000171221     c+     GROUP BY b.i2parcel ) c ) ) DP4
007100171221     c+  ON pndipn = i2PARCEL
007200171221     C+ WHERE pndidp7 <> ' ' and substr(PNDDPE, 1, 2)='DI' and pndDGS =0
007300171221     C+   and CHARACTER_LENGTH(TRIM(substr(I2DEPNAME, 1, 7)))=7
007400171221     c+   and substr(I2DEPNAME, 1, 7) <> pndidp7 and pndisc <> '300')
007500180219     c+   skip locked data
007600171219     C/END-EXEC
007700171219      ***
007800171219      ***-----------------------
007900171219      ***  aggiorna i DEPOT di PARTENZA con SCAN 05 (VECCHIO TIDP200F)
008000171219      ***-----------------------
008100171219     C/EXEC SQL
008200180206     c+  update fipnd00f ad SET
008300171221     c+    pndidp7 =(select  substr(I2DEPNAME, 1, 7) from tidp200f
008400180206     c+               where I2PARCEL = ad.pndipn  and rrn(tidp200f) in
008500171221     c+  (select record from
008600171221     c+    (SELECT b.i2parcel, min(rrn(b)) record FROM tidp200f b JOIN
008700171221     c+    (SELECT i2parcel ,  min(i2dattim) data FROM tidp200f
008800171221     c+      WHERE CHARACTER_LENGTH(TRIM(substr(I2DEPNAME, 1, 7)))=7
008900171221     c+      GROUP BY i2parcel ) a
009000171221     c+         ON b.i2parcel= a.i2parcel and b.i2dattim= a.data
009100171221     c+    group by  b.i2parcel ) a )
009200171221     c+              ),
009300171221     c+   pndbucnp =(select  substr(I2DEPNAME, 1, 3) from tidp200f
009400180206     c+               where I2PARCEL = ad.pndipn  and rrn(tidp200f) in
009500171221     c+  (select record from
009600171221     c+    (SELECT b.i2parcel, min(rrn(b)) record FROM tidp200f b JOIN
009700171221     c+    (SELECT i2parcel ,  min(i2dattim) data FROM tidp200f
009800171221     c+      WHERE CHARACTER_LENGTH(TRIM(substr(I2DEPNAME, 1, 7)))=7
009900171221     c+      GROUP BY i2parcel ) a
010000171221     c+         ON b.i2parcel= a.i2parcel and b.i2dattim= a.data
010100171221     c+    group by  b.i2parcel ) a )
010200171221     c+              )
010300171221     c+   WHERE
010400171221     c+    pndipn in (select I2PARCEL from tidp200f where
010500171221     c+              CHARACTER_LENGTH(TRIM(substr(I2DEPNAME, 1, 7)))=7)
010600171221     c+          and substr(PNDDPE, 1, 2)='DI' and pndDGS =0
010700171221     c+          and pndisc <> '300' and pndbucnp=0 and PNDIDP7 =' '
010800180219     c+   skip locked data
010900171219     C/END-EXEC
011000171219      ***
011100171219      ***-----------------------
011200171219      ***    al momento finisce qua ed esce dal PGM
011300171219      ***    FINCHE'
011400171219      ***       DPD non invierà sul TTEVENTS lo scan 05
011500171219      ***-----------------------
011600171219      ***
011700171219     c                   RETURN
011800171219      ***
011900171219      *****************************************************
012000180206      ***************************************************** ******************
012100180206      ***************************************************** ******************
012200180206      *****************************************************
012201180305
012202180305     c     AL_TIDP4      TAG
012300180305      *
012400180206      ***-----------------------
012500180206      ***  Se il DEPOT di PARTENZA presente sul FIPND è diverso
012600180206      ***   da quello dello SCAN 05 (TIDP400F)
012700180206      ***   prima sposta il presente depot del campo lungo 7
012800180206      ***    alla DS alla posizione 7 (per non dimenticarsi cosa c'era prima)
012900180206      ***-----------------------
013000180206     C/EXEC SQL
013100180206     c+  update fipnd00f set
013200180206     c+         pndetiu = substr(pndetiu, 1, 6) concat pndidp7
013300180206     c+  where  rrn(fipnd00f) in (SELECT rrn(fipnd00f) FROM
013400180206     c+  fipnd00f JOIN  (select * from tidp400f
013500180206     c+    where rrn(tidp400f) in  (select record from
013600180206     c+    (SELECT b.dp4parcel, min(rrn(b)) record FROM tidp400f b
013700180206     c+ JOIN (SELECT dp4parcel , min(dp4dattim) data FROM tidp400f
013800180302     c+   WHERE dp4SCAN ='05'
013900180302     c+        and dp4parcel not like '0844%' and DP4SERVICE<> 300
013901180302     c+        and CHARACTER_LENGTH(TRIM(DP4DEPCOD))=7
013902180302     c+   GROUP BY dp4parcel) a
014000180206     c+     ON b.dp4parcel= a.dp4parcel and b.dp4dattim= a.data
014100180206     c+     GROUP BY b.dp4parcel ) c ) ) DP4
014200180206     c+  ON pndipn = DP4PARCEL
014300180302     C+ WHERE  DP4SCAN ='05'  and pndisc <> '300'  and pndidp7 <> ' '
014301180302     C+    and pndDGS =0
014500180302     c+    and DP4DEPCOD <> pndidp7
014600180302     c+    and substr(PNDDPE, 1, 2)='DI' )
014700180219     c+   skip locked data
014800180206     C/END-EXEC
014900180206      ***
015000180206      ***-----------------------
015100180206      ***  poi pulisce i campi DEPOT7 e BUNIT
015200180206      ***   SQL seguente aggiorna tutti i records FIPND che non hanno
015300180206      ***    il DEPOT e la B.U con quanto ricevuto sullo SCAN 05
015400180206      ***-----------------------
015500180206     C/EXEC SQL
015600180206     c+  update fipnd00f set pndidp7 =' ', pndbucnp =0
015603180302     c+  where  rrn(fipnd00f) in (SELECT rrn(fipnd00f) FROM
015604180302     c+  fipnd00f JOIN  (select * from tidp400f
015605180302     c+    where rrn(tidp400f) in  (select record from
015606180302     c+    (SELECT b.dp4parcel, min(rrn(b)) record FROM tidp400f b
015607180302     c+ JOIN (SELECT dp4parcel , min(dp4dattim) data FROM tidp400f
015608180302     c+   WHERE dp4SCAN ='05'
015609180302     c+        and dp4parcel not like '0844%' and DP4SERVICE<> 300
015610180302     c+        and CHARACTER_LENGTH(TRIM(DP4DEPCOD))=7
015611180302     c+   GROUP BY dp4parcel) a
015612180302     c+     ON b.dp4parcel= a.dp4parcel and b.dp4dattim= a.data
015613180302     c+     GROUP BY b.dp4parcel ) c ) ) DP4
015614180302     c+  ON pndipn = DP4PARCEL
015615180302     C+ WHERE  DP4SCAN ='05'  and pndisc <> '300'  and pndidp7 <> ' '
015616180302     C+    and pndDGS =0
015617180302     c+    and DP4DEPCOD <> pndidp7
015618180302     c+    and substr(PNDDPE, 1, 2)='DI' )
015619180302     c+   skip locked data
017200180206     C/END-EXEC
017300180206      ***
017400180206      ***-----------------------
017500180206      ***  aggiorna i DEPOT di PARTENZA con gli SCAN 05 (NUOVO TIDP400F)
017600180206      ***-----------------------
017601180305      *-----           *--------------------*
017602180305     C                   GOTO      NONFARE
017603180305      *-----           *--------------------*
017700180206     C/EXEC SQL
017800180206     C+ update fipnd00f ad SET
017900180302     C+   pndidp7 = (select  DP4DEPCOD from tidp400f
018000180206     C+              where dp4parcel = ad.PNDIPN and rrn(tidp400f) in
018100180302     C+ (select   record from
018200180206     C+   (SELECT b.dp4parcel, min(rrn(b)) record FROM tidp400f b JOIN
018300180206     C+   (SELECT dp4parcel , min(dp4dattim) data FROM tidp400f
018400180302     C+     WHERE dp4SCAN ='05'  and dp4parcel not like '0844%'
018401180302     c+       and CHARACTER_LENGTH(TRIM(DP4DEPCOD))=7
018500180302     C+           GROUP BY dp4parcel ) a
018600180206     C+        ON b.dp4parcel= a.dp4parcel and b.dp4dattim= a.data
018700180302     C+  group by b.dp4parcel ) a )        ),
018800180206     C+   pndbucnp =(select  substr(DP4DEPCOD, 1, 3) from tidp400f
018900180206     C+              where dp4parcel = ad.PNDIPN and rrn(tidp400f) in
019000180302     C+ (select   record from
019100180206     C+   (SELECT b.dp4parcel, min(rrn(b)) record FROM tidp400f b JOIN
019200180206     C+   (SELECT dp4parcel, min(dp4dattim) data FROM tidp400f
019300180302     C+     WHERE dp4SCAN ='05'  and dp4parcel not like '0844%'
019301180302     c+       and CHARACTER_LENGTH(TRIM(DP4DEPCOD))=7
019400180302     C+           GROUP BY dp4parcel ) a
019500180206     C+        ON b.dp4parcel= a.dp4parcel and b.dp4dattim= a.data
019600180302     C+  group by b.dp4parcel) c )         )
019700180305     C+ WHERE pndDGS =0 and pndipn IN
019800180206     C+  (select DP4PARCEL from tidp400f where DP4SCAN ='05'
019900180206     C+      and DP4PARCEL not like '0844%'
019901180305     C+      and DP4SERVICE <> '300'
020000180206     C+      and CHARACTER_LENGTH(TRIM(DP4DEPCOD))=7)
020100180305     C+      and substr(PNDDPE, 1, 2)='DI'
020200180206     C+      and PNDISC <> '300' and pndbucnp=0 and PNDIDP7=' '
020300180219     c+   skip locked data
020400180206     C/END-EXEC
020401180305      *- *--------------------*
020402180305     c     NONFARE       tag
020403180305      *- *--------------------*
020500180206      ***
020501180305     C*
020502180305     C/EXEC SQL
020503180305     C+ Declare A1 Cursor for
020504180305     c+  select  dp4parcel, DP4DEPCOD from tidp400f join fipnd00f
020508180305     c+      ON  dp4parcel = pndipn and pndbucnp=0 and PNDIDP7=' '
020509180305     c+   where  rrn(tidp400f) in
020510180305     c+ (select record from
020511180305     c+ (SELECT b.dp4parcel, min(rrn(b)) record FROM tidp400f b
020512180305     c+    JOIN
020513180305     c+ (SELECT dp4parcel , min(dp4dattim) data FROM tidp400f
020514180305     c+   WHERE dp4SCAN ='05'  and dp4parcel not like '0844%'
020515180305     c+     and CHARACTER_LENGTH(TRIM(DP4DEPCOD))=7
020516180305     c+     and DP4SERVICE <> '300'
020517180305     c+   GROUP BY dp4parcel ) a
020518180305     c+      ON b.dp4parcel= a.dp4parcel and
020519180305     c+         b.dp4dattim= a.data
020520180305     c+   group by b.dp4parcel ) a )
020527180305     C/END-EXEC
020528180305      *          apertura cursore
020529180305     C/EXEC SQL
020530180305     C+ OPEN A1
020531180305     C/END-EXEC
020532180305     C                   DOU       SqlCod = 100 OR SqlCod < 0
020533180305      *          lettura cursore
020534180305     C/EXEC SQL
020535180305     C+ Fetch Next From A1 Into :parcel14, :depot7
020536180305     C/END-EXEC
020537180305      *
020538180305     C                   SELECT
020539180305     C                   WHEN      SqlCod = 100
020540180305      *
020541180305     C                   WHEN      SqlCod >= 0
020542180305     C                   Exsr      Update_DEPOT
020543180305      *
020544180305     C                   OTHER
020545180305     C                   EXSR      SqlError
020546180305     C                   ENDSL
020547180305      *
020548180305     C                   ENDDO
020549180305     C/EXEC SQL
020550180305     C+ Close A1
020551180305     C/END-EXEC
020554180305      ***
020600180206     c                   RETURN
020801180305     C*---------------------------------------------------------------*
020802180305     C* Aggiorna il DEPOT e la BUNIT
020803180305     C*---------------------------------------------------------------*
020804180305     C     Update_DEPOT  BEGSR
020805180305      *
020806180305     C/EXEC SQL
020807180305     C+   UPDATE fipnd00f
020808180305     C+      SET pndidp7 = :DEPOT7, pndbucnp = substr(:DEPOT7, 1, 3)
020809180305     C+    WHERE PNDIPN  = :PARCEL14
020810180305     C+      and PNDISC <> '300'
020811180305     C+      and substr(PNDDPE, 1, 2)='DI'
020812180305     C+      and pndbucnp=0 and PNDIDP7=' '
020819180305     C/END-EXEC
020820180305     C                   if        SQLCOD < 0
020821180305     C                   Exsr      SqlError
020822180305     C                   END
020823180305      *
020824180305     C                   ENDSR
020825180305     C*---------------------------------------------------------------*
020826180305     C* per emissione errore su SQL
020827180305     C*---------------------------------------------------------------*
020828180305     C     SqlError      BEGSR
020829180305      *
020830180305      * Forzo la stampa del JOBLOG.
020831180305      *   e chiude il pgm forzatamente
020832180305     C                   CALL      'X66CHGJOB'
020833180305     C                   RETURN
020834180305      *
020835180305     C                   ENDSR
020836180305     C*---------------------------------------------------------------*
