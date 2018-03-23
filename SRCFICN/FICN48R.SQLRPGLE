000100100624     H DEBUG DECEDIT('0,') DATEDIT(*ymd.)
000200000000     H*--------------------------------------------------------------*
000300020624      *
000400100624     D kpjba         e DS
000500100624     D param           DS
000600020624     D  datadal                       8s 0
000700020624     D  dataal                        8s 0
000800100624     d dtaiso                          d
000900020624     D*
001000011026
001100000000     C*---------------------------------------------------------------*
001200020624     c     *entry        plist
001300100624     c                   parm                    kpjba
001400100624     c* se non passate date le calcolo
001500100624     c                   if        %subst(kpjbu: 1: 1) = ' '
001600100624     c                   clear                   datadal
001700100624     c                   clear                   dataal
001800100624     c                   movel     *date         dtaiso
001900100624     c                   movel     dtaiso        dtacom            8 0
002000100624     c                   move      01            dtacom
002100100624     c                   movel     dtacom        dtaiso
002200100624     c     dtaiso        subdur    1:*d          dtaiso
002300100624     c                   movel     dtaiso        dataal
002400100624     c                   movel     dataal        datadal
002500100624     c                   move      01            datadal
002600100624     c                   else
002700100624     c                   movel     kpjbu         param
002800100624     c                   end
002900100623
003000100623     c* file a dettaglio
003100100623
003200020313     C/EXEC SQL
003300100628     C+ delete from wffifre0f
003400020313     C/END-EXEC
003500100623
003600100623     C/EXEC SQL
003700100628     C+ INSERT INTO WFFIFRE0F SELECT substr(digits(FREPDR), 1,
003800100623     C+ 3), FREPDR, apdrsf, FREDDC, FRETSR, FRECRE, substr(TBEUNI, 1,
003900100623     C+ 30), FRETIM, FREPNO, FRENOT FROM fifre00f join fiapd00f on fretip
004000100623     C+ = apdtip and frepdr = apdpdr join tntbe00f on tbecod = 'CRE' and
004100100623     C+ tbeke1 = frecre WHERE FRETIP = 'A' and FREDDC between :datadal
004200100623     C+ and :dataal  ORDER BY FREPDR, FREDDC, FRECRE, FREPNO
004300100623     C/END-EXEC
004400100623
004500100623     c* file a totale di filiale
004600100623
004700100623     C/EXEC SQL
004800100628     C+ delete from wffifre1f
004900100623     C/END-EXEC
005000100623
005100100623     C/EXEC SQL
005200100628     C+ INSERT INTO WFFIFRE1F
005300100624     C+ SELECT fre0fgs, orgcar, SUM(A) , SUM(B) from (SELECT fre0fgs, CASE
005400100624     C+ WHEN fre0tim > 0 THEN fre0tim ELSE 0 END AS A ,
005500100624     C+ CASE WHEN fre0tim < 0
005600100628     C+ THEN fre0tim ELSE 0 END AS B FROM wffifre0f ) AS TABELLA
005700100624     C+ join azorg00f on fre0fgs = digits(orgfil) GROUP BY fre0fgs, orgcar
005800100624     C+ order by fre0fgs
005900100623     C/END-EXEC
006000020624     c                   seton                                        lr
006100011129     C*--------------------------------------------------------------------
