000100100624     H DEBUG DECEDIT('0,') DATEDIT(*ymd.)
000200000000     H*--------------------------------------------------------------*
000300020624      *
000400100624     D kpjba         e DS
000500100730     D ficn480ds     e DS
000600100730     d tibs55ds      E DS
000700100624     d dtaiso                          d
000800100730     d wrksql          s            500    inz
000900100730     d ist             s             70    dim(6) ctdata
001000020624     D*
001100011026
001200000000     C*---------------------------------------------------------------*
001300020624     c     *entry        plist
001400100624     c                   parm                    kpjba
001500100730     c                   movel     kpjbu         ficn480ds
001600100730     c                   move      f480dti       dta               8
001700100730     c                   eval      %subst(ist(4): 51: 8)= dta
001800100730     C* Richiamo TIBS55R per conoscere librerie/S.Informativi
001900100730     C* da/a cui spedire
002000100730     C                   CLEAR                   TIBS55ds
002100100730     C                   MOVEL     'L'           I50TLA
002200100730     C                   MOVEL     '001'         I50APO
002300100730     C                   CALL      'TIBS55R'
002400100730     C                   PARM                    TIBS55ds
002500100730     c                   eval      %subst(ist(2): 57: 10) = o50ala
002600100730     c                   eval      wrksql = ist(1) +
002700100730     c                                      ist(2) +
002800100730     c                                      ist(3) +
002900100730     c                                      ist(4) +
003000100730     c                                      ist(5) +
003100100730     c                                      ist(6)
003200100623
003300020313     C/EXEC SQL
003400100730     C+ delete from wffidst0f
003500020313     C/END-EXEC
003600100730
003700100730     C/EXEC SQL
003800100730     C+ EXECUTE IMMEDIATE :wrksql
003900100730     C/END-EXEC
004000100623
004100100730     c                   seton                                        lr
004200100730**
004300100730INSERT INTO WFFIDST0F SELECT substr(digits(apdpdr), 1, 3),
004400100730count(*) FROM fiapd00f as b WHERE exists (select * from :o50ala
004500100730/fidst00f as a where a.dstpdr = b.apdpdr and a.dstatb = ' ' and
004600100730(a.DSTNBLA<> 0 or a.DSTRNBLA <> 0) and a.dstdfv = :f480dti ) and
004700100730APDPDD = ' ' and apdatb = ' ' and apdtip='A' GROUP BY
004800100730substr(digits(apdpdr), 1, 3) ORDER BY substr(digits(apdpdr), 1, 3)
