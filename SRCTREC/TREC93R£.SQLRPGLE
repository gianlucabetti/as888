000100060125     H DECEDIT('0,') DATEDIT(*DMY.)
000200060125     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000300060125     D sumecmov        s             19  3
000400060125     D sumfatt         s             19  3
000500060125     D diffe           s             19  3
000600060125     dkpjba          e ds
000700060206     D WLBDA8          DS
000800060206     D  G08DAT                 1      8  0
000900060206     D  G08INV                 9     16  0
001000060206     D  G08ERR                17     17
001100060206     D  G08TGI                18     22  0
001200060125
001300030115
001400030115     C     *ENTRY        PLIST
001500051125     C                   PARM                    kpjba
001600060206     C                   TIME                    WHHUDT           14 0
001700060206     C                   MOVEL     WHHUDT        ORA               6 0
001800060206     C                   MOVE      WHHUDT        G08DAT
001900060206     C                   MOVE      *ZEROS        G08INV
002000060206     C                   MOVEL     *BLANK        G08ERR
002100060206     C                   CALL      'XSRDA8'
002200060206     C                   PARM                    WLBDA8
002300060206     C                   Z-ADD     G08DAT        GMAUD8            8 0
002400060206     c*
002500060125     c                   movel     kpjbu         aammce            6 0
002600060125     c                   clear                   perce
002700060125     c                   movel     aammce        perce             8 0
002800060125     c* ricavi c/e
002900030115     C/EXEC SQL
003000060125     C+ SELECT sum(movimp) INTO :sumecmov FROM ecmov00f WHERE MOVVOC in
003100060125     C+ ( SELECT substr(digits(ecdkcc), 2, 3) FROM ececd00f WHERE ecdnbc
003200060125     C+ = (sELECT max(ecdnbc) FROM ececd00f WHERE ECDSEG = 'A' and ECDCMP
003300060125     C+ >=500) and ECDSEG = 'A' and ECDCMP >=500 GROUP BY ecdkcc ) and
003400060125     C+ movcdp = :perce
003500030115     C/END-EXEC
003600060125     c* ricavi fatturazione
003700060125     C/EXEC SQL
003800060125     C+ SELECT VFTVAN INTO :sumfatt FROM wfvftce00f WHERE VFTSVN = '['
003900060125     C+ and VFTAMF =:aammce
004000060125     C/END-EXEC
004100060125     c* differenza
004200060125     c                   eval      diffe = sumecmov - sumfatt
004300060125     c                   except    risultato
004400030115
004500060125     C                   seton                                        lr
004600030115
004700030115
004800060125     OQSYSPRT   E            risultato        02
004900060206     O                                          +40 'Quadratura RICAVI c/e e -
005000060206     O                                              fatturazione periodo'
005100060206     O                       aammce              +1
005200060206     O                       G08dat             115 '  /  /    '
005300060206     O                       ora                 +2 '  :  :  '
005400060125     O                       PAGE          Z    132
005500060125     O          E            risultato        04
005600060125     O                                           +0 'Conto economico'
005700060125     O                       sumecmov      k     +1
005800060125     O                                           +1 'Fatturazione'
005900060125     O                       sumfatt       k     +1
006000060125     O                                           +1 'Differenza'
006100060125     O                       diffe         k     +1
