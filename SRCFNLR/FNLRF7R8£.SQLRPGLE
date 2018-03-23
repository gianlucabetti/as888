000100110728     H* Fnlrf7R7 *----------------------------------------------------*
000200110728     H* verifico le spedizioni di un perfect order
000300930225     H*---------------------------------------------------------------*
000400110727     d conta           s              1  0
000500110729     d xco             s                   like(£arbxco)
000600110728     d kpjba         e ds
000700110729     d fnarbds£      e ds                  extname(fnarb00f) prefix(£)
000800110728     d fnlrf7r8ds    e ds
000900070130     C**********************************************************************
001000110728     c* 1° bolla con cui sto verificando le sorelle
001100110726     c                   eval      conta = 1
001200110726     c* leggo le sorelle
001300110726     C/EXEC SQL
001400110727     C+ DECLARE B1 CURSOR FOR SELECT fnarb01l.* FROM fnarb01l WHERE
001500110729     C+ arbrma = :dr8rmai and arbxco  <> ' ' and arbnsp <> :dr8nspi
001600110726     C/END-EXEC
001700110726
001800110726     C/EXEC SQL
001900110726     C+ OPEN b1
002000110726     C/END-EXEC
002100110726
002200110726     C                   do        *hival
002300110726
002400110726     C/EXEC SQL
002500110727     C+ FETCH NEXT FROM b1 INTO :fnarbds£
002600110726     C/END-EXEC
002700110726
002800110726     C                   select
002900110726     C                   when      sqlcod = 100
003000110726     c                   leave
003100110726     C                   WHEN      SqlCod < 0
003200110726     c                   if        sqlcod <> -913
003300110726     C                   seton                                        H1
003400110726     c                   end
003500110728     c                   seton                                        lr
003600110728     c                   return
003700110726     C                   other
003800110726     c* conteggio sorelle per controllare XCO
003900110726     c                   add       1             conta
004000110905     c                   if        DR8lnai= £arblna and £arbfbc <> 'G'
004100110905     c*                            and £arbfdn = ' '
004200110728     C                   if        £arbndc = 0
004300110729     c                   eval      dr8erro= '007'
004400110802     c                   leave
004500110728     c                   else
004600110729     C                   if        £arbndc <> dr8ndci
004700110729     c                   eval      dr8erro= '008'
004800110802     c                   leave
004900110727     c                   end
005000110727     c                   end
005100110728     C                   end
005200110728     C                   endsl
005300110726     C*
005400110726     C                   enddo
005500110726
005600110726     C/EXEC SQL
005700110726     C+ CLOSE b1
005800110726     C/END-EXEC
005900110727     c* verifico se ho già tutte le spedizioni
006000110727     c                   move      conta         xco
006100110905     c                   if        xco <> dr8xcoi
006200110729     c                   eval      dr8erro= '009'
006300110727     c                   end
006400110727     c*
006500110728     c                   eval      kpjbu = fnlrf7r8ds
006600070131     c                   seton                                        lr
006700070131     C**********************************************************************
006800070131     c     *inzsr        begsr
006900070131     C**********************************************************************
007000070131     c*
007100061002     C     *ENTRY        PLIST
007200070130     C                   PARM                    kpjba
007300110728     c                   movel     kpjbu         fnlrf7r8ds
007400110729     c                   clear                   dr8erro
007500110728     c*
007600110728     C     Karb          KLIST
007700110728     C                   KFLD                    dr8aasi
007800110728     C                   KFLD                    dr8lnpi
007900110728     C                   KFLD                    dr8nrsi
008000110728     C                   KFLD                    dr8nspi
008100070130     c*
008200070130     c                   endsr
