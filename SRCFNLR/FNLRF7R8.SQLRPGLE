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
001200130221      *** Wurth adesso manda nel campo non solo numeri quindi per identificare
001300130221      ***  il Perfect Order si deve sostituire il test da diverso da *blank a
001400130221      ***  maggiore di '0'.
001500110726     c* leggo le sorelle
001600130221     ********C/EXEC SQL
001700130221     ********C+ DECLARE B1 CURSOR FOR SELECT fnarb01l.* FROM fnarb01l WHERE
001800130221     ********C+ arbrma = :dr8rmai and arbxco  <> ' ' and arbnsp <> :dr8nspi
001900130221     ********C/END-EXEC
002000110726
002100130221     C/EXEC SQL
002200130221     C+ DECLARE B1 CURSOR FOR SELECT fnarb01l.* FROM fnarb01l WHERE
002300130221     C+ arbrma = :dr8rmai and arbxco  > '0' and arbnsp <> :dr8nspi
002400130221     C/END-EXEC
002500130221
002600110726     C/EXEC SQL
002700110726     C+ OPEN b1
002800110726     C/END-EXEC
002900110726
003000110726     C                   do        *hival
003100110726
003200110726     C/EXEC SQL
003300110727     C+ FETCH NEXT FROM b1 INTO :fnarbds£
003400110726     C/END-EXEC
003500110726
003600110726     C                   select
003700110726     C                   when      sqlcod = 100
003800110726     c                   leave
003900110726     C                   WHEN      SqlCod < 0
004000110726     c                   if        sqlcod <> -913
004100110726     C                   seton                                        H1
004200110726     c                   end
004300110728     c                   seton                                        lr
004400110728     c                   return
004500110726     C                   other
004600110726     c* conteggio sorelle per controllare XCO
004700110726     c                   add       1             conta
004800110905     c                   if        DR8lnai= £arblna and £arbfbc <> 'G'
004900110905     c*                            and £arbfdn = ' '
005000110728     C                   if        £arbndc = 0
005100110729     c                   eval      dr8erro= '007'
005200110802     c                   leave
005300110728     c                   else
005400110729     C                   if        £arbndc <> dr8ndci
005500110729     c                   eval      dr8erro= '008'
005600110802     c                   leave
005700110727     c                   end
005800110727     c                   end
005900110728     C                   end
006000110728     C                   endsl
006100110726     C*
006200110726     C                   enddo
006300110726
006400110726     C/EXEC SQL
006500110726     C+ CLOSE b1
006600110726     C/END-EXEC
006700110727     c* verifico se ho già tutte le spedizioni
006800110727     c                   move      conta         xco
006900130221     c                   if        xco <> dr8xcoi and dr8xcoi > '0'
007000110729     c                   eval      dr8erro= '009'
007100110727     c                   end
007200110727     c*
007300110728     c                   eval      kpjbu = fnlrf7r8ds
007400070131     c                   seton                                        lr
007500070131     C**********************************************************************
007600070131     c     *inzsr        begsr
007700070131     C**********************************************************************
007800070131     c*
007900061002     C     *ENTRY        PLIST
008000070130     C                   PARM                    kpjba
008100110728     c                   movel     kpjbu         fnlrf7r8ds
008200110729     c                   clear                   dr8erro
008300110728     c*
008400110728     C     Karb          KLIST
008500110728     C                   KFLD                    dr8aasi
008600110728     C                   KFLD                    dr8lnpi
008700110728     C                   KFLD                    dr8nrsi
008800110728     C                   KFLD                    dr8nspi
008900070130     c*
009000070130     c                   endsr
