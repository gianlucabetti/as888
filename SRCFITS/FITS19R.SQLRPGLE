000100040805     h decedit('0,') datedit(*ymd.)
000200021010      *
000300170313      * FITS19R  *----------------------------------------------------*
000400170313      *  FISGN
000500021010      *---------------------------------------------------------------* c
000700170313     ffisgnNEW  o  A e           k disk    rename(fisgn000:fisgnW)
001400021008      *
001500170313     d fisgnds       E DS                  extname(fisgn00f)
001600060201     d KPJBA         E DS
001700040805     D* Schiera p.o. da trasferire
001800040805     D  SKPO           s              3    dim(500)
001900170313     d KLIN            s                   like(sgnLNP)
002000040805      *
002100021008      *****************************************************************
002200021010     c     *ENTRY        PLIST
002300021010     c                   PARM                    KPJBA
002400040805     C                   parm                    PARAM          1500
002500040805     c
002600040805     c     ' ':'0'       xlate     param         param
002700040805     c                   movea     param         skpo
002800040805     C*
003900021010      *---------------------------------------------------------------*
004000021008      ****
004100040910      ** LETTURA
004200021008      ****
004300040913     c                   z-add     1             x                 4 0
004400040913    1c                   dow       skpo(x)>*zeros
004500040913     c                   movel     skpo(X)       klin
004600060201     C/EXEC SQL
004700170313     C+ DECLARE E1 CURSOR FOR SELECT * FROM fisgn00f WHERE sgnlnp = :klin
004800170313     C+ or sgnlna = :klin
005000060201     C/END-EXEC
005100060201
005200060201     C/EXEC SQL
005300060201     C+ OPEN E1
005400060201     C/END-EXEC
005500060201     C                   DO        *hival
005600060201     C/EXEC SQL
005700170313     C+ FETCH NEXT FROM E1 INTO :fisgnds
005800060201     C/END-EXEC
005900060201     C                   SELECT
006000060201     c*
006100060201     C                   WHEN      SqlCod = 100
006200060201     c                   leave
006300060201     C                   WHEN      SqlCod < 0
006400060201     c                   leave
006500060201     c                   other
006600170313     C                   write     fisgnw
006700060201     C                   ENDSL
006800060201      *
006900060201     C                   ENDDO
007000060201     C/EXEC SQL
007100060201     C+ CLOSE E1
007200060201     C/END-EXEC
007300040913     C                   ADD       1             X
007400040913     C                   ENDDO
007500021010      *
007700021011     c                   seton                                        LR
011800021009      *
