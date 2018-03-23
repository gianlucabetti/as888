000100020819     H/TITLE  Trascodifica AZUTE00F x profili in sede
000200020819     H DEBUG DECEDIT('0,') DATEDIT(*ymd.)
000300040218
000400021002     Fazute01l  if a E           K DISK
000500021001     Fazute02l  if   E           K DISK    rename(azute000:azute02)
000600021001     f                                     prefix(£)
000700040218
000800021017     D azute         E DS                  extname(azute00f)
000900951009?     *--------------------------------------------------------------*
001000021001     c                   clear                   azute
001100021002     c     £prfnmu       chain     azute01l
001200040218    1c                   if        not %found(azute01l)
001300021001     c                   eval      utedsc = 20391231
001400021001     c                   eval      uteute = £prfnmu
001500040218     c                   move      PO            utefil
001600021017     c                   eval      utefun = 'POC'
001700021017     c                   eval      uteaut = 'AZ'
001800030911     c* cerco il primo numero libero
001900040218     c                   move      *hival        £utecdu
002000030911     c     kprf          setll     azute02l                               03
002100030911     c   03              exsr      srcdu
002200040218    2c  n02              do
002300040218    3c                   if        not *in03
002400030911     c     utefil        readpe    azute02l
002500040218    4c                   if        %eof(azute02l)
002600030911     c                   z-add     1             utecdu
002700040218   x4c                   else
002800040218    5c                   if        £utecdu <> 999
002900030911     c                   add       1             £utecdu
003000030911     c                   z-add     £utecdu       utecdu
003100040218   x5c                   else
003200030911     c                   exsr      srcdu
003300030911     c   02              leave
003400040218    5c                   endIf
003500040218    4c                   endIf
003600040218    3c                   endIf
003700030911     c                   write     azute000
003800040218    2c                   enddo
003900040218    3c                   endIf
004000040218      *
004100040218     c                   eval      *INLR = *ON
004200030911?     *--------------------------------------------------------------*
004300030911?     *  cerco il numero in azute00f                                 *
004400030911?     *--------------------------------------------------------------*
004500030911     C     srcdu         BEGSR
004600030911     c                   clear                   £utecdu
004700030911     c                   clear                   conta             3 0
004800030911     c     kprf          setll     azute02l
004900030911     c                   do        *hival
005000030911     c     utefil        reade     azute02l
005100030911     c                   if        %eof(azute02l)
005200030911     c                   leave
005300030911     c                   end
005400030911     c*
005500030911     c                   if        conta = 999
005600030911     c                   seton                                        0299
005700030911     c                   leave
005800030911     c                   end
005900030911     c*
006000030911     c                   add       1             conta
006100030911     c                   if        £utecdu = conta
006200030911     c                   iter
006300030911     c                   else
006400030911     c                   z-add     conta         utecdu
006500030911     c                   leave
006600030911     c                   end
006700030911     c                   enddo
006800030911     c*
006900030911     c                   endsr
007000020819?     *--------------------------------------------------------------*
007100020819?     *  *INZSR: Operazioni di inizializzazione dati                 *
007200020819?     *--------------------------------------------------------------*
007300020819     C     *INZSR        BEGSR
007400951011      *
007500951011     C     *ENTRY        PLIST
007600040514     C                   PARM                    po                3
007700040514     C                   PARM                    £PRFnmu          10
007800040218      *
007900021001     c     kprf          klist
008000021010     c                   kfld                    utefil
008100021010     c                   kfld                    £utecdu
008200040218     c*
008300040218     c                   endsr
008400040218?     *--------------------------------------------------------------*
