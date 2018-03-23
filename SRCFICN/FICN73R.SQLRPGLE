000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200020409     h*****************************************************************
000300030828     h* Il pgm pulisce tutti gli archivi dei conteggi aff/defl
000400020409     h*****************************************************************
000500030828     ffiatt07l  uf   e           k disk
000600020826     ftntbe01l  uf   e           k disk
000700000607     D kpjba         e ds
000800030828     D dbld          e ds
000900020702     d pardt           s              8  0
001000040217     d pardc           s              8  0
001100040217     d dtaiso          s               d
001200030828     d dtf             s                   like(attdft) inz
001300040217     c* pulisco i record del fiatt00f generati x i conteggi aff/def
001400040217     c* con data prestaz. <= dta fattura richiesta - 1
001500040217     c*
001600040217     C/EXEC SQL
001700040217     C+ DELETE FROM FIatt00f WHERE attddc <= :pardc
001800040217     C/END-EXEC
001900040217     c*
002000130527     c*
002100130527     C/EXEC SQL
002200130527     C+ DELETE FROM FIatt10f WHERE at1ddc <= :pardc
002300130527     C/END-EXEC
002400070607     c*
002500030213     c* leggo tutti i record confermati che hanno data fattura = video
002600030213     c                   z-add     1             dtf
002700030828     c     dtf           setll     fiatt07l
002800020603     c                   do        *hival
002900030828     c                   read      fiatt07l
003000030828     c                   if        %eof(fiatt07l)
003100020603     c                   leave
003200020603     c                   end
003300030213     C* controllo data di fatturazione
003400030828     c                   if        attdft > pardt
003500030213     c                   leave
003600030213     c                   end
003700030828     c* pulisco
003800030828     c                   delete    FIatt000
003900030213     c                   enddo
004000030828     c* memorizzo la data nella tabella BLD
004100030213     c     ktbe          chain     tntbe01l
004200030213     c                   if        %found(tntbe01l)
004300030828     c                   movel(p)  tbeuni        dbld
004400030828     c                   move      pardt         §bldclr
004500030828     c                   movel(p)  dbld          tbeuni
004600030213     c                   update    tntbe000
004700030213     c                   end
004800030213     C                   seton                                        lr
004900000607      *-----------------------------------------------
005000000607     c     *inzsr        begsr
005100000607      *-----------------------------------------------
005200000607     c
005300000607     c     *entry        plist
005400020205     c                   parm                    kpjba
005500020702     c                   movel     kpjbu         pardt
005600040217     c* tolgo 1 mesi alla data fattura richiesta a video
005700040217     c                   movel     pardt         dtaiso
005800040217     c                   subdur    1:*m          dtaiso
005900040217     c                   movel     dtaiso        pardc
006000020822     C     Ktbe          KLIST
006100020822     C                   KFLD                    tbecod
006200020822     C                   KFLD                    tbeke1
006300030828     c                   movel(p)  'BLD'         tbecod
006400020822     c                   movel(p)  '1'           tbeke1
006500020402     c*
006600020402     c                   endsr
