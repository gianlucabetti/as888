000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200020409     h*****************************************************************
000300030828     h* Il pgm pulisce tutti gli archivi dei conteggi aff/defl
000400020409     h*****************************************************************
000500030828     ffiatt07l  uf   e           k disk
000600020826     ftntbe01l  uf   e           k disk
000700000607     D kpjba         e ds
000800030828     D dbld          e ds
000900020702     d pardt           s              8  0
000901040217     d pardc           s              8  0
000902040217     d dtaiso          s               d
001000030828     d dtf             s                   like(attdft) inz
001101040217     c* pulisco i record del fiatt00f generati x i conteggi aff/def
001102040217     c* con data prestaz. <= dta fattura richiesta - 1
001103040217     c*
001104040217     C/EXEC SQL
001105040217     C+ DELETE FROM FIatt00f WHERE attddc <= :pardc
001106040217     C/END-EXEC
001107040217     c*
001108130527     c*
001109130527     C/EXEC SQL
001110130527     C+ DELETE FROM FIatt10f WHERE at1ddc <= :pardc
001111130527     C/END-EXEC
001112130527     c*
001113070607     C/EXEC SQL
001114070607     C+ DELETE FROM tnanf00f WHERE anfdft <= :pardc and anftip = 'D'
001115070607     C+ and anftsr = 'D'
001116070607     C/END-EXEC
001117070607     c*
001200030213     c* leggo tutti i record confermati che hanno data fattura = video
001400030213     c                   z-add     1             dtf
001500030828     c     dtf           setll     fiatt07l
001600020603     c                   do        *hival
001700030828     c                   read      fiatt07l
001800030828     c                   if        %eof(fiatt07l)
001900020603     c                   leave
002000020603     c                   end
002100030213     C* controllo data di fatturazione
002200030828     c                   if        attdft > pardt
002300030213     c                   leave
002400030213     c                   end
002900030828     c* pulisco
003000030828     c                   delete    FIatt000
003100030213     c                   enddo
003200030828     c* memorizzo la data nella tabella BLD
003300030213     c     ktbe          chain     tntbe01l
003400030213     c                   if        %found(tntbe01l)
003500030828     c                   movel(p)  tbeuni        dbld
003600030828     c                   move      pardt         §bldclr
003700030828     c                   movel(p)  dbld          tbeuni
003800030213     c                   update    tntbe000
003900030213     c                   end
004000030213     C                   seton                                        lr
004100000607      *-----------------------------------------------
004200000607     c     *inzsr        begsr
004300000607      *-----------------------------------------------
004400000607     c
004500000607     c     *entry        plist
004600020205     c                   parm                    kpjba
004700020702     c                   movel     kpjbu         pardt
004701040217     c* tolgo 1 mesi alla data fattura richiesta a video
004702040217     c                   movel     pardt         dtaiso
004703040217     c                   subdur    1:*m          dtaiso
004704040217     c                   movel     dtaiso        pardc
004800020822     C     Ktbe          KLIST
004900020822     C                   KFLD                    tbecod
005000020822     C                   KFLD                    tbeke1
005100030828     c                   movel(p)  'BLD'         tbecod
005200020822     c                   movel(p)  '1'           tbeke1
005300020402     c*
005400020402     c                   endsr
