000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200020409     h*****************************************************************
000300030212     h* Il pgm pulisce tutti gli archivi dei conteggi cooperative
000400020409     h*****************************************************************
000500030213     ffictt07l  uf   e           k disk    commit
000600030123     ffifre02l  uf   e           k disk    commit
000700030213     ffictd02l  uf   e           k disk    commit
000800020826     ftntbe01l  uf   e           k disk
000900000607     D kpjba         e ds
001000030212     D dblo          e ds
001100020702     d pardt           s              8  0
001200040217     d pardc           s              8  0
001300040217     d dtaiso          s               d
001400030213     d dtf             s                   like(cttdft) inz
001500020702     c*
001600140901     c* pulisco i record del fictt00f/fifre00f/fictd00f/         con data
001700040217     c* prestazione <= data fattura richiesta a video - 1 mese
001800040217     c*
001900040217     C/EXEC SQL
002000040217     C+ DELETE FROM FIctt00f WHERE cttddc <= :pardc
002100040217     C/END-EXEC
002200040217     c*
002300040217     C/EXEC SQL
002400040217     C+ DELETE FROM FIctd00F WHERE ctdddc <= :pardc
002500040217     C/END-EXEC
002600040217     c*
002700040217     C/EXEC SQL
002800040217     C+ DELETE FROM FIFRE00F WHERE freddc <= :pardc and fretip = 'C'
002900040217     C/END-EXEC
003000040217     c*
003100030213     c* leggo tutti i record confermati che hanno data fattura = video
003200030213     c                   z-add     1             dtf
003300030213     c     dtf           setll     fictt07l
003400020603     c                   do        *hival
003500030213     c                   read      fictt07l
003600030213     c                   if        %eof(fictt07l)
003700020603     c                   leave
003800020603     c                   end
003900030213     C* controllo data di fatturazione
004000030213     c                   if        cttdft > pardt
004100030213     c                   leave
004200030213     c                   end
004300030213     C* pulisco FIFRE00F
004400030213     c     kfre          setll     fifre02l
004500030213     c                   do        *hival
004600030213     c     kfre          reade     fifre02l
004700030213     c                   if        %eof
004800030213     c                   leave
004900030213     c                   end
005000030213     c                   delete    FIFRE000
005100030213     c                   enddo
005200030213     C* pulisco FIcTD00F
005300030213     c     kctd          setll     fictd02l
005400020328     c                   do        *hival
005500030213     c     kctd          reade     fictd02l
005600020328     c                   if        %eof
005700020328     c                   leave
005800020328     c                   end
005900030213     c                   delete    FIcTD000
006000020328     c                   enddo
006100030213     c* pulisco testata
006200030213     c                   delete    FIctt000
006300030213     c                   commit
006400030213     c                   enddo
006500030213     c* memorizzo la data nella tabella BLO
006600030213     c     ktbe          chain     tntbe01l
006700030213     c                   if        %found(tntbe01l)
006800030213     c                   movel(p)  tbeuni        dblo
006900030213     c                   move      pardt         §bloclr
007000030213     c                   movel(p)  dblo          tbeuni
007100030213     c                   update    tntbe000
007200030213     c                   end
007300030213     C                   seton                                        lr
007400000607      *-----------------------------------------------
007500000607     c     *inzsr        begsr
007600000607      *-----------------------------------------------
007700000607     c
007800000607     c     *entry        plist
007900020205     c                   parm                    kpjba
008000020702     c                   movel     kpjbu         pardt
008100040217     c* tolgo 1 mesi dalla data fattura richiesta a video
008200040217     c                   movel     pardt         dtaiso
008300040217     c                   subdur    1:*m          dtaiso
008400040217     c                   movel     dtaiso        pardc
008500020328     c     kfre          klist
008600030123     c                   kfld                    ktip
008700030212     c                   kfld                    cttpdr
008800030212     c                   kfld                    cttddc
008900030212     c                   kfld                    ctttsr
009000030212     c                   move      'C'           ktip              1
009100030213     c     kctd          klist
009200030213     c                   kfld                    cttfgs
009300030213     c                   kfld                    cttpdr
009400030212     c                   kfld                    ctttsr
009500030212     c                   kfld                    cttddc
009600020822     C     Ktbe          KLIST
009700020822     C                   KFLD                    tbecod
009800020822     C                   KFLD                    tbeke1
009900030212     c                   movel(p)  'BLO'         tbecod
010000020822     c                   movel(p)  '1'           tbeke1
010100020402     c*
010200020402     c                   endsr
