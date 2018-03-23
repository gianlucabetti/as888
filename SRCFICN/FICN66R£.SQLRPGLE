000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200020409     h*****************************************************************
000300030212     h* Il pgm pulisce tutti gli archivi dei conteggi cooperative
000400020409     h*****************************************************************
000500030213     ffictt07l  uf   e           k disk    commit
000600030123     ffifre02l  uf   e           k disk    commit
000800030213     ffictd02l  uf   e           k disk    commit
000900020826     ftntbe01l  uf   e           k disk
001000000607     D kpjba         e ds
001100030212     D dblo          e ds
001300020702     d pardt           s              8  0
001301040217     d pardc           s              8  0
001302040217     d dtaiso          s               d
001303030213     d dtf             s                   like(cttdft) inz
001400020702     c*
001401070607     c* pulisco i record del fictt00f/fifre00f/fictd00f/TNANF00F con data
001402040217     c* prestazione <= data fattura richiesta a video - 1 mese
001403040217     c*
001404040217     C/EXEC SQL
001405040217     C+ DELETE FROM FIctt00f WHERE cttddc <= :pardc
001406040217     C/END-EXEC
001407040217     c*
001408040217     C/EXEC SQL
001409040217     C+ DELETE FROM FIctd00F WHERE ctdddc <= :pardc
001411040217     C/END-EXEC
001412040217     c*
001413040217     C/EXEC SQL
001414040217     C+ DELETE FROM FIFRE00F WHERE freddc <= :pardc and fretip = 'C'
001415040217     C/END-EXEC
001416070607     c*
001417070607     C/EXEC SQL
001418070607     C+ DELETE FROM tnanf00f WHERE anfdft <= :pardc and anftip = 'C'
001419070607     C+ and anftsr = 'C'
001420070607     C/END-EXEC
001421040217     c*
001500030213     c* leggo tutti i record confermati che hanno data fattura = video
001601030213     c                   z-add     1             dtf
006900030213     c     dtf           setll     fictt07l
007000020603     c                   do        *hival
007100030213     c                   read      fictt07l
007200030213     c                   if        %eof(fictt07l)
007300020603     c                   leave
007400020603     c                   end
007401030213     C* controllo data di fatturazione
007402030213     c                   if        cttdft > pardt
007403030213     c                   leave
007404030213     c                   end
007409030213     C* pulisco FIFRE00F
007410030213     c     kfre          setll     fifre02l
007411030213     c                   do        *hival
007412030213     c     kfre          reade     fifre02l
007413030213     c                   if        %eof
007414030213     c                   leave
007415030213     c                   end
007416030213     c                   delete    FIFRE000
007417030213     c                   enddo
007500030213     C* pulisco FIcTD00F
007600030213     c     kctd          setll     fictd02l
007700020328     c                   do        *hival
007800030213     c     kctd          reade     fictd02l
007900020328     c                   if        %eof
008000020328     c                   leave
008100020328     c                   end
008200030213     c                   delete    FIcTD000
008300020328     c                   enddo
008301030213     c* pulisco testata
008302030213     c                   delete    FIctt000
008303030213     c                   commit
008304030213     c                   enddo
009601030213     c* memorizzo la data nella tabella BLO
009602030213     c     ktbe          chain     tntbe01l
009603030213     c                   if        %found(tntbe01l)
009604030213     c                   movel(p)  tbeuni        dblo
009605030213     c                   move      pardt         §bloclr
009606030213     c                   movel(p)  dblo          tbeuni
009607030213     c                   update    tntbe000
009608030213     c                   end
009609030213     C                   seton                                        lr
010500000607      *-----------------------------------------------
010600000607     c     *inzsr        begsr
010700000607      *-----------------------------------------------
010800000607     c
010900000607     c     *entry        plist
011000020205     c                   parm                    kpjba
011100020702     c                   movel     kpjbu         pardt
011101040217     c* tolgo 1 mesi dalla data fattura richiesta a video
011102040217     c                   movel     pardt         dtaiso
011103040217     c                   subdur    1:*m          dtaiso
011104040217     c                   movel     dtaiso        pardc
011700020328     c     kfre          klist
011701030123     c                   kfld                    ktip
011800030212     c                   kfld                    cttpdr
011900030212     c                   kfld                    cttddc
011901030212     c                   kfld                    ctttsr
011902030212     c                   move      'C'           ktip              1
012600030213     c     kctd          klist
012700030213     c                   kfld                    cttfgs
012701030213     c                   kfld                    cttpdr
012800030212     c                   kfld                    ctttsr
013000030212     c                   kfld                    cttddc
013100020822     C     Ktbe          KLIST
013200020822     C                   KFLD                    tbecod
013300020822     C                   KFLD                    tbeke1
013400030212     c                   movel(p)  'BLO'         tbecod
013500020822     c                   movel(p)  '1'           tbeke1
013600020402     c*
013700020402     c                   endsr
