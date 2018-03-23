000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200020409     h*****************************************************************
000300020603     h* Il pgm pulisce tutti gli archivi dei conteggi autotrasportatori
000400020409     h*****************************************************************
000500020702     ffiftt02l  uf   e           k disk    commit
000600030123     ffifre02l  uf   e           k disk    commit
000700020702     ffifce01l  uf   e           k disk    commit
000800020702     ffiftd01l  uf   e           k disk    commit
000900020826     ftntbe01l  uf   e           k disk
001000000607     D kpjba         e ds
001100020822     D dblc          e ds
001200020702     D fiftt         e ds                  extname(fiftt00f) inz
001300020702     d pardt           s              8  0
001301040217     d pardc           s              8  0
001302040217     d dtaiso          s               d
001303040217     c* cancello le valorizzaz. non fatturate con data valorizzaz.
001304040217     c* uguale a data fattura richiesta a video - 3 mesi
001305040217     c                   exsr      srddc
001306040217     c* cancello le valorizzaz. fatturate con data fatturaz.
001307040217     c* uguale a data fattura richiesta a video
001308040217     c                   exsr      srdft
001309040217     c* memorizzo la data nella tabella BLC
001310040217     c     ktbe          chain     tntbe01l
001311040217     c                   if        %found(tntbe01l)
001312040217     c                   movel(p)  tbeuni        dblc
001313040217     c                   move      pardt         §blcclr
001314040217     c                   movel(p)  dblc          tbeuni
001315040217     c                   update    tntbe000
001316040217     c                   end
001317040217     c                   seton                                        lr
001318040217      *-----------------------------------------------
001319040217     c     srddc         begsr
001320040217      *-----------------------------------------------
001321040217     C/EXEC SQL
001322040217     C+ DECLARE A1 CURSOR FOR SELECT * FROM fiftt07l WHERE fttddc <=
001323040217     C+ :pardc and fttdft = 0
001324040217     C/END-EXEC
001325040217
001326040217     C/EXEC SQL
001327040217     C+ OPEN A1
001328040217     C/END-EXEC
001329040217
001330040217     C                   do        *hival
001331040217
001332040217     C/EXEC SQL
001333040217     C+ FETCH NEXT FROM A1 INTO :fiftt
001334040217     C/END-EXEC
001335040217
001336040217     C                   select
001337040217     C                   when      sqlcod = 100
001338040217     c                   leave
001339040217     C                   when      sqlcod = 0
001340040217     C                   exsr      srdel
001341040217     c                   commit
001342040217     C                   other
001343040217     C                   exsr      sqlerror
001344040217     C                   endsl
001345040217     C*
001346040217     C                   enddo
001347040217
001348040217     C/EXEC SQL
001349040217     C+ CLOSE A1
001350040217     C/END-EXEC
001355040217     c*
001356040217     c                   endsr
001357040217      *-----------------------------------------------
001358040217     c     srdft         begsr
001359040217      *-----------------------------------------------
001360040217     C/EXEC SQL
001361040217     C+ DECLARE B1 CURSOR FOR SELECT * FROM fiftt07l WHERE fttdft between
001362040217     C+ 1 and :pardt
001363040217     C/END-EXEC
001364040217
001365040217     C/EXEC SQL
001366040217     C+ OPEN B1
001367040217     C/END-EXEC
001368040217
001369040217     C                   do        *hival
001370040217
001371040217     C/EXEC SQL
001372040217     C+ FETCH NEXT FROM B1 INTO :fiftt
001373040217     C/END-EXEC
001374040217
001375040217     C                   select
001376040217     C                   when      sqlcod = 100
001377040217     c                   leave
001378040217     C                   when      sqlcod = 0
001379040217     C                   exsr      srdel
001380040217     c                   commit
001381040217     C                   other
001382040217     C                   exsr      sqlerror
001383040217     C                   endsl
001384040217     C*
001385040217     C                   enddo
001386040217
001387040217     C/EXEC SQL
001388040217     C+ CLOSE B1
001389040217     C/END-EXEC
001390040217     c*
001391040217     c* pulisco i record del fifce00f generati x i conteggi autotr. e che
001392040217     c* non hanno agganci con il fiftt00f
001393040217     c*
001394040217     C/EXEC SQL
001395040217     C+ DELETE FROM FIFCE03L WHERE fceddc <= :pardt
001396040217     C/END-EXEC
001397070607     c*
001398070607     c* pulisco i record dello storico anagrafiche autofatturazione
001400070607     c*
001401070607     C/EXEC SQL
001402070607     C+ DELETE FROM tnanf00f WHERE anfdft <= :pardt and anftip = 'A' and
001403070607     C+ anftsr = 'A'
001404070607     C/END-EXEC
001405040217     c                   commit
001406040217     c*
001407040217     c                   endsr
005600020702      *-----------------------------------------------
005700020702     c     srdel         begsr
005800020702      *-----------------------------------------------
005900020702     C* pulisco FIFRE00F
006000030123     c     kfre          setll     fifre02l
006100020702     c                   do        *hival
006200030123     c     kfre          reade     fifre02l
006300020702     c                   if        %eof
006400020702     c                   leave
006500020702     c                   end
006600020702     c                   delete    FIFRE000
006700020702     c                   enddo
006800020702     C* leggo dettagli fiftt00f x data conteggio
006900020702     c     kftt          setll     fiftt02l
007000020603     c                   do        *hival
007100020702     c     kftt          reade     fiftt02l
007200020702     c                   if        %eof(fiftt02l)
007300020603     c                   leave
007400020603     c                   end
007500020702     C* pulisco FIFTD00F
007600020328     c     kftd          setll     fiftd01l
007700020328     c                   do        *hival
007800020328     c     kftd          reade     fiftd01l
007900020328     c                   if        %eof
008000020328     c                   leave
008100020328     c                   end
008200020702     c                   delete    FIFTD000
008300020328     c                   enddo
008400020702     C* pulisco FIFCE00F
008500020328     c     kfce          setll     fifce01l
008600020328     c                   do        *hival
008700020328     c     kfce          reade     fifce01l
008800020328     c                   if        %eof
008900020328     c                   leave
009000020328     c                   end
009100020702     c                   delete    FIFCE000
009200020328     c                   enddo
009300020702     C* pulisco FIFTT00F
009400020702     c                   delete    FIFTT000
009500020702     c                   enddo
009600020702     c                   endsr
009700020702      *-----------------------------------------------
009800020702     C     sqlerror      begsr
009900020702      *-----------------------------------------------
010000020702     C*
010100020702     C                   call      'X66CHGJOB'
010200020702     C                   seton                                        lr
010300020702     C*
010400020702     C                   endsr
010500000607      *-----------------------------------------------
010600000607     c     *inzsr        begsr
010700000607      *-----------------------------------------------
010800000607     c
010900000607     c     *entry        plist
011000020205     c                   parm                    kpjba
011100020702     c                   movel     kpjbu         pardt
011101040217     c* tolgo 3 mesi dalla data fattura richiesta a video
011102040217     c                   movel     pardt         dtaiso
011103040217     c                   subdur    3:*m          dtaiso
011104040217     c                   movel     dtaiso        pardc
011200020702     c*
011300020328     c     kftt          klist
011400020328     c                   kfld                    fttpdr
011500020702     c                   kfld                    fttfvl
011600020328     c                   kfld                    fttddc
011700020328     c     kfre          klist
011701030123     c                   kfld                    ktip
011800020328     c                   kfld                    fttpdr
011900020328     c                   kfld                    fttddc
011901030123     c                   move      'A'           ktip              1
012000020328     c     kfce          klist
012100020328     c                   kfld                    fttfgs
012200020328     c                   kfld                    fttpdr
012300020328     c                   kfld                    ftttsr
012400020328     c                   kfld                    fttndc
012500020328     c                   kfld                    fttddc
012600020328     c     kftd          klist
012700020328     c                   kfld                    fttpdr
012800020328     c                   kfld                    ftttsr
012900020328     c                   kfld                    fttndc
013000020328     c                   kfld                    fttddc
013100020822     C     Ktbe          KLIST
013200020822     C                   KFLD                    tbecod
013300020822     C                   KFLD                    tbeke1
013400020822     c                   movel(p)  'BLC'         tbecod
013500020822     c                   movel(p)  '1'           tbeke1
013600020402     c*
013700020402     c                   endsr
