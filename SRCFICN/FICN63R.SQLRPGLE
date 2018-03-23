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
001400040217     d pardc           s              8  0
001500040217     d dtaiso          s               d
001600040217     c* cancello le valorizzaz. non fatturate con data valorizzaz.
001700040217     c* uguale a data fattura richiesta a video - 3 mesi
001800040217     c                   exsr      srddc
001900040217     c* cancello le valorizzaz. fatturate con data fatturaz.
002000040217     c* uguale a data fattura richiesta a video
002100040217     c                   exsr      srdft
002200040217     c* memorizzo la data nella tabella BLC
002300040217     c     ktbe          chain     tntbe01l
002400040217     c                   if        %found(tntbe01l)
002500040217     c                   movel(p)  tbeuni        dblc
002600040217     c                   move      pardt         §blcclr
002700040217     c                   movel(p)  dblc          tbeuni
002800040217     c                   update    tntbe000
002900040217     c                   end
003000040217     c                   seton                                        lr
003100040217      *-----------------------------------------------
003200040217     c     srddc         begsr
003300040217      *-----------------------------------------------
003400040217     C/EXEC SQL
003500040217     C+ DECLARE A1 CURSOR FOR SELECT * FROM fiftt07l WHERE fttddc <=
003600040217     C+ :pardc and fttdft = 0
003700040217     C/END-EXEC
003800040217
003900040217     C/EXEC SQL
004000040217     C+ OPEN A1
004100040217     C/END-EXEC
004200040217
004300040217     C                   do        *hival
004400040217
004500040217     C/EXEC SQL
004600040217     C+ FETCH NEXT FROM A1 INTO :fiftt
004700040217     C/END-EXEC
004800040217
004900040217     C                   select
005000040217     C                   when      sqlcod = 100
005100040217     c                   leave
005200040217     C                   when      sqlcod = 0
005300040217     C                   exsr      srdel
005400040217     c                   commit
005500040217     C                   other
005600040217     C                   exsr      sqlerror
005700040217     C                   endsl
005800040217     C*
005900040217     C                   enddo
006000040217
006100040217     C/EXEC SQL
006200040217     C+ CLOSE A1
006300040217     C/END-EXEC
006400040217     c*
006500040217     c                   endsr
006600040217      *-----------------------------------------------
006700040217     c     srdft         begsr
006800040217      *-----------------------------------------------
006900040217     C/EXEC SQL
007000040217     C+ DECLARE B1 CURSOR FOR SELECT * FROM fiftt07l WHERE fttdft between
007100040217     C+ 1 and :pardt
007200040217     C/END-EXEC
007300040217
007400040217     C/EXEC SQL
007500040217     C+ OPEN B1
007600040217     C/END-EXEC
007700040217
007800040217     C                   do        *hival
007900040217
008000040217     C/EXEC SQL
008100040217     C+ FETCH NEXT FROM B1 INTO :fiftt
008200040217     C/END-EXEC
008300040217
008400040217     C                   select
008500040217     C                   when      sqlcod = 100
008600040217     c                   leave
008700040217     C                   when      sqlcod = 0
008800040217     C                   exsr      srdel
008900040217     c                   commit
009000040217     C                   other
009100040217     C                   exsr      sqlerror
009200040217     C                   endsl
009300040217     C*
009400040217     C                   enddo
009500040217
009600040217     C/EXEC SQL
009700040217     C+ CLOSE B1
009800040217     C/END-EXEC
009900040217     c*
010000040217     c* pulisco i record del fifce00f generati x i conteggi autotr. e che
010100040217     c* non hanno agganci con il fiftt00f
010200040217     c*
010300040217     C/EXEC SQL
010400040217     C+ DELETE FROM FIFCE03L WHERE fceddc <= :pardt
010500040217     C/END-EXEC
010600070607     c*
010700040217     c                   commit
010800040217     c*
010900040217     c                   endsr
011000020702      *-----------------------------------------------
011100020702     c     srdel         begsr
011200020702      *-----------------------------------------------
011300020702     C* pulisco FIFRE00F
011400030123     c     kfre          setll     fifre02l
011500020702     c                   do        *hival
011600030123     c     kfre          reade     fifre02l
011700020702     c                   if        %eof
011800020702     c                   leave
011900020702     c                   end
012000020702     c                   delete    FIFRE000
012100020702     c                   enddo
012200020702     C* leggo dettagli fiftt00f x data conteggio
012300020702     c     kftt          setll     fiftt02l
012400020603     c                   do        *hival
012500020702     c     kftt          reade     fiftt02l
012600020702     c                   if        %eof(fiftt02l)
012700020603     c                   leave
012800020603     c                   end
012900020702     C* pulisco FIFTD00F
013000020328     c     kftd          setll     fiftd01l
013100020328     c                   do        *hival
013200020328     c     kftd          reade     fiftd01l
013300020328     c                   if        %eof
013400020328     c                   leave
013500020328     c                   end
013600020702     c                   delete    FIFTD000
013700020328     c                   enddo
013800020702     C* pulisco FIFCE00F
013900020328     c     kfce          setll     fifce01l
014000020328     c                   do        *hival
014100020328     c     kfce          reade     fifce01l
014200020328     c                   if        %eof
014300020328     c                   leave
014400020328     c                   end
014500020702     c                   delete    FIFCE000
014600020328     c                   enddo
014700020702     C* pulisco FIFTT00F
014800020702     c                   delete    FIFTT000
014900020702     c                   enddo
015000020702     c                   endsr
015100020702      *-----------------------------------------------
015200020702     C     sqlerror      begsr
015300020702      *-----------------------------------------------
015400020702     C*
015500020702     C                   call      'X66CHGJOB'
015600020702     C                   seton                                        lr
015700020702     C*
015800020702     C                   endsr
015900000607      *-----------------------------------------------
016000000607     c     *inzsr        begsr
016100000607      *-----------------------------------------------
016200000607     c
016300000607     c     *entry        plist
016400020205     c                   parm                    kpjba
016500020702     c                   movel     kpjbu         pardt
016600040217     c* tolgo 3 mesi dalla data fattura richiesta a video
016700040217     c                   movel     pardt         dtaiso
016800040217     c                   subdur    3:*m          dtaiso
016900040217     c                   movel     dtaiso        pardc
017000020702     c*
017100020328     c     kftt          klist
017200020328     c                   kfld                    fttpdr
017300020702     c                   kfld                    fttfvl
017400020328     c                   kfld                    fttddc
017500020328     c     kfre          klist
017600030123     c                   kfld                    ktip
017700020328     c                   kfld                    fttpdr
017800020328     c                   kfld                    fttddc
017900030123     c                   move      'A'           ktip              1
018000020328     c     kfce          klist
018100020328     c                   kfld                    fttfgs
018200020328     c                   kfld                    fttpdr
018300020328     c                   kfld                    ftttsr
018400020328     c                   kfld                    fttndc
018500020328     c                   kfld                    fttddc
018600020328     c     kftd          klist
018700020328     c                   kfld                    fttpdr
018800020328     c                   kfld                    ftttsr
018900020328     c                   kfld                    fttndc
019000020328     c                   kfld                    fttddc
019100020822     C     Ktbe          KLIST
019200020822     C                   KFLD                    tbecod
019300020822     C                   KFLD                    tbeke1
019400020822     c                   movel(p)  'BLC'         tbecod
019500020822     c                   movel(p)  '1'           tbeke1
019600020402     c*
019700020402     c                   endsr
