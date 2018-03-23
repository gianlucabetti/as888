000100021104      *PARMS CLOSQLCSR(*ENDMOD)
000200000000     H DECEDIT('0,') DATEDIT(*DMY.)
000300011206     F*          STATISTICA CONSUMI                        *
000400011206     F* SCRIVO RECORD DELLO STORICO PER AUTISTA            *
000500000000     F*----------------------------------------------------*
000600021105     FMZIMP01l  uf   e           K disk    rename(mzimp000:mzimp001)
000700021104     FMZIMP02l  uf a e           K disk
000800011205     d param           ds
000900011205     d anno                           4  0
001000011205     d mese                           2  0
001100011205     d vscef                          1
001200011205     d vscec                          1
001300011205     d vscea                          1
001400011205     d vscet                          1
001500011205     d vfil                           3  0
001600141124     d vcat                          20
001700011205     d vaus                           2
001800011205     d vtga                           2
001900011205     d vtgn                           9
002000141124     d vvend                          1
002100160810     d vkmdal                         6  0
002200160810     d vkmal                          6  0
002300160810     d vcaut                          2
002400160902     d vfun                           5
002500020607     D KPJBA         E DS
002600021104     D mzimp         E DS                  extname(mzimp00f) inz
002700021104     D mzimps        E DS                  extname(mzimp00f) prefix(s) inz
002800021104     D mzimp£        E DS                  extname(mzimp00f) prefix(£) inz
002900021105     D savdat          S                   like(impdat) inz
003000021105     D savqta          S                   like(impqta) inz
003100021104     D savkm           S                   like(impkm) inz
003200021104     D data1           S              8  0
003300021104     D data2           S              8  0
003400021104     D dat             S                   like(impdat) inz
003500021104     D nrf             S                   like(impnrf) inz
003600021104     D int             S                   like(impint) inz
003700020710     C*---------------------------------------------------------------*
003800021104     C* leggo tutti gli interventi £1 nel periodo scelto dal più grande al
003900021104     c* più piccolo distribuendo il consumo all'inizio del mese invece che
004000021104     c* alla fine. Questo è valido fino al 31/12/02 dopo le £1 dovranno
004100021104     c* essere immesse con lo stesso criterio dell'intervento CB e cioè
004200021104     c* i consumi sono spalmati in avanti e non all'indietro.
004300021104     C/EXEC SQL
004400021104     C+ DECLARE A1 CURSOR FOR SELECT * FROM MZIMP00F WHERE impint = '£1'
004500021104     C+ and IMPDAT BETWEEN :DATA1 AND :DATA2 and impdat <= 20021231 ORDER
004600021104     C+ BY IMPNRf, IMPDAT desc
004700021104     C/END-EXEC
004800021104
004900021104     C/EXEC SQL
005000021104     C+ OPEN A1
005100021104     C/END-EXEC
005200021104
005300021104     C                   dou       *inlr
005400021104
005500021104     C/EXEC SQL
005600021104     C+ FETCH NEXT FROM A1 INTO :mzimp
005700021104     C/END-EXEC
005800021104
005900021104     C                   select
006000021104     C                   when      sqlcod = 100
006100021104     c                   exsr      srwrite
006200021104     C                   seton                                        lr
006300021104     C                   when      sqlcod = 0
006400021104     C                   exsr      calcoli
006500021104     C                   other
006600021104     C                   exsr      sqlerror
006700021104     C                   endsl
006800021104     C*
006900021104     C                   enddo
007000021104
007100021104     C/EXEC SQL
007200021104     C+ CLOSE A1
007300021104     C/END-EXEC
007400021104     c*
007500021104     C*---------------------------------------------------------------*
007600021104     c     calcoli       begsr
007700021104     C*---------------------------------------------------------------*
007800021104     c* rottura numero riferimento
007900021104     c                   if        impnrf <> simpnrf
008000021104     c                   exsr      srwrite
008100021104     c                   z-add     0             savqta
008200021104     c                   end
008300021104     c*
008400021104     c                   eval      mzimps = mzimp
008500021104     c*
008600021104     c     kimp          chain     mzimp02l
008700021104     c                   if        %found(mzimp02l)
008800021104     c                   move      savqta        impqta
008900021104     c                   update    mzimp000
009000021104     c                   move      simpqta       savqta
009100021104     c                   end
009200021104     c                   endsr
009300020607     C*---------------------------------------------------------------*
009400021104     c     srwrite       begsr
009500020607     C*---------------------------------------------------------------*
009600021105     c* se sono a fine lettura è ho della qta ancora da attribuire cerco
009700021105     c* il record del mese precedente oppure se non lo trovo creo
009800021104     c* un record all'inizio del mese a meno che non esista già un record CB
009900021105     c* nel quale aggiungere la qta ancora da attribuire
010000021104     c                   eval      mzimp£ = mzimp
010100021104     c                   if        savqta <> 0
010200021104     c* controllo che non esista un record CB
010300021104     c                   move      'CB'          int
010400021104     c                   move      simpdat       dat
010500021104     c                   move      01            dat
010600021104     c                   move      simpnrf       nrf
010700021104     c     kimp1         chain     mzimp02l
010800021104     c                   if        %found(mzimp02l)
010900021104     c                   add       savqta        impqta
011000021104     c                   update    mzimp000
011100021104     c                   else
011200021104     c                   move      '£1'          int
011300021104     c                   move      simpdat       dat
011400021104     c                   move      simpnrf       nrf
011500021104     C     KIMP2         setll     mzimp01l
011600021104     C     kimp3         readpe    mzimp01l
011700021104     c                   if        %eof(mzimp01l)
011800021105     c                   eval      mzimp = mzimps
011900021105     c                   z-add     1             impkm
012000021105     c                   move      01            impdat
012100021105     c                   move      savqta        impqta
012200021105     c                   write     mzimp000
012300021104     c                   else
012400021105     c                   move      savqta        impqta
012500021105     c                   update    mzimp001
012600021104     c                   end
012700021104     c                   end
012800021104     c                   end
012900021104     c                   eval      mzimp = mzimp£
013000021104     c                   endsr
013100021104     C*---------------------------------------------------------------*
013200021104     c     *inzsr        begsr
013300021104     C*---------------------------------------------------------------*
013400020607     C     *ENTRY        PLIST
013500020607     C                   PARM                    KPJBA
013600020607     c                   movel     kpjbu         param
013700020607     C* data inizio
013800020607     c                   if        anno <> 0
013900020607     C                   MOVEl     anno          DATAX             6 0
014000020607     c                   else
014100020607     C                   movel     0001          DATAX
014200020607     c                   end
014300020607     c                   if        mese = 0
014400020607     c                   move      01            datax
014500020607     c                   else
014600020607     C                   MOVE      mese          DATAX
014700020607     c                   end
014800020607     C                   movel     datax         DATA1
014900020607     C                   MOVE      01            DATA1
015000021104     c* ricalcolo solo le £1 antecedenti il 31/12/02
015100021104     c                   if        data1 > 20021231
015200021104     c                   seton                                        lr
015300021104     c                   return
015400021104     c                   end
015500020607     C* data fine
015600020607     c                   if        anno <> 0
015700020607     C                   MOVEl     anno          DATAX             6 0
015800020607     c                   else
015900020607     C                   movel     9999          DATAX
016000020607     c                   end
016100020607     c                   if        mese = 0
016200020607     c                   move      12            datax
016300020607     c                   else
016400020607     C                   MOVE      mese          DATAX
016500020607     c                   end
016600020607     C                   movel     datax         DATA2
016700020607     C                   MOVE      31            DATA2
016800020607     C* POSIZIONAMENTO SU MZIMP000                         *
016900020607     C     KIMP          KLIST
017000021104     C                   KFLD                    Impint
017100021104     C                   KFLD                    impdat
017200021104     C                   KFLD                    impnrf
017300021104     C     KIMP1         KLIST
017400021104     C                   KFLD                    int
017500021104     C                   KFLD                    dat
017600021104     C                   KFLD                    nrf
017700021104     C     KIMP2         KLIST
017800021104     C                   KFLD                    nrf
017900021104     C                   KFLD                    int
018000021104     C                   KFLD                    dat
018100021104     C     KIMP3         KLIST
018200021104     C                   KFLD                    nrf
018300021104     C                   KFLD                    int
018400020607     c                   endsr
018500021104     C*---------------------------------------------------------------*
018600021104     C     sqlerror      begsr
018700021104     C*---------------------------------------------------------------*
018800021104     C*
018900021104     C                   call      'X66CHGJOB'
019000021104     C                   seton                                        lr
019100021104     C*
019200021104     C                   endsr
