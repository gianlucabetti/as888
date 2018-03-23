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
002100020607     D KPJBA         E DS
002200021104     D mzimp         E DS                  extname(mzimp00f) inz
002300021104     D mzimps        E DS                  extname(mzimp00f) prefix(s) inz
002400021104     D mzimp£        E DS                  extname(mzimp00f) prefix(£) inz
002500021105     D savdat          S                   like(impdat) inz
002600021105     D savqta          S                   like(impqta) inz
002700021104     D savkm           S                   like(impkm) inz
002800021104     D data1           S              8  0
002900021104     D data2           S              8  0
003000021104     D dat             S                   like(impdat) inz
003100021104     D nrf             S                   like(impnrf) inz
003200021104     D int             S                   like(impint) inz
003300020710     C*---------------------------------------------------------------*
003400021104     C* leggo tutti gli interventi £1 nel periodo scelto dal più grande al
003500021104     c* più piccolo distribuendo il consumo all'inizio del mese invece che
003600021104     c* alla fine. Questo è valido fino al 31/12/02 dopo le £1 dovranno
003700021104     c* essere immesse con lo stesso criterio dell'intervento CB e cioè
003800021104     c* i consumi sono spalmati in avanti e non all'indietro.
003900021104     C/EXEC SQL
004000021104     C+ DECLARE A1 CURSOR FOR SELECT * FROM MZIMP00F WHERE impint = '£1'
004100021104     C+ and IMPDAT BETWEEN :DATA1 AND :DATA2 and impdat <= 20021231 ORDER
004200021104     C+ BY IMPNRf, IMPDAT desc
004300021104     C/END-EXEC
004400021104
004500021104     C/EXEC SQL
004600021104     C+ OPEN A1
004700021104     C/END-EXEC
004800021104
004900021104     C                   dou       *inlr
005000021104
005100021104     C/EXEC SQL
005200021104     C+ FETCH NEXT FROM A1 INTO :mzimp
005300021104     C/END-EXEC
005400021104
005500021104     C                   select
005600021104     C                   when      sqlcod = 100
005700021104     c                   exsr      srwrite
005800021104     C                   seton                                        lr
005900021104     C                   when      sqlcod = 0
006000021104     C                   exsr      calcoli
006100021104     C                   other
006200021104     C                   exsr      sqlerror
006300021104     C                   endsl
006400021104     C*
006500021104     C                   enddo
006600021104
006700021104     C/EXEC SQL
006800021104     C+ CLOSE A1
006900021104     C/END-EXEC
007000021104     c*
007100021104     C*---------------------------------------------------------------*
007200021104     c     calcoli       begsr
007300021104     C*---------------------------------------------------------------*
007400021104     c* rottura numero riferimento
007500021104     c                   if        impnrf <> simpnrf
007600021104     c                   exsr      srwrite
007700021104     c                   z-add     0             savqta
007800021104     c                   end
007900021104     c*
008000021104     c                   eval      mzimps = mzimp
008100021104     c*
008200021104     c     kimp          chain     mzimp02l
008300021104     c                   if        %found(mzimp02l)
008400021104     c                   move      savqta        impqta
008500021104     c                   update    mzimp000
008600021104     c                   move      simpqta       savqta
008700021104     c                   end
008800021104     c                   endsr
008900020607     C*---------------------------------------------------------------*
009000021104     c     srwrite       begsr
009100020607     C*---------------------------------------------------------------*
009200021105     c* se sono a fine lettura è ho della qta ancora da attribuire cerco
009300021105     c* il record del mese precedente oppure se non lo trovo creo
009400021104     c* un record all'inizio del mese a meno che non esista già un record CB
009500021105     c* nel quale aggiungere la qta ancora da attribuire
009600021104     c                   eval      mzimp£ = mzimp
009700021104     c                   if        savqta <> 0
009800021104     c* controllo che non esista un record CB
009900021104     c                   move      'CB'          int
010000021104     c                   move      simpdat       dat
010100021104     c                   move      01            dat
010200021104     c                   move      simpnrf       nrf
010300021104     c     kimp1         chain     mzimp02l
010400021104     c                   if        %found(mzimp02l)
010500021104     c                   add       savqta        impqta
010600021104     c                   update    mzimp000
010700021104     c                   else
010800021104     c                   move      '£1'          int
010900021104     c                   move      simpdat       dat
011000021104     c                   move      simpnrf       nrf
011100021104     C     KIMP2         setll     mzimp01l
011200021104     C     kimp3         readpe    mzimp01l
011300021104     c                   if        %eof(mzimp01l)
011400021105     c                   eval      mzimp = mzimps
011500021105     c                   z-add     1             impkm
011600021105     c                   move      01            impdat
011700021105     c                   move      savqta        impqta
011800021105     c                   write     mzimp000
011900021104     c                   else
012000021105     c                   move      savqta        impqta
012100021105     c                   update    mzimp001
012200021104     c                   end
012300021104     c                   end
012400021104     c                   end
012500021104     c                   eval      mzimp = mzimp£
012600021104     c                   endsr
012700021104     C*---------------------------------------------------------------*
012800021104     c     *inzsr        begsr
012900021104     C*---------------------------------------------------------------*
013000020607     C     *ENTRY        PLIST
013100020607     C                   PARM                    KPJBA
013200020607     c                   movel     kpjbu         param
013300020607     C* data inizio
013400020607     c                   if        anno <> 0
013500020607     C                   MOVEl     anno          DATAX             6 0
013600020607     c                   else
013700020607     C                   movel     0001          DATAX
013800020607     c                   end
013900020607     c                   if        mese = 0
014000020607     c                   move      01            datax
014100020607     c                   else
014200020607     C                   MOVE      mese          DATAX
014300020607     c                   end
014400020607     C                   movel     datax         DATA1
014500020607     C                   MOVE      01            DATA1
014600021104     c* ricalcolo solo le £1 antecedenti il 31/12/02
014700021104     c                   if        data1 > 20021231
014800021104     c                   seton                                        lr
014900021104     c                   return
015000021104     c                   end
015100020607     C* data fine
015200020607     c                   if        anno <> 0
015300020607     C                   MOVEl     anno          DATAX             6 0
015400020607     c                   else
015500020607     C                   movel     9999          DATAX
015600020607     c                   end
015700020607     c                   if        mese = 0
015800020607     c                   move      12            datax
015900020607     c                   else
016000020607     C                   MOVE      mese          DATAX
016100020607     c                   end
016200020607     C                   movel     datax         DATA2
016300020607     C                   MOVE      31            DATA2
016400020607     C* POSIZIONAMENTO SU MZIMP000                         *
016500020607     C     KIMP          KLIST
016600021104     C                   KFLD                    Impint
016700021104     C                   KFLD                    impdat
016800021104     C                   KFLD                    impnrf
016900021104     C     KIMP1         KLIST
017000021104     C                   KFLD                    int
017100021104     C                   KFLD                    dat
017200021104     C                   KFLD                    nrf
017300021104     C     KIMP2         KLIST
017400021104     C                   KFLD                    nrf
017500021104     C                   KFLD                    int
017600021104     C                   KFLD                    dat
017700021104     C     KIMP3         KLIST
017800021104     C                   KFLD                    nrf
017900021104     C                   KFLD                    int
018000020607     c                   endsr
018100021104     C*---------------------------------------------------------------*
018200021104     C     sqlerror      begsr
018300021104     C*---------------------------------------------------------------*
018400021104     C*
018500021104     C                   call      'X66CHGJOB'
018600021104     C                   seton                                        lr
018700021104     C*
018800021104     C                   endsr
