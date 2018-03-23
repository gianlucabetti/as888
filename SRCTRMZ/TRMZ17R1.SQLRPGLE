000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200011206     F*          STATISTICA CONSUMI                        *
000300011206     F* SCRIVO RECORD DELLO STORICO PER AUTISTA            *
000400000000     F*----------------------------------------------------*
000500141201     Fwrkame1L  If   e           K disk
000600020607     FMZIMP01L  If   e           K disk    rename(mzimp000:mzimp01)
000700020611     f                                     prefix($)
000800020709     FWRKimp0F  O    e             disk    rename(mzimp000:wrkimp00)
000900020607     f                                     prefix(£)
001000020709     FWRKimp1L  UF   e           k disk    RENAME(MZIMP000:WRKIMP01)
001100020607     f                                     prefix(£)
001200011205     d param           ds
001300011205     d anno                           4  0
001400011205     d mese                           2  0
001500011205     d vscef                          1
001600011205     d vscec                          1
001700011205     d vscea                          1
001800011205     d vscet                          1
001900011205     d vfil                           3  0
002000141124     d vcat                          20
002100011205     d vaus                           2
002200011205     d vtga                           2
002300011205     d vtgn                           9
002400141124     d vvend                          1
002500160810     d vkmdal                         6  0
002600160810     d vkmal                          6  0
002700160810     d vcaut                          2
002800160902     d vfun                           5
002900160810      *
003000020607     D KPJBA         E DS
003100000000     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
003200020607     D wrkimp        E DS                  EXTNAME(wrkimp0F) prefix(£)
003300020607     D mzimp         E DS                  EXTNAME(mzimp00F)
003400020710     d restokm         s                   like(impkm)
003500020710     d restokm1        s                   like(impkm)
003600020710     d contakm         s                   like(impkm)
003700020610     d contaqta        s                   like(impqta)
003800020710     d restoqt         s                   like(impqta)
003900020710     d restoqt1        s                   like(impqta)
004000020710     d daldat          s                   like(datini)
004100020610     D datini          S              8  0
004200020610     D data1           S              8  0
004300020610     D data2           S              8  0
004400020610     D difgg           S              5  0
004500141219     D ugma            S                   dim(9999) like(daldat)
004600141219     D cm              S              5  0
004700020710     d xx              s                   like(cm)
004800020710     d madata          s              6  0
004900020710     d maini           s                   like(madata)
005000020610     D difkm           S                   like(impkm)
005100020610     D isoimp          S               d
005200020610     D isoini          S               d
005300020730     D DTAISO          S               d
005400020610     d kmini           s                   like(impkm)
005500020610     d qtaini          s                   like(impqta)
005600020610     d nrfs            s                   like(impnrf)
005700020730     d INTER           s                   like(impINT)
005800020607     C*---------------------------------------------------------------*
005900020730     C/EXEC SQL
006000020730     C+ DECLARE A1 CURSOR FOR SELECT * FROM MZIMP00F WHERE (IMPINT = '£1'
006100020730     C+ or IMPINT = 'CB') AND IMPDAT BETWEEN :DATA1 AND :DATA2 ORDER BY
006200020730     C+ IMPNRf, IMPDAT
006300020730     C/END-EXEC
006400020730
006500020730     C/EXEC SQL
006600020730     C+ OPEN A1
006700020730     C/END-EXEC
006800020730
006900020730     C                   dou       *inlr
007000020730
007100020730     C/EXEC SQL
007200020730     C+ FETCH NEXT FROM A1 INTO :mzimp
007300020730     C/END-EXEC
007400020730
007500020730     C                   select
007600020730     C                   when      sqlcod = 100
007700020730     C                   seton                                        lr
007800020730     C                   when      sqlcod = 0
007900160902     c                   if        vkmdal <> 0
008000160902     c                   if        impkm < vkmdal or impkm > vkmal
008100160902     c                   iter
008200160902     c                   end
008300160902     c                   end
008400020730     C                   exsr      calcoli
008500020730     C                   other
008600020730     C                   exsr      sqlerror
008700020730     C                   endsl
008800020730     C*
008900020730     C                   enddo
009000020730
009100020730     C/EXEC SQL
009200020730     C+ CLOSE A1
009300020730     C/END-EXEC
009400020730     C*---------------------------------------------------------------*
009500020730     c     calcoli       begsr
009600020730     C*---------------------------------------------------------------*
009700020717     C* reperisco data, km e consumi precedenti
009800020730     C                   IF        IMPNRf <> NRfs
009900020607     C                   EXSR      SRINI
010000020730     C                   END
010100020607     c*
010200020607     C                   MOVE      IMPDAT        ISOIMP
010300020610     C     ISOimp        SUBDUR    ISOINI        DIFGG:*d
010400020607     C     IMPKM         SUB       KMINI         DIFKM
010500020730     c                   if        difkm >  0
010600020607     C*
010700020611     c                   clear                   wrkimp
010800020610     c                   move      impnrf        £impnrf
010900020611     C                   EVAL      £IMPKM = DIFKM/DIFGG
011000020717     C                   EVAL      £IMPQTA= qtaini/DIFGG
011100020730     C                   MOVE      INTER         £IMPint
011200020705     c*
011300021105     c                   z-add     0             cm
011400021105     c                   clear                   ugma
011500020716     c                   if        £impkm <> 0
011600020607     c                   adddur    1:*d          isoini
011700020610     c                   move      isoini        datini
011800020710     c                   movel     datini        maini
011900020710     c                   clear                   daldat
012000020607     C* spalmo la media nei gg
012100020610     C     datINI        DO        impdat        DATA
012200020607     C                   TEST(D)                 DATA                   02
012300020607     C                   IF        *IN02
012400020607     C                   ITER
012500020607     C                   END
012600020710     c                   movel     data          madata
012700020710     C* conto quanti mesi sto esaminando
012800020710     c                   if        madata <> maini
012900020710     C                   ADD       1             Cm
013000020710     c                   move      daldat        ugma(cm)
013100020716     c                   move      madata        maini
013200020710     c                   end
013300020607     C                   MOVE      DATA          £IMPDAT
013400020710     C                   ADD       £IMPKM        CONTAKM
013500020710     C                   ADD       £IMPqta       contaqta
013600020610     C                   WRITE     WRKIMP00
013700020710     C                   move      £IMPdat       daldat
013800020607     c                   enddo
013900020716     c                   end
014000020710     C                   ADD       1             Cm
014100020716     c                   move      impdat        ugma(cm)
014200020716     C* SE C'È DEL RESTO lo aggiungo nell'ultimo gg di ogni mese
014300020710     C* inserito x il periodo che sto analizzando (da datini a impdat)
014400020717     c* tranne per l'ultimo mese che gli aggiungo anche l'ulteriore resto
014500020611     C                   IF        CONTAKM < difKM
014600020717     C                             or CONTAQTA < qtaini
014700020717     C                   EVAL      restokm=(difKM-CONTAKM)/cm
014800020717     C                   EVAL      restokm1=(difKM-CONTAKM)-(restokm*cm)
014900020717     C                   EVAL      restoqt=(qtaini-CONTAqta)/cm
015000020717     C                   EVAL      restoqt1=(qtaini-CONTAqta)-(restoqt*cm)
015100020710     c* ultimo gg di ogni mese del periodo analizzato
015200020710     c                   do        cm            xx
015300020710     c                   move      ugma(xx)      data
015400020710     c                   move      ugma(xx)      £impdat
015500020710     C     KIMP          CHAIN     WRKIMP1L
015600020710     c* se ancora resto lo aggiungo nell'ultimo gg del periodo analizzato
015700020717     c                   if        cm <> xx
015800020716     C                   EVAL      £IMPKM = £IMPKM + restokm
015900020716     C                   EVAL      £IMPQTA = £IMPQTA + restoqt
016000020710     c                   else
016100020717     C                   EVAL      £IMPKM = £IMPKM + restokm + restokm1
016200020717     C                   EVAL      £IMPQTA = £IMPQTA + restoqt + restoqt1
016300020716     c                   end
016400020710     C                   IF        %FOUND(WRKIMP1L)
016500020710     C                   UPDATE    WRKIMP01
016600020710     c                   else
016700020710     C                   write     WRKIMP00
016800020710     C                   END
016900020710     c                   enddo
017000020710     C                   END
017100020730     C                   END
017200020730     C                   MOVE      IMPDAT        ISOINI
017300020730     C                   MOVE      IMPKM         KMINI
017400020730     C                   MOVE      IMPQTA        QTAINI
017500020730     c                   z-add     0             contakm
017600020730     c                   z-add     0             contaqta
017700020730     C*
017800020730     C                   endsr
017900020710     C*---------------------------------------------------------------*
018000020710     c     srini         begsr
018100020710     C*---------------------------------------------------------------*
018200020730     C* CERCO L'ULTIMO INTERVENTO TRA £1 E CB
018300020730     C                   MOVE      *LOVAL        ISOINI
018400020730     C                   MOVEL     '£1'          IMPINT
018500020607     c     kimp2         setll     mzimp01l
018600020607     c     kimp1         readpe    mzimp01l
018700020610     c                   if        not %eof(mzimp01l)
018800020611     c                   move      $impdat       isoini
018900020611     c                   move      $impkm        kmini
019000021106     c* se il periodo richiesto della statistica è nel 2003 e il
019100021106     c* rifornimento iniziale è nel 2002 il consumo sarà = 0
019200021106     c                   if        anno > 2002 and $impdat <=20021231
019300021106     c                   z-add     0             qtaini
019400021106     c                   else
019500020611     c                   move      $impqta       qtaini
019600020730     c                   END
019700021106     c                   END
019800020730     C                   MOVEL     'CB'          IMPINT
019900020730     c     kimp2         setll     mzimp01l
020000020730     c     kimp1         readpe    mzimp01l
020100020730     c                   if        not %eof(mzimp01l)
020200020730     c                   move      $impdat       DTAISO
020300021210     c                   if        DTAISO > ISOINI
020400020730     c                   move      $impdat       isoini
020500020730     c                   move      $impkm        kmini
020600020730     c                   move      $impqta       qtaini
020700020730     c                   END
020800020730     c                   END
020900020730     C                   IF        ISOINI = *LOVAL
021000141201     c     impnrf        chain     wrkame1l
021100141218     c*                  if        %found(wrkame1l) and amedal > data1
021200141218     c                   if        %found(wrkame1l)
021300020717     c                             and amedal < impdat
021400020717     c                   move      amedal        isoini
021500141218     c                   else
021600141218     c                   move      data1         isoini
021700020717     c                   end
021800020710     c                   subdur    1:*d          isoini
021900020607     c                   z-add     0             kmini
022000020607     c                   z-add     0             qtaini
022100020607     c                   end
022200020730     C                   MOVE      IMPNRf        NRfs
022300020607     c                   endsr
022400020607     C*---------------------------------------------------------------*
022500020607     c     *inzsr        begsr
022600020607     C*---------------------------------------------------------------*
022700020607     C     *ENTRY        PLIST
022800020607     C                   PARM                    KPJBA
022900020607     c                   movel     kpjbu         param
023000020607     C                   Z-ADD     1             IMPNRF
023100020607     C* data inizio
023200020607     c                   if        anno <> 0
023300020607     C                   MOVEl     anno          DATAX             6 0
023400020607     c                   else
023500020607     C                   movel     0001          DATAX
023600020607     c                   end
023700020607     c                   if        mese = 0
023800020607     c                   move      01            datax
023900020607     c                   else
024000020607     C                   MOVE      mese          DATAX
024100020607     c                   end
024200020607     C                   movel     datax         DATA1
024300020607     C                   MOVE      01            DATA1
024400020607     C* data fine
024500021210     c*                  if        anno <> 0
024600021210     C*                  MOVEl     anno          DATAX             6 0
024700021210     c*                  else
024800021210     C*                  movel     9999          DATAX
024900021210     c*                  end
025000021210     c*                  if        mese = 0
025100021210     c*                  move      12            datax
025200021210     c*                  else
025300021210     C*                  MOVE      mese          DATAX
025400021210     c*                  end
025500021210     C*                  movel     datax         DATA2
025600021210     C*                  MOVE      31            DATA2
025700021210     C                   MOVE      99991231      DATA2
025800020607     C* POSIZIONAMENTO SU MZIMP000                         *
025900020730     c                   movel     '£1'          inter
026000020607     C     KIMP          KLIST
026100020607     C                   KFLD                    impNRF
026200020607     C                   KFLD                    INTER
026300020607     C                   KFLD                    DATA              8 0
026400020607     C     KIMP1         KLIST
026500020607     C                   KFLD                    impnrf
026600020730     C                   KFLD                    IMPINT
026700020607     C     KIMP2         KLIST
026800020607     C                   KFLD                    impNRF
026900020730     C                   KFLD                    IMPINT
027000020611     C                   KFLD                    impdat
027100020607     c                   endsr
027200020730     C*---------------------------------------------------------------*
027300020730     C     sqlerror      begsr
027400020730     C*---------------------------------------------------------------*
027500020730     C*
027600020730     C                   call      'X66CHGJOB'
027700020730     C                   seton                                        lr
027800020730     C*
027900020730     C                   endsr
