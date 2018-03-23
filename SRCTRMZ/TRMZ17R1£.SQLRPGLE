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
002500020607     D KPJBA         E DS
002600000000     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
002700020607     D wrkimp        E DS                  EXTNAME(wrkimp0F) prefix(£)
002800020607     D mzimp         E DS                  EXTNAME(mzimp00F)
002900020710     d restokm         s                   like(impkm)
003000020710     d restokm1        s                   like(impkm)
003100020710     d contakm         s                   like(impkm)
003200020610     d contaqta        s                   like(impqta)
003300020710     d restoqt         s                   like(impqta)
003400020710     d restoqt1        s                   like(impqta)
003500020710     d daldat          s                   like(datini)
003600020610     D datini          S              8  0
003700020610     D data1           S              8  0
003800020610     D data2           S              8  0
003900020610     D difgg           S              5  0
004000141219     D ugma            S                   dim(9999) like(daldat)
004100141219     D cm              S              5  0
004200020710     d xx              s                   like(cm)
004300020710     d madata          s              6  0
004400020710     d maini           s                   like(madata)
004500020610     D difkm           S                   like(impkm)
004600020610     D isoimp          S               d
004700020610     D isoini          S               d
004800020730     D DTAISO          S               d
004900020610     d kmini           s                   like(impkm)
005000020610     d qtaini          s                   like(impqta)
005100020610     d nrfs            s                   like(impnrf)
005200020730     d INTER           s                   like(impINT)
005300020607     C*---------------------------------------------------------------*
005400020730     C/EXEC SQL
005500020730     C+ DECLARE A1 CURSOR FOR SELECT * FROM MZIMP00F WHERE (IMPINT = '£1'
005600020730     C+ or IMPINT = 'CB') AND IMPDAT BETWEEN :DATA1 AND :DATA2 ORDER BY
005700020730     C+ IMPNRf, IMPDAT
005800020730     C/END-EXEC
005900020730
006000020730     C/EXEC SQL
006100020730     C+ OPEN A1
006200020730     C/END-EXEC
006300020730
006400020730     C                   dou       *inlr
006500020730
006600020730     C/EXEC SQL
006700020730     C+ FETCH NEXT FROM A1 INTO :mzimp
006800020730     C/END-EXEC
006900020730
007000020730     C                   select
007100020730     C                   when      sqlcod = 100
007200020730     C                   seton                                        lr
007300020730     C                   when      sqlcod = 0
007400020730     C                   exsr      calcoli
007500020730     C                   other
007600020730     C                   exsr      sqlerror
007700020730     C                   endsl
007800020730     C*
007900020730     C                   enddo
008000020730
008100020730     C/EXEC SQL
008200020730     C+ CLOSE A1
008300020730     C/END-EXEC
008400020730     C*---------------------------------------------------------------*
008500020730     c     calcoli       begsr
008600020730     C*---------------------------------------------------------------*
008700020717     C* reperisco data, km e consumi precedenti
008800020730     C                   IF        IMPNRf <> NRfs
008900020607     C                   EXSR      SRINI
009000020730     C                   END
009100020607     c*
009200020607     C                   MOVE      IMPDAT        ISOIMP
009300020610     C     ISOimp        SUBDUR    ISOINI        DIFGG:*d
009400020607     C     IMPKM         SUB       KMINI         DIFKM
009500020730     c                   if        difkm >  0
009600020607     C*
009700020611     c                   clear                   wrkimp
009800020610     c                   move      impnrf        £impnrf
009900020611     C                   EVAL      £IMPKM = DIFKM/DIFGG
010000020717     C                   EVAL      £IMPQTA= qtaini/DIFGG
010100020730     C                   MOVE      INTER         £IMPint
010200020705     c*
010300021105     c                   z-add     0             cm
010400021105     c                   clear                   ugma
010500020716     c                   if        £impkm <> 0
010600020607     c                   adddur    1:*d          isoini
010700020610     c                   move      isoini        datini
010800020710     c                   movel     datini        maini
010900020710     c                   clear                   daldat
011000020607     C* spalmo la media nei gg
011100020610     C     datINI        DO        impdat        DATA
011200020607     C                   TEST(D)                 DATA                   02
011300020607     C                   IF        *IN02
011400020607     C                   ITER
011500020607     C                   END
011600020710     c                   movel     data          madata
011700020710     C* conto quanti mesi sto esaminando
011800020710     c                   if        madata <> maini
011900020710     C                   ADD       1             Cm
012000020710     c                   move      daldat        ugma(cm)
012100020716     c                   move      madata        maini
012200020710     c                   end
012300020607     C                   MOVE      DATA          £IMPDAT
012400020710     C                   ADD       £IMPKM        CONTAKM
012500020710     C                   ADD       £IMPqta       contaqta
012600020610     C                   WRITE     WRKIMP00
012700020710     C                   move      £IMPdat       daldat
012800020607     c                   enddo
012900020716     c                   end
013000020710     C                   ADD       1             Cm
013100020716     c                   move      impdat        ugma(cm)
013200020716     C* SE C'È DEL RESTO lo aggiungo nell'ultimo gg di ogni mese
013300020710     C* inserito x il periodo che sto analizzando (da datini a impdat)
013400020717     c* tranne per l'ultimo mese che gli aggiungo anche l'ulteriore resto
013500020611     C                   IF        CONTAKM < difKM
013600020717     C                             or CONTAQTA < qtaini
013700020717     C                   EVAL      restokm=(difKM-CONTAKM)/cm
013800020717     C                   EVAL      restokm1=(difKM-CONTAKM)-(restokm*cm)
013900020717     C                   EVAL      restoqt=(qtaini-CONTAqta)/cm
014000020717     C                   EVAL      restoqt1=(qtaini-CONTAqta)-(restoqt*cm)
014100020710     c* ultimo gg di ogni mese del periodo analizzato
014200020710     c                   do        cm            xx
014300020710     c                   move      ugma(xx)      data
014400020710     c                   move      ugma(xx)      £impdat
014500020710     C     KIMP          CHAIN     WRKIMP1L
014600020710     c* se ancora resto lo aggiungo nell'ultimo gg del periodo analizzato
014700020717     c                   if        cm <> xx
014800020716     C                   EVAL      £IMPKM = £IMPKM + restokm
014900020716     C                   EVAL      £IMPQTA = £IMPQTA + restoqt
015000020710     c                   else
015100020717     C                   EVAL      £IMPKM = £IMPKM + restokm + restokm1
015200020717     C                   EVAL      £IMPQTA = £IMPQTA + restoqt + restoqt1
015300020716     c                   end
015400020710     C                   IF        %FOUND(WRKIMP1L)
015500020710     C                   UPDATE    WRKIMP01
015600020710     c                   else
015700020710     C                   write     WRKIMP00
015800020710     C                   END
015900020710     c                   enddo
016000020710     C                   END
016100020730     C                   END
016200020730     C                   MOVE      IMPDAT        ISOINI
016300020730     C                   MOVE      IMPKM         KMINI
016400020730     C                   MOVE      IMPQTA        QTAINI
016500020730     c                   z-add     0             contakm
016600020730     c                   z-add     0             contaqta
016700020730     C*
016800020730     C                   endsr
016900020710     C*---------------------------------------------------------------*
017000020710     c     srini         begsr
017100020710     C*---------------------------------------------------------------*
017200020730     C* CERCO L'ULTIMO INTERVENTO TRA £1 E CB
017300020730     C                   MOVE      *LOVAL        ISOINI
017400020730     C                   MOVEL     '£1'          IMPINT
017500020607     c     kimp2         setll     mzimp01l
017600020607     c     kimp1         readpe    mzimp01l
017700020610     c                   if        not %eof(mzimp01l)
017800020611     c                   move      $impdat       isoini
017900020611     c                   move      $impkm        kmini
018000021106     c* se il periodo richiesto della statistica è nel 2003 e il
018100021106     c* rifornimento iniziale è nel 2002 il consumo sarà = 0
018200021106     c                   if        anno > 2002 and $impdat <=20021231
018300021106     c                   z-add     0             qtaini
018400021106     c                   else
018500020611     c                   move      $impqta       qtaini
018600020730     c                   END
018700021106     c                   END
018800020730     C                   MOVEL     'CB'          IMPINT
018900020730     c     kimp2         setll     mzimp01l
019000020730     c     kimp1         readpe    mzimp01l
019100020730     c                   if        not %eof(mzimp01l)
019200020730     c                   move      $impdat       DTAISO
019300021210     c                   if        DTAISO > ISOINI
019400020730     c                   move      $impdat       isoini
019500020730     c                   move      $impkm        kmini
019600020730     c                   move      $impqta       qtaini
019700020730     c                   END
019800020730     c                   END
019900020730     C                   IF        ISOINI = *LOVAL
020000141201     c     impnrf        chain     wrkame1l
020100141218     c*                  if        %found(wrkame1l) and amedal > data1
020200141218     c                   if        %found(wrkame1l)
020300020717     c                             and amedal < impdat
020400020717     c                   move      amedal        isoini
020500141218     c                   else
020600141218     c                   move      data1         isoini
020700020717     c                   end
020800020710     c                   subdur    1:*d          isoini
020900020607     c                   z-add     0             kmini
021000020607     c                   z-add     0             qtaini
021100020607     c                   end
021200020730     C                   MOVE      IMPNRf        NRfs
021300020607     c                   endsr
021400020607     C*---------------------------------------------------------------*
021500020607     c     *inzsr        begsr
021600020607     C*---------------------------------------------------------------*
021700020607     C     *ENTRY        PLIST
021800020607     C                   PARM                    KPJBA
021900020607     c                   movel     kpjbu         param
022000020607     C                   Z-ADD     1             IMPNRF
022100020607     C* data inizio
022200020607     c                   if        anno <> 0
022300020607     C                   MOVEl     anno          DATAX             6 0
022400020607     c                   else
022500020607     C                   movel     0001          DATAX
022600020607     c                   end
022700020607     c                   if        mese = 0
022800020607     c                   move      01            datax
022900020607     c                   else
023000020607     C                   MOVE      mese          DATAX
023100020607     c                   end
023200020607     C                   movel     datax         DATA1
023300020607     C                   MOVE      01            DATA1
023400020607     C* data fine
023500021210     c*                  if        anno <> 0
023600021210     C*                  MOVEl     anno          DATAX             6 0
023700021210     c*                  else
023800021210     C*                  movel     9999          DATAX
023900021210     c*                  end
024000021210     c*                  if        mese = 0
024100021210     c*                  move      12            datax
024200021210     c*                  else
024300021210     C*                  MOVE      mese          DATAX
024400021210     c*                  end
024500021210     C*                  movel     datax         DATA2
024600021210     C*                  MOVE      31            DATA2
024700021210     C                   MOVE      99991231      DATA2
024800020607     C* POSIZIONAMENTO SU MZIMP000                         *
024900020730     c                   movel     '£1'          inter
025000020607     C     KIMP          KLIST
025100020607     C                   KFLD                    impNRF
025200020607     C                   KFLD                    INTER
025300020607     C                   KFLD                    DATA              8 0
025400020607     C     KIMP1         KLIST
025500020607     C                   KFLD                    impnrf
025600020730     C                   KFLD                    IMPINT
025700020607     C     KIMP2         KLIST
025800020607     C                   KFLD                    impNRF
025900020730     C                   KFLD                    IMPINT
026000020611     C                   KFLD                    impdat
026100020607     c                   endsr
026200020730     C*---------------------------------------------------------------*
026300020730     C     sqlerror      begsr
026400020730     C*---------------------------------------------------------------*
026500020730     C*
026600020730     C                   call      'X66CHGJOB'
026700020730     C                   seton                                        lr
026800020730     C*
026900020730     C                   endsr
