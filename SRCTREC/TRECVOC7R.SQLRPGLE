000100071228     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200071228
000300160629     ftabel00f  if   e           k disk
000400160629     fazorg01l  if   e           k disk
000500160630     feccev30c  if   e           k disk
000600160630     fecc2v30c  if   e           k disk
000700160628     fwfvocp1l  uf a e           k disk    usropn
000800071228
000900071228      //-----------------------------------------------------------------------
001000071228      //? VARIABILI
001100071228      //-----------------------------------------------------------------------
001200071228     d codut           s              1  0 inz(1)
001300071228     d lengh           s             15  5 inz(80)
001400160628     d kcod            s                   like(tblcod)
001500160628     d kkey            s                   like(tblkey)
001600080103     d pdataal         s             10
001700080103     d pdatada         s             10
001800160630     d evdcli          s              7  0 inz
001900160630     d evdvoc          s              3  0 inz
002000160630     d xx              s              3  0 inz
002100160629     d wdataspe        s                   like(VOCPdal)
002200160629     d wdate           s                   like(VOCPal)
002300071228     d wtime           s             14  0
002400071228      //-----------------------------------------------------------------------
002500071228      //? SCHIERE
002600071228      //-----------------------------------------------------------------------
002700071228     d cmd             s             80    dim(1) ctdata perrcd(1)
002800160628     d VOC             s              3  0 dim(10) ctdata perrcd(1)
002900160630     d imp             s             13  5 dim(10)
003000071228
003100071228      //-----------------------------------------------------------------------
003200071228      //? STRUTTURE DATI
003300071228      //-----------------------------------------------------------------------
003400071228     d param           ds
003500071228     d  waada                  1      4  0
003600071228     d  wmgda                  5      8  0
003700071228     d  wmmda                  5      6  0
003800160629     d  wggda                  7      8  0
003900071228     d  wdatada                1      8  0
004000160629     d  da                     1      8
004100071228     d  waaal                  9     12  0
004200071228     d  wmmal                 13     14  0
004300071228     d  wggal                 15     16  0
004400071228     d  wdataal                9     16  0
004500160629     d  a                      9     16
004600160704     d  anno                  17     20  0
004700160722     d  datarif               21     28  0
004800071228
004900071228     d ds_cnaco      e ds                  inz extname(cnaco00f)
005000071228     d ds_cnind      e ds                  inz extname(cnind00f)
005100071228     d ds_cnclp      e ds                  inz extname(cnclp00f)
005200071228     d ds_fncls      e ds                  inz extname(fncls00f)
005300160630     d titasds       e ds                  inz extname(titas10f)
005400071228     d kpjba         e ds
005500071228     d tibs10ds      e ds
005600071228     d tibs69ds      e ds
005700160701     d ds$3          e ds
005800071228
005900071228      //-----------------------------------------------------------------------
006000160701     c                   eval      *inlr = *on
006100160701      * clrpfm del file di work
006200160701     c                   call(e)   'QCMDEXC'
006300160701     c                   parm                    cmd(1)
006400160701     c                   parm                    lengh
006500160701      * se riuscito comando vado avanti con l'elaborazione
006600160701     c                   if        %error
006700160701     c                   return
006800160701     c                   end
006900160701     c                   open      wfvocp1l
007000160701     c* tasp o tas0
007100160701     c                   if        §$3D10>=wdataal
007200160630     C/EXEC SQL
007300160705     C+ DECLARE B1 CURSOR FOR SELECT * FROM titasp0f WHERE
007400160704     C+ digits(tasaas) concat digits(tasmgs) between :da and :a
007500160630     C/END-EXEC
007600160630
007700160630     C/EXEC SQL
007800160630     C+ OPEN b1
007900160630     C/END-EXEC
008000160701     c                   else
008100160701     C/EXEC SQL
008200160701     C+ DECLARE B2 CURSOR FOR SELECT * FROM titas10f WHERE digits(tasaas)
008300160704     C+ concat digits(tasmgs) between :da and :a
008400160701     C/END-EXEC
008500160701
008600160701     C/EXEC SQL
008700160701     C+ OPEN b2
008800160701     C/END-EXEC
008900160701     c                   end
009000160701
009100160630     C                   DO        *HIVAL
009200160630
009300160701     c* tasp o tas0
009400160701     c                   if        §$3D10>=wdataal
009500160630     C/EXEC SQL
009600160630     C+ FETCH NEXT FROM b1 INTO :titasds
009700160630     C/END-EXEC
009800160701     c                   else
009900160701     C/EXEC SQL
010000160701     C+ FETCH NEXT FROM b2 INTO :titasds
010100160701     C/END-EXEC
010200160701     c                   end
010300160701
010400160630     C                   IF        SQLCod = 100 or sqlcod < 0
010500160630     c                   clear                   sqlcod
010600160630     C                   LEAVE
010700160630     C                   ENDIF
010800160630     c*
010900160630     c     kcev          chain     eccev30c
011000160630     c                   if        not %found(eccev30c)
011100160630     c                   iter
011200160630     c                   end
011300160630     c     kcev          chain     ecc2v30c
011400160630     C                   CLEAR                   IMP
011500160630
011600160628     c                   EXSR      conta
011700160630     c                   if        %found(ecc2v30c)
011800160630     c                   EXSR      conta2
011900160630     C                   END
012000160628      *       scrivo nuovo record
012100160628     c                   exsr      sr_scrivi
012200160628     C                   ENDDO
012300160628
012400160701     c* tasp o tas0
012500160701     c                   if        §$3D10>=wdataal
012600160628     C/EXEC SQL
012700160630     C+ CLOSE b1
012800160628     C/END-EXEC
012900160701     c                   else
013000160701     C/EXEC SQL
013100160701     C+ CLOSE b2
013200160701     C/END-EXEC
013300160701     c                   end
013400160628     C*--------------------------------------------------------------- ---------
013500160628     C     conta         BEGSR
013600160630     C*--------------------------------------------------------------- ---------
013700160704     c                   clear                   imptot           13 5
013800160704     c                   z-add     1             xx
013900160704     c     cevc01        lookup    voc(xx)                                01
014000160704     c                   if        *in01
014100160628     c                   add       cevi01        IMP(XX)
014200160704     c                   else
014300160704     c                   add       cevi01        imptot
014400160628     c                   end
014500160704     c                   z-add     1             xx
014600160704     c     cevc02        lookup    voc(xx)                                01
014700160704     c                   if        *in01
014800160628     c                   add       cevi02        IMP(XX)
014900160704     c                   else
015000160704     c                   add       cevi02        imptot
015100160628     c                   end
015200160704     c                   z-add     1             xx
015300160704     c     cevc03        lookup    voc(xx)                                01
015400160704     c                   if        *in01
015500160628     c                   add       cevi03        IMP(XX)
015600160704     c                   else
015700160704     c                   add       cevi03        imptot
015800160628     c                   end
015900160704     c                   z-add     1             xx
016000160704     c     cevc04        lookup    voc(xx)                                01
016100160704     c                   if        *in01
016200160628     c                   add       cevi04        IMP(XX)
016300160704     c                   else
016400160704     c                   add       cevi04        imptot
016500160628     c                   end
016600160704     c                   z-add     1             xx
016700160704     c     cevc05        lookup    voc(xx)                                01
016800160704     c                   if        *in01
016900160628     c                   add       cevi05        IMP(XX)
017000160704     c                   else
017100160704     c                   add       cevi05        imptot
017200160628     c                   end
017300160704     c                   z-add     1             xx
017400160704     c     cevc06        lookup    voc(xx)                                01
017500160704     c                   if        *in01
017600160628     c                   add       cevi06        IMP(XX)
017700160704     c                   else
017800160704     c                   add       cevi06        imptot
017900160628     c                   end
018000160704     c                   z-add     1             xx
018100160704     c     cevc07        lookup    voc(xx)                                01
018200160704     c                   if        *in01
018300160628     c                   add       cevi07        IMP(XX)
018400160704     c                   else
018500160704     c                   add       cevi07        imptot
018600160628     c                   end
018700160704     c                   z-add     1             xx
018800160704     c     cevc08        lookup    voc(xx)                                01
018900160704     c                   if        *in01
019000160628     c                   add       cevi08        IMP(XX)
019100160704     c                   else
019200160704     c                   add       cevi08        imptot
019300160628     c                   end
019400160704     c                   z-add     1             xx
019500160704     c     cevc09        lookup    voc(xx)                                01
019600160704     c                   if        *in01
019700160628     c                   add       cevi09        IMP(XX)
019800160704     c                   else
019900160704     c                   add       cevi09        imptot
020000160628     c                   end
020100160704     c                   z-add     1             xx
020200160704     c     cevc10        lookup    voc(xx)                                01
020300160704     c                   if        *in01
020400160628     c                   add       cevi10        IMP(XX)
020500160704     c                   else
020600160704     c                   add       cevi10        imptot
020700160628     c                   end
020800160704     c                   z-add     1             xx
020900160704     c     cevc11        lookup    voc(xx)                                01
021000160704     c                   if        *in01
021100160704     c                   add       cevi11        IMP(XX)
021200160704     c                   else
021300160704     c                   add       cevi11        imptot
021400160704     c                   end
021500160704     c                   z-add     1             xx
021600160704     c     cevc12        lookup    voc(xx)                                01
021700160704     c                   if        *in01
021800160704     c                   add       cevi12        IMP(XX)
021900160704     c                   else
022000160704     c                   add       cevi12        imptot
022100160628     c                   end
022200160704     c                   z-add     1             xx
022300160704     c     cevc13        lookup    voc(xx)                                01
022400160704     c                   if        *in01
022500160704     c                   add       cevi13        IMP(XX)
022600160704     c                   else
022700160704     c                   add       cevi13        imptot
022800160628     c                   end
022900160704     c                   z-add     1             xx
023000160704     c     cevc14        lookup    voc(xx)                                01
023100160704     c                   if        *in01
023200160704     c                   add       cevi14        IMP(XX)
023300160704     c                   else
023400160704     c                   add       cevi14        imptot
023500160628     c                   end
023600160704     c                   z-add     1             xx
023700160704     c     cevc15        lookup    voc(xx)                                01
023800160704     c                   if        *in01
023900160704     c                   add       cevi15        IMP(XX)
024000160704     c                   else
024100160704     c                   add       cevi15        imptot
024200160628     c                   end
024300160704     c                   z-add     1             xx
024400160704     c     cevc16        lookup    voc(xx)                                01
024500160704     c                   if        *in01
024600160704     c                   add       cevi16        IMP(XX)
024700160704     c                   else
024800160704     c                   add       cevi16        imptot
024900160628     c                   end
025000160704     c                   z-add     1             xx
025100160704     c     cevc17        lookup    voc(xx)                                01
025200160704     c                   if        *in01
025300160704     c                   add       cevi17        IMP(XX)
025400160704     c                   else
025500160704     c                   add       cevi17        imptot
025600160628     c                   end
025700160704     c                   z-add     1             xx
025800160704     c     cevc18        lookup    voc(xx)                                01
025900160704     c                   if        *in01
026000160704     c                   add       cevi18        IMP(XX)
026100160704     c                   else
026200160704     c                   add       cevi18        imptot
026300160628     c                   end
026400160704     c                   z-add     1             xx
026500160704     c     cevc19        lookup    voc(xx)                                01
026600160704     c                   if        *in01
026700160628     c                   add       cevi19        IMP(XX)
026800160704     c                   else
026900160704     c                   add       cevi19        imptot
027000160628     c                   end
027100160704     c                   z-add     1             xx
027200160704     c     cevc20        lookup    voc(xx)                                01
027300160704     c                   if        *in01
027400160628     c                   add       cevi20        IMP(XX)
027500160704     c                   else
027600160704     c                   add       cevi20        imptot
027700160628     c                   end
027800160704     c                   z-add     1             xx
027900160704     c     cevc21        lookup    voc(xx)                                01
028000160704     c                   if        *in01
028100160628     c                   add       cevi21        IMP(XX)
028200160704     c                   else
028300160704     c                   add       cevi21        imptot
028400160628     c                   end
028500160704     c                   z-add     1             xx
028600160704     c     cevc22        lookup    voc(xx)                                01
028700160704     c                   if        *in01
028800160628     c                   add       cevi22        IMP(XX)
028900160704     c                   else
029000160704     c                   add       cevi22        imptot
029100160628     c                   end
029200160704     c                   z-add     1             xx
029300160704     c     cevc23        lookup    voc(xx)                                01
029400160704     c                   if        *in01
029500160628     c                   add       cevi23        IMP(XX)
029600160704     c                   else
029700160704     c                   add       cevi23        imptot
029800160628     c                   end
029900160704     c                   z-add     1             xx
030000160704     c     cevc24        lookup    voc(xx)                                01
030100160704     c                   if        *in01
030200160628     c                   add       cevi24        IMP(XX)
030300160704     c                   else
030400160704     c                   add       cevi24        imptot
030500160628     c                   end
030600160704     c                   z-add     1             xx
030700160704     c     cevc25        lookup    voc(xx)                                01
030800160704     c                   if        *in01
030900160628     c                   add       cevi25        IMP(XX)
031000160704     c                   else
031100160704     c                   add       cevi25        imptot
031200160628     c                   end
031300160704     c                   z-add     1             xx
031400160704     c     cevc26        lookup    voc(xx)                                01
031500160704     c                   if        *in01
031600160628     c                   add       cevi26        IMP(XX)
031700160704     c                   else
031800160704     c                   add       cevi26        imptot
031900160628     c                   end
032000160704     c                   z-add     1             xx
032100160704     c     cevc27        lookup    voc(xx)                                01
032200160704     c                   if        *in01
032300160628     c                   add       cevi27        IMP(XX)
032400160704     c                   else
032500160704     c                   add       cevi27        imptot
032600160628     c                   end
032700160704     c                   z-add     1             xx
032800160704     c     cevc28        lookup    voc(xx)                                01
032900160704     c                   if        *in01
033000160628     c                   add       cevi28        IMP(XX)
033100160704     c                   else
033200160704     c                   add       cevi28        imptot
033300160628     c                   end
033400160704     c                   z-add     1             xx
033500160704     c     cevc29        lookup    voc(xx)                                01
033600160704     c                   if        *in01
033700160628     c                   add       cevi29        IMP(XX)
033800160704     c                   else
033900160704     c                   add       cevi29        imptot
034000160628     c                   end
034100160704     c                   z-add     1             xx
034200160704     c     cevc30        lookup    voc(xx)                                01
034300160704     c                   if        *in01
034400160628     c                   add       cevi30        IMP(XX)
034500160704     c                   else
034600160704     c                   add       cevi30        imptot
034700160628     c                   end
034800160704     c                   z-add     1             xx
034900160704     c     cevc31        lookup    voc(xx)                                01
035000160704     c                   if        *in01
035100160628     c                   add       cevi31        IMP(XX)
035200160704     c                   else
035300160704     c                   add       cevi31        imptot
035400160628     c                   end
035500160704     c                   z-add     1             xx
035600160704     c     cevc32        lookup    voc(xx)                                01
035700160704     c                   if        *in01
035800160628     c                   add       cevi32        IMP(XX)
035900160704     c                   else
036000160704     c                   add       cevi32        imptot
036100160628     c                   end
036200160704     c                   z-add     1             xx
036300160704     c     cevc33        lookup    voc(xx)                                01
036400160704     c                   if        *in01
036500160628     c                   add       cevi33        IMP(XX)
036600160704     c                   else
036700160704     c                   add       cevi33        imptot
036800160628     c                   end
036900160704     c                   z-add     1             xx
037000160704     c     cevc34        lookup    voc(xx)                                01
037100160704     c                   if        *in01
037200160628     c                   add       cevi34        IMP(XX)
037300160704     c                   else
037400160704     c                   add       cevi34        imptot
037500160628     c                   end
037600160704     c                   z-add     1             xx
037700160704     c     cevc35        lookup    voc(xx)                                01
037800160704     c                   if        *in01
037900160628     c                   add       cevi35        IMP(XX)
038000160704     c                   else
038100160704     c                   add       cevi35        imptot
038200160628     c                   end
038300160704     c                   z-add     1             xx
038400160704     c     cevc36        lookup    voc(xx)                                01
038500160704     c                   if        *in01
038600160628     c                   add       cevi36        IMP(XX)
038700160704     c                   else
038800160704     c                   add       cevi36        imptot
038900160628     c                   end
039000160704     c                   z-add     1             xx
039100160704     c     cevc37        lookup    voc(xx)                                01
039200160704     c                   if        *in01
039300160628     c                   add       cevi37        IMP(XX)
039400160704     c                   else
039500160704     c                   add       cevi37        imptot
039600160628     c                   end
039700160704     c                   z-add     1             xx
039800160704     c     cevc38        lookup    voc(xx)                                01
039900160704     c                   if        *in01
040000160628     c                   add       cevi38        IMP(XX)
040100160704     c                   else
040200160704     c                   add       cevi38        imptot
040300160628     c                   end
040400160704     c                   z-add     1             xx
040500160704     c     cevc39        lookup    voc(xx)                                01
040600160704     c                   if        *in01
040700160628     c                   add       cevi39        IMP(XX)
040800160704     c                   else
040900160704     c                   add       cevi39        imptot
041000160628     c                   end
041100160704     c                   z-add     1             xx
041200160704     c     cevc40        lookup    voc(xx)                                01
041300160704     c                   if        *in01
041400160628     c                   add       cevi40        IMP(XX)
041500160704     c                   else
041600160704     c                   add       cevi40        imptot
041700160628     c                   end
041800160704     c                   z-add     1             xx
041900160704     c     cevc41        lookup    voc(xx)                                01
042000160704     c                   if        *in01
042100160628     c                   add       cevi41        IMP(XX)
042200160704     c                   else
042300160704     c                   add       cevi41        imptot
042400160628     c                   end
042500160704     c                   z-add     1             xx
042600160704     c     cevc42        lookup    voc(xx)                                01
042700160704     c                   if        *in01
042800160628     c                   add       cevi42        IMP(XX)
042900160704     c                   else
043000160704     c                   add       cevi42        imptot
043100160628     c                   end
043200160704     c                   z-add     1             xx
043300160704     c     cevc43        lookup    voc(xx)                                01
043400160704     c                   if        *in01
043500160628     c                   add       cevi43        IMP(XX)
043600160704     c                   else
043700160704     c                   add       cevi43        imptot
043800160628     c                   end
043900160704     c                   z-add     1             xx
044000160704     c     cevc44        lookup    voc(xx)                                01
044100160704     c                   if        *in01
044200160628     c                   add       cevi44        IMP(XX)
044300160704     c                   else
044400160704     c                   add       cevi44        imptot
044500160628     c                   end
044600160704     c                   z-add     1             xx
044700160704     c     cevc45        lookup    voc(xx)                                01
044800160704     c                   if        *in01
044900160628     c                   add       cevi45        IMP(XX)
045000160704     c                   else
045100160704     c                   add       cevi45        imptot
045200160628     c                   end
045300160704     c                   z-add     1             xx
045400160704     c     cevc46        lookup    voc(xx)                                01
045500160704     c                   if        *in01
045600160628     c                   add       cevi46        IMP(XX)
045700160704     c                   else
045800160704     c                   add       cevi46        imptot
045900160704     c                   end
046000160704     c                   z-add     1             xx
046100160704     c     cevc47        lookup    voc(xx)                                01
046200160704     c                   if        *in01
046300160628     c                   add       cevi47        IMP(XX)
046400160704     c                   else
046500160704     c                   add       cevi47        imptot
046600160628     c                   end
046700160704     c                   z-add     1             xx
046800160704     c     cevc48        lookup    voc(xx)                                01
046900160704     c                   if        *in01
047000160628     c                   add       cevi48        IMP(XX)
047100160704     c                   else
047200160704     c                   add       cevi48        imptot
047300160628     c                   end
047400160704     c                   z-add     1             xx
047500160704     c     cevc49        lookup    voc(xx)                                01
047600160704     c                   if        *in01
047700160628     c                   add       cevi49        IMP(XX)
047800160704     c                   else
047900160704     c                   add       cevi49        imptot
048000160628     c                   end
048100160704     c                   z-add     1             xx
048200160704     c     cevc50        lookup    voc(xx)                                01
048300160704     c                   if        *in01
048400160628     c                   add       cevi50        IMP(XX)
048500160704     c                   else
048600160704     c                   add       cevi50        imptot
048700160628     c                   end
048800160628     C                   ENDSR
048900160629     C*--------------------------------------------------------------- ---------
049000160629     C     conta2        BEGSR
049100160629     C*--------------------------------------------------------------- ---------
049200160704     c                   z-add     1             xx
049300160704     c     c2vc51        lookup    voc(xx)                                01
049400160704     c                   if        *in01
049500160629     c                   add       C2Vi51        imp(xx)
049600160704     c                   else
049700160704     c                   add       c2vi51        imptot
049800160629     c                   end
049900160704     c                   z-add     1             xx
050000160704     c     c2vc52        lookup    voc(xx)                                01
050100160704     c                   if        *in01
050200160629     c                   add       C2Vi52        imp(xx)
050300160704     c                   else
050400160704     c                   add       c2vi52        imptot
050500160629     c                   end
050600160704     c                   z-add     1             xx
050700160704     c     c2vc53        lookup    voc(xx)                                01
050800160704     c                   if        *in01
050900160629     c                   add       C2Vi53        imp(xx)
051000160704     c                   else
051100160704     c                   add       c2vi53        imptot
051200160629     c                   end
051300160704     c                   z-add     1             xx
051400160704     c     c2vc54        lookup    voc(xx)                                01
051500160704     c                   if        *in01
051600160629     c                   add       C2Vi54        imp(xx)
051700160704     c                   else
051800160704     c                   add       c2vi54        imptot
051900160629     c                   end
052000160704     c                   z-add     1             xx
052100160704     c     c2vc55        lookup    voc(xx)                                01
052200160704     c                   if        *in01
052300160629     c                   add       C2Vi55        imp(xx)
052400160704     c                   else
052500160704     c                   add       c2vi55        imptot
052600160629     c                   end
052700160704     c                   z-add     1             xx
052800160704     c     c2vc56        lookup    voc(xx)                                01
052900160704     c                   if        *in01
053000160629     c                   add       C2Vi56        imp(xx)
053100160704     c                   else
053200160704     c                   add       c2vi56        imptot
053300160629     c                   end
053400160704     c                   z-add     1             xx
053500160704     c     c2vc57        lookup    voc(xx)                                01
053600160704     c                   if        *in01
053700160629     c                   add       C2Vi57        imp(xx)
053800160704     c                   else
053900160704     c                   add       c2vi57        imptot
054000160629     c                   end
054100160704     c                   z-add     1             xx
054200160704     c     c2vc58        lookup    voc(xx)                                01
054300160704     c                   if        *in01
054400160629     c                   add       C2Vi58        imp(xx)
054500160704     c                   else
054600160704     c                   add       c2vi58        imptot
054700160629     c                   end
054800160704     c                   z-add     1             xx
054900160704     c     c2vc59        lookup    voc(xx)                                01
055000160704     c                   if        *in01
055100160629     c                   add       C2Vi59        imp(xx)
055200160704     c                   else
055300160704     c                   add       c2vi59        imptot
055400160629     c                   end
055500160704     c                   z-add     1             xx
055600160704     c     c2vc60        lookup    voc(xx)                                01
055700160704     c                   if        *in01
055800160629     c                   add       C2Vi60        imp(xx)
055900160704     c                   else
056000160704     c                   add       c2vi60        imptot
056100160629     c                   end
056200160704     c                   z-add     1             xx
056300160704     c     c2vc61        lookup    voc(xx)                                01
056400160704     c                   if        *in01
056500160629     c                   add       C2Vi61        imp(xx)
056600160704     c                   else
056700160704     c                   add       c2vi61        imptot
056800160629     c                   end
056900160704     c                   z-add     1             xx
057000160704     c     c2vc62        lookup    voc(xx)                                01
057100160704     c                   if        *in01
057200160629     c                   add       C2Vi62        imp(xx)
057300160704     c                   else
057400160704     c                   add       c2vi62        imptot
057500160629     c                   end
057600160704     c                   z-add     1             xx
057700160704     c     c2vc63        lookup    voc(xx)                                01
057800160704     c                   if        *in01
057900160629     c                   add       C2Vi63        imp(xx)
058000160704     c                   else
058100160704     c                   add       c2vi63        imptot
058200160629     c                   end
058300160704     c                   z-add     1             xx
058400160704     c     c2vc64        lookup    voc(xx)                                01
058500160704     c                   if        *in01
058600160629     c                   add       C2Vi64        imp(xx)
058700160704     c                   else
058800160704     c                   add       c2vi64        imptot
058900160629     c                   end
059000160704     c                   z-add     1             xx
059100160704     c     c2vc65        lookup    voc(xx)                                01
059200160704     c                   if        *in01
059300160629     c                   add       C2Vi65        imp(xx)
059400160704     c                   else
059500160704     c                   add       c2vi65        imptot
059600160629     c                   end
059700160704     c                   z-add     1             xx
059800160704     c     c2vc66        lookup    voc(xx)                                01
059900160704     c                   if        *in01
060000160629     c                   add       C2Vi66        imp(xx)
060100160704     c                   else
060200160704     c                   add       c2vi66        imptot
060300160629     c                   end
060400160704     c                   z-add     1             xx
060500160704     c     c2vc67        lookup    voc(xx)                                01
060600160704     c                   if        *in01
060700160629     c                   add       C2Vi67        imp(xx)
060800160704     c                   else
060900160704     c                   add       c2vi67        imptot
061000160629     c                   end
061100160704     c                   z-add     1             xx
061200160704     c     c2vc68        lookup    voc(xx)                                01
061300160704     c                   if        *in01
061400160629     c                   add       C2Vi68        imp(xx)
061500160704     c                   else
061600160704     c                   add       c2vi68        imptot
061700160629     c                   end
061800160704     c                   z-add     1             xx
061900160704     c     c2vc69        lookup    voc(xx)                                01
062000160704     c                   if        *in01
062100160629     c                   add       C2Vi69        imp(xx)
062200160704     c                   else
062300160704     c                   add       c2vi69        imptot
062400160629     c                   end
062500160704     c                   z-add     1             xx
062600160704     c     c2vc70        lookup    voc(xx)                                01
062700160704     c                   if        *in01
062800160629     c                   add       C2Vi70        imp(xx)
062900160704     c                   else
063000160704     c                   add       c2vi70        imptot
063100160629     c                   end
063200160704     c                   z-add     1             xx
063300160704     c     c2vc71        lookup    voc(xx)                                01
063400160704     c                   if        *in01
063500160629     c                   add       C2Vi71        imp(xx)
063600160704     c                   else
063700160704     c                   add       c2vi71        imptot
063800160629     c                   end
063900160704     c                   z-add     1             xx
064000160704     c     c2vc72        lookup    voc(xx)                                01
064100160704     c                   if        *in01
064200160629     c                   add       C2Vi72        imp(xx)
064300160704     c                   else
064400160704     c                   add       c2vi72        imptot
064500160629     c                   end
064600160704     c                   z-add     1             xx
064700160704     c     c2vc73        lookup    voc(xx)                                01
064800160704     c                   if        *in01
064900160629     c                   add       C2Vi73        imp(xx)
065000160704     c                   else
065100160704     c                   add       c2vi73        imptot
065200160629     c                   end
065300160704     c                   z-add     1             xx
065400160704     c     c2vc74        lookup    voc(xx)                                01
065500160704     c                   if        *in01
065600160629     c                   add       C2Vi74        imp(xx)
065700160704     c                   else
065800160704     c                   add       c2vi74        imptot
065900160629     c                   end
066000160704     c                   z-add     1             xx
066100160704     c     c2vc75        lookup    voc(xx)                                01
066200160704     c                   if        *in01
066300160629     c                   add       C2Vi75        imp(xx)
066400160704     c                   else
066500160704     c                   add       c2vi75        imptot
066600160629     c                   end
066700160629     C                   ENDSR
066800071228      //-----------------------------------------------------------------------
066900071228      //? Scrivo il file di work
067000071228      //-----------------------------------------------------------------------
067100071228     c     sr_scrivi     begsr
067200160630     c*
067300160630     c                   clear                   wfvocp00
067400160630     c     tasksc        chain     wfvocp1l
067500160628     c                   add       1             vocptsped
067600160628     c                   add       tasncl        vocptcolli
067700160628     c                   add       tasvlf        vocptvol
067800160628     C                   DO        10            XX
067900160705     C                   IF        VOC(XX) =  0
068000160630     c                   leave
068100160630     c                   end
068200160630     c*
068300160628     C                   IF        VOC(XX) = 010
068400160628     C                   ADD       imp(xx)       vocp010
068500160628     c                   end
068600160628     C                   IF        VOC(XX) = 023
068700160628     C                   ADD       imp(xx)       vocp023
068800160628     c                   end
068900160628     C                   IF        VOC(XX) = 060
069000160628     C                   ADD       imp(xx)       vocp060
069100160628     c                   end
069200160628     C                   IF        VOC(XX) = 070
069300160628     C                   ADD       imp(xx)       vocp070
069400160628     c                   end
069500160628     C                   IF        VOC(XX) = 080
069600160628     C                   ADD       imp(xx)       vocp080
069700160628     c                   end
069800160628     C                   IF        VOC(XX) = 081
069900160628     C                   ADD       imp(xx)       vocp081
070000160629     c                   end
070100160705     C                   IF        VOC(XX) = 900
070200160705     C                   ADD       imp(xx)       vocp900
070300160705     c                   end
070400160630     c                   enddo
070500160704     c                   ADD       imptot        vocpxxx
070600160630     c                   if        not %found(wfvocp1l)
070700160630      *       recupero i dati necessari da memorizzare sul file
070800160630     c                   exsr      sr_daticom
070900160629     c                   write     wfvocp00
071000160630     c                   else
071100160630     c                   update    wfvocp00
071200160630     c                   end
071300071228
071400071228     c                   endsr
071500160630      //-----------------------------------------------------------------------
071600160630      //? Recupero i dati comuni da memorizzare sul file
071700160630      //-----------------------------------------------------------------------
071800160630     c     sr_daticom    begsr
071900160630
072000160630     c                   eval      vocpdal = wdatada
072100160630     c                   eval      vocpal = wdataal
072200160630      *?recupero la ragione sociale del cliente
072300160630     c                   clear                   tibs69ds
072400160630     c                   clear                   ds_cnaco
072500160630     c                   clear                   ds_cnind
072600160630     c                   clear                   ds_cnclp
072700160630     c                   clear                   ds_fncls
072800160630     c                   eval      i69kac = tasksc
072900160630     c                   eval      i69kcp = tasksc
073000160630     c                   call      'TIBS69R'
073100160630     c                   parm                    tibs69ds
073200160630     c                   parm                    ds_cnaco
073300160630     c                   parm                    ds_cnind
073400160630     c                   parm                    ds_cnclp
073500160630     c                   parm                    ds_fncls
073600160630     c                   eval      vocpksc  = tasksc
073700160630     c                   eval      vocpkscd = acorag
073800160630
073900160630      *?recupero il cliente unificante
074000160630      * --> controllo se è padre
074100160630     c                   clear                   tibs10ds
074200160708     c*                  movel     tasaas        d10drf
074300160722     c***********        move      20160701      d10drf
074400160722     c                   move      datarif       d10drf
074500160630     c                   eval      d10tle = 'ST'
074600160630     c                   eval      d10paf = 'F'
074700160630     c                   eval      d10cod = tasksc
074800160630     c                   call      'TIBS10R'
074900160630     c                   parm                    tibs10ds
075000160630     c                   if        d10err = *blanks
075100160630     c                   z-add     d10cod        vocpkun
075200160630      * --> non è padre controllo se è figlio
075300160630     c                   else
075400160722     c***********        eval      d10drf = *date
075500160722     c                   move      datarif       d10drf
075600160630     c                   eval      d10tle = 'ST'
075700160630     c                   eval      d10paf = 'P'
075800160630     c                   eval      d10cod = tasksc
075900160630     c                   call      'TIBS10R'
076000160630     c                   parm                    tibs10ds
076100160630     c                   if        d10err = *blanks
076200160630     c                   z-add     d10cop        vocpkun
076300160630      * --> non è padre e nemmeno figlio
076400160630     c                   else
076500160630     c                   eval      vocpkun = vocpksc
076600160630     c                   endif
076700160630     c                   endif
076800160630
076900160630      *?recupero la ragione sociale dell'unificante
077000160630     c                   if        vocpkun > *zeros
077100160630     c                   clear                   tibs69ds
077200160630     c                   clear                   ds_cnaco
077300160630     c                   clear                   ds_cnind
077400160630     c                   clear                   ds_cnclp
077500160630     c                   clear                   ds_fncls
077600160630     c                   eval      i69kac = vocpkun
077700160630     c                   call      'TIBS69R'
077800160630     c                   parm                    tibs69ds
077900160630     c                   parm                    ds_cnaco
078000160630     c                   parm                    ds_cnind
078100160630     c                   parm                    ds_cnclp
078200160630     c                   parm                    ds_fncls
078300160630     c                   eval      vocpkund = acorag
078400160630     c                   endif
078500160630
078600160630      *?recupero la filiale dell'unificante
078700160630     c                   movel     vocpkun       vocpfil
078800160630     c     vocpfil       chain     azorg01l
078900160630     c                   if        %found(azorg01l)
079000160630     c                   eval      vocpfild = orgdes
079100160630
079200160630      *?recupero l'area dell'unificante
079300160630     c                   eval      vocpcar = orgcar
079400160630     c                   eval      kcod = '05'
079500160630     c                   eval      kkey = %editc(vocpcar:'X')
079600160630     c     ktabel        chain     tabel00f
079700160630     c                   if        %found(tabel00f)
079800160630     c                   eval      vocpcard = tbluni
079900160630     c                   endif
080000160630
080100160630      *?recupero il distretto dell'unificante
080200160630     c                   eval      vocpdit = orgfl3
080300160630     c                   eval      kcod = '17'
080400160630     c                   eval      kkey = vocpdit
080500160630     c     ktabel        chain     tabel00f
080600160630     c                   if        %found(tabel00f)
080700160630     c                   eval      vocpditd = tbluni
080800160630     c                   endif
080900160630
081000160630     c                   endif
081100160630     c                   endsr
081200071228      //-----------------------------------------------------------------------
081300071228      //? Routine iniziale
081400071228      //-----------------------------------------------------------------------
081500071228     c     *inzsr        begsr
081600071228
081700071228     c     *entry        plist
081800071228     c                   parm                    kpjba
081900071228     c                   eval      param = kpjbu
082000071228
082100071228      * data del giorno
082200071228     c                   time                    wtime
082300071228     c                   move      wtime         wdate
082400071228
082500160630     c     kcev          klist
082600160704     c                   kfld                    anno
082700160630     c                   kfld                    taslnp
082800160630     c                   kfld                    tasnrs
082900160630     c                   kfld                    tasnsp
083000160630     c                   kfld                    tastbl
083100160628     c     ktabel        klist
083200160628     c                   kfld                    codut
083300160628     c                   kfld                    kcod
083400160628     c                   kfld                    kkey
083500160701     c* verifico se analizzare il TASP e il CEVP
083600160701     c                   eval      kcod = '$3'
083700160701     c                   movel     '1'           kkey
083800160701     c     ktabel        chain     tabel00f
083900160701     c                   if        %found(tabel00f)
084000160701     c                   eval      ds$3 = tbluni
084100160701     c                   end
084200071228
084300071228     c                   endsr
084400071228** cmd
084500160628CLRPFM FILE(WFVOCP0F)
084600160628** VOC
084700160629010
084800160629023
084900160629060
085000160629070
085100160629080
085200160629081
085300160629900
