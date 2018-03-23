000100071228     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600071228
000700160629     ftabel00f  if   e           k disk
000701160629     fazorg01l  if   e           k disk
000900160630     feccev30c  if   e           k disk
000901160630     fecc2v30c  if   e           k disk
001100160628     fwfvocp1l  uf a e           k disk    usropn
001300071228
001400071228      //-----------------------------------------------------------------------
001500071228      //? VARIABILI
001600071228      //-----------------------------------------------------------------------
001700071228     d codut           s              1  0 inz(1)
001800071228     d lengh           s             15  5 inz(80)
001801160628     d kcod            s                   like(tblcod)
001802160628     d kkey            s                   like(tblkey)
002100080103     d pdataal         s             10
002200080103     d pdatada         s             10
002300160630     d evdcli          s              7  0 inz
002301160630     d evdvoc          s              3  0 inz
002303160630     d xx              s              3  0 inz
002800160629     d wdataspe        s                   like(VOCPdal)
002900160629     d wdate           s                   like(VOCPal)
003200071228     d wtime           s             14  0
004000071228      //-----------------------------------------------------------------------
004100071228      //? SCHIERE
004200071228      //-----------------------------------------------------------------------
004300071228     d cmd             s             80    dim(1) ctdata perrcd(1)
004301160628     d VOC             s              3  0 dim(10) ctdata perrcd(1)
004302160630     d imp             s             13  5 dim(10)
004600071228
004700071228      //-----------------------------------------------------------------------
004800071228      //? STRUTTURE DATI
004900071228      //-----------------------------------------------------------------------
005000071228     d param           ds
005100071228     d  waada                  1      4  0
005200071228     d  wmgda                  5      8  0
005300071228     d  wmmda                  5      6  0
005400160629     d  wggda                  7      8  0
005500071228     d  wdatada                1      8  0
005501160629     d  da                     1      8
005600071228     d  waaal                  9     12  0
005700071228     d  wmmal                 13     14  0
005800071228     d  wggal                 15     16  0
005900071228     d  wdataal                9     16  0
005901160629     d  a                      9     16
005902160704     d  anno                  17     20  0
006400071228
007100071228     d ds_cnaco      e ds                  inz extname(cnaco00f)
007200071228     d ds_cnind      e ds                  inz extname(cnind00f)
007300071228     d ds_cnclp      e ds                  inz extname(cnclp00f)
007400071228     d ds_fncls      e ds                  inz extname(fncls00f)
007401160630     d titasds       e ds                  inz extname(titas10f)
007500071228     d kpjba         e ds
007600071228     d tibs10ds      e ds
007700071228     d tibs69ds      e ds
007701160701     d ds$3          e ds
007800071228
007900071228      //-----------------------------------------------------------------------
007901160701     c                   eval      *inlr = *on
007902160701      * clrpfm del file di work
007903160701     c                   call(e)   'QCMDEXC'
007904160701     c                   parm                    cmd(1)
007905160701     c                   parm                    lengh
007906160701      * se riuscito comando vado avanti con l'elaborazione
007907160701     c                   if        %error
007908160701     c                   return
007909160701     c                   end
007910160701     c                   open      wfvocp1l
010827160701     c* tasp o tas0
010828160701     c                   if        §$3D10>=wdataal
010829160630     C/EXEC SQL
010830160705     C+ DECLARE B1 CURSOR FOR SELECT * FROM titasp0f WHERE
010831160704     C+ digits(tasaas) concat digits(tasmgs) between :da and :a
010833160630     C/END-EXEC
010834160630
010835160630     C/EXEC SQL
010836160630     C+ OPEN b1
010837160630     C/END-EXEC
010838160701     c                   else
010839160701     C/EXEC SQL
010840160701     C+ DECLARE B2 CURSOR FOR SELECT * FROM titas10f WHERE digits(tasaas)
010841160704     C+ concat digits(tasmgs) between :da and :a
010843160701     C/END-EXEC
010844160701
010845160701     C/EXEC SQL
010846160701     C+ OPEN b2
010847160701     C/END-EXEC
010848160701     c                   end
010849160701
010850160630     C                   DO        *HIVAL
010851160630
010852160701     c* tasp o tas0
010853160701     c                   if        §$3D10>=wdataal
010854160630     C/EXEC SQL
010855160630     C+ FETCH NEXT FROM b1 INTO :titasds
010856160630     C/END-EXEC
010857160701     c                   else
010858160701     C/EXEC SQL
010859160701     C+ FETCH NEXT FROM b2 INTO :titasds
010860160701     C/END-EXEC
010861160701     c                   end
010862160701
010863160630     C                   IF        SQLCod = 100 or sqlcod < 0
010864160630     c                   clear                   sqlcod
010865160630     C                   LEAVE
010866160630     C                   ENDIF
010867160630     c*
010868160630     c     kcev          chain     eccev30c
010869160630     c                   if        not %found(eccev30c)
010870160630     c                   iter
010871160630     c                   end
010872160630     c     kcev          chain     ecc2v30c
010873160630     C                   CLEAR                   IMP
013062160630
013066160628     c                   EXSR      conta
013067160630     c                   if        %found(ecc2v30c)
013068160630     c                   EXSR      conta2
013070160630     C                   END
013090160628      *       scrivo nuovo record
013091160628     c                   exsr      sr_scrivi
013095160628     C                   ENDDO
013096160628
013097160701     c* tasp o tas0
013098160701     c                   if        §$3D10>=wdataal
013099160628     C/EXEC SQL
013100160630     C+ CLOSE b1
013101160628     C/END-EXEC
013102160701     c                   else
013103160701     C/EXEC SQL
013104160701     C+ CLOSE b2
013105160701     C/END-EXEC
013106160701     c                   end
013223160628     C*--------------------------------------------------------------- ---------
013224160628     C     conta         BEGSR
013225160630     C*--------------------------------------------------------------- ---------
013226160704     c                   clear                   imptot           13 5
013227160704     c                   z-add     1             xx
013228160704     c     cevc01        lookup    voc(xx)                                01
013229160704     c                   if        *in01
013230160628     c                   add       cevi01        IMP(XX)
013231160704     c                   else
013232160704     c                   add       cevi01        imptot
013233160628     c                   end
013235160704     c                   z-add     1             xx
013236160704     c     cevc02        lookup    voc(xx)                                01
013237160704     c                   if        *in01
013238160628     c                   add       cevi02        IMP(XX)
013240160704     c                   else
013241160704     c                   add       cevi02        imptot
013243160628     c                   end
013244160704     c                   z-add     1             xx
013245160704     c     cevc03        lookup    voc(xx)                                01
013246160704     c                   if        *in01
013247160628     c                   add       cevi03        IMP(XX)
013250160704     c                   else
013251160704     c                   add       cevi03        imptot
013252160628     c                   end
013253160704     c                   z-add     1             xx
013254160704     c     cevc04        lookup    voc(xx)                                01
013255160704     c                   if        *in01
013256160628     c                   add       cevi04        IMP(XX)
013257160704     c                   else
013258160704     c                   add       cevi04        imptot
013261160628     c                   end
013262160704     c                   z-add     1             xx
013263160704     c     cevc05        lookup    voc(xx)                                01
013264160704     c                   if        *in01
013265160628     c                   add       cevi05        IMP(XX)
013266160704     c                   else
013267160704     c                   add       cevi05        imptot
013270160628     c                   end
013271160704     c                   z-add     1             xx
013272160704     c     cevc06        lookup    voc(xx)                                01
013273160704     c                   if        *in01
013274160628     c                   add       cevi06        IMP(XX)
013277160704     c                   else
013278160704     c                   add       cevi06        imptot
013279160628     c                   end
013280160704     c                   z-add     1             xx
013281160704     c     cevc07        lookup    voc(xx)                                01
013282160704     c                   if        *in01
013283160628     c                   add       cevi07        IMP(XX)
013286160704     c                   else
013287160704     c                   add       cevi07        imptot
013288160628     c                   end
013289160704     c                   z-add     1             xx
013290160704     c     cevc08        lookup    voc(xx)                                01
013291160704     c                   if        *in01
013292160628     c                   add       cevi08        IMP(XX)
013295160704     c                   else
013296160704     c                   add       cevi08        imptot
013297160628     c                   end
013298160704     c                   z-add     1             xx
013299160704     c     cevc09        lookup    voc(xx)                                01
013300160704     c                   if        *in01
013301160628     c                   add       cevi09        IMP(XX)
013302160704     c                   else
013303160704     c                   add       cevi09        imptot
013306160628     c                   end
013307160704     c                   z-add     1             xx
013308160704     c     cevc10        lookup    voc(xx)                                01
013309160704     c                   if        *in01
013310160628     c                   add       cevi10        IMP(XX)
013313160704     c                   else
013314160704     c                   add       cevi10        imptot
013315160628     c                   end
013316160704     c                   z-add     1             xx
013317160704     c     cevc11        lookup    voc(xx)                                01
013318160704     c                   if        *in01
013319160704     c                   add       cevi11        IMP(XX)
013322160704     c                   else
013323160704     c                   add       cevi11        imptot
013324160704     c                   end
013325160704     c                   z-add     1             xx
013326160704     c     cevc12        lookup    voc(xx)                                01
013327160704     c                   if        *in01
013328160704     c                   add       cevi12        IMP(XX)
013331160704     c                   else
013332160704     c                   add       cevi12        imptot
013333160628     c                   end
013334160704     c                   z-add     1             xx
013335160704     c     cevc13        lookup    voc(xx)                                01
013336160704     c                   if        *in01
013337160704     c                   add       cevi13        IMP(XX)
013340160704     c                   else
013341160704     c                   add       cevi13        imptot
013342160628     c                   end
013343160704     c                   z-add     1             xx
013344160704     c     cevc14        lookup    voc(xx)                                01
013345160704     c                   if        *in01
013346160704     c                   add       cevi14        IMP(XX)
013347160704     c                   else
013348160704     c                   add       cevi14        imptot
013351160628     c                   end
013352160704     c                   z-add     1             xx
013353160704     c     cevc15        lookup    voc(xx)                                01
013354160704     c                   if        *in01
013355160704     c                   add       cevi15        IMP(XX)
013358160704     c                   else
013359160704     c                   add       cevi15        imptot
013360160628     c                   end
013361160704     c                   z-add     1             xx
013362160704     c     cevc16        lookup    voc(xx)                                01
013363160704     c                   if        *in01
013364160704     c                   add       cevi16        IMP(XX)
013367160704     c                   else
013368160704     c                   add       cevi16        imptot
013369160628     c                   end
013370160704     c                   z-add     1             xx
013371160704     c     cevc17        lookup    voc(xx)                                01
013372160704     c                   if        *in01
013373160704     c                   add       cevi17        IMP(XX)
013376160704     c                   else
013377160704     c                   add       cevi17        imptot
013378160628     c                   end
013379160704     c                   z-add     1             xx
013380160704     c     cevc18        lookup    voc(xx)                                01
013381160704     c                   if        *in01
013382160704     c                   add       cevi18        IMP(XX)
013385160704     c                   else
013386160704     c                   add       cevi18        imptot
013387160628     c                   end
013388160704     c                   z-add     1             xx
013390160704     c     cevc19        lookup    voc(xx)                                01
013391160704     c                   if        *in01
013392160628     c                   add       cevi19        IMP(XX)
013395160704     c                   else
013396160704     c                   add       cevi19        imptot
013397160628     c                   end
013398160704     c                   z-add     1             xx
013399160704     c     cevc20        lookup    voc(xx)                                01
013400160704     c                   if        *in01
013401160628     c                   add       cevi20        IMP(XX)
013404160704     c                   else
013405160704     c                   add       cevi20        imptot
013406160628     c                   end
013407160704     c                   z-add     1             xx
013408160704     c     cevc21        lookup    voc(xx)                                01
013409160704     c                   if        *in01
013410160628     c                   add       cevi21        IMP(XX)
013413160704     c                   else
013414160704     c                   add       cevi21        imptot
013415160628     c                   end
013416160704     c                   z-add     1             xx
013417160704     c     cevc22        lookup    voc(xx)                                01
013418160704     c                   if        *in01
013419160628     c                   add       cevi22        IMP(XX)
013422160704     c                   else
013423160704     c                   add       cevi22        imptot
013424160628     c                   end
013425160704     c                   z-add     1             xx
013426160704     c     cevc23        lookup    voc(xx)                                01
013427160704     c                   if        *in01
013428160628     c                   add       cevi23        IMP(XX)
013431160704     c                   else
013432160704     c                   add       cevi23        imptot
013433160628     c                   end
013434160704     c                   z-add     1             xx
013435160704     c     cevc24        lookup    voc(xx)                                01
013436160704     c                   if        *in01
013437160628     c                   add       cevi24        IMP(XX)
013440160704     c                   else
013441160704     c                   add       cevi24        imptot
013442160628     c                   end
013443160704     c                   z-add     1             xx
013444160704     c     cevc25        lookup    voc(xx)                                01
013445160704     c                   if        *in01
013446160628     c                   add       cevi25        IMP(XX)
013449160704     c                   else
013450160704     c                   add       cevi25        imptot
013451160628     c                   end
013452160704     c                   z-add     1             xx
013453160704     c     cevc26        lookup    voc(xx)                                01
013454160704     c                   if        *in01
013455160628     c                   add       cevi26        IMP(XX)
013458160704     c                   else
013459160704     c                   add       cevi26        imptot
013460160628     c                   end
013461160704     c                   z-add     1             xx
013462160704     c     cevc27        lookup    voc(xx)                                01
013463160704     c                   if        *in01
013464160628     c                   add       cevi27        IMP(XX)
013465160704     c                   else
013466160704     c                   add       cevi27        imptot
013469160628     c                   end
013470160704     c                   z-add     1             xx
013471160704     c     cevc28        lookup    voc(xx)                                01
013472160704     c                   if        *in01
013473160628     c                   add       cevi28        IMP(XX)
013474160704     c                   else
013475160704     c                   add       cevi28        imptot
013478160628     c                   end
013479160704     c                   z-add     1             xx
013480160704     c     cevc29        lookup    voc(xx)                                01
013481160704     c                   if        *in01
013482160628     c                   add       cevi29        IMP(XX)
013485160704     c                   else
013486160704     c                   add       cevi29        imptot
013487160628     c                   end
013488160704     c                   z-add     1             xx
013489160704     c     cevc30        lookup    voc(xx)                                01
013490160704     c                   if        *in01
013491160628     c                   add       cevi30        IMP(XX)
013494160704     c                   else
013495160704     c                   add       cevi30        imptot
013496160628     c                   end
013497160704     c                   z-add     1             xx
013498160704     c     cevc31        lookup    voc(xx)                                01
013499160704     c                   if        *in01
013500160628     c                   add       cevi31        IMP(XX)
013503160704     c                   else
013504160704     c                   add       cevi31        imptot
013505160628     c                   end
013506160704     c                   z-add     1             xx
013507160704     c     cevc32        lookup    voc(xx)                                01
013508160704     c                   if        *in01
013509160628     c                   add       cevi32        IMP(XX)
013510160704     c                   else
013511160704     c                   add       cevi32        imptot
013514160628     c                   end
013515160704     c                   z-add     1             xx
013516160704     c     cevc33        lookup    voc(xx)                                01
013517160704     c                   if        *in01
013518160628     c                   add       cevi33        IMP(XX)
013521160704     c                   else
013522160704     c                   add       cevi33        imptot
013523160628     c                   end
013524160704     c                   z-add     1             xx
013525160704     c     cevc34        lookup    voc(xx)                                01
013526160704     c                   if        *in01
013527160628     c                   add       cevi34        IMP(XX)
013530160704     c                   else
013531160704     c                   add       cevi34        imptot
013532160628     c                   end
013533160704     c                   z-add     1             xx
013534160704     c     cevc35        lookup    voc(xx)                                01
013535160704     c                   if        *in01
013536160628     c                   add       cevi35        IMP(XX)
013539160704     c                   else
013540160704     c                   add       cevi35        imptot
013541160628     c                   end
013542160704     c                   z-add     1             xx
013543160704     c     cevc36        lookup    voc(xx)                                01
013544160704     c                   if        *in01
013545160628     c                   add       cevi36        IMP(XX)
013548160704     c                   else
013549160704     c                   add       cevi36        imptot
013550160628     c                   end
013551160704     c                   z-add     1             xx
013552160704     c     cevc37        lookup    voc(xx)                                01
013553160704     c                   if        *in01
013554160628     c                   add       cevi37        IMP(XX)
013555160704     c                   else
013556160704     c                   add       cevi37        imptot
013559160628     c                   end
013560160704     c                   z-add     1             xx
013561160704     c     cevc38        lookup    voc(xx)                                01
013562160704     c                   if        *in01
013563160628     c                   add       cevi38        IMP(XX)
013566160704     c                   else
013567160704     c                   add       cevi38        imptot
013568160628     c                   end
013569160704     c                   z-add     1             xx
013570160704     c     cevc39        lookup    voc(xx)                                01
013571160704     c                   if        *in01
013572160628     c                   add       cevi39        IMP(XX)
013575160704     c                   else
013576160704     c                   add       cevi39        imptot
013577160628     c                   end
013578160704     c                   z-add     1             xx
013579160704     c     cevc40        lookup    voc(xx)                                01
013580160704     c                   if        *in01
013581160628     c                   add       cevi40        IMP(XX)
013584160704     c                   else
013585160704     c                   add       cevi40        imptot
013586160628     c                   end
013587160704     c                   z-add     1             xx
013588160704     c     cevc41        lookup    voc(xx)                                01
013589160704     c                   if        *in01
013590160628     c                   add       cevi41        IMP(XX)
013593160704     c                   else
013594160704     c                   add       cevi41        imptot
013595160628     c                   end
013596160704     c                   z-add     1             xx
013597160704     c     cevc42        lookup    voc(xx)                                01
013598160704     c                   if        *in01
013599160628     c                   add       cevi42        IMP(XX)
013600160704     c                   else
013601160704     c                   add       cevi42        imptot
013604160628     c                   end
013605160704     c                   z-add     1             xx
013606160704     c     cevc43        lookup    voc(xx)                                01
013607160704     c                   if        *in01
013608160628     c                   add       cevi43        IMP(XX)
013611160704     c                   else
013612160704     c                   add       cevi43        imptot
013613160628     c                   end
013614160704     c                   z-add     1             xx
013615160704     c     cevc44        lookup    voc(xx)                                01
013616160704     c                   if        *in01
013617160628     c                   add       cevi44        IMP(XX)
013620160704     c                   else
013621160704     c                   add       cevi44        imptot
013622160628     c                   end
013623160704     c                   z-add     1             xx
013624160704     c     cevc45        lookup    voc(xx)                                01
013625160704     c                   if        *in01
013626160628     c                   add       cevi45        IMP(XX)
013629160704     c                   else
013630160704     c                   add       cevi45        imptot
013631160628     c                   end
013632160704     c                   z-add     1             xx
013633160704     c     cevc46        lookup    voc(xx)                                01
013634160704     c                   if        *in01
013635160628     c                   add       cevi46        IMP(XX)
013639160704     c                   else
013640160704     c                   add       cevi46        imptot
013641160704     c                   end
013642160704     c                   z-add     1             xx
013643160704     c     cevc47        lookup    voc(xx)                                01
013644160704     c                   if        *in01
013645160628     c                   add       cevi47        IMP(XX)
013648160704     c                   else
013649160704     c                   add       cevi47        imptot
013650160628     c                   end
013651160704     c                   z-add     1             xx
013652160704     c     cevc48        lookup    voc(xx)                                01
013653160704     c                   if        *in01
013654160628     c                   add       cevi48        IMP(XX)
013657160704     c                   else
013658160704     c                   add       cevi48        imptot
013659160628     c                   end
013660160704     c                   z-add     1             xx
013661160704     c     cevc49        lookup    voc(xx)                                01
013662160704     c                   if        *in01
013663160628     c                   add       cevi49        IMP(XX)
013666160704     c                   else
013667160704     c                   add       cevi49        imptot
013668160628     c                   end
013669160704     c                   z-add     1             xx
013670160704     c     cevc50        lookup    voc(xx)                                01
013671160704     c                   if        *in01
013672160628     c                   add       cevi50        IMP(XX)
013675160704     c                   else
013676160704     c                   add       cevi50        imptot
013677160628     c                   end
013678160628     C                   ENDSR
013679160629     C*--------------------------------------------------------------- ---------
013680160629     C     conta2        BEGSR
013681160629     C*--------------------------------------------------------------- ---------
013683160704     c                   z-add     1             xx
013684160704     c     c2vc51        lookup    voc(xx)                                01
013685160704     c                   if        *in01
013686160629     c                   add       C2Vi51        imp(xx)
013687160704     c                   else
013688160704     c                   add       c2vi51        imptot
013691160629     c                   end
013692160704     c                   z-add     1             xx
013693160704     c     c2vc52        lookup    voc(xx)                                01
013694160704     c                   if        *in01
013695160629     c                   add       C2Vi52        imp(xx)
013698160704     c                   else
013699160704     c                   add       c2vi52        imptot
013700160629     c                   end
013701160704     c                   z-add     1             xx
013702160704     c     c2vc53        lookup    voc(xx)                                01
013703160704     c                   if        *in01
013704160629     c                   add       C2Vi53        imp(xx)
013707160704     c                   else
013708160704     c                   add       c2vi53        imptot
013709160629     c                   end
013710160704     c                   z-add     1             xx
013711160704     c     c2vc54        lookup    voc(xx)                                01
013712160704     c                   if        *in01
013713160629     c                   add       C2Vi54        imp(xx)
013716160704     c                   else
013717160704     c                   add       c2vi54        imptot
013718160629     c                   end
013719160704     c                   z-add     1             xx
013720160704     c     c2vc55        lookup    voc(xx)                                01
013721160704     c                   if        *in01
013722160629     c                   add       C2Vi55        imp(xx)
013725160704     c                   else
013726160704     c                   add       c2vi55        imptot
013727160629     c                   end
013728160704     c                   z-add     1             xx
013729160704     c     c2vc56        lookup    voc(xx)                                01
013730160704     c                   if        *in01
013731160629     c                   add       C2Vi56        imp(xx)
013732160704     c                   else
013733160704     c                   add       c2vi56        imptot
013736160629     c                   end
013737160704     c                   z-add     1             xx
013738160704     c     c2vc57        lookup    voc(xx)                                01
013739160704     c                   if        *in01
013740160629     c                   add       C2Vi57        imp(xx)
013743160704     c                   else
013744160704     c                   add       c2vi57        imptot
013745160629     c                   end
013746160704     c                   z-add     1             xx
013747160704     c     c2vc58        lookup    voc(xx)                                01
013748160704     c                   if        *in01
013749160629     c                   add       C2Vi58        imp(xx)
013752160704     c                   else
013753160704     c                   add       c2vi58        imptot
013754160629     c                   end
013755160704     c                   z-add     1             xx
013756160704     c     c2vc59        lookup    voc(xx)                                01
013757160704     c                   if        *in01
013758160629     c                   add       C2Vi59        imp(xx)
013761160704     c                   else
013762160704     c                   add       c2vi59        imptot
013763160629     c                   end
013764160704     c                   z-add     1             xx
013765160704     c     c2vc60        lookup    voc(xx)                                01
013766160704     c                   if        *in01
013767160629     c                   add       C2Vi60        imp(xx)
013770160704     c                   else
013771160704     c                   add       c2vi60        imptot
013772160629     c                   end
013773160704     c                   z-add     1             xx
013774160704     c     c2vc61        lookup    voc(xx)                                01
013775160704     c                   if        *in01
013776160629     c                   add       C2Vi61        imp(xx)
013777160704     c                   else
013778160704     c                   add       c2vi61        imptot
013781160629     c                   end
013782160704     c                   z-add     1             xx
013783160704     c     c2vc62        lookup    voc(xx)                                01
013784160704     c                   if        *in01
013785160629     c                   add       C2Vi62        imp(xx)
013788160704     c                   else
013789160704     c                   add       c2vi62        imptot
013790160629     c                   end
013791160704     c                   z-add     1             xx
013792160704     c     c2vc63        lookup    voc(xx)                                01
013793160704     c                   if        *in01
013794160629     c                   add       C2Vi63        imp(xx)
013797160704     c                   else
013798160704     c                   add       c2vi63        imptot
013799160629     c                   end
013800160704     c                   z-add     1             xx
013801160704     c     c2vc64        lookup    voc(xx)                                01
013802160704     c                   if        *in01
013803160629     c                   add       C2Vi64        imp(xx)
013806160704     c                   else
013807160704     c                   add       c2vi64        imptot
013808160629     c                   end
013809160704     c                   z-add     1             xx
013810160704     c     c2vc65        lookup    voc(xx)                                01
013811160704     c                   if        *in01
013812160629     c                   add       C2Vi65        imp(xx)
013815160704     c                   else
013816160704     c                   add       c2vi65        imptot
013817160629     c                   end
013818160704     c                   z-add     1             xx
013819160704     c     c2vc66        lookup    voc(xx)                                01
013820160704     c                   if        *in01
013821160629     c                   add       C2Vi66        imp(xx)
013822160704     c                   else
013823160704     c                   add       c2vi66        imptot
013826160629     c                   end
013827160704     c                   z-add     1             xx
013828160704     c     c2vc67        lookup    voc(xx)                                01
013829160704     c                   if        *in01
013830160629     c                   add       C2Vi67        imp(xx)
013833160704     c                   else
013834160704     c                   add       c2vi67        imptot
013835160629     c                   end
013836160704     c                   z-add     1             xx
013837160704     c     c2vc68        lookup    voc(xx)                                01
013838160704     c                   if        *in01
013839160629     c                   add       C2Vi68        imp(xx)
013842160704     c                   else
013843160704     c                   add       c2vi68        imptot
013844160629     c                   end
013845160704     c                   z-add     1             xx
013846160704     c     c2vc69        lookup    voc(xx)                                01
013847160704     c                   if        *in01
013848160629     c                   add       C2Vi69        imp(xx)
013851160704     c                   else
013852160704     c                   add       c2vi69        imptot
013853160629     c                   end
013854160704     c                   z-add     1             xx
013855160704     c     c2vc70        lookup    voc(xx)                                01
013856160704     c                   if        *in01
013857160629     c                   add       C2Vi70        imp(xx)
013860160704     c                   else
013861160704     c                   add       c2vi70        imptot
013862160629     c                   end
013863160704     c                   z-add     1             xx
013864160704     c     c2vc71        lookup    voc(xx)                                01
013865160704     c                   if        *in01
013866160629     c                   add       C2Vi71        imp(xx)
013867160704     c                   else
013868160704     c                   add       c2vi71        imptot
013871160629     c                   end
013872160704     c                   z-add     1             xx
013873160704     c     c2vc72        lookup    voc(xx)                                01
013874160704     c                   if        *in01
013875160629     c                   add       C2Vi72        imp(xx)
013878160704     c                   else
013879160704     c                   add       c2vi72        imptot
013880160629     c                   end
013881160704     c                   z-add     1             xx
013882160704     c     c2vc73        lookup    voc(xx)                                01
013883160704     c                   if        *in01
013884160629     c                   add       C2Vi73        imp(xx)
013887160704     c                   else
013888160704     c                   add       c2vi73        imptot
013889160629     c                   end
013890160704     c                   z-add     1             xx
013891160704     c     c2vc74        lookup    voc(xx)                                01
013892160704     c                   if        *in01
013893160629     c                   add       C2Vi74        imp(xx)
013896160704     c                   else
013897160704     c                   add       c2vi74        imptot
013898160629     c                   end
013899160704     c                   z-add     1             xx
013900160704     c     c2vc75        lookup    voc(xx)                                01
013901160704     c                   if        *in01
013902160629     c                   add       C2Vi75        imp(xx)
013905160704     c                   else
013906160704     c                   add       c2vi75        imptot
013907160629     c                   end
013910160629     C                   ENDSR
055100071228      //-----------------------------------------------------------------------
055200071228      //? Scrivo il file di work
055300071228      //-----------------------------------------------------------------------
055400071228     c     sr_scrivi     begsr
055401160630     c*
055402160630     c                   clear                   wfvocp00
055403160630     c     tasksc        chain     wfvocp1l
055900160628     c                   add       1             vocptsped
056000160628     c                   add       tasncl        vocptcolli
056100160628     c                   add       tasvlf        vocptvol
056102160628     C                   DO        10            XX
056103160705     C                   IF        VOC(XX) =  0
056104160630     c                   leave
056105160630     c                   end
056129160630     c*
056130160628     C                   IF        VOC(XX) = 010
056131160628     C                   ADD       imp(xx)       vocp010
056133160628     c                   end
056134160628     C                   IF        VOC(XX) = 023
056135160628     C                   ADD       imp(xx)       vocp023
056137160628     c                   end
056138160628     C                   IF        VOC(XX) = 060
056139160628     C                   ADD       imp(xx)       vocp060
056141160628     c                   end
056142160628     C                   IF        VOC(XX) = 070
056143160628     C                   ADD       imp(xx)       vocp070
056145160628     c                   end
056146160628     C                   IF        VOC(XX) = 080
056147160628     C                   ADD       imp(xx)       vocp080
056149160628     c                   end
056150160628     C                   IF        VOC(XX) = 081
056152160628     C                   ADD       imp(xx)       vocp081
056154160629     c                   end
056155160705     C                   IF        VOC(XX) = 900
056156160705     C                   ADD       imp(xx)       vocp900
056157160705     c                   end
056169160630     c                   enddo
056170160704     c                   ADD       imptot        vocpxxx
056172160630     c                   if        not %found(wfvocp1l)
056173160630      *       recupero i dati necessari da memorizzare sul file
056174160630     c                   exsr      sr_daticom
056175160629     c                   write     wfvocp00
056176160630     c                   else
056177160630     c                   update    wfvocp00
056179160630     c                   end
056300071228
056400071228     c                   endsr
056401160630      //-----------------------------------------------------------------------
056402160630      //? Recupero i dati comuni da memorizzare sul file
056403160630      //-----------------------------------------------------------------------
056404160630     c     sr_daticom    begsr
056405160630
056406160630     c                   eval      vocpdal = wdatada
056407160630     c                   eval      vocpal = wdataal
056408160630      *?recupero la ragione sociale del cliente
056409160630     c                   clear                   tibs69ds
056410160630     c                   clear                   ds_cnaco
056411160630     c                   clear                   ds_cnind
056412160630     c                   clear                   ds_cnclp
056413160630     c                   clear                   ds_fncls
056414160630     c                   eval      i69kac = tasksc
056415160630     c                   eval      i69kcp = tasksc
056416160630     c                   call      'TIBS69R'
056417160630     c                   parm                    tibs69ds
056418160630     c                   parm                    ds_cnaco
056419160630     c                   parm                    ds_cnind
056420160630     c                   parm                    ds_cnclp
056421160630     c                   parm                    ds_fncls
056422160630     c                   eval      vocpksc  = tasksc
056423160630     c                   eval      vocpkscd = acorag
056424160630
056425160630      *?recupero il cliente unificante
056426160630      * --> controllo se è padre
056427160630     c                   clear                   tibs10ds
056428160708     c*                  movel     tasaas        d10drf
056429160708     c                   move      20160701      d10drf
056430160630     c                   eval      d10tle = 'ST'
056431160630     c                   eval      d10paf = 'F'
056432160630     c                   eval      d10cod = tasksc
056433160630     c                   call      'TIBS10R'
056434160630     c                   parm                    tibs10ds
056435160630     c                   if        d10err = *blanks
056436160630     c                   z-add     d10cod        vocpkun
056437160630      * --> non è padre controllo se è figlio
056438160630     c                   else
056439160630     c                   eval      d10drf = *date
056440160630     c                   eval      d10tle = 'ST'
056441160630     c                   eval      d10paf = 'P'
056442160630     c                   eval      d10cod = tasksc
056443160630     c                   call      'TIBS10R'
056444160630     c                   parm                    tibs10ds
056445160630     c                   if        d10err = *blanks
056446160630     c                   z-add     d10cop        vocpkun
056447160630      * --> non è padre e nemmeno figlio
056448160630     c                   else
056449160630     c                   eval      vocpkun = vocpksc
056450160630     c                   endif
056451160630     c                   endif
056452160630
056453160630      *?recupero la ragione sociale dell'unificante
056454160630     c                   if        vocpkun > *zeros
056455160630     c                   clear                   tibs69ds
056456160630     c                   clear                   ds_cnaco
056457160630     c                   clear                   ds_cnind
056458160630     c                   clear                   ds_cnclp
056459160630     c                   clear                   ds_fncls
056460160630     c                   eval      i69kac = vocpkun
056461160630     c                   call      'TIBS69R'
056462160630     c                   parm                    tibs69ds
056463160630     c                   parm                    ds_cnaco
056464160630     c                   parm                    ds_cnind
056465160630     c                   parm                    ds_cnclp
056466160630     c                   parm                    ds_fncls
056467160630     c                   eval      vocpkund = acorag
056468160630     c                   endif
056469160630
056470160630      *?recupero la filiale dell'unificante
056471160630     c                   movel     vocpkun       vocpfil
056472160630     c     vocpfil       chain     azorg01l
056473160630     c                   if        %found(azorg01l)
056474160630     c                   eval      vocpfild = orgdes
056475160630
056476160630      *?recupero l'area dell'unificante
056477160630     c                   eval      vocpcar = orgcar
056478160630     c                   eval      kcod = '05'
056479160630     c                   eval      kkey = %editc(vocpcar:'X')
056480160630     c     ktabel        chain     tabel00f
056481160630     c                   if        %found(tabel00f)
056482160630     c                   eval      vocpcard = tbluni
056483160630     c                   endif
056484160630
056485160630      *?recupero il distretto dell'unificante
056486160630     c                   eval      vocpdit = orgfl3
056487160630     c                   eval      kcod = '17'
056488160630     c                   eval      kkey = vocpdit
056489160630     c     ktabel        chain     tabel00f
056490160630     c                   if        %found(tabel00f)
056491160630     c                   eval      vocpditd = tbluni
056492160630     c                   endif
056493160630
056494160630     c                   endif
056495160630     c                   endsr
057800071228      //-----------------------------------------------------------------------
057900071228      //? Routine iniziale
058000071228      //-----------------------------------------------------------------------
058100071228     c     *inzsr        begsr
058200071228
058300071228     c     *entry        plist
058400071228     c                   parm                    kpjba
058500071228     c                   eval      param = kpjbu
058600071228
058700071228      * data del giorno
058800071228     c                   time                    wtime
058900071228     c                   move      wtime         wdate
059000071228
062301160630     c     kcev          klist
062302160704     c                   kfld                    anno
062303160630     c                   kfld                    taslnp
062304160630     c                   kfld                    tasnrs
062305160630     c                   kfld                    tasnsp
062306160630     c                   kfld                    tastbl
062313160628     c     ktabel        klist
062314160628     c                   kfld                    codut
062315160628     c                   kfld                    kcod
062316160628     c                   kfld                    kkey
062317160701     c* verifico se analizzare il TASP e il CEVP
062319160701     c                   eval      kcod = '$3'
062320160701     c                   movel     '1'           kkey
062321160701     c     ktabel        chain     tabel00f
062322160701     c                   if        %found(tabel00f)
062323160701     c                   eval      ds$3 = tbluni
062324160701     c                   end
062400071228
062900071228     c                   endsr
065300071228** cmd
065400160628CLRPFM FILE(WFVOCP0F)
065500160628** VOC
065600160629010
065700160629023
065800160629060
065900160629070
066000160629080
066100160629081
066200160629900
