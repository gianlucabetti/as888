000100981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*--------------------------------------------------------------*
000300150310     Ftrmz63d   CF   E             WORKSTN
000400020304     dkpjba          e ds
000500150312     D TIBS34DS      E DS                                                       *Profili utente
000600150312     D DDATIUTE      E DS                                                       *Dati utente
000700150312     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
000800030630     d Tibs36Ds      e ds
000900150310     d Trmz63ds      e ds
001000070605     D WLBDA8          DS
001100070605     D  G02DAT                 1      8  0
001200070605     D  G02INV                 9     16  0
001300070605     D  G02ERR                17     17
001400070605     D  G02TGI                18     22  0
001500150310      **
001600150310     D DATECNV0F     E DS
001700150310      **
001800150310     D da_lunedi_scorso...
001900150310     D                 S               D
002000150310     D a_domenica_scorsa...
002100150310     D                 S               D
002200150311     D data_Inizio_week...
002300150311     D                 S               D
002400150311     D data_Fine_week...
002500150311     D                 S               D
002600150311     D Anno_week       S              4  0
002700150311     D Week            S              2  0
002800150311      **
002900150310     D oggi            S               D
003000150310      **
003100150310     d DataISO         s               d   datfmt(*iso)
003200150310     d DataEUR         s               d   datfmt(*eur)
003300150310      **
003400150313     D AnnoWWDa        S              6  0
003500150313     D AnnoWWA         S              6  0
003600150310      **
003700000000     C*---------------------------------------------------------------*
003800150310     c                   seton                                        lr
003900150310     c*
004000150312     c                   eval      vtipoela = 'S'
004100150306     c*
004200031105     c                   do        *hival
004300031105     c                   exfmt     video1
004400031105     c                   clear                   $msg
004500031105     c   kc              leave
004600031105     c                   exsr      controv1
004700031105     c   96              iter
004800050623     c                   if        *inkf
004900050623     c                   exsr      elabora
005000050805     c                   leave
005100050623     c                   endif
005200050805     c                   enddo
005300150306     c*
005400050802     c*-------------------------------------------------
005500090518     c     elabora       begsr
005600050802     c*-------------------------------------------------
005700150306     c*
005800150310     c                   clear                   trmz63ds
005900150310     c                   eval      i63tipoela= vtipoela
006000150310     c                   eval      i63datdal= vdatdalg
006100150310     c                   eval      i63datal = vdatalg
006200150310     c                   movel(p)  trmz63ds      kpjbu
006300150313     c*****              call      'TRMZ63R1'
006400150313     C*****              PARM                    KPJBA
006500150312      *
006600150313     C                   MOVEL     'MZ6A'        KCOAZ
006700150313     c                   call      'BCH10'
006800150313     C                   PARM                    KPJBA
006900150306     c*
007000070530     c                   endsr
007100031105     c*-------------------------------------------------
007200031105     c     controv1      begsr
007300031105     c*-------------------------------------------------
007400150313     c                   setoff                                       96
007500150313      * SULLE WEEK
007600150313     c                   movel     VANNDA        AnnowwDA
007700150313     c                   move      VWEKDA        AnnowwDA
007800150313      *
007900150313     c                   movel     VANNA         AnnowwA
008000150313     c                   move      VWEKA         AnnowwA
008100150313      *
008200150313      *  se immesso uno dei 2 periodi occorre inserire anche l'altro
008300150313     c                   if        AnnowwDA >0 and AnnowwA =0
008400150313     c                               or
008500150313     c                             AnnowwA  >0 and AnnowwDA=0
008600150313     C                   SETON                                        5696
008700150313     c                   goto      endcontr
008800150313     c                   end
008900150313      *
009000150313      *  range di periodo
009100150313     c                   if        AnnowwDA > AnnowwA
009200150313     c                   seton                                        5396
009300150313     c                   goto      endcontr
009400150313     c                   endif
009500150313      *
009600150313      * ricavo la data INIZIO
009700150313     C/EXEC SQL
009800150313     C+ SELECT DISTINCT
009900150313     C+   date(days(DC_WEEK_STARTING_DATE)),
010000150313     C+   date(days(DC_WEEK_ENDING_DATE))
010100150313     C+ INTO  :da_Lunedi_scorso, :a_domenica_scorsa
010200150313     C+ FROM DATECNV0F
010300150313     C+ WHERE
010400150313     C+   DC_YOY_ISO = :VannDA and
010500150313     C+   DC_WOY_ISO = :VwekDA
010600150313     C/END-EXEC
010700150313      **
010800150313     c                   eval      dataiso = da_Lunedi_scorso
010900150313     c                   move      dataiso       dataeur
011000150313     c                   move      dataeur       vDATdal
011100150313     c                   exsr      ctrweek
011200150313     c                   exsr      NotaWeekDal
011300150313      **
011400150313      * ricavo la data FINE
011500150313     C/EXEC SQL
011600150313     C+ SELECT DISTINCT
011700150313     C+   date(days(DC_WEEK_STARTING_DATE)),
011800150313     C+   date(days(DC_WEEK_ENDING_DATE))
011900150313     C+ INTO  :da_Lunedi_scorso, :a_domenica_scorsa
012000150313     C+ FROM DATECNV0F
012100150313     C+ WHERE
012200150313     C+   DC_YOY_ISO = :VannA and
012300150313     C+   DC_WOY_ISO = :VwekA
012400150313     C/END-EXEC
012500150313      **
012600150313     c                   eval      dataiso = a_domenica_scorsa
012700150313     c                   move      dataiso       dataeur
012800150313     c                   move      dataeur       vDATal
012900150313     c                   exsr      ctrweek
013000150313     c                   exsr      NotaWeekAl
013100150313      **
013200150313      * SULLE DATE
013300150313      *____________________________
013400150311      *  se immessa una delle 2 date occorre inserire anche l'altra
013500150311     c                   if        vdatdal > 0 and vdatal = 0
013600150311     c                               or
013700150311     c                             vdatdal = 0 and vdatal > 0
013800150311     C                   SETON                                        4696
013900150311     c                   goto      endcontr
014000150311     c                   end
014100150311      *
014200150311      *  oppure se non ci sono le date
014300150311      *     si vuole analizzare la settimana passata
014400150311     c                   if        vdatdal = 0 and vdatal = 0
014500150311     c                   exsr      week_passata
014600150311      **
014700150311     c                   end
014800150310      *
014900150310     c                   if        vdatdal > 0
015000150310     C                   MOVE      Vdatdal       G02DAT
015100150310     C                   MOVEL     *BLANK        G02ERR
015200150310     C                   CALL      'XSRDA8'
015300150310     C                   PARM                    WLBDA8
015400150310     C     G02ERR        IFEQ      '1'
015500150311     C                   SETON                                        4296
015600150310     c                   goto      endcontr
015700150310     C                   END
015800150310     c                   move      g02dat        vdatdal
015900150310     c                   move      g02inv        vdatdalg          8 0
016000150311     c     *iso          move      g02inv        dataiso
016100150311     c                   exsr      ctrweek
016200150311     c                   exsr      NotaWeekDal
016300150311      *
016400150311     c   KF              if        vdatdal <> data08
016500150311     C                   SETON                                        4496
016600150311     c                   goto      endcontr
016700150311     c                   end
016800150311      **
016900150310     c                   else
017000150310     c                   clear                   vdatdalg          8 0
017100150311     c                   clear                   vNOTdal
017200150311     c                   clear                   daweekval
017300150310     c                   endif
017400150310      *
017500090518     c                   if        vdatal > 0
017600090518     C                   MOVE      Vdatal        G02DAT
017700070605     C                   MOVEL     *BLANK        G02ERR
017800070605     C                   CALL      'XSRDA8'
017900070605     C                   PARM                    WLBDA8
018000070605     C     G02ERR        IFEQ      '1'
018100150311     C                   SETON                                        4196
018200070605     c                   goto      endcontr
018300070605     C                   END
018400090518     c                   move      g02dat        vdatal
018500090518     c                   move      g02inv        vdatalg           8 0
018600150311     c     *iso          move      g02inv        dataiso
018700150311     c                   exsr      ctrweek
018800150311     c                   exsr      NotaWeekal
018900150311      *
019000150311     c   KF              if        vdatal <> data08
019100150311     C                   SETON                                        4596
019200150311     c                   goto      endcontr
019300150311     c                   end
019400150311      **
019500150306     c                   else
019600150306     c                   clear                   vdatalg           8 0
019700150311     c                   clear                   vNOTal
019800150311     c                   clear                   aweekval
019900070529     c                   endif
020000150310      *  range di date
020100150310     c                   if        vdatdalg > vdatalg
020200150310     c                   seton                                        4396
020300150310     c                   goto      endcontr
020400150310     c                   endif
020500150310      *
020600051122     c     endcontr      endsr
020700011026     c*-------------------------------------------------
020800011026     c     *inzsr        begsr
020900011026     c*-------------------------------------------------
021000020304     c     *entry        plist
021100020304     c                   parm                    kpjba
021200050801
021300020218     C                   time                    w0120            14 0
021400020218     C                   move      w0120         wdat              8 0
021500030630     C                   CLEAR                   Tibs36Ds
021600030630     C                   EVAL      I36ute = knmus
021700030630     C                   CALL      'TIBS36R'
021800030630     C                   PARM                    tibs36ds
021900150312      *
022000150312     C* INIZIALIZZA VARIABILI DI WRK
022100150312     C                   CLEAR                   TIBS34DS
022200150312     C                   CLEAR                   AZUTEDS
022300150312     C                   CLEAR                   DDATIUTE
022400150312     C*
022500150312     C     *DTAARA       DEFINE    §azute        azuteds
022600150312     C     *DTAARA       DEFINE    §datiute      ddatiute
022700150312     C                   IN(E)     *DTAARA
022800150312     C                   IF        %Error
022900150312     C                   EVAL      I34Tla = 'L'
023000150312     C                   CALL      'TIBS34R'
023100150312     C                   PARM                    Tibs34Ds
023200150312     C                   IN        *DTAARA
023300150312     C                   ENDIF
023400150311      **
023500150311     c                   exsr      week_passata
023600150311      **
023700150311     c                   endsr
023800150311     c*-------------------------------------------------
023900150311     c     week_passata  begsr
024000150311     c*-------------------------------------------------
024100150313      ***
024200150313      ***  deve prendere l'inizio anno relativamente ad OGGI
024300150313      ***  prendendo come riferimento OGGI
024400150313      ***    ricava il primo e ultimo giorno della settimana inizio Anno
024500150313      ***       lunedì da PROPORRE a VIDEO
024600150313     C/EXEC SQL
024700150313     C+ SELECT DISTINCT
024800150313     C+   date(days(DC_WEEK_STARTING_DATE)),
024900150313     C+   date(days(DC_WEEK_ENDING_DATE)),
025000150313     C+   curdate()
025100150313     C+ INTO  :da_Lunedi_scorso, :a_domenica_scorsa, :oggi
025200150313     C+ FROM DATECNV0F
025300150313     C+ WHERE DC_YOY_ISO = YEAR(curdate()) and DC_WOY_ISO = 1
025400150313     C/END-EXEC
025500150313      **
025600150313      **
025700150313     c                   eval      dataiso = da_Lunedi_scorso
025800150313     c                   move      dataiso       dataeur
025900150313     c                   move      dataeur       vDATdal
026000150313     c                   exsr      ctrweek
026100150313     c                   exsr      NotaWeekDal
026200150311      **
026300150310      ***
026400150310      ***  prendendo come riferimento OGGI
026500150310      ***    ricava il primo e ultimo giorno della settimana PRECEDENTE
026600150313      ***       domenica da PROPORRE a VIDEO
026700150310     C/EXEC SQL
026800150310     C+ SELECT DISTINCT
026900150310     C+   date(days(DC_WEEK_STARTING_DATE)-7),
027000150310     C+   date(days(DC_WEEK_ENDING_DATE)-7),
027100150310     C+   curdate()
027200150310     C+ INTO  :da_Lunedi_scorso, :a_domenica_scorsa, :oggi
027300150310     C+ FROM DATECNV0F
027400150310     C+ WHERE DC_DATE = curdate()
027500150310     C/END-EXEC
027600150310      **
027700150310     c                   eval      dataiso = a_domenica_scorsa
027800150310     c                   move      dataiso       dataeur
027900150310     c                   move      dataeur       vDATal
028000150311     c                   exsr      ctrweek
028100150311     c                   exsr      NotaWeekAl
028200150310      **
028300090518     c                   endsr
028400150310     c*-------------------------------------------------
028500150311     c     NotaWeekDal   begsr
028600150311     c*-------------------------------------------------
028700150311      **
028800150311     c                   eval      dataiso = data_Inizio_week
028900150311     c                   move      dataiso       dataeur
029000150311     c                   move      dataeur       data08            8 0
029100150311      **
029200150313     c                   eval      VnotDAL = '_________________'
029300150311     c                             + %Editc(data08:'Y')
029400150313      *
029500150311     c                   eval      daweekval = 'WK.' +
029600150311     c                             %Editc(Anno_week:'X') +
029700150311     c                             '/' + %Editc(week:'X')
029800150311      **
029900150313     c                   eval      VannDA  =  Anno_week
030000150313     c                   eval      VwekDA  =  week
030100150313      **
030200150311     c                   endsr
030300150311     c*-------------------------------------------------
030400150311     c     NotaWeekAl    begsr
030500150311     c*-------------------------------------------------
030600150311      **
030700150311     c                   eval      dataiso = data_Fine_week
030800150311     c                   move      dataiso       dataeur
030900150311     c                   move      dataeur       data08            8 0
031000150311      **
031100150313     c                   eval      VnotAL  = '_________________'
031200150311     c                             + %Editc(data08:'Y')
031300150313      **
031400150311     c                   eval      aweekval = 'WK.' +
031500150311     c                             %Editc(Anno_week:'X') +
031600150311     c                             '/' + %Editc(week:'X')
031700150311      **
031800150313     c                   eval      VannA   =  Anno_week
031900150313     c                   eval      VwekA   =  week
032000150313      **
032100150311     c                   endsr
032200150311     c*-------------------------------------------------
032300150311     c     ctrweek       begsr
032400150311     c*-------------------------------------------------
032500150311      **
032600150311      **    Ricava il Lunedì della Data INIZIO messa a video
032700150311      **
032800150311     C/EXEC SQL
032900150311     C+ SELECT DISTINCT
033000150311     C+   date(days(DC_WEEK_STARTING_DATE)),
033100150311     C+   date(days(DC_WEEK_ENDING_DATE)),
033200150311     C+   DC_YOY_ISO,
033300150311     C+   DC_WOY_ISO
033400150311     C+ INTO  :data_Inizio_week, :data_Fine_week, :Anno_week, :Week
033500150311     C+ FROM DATECNV0F
033600150311     C+ WHERE DC_DATE = :DataISO
033700150311     C/END-EXEC
033800150311      **
033900150311      **
034000150311     c                   endsr
034100150311     c*-------------------------------------------------
