000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200020409     H**********************************************************************
000300041018     fWFNRG00f  o  a E             disk    usropn
000400040929     F*--------
000500040929     FTABEL00F  IF   E           K DISK
000600040929     F*--------
000700041001     FazORG01L  IF   E           K DISK
000800041001     F*--------
000900040928     fFnlg51P   o    E             PRINTER OFLIND(*in66) usropn
001000040820     H**********************************************************************
001100040820     d KPJBA         E DS
001200050406     D tigcp00f      e DS
001300040929     D cnaco00f      e DS
001400040929     D ds2F          e DS
001500041125     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
001600041125     D  TCU                  398    697    DIM(50)
001700041125     D  KCU                  698    847P 0 DIM(50)
001800041125     D                                     PACKEVEN
001900041125     D CNCR80        E DS
002000040824     D*
002100040928     D fnlg50ds      e DS                  inz
002200040929     D  lnp                   24    113  0 dim(30)
002300041001     D  tcm                  114    123    Dim(10)
002400040928     D*
002500040820     D WLBDAT          DS
002600040820     D  G02DAT                 1      8  0
002700040820     D  G02INV                 9     16  0
002800040820     D  G02ERR                17     17
002900040820     D  G02TGI                18     22  0
003000040820      *
003100040825     D WrkSqlCmd       S           1020
003200040825     D Ln_sql          s             68    DIM(15)
003300040824      *
003400040820     D  data_Oggi      s                   like(G02INV) INZ(0)
003500040929     D  total70        s              7  0 INZ(0)
003600040929     D  total50        s              5  0 INZ(0)
003700041001     D  savlnp         s                   like(gcplnp)
003800040929     D  detlnp         s                   like(gcplnp)
003900040929     D  detscm         s                   like(gcpscm)
004000040929     D  dettcm         s                   like(gcptcm)
004100040929     D  detrag         s                   like(acorag)
004200041001     D  lnpdet_sav     s                   like(lnpdet)
004300041001     D  are            s                   like(orgcar)
004400041001     D  dis            s                   like(orgfl3)
004500041001     D  almeno_uno     s              1
004600041001     D  tTCM           s              1    Dim(50)
004700041001     D  tDCM           s             20    Dim(50)
004800041001      * in dettaglio e in stampa
004900041001     D  dTCM           s              1    Dim(12)
005000041001     D  dTot           s              5  0 Dim(12)
005100040929      *
005200040820     D digits          C                   CONST('0123456789')
005300040820      *****************************************************************
005400040903     c*  Ciclo principale
005500040928     c*
005600040928     c*   Apre il file di stampa e predispone a stampare testata con 66 = On
005700040929     c                   open      fnlg51p
005800040928     c* compongo istruzione sql
005900040928     c                   exsr      istruz
006000040929     c*  stampa prima pagina
006100040929     c                   exsr      pripag
006200040929     c                   seton                                        66
006300040928     C/EXEC SQL
006400040928     C+ PREPARE S1 FROM :WrkSqlCmd
006500040928     C/END-EXEC
006600040928
006700040928     C/EXEC SQL
006800040928     C+ DECLARE A1 CURSOR FOR S1
006900040928     C/END-EXEC
007000040928
007100040928     C/EXEC SQL
007200040928     C+ OPEN A1
007300040928     C/END-EXEC
007400040928     C                   DOU       SqlCod <> 0
007500040928     C/EXEC SQL
007600040929     C+ FETCH NEXT FROM A1 INTO :gcplnp, :gcpscm, :acorag, :total70
007700040928     C/END-EXEC
007800040928     C                   SELECT
007900040930     c*
008000040930     c* a fine file
008100040930     C                   WHEN      SqlCod = 100
008200041001     c*
008300041001     c                   if        almeno_uno ='S'
008400041001     c                   exsr      total_LNP
008500041001     c                   endIF
008600040930     c                   leave
008700040930     **
008800040930     C                   WHEN      SqlCod < 0
008900040930     C                   seton                                        H1
009000040930     c                   goto      fine
009100040928     c*
009200040930     C                   OTHER
009300041001     c*
009400041001     c*  imposta il totale x P.O. (LNP)
009500041001     c                   if        gcplnp <> savlnp and almeno_uno ='S'
009600041005     c                   if        savlnp > 0
009700041001     c                   exsr      total_LNP
009800041005     c                   end
009900041001     c                   eval      savlnp = gcplnp
010000041001     c                   endIF
010100041001     c*
010200040928     c* DETTAGLIO
010300040928     c                   if        *in66
010400040928      *                 * ------------------ *
010500040928     c                   write     testa
010600040928     c                   write     tesdetta
010700040928      *                 * ------------------ *
010800040928     c                   setoff                                       66
010900040928     c                   end
011000040929     c* imposta dettaglio
011100040929     c                   clear                   dettagli
011200040928     c                   exsr      imp_rig_det
011300040929      **  stampa la riga di dettaglio
011400040928     c                   write     dettagli
011500041001     c                   eval      almeno_uno = 'S'
011600040929      *
011700041001     c                   exsr      wrtDETfile
011800040928     c*
011900040928     C                   ENDSL
012000040929      *
012100040928     C                   ENDDO
012200040928     C/EXEC SQL
012300040928     C+ CLOSE A1
012400040928     C/END-EXEC
012500040928     c*
012600040928     C* totali generali
012700040928     c                   if        *in66
012800040928      *                 * ------------------ *
012900040928     c                   write     testa
013000040928     c                   write     tesdetta
013100040928      *                 * ------------------ *
013200040928     c                   setoff                                       66
013300040928     c                   end
013400040928     c                   write     totale
013500040929      *
013600040930     c     fine          tag
013700040929     c                   close     fnlg51p
013800040928      *
013900020326     C                   SETON                                        LR
014000040820     C**************************************************************************
014100040903     C* mi compongo l'istruzione sql
014200040903     C**************************************************************************
014300040903     C     istruz        BEGSR
014400040903      **
014500040929      **  esempio di richiesta SQL:
014600040929      **    select * from ( SELECT gcplnp, GCPSCM, acorag,
014700040929      **    DECIMAL(COUNT(*), 7, 0) as n_giac FROM
014800050406      **    tigcp00f join cnaco00f on acoksc=gcpscm WHERE acokcc=151 and
014900041005      **    ((GCPAGC*10000+GCPMGC) between 20040801 and 20040831 or gcpdur
015000040929      **    between 20040801 and 20040831) and gcptcm not in (' ', 'R')
015100040929      **    and gcplnp in (43, 63)
015200040929      **    GROUP BY gcplnp, GCPSCM, acorag ORDER BY gcplnp, GCPSCM)
015300040929      **    as a where n_giac > 15 order by n_giac desc
015400040929      **
015500050406     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file tIGCP
015600040903     C                   EVAL      WrkSqlCmd
015700040929     C                             = 'SELECT * FROM '                          +
015800040928     C                             '(SELECT gcplnp, gcpscm, acorag, '          +
015900040929     C                             ' DECIMAL(COUNT(*), 7, 0) AS n_giac '       +
016000050406     C                             ' FROM tigcp00f JOIN cnaco00f ON '          +
016100040929     C                             'acoksc = gcpscm WHERE acokcc = 151 AND '   +
016200041005     C                             '((gcpagc * 10000 + gcpmgc) BETWEEN '       +
016300040929     C                             %TRIM(%CHAR(d50dsd)) + ' AND '              +
016400040929     C                             %TRIM(%CHAR(d50dsa))                        +
016500040929     C                             ' OR gcpdur BETWEEN '                       +
016600040929     C                             %TRIM(%CHAR(d50dsd)) + ' AND '              +
016700040929     C                             %TRIM(%CHAR(d50dsa))                        +
016800040929     c                             ') AND gcptcm NOT IN ('' '', ''R'') '
016900040928     ***
017000040928     ***   qui si devono inserire i filtri delle selezioni a video
017100050309      **  + CLIENTE
017200050309     c                   if        d50sed = *blank
017300050309     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)               +
017400050406     c                             ' AND gcpsede = '' '''
017500050309     c                   end
017600040929      **  + CLIENTE
017700040929     c                   if        d50ksc <> *all'9'
017800040929     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)               +
017900040929     c                             ' AND acoksc =' + %TRIM(%CHAR(d50ksc))
018000040929     c                   end
018100040929      **  + LNP
018200040929     c                   if        d50l01 <> 046
018300040929     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)               +
018400040929     C                             ' AND gcplnp IN ('
018500040929     c                   do        30            y                 3 0
018600040929      **
018700040929     c                   if        lnp(y) <> 0
018800040929     c                   if        y > 1
018900040930     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd) + ' ,'
019000040929     c                   end
019100040930     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd) + ' ' +
019200040930     C                             %CHAR(lnp(y))
019300040930     C******                       %TRIM(%CHAR(lnp(y)))
019400040929     c                   end
019500040929     c                   if        y = 30
019600040930     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd) + ')'
019700040929     c                   end
019800040929      **
019900040929     c                   end
020000040929     c                   end
020100040929      **  + TCM
020200040929     c                   if        tcm(1) <> '9'
020300040929     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)               +
020400040929     C                             ' AND gcptcm IN ('
020500040929     c                   do        10            y                 3 0
020600040929      **
020700040929     c                   if        tcm(y) <> ' '
020800040929     c                   if        y > 1
020900040929     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd) + ', '
021000040929     c                   end
021100040929     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd) + '''' +
021200040929     C                             %TRIM(tcm(y))  + ''''
021300040929     c                   end
021400040929     c                   if        y = 10
021500040929     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd) + ')'
021600040929     c                   end
021700040929      **
021800040929     c                   end
021900040929     c                   end
022000040929     ***
022100040929     ***   in ultimo deve chiudersi con il GROUP BY
022200040928     ***
022300040928     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)               +
022400041005     c                             ' GROUP BY gcplnp, gcpscm, acorag)'         +
022500041005     c                             '  AS a_file'                               +
022600040929     c                             ' WHERE n_giac > ' + %TRIM(%CHAR(d50sup))   +
022700041006     C                             ' ORDER BY gcplnp, n_giac DESC, gcpscm '
022800040903      **
022900040903      ** Stampa in prima pagina
023000041001     c                   movea     WrkSqlCmd     Ln_sql
023100041001     c                   if        sqlprt = 'S'
023200041001     c                   move      'SQL CMD  -->'sqlcmd
023300040903     c                   eval      lin1 = Ln_sql(1)
023400040903     c                   eval      lin2 = Ln_sql(2)
023500040903     c                   eval      lin3 = Ln_sql(3)
023600040903     c                   eval      lin4 = Ln_sql(4)
023700040903     c                   eval      lin5 = Ln_sql(5)
023800040903     c                   eval      lin6 = Ln_sql(6)
023900040903     c                   eval      lin7 = Ln_sql(7)
024000040903     c                   eval      lin8 = Ln_sql(8)
024100040903     c                   eval      lin9 = Ln_sql(9)
024200040903     c                   eval      lin10 = Ln_sql(10)
024300040903     c                   eval      lin11 = Ln_sql(11)
024400040903     c                   eval      lin12 = Ln_sql(12)
024500040903     c                   eval      lin13 = Ln_sql(13)
024600040903     c                   eval      lin14 = Ln_sql(14)
024700040903     c                   eval      lin15 = Ln_sql(15)
024800041001     c                   end
024900040903      **
025000040903     C                   ENDSR
025100020326     C**************************************************************************
025200040820      *
025300040820     C**************************************************************************
025400040831     C     imp_rig_det   begSR
025500040820      **
025600040929     c                   clear                   dtot
025700040929     c                   clear                   total50
025800040929      **
025900040929      **  a questo punto esegue un SQL x prendere i valori x cliente
026000040929      **   dei tipi comunicazione effettuati
026100040929     C/EXEC SQL
026200040929     C+ DECLARE S2 CURSOR FOR
026300040929     c+   SELECT gcplnp, gcpscm, gcptcm, DECIMAL(COUNT(*), 5, 0) AS total50
026400050406     c+   FROM tigcp00f WHERE
026500041005     c+   ((gcpagc*10000+gcpmgc) BETWEEN :d50dsd AND :d50dsa
026600041005     c+   OR              gcpdur BETWEEN :d50dsd AND :d50dsa)
026700040929     c+   AND gcptcm NOT IN (' ', 'R')
026800040929     c+   AND gcpscm = :gcpscm
026900040929     c+   AND gcplnp = :gcplnp
027000040929     c+   GROUP BY gcplnp, gcpscm, gcptcm
027100040929     c+   ORDER BY gcplnp, gcpscm, gcptcm
027200040929     C/END-EXEC
027300040929      *          apertura cursore
027400040929     C/EXEC SQL
027500040929     C+ OPEN S2
027600040929     C/END-EXEC
027700040929     c                   z-add     0             nr                3 0
027800040929      *
027900040929     C                   DOU       SqlCod = 100 OR SqlCod < 0
028000040929      *          lettura cursore
028100040929     C/EXEC SQL
028200040929     C+ FETCH NEXT FROM S2 INTO :detlnp, :detscm, :dettcm, :total50
028300040929     C/END-EXEC
028400040929     C                   SELECT
028500040929     C                   WHEN      SqlCod = 100
028600040930     c                   leave
028700040929      *
028800040930     C                   WHEN      SqlCod <  0
028900040930     c                   seton                                        H2
029000040930     c                   leave
029100040930      *
029200040930     C                   OTHER
029300040930      * lettura buona
029400041001     c                   if        nr =12
029500040929     c                   leave
029600040929     c                   end
029700040929      **
029800040929     c                   z-add     1             find              3 0
029900040929     c     dettcm        lookup    tcm(find)                              30
030000040929      **
030100040929     c                   if        d50t01 = '9' or *in30
030200041001     c                   eval      nr = nr + 1
030300041001     c     dettcm        lookup    dtcm(nr)                               30
030400041001     c   30              eval      dtot(nr) = total50
030500040929     c                   end
030600040929      *
030700040929     C                   ENDSL
030800040929      *
030900040929     C                   ENDDO
031000040929     C/EXEC SQL
031100040929     C+ CLOSE S2
031200040929     C/END-EXEC
031300040929      *
031400040929      *  imposta la riga di dettaglio in stampa
031500040929     c                   eval      lnpdet = gcplnp
031600040929     c                   eval      kscdet = gcpscm
031700040929     c                   eval      desrag = acorag
031800041001      *
031900041001     c                   eval      nr01   = dtot(1)
032000041001     c                   eval      nr02   = dtot(2)
032100041001     c                   eval      nr03   = dtot(3)
032200041001     c                   eval      nr04   = dtot(4)
032300041001     c                   eval      nr05   = dtot(5)
032400041001     c                   eval      nr06   = dtot(6)
032500041001     c                   eval      nr07   = dtot(7)
032600041001     c                   eval      nr08   = dtot(8)
032700041001     c                   eval      nr09   = dtot(9)
032800041001     c                   eval      nr10   = dtot(10)
032900041001     c                   eval      nr11   = dtot(11)
033000041001     c                   eval      nr12   = dtot(12)
033100041001     c                   eval      nrTot  = total70
033200041001      *
033300041001      *  Totali x Punto Operativo
033400041001     c                   add       nr01          tp01
033500041001     c                   add       nr02          tp02
033600041001     c                   add       nr03          tp03
033700041001     c                   add       nr04          tp04
033800041001     c                   add       nr05          tp05
033900041001     c                   add       nr06          tp06
034000041001     c                   add       nr07          tp07
034100041001     c                   add       nr08          tp08
034200041001     c                   add       nr09          tp09
034300041001     c                   add       nr10          tp10
034400041001     c                   add       nr11          tp11
034500041001     c                   add       nr12          tp12
034600041001     c                   add       nrtot         tptot
034700040929      *
034800040826     C                   ENDSR
034900040929     C**************************************************************************
035000040929     C* prima pagina
035100040929     C**************************************************************************
035200040929     C     pripag        BEGSR
035300040929      **
035400040929     c                   movea     '0000'        *in(1)
035500040929     c                   movea     '0000'        *in(11)
035600040929      **
035700040929     c                   write     testa
035800040929     c                   write     pripage
035900040929     c                   if        %subst(knmus:1:3) = 'EDP'
036000040929     c                   write     secpage
036100040929     c                   end
036200040929      *
036300040929     C                   ENDSR
036400040929     C**************************************************************************
036500041001     C* Totali x P.O.
036600040929     C**************************************************************************
036700041001     C     Total_LNP     BEGSR
036800041001      *
036900041001      *  aggiunge ai Totali Generali
037000041001     c                   add       tp01          tg01
037100041001     c                   add       tp02          tg02
037200041001     c                   add       tp03          tg03
037300041001     c                   add       tp04          tg04
037400041001     c                   add       tp05          tg05
037500041001     c                   add       tp06          tg06
037600041001     c                   add       tp07          tg07
037700041001     c                   add       tp08          tg08
037800041001     c                   add       tp09          tg09
037900041001     c                   add       tp10          tg10
038000041001     c                   add       tp11          tg11
038100041001     c                   add       tp12          tg12
038200041001     c                   add       tptot         tgtot
038300041001     C* totali x P.O.
038400041001     c                   if        *in66
038500041001      *                 * ------------------ *
038600041001     c                   write     testa
038700041001     c                   write     tesdetta
038800041001      *                 * ------------------ *
038900041001     c                   setoff                                       66
039000041001     c                   end
039100041001     c                   write     totxPO
039200041001      **
039300041001      ** scrive il WRKfile x P.O.
039400041001     c                   exsr      wrtTOTfile
039500041001      **
039600041001     c                   clear                   tp01
039700041001     c                   clear                   tp02
039800041001     c                   clear                   tp03
039900041001     c                   clear                   tp04
040000041001     c                   clear                   tp05
040100041001     c                   clear                   tp06
040200041001     c                   clear                   tp07
040300041001     c                   clear                   tp08
040400041001     c                   clear                   tp09
040500041001     c                   clear                   tp10
040600041001     c                   clear                   tp11
040700041001     c                   clear                   tp12
040800041001     c                   clear                   tptot
040900041001      **
041000041001     C                   ENDSR
041100041001     C**************************************************************************
041200041001     C* scrive il workfile x i totali solo se si tratta di una richiesta di sede
041300041001     C**************************************************************************
041400041001     C     wrtTOTfile    BEGSR
041500041001      **
041600041001     c                   if        d50sed ='S' and savlnp > 0
041700041001      **
041800041001     c                   clear                   wfnrg000
041900041001      **
042000041001     c                   move      dadata        wngdda
042100041001     c                   move      adata         wngada
042200041001     c                   move      dis           wngdis
042300041001     c                   move      are           wngare
042400041001      **
042500041001     c                   move      savlnp        wnglnp
042600041001     c                   move      '1'           wngtrc
042700041001      *
042800041001     c                   eval      WNGNRE = tp01
042900041001     c                   eval      WNGNRF = tp02
043000041001     c                   eval      WNGNRI = tp03
043100041001     c                   eval      WNGNRM = tp04
043200041001     c                   eval      WNGNRN = tp05
043300041001     c                   eval      WNGNRP = tp06
043400041001     c                   eval      WNGNRS = tp07
043500041001     c                   eval      WNGNRT = tp08
043600041001     c                   eval      WNGNRV = tp09
043700041001     c                   eval      WNGNRW = tp10
043800041001     c                   eval      WNGNRX = tp11
043900041001     c                   eval      WNGNRZ = tp12
044000041001     c                   eval      wngtot = tpTot
044100041001      **
044200041001      *   se si tratta della sede deve scrivere il file di work
044300041001     c                   write     wfnrg000
044400041001     c                   end
044500041001      **
044600041001      **
044700041001     C                   ENDSR
044800041001     C**************************************************************************
044900041001     C* scrive il workfile solo se si tratta di una richiesta della sede
045000041001     C**************************************************************************
045100041001     C     wrtDETfile    BEGSR
045200041001      **
045300040929     c                   if        d50sed ='S'
045400041001      **
045500041001     c                   clear                   wfnrg000
045600041001      **
045700041001      **  decodifica l'area ed il distretto
045800041001     c                   if        lnpdet <> lnpdet_sav
045900041001     c     lnpdet        chain     azorg01l
046000041001     c                   if        %found(azorg01l)
046100041001     c                   eval      are = orgcar
046200041001     c                   eval      dis = orgfl3
046300041001     c                   end
046400041001     c                   eval      lnpdet_sav = lnpdet
046500041001     c                   end
046600041001      **
046700040929     c                   move      dadata        wngdda
046800040929     c                   move      adata         wngada
046900041001     c                   move      dis           wngdis
047000041001     c                   move      are           wngare
047100041001      **
047200041001     c                   move      lnpdet        wnglnp
047300040929     c                   move      kscdet        wngksc
047400040929     c                   movel     desrag        wngrag
047500041001      *
047600041001     c                   eval      WNGNRE = nr01
047700041001     c                   eval      WNGNRF = nr02
047800041001     c                   eval      WNGNRI = nr03
047900041001     c                   eval      WNGNRM = nr04
048000041001     c                   eval      WNGNRN = nr05
048100041001     c                   eval      WNGNRP = nr06
048200041001     c                   eval      WNGNRS = nr07
048300041001     c                   eval      WNGNRT = nr08
048400041001     c                   eval      WNGNRV = nr09
048500041001     c                   eval      WNGNRW = nr10
048600041001     c                   eval      WNGNRX = nr11
048700041001     c                   eval      WNGNRZ = nr12
048800041001     c                   eval      wngtot = nrTot
048900040929      **
049000040929      *   se si tratta della sede deve scrivere il file di work
049100040929     c                   write     wfnrg000
049200040929     c                   end
049300040929      **
049400040929     C                   ENDSR
049500041001     ***********************************************************************
049600041001     C* INIZIALIZZAZIONE
049700041001     C**************************************************************************
049800041001     C     *inzsr        BEGSR
049900041001      **
050000041001     C     *ENTRY        PLIST
050100041001     C                   PARM                    KPJBA
050200041001     c                   movel     kpjbu         fnlg50ds
050300041005     c                   move      kpjbu         sqlprt            1
050400041125     C* Richiamo XPARUT
050500041125     C                   Z-ADD     1             CODUT
050600041125     C                   CALL      'X§PARUT'
050700041125     C                   PARM                    UTEDSE
050800041125     C                   MOVEL     RAGUT         RSUT
050900041125     C                   MOVEL     REC80         CNCR80
051000041125     C                   MOVEL     SIMFEL        STPFEL
051100041018      **
051200041018      **  solo se x utenti di sede
051300041018     c                   if        d50sed = 'S'
051400041018     c                   open      wfnrg00f
051500041018     c                   end
051600041001      **
051700041001     C     *LIKE         DEFINE    TBLCOD        KCOD
051800041001     C     *LIKE         DEFINE    TBLKEY        KKEY
051900041001     C     *LIKE         DEFINE    ACOKUT        KKUT
052000041001     C*  KLIST
052100041001     C     KTAB          KLIST
052200041001     C                   KFLD                    KKUT
052300041001     C                   KFLD                    KCOD
052400041001      *
052500041001      *  imposta UDATE di elaborazione
052600041001     C                   move      *date         G02DAT
052700041001     C                   MOVE      *ZEROS        G02INV
052800041001     C                   MOVE      *BLANKS       G02ERR
052900041001     C                   CALL      'XSRDA8'
053000041001     C                   PARM                    WLBDAT
053100041001     C                   Z-ADD     G02INV        data_oggi
053200041125     C                   move      data_oggi     day               8 0
053300041001     C                   TIME                    W0140            14 0
053400041125     C                   MOVEl     W0140         ore               6 0
053500041001      **  date
053600041125     c     *like         define    D50DSD        dadata
053700041125     c     *like         define    D50DSA        adata
053800041125     c                   move      D50DSD        dadata
053900041001     c                   move      D50DSA        adata
054000041125      *
054100041125     C                   move      D50dsd        G02INV
054200041125     C                   MOVE      *zeros        G02DAT
054300041125     C                   MOVE      '3'           G02ERR
054400041125     C                   CALL      'XSRDA8'
054500041125     C                   PARM                    WLBDAT
054600041125     C                   Z-ADD     G02DAT        datada
054700041125      *
054800041125     C                   move      D50dsa        G02INV
054900041125     C                   MOVE      *ZEROS        G02DAT
055000041125     C                   MOVE      '3'           G02ERR
055100041125     C                   CALL      'XSRDA8'
055200041125     C                   PARM                    WLBDAT
055300041125     C                   Z-ADD     G02DAT        dataa
055400041001      **  prima pagina
055500041001     c                   eval      ksc    = d50ksc
055600041001     c                   eval      dksc   = d50rag
055700041001     c                   if        d50l01 = 046
055800041001     c                   eval      lnp01  = 999
055900041001     c                   else
056000041001     c                   eval      lnp01  = d50l01
056100041001     c                   end
056200041001     c                   eval      lnp02  = d50l02
056300041001     c                   eval      lnp03  = d50l03
056400041001     c                   eval      lnp04  = d50l04
056500041001     c                   eval      lnp05  = d50l05
056600041001     c                   eval      lnp06  = d50l06
056700041001     c                   eval      lnp07  = d50l07
056800041001     c                   eval      lnp08  = d50l08
056900041001     c                   eval      lnp09  = d50l09
057000041001     c                   eval      lnp10  = d50l10
057100041125     c                   eval      lnp11  = d50l11
057200041125     c                   eval      lnp12  = d50l12
057300041125     c                   eval      lnp13  = d50l13
057400041125     c                   eval      lnp14  = d50l14
057500041125     c                   eval      lnp15  = d50l15
057600041125     c                   eval      lnp16  = d50l16
057700041125     c                   eval      lnp17  = d50l17
057800041125     c                   eval      lnp18  = d50l18
057900041125     c                   eval      lnp19  = d50l19
058000041125     c                   eval      lnp20  = d50l20
058100041125     c                   eval      lnp21  = d50l21
058200041125     c                   eval      lnp22  = d50l22
058300041125     c                   eval      lnp23  = d50l23
058400041125     c                   eval      lnp24  = d50l24
058500041125     c                   eval      lnp25  = d50l25
058600041125     c                   eval      lnp26  = d50l26
058700041125     c                   eval      lnp27  = d50l27
058800041125     c                   eval      lnp28  = d50l28
058900041125     c                   eval      lnp29  = d50l29
059000041125     c                   eval      lnp30  = d50l30
059100041001     c                   eval      tc1d50 = d50t01
059200041001     c                   eval      tc2d50 = d50t02
059300041001     c                   eval      tc3d50 = d50t03
059400041001     c                   eval      tc4d50 = d50t04
059500041001     c                   eval      tc5d50 = d50t05
059600041001     c                   eval      tc6d50 = d50t06
059700041001     c                   eval      tc7d50 = d50t07
059800041001     c                   eval      tc8d50 = d50t08
059900041001     c                   eval      tc9d50 = d50t09
060000041001     c                   eval      tc0d50 = d50t10
060100041001     c                   eval      sup    = d50sup
060200041001     c                   movel     'Sede'        sedfil
060300041001     c                   if        d50sed <> 'S'
060400041125     c                   movel     'Filiale'     sedfil            7
060500041001     c                   end
060600041001      *
060700041001     C                   z-add     0             T                 3 0
060800041001     C                   z-add     1             Kkut
060900041001     C                   MOVEL     '2F'          Kcod
061000041001     C     KTAB          setll     TABEL00F
061100041001     C     KTAB          reade     TABEL00F
061200041001     c                   dow       not %eof(Tabel00F)
061300041001     C                   MOVEL     TBLUNI        DS2F
061400041001     c                   if        %subst(tblkey: 1: 1) <> *blank and
061500041001     c                             %subst(tblkey: 1: 1) <> 'R'
061600041001     C                   eval      T = T + 1
061700041001     c                   eval      ttcm (T) = %subst(tblkey: 1: 1)
061800041001     c                   eval      tdcm (T) = §2FDES
061900041001     C                   end
062000041001     C     KTAB          reade     TABEL00F
062100041001     C                   ENDdo
062200041001      **
062300041001      **   imposta schiera Tipi Comunicazioni in stampa
062400041001     c                   eval      tcm01  = ttcm(1)
062500041001     c                   eval      tcm02  = ttcm(2)
062600041001     c                   eval      tcm03  = ttcm(3)
062700041001     c                   eval      tcm04  = ttcm(4)
062800041001     c                   eval      tcm05  = ttcm(5)
062900041001     c                   eval      tcm06  = ttcm(6)
063000041001     c                   eval      tcm07  = ttcm(7)
063100041001     c                   eval      tcm08  = ttcm(8)
063200041001     c                   eval      tcm09  = ttcm(9)
063300041001     c                   eval      tcm10  = ttcm(10)
063400041001     c                   eval      tcm11  = ttcm(11)
063500041001     c                   eval      tcm12  = ttcm(12)
063600041001     c                   eval      tcm13  = ttcm(13)
063700041001      *
063800041001     c                   eval      dcm01  = tdcm(1)
063900041001     c                   eval      dcm02  = tdcm(2)
064000041001     c                   eval      dcm03  = tdcm(3)
064100041001     c                   eval      dcm04  = tdcm(4)
064200041001     c                   eval      dcm05  = tdcm(5)
064300041001     c                   eval      dcm06  = tdcm(6)
064400041001     c                   eval      dcm07  = tdcm(7)
064500041001     c                   eval      dcm08  = tdcm(8)
064600041001     c                   eval      dcm09  = tdcm(9)
064700041001     c                   eval      dcm10  = tdcm(10)
064800041001     c                   eval      dcm11  = tdcm(11)
064900041001     c                   eval      dcm12  = tdcm(12)
065000041001     c                   eval      dcm13  = tdcm(13)
065100041001      **  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - **
065200041001      * Fissa le colonne della stampa uguali a quelle del Workfile
065300041001      **  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - **
065400041001     c                   eval      dtcm(1) = 'E'
065500041001     c                   eval      dtcm(2) = 'F'
065600041001     c                   eval      dtcm(3) = 'I'
065700041001     c                   eval      dtcm(4) = 'M'
065800041001     c                   eval      dtcm(5) = 'N'
065900041001     c                   eval      dtcm(6) = 'P'
066000041001     c                   eval      dtcm(7) = 'S'
066100041001     c                   eval      dtcm(8) = 'T'
066200041001     c                   eval      dtcm(9) = 'V'
066300041001     c                   eval      dtcm(10)= 'W'
066400041001     c                   eval      dtcm(11)= 'X'
066500041001     c                   eval      dtcm(12)= 'Z'
066600041001      **  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - **
066700041001      **  Quindi riporta i campi di testata                             **
066800041001      **  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - **
066900041001     c                   eval      tc01    = dtcm(1)
067000041001     c                   eval      tc02    = dtcm(2)
067100041001     c                   eval      tc03    = dtcm(3)
067200041001     c                   eval      tc04    = dtcm(4)
067300041001     c                   eval      tc05    = dtcm(5)
067400041001     c                   eval      tc06    = dtcm(6)
067500041001     c                   eval      tc07    = dtcm(7)
067600041001     c                   eval      tc08    = dtcm(8)
067700041001     c                   eval      tc09    = dtcm(9)
067800041001     c                   eval      tc10    = dtcm(10)
067900041001     c                   eval      tc11    = dtcm(11)
068000041001     c                   eval      tc12    = dtcm(12)
068100041001      *
068200041001     c                   clear                   almeno_uno
068300041001      *
068400041001     C                   ENDSR
068500040929     C**************************************************************************
