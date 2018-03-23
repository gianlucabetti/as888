000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200020409     H**********************************************************************
000300040823     fFIEU83P   o    E             PRINTER OFLIND(*in66)
000400040820     H**********************************************************************
000500040820     d KPJBA         E DS
000600040820     D*
000700040820     D tnecf00f      e DS
000800041103     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
000900041103     D CNCR80        E DS
001000040820     D fieu82ds      e DS                  inz
001100040831     d  exlgrp                54     83
001200040820     D*
001300040820     D WLBDAT          DS
001400040820     D  G02DAT                 1      8  0
001500040820     D  G02INV                 9     16  0
001600040820     D  G02ERR                17     17
001700040820     D  G02TGI                18     22  0
001800040820      *
001900040820     D WrkSqlCmd       S           1024
002000040820     D  data_Oggi      s                   like(G02INV) INZ(0)
002100040820      *
002200040820     D  savdfp         s                   like(ecfdfp)
002300040820     D  savnfp         s                   like(ecfnfp)
002400040823     D  totfat         s                   like(ecfimp) INZ(0)
002500040823     D  totgen         s                   like(ecfimp) INZ(0)
002600040826     D  almeno_uno     s              1
002700040823     D GRp             S              3    DIM(50) INZ(' ')
002800040823     D IMp             S             16  3 DIM(50) INZ(0)
002900040820      *
003000040820     D digits          C                   CONST('0123456789')
003100040820      *****************************************************************
003200011210     C/EXEC SQL
003300011210     C+ PREPARE S1 FROM :WrkSqlCmd
003400011210     C/END-EXEC
003500011210
003600011210     C/EXEC SQL
003700011210     C+ DECLARE A1 CURSOR FOR S1
003800011210     C/END-EXEC
003900011210
004000011210     C/EXEC SQL
004100011210     C+ OPEN A1
004200011210     C/END-EXEC
004300040820
004400040820     C                   DOU       SqlCod <> 0
004500040823     c*
004600011205     C/EXEC SQL
004700040820     C+ FETCH NEXT FROM A1 INTO :tnecf00f
004800011205     C/END-EXEC
004900011210     C                   SELECT
005000041007     c*
005100041007     c* a fine file Totali x rotture
005200041007     C                   WHEN      SqlCod = 100
005300041007     c                   if        almeno_uno ='S'
005400041007     c                   exsr      TOTals
005500041007     c                   else
005600041007     c                   write     testa
005700041007     c                   write     tesdetta
005800041007     c                   write     NODATI
005900041007     c                   end
006000041007     c                   leave
006100041007     **
006200041007     C                   WHEN      SqlCod < 0
006300041007     C                   seton                                        H1
006400041007     c                   goto      fine
006500040820     c*
006600041007     C                   OTHER
006700040825     c* Totali x rotture
006800040825     c                   exsr      TOTals
006900040820     c* DETTAGLIO
007000040823     c                   if        *in66
007100040823      *                 * ------------------ *
007200040823     c                   write     testa
007300040823     c                   write     tesdetta
007400040823      *                 * ------------------ *
007500040823     c                   setoff                                       66
007600040825     c                   eval      prima_volta = 'N'
007700040823     c                   end
007800040823      *
007900040823     c                   if        *in04 and ecfdta = 0 or
008000040823     c                             *in05 and ecfdta > 0
008100040820     c                   write     dettagli
008200040823     c                   end
008300040823     c*
008400040823     c* Totalizzo
008500040826     c                   move      'S'           almeno_uno
008600040823     c                   add       ecfimp        totfat
008700040823     c                   add       ecfimp        totgen
008800040823      *
008900040823      * Incasella l'importo nel gruppo di appartenenza
009000040823     c                   z-add     1             Ip                3 0
009100040823     c     ecfGRP        lookup    Grp(Ip)                                21
009200040823     c  n21              z-add     1             Ip
009300040823     c  n21*blank        lookup    Grp(Ip)                                21
009400040823     c                   movel     ecfGRP        grP(Ip)
009500040823     c                   add       ecfIMP        ImP(Ip)
009600040820     c*
009700011210     C                   ENDSL
009800040820
009900011205     C                   ENDDO
010000011205     C/EXEC SQL
010100011205     C+ CLOSE A1
010200011205     C/END-EXEC
010300011205     c*
010400040820     C* totali generali
010500041007     c                   if        almeno_uno ='S'
010600040823     c                   if        *in66
010700040823      *                 * ------------------ *
010800040823     c                   write     testa
010900040823     c                   write     tesdetta
011000040823      *                 * ------------------ *
011100040823     c                   setoff                                       66
011200040823     c                   end
011300040823     c                   write     totale
011400041007     c                   end
011500040823      *
011600041007     c     fine          tag
011700020326     C                   SETON                                        LR
011800040820     C**************************************************************************
011900040820     C* INIZIALIZZAZIONE
012000040820     C**************************************************************************
012100040820     C     *inzsr        BEGSR
012200040820      **
012300040820     C     *ENTRY        PLIST
012400040820     C                   PARM                    KPJBA
012500040820     c                   movel     kpjbu         fieu82ds
012600041103      *
012700041103     C                   Z-ADD     1             CODUT
012800041103     C                   CALL      'X§PARUT'
012900041103     C                   PARM                    UT§DSE
013000041103     C                   MOVEL     REC80         CNCR80
013100041103     C                   MOVEL     RAGUT         RSUT             20
013200041103     C*
013300040820      *
013400040820      *  imposta UDATE di elaborazione
013500040820     C                   move      *date         G02DAT
013600040820     C                   MOVE      *ZEROS        G02INV
013700040820     C                   MOVE      *BLANKS       G02ERR
013800040820     C                   CALL      'XSRDA8'
013900040820     C                   PARM                    WLBDAT
014000040820     C                   Z-ADD     G02INV        data_oggi
014100040820     C                   move      data_oggi     day
014200040820     C                   TIME                    W0140            14 0
014300040820     C                   MOVEl     W0140         ore
014400040823      **
014500040820     c                   eval      partner = e82ntw
014600040820     c                   move      e82dti        dadata
014700040820     c                   move      e82dtf        adata
014800040823      **  E82NFP
014900040820     c                   if        e82nfp <> *blank
015000040820     c                   eval      nrfat   = e82nfp
015100040820     c                   seton                                        01
015200040820     c                   end
015300040823      **  E82DIN  E82DFI
015400040820     c                   if        e82din <> *zero
015500040820     c                   move      e82din        dainca
015600040820     c                   move      e82dfi        ainca
015700040820     c                   seton                                        02
015800040820     c                   end
015900040823      **  E82TSN
016000040823     c                   if        e82tsn = 'S'
016100040823     c                   seton                                        03
016200040823     c                   end
016300040823      **  E82NIN
016400040823     c                   if        e82nin = 'S'
016500040823     c                   seton                                        04
016600040823     c                   end
016700040823      **  E82SIN
016800040823     c                   if        e82sin = 'S'
016900040823     c                   seton                                        05
017000040823     c                   end
017100040823      **
017200040823     c                   seton                                        66
017300040825     c                   clear                   prima_volta       1
017400040826     c                   clear                   almeno_uno
017500040820     c*
017600040820     c* compongo istruzione sql
017700040820     c                   exsr      istruz
017800040820     c*
017900040820     C                   ENDSR
018000040820     C**************************************************************************
018100040820     C* mi compongo l'istruzione sql
018200040820     C**************************************************************************
018300040820     C     istruz        BEGSR                                                  *
018400040820      **
018500040820     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file TNECF
018600040820     C                   EVAL      WrkSqlCmd
018700040820     C                             =
018800040823     C                             'SELECT * from TNECF00F where'
018900040823     c                              + ' ecfntw =' + '''' + %TRIM(e82ntw) + ''''
019000040823     c                              + ' and  ecfdfp between '
019100040823     c                              + %trim(%char(e82dti)) +  ' and '
019200040823     c                              + %trim(%char(e82dtf))
019300040820      **  + nr.fattura partner
019400040820     c                   if        e82nfp <> *blank
019500040820     C                   EVAL      WrkSqlCmd
019600040820     C                             =
019700040820     C                             %TRIMR(WrkSqlCmd)
019800040823     c                             + ' and ecfnfp =' + '''' + %trim(e82nfp)
019900040823     c                             +  ''''
020000040820     c                   end
020100040820      **  + data incasellamento
020200040820     c                   if        e82din > 0
020300040820     C                   EVAL      WrkSqlCmd
020400040820     C                             =
020500040820     C                             %TRIMR(WrkSqlCmd)
020600040823     c                             + ' and ecfdta between '
020700040823     c                             + %trim(%char(e82din)) +  ' and '
020800040823     c                             + %trim(%char(e82dfi))
020900040820     c                   end
021000040831      **
021100040831      ** altre selezioni
021200040831      **
021300040831      **  Esclusioni/Inclusioni di raggruppamenti specifici
021400040831     c                   if        exlgrp <> *blank
021500040831     c                   if        e82gie = 'E'
021600040831     C                   EVAL      WrkSqlCmd
021700040831     C                             = %TRIMR(WrkSqlCmd) + ' and ecfgrp NOT IN ('
021800040831     c                   else
021900040831     C                   EVAL      WrkSqlCmd
022000040831     C                             = %TRIMR(WrkSqlCmd) + ' and ecfgrp IN ('
022100040831     c                   end
022200040831      **
022300040831     c                   if        e82gr1 <> *blank
022400040831     C                   EVAL      WrkSqlCmd
022500040831     C                             = %TRIMR(WrkSqlCmd) + '''' + e82gr1 + ''''
022600040831     c                   end
022700040831     c                   if        e82gr2 <> *blank
022800040831     C                   EVAL      WrkSqlCmd
022900040831     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e82gr2 + ''''
023000040831     c                   end
023100040831     c                   if        e82gr3 <> *blank
023200040831     C                   EVAL      WrkSqlCmd
023300040831     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e82gr3 + ''''
023400040831     c                   end
023500040831     c                   if        e82gr4 <> *blank
023600040831     C                   EVAL      WrkSqlCmd
023700040831     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e82gr4 + ''''
023800040831     c                   end
023900040831     c                   if        e82gr5 <> *blank
024000040831     C                   EVAL      WrkSqlCmd
024100040831     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e82gr5 + ''''
024200040831     c                   end
024300040831     c                   if        e82gr6 <> *blank
024400040831     C                   EVAL      WrkSqlCmd
024500040831     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e82gr6 + ''''
024600040831     c                   end
024700040831     c                   if        e82gr7 <> *blank
024800040831     C                   EVAL      WrkSqlCmd
024900040831     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e82gr7 + ''''
025000040831     c                   end
025100040831     c                   if        e82gr8 <> *blank
025200040831     C                   EVAL      WrkSqlCmd
025300040831     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e82gr8 + ''''
025400040831     c                   end
025500040831     c                   if        e82gr9 <> *blank
025600040831     C                   EVAL      WrkSqlCmd
025700040831     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e82gr9 + ''''
025800040831     c                   end
025900040831     c                   if        e82gr0 <> *blank
026000040831     C                   EVAL      WrkSqlCmd
026100040831     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e82gr0 + ''''
026200040831     c                   end
026300040831      **
026400040831     C                   EVAL      WrkSqlCmd
026500040831     C                             = %TRIMR(WrkSqlCmd) + ' )'
026600040831     c                   end
026700040831      **
026800040820      **  Ordinamento di lettura
026900040820     C                   EVAL      WrkSqlCmd
027000040820     C                             =
027100040820     C                             %TRIMR(WrkSqlCmd)
027200040820     c                             +
027300040820     c                             ' ORDER BY ecfdfp, ecfnfp, ecfrif'
027400040820      **
027500040820     C                   ENDSR
027600020326     C**************************************************************************
027700040820      *
027800040820     C**************************************************************************
027900040820     C     testata       begSR
028000040820      **
028100040820      *                 * ------------------ *
028200040820     c                   write     testa
028300040820      *                 * ------------------ *
028400040820      *
028500040820     C                   ENDSR
028600040825     C**************************************************************************
028700040825      *
028800040825     C**************************************************************************
028900040825     C     TOTals        begSR
029000040826      *
029100040825     c* ROTTURA Fattura
029200040825     c                   if        ecfnfp <> savnfp or
029300040825     C                             SqlCod = 100
029400040831     c                   if        almeno_uno ='S'
029500040825     c                   exsr      TESfat
029600040831     c                   end
029700040825     c                   eval      savnfp =  ecfnfp
029800041007     c                   eval      savdfp =  ecfdfp
029900040826     c                   end
030000040825      *
030100040825     C                   ENDSR
030200040820     C**************************************************************************
030300040820     C     tesfat        begSR
030400040823      *
030500040823     C* totale
030600040825     c                   if        prima_volta ='N'
030700040831      *
030800040831     c                   if        *in03
030900040831      *    tot   schiere x gruppo
031000040831     c                   do        50            cnt               3 0
031100040831     c                   if        grp(cnt) = *blank
031200040831     c                             and cnt > 1
031300040831     c                   leave
031400040831     c                   end
031500040831     c                   movel     grp(cnt)      Gruppo
031600040831     c                   z-add     ImP(cnt)      totGim
031700040831     c                   if        *in66
031800040831      *                 * ------------------ *
031900040831     c                   write     testa
032000040831     c                   write     tesdetta
032100040831      *                 * ------------------ *
032200040831     c                   setoff                                       66
032300040831     c                   end
032400040831     c                   write     Totgrp
032500040831     c                   EndDO
032600040831      *
032700040831     C                   Clear                   grp
032800040831     C                   Clear                   imp
032900040831      *
033000040823     c                   if        *in66
033100040823      *                 * ------------------ *
033200040823     c                   write     testa
033300040823     c                   write     tesdetta
033400040823      *                 * ------------------ *
033500040823     c                   setoff                                       66
033600040823     c                   end
033700040823     c                   write     totfattu
033800040823     c                   end
033900040825      *
034000040823     c                   clear                   totfat
034100040825     c                   end
034200040820      *
034300040820     C                   ENDSR
034400040820     C**************************************************************************
