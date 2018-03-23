000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200020409     H**********************************************************************
000300040823     fFIEU83P   o    E             PRINTER OFLIND(*in66)
000400040820     H**********************************************************************
000500040820     d KPJBA         E DS
000600040820     D*
000700040820     D tnecf00f      e DS
000800040820     D fieu82ds      e DS                  inz
000900040831     d  exlgrp                54     83
001000040820     D*
001100040820     D WLBDAT          DS
001200040820     D  G02DAT                 1      8  0
001300040820     D  G02INV                 9     16  0
001400040820     D  G02ERR                17     17
001500040820     D  G02TGI                18     22  0
001600040820      *
001700040820     D WrkSqlCmd       S           1024
001800040820     D  data_Oggi      s                   like(G02INV) INZ(0)
001900040820      *
002000040820     D  savdfp         s                   like(ecfdfp)
002100040820     D  savnfp         s                   like(ecfnfp)
002200040823     D  totfat         s                   like(ecfimp) INZ(0)
002300040823     D  totgen         s                   like(ecfimp) INZ(0)
002400040826     D  almeno_uno     s              1
002500040823     D GRp             S              3    DIM(50) INZ(' ')
002600040823     D IMp             S             16  3 DIM(50) INZ(0)
002700040820      *
002800040820     D digits          C                   CONST('0123456789')
002900040820      *****************************************************************
003000011210     C/EXEC SQL
003100011210     C+ PREPARE S1 FROM :WrkSqlCmd
003200011210     C/END-EXEC
003300011210
003400011210     C/EXEC SQL
003500011210     C+ DECLARE A1 CURSOR FOR S1
003600011210     C/END-EXEC
003700011210
003800011210     C/EXEC SQL
003900011210     C+ OPEN A1
004000011210     C/END-EXEC
004100040820
004200040820     C                   DOU       SqlCod <> 0
004300040823     c*
004400011205     C/EXEC SQL
004500040820     C+ FETCH NEXT FROM A1 INTO :tnecf00f
004600011205     C/END-EXEC
004700011210     C                   SELECT
004800041007     c*
004900041007     c* a fine file Totali x rotture
005000041007     C                   WHEN      SqlCod = 100
005100041007     c                   if        almeno_uno ='S'
005200041007     c                   exsr      TOTals
005300041007     c                   else
005400041007     c                   write     testa
005500041007     c                   write     tesdetta
005600041007     c                   write     NODATI
005700041007     c                   end
005800041007     c                   leave
005900041007     **
006000041007     C                   WHEN      SqlCod < 0
006100041007     C                   seton                                        H1
006200041007     c                   goto      fine
006300040820     c*
006400041007     C                   OTHER
006500040825     c* Totali x rotture
006600040825     c                   exsr      TOTals
006700040820     c* DETTAGLIO
006800040823     c                   if        *in66
006900040823      *                 * ------------------ *
007000040823     c                   write     testa
007100040823     c                   write     tesdetta
007200040823      *                 * ------------------ *
007300040823     c                   setoff                                       66
007400040825     c                   eval      prima_volta = 'N'
007500040823     c                   end
007600040823      *
007700040823     c                   if        *in04 and ecfdta = 0 or
007800040823     c                             *in05 and ecfdta > 0
007900040820     c                   write     dettagli
008000040823     c                   end
008100040823     c*
008200040823     c* Totalizzo
008300040826     c                   move      'S'           almeno_uno
008400040823     c                   add       ecfimp        totfat
008500040823     c                   add       ecfimp        totgen
008600040823      *
008700040823      * Incasella l'importo nel gruppo di appartenenza
008800040823     c                   z-add     1             Ip                3 0
008900040823     c     ecfGRP        lookup    Grp(Ip)                                21
009000040823     c  n21              z-add     1             Ip
009100040823     c  n21*blank        lookup    Grp(Ip)                                21
009200040823     c                   movel     ecfGRP        grP(Ip)
009300040823     c                   add       ecfIMP        ImP(Ip)
009400040820     c*
009500011210     C                   ENDSL
009600040820
009700011205     C                   ENDDO
009800011205     C/EXEC SQL
009900011205     C+ CLOSE A1
010000011205     C/END-EXEC
010100011205     c*
010200040820     C* totali generali
010300041007     c                   if        almeno_uno ='S'
010400040823     c                   if        *in66
010500040823      *                 * ------------------ *
010600040823     c                   write     testa
010700040823     c                   write     tesdetta
010800040823      *                 * ------------------ *
010900040823     c                   setoff                                       66
011000040823     c                   end
011100040823     c                   write     totale
011200041007     c                   end
011300040823      *
011400041007     c     fine          tag
011500020326     C                   SETON                                        LR
011600040820     C**************************************************************************
011700040820     C* INIZIALIZZAZIONE
011800040820     C**************************************************************************
011900040820     C     *inzsr        BEGSR
012000040820      **
012100040820     C     *ENTRY        PLIST
012200040820     C                   PARM                    KPJBA
012300040820     c                   movel     kpjbu         fieu82ds
012400040820      *
012500040820      *  imposta UDATE di elaborazione
012600040820     C                   move      *date         G02DAT
012700040820     C                   MOVE      *ZEROS        G02INV
012800040820     C                   MOVE      *BLANKS       G02ERR
012900040820     C                   CALL      'XSRDA8'
013000040820     C                   PARM                    WLBDAT
013100040820     C                   Z-ADD     G02INV        data_oggi
013200040820     C                   move      data_oggi     day
013300040820     C                   TIME                    W0140            14 0
013400040820     C                   MOVEl     W0140         ore
013500040823      **
013600040820     c                   eval      partner = e82ntw
013700040820     c                   move      e82dti        dadata
013800040820     c                   move      e82dtf        adata
013900040823      **  E82NFP
014000040820     c                   if        e82nfp <> *blank
014100040820     c                   eval      nrfat   = e82nfp
014200040820     c                   seton                                        01
014300040820     c                   end
014400040823      **  E82DIN  E82DFI
014500040820     c                   if        e82din <> *zero
014600040820     c                   move      e82din        dainca
014700040820     c                   move      e82dfi        ainca
014800040820     c                   seton                                        02
014900040820     c                   end
015000040823      **  E82TSN
015100040823     c                   if        e82tsn = 'S'
015200040823     c                   seton                                        03
015300040823     c                   end
015400040823      **  E82NIN
015500040823     c                   if        e82nin = 'S'
015600040823     c                   seton                                        04
015700040823     c                   end
015800040823      **  E82SIN
015900040823     c                   if        e82sin = 'S'
016000040823     c                   seton                                        05
016100040823     c                   end
016200040823      **
016300040823     c                   seton                                        66
016400040825     c                   clear                   prima_volta       1
016500040826     c                   clear                   almeno_uno
016600040820     c*
016700040820     c* compongo istruzione sql
016800040820     c                   exsr      istruz
016900040820     c*
017000040820     C                   ENDSR
017100040820     C**************************************************************************
017200040820     C* mi compongo l'istruzione sql
017300040820     C**************************************************************************
017400040820     C     istruz        BEGSR                                                  *
017500040820      **
017600040820     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file TNECF
017700040820     C                   EVAL      WrkSqlCmd
017800040820     C                             =
017900040823     C                             'SELECT * from TNECF00F where'
018000040823     c                              + ' ecfntw =' + '''' + %TRIM(e82ntw) + ''''
018100040823     c                              + ' and  ecfdfp between '
018200040823     c                              + %trim(%char(e82dti)) +  ' and '
018300040823     c                              + %trim(%char(e82dtf))
018400040820      **  + nr.fattura partner
018500040820     c                   if        e82nfp <> *blank
018600040820     C                   EVAL      WrkSqlCmd
018700040820     C                             =
018800040820     C                             %TRIMR(WrkSqlCmd)
018900040823     c                             + ' and ecfnfp =' + '''' + %trim(e82nfp)
019000040823     c                             +  ''''
019100040820     c                   end
019200040820      **  + data incasellamento
019300040820     c                   if        e82din > 0
019400040820     C                   EVAL      WrkSqlCmd
019500040820     C                             =
019600040820     C                             %TRIMR(WrkSqlCmd)
019700040823     c                             + ' and ecfdta between '
019800040823     c                             + %trim(%char(e82din)) +  ' and '
019900040823     c                             + %trim(%char(e82dfi))
020000040820     c                   end
020100040831      **
020200040831      ** altre selezioni
020300040831      **
020400040831      **  Esclusioni/Inclusioni di raggruppamenti specifici
020500040831     c                   if        exlgrp <> *blank
020600040831     c                   if        e82gie = 'E'
020700040831     C                   EVAL      WrkSqlCmd
020800040831     C                             = %TRIMR(WrkSqlCmd) + ' and ecfgrp NOT IN ('
020900040831     c                   else
021000040831     C                   EVAL      WrkSqlCmd
021100040831     C                             = %TRIMR(WrkSqlCmd) + ' and ecfgrp IN ('
021200040831     c                   end
021300040831      **
021400040831     c                   if        e82gr1 <> *blank
021500040831     C                   EVAL      WrkSqlCmd
021600040831     C                             = %TRIMR(WrkSqlCmd) + '''' + e82gr1 + ''''
021700040831     c                   end
021800040831     c                   if        e82gr2 <> *blank
021900040831     C                   EVAL      WrkSqlCmd
022000040831     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e82gr2 + ''''
022100040831     c                   end
022200040831     c                   if        e82gr3 <> *blank
022300040831     C                   EVAL      WrkSqlCmd
022400040831     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e82gr3 + ''''
022500040831     c                   end
022600040831     c                   if        e82gr4 <> *blank
022700040831     C                   EVAL      WrkSqlCmd
022800040831     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e82gr4 + ''''
022900040831     c                   end
023000040831     c                   if        e82gr5 <> *blank
023100040831     C                   EVAL      WrkSqlCmd
023200040831     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e82gr5 + ''''
023300040831     c                   end
023400040831     c                   if        e82gr6 <> *blank
023500040831     C                   EVAL      WrkSqlCmd
023600040831     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e82gr6 + ''''
023700040831     c                   end
023800040831     c                   if        e82gr7 <> *blank
023900040831     C                   EVAL      WrkSqlCmd
024000040831     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e82gr7 + ''''
024100040831     c                   end
024200040831     c                   if        e82gr8 <> *blank
024300040831     C                   EVAL      WrkSqlCmd
024400040831     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e82gr8 + ''''
024500040831     c                   end
024600040831     c                   if        e82gr9 <> *blank
024700040831     C                   EVAL      WrkSqlCmd
024800040831     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e82gr9 + ''''
024900040831     c                   end
025000040831     c                   if        e82gr0 <> *blank
025100040831     C                   EVAL      WrkSqlCmd
025200040831     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e82gr0 + ''''
025300040831     c                   end
025400040831      **
025500040831     C                   EVAL      WrkSqlCmd
025600040831     C                             = %TRIMR(WrkSqlCmd) + ' )'
025700040831     c                   end
025800040831      **
025900040820      **  Ordinamento di lettura
026000040820     C                   EVAL      WrkSqlCmd
026100040820     C                             =
026200040820     C                             %TRIMR(WrkSqlCmd)
026300040820     c                             +
026400040820     c                             ' ORDER BY ecfdfp, ecfnfp, ecfrif'
026500040820      **
026600040820     C                   ENDSR
026700020326     C**************************************************************************
026800040820      *
026900040820     C**************************************************************************
027000040820     C     testata       begSR
027100040820      **
027200040820      *                 * ------------------ *
027300040820     c                   write     testa
027400040820      *                 * ------------------ *
027500040820      *
027600040820     C                   ENDSR
027700040825     C**************************************************************************
027800040825      *
027900040825     C**************************************************************************
028000040825     C     TOTals        begSR
028100040826      *
028200040825     c* ROTTURA Fattura
028300040825     c                   if        ecfnfp <> savnfp or
028400040825     C                             SqlCod = 100
028500040831     c                   if        almeno_uno ='S'
028600040825     c                   exsr      TESfat
028700040831     c                   end
028800040825     c                   eval      savnfp =  ecfnfp
028900041007     c                   eval      savdfp =  ecfdfp
029000040826     c                   end
029100040825      *
029200040825     C                   ENDSR
029300040820     C**************************************************************************
029400040820     C     tesfat        begSR
029500040823      *
029600040823     C* totale
029700040825     c                   if        prima_volta ='N'
029800040831      *
029900040831     c                   if        *in03
030000040831      *    tot   schiere x gruppo
030100040831     c                   do        50            cnt               3 0
030200040831     c                   if        grp(cnt) = *blank
030300040831     c                             and cnt > 1
030400040831     c                   leave
030500040831     c                   end
030600040831     c                   movel     grp(cnt)      Gruppo
030700040831     c                   z-add     ImP(cnt)      totGim
030800040831     c                   if        *in66
030900040831      *                 * ------------------ *
031000040831     c                   write     testa
031100040831     c                   write     tesdetta
031200040831      *                 * ------------------ *
031300040831     c                   setoff                                       66
031400040831     c                   end
031500040831     c                   write     Totgrp
031600040831     c                   EndDO
031700040831      *
031800040831     C                   Clear                   grp
031900040831     C                   Clear                   imp
032000040831      *
032100040823     c                   if        *in66
032200040823      *                 * ------------------ *
032300040823     c                   write     testa
032400040823     c                   write     tesdetta
032500040823      *                 * ------------------ *
032600040823     c                   setoff                                       66
032700040823     c                   end
032800040823     c                   write     totfattu
032900040823     c                   end
033000040825      *
033100040823     c                   clear                   totfat
033200040825     c                   end
033300040820      *
033400040820     C                   ENDSR
033500040820     C**************************************************************************
