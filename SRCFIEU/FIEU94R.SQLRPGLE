000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200020409     H**********************************************************************
000300130906     fFIEU94P   o    E             PRINTER OFLIND(*in66)
000400040820     H**********************************************************************
000500040820     d KPJBA         E DS
000600040820     D*
000700130906     D tnefr00f      e DS
000800041103     D UT�DSE        E DS                  EXTNAME(UT�DSE0F)
000900041103     D CNCR80        E DS
001000130906     D fieu94ds      e DS                  inz
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
002200130906     D  savdft         s                   like(efrdft)
002300130910     D  savnft         s                   like(efrnft)
002400130906     D  totfat         s                   like(efrimp) INZ(0)
002500130906     D  totgen         s                   like(efrimp) INZ(0)
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
004700130906     C+ FETCH NEXT FROM A1 INTO :tnefr00f
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
007900130906     c                   if        E94TSN ='N'
008000040820     c                   write     dettagli
008100040823     c                   end
008200040823     c*
008300040823     c* Totalizzo
008400040826     c                   move      'S'           almeno_uno
008500130906     c                   add       efrimp        totfat
008600130906     c                   add       efrimp        totgen
008700040823      *
008800130906      * x il gruppo di appartenenza
008900040823     c                   z-add     1             Ip                3 0
009000130906     c     efrGRP        lookup    Grp(Ip)                                21
009100040823     c  n21              z-add     1             Ip
009200040823     c  n21*blank        lookup    Grp(Ip)                                21
009300130906     c                   movel     efrGRP        grP(Ip)
009400130906     c                   add       efrIMP        ImP(Ip)
009500040820     c*
009600011210     C                   ENDSL
009700040820
009800011205     C                   ENDDO
009900011205     C/EXEC SQL
010000011205     C+ CLOSE A1
010100011205     C/END-EXEC
010200011205     c*
010300040820     C* totali generali
010400041007     c                   if        almeno_uno ='S'
010500040823     c                   if        *in66
010600040823      *                 * ------------------ *
010700040823     c                   write     testa
010800040823     c                   write     tesdetta
010900040823      *                 * ------------------ *
011000040823     c                   setoff                                       66
011100040823     c                   end
011200040823     c                   write     totale
011300041007     c                   end
011400040823      *
011500041007     c     fine          tag
011600020326     C                   SETON                                        LR
011700040820     C**************************************************************************
011800040820     C* INIZIALIZZAZIONE
011900040820     C**************************************************************************
012000040820     C     *inzsr        BEGSR
012100040820      **
012200040820     C     *ENTRY        PLIST
012300040820     C                   PARM                    KPJBA
012400130906     c                   movel     kpjbu         fieu94ds
012500041103      *
012600041103     C                   Z-ADD     1             CODUT
012700041103     C                   CALL      'X�PARUT'
012800041103     C                   PARM                    UT�DSE
012900041103     C                   MOVEL     REC80         CNCR80
013000041103     C                   MOVEL     RAGUT         RSUT             20
013100041103     C*
013200040820      *  imposta UDATE di elaborazione
013300040820     C                   move      *date         G02DAT
013400040820     C                   MOVE      *ZEROS        G02INV
013500040820     C                   MOVE      *BLANKS       G02ERR
013600040820     C                   CALL      'XSRDA8'
013700040820     C                   PARM                    WLBDAT
013800040820     C                   Z-ADD     G02INV        data_oggi
013900040820     C                   move      data_oggi     day
014000040820     C                   TIME                    W0140            14 0
014100040820     C                   MOVEl     W0140         ore
014200040823      **
014300130906     c                   eval      partner = e94ntw
014400130906     c                   move      e94dti        dadata
014500130906     c                   move      e94dtf        adata
014600130910      **  E94nfp
014700130910     c                   if        e94nfp <> *blank
014800130910     c                   eval      nrfat   = e94nfp
014900040820     c                   seton                                        01
015000040820     c                   end
015100130910      **  E94DEL
015200130910     c                   if        e94del <> *zero
015300130910     c                   seton                                        02
015400040820     c                   end
015500130906      **  E94TSN
015600130906     c                   if        e94tsn = 'S'
015700040823     c                   seton                                        03
015800040823     c                   end
015900040823      **
016000040823     c                   seton                                        66
016100040825     c                   clear                   prima_volta       1
016200040826     c                   clear                   almeno_uno
016300040820     c*
016400040820     c* compongo istruzione sql
016500040820     c                   exsr      istruz
016600040820     c*
016700040820     C                   ENDSR
016800040820     C**************************************************************************
016900040820     C* mi compongo l'istruzione sql
017000040820     C**************************************************************************
017100040820     C     istruz        BEGSR                                                  *
017200040820      **
017300130910     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file TNEFR
017400040820     C                   EVAL      WrkSqlCmd
017500040820     C                             =
017600130910     C                             'SELECT * from TNEFR00F where'
017700130910     c                              + ' EFRntw =' + '''' + %TRIM(e94ntw) + ''''
017800130910     c                              + ' and  EFRdft between '
017900130906     c                              + %trim(%char(e94dti)) +  ' and '
018000130906     c                              + %trim(%char(e94dtf))
018100040820      **  + nr.fattura partner
018200130910     c                   if        e94nfp <> *blank
018300040820     C                   EVAL      WrkSqlCmd
018400040820     C                             =
018500040820     C                             %TRIMR(WrkSqlCmd)
018600130910     c                             + ' and EFRnft =' + '''' + %trim(e94nfp)
018700040823     c                             +  ''''
018800040820     c                   end
018900130910      **  + data inserimento
019000130910     c                   if        e94del > 0
019100040820     C                   EVAL      WrkSqlCmd
019200040820     C                             =
019300040820     C                             %TRIMR(WrkSqlCmd)
019400130910     c                             + ' and EFRdtr = '
019500130910     c                             + %trim(%char(e94del))
019600040820     c                   end
019700040831      **
019800040831      ** altre selezioni
019900040831      **
020000040831      **  Esclusioni/Inclusioni di raggruppamenti specifici
020100040831     c                   if        exlgrp <> *blank
020200130906     c                   if        e94gie = 'E'
020300040831     C                   EVAL      WrkSqlCmd
020400130910     C                             = %TRIMR(WrkSqlCmd) + ' and efrgrp NOT IN ('
020500040831     c                   else
020600040831     C                   EVAL      WrkSqlCmd
020700130910     C                             = %TRIMR(WrkSqlCmd) + ' and efrgrp IN ('
020800040831     c                   end
020900040831      **
021000130906     c                   if        e94gr1 <> *blank
021100040831     C                   EVAL      WrkSqlCmd
021200130906     C                             = %TRIMR(WrkSqlCmd) + '''' + e94gr1 + ''''
021300040831     c                   end
021400130906     c                   if        e94gr2 <> *blank
021500040831     C                   EVAL      WrkSqlCmd
021600130906     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e94gr2 + ''''
021700040831     c                   end
021800130906     c                   if        e94gr3 <> *blank
021900040831     C                   EVAL      WrkSqlCmd
022000130906     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e94gr3 + ''''
022100040831     c                   end
022200130906     c                   if        e94gr4 <> *blank
022300040831     C                   EVAL      WrkSqlCmd
022400130906     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e94gr4 + ''''
022500040831     c                   end
022600130906     c                   if        e94gr5 <> *blank
022700040831     C                   EVAL      WrkSqlCmd
022800130906     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e94gr5 + ''''
022900040831     c                   end
023000130906     c                   if        e94gr6 <> *blank
023100040831     C                   EVAL      WrkSqlCmd
023200130906     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e94gr6 + ''''
023300040831     c                   end
023400130906     c                   if        e94gr7 <> *blank
023500040831     C                   EVAL      WrkSqlCmd
023600130906     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e94gr7 + ''''
023700040831     c                   end
023800130906     c                   if        e94gr8 <> *blank
023900040831     C                   EVAL      WrkSqlCmd
024000130906     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e94gr8 + ''''
024100040831     c                   end
024200130906     c                   if        e94gr9 <> *blank
024300040831     C                   EVAL      WrkSqlCmd
024400130906     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e94gr9 + ''''
024500040831     c                   end
024600130906     c                   if        e94gr0 <> *blank
024700040831     C                   EVAL      WrkSqlCmd
024800130906     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e94gr0 + ''''
024900040831     c                   end
025000040831      **
025100040831     C                   EVAL      WrkSqlCmd
025200040831     C                             = %TRIMR(WrkSqlCmd) + ' )'
025300040831     c                   end
025400040831      **
025500040820      **  Ordinamento di lettura
025600040820     C                   EVAL      WrkSqlCmd
025700040820     C                             =
025800040820     C                             %TRIMR(WrkSqlCmd)
025900040820     c                             +
026000130910     c                             ' ORDER BY efrdft, efrnft, efrrif'
026100040820      **
026200040820     C                   ENDSR
026300020326     C**************************************************************************
026400040820      *
026500040820     C**************************************************************************
026600040820     C     testata       begSR
026700040820      **
026800040820      *                 * ------------------ *
026900040820     c                   write     testa
027000040820      *                 * ------------------ *
027100040820      *
027200040820     C                   ENDSR
027300040825     C**************************************************************************
027400040825      *
027500040825     C**************************************************************************
027600040825     C     TOTals        begSR
027700040826      *
027800040825     c* ROTTURA Fattura
027900130910     c                   if        efrnft <> savnft or
028000040825     C                             SqlCod = 100
028100040831     c                   if        almeno_uno ='S'
028200130910     c                   eval      fatnr =savnft
028300040825     c                   exsr      TESfat
028400040831     c                   end
028500130910     c                   eval      savnft =  efrnft
028600130910     c                   eval      savdft =  efrdft
028700040826     c                   end
028800040825      *
028900040825     C                   ENDSR
029000040820     C**************************************************************************
029100040820     C     tesfat        begSR
029200040823      *
029300040823     C* totale
029400040825     c                   if        prima_volta ='N'
029500040831      *
029600040831     c                   if        *in03
029700040831      *    tot   schiere x gruppo
029800040831     c                   do        50            cnt               3 0
029900040831     c                   if        grp(cnt) = *blank
030000040831     c                             and cnt > 1
030100040831     c                   leave
030200040831     c                   end
030300040831     c                   movel     grp(cnt)      Gruppo
030400040831     c                   z-add     ImP(cnt)      totGim
030500040831     c                   if        *in66
030600040831      *                 * ------------------ *
030700040831     c                   write     testa
030800040831     c                   write     tesdetta
030900040831      *                 * ------------------ *
031000040831     c                   setoff                                       66
031100040831     c                   end
031200040831     c                   write     Totgrp
031300040831     c                   EndDO
031400040831      *
031500040831     C                   Clear                   grp
031600040831     C                   Clear                   imp
031700040831      *
031800040823     c                   if        *in66
031900040823      *                 * ------------------ *
032000040823     c                   write     testa
032100040823     c                   write     tesdetta
032200040823      *                 * ------------------ *
032300040823     c                   setoff                                       66
032400040823     c                   end
032500040823     c                   write     totfattu
032600040823     c                   end
032700040825      *
032800040823     c                   clear                   totfat
032900040825     c                   end
033000040820      *
033100040820     C                   ENDSR
033200040820     C**************************************************************************
