000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200020409     H**********************************************************************
000300160620     fFIEU85P2  o    E             PRINTER OFLIND(*in66)
000400040820     H**********************************************************************
000500160621     d KPJBA         E DS
000600160621     D*
000700160621     D fieu84ds      e DS                  inz
000800160621     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
000900160621     D CNCR80        E DS
001000160621     D*
001100160621     D F85NTREC      E DS                  EXTNAME(wfieu85nt)
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
002200160621     D  savnaz         s                   like(N85NAZ)
002300160621     D  savtipo        s                   like(N85TIPO)
002400160621      *
002500160621      *Tipo
002600160621     D  t1NUMSPE       s                   like(NUMSPE)
002700160621     D  t1IMPDPD       s                   like(IMPDPD)
002800160621     D  t1IMPBRT       s                   like(IMPBRT)
002900160621     D  t1IMPDIF       s                   like(IMPDIF)
003000160621      *
003100160621      *Nazione
003200160621     D  t2NUMSPE       s                   like(NUMSPE)
003300160621     D  t2IMPDPD       s                   like(IMPDPD)
003400160621     D  t2IMPBRT       s                   like(IMPBRT)
003500160621     D  t2IMPDIF       s                   like(IMPDIF)
003600160621      *
003700160621      *Generale
003800160621     D  t3NUMSPE       s                   like(NUMSPE)
003900160621     D  t3IMPDPD       s                   like(IMPDPD)
004000160621     D  t3IMPBRT       s                   like(IMPBRT)
004100160621     D  t3IMPDIF       s                   like(IMPDIF)
004200160621      *
004300040826     D  almeno_uno     s              1
004400040820      *
004500040820     D digits          C                   CONST('0123456789')
004600040820      *****************************************************************
004700160620
004800011210     C/EXEC SQL
004900011210     C+ PREPARE S1 FROM :WrkSqlCmd
005000011210     C/END-EXEC
005100011210
005200011210     C/EXEC SQL
005300011210     C+ DECLARE A1 CURSOR FOR S1
005400011210     C/END-EXEC
005500011210
005600011210     C/EXEC SQL
005700011210     C+ OPEN A1
005800011210     C/END-EXEC
005900040820
006000040820     C                   DOU       SqlCod <> 0
006100040823     c*
006200011205     C/EXEC SQL
006300160621     C+ FETCH NEXT FROM A1 INTO :F85NTREC
006400011205     C/END-EXEC
006500011210     C                   SELECT
006600041007     c*
006700041007     c* a fine file Totali x rotture
006800041007     C                   WHEN      SqlCod = 100
006900041007     c                   if        almeno_uno ='S'
007000160621     c                   exsr      tot_TIPNAZ
007100041007     c                   else
007200160621      *
007300160621     c                   write     tesPAG
007400041007     c                   write     testa
007500041007     c                   end
007600041007     c                   leave
007700041007     **
007800041007     C                   WHEN      SqlCod < 0
007900041007     C                   seton                                        H1
008000041007     c                   goto      fine
008100040820     c*
008200041007     C                   OTHER
008300040820     c* DETTAGLIO
008400040823     c                   if        *in66
008500040823      *                 * ------------------ *
008600160621     c                   write     tesPAG
008700040823     c                   write     testa
008800040823      *                 * ------------------ *
008900040823     c                   setoff                                       66
009000040825     c                   eval      prima_volta = 'N'
009100040823     c                   end
009200040823      *
009300160621     c                   if        savnaz  <> N85NAZ or
009400160621     c                             savtipo <> N85TIPO
009500160621     c                   eval      savnaz  =  N85NAZ
009600160621     c                   eval      savtipo =  N85TIPO
009700160621     c                   exsr      tot_TIPNAZ
009800160621     c                   end
009900160621      *
010000160621     c                   exsr      RIGADET
010100040823     c* Totalizzo
010200040826     c                   move      'S'           almeno_uno
010300040823      *
010400011210     C                   ENDSL
010500040820
010600011205     C                   ENDDO
010700011205     C/EXEC SQL
010800011205     C+ CLOSE A1
010900011205     C/END-EXEC
011000011205     c*
011100040820     C* totali generali
011200040823      *
011300041007     c     fine          tag
011400020326     C                   SETON                                        LR
011500040820     C**************************************************************************
011600040820     C* INIZIALIZZAZIONE
011700040820     C**************************************************************************
011800040820     C     *inzsr        BEGSR
011900160621      **
012000160621     C     *ENTRY        PLIST
012100160621     C                   PARM                    KPJBA
012200160621     c                   movel     kpjbu         fieu84ds
012300160621      *
012400160621     C                   Z-ADD     1             CODUT
012500160621     C                   CALL      'X§PARUT'
012600160621     C                   PARM                    UT§DSE
012700160621     C                   MOVEL     REC80         CNCR80
012800160621     C                   MOVEL     RAGUT         RSUT             20
012900160621     C*
013000160621      **
013100160621     c                   eval      partner = e84ntw
013200160621     c                   clear                   tipo             15
013300160621      **
013400160621      **  x incasellamento
013500160621     c                   if        e84dii <> *zero
013600160621     c                   move      e84dii        dadata
013700160621     c                   move      e84dfi        adata
013800160621     c                   move(p)   'Incasellate' periodo
013900160621     c                   eval      tipo = 'Incasellamento'
014000160621     c                   end
014100160621      **  x spedizione
014200160621     c                   if        e84dis <> *zero
014300160621     c                   move      e84dis        dadata
014400160621     c                   move      e84dfs        adata
014500160621     c                   move(p)   'Spedizioni'  periodo
014600160621     c                   eval      tipo = 'Spedizione   '
014700160621     c                   end
014800160621      **  x fattura
014900160621     c                   if        e84dif <> *zero
015000160621     c                   move      e84dif        dadata
015100160621     c                   move      e84dff        adata
015200160621     c                   move(p)   'Fatturate '  periodo
015300160621     c                   eval      tipo = 'Fattura      '
015400160621     c                   end
015500040820      **
015600040820      *  imposta UDATE di elaborazione
015700040820     C                   move      *date         G02DAT
015800040820     C                   MOVE      *ZEROS        G02INV
015900040820     C                   MOVE      *BLANKS       G02ERR
016000040820     C                   CALL      'XSRDA8'
016100040820     C                   PARM                    WLBDAT
016200040820     C                   Z-ADD     G02INV        data_oggi
016300040820     C                   move      data_oggi     day
016400040820     C                   TIME                    W0140            14 0
016500040820     C                   MOVEl     W0140         ore
016600040823      **
016700040823     c                   seton                                        66
016800040825     c                   clear                   prima_volta       1
016900040826     c                   clear                   almeno_uno
017000040820     c*
017100040820     c* compongo istruzione sql
017200040820     c                   exsr      istruz
017300040820     c*
017400040820     C                   ENDSR
017500040820     C**************************************************************************
017600160621     C*  stampa la riga di dettaglio
017700040820     C**************************************************************************
017800160621     C     RIGADET       BEGSR                                                  *
017900160621      *
018000160621     c                   eval      TIPDAT = N85TIPO
018100160621     c                   eval      CODNAZ = N85NAZ
018200160621     c                   eval      CODTAS = N85CTS
018300160621     c                   eval      NUMSPE =   N85SPD + N85SPDR
018400160621     c                   eval      IMPDPD =   N85PTN + N85PTNR
018500160621     c                   eval      IMPBRT =   N85BRT + N85BRTR
018600160621     c                   eval      IMPDIF =   N85DIF + N85DIFR
018700160621      **
018800160621     c                   write     Dettagli
018900160621      *
019000160621     c                   eval      t1NUMSPE =  NUMSPE + t1NUMSPE
019100160621     c                   eval      t1IMPDPD =  IMPDPD + t1IMPDPD
019200160621     c                   eval      t1IMPBRT =  IMPBRT + t1IMPBRT
019300160621     c                   eval      t1IMPDIF =  IMPDIF + t1IMPDIF
019400160621      *
019500160621     C                   ENDSR
019600160621     C**************************************************************************
019700160621     C* Totali x Tipo
019800160621     C**************************************************************************
019900160621     C     Tot_TIPNAZ    BEGSR                                                  *
020000160621      **
020100160621     c                   eval      totNSP =  t1NUMSPE
020200160621     c                   eval      totDPD =  t1IMPDPD
020300160621     c                   eval      totBRT =  t1IMPBRT
020400160621     c                   eval      totDIF =  t1IMPDIF
020500160621      **
020600160621     c                   write     Totali
020700160621     c                   write     RIga
020800160621      **
020900160621     c                   clear                   t1NUMSPE
021000160621     c                   clear                   t1IMPDPD
021100160621     c                   clear                   t1IMPBRT
021200160621     c                   clear                   t1IMPDIF
021300160621      **
021400160621     C                   ENDSR
021500160621     C**************************************************************************
021600160621     C* mi compongo l'istruzione sql
021700160621     C**************************************************************************
021800160621     C     istruz        BEGSR                                                  *
021900160621      **
022000040820     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file TNECF
022100040820     C                   EVAL      WrkSqlCmd
022200040820     C                             =
022300160621     C                             'SELECT * FROM wfieu85nt '
022400160621     c                              + 'ORDER BY N85NAZ, N85TIPO desc, n85cts'
022500040820      **
022600040820     C                   ENDSR
022700040825     C**************************************************************************
022800040825      *
022900040825     C**************************************************************************
023000040825     C     TOTals        begSR
023100040826      *
023200040825      *
023300040825     C                   ENDSR
023400040820     C**************************************************************************
