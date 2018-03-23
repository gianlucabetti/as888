000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200020409     H**********************************************************************
000300161108     Ffiapd01l  IF   E           K DISK
000400161108     Ftntbe01l  IF   E           K DISK
000500161108     Fbiasp00F  o  A E             DISK
000600040820     H**********************************************************************
000700161108     d dAPDflr       E DS
000800161108     d dAIG          E DS
000900161109     d dAGL          E DS
001000161109     D  gg_mese                1     24  0 DIM(12)
001100170421      *
001200040820     d KPJBA         E DS
001300161109     D  massa_DA                      7  1 DIM(10)
001400161109     D  massa_A                       7  1 DIM(10)
001500161109     D  massa_COD                     1    DIM(10)
001600040820     D*
001700040820     D WLBDAT          DS
001800040820     D  G02DAT                 1      8  0
001900040820     D  G02INV                 9     16  0
002000040820     D  G02ERR                17     17
002100040820     D  G02TGI                18     22  0
002200040820      *
002300040820     D WrkSqlCmd       S           1024
002400040820     D  data_Oggi      s                   like(G02INV) INZ(0)
002500161108     D  pdrsav         s              7  0
002600161108     d tmp             s             17
002700040820      *
002800161108     D  fgssql         s              3  0
002900161108     D  pdrsql         s              7  0
003000161108     D  numsql         s              5  0
003100161108     D  volsql         s              7  3
003200161109     D  dfvsql         s              8s 0
003300161108      *
003400040826     D  almeno_uno     s              1
003500040820      *
003600040820     D digits          C                   CONST('0123456789')
003700161108      *****************************************************************
003800161108     c* compongo istruzione sql
003900161108     c                   exsr      istruz
004000161109      *
004100161108      *
004200161108      *****************************************************************
004300161108      *  poi li rigenera
004400011210     C/EXEC SQL
004500011210     C+ PREPARE S1 FROM :WrkSqlCmd
004600011210     C/END-EXEC
004700011210
004800011210     C/EXEC SQL
004900011210     C+ DECLARE A1 CURSOR FOR S1
005000011210     C/END-EXEC
005100011210
005200011210     C/EXEC SQL
005300011210     C+ OPEN A1
005400011210     C/END-EXEC
005500040820     C                   DOU       SqlCod <> 0
005600040823     c*
005700011205     C/EXEC SQL
005800161109     C+ FETCH NEXT FROM A1 INTO :fgssql, :pdrsql, :dfvsql, :numsql, :volsql
005900011205     C/END-EXEC
006000011210     C                   SELECT
006100041007     c*
006200041007     c* a fine file Totali x rotture
006300041007     C                   WHEN      SqlCod = 100
006400041007     c                   if        almeno_uno ='S'
006500161108     c                   exsr      TOTali
006600041007     c                   end
006700041007     c                   leave
006800041007     **
006900041007     C                   WHEN      SqlCod < 0
007000041007     C                   seton                                        H1
007100041007     c                   goto      fine
007200040820     c*
007300041007     C                   OTHER
007400040825     c* Totali x rotture
007500161108     c                   exsr      TOTali
007600040820     c* DETTAGLIO
007700161110     c                   eval      prima_volta = 'N'
007800040826     c                   move      'S'           almeno_uno
007900161110      *
008000161110     c                   clear                   biasp000
008100161108     c                   exsr      dettaglio
008200161110     c                   write     biasp000
008300040823      *
008400011210     C                   ENDSL
008500040820
008600011205     C                   ENDDO
008700011205     C/EXEC SQL
008800011205     C+ CLOSE A1
008900011205     C/END-EXEC
009000011205     c*
009100040820     C* totali generali
009200041007     c                   if        almeno_uno ='S'
009300041007     c                   end
009400040823      *
009500041007     c     fine          tag
009600020326     C                   SETON                                        LR
009700040820     C**************************************************************************
009800040820     C* INIZIALIZZAZIONE
009900040820     C**************************************************************************
010000040820     C     *inzsr        BEGSR
010100040820      **
010200161108     C     apd01         kLIST
010300161108     c                   kFLD                    apdtip
010400161108     c                   kFLD                    apdpdr
010500161108      **
010600161108     C     tbe01         kLIST
010700161108     c                   kFLD                    tbeCOD
010800161108     c                   kFLD                    tbeKE1
010900161108      **
011000161109     C     tbe02         kLIST
011100161109     c                   kFLD                    tbeCOD
011200161109     c                   kFLD                    tbeKE1
011300161109     c                   kFLD                    tbeKE2
011400161109      **
011500040820     C     *ENTRY        PLIST
011600040820     C                   PARM                    KPJBA
011700161109      **
011800040820      *  imposta UDATE di elaborazione
011900040820     C                   move      *date         G02DAT
012000040820     C                   MOVE      *ZEROS        G02INV
012100040820     C                   MOVE      *BLANKS       G02ERR
012200040820     C                   CALL      'XSRDA8'
012300040820     C                   PARM                    WLBDAT
012400040820     C                   Z-ADD     G02INV        data_oggi
012500161108     C                   move      data_oggi     day               8 0
012600040820     C                   TIME                    W0140            14 0
012700161108     C                   MOVEl     W0140         ore               6 0
012800040823      **
012900040825     c                   clear                   prima_volta       1
013000040826     c                   clear                   almeno_uno
013100161109      **
013200161109     c                   movel     'MAS'         tbeCOD
013300161109     c                   clear                   massa_da
013400161109     c                   clear                   massa_a
013500161109     c                   clear                   massa_cod
013600161109     c                   z-add     0             nr                3 0
013700161109     C     tbeCOD        setll     tntbe01l
013800161109     C     tbeCOD        reade     tntbe01l
013900161109     c                   dow       not %eof(tntbe01l) and nr < 10
014000161109     c                   add       1             nr
014100161109     c                   movel     tbeke1        massa_DA(nr)
014200161109     c                   movel     tbeke2        massa_cod(nr)
014300161109     c                   if        nr > 1
014400161109     c                   eval       massa_A(nr-1) = massa_DA(nr) - 0,1
014500161109     c                   end
014600161109     C     tbeCOD        reade     tntbe01l
014700161109     c                   end
014800161109     c* finale estremo
014900161109     c                   if        nr=1
015000161109     c                   eval       massa_A(1) = 999999,9
015100161109     c                   else
015200161109     c                   eval       massa_A(nr) = 999999,9
015300161109     c                   eval       massa_A(nr-1) = massa_DA(nr) - 0,1
015400161109     c                   end
015500040820     c*
015600040820     C                   ENDSR
015700040820     C**************************************************************************
015800040820     C* mi compongo l'istruzione sql
015900040820     C**************************************************************************
016000040820     C     istruz        BEGSR                                                  *
016100040820      **
016200040820     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file TNECF
016300040820     C                   EVAL      WrkSqlCmd
016400040820     C                             =
016500161109     C                             'SELECT TDIFGS, TDIPDR, TDIDFV, '           +
016600170313     c                             'dec(sum(TDINBLA + TDINBLR), 5) nr_sped, '  +
016700170313     c                             'dec(sum(TDIVLUA + TDIVLUR), 7, 3) volume ' +
016800170421     c                             'FROM BITDI00f '                            +
016900161115     C                             'GROUP BY TDIFGS, TDIPDR, TDIDFV '
017000040820     C                   ENDSR
017100020326     C**************************************************************************
017200161108      *   esegue il dettaglio
017300040820     C**************************************************************************
017400161108     C     dettaglio     begSR
017500040820      *
017600161109     c                   move      dfvsql        datmmgg           4 0
017700161109     c                   movel     datmmgg       datmese           2 0
017800161109      *
017900161109     c                   z-add     dfvsql        aspdfv
018000161108     c                   z-add     fgssql        aspfgs
018100161108     c                   z-add     pdrsql        asppdr
018200170313     c******             z-add     numsql        aspNBLA
018300170313     c******             z-add     volsql        aspVLUA
018400161108     c                   eval      tmp= %char(%timestamp: *iso0)
018500161108     c                   eval      ASPDatora  =   tmp
018600161108      *
018700161108      *  decodifiche da anagrafica
018800161108     c                   move      'A'           apdTIP
018900161108     c                   z-add     pdrsql        apdPDR
019000161108     C     apd01         chain     fiapd01l
019100161108     c                   if        %Found(fiapd01l)
019200161108     c                   clear                   chiave_mas        1
019300161108     c                   clear                   chiave_aat        1
019400161108     c                   clear                   chiave_ats        1
019500161108     c                   movel(p)  apdflr        Dapdflr
019600161108      *massa
019700161109     c                   DO        10            nr
019800161109     c                   if        §apdMCP  >= massa_DA(nr) and
019900161109     c                             §apdMCP  <= massa_A(nr)
020000161109     c                   movel     massa_COD(nr) chiave_mas
020100161109     c                   leave
020200161109     c                   end
020300161109     c                   endDO
020400161108      *attività
020500161108     c                   movel     'AAT'         tbeCOD
020600161108     c                   movel(p)  §apdAAT       tbeKE1
020700161108     C     tbe01         chain     tntbe01l
020800161108     c                   if        %found(tntbe01l)
020900161108     c                   movel     tbeke1        chiave_aat
021000161108     c                   end
021100161108      *territorio
021200161108     c                   movel     'ATS'         tbeCOD
021300161108     c                   movel(p)  §apdATS       tbeKE1
021400161108     C     tbe01         chain     tntbe01l
021500161108     c                   if        %found(tntbe01l)
021600161108     c                   movel     tbeke1        chiave_ats
021700161108     c                   end
021800161108      *  tab.AIG aspettativa
021900161108     c                   movel     'AIG'         tbeCOD
022000161108     c                   eval      tbeKE1 = %editc(aspfgs:'X') + 'N' +
022100161108     c                             chiave_ats + chiave_mas + chiave_aat
022200161115     c                   movel(p)  dfvsql        tbeKE2
022300161115     C     tbe02         setll     tntbe01l
022400161115     C     tbe01         reade     tntbe01l
022500161115     c                   if        %eof(tntbe01l)
022600161109     c                   movel     '999'         tbeKE1
022700161115     C     tbe02         setll     tntbe01l
022800161115     C     tbe01         reade     tntbe01l
022900161109     c                   end
023000161115     c                   if        not %eof(tntbe01l)
023100161108     c                   movel     tbeuni        dAIG
023200161108     c                   z-add     D§AIGIMP      aspIMPN
023300161108     c                   end
023400161108      *-
023500161108     c                   eval      tbeKE1 = %editc(aspfgs:'X') + 'O' +
023600161108     c                             chiave_ats + chiave_mas + chiave_aat
023700161115     c                   movel(p)  dfvsql        tbeKE2
023800161115     C     tbe02         setll     tntbe01l
023900161115     C     tbe01         reade     tntbe01l
024000161115     c                   if        %eof(tntbe01l)
024100161115     c                   movel     '999'         tbeKE1
024200161115     C     tbe02         setll     tntbe01l
024300161115     C     tbe01         reade     tntbe01l
024400161115     c                   end
024500161115     c                   if        not %eof(tntbe01l)
024600161108     c                   movel     tbeuni        dAIG
024700161108     c                   z-add     D§AIGIMP      aspIMPO
024800170313     c                   z-add(h)  D§AIGQTAs     aspQTAS
024900170313     c                   z-add     D§AIGQTAv     aspQTAV
025000161108     c                   end
025100161108      *-
025200161108     c                   end
025300161108      * ASPGG
025400161109      * Giorni di aspettativa
025500161109     c                   movel     'AGL'         tbeCOD
025600161109     c                   movel(p)  fgssql        tbeKE1
025700161109     c                   movel(p)  dfvsql        tbeKE2
025800161109     C     tbe02         setll     tntbe01l
025900161109     C     tbe01         reade     tntbe01l
026000161109     c                   if        %eof(tntbe01l)
026100161109     c                   movel(p)  '999'         tbeKE1
026200161109     C     tbe02         setll     tntbe01l
026300161109     C     tbe01         reade     tntbe01l
026400161109     c                   end
026500161109     c                   if        not %eof(tntbe01l)
026600161109     c                   movel     tbeuni        dAGL
026700161109     c                   eval      aspGG = gg_mese(datmese)
026800161109     c                   end
026900161109      *
027000040820     C                   ENDSR
027100040825     C**************************************************************************
027200040825      *
027300040825     C**************************************************************************
027400161108     C     TOTali        begSR
027500040826      *
027600161108     c* ROTTURA chiave
027700161108     c                   if        pdrsql <> pdrsav or
027800040825     C                             SqlCod = 100
027900040831     c                   if        almeno_uno ='S'
028000161108     c                   exsr      rot_key
028100161108     c                   end
028200161108     c                   eval      pdrsav =  pdrsql
028300040826     c                   end
028400040825      *
028500040825     C                   ENDSR
028600040820     C**************************************************************************
028700161108     C     rot_key       begSR
028800040823      *
028900040823     C* totale
029000040825     c                   if        prima_volta ='N'
029100040825      *
029200040825     c                   end
029300040820      *
029400040820     C                   ENDSR
029500170421      * ?------------------------------------------------------------------ */
029600170421      *?      X non bloccare in nessun caso il traduttore CLIENTI
029700170421      * ?------------------------------------------------------------------ */
029800170421     C     *pssr         BEGSR
029900170421     C
030000170421     c                   seton                                        LR
030100170421     c                   return
030200170421     C
030300170421     C                   ENDSR
030400170421      * ?------------------------------------------------------------------ */
