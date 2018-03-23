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
001100040820     d KPJBA         E DS
001200161108     D trbi05ds      e DS                  inz
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
003300161109     D  data_8         s              8s 0
003400161108      *
003500040826     D  almeno_uno     s              1
003600040820      *
003700040820     D digits          C                   CONST('0123456789')
003800161108      *****************************************************************
003900161108     c* compongo istruzione sql
004000161108     c                   exsr      istruz
004100161109      *
004200161108      *
004300161108      *****************************************************************
004400161108      *  poi li rigenera
004500011210     C/EXEC SQL
004600011210     C+ PREPARE S1 FROM :WrkSqlCmd
004700011210     C/END-EXEC
004800011210
004900011210     C/EXEC SQL
005000011210     C+ DECLARE A1 CURSOR FOR S1
005100011210     C/END-EXEC
005200011210
005300011210     C/EXEC SQL
005400011210     C+ OPEN A1
005500011210     C/END-EXEC
005600040820     C                   DOU       SqlCod <> 0
005700040823     c*
005800011205     C/EXEC SQL
005900161109     C+ FETCH NEXT FROM A1 INTO :fgssql, :pdrsql, :dfvsql, :numsql, :volsql
006000011205     C/END-EXEC
006100011210     C                   SELECT
006200041007     c*
006300041007     c* a fine file Totali x rotture
006400041007     C                   WHEN      SqlCod = 100
006500041007     c                   if        almeno_uno ='S'
006600161108     c                   exsr      TOTali
006700041007     c                   end
006800041007     c                   leave
006900041007     **
007000041007     C                   WHEN      SqlCod < 0
007100041007     C                   seton                                        H1
007200041007     c                   goto      fine
007300040820     c*
007400041007     C                   OTHER
007500040825     c* Totali x rotture
007600161108     c                   exsr      TOTali
007700040820     c* DETTAGLIO
007800161110     c                   eval      prima_volta = 'N'
007900040826     c                   move      'S'           almeno_uno
008000161110      *
008100161110     c                   clear                   biasp000
008200161108     c                   exsr      dettaglio
008300161110     c                   write     biasp000
008400040823      *
008500011210     C                   ENDSL
008600040820
008700011205     C                   ENDDO
008800011205     C/EXEC SQL
008900011205     C+ CLOSE A1
009000011205     C/END-EXEC
009100011205     c*
009200040820     C* totali generali
009300041007     c                   if        almeno_uno ='S'
009400041007     c                   end
009500040823      *
009600041007     c     fine          tag
009700020326     C                   SETON                                        LR
009800040820     C**************************************************************************
009900040820     C* INIZIALIZZAZIONE
010000040820     C**************************************************************************
010100040820     C     *inzsr        BEGSR
010200040820      **
010300161108     C     apd01         kLIST
010400161108     c                   kFLD                    apdtip
010500161108     c                   kFLD                    apdpdr
010600161108      **
010700161108     C     tbe01         kLIST
010800161108     c                   kFLD                    tbeCOD
010900161108     c                   kFLD                    tbeKE1
011000161108      **
011100161109     C     tbe02         kLIST
011200161109     c                   kFLD                    tbeCOD
011300161109     c                   kFLD                    tbeKE1
011400161109     c                   kFLD                    tbeKE2
011500161109      **
011600040820     C     *ENTRY        PLIST
011700040820     C                   PARM                    KPJBA
011800161109      **
011900161110     c                   clear                   TRBI05DS
012000161110      **
012100161109      *   data passata e diversa da "00000000"
012200161110     c                   if        kpjbu = *blanks
012300161110     c                   movel     *zeros        data8alf          8
012400161110     c                   else
012500161109     c                   movel     kpjbu         data8alf          8
012600161109      ** controlla che vi siano solo dei numeri nel campo
012700161109     c     digits        check     data8alf
012800161109     c                   if        not %Found
012900161109     c                   movel     kpjbu         TRBI05DS
013000161109     c                   end
013100161109     c                   end
013200040820      *
013300040820      *  imposta UDATE di elaborazione
013400040820     C                   move      *date         G02DAT
013500040820     C                   MOVE      *ZEROS        G02INV
013600040820     C                   MOVE      *BLANKS       G02ERR
013700040820     C                   CALL      'XSRDA8'
013800040820     C                   PARM                    WLBDAT
013900040820     C                   Z-ADD     G02INV        data_oggi
014000161108     C                   move      data_oggi     day               8 0
014100040820     C                   TIME                    W0140            14 0
014200161108     C                   MOVEl     W0140         ore               6 0
014300040823      **
014400040825     c                   clear                   prima_volta       1
014500040826     c                   clear                   almeno_uno
014600161109      **
014700161109     c                   movel     'MAS'         tbeCOD
014800161109     c                   clear                   massa_da
014900161109     c                   clear                   massa_a
015000161109     c                   clear                   massa_cod
015100161109     c                   z-add     0             nr                3 0
015200161109     C     tbeCOD        setll     tntbe01l
015300161109     C     tbeCOD        reade     tntbe01l
015400161109     c                   dow       not %eof(tntbe01l) and nr < 10
015500161109     c                   add       1             nr
015600161109     c                   movel     tbeke1        massa_DA(nr)
015700161109     c                   movel     tbeke2        massa_cod(nr)
015800161109     c                   if        nr > 1
015900161109     c                   eval       massa_A(nr-1) = massa_DA(nr) - 0,1
016000161109     c                   end
016100161109     C     tbeCOD        reade     tntbe01l
016200161109     c                   end
016300161109     c* finale estremo
016400161109     c                   if        nr=1
016500161109     c                   eval       massa_A(1) = 999999,9
016600161109     c                   else
016700161109     c                   eval       massa_A(nr) = 999999,9
016800161109     c                   eval       massa_A(nr-1) = massa_DA(nr) - 0,1
016900161109     c                   end
017000040820     c*
017100040820     C                   ENDSR
017200040820     C**************************************************************************
017300040820     C* mi compongo l'istruzione sql
017400040820     C**************************************************************************
017500040820     C     istruz        BEGSR                                                  *
017600040820      **
017700040820     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file TNECF
017800040820     C                   EVAL      WrkSqlCmd
017900040820     C                             =
018000161109     C                             'SELECT TDIFGS, TDIPDR, TDIDFV, '           +
018100170313     c                             'dec(sum(TDINBLA + TDINBLR), 5) nr_sped, '  +
018200170313     c                             'dec(sum(TDIVLUA + TDIVLUR), 7, 3) volume ' +
018300161109     c                             'FROM BITDI00f '
018400161109      ***
018500161109      * Solo se deve rigenerare la data
018600161109     c                   if           i05data <> 0
018700161109     C                   EVAL      WrkSqlCmd
018800161109     C                             =  %trim(WrkSqlCmd) + '  '                  +
018900161109     c                             'WHERE TDIDFV= ' + %trim(%char(i05data))
019000161109     c                   end
019100161109      *
019200161109     C                   EVAL      WrkSqlCmd
019300161109     C                             =  %trim(WrkSqlCmd) + '  '                  +
019400161115     C                             'GROUP BY TDIFGS, TDIPDR, TDIDFV '
019500040820     C                   ENDSR
019600020326     C**************************************************************************
019700161108      *   esegue il dettaglio
019800040820     C**************************************************************************
019900161108     C     dettaglio     begSR
020000040820      *
020100161109     c                   move      dfvsql        datmmgg           4 0
020200161109     c                   movel     datmmgg       datmese           2 0
020300161109      *
020400161109     c                   z-add     dfvsql        aspdfv
020500161108     c                   z-add     fgssql        aspfgs
020600161108     c                   z-add     pdrsql        asppdr
020700170313     c******             z-add     numsql        aspNBLA
020800170313     c******             z-add     volsql        aspVLUA
020900161108     c                   eval      tmp= %char(%timestamp: *iso0)
021000161108     c                   eval      ASPDatora  =   tmp
021100161108      *
021200161108      *  decodifiche da anagrafica
021300161108     c                   move      'A'           apdTIP
021400161108     c                   z-add     pdrsql        apdPDR
021500161108     C     apd01         chain     fiapd01l
021600161108     c                   if        %Found(fiapd01l)
021700161108     c                   clear                   chiave_mas        1
021800161108     c                   clear                   chiave_aat        1
021900161108     c                   clear                   chiave_ats        1
022000161108     c                   movel(p)  apdflr        Dapdflr
022100161108      *massa
022200161109     c                   DO        10            nr
022300161109     c                   if        §apdMCP  >= massa_DA(nr) and
022400161109     c                             §apdMCP  <= massa_A(nr)
022500161109     c                   movel     massa_COD(nr) chiave_mas
022600161109     c                   leave
022700161109     c                   end
022800161109     c                   endDO
022900161108      *attività
023000161108     c                   movel     'AAT'         tbeCOD
023100161108     c                   movel(p)  §apdAAT       tbeKE1
023200161108     C     tbe01         chain     tntbe01l
023300161108     c                   if        %found(tntbe01l)
023400161108     c                   movel     tbeke1        chiave_aat
023500161108     c                   end
023600161108      *territorio
023700161108     c                   movel     'ATS'         tbeCOD
023800161108     c                   movel(p)  §apdATS       tbeKE1
023900161108     C     tbe01         chain     tntbe01l
024000161108     c                   if        %found(tntbe01l)
024100161108     c                   movel     tbeke1        chiave_ats
024200161108     c                   end
024300161108      *  tab.AIG aspettativa
024400161108     c                   movel     'AIG'         tbeCOD
024500161108     c                   eval      tbeKE1 = %editc(aspfgs:'X') + 'N' +
024600161108     c                             chiave_ats + chiave_mas + chiave_aat
024700161115     c                   movel(p)  dfvsql        tbeKE2
024800161115     C     tbe02         setll     tntbe01l
024900161115     C     tbe01         reade     tntbe01l
025000161115     c                   if        %eof(tntbe01l)
025100161109     c                   movel     '999'         tbeKE1
025200161115     C     tbe02         setll     tntbe01l
025300161115     C     tbe01         reade     tntbe01l
025400161109     c                   end
025500161115     c                   if        not %eof(tntbe01l)
025600161108     c                   movel     tbeuni        dAIG
025700161108     c                   z-add     D§AIGIMP      aspIMPN
025800161108     c                   end
025900161108      *-
026000161108     c                   eval      tbeKE1 = %editc(aspfgs:'X') + 'O' +
026100161108     c                             chiave_ats + chiave_mas + chiave_aat
026200161115     c                   movel(p)  dfvsql        tbeKE2
026300161115     C     tbe02         setll     tntbe01l
026400161115     C     tbe01         reade     tntbe01l
026500161115     c                   if        %eof(tntbe01l)
026600161115     c                   movel     '999'         tbeKE1
026700161115     C     tbe02         setll     tntbe01l
026800161115     C     tbe01         reade     tntbe01l
026900161115     c                   end
027000161115     c                   if        not %eof(tntbe01l)
027100161108     c                   movel     tbeuni        dAIG
027200161108     c                   z-add     D§AIGIMP      aspIMPO
027300170313     c                   z-add(h)  D§AIGQTAs     aspQTAS
027400170313     c                   z-add     D§AIGQTAv     aspQTAV
027500161108     c                   end
027600161108      *-
027700161108     c                   end
027800161108      * ASPGG
027900161109      * Giorni di aspettativa
028000161109     c                   movel     'AGL'         tbeCOD
028100161109     c                   movel(p)  fgssql        tbeKE1
028200161109     c                   movel(p)  dfvsql        tbeKE2
028300161109     C     tbe02         setll     tntbe01l
028400161109     C     tbe01         reade     tntbe01l
028500161109     c                   if        %eof(tntbe01l)
028600161109     c                   movel(p)  '999'         tbeKE1
028700161109     C     tbe02         setll     tntbe01l
028800161109     C     tbe01         reade     tntbe01l
028900161109     c                   end
029000161109     c                   if        not %eof(tntbe01l)
029100161109     c                   movel     tbeuni        dAGL
029200161109     c                   eval      aspGG = gg_mese(datmese)
029300161109     c                   end
029400161109      *
029500040820     C                   ENDSR
029600040825     C**************************************************************************
029700040825      *
029800040825     C**************************************************************************
029900161108     C     TOTali        begSR
030000040826      *
030100161108     c* ROTTURA chiave
030200161108     c                   if        pdrsql <> pdrsav or
030300040825     C                             SqlCod = 100
030400040831     c                   if        almeno_uno ='S'
030500161108     c                   exsr      rot_key
030600161108     c                   end
030700161108     c                   eval      pdrsav =  pdrsql
030800040826     c                   end
030900040825      *
031000040825     C                   ENDSR
031100040820     C**************************************************************************
031200161108     C     rot_key       begSR
031300040823      *
031400040823     C* totale
031500040825     c                   if        prima_volta ='N'
031600040825      *
031700040825     c                   end
031800040820      *
031900040820     C                   ENDSR
032000040820     C**************************************************************************
