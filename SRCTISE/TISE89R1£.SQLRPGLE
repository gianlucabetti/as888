000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200020409     H**********************************************************************
000300111114     fTISE89P   o    E             PRINTER OFLIND(*in66)
000400111115     fwfATR89f  o  a E             disk
000500040820     H**********************************************************************
000600040820     d KPJBA         E DS
000700040820     D*
000800041103     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
000900041103     D CNCR80        E DS
001000111115     D*
001100111114     D tise89ds      e DS                  inz
001200040820     D*
001300040820     D WLBDAT          DS
001400040820     D  G02DAT                 1      8  0
001500040820     D  G02INV                 9     16  0
001600040820     D  G02ERR                17     17
001700040820     D  G02TGI                18     22  0
001800040820      *
001900040820     D WrkSqlCmd       S           1024
002000111115     D WrkEventi       S           1024
002100111115      *
002200040820     D  data_Oggi      s                   like(G02INV) INZ(0)
002300040820      *
002400111115     D Sav_Anno        s              4s 0
002500111115     D Sav_MeseGiorno  s              4s 0
002600040826     D  almeno_uno     s              1
002700111115     D Anno            s              4s 0
002800111115     D MeseGiorno      s              4s 0
002900111115     D LineaArrivo     s              3s 0
003000111115     D NrBolle         s              7s 0
003100111115     D NrEventi        s              7s 0
003200111115     D TotBolle        s              7s 0
003300111115     D TotEventi       s              7s 0
003400111115     D GenBolle        s              7s 0
003500111115     D GenEventi       s              7s 0
003600040820      *
003700040820     D digits          C                   CONST('0123456789')
003800040820      *****************************************************************
003900111115      *
004000011210     C/EXEC SQL
004100011210     C+ PREPARE S1 FROM :WrkSqlCmd
004200011210     C/END-EXEC
004300111115      *
004400011210     C/EXEC SQL
004500011210     C+ DECLARE A1 CURSOR FOR S1
004600011210     C/END-EXEC
004700111115      *
004800011210     C/EXEC SQL
004900011210     C+ OPEN A1
005000011210     C/END-EXEC
005100111115      *
005200040820     C                   DOU       SqlCod <> 0
005300011205     C/EXEC SQL
005400111115     C+ FETCH NEXT FROM A1 INTO :Anno, :MeseGiorno, :LineaArrivo, :NrBolle
005500011205     C/END-EXEC
005600011210     C                   SELECT
005700041007     c*
005800041007     c* a fine file Totali x rotture
005900041007     C                   WHEN      SqlCod = 100
006000041007     c                   if        almeno_uno ='S'
006100041007     c                   exsr      TOTals
006200041007     c                   else
006300041007     c                   write     testa
006400041007     c                   write     tesdetta
006500041007     c                   write     NODATI
006600041007     c                   end
006700041007     c                   leave
006800041007     **
006900041007     C                   WHEN      SqlCod < 0
007000041007     C                   seton                                        H1
007100041007     c                   goto      fine
007200040820     c*
007300041007     C                   OTHER
007400111115     c*
007500111115     c*  Calcola gli Eventi  di ATR
007600111115     c                   exsr      Cal_Eventi
007700111115     c*
007800111115     c* Totali x giornata
007900040825     c                   exsr      TOTals
008000040820     c* DETTAGLIO
008100040823     c                   if        *in66
008200040823      *                 * ------------------ *
008300040823     c                   write     testa
008400040823     c                   write     tesdetta
008500040823      *                 * ------------------ *
008600040823     c                   setoff                                       66
008700040825     c                   eval      prima_volta = 'N'
008800040823     c                   end
008900040823      *
009000111115     c                   movel     Anno          stpGior
009100111115     c                   move      MeseGiorno    stpGior
009200111115     c                   eval      stpanno = Anno
009300111115     c                   movel     MeseGiorno    stpMese
009400111115     c                   move      MeseGiorno    stpGior
009500111115     c                   eval      stpLNA  = LineaArrivo
009600111115     c                   eval      stpNRB  = NrBolle
009700111115     c                   eval      stpNEV  = NrEventi
009800111115     c                   z-add     0             stpPRC
009900111115     c                   if        NrBolle > 0
010000111116     c                   eval       stpPRC  = 100 - (NrEventi/NrBolle * 100)
010100111115     c                   end
010200120105      *  prima azzera i negativi
010300120105     c                   If         stpPRC  < 0
010400120105     c                   eval       stpPRC  = 0
010500120105     c                   end
010600120105     c*
010700111116     c                   write     dettagli
010800111115     c                   exsr      wri_record
010900040823     c*
011000111115     c* Totalizzatori
011100040826     c                   move      'S'           almeno_uno
011200111115     c                   add       NrBolle       totBolle
011300111115     c                   add       NrEventi      totEventi
011400111115     c                   add       NrBolle       genBolle
011500111115     c                   add       NrEventi      genEventi
011600040823      *
011700011210     C                   ENDSL
011800040820
011900011205     C                   ENDDO
012000011205     C/EXEC SQL
012100011205     C+ CLOSE A1
012200011205     C/END-EXEC
012300011205     c*
012400040820     C* totali generali
012500041007     c                   if        almeno_uno ='S'
012600040823     c                   if        *in66
012700040823      *                 * ------------------ *
012800120105     c************       write     testa
012900120105     c************       write     tesdetta
013000040823      *                 * ------------------ *
013100040823     c                   setoff                                       66
013200040823     c                   end
013300120105     c************       eval      totnrb =  GenBolle
013400120105     c************       eval      totnev =  GenEventi
013500120105     c************       write     totale
013600041007     c                   end
013700040823      *
013800041007     c     fine          tag
013900020326     C                   SETON                                        LR
014000040820     C**************************************************************************
014100040820     C* INIZIALIZZAZIONE
014200040820     C**************************************************************************
014300040820     C     *inzsr        BEGSR
014400040820      **
014500040820     C     *ENTRY        PLIST
014600040820     C                   PARM                    KPJBA
014700111115     c                   movel     kpjbu         tise89ds
014800041103      *
014900041103     C                   Z-ADD     1             CODUT
015000041103     C                   CALL      'X§PARUT'
015100041103     C                   PARM                    UT§DSE
015200041103     C                   MOVEL     REC80         CNCR80
015300041103     C                   MOVEL     RAGUT         RSUT             20
015400041103     C*
015500040820      *
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
016700111115     c                   move      e89dti        dadata
016800111115     c                   move      e89dtf        adata
016900111115      **
017000111115     c                   movel     e89dti        annoI             4 0
017100111115     c                   move      e89dti        mesgioI           4 0
017200111115      **
017300111115     c                   movel     e89dtf        annoF             4 0
017400111115     c                   move      e89dtf        mesgioF           4 0
017500040823      **
017600040823     c                   seton                                        66
017700111115      **
017800040825     c                   clear                   prima_volta       1
017900040826     c                   clear                   almeno_uno
018000040820     c*
018100040820     c* compongo istruzione sql
018200040820     c                   exsr      istruz
018300040820     c*
018400040820     C                   ENDSR
018500040820     C**************************************************************************
018600040820     C* mi compongo l'istruzione sql
018700040820     C**************************************************************************
018800040820     C     istruz        BEGSR                                                  *
018900040820      **
019000111115     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file TITAS
019100111115      **   suddividendo per ogni giorno del periodo
019200111115      **   il numero delle Bolle EXPORT x LINEA ARRIVO inviate al Partner
019300111115     C                   EVAL      WrkSqlCmd =
019400111115     C                             'select tasAAS, tasMGS, tasLNA, '  +
019500111115     C                             ' count(*) from ('                 +
019600111128      *----
019700111115     c                             ' select TITAS00F.* from titas00f '+
019800111116     c                             ' WHERE '                          +
019900111128     c                             ' taslna between 300 and 400 '
020000111128      ****
020100111128      **  Range di Periodo nello stesso Anno oppure su anni contigui
020200111128     c                   if        AnnoI = AnnoF
020300111128      ****
020400111128     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)      +
020500111128     C                             ' and tasAAS = '                   +
020600111128     C                             %editc(AnnoI:'X')                  +
020700111128     c                             ' AND '                            +
020800111128     C                             ' tasMGS between '                 +
020900111128     C                             %editc(mesgioI:'X')                +
021000111128     c                             ' and '                            +
021100111128     C                             %editc(mesgioF:'X')
021200111128     c                   eLSe
021300111128      ****
021400111128      *  Se gli anni sono differenti
021500111128     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)      +
021600111128     C                             ' and ( ( '                        +
021700111128     C                             ' tasAAS = '                       +
021800111128     C                             %editc(AnnoI:'X')                  +
021900111128     c                             ' AND '                            +
022000111128     C                             ' tasMGS >= '                      +
022100111128     C                             %editc(mesgioI:'X')                +
022200111128     c                             ' ) OR ( '                         +
022300111128     C                             ' tasAAS = '                       +
022400111128     C                             %editc(AnnoF:'X')                  +
022500111128     c                             ' AND '                            +
022600111128     C                             ' tasMGS <= '                      +
022700111128     C                             %editc(mesgioF:'X')                +
022800111128     c                             ' ) ) '
022900111128     c                   end
023000111128      *----
023100111128     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)      +
023200111115     c                             ' UNION '                          +
023300111115     c                             ' select TITAS10F.* from titas10f '+
023400111116     c                             ' WHERE '                          +
023500111128     c                             ' taslna between 300 and 400 '
023600111128      ****
023700111128      **  Range di Periodo nello stesso Anno oppure su anni contigui
023800111128     c                   if        AnnoI = AnnoF
023900111128      ****
024000111128     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)      +
024100111128     C                             ' and tasAAS = '                   +
024200111128     C                             %editc(AnnoI:'X')                  +
024300111128     c                             ' AND '                            +
024400111128     C                             ' tasMGS between '                 +
024500111128     C                             %editc(mesgioI:'X')                +
024600111128     c                             ' and '                            +
024700111128     C                             %editc(mesgioF:'X')
024800111128     c                   eLSe
024900111128      ****
025000111128      *  Se gli anni sono differenti
025100111128     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)      +
025200111128     C                             ' and ( ( '                        +
025300111128     C                             ' tasAAS = '                       +
025400111128     C                             %editc(AnnoI:'X')                  +
025500111128     c                             ' AND '                            +
025600111128     C                             ' tasMGS >= '                      +
025700111128     C                             %editc(mesgioI:'X')                +
025800111128     c                             ' ) OR ( '                         +
025900111128     C                             ' tasAAS = '                       +
026000111128     C                             %editc(AnnoF:'X')                  +
026100111128     c                             ' AND '                            +
026200111128     C                             ' tasMGS <= '                      +
026300111128     C                             %editc(mesgioF:'X')                +
026400111128     c                             ' ) ) '
026500111128     c                   end
026600111115      ****
026700111115      **  Ordinamento di lettura  x ANNO/MESEGIORNO/LINEA ARRIVO
026800111115      ****
026900111115     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)       +
027000111128     c                             ' ) as Bolle '                      +
027100111115     c                             ' GROUP BY tasAAS, tasMGS, tasLNA ' +
027200111115     c                             ' ORDER BY tasAAS, tasMGS, tasLNA '
027300040820      **
027400040820     C                   ENDSR
027500020326     C**************************************************************************
027600040820      *
027700040820     C**************************************************************************
027800040820     C     testata       begSR
027900040820      **
028000040820      *                 * ------------------ *
028100040820     c                   write     testa
028200040820      *                 * ------------------ *
028300040820      *
028400040820     C                   ENDSR
028500040825     C**************************************************************************
028600040825      *
028700040825     C**************************************************************************
028800040825     C     TOTals        begSR
028900040826      *
029000040825     c* ROTTURA Fattura
029100111115     c                   if        Anno <> sav_Anno or
029200111115     C                             MeseGiorno <> sav_MeseGIORNO OR
029300111115     C                             SqlCod = 100
029400040831     c                   if        almeno_uno ='S'
029500111115     c                   exsr      TES_GIORNO
029600040831     c                   end
029700111115     c                   eval      sav_Anno = Anno
029800111115     c                   eval      sav_MeseGIORNO = MeseGIORNO
029900040826     c                   end
030000040825      *
030100040825     C                   ENDSR
030200040820     C**************************************************************************
030300111115      *
030400111115     C**************************************************************************
030500111115     C     TES_GIORNO    begSR
030600040823      *
030700040823     C* totale
030800040825     c                   if        prima_volta ='N'
030900040831      *
031000040823     c                   if        *in66
031100040823      *                 * ------------------ *
031200120105     c************       write     testa
031300120105     c************       write     tesdetta
031400040823      *                 * ------------------ *
031500040823     c                   setoff                                       66
031600040823     c                   end
031700111115      *
031800120105     c************       eval      totnrb =  TotBolle
031900120105     c************       eval      totnev =  TotEventi
032000120105     c************       write     totale
032100040823     c                   end
032200040825      *
032300111115     c                   clear                   totbolle
032400111115     c                   clear                   toteventi
032500040820      *
032600040820     C                   ENDSR
032700111115     C**************************************************************************
032800111115      *   sulla giornata x LNA calcola i relativi eventi di ATR ESEGUITI
032900111115     C**************************************************************************
033000111115     C     cal_Eventi    begSR
033100111115      **
033200111115     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file degli EVENTI
033300111115      **  sulla base dell BOLLE con LINEA estera
033400111115      **  etratte sulla giornata specifica.
033500111115     C                   EVAL      WrkEVENTI =
033600111115     C                             'select count(*) from fnevb00f  '  +
033700111115     C                             ' where evbcev like ''EE%'' and '  +
033800111115     c                             ' digits(evbaas) concat '          +
033900111115     c                             ' digits(evblnp) concat '          +
034000111115     c                             ' digits(evbnrs) concat '          +
034100111115     c                             ' digits(evbnsp)   IN ( '          +
034200111115     c                             ' SELECT '                         +
034300111115     c                             ' digits(tasaas) concat '          +
034400111115     c                             ' digits(taslnp) concat '          +
034500111115     c                             ' digits(tasnrs) concat '          +
034600111115     c                             ' digits(tasnsp)  FROM ( '         +
034700111115     c                             ' select TITAS00F.* from titas00f '+
034800111116     c                             ' WHERE '                          +
034900111116     c                             ' taslna = '                       +
035000111116     C                             %editc(LineaArrivo:'X')            +
035100111116     c                             ' AND '                            +
035200111116     c                             ' tasAAS = '                       +
035300111116     C                             %editc(Anno:'X')                   +
035400111116     c                             ' AND '                            +
035500111116     c                             ' tasMGS = '                       +
035600111116     C                             %editc(MeseGiorno:'X')             +
035700111128      *--
035800111115     c                             ' UNION '                          +
035900111115     c                             ' select TITAS10F.* from titas10f '+
036000111116     c                             ' WHERE '                          +
036100111116     c                             ' taslna = '                       +
036200111116     C                             %editc(LineaArrivo:'X')            +
036300111116     c                             ' AND '                            +
036400111116     c                             ' tasAAS = '                       +
036500111116     C                             %editc(Anno:'X')                   +
036600111116     c                             ' AND '                            +
036700111116     c                             ' tasMGS = '                       +
036800111116     C                             %editc(MeseGiorno:'X')             +
036900111116     c                             ' ) as Bolle '                     +
037000111115     c                             ' )   '
037100111115      **
037200111115      *
037300111115     c                   clear                   NrEventi
037400111115      *
037500111115     C/EXEC SQL
037600111115     C+ PREPARE S2 FROM :WrkEVENTI
037700111115     C/END-EXEC
037800111115      *
037900111115     C/EXEC SQL
038000111115     C+ DECLARE A2 CURSOR FOR S2
038100111115     C/END-EXEC
038200111115      *
038300111115     C/EXEC SQL
038400111115     C+ OPEN A2
038500111115     C/END-EXEC
038600111115      *
038700111115     C                   IF        SqlCod >= 0
038800111115     C/EXEC SQL
038900111115     C+ FETCH NEXT FROM A2 INTO  :NrEventi
039000111115     C/END-EXEC
039100111115     C                   SELECT
039200111115     c*
039300111115     c* a fine file Totali x rotture
039400111115     C                   WHEN      SqlCod = 100
039500111115     c                   clear                   NrEventi
039600111115     **
039700111115     C                   WHEN      SqlCod < 0
039800111115     C                   seton                                        H2
039900111115     c                   goto      fine
040000111115     c*
040100111115     C                   OTHER
040200111115      *   se tutto Bene
040300111115     c*    il contatore degli Eventi contiene un valore
040400111115     C                   ENDSL
040500111115
040600111115     C                   END
040700111115     C/EXEC SQL
040800111115     C+ CLOSE A2
040900111115     C/END-EXEC
041000111115      *
041100111115     C                   ENDSR
041200111115     C**************************************************************************
041300111115      *   Scrive il record nel file di WRK WFATR89F
041400111115     C**************************************************************************
041500111115     C     wri_Record    begSR
041600111115      *
041700111115     C* scrive il record sul file di work da elaborare
041800111115     c                   clear                   wfATR89
041900111115     c                   movel     Anno          F89Gio
042000111115     c                   move      MeseGiorno    F89Gio
042100111115     c                   eval       F89DAL  =  e89dti
042200111115     c                   eval       F89AL   =  e89dtf
042300111115     c                   eval       F89LNA  = LineaArrivo
042400111115     c                   eval       F89NSP  = NrBolle
042500111115     c                   eval       F89ATR  = NrEventi
042600111115     c                   eval       F89NON  = NrBolle - NrEventi
042700120105     c                   if        F89NON < 0
042800120105     c                   z-add     0             F89NON
042900120105     c                   end
043000111115     c                   if        NrBolle > 0
043100111116     c                   eval       F89PRC  = 100 - (NrEventi/NrBolle * 100)
043200111115     c                   end
043300120105     c                   if        F89PRC < 0
043400120105     c                   z-add     0             F89PRC
043500120105     c                   end
043600111115     c                   write     wfATR89
043700111115      *
043800111115     C                   ENDSR
043900040820     C**************************************************************************
