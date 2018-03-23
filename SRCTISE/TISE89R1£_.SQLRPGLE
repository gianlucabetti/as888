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
010200111116     c                   write     dettagli
010300111115     c                   exsr      wri_record
010400040823     c*
010500111115     c* Totalizzatori
010600040826     c                   move      'S'           almeno_uno
010700111115     c                   add       NrBolle       totBolle
010800111115     c                   add       NrEventi      totEventi
010900111115     c                   add       NrBolle       genBolle
011000111115     c                   add       NrEventi      genEventi
011100040823      *
011200011210     C                   ENDSL
011300040820
011400011205     C                   ENDDO
011500011205     C/EXEC SQL
011600011205     C+ CLOSE A1
011700011205     C/END-EXEC
011800011205     c*
011900040820     C* totali generali
012000041007     c                   if        almeno_uno ='S'
012100040823     c                   if        *in66
012200040823      *                 * ------------------ *
012300040823     c                   write     testa
012400040823     c                   write     tesdetta
012500040823      *                 * ------------------ *
012600040823     c                   setoff                                       66
012700040823     c                   end
012800111115     c                   eval      totnrb =  GenBolle
012900111115     c                   eval      totnev =  GenEventi
013000040823     c                   write     totale
013100041007     c                   end
013200040823      *
013300041007     c     fine          tag
013400020326     C                   SETON                                        LR
013500040820     C**************************************************************************
013600040820     C* INIZIALIZZAZIONE
013700040820     C**************************************************************************
013800040820     C     *inzsr        BEGSR
013900040820      **
014000040820     C     *ENTRY        PLIST
014100040820     C                   PARM                    KPJBA
014200111115     c                   movel     kpjbu         tise89ds
014300041103      *
014400041103     C                   Z-ADD     1             CODUT
014500041103     C                   CALL      'X§PARUT'
014600041103     C                   PARM                    UT§DSE
014700041103     C                   MOVEL     REC80         CNCR80
014800041103     C                   MOVEL     RAGUT         RSUT             20
014900041103     C*
015000040820      *
015100040820      *  imposta UDATE di elaborazione
015200040820     C                   move      *date         G02DAT
015300040820     C                   MOVE      *ZEROS        G02INV
015400040820     C                   MOVE      *BLANKS       G02ERR
015500040820     C                   CALL      'XSRDA8'
015600040820     C                   PARM                    WLBDAT
015700040820     C                   Z-ADD     G02INV        data_oggi
015800040820     C                   move      data_oggi     day
015900040820     C                   TIME                    W0140            14 0
016000040820     C                   MOVEl     W0140         ore
016100040823      **
016200111115     c                   move      e89dti        dadata
016300111115     c                   move      e89dtf        adata
016400111115      **
016500111115     c                   movel     e89dti        annoI             4 0
016600111115     c                   move      e89dti        mesgioI           4 0
016700111115      **
016800111115     c                   movel     e89dtf        annoF             4 0
016900111115     c                   move      e89dtf        mesgioF           4 0
017000040823      **
017100040823     c                   seton                                        66
017200111115      **
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
018500111115     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file TITAS
018600111115      **   suddividendo per ogni giorno del periodo
018700111115      **   il numero delle Bolle EXPORT x LINEA ARRIVO inviate al Partner
018800111115     C                   EVAL      WrkSqlCmd =
018900111115     C                             'select tasAAS, tasMGS, tasLNA, '  +
019000111115     C                             ' count(*) from ('                 +
019100111128      *----
019200111115     c                             ' select TITAS00F.* from titas00f '+
019300111116     c                             ' WHERE '                          +
019400111128     c                             ' taslna between 300 and 400 '
019500111128      ****
019600111128      **  Range di Periodo nello stesso Anno oppure su anni contigui
019700111128     c                   if        AnnoI = AnnoF
019800111128      ****
019900111128     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)      +
020000111128     C                             ' and tasAAS = '                   +
020100111128     C                             %editc(AnnoI:'X')                  +
020200111128     c                             ' AND '                            +
020300111128     C                             ' tasMGS between '                 +
020400111128     C                             %editc(mesgioI:'X')                +
020500111128     c                             ' and '                            +
020600111128     C                             %editc(mesgioF:'X')
020700111128     c                   eLSe
020800111128      ****
020900111128      *  Se gli anni sono differenti
021000111128     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)      +
021100111128     C                             ' and ( ( '                        +
021200111128     C                             ' tasAAS = '                       +
021300111128     C                             %editc(AnnoI:'X')                  +
021400111128     c                             ' AND '                            +
021500111128     C                             ' tasMGS >= '                      +
021600111128     C                             %editc(mesgioI:'X')                +
021700111128     c                             ' ) OR ( '                         +
021800111128     C                             ' tasAAS = '                       +
021900111128     C                             %editc(AnnoF:'X')                  +
022000111128     c                             ' AND '                            +
022100111128     C                             ' tasMGS <= '                      +
022200111128     C                             %editc(mesgioF:'X')                +
022300111128     c                             ' ) ) '
022400111128     c                   end
022500111128      *----
022600111128     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)      +
022700111115     c                             ' UNION '                          +
022800111115     c                             ' select TITAS10F.* from titas10f '+
022900111116     c                             ' WHERE '                          +
023000111128     c                             ' taslna between 300 and 400 '
023100111128      ****
023200111128      **  Range di Periodo nello stesso Anno oppure su anni contigui
023300111128     c                   if        AnnoI = AnnoF
023400111128      ****
023500111128     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)      +
023600111128     C                             ' and tasAAS = '                   +
023700111128     C                             %editc(AnnoI:'X')                  +
023800111128     c                             ' AND '                            +
023900111128     C                             ' tasMGS between '                 +
024000111128     C                             %editc(mesgioI:'X')                +
024100111128     c                             ' and '                            +
024200111128     C                             %editc(mesgioF:'X')
024300111128     c                   eLSe
024400111128      ****
024500111128      *  Se gli anni sono differenti
024600111128     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)      +
024700111128     C                             ' and ( ( '                        +
024800111128     C                             ' tasAAS = '                       +
024900111128     C                             %editc(AnnoI:'X')                  +
025000111128     c                             ' AND '                            +
025100111128     C                             ' tasMGS >= '                      +
025200111128     C                             %editc(mesgioI:'X')                +
025300111128     c                             ' ) OR ( '                         +
025400111128     C                             ' tasAAS = '                       +
025500111128     C                             %editc(AnnoF:'X')                  +
025600111128     c                             ' AND '                            +
025700111128     C                             ' tasMGS <= '                      +
025800111128     C                             %editc(mesgioF:'X')                +
025900111128     c                             ' ) ) '
026000111128     c                   end
026100111128      *----
026200111128     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)      +
026300111115     c                             ' UNION '                          +
026400111116     c                             ' select TITASP0F.* from titasP0f '+
026500111116     c                             ' WHERE '                          +
026600111128     c                             ' taslna between 300 and 400 '
026700111128      ****
026800111128      **  Range di Periodo nello stesso Anno oppure su anni contigui
026900111128     c                   if        AnnoI = AnnoF
027000111128      ****
027100111128     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)      +
027200111128     C                             ' and tasAAS = '                   +
027300111128     C                             %editc(AnnoI:'X')                  +
027400111128     c                             ' AND '                            +
027500111128     C                             ' tasMGS between '                 +
027600111128     C                             %editc(mesgioI:'X')                +
027700111128     c                             ' and '                            +
027800111128     C                             %editc(mesgioF:'X')
027900111128     c                   eLSe
028000111128      ****
028100111128      *  Se gli anni sono differenti
028200111128     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)      +
028300111128     C                             ' and ( ( '                        +
028400111128     C                             ' tasAAS = '                       +
028500111128     C                             %editc(AnnoI:'X')                  +
028600111128     c                             ' AND '                            +
028700111128     C                             ' tasMGS >= '                      +
028800111128     C                             %editc(mesgioI:'X')                +
028900111128     c                             ' ) OR ( '                         +
029000111128     C                             ' tasAAS = '                       +
029100111128     C                             %editc(AnnoF:'X')                  +
029200111128     c                             ' AND '                            +
029300111128     C                             ' tasMGS <= '                      +
029400111128     C                             %editc(mesgioF:'X')                +
029500111128     c                             ' ) ) '
029600111128     c                   end
029700111128      *----
029800111115      ****
029900111115      **  Ordinamento di lettura  x ANNO/MESEGIORNO/LINEA ARRIVO
030000111115      ****
030100111115     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)       +
030200111128     c                             ' ) as Bolle '                      +
030300111115     c                             ' GROUP BY tasAAS, tasMGS, tasLNA ' +
030400111115     c                             ' ORDER BY tasAAS, tasMGS, tasLNA '
030500040820      **
030600040820     C                   ENDSR
030700020326     C**************************************************************************
030800040820      *
030900040820     C**************************************************************************
031000040820     C     testata       begSR
031100040820      **
031200040820      *                 * ------------------ *
031300040820     c                   write     testa
031400040820      *                 * ------------------ *
031500040820      *
031600040820     C                   ENDSR
031700040825     C**************************************************************************
031800040825      *
031900040825     C**************************************************************************
032000040825     C     TOTals        begSR
032100040826      *
032200040825     c* ROTTURA Fattura
032300111115     c                   if        Anno <> sav_Anno or
032400111115     C                             MeseGiorno <> sav_MeseGIORNO OR
032500111115     C                             SqlCod = 100
032600040831     c                   if        almeno_uno ='S'
032700111115     c                   exsr      TES_GIORNO
032800040831     c                   end
032900111115     c                   eval      sav_Anno = Anno
033000111115     c                   eval      sav_MeseGIORNO = MeseGIORNO
033100040826     c                   end
033200040825      *
033300040825     C                   ENDSR
033400040820     C**************************************************************************
033500111115      *
033600111115     C**************************************************************************
033700111115     C     TES_GIORNO    begSR
033800040823      *
033900040823     C* totale
034000040825     c                   if        prima_volta ='N'
034100040831      *
034200040823     c                   if        *in66
034300040823      *                 * ------------------ *
034400040823     c                   write     testa
034500040823     c                   write     tesdetta
034600040823      *                 * ------------------ *
034700040823     c                   setoff                                       66
034800040823     c                   end
034900111115      *
035000111115     c                   eval      totnrb =  TotBolle
035100111115     c                   eval      totnev =  TotEventi
035200111115     c                   write     totale
035300040823     c                   end
035400040825      *
035500111115     c                   clear                   totbolle
035600111115     c                   clear                   toteventi
035700040820      *
035800040820     C                   ENDSR
035900111115     C**************************************************************************
036000111115      *   sulla giornata x LNA calcola i relativi eventi di ATR ESEGUITI
036100111115     C**************************************************************************
036200111115     C     cal_Eventi    begSR
036300111115      **
036400111115     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file degli EVENTI
036500111115      **  sulla base dell BOLLE con LINEA estera
036600111115      **  etratte sulla giornata specifica.
036700111115     C                   EVAL      WrkEVENTI =
036800111115     C                             'select count(*) from fnevb00f  '  +
036900111115     C                             ' where evbcev like ''EE%'' and '  +
037000111115     c                             ' digits(evbaas) concat '          +
037100111115     c                             ' digits(evblnp) concat '          +
037200111115     c                             ' digits(evbnrs) concat '          +
037300111115     c                             ' digits(evbnsp)   IN ( '          +
037400111115     c                             ' SELECT '                         +
037500111115     c                             ' digits(tasaas) concat '          +
037600111115     c                             ' digits(taslnp) concat '          +
037700111115     c                             ' digits(tasnrs) concat '          +
037800111115     c                             ' digits(tasnsp)  FROM ( '         +
037900111115     c                             ' select TITAS00F.* from titas00f '+
038000111116     c                             ' WHERE '                          +
038100111116     c                             ' taslna = '                       +
038200111116     C                             %editc(LineaArrivo:'X')            +
038300111116     c                             ' AND '                            +
038400111116     c                             ' tasAAS = '                       +
038500111116     C                             %editc(Anno:'X')                   +
038600111116     c                             ' AND '                            +
038700111116     c                             ' tasMGS = '                       +
038800111116     C                             %editc(MeseGiorno:'X')             +
038900111128      *--
039000111115     c                             ' UNION '                          +
039100111115     c                             ' select TITAS10F.* from titas10f '+
039200111116     c                             ' WHERE '                          +
039300111116     c                             ' taslna = '                       +
039400111116     C                             %editc(LineaArrivo:'X')            +
039500111116     c                             ' AND '                            +
039600111116     c                             ' tasAAS = '                       +
039700111116     C                             %editc(Anno:'X')                   +
039800111116     c                             ' AND '                            +
039900111116     c                             ' tasMGS = '                       +
040000111116     C                             %editc(MeseGiorno:'X')             +
040100111128      *--
040200111115     c                             ' UNION '                          +
040300111116     c                             ' select TITASP0F.* from titasP0f '+
040400111116     c                             ' WHERE '                          +
040500111116     c                             ' taslna = '                       +
040600111116     C                             %editc(LineaArrivo:'X')            +
040700111116     c                             ' AND '                            +
040800111116     c                             ' tasAAS = '                       +
040900111116     C                             %editc(Anno:'X')                   +
041000111116     c                             ' AND '                            +
041100111116     c                             ' tasMGS = '                       +
041200111116     C                             %editc(MeseGiorno:'X')             +
041300111116     c                             ' ) as Bolle '                     +
041400111115     c                             ' )   '
041500111115      **
041600111115      *
041700111115     c                   clear                   NrEventi
041800111115      *
041900111115     C/EXEC SQL
042000111115     C+ PREPARE S2 FROM :WrkEVENTI
042100111115     C/END-EXEC
042200111115      *
042300111115     C/EXEC SQL
042400111115     C+ DECLARE A2 CURSOR FOR S2
042500111115     C/END-EXEC
042600111115      *
042700111115     C/EXEC SQL
042800111115     C+ OPEN A2
042900111115     C/END-EXEC
043000111115      *
043100111115     C                   IF        SqlCod >= 0
043200111115     C/EXEC SQL
043300111115     C+ FETCH NEXT FROM A2 INTO  :NrEventi
043400111115     C/END-EXEC
043500111115     C                   SELECT
043600111115     c*
043700111115     c* a fine file Totali x rotture
043800111115     C                   WHEN      SqlCod = 100
043900111115     c                   clear                   NrEventi
044000111115     **
044100111115     C                   WHEN      SqlCod < 0
044200111115     C                   seton                                        H2
044300111115     c                   goto      fine
044400111115     c*
044500111115     C                   OTHER
044600111115      *   se tutto Bene
044700111115     c*    il contatore degli Eventi contiene un valore
044800111115     C                   ENDSL
044900111115
045000111115     C                   END
045100111115     C/EXEC SQL
045200111115     C+ CLOSE A2
045300111115     C/END-EXEC
045400111115      *
045500111115     C                   ENDSR
045600111115     C**************************************************************************
045700111115      *   Scrive il record nel file di WRK WFATR89F
045800111115     C**************************************************************************
045900111115     C     wri_Record    begSR
046000111115      *
046100111115     C* scrive il record sul file di work da elaborare
046200111115     c                   clear                   wfATR89
046300111115     c                   movel     Anno          F89Gio
046400111115     c                   move      MeseGiorno    F89Gio
046500111115     c                   eval       F89DAL  =  e89dti
046600111115     c                   eval       F89AL   =  e89dtf
046700111115     c                   eval       F89LNA  = LineaArrivo
046800111115     c                   eval       F89NSP  = NrBolle
046900111115     c                   eval       F89ATR  = NrEventi
047000111115     c                   eval       F89NON  = NrBolle - NrEventi
047100111115     c                   if        NrBolle > 0
047200111116     c                   eval       F89PRC  = 100 - (NrEventi/NrBolle * 100)
047300111115     c                   end
047400111115     c                   write     wfATR89
047500111115      *
047600111115     C                   ENDSR
047700040820     C**************************************************************************
