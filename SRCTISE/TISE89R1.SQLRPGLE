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
010000130109     c                   clear                   PERCENT152       15 2
010100130109     c                   eval       PERCENT152 = 100 - (NrEventi/NrBolle * 100)
010200130109     C                   IF         PERCENT152 > 999,99
010300130109     c                   eval       stpPRC  = *ALL'9'
010400130109     C                   ELSEIF     PERCENT152 < -999,99
010500130109     c                   eval       stpPRC  = 0
010600130109     c                   eLSE
010700130109     c                   eval       stpPRC  = PERCENT152
010800130109     c                   end
010900111115     c                   end
011000120105      *  prima azzera i negativi
011100120105     c                   If         stpPRC  < 0
011200120105     c                   eval       stpPRC  = 0
011300120105     c                   end
011400120105     c*
011500111116     c                   write     dettagli
011600111115     c                   exsr      wri_record
011700040823     c*
011800111115     c* Totalizzatori
011900040826     c                   move      'S'           almeno_uno
012000111115     c                   add       NrBolle       totBolle
012100111115     c                   add       NrEventi      totEventi
012200111115     c                   add       NrBolle       genBolle
012300111115     c                   add       NrEventi      genEventi
012400040823      *
012500011210     C                   ENDSL
012600040820
012700011205     C                   ENDDO
012800011205     C/EXEC SQL
012900011205     C+ CLOSE A1
013000011205     C/END-EXEC
013100011205     c*
013200040820     C* totali generali
013300041007     c                   if        almeno_uno ='S'
013400040823     c                   if        *in66
013500040823      *                 * ------------------ *
013600120105     c************       write     testa
013700120105     c************       write     tesdetta
013800040823      *                 * ------------------ *
013900040823     c                   setoff                                       66
014000040823     c                   end
014100120105     c************       eval      totnrb =  GenBolle
014200120105     c************       eval      totnev =  GenEventi
014300120105     c************       write     totale
014400041007     c                   end
014500040823      *
014600041007     c     fine          tag
014700020326     C                   SETON                                        LR
014800040820     C**************************************************************************
014900040820     C* INIZIALIZZAZIONE
015000040820     C**************************************************************************
015100040820     C     *inzsr        BEGSR
015200040820      **
015300040820     C     *ENTRY        PLIST
015400040820     C                   PARM                    KPJBA
015500111115     c                   movel     kpjbu         tise89ds
015600041103      *
015700041103     C                   Z-ADD     1             CODUT
015800041103     C                   CALL      'X§PARUT'
015900041103     C                   PARM                    UT§DSE
016000041103     C                   MOVEL     REC80         CNCR80
016100041103     C                   MOVEL     RAGUT         RSUT             20
016200041103     C*
016300040820      *
016400040820      *  imposta UDATE di elaborazione
016500040820     C                   move      *date         G02DAT
016600040820     C                   MOVE      *ZEROS        G02INV
016700040820     C                   MOVE      *BLANKS       G02ERR
016800040820     C                   CALL      'XSRDA8'
016900040820     C                   PARM                    WLBDAT
017000040820     C                   Z-ADD     G02INV        data_oggi
017100040820     C                   move      data_oggi     day
017200040820     C                   TIME                    W0140            14 0
017300040820     C                   MOVEl     W0140         ore
017400040823      **
017500111115     c                   move      e89dti        dadata
017600111115     c                   move      e89dtf        adata
017700111115      **
017800111115     c                   movel     e89dti        annoI             4 0
017900111115     c                   move      e89dti        mesgioI           4 0
018000111115      **
018100111115     c                   movel     e89dtf        annoF             4 0
018200111115     c                   move      e89dtf        mesgioF           4 0
018300040823      **
018400040823     c                   seton                                        66
018500111115      **
018600040825     c                   clear                   prima_volta       1
018700040826     c                   clear                   almeno_uno
018800040820     c*
018900040820     c* compongo istruzione sql
019000040820     c                   exsr      istruz
019100040820     c*
019200040820     C                   ENDSR
019300040820     C**************************************************************************
019400040820     C* mi compongo l'istruzione sql
019500040820     C**************************************************************************
019600040820     C     istruz        BEGSR                                                  *
019700040820      **
019800111115     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file TITAS
019900111115      **   suddividendo per ogni giorno del periodo
020000111115      **   il numero delle Bolle EXPORT x LINEA ARRIVO inviate al Partner
020100111115     C                   EVAL      WrkSqlCmd =
020200111115     C                             'select tasAAS, tasMGS, tasLNA, '  +
020300111115     C                             ' count(*) from ('                 +
020400111128      *----
020500111115     c                             ' select TITAS00F.* from titas00f '+
020600111116     c                             ' WHERE '                          +
020700111128     c                             ' taslna between 300 and 400 '
020800111128      ****
020900111128      **  Range di Periodo nello stesso Anno oppure su anni contigui
021000111128     c                   if        AnnoI = AnnoF
021100111128      ****
021200111128     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)      +
021300111128     C                             ' and tasAAS = '                   +
021400111128     C                             %editc(AnnoI:'X')                  +
021500111128     c                             ' AND '                            +
021600111128     C                             ' tasMGS between '                 +
021700111128     C                             %editc(mesgioI:'X')                +
021800111128     c                             ' and '                            +
021900111128     C                             %editc(mesgioF:'X')
022000111128     c                   eLSe
022100111128      ****
022200111128      *  Se gli anni sono differenti
022300111128     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)      +
022400111128     C                             ' and ( ( '                        +
022500111128     C                             ' tasAAS = '                       +
022600111128     C                             %editc(AnnoI:'X')                  +
022700111128     c                             ' AND '                            +
022800111128     C                             ' tasMGS >= '                      +
022900111128     C                             %editc(mesgioI:'X')                +
023000111128     c                             ' ) OR ( '                         +
023100111128     C                             ' tasAAS = '                       +
023200111128     C                             %editc(AnnoF:'X')                  +
023300111128     c                             ' AND '                            +
023400111128     C                             ' tasMGS <= '                      +
023500111128     C                             %editc(mesgioF:'X')                +
023600111128     c                             ' ) ) '
023700111128     c                   end
023800111128      *----
023900111128     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)      +
024000111115     c                             ' UNION '                          +
024100111115     c                             ' select TITAS10F.* from titas10f '+
024200111116     c                             ' WHERE '                          +
024300111128     c                             ' taslna between 300 and 400 '
024400111128      ****
024500111128      **  Range di Periodo nello stesso Anno oppure su anni contigui
024600111128     c                   if        AnnoI = AnnoF
024700111128      ****
024800111128     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)      +
024900111128     C                             ' and tasAAS = '                   +
025000111128     C                             %editc(AnnoI:'X')                  +
025100111128     c                             ' AND '                            +
025200111128     C                             ' tasMGS between '                 +
025300111128     C                             %editc(mesgioI:'X')                +
025400111128     c                             ' and '                            +
025500111128     C                             %editc(mesgioF:'X')
025600111128     c                   eLSe
025700111128      ****
025800111128      *  Se gli anni sono differenti
025900111128     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)      +
026000111128     C                             ' and ( ( '                        +
026100111128     C                             ' tasAAS = '                       +
026200111128     C                             %editc(AnnoI:'X')                  +
026300111128     c                             ' AND '                            +
026400111128     C                             ' tasMGS >= '                      +
026500111128     C                             %editc(mesgioI:'X')                +
026600111128     c                             ' ) OR ( '                         +
026700111128     C                             ' tasAAS = '                       +
026800111128     C                             %editc(AnnoF:'X')                  +
026900111128     c                             ' AND '                            +
027000111128     C                             ' tasMGS <= '                      +
027100111128     C                             %editc(mesgioF:'X')                +
027200111128     c                             ' ) ) '
027300111128     c                   end
027400111115      ****
027500111115      **  Ordinamento di lettura  x ANNO/MESEGIORNO/LINEA ARRIVO
027600111115      ****
027700111115     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)       +
027800111128     c                             ' ) as Bolle '                      +
027900111115     c                             ' GROUP BY tasAAS, tasMGS, tasLNA ' +
028000111115     c                             ' ORDER BY tasAAS, tasMGS, tasLNA '
028100040820      **
028200040820     C                   ENDSR
028300020326     C**************************************************************************
028400040820      *
028500040820     C**************************************************************************
028600040820     C     testata       begSR
028700040820      **
028800040820      *                 * ------------------ *
028900040820     c                   write     testa
029000040820      *                 * ------------------ *
029100040820      *
029200040820     C                   ENDSR
029300040825     C**************************************************************************
029400040825      *
029500040825     C**************************************************************************
029600040825     C     TOTals        begSR
029700040826      *
029800040825     c* ROTTURA Fattura
029900111115     c                   if        Anno <> sav_Anno or
030000111115     C                             MeseGiorno <> sav_MeseGIORNO OR
030100111115     C                             SqlCod = 100
030200040831     c                   if        almeno_uno ='S'
030300111115     c                   exsr      TES_GIORNO
030400040831     c                   end
030500111115     c                   eval      sav_Anno = Anno
030600111115     c                   eval      sav_MeseGIORNO = MeseGIORNO
030700040826     c                   end
030800040825      *
030900040825     C                   ENDSR
031000040820     C**************************************************************************
031100111115      *
031200111115     C**************************************************************************
031300111115     C     TES_GIORNO    begSR
031400040823      *
031500040823     C* totale
031600040825     c                   if        prima_volta ='N'
031700040831      *
031800040823     c                   if        *in66
031900040823      *                 * ------------------ *
032000120105     c************       write     testa
032100120105     c************       write     tesdetta
032200040823      *                 * ------------------ *
032300040823     c                   setoff                                       66
032400040823     c                   end
032500111115      *
032600120105     c************       eval      totnrb =  TotBolle
032700120105     c************       eval      totnev =  TotEventi
032800120105     c************       write     totale
032900040823     c                   end
033000040825      *
033100111115     c                   clear                   totbolle
033200111115     c                   clear                   toteventi
033300040820      *
033400040820     C                   ENDSR
033500111115     C**************************************************************************
033600111115      *   sulla giornata x LNA calcola i relativi eventi di ATR ESEGUITI
033700111115     C**************************************************************************
033800111115     C     cal_Eventi    begSR
033900111115      **
034000111115     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file degli EVENTI
034100111115      **  sulla base dell BOLLE con LINEA estera
034200111115      **  etratte sulla giornata specifica.
034300111115     C                   EVAL      WrkEVENTI =
034400111115     C                             'select count(*) from fnevb00f  '  +
034500111115     C                             ' where evbcev like ''EE%'' and '  +
034600111115     c                             ' digits(evbaas) concat '          +
034700111115     c                             ' digits(evblnp) concat '          +
034800111115     c                             ' digits(evbnrs) concat '          +
034900111115     c                             ' digits(evbnsp)   IN ( '          +
035000111115     c                             ' SELECT '                         +
035100111115     c                             ' digits(tasaas) concat '          +
035200111115     c                             ' digits(taslnp) concat '          +
035300111115     c                             ' digits(tasnrs) concat '          +
035400111115     c                             ' digits(tasnsp)  FROM ( '         +
035500111115     c                             ' select TITAS00F.* from titas00f '+
035600111116     c                             ' WHERE '                          +
035700111116     c                             ' taslna = '                       +
035800111116     C                             %editc(LineaArrivo:'X')            +
035900111116     c                             ' AND '                            +
036000111116     c                             ' tasAAS = '                       +
036100111116     C                             %editc(Anno:'X')                   +
036200111116     c                             ' AND '                            +
036300111116     c                             ' tasMGS = '                       +
036400111116     C                             %editc(MeseGiorno:'X')             +
036500111128      *--
036600111115     c                             ' UNION '                          +
036700111115     c                             ' select TITAS10F.* from titas10f '+
036800111116     c                             ' WHERE '                          +
036900111116     c                             ' taslna = '                       +
037000111116     C                             %editc(LineaArrivo:'X')            +
037100111116     c                             ' AND '                            +
037200111116     c                             ' tasAAS = '                       +
037300111116     C                             %editc(Anno:'X')                   +
037400111116     c                             ' AND '                            +
037500111116     c                             ' tasMGS = '                       +
037600111116     C                             %editc(MeseGiorno:'X')             +
037700111116     c                             ' ) as Bolle '                     +
037800111115     c                             ' )   '
037900111115      **
038000111115      *
038100111115     c                   clear                   NrEventi
038200111115      *
038300111115     C/EXEC SQL
038400111115     C+ PREPARE S2 FROM :WrkEVENTI
038500111115     C/END-EXEC
038600111115      *
038700111115     C/EXEC SQL
038800111115     C+ DECLARE A2 CURSOR FOR S2
038900111115     C/END-EXEC
039000111115      *
039100111115     C/EXEC SQL
039200111115     C+ OPEN A2
039300111115     C/END-EXEC
039400111115      *
039500111115     C                   IF        SqlCod >= 0
039600111115     C/EXEC SQL
039700111115     C+ FETCH NEXT FROM A2 INTO  :NrEventi
039800111115     C/END-EXEC
039900111115     C                   SELECT
040000111115     c*
040100111115     c* a fine file Totali x rotture
040200111115     C                   WHEN      SqlCod = 100
040300111115     c                   clear                   NrEventi
040400111115     **
040500111115     C                   WHEN      SqlCod < 0
040600111115     C                   seton                                        H2
040700111115     c                   goto      fine
040800111115     c*
040900111115     C                   OTHER
041000111115      *   se tutto Bene
041100111115     c*    il contatore degli Eventi contiene un valore
041200111115     C                   ENDSL
041300111115
041400111115     C                   END
041500111115     C/EXEC SQL
041600111115     C+ CLOSE A2
041700111115     C/END-EXEC
041800111115      *
041900111115     C                   ENDSR
042000111115     C**************************************************************************
042100111115      *   Scrive il record nel file di WRK WFATR89F
042200111115     C**************************************************************************
042300111115     C     wri_Record    begSR
042400111115      *
042500111115     C* scrive il record sul file di work da elaborare
042600111115     c                   clear                   wfATR89
042700111115     c                   movel     Anno          F89Gio
042800111115     c                   move      MeseGiorno    F89Gio
042900111115     c                   eval       F89DAL  =  e89dti
043000111115     c                   eval       F89AL   =  e89dtf
043100111115     c                   eval       F89LNA  = LineaArrivo
043200111115     c                   eval       F89NSP  = NrBolle
043300111115     c                   eval       F89ATR  = NrEventi
043400111115     c                   eval       F89NON  = NrBolle - NrEventi
043500120105     c                   if        F89NON < 0
043600120105     c                   z-add     0             F89NON
043700120105     c                   end
043800111115     c                   if        NrBolle > 0
043900111116     c                   eval       F89PRC  = 100 - (NrEventi/NrBolle * 100)
044000111115     c                   end
044100120105     c                   if        F89PRC < 0
044200120105     c                   z-add     0             F89PRC
044300120105     c                   end
044400111115     c                   write     wfATR89
044500111115      *
044600111115     C                   ENDSR
044700040820     C**************************************************************************
