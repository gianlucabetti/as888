000100150429     hDECEDIT('0,') DATEDIT(*YMD.)
000200000000      *****************************************************************
000300030904      *       prompt di controllo dati ricevuti dal partner
000400000000      *****************************************************************
000500130717     Ftnefr00F  iF   E             disk
000600040906     Ffieu69D   CF   E             WORKSTN
000700030116      *****************************************************************
000800030116     d KPJBA         E DS
000900030120     D Tibs36ds      E DS
001000000000     D CNCR80        E DS
001100000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001200040906     D fieu69ds      e DS                  inz
001300940926     D*
001400000000     D WLBDAT          DS
001500940926     D  G02DAT                 1      8  0
001600940926     D  G02INV                 9     16  0
001700940926     D  G02ERR                17     17
001800940926     D  G02TGI                18     22  0
001900030205      *
002000030116     D digits          C                   CONST('0123456789')
002100030116      *****************************************************************
002200000000     C     *ENTRY        PLIST
002300000000     C                   PARM                    KPJBA
002400030120      *
002500030825     C* Imposta la UDATE  da TIME
002600030825     C                   TIME                    W0140            14 0
002700030825     C                   MOVE      W0140         UDATE8            8 0
002800150429     C                   MOVE      *date         Oggi              8 0
002900030825     C*
003000030120      *  In base all'utente prende il P.O.
003100030120     C                   CLEAR                   Tibs36Ds
003200030120     c                   EVAL      I36ute = Knmus
003300030120     c                   EVAL      I36Tla = 'L'
003400030120     C                   CALL      'TIBS36R'
003500030120     C                   PARM                    Tibs36Ds
003600030827     c                   movel     KNMUS         ALFA3             3
003700030829     c                   setoff                                       0102
003800030829      * solo per EDP....:
003900030829     c                   if        ALFA3 ='EDP'
004000030829     c                   seton                                        01
004100030829     c                   end
004200030829      * per utenti di filiale:
004300030829     c                   if        ALFA3<>'EDP' and O36Pou <>46
004400030829     c                   seton                                        02
004500030829     c                   end
004600000000      *--------------------------------------------------------------*
004700000000      *             M A I N      L I N E
004800000000      *--------------------------------------------------------------*
004900000000     C                   Z-ADD     1             CODUT
005000000000     C                   CALL      'X§PARUT'
005100000000     C                   PARM                    UT§DSE
005200000000     C                   MOVEL     RAGUT         RSUT             20
005300000000     C                   MOVEL     RAGUT         RSUT2            20
005400000000     C                   MOVEL     REC80         CNCR80
005500030825      *
005600030116     C                   Z-ADD     *ZERO         D1DDAL                         DATA INIZIALE
005700030116     C                   Z-ADD     *ZERO         D1DAL                          DATA FINALE
005800150423     c                   move      'DPD'         D1NTW
005900030825      *
006000030915     C     INIZIO        tag
006100030915      *
006200000000      * Emetto il formato video
006300030116     C     Video1        TAG
006400020308     C                   SETOFF                                       90
006500040906     C                   EXFMT     eu69d01
006600000000      *
006700000000     C   KC              GOTO      FINE                                         F3=USCITA
006800000000      *
006900000000      * Eseguo i  controlli sui campi immessi a video
007000000000     C                   EXSR      CONTR
007100000000      *
007200020308      * Se non vi sono errori ma non e' premuto F6 emetto video
007300030915     C   90
007400150423     Cornkf              GOTO      Video1
007500030905      *
007600040906     c                   clear                   fieu69ds
007700030905      * compone il codice numerico
007800030905     c                   z-add     DalAmg        e80dti
007900030905     c                   z-add     AlAmg         e80dtf
008000030905     c                   move      d1ntw         e80ntw
008100150423      * Finestra di attesa x elaborazione dati
008200150423     c                   write     eu69w00
008300150423      *
008400030915      * Visualizza totali generali fra date fatture
008500030904     c                   exsr      conta
008600030904      *
008700030904     c                   dow       *inkl = *off
008800040906     C                   EXFMT     eu69w01
008900030904     C   KC              GOTO      FINE                                         F3=USCITA
009000030904     c                   enddo
009100030904      *
009200000000      *
009300030116     C                   GOTO      INIZIO
009400030116      *
009500000000     C     FINE          TAG
009600000000     C                   SETON                                        LR
009700000000      *--------------------------------------------------------------*
009800000000      * Routine per i controlli sui campi del formato video          *
009900000000      *--------------------------------------------------------------*
010000000000     C     CONTR         BEGSR
010100000000      *
010200030116     c                   movea     '0000000000'  *in(41)
010300030116      * --------------
010400020624      * controllo immissione delle date
010500030116     C     d1DDAL        ifeq      0
010600020308     C                   SETON                                        4390
010700030116     C                   GOTO      endct1
010800030116     C                   Else
010900030116     C                   Z-ADD     d1DDAL        G02DAT
011000000000     C                   MOVEL     *BLANK        G02ERR
011100940926     C                   CALL      'XSRDA8'
011200000000     C                   PARM                    WLBDAT
011300000000     C     G02ERR        IFEQ      '1'
011400020308     C                   SETON                                        44  90
011500030116     C                   GOTO      endct1
011600000000     C                   ENDIF
011700030116     C                   Z-ADD     G02INV        DalAmg            8 0
011800030116     C                   Z-ADD     G02dat        D1ddal
011900020307     c                   endif
012000030825      *
012100020624      * Controllo data finale
012200030116     C     d1Dal         ifeq      0
012300030127     C                   Z-ADD     d1DDal        d1Dal
012400030127     C                   End
012500030825      *
012600030116     C                   Z-ADD     d1Dal         G02DAT
012700020624     C                   MOVEL     *BLANK        G02ERR
012800020624     C                   CALL      'XSRDA8'
012900020624     C                   PARM                    WLBDAT
013000020624     C     G02ERR        IFEQ      '1'
013100020624     C                   SETON                                        42  90
013200030116     C                   GOTO      endct1
013300020624     C                   ENDIF
013400030825     c*
013500030116     C                   Z-ADD     G02INV        AlAmG             8 0
013600030116     C                   Z-ADD     G02dat        d1dal
013700030127     c*
013800030116     C     DalAmg        ifgt      AlAmg
013900020624     C                   SETON                                        4590
014000030116     C                   GOTO      endct1
014100020624     C                   END
014200030129      *
014300030116     C     endct1        ENDSR
014400030116      *--------------------------------------------------------------*
014500030904      * conta
014600030904      *--------------------------------------------------------------*
014700030904     C     Conta         BEGSR
014800030904      *
014900030904     c                   z-add     0             cnt               7 0
015000030904     c                   z-add     0             imp              11 3
015100030904     C/EXEC SQL
015200150527     C+ Declare DEB Cursor for
015300030904     c+ SELECT count(*), sum(Importo) from (SELECT EFRNTW, EFRRIF,
015400030904     c+ count(*), sum(EFRIMP) as Importo
015500130717     c+ FROM tnefr00f WHERE EFRNTW = :d1ntw
015600030904     c+ and EFRDFT between :dalAMG and :alAMG
015700150527     c+ and substr(EFRCAU, 1, 1) = 'N'  and efrimp>=0
015800030904     c+ GROUP BY EFRNTW, EFRRIF) as a
015900030904     C/END-EXEC
016000030904      *          apertura cursore
016100030904     C/EXEC SQL
016200150527     C+ OPEN DEB
016300030904     C/END-EXEC
016400030904     C                   if        SqlCod <> 100 OR SqlCod = 0
016500030904      *          lettura cursore
016600030904     C/EXEC SQL
016700150527     C+ Fetch Next From DEB Into :cnt, :imp
016800030904     C/END-EXEC
016900150527     C                   z-add     cnt           w1totd
017000150527     C                   z-add     imp           w1timd
017100030904     C                   End
017200030904     C/EXEC SQL
017300150527     C+ Close DEB
017400030904     C/END-EXEC
017500030915     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
017600150527      *
017700150527     c                   z-add     0             cnt               7 0
017800150527     c                   z-add     0             imp              11 3
017900150527     C/EXEC SQL
018000150527     C+ Declare CRE Cursor for
018100150527     c+ SELECT count(*), sum(Importo) from (SELECT EFRNTW, EFRRIF,
018200150527     c+ count(*), sum(EFRIMP) as Importo
018300150527     c+ FROM tnefr00f WHERE EFRNTW = :d1ntw
018400150527     c+ and EFRDFT between :dalAMG and :alAMG
018500150527     c+ and substr(EFRCAU, 1, 1) = 'N'  and efrimp<0
018600150527     c+ GROUP BY EFRNTW, EFRRIF) as a
018700150527     C/END-EXEC
018800150527      *          apertura cursore
018900150527     C/EXEC SQL
019000150527     C+ OPEN CRE
019100150527     C/END-EXEC
019200150527     C                   if        SqlCod <> 100 OR SqlCod = 0
019300150527      *          lettura cursore
019400150527     C/EXEC SQL
019500150527     C+ Fetch Next From CRE Into :cnt, :imp
019600150527     C/END-EXEC
019700150527     C                   z-add     cnt           w1totc
019800150527     C                   z-add     imp           w1timc
019900150527     C                   End
020000150527     C/EXEC SQL
020100150527     C+ Close CRE
020200150527     C/END-EXEC
020300150527     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
020400150527      *
020500150527     c                   z-add     0             cnt               7 0
020600150527     c                   z-add     0             imp              11 3
020700150527     C/EXEC SQL
020800150527     C+ Declare VTG Cursor for
020900150527     c+ SELECT count(*), sum(Importo) from (SELECT EFRNTW, EFRRIF,
021000150527     c+ count(*), sum(EFRIMP) as Importo
021100150527     c+ FROM tnefr00f WHERE EFRNTW = :d1ntw
021200150527     c+ and EFRDFT between :dalAMG and :alAMG
021300150527     c+ and substr(EFRCAU, 1, 1) <>'N'
021400150527     c+ GROUP BY EFRNTW, EFRRIF) as a
021500150527     C/END-EXEC
021600150527      *          apertura cursore
021700150527     C/EXEC SQL
021800150527     C+ OPEN VTG
021900150527     C/END-EXEC
022000150527     C                   if        SqlCod <> 100 OR SqlCod = 0
022100150527      *          lettura cursore
022200150527     C/EXEC SQL
022300150527     C+ Fetch Next From VTG Into :cnt, :imp
022400150527     C/END-EXEC
022500150527     C                   z-add     cnt           w1totv
022600150527     C                   z-add     imp           w1timv
022700150527     C                   End
022800150527     C/EXEC SQL
022900150527     C+ Close VTG
023000150527     C/END-EXEC
023100150527      *
023200150527     c                   goto      fine_calcoli
023300150527      *
023400150527     C*===============================================================
023500150527      *  vecchi calcoli lasciati lì.....non si sa mai
023600150527     C*===============================================================
023700150423     c                   z-add     0             cntv              7 0
023800150423     c                   z-add     0             impv             11 3
023900150423     C/EXEC SQL
024000150423     C+ Declare V1 Cursor for
024100150423     c+ SELECT count(*), sum(Importo) from (SELECT EFRNTW, EFRRIF,
024200150423     c+ count(*), sum(EFRIMP) as Importo
024300150423     c+ FROM tnefr00f WHERE EFRNTW = :d1ntw
024400150423     c+ and EFRDFT between :dalAMG and :alAMG
024500150429     c+ and substr(efrrif, 1, 4) in (select cldDEP from tnCLD00F where
024600150429     c+ cldCOD = 'VTG'     and :oggi between CLDDDA and CLDADA)
024700150423     c+ GROUP BY EFRNTW, EFRRIF) as a
024800150423     C/END-EXEC
024900150423      *          apertura cursore
025000150423     C/EXEC SQL
025100150423     C+ OPEN V1
025200150423     C/END-EXEC
025300150423     C                   if        SqlCod <> 100 OR SqlCod = 0
025400150423      *          lettura cursore
025500150423     C/EXEC SQL
025600150423     C+ Fetch Next From V1 Into :cntv, :impv
025700150423     C/END-EXEC
025800150423     C                   z-add     cntv          w1totv
025900150423     C                   z-add     impv          w1timv
026000150423     C                   End
026100150423     C/EXEC SQL
026200150423     C+ Close V1
026300150423     C/END-EXEC
026400150515      *---
026500150515      *---   Reperisco anche eventuali ORM VTG da includere
026600150515      *---       SOLO LA PARTE a CREDITO  quindi < 0
026700150515     C/EXEC SQL
026800150515     C+ Declare O1 Cursor for
026900150515     c+ SELECT count(*), sum(Importo) from (SELECT EFRNTW, EFRRIF,
027000150515     c+ count(*), sum(EFRIMP) as Importo
027100150515     c+ FROM tnefr00f WHERE EFRNTW = :d1ntw
027200150515     c+ and EFRDFT between :dalAMG and :alAMG  and efrimp< 0
027300150515     c+   and efrrif in (
027400150515     c+   select dponrp from fidpo00f where dpodep in
027500150515     c+ (select cldDEP from tnCLD00F where
027600150515     c+ cldCOD = 'VTG'     and :oggi between CLDDDA and CLDADA) )
027700150515     c+ GROUP BY EFRNTW, EFRRIF) as a
027800150515     C/END-EXEC
027900150515      *          apertura cursore
028000150515     C/EXEC SQL
028100150515     C+ OPEN O1
028200150515     C/END-EXEC
028300150515     C                   if        SqlCod <> 100 OR SqlCod = 0
028400150515      *          lettura cursore
028500150515     C/EXEC SQL
028600150515     C+ Fetch Next From O1 Into :cntv, :impv
028700150515     C/END-EXEC
028800150515     C                   add       cntv          w1totv
028900150515     C                   add       impv          w1timv
029000150515     C                   End
029100150515     C/EXEC SQL
029200150515     C+ Close O1
029300150515     C/END-EXEC
029400150423     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
029500150515      *   TUTTO il DEPOT 0124 da cui arriva anche DUNFOSS
029600150515     c                   z-add     0             cnt0124           7 0
029700150515     c                   z-add     0             imp0124          11 3
029800150423     C/EXEC SQL
029900150515     C+ Declare D0124 Cursor for
030000150423     c+ SELECT count(*), sum(Importo) from (SELECT EFRNTW, EFRRIF,
030100150423     c+ count(*), sum(EFRIMP) as Importo
030200150423     c+ FROM tnefr00f WHERE EFRNTW = :d1ntw
030300150423     c+ and EFRDFT between :dalAMG and :alAMG
030400150515     c+ and substr(efrrif, 1, 4) = '0124'
030500150423     c+ GROUP BY EFRNTW, EFRRIF) as a
030600150423     C/END-EXEC
030700150423      *          apertura cursore
030800150423     C/EXEC SQL
030900150515     C+ OPEN D0124
031000150423     C/END-EXEC
031100150423     C                   if        SqlCod <> 100 OR SqlCod = 0
031200150423      *          lettura cursore
031300150423     C/EXEC SQL
031400150515     C+ Fetch Next From D0124 Into :cnt0124, :imp0124
031500150423     C/END-EXEC
031600150515     C                   z-add     cnt0124       w1tot0124
031700150515     C                   z-add     imp0124       w1tim0124
031800150423     C                   End
031900150423     C/EXEC SQL
032000150515     C+ Close D0124
032100150423     C/END-EXEC
032200150515     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
032300150515      *
032400150515     c                   z-add     0             cntd              7 0
032500150515     c                   z-add     0             impd             11 3
032600150519     *****C/EXEC SQL
032700150519     *****C+ Declare D1 Cursor for
032800150519     *****c+ SELECT count(*), sum(Importo) from (SELECT EFRNTW, EFRRIF,
032900150519     *****c+ count(*), sum(EFRIMP) as Importo
033000150519     *****c+ FROM tnefr00f WHERE EFRNTW = :d1ntw
033100150519     *****c+ and EFRDFT between :dalAMG and :alAMG
033200150519     *****c+ and substr(efrrif, 1, 4) in (select cldDEP from tnCLD00F where
033300150519     *****c+ cldCOD = 'DANFOSS' and :oggi between CLDDDA and CLDADA)
033400150519     *****c+ and efrrif in (select PARCEL from wfdanf0f)
033500150519     *****c+ GROUP BY EFRNTW, EFRRIF) as a
033600150519     *****C/END-EXEC
033700150519      ***           solo pacchi grandi
033800150519     C/EXEC SQL
033900150519     c+ SELECT count(*)  into :cntd from WFDANF0F
034000150519     c+   where DATA_FAT between :dalAMG and :alAMG and SERVICECOD ='101'
034100150519     C/END-EXEC
034200150515     C                   if        SqlCod <> 100 OR SqlCod = 0
034300150515      *          lettura cursore
034400150519     C                   z-add     cntd          w1totdg
034500150519     C     0,78          mult      cntd          w1timdg
034600150515     C                   End
034700150519      *---------------------------------------------------------------------
034800150519     c                   z-add     0             cntd              7 0
034900150519     c                   z-add     0             impd             11 3
035000150519      *
035100150519      ***           solo pacchi piccoli
035200150519     C/EXEC SQL
035300150519     c+ SELECT count(*)  into :cntd from WFDANF0F
035400150519     c+   where DATA_FAT between :dalAMG and :alAMG and SERVICECOD ='136'
035500150519     C/END-EXEC
035600150519     C                   if        SqlCod <> 100 OR SqlCod = 0
035700150519      *          lettura cursore
035800150519     C                   z-add     cntd          w1totdp
035900150519     C     0,22          mult      cntd          w1timdp
036000150519     C                   End
036100150519      *
036200150519      *---------------------------------------------------------------------
036300150519      *
036400150519     c                   z-add     0             cntd              7 0
036500150519     c                   z-add     0             impd             11 3
036600150519      *
036700150519      ***           solo pacchi piccoli
036800150519     C/EXEC SQL
036900150519     c+ SELECT count(*)  into :cntd from WFDANF0F
037000150519     c+   where DATA_00001 between :dalAMG and :alAMG
037100150519     c+     and DATA_FAT = 0  and SERVICECOD ='101'
037200150519     C/END-EXEC
037300150519     C                   if        SqlCod <> 100 OR SqlCod = 0
037400150519      *          lettura cursore
037500150519     C                   z-add     cntd          w1totdmg
037600150519     C     0,78          mult      cntd          w1timdmg
037700150519     C                   End
037800150519      *
037900150519     c                   z-add     0             cntd              7 0
038000150519     c                   z-add     0             impd             11 3
038100150519      *
038200150519      ***           solo pacchi piccoli
038300150519     C/EXEC SQL
038400150519     c+ SELECT count(*)  into :cntd from WFDANF0F
038500150519     c+   where DATA_00001 between :dalAMG and :alAMG
038600150519     c+     and DATA_FAT = 0  and SERVICECOD ='136'
038700150519     C/END-EXEC
038800150519     C                   if        SqlCod <> 100 OR SqlCod = 0
038900150519      *          lettura cursore
039000150519     C                   z-add     cntd          w1totdmp
039100150519     C     0,22          mult      cntd          w1timdmp
039200150519     C                   End
039300150423     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
039400150515     c*****              eval      w1tot1 = w1tot-w1totv-w1totd
039500150515     c*****              eval      w1tim1 = w1tim-w1timv-w1timd
039600150423     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
039700030904     C/EXEC SQL
039800030904     C+ Declare S1 Cursor for
039900040906     c+ SELECT count(*), sum(Importo) from (SELECT EfrNTW, EfrRIF,
040000040906     c+ count(*), sum(EfrIMP) as Importo
040100130717     c+ from tnefr00f WHERE EfrNTW = :d1ntw
040200040906     c+ and EfrDFT between :dalAMG and :alAMG
040300040906     c+ and EfrCAE in ('NTR', 'NFT')
040400040906     c+ GROUP BY EfrNTW, EfrRIF) as a
040500030904     C/END-EXEC
040600030904      *          apertura cursore
040700030904     C/EXEC SQL
040800030904     C+ OPEN S1
040900030904     C/END-EXEC
041000030904     C                   if        SqlCod <> 100 OR SqlCod = 0
041100030904      *          lettura cursore
041200030904     C/EXEC SQL
041300030904     C+ Fetch Next From S1 Into :cnt, :imp
041400030904     C/END-EXEC
041500030904     C                   z-add     cnt           w1sca
041600030904     C                   z-add     imp           w1sim
041700030904     C                   End
041800030904     C/EXEC SQL
041900030904     C+ Close S1
042000030904     C/END-EXEC
042100030904      *
042200150527     c***  w1tot         sub       w1sca         w1abb
042300150527     c***  w1tim         sub       w1sim         w1aim
042400150527      *-
042500150527     C     fine_calcoli  ENDSR
042600030904      *--------------------------------------------------------------*
