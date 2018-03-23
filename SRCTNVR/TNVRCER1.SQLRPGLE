000500000920     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900050428     Fecect01l  iF   E           K DISK
001200050428     Fqsysprt   o    f  132        printer oflind(*inof)
001201050428     D* SCHIERA NORMALIZZAZIONE VOCE PROJ
001202050428     D S4              S              1    DIM(4)
001203050428     D S4B             S              1    DIM(4)
004100030808     D*------------------
004200030808     D* DS X PASSAGGIO PARAMETRI
004300030808     D*------------------
004301050428     d kpjba         e DS
004400050428     d PARAM           DS
005301050428     D  PERIODOCE                     6s 0
005302050428     D  dettaglio                     1
005800011206     D*-------------------
005900011206     D* VARIABILI DI WRK
006000011206     D*-------------------
006100011206     D  SQLline        s           5000    varying
006101050428     D  wdatada        s              8s 0
006102050428     D  wdataa         s              8s 0
006103050428     D  wcdp           s              8s 0
006104050503     D  wdrg           s              8s 0
006105050428     D  nbc            s                   like(ectnbc)
006106050428     D  voce           s              4  0
006107050502     D  voces          s              4  0
006108050502     D  sottovoce      s              7  0
006109050428     D  imp            s             19  3
006110050428     D  totimp         s             19  3
006111050428     D  kcc            s              8
006800970526     C*------------------------------------------------------------------------*
006900991214     C* MAIN LINES
007000970526     C*------------------------------------------------------------------------*
011600011206     C*
012100050428     C                   EVAL      wDatada = periodoce*100+01
012200050428     C                   EVAL      wDataa  = periodoce*100+99
012201050428     C                   EVAL      wcdp  = periodoce*100+00
012202050503     C                   EVAL      wdrg  = periodoce*100+01
012203050429     c* ricavi
012204050429     c                   exsr      srric
012205050429     c* competenze
012206050429     c                   exsr      srcomp
012207050429     C*
012208050429     C                   SETON                                        LR
012209050429     C*------------------------------------------------------------------------*
012210050429     C* srric  - Routine x ricavi
012211050429     C*------------------------------------------------------------------------*
012212050429     C     srric         BEGSR
012213050428     c                   clear                   totimp
012500030808     C*
012600030808     C/EXEC SQL
012700050502     C+ DECLARE C1 CURSOR FOR SELECT ECECD00F.ECDKCC, ECECD00F.ECDKSC
012800050502     C+ FROM ecect00f join ececd00f on ectcmp = ecdcmp and ectnbc =
012900050502     C+ ecdnbc WHERE EcTDDC <= :wdatada and ECTDSC>= :wdataa and
013000050502     C+ ECTMCE='S' and ECTFCR = 'R' and ectnbc =:nbc and ecdddo<=
013100050502     C+ :wdatada and ecddso >=:wdataa and ECECD00F.ECDSEG = 'A' GROUP BY
013101050502     C+ ECECD00F.ECDKCC, ECECD00F.ECDKSC
014000030808     C/END-EXEC
014100030808     C
014200030808     C/EXEC SQL
014300030808     C+ open C1
014400030808     C/END-EXEC
014500030808     C
014501050428     C                   do        *hival
014600030808     C/EXEC SQL
014700050502     C+ Fetch C1 into :voce, :sottovoce
014800030808     C/END-EXEC
014900030808     C*
014901050428     C                   select
014902050428     C                   when      sqlcod = 100
014903050428     c                   leave
014904050428     C                   when      sqlcod = 0
014906050428     C                   exsr      calcola
014907050428     C                   other
014908050428     c                   leave
014909050428     C                   endsl
015900030808     C*
016000030808     C                   enddo
016100030808     C*
016200030808     C/EXEC SQL
016300030808     C+ close C1
016400030808     C/END-EXEC
016401050428     c                   seton                                        02
016402050428     c                   exsr      stampa
016403050429     c                   endsr
016404050428     C*
016405050429     C*------------------------------------------------------------------------*
016406050429     C* srcomp - Routine x competenze
016407050429     C*------------------------------------------------------------------------*
016408050429     C     srcomp        BEGSR
016409050429     c                   clear                   totimp
016410050502     c                   clear                   voces
016411050502     c                   clear                   voce
016412050429     C*
016413050429     C/EXEC SQL
016414050502     C+ DECLARE A1 CURSOR FOR SELECT ECECD00F.ECDKCC, ECECD00F.ECDKSC
016415050502     C+ FROM ecect00f join ececd00f on ectcmp = ecdcmp and ectnbc =
016416050502     C+ ecdnbc WHERE EcTDDC <= :wdatada and ECTDSC>= :wdataa and
016417050502     C+ ECTMCE='S' and ECTFCR = 'C' and ectnbc =:nbc and ecdddo<=
016418050502     C+ :wdatada and ecddso >=:wdataa and ECECD00F.ECDSEG = 'A' and
016419050502     C+ ECECT00F.ECTCMP not in (11, 12, 205, 204, 53, 206, 54, 207, 55,
016420050502     C+ 208) GROUP BY ECECD00F.ECDKCC, ECECD00F.ECDKSC
016421050429     C/END-EXEC
016422050429     C
016423050429     C/EXEC SQL
016424050429     C+ open a1
016425050429     C/END-EXEC
016426050429     C
016427050429     C                   do        *hival
016428050429     C/EXEC SQL
016429050502     C+ Fetch a1 into :voce, :sottovoce
016430050429     C/END-EXEC
016431050429     C*
016432050429     C                   select
016433050429     C                   when      sqlcod = 100
016434050429     c                   leave
016435050429     C                   when      sqlcod = 0
016436050429     C                   exsr      calcolac
016437050429     C                   other
016438050429     c                   leave
016439050429     C                   endsl
016440050429     C*
016441050429     C                   enddo
016442050429     C*
016443050429     C/EXEC SQL
016444050429     C+ close a1
016445050429     C/END-EXEC
016446050429     c                   seton                                        04
016447050429     c                   exsr      stampa
016448050429     c                   endsr
016700030811     C*------------------------------------------------------------------------*
016800050429     C* calcola  Routine di calcolo ricavi
016900030811     C*------------------------------------------------------------------------*
017000050429     C     calcola       BEGSR
017001050502     c                   if        voce <> voces
017003050428     C                   EXSR      NORVOC
017004050428     c                   movel     wvocea        kcc
017005050428     c                   clear                   imp
017006050502     c                   eval      voces=voce
017007050502     c                   end
017100030811     C*
017101050428     C/EXEC SQL
017102050428     C+ DECLARE B1 CURSOR FOR SELECT sum(movimp) FROM ecmov00f WHERE
017103050502     C+ MOVVOC =:kcc and movcdp =:wcdp and movda = 'A' and movsvo =
017104050502     C+ :sottovoce
017108050428     C/END-EXEC
017109050428     C
017110050428     C/EXEC SQL
017111050428     C+ open b1
017112050428     C/END-EXEC
017113050428     C
017114050428     C                   do        *hival
017115050428     C/EXEC SQL
017116050428     C+ Fetch b1 into :imp
017117050428     C/END-EXEC
017118050428     C*
017119050428     C                   select
017120050428     C                   when      sqlcod = 100
017121050428     c                   leave
017122050428     C                   when      sqlcod = 0
017123050428     C                   add       imp           totimp
017124050428     C                   other
017125050428     c                   leave
017126050428     C                   endsl
017127050428     C*
017128050428     C                   enddo
017129050428     C*
017130050428     C/EXEC SQL
017131050428     C+ close b1
017132050428     C/END-EXEC
017133050428     c*
017134050428     c                   if        dettaglio = 'S'  and imp <> 0
017135050428     c                   seton                                        01
017136050428     c                   exsr      stampa
017137050428     c                   end
017138050428     C                   ENDSR
017139050429     C*------------------------------------------------------------------------*
017140050429     C* calcolac Routine di calcolo ricavi
017141050429     C*------------------------------------------------------------------------*
017142050429     C     calcolac      BEGSR
017143050502     c                   if        voce <> voces
017144050429     C                   EXSR      NORVOC
017145050429     c                   movel     wvocea        kcc
017146050429     c                   clear                   imp
017147050502     c                   eval      voces=voce
017148050502     c                   end
017149050429     C*
017150050429     C/EXEC SQL
017151050502     C+ DECLARE D1 CURSOR FOR SELECT sum(movimp) FROM ecmov00f WHERE
017152050503     C+ MOVVOC =:kcc and movdrg =:wdrg and movda = 'A' and movsvo =
017153050502     C+ :sottovoce
017154050429     C/END-EXEC
017155050429     C
017156050429     C/EXEC SQL
017157050429     C+ open d1
017158050429     C/END-EXEC
017159050429     C
017160050429     C                   do        *hival
017161050429     C/EXEC SQL
017162050429     C+ Fetch d1 into :imp
017163050429     C/END-EXEC
017164050429     C*
017165050429     C                   select
017166050429     C                   when      sqlcod = 100
017167050429     c                   leave
017168050429     C                   when      sqlcod = 0
017169050429     C                   add       imp           totimp
017170050429     C                   other
017171050429     c                   leave
017172050429     C                   endsl
017173050429     C*
017174050429     C                   enddo
017175050429     C*
017176050429     C/EXEC SQL
017177050429     C+ close d1
017178050429     C/END-EXEC
017179050429     c*
017180050429     c                   if        dettaglio = 'S'  and imp <> 0
017181050429     c                   seton                                        03
017182050429     c                   exsr      stampa
017183050429     c                   end
017184050429     C                   ENDSR
017185050428     C*------------------------------------------------------------------------*
017186050428     C*?stampa
017187050428     C*------------------------------------------------------------------------*
017188050428     C     stampa        BEGSR
017189050428     c   of              except    testa
017190050428     c                   setoff                                       of
017191050428     c   01              except    riga
017192050428     c   02              except    rigtot
017193050429     c   03              except    rigac
017194050429     c   04              except    rigtotc
017195050429     c                   setoff                                       010203
017196050429     c                   setoff                                       04
019000030811     C                   ENDSR
019001050428     C*------------------------------------------------------------------------*
019002050428     C*?NORVOC - NORMALIZZA COD.VOCE TOGLIENDO ZERI A SX E ALLINEANDO
019004050428     C*------------------------------------------------------------------------*
019005050428     C     NORVOC        BEGSR
019006050428     C*
019007050428     C                   CLEAR                   S4
019008050428     C                   CLEAR                   S4B
019009050428     C                   MOVEL     voce          wvocea            5
019010050428     C                   MOVEA     WVOCEA        S4
019011050428     C* TOGLIE ZERI NON SIGNIFICATIVI
019012050428     C                   MOVEL     '0'           F4                1
019013050428     C                   Z-ADD     1             A4                6 0
019014050428     C     F4            DOWEQ     '0'
019015050428     C     A4            ANDLE     4
019016050428     C     S4(A4)        IFGT      *ZEROS
019017050428     C                   MOVEL     '1'           F4
019018050428     C                   SUB       1             A4
019019050428     C                   ELSE
019020050428     C                   MOVEL     *BLANKS       S4(A4)
019021050428     C                   ADD       1             A4
019022050428     C                   ENDIF
019023050428     C                   ENDDO
019024050428     C*
019025050428     C* ALLINEA IL CAMPO A SINISTRA
019026050428     C                   Z-ADD     *ZEROS        C4                6 0
019027050428     C     1             DO        4             A4
019028050428     C     S4(A4)        IFNE      *BLANKS
019029050428     C                   ADD       1             C4
019030050428     C                   MOVEL     S4(A4)        S4B(C4)
019031050428     C                   ENDIF
019032050428     C                   ENDDO
019033050428     C*
019034050428     C                   MOVEA     S4B           WVOCEA
019035050428     C*
019036050428     C                   ENDSR
039100970526     C*------------------------------------------------------------------------*
039200970526     C* *INZSR - OPERAZIONI INIZIALI
039300970526     C*------------------------------------------------------------------------*
039400970526     C     *INZSR        BEGSR
039500970526     C*---
039600970526     C* RICEVIMENTO PARAMETRI
039700970526     C*---
039800970526     C     *ENTRY        PLIST
039900970612     C                   PARM                    KPJBA
040000991214     C                   MOVEL     KPJBU         PARAM
043301050428     C     *HIVAL        SETGT     ECECT01L
043302050428     C                   READP     ECECT01L                               99
043303050428     C   99              Z-ADD     1             NBC
043304050428     C  N99              Z-ADD     ECTNBC        NBC
043305050428     c*
043306050428     c                   seton                                        of
043400991214     C*
043500970526     C                   ENDSR
043600050428     oqsysprt   e            testa             1  3
043700050429     o                                           80 'CALCOLO RICAVI E COMPETENZ-
043701050429     o                                              E DA ECMOV00F PERIODO'
043702050428     o                       periodoce           +1
043800050428     o          e            riga        1
043900050429     o                                           +0 'Totale voce ricavo'
044000050428     o                       kcc                 +1
044001050428     o                       imp           4     +1
044100050428     o          e            rigtot      1
044200050428     o                                           +0 'Totale ricavi'
044300050428     o                       totimp        4     +1
044400050429     o          e            rigac       1
044500050429     o                                           +0 'Totale voce competenza'
044600050429     o                       kcc                 +1
044700050429     o                       imp           4     +1
044800050429     o          e            rigtotc     1
044900050429     o                                           +0 'Totale competenze'
045000050429     o                       totimp        4     +1
