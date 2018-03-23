000100030206     H DECEDIT('0,') DATEDIT(*YMD.)
000200030206
000300160414     feccev30c  if   e           k Disk
000301160414     f                                     Ignore(ECCEvP00)
000302160414     fecc2v30c  if   e           k Disk
000303160414     f                                     Ignore(ECC2vP00)
000800040319     fsisde01l  uf a e           k Disk
000900040319     ftabel00f  if   e           k Disk
001000040319     fazorg01l  if   e           k Disk
001100040319     D* FILIALI CON RELATIVO RAGGRUPPAMENTO REGIONE
001200040319     D FIL             S              3  0 DIM(500)
001300040319     D RRN             S              3  0 DIM(500)
001400040319     D FPC             S              2    DIM(20)
001500040319     D KIC             S              6  0 DIM(20)
001600040319     D KFC             S              6  0 DIM(20)
001700040319     D RBL             S              1    DIM(50)
001800040319     D TPO             S              1    DIM(50)
001900040319     D Tbl             S              2    DIM(50)
001907160413     d wdpes           s                   like(taspkc)
001908160413     d wdvol           s                   like(tasvlc)
001909160414     d totale          s                   like(§k30irp)
002000040319
002100040319     D Ds14          E DS
002200040319     D Ds2l          E DS
002300040319     D Dstb          E DS
002400160413     D DsdeK30       E DS
002401160405     D Dtas          E DS                  extname(titas10f)
002800040319
002900030206
002901160405     c                   Eval      *InLr = *On
002902160405     C/EXEC SQL
002903160413     C+ DECLARE T CURSOR FOR SELECT P, V,
002904160413     C+ tastbl, Taslnp, taslna,
002905160413     C+ tasaas, tasmgs, tasksc, tastsp, tasctr,
002906160413     C+ tasPKB, tasPKC, tasVLB, tasFVF, tasVLF, TASVLC, TASNCL, TASNCR,
002907160414     C+ tasNCP, tasimv, tasnrs, tasnsp
002908160413     C+ from ( SELECT rtvpes(tasncl, tasncp, taspkc, taspkf) p         ,
002909160412     C+ rtvvol(tasncl, tasncr, tasfvf, tasvlc, tasvlf) v, titas10f.* from
002910160419     C+ titas10f where tasaas = 2016 and tasmgs between 0201 and 0331
002911160414     C+ and tasfa2 = ' ' ) as tas
002912160414     C+ where p between 25,1 and 30
002913160405     C/END-EXEC
002914160405
002915160405     C/EXEC SQL
002916160412     C+ OPEN t
002917160405     C/END-EXEC
002918160405
002919160405     C                   DO        *HIVAL
002920160405
002921160405     C/EXEC SQL
002924160413     C+ FETCH NEXT FROM t INTO :wdpes, :wdvol, :tastbl, :taslnp, :taslna,
002925160413     C+ :tasaas, :tasmgs, :tasksc, :tastsp, :tasctr,
002926160413     C+ :tasPKB, :tasPKC, :tasVLB, :tasFVF, :tasVLF, :TASVLC,
002927160414     C+ :TASNCL, :TASNCR, :TASNCP, :tasimv, :tasnrs, :tasnsp
002928160412     C/END-EXEC
002929160405
002930160405     C                   SELECT
002931160405     C
002932160405     C                   WHEN      SQLCod = 100
002933160405     C                   LEAVE
002934160405     C
002935160405     C                   WHEN      SQLCod < 0
002936160405     C                   EVAL      *INH1 = *ON
002937160405     C                   LEAVE
002938160405     C
002939160405     C                   OTHER
002940160413     C                   EXSR      AGGSDE
005101160405     C                   ENDSL
005102160405
005103160405     C                   ENDDO
005105160405
005106160405     C/EXEC SQL
005107160412     C+ CLOSE t
005108160405     C/END-EXEC
005113160405     C                   RETURN
005200040319     c**********************************************************************
005300040319     c     aggsde        begsr
005400040319     c**********************************************************************
005500040319     c*
005600040319     C* RECUPERO LA REGIONE PARTENZA
005700040319     c                   z-add     1             i
005800040319     C     taslnp        LOOKUP    FIL(I)                                 99
005900040319     C                   Z-ADD     RRN(I)        sderep
006000040319     C*
006100040319     C* RECUPERO LA REGIONE DI ARRIVO
006200040319     c                   z-add     1             i
006300040319     C     taslna        LOOKUP    FIL(I)                                 99
006400040319     C                   Z-ADD     RRN(I)        sderea
006500040319     C*
006600040319     C* FASCIA DI PESO DA TABELLA '2L' X FILE CLIENTI
006700160412     C     wdpes         IFEQ      *ZEROS
006800040319     C                   MOVEL     FPC(1)        sdefpe
006900040319     C                   ELSE
007000040319     C                   DO        20            I
007100160412     C     wdpes         IFGT      KIC(I)
007200160413     C     wdpes         ANDLE     KFC(I)
007300040319     C                   MOVEL     FPC(I)        sdefpe
007400040319     C                   LEAVE
007500040319     C                   ENDIF
007600040319     C                   ENDDO
007700040319     C                   ENDIF
007800040319     C*
007900040319     C* tipo bolla e raggruppamento bolla
008000040319     C                   Z-ADD     1             I                 6 0
008100040319     C     tastbl        LOOKUP    TBL(I)                                 99
008200040319     C                   MOVEL     TPO(I)        sdeTPO
008300040319     C                   MOVEL     RBL(I)        sdeRBL
008400040319
008500160413     c                   movel     'K30'         sDETRD
008501160405     c                   movel     'SDC'         sDEFILE
008600040319     c                   z-add     tasksc        sDEKSC
008700040319     c                   z-add     tasaas        sDEANN
008800040319     c                   movel     tasmgs        sDEMES
008900040319     c                   movel     tastsp        sDETSP
009000040319     c                   z-add     tasctr        sDECTR
009100040319     c                   clear                   sDETPR
009200040319     c* aggancio SISDE01l
009300040319     C     KEYSDE        CHAIN     SISDE01L                           99
009400040319     C     *IN99         IFEQ      '0'
009500040319     C                   EXSR      ADDSDE
009600040319     C                   UPDATE    SISDE000
009700040319     C                   ELSE
009800040319     C                   EXSR      AZZSDE
009900040319     C                   EXSR      ADDSDE
010000040319     C                   WRITE     SISDE000
010100040319     C                   END
010200040319     c
010300040319     c                   endsr
010400040319     C********************************************************************
010500040319     C*   AZZSDE - AZZERA I CAMPI DEL FILE SISDE00F (ESTENSIONE SALDI)      )
010600040319     C********************************************************************
010700040319     C     AZZSDE        BEGSR
011100160413     C                   CLEAR                   §K30NSP
011101160413     C                   CLEAR                   §K30PKG
011102160413     C                   CLEAR                   §K30PCC
011103160413     C                   CLEAR                   §K30PLS
011104160413     C                   CLEAR                   §K30NCO
011105160413     C                   CLEAR                   §K30VOL
011106160413     C                   CLEAR                   §K30VCC
011107160413     C                   CLEAR                   §K30VLS
011108160414     C                   CLEAR                   §K30irb
011109160414     C                   CLEAR                   §K30irp
011200160413     C                   eval      sdeuni = Dsdek30
011800040319     C                   MOVEL     *BLANKS       SDEFTR
011900040319     C                   Z-ADD     *ZEROS        SDEDTR
012000040319     C                   ENDSR
012100040319     C********************************************************************
012200040319     C*   ADDSDE - AGGIUNGE AI VALORI DEL FILE SISDE00F (ESTENSIONE SALDI)  EG)
012300040319     C********************************************************************
012400040319     C     ADDSDE        BEGSR
012800160413     C                   eval      Dsdek30 = sdeuni
012901160413     c                   add       1             §K30NSP
012902160414     c                   add       tasncl        §K30NCO
012903160413     c                   add       WdPES         §K30PKG
012906160413     c                   add       WDVOL         §K30VOL
012909160414     c                   add       tasimv        §K30irb
012910160414     c                   exsr      conta
012912160414     c                   add       totale        §K30irp
012913160414     c*
012914160413IF  2C     TASPKC        IFGT      *ZEROS
012915160413     C     TASNCL        ANDEQ     TASNCP
012916160413     C                   ADD       TASPKB        §K30PCC
012917160413     C                   ADD       TASPKC        §K30PLS
012918160413     C                   END
012920160413     C     tasFVF        IFEQ      'T'
012921160413     c                   add       tasVLB        §K30VCC
012922160413     c                   add       tasVLf        §K30VLS
012923160413     C                   ELSE
012924160413     C     tasVLC        IFGT      *ZEROS
012925160413     C     tasNCL        ANDEQ     tasNCR
012926160413     c                   add       tasVLB        §K30VCC
012927160413     C                   ADD       TASVLC        §K30VLS
012928160413     c                   end
012929160413     c                   end
013300160413     C                   eval      sdeuni = Dsdek30
014100040319     C                   MOVEL     *BLANKS       SDEFTR
014200040319     C                   Z-ADD     *ZEROS        SDEDTR
014300040319     C                   ENDSR
014301160414     C*--------------------------------------------------------------- ---------
014302160414     C     conta         BEGSR
014303160414     C*--------------------------------------------------------------- ---------
014304160414     c                   clear                   totale
014305160414     c     ktas          chain     eccev30c
014306160414     c                   if        %found(eccev30c)
014307160414     c                   select
014308160414     c                   when      cevc01 = 900
014309160414     c                   add       cevi01        totale
014311160414     c                   when      cevc02 = 900
014312160414     c                   add       cevi02        totale
014314160414     c                   when      cevc03 = 900
014315160414     c                   add       cevi03        totale
014317160414     c                   when      cevc04 = 900
014318160414     c                   add       cevi04        totale
014320160414     c                   when      cevc05 = 900
014321160414     c                   add       cevi05        totale
014323160414     c                   when      cevc06 = 900
014324160414     c                   add       cevi06        totale
014326160414     c                   when      cevc07 = 900
014327160414     c                   add       cevi07        totale
014329160414     c                   when      cevc08 = 900
014330160414     c                   add       cevi08        totale
014332160414     c                   when      cevc09 = 900
014333160414     c                   add       cevi09        totale
014335160414     c                   when      cevc10 = 900
014336160414     c                   add       cevi10        totale
014338160414     c                   when      cevc11 = 900
014339160414     c                   add       cevi11        totale
014341160414     c                   when      cevc12 = 900
014342160414     c                   add       cevi12        totale
014344160414     c                   when      cevc13 = 900
014345160414     c                   add       cevi13        totale
014347160414     c                   when      cevc14 = 900
014348160414     c                   add       cevi14        totale
014350160414     c                   when      cevc15 = 900
014351160414     c                   add       cevi15        totale
014353160414     c                   when      cevc16 = 900
014354160414     c                   add       cevi16        totale
014356160414     c                   when      cevc17 = 900
014357160414     c                   add       cevi17        totale
014359160414     c                   when      cevc18 = 900
014360160414     c                   add       cevi18        totale
014362160414     c                   when      cevc19 = 900
014363160414     c                   add       cevi19        totale
014365160414     c                   when      cevc20 = 900
014366160414     c                   add       cevi20        totale
014368160414     c                   when      cevc21 = 900
014369160414     c                   add       cevi21        totale
014371160414     c                   when      cevc22 = 900
014372160414     c                   add       cevi22        totale
014374160414     c                   when      cevc23 = 900
014375160414     c                   add       cevi23        totale
014377160414     c                   when      cevc24 = 900
014378160414     c                   add       cevi24        totale
014380160414     c                   when      cevc25 = 900
014381160414     c                   add       cevi25        totale
014383160414     c                   when      cevc26 = 900
014384160414     c                   add       cevi26        totale
014386160414     c                   when      cevc27 = 900
014387160414     c                   add       cevi27        totale
014389160414     c                   when      cevc28 = 900
014390160414     c                   add       cevi28        totale
014392160414     c                   when      cevc29 = 900
014393160414     c                   add       cevi29        totale
014395160414     c                   when      cevc30 = 900
014396160414     c                   add       cevi30        totale
014398160414     c                   when      cevc31 = 900
014399160414     c                   add       cevi31        totale
014401160414     c                   when      cevc32 = 900
014402160414     c                   add       cevi32        totale
014404160414     c                   when      cevc33 = 900
014405160414     c                   add       cevi33        totale
014407160414     c                   when      cevc34 = 900
014408160414     c                   add       cevi34        totale
014410160414     c                   when      cevc35 = 900
014411160414     c                   add       cevi35        totale
014413160414     c                   when      cevc36 = 900
014414160414     c                   add       cevi36        totale
014416160414     c                   when      cevc37 = 900
014417160414     c                   add       cevi37        totale
014419160414     c                   when      cevc38 = 900
014420160414     c                   add       cevi38        totale
014422160414     c                   when      cevc39 = 900
014423160414     c                   add       cevi39        totale
014425160414     c                   when      cevc40 = 900
014426160414     c                   add       cevi40        totale
014428160414     c                   when      cevc41 = 900
014429160414     c                   add       cevi41        totale
014431160414     c                   when      cevc42 = 900
014432160414     c                   add       cevi42        totale
014434160414     c                   when      cevc43 = 900
014435160414     c                   add       cevi43        totale
014437160414     c                   when      cevc44 = 900
014438160414     c                   add       cevi44        totale
014440160414     c                   when      cevc45 = 900
014441160414     c                   add       cevi45        totale
014443160414     c                   when      cevc46 = 900
014444160414     c                   add       cevi46        totale
014446160414     c                   when      cevc47 = 900
014447160414     c                   add       cevi47        totale
014449160414     c                   when      cevc48 = 900
014450160414     c                   add       cevi48        totale
014452160414     c                   when      cevc49 = 900
014453160414     c                   add       cevi49        totale
014455160414     c                   when      cevc50 = 900
014456160414     c                   add       cevi50        totale
014457160414     c                   endsl
014458160414     c                   if        totale = 0
014459160414     c     ktas          chain     ecc2v30c
014460160414     c                   if        %found(ecc2v30c)
014461160414     c                   select
014462160414     c                   when      c2vc51 = 900
014463160414     c                   add       c2vi51        totale
014464160414     c                   when      c2vc52 = 900
014465160414     c                   add       c2vi52        totale
014466160414     c                   when      c2vc53 = 900
014467160414     c                   add       c2vi53        totale
014468160414     c                   when      c2vc54 = 900
014469160414     c                   add       c2vi54        totale
014470160414     c                   when      c2vc55 = 900
014471160414     c                   add       c2vi55        totale
014472160414     c                   when      c2vc56 = 900
014473160414     c                   add       c2vi56        totale
014474160414     c                   when      c2vc57 = 900
014475160414     c                   add       c2vi57        totale
014476160414     c                   when      c2vc58 = 900
014477160414     c                   add       c2vi58        totale
014478160414     c                   when      c2vc59 = 900
014479160414     c                   add       c2vi59        totale
014480160414     c                   when      c2vc60 = 900
014481160414     c                   add       c2vi60        totale
014482160414     c                   when      c2vc61 = 900
014483160414     c                   add       c2vi61        totale
014484160414     c                   when      c2vc62 = 900
014485160414     c                   add       c2vi62        totale
014486160414     c                   when      c2vc63 = 900
014487160414     c                   add       c2vi63        totale
014488160414     c                   when      c2vc64 = 900
014489160414     c                   add       c2vi64        totale
014490160414     c                   when      c2vc65 = 900
014491160414     c                   add       c2vi65        totale
014492160414     c                   when      c2vc66 = 900
014493160414     c                   add       c2vi66        totale
014494160414     c                   when      c2vc67 = 900
014495160414     c                   add       c2vi67        totale
014496160414     c                   when      c2vc68 = 900
014497160414     c                   add       c2vi68        totale
014498160414     c                   when      c2vc69 = 900
014499160414     c                   add       c2vi69        totale
014500160414     c                   when      c2vc70 = 900
014501160414     c                   add       c2vi70        totale
014502160414     c                   when      c2vc71 = 900
014503160414     c                   add       c2vi71        totale
014504160414     c                   when      c2vc72 = 900
014505160414     c                   add       c2vi72        totale
014506160414     c                   when      c2vc73 = 900
014507160414     c                   add       c2vi73        totale
014508160414     c                   when      c2vc74 = 900
014509160414     c                   add       c2vi74        totale
014510160414     c                   when      c2vc75 = 900
014511160414     c                   add       c2vi75        totale
014512160414     c                   endsl
014513160414     c                   end
014514160414     c                   end
014515160414     c                   end
014516160414     C                   ENDSR
014603160414     c**********************************************************************
014604160414     c     *inzsr        begsr
014605160414     c**********************************************************************
014700040319
014800040319     c     keysde        Klist
014900040319     c                   Kfld                    sDEFILE
015000040319     c                   Kfld                    sDETRD
015100040319     c                   Kfld                    sDEKSC
015200040319     c                   Kfld                    sDEANN
015300040319     c                   Kfld                    sDEMES
015400040319     c                   Kfld                    sDEREP
015500040319     c                   Kfld                    sDEREA
015600040319     c                   Kfld                    sDEFPE
015700040319     c                   Kfld                    sDETSP
015800040319     c                   Kfld                    sDECTR
015900040319     c                   Kfld                    sDETPO
016000040319     c                   Kfld                    sDERBL
016100040319     c                   Kfld                    sDETPR
016101160414     c     ktas          Klist
016102160414     c                   Kfld                    tasaas
016103160414     c                   Kfld                    taslnp
016104160414     c                   Kfld                    tasnrs
016105160414     c                   Kfld                    tasnsp
016106160414     c                   Kfld                    tastbl
016700040319     c     ktab          Klist
016800040319     c                   Kfld                    tblkut
016900040319     c                   Kfld                    tblcod
017000040319     c     ktab1         Klist
017100040319     c                   Kfld                    tblkut
017200040319     c                   Kfld                    tblcod
017300040319     c                   Kfld                    tblkey
017400040319     c                   z-add     1             tblkut
017500040319     C* CARICA ORGANIGRAMMA
017600040319     C                   Z-ADD     *ZEROS        orgFIL
017700040319     C     orgfil        SETLL     AZORG01L                           99
017800040319     C  N99              READ      AZORG01L                               99
017900040319     C                   Z-ADD     *ZEROS        I
018000040319DO  2C     *IN99         DOWEQ     *OFF
018100040319IF  3C     ORGFVA        IFEQ      *BLANKS
018200040319IF  4C     ORGFAG        andne     'V'
018300040319     C                   ADD       1             I
018400040319     C                   Z-ADD     ORGFIL        FIL(I)
018500040319     C                   movel(p)  ORGcre        tblkey
018600040319     C* RAGGRUPPAMENTI
018700040319     c                   eval      tblcod = '14'
018800040319     C     ktab1         chain     tabel00f
018900040319IF  5C                   IF        %found(tabel00f)
019000040319     c                   eval      ds14= tbluni
019100040319     C                   Z-ADD     §14crn        RRN(I)
019200040319E   5C                   ENDIF
019300040319E   3C                   ENDIF
019400040319     C                   READ      AZORG01L                               99
019500040319E   2C                   ENDDO
019600040319     C* CARICA TABELLA -2L- FASCIE DI PESO
019700040319     c                   eval      tblcod = '2L'
019800040319     C                   Z-ADD     *ZEROS        I                 6 0
019900040319     C     Ktab          SETLL     TABEL00F                           99
020000040319     C  N99ktab          READe     TABEL00F                               99
020100040319     C     *IN99         DOWEQ     '0'
020200040319     C     TBLFLG        IFEQ      *BLANKS
020300040319     C                   MOVEL     TBLKEY        WTM03             3
020400040319     C                   MOVEL     TBLKEY        WTM05             5
020500040319     C                   MOVEL     TBLUNI        DS2L
020600040319     C     WTM03         ifeq      'S2C'
020700040319     C                   ADD       1             I
020800040319     C                   MOVE      WTM05         FPC(I)
020900040319     C                   Z-ADD     §2LKGI        KIC(I)
021000040319     C                   Z-ADD     §2LKGF        KFC(I)
021100040319     C                   ENDIF
021200040319     C                   ENDIF
021300040319     C     ktab          READe     TABEL00F                               99
021400040319     C                   ENDDO
021500040319     C*
021600040319     C* CARICA TABELLA -TB- TIPI BOLLA
021700040319     C                   Z-ADD     0             I
021800040319     C                   MOVEL     'TB'          tblcod
021900040319     C     Ktab          SETLL     TABEL00F                           99
022000040319     C  N99ktab          READe     TABEL00F                               99
022100040319     C     *IN99         DOWEQ     '0'
022200040319     C     TBLFLG        IFEQ      *BLANKS
022300040319     C                   ADD       1             I
022400040319     C                   MOVEL     TBLUNI        DSTB
022500040319     C                   MOVEL     tblkey        Tbl(I)
022600040319     C                   MOVEL     §TBTPO        TPO(I)
022700040319     C                   MOVEL     §TBRBL        RBL(I)
022800040319     C                   END
022900040319     C     ktab          READe     TABEL00F                               99
023000040319     C                   ENDdo
023100040319     c                   endsr
