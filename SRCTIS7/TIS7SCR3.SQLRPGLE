000100010925      *PARMS DYNUSRPRF(*OWNER)
000200890921     H DECEDIT('0,') DATEDIT(*DMY.)
000300991027     H dftactgrp(*yes)
000400991027
000500991027     Ffnvacwwt  O    E             DISK
000600001215     Fprtf198   O    F  198        PRINTER OFLIND(*INOF)
000700001215     F                                     FORMLEN(66)
000800001215     F                                     FORMOFL(60)
000900001012
001000000629     D*---------
001100991027     D prmppt          s             50
001200991027     D prmesito        s              1
001300991027     D wrkesito        s                   like(prmesito)
001400000629     D*---------
001500941221     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
001600941221     D* 10+3
001700941221     D                 DS
001800941221     D  WS15                   1      1
001900941221     D  WI15                   2     11
002000991125     D  WV15                  12     12    inz(',')
002100941221     D  WD15                  13     15
002200941221     D  WASC15                 1     15
002300941221     D*  6+1
002400941221     D                 DS
002500941221     D  WS09                   1      1
002600941221     D  WI09                   2      7
002700991125     D  WV09                   8      8    inz(',')
002800941221     D  WD09                   9      9
002900941221     D  WASC09                 1      9
003000941221     D*  2+3
003100941221     D                 DS
003200941221     D  WS07                   1      1
003300941221     D  WI07                   2      3
003400991125     D  WV07                   4      4    inz(',')
003500941221     D  WD07                   5      7
003600941221     D  WASC07                 1      7
003700991027
003800000929
003900001215     D*-----------------
004000001215     D* DS di lettura formato via SQL
004100001215     D*-----------------
004200001215     D  fnvac00s     E DS
004300001215
004400000929
004500001215     C*-----------------
004600001215     C* MAIN
004700001215     C*-----------------
004800000629     C                   EXSR      traduci
004900921023     C                   SETON                                        LR
005000001215     C*
005100000629
005200000629
005300991027
005400991027     C     traduci       BEGSR
005500991027     C*
005600001215     C*
005700001215     C/EXEC SQL
005800001215     C+ declare C1 cursor for
005900001215     C+ select * from sdavac/fnvacxxs
006000001215     C+ where vaccca = ' ' and vacdcm > 0
006100001215     C+ order by vacnrs, vacnsp, vacdcm
006200001215     C+ for fetch only
006300001215     C/END-EXEC
006400001215     C
006500001215     C/EXEC SQL
006600001215     C+ open C1
006700001215     C/END-EXEC
006800001215     C
006900001215     C/EXEC SQL
007000001215     C+ Fetch C1 into :fnvac00s
007100001215     C/END-EXEC
007200001215     C*
007300001215     C                   z-add     *zeros        rotnrs            2 0
007400001215     C                   z-add     *zeros        rotnsp            7 0
007500001215     C                   z-add     *zeros        tstdcm            8 0
007600001215     C                   z-add     *zeros        contatore        10 0
007700001215     C*
007800001215     C                   dow       sqlcod = *zeros
007900001215     C* Verifico rottura di codice rispetto a NRS + NSP
008000001215     C                   if        vacnrs = rotnrs and
008100001215     C                             vacnsp = rotnsp
008200001215     C* Verifico se trattasi di bolla ripetuta con diversa data consegna
008300001215     C                   if        vacdcm <> tstdcm
008400001215     C                   add       1             contatore
008500001215     C                   endif
008600001215     C                   z-add     vacdcm        tstdcm
008700001215     C                   else
008800001215     C* Effettuo la traduzione e la scrittura
008900001215     C                   MOVE      VACAAS        VHCAAS
009000001215     C                   MOVE      VACLNP        VHCLNP
009100001215     C                   MOVE      VACNRS        VHCNRS
009200001215     C                   MOVE      VACNSP        VHCNSP
009300001215     C                   MOVE      VACMGS        VHCMGS
009400001215     C                   MOVE      VACCBO        VHCCBO
009500001215     C                   MOVE      VACLNA        VHCLNA
009600001215     C                   MOVE      VACRSD        VHCRSD
009700001215     C                   MOVE      VACPRD        VHCPRD
009800001215     C                   MOVE      VACGC1        VHCGC1
009900001215     C                   MOVE      VACGC2        VHCGC2
010000001215     C                   MOVE      VACCTR        VHCCTR
010100001215     C                   MOVE      VACCTS        VHCCTS
010200001215     C                   MOVE      VACFTM        VHCFTM
010300001215     C                   MOVE      VACFIN        VHCFIN
010400001215     C                   MOVE      VACFAP        VHCFAP
010500001215     C                   MOVE      VACTSP        VHCTSP
010600001215     C                   MOVE      VACIAS        WD15
010700001215     C                   MOVEL     VACIAS        WI15
010800001215     C                   MOVE      WASC15        VHCIAS
010900001215     C                   MOVE      VACVAS        VHCVAS
011000001215     C                   MOVE      VACNAS        VHCNAS
011100001215     C                   MOVE      VACNCL        VHCNCL
011200001215     C                   MOVE      VACPKB        WD09
011300001215     C                   MOVEL     VACPKB        WI09
011400001215     C                   MOVE      WASC09        VHCPKB
011500001215     C                   MOVE      VACVLB        WD07
011600001215     C                   MOVEL     VACVLB        WI07
011700001215     C                   MOVE      WASC07        VHCVLB
011800001215     C                   MOVE      VACQFT        WD15
011900001215     C                   MOVEL     VACQFT        WI15
012000001215     C                   MOVE      WASC15        VHCQFT
012100001215     C                   MOVE      VACTIC        VHCTIC
012200001215     C                   MOVE      VACCAS        WD15
012300001215     C                   MOVEL     VACCAS        WI15
012400001215     C                   MOVE      WASC15        VHCCAS
012500001215     C                   MOVE      VACVCA        VHCVCA
012600001215     C                   MOVE      VACCCM        VHCCCM
012700001215     C                   MOVE      VACRMN        VHCRMN
012800001215     C                   MOVE      VACRMA        VHCRMA
012900001215     C                   MOVE      VACRMO        VHCRMO
013000001215     C                   MOVE      VACFFD        VHCFFD
013100001215     C                   MOVE      VACTCR        VHCTCR
013200001215     C                   MOVE      VACDCR        VHCDCR
013300001215     C                   MOVE      VACHCR        VHCHCR
013400001215     C                   MOVE      VACDCM        VHCDCM
013500001215     C                   MOVE      VACHMC        VHCHMC
013600001215     C                   MOVE      VACTC1        VHCTC1
013700001215     C                   MOVE      VACTC2        VHCTC2
013800001215     C                   MOVE      VACCCA        VHCCCA
013900001215     C                   MOVE      VACDLA        VHCDLA
014000001215     C                   MOVE      VACDAG        VHCDAG
014100001215     C                   WRITE     FNVACAA
014200001215     C* Ri-assegno i campi di rottura
014300001215     C                   z-add     vacnrs        rotnrs
014400001215     C                   z-add     vacnsp        rotnsp
014500001215     C                   z-add     *zeros        tstdcm
014600001215     C                   z-add     *zeros        contatore
014700001215     C                   endif
014800001215     C* Se vi sono state bolle ripetute ma con data consegna diversa => stampo la chiave bolla
014900001215     C                   if        contatore > 1
015000001215     C                   except    corpo
015100001215     C                   endif
015200001215     C* Continuo con la lettura dello snapshot
015300001215     C*
015400001215     C/EXEC SQL
015500001215     C+ Fetch C1 into :fnvac00s
015600001215     C/END-EXEC
015700001215     C*
015800001215     C                   enddo
015900001215     C*
016000001215     C/EXEC SQL
016100001215     C+ close C1
016200001215     C/END-EXEC
016300991027     C*
016400910830     C                   ENDSR
016500991027
016600991027      /TITLE Operazioni iniziali.
016700991027     C     *inzsr        BEGSR
016800991027     C*
016900991027     C     *ENTRY        PLIST
017000991027     C                   parm                    prmppt
017100991027     C     wrkesito      parm      wrkesito      prmesito
017200991027     C*
017300991027     C                   ENDSR
017400001215     O*------------------------------------------------------------------------*
017500001215     OPRTF198   E            CORPO          1 02
017600001215     O                       VACAAS            +  1
017700001215     O                       VACLNP            +  1
017800001215     O                       VACNRS            +  1
017900001215     O                       VACNSP            +  1
018000001215     O                       VACCBO            +  1
018100001215     O*------------------------
