000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000301080924     Ftivgdtmp  uF   E             DISK
000500991027     Ffnvacwwt  O    E             DISK
000700000629     Ftabel00f  if   E           k DISK
000800001010     Fwptas01l  uF a E           k DISK
000900001010     Ftitas30c  uF   E           k DISK
001000001010     Fazcln01l  iF   E           k DISK
001100991027
001200000629     D*---------
001300000629     D* SCHIERE
001400000629     D*---------
001500000629     D ANO             S              1    DIM(200)                             CONS. ANOMALE X SDA
001600000629     D*---------
001700991027     D prmppt          s             50
001800991027     D prmesito        s              1
001900991027     D wrkesito        s                   like(prmesito)
002000000629     D ktbkut          s                   like(tblkut)
002100000629     D ktbcod          s                   like(tblcod)
002200001010     D kwpaas          s                   like(tasaas)
002300001010     D kwplnp          s                   like(taslnp)
002400001010     D kwpnrs          s                   like(tasnrs)
002500001010     D kwpnsp          s                   like(tasnsp)
002600001010     D kwptbl          s                   like(tastbl) inz('F1')
002700001010     D kcltfp          s                   like(clntfp)
002800001010     D kcltfa          s                   like(clntfa)
002900001010     D kclann          s                   like(clnann)
003000001010     D kclmes          s                   like(clnmes) inz(*loval)
003100000629     D i               s              3  0 inz(*zeros)
003200000629     D ds7o          e ds
003300001010     D wrkdat          s               d   DATFMT(*ISO) INZ(D'1999-01-01')
003400001010     D dataok          s              1
003500001010     D totore          s              2  0
003600001011     D subore          s              2  0
003601080924     D fnvacds       e ds                  extname(fnvac00t)
003700001010
003800001010
003900000629     D*---------
004000941221     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
004100001010     D*---------
004200941221     D* 10+3
004300941221     D                 DS
004400941221     D  WS15                   1      1
004500941221     D  WI15                   2     11
004600991125     D  WV15                  12     12    inz(',')
004700941221     D  WD15                  13     15
004800941221     D  WASC15                 1     15
004900941221     D*  6+1
005000941221     D                 DS
005100941221     D  WS09                   1      1
005200941221     D  WI09                   2      7
005300991125     D  WV09                   8      8    inz(',')
005400941221     D  WD09                   9      9
005500941221     D  WASC09                 1      9
005600941221     D*  2+3
005700941221     D                 DS
005800941221     D  WS07                   1      1
005900941221     D  WI07                   2      3
006000991125     D  WV07                   4      4    inz(',')
006100941221     D  WD07                   5      7
006200941221     D  WASC07                 1      7
006300001010     D*---------
006400001010     D* DS PER COMPORRE/SCOMPORRE DATE
006500001010     D*---------
006600001010     D                 DS
006700001010     D  annos                  1      4  0
006800001010     D  meses                  5      6  0
006900001010     D  giornos                7      8  0
007000001010     D  mmggs                  5      8  0
007100001010     D  datas                  1      8  0
007200001010     D*
007300001010     D                 DS
007400001010     D  annoc                  1      4  0
007500001010     D  mesec                  5      6  0
007600001010     D  giornoc                7      8  0
007700001010     D  mmggc                  5      8  0
007800001010     D  datac                  1      8  0
007900000929
008000001011     I*********
008100001011     I* INDICATORI PER VERIFICA FORMATO RECORD CORRENTE (X AGGIORNAMENTO)
008200001011     I*********
008300001011     ITITAS000      01
008400001011     ITITAS010      02
008500001011     ITITASP00      03
008600001011     I*
008700001011
008800000929
008900000629     C                   EXSR      cartab
009000000629     C                   EXSR      traduci
009100921023     C                   SETON                                        LR
009200000629
009300001002
009400000629     C     cartab        begsr
009500000629     C*
009600000629     C                   movel     '1'           KTBKUT
009700000629     C                   move      '7O'          KTBCOD
009800000629     C     KEYTAB        setll     TABEL
009900000629     C     KEYTAB        reade     TABEL
010000000629     C                   dow       not %eof
010100000629     C                   movel     TBLUNI        DS7O
010200000629     C                   if        §7ORMS = 'S'
010300000629     C                   add       1             i
010400000629     C                   movel     TBLKEY        ano(i)
010500000629     C                   endif
010600000629     C     KEYTAB        reade     TABEL
010700000629     C                   enddo
010800000629     C*
010900000629     C                   ENDSR
011000000629
011100000629
011200991027
011300991027     C     traduci       BEGSR
011400991027     C*
011401080924     C                   READ      tivgdtmp
011600991027     C                   dow       not %eof
011601080924     c                   movel     vgddta        fnvacds
011700001012     C                   if        vacdcm > *zeros
011800000629     C*
011900001012     C                   if        vaccca = *blanks
012000001010     C                   exsr      chktas
012100001010     C                   endif
012200001010     C*
012300001012     C* Verifica se è un record valido x SDA (questione POSTE)
012400001012     C                   if        vaccca <> *blanks
012500001012     C     VACCCA        LOOKUP    ano                                    80
012600001012     C                   if        %found
012700941221     C                   MOVE      VACAAS        VHCAAS
012800941221     C                   MOVE      VACLNP        VHCLNP
012900941221     C                   MOVE      VACNRS        VHCNRS
013000941221     C                   MOVE      VACNSP        VHCNSP
013100941221     C                   MOVE      VACMGS        VHCMGS
013200941221     C                   MOVE      VACCBO        VHCCBO
013300941221     C                   MOVE      VACLNA        VHCLNA
013400941221     C                   MOVE      VACRSD        VHCRSD
013500941221     C                   MOVE      VACPRD        VHCPRD
013600941221     C                   MOVE      VACGC1        VHCGC1
013700941221     C                   MOVE      VACGC2        VHCGC2
013800941221     C                   MOVE      VACCTR        VHCCTR
013900941221     C                   MOVE      VACCTS        VHCCTS
014000941221     C                   MOVE      VACFTM        VHCFTM
014100941221     C                   MOVE      VACFIN        VHCFIN
014200941221     C                   MOVE      VACFAP        VHCFAP
014300941221     C                   MOVE      VACTSP        VHCTSP
014400941221     C*
014500941221     C                   MOVE      VACIAS        WD15
014600941221     C                   MOVEL     VACIAS        WI15
014700941221     C                   MOVE      WASC15        VHCIAS
014800941221     C*
014900941221     C                   MOVE      VACVAS        VHCVAS
015000941221     C                   MOVE      VACNAS        VHCNAS
015100941221     C                   MOVE      VACNCL        VHCNCL
015200941221     C*
015300941221     C                   MOVE      VACPKB        WD09
015400941221     C                   MOVEL     VACPKB        WI09
015500941221     C                   MOVE      WASC09        VHCPKB
015600941221     C*
015700941221     C                   MOVE      VACVLB        WD07
015800941221     C                   MOVEL     VACVLB        WI07
015900941221     C                   MOVE      WASC07        VHCVLB
016000941221     C*
016100941221     C                   MOVE      VACQFT        WD15
016200941221     C                   MOVEL     VACQFT        WI15
016300941221     C                   MOVE      WASC15        VHCQFT
016400941221     C*
016500941221     C                   MOVE      VACTIC        VHCTIC
016600941221     C*
016700941221     C                   MOVE      VACCAS        WD15
016800941221     C                   MOVEL     VACCAS        WI15
016900941221     C                   MOVE      WASC15        VHCCAS
017000941221     C*
017100941221     C                   MOVE      VACVCA        VHCVCA
017200941221     C                   MOVE      VACCCM        VHCCCM
017300941221     C                   MOVE      VACRMN        VHCRMN
017400941221     C                   MOVE      VACRMA        VHCRMA
017500941221     C                   MOVE      VACRMO        VHCRMO
017600941221     C                   MOVE      VACFFD        VHCFFD
017700941221     C                   MOVE      VACTCR        VHCTCR
017800941221     C                   MOVE      VACDCR        VHCDCR
017900941221     C                   MOVE      VACHCR        VHCHCR
018000941221     C                   MOVE      VACDCM        VHCDCM
018100941221     C                   MOVE      VACHMC        VHCHMC
018200941221     C                   MOVE      VACTC1        VHCTC1
018300941221     C                   MOVE      VACTC2        VHCTC2
018400941221     C                   MOVE      VACCCA        VHCCCA
018500941221     C                   MOVE      VACDLA        VHCDLA
018600941221     C                   MOVE      VACDAG        VHCDAG
018700930409     C*
018800941221     C                   WRITE     FNVACAA
018900001012     C                   endif
019000000629     C                   endif
019100001012     C                   endif
019101080924     C                   delete    tivgd000
019400991027     C*
019500080924     C                   READ      tivgdtmp
019600991027     C                   ENDdo
019700991027     C*
019800991027     C                   eval      wrkesito = '0'
019900991027     C*
020000910830     C                   ENDSR
020100001010
020200001010
020300001010
020400001010     C     chktas        begsr
020500001010     C*
020600001010     C* Compongo data spedizione
020700001010     C                   z-add     vacaas        annos
020800001010     C                   z-add     vacmgs        mmggs
020900001010     C*
021000001108     C                   if        datas >= 20001000 and
021100001107     C                             datas <= 20001099
021200001010     C                   z-add     vacaas        kwpaas
021300001010     C                   z-add     vaclnp        kwplnp
021400001010     C                   z-add     vacnrs        kwpnrs
021500001010     C                   z-add     vacnsp        kwpnsp
021600001011     C                   setoff                                       010203
021700001010     C     keytas        chain     titas30c
021800001010     C                   if        %found(titas30c)
021900001010     C                   if        tasnrc > 0
022000001012     C                   z-add     24            totore
022100001011     C                   z-add     totore        subore
022200001011     C                   z-add     tasdcm        datac
022300001010     C*
022400001010     C                   eval      dataok = '0'
022500001010     C                   dow       dataok = '0' and datac > datas
022600001010     C     *iso          move      datac         wrkdat
022700001010     C     wrkdat        subdur    1:*D          wrkdat
022800001010     C     *iso          move      wrkdat        datac
022900001010     C                   z-add     tastfp        kcltfp
023000001010     C                   z-add     tastfa        kcltfa
023100001010     C                   z-add     annoc         kclann
023200001010     C                   if        mesec <> kclmes
023300001010     C                   z-add     mesec         kclmes
023400001010     C     keycln        chain     azcln01l
023500001010     C                   endif
023600001010     C                   if        %found(azcln01l)
023700001010     C                   if        %subst(clnmat:giornoc:1) = *blanks
023800001010     C                   sub       24            totore
023900001011     C                   if        totore <= *zeros
024000001010     C                   eval      dataok = '1'
024100001010     C                   endif
024200001010     C                   endif
024300001010     C                   else
024400001010     C                   leave
024500001010     C                   endif
024600001010     C                   enddo
024700001010     C*
024800001010     C                   if        dataok = '1'
024900001010     C                   z-add     tasaas        wptaas
025000001011     C                   z-add     meses         wptmes
025100001010     C                   z-add     taslnp        wptlnp
025200001010     C                   z-add     tasnrs        wptnrs
025300001010     C                   z-add     tasnsp        wptnsp
025400001010     C                   movel     'F1'          wpttbl
025500001010     C                   z-add     datac         vacdcm
025600001010     C                   z-add     datac         tasdcm
025700001011     C                   eval      tasnrc = tasnrc - subore
025800001108     C                   z-add     datac         wptdcma
025900001108     C                   eval      wptnrca = tasnrc - subore
026000001108     C                   eval      wptmod = deptimea
026100001010     C     keytas        chain     wptas01l
026200001010     C                   if        %found(wptas01l)
026300001010     C                   update(e) wptas000
026400001010     C                   else
026500001108     C                   z-add     tasdcm        wptdcmp
026600001108     C                   eval      wptnrcp = tasnrc
026700001010     C                   write(e)  wptas000
026800001010     C                   endif
026900001010     C                   if        not %error
027000001107     C   01              update    titas000
027100001107     C   02              update    titas010
027200001107     C   03              update    titasp00
027300001010     C                   endif
027400001010     C                   endif
027500001010     C                   endif
027600001010     C                   endif
027700001010     C                   endif
027800001010     C*
027900001010     C                   endsr
028000001010
028100001010
028200991027
028300991027      /TITLE Operazioni iniziali.
028400991027     C     *inzsr        BEGSR
028500991027     C*
028600991027     C     *ENTRY        PLIST
028700991027     C                   parm                    prmppt
028800991027     C     wrkesito      parm      wrkesito      prmesito
028900991027     C*
029000000629     C     keytab        KLIST
029100000629     C                   KFLD                    ktbkut
029200000629     C                   KFLD                    ktbcod
029300001010     C*
029400001010     C     keytas        KLIST
029500001010     C                   KFLD                    kwpaas
029600001010     C                   KFLD                    kwplnp
029700001010     C                   KFLD                    kwpnrs
029800001010     C                   KFLD                    kwpnsp
029900001010     C                   KFLD                    kwptbl
030000001010     C*
030100001010     C     keycln        KLIST
030200001010     C                   KFLD                    kcltfp
030300001010     C                   KFLD                    kcltfa
030400001010     C                   KFLD                    kclann
030500001010     C                   KFLD                    kclmes
030600000629     C*
030700001011     C* Calcolo e converto subtito la data/ora corrente
030800001011     C                   time                    deptimen         14 0
030900001011     C                   movel     deptimen      deptimea         14
031000001011     C                   eval      deptimea = %subst(deptimea:11:4)+
031100001011     C                                        %subst(deptimea:9:2)+
031200001011     C                                        %subst(deptimea:7:2)+
031300001011     C                                        %subst(deptimea:1:6)
031400001011     C*
031500991027     C                   ENDSR
