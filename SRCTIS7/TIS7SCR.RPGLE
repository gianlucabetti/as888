000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000301080924     Ftivgdtmp  uF   E             DISK
000500991027     Ffnvacwwt  O    E             DISK
000700000629     Ftabel00f  if   E           k DISK
000800001214     F*wptas01l  uF a E           k DISK
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
003700010226     D countca5        s              2  0 inz(*zeros)
003701080924     D fnvacds       e ds                  extname(fnvac00t)
003900001010
004000000629     D*---------
004100941221     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
004200001010     D*---------
004300941221     D* 10+3
004400941221     D                 DS
004500941221     D  WS15                   1      1
004600941221     D  WI15                   2     11
004700991125     D  WV15                  12     12    inz(',')
004800941221     D  WD15                  13     15
004900941221     D  WASC15                 1     15
005000941221     D*  6+1
005100941221     D                 DS
005200941221     D  WS09                   1      1
005300941221     D  WI09                   2      7
005400991125     D  WV09                   8      8    inz(',')
005500941221     D  WD09                   9      9
005600941221     D  WASC09                 1      9
005700941221     D*  2+3
005800941221     D                 DS
005900941221     D  WS07                   1      1
006000941221     D  WI07                   2      3
006100991125     D  WV07                   4      4    inz(',')
006200941221     D  WD07                   5      7
006300941221     D  WASC07                 1      7
006400001010     D*---------
006500001010     D* DS PER COMPORRE/SCOMPORRE DATE
006600001010     D*---------
006700001010     D                 DS
006800001010     D  annos                  1      4  0
006900001010     D  meses                  5      6  0
007000001010     D  giornos                7      8  0
007100001010     D  mmggs                  5      8  0
007200001010     D  datas                  1      8  0
007300001010     D*
007400001010     D                 DS
007500001010     D  annoc                  1      4  0
007600001010     D  mesec                  5      6  0
007700001010     D  giornoc                7      8  0
007800001010     D  mmggc                  5      8  0
007900001010     D  datac                  1      8  0
008000021008     D*-------------------
008100021008     D* COSTANTI
008200021008     D*-------------------
008300021008     D charOK          C                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ-
008400021008     D                                     abcdefghijklmnopqrstuvwxyz1234567-
008500021008     D                                     890,;.:-_òçà°ù§+*èé!"£$%&/()=?ì\|')
008600021008     D*-------------------
008700021008     D* SCHIERE DI WRK
008800021008     D*-------------------
008900021008     D SKcharOK        S              1    DIM(92)
009000021008
009100021008
009200001011     I*********
009300001011     I* INDICATORI PER VERIFICA FORMATO RECORD CORRENTE (X AGGIORNAMENTO)
009400001011     I*********
009500001011     ITITAS000      01
009600001011     ITITAS010      02
009700001011     ITITASP00      03
009800001011     I*
009900001011
010000000929
010100000629     C                   EXSR      cartab
010200021008     C                   MOVEA     charOK        SKcharOK
010300000629     C                   EXSR      traduci
010400921023     C                   SETON                                        LR
010500000629
010600001002
010700000629     C     cartab        begsr
010800000629     C*
010900000629     C                   movel     '1'           KTBKUT
011000000629     C                   move      '7O'          KTBCOD
011100000629     C     KEYTAB        setll     TABEL
011200000629     C     KEYTAB        reade     TABEL
011300000629     C                   dow       not %eof
011400000629     C                   movel     TBLUNI        DS7O
011500000629     C                   if        §7ORMS = 'S'
011600000629     C                   add       1             i
011700000629     C                   movel     TBLKEY        ano(i)
011800000629     C                   endif
011900000629     C     KEYTAB        reade     TABEL
012000000629     C                   enddo
012100000629     C*
012200000629     C                   ENDSR
012300000629
012400000629
012500991027
012600991027     C     traduci       BEGSR
012700991027     C*
012801080924     C                   READ      tivgdtmp
012900991027     C                   dow       not %eof
012901080924     c                   movel     vgddta        fnvacds
013000001012     C                   if        vacdcm > *zeros
013100010223     C*
013200010223     C* Effettuo considerazioni su "ore non conformità cliente"
013300010223     C                   if        vaccca = *blanks
013400010223     C                   exsr      chktas
013500010223     C                   endif
013600010223     C*
013700010223     C* Effettuo considerazioni x CA = 5
013800010223     C                   if        vaccca = '5'
013900011015     C***                if        %trim(vacrsd) <> 'VEDI PACCO'
014000010223     C                   z-add     vacaas        kwpaas
014100010223     C                   z-add     vaclnp        kwplnp
014200010223     C                   z-add     vacnrs        kwpnrs
014300010223     C                   z-add     vacnsp        kwpnsp
014400010223     C                   setoff                                       010203
014500010223     C     keytas        chain     titas30c
014600010223     C                   if        %found(titas30c)
014700010424     C                   if        tasfda <> 'S' and
014800010424     C                             tasfca <> 'S'
014900010223     C                   eval      countca5 = countca5 + 1
015000011015     C                   if        countca5 <=  9                               9 bolle su countca5
015100010223     C                   eval      vaccca = *blanks
015200010223     C                   eval      tascca = '7'
015300010226     C   01              update    titas000
015400010226     C   02              update    titas010
015500010226     C   03              update    titasp00
015600010322     C                   endif
015700010322     C                   if        countca5 = 10                                *base 100 x  mod.
015800010712     C                   eval      countca5 = *zeros
015900010223     C                   endif
016000010223     C                   endif
016100011015     C                   endif
016200011015     C***                endif
016300010223     C                   endif
016400001010     C*
016500001012     C* Verifica se è un record valido x SDA (questione POSTE)
016600010223     C* esclude di fatto solamente le anomalie 5
016700001214     C                   setoff                                       80
016800001206     C                   if        vaccca <> *blanks
016900001012     C     VACCCA        LOOKUP    ano                                    80
017000001214     C                   endif
017100001214     C                   if        *in80 = *on or vaccca = *blanks
017200941221     C                   MOVE      VACAAS        VHCAAS
017300941221     C                   MOVE      VACLNP        VHCLNP
017400941221     C                   MOVE      VACNRS        VHCNRS
017500941221     C                   MOVE      VACNSP        VHCNSP
017600941221     C                   MOVE      VACMGS        VHCMGS
017700941221     C                   MOVE      VACCBO        VHCCBO
017800941221     C                   MOVE      VACLNA        VHCLNA
017900941221     C                   MOVE      VACRSD        VHCRSD
018000021008     C* Effettuo considerazioni sui caratteri della Ragione Sociale Destinatario
018100021008     C     1             DO        35            x                 2 0
018200021008     C                   MOVEL     *blanks       wrkCHAR           1
018300021008     C                   EVAL      wrkCHAR = %subst(VHCRSD:x:1)
018400021008     C                   IF        wrkCHAR <> *blanks AND
018500021008     C                             wrkCHAR <> ''''
018600021008     C     wrkCHAR       LOOKUP    SKcharOK                               20
018700021008     C                   IF        *in20 = *off
018800021008     C                   EVAL      %subst(VHCRSD:x:1) = *blanks
018900021008     C                   ENDIF
019000021008     C                   ENDIF
019100021008     C                   ENDDO
019200021008     C*
019300941221     C                   MOVE      VACPRD        VHCPRD
019400941221     C                   MOVE      VACGC1        VHCGC1
019500941221     C                   MOVE      VACGC2        VHCGC2
019600941221     C                   MOVE      VACCTR        VHCCTR
019700941221     C                   MOVE      VACCTS        VHCCTS
019800941221     C                   MOVE      VACFTM        VHCFTM
019900941221     C                   MOVE      VACFIN        VHCFIN
020000941221     C                   MOVE      VACFAP        VHCFAP
020100941221     C                   MOVE      VACTSP        VHCTSP
020200941221     C*
020300941221     C                   MOVE      VACIAS        WD15
020400941221     C                   MOVEL     VACIAS        WI15
020500941221     C                   MOVE      WASC15        VHCIAS
020600941221     C*
020700941221     C                   MOVE      VACVAS        VHCVAS
020800941221     C                   MOVE      VACNAS        VHCNAS
020900941221     C                   MOVE      VACNCL        VHCNCL
021000941221     C*
021100941221     C                   MOVE      VACPKB        WD09
021200941221     C                   MOVEL     VACPKB        WI09
021300941221     C                   MOVE      WASC09        VHCPKB
021400941221     C*
021500941221     C                   MOVE      VACVLB        WD07
021600941221     C                   MOVEL     VACVLB        WI07
021700941221     C                   MOVE      WASC07        VHCVLB
021800941221     C*
021900941221     C                   MOVE      VACQFT        WD15
022000941221     C                   MOVEL     VACQFT        WI15
022100941221     C                   MOVE      WASC15        VHCQFT
022200941221     C*
022300941221     C                   MOVE      VACTIC        VHCTIC
022400941221     C*
022500941221     C                   MOVE      VACCAS        WD15
022600941221     C                   MOVEL     VACCAS        WI15
022700941221     C                   MOVE      WASC15        VHCCAS
022800941221     C*
022900941221     C                   MOVE      VACVCA        VHCVCA
023000941221     C                   MOVE      VACCCM        VHCCCM
023100941221     C                   MOVE      VACRMN        VHCRMN
023200941221     C                   MOVE      VACRMA        VHCRMA
023300941221     C                   MOVE      VACRMO        VHCRMO
023400941221     C                   MOVE      VACFFD        VHCFFD
023500941221     C                   MOVE      VACTCR        VHCTCR
023600941221     C                   MOVE      VACDCR        VHCDCR
023700941221     C                   MOVE      VACHCR        VHCHCR
023800941221     C                   MOVE      VACDCM        VHCDCM
023900941221     C                   MOVE      VACHMC        VHCHMC
024000941221     C                   MOVE      VACTC1        VHCTC1
024100941221     C                   MOVE      VACTC2        VHCTC2
024200941221     C                   MOVE      VACCCA        VHCCCA
024300941221     C                   MOVE      VACDLA        VHCDLA
024400941221     C                   MOVE      VACDAG        VHCDAG
024500001117     C                   if        VACDLA = *zeros  and
024600001117     C                             VACCCA = 'D'
024700001117     C                   MOVE      VACDCM        VHCDLA
024800001117     C                   endif
024900001117     C                   if         VACDAG = *zeros   and
025000001117     C                             (VACCCA = 'E'     or
025100001117     C                              VACCCA = 'F'     or
025200001117     C                              VACCCA = 'L')
025300001117     C                   MOVE      VACDCM        VHCDAG
025400001117     C                   endif
025500930409     C*
025600941221     C                   WRITE     FNVACAA
025700000629     C                   endif
025800001012     C                   endif
026001080924     C                   delete    tivgd000
026100991027     C*
026201080924     C                   READ      tivgdtmp
026300001214     C                   ENDDO
026400991027     C*
026500991027     C                   eval      wrkesito = '0'
026600991027     C*
026700910830     C                   ENDSR
026800001010
026900001010
027000001010
027100010213     C     chktas        begsr
027200010213     C*
027300010213     C**********************************
027400010213     C* ALGORITMO:
027500010213     C* -se data spedizione >= 20000900
027600010213     C*   -se ore non conformità cliente > 0
027700021017     C*     -se data spedizione <= 20001299
027800010213     C*       -se ore non conformità cliente da 00 a 24 => tolgo 1 gg a TASNRC e a TASDCM
027900010213     C*       -se ore non conformità cliente da 24 a 48 => tolgo 2 gg a TASNRC e a TASDCM
028000010213     C*       -se ore non conformità cliente       > 48 => tolgo 2 gg a TASNRC e a TASDCM ma:
028100010213     C*         -se data spedizione >= 20000900 e <= 20000999 => tolgo 3 gg a TASNRC e a TASDCM
028200010213     C*         -se data spedizione >= 20001000 e <= 20001099 => tolgo 3 gg a TASNRC e a TASDCM
028300030411     C*     -se data spedizione >= 20010100  tolgo da 1 a 2 gg SOLO a TASDCM  (da metà febbraio)
028400010625     C*     -se terminal arrivo = 063/255 tolgo fino a 3 gg. TASDCM (fino al 20010709)
028500010828     C*     -se data sped. agosto 2001 tolgo altri 3 gg SOLO a TASDCM
028600010213     C*   -end
028700010213     C* -end
028800010213     C*
028900010213     C* NOTE:
029000010213     C* per quanto riguarda TASDCM (data consegna merce) i gg che vengono tolti sono da intendersi
029100021017     C* giorni lavorativio.   Ciò si traduce che una data di consegna originale che cade di lunedì
029200010213     C* portata indietro di 1 giorno fa si che vengano tolti anche 1 giorno relativo alla domenica
029300010625     C* ed 1 giorno relativo al sabato per cui, in questo caso togliendo 1 giorno di calendario si
029400010625     C* porterà in realtà la data di consegna indietro di 3 giorni. (Oltre ai sabati ed
029500021017     C* alle domeniche il file del calendario può prevedere anche i giorni di festività particolari
029600010213     C* sia a livello nazionale che locale, nonchè eventuali fattori contingenziali eccezionali).
029700010213     C**********************************
029800010213     C*
029900001010     C*
030000001010     C* Compongo data spedizione
030100001010     C                   z-add     vacaas        annos
030200001010     C                   z-add     vacmgs        mmggs
030300001010     C*
030400010213     C                   if        datas >= 20000900
030500001010     C                   z-add     vacaas        kwpaas
030600001010     C                   z-add     vaclnp        kwplnp
030700001010     C                   z-add     vacnrs        kwpnrs
030800001010     C                   z-add     vacnsp        kwpnsp
030900001011     C                   setoff                                       010203
031000001010     C     keytas        chain     titas30c
031100001010     C                   if        %found(titas30c)
031200001010     C                   if        tasnrc > 0
031300010213     C                   if        datas <= 20001299
031400001012     C                   if        tasnrc >   0 and
031500001012     C                             tasnrc <= 24
031600001012     C                   z-add     24            totore
031700001012     C                   endif
031800001012     C                   if        tasnrc >  24 and
031900001012     C                             tasnrc <= 48
032000001011     C                   z-add     48            totore
032100001012     C                   endif
032200001012     C                   if        tasnrc >  48
032300001207     C                   z-add     48            totore
032400001107     C                   if        datas >= 20000900 and
032500001107     C                             datas <= 20000999
032600001012     C                   z-add     72            totore
032700001012     C                   endif
032800001107     C                   if        datas >= 20001000 and
032900001107     C                             datas <= 20001099
033000001108     C                   z-add     72            totore
033100001107     C                   endif
033200001107     C                   endif
033300010213     C                   endif
033400030411     C*
033500030411     C*** UNICA ATTIVA ***
033600030411     C                   if        datas >= 20010100 and datas <= 20030331
033700021017     C                   if        tasnrc >   0 and
033800021017     C                             tasnrc <= 24
033900021017     C                   z-add     24            totore
034000021017     C                   endif
034100021017     C                   if        tasnrc >  24
034200021017     C                   z-add     48            totore
034300021017     C                   endif
034400010213     C                   endif
034500030411     C*** UNICA ATTIVA ***
034600010625     C                   if        datas >= 20010100 and datas <= 20010709
034700010625     C                   if        tastfa = 063 or
034800010625     C                             tastfa = 255
034900010625     C                   if        tasnrc >   0 and
035000010625     C                             tasnrc <= 24
035100010625     C                   z-add     24            totore
035200010625     C                   endif
035300010625     C                   if        tasnrc >  24 and
035400010625     C                             tasnrc <= 48
035500010625     C                   z-add     48            totore
035600010625     C                   endif
035700010625     C                   if        tasnrc >  48
035800010625     C                   z-add     72            totore
035900010625     C                   endif
036000010625     C                   endif
036100010625     C                   endif
036200010828     C                   if        datas >= 20010800 and datas <= 20010831
036300010828     C                   if        tasnrc >   0 and
036400010828     C                             tasnrc <= 24
036500010828     C                   z-add     24            totore
036600010828     C                   endif
036700010828     C                   if        tasnrc >  24 and
036800010828     C                             tasnrc <= 48
036900010828     C                   z-add     48            totore
037000010828     C                   endif
037100010828     C                   if        tasnrc >  48 and
037200010828     C                             tasnrc <= 72
037300010828     C                   z-add     72            totore
037400010828     C                   endif
037500010828     C                   if        tasnrc >  72
037600010828     C                   z-add     96            totore
037700010828     C                   endif
037800010828     C                   endif
037900010625     C*
038000001011     C                   z-add     totore        subore
038100001011     C                   z-add     tasdcm        datac
038200001010     C*
038300001010     C                   eval      dataok = '0'
038400001010     C                   dow       dataok = '0' and datac > datas
038500001010     C     *iso          move      datac         wrkdat
038600001010     C     wrkdat        subdur    1:*D          wrkdat
038700001010     C     *iso          move      wrkdat        datac
038800001010     C                   z-add     tastfp        kcltfp
038900001010     C                   z-add     tastfa        kcltfa
039000001010     C                   z-add     annoc         kclann
039100001010     C                   if        mesec <> kclmes
039200001010     C                   z-add     mesec         kclmes
039300001010     C     keycln        chain     azcln01l
039400001010     C                   endif
039500001010     C                   if        %found(azcln01l)
039600001010     C                   if        %subst(clnmat:giornoc:1) = *blanks
039700001010     C                   sub       24            totore
039800001011     C                   if        totore <= *zeros
039900001010     C                   eval      dataok = '1'
040000001010     C                   endif
040100001010     C                   endif
040200001010     C                   else
040300001010     C                   leave
040400001010     C                   endif
040500001010     C                   enddo
040600001010     C*
040700001010     C                   if        dataok = '1'
040800001214     C*                  z-add     tasaas        wptaas
040900001214     C*                  z-add     meses         wptmes
041000001214     C*                  z-add     taslnp        wptlnp
041100001214     C*                  z-add     tasnrs        wptnrs
041200001214     C*                  z-add     tasnsp        wptnsp
041300001214     C*                  movel     'F1'          wpttbl
041400001214     C*                  z-add     tasdcm        wptdcmp
041500001214     C*                  z-add     datac         wptdcma
041600001214     C*                  eval      wptnrcp = tasnrc
041700001214     C*                  eval      wptnrca = tasnrc - subore
041800001214     C*                  eval      wptmod = deptimea
041900001010     C                   z-add     datac         vacdcm
042000010213     C                   z-add     datac         tasdcm
042100010213     C                   if        datas <= 20001299
042200001011     C                   eval      tasnrc = tasnrc - subore
042300010213     C                   endif
042400001214     C*    keytas        chain     wptas01l
042500001214     C*                  if        %found(wptas01l)
042600001214     C*                  update(e) wptas000
042700001214     C*                  else
042800001214     C*                  write(e)  wptas000
042900001214     C*                  endif
043000001214     C*                  if        not %error
043100010226     C   01              update    titas000
043200010226     C   02              update    titas010
043300010226     C   03              update    titasp00
043400001214     C*                  endif
043500001010     C                   endif
043600001010     C                   endif
043700001010     C                   endif
043800001010     C                   endif
043900001010     C*
044000001010     C                   endsr
044100001010
044200001010
044300991027
044400991027      /TITLE Operazioni iniziali.
044500991027     C     *inzsr        BEGSR
044600991027     C*
044700991027     C     *ENTRY        PLIST
044800991027     C                   parm                    prmppt
044900991027     C     wrkesito      parm      wrkesito      prmesito
045000991027     C*
045100000629     C     keytab        KLIST
045200000629     C                   KFLD                    ktbkut
045300000629     C                   KFLD                    ktbcod
045400001010     C*
045500001010     C     keytas        KLIST
045600001010     C                   KFLD                    kwpaas
045700001010     C                   KFLD                    kwplnp
045800001010     C                   KFLD                    kwpnrs
045900001010     C                   KFLD                    kwpnsp
046000001010     C                   KFLD                    kwptbl
046100001010     C*
046200001010     C     keycln        KLIST
046300001010     C                   KFLD                    kcltfp
046400001010     C                   KFLD                    kcltfa
046500001010     C                   KFLD                    kclann
046600001010     C                   KFLD                    kclmes
046700000629     C*
046800001011     C* Calcolo e converto subtito la data/ora corrente
046900001214     C*                  time                    deptimen         14 0
047000001214     C*                  movel     deptimen      deptimea         14
047100001214     C*                  eval      deptimea = %subst(deptimea:11:4)+
047200001214     C*                                       %subst(deptimea:9:2)+
047300001214     C*                                       %subst(deptimea:7:2)+
047400001214     C*                                       %subst(deptimea:1:6)
047500001011     C*
047600991027     C                   ENDSR
