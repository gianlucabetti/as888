000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300170816      * Aggiorna pesi e volumi bolle errati
000400170816      * ---------------------------------------------------------
000500160930     fFNblp31l  iF   E           K DISK    infds(dsblp)
000600160930     f                                     rename(fnblp000:fnblp031)
000700170817     fFNarb01l  uF   E           k DISK
000800160930     fFNblp00f  uF   E             DISK    rename(fnblp000:fnblp0fis)
000900100315     ftitas30c  uF   E           K DISK
001000170816     ffnlbl01l  iF   E           K DISK
001100170817     ftabel00f  iF   E           K DISK
001200080320     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001300080320     d
001400170816     dsavvlf           s                   like(blpvlf)
001500170816     dsavfvf           s                   like(blpfvf)
001600170816     dsavvlb           s                   like(blpvlb)
001700170816     dsavfvb           s                   like(blpfvb)
001800170817     dsavpkb           s                   like(blppkb)
001900170817     dsavpkf           s                   like(blppkf)
002000170816
002100160928     D* DS PER FNLV18R - CALCOLO DI UN VOLUME AUTOMATICO
002200160928     D DSLV18        E DS                  EXTNAME(FNLV18DS)
002300160928     D trulc7ds      E DS
002400170817     D* DS PER TRUL27R1 - Ricerca network bolla
002500170817     D trul27ds1     E DS
002600170817     D ds3a          E DS
002700170817     D kpjba         E DS
002800170817     d
002900170817     D                 DS
003000170817     D  blpaas                 1      4  0
003100170817     D  blpmgs                 5      8  0
003200170817     D  blpdsp                 1      8  0
003300170817
003400170817     D fnlvtvds        DS
003500170816     D  vidlnp                 1      3  0
003600170816     D  vidccm                 4     10  0
003700170816     D  comela                11     18  0
003800170816     D  vidpes                19     20
003900170816     D  vidpes1               21     27  1
004000170816     D  vidpes2               28     28
004100170816     D  vidvol                29     30
004200170816     D  vidvol1               31     35  3
004300170817     D  vidvol3               36     36
004400170816     D  vidaggio              37     37
004500170816     D  comela2               38     45  0
004600170816     D  vidpes3               46     46
004700170817     D  vidvol4               47     47
004800170817     D  vidvol5               48     48
004900170817     D  vidvol6               49     49
005000170816     d
005100160930     D dsblp           DS
005200160930     D  blpNRR               397    400B 0
005300160930     d
005400160930     d Dataiso         s               d   datfmt(*iso) inz(*sys)
005500170817     d kcod            S                   like(tblcod)
005600170817     d kkey            S                   like(tblkey)
005700170817     d contapb         s              7  0 inz
005800170817     d contapf         s              7  0 inz
005900170817     d contavb         s              7  0 inz
006000170817     d contavf         s              7  0 inz
006100170817     d
006200080320     ititas000      01
006300980611      * ---------------------------------------------------------
006400160928     c     *entry        plist
006500170816     c                   parm                    kpjba
006600170817     c                   movel     kpjbu         fnlvtvds
006700170816
006800170817     c     ktab          klist
006900170817     c                   kfld                    codut             1 0
007000170817     c                   kfld                    kcod
007100170817     c                   kfld                    kkey
007200080212     C     Kblp          KLIST
007300170816     C                   KFLD                    blpAAS
007400170816     C                   KFLD                    blpLNP
007500170816     C                   KFLD                    blpNRS
007600170816     C                   KFLD                    blpNSP
007700160928     C     Kblp3         KLIST
007800160928     C                   KFLD                    klnp              3 0
007900160928     C                   KFLD                    kaAS              4 0
008000160928     C                   KFLD                    kmgs              4 0
008100041222     C
008200080212
008300160928     c                   setoff                                       30
008400170816     c                   eval      klnp=vidlnp
008500170816     c                   movel     comela        kaas
008600170816     c                   move      comela        kmgs
008700170816     c
008800170817     c*
008900170817     c                   if        vidvol='SI'
009000170817     c                   seton                                        10
009100170817     c                   endif
009200170817     c                   if        vidpes='SI'
009300170817     c                   seton                                        11
009400170817     c                   endif
009500170816     c
009600170817     c                   if        *in10 or *in11
009700170817     c                   exsr      elabora
009800170817     c                   endif
009900100315     c
010000100315     c                   except    fine
010100100315     c
010200080212     c                   seton                                        lr
010300170817     c* elabora ----------------------------------------------------*
010400170817     c     Elabora       BEGSR
010500160930
010600160930     c     kblp3         SETLL     fnblp31l
010700170816    1c                   if        comela2=0 or comela2=comela
010800170816     c     kblp3         READe     fnblp31l
010900170816     c                   else
011000170817     c     klnp          READe     fnblp31l
011100170816    1c                   endif
011200160930     C
011300170816    1C                   DOW       NOT %EOF(fnblp31l)
011400170817     c* data spedizione > della richiesta --> fine
011500170817    2c                   if        comela2>0 and blpdsp>comela2
011600170817     c                   leave
011700170817    2c                   endif
011800170817     c
011900170816     c*  verifico mittente
012000170816    2c                   if        (blpccm>0 and blpccm=vidccm)or
012100170816     c                             (blpccm=0 and blpksc=vidccm)
012200170816
012300170816     c* Escludo le bolle legate figlie ( rese o dirottate)
012400170817    3c     kblp          chain     fnlbl01l
012500170816     c                   if        not %found(fnlbl01l)
012600170816     c                   clear                   savpkb
012700170816     c                   clear                   savpkf
012800170816     c                   clear                   savvlb
012900170816     c                   clear                   savfvb
013000170816     c                   clear                   savvlf
013100170816     c                   clear                   savfvf
013200170816     c
013300170816     c* Aggiorno sempre il BOLLETTATO
013400170817     c* il peso BOLL/VARIATO solo se uguale al bollettato
013500170816    4c                   if        vidpes='SI'
013600170816
013700170816    5C                   IF        BLPPKC>0 AND BLPncp=BLPNCL AND
013800170816     C                             vIDPES3='S'
013900170816
014000170816    6c                   if        blppkb=blppkf
014100170816     c                   eval      savpkf=blppkc
014200170816     c                   eval      savpkb=blppkc
014300170816     c                   else
014400170816     c                   eval      savpkb=blppkc
014500170816    6c                   endif
014600170816
014700170816   x5C                   ELSE
014800170816
014900170816    6c                   if        vidpes1>0
015000170816    7c                   if        blppkb=blppkf
015100170816     c                   eval      savpkf=vidpes1
015200170816     c                   eval      savpkb=vidpes1
015300170816   x7c                   else
015400170816     c                   eval      savpkb=vidpes1
015500170816    7c                   endif
015600170816
015700170816   x6c                   else
015800170816     c* / 0
015900170816    7c                   if        blppkb=blppkf
016000170817     c                   eval(h)   SAVpkf   =blppkf/10
016100170817     c                   eval(h)   SAVpkb   =blppkb/10
016200170816     c                   else
016300170817     c                   eval(h)   SAVpkb   =blppkb/10
016400170816    7c                   endif
016500170816
016600170816    6c                   endif
016700170816    5c                   endif
016800170816     c
016900170817     C*
017000170816    4c                   endif
017100160930     c
017200170816    4c                   if        vidvol='SI'
017300170817     c* SEMPRE CON VOLUME REALE
017400170817   4ac                   if        vidvol3='S'
017500170816
017600170817    5c                   if        blpvlf=blpvlb
017700170816     c                   eval      savvlf=vidvol1
017800170816     c                   eval      savfvf='R'
017900170816     c                   eval      savvlb=vidvol1
018000170816     c                   eval      savfvb='R'
018100170816     c                   else
018200170816     c                   eval      savpkb=vidvol1
018300170816     c                   eval      savfvb='R'
018400170817    5c                   endif
018500170817
018600170817  x4ac                   else
018700170817     C* ALTRI CASI
018800170817     C* VDL SE PRESENTE
018900170817    5c                   if        vidvol4='S' and
019000170817     C                             BLPvlC>0 AND BLPncr=BLPNCL
019100170816
019200170817    6c                   if        blpvlf=blpvlb
019300170816     c                   eval      savvlf=blpvlc
019400170816     c                   eval      savfvf='R'
019500170816     c                   eval      savvlb=blpvlc
019600170816     c                   eval      savfvb='R'
019700170816     c                   else
019800170816     c                   eval      savvlb=blppkc
019900170816     c                   eval      savfvb='R'
020000170816    6c                   endif
020100170817    5c                   endif
020200170816     c* Reale
020300170817    5C                   if        blpfvb='R' and vidvol6='S'
020400170817     C                             AND SAVVLB=0
020500170817     C
020600170817    6c                   if        blpvlf=blpvlb
020700170816     c                   eval      savvlf=vidvol1
020800170816     c                   eval      savfvf='R'
020900170817     c                   eval      savvlb=vidvol1
021000170816     c                   eval      savfvb='R'
021100170816     c                   else
021200170816     c                   eval      savvlb=vidvol1
021300170816     c                   eval      savfvb='R'
021400170816    6c                   endif
021500170817    5c                   endif
021600170816
021700170817    5C                   if        blpfvb='P' or blpfvb='K'
021800170817     c                             and vidvol5='S'
021900170817     C                             AND SAVVLB=0
022000170817     c* calcolo il tipo network tariffa
022100170817     c*
022200170817     c                   clear                   trul27ds1
022300170817     c                   Z-Add     blppkb        I27Pkg
022400170817     c                   movel     blptsp        i27tsp
022500170817     c                   movel     blpctr        i27ctb
022600170817     c                   z-add     blplnp        i27lnp
022700170817     c                   z-add     blplna        i27lna
022800170817    6c                   if        blpccm>0
022900170817     c                   movel     blpccm        i27cli
023000170817     c                   else
023100170817     c                   movel     blpksc        i27cli
023200170817    6c                   endif
023300170817     c*
023400170817     c                   call      'TRUL27R1'
023500170817     c                   parm                    trul27ds1
023600170817     c
023700170817    6c                   if        o27err=' '
023800160930     c* calcolo il presunto
023900160930     c                   clear                   trulc7ds
024000170817     c                   eval      IC7tFIE = o27fie
024100170817     c                   z-add     blpmgs        ic7dta
024200170816     c                   movel     blpaas        ic7dta
024300170816     c                   eval      IC7PKG  =  blppkf
024400160930     c                   call      'TRULC7R'
024500160930     c                   parm                    trulc7ds
024600160930     c                   clear                   dslv18
024700160930     c                   movel     'L'           d18tla
024800160930     C                   MOVEL     oc7KSCc       D18KSC
024900160930     C                   MOVEL     oc7PRGc       D18PRG
025000160930     C                   MOVEL     oc7CTRc       D18CTR
025100160930     c                   movel     'C'           d18tsp
025200160930     c                   movel     arblna        d18lna
025300160930     c                   z-add     arblnp        d18lnp
025400160930     c                   z-add     arbncl        d18ncl
025500170817     c                   z-add     arbpkb        d18pkf
025600170817     c                   eval      codut=1
025700170817     c                   eval      kcod='3A'
025800170817     c                   movel     blpcbo        kkey
025900170817     c     ktab          chain     tabel
026000170817    7c                   if        %found(tabel00f)
026100170817     c                   eval      ds3a=tbluni
026200170817     c                   else
026300170817     c                   eval      §3atb1='F1'
026400170817    7c                   endif
026500170817     c                   movel     §3atb1        d18tbl
026600160930     C*
026700160930     C                   CALL      'FNLV18R'
026800160930     C                   PARM                    DSLV18
026900160930     C*
027000170817    7c                   if        blpvlf=blpvlb
027100170817     c                   eval      savvlf=d18vlf
027200170817     c                   eval      savfvf=d18fvf
027300170817     c                   eval      savvlb=d18vlf
027400170817     c                   eval      savfvb=d18fvf
027500170817   x7c                   else
027600170817     c                   eval      savvlb=d18vlf
027700170817     c                   eval      savfvb=d18fvf
027800170817    7c                   endif
027900170817    6c                   endif
028000160930     c
028100170817    5c                   endif
028200170817   4ac                   endif
028300170817    4c                   endif
028400160930
028500170817     c** Conteggi
028600170817    4c                   if        savpkb>0
028700170817     c                   eval      contapb=contapb+1
028800170817    4c                   endif
028900170817    4c                   if        savpkf>0
029000170817     c                   eval      contapf=contapf+1
029100170817    4c                   endif
029200170817    4c                   if        savvlb>0
029300170817     c                   eval      contavb=contavb+1
029400170817    4c                   endif
029500170817    4c                   if        savvlf>0
029600170817     c                   eval      contavf=contavf+1
029700170817    4c                   endif
029800170817
029900170817     c                   except    stampa
030000170817     c** AGGIORNAMENTO
030100170817    4c                   if        vidaggio='S'
030200170817     c     kblp          chain     fnarb01l
030300170817    5c                   if        %found(fnarb01l)
030400170817     c                   if        savpkb>0
030500170817     c                   eval      arbpkb=savpkb
030600170817     c                   endif
030700170817     c                   if        arbpkf>0
030800170817     c                   eval      arbpkf=savpkf
030900170817     c                   endif
031000170817     c                   if        savvlb>0
031100170817     c                   eval      arbvlb=savvlb
031200170817     c                   eval      arbfvb=savfvb
031300170817     c                   if        arbfvb=' '
031400170817     c                   eval      arbfvb='R'
031500170817     c                   endif
031600170817     c                   endif
031700170817     c                   if        savvlf>0
031800170817     c                   eval      arbvlf=savvlf
031900170817     c                   eval      arbfvf=savfvf
032000170817     c                   if        arbfvf=' '
032100170817     c                   eval      arbfvf='R'
032200170817     c                   endif
032300170817     c                   endif
032400170817     c
032500170817     c                   update    fnarb000
032600170817    5c                   endif
032700170817
032800170817     c                   setoff                                       01
032900170817     C     Kblp          chain     titas30c
033000170817    5c                   if        %found(titas30c) and *in01
033100170817     c                             and tasfic=' '
033200170817     c                   if        savpkb>0
033300170817     c                   eval      taspkb=savpkb
033400170817     c                   endif
033500170817     c                   if        taspkf>0
033600170817     c                   eval      taspkf=savpkf
033700170817     c                   endif
033800170817     c                   if        savvlb>0
033900170817     c                   eval      tasvlb=savvlb
034000170817     c                   eval      tasfvb=savfvb
034100170817     c                   if        tasfvb=' '
034200170817     c                   eval      tasfvb='R'
034300170817     c                   endif
034400170817     c                   endif
034500170817     c                   if        savvlf>0
034600170817     c                   eval      tasvlf=savvlf
034700170817     c                   eval      tasfvf=savfvf
034800170817     c                   if        tasfvf=' '
034900170817     c                   eval      tasfvf='R'
035000170817     c                   endif
035100170817     c                   endif
035200170817     c                   update    titas000
035300170817    5c                   endif
035400160930     c
035500160930     c     blpnrr        chain     fnblp00f
035600170817    5c                   if        %found
035700170817     c                   if        savpkb>0
035800170817     c                   eval      blppkb=savpkb
035900170817     c                   endif
036000170817     c                   if        savpkf>0
036100170817     c                   eval      blppkf=savpkf
036200170817     c                   endif
036300170817     c                   if        savvlb>0
036400170817     c                   eval      blpvlb=savvlb
036500170817     c                   eval      blpfvb=savfvb
036600170817     c                   if        blpfvb=' '
036700170817     c                   eval      blpfvb='R'
036800170817     c                   endif
036900170817     c                   endif
037000170817     c                   if        savvlf>0
037100170817     c                   eval      blpvlf=savvlf
037200170817     c                   eval      blpfvf=savfvf
037300170817     c                   if        blpfvf=' '
037400170817     c                   eval      blpfvf='R'
037500170817     c                   endif
037600170817     c                   endif
037700170817     c
037800160930     c                   update    fnblp0fis
037900160930
038000170817    5c                   endif
038100160930    4c                   endif
038200170817    3c                   endif
038300170817    2c                   endif
038400160930     c
038500170817    1c                   if        comela2=0 or comela2=comela
038600170817     c     kblp3         READe     fnblp31l
038700170817     c                   else
038800170817     c     klnp          READe     fnblp31l
038900170817    1c                   endif
039000170817    1c                   enddo
039100160930
039200160930     c                   ENDSR
039300160930     c
039400100315     OQSYSPRT   E            fine        1
039500160929     O                                              'Totale bolle aggiornate'
039600170817     O               11                       +   1 'PESO Boll'
039700170817     O               11      contapb       2  +   1
039800170817     O               11                       +   1 'PESO Modif'
039900170817     O               11      contapf       2  +   1
040000170817     O               10                       +   1 'VOL. Boll'
040100170817     O               10      contavb       2  +   1
040200170817     O               10                       +   1 'VOL. Modif'
040300170817     O               10      contavf       2  +   1
040400170817     o
040500100315     OQSYSPRT   E            STAMPA      1
040600170817     O                       blpaas           +   0
040700170817     O                       blplnp           +   1
040800170817     O                       blpnrs        z  +   1
040900170817     O                       blpnsp        z  +   1
041000170817     O                       blpmgs           +   1 '  /  '
041100170817     O                       blppkb        4  +   1
041200170817     O                       blppkf        4  +   1
041300170817     o               11                       +  60 'PESO SOST'
041400170817     O               11      savpkb        4  +   1
041500170817     O               11      savpkf        4  +   1
041600170817     OQSYSPRT   E            STAMPA      1
041700170817     o                                           60 'Vol Boll/Mod/VDL'
041800170817     O                       blpfvb           +   1
041900170817     O                       blpvlb        4  +   1
042000170817     O                       blpfvf           +   1
042100170817     O                       blpvlF        4  +   1
042200170817     O                       blpvlc        4  +   1
042300170817     O                       blpncp        4  +   1
042400170817     o               10                       +   1 'VOL SOST'
042500170817     O               10      savfvb           +   1
042600170817     O               10      savvlb        4  +   1
042700170817     O               10      savfvf           +   1
042800170817     O               10      savvlf        4  +   1
