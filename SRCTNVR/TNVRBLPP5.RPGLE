000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300160928     fazorg01l  iF   E           K DISK
000400160928     fFNarb27l  iF   E           K DISK    infds(dsarb)
000500160930     fFNblp31l  iF   E           K DISK    infds(dsblp)
000600160930     f                                     rename(fnblp000:fnblp031)
000700160930     f                                     prefix(ARB:3)
000800160928     fFNarb00f  uF   E             DISK    rename(fnarb000:fnarb0fis)
000900160930     fFNblp00f  uF   E             DISK    rename(fnblp000:fnblp0fis)
001000100315     ftitas30c  uF   E           K DISK
001100100315     ffnblp01l  uF   E           K DISK
001200080320     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001300080320     d
001400160928     dsavvol           s                   like(blpvlf)
001500160928     dsavflg           s              1
001600160928     d og143         E ds
001700160928     D* DS PER FNLV18R - CALCOLO DI UN VOLUME AUTOMATICO
001800160928     D DSLV18        E DS                  EXTNAME(FNLV18DS)
001900160928     D trulc7ds      E DS
002000100315     d
002100160928     D dsarb           DS
002200160928     D  arbNRR               397    400B 0
002300160930     D dsblp           DS
002400160930     D  blpNRR               397    400B 0
002500160930     d
002600160930     d Dataiso         s               d   datfmt(*iso) inz(*sys)
002700080320     ititas000      01
002800980611      * ---------------------------------------------------------
002900160928     c     *entry        plist
003000160928     c                   parm                    aggior            1
003100080212     C     Kblp          KLIST
003200160928     C                   KFLD                    arbAAS
003300160928     C                   KFLD                    arbLNP
003400160928     C                   KFLD                    arbNRS
003500160928     C                   KFLD                    arbNSP
003600160928     C     Kblp3         KLIST
003700160928     C                   KFLD                    klnp              3 0
003800160928     C                   KFLD                    kaAS              4 0
003900160928     C                   KFLD                    kmgs              4 0
004000160928     C     Kblp2         KLIST
004100160928     C                   KFLD                    klnp              3 0
004200160928     C                   KFLD                    kaAS              4 0
004300041222     C
004400160928     c                   eval      kaas=2016
004500160928     c                   clear                   conta             9 0
004600160929     c                   clear                   contam            9 0
004700080212
004800160928     c                   clear                   klnp              3 0
004900160928     c                   setoff                                       30
005000160928    0c                   dow       not *in30
005100160928     c     klnp          setgt     fnarb27l
005200160928     c                   read      fnarb27l                               30
005300160928     c
005400160928
005500160928    1c                   if        not *in30
005600160928     c                   clear                   og143
005700160928     c     arblnp        chain     azorg01l
005800160928    2c                   if        %found(azorg01l)
005900160928     c                   movel     orgde3        og143
006000160928    2c                   endif
006100160928     c                   eval      klnp=arblnp
006200160928
006300160928    2c                   if        §ogntw='DPD'
006400161006     c                   eval      kmgs=1001
006500041222     C
006600160928     c     kblp3         SETLL     fnarb27l
006700160928     c     kblp2         READe     fnarb27l
006800041222     C
006900161006    3C                   DOW       NOT %EOF(fnarb27l) and arbmgs<=1031
007000160928     c
007100160928     c* Se il volume da fatturare è reale, riaggiorno
007200160928     c                   clear                   savvol
007300160928     c                   clear                   savflg
007400161006    4c                   if        arbfvf='R'  and arbcbo<>'$ '
007500161006     c                             and arbfbr<>'S'
007600160929     c*  se volume FAT <> volume BOL  conteggio e non modifico
007700160929   4ac                   if        arbvlf<>arbvlb
007800160929     c                   eval      contam=contam+1
007900160929     c                   else
008000160928     c* aggiorno con volume VDL se c'è
008100160928    5c                   if        arbvlc>0
008200160928     c                   eval      savvol=arbvlc
008300160928     c                   eval      savflg='R'
008400160928    5c                   else
008500160928     c* calcolo il presunto
008600160928     c                   clear                   trulc7ds
008700160928     c                   eval      IC7tFIE = 'D'
008800160928     c                   z-add     arbmgs        ic7dta
008900160928     c                   movel     arbaas        ic7dta
009000160928     c                   eval      IC7PKG  =  arbpkf
009100160928     c                   call      'TRULC7R'
009200160928     c                   parm                    trulc7ds
009300160928     c                   clear                   dslv18
009400160928     c                   movel     'L'           d18tla
009500160928     C                   MOVEL     oc7KSCc       D18KSC
009600160928     C                   MOVEL     oc7PRGc       D18PRG
009700160928     C                   MOVEL     oc7CTRc       D18CTR
009800160928     c                   movel     'C'           d18tsp
009900160928     c                   movel     arblna        d18lna
010000160928     c                   z-add     arblnp        d18lnp
010100160928     c                   z-add     arbncl        d18ncl
010200160928     c                   z-add     arbpkf        d18pkf
010300160928     c                   movel     'A2'          d18tbl
010400160928     C*
010500160928     C                   CALL      'FNLV18R'
010600160928     C                   PARM                    DSLV18
010700160928     C*
010800160928     C                   MOVEL     D18FVF        savflg
010900160928     C                   MOVEL     D18VLF        savvol
011000160928     c
011100160928    5c                   endif
011200160928
011300160928    5c                   if        savvol>0
011400160928     c                   eval      conta=conta+1
011500160928
011600160928     c                   except    stampa
011700160929     c
011800160929    6c                   if        aggior= 'S'
011900080320     c
012000160928     c     arbnrr        chain     fnarb00f
012100160928     c                   eval      arbvlb=savvol
012200160928     c                   eval      arbfvb=savflg
012300160928     c                   eval      arbvlf=savvol
012400160928     c                   eval      arbfvf=savflg
012500160928    7c                   if        arbfvf='R'
012600160928     c                   eval      arbfvf='T'
012700160928    7c                   endif
012800160928     c                   update    fnarb0fis
012900100315     c
013000100315
013100100315     C     Kblp          chain     fnblp01l
013200160928     c                   eval      blpvlb=savvol
013300160928     c                   eval      blpfvb=savflg
013400160928     c                   eval      blpvlf=savvol
013500160928     c                   eval      blpfvf=savflg
013600160928    7c                   if        blpfvf='R'
013700160928     c                   eval      blpfvf='T'
013800160928    7c                   endif
013900160928    7c                   if        %found(fnblp01l)
014000100315     c                   update    fnblp000
014100160928    7c                   endif
014200100315     c
014300100315     c                   setoff                                       01
014400100315     C     Kblp          chain     titas30c
014500160928    7c                   if        %found(titas30c) and *in01
014600160928     c                   eval      tasvlb=savvol
014700160928     c                   eval      tasfvb=savflg
014800160928     c                   eval      tasvlf=savvol
014900160928     c                   eval      tasfvf=savflg
015000160928    8c                   if        tasfvf='R'
015100160928     c                   eval      tasfvf='T'
015200160928    8c                   endif
015300100315     c                   update    titas000
015400160928    7c                   endif
015500160928
015600160928    6c                   endif
015700160928    5c                   endif
015800100315
015900160929   4ac                   endif
016000160929    4c                   endif
016100160928     c
016200160928     c     kblp2         READe     fnarb27l
016300160928    3c                   enddo
016400160930     c
016500160930     c                   exsr      soloblp
016600100315    2c                   endif
016700160928    1c                   endif
016800080320     c
016900160928    0c                   enddo
017000080320     c
017100100315     c
017200100315     c                   except    fine
017300100315     c
017400080212     c                   seton                                        lr
017500160930     c* elabora FNBLP da partire -----------------------------------*
017600160930     c     SoloBLP       BEGSR
017700160930     c                   movel     dataiso       dateu             8 0
017800160930     c                   move      dateu         kmgs
017900160930
018000160930     c     kblp3         SETLL     fnblp31l
018100160930     c     kblp2         READe     fnblp31l
018200160930     C
018300160930    3C                   DOW       NOT %EOF(fnblp31l)
018400160930     c
018500160930     c
018600160930     c* Se il volume da fatturare è reale, riaggiorno
018700160930     c                   clear                   savvol
018800160930     c                   clear                   savflg
018900160930    4c                   if        arbfvf='R'  and arbft1=' '
019000161006     c                             and arbcbo<>'$ ' and arbfbr<>'S'
019100160930     c*  se volume FAT <> volume BOL  conteggio e non modifico
019200160930   4ac                   if        arbvlf<>arbvlb
019300160930     c                   eval      contam=contam+1
019400160930     c                   else
019500160930     c* aggiorno con volume VDL se c'è
019600160930    5c                   if        arbvlc>0
019700160930     c                   eval      savvol=arbvlc
019800160930     c                   eval      savflg='R'
019900160930    5c                   else
020000160930     c* calcolo il presunto
020100160930     c                   clear                   trulc7ds
020200160930     c                   eval      IC7tFIE = 'D'
020300160930     c                   z-add     arbmgs        ic7dta
020400160930     c                   movel     arbaas        ic7dta
020500160930     c                   eval      IC7PKG  =  arbpkf
020600160930     c                   call      'TRULC7R'
020700160930     c                   parm                    trulc7ds
020800160930     c                   clear                   dslv18
020900160930     c                   movel     'L'           d18tla
021000160930     C                   MOVEL     oc7KSCc       D18KSC
021100160930     C                   MOVEL     oc7PRGc       D18PRG
021200160930     C                   MOVEL     oc7CTRc       D18CTR
021300160930     c                   movel     'C'           d18tsp
021400160930     c                   movel     arblna        d18lna
021500160930     c                   z-add     arblnp        d18lnp
021600160930     c                   z-add     arbncl        d18ncl
021700160930     c                   z-add     arbpkf        d18pkf
021800160930     c                   movel     'A2'          d18tbl
021900160930     C*
022000160930     C                   CALL      'FNLV18R'
022100160930     C                   PARM                    DSLV18
022200160930     C*
022300160930     C                   MOVEL     D18FVF        savflg
022400160930     C                   MOVEL     D18VLF        savvol
022500160930     c
022600160930    5c                   endif
022700160930
022800160930    5c                   if        savvol>0
022900160930     c                   eval      conta=conta+1
023000160930
023100160930     c                   except    stampa
023200160930     c
023300160930    6c                   if        aggior= 'S'
023400160930     c
023500160930     c     blpnrr        chain     fnblp00f
023600160930     c                   eval      blpvlb=savvol
023700160930     c                   eval      blpfvb=savflg
023800160930     c                   eval      blpvlf=savvol
023900160930     c                   eval      blpfvf=savflg
024000160930    7c                   if        blpfvf='R'
024100160930     c                   eval      blpfvf='T'
024200160930    7c                   endif
024300160930     c                   update    fnblp0fis
024400160930
024500160930    6c                   endif
024600160930    5c                   endif
024700160930
024800160930   4ac                   endif
024900160930    4c                   endif
025000160930     c
025100160930     c     kblp2         READe     fnblp31l
025200160930    3c                   enddo
025300160930
025400160930     c                   ENDSR
025500160930     c
025600100315     OQSYSPRT   E            fine        1
025700160929     O                                              'Totale bolle aggiornate'
025800100315     O                       conta         2  +   1
025900160929     O                                        +   1 'Totale bolle con vol. R'
026000160929     O                                        +   1 'modificato in arr'
026100160929     O                       contam        2  +   1
026200100315     OQSYSPRT   E            STAMPA      1
026300160928     O                       arblnp           +   0
026400160928     O                       arbnrs        z  +   1
026500160928     O                       arbnsp        2  +   1
026600160928     O                       arbmgs           +   1 '  /  '
026700160928     O                       arbncl        z  +   1
026800160928     o                                        +   2 'VOL da SOST'
026900160928     O                       savflg           +   1
027000160928     O                       savvol        4  +   1
027100160928     o                                        +   2 'Vol Boll/Fatt/VDL'
027200160928     O                       arbfvb           +   1
027300160928     O                       arbvlb        4  +   1
027400160928     O                       arbfvf           +   1
027500160928     O                       arbvlF        4  +   1
027600160928     O                       arbvlc        4  +   1
027700160928     O                       arbncp        4  +   1
