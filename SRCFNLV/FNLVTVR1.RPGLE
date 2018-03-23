000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300170816      * Aggiorna pesi e volumi bolle errati
000400170816      * ---------------------------------------------------------
000500160930     fFNblp31l  iF   E           K DISK    infds(dsblp)
000600160930     f                                     rename(fnblp000:fnblp031)
000700170817     fFNarb01l  uF   E           k DISK
000800160930     fFNblp00f  uF   E             DISK    rename(fnblp000:fnblp0fis)
000900170818     ftitas30c  uF   E           K DISK    usropn extfile(WTITAS)
001000170816     ffnlbl01l  iF   E           K DISK
001100170818     ffnlbl02l  iF   E           K DISK    rename(fnlbl000:fnlbl002)
001200170817     ftabel00f  iF   E           K DISK
001300170818     ffnblp01l  iF   E           K DISK    infds(dsblp1)
001400170818     FFIAR601L  IF   E           K DISK
001500080320     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001600170818     D riga            S             66    DIM(1) CTDATA PERRCD(1)
001700080320     d
001800170816     dsavvlf           s                   like(blpvlf)
001900170816     dsavfvf           s                   like(blpfvf)
002000170816     dsavvlb           s                   like(blpvlb)
002100170816     dsavfvb           s                   like(blpfvb)
002200170817     dsavpkb           s                   like(blppkb)
002300170817     dsavpkf           s                   like(blppkf)
002400170818     dwtrc             s                   like(ar6trc)
002500170818     doldvlb           s                   like(blpvlb)
002600170818     doldpkb           s                   like(blppkb)
002700170816
002800160928     D* DS PER FNLV18R - CALCOLO DI UN VOLUME AUTOMATICO
002900160928     D DSLV18        E DS                  EXTNAME(FNLV18DS)
003000170818     D DSLV20        E DS                  EXTNAME(FNLV20DS)
003100170817     D ds3a          E DS
003200170817     D kpjba         E DS
003300170817     d
003400170817     D                 DS
003500170817     D  blpaas                 1      4  0
003600170817     D  blpmgs                 5      8  0
003700170817     D  blpdsp                 1      8  0
003800170817
003900170817     D fnlvtvds        DS
004000170816     D  vidlnp                 1      3  0
004100170816     D  vidccm                 4     10  0
004200170816     D  comela                11     18  0
004300170816     D  vidpes                19     20
004400170816     D  vidpes1               21     27  1
004500170817     D  vidpes10              28     28
004600170816     D  vidvol                29     30
004700170816     D  vidvol1               31     35  3
004800170817     D  vidvol3               36     36
004900170816     D  vidaggio              37     37
005000170816     D  comela2               38     45  0
005100170817     D  vidpesVDL             46     46
005200170817     D  vidvolVDL             47     47
005300170817     D  vidvol5               48     48
005400170817     D  vidvol6               49     49
005500170817     D  contapb               50     56  0
005600170816     d
005700160930     D dsblp           DS
005800160930     D  blpNRR               397    400B 0
005900170818     D dsblp1          DS
006000170818     D  blpNRR1              397    400B 0
006100160930     d
006200160930     d Dataiso         s               d   datfmt(*iso) inz(*sys)
006300170817     d kcod            S                   like(tblcod)
006400170817     d kkey            S                   like(tblkey)
006500170817     d contapf         s              7  0 inz
006600170817     d contavb         s              7  0 inz
006700170817     d contavf         s              7  0 inz
006800170818     d WTITAS          s             21    inz('GAITRAGRPS/TITAS30C')
006900170817     d
007000080320     ititas000      01
007100980611      * ---------------------------------------------------------
007200160928     c     *entry        plist
007300170816     c                   parm                    kpjba
007400170817     c                   movel     kpjbu         fnlvtvds
007500170816
007600170817     c     ktab          klist
007700170817     c                   kfld                    codut             1 0
007800170817     c                   kfld                    kcod
007900170817     c                   kfld                    kkey
008000080212     C     Kblp          KLIST
008100170816     C                   KFLD                    blpAAS
008200170816     C                   KFLD                    blpLNP
008300170816     C                   KFLD                    blpNRS
008400170816     C                   KFLD                    blpNSP
008500170818     C     KlblM         KLIST
008600170818     C                   KFLD                    lblAAn
008700170818     C                   KFLD                    lblLpn
008800170818     C                   KFLD                    lblNRn
008900170818     C                   KFLD                    lblNSn
009000160928     C     Kblp3         KLIST
009100160928     C                   KFLD                    klnp              3 0
009200160928     C                   KFLD                    kaAS              4 0
009300160928     C                   KFLD                    kmgs              4 0
009400170818     C     KAR6          KLIST
009500170818     C                   KFLD                    ARBAAS
009600170818     C                   KFLD                    ARBLNP
009700170818     C                   KFLD                    ARBNRS
009800170818     C                   KFLD                    ARBNSP
009900170818     C                   KFLD                    WTRC
010000170818     C                   MOVEL     '1'           WTRC
010100170818      * ---------------------------------------------------------
010200041222     C
010300160928     c                   setoff                                       30
010400170816     c                   eval      klnp=vidlnp
010500170816     c                   movel     comela        kaas
010600170816     c                   move      comela        kmgs
010700170817     c                   clear                   contapb
010800170817     c                   clear                   contapf
010900170817     c                   clear                   contavb
011000170817     c                   clear                   contavf
011100170817     C                   SETOFF                                       101112
011200170817     C                   SETOFF                                       131415
011300170817     C                   SETOFF                                       161718
011400170817     c*
011500170817     c                   if        vidvol='SI'
011600170817     c                   seton                                        10
011700170817     C* SEMPRE
011800170817     C                   IF        VIDVOL3='S'
011900170817     c                   seton                                        15
012000170817     c                   endif
012100170817     C                   IF        VIDVOLvdl='S'
012200170817     c                   seton                                        16
012300170817     c                   endif
012400170817     C                   IF        VIDVOL5='S'
012500170817     c                   seton                                        17
012600170817     c                   endif
012700170817     C                   IF        VIDVOL6='S'
012800170817     c                   seton                                        18
012900170817     c                   endif
013000170817     c                   endif
013100170817
013200170817     c                   if        vidpes='SI'
013300170817     c                   seton                                        11
013400170817     C                   IF        VIDpes10='S'
013500170817     c                   seton                                        14
013600170817     c                   endif
013700170817     C                   IF        VIDpesVDL='S'
013800170817     c                   seton                                        13
013900170817     c                   endif
014000170817     c                   endif
014100170817
014200170817     c                   if        vidaggio='S'
014300170817     c                   seton                                        12
014400170817     c                   endif
014500170818     c*------------------------
014600170817     c                   if        *in10 or *in11
014700170817     c                   exsr      elabora
014800170817     c                   endif
014900170818     c*------------------------
015000100315     c
015100100315     c                   except    fine
015200100315     c
015300170818     c*------------------------
015400170817     c                   movel     fnlvtvds      kpjbu
015500080212     c                   seton                                        lr
015600170818     c* ------------------------------------------------------------*
015700170817     c     Elabora       BEGSR
015800170818     c                   open      titas30c
015900170818     c                   if        not %open(titas30c)
016000170818     c                   eval      %subst(WTITAS:7:4)='GRU '
016100170818     C                   OPEN      titas30c
016200170818     c                   endif
016300160930
016400160930     c     kblp3         SETLL     fnblp31l
016500170816    1c                   if        comela2=0 or comela2=comela
016600170816     c     kblp3         READe     fnblp31l
016700170816     c                   else
016800170817     c     klnp          READe     fnblp31l
016900170816    1c                   endif
017000160930     C
017100170816    1C                   DOW       NOT %EOF(fnblp31l)
017200170817     c* data spedizione > della richiesta --> fine
017300170817    2c                   if        comela2>0 and blpdsp>comela2
017400170817     c                   leave
017500170817    2c                   endif
017600170817     c
017700170816     c*  verifico mittente
017800170816    2c                   if        (blpccm>0 and blpccm=vidccm)or
017900170816     c                             (blpccm=0 and blpksc=vidccm)
018000170817     c* Escludo le bolle di recupero
018100170817     c                   eval      codut=1
018200170817     c                   eval      kcod='3A'
018300170817     c                   movel     blpcbo        kkey
018400170817     c     ktab          chain     tabel
018500170817    3c                   if        %found(tabel00f)
018600170817     c                   eval      ds3a=tbluni
018700170817     c                   else
018800170817     c                   eval      §3atb1='F1'
018900170817    3c                   endif
019000170817
019100170817   2ac                   if        §3arbl<>'R'
019200170816
019300170816     c* Escludo le bolle legate figlie ( rese o dirottate)
019400170817    3c     kblp          chain     fnlbl01l
019500170816     c                   if        not %found(fnlbl01l)
019600170816     c                   clear                   savpkb
019700170816     c                   clear                   savpkf
019800170816     c                   clear                   savvlb
019900170816     c                   clear                   savfvb
020000170816     c                   clear                   savvlf
020100170816     c                   clear                   savfvf
020200170816     c
020300170816     c* Aggiorno sempre il BOLLETTATO
020400170817     c* il peso BOLL/VARIATO solo se uguale al bollettato
020500170816    4c                   if        vidpes='SI'
020600170816
020700170816    5C                   IF        BLPPKC>0 AND BLPncp=BLPNCL AND
020800170817     C                             vIDPESVDL='S'
020900170816
021000170816    6c                   if        blppkb=blppkf
021100170816     c                   eval      savpkf=blppkc
021200170816     c                   eval      savpkb=blppkc
021300170816     c                   else
021400170816     c                   eval      savpkb=blppkc
021500170816    6c                   endif
021600170816
021700170816   x5C                   ELSE
021800170816
021900170816    6c                   if        vidpes1>0
022000170816    7c                   if        blppkb=blppkf
022100170816     c                   eval      savpkf=vidpes1
022200170816     c                   eval      savpkb=vidpes1
022300170816   x7c                   else
022400170816     c                   eval      savpkb=vidpes1
022500170816    7c                   endif
022600170816
022700170816   x6c                   else
022800170816     c* / 0
022900170816    7c                   if        blppkb=blppkf
023000170817     c                   eval(h)   SAVpkf   =blppkf/10
023100170817     c                   eval(h)   SAVpkb   =blppkb/10
023200170816     c                   else
023300170817     c                   eval(h)   SAVpkb   =blppkb/10
023400170816    7c                   endif
023500170816
023600170816    6c                   endif
023700170816    5c                   endif
023800170816     c
023900170817     C*
024000170816    4c                   endif
024100160930     c
024200170816    4c                   if        vidvol='SI'
024300170817     c* SEMPRE CON VOLUME REALE
024400170817   4ac                   if        vidvol3='S'
024500170816
024600170817    5c                   if        blpvlf=blpvlb
024700170816     c                   eval      savvlf=vidvol1
024800170816     c                   eval      savfvf='R'
024900170816     c                   eval      savvlb=vidvol1
025000170816     c                   eval      savfvb='R'
025100170816     c                   else
025200170817     c                   eval      savvlb=vidvol1
025300170816     c                   eval      savfvb='R'
025400170817    5c                   endif
025500170817
025600170817  x4ac                   else
025700170817     C* ALTRI CASI
025800170817     C* VDL SE PRESENTE
025900170817    5c                   if        vidvolvdl='S' and
026000170817     C                             BLPvlC>0 AND BLPncr=BLPNCL
026100170816
026200170817    6c                   if        blpvlf=blpvlb
026300170816     c                   eval      savvlf=blpvlc
026400170816     c                   eval      savfvf='R'
026500170816     c                   eval      savvlb=blpvlc
026600170816     c                   eval      savfvb='R'
026700170816     c                   else
026800170817     c                   eval      savvlb=blpvlc
026900170816     c                   eval      savfvb='R'
027000170816    6c                   endif
027100170817    5c                   endif
027200170816     c* Reale
027300170817    5C                   if        blpfvb='R' and vidvol6='S'
027400170817     C                             AND SAVVLB=0
027500170817     C
027600170817    6c                   if        blpvlf=blpvlb
027700170816     c                   eval      savvlf=vidvol1
027800170816     c                   eval      savfvf='R'
027900170817     c                   eval      savvlb=vidvol1
028000170816     c                   eval      savfvb='R'
028100170816     c                   else
028200170816     c                   eval      savvlb=vidvol1
028300170816     c                   eval      savfvb='R'
028400170816    6c                   endif
028500170817    5c                   endif
028600170816
028700170817    5C                   if        blpfvb='P' or blpfvb='K'
028800170817     c                             and vidvol5='S'
028900170817     C                             AND SAVVLB=0
029000160930     c                   clear                   dslv18
029100170817     c                   z-add     blpmgs        d18mgs
029200170817     c                   movel     blpaas        d18aas
029300160930     c                   movel     'L'           d18tla
029400170817    6c                   if        blpccm>0
029500170817     c                   movel     blpccm        d18ksc
029600170817     c                   else
029700170817     c                   movel     blpksc        d18ksc
029800170817    6c                   endif
029900170817     c                   movel     blpctr        d18ctr
030000170817     c                   movel     blptsp        d18tsp
030100170817     c                   movel     blplna        d18lna
030200170817     c                   z-add     blplnp        d18lnp
030300170817     c                   z-add     blpncl        d18ncl
030400170817     c                   if        savpkb>0
030500170817     c                   z-add     savpkb        d18pkf
030600170817     c                   else
030700170817     c                   z-add     blppkb        d18pkf
030800170817     c                   endif
030900170817     c                   movel     §3atb1        d18tbl
031000160930     C*
031100160930     C                   CALL      'FNLV18R'
031200160930     C                   PARM                    DSLV18
031300160930     C*
031400170817    7c                   if        blpvlf=blpvlb
031500170817     c                   eval      savvlf=d18vlf
031600170817     c                   eval      savfvf=d18fvf
031700170817     c                   eval      savvlb=d18vlf
031800170817     c                   eval      savfvb=d18fvf
031900170817   x7c                   else
032000170817     c                   eval      savvlb=d18vlf
032100170817     c                   eval      savfvb=d18fvf
032200170817    7c                   endif
032300160930     c
032400170817    5c                   endif
032500170817   4ac                   endif
032600170817    4c                   endif
032700160930
032800170818     c* STAMPA  e  AGGIORNAMENTO
032900170818     c                   exsr      Sta_e_Agg
033000170818
033100170817    3c                   endif
033200170817   2ac                   endif
033300170817    2c                   endif
033400160930     c
033500170817    1c                   if        comela2=0 or comela2=comela
033600170817     c     kblp3         READe     fnblp31l
033700170817     c                   else
033800170817     c     klnp          READe     fnblp31l
033900170817    1c                   endif
034000170817    1c                   enddo
034100160930
034200160930     c                   ENDSR
034300170818     c* ----------------------------------------------------------------
034400170818     C     STA_e_AGG     BEGSR
034500170818     c** Conteggi
034600170818    4c                   if        savpkb>0
034700170818     c                   eval      contapb=contapb+1
034800170818    4c                   endif
034900170818    4c                   if        savpkf>0
035000170818     c                   eval      contapf=contapf+1
035100170818    4c                   endif
035200170818    4c                   if        savvlb>0
035300170818     c                   eval      contavb=contavb+1
035400170818    4c                   endif
035500170818    4c                   if        savvlf>0
035600170818     c                   eval      contavf=contavf+1
035700170818    4c                   endif
035800170818
035900170818     c                   clear                   past             23
036000170818     c                   clear                   pmam              1
036100170818    4c                   if        (BLPfvf='Z'and savvlb>0) or (BLPncr>0
036200170818     c                             and BLPncr<BLPncl and savvlf>0)
036300170818    5c                   if        BLPvlc>0
036400170818     c                   eval      past='Ricalcolo per VDL Parz'
036500170818    5c                   endif
036600170818    4c                   endif
036700170818     c
036800170818     c* se bolla MAMMA --> segnalo
036900170818     c     kblp          setll     fnlbl02l
037000170818    4c                   if        %equal(fnlbl02l)
037100170818     c                   eval      pmam='M'
037200170818    4c                   endif
037300170818     c
037400170818     c                   except    stampa
037500170818
037600170818     c** A G G I O R N A M E N T O
037700170818    4c                   if        vidaggio='S'
037800170818     c
037900170818     c                   exsr      AggioARB
038000170818
038100170818     c                   exsr      AggioTAS
038200170818     c
038300170818     c     blpnrr        chain     fnblp00f
038400170818
038500170818    5c                   if        %found
038600170818     c* Prima di aggiornare fnblp, salvo i campi per verificare in caso di
038700170818     c*  bolla legata "mamma", se  anche la figlia necessita degli stessi aggior
038800170818     c                   eval      oldpkb=blppkb
038900170818     c                   eval      oldvlb=blpvlb
039000170818
039100170818     c                   exsr      AggioBLP
039200170818
039300170818     c* Verifico se si tratta di bolla "mamma"
039400170818     c     kblp          chain     fnlbl02l
039500170818    6c                   if        %found(fnlbl02l)
039600170818     c     klblm         chain     fnblp01l
039700170818    7c                   if        %found(fnblp01l)
039800170818    8c                   if        (savpkb>0 and blppkb=oldpkb) or
039900170818     c                             (savvlb>0 and blpvlb=oldvlb)
040000170818     c* Aggiorno anche la bolla figlia
040100170818     c                   eval      pmam='F'
040200170818     c                   except    Stampa
040300170818
040400170818     c                   exsr      Aggfiglia
040500170818    8c                   endif
040600170818    7c                   endif
040700170818    6c                   endif
040800170818     c
040900170818    5c                   endif
041000170818    4c                   endif
041100170818
041200170818     C                   ENDSR
041300170818     c* ----------------------------------------------------------------
041400170818     c     Aggfiglia     BEGSR
041500170818     c                   exsr      AggioARB
041600170818
041700170818     c                   exsr      AggioTAS
041800170818
041900170818     c     blpnrr1       chain     fnblp00f
042000170818
042100170818    5c                   if        %found
042200170818     c                   exsr      AggioBLP
042300170818    5c                   endif
042400170818
042500170818     c                   ENDSR
042600170818     c* ----------------------------------------------------------------
042700170818     c     AggioARB      BEGSR
042800170818     c                   clear                   aggiovdl          1
042900170818     c     kblp          chain     fnarb01l
043000170818    5c                   if        %found(fnarb01l)
043100170818     c                   if        savpkb>0
043200170818     c                   eval      arbpkb=savpkb
043300170818     c                   endif
043400170818     c                   if        savpkf>0
043500170818     c                   eval      arbpkf=savpkf
043600170818     c                   endif
043700170818     c                   if        savvlb>0
043800170818     c                   eval      arbvlb=savvlb
043900170818     c                   eval      arbfvb=savfvb
044000170818     c                   if        arbfvb=' '
044100170818     c                   eval      arbfvb='R'
044200170818     c                   endif
044300170818     c                   endif
044400170818     c                   if        savvlf>0
044500170818     c                   eval      arbvlf=savvlf
044600170818     c                   eval      arbfvf=savfvf
044700170818     c                   if        arbfvf=' '
044800170818     c                   eval      arbfvf='R'
044900170818     c                   endif
045000170818     c                   endif
045100170818     c
045200170818     c* Se presente vdl parziale, verifico se cambia qualche cosa
045300170818    6c                   if        (arbfvf='Z'and savvlb>0) or (arbncr>0
045400170818     c                             and arbncr<arbncl and savvlf>0)
045500170818    7c                   if        arbvlc>0
045600170818     c                   clear                   dslv20
045700170818     C                   MOVEL     'L'           D20TLA
045800170818     C                   MOVEL     'A'           D20TBO
045900170818     C                   Z-ADD(H)  arbvlc        D20VLU
046000170818     C                   Z-ADD(H)  arbncr        D20ncr
046100170818     C*****              MOVEL     'S'           D20FTR
046200170818     C                   MOVEL     'C'           D20TVL
046300170818     c* se il volume da fatturare è "Z", reimposto il volume bollettato
046400170818     c*  se cambiato
046500170818    8c                   if        arbfvf='Z'
046600170818     C                   MOVEL     ARBFVb        D20FVF
046700170818     C                   MOVEL     ARBVLb        D20VLF
046800170818     c                   else
046900170818     C                   MOVEL     ARBFVF        D20FVF
047000170818     C                   MOVEL     ARBVLF        D20VLF
047100170818    8c                   endif
047200170818     C                   MOVEL     ARBFVB        D20FVB
047300170818     C                   MOVEL     ARBVLB        D20VLB
047400170818     C                   MOVEL     ARBAAS        D20AAS
047500170818     C                   MOVEL     ARBLNP        D20LNP
047600170818     C                   MOVEL     ARBNRS        D20NRS
047700170818     C                   MOVEL     ARBNSP        D20NSP
047800170818     C                   MOVEL     ARBCBO        D20CBO
047900170818     C                   MOVEL     ARBNCL        D20NCL
048000170818     C* CHAIN SU FIAR6 PER PRENDERE DATA FATTURA
048100170818     C                   MOVEL     '1'           WTRC
048200170818     C     KAR6          CHAIN     FIAR6000                           30
048300170818     C  N30              MOVEL     AR6DFT        D20DFT
048400170818     C                   MOVEL     ARBLNA        D20LNA
048500170818     C*
048600170818     C                   MOVE      ARBDBR        D20DTE
048700170818     C                   MOVEL     DSLV20        KPJBU
048800170818     C                   CALL      'FNLV20R'
048900170818     C                   PARM                    KPJBA
049000170818
049100170818     C                   MOVEL     KPJBU         DSLV20
049200170818     C*
049300170818     C* SE NON C'E' ERRORE : AGGIORNO LA BOLLA
049400170818    8C     D20ERR        IFNE      '1'
049500170818     C                   MOVEL     D20FVF        ARBFVF
049600170818     C                   MOVEL     D20VLF        ARBVLF
049700170818     c                   eval      aggiovdl='S'
049800170818    8c                   endif
049900170818    7c                   endif
050000170818    6c                   endif
050100170818     c
050200170818     c                   update    fnarb000
050300170818    5c                   endif
050400170818     c                   ENDSR
050500170818     c* ----------------------------------------------------------------
050600170818     c     AggioTAS      BEGSR
050700170818     c                   setoff                                       01
050800170818     C     Kblp          chain     titas30c
050900170818    5c                   if        %found(titas30c) and *in01
051000170818     c                             and tasfic=' '
051100170818     c                   if        savpkb>0
051200170818     c                   eval      taspkb=savpkb
051300170818     c                   endif
051400170818     c                   if        savpkf>0
051500170818     c                   eval      taspkf=savpkf
051600170818     c                   endif
051700170818     c                   if        savvlb>0
051800170818     c                   eval      tasvlb=savvlb
051900170818     c                   eval      tasfvb=savfvb
052000170818     c                   if        tasfvb=' '
052100170818     c                   eval      tasfvb='R'
052200170818     c                   endif
052300170818     c                   endif
052400170818     c                   if        savvlf>0
052500170818     c                   eval      tasvlf=savvlf
052600170818     c                   eval      tasfvf=savfvf
052700170818     c                   if        tasfvf=' '
052800170818     c                   eval      tasfvf='R'
052900170818     c                   endif
053000170818     c                   endif
053100170818     c                   if        aggiovdl='S'
053200170818     C                   MOVEL     D20FVF        tasFVF
053300170818     C                   MOVEL     D20VLF        tasVLF
053400170818     c                   endif
053500170818
053600170818     c                   update    titas000
053700170818    5c                   endif
053800170818     c                   ENDSR
053900170818     c* ----------------------------------------------------------------
054000170818     c     AggioBLP      BEGSR
054100170818     c                   if        savpkb>0
054200170818     c                   eval      blppkb=savpkb
054300170818     c                   endif
054400170818     c                   if        savpkf>0
054500170818     c                   eval      blppkf=savpkf
054600170818     c                   endif
054700170818     c                   if        savvlb>0
054800170818     c                   eval      blpvlb=savvlb
054900170818     c                   eval      blpfvb=savfvb
055000170818     c                   if        blpfvb=' '
055100170818     c                   eval      blpfvb='R'
055200170818     c                   endif
055300170818     c                   endif
055400170818     c                   if        savvlf>0
055500170818     c                   eval      blpvlf=savvlf
055600170818     c                   eval      blpfvf=savfvf
055700170818     c                   if        blpfvf=' '
055800170818     c                   eval      blpfvf='R'
055900170818     c                   endif
056000170818     c                   endif
056100170818     c
056200170818     c                   if        aggiovdl='S'
056300170818     C                   MOVEL     D20FVF        blpFVF
056400170818     C                   MOVEL     D20VLF        blpVLF
056500170818     c                   endif
056600170818     c
056700170818     c                   update    fnblp0fis
056800170818     c                   ENDSR
056900160930     c
057000170817     OQSYSPRT   E            fine        2
057100170817     O               12                          23 'Totale bolle aggiornate'
057200170817     O              n12                          23 'Totale bolle DA AGGIORN'
057300170817     O              n12                       +   0 'ARE'
057400170817     o                                        +   2 'LNP'
057500170817     o                       vidlnp           +   1
057600170817     o                                        +   2 'Mittente'
057700170817     o                       vidccm           +   1
057800170817     o                                        +   2 'dal'
057900170817     o                       comela           +   1 '    /  /  '
058000170817     o                                        +   2 'al'
058100170817     o                       comela2          +   1 '    /  /  '
058200170817     OQSYSPRT   E    11      fine        1
058300170817     O                                        +   0 'PESO  :'
058400170817     O              n13n14   vidpes1       4  +   1
058500170817     O              n13n14                    +   2 'SEMPRE'
058600170817     O               13                       +   0 'VDL se presente-'
058700170817     O               14                       +   2 'diviso / 10'
058800170817     O               13                          88 'negli altri casi    '
058900170817     O               13      vidpes1       4  +   1
059000170817     OQSYSPRT   E    10      fine        1
059100170817     O                                        +   0 'VOLUME:'
059200170817     O               15      vidVOL1       4  +   3
059300170817     O               15                       +   2 'SEMPRE'
059400170817     O               16                       +   0 'VDL se presente-'
059500170817     O               17                       +   2 'Ricalcolo se automatico-'
059600170817     O               18                       +   2 'negli altri casi "R"'
059700170817     O               18      vidVOL1       4  +   1
059800170818     O              n18n15                       95 'Non aggiornamento dei'
059900170818     O              n18n15                    +   1 'Reali'
060000170817     OQSYSPRT   E            fine        1
060100170817     O               11                          15 'PESO Boll'
060200170817     O               11      contapb       2  +   1
060300170817     O               11                       +   1 'PESO Modif'
060400170817     O               11      contapf       2  +   1
060500170817     O               10                       +   1 'VOL. Boll'
060600170817     O               10      contavb       2  +   1
060700170817     O               10                       +   1 'VOL. Modif'
060800170817     O               10      contavf       2  +   1
060900170817     o
061000100315     OQSYSPRT   E            STAMPA      1
061100170817     O                       blpaas           +   0
061200170817     O                       blplnp           +   1
061300170817     O                       blpnrs        z  +   1
061400170817     O                       blpnsp        z  +   1
061500170817     O                       blpmgs           +   1 '  /  '
061600170817     O                       blpcbo           +   1
061700170818     o                                           45 'Peso Boll/Mod/VDL'
061800170817     O                       blppkb        4  +   1
061900170817     O                       blppkf        4  +   1
062000170817     O                       blppkc        4  +   1
062100170817     O                       blpncp        z  +   1
062200170818     o               11                          88 'MODIFICA'
062300170817     O               11      savpkb        4  +   1
062400170817     O               11      savpkf        4  +   1
062500170817     OQSYSPRT   E            STAMPA      1
062600170818     o                                           45 'Vol  Boll/Mod/VDL'
062700170817     O                       blpfvb           +   1
062800170817     O                       blpvlb        4  +   1
062900170817     O                       blpfvf           +   1
063000170817     O                       blpvlF        4  +   1
063100170817     O                       blpvlc        4  +   3
063200170817     O                       blpncr        z  +   1
063300170818     o               10                          88 'MODIFICA'
063400170817     O               10      savfvb           +   1
063500170817     O               10      savvlb        4  +   1
063600170817     O               10      savfvf           +   1
063700170817     O               10      savvlf        4  +   1
063800170818     O                       Pmam             +   1
063900170818     O               10      PAST             +   1
064000170818     o          E            stampa      1
064100170818     o                       riga (1)            66
064200170818     o                       riga (1)           132
064300170818**
064400170818------------------------------------------------------------------
