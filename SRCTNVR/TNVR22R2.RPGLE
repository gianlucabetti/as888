000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020116     H* TNVR22R *----------------------------------------------------*
000300020111     H* PULIZIA ARCHIVI LEGATI A FILE VISITE SENZA RELATIVO RECORD
000400000000     H*--------------------------------------------------------------*
000500100407     FTNVIS06L  IF   E           K DISK
000600020111      * Anagrafiche
000700100407     FTFACO00F  iF   E           K DISK
000800100407     FTFIND00F  iF   E           K DISK
000900100407     FTFCLP00F  iF   E           K DISK
001000020111      * Offerte
001100041202     FTNOFM01L  iF   E           K DISK
001200041202     F                                     INFDS(ofm1)
001300041202     FTIOFD01L  iF   E           K DISK
001400041202     F                                     INFDS(ofd1)
001500041202     FTIOPT01L  iF   E           K DISK
001600041202     F                                     INFDS(opt1)
001700041202     FTIOPD01L  iF   E           K DISK
001800041202     F                                     INFDS(opd1)
001900100408     Ftiogc01L  iF   E           K DISK
002000041202     F                                     INFDS(ogc1)
002100041202
002200100407     FTNOFM00f  iF   E             DISK    rename(tntam000:tnofm000)
002300100408     FTIOFD00f  uF   E             DISK    rename(titad000:tiofd000)
002400100408     FTIOPT00f  uF   E             DISK    rename(titpt000:tiopt000)
002500100408     FTIOPD00f  uF   E             DISK    rename(titpd000:tiopd000)
002600100408     FTIOGC00f  uF   E             DISK    rename(titgc000:tiogc000)
002700100407     FTFNTC00f  iF   E             DISK    rename(tfntc: tfntc000)
002800020111      * Note
002900041202     FTFNTC01L  iF   E           K DISK
003000041206     F                                     INFDS(ntc1)
003100020111      * Cat
003200100408     FTEETC01L  uF   E           K DISK
003300100408     FTEETS01L  uF   E           K DISK
003400100408     FTEETT01L  uF   E           K DISK
003500020325      *
003600020325     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
003700020325      *
003800020910     D tibs34ds      E DS                  inz
003900020910     D ddatiute      e ds
004000020910     D azuteds       e ds                  extname(AZUTE00F)
004100020910      *
004200020325     D$Totale          s              5  0 inz
004300020910     D$TotaleA         s              5  0 inz
004400100408     Dkksc             s                   like(tadksc)
004500100408     Dkctr             s                   like(tadctr)
004600100408     d
004700041202     D ofm1            DS
004800041202     D  ofmNRR               397    400B 0
004900041202     D ofd1            DS
005000041202     D  ofdNRR               397    400B 0
005100041202     D opt1            DS
005200041202     D  optNRR               397    400B 0
005300041202     D opd1            DS
005400041202     D  opdNRR               397    400B 0
005500041202     D ogc1            DS
005600041202     D  ogcNRR               397    400B 0
005700041206     D ntc1            DS
005800041206     D  ntcNRR               397    400B 0
005900921027     C*****************************************************************
006000100408     c     *entry        plist
006100100408     c                   parm                    wdelete           1
006200100408      *
006300020325      *
006400020910     c     *dtaara       define    §azute        azuteds
006500020910     c     *dtaara       define    §datiute      ddatiute
006600020910     C                   in(E)     *dtaara
006700020910     C                   IF        %error  or RSUT = *blanks
006800020910     C                   CLEAR                   tibs34ds
006900020910     C                   CALL      'TIBS34R'
007000020910     C                   PARM                    tibs34ds
007100020910     C                   in        *dtaara
007200020910     c                   endif
007300020910      *
007400100407     C     KACO          KLIST
007500100407     C                   KFLD                    CodUt
007600100407     C                   KFLD                    DutKci
007700100407     C                   KFLD                    VISNRV
007800100408     C     KCAT          KLIST
007900100408     C                   KFLD                    $DSF
008000100408     C                   KFLD                    $CTO
008100100408     C     Kofm          KLIST
008200100408     C                   KFLD                    kksc
008300100408     C                   KFLD                    kctr
008400100408      * DEFINIZIONE CAMPI
008500100408     C     *LIKE         DEFINE    ETCDSF        $DSF
008600100408     C     *LIKE         DEFINE    ETCCTO        $CTO
008700100407     c
008800100407     C                   Z-ADD     1             CODUT             1 0
008900100408     c
009000100408     c                   except    tesvis
009100100407
009200100408     c* Legggo OFD OPT OPD senza OFM
009300100407     c
009400100408     C     *LOVAL        SETLL     tiofd01l
009500100408     C                   READ(N)   tiofd01l                               30
009600100407      * 30 on - fine lettura
009700100407    1C     *IN30         DOWEQ     '0'
009800100407
009900100408     c                   eval      kksc=tadksc
010000100408     c                   eval      kctr=tadctr
010100100408     C     kofm          chain     tnofm01l
010200100408     c                   if        not %found(tnofm01l)
010300100408     c                   except    STAOFD
010400100408     c
010500100408     C                   Add       1             $Totale
010600100408     c                   if        wdelete='S'
010700100408     C     ofdnrr        DELETE    TIOFD000
010800100408     C                   ENDIF
010900100407     c                   endif
011000100408     c
011100100408     C                   READ(N)   tiofd01l                               30
011200100408     c                   enddo
011300100408     C     $Totale       ifgt      0
011400100408     C   OF              Except    TESVIS
011500100408     C   OF              setoff                                       OF
011600100408     C                   Except    TESOFD
011700100408     C                   Except    TOTALE
011800100408     C                   z-add     *zeros        $Totale
011900100408     C                   endif
012000100408     c
012100100408     C     *LOVAL        SETLL     tiopd01l
012200100408     C                   READ(N)   tiopd01l                               30
012300100408      * 30 on - fine lettura
012400100408    1C     *IN30         DOWEQ     '0'
012500100408
012600100408     c                   eval      kksc=tpdksc
012700100408     c                   eval      kctr=tpdctr
012800100408     C     kofm          chain     tnofm01l
012900100408     c                   if        not %found(tnofm01l)
013000100408     c                   except    STAOpD
013100100408     c
013200100408     C                   Add       1             $Totale
013300100408     c                   if        wdelete='S'
013400100408     C     opdnrr        DELETE    TIOPD000
013500100408     C                   ENDIF
013600100408     c                   endif
013700100408     c
013800100408     C                   READ(N)   tiopd01l                               30
013900100408     c                   enddo
014000100408
014100100408     C     $Totale       ifgt      0
014200100408     C   OF              Except    TESVIS
014300100408     C   OF              setoff                                       OF
014400100408     C                   Except    TESOPD
014500100408     C                   Except    TOTALE
014600100408     C                   z-add     *zeros        $Totale
014700100408     C                   endif
014800100408     c
014900100408     C     *LOVAL        SETLL     tiopt01l
015000100408     C                   READ(N)   tiopt01l                               30
015100100408      * 30 on - fine lettura
015200100408    1C     *IN30         DOWEQ     '0'
015300100408
015400100408     c                   eval      kksc=tptksc
015500100408     c                   eval      kctr=tptctr
015600100408     C     kofm          chain     tnofm01l
015700100408     c                   if        not %found(tnofm01l)
015800100408     c                   except    STAOpt
015900100408     C                   Add       1             $Totale
016000100408     c                   if        wdelete='S'
016100100408     C     optnrr        DELETE    TIOPT000
016200100408     C                   ENDIF
016300100408     c                   endif
016400100408     c
016500100408     C                   READ(N)   tiopt01l                               30
016600100408     c                   enddo
016700100408     C     $Totale       ifgt      0
016800100408     C   OF              Except    TESVIS
016900100408     C   OF              setoff                                       OF
017000100408     C                   Except    TESOPt
017100100408     C                   Except    TOTALE
017200100408     C                   z-add     *zeros        $Totale
017300100408     C                   endif
017400100407
017500100408     C     *LOVAL        SETLL     tiogc01l
017600100408     C                   READ(N)   tiogc01l                               30
017700100408      * 30 on - fine lettura
017800100408    1C     *IN30         DOWEQ     '0'
017900100408
018000100408     c                   eval      kksc=tgcksc
018100100408     c                   eval      kctr=tgcctr
018200100408     C     kofm          chain     tnofm01l
018300100408     c                   if        not %found(tnofm01l)
018400100408     c                   except    STAOgc
018500100408     C                   Add       1             $Totale
018600100408     c                   if        wdelete='S'
018700100408     C     ogcnrr        DELETE    TIOGC000
018800100408     C                   ENDIF
018900100408     c                   endif
019000100408     c
019100100408     C                   READ(N)   tiogc01l                               30
019200100408     c                   enddo
019300100408     C     $Totale       ifgt      0
019400100408     C   OF              Except    TESVIS
019500100408     C   OF              setoff                                       OF
019600100408     C                   Except    TESOGC
019700100408     C                   Except    TOTALE
019800100408     C                   z-add     *zeros        $Totale
019900100408     C                   endif
020000100408     c
020100100408     C                   MOVE      'S'           $DSF
020200100408     C                   MOVE      'O'           $CTO
020300100408     C     KCAT          SETLL     TEETC01L
020400100408     C     KCAT          READE     TEETC01L                               30
020500100408      * 30 on - Fine lettura
020600100408     C     *IN30         DOWEQ     '0'
020700100408      *
020800100408     c                   eval      kksc=etcksc
020900100408     c                   eval      kctr=etcctr
021000100408     C     kofm          CHAIN     tnofm01l
021100100408     c                   if        not %found(tnofm01l)
021200100408     c                   except    STAETC
021300100408     C                   Add       1             $Totale
021400100408     c                   if        wdelete='S'
021500100408     C                   DELETE    Teetc000
021600100408     C                   ENDIF
021700100408     c                   endif
021800100408      *
021900100408     C     KCAT          READE     TEETC01L                               30
022000100408     C                   ENDDO
022100100408     C     $Totale       ifgt      0
022200100408     C                   Except    tesetc
022300100408     C                   Except    TOTALE
022400100408     C                   z-add     *zeros        $Totale
022500100408     C                   endif
022600100408     c
022700100408     c
022800100408     C     KCAT          SETLL     TEETs01L
022900100408     C     KCAT          READE     TEETs01L                               30
023000100408      * 30 on - Fine lettura
023100100408     C     *IN30         DOWEQ     '0'
023200100408      *
023300100408     c                   eval      kksc=etsksc
023400100408     c                   eval      kctr=etsctr
023500100408     C     kofm          CHAIN     tnofm01l
023600100408     c                   if        not %found(tnofm01l)
023700100408     c                   except    STAETs
023800100408     C                   Add       1             $Totale
023900100408     c                   if        wdelete='S'
024000100408     C                   DELETE    Teets000
024100100408     C                   ENDIF
024200100408     c                   endif
024300100408      *
024400100408     C     KCAT          READE     TEETs01L                               30
024500100408     C                   ENDDO
024600100408     C     $Totale       ifgt      0
024700100408     C                   Except    tesets
024800100408     C                   Except    TOTALE
024900100408     C                   z-add     *zeros        $Totale
025000100408     C                   endif
025100100408     c
025200100408     C     KCAT          SETLL     TEETt01L
025300100408     C     KCAT          READE     TEETt01L                               30
025400100408      * 30 on - Fine lettura
025500100408     C     *IN30         DOWEQ     '0'
025600100408      *
025700100408     c                   eval      kksc=ettksc
025800100408     c                   eval      kctr=ettctr
025900100408     C     kofm          CHAIN     tnofm01l
026000100408     c                   if        not %found(tnofm01l)
026100100408     c                   except    STAETT
026200100408     C                   Add       1             $Totale
026300100408     c                   if        wdelete='S'
026400100408     C                   DELETE    Teett000
026500100408     C                   ENDIF
026600100408     c                   endif
026700100408      *
026800100408     C     KCAT          READE     TEETt01L                               30
026900100408     C                   ENDDO
027000100408     C     $Totale       ifgt      0
027100100408     C                   Except    tesett
027200100408     C                   Except    TOTALE
027300100408     C                   z-add     *zeros        $Totale
027400100408     C                   endif
027500020114      *
027600020114     C                   SETON                                        LR
027700020114      *
027800020325     OQSYSPRT   E            TESVIS         2 02
027900020325     O                                         + 10 'CANCELLAZIONE RECORDS '
028000100408     O                                         +  1 'offerte senza testata OFM'
028100100408     O                                         +  8 'TNVR22R2'
028200020325     O                       UDATE             +  5 '  /  /  '
028300020325     O                                         +  5 'PAG.'
028400020325     O                       PAGE1         Z
028500100408     O          E            STAOFD         1
028600100408     O                                         +  1 'OFD : '
028700100408     O                       TAdksc            +  0 '      / '
028800100408     O                       tadctr            +  1
028900100408     O                       taddtr            +  1 '    /  /  '
029000100408     O          E            STAOPT         1
029100100408     O                                         +  1 'OPT : '
029200100408     O                       Tptksc            +  0 '      / '
029300100408     O                       tptctr            +  1
029400100408     O                       tptftc            +  1
029500100408     O                       tptduv            +  1 '    /  /  '
029600100408     O          E            STAOPD         1
029700100408     O                                         +  1 'OPD : '
029800100408     O                       Tpdksc            +  0 '      / '
029900100408     O                       tpdctr            +  1
030000100408     O                       tpdftc            +  1
030100100408     O                       tpddtr            +  1 '    /  /  '
030200100408     O          E            STAOGC         1
030300100408     O                                         +  1 'OGC : '
030400100408     O                       Tgcksc            +  0 '      / '
030500100408     O                       tgcctr            +  1
030600100408     O                       tgcdtr            +  1 '    /  /  '
030700100408     O          E            STAETC         1
030800100408     O                                         +  1 'ETC : '
030900100408     O                       etcksc            +  0 '      / '
031000100408     O                       etcctr            +  1
031100100408     O                       etcdtr            +  1 '    /  /  '
031200100408     O          E            STAETS         1
031300100408     O                                         +  1 'ETS : '
031400100408     O                       etsksc            +  0 '      / '
031500100408     O                       etsctr            +  1
031600100408     O                       etsdtr            +  1 '    /  /  '
031700100408     O          E            STAETT         1
031800100408     O                                         +  1 'ETT : '
031900100408     O                       ettksc            +  0 '      / '
032000100408     O                       ettctr            +  1
032100100408     O                       ettdtr            +  1 '    /  /  '
032200020325      *
032300100408     O          E            TESOFD         1
032400100408     O                                         +  1 'TIOFD00F:'
032500100408     O          E            TESOPD         1
032600100408     O                                         +  1 'TIOPD00F:'
032700100408     O          E            TESOPT         1
032800100408     O                                         +  1 'TIOPT00F:'
032900100408     O          E            TESOGC         1
033000100408     O                                         +  1 'TIOGC00F:'
033100100408     O          E            TESETC         1
033200100408     O                                         +  1 'TEETC00F:'
033300100408     O          E            TESETS         1
033400100408     O                                         +  1 'TEETS00F:'
033500100408     O          E            TESETT         1
033600100408     O                                         +  1 'TEETT00F:'
033700100408      *
033800100408     O          E            TOTALE         2
033900100408     O                                         +  1 'TOTALE  :'
034000100408     O                       $Totale       2   +  2
