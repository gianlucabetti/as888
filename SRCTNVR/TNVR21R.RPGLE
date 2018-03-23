000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200100420     H* TNVR21R *----------------------------------------------------*
000300100420     H* PULIZIA file note
000400000000     H*--------------------------------------------------------------*
000500100420     FTNcpo01L  iF   E           K DISK
000600100420     FTNvis05l  iF   E           K DISK
000700100420     FTNtam01L  iF   E           K DISK
000800100420     FTNofm01L  iF   E           K DISK    rename(tntam000:tnofm000)
000900100420     FTFNTC01L  iF   E           K DISK
001000100420     F                                     INFDS(ntc1)
001100100420     FTFNTC00f  uF   E             DISK    rename(tfntc: tfntc000)
001200100420     Fcnaco00f  iF   E           K DISK
001300020325     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001400020325      *
001500020910     D tibs34ds      E DS                  inz
001600020910     D ddatiute      e ds
001700020910     D azuteds       e ds                  extname(AZUTE00F)
001800020910      *
001900100420     D ntc1            DS
002000100420     D  ntcNRR               397    400B 0
002100100429     d
002200100429     d DataSYS         s               d   inz(*sys)
002300100429     d Dataiso         s               d   datfmt(*iso)
002400100429     d Dataymd         s               d   datfmt(*ymd)
002500100429     d Alfaoggi        s              8
002600100429     d W0060           s              6  0
002700921027     C*****************************************************************
002800100420     c     *entry        plist
002900100420     c                   parm                    wdelete           1
003000020325      *
003100020910     c     *dtaara       define    §azute        azuteds
003200020910     c     *dtaara       define    §datiute      ddatiute
003300020910     C                   in(E)     *dtaara
003400020910     C                   IF        %error  or RSUT = *blanks
003500020910     C                   CLEAR                   tibs34ds
003600020910     C                   CALL      'TIBS34R'
003700020910     C                   PARM                    tibs34ds
003800020910     C                   in        *dtaara
003900020910     c                   endif
004000100429     c
004100100429     c                   eval      dataiso=datasys
004200100429     c                   eval      dataiso=dataymd
004300100429     c                   eval      w0060=%dec(dataymd)
004400020910      *
004500100420     C                   Except    TESNTC
004600100420     c
004700100420     c                   z-add     1             kkut              1 0
004800100420     c                   z-add     151           kkcc              4 0
004900020111      *
005000020111      * NOTE
005100100420     C                   MOVE      'C'           NTCAPL
005200020111     C     NTCAPL        SETLL     TFNTC01L
005300020111     C     NTCAPL        READE     TFNTC01L                               30
005400020114      * 30 on - Fine lettura
005500100420    1C     *IN30         DOWEQ     '0'
005600100429     c                   if        ntcntr<w0060
005700100420     c* prima controllo se esiste il codice cliente
005800100420     c                   move      ntcnk1        kksc              7 0
005900100420     c     kaco          chain     cnaco00f
006000100420    2c                   if        not %found(cnaco00f)
006100100420     C                   Except    DETM
006200100420     c                   add       1             totm              7 0
006300100420     c
006400100420     c                   if        wdelete='S'
006500100420     C     ntcnrr        DELETE    TFNTC000
006600100420     C                   ENDIF
006700100420
006800100420   x2c                   else
006900100420     c
007000020114      *
007100100420      * elaboro solo note con cod tariffa
007200100420    3c                   if        ntcnk2<>'    '
007300100420     c* Verifico sulle tariffe se c'e'
007400100420     c                   movel     ntcnk2        kctr              3 0
007500100420     c
007600100420     C     ktam          CHAIN     TNtam01L                           31
007700100420    4C     *IN31         IFEQ      '1'
007800100420     C                   Except    DETC
007900100420     c                   add       1             totc              7 0
008000100420
008100100420     c                   if        wdelete='S'
008200100420     C     ntcnrr        DELETE    TFNTC000
008300100420     C                   ENDIF
008400100420
008500100420    4C                   ENDIF
008600100421     c*
008700100421   x3c                   else
008800100421     c* Per le note 10 stampo se il cliente non ha più tariffe
008900100421    4c                   if        ntctnt='10'
009000100421     c     kksc          chain     tntam01l
009100100421    5c                   if        not %found(tntam01l)
009200100421     C                   Except    DET10
009300100421     c                   add       1             tot10             7 0
009400100429
009500100429     c                   if        wdelete='S'
009600100429     C     ntcnrr        DELETE    TFNTC000
009700100429     C                   ENDIF
009800100429
009900100421    5C                   ENDIF
010000100421    4C                   ENDIF
010100100421     c
010200100421    3C                   ENDIF
010300100420    2C                   ENDIF
010400100429    2C                   ENDIF
010500100408     c
010600020111     C     NTCAPL        READE     TFNTC01L                               30
010700020111     C                   ENDDO
010800100420
010900100420     C                   MOVE      'P'           NTCAPL
011000100420     C     NTCAPL        SETLL     TFNTC01L
011100100420     C     NTCAPL        READE     TFNTC01L                               30
011200100420      * 30 on - Fine lettura
011300100420     C     *IN30         DOWEQ     '0'
011400100429     c* Deleto tutte le note 10
011500100429    4c                   if        ntctnt='10'
011600100429     c                   if        wdelete='S'
011700100429     C     ntcnrr        DELETE    TFNTC000
011800100429     C                   ENDIF
011900100429     c                   else
012000100429
012100100429     c                   if        ntcntr<w0060
012200100429     c* prima controllo se esiste il codice potenziale
012300100420     c                   move      ntcnk1        kcpo             11 0
012400100420     c     kcpo          chain     tncpo01l
012500100420     c                   if        not %found(tncpo01l)
012600100420     C                   Except    DETP
012700100420     c                   add       1             totP              7 0
012800100420     c
012900100420     c                   if        wdelete='S'
013000100420     C     ntcnrr        DELETE    TFNTC000
013100100420     C                   ENDIF
013200100420     c                   endif
013300100429     c                   endif
013400100429     c
013500100429     c                   endif
013600100420
013700100420     C     NTCAPL        READE     TFNTC01L                               30
013800100420     c                   enddo
013900020114      *
014000100420
014100100420     C                   MOVE      'V'           NTCAPL
014200100420     C     NTCAPL        SETLL     TFNTC01L
014300100420     C     NTCAPL        READE     TFNTC01L                               30
014400100420      * 30 on - Fine lettura
014500100420    1C     *IN30         DOWEQ     '0'
014600100429     c                   if        ntcntr<w0060
014700100420     c* prima controllo se esiste il codice cliente
014800100420     c                   move      ntcnk1        kksc
014900100420     c     kksc          chain     tnvis05l
015000100420    2c                   if        not %found(tnvis05l)
015100100420     C                   Except    DETV
015200100420     c                   add       1             totV              7 0
015300100420     c
015400100420     c                   if        wdelete='S'
015500100420     C     ntcnrr        DELETE    TFNTC000
015600100420     C                   ENDIF
015700100420   x2c                   else
015800100420      * elaboro solo note con cod tariffa
015900100420    3c                   if        ntcnk2<>'    '
016000100420     c* Verifico sulle tariffe se c'e'
016100100420     c                   movel     ntcnk2        kctr              3 0
016200100420     c
016300100420     C     ktam          CHAIN     TNofm01L                           31
016400100420    4C     *IN31         IFEQ      '1'
016500100420     C                   Except    DETVC
016600100420     c                   add       1             totVC             7 0
016700100420
016800100420     c                   if        wdelete='S'
016900100420     C     ntcnrr        DELETE    TFNTC000
017000100420     C                   ENDIF
017100100420
017200100420    4C                   ENDIF
017300100420    3C                   ENDIF
017400100420    2C                   ENDIF
017500100429    2C                   ENDIF
017600100420
017700100420     C     NTCAPL        READE     TFNTC01L                               30
017800100420    1c                   enddo
017900100420     C                   Except    FINE
018000020114     C                   SETON                                        LR
018100020114      *
018200020128      *CAT
018300100420     C     Ktam          KLIST
018400100420     C                   KFLD                    kksc
018500100420     C                   KFLD                    kctr
018600100420     C     Kaco          KLIST
018700100420     C                   KFLD                    kkut
018800100420     C                   KFLD                    kkcc
018900100420     C                   KFLD                    kksc
019000100420     c
019100100420     OQSYSPRT   E            TESNTC         2 02
019200100420     O                                         + 10 'controllo note con COD'
019300100420     O                                         +  1 'TARIFFA'
019400100420     O                                         +  8 'TNVR21R'
019500020325     O                       UDATE             +  5 '  /  /  '
019600020325     O                                         +  5 'PAG.'
019700020325     O                       PAGE1         Z
019800100420     O          E            detM           1
019900100420     O                       ntcnk1
020000100420     O                       ntcnk2            +  1
020100100420     O                       ntctnt            +  1
020200100420     O                       ntcrnt            +  1
020300100420     O                       ntcntr            +  1 '  /  /  '
020400100420     O                                         +  1 'MANCA Aco'
020500100420     O          E            detC           1
020600100420     O                       ntcnk1
020700100420     O                       ntcnk2            +  1
020800100420     O                       ntctnt            +  1
020900100420     O                       ntcrnt            +  1
021000100420     O                       ntcntr            +  1 '  /  /  '
021100100420     O                                         +  1 'MANCA Cod Tariffa'
021200100420     O          E            detP           1
021300100420     O                       ntcnk1
021400100420     O                       ntcnk2            +  1
021500100420     O                       ntctnt            +  1
021600100420     O                       ntcrnt            +  1
021700100420     O                       ntcntr            +  1 '  /  /  '
021800100420     O                                         +  1 'MANCA potenziale'
021900100420     O          E            detV           1
022000100420     O                       ntcnk1
022100100420     O                       ntcnk2            +  1
022200100420     O                       ntctnt            +  1
022300100420     O                       ntcrnt            +  1
022400100420     O                       ntcntr            +  1 '  /  /  '
022500100420     O                                         +  1 'MANCA visita'
022600100420     O          E            detVC          1
022700100420     O                       ntcnk1
022800100420     O                       ntcnk2            +  1
022900100420     O                       ntctnt            +  1
023000100420     O                       ntcrnt            +  1
023100100420     O                       ntcntr            +  1 '  /  /  '
023200100420     O                                         +  1 'MANCA offerta'
023300100421     O          E            det10          1
023400100421     O                       ntcnk1
023500100421     O                       ntcnk2            +  1
023600100421     O                       ntctnt            +  1
023700100421     O                       ntcrnt            +  1
023800100421     O                       ntcntr            +  1 '  /  /  '
023900100421     O                                         +  1 'nota 10 ma cli senza TAR'
024000100420     O          E            FINE           1
024100100420     O                                         +  1 'TOTALE MANCA ACO'
024200100420     O                       totm          2  +   1
024300100420     O                                         +  1 'TOTALE MANCA TAM'
024400100420     O                       totc          2  +   1
024500100420     O                                         +  1 'TOTALE MANCA POT'
024600100420     O                       totp          2  +   1
024700100420     O                                         +  1 'TOTALE MANCA VIS'
024800100420     O                       totv          2  +   1
024900100420     O          E            FINE           1
025000100420     O                                         +  1 'TOTALE MANCA OFF'
025100100420     O                       totvc         2  +   1
025200100421     O                                         +  1 'TOTALE 10 no TAR'
025300100421     O                       tot10         2  +   1
