000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200100929     H* TNVR22R7*----------------------------------------------------*
000300100929     H* PULIZIA ARCHIVI NON LWEGATI A TIVIS
000400000000     H*--------------------------------------------------------------*
000500100929     FTIVIS05L  IF   E           K DISK
000600020111      * Anagrafiche
000700100408     FTFACO00F  uF   E           K DISK
000800100408     FTFIND00F  uF   E           K DISK
000900100408     FTFCLP00F  uF   E           K DISK
001000100408     FTFCLS01l  uF   E           K DISK
001100020111      * Offerte
001200041202     FTNOFM01L  iF   E           K DISK
001300041202     F                                     INFDS(ofm1)
001400041202     FTIOFD01L  iF   E           K DISK
001500041202     F                                     INFDS(ofd1)
001600041202     FTIOPT01L  iF   E           K DISK
001700041202     F                                     INFDS(opt1)
001800041202     FTIOPD01L  iF   E           K DISK
001900041202     F                                     INFDS(opd1)
002000041202     FTIOGC01L  iF   E           K DISK
002100041202     F                                     INFDS(ogc1)
002200041202
002300100408     FTNOFM00f  uF   E             DISK    rename(tntam000:tnofm000)
002400100408     FTIOFD00f  uF   E             DISK    rename(titad000:tiofd000)
002500100408     FTIOPT00f  uF   E             DISK    rename(titpt000:tiopt000)
002600100408     FTIOPD00f  uF   E             DISK    rename(titpd000:tiopd000)
002700100408     FTIOGC00f  uF   E             DISK    rename(titgc000:tiogc000)
003200020111      * Cat
003300100408     FTEETC01L  uF   E           K DISK
003400100408     FTEETS01L  uF   E           K DISK
003401100930     FTEETS11L  uF   E           K DISK
003402100930     F                                     RENAME(TEETS000:TEETS11)
003500100408     FTEETT01L  uF   E           K DISK
003600020325      *
003700020325     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
003800020325      *
003900020910     D tibs34ds      E DS                  inz
004000020910     D ddatiute      e ds
004100020910     D azuteds       e ds                  extname(AZUTE00F)
004200020910      *
004300100930     D$Totale          s              9  0 inz
004500041202     D ofm1            DS
004600041202     D  ofmNRR               397    400B 0
004700041202     D ofd1            DS
004800041202     D  ofdNRR               397    400B 0
004900041202     D opt1            DS
005000041202     D  optNRR               397    400B 0
005100041202     D opd1            DS
005200041202     D  opdNRR               397    400B 0
005300041202     D ogc1            DS
005400041202     D  ogcNRR               397    400B 0
005700921027     C*****************************************************************
005800100408     c     *entry        plist
005900100408     c                   parm                    wdelete           1
006000100408     c                   parm                    soloCAT           1
006100020325      *
006200020910     c     *dtaara       define    §azute        azuteds
006300020910     c     *dtaara       define    §datiute      ddatiute
006400020910     C                   in(E)     *dtaara
006500020910     C                   IF        %error  or RSUT = *blanks
006600020910     C                   CLEAR                   tibs34ds
006700020910     C                   CALL      'TIBS34R'
006800020910     C                   PARM                    tibs34ds
006900020910     C                   in        *dtaara
007000020910     c                   endif
007100020910      *
007200020325     C                   Except    TESVIS
007300100408     c
007400100408     c                   if        soloCAT<>'S'
007500020111      *
007600020111      * ANAGRAFICHE
007700020128      *
007800020128      * TfAco
007900020325     C                   z-add     *zeros        $Totale
008000020327     C                   move      *off          $Prima            1
008100020111     C     *LOVAL        SETLL     TFACO00F
008200041202     C                   READ(N)   TFACO00F                               30
008300020114      * 30 on - fine lettura
008400020111     C     *IN30         DOWEQ     '0'
008500020111      *
008600100929     C     ACOKSC        CHAIN     TIVIS05L                           31
008700020128      *
008800020111     C     *IN31         IFEQ      '1'
008900020325     C   OF              Except    TESVIS
009000020325     C   OF              setoff                                       OF
009100020327      *
009200020327     C     $Prima        ifeq      *off
009300020326     C                   Except    TESACO
009400020327     C                   move      *on           $Prima
009500020327     C                   endif
009600020327      *
009700020325     C                   Except    DELACO
009800020325     C                   Add       1             $Totale
009900100408     c                   if        wdelete='S'
010000100408     C     kaco          DELETE    TFACO00F
010100100408     C                   ENDIF
010200100408     C                   ENDIF
010300020128      *
010400041202     C                   READ(N)   TFACO00F                               30
010500020111     C                   ENDDO
010600020325     C     $Totale       ifgt      0
010700020325     C   OF              Except    TESVIS
010800020325     C   OF              setoff                                       OF
010900020325     C                   Except    TOTALE
011000020325     C                   z-add     *zeros        $Totale
011100020325     C                   endif
011200020128      *
011300020128      * TfClp
011400020128     C     *LOVAL        SETLL     TFCLP00F
011500041202     C                   READ(N)   TFCLP00F                               30
011600020128      * 30 on - fine lettura
011700020128     C     *IN30         DOWEQ     '0'
011800020128      *
011900100929     C     CLPKSC        CHAIN     TIVIS05L                           31
012000020128      *
012100020128     C     *IN31         IFEQ      '1'
012200020325     C                   add       1             $Totale
012300100408     c                   if        wdelete='S'
012400100408     C     kclp          DELETE    TFCLP00F
012500020128     C                   ENDIF
012600100408     C                   ENDIF
012700020128      *
012800041202     C                   READ(N)   TFCLP00F                               30
012900020128     C                   ENDDO
013000020325     C     $Totale       ifgt      0
013100020325     C   OF              Except    TESVIS
013200020325     C   OF              setoff                                       OF
013300020325     C                   Except    TESCLP
013400020325     C                   Except    TOTALE
013500020325     C                   z-add     *zeros        $Totale
013600020325     C                   endif
013700020128      * TfInd
013800020128     C     *LOVAL        SETLL     TFIND00F
013900041202     C                   READ(N)   TFIND00F                               30
014000020128      * 30 on - fine lettura
014100020128     C     *IN30         DOWEQ     '0'
014200020128      *
014300100929     C     INDKSC        CHAIN     TIVIS05L                           31
014400020128      *
014500020128     C     *IN31         IFEQ      '1'
014600020325     C                   Add       1             $Totale
014700100408     c                   if        wdelete='S'
014800100408     C     kind          DELETE    TFIND00F
014900020128     C                   ENDIF
015000100408     C                   ENDIF
015100020128      *
015200041202     C                   READ(N)   TFIND00F                               30
015300020128     C                   ENDDO
015400020325     C     $Totale       ifgt      0
015500020325     C   OF              Except    TESVIS
015600020325     C   OF              setoff                                       OF
015700020325     C                   Except    TESIND
015800020325     C                   Except    TOTALE
015900020325     C                   z-add     *zeros        $Totale
016000020325     C                   endif
016100100407      * Tfcls
016200100407     C     *LOVAL        SETLL     TFcls01l
016300100407     C                   READ(N)   TFcls01l                               30
016400100407      * 30 on - fine lettura
016500100407     C     *IN30         DOWEQ     '0'
016600100407      *
016700100929     C     clsKSC        CHAIN     TIVIS05L                           31
016800100407      *
016900100407     C     *IN31         IFEQ      '1'
017000100407     C                   Add       1             $Totale
017100100408     c                   if        wdelete='S'
017200100408     C     clsksc        DELETE    TFcls01l
017300100407     C                   ENDIF
017400100408     C                   ENDIF
017500100407      *
017600100407     C                   READ(N)   TFcls01l                               30
017700100407     C                   ENDDO
017800100407     C     $Totale       ifgt      0
017900100407     C   OF              Except    TESVIS
018000100407     C   OF              setoff                                       OF
018100100407     C                   Except    TEScls
018200100407     C                   Except    TOTALE
018300100407     C                   z-add     *zeros        $Totale
018400100407     C                   endif
018500020111      *
018600020128      * OFFERTE
018700020128      *
018800020128      * TnOfm
018900020327     C                   move      *off          $Prima
019000020111     C     *LOVAL        SETLL     TNOFM01L
019100020111     C                   READ      TNOFM01L                               30
019200020114      * 30 on - Fine lettura
019300020111     C     *IN30         DOWEQ     '0'
019400020111      *
019500100929     C     TAMKSC        CHAIN     TIVIS05L                           31
019600020111     C     *IN31         IFEQ      '1'
019700020325     C   OF              Except    TESVIS
019800020325     C   OF              setoff                                       OF
019900020327      *
020000020327     C     $Prima        ifeq      *off
020100020327     C                   Except    TESOFM
020200020327     C                   move      *on           $Prima
020300020327     C                   endif
020400020327      *
020500020325     C                   Except    DELOFM
020600020325     C                   Add       1             $Totale
020700100408     c                   if        wdelete='S'
020800100408     C     ofmnrr        DELETE    TNOFM000
020900020111     C                   ENDIF
021000100408     C                   ENDIF
021100020128      *
021200020111     C                   READ      TNOFM01L                               30
021300020111     C                   ENDDO
021400020325     C     $Totale       ifgt      0
021500020327     C   OF              Except    TESVIS
021600020325     C   OF              setoff                                       OF
021700020325     C                   Except    TOTALE
021800020325     C                   z-add     *zeros        $Totale
021900020325     C                   endif
022000020128      *
022100020128      * TiOfd
022200020128     C     *LOVAL        SETLL     TIOFD01L
022300020128     C                   READ      TIOFD01L                               30
022400020128      * 30 on - Fine lettura
022500020128     C     *IN30         DOWEQ     '0'
022600020128      *
022700100929     C     TADKSC        CHAIN     TIVIS05L                           31
022800020128     C     *IN31         IFEQ      '1'
022900020325     C                   Add       1             $Totale
023000100408     c                   if        wdelete='S'
023100100408     C     ofdnrr        DELETE    TIOFD000
023200020128     C                   ENDIF
023300100408     C                   ENDIF
023400020128      *
023500020128     C                   READ      TIOFD01L                               30
023600020128     C                   ENDDO
023700020325     C     $Totale       ifgt      0
023800020325     C   OF              Except    TESVIS
023900020325     C   OF              setoff                                       OF
024000020325     C                   Except    TESOFD
024100020325     C                   Except    TOTALE
024200020325     C                   z-add     *zeros        $Totale
024300020325     C                   endif
024400020128      *
024500020128      * TiOpt
024600020327     C                   move      *off          $Prima
024700020128     C     *LOVAL        SETLL     TIOPT01L
024800020128     C                   READ      TIOPT01L                               30
024900020128      * 30 on - Fine lettura
025000020128     C     *IN30         DOWEQ     '0'
025100020128      *
025200100929     C     TPTKSC        CHAIN     TIVIS05L                           31
025300020128     C     *IN31         IFEQ      '1'
025400020325     C   OF              Except    TESVIS
025500020325     C   OF              setoff                                       OF
025600020327      *
025700020327     C     $Prima        ifeq      *off
025800020327     C                   Except    TESOPT
025900020327     C                   move      *on           $Prima
026000020327     C                   endif
026100020327      *
026200020325     C                   Except    DELOPT
026300020325     C                   Add       1             $Totale
026400100408     c                   if        wdelete='S'
026500100408     C     optnrr        DELETE    TIOPT000
026600020128     C                   ENDIF
026700100408     C                   ENDIF
026800020128      *
026900020128     C                   READ      TIOPT01L                               30
027000020128     C                   ENDDO
027100020325     C     $Totale       ifgt      0
027200020325     C   OF              Except    TESVIS
027300020325     C   OF              setoff                                       OF
027400020325     C                   Except    TOTALE
027500020325     C                   z-add     *zeros        $Totale
027600020325     C                   endif
027700020128      *
027800020128      * TiOpd
027900020128     C     *LOVAL        SETLL     TIOPD01L
028000020128     C                   READ      TIOPD01L                               30
028100020128      * 30 on - Fine lettura
028200020128     C     *IN30         DOWEQ     '0'
028300020128      *
028400100929     C     TPDKSC        CHAIN     TIVIS05L                           31
028500020128     C     *IN31         IFEQ      '1'
028600020325     C                   Add       1             $Totale
028700100408     c                   if        wdelete='S'
028800100408     C     opdnrr        DELETE    TIOPD000
028900020128     C                   ENDIF
029000100408     C                   ENDIF
029100020128      *
029200020128     C                   READ      TIOPD01L                               30
029300020128     C                   ENDDO
029400020325     C     $Totale       ifgt      0
029500020325     C   OF              Except    TESVIS
029600020325     C   OF              setoff                                       OF
029700020327     C                   Except    TESOPD
029800020325     C                   Except    TOTALE
029900020325     C                   z-add     *zeros        $Totale
030000020325     C                   endif
030100020128      *
030200020128      * TiOgc
030300020128     C     *LOVAL        SETLL     TIOGC01L
030400020128     C                   READ      TIOGC01L                               30
030500020128      * 30 on - Fine lettura
030600020128     C     *IN30         DOWEQ     '0'
030700020128      *
030800100929     C     TGCKSC        CHAIN     TIVIS05L                           31
030900020128     C     *IN31         IFEQ      '1'
031000020325     C                   Add       1             $Totale
031100100408     c                   if        wdelete='S'
031200100408     C     ogcnrr        DELETE    TIOGC000
031300020128     C                   ENDIF
031400100408     C                   ENDIF
031500020128      *
031600020128     C                   READ      TIOGC01L                               30
031700020128     C                   ENDDO
031800020325     C     $Totale       ifgt      0
031900020325     C   OF              Except    TESVIS
032000020325     C   OF              setoff                                       OF
032100020325     C                   Except    TESOGC
032200020325     C                   Except    TOTALE
032300020325     C                   z-add     *zeros        $Totale
032400020325     C                   endif
032401100930
032402100930     C                   endif
037000020111      *
037100020111      * CAT
037200020128      *
037300020128      * Teetc
037400020327     C                   move      *off          $Prima
037500100408     C                   MOVE      'S'           $DSF
037600020114     C                   MOVE      'O'           $CTO
037700020128     C     KCAT          SETLL     TEETC01L
037800020128     C     KCAT          READE     TEETC01L                               30
037900020114      * 30 on - Fine lettura
038000020111     C     *IN30         DOWEQ     '0'
038100020111      *
038200100929     C     ETCKSC        CHAIN     TIVIS05L                           31
038300020111     C     *IN31         IFEQ      '1'
038400020325     C   OF              Except    TESVIS
038500020325     C   OF              setoff                                       OF
038600020327      *
038700020327     C     $Prima        ifeq      *off
038800020327     C                   Except    TESETC
038900020327     C                   move      *on           $Prima
039000020327     C                   endif
039100020327      *
039200020325     C                   Except    DELETC
039300020325     C                   Add       1             $Totale
039400100408     c                   if        wdelete='S'
039500100408     C                   DELETE    TEETC01L
039600020128     C                   ENDIF
039700100408     C                   ENDIF
039800020128     C     KCAT          READE     TEETC01L                               30
039900020128     C                   ENDDO
040000020325     C     $Totale       ifgt      0
040100020325     C   OF              Except    TESVIS
040200020325     C   OF              setoff                                       OF
040300020325     C                   Except    TOTALE
040400020325     C                   z-add     *zeros        $Totale
040500020325     C                   endif
040600020128      *
040700020128      * Teets
040800020327     C                   move      *off          $Prima
040900100408     C                   MOVE      'S'           $DSF
041000020128     C                   MOVE      'O'           $CTO
041100020128     C     KCAT          SETLL     TEETS01L
041200020128     C     KCAT          READE     TEETS01L                               30
041300020128      * 30 on - Fine lettura
041400020128     C     *IN30         DOWEQ     '0'
041500020128      *
041600100929     C     ETSKSC        CHAIN     TIVIS05L                           31
041700020128     C     *IN31         IFEQ      '1'
041800020325     C   OF              Except    TESVIS
041900020325     C   OF              setoff                                       OF
042000020327      *
042100020327     C     $Prima        ifeq      *off
042200020327     C                   Except    TESETS
042300020327     C                   move      *on           $Prima
042400020327     C                   endif
042500020327      *
042600020325     C                   Except    DELETS
042700020325     C                   Add       1             $Totale
042800100408     c                   if        wdelete='S'
042900100408     C                   DELETE    TEETS01L
043000020128     C                   ENDIF
043100100408     C                   ENDIF
043200020128     C     KCAT          READE     TEETS01L                               30
043300020128     C                   ENDDO
043400020325     C     $Totale       ifgt      0
043500020325     C   OF              Except    TESVIS
043600020325     C   OF              setoff                                       OF
043700020325     C                   Except    TOTALE
043800020325     C                   z-add     *zeros        $Totale
043900020325     C                   endif
043901100930      *
043902100930      * Teets10
043903100930     C                   move      *off          $Prima
043904100930     C                   MOVE      'S'           $DSF
043905100930     C                   MOVE      'O'           $CTO
043906100930     C     KCAT          SETLL     TEETS11L
043907100930     C     KCAT          READE     TEETS11L                               30
043908100930      * 30 on - Fine lettura
043909100930     C     *IN30         DOWEQ     '0'
043910100930      *
043911100930     C     ETSKSC        CHAIN     TIVIS05L                           31
043912100930     C     *IN31         IFEQ      '1'
043913100930     C   OF              Except    TESvis
043914100930     C   OF              setoff                                       OF
043915100930      *
043916100930     C     $Prima        ifeq      *off
043917100930     C                   Except    TESETS10
043918100930     C                   move      *on           $Prima
043919100930     C                   endif
043920100930      *
043921100930     C                   Except    DELETS10
043922100930     C                   Add       1             $Totale
043923100930     c                   if        wdelete='S'
043924100930     C                   DELETE    TEETS11L
043925100930     C                   ENDIF
043926100930     C                   ENDIF
043927100930     C     KCAT          READE     TEETS11L                               30
043928100930     C                   ENDDO
043929100930     C     $Totale       ifgt      0
043930100930     C   OF              Except    TESVIS
043931100930     C   OF              setoff                                       OF
043932100930     C                   Except    TOTALE
043933100930     C                   z-add     *zeros        $Totale
043934100930     C                   endif
044000020128      *
044100020128      * Teett
044200020327     C                   move      *off          $Prima
044300100408     C                   MOVE      'S'           $DSF
044400020128     C                   MOVE      'O'           $CTO
044500020128     C     KCAT          SETLL     TEETT01L
044600020128     C     KCAT          READE     TEETT01L                               30
044700020128      * 30 on - Fine lettura
044800020128     C     *IN30         DOWEQ     '0'
044900020128      *
045000100929     C     ETTKSC        CHAIN     TIVIS05L                           31
045100020128     C     *IN31         IFEQ      '1'
045200020325     C   OF              Except    TESVIS
045300020325     C   OF              setoff                                       OF
045400020327      *
045500020327     C     $Prima        ifeq      *off
045600020327     C                   Except    TESETT
045700020327     C                   move      *on           $Prima
045800020327     C                   endif
045900020327      *
046000020325     C                   Except    DELETT
046100020325     C                   Add       1             $Totale
046200100408     c                   if        wdelete='S'
046300100408     C                   DELETE    TEETT01L
046400020128     C                   ENDIF
046500100408     C                   ENDIF
046600020128     C     KCAT          READE     TEETT01L                               30
046700020128     C                   ENDDO
046800020325     C     $Totale       ifgt      0
046900020325     C   OF              Except    TESVIS
047000020325     C   OF              setoff                                       OF
047100020325     C                   Except    TOTALE
047200020325     C                   z-add     *zeros        $Totale
047300020325     C                   endif
047400020114      *
047500020114     C                   SETON                                        LR
047600020114      *
047700020114      * DEFINIZIONE CAMPI
047800020114     C     *LIKE         DEFINE    ETCDSF        $DSF
047900020114     C     *LIKE         DEFINE    ETCCTO        $CTO
048000020128      *
048100020128      *CAT
048200020128     C     KCAT          KLIST
048300020114     C                   KFLD                    $DSF
048400020114     C                   KFLD                    $CTO
048500041202     C     Kaco          KLIST
048600041202     C                   KFLD                    acokut
048700041202     C                   KFLD                    acokcc
048800041202     C                   KFLD                    acoksc
048900041202     C     Kind          KLIST
049000041202     C                   KFLD                    indkut
049100041202     C                   KFLD                    indkcc
049200041202     C                   KFLD                    indksc
049300041202     C     Kclp          KLIST
049400041202     C                   KFLD                    clpkut
049500041202     C                   KFLD                    clpkcc
049600041202     C                   KFLD                    clpksc
049700020325     OQSYSPRT   E            TESVIS         2 02
049800020910     O                                         +  2 'PUNTO OPERATIVO:'
049900020910     O                       DUTPOU
050000020325     O                                         + 10 'CANCELLAZIONE RECORDS '
050100020325     O                                         +  1 'FILES LEGATI ALLE VISITE:'
050200020325     O                                         +  8 'TNVR22R'
050300020325     O                       UDATE             +  5 '  /  /  '
050400020325     O                                         +  5 'PAG.'
050500020325     O                       PAGE1         Z
050600020327     O          E            TESACO         1
050700020327     O                                         +  1 'TFACO00F: N.Visita Ragion-
050800020327     O                                              e sociale'
050900020327     O                                         + 38 'Dt.ult.var.   Dt.Trasf.'
051000020325     O          E            DELACO         1
051100020325     O                       ACOKSC            + 11 '      / '
051200020325     O                       ACORAG            +  1
051300020325     O                       ACODUV            +  4 '  /  /  '
051400020327     O                       ACODTR            +  7 '  /  /  '
051500020325      *
051600020327     O          E            TESIND         1
051700020325     O                                         +  1 'TFIND00F:'
051800020325      *
051900020327     O          E            TESCLP         1
052000020325     O                                         +  1 'TFCLP00F:'
052100020325      *
052200100407     O          E            TEScls         1
052300100407     O                                         +  1 'TFCLS00F:'
052400020327     O          E            TESOFM         1
052500100929     O                                         +  1 'TNOFM00F: N.Visita    Dt.d-
052600020327     O                                              ecorrenza Dt.scadenza   Dt.-
052700020327     O                                              variazione'
052800020325     O          E            DELOFM         1
052900020325     O                       TAMKSC            + 11 '      / '
053000020325     O                       TAMDDT            +  4 '    /  /  '
053100020325     O                       TAMDST            +  4 '    /  /  '
053200020325     O                       TAMDUV            +  4 '    /  /  '
053300020325      *
053400020327     O          E            TESOFD         1
053500020325     O                                         +  1 'TIOFD00F:'
053600020325      *
053700020327     O          E            TESOPT         1
053800100929     O                                         +  1 'TIOPT00F: N.Visita    Dt.u-
053900020327     O                                              lt.var.'
054000020325      *
054100020325     O          E            DELOPT         1
054200020325     O                       TPTKSC            + 11 '      / '
054300020325     O                       TPTDUV            +  4 '    /  /  '
054400020325      *
054500020327     O          E            TESOPD         1
054600020325     O                                         +  1 'TIOPD00F:'
054700020325      *
054800020327     O          E            TESOGC         1
054900020325     O                                         +  1 'TIOGC00F:'
055000020325      *
055400020327     O          E            TESETC         1
055500020325     O                                         +  1 'TEETC00F: N.Visita  Dt.ins-
055600020327     O                                              erimento  Dt.ult.var.'
055700020325      *
055800020325     O          E            DELETC         1
055900020327     O                       ETCKSC              19 '      / '
056000020325     O                       ETCDIR            +  4 '    /  /  '
056100020325     O                       ETCDUV            +  4 '    /  /  '
056200020325      *
056300020327     O          E            TESETS         1
056400020325     O                                         +  1 'TEETS00F: N.Visita  Dt.ins-
056500020327     O                                              erimento  Dt.ult.var.'
056600020325      *
056700020325     O          E            DELETS         1
056800020327     O                       ETSKSC              19 '      / '
056900020325     O                       ETSDIR            +  4 '    /  /  '
057000020325     O                       ETSDUV            +  4 '    /  /  '
057001100930      *
057002100930     O          E            TESETS10       1
057003100930     O                                         +  1 'TEETS10F: N.Visita  Dt.ins-
057004100930     O                                              erimento  Dt.ult.var.'
057005100930      *
057006100930     O          E            DELETS10       1
057007100930     O                       ETSKSC              19 '      / '
057008100930     O                       ETSDIR            +  4 '    /  /  '
057009100930     O                       ETSDUV            +  4 '    /  /  '
057100020325      *
057200020327     O          E            TESETT         1
057300020325     O                                         +  1 'TEETT00F: N.Visita  Dt.ins-
057400020327     O                                              erimento  Dt.ult.var.'
057500020325      *
057600020325     O          E            DELETT         1
057700020327     O                       ETTKSC              19 '      / '
057800020325     O                       ETTDIR            +  4 '    /  /  '
057900020325     O                       ETTDUV            +  4 '    /  /  '
058000020325      *
058100020327     O          E            TOTALE         2
058200020325     O                                         +  1 'TOTALE  :'
058300100407     O                       $Totale       2   +  2
