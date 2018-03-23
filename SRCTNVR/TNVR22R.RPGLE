000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020116     H* TNVR22R *----------------------------------------------------*
000300020111     H* PULIZIA ARCHIVI LEGATI A FILE VISITE SENZA RELATIVO RECORD
000400000000     H*--------------------------------------------------------------*
000500020111     FTNVIS05L  IF   E           K DISK
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
002800100408     FTFNTC00f  uF   E             DISK    rename(tfntc: tfntc000)
002900020111      * Note
003000041202     FTFNTC01L  iF   E           K DISK
003100041206     F                                     INFDS(ntc1)
003200020111      * Cat
003300100408     FTEETC01L  uF   E           K DISK
003400100408     FTEETS01L  uF   E           K DISK
003500100408     FTEETT01L  uF   E           K DISK
003600020325      *
003700020325     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
003800020325      *
003900020910     D tibs34ds      E DS                  inz
004000020910     D ddatiute      e ds
004100020910     D azuteds       e ds                  extname(AZUTE00F)
004200020910      *
004300020325     D$Totale          s              5  0 inz
004400020910     D$TotaleA         s              5  0 inz
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
005500041206     D ntc1            DS
005600041206     D  ntcNRR               397    400B 0
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
008600020111     C     ACOKSC        CHAIN     TNVIS05L                           31
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
011900020128     C     CLPKSC        CHAIN     TNVIS05L                           31
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
014300020128     C     INDKSC        CHAIN     TNVIS05L                           31
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
016700100407     C     clsKSC        CHAIN     TNVIS05L                           31
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
019500020114     C     TAMKSC        CHAIN     TNVIS05L                           31
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
022700020128     C     TADKSC        CHAIN     TNVIS05L                           31
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
025200020128     C     TPTKSC        CHAIN     TNVIS05L                           31
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
028400020128     C     TPDKSC        CHAIN     TNVIS05L                           31
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
030800020128     C     TGCKSC        CHAIN     TNVIS05L                           31
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
032500020111      *
032600020111      * NOTE
032700020910     c                   seton                                        05
032800020910     C                   z-add     *zeros        $TotaleA
032900020111     C                   MOVE      'V'           NTCAPL
033000020111     C     NTCAPL        SETLL     TFNTC01L
033100020111     C     NTCAPL        READE     TFNTC01L                               30
033200020114      * 30 on - Fine lettura
033300020111     C     *IN30         DOWEQ     '0'
033400020114      *
033500020114      * Compongo il numero visita
033600020111     C                   MOVE      *BLANKS       KEY11            11
033700020111     C                   MOVE      *ZEROS        NROVIS            7 0
033800020111     C                   MOVEL     NTCNK1        KEY11
033900020111     C                   MOVE      KEY11         NROVIS
034000020111     C     NROVIS        CHAIN     TNVIS05L                           31
034100020111     C     *IN31         IFEQ      '1'
034200020111      *
034300020910      * Totalizzo separatamente gli annullati dai non annullati
034400020910     C     NtcFlt        ifne      'A'
034500020325     C                   Add       1             $Totale
034600020910     C                   else
034700020910     C                   Add       1             $TotaleA
034800020910     C                   endif
034900020910     C
035000041206     C                   Except    stantc
035100100408     c                   if        wdelete='S'
035200100408     C     ntcnrr        DELETE    TFNTC000
035300020111     C                   ENDIF
035400100408     C                   ENDIF
035500100408     c
035600020111     C     NTCAPL        READE     TFNTC01L                               30
035700020111     C                   ENDDO
035800020325     C     $Totale       ifgt      0
035900020325     C   OF              Except    TESVIS
036000020325     C   OF              setoff                                       OF
036100020325     C                   Except    TESNTC
036200020325     C                   Except    TOTALE
036300020325     C                   z-add     *zeros        $Totale
036400020910     C                   z-add     *zeros        $TotaleA
036500020325     C                   endif
036600020910     c                   setoff                                       05
036700100408
036800100408
036900100408     C                   endif
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
038200020111     C     ETCKSC        CHAIN     TNVIS05L                           31
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
041600020128     C     ETSKSC        CHAIN     TNVIS05L                           31
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
045000020128     C     ETTKSC        CHAIN     TNVIS05L                           31
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
052500020325     O                                         +  1 'TNOFM00F: N.Visita    Dt.d-
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
053800020327     O                                         +  1 'TIOPT00F: N.Visita    Dt.u-
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
055100020327     O          E            TESNTC         1
055200020325     O                                         +  1 'TFNTC00F:'
055300020325      *
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
058400020910     O               05                        +  1 'TOTALE ANNULLATI:'
058500020910     O               05      $TotaleA          +  2
058600041206     O          E            stantc         1
058700041206     O                                         +  1 'NTC:'
058800041206     O                       ntcnk1            +  1
058900041206     O                       ntctnt            +  1
059000100407     O                       ntcrnt            +  1
059100100407     O                       ntcntr            +  1
