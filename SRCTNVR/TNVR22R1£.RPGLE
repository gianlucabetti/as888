000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020116     H* TNVR22R *----------------------------------------------------*
000300020111     H* PULIZIA ARCHIVI LEGATI A FILE VISITE SENZA RELATIVO RECORD
000400000000     H*--------------------------------------------------------------*
000500020111     FTNVIS05L  IF   E           K DISK
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
001900041202     FTIOGC01L  iF   E           K DISK
002000041202     F                                     INFDS(ogc1)
002100041202
002200100407     FTNOFM00f  iF   E             DISK    rename(tntam000:tnofm000)
002300100407     FTIOFD00f  iF   E             DISK    rename(titad000:tiofd000)
002400100407     FTIOPT00f  iF   E             DISK    rename(titpt000:tiopt000)
002500100407     FTIOPD00f  iF   E             DISK    rename(titpd000:tiopd000)
002600100407     FTIOGC00f  iF   E             DISK    rename(titgc000:tiogc000)
002700100407     FTFNTC00f  iF   E             DISK    rename(tfntc: tfntc000)
002800020111      * Note
002900041202     FTFNTC01L  iF   E           K DISK
003000041206     F                                     INFDS(ntc1)
003100020111      * Cat
003200041202     FTEETC01L  iF   E           K DISK
003300041202     FTEETS01L  iF   E           K DISK
003400041202     FTEETT01L  iF   E           K DISK
003500020325      *
003600020325     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
003700020325      *
003800020910     D tibs34ds      E DS                  inz
003900020910     D ddatiute      e ds
004000020910     D azuteds       e ds                  extname(AZUTE00F)
004100020910      *
004200020325     D$Totale          s              5  0 inz
004300020910     D$TotaleA         s              5  0 inz
004400041202     D ofm1            DS
004500041202     D  ofmNRR               397    400B 0
004600041202     D ofd1            DS
004700041202     D  ofdNRR               397    400B 0
004800041202     D opt1            DS
004900041202     D  optNRR               397    400B 0
005000041202     D opd1            DS
005100041202     D  opdNRR               397    400B 0
005200041202     D ogc1            DS
005300041202     D  ogcNRR               397    400B 0
005400041206     D ntc1            DS
005500041206     D  ntcNRR               397    400B 0
005600921027     C*****************************************************************
005700020325      *
005800020910     c     *dtaara       define    §azute        azuteds
005900020910     c     *dtaara       define    §datiute      ddatiute
006000020910     C                   in(E)     *dtaara
006100020910     C                   IF        %error  or RSUT = *blanks
006200020910     C                   CLEAR                   tibs34ds
006300020910     C                   CALL      'TIBS34R'
006400020910     C                   PARM                    tibs34ds
006500020910     C                   in        *dtaara
006600020910     c                   endif
006700020910      *
006800020325     C                   Except    TESVIS
006900020111      *
007000020111      * ANAGRAFICHE
007100020128      *
007200020128      * TfAco
007300020325     C                   z-add     *zeros        $Totale
007400020327     C                   move      *off          $Prima            1
007500020111     C     *LOVAL        SETLL     TFACO00F
007600041202     C                   READ(N)   TFACO00F                               30
007700020114      * 30 on - fine lettura
007800020111     C     *IN30         DOWEQ     '0'
007900020111      *
008000020111     C     ACOKSC        CHAIN     TNVIS05L                           31
008100020128      *
008200020111     C     *IN31         IFEQ      '1'
008300020325     C   OF              Except    TESVIS
008400020325     C   OF              setoff                                       OF
008500020327      *
008600020327     C     $Prima        ifeq      *off
008700020326     C                   Except    TESACO
008800020327     C                   move      *on           $Prima
008900020327     C                   endif
009000020327      *
009100020325     C                   Except    DELACO
009200020325     C                   Add       1             $Totale
009300100407     C**   kaco          DELETE    TFACO00F
009400020111     C                   ENDIF
009500020128      *
009600041202     C                   READ(N)   TFACO00F                               30
009700020111     C                   ENDDO
009800020325     C     $Totale       ifgt      0
009900020325     C   OF              Except    TESVIS
010000020325     C   OF              setoff                                       OF
010100020325     C                   Except    TOTALE
010200020325     C                   z-add     *zeros        $Totale
010300020325     C                   endif
010400020128      *
010500020128      * TfClp
010600020128     C     *LOVAL        SETLL     TFCLP00F
010700041202     C                   READ(N)   TFCLP00F                               30
010800020128      * 30 on - fine lettura
010900020128     C     *IN30         DOWEQ     '0'
011000020128      *
011100020128     C     CLPKSC        CHAIN     TNVIS05L                           31
011200020128      *
011300020128     C     *IN31         IFEQ      '1'
011400020325     C                   add       1             $Totale
011500100407     C**   kclp          DELETE    TFCLP00F
011600020128     C                   ENDIF
011700020128      *
011800041202     C                   READ(N)   TFCLP00F                               30
011900020128     C                   ENDDO
012000020325     C     $Totale       ifgt      0
012100020325     C   OF              Except    TESVIS
012200020325     C   OF              setoff                                       OF
012300020325     C                   Except    TESCLP
012400020325     C                   Except    TOTALE
012500020325     C                   z-add     *zeros        $Totale
012600020325     C                   endif
012700020128      * TfInd
012800020128     C     *LOVAL        SETLL     TFIND00F
012900041202     C                   READ(N)   TFIND00F                               30
013000020128      * 30 on - fine lettura
013100020128     C     *IN30         DOWEQ     '0'
013200020128      *
013300020128     C     INDKSC        CHAIN     TNVIS05L                           31
013400020128      *
013500020128     C     *IN31         IFEQ      '1'
013600020325     C                   Add       1             $Totale
013700100407     C**   kind          DELETE    TFIND00F
013800020128     C                   ENDIF
013900020128      *
014000041202     C                   READ(N)   TFIND00F                               30
014100020128     C                   ENDDO
014200020325     C     $Totale       ifgt      0
014300020325     C   OF              Except    TESVIS
014400020325     C   OF              setoff                                       OF
014500020325     C                   Except    TESIND
014600020325     C                   Except    TOTALE
014700020325     C                   z-add     *zeros        $Totale
014800020325     C                   endif
014900020111      *
015000020128      * OFFERTE
015100020128      *
015200020128      * TnOfm
015300020327     C                   move      *off          $Prima
015400020111     C     *LOVAL        SETLL     TNOFM01L
015500020111     C                   READ      TNOFM01L                               30
015600020114      * 30 on - Fine lettura
015700020111     C     *IN30         DOWEQ     '0'
015800020111      *
015900020114     C     TAMKSC        CHAIN     TNVIS05L                           31
016000020111     C     *IN31         IFEQ      '1'
016100020325     C   OF              Except    TESVIS
016200020325     C   OF              setoff                                       OF
016300020327      *
016400020327     C     $Prima        ifeq      *off
016500020327     C                   Except    TESOFM
016600020327     C                   move      *on           $Prima
016700020327     C                   endif
016800020327      *
016900020325     C                   Except    DELOFM
017000020325     C                   Add       1             $Totale
017100100407     C**   ofmnrr        DELETE    TNOFM000
017200020111     C                   ENDIF
017300020128      *
017400020111     C                   READ      TNOFM01L                               30
017500020111     C                   ENDDO
017600020325     C     $Totale       ifgt      0
017700020327     C   OF              Except    TESVIS
017800020325     C   OF              setoff                                       OF
017900020325     C                   Except    TOTALE
018000020325     C                   z-add     *zeros        $Totale
018100020325     C                   endif
018200020128      *
018300020128      * TiOfd
018400020128     C     *LOVAL        SETLL     TIOFD01L
018500020128     C                   READ      TIOFD01L                               30
018600020128      * 30 on - Fine lettura
018700020128     C     *IN30         DOWEQ     '0'
018800020128      *
018900020128     C     TADKSC        CHAIN     TNVIS05L                           31
019000020128     C     *IN31         IFEQ      '1'
019100020325     C                   Add       1             $Totale
019200100407     C**   ofdnrr        DELETE    TIOFD000
019300020128     C                   ENDIF
019400020128      *
019500020128     C                   READ      TIOFD01L                               30
019600020128     C                   ENDDO
019700020325     C     $Totale       ifgt      0
019800020325     C   OF              Except    TESVIS
019900020325     C   OF              setoff                                       OF
020000020325     C                   Except    TESOFD
020100020325     C                   Except    TOTALE
020200020325     C                   z-add     *zeros        $Totale
020300020325     C                   endif
020400020128      *
020500020128      * TiOpt
020600020327     C                   move      *off          $Prima
020700020128     C     *LOVAL        SETLL     TIOPT01L
020800020128     C                   READ      TIOPT01L                               30
020900020128      * 30 on - Fine lettura
021000020128     C     *IN30         DOWEQ     '0'
021100020128      *
021200020128     C     TPTKSC        CHAIN     TNVIS05L                           31
021300020128     C     *IN31         IFEQ      '1'
021400020325     C   OF              Except    TESVIS
021500020325     C   OF              setoff                                       OF
021600020327      *
021700020327     C     $Prima        ifeq      *off
021800020327     C                   Except    TESOPT
021900020327     C                   move      *on           $Prima
022000020327     C                   endif
022100020327      *
022200020325     C                   Except    DELOPT
022300020325     C                   Add       1             $Totale
022400100407     C**   optnrr        DELETE    TIOPT000
022500020128     C                   ENDIF
022600020128      *
022700020128     C                   READ      TIOPT01L                               30
022800020128     C                   ENDDO
022900020325     C     $Totale       ifgt      0
023000020325     C   OF              Except    TESVIS
023100020325     C   OF              setoff                                       OF
023200020325     C                   Except    TOTALE
023300020325     C                   z-add     *zeros        $Totale
023400020325     C                   endif
023500020128      *
023600020128      * TiOpd
023700020128     C     *LOVAL        SETLL     TIOPD01L
023800020128     C                   READ      TIOPD01L                               30
023900020128      * 30 on - Fine lettura
024000020128     C     *IN30         DOWEQ     '0'
024100020128      *
024200020128     C     TPDKSC        CHAIN     TNVIS05L                           31
024300020128     C     *IN31         IFEQ      '1'
024400020325     C                   Add       1             $Totale
024500100407     C**   opdnrr        DELETE    TIOPD000
024600020128     C                   ENDIF
024700020128      *
024800020128     C                   READ      TIOPD01L                               30
024900020128     C                   ENDDO
025000020325     C     $Totale       ifgt      0
025100020325     C   OF              Except    TESVIS
025200020325     C   OF              setoff                                       OF
025300020327     C                   Except    TESOPD
025400020325     C                   Except    TOTALE
025500020325     C                   z-add     *zeros        $Totale
025600020325     C                   endif
025700020128      *
025800020128      * TiOgc
025900020128     C     *LOVAL        SETLL     TIOGC01L
026000020128     C                   READ      TIOGC01L                               30
026100020128      * 30 on - Fine lettura
026200020128     C     *IN30         DOWEQ     '0'
026300020128      *
026400020128     C     TGCKSC        CHAIN     TNVIS05L                           31
026500020128     C     *IN31         IFEQ      '1'
026600020325     C                   Add       1             $Totale
026700100407     C**   ogcnrr        DELETE    TIOGC000
026800020128     C                   ENDIF
026900020128      *
027000020128     C                   READ      TIOGC01L                               30
027100020128     C                   ENDDO
027200020325     C     $Totale       ifgt      0
027300020325     C   OF              Except    TESVIS
027400020325     C   OF              setoff                                       OF
027500020325     C                   Except    TESOGC
027600020325     C                   Except    TOTALE
027700020325     C                   z-add     *zeros        $Totale
027800020325     C                   endif
027900020111      *
028000020111      * NOTE
028100020910     c                   seton                                        05
028200020910     C                   z-add     *zeros        $TotaleA
028300020111     C                   MOVE      'V'           NTCAPL
028400020111     C     NTCAPL        SETLL     TFNTC01L
028500020111     C     NTCAPL        READE     TFNTC01L                               30
028600020114      * 30 on - Fine lettura
028700020111     C     *IN30         DOWEQ     '0'
028800020114      *
028900020114      * Compongo il numero visita
029000020111     C                   MOVE      *BLANKS       KEY11            11
029100020111     C                   MOVE      *ZEROS        NROVIS            7 0
029200020111     C                   MOVEL     NTCNK1        KEY11
029300020111     C                   MOVE      KEY11         NROVIS
029400020111     C     NROVIS        CHAIN     TNVIS05L                           31
029500020111     C     *IN31         IFEQ      '1'
029600020111      *
029700020910      * Totalizzo separatamente gli annullati dai non annullati
029800020910     C     NtcFlt        ifne      'A'
029900020325     C                   Add       1             $Totale
030000020910     C                   else
030100020910     C                   Add       1             $TotaleA
030200020910     C                   endif
030300020910     C
030400041206     C                   Except    stantc
030500100407     C**   ntcnrr        DELETE    TFNTC000
030600020111     C                   ENDIF
030700020111     C     NTCAPL        READE     TFNTC01L                               30
030800020111     C                   ENDDO
030900020325     C     $Totale       ifgt      0
031000020325     C   OF              Except    TESVIS
031100020325     C   OF              setoff                                       OF
031200020325     C                   Except    TESNTC
031300020325     C                   Except    TOTALE
031400020325     C                   z-add     *zeros        $Totale
031500020910     C                   z-add     *zeros        $TotaleA
031600020325     C                   endif
031700020910     c                   setoff                                       05
031800020111      *
031900020111      * CAT
032000020128      *
032100020128      * Teetc
032200020327     C                   move      *off          $Prima
032300020114     C                   MOVE      'F'           $DSF
032400020114     C                   MOVE      'O'           $CTO
032500020128     C     KCAT          SETLL     TEETC01L
032600020128     C     KCAT          READE     TEETC01L                               30
032700020114      * 30 on - Fine lettura
032800020111     C     *IN30         DOWEQ     '0'
032900020111      *
033000020111     C     ETCKSC        CHAIN     TNVIS05L                           31
033100020111     C     *IN31         IFEQ      '1'
033200020325     C   OF              Except    TESVIS
033300020325     C   OF              setoff                                       OF
033400020327      *
033500020327     C     $Prima        ifeq      *off
033600020327     C                   Except    TESETC
033700020327     C                   move      *on           $Prima
033800020327     C                   endif
033900020327      *
034000020325     C                   Except    DELETC
034100020325     C                   Add       1             $Totale
034200041202     C***                DELETE    TEETC01L
034300020128     C                   ENDIF
034400020128     C     KCAT          READE     TEETC01L                               30
034500020128     C                   ENDDO
034600020325     C     $Totale       ifgt      0
034700020325     C   OF              Except    TESVIS
034800020325     C   OF              setoff                                       OF
034900020325     C                   Except    TOTALE
035000020325     C                   z-add     *zeros        $Totale
035100020325     C                   endif
035200020128      *
035300020128      * Teets
035400020327     C                   move      *off          $Prima
035500020128     C                   MOVE      'F'           $DSF
035600020128     C                   MOVE      'O'           $CTO
035700020128     C     KCAT          SETLL     TEETS01L
035800020128     C     KCAT          READE     TEETS01L                               30
035900020128      * 30 on - Fine lettura
036000020128     C     *IN30         DOWEQ     '0'
036100020128      *
036200020128     C     ETSKSC        CHAIN     TNVIS05L                           31
036300020128     C     *IN31         IFEQ      '1'
036400020325     C   OF              Except    TESVIS
036500020325     C   OF              setoff                                       OF
036600020327      *
036700020327     C     $Prima        ifeq      *off
036800020327     C                   Except    TESETS
036900020327     C                   move      *on           $Prima
037000020327     C                   endif
037100020327      *
037200020325     C                   Except    DELETS
037300020325     C                   Add       1             $Totale
037400041202     C***                DELETE    TEETS01L
037500020128     C                   ENDIF
037600020128     C     KCAT          READE     TEETS01L                               30
037700020128     C                   ENDDO
037800020325     C     $Totale       ifgt      0
037900020325     C   OF              Except    TESVIS
038000020325     C   OF              setoff                                       OF
038100020325     C                   Except    TOTALE
038200020325     C                   z-add     *zeros        $Totale
038300020325     C                   endif
038400020128      *
038500020128      * Teett
038600020327     C                   move      *off          $Prima
038700020128     C                   MOVE      'F'           $DSF
038800020128     C                   MOVE      'O'           $CTO
038900020128     C     KCAT          SETLL     TEETT01L
039000020128     C     KCAT          READE     TEETT01L                               30
039100020128      * 30 on - Fine lettura
039200020128     C     *IN30         DOWEQ     '0'
039300020128      *
039400020128     C     ETTKSC        CHAIN     TNVIS05L                           31
039500020128     C     *IN31         IFEQ      '1'
039600020325     C   OF              Except    TESVIS
039700020325     C   OF              setoff                                       OF
039800020327      *
039900020327     C     $Prima        ifeq      *off
040000020327     C                   Except    TESETT
040100020327     C                   move      *on           $Prima
040200020327     C                   endif
040300020327      *
040400020325     C                   Except    DELETT
040500020325     C                   Add       1             $Totale
040600041202     C***                DELETE    TEETT01L
040700020128     C                   ENDIF
040800020128     C     KCAT          READE     TEETT01L                               30
040900020128     C                   ENDDO
041000020325     C     $Totale       ifgt      0
041100020325     C   OF              Except    TESVIS
041200020325     C   OF              setoff                                       OF
041300020325     C                   Except    TOTALE
041400020325     C                   z-add     *zeros        $Totale
041500020325     C                   endif
041600020114      *
041700020114     C                   SETON                                        LR
041800020114      *
041900020114      * DEFINIZIONE CAMPI
042000020114     C     *LIKE         DEFINE    ETCDSF        $DSF
042100020114     C     *LIKE         DEFINE    ETCCTO        $CTO
042200020128      *
042300020128      *CAT
042400020128     C     KCAT          KLIST
042500020114     C                   KFLD                    $DSF
042600020114     C                   KFLD                    $CTO
042700041202     C     Kaco          KLIST
042800041202     C                   KFLD                    acokut
042900041202     C                   KFLD                    acokcc
043000041202     C                   KFLD                    acoksc
043100041202     C     Kind          KLIST
043200041202     C                   KFLD                    indkut
043300041202     C                   KFLD                    indkcc
043400041202     C                   KFLD                    indksc
043500041202     C     Kclp          KLIST
043600041202     C                   KFLD                    clpkut
043700041202     C                   KFLD                    clpkcc
043800041202     C                   KFLD                    clpksc
043900020325     OQSYSPRT   E            TESVIS         2 02
044000020910     O                                         +  2 'PUNTO OPERATIVO:'
044100020910     O                       DUTPOU
044200020325     O                                         + 10 'CANCELLAZIONE RECORDS '
044300020325     O                                         +  1 'FILES LEGATI ALLE VISITE:'
044400020325     O                                         +  8 'TNVR22R'
044500020325     O                       UDATE             +  5 '  /  /  '
044600020325     O                                         +  5 'PAG.'
044700020325     O                       PAGE1         Z
044800020327     O          E            TESACO         1
044900020327     O                                         +  1 'TFACO00F: N.Visita Ragion-
045000020327     O                                              e sociale'
045100020327     O                                         + 38 'Dt.ult.var.   Dt.Trasf.'
045200020325     O          E            DELACO         1
045300020325     O                       ACOKSC            + 11 '      / '
045400020325     O                       ACORAG            +  1
045500020325     O                       ACODUV            +  4 '  /  /  '
045600020327     O                       ACODTR            +  7 '  /  /  '
045700020325      *
045800020327     O          E            TESIND         1
045900020325     O                                         +  1 'TFIND00F:'
046000020325      *
046100020327     O          E            TESCLP         1
046200020325     O                                         +  1 'TFCLP00F:'
046300020325      *
046400020327     O          E            TESOFM         1
046500020325     O                                         +  1 'TNOFM00F: N.Visita    Dt.d-
046600020327     O                                              ecorrenza Dt.scadenza   Dt.-
046700020327     O                                              variazione'
046800020325     O          E            DELOFM         1
046900020325     O                       TAMKSC            + 11 '      / '
047000020325     O                       TAMDDT            +  4 '    /  /  '
047100020325     O                       TAMDST            +  4 '    /  /  '
047200020325     O                       TAMDUV            +  4 '    /  /  '
047300020325      *
047400020327     O          E            TESOFD         1
047500020325     O                                         +  1 'TIOFD00F:'
047600020325      *
047700020327     O          E            TESOPT         1
047800020327     O                                         +  1 'TIOPT00F: N.Visita    Dt.u-
047900020327     O                                              lt.var.'
048000020325      *
048100020325     O          E            DELOPT         1
048200020325     O                       TPTKSC            + 11 '      / '
048300020325     O                       TPTDUV            +  4 '    /  /  '
048400020325      *
048500020327     O          E            TESOPD         1
048600020325     O                                         +  1 'TIOPD00F:'
048700020325      *
048800020327     O          E            TESOGC         1
048900020325     O                                         +  1 'TIOGC00F:'
049000020325      *
049100020327     O          E            TESNTC         1
049200020325     O                                         +  1 'TFNTC00F:'
049300020325      *
049400020327     O          E            TESETC         1
049500020325     O                                         +  1 'TEETC00F: N.Visita  Dt.ins-
049600020327     O                                              erimento  Dt.ult.var.'
049700020325      *
049800020325     O          E            DELETC         1
049900020327     O                       ETCKSC              19 '      / '
050000020325     O                       ETCDIR            +  4 '    /  /  '
050100020325     O                       ETCDUV            +  4 '    /  /  '
050200020325      *
050300020327     O          E            TESETS         1
050400020325     O                                         +  1 'TEETS00F: N.Visita  Dt.ins-
050500020327     O                                              erimento  Dt.ult.var.'
050600020325      *
050700020325     O          E            DELETS         1
050800020327     O                       ETSKSC              19 '      / '
050900020325     O                       ETSDIR            +  4 '    /  /  '
051000020325     O                       ETSDUV            +  4 '    /  /  '
051100020325      *
051200020327     O          E            TESETT         1
051300020325     O                                         +  1 'TEETT00F: N.Visita  Dt.ins-
051400020327     O                                              erimento  Dt.ult.var.'
051500020325      *
051600020325     O          E            DELETT         1
051700020327     O                       ETTKSC              19 '      / '
051800020325     O                       ETTDIR            +  4 '    /  /  '
051900020325     O                       ETTDUV            +  4 '    /  /  '
052000020325      *
052100020327     O          E            TOTALE         2
052200020325     O                                         +  1 'TOTALE  :'
052300020325     O                       $Totale           +  2
052400020910     O               05                        +  1 'TOTALE ANNULLATI:'
052500020910     O               05      $TotaleA          +  2
052600041206     O          E            stantc         1
052700041206     O                                         +  1 'NTC:'
052800041206     O                       ntcnk1            +  1
052900041206     O                       ntctnt            +  1
053000041206     O                       ntcrnt
