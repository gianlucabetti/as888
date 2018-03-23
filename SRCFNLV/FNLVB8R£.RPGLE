000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200980903      *---------------------------------------------------------*
000300980903      *  STAMPA STATISTICA SPEDIZIONI CLIENTE "BAUSCH & LOMB"   *
000400980903      *---------------------------------------------------------*
000500990809     FTITAS38C  IF   E           K DISK
000600990809     FTITA730C  IF   E           K DISK
000700980903     FTNCSB03L  IF   E           K DISK
000800980903     FTABEL00F  IF   E           K DISK
000900980903     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001000980903      *
001100980903     D NSP             S              6  0 DIM(4)
001200980903     D NCL             S              6  0 DIM(4)
001300980903     D PKB             S              7  1 DIM(4)
001400020108     D TPO             S             13  3 DIM(4)
001500020108     D TAL             S             13  3 DIM(4)
001600980904     D §SP             S              7  0 DIM(4)
001700980904     D §CL             S              7  0 DIM(4)
001800980904     D §KB             S              8  1 DIM(4)
001900020108     D §PO             S             13  3 DIM(4)
002000020108     D §AL             S             13  3 DIM(4)
002100980904     D TBS             S              2    DIM(20)
002200980904     D TBR             S              2    DIM(20)
002300980904     D KOS             S             66    DIM(9) CTDATA PERRCD(1)
002400980903      *
002500890921     D KPJBA         E DS
002600980903     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002700980903     D DSTB          E DS
002800990809     D DS$2          E DS
002900031107     d dsQT1         e ds
003000031107
003100900315     D PARAM           DS
003200980903     D  PAAI                   1      4  0
003300980903     D  PMMI                   5      6  0
003400980903     D  PGGI                   7      8  0
003500980903     D  PMGI                   5      8  0
003600980903     D  PAAF                   9     12  0
003700980903     D  PMMF                  13     14  0
003800980903     D  PGGF                  15     16  0
003900980903     D  PMGF                  13     16  0
004000980903     D  PKSC                  17     23  0
004100980903     D                 DS
004200980903     D  OMGS                   1      4  0
004300980903     D  OMS                    1      2  0
004400980903     D  OGS                    3      4  0
004500031107
004600031107     d wntara          s                   Like(TasPkf)
004700031107     d wpkf            s                   Like(TasPkf)
004800031107     d wtara           s                   Like(§QtTpc)
004900980903      *---------------------------------------------------------*
005000980903     C                   Z-ADD     TASAAS        OAAS              4 0
005100980903     C                   Z-ADD     TASMGS        OMGS              4 0
005200980903     C                   SETON                                        OF
005300980903      *
005400990809     C     KTAS          SETLL     TITAS38C
005500980903      *
005600921023     C                   DO        *HIVAL
005700990809     C                   READ      TITAS38C                               70
005800980903      *
005900980903      * Se supero data limite stampo vado a fine
006000980903     C  N70TASAAS        IFGT      PAAF
006100980903     C     TASAAS        OREQ      PAAF
006200980903     C     TASMGS        ANDGT     PMGF
006300980903     C                   SETON                                        70
006400980903     C                   ELSE
006500980903      *
006600980903      * Elaboro solo cod. cliente selezionato
006700980903     C     TASKSC        IFEQ      PKSC
006800980903      *
006900980903      * Elaboro solo tipo bolla valido
007000980904     C     TASTBL        LOOKUP    TBS                                    71
007100980904     C     TASTBL        LOOKUP    TBR                                    72
007200980904      *
007300980903     C     *IN71         IFEQ      *ON
007400980904     C     *IN72         OREQ      *ON
007500980903      *
007600980903      * Stampo a rottura di giorno tranne il primo giro
007700980903     C     TASAAS        IFNE      OAAS
007800980903     C     OAAS          ANDNE     *ZEROS
007900980903     C     TASMGS        ORNE      OMGS
008000980903     C     OMGS          ANDNE     *ZEROS
008100980903     C                   EXSR      STAMPA
008200980903     C                   END
008300980903      *
008400980903      * Elaboro spedizione
008500980903     C                   EXSR      ELABOR
008600980903     C                   END
008700980903     C                   END
008800980903      *
008900980903     C                   END
009000980903     C  N70              END
009100980903      *
009200980903      * Stampo ultimo gg
009300980903     C                   EXSR      STAMPA
009400980904      *
009500980904      * Stampo totale
009600980904     C                   EXCEPT    TESTA
009700980904     C                   EXCEPT    TOTAL
009800980903      *
009900980903     C                   SETON                                        LR
010000980903      *--------------------------------------------------------
010100980903      * ELABORO SPEDIZIONE E INCREMENTO I TOTALI GIORNALIERI
010200980903      *--------------------------------------------------------
010300980903     C     ELABOR        BEGSR
010400980903      *
010500980903     C                   Z-ADD     TASAAS        OAAS              4 0
010600980903     C                   Z-ADD     TASMGS        OMGS              4 0
010700031107
010800031107      * P E S O
010900031107      *--------
011000031107      *  --> Bolla fatturata
011100031107if  1c                   If        TasNft > *Zeros and TasDft > *Zeros
011200031107      *   --> Peso da VDL
011300031107      *        imposto il peso VDL - la tara
011400031107if  2c                   If        TasFpf = 'V'
011500031107     c     §QtTpc        Mult      TasNcp        wtara
011600031107     c     TasPkc        Sub(h)    wtara         wntara
011700031107     c                   Z-Add     wntara        wpkf
011800031107      *   --> No peso da VDL
011900031107      *        imposto il peso da fatturare
012000031107   x2c                   Else
012100031107     c                   Z-Add     TasPkf        wpkf
012200031107    2c                   EndIf
012300031107      *  --> Bolla da fatturare
012400031107   x1c                   Else
012500031107      * calcolo peso da VDL - tara
012600031107     c     §QtTpc        Mult      TasNcp        wtara
012700031107     c     TasPkc        Sub(h)    wtara         wntara
012800031107      *   --> Peso da VDL >= peso da fatturare
012900031107      *        imposto il peso da VDL
013000031107if  2c                   If        wntara >= TasPkf
013100031107     c                   Z-Add     wntara        wpkf
013200031107      *   --> Peso da VDL < peso da fatturare
013300031107      *        imposto il peso da fatturare
013400031107   x2c                   Else
013500031107     c                   Z-Add     TasPkf        wpkf
013600031107    2c                   EndIf
013700031107    1c                   EndIF
013800980903      *
013900980903      * Determino la fascia di peso
014000980903     C                   SELECT
014100031107     C**!!!TASPKF        WHENLE    3
014200031107     c     wpkf          WHENLE    3
014300980903     C                   Z-ADD     01            XX
014400031107     C**!!!TASPKF        WHENLE    10
014500031107     c     wpkf          WHENLE    10
014600980903     C                   Z-ADD     02            XX
014700031107     C**!!!TASPKF        WHENLE    50
014800031107     c     wpkf          WHENLE    50
014900980903     C                   Z-ADD     03            XX
015000980903     C                   OTHER
015100980903     C                   Z-ADD     04            XX
015200980903     C                   ENDSL
015300980904      *
015400980904      * Ricavo importi aggiuntivi e determino se la spedizione è fatturata
015500980904      * se previsto dal tipo bolla
015600980904     C     *IN72         IFEQ      '1'
015700980904     C                   EXSR      IMPORT
015800020108     C* TRASF.IN EURO
015900020108     C                   Z-ADD     TASPOR        WWIMP            13 3
016000020108     C     TASDIV        IFNE      'EUR'
016100020108     C     TASPOR        DIV(H)    1936,27       WWIMP
016200020108     C                   ENDIF
016300020108     C*
016400020108     C                   ADD       WWIMP         TPO(XX)
016500980904     C                   ADD       ALTIMP        TAL(XX)
016600980904     C                   END
016700980903      *
016800980904      * Totalizzo dati spedizione se previsto dal tipo bolla
016900980904     C     *IN71         IFEQ      '1'
017000980903     C                   ADD       1             NSP(XX)
017100980903     C                   ADD       TASNCL        NCL(XX)
017200031107     C**!!!              ADD       TASPKF        PKB(XX)
017300031107     c                   Add       wpkf          pkb(xx)
017400980904     C                   END
017500980903      *
017600980903      * Determino se sped. con C/assegno ed eventualmente totalizzo
017700980903     C     TASFCA        IFEQ      'S'
017800980904     C     KCSB          CHAIN     TNCSB000                           73
017900020108     C  N73              DO
018000020108     C                   Z-ADD     CSBCAS        WWIMP
018100020108     C     CSBVCA        IFNE      'EUR'
018200020108     C     CSBCAS        DIV(H)    1936,27       WWIMP
018300020108     C                   ENDIF
018400020108     C                   ADD       1             CODSPE            6 0
018500020108     C                   ADD       WWIMP         CODIMP           13 3
018600020108     C                   ENDDO
018700980903     C                   END
018800980903      *
018900941122     C                   ENDSR
019000980903      *----------------------------------------------------*
019100980903      *  IMPORTI AGGIUNTIVI E TEST FATTURATA
019200980903      *----------------------------------------------------*
019300980903     C     IMPORT        BEGSR
019400980903      *
019500980904      * Tengo separato l'importo della varia G
019600990809      * NON CONTEGGIO IVA E BOLLO
019700020219      * NON CONTEGGIO VARIA "N - 88888888"
019800980904      *
019900020108     C                   Z-ADD     *ZEROS        ALTIMP           13 3
020000980904      *
020100990809     C     TASSV1        IFNE      §$2BOL
020200990809     C     TASSV1        ANDNE     §$2IVA
020300020219     C     TASSV1        IFNE      'N'
020400020219     C     TASVA1        ANDNE     88888888
020500020108     C                   Z-ADD     TASVA1        WWIMP
020600020108     C     TASDIV        IFNE      'EUR'
020700020108     C     TASVA1        DIV(H)    1936,27       WWIMP
020800020108     C                   ENDIF
020900990809     C     TASSV1        IFEQ      'G'
021000020108     C                   ADD       WWIMP         CODIVG           13 3
021100980904     C                   ELSE
021200020108     C                   ADD       WWIMP         ALTIMP
021300980904     C                   END
021400020219     C                   ENDIF
021500990809     C                   ENDIF
021600980904      *
021700020108     C     TASSV2        IFNE      §$2BOL
021800020108     C     TASSV2        ANDNE     §$2IVA
021900020219     C     TASSV2        IFNE      'N'
022000020219     C     TASVA2        ANDNE     88888888
022100020108     C                   Z-ADD     TASVA2        WWIMP
022200020108     C     TASDIV        IFNE      'EUR'
022300020108     C     TASVA2        DIV(H)    1936,27       WWIMP
022400020108     C                   ENDIF
022500980904     C     TASSV2        IFEQ      'G'
022600020108     C                   ADD       WWIMP         CODIVG
022700980904     C                   ELSE
022800020108     C                   ADD       WWIMP         ALTIMP
022900980904     C                   END
023000020219     C                   ENDIF
023100990809     C                   ENDIF
023200980904      *
023300020108     C     TASSV3        IFNE      §$2BOL
023400020108     C     TASSV3        ANDNE     §$2IVA
023500020219     C     TASSV3        IFNE      'N'
023600020219     C     TASVA3        ANDNE     88888888
023700020108     C                   Z-ADD     TASVA3        WWIMP
023800020108     C     TASDIV        IFNE      'EUR'
023900020108     C     TASVA3        DIV(H)    1936,27       WWIMP
024000020108     C                   ENDIF
024100980904     C     TASSV3        IFEQ      'G'
024200020108     C                   ADD       WWIMP         CODIVG
024300980904     C                   ELSE
024400020108     C                   ADD       WWIMP         ALTIMP
024500980904     C                   END
024600020219     C                   ENDIF
024700990809     C                   ENDIF
024800980903      *
024900990809      * Leggo TITA7 per altri importi
025000980903     C     TASSV3        IFNE      *BLANK
025100990809     C     KTA7          SETLL     TITA730C
025200980903     C                   DO        *HIVAL
025300980904      *
025400990809     C     KTA7          READE     TITA730C                               73
025500020108     C  N73TA7SVN        IFNE      §$2BOL
025600020108     C     TA7SVN        ANDNE     §$2IVA
025700020219     C     TA7SVN        IFNE      'N'
025800020219     C     TA7VAN        ANDNE     88888888
025900020108     C                   Z-ADD     TA7VAN        WWIMP
026000020108     C     TASDIV        IFNE      'EUR'
026100020108     C     TA7VAN        DIV(H)    1936,27       WWIMP
026200020108     C                   ENDIF
026300990809     C     TA7SVN        IFEQ      'G'
026400020108     C                   ADD       WWIMP         CODIVG
026500980904     C                   ELSE
026600020108     C                   ADD       WWIMP         ALTIMP
026700980904     C                   END
026800020219     C                   ENDIF
026900990809     C                   ENDIF
027000980904      *
027100980904     C  N73              END
027200980903     C                   END
027300980904      *
027400980903     C                   ENDSR
027500980903      *----------------------------------------------------*
027600980903      *  STAMPA
027700980903      *----------------------------------------------------*
027800980903     C     STAMPA        BEGSR
027900980903      *
028000980903     C                   XFOOT     NSP           WNSP              7 0
028100980903      *
028200980903     C     WNSP          IFGT      *ZEROS
028300980903     C   OF              EXCEPT    TESTA
028400980903     C   OF              SETOFF                                       OF
028500980903     C                   EXCEPT    DETTA
028600980903     C                   END
028700980904      *
028800980904      * Sommo totali
028900980904     C     1             DO        4             XX
029000980904     C                   ADD       TPO(XX)       §PO(XX)
029100980904     C                   ADD       TAL(XX)       §AL(XX)
029200980904     C                   ADD       NSP(XX)       §SP(XX)
029300980904     C                   ADD       NCL(XX)       §CL(XX)
029400980904     C                   ADD       PKB(XX)       §KB(XX)
029500980904     C                   ENDDO
029600980904     C                   ADD       CODSPE        §ODSPE            7 0
029700980904     C                   ADD       CODIMP        §ODIMP           13 3
029800020108     C                   ADD       CODIVG        §ODIVG           13 3
029900980904      *
030000980904      * Azzero campi di comodo
030100980903     C                   CLEAR                   NSP
030200980903     C                   CLEAR                   NCL
030300980903     C                   CLEAR                   PKB
030400980903     C                   CLEAR                   TPO
030500980903     C                   CLEAR                   TAL
030600980903     C                   CLEAR                   CODSPE
030700980903     C                   CLEAR                   CODIMP
030800980904     C                   CLEAR                   CODIVG
030900980903      *
031000980903     C                   ENDSR
031100980903      *----------------------------------------------------*
031200980903      *  SUBROUTINE INIZIALE
031300980903      *----------------------------------------------------*
031400980903     C     *INZSR        BEGSR
031500980903      *
031600980903     C     *ENTRY        PLIST
031700980903     C                   PARM                    KPJBA
031800980903      *
031900980903     C                   MOVEL     KPJBU         PARAM
032000990809      * TITAS
032100980903     C     KTAS          KLIST
032200980903     C                   KFLD                    PAAI
032300980903     C                   KFLD                    PMGI
032400980903     C                   KFLD                    PKSC
032500990809      * TITA7
032600980903     C     KTA7          KLIST
032700980903     C                   KFLD                    TASAAS
032800980903     C                   KFLD                    TASLNP
032900980903     C                   KFLD                    TASNRS
033000980903     C                   KFLD                    TASNSP
033100980903     C                   KFLD                    TASTBL
033200980903      * TNCSB
033300980903     C     KCSB          KLIST
033400980903     C                   KFLD                    TASAAS
033500980903     C                   KFLD                    TASLNP
033600980903     C                   KFLD                    TASNRS
033700980903     C                   KFLD                    TASNSP
033800980903      *
033900980903     C     KTB           KLIST
034000980903     C                   KFLD                    TBLKUT
034100980903     C                   KFLD                    TBLCOD
034200980903      *
034300980903     C                   Z-ADD     1             CODUT
034400980903     C                   CALL      'X§PARUT'
034500980903     C                   PARM                    UT§DSE
034600980903     C                   MOVEL     RAGUT         RSUT             20
034700980903      *
034800980903     C                   TIME                    WTIME            14 0
034900980903     C                   MOVE      WTIME         WDATE             8 0
035000980903     C                   MOVEL     WTIME         UTIME             6 0
035100990809      *
035200990809      * CARICO DS SIGLA VARIE
035300990809     C                   Z-ADD     1             TBLKUT
035400990809     C                   MOVEL     '$2'          TBLCOD
035500990809     C                   CLEAR                   DS$2
035600990809     C     KTB           CHAIN     TABEL00F                           73
035700990809     C  N73TBLFLG        IFNE      '*'
035800990809     C                   MOVEL     TBLUNI        DS$2
035900990809     C                   ENDIF
036000031107
036100031107      ** Aggancio tabella QT - Campi x tassazione STD
036200031107     c                   Movel     'QT'          TblCod
036300031107     c                   Movel     1             TblKey
036400031107     c                   Clear                   dsQT1
036500031107     c     kTb           Chain     Tabel00f
036600031107     c                   If        %Found(Tabel00f) and TblFlg <> '*'
036700031107     c                   Movel     TblUni        dsQT1
036800031107     c                   EndIf
036900990809      *
037000980903      *
037100980903      * Riempio schiera con tipo bolla validi
037200980903     C                   Z-ADD     1             TBLKUT
037300980903     C                   MOVEL     'TB'          TBLCOD
037400980903     C                   Z-ADD     *ZEROS        XX                2 0
037500980904     C                   Z-ADD     *ZEROS        JJ                2 0
037600980903      *
037700980903     C     KTB           SETLL     TABEL00F
037800980903     C                   DO        *HIVAL
037900980904     C     KTB           READE     TABEL00F                               73
038000980904     C  N73TBLFLG        IFNE      '*'
038100980903     C                   MOVEL     TBLUNI        DSTB
038200980904      *
038300980904      * Tipo porto per spedizione
038400980903     C     §TBRBL        IFEQ      'N'
038500980903     C     §TBRBL        OREQ      'S'
038600980903     C                   ADD       1             XX
038700980904     C                   MOVEL     TBLKEY        TBS(XX)
038800980903     C                   ENDIF
038900980904      *
039000980904      * Tipo porto per ricavo
039100980904     C     §TBRBL        IFEQ      'N'
039200980904     C     §TBRBL        OREQ      'R'
039300980904     C                   ADD       1             JJ
039400980904     C                   MOVEL     TBLKEY        TBR(JJ)
039500980904     C                   ENDIF
039600980904      *
039700980903     C                   ENDIF
039800980904     C  N73              ENDDO
039900980903      *
040000980903     C                   ENDSR
040100980903      *--------------------------------------------------------
040200980903     OQSYSPRT   E            TESTA            02
040300980903     O                       RSUT                20
040400020108     O                                           56 '*** ELENCO SPEDIZIONI BA'
040500020108     O                                         +  0 'USCH & LOMB <IN EURO>***'
040600980914     O                                         +  5 '*FNLVB8R*'
040700980903     O                       WDATE              116 '  /  /    '
040800980903     O                       PAGE          Z    132
040900980903     O          E            TESTA       1
041000980903     O                       KNSIF               10
041100980903     O                       KNMUS               21
041200980904     O                                           50 'Codice cliente'
041300980903     O                       PKSC          Z   +  1
041400980903     O                                         +  2 'dal'
041500980903     O                       PGGI          Z   +  1
041600980903     O                                         +  0 '/'
041700980903     O                       PMMI              +  0
041800980903     O                                         +  0 '/'
041900980903     O                       PAAI              +  0
042000980904     O                                         +  2 'al'
042100980903     O                       PGGF          Z   +  1
042200980903     O                                         +  0 '/'
042300980903     O                       PMMF              +  0
042400980903     O                                         +  0 '/'
042500980903     O                       PAAF              +  0
042600980903     O                       UTIME              115 '  :  :  '
042700980904      *
042800980903     O          E            DETTA       2
042900980903     O                       KOS(1)              66
043000980903     O                       KOS(2)             132
043100980903     O                       OGS           Z     10
043200980903     O                                         +  0 '/'
043300980903     O                       OMS               +  0
043400980903     O                                         +  0 '/'
043500980903     O                       OAAS              +  0
043600980903      *
043700980903     O          E            DETTA       1
043800980903     O                       KOS(3)              66
043900980903     O                       NSP(1)        2     34
044000980903     O                       NCL(1)        2     44
044100980903     O                       PKB(1)        2     56
044200020108     O                       TPO(1)        2     75
044300020108     O                       TAL(1)        2    104
044400980903      *
044500980903     O          E            DETTA       1
044600980903     O                       KOS(4)              66
044700980903     O                       NSP(2)        2     34
044800980903     O                       NCL(2)        2     44
044900980903     O                       PKB(2)        2     56
045000020108     O                       TPO(2)        2     75
045100020108     O                       TAL(2)        2    104
045200980903      *
045300980903     O          E            DETTA       1
045400980903     O                       KOS(5)              66
045500980903     O                       NSP(3)        2     34
045600980903     O                       NCL(3)        2     44
045700980903     O                       PKB(3)        2     56
045800020108     O                       TPO(3)        2     75
045900020108     O                       TAL(3)        2    104
046000980903      *
046100980903     O          E            DETTA       1
046200980903     O                       KOS(6)              66
046300980903     O                       NSP(4)        2     34
046400980903     O                       NCL(4)        2     44
046500980903     O                       PKB(4)        2     56
046600020108     O                       TPO(4)        2     75
046700020108     O                       TAL(4)        2    104
046800980903      *
046900980903     O          E            DETTA       1
047000980903     O                       KOS(7)              66
047100980904     O                       KOS(8)             132
047200020108     O          E            DETTA       1
047300980904     O                       CODSPE        2     34
047400020108     O                       CODIMP        2     67
047500020108     O                       CODIVG        2     94
047600980904      *
047700980904     O          E            TOTAL       2
047800980904     O                       KOS(9)              66
047900980904     O                       KOS(2)             132
048000980904      *
048100980904     O          E            TOTAL       0
048200980904     O                                           12 'T O T A L E:'
048300980904      *
048400980904     O          E            TOTAL       1
048500980904     O                       KOS(3)              66
048600980904     O                       §SP(1)        2     34
048700980904     O                       §CL(1)        2     44
048800980904     O                       §KB(1)        2     56
048900020108     O                       §PO(1)        2     75
049000020108     O                       §AL(1)        2    104
049100980904      *
049200980904     O          E            TOTAL       1
049300980904     O                       KOS(4)              66
049400980904     O                       §SP(2)        2     34
049500980904     O                       §CL(2)        2     44
049600980904     O                       §KB(2)        2     56
049700020108     O                       §PO(2)        2     75
049800020108     O                       §AL(2)        2    104
049900980904      *
050000980904     O          E            TOTAL       1
050100980904     O                       KOS(5)              66
050200980904     O                       §SP(3)        2     34
050300980904     O                       §CL(3)        2     44
050400980904     O                       §KB(3)        2     56
050500020108     O                       §PO(3)        2     75
050600020108     O                       §AL(3)        2    104
050700980904      *
050800980904     O          E            TOTAL       1
050900980904     O                       KOS(6)              66
051000980904     O                       §SP(4)        2     34
051100980904     O                       §CL(4)        2     44
051200980904     O                       §KB(4)        2     56
051300020108     O                       §PO(4)        2     75
051400020108     O                       §AL(4)        2    104
051500980904      *
051600980904     O          E            TOTAL       1
051700980904     O                       KOS(7)              66
051800980904     O                       KOS(8)             132
051900020108     O          E            TOTAL       1
052000980904     O                       §ODSPE        2     35
052100020108     O                       §ODIMP        2     67
052200020108     O                       §ODIVG        2     94
052300020108**  KOST  8E/L66                                                 *
052400020108Giorno:                 Spedizioni     Colli     Peso Kg            1
052500020108    Porto  Altri importi tranne varia G                             2
052600980903da  0  a  3  Kg                                                     3
052700980914da  4  a 10  Kg                                                     4
052800980914da 11  a 50  Kg                                                     5
052900980903   oltre 50  Kg                                                     6
053000020108COD             Spedizioni                Importo                   7
053100020108       Varia  G                                                     8
053200020108T O T A L E:            Spedizioni     Colli     Peso Kg            9
