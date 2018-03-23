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
001100071010     D NSP             S              6  0 DIM(6)
001200071010     D NCL             S              6  0 DIM(6)
001300071010     D PKB             S              7  1 DIM(6)
001400071010     D TPO             S             13  3 DIM(6)
001500071010     D TAL             S             13  3 DIM(6)
001600071010     D §SP             S              7  0 DIM(6)
001700071010     D §CL             S              7  0 DIM(6)
001800071010     D §KB             S              8  1 DIM(6)
001900071010     D §PO             S             13  3 DIM(6)
002000071010     D §AL             S             13  3 DIM(6)
002100980904     D TBS             S              2    DIM(20)
002200980904     D TBR             S              2    DIM(20)
002300071010     D KOS             S             66    DIM(11) CTDATA PERRCD(1)
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
014200071010     c     wpkf          WHENLE    1
014300980903     C                   Z-ADD     01            XX
014400031107     C**!!!TASPKF        WHENLE    10
014500071010     c     wpkf          WHENLE    5
014600980903     C                   Z-ADD     02            XX
014700031107     C**!!!TASPKF        WHENLE    50
014800071010     c     wpkf          WHENLE    10
014900980903     C                   Z-ADD     03            XX
015000071010     c     wpkf          WHENLE    20
015100071010     C                   Z-ADD     04            XX
015200071010     c     wpkf          WHENLE    50
015300071010     C                   Z-ADD     05            XX
015400980903     C                   OTHER
015500071010     C                   Z-ADD     06            XX
015600980903     C                   ENDSL
015700980904      *
015800980904      * Ricavo importi aggiuntivi e determino se la spedizione è fatturata
015900980904      * se previsto dal tipo bolla
016000980904     C     *IN72         IFEQ      '1'
016100980904     C                   EXSR      IMPORT
016200020108     C* TRASF.IN EURO
016300020108     C                   Z-ADD     TASPOR        WWIMP            13 3
016400020108     C     TASDIV        IFNE      'EUR'
016500020108     C     TASPOR        DIV(H)    1936,27       WWIMP
016600020108     C                   ENDIF
016700020108     C*
016800020108     C                   ADD       WWIMP         TPO(XX)
016900980904     C                   ADD       ALTIMP        TAL(XX)
017000980904     C                   END
017100980903      *
017200980904      * Totalizzo dati spedizione se previsto dal tipo bolla
017300980904     C     *IN71         IFEQ      '1'
017400980903     C                   ADD       1             NSP(XX)
017500980903     C                   ADD       TASNCL        NCL(XX)
017600031107     C**!!!              ADD       TASPKF        PKB(XX)
017700031107     c                   Add       wpkf          pkb(xx)
017800980904     C                   END
017900980903      *
018000980903      * Determino se sped. con C/assegno ed eventualmente totalizzo
018100980903     C     TASFCA        IFEQ      'S'
018200980904     C     KCSB          CHAIN     TNCSB000                           73
018300020108     C  N73              DO
018400020108     C                   Z-ADD     CSBCAS        WWIMP
018500020108     C     CSBVCA        IFNE      'EUR'
018600020108     C     CSBCAS        DIV(H)    1936,27       WWIMP
018700020108     C                   ENDIF
018800020108     C                   ADD       1             CODSPE            6 0
018900020108     C                   ADD       WWIMP         CODIMP           13 3
019000020108     C                   ENDDO
019100980903     C                   END
019200980903      *
019300941122     C                   ENDSR
019400980903      *----------------------------------------------------*
019500980903      *  IMPORTI AGGIUNTIVI E TEST FATTURATA
019600980903      *----------------------------------------------------*
019700980903     C     IMPORT        BEGSR
019800980903      *
019900980904      * Tengo separato l'importo della varia G
020000990809      * NON CONTEGGIO IVA E BOLLO
020100020219      * NON CONTEGGIO VARIA "N - 88888888"
020200980904      *
020300020108     C                   Z-ADD     *ZEROS        ALTIMP           13 3
020400980904      *
020500990809     C     TASSV1        IFNE      §$2BOL
020600990809     C     TASSV1        ANDNE     §$2IVA
020700020219     C     TASSV1        IFNE      'N'
020800020219     C     TASVA1        ANDNE     88888888
020900020108     C                   Z-ADD     TASVA1        WWIMP
021000020108     C     TASDIV        IFNE      'EUR'
021100020108     C     TASVA1        DIV(H)    1936,27       WWIMP
021200020108     C                   ENDIF
021300990809     C     TASSV1        IFEQ      'G'
021400020108     C                   ADD       WWIMP         CODIVG           13 3
021500980904     C                   ELSE
021600020108     C                   ADD       WWIMP         ALTIMP
021700980904     C                   END
021800020219     C                   ENDIF
021900990809     C                   ENDIF
022000980904      *
022100020108     C     TASSV2        IFNE      §$2BOL
022200020108     C     TASSV2        ANDNE     §$2IVA
022300020219     C     TASSV2        IFNE      'N'
022400020219     C     TASVA2        ANDNE     88888888
022500020108     C                   Z-ADD     TASVA2        WWIMP
022600020108     C     TASDIV        IFNE      'EUR'
022700020108     C     TASVA2        DIV(H)    1936,27       WWIMP
022800020108     C                   ENDIF
022900980904     C     TASSV2        IFEQ      'G'
023000020108     C                   ADD       WWIMP         CODIVG
023100980904     C                   ELSE
023200020108     C                   ADD       WWIMP         ALTIMP
023300980904     C                   END
023400020219     C                   ENDIF
023500990809     C                   ENDIF
023600980904      *
023700020108     C     TASSV3        IFNE      §$2BOL
023800020108     C     TASSV3        ANDNE     §$2IVA
023900020219     C     TASSV3        IFNE      'N'
024000020219     C     TASVA3        ANDNE     88888888
024100020108     C                   Z-ADD     TASVA3        WWIMP
024200020108     C     TASDIV        IFNE      'EUR'
024300020108     C     TASVA3        DIV(H)    1936,27       WWIMP
024400020108     C                   ENDIF
024500980904     C     TASSV3        IFEQ      'G'
024600020108     C                   ADD       WWIMP         CODIVG
024700980904     C                   ELSE
024800020108     C                   ADD       WWIMP         ALTIMP
024900980904     C                   END
025000020219     C                   ENDIF
025100990809     C                   ENDIF
025200980903      *
025300990809      * Leggo TITA7 per altri importi
025400980903     C     TASSV3        IFNE      *BLANK
025500990809     C     KTA7          SETLL     TITA730C
025600980903     C                   DO        *HIVAL
025700980904      *
025800990809     C     KTA7          READE     TITA730C                               73
025900020108     C  N73TA7SVN        IFNE      §$2BOL
026000020108     C     TA7SVN        ANDNE     §$2IVA
026100020219     C     TA7SVN        IFNE      'N'
026200020219     C     TA7VAN        ANDNE     88888888
026300020108     C                   Z-ADD     TA7VAN        WWIMP
026400020108     C     TASDIV        IFNE      'EUR'
026500020108     C     TA7VAN        DIV(H)    1936,27       WWIMP
026600020108     C                   ENDIF
026700990809     C     TA7SVN        IFEQ      'G'
026800020108     C                   ADD       WWIMP         CODIVG
026900980904     C                   ELSE
027000020108     C                   ADD       WWIMP         ALTIMP
027100980904     C                   END
027200020219     C                   ENDIF
027300990809     C                   ENDIF
027400980904      *
027500980904     C  N73              END
027600980903     C                   END
027700980904      *
027800980903     C                   ENDSR
027900980903      *----------------------------------------------------*
028000980903      *  STAMPA
028100980903      *----------------------------------------------------*
028200980903     C     STAMPA        BEGSR
028300980903      *
028400980903     C                   XFOOT     NSP           WNSP              7 0
028500980903      *
028600980903     C     WNSP          IFGT      *ZEROS
028700980903     C   OF              EXCEPT    TESTA
028800980903     C   OF              SETOFF                                       OF
028900980903     C                   EXCEPT    DETTA
029000980903     C                   END
029100980904      *
029200980904      * Sommo totali
029300071010     C     1             DO        6             XX
029400980904     C                   ADD       TPO(XX)       §PO(XX)
029500980904     C                   ADD       TAL(XX)       §AL(XX)
029600980904     C                   ADD       NSP(XX)       §SP(XX)
029700980904     C                   ADD       NCL(XX)       §CL(XX)
029800980904     C                   ADD       PKB(XX)       §KB(XX)
029900980904     C                   ENDDO
030000980904     C                   ADD       CODSPE        §ODSPE            7 0
030100980904     C                   ADD       CODIMP        §ODIMP           13 3
030200020108     C                   ADD       CODIVG        §ODIVG           13 3
030300980904      *
030400980904      * Azzero campi di comodo
030500980903     C                   CLEAR                   NSP
030600980903     C                   CLEAR                   NCL
030700980903     C                   CLEAR                   PKB
030800980903     C                   CLEAR                   TPO
030900980903     C                   CLEAR                   TAL
031000980903     C                   CLEAR                   CODSPE
031100980903     C                   CLEAR                   CODIMP
031200980904     C                   CLEAR                   CODIVG
031300980903      *
031400980903     C                   ENDSR
031500980903      *----------------------------------------------------*
031600980903      *  SUBROUTINE INIZIALE
031700980903      *----------------------------------------------------*
031800980903     C     *INZSR        BEGSR
031900980903      *
032000980903     C     *ENTRY        PLIST
032100980903     C                   PARM                    KPJBA
032200980903      *
032300980903     C                   MOVEL     KPJBU         PARAM
032400990809      * TITAS
032500980903     C     KTAS          KLIST
032600980903     C                   KFLD                    PAAI
032700980903     C                   KFLD                    PMGI
032800980903     C                   KFLD                    PKSC
032900990809      * TITA7
033000980903     C     KTA7          KLIST
033100980903     C                   KFLD                    TASAAS
033200980903     C                   KFLD                    TASLNP
033300980903     C                   KFLD                    TASNRS
033400980903     C                   KFLD                    TASNSP
033500980903     C                   KFLD                    TASTBL
033600980903      * TNCSB
033700980903     C     KCSB          KLIST
033800980903     C                   KFLD                    TASAAS
033900980903     C                   KFLD                    TASLNP
034000980903     C                   KFLD                    TASNRS
034100980903     C                   KFLD                    TASNSP
034200980903      *
034300980903     C     KTB           KLIST
034400980903     C                   KFLD                    TBLKUT
034500980903     C                   KFLD                    TBLCOD
034600980903      *
034700980903     C                   Z-ADD     1             CODUT
034800980903     C                   CALL      'X§PARUT'
034900980903     C                   PARM                    UT§DSE
035000980903     C                   MOVEL     RAGUT         RSUT             20
035100980903      *
035200980903     C                   TIME                    WTIME            14 0
035300980903     C                   MOVE      WTIME         WDATE             8 0
035400980903     C                   MOVEL     WTIME         UTIME             6 0
035500990809      *
035600990809      * CARICO DS SIGLA VARIE
035700990809     C                   Z-ADD     1             TBLKUT
035800990809     C                   MOVEL     '$2'          TBLCOD
035900990809     C                   CLEAR                   DS$2
036000990809     C     KTB           CHAIN     TABEL00F                           73
036100990809     C  N73TBLFLG        IFNE      '*'
036200990809     C                   MOVEL     TBLUNI        DS$2
036300990809     C                   ENDIF
036400031107
036500031107      ** Aggancio tabella QT - Campi x tassazione STD
036600031107     c                   Movel     'QT'          TblCod
036700031107     c                   Movel     1             TblKey
036800031107     c                   Clear                   dsQT1
036900031107     c     kTb           Chain     Tabel00f
037000031107     c                   If        %Found(Tabel00f) and TblFlg <> '*'
037100031107     c                   Movel     TblUni        dsQT1
037200031107     c                   EndIf
037300990809      *
037400980903      *
037500980903      * Riempio schiera con tipo bolla validi
037600980903     C                   Z-ADD     1             TBLKUT
037700980903     C                   MOVEL     'TB'          TBLCOD
037800980903     C                   Z-ADD     *ZEROS        XX                2 0
037900980904     C                   Z-ADD     *ZEROS        JJ                2 0
038000980903      *
038100980903     C     KTB           SETLL     TABEL00F
038200980903     C                   DO        *HIVAL
038300980904     C     KTB           READE     TABEL00F                               73
038400980904     C  N73TBLFLG        IFNE      '*'
038500980903     C                   MOVEL     TBLUNI        DSTB
038600980904      *
038700980904      * Tipo porto per spedizione
038800980903     C     §TBRBL        IFEQ      'N'
038900980903     C     §TBRBL        OREQ      'S'
039000980903     C                   ADD       1             XX
039100980904     C                   MOVEL     TBLKEY        TBS(XX)
039200980903     C                   ENDIF
039300980904      *
039400980904      * Tipo porto per ricavo
039500980904     C     §TBRBL        IFEQ      'N'
039600980904     C     §TBRBL        OREQ      'R'
039700980904     C                   ADD       1             JJ
039800980904     C                   MOVEL     TBLKEY        TBR(JJ)
039900980904     C                   ENDIF
040000980904      *
040100980903     C                   ENDIF
040200980904     C  N73              ENDDO
040300980903      *
040400980903     C                   ENDSR
040500980903      *--------------------------------------------------------
040600980903     OQSYSPRT   E            TESTA            02
040700980903     O                       RSUT                20
040800020108     O                                           56 '*** ELENCO SPEDIZIONI BA'
040900020108     O                                         +  0 'USCH & LOMB <IN EURO>***'
041000980914     O                                         +  5 '*FNLVB8R*'
041100980903     O                       WDATE              116 '  /  /    '
041200980903     O                       PAGE          Z    132
041300980903     O          E            TESTA       1
041400980903     O                       KNSIF               10
041500980903     O                       KNMUS               21
041600980904     O                                           50 'Codice cliente'
041700980903     O                       PKSC          Z   +  1
041800980903     O                                         +  2 'dal'
041900980903     O                       PGGI          Z   +  1
042000980903     O                                         +  0 '/'
042100980903     O                       PMMI              +  0
042200980903     O                                         +  0 '/'
042300980903     O                       PAAI              +  0
042400980904     O                                         +  2 'al'
042500980903     O                       PGGF          Z   +  1
042600980903     O                                         +  0 '/'
042700980903     O                       PMMF              +  0
042800980903     O                                         +  0 '/'
042900980903     O                       PAAF              +  0
043000980903     O                       UTIME              115 '  :  :  '
043100980904      *
043200980903     O          E            DETTA       2
043300980903     O                       KOS(1)              66
043400980903     O                       KOS(2)             132
043500980903     O                       OGS           Z     10
043600980903     O                                         +  0 '/'
043700980903     O                       OMS               +  0
043800980903     O                                         +  0 '/'
043900980903     O                       OAAS              +  0
044000980903      *
044100980903     O          E            DETTA       1
044200980903     O                       KOS(3)              66
044300980903     O                       NSP(1)        2     34
044400980903     O                       NCL(1)        2     44
044500980903     O                       PKB(1)        2     56
044600020108     O                       TPO(1)        2     75
044700020108     O                       TAL(1)        2    104
044800980903      *
044900980903     O          E            DETTA       1
045000980903     O                       KOS(4)              66
045100980903     O                       NSP(2)        2     34
045200980903     O                       NCL(2)        2     44
045300980903     O                       PKB(2)        2     56
045400020108     O                       TPO(2)        2     75
045500020108     O                       TAL(2)        2    104
045600980903      *
045700980903     O          E            DETTA       1
045800980903     O                       KOS(5)              66
045900980903     O                       NSP(3)        2     34
046000980903     O                       NCL(3)        2     44
046100980903     O                       PKB(3)        2     56
046200020108     O                       TPO(3)        2     75
046300020108     O                       TAL(3)        2    104
046400980903      *
046500980903     O          E            DETTA       1
046600071010     O                       KOS(10)             66
046700980903     O                       NSP(4)        2     34
046800980903     O                       NCL(4)        2     44
046900980903     O                       PKB(4)        2     56
047000020108     O                       TPO(4)        2     75
047100020108     O                       TAL(4)        2    104
047200071010      *
047300071010     O          E            DETTA       1
047400071010     O                       KOS(11)             66
047500071010     O                       NSP(5)        2     34
047600071010     O                       NCL(5)        2     44
047700071010     O                       PKB(5)        2     56
047800071010     O                       TPO(5)        2     75
047900071010     O                       TAL(5)        2    104
048000071010      *
048100071010     O          E            DETTA       1
048200071010     O                       KOS(6)              66
048300071010     O                       NSP(6)        2     34
048400071010     O                       NCL(6)        2     44
048500071010     O                       PKB(6)        2     56
048600071010     O                       TPO(6)        2     75
048700071010     O                       TAL(6)        2    104
048800980903      *
048900980903     O          E            DETTA       1
049000980903     O                       KOS(7)              66
049100980904     O                       KOS(8)             132
049200020108     O          E            DETTA       1
049300980904     O                       CODSPE        2     34
049400020108     O                       CODIMP        2     67
049500020108     O                       CODIVG        2     94
049600980904      *
049700980904     O          E            TOTAL       2
049800980904     O                       KOS(9)              66
049900980904     O                       KOS(2)             132
050000980904      *
050100980904     O          E            TOTAL       0
050200980904     O                                           12 'T O T A L E:'
050300980904      *
050400980904     O          E            TOTAL       1
050500980904     O                       KOS(3)              66
050600980904     O                       §SP(1)        2     34
050700980904     O                       §CL(1)        2     44
050800980904     O                       §KB(1)        2     56
050900020108     O                       §PO(1)        2     75
051000020108     O                       §AL(1)        2    104
051100980904      *
051200980904     O          E            TOTAL       1
051300980904     O                       KOS(4)              66
051400980904     O                       §SP(2)        2     34
051500980904     O                       §CL(2)        2     44
051600980904     O                       §KB(2)        2     56
051700020108     O                       §PO(2)        2     75
051800020108     O                       §AL(2)        2    104
051900980904      *
052000980904     O          E            TOTAL       1
052100980904     O                       KOS(5)              66
052200980904     O                       §SP(3)        2     34
052300980904     O                       §CL(3)        2     44
052400980904     O                       §KB(3)        2     56
052500020108     O                       §PO(3)        2     75
052600020108     O                       §AL(3)        2    104
052700980904      *
052800980904     O          E            TOTAL       1
052900071010     O                       KOS(10)             66
053000980904     O                       §SP(4)        2     34
053100980904     O                       §CL(4)        2     44
053200980904     O                       §KB(4)        2     56
053300020108     O                       §PO(4)        2     75
053400020108     O                       §AL(4)        2    104
053500071010      *
053600071010     O          E            TOTAL       1
053700071010     O                       KOS(11)             66
053800071010     O                       §SP(5)        2     34
053900071010     O                       §CL(5)        2     44
054000071010     O                       §KB(5)        2     56
054100071010     O                       §PO(5)        2     75
054200071010     O                       §AL(5)        2    104
054300071010      *
054400071010     O          E            TOTAL       1
054500071010     O                       KOS(6)              66
054600071010     O                       §SP(6)        2     34
054700071010     O                       §CL(6)        2     44
054800071010     O                       §KB(6)        2     56
054900071010     O                       §PO(6)        2     75
055000071010     O                       §AL(6)        2    104
055100980904      *
055200980904     O          E            TOTAL       1
055300980904     O                       KOS(7)              66
055400980904     O                       KOS(8)             132
055500020108     O          E            TOTAL       1
055600980904     O                       §ODSPE        2     35
055700020108     O                       §ODIMP        2     67
055800020108     O                       §ODIVG        2     94
055900020108**  KOST  8E/L66                                                 *
056000020108Giorno:                 Spedizioni     Colli     Peso Kg            1
056100020108    Porto  Altri importi tranne varia G                             2
056200071010da  0  a  1  Kg                                                     3
056300071010da  2  a  5  Kg                                                     4
056400071010da  6  a 10  Kg                                                     5
056500980903   oltre 50  Kg                                                     6
056600020108COD             Spedizioni                Importo                   7
056700020108       Varia  G                                                     8
056800020108T O T A L E:            Spedizioni     Colli     Peso Kg            9
056900071010da 11  a 20  Kg                                                     10
057000071010da 21  a 50  Kg                                                     11
