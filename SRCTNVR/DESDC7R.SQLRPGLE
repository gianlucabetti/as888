000100000000     H*------------------------------------------------------------------------*
000200000000     H* AZZERA I FILE DI C/E PER RICALCOLO COMPETENZE - ESEGUE
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     F*---
000900000000     F* SALDI
001000000000     F*---
001100990610     FSISDO01L  UF   E           K DISK
001200990610     FSISDC01L  UF   E           K DISK
001300990610     FSISDA01L  UF   E           K DISK
001400990610     FSISDP01L  UF   E           K DISK
001500990610     FSISDF01L  UF   E           K DISK
001600990610     FSISDR01L  UF   E           K DISK
001700990610     FSISDT01L  UF   E           K DISK
001800000000     F*---
001900000000     F* WFILE MOVIMENTI CONTABILI
002000000000     F*---
002100020219     FECMOV02L  UF   E           K DISK
002200990219     F*---
002300990219     F* BOLLE
002400990219     F* Solo per controllo
002500990219     F*---
002600990824     FTITAS30C  IF   E           K DISK
002700990824     F                                     IGNORE(TITASP00)
002800000000     F*---
002900000000     F* PRINTER FILE
003000000000     F*---
003100000000     FQSYSPRT   O    F  132        PRINTER
003200000000     D*------------------------------------------------------------------------*
003300000000     D* SCHIERE
003400000000     D*------------------------------------------------------------------------*
003500000000     D*---
003600000000     D* RIGHE DI STAMPA
003700000000     D*---
003800000000     D STM             S             66    DIM(15) CTDATA PERRCD(1)
003900000000     D*------------------------------------------------------------------------*
004000000000     D* INPUT
004100000000     D*------------------------------------------------------------------------*
004200000000     D*-------------------
004300000000     D* DS REPERIMENTO DATI UTENTE
004400000000     D*-------------------
004500000000     D DSFIL         E DS
004600000000     D*---
004700000000     D* DS DATA GENERICA
004800000000     D*---
004900000000     D                 DS
005000020219     D  DSANN                  1      4  0
005100000000     D  DSMES                  5      6  0
005200000000     D  DSGIO                  7      8  0
005300000000     D  DSDAT8                 1      8  0
005400990219     D*---
005500990824     D* DS SCOMPOSIZIONE DATA TITAS
005600990219     D*---
005700990219     D                 DS
005800990219     D  TASMES                 1      2  0
005900990219     D  TASGIO                 3      4  0
006000990219     D  TASMGS                 1      4  0
006100000000     D*---
006200000000     D* PARAMETRI LANCIO
006300000000     D*---
006400000000     D PARAM           DS
006500990610     D  PARANN                 1      4  0
006600990610     D  PARMMI                 5      6  0
006700990610     D  PARMMF                 7      8  0
006800990610     D  PARSDO                 9      9
006900990610     D  PARSDC                10     10
007000990610     D  PARSDA                11     11
007100990610     D  PARSDP                12     12
007200990610     D  PARSDF                13     13
007300990610     D  PARSDR                14     14
007400990610     D  PARSDT                15     15
007500990610     D  PARWFM                16     16
007600990610     D  PARCET                17     17
007700990610     D  PARCED                18     18
007800011127     D  PARFIM                19     19
007900000000     D*---
008000000000     D* ARCHITETTURA
008100000000     D*---
008200000000     D KPJBA         E DS
008300000000     C*------------------------------------------------------------------------*
008400000000     C* MAIN LINE
008500000000     C*------------------------------------------------------------------------*
008600000000     C*---
008700990610     C* SISDO -> SALDI CLIENTE TESTATA
008800000000     C*---
008900000000IF  1C     PARSDO        IFEQ      'S'
009000990610     C     *LOVAL        SETLL     SISDO01L
009100990610     C                   READ      SISDO01L                               99
009200000000DO  2C     *IN99         DOWEQ     *OFF
009300000000     C                   MOVEL     'N'           WRECOK
009400000000     C                   EXSR      CHKSDO
009500000000IF  3C     WRECOK        IFEQ      'S'
009600000000     C                   Z-ADD     *ZEROS        SDONSP
009700000000     C                   Z-ADD     *ZEROS        SDOPKG
009800000000     C                   Z-ADD     *ZEROS        SDONCO
009900000000     C                   Z-ADD     *ZEROS        SDOVOL
010000000000     C                   Z-ADD     *ZEROS        SDOIRB
010100000000     C                   Z-ADD     *ZEROS        SDOIRR
010200000000     C                   Z-ADD     *ZEROS        SDOINC
010300000000     C                   Z-ADD     *ZEROS        SDOIRP
010400000304     C                   Z-ADD     *ZEROS        SDOIRC
010500000000     C                   Z-ADD     *ZEROS        SDOIRD
010600000000     C                   Z-ADD     *ZEROS        SDONNC
010700000000     C                   MOVEL     *BLANKS       SDOCLN
010800000000     C                   MOVEL     *BLANKS       SDOFTR
010900000000     C                   Z-ADD     *ZEROS        SDODTR
011000000000     C                   ADD       1             RECSDO
011100990610     C                   UPDATE    SISDO000
011200000000E   3C                   ENDIF
011300990610     C                   READ      SISDO01L                               99
011400000000E   2C                   ENDDO
011500000000E   1C                   ENDIF
011600000000     C*---
011700990610     C* SISDC -> SALDI CLIENTE DETTAGLIO
011800000000     C*---
011900000000IF  1C     PARSDC        IFEQ      'S'
012000990610     C     *LOVAL        SETLL     SISDC01L
012100990610     C                   READ      SISDC01L                               99
012200000000DO  2C     *IN99         DOWEQ     *OFF
012300000000     C                   MOVEL     'N'           WRECOK
012400000000     C                   EXSR      CHKSDC
012500000000IF  3C     WRECOK        IFEQ      'S'
012600000000     C                   Z-ADD     *ZEROS        SDCNSP
012700000000     C                   Z-ADD     *ZEROS        SDCPKG
012800990610     C                   Z-ADD     *ZEROS        SDCPCC
012900990610     C                   Z-ADD     *ZEROS        SDCPLS
013000000000     C                   Z-ADD     *ZEROS        SDCNCO
013100000000     C                   Z-ADD     *ZEROS        SDCVOL
013200000000     C                   Z-ADD     *ZEROS        SDCVCC
013300000000     C                   Z-ADD     *ZEROS        SDCVLS
013400000000     C                   Z-ADD     *ZEROS        SDCIRB
013500000000     C                   Z-ADD     *ZEROS        SDCIRR
013600000000     C                   Z-ADD     *ZEROS        SDCINC
013700000000     C                   Z-ADD     *ZEROS        SDCIRP
013800000304     C                   Z-ADD     *ZEROS        SDCIRC
013900000000     C                   Z-ADD     *ZEROS        SDCIRD
014000000000     C                   MOVEL     *BLANKS       SDCFTR
014100000000     C                   Z-ADD     *ZEROS        SDCDTR
014200000000     C                   ADD       1             RECSDC
014300990610     C                   UPDATE    SISDC000
014400000000E   3C                   ENDIF
014500990610     C                   READ      SISDC01L                               99
014600000000E   2C                   ENDDO
014700000000E   1C                   ENDIF
014800000000     C*---
014900990610     C* SISDA -> SALDI CLIENTE AFFIDABILITA'
015000000000     C*---
015100000000IF  1C     PARSDA        IFEQ      'S'
015200990610     C     *LOVAL        SETLL     SISDA01L
015300990610     C                   READ      SISDA01L                               99
015400000000DO  2C     *IN99         DOWEQ     *OFF
015500000000     C                   MOVEL     'N'           WRECOK
015600000000     C                   EXSR      CHKSDA
015700000000IF  3C     WRECOK        IFEQ      'S'
015800000000     C                   Z-ADD     *ZEROS        SDANSP
015900000000     C                   MOVEL     *BLANKS       SDAFTR
016000000000     C                   Z-ADD     *ZEROS        SDADTR
016100000000     C                   ADD       1             RECSDA
016200990610     C                   UPDATE    SISDA000
016300000000E   3C                   ENDIF
016400990610     C                   READ      SISDA01L                               99
016500000000E   2C                   ENDDO
016600000000E   1C                   ENDIF
016700000000     C*---
016800990610     C* SISDP -> SALDI CLIENTE PARTICOLARITA'
016900000000     C*---
017000000000IF  1C     PARSDP        IFEQ      'S'
017100990610     C     *LOVAL        SETLL     SISDP01L
017200990610     C                   READ      SISDP01L                               99
017300000000DO  2C     *IN99         DOWEQ     *OFF
017400000000     C                   MOVEL     'N'           WRECOK
017500000000     C                   EXSR      CHKSDP
017600000000IF  3C     WRECOK        IFEQ      'S'
017700000000     C                   Z-ADD     *ZEROS        SDPNSP
017800000000     C                   Z-ADD     *ZEROS        SDPRCP
017900000000     C                   MOVEL     *BLANKS       SDPFTR
018000000000     C                   Z-ADD     *ZEROS        SDPDTR
018100000000     C                   ADD       1             RECSDP
018200990610     C                   UPDATE    SISDP000
018300000000E   3C                   ENDIF
018400990610     C                   READ      SISDP01L                               99
018500000000E   2C                   ENDDO
018600000000E   1C                   ENDIF
018700000000     C*---
018800990610     C* SISDF -> SALDI FILIALE TESTATA
018900000000     C*---
019000000000IF  1C     PARSDF        IFEQ      'S'
019100000000     C                   Z-ADD     PARANN        KDFANN
019200990610     C     KEYSDF        SETLL     SISDF01L
019300990610     C     KEYSDF        READE     SISDF01L                               99
019400000000DO  2C     *IN99         DOWEQ     *OFF
019500000000     C                   MOVEL     'N'           WRECOK
019600000000     C                   EXSR      CHKSDF
019700000000IF  3C     WRECOK        IFEQ      'S'
019800000000     C                   Z-ADD     *ZEROS        SDFNSP
019900000000     C                   Z-ADD     *ZEROS        SDFPKG
020000000000     C                   Z-ADD     *ZEROS        SDFNCO
020100000000     C                   Z-ADD     *ZEROS        SDFVOL
020200000000     C                   Z-ADD     *ZEROS        SDFPTS
020300000000     C                   Z-ADD     *ZEROS        SDFIRB
020400000000     C                   Z-ADD     *ZEROS        SDFIRR
020500000000     C                   Z-ADD     *ZEROS        SDFINC
020600000000     C                   ADD       1             RECSDF
020700990610     C                   UPDATE    SISDF000
020800000000E   3C                   ENDIF
020900990610     C     KEYSDF        READE     SISDF01L                               99
021000000000E   2C                   ENDDO
021100000000E   1C                   ENDIF
021200000000     C*---
021300990610     C* SISDR -> SALDI FILIALE DETTAGLIO
021400000000     C*---
021500000000IF  1C     PARSDR        IFEQ      'S'
021600000000     C                   Z-ADD     PARANN        KDRANN
021700990610     C     KEYSDR        SETLL     SISDR01L
021800990610     C     KEYSDR        READE     SISDR01L                               99
021900000000DO  2C     *IN99         DOWEQ     *OFF
022000000000     C                   MOVEL     'N'           WRECOK
022100000000     C                   EXSR      CHKSDR
022200000000IF  3C     WRECOK        IFEQ      'S'
022300000000     C                   Z-ADD     *ZEROS        SDRNSP
022400000000     C                   Z-ADD     *ZEROS        SDRPKG
022500000000     C                   Z-ADD     *ZEROS        SDRNCO
022600000000     C                   Z-ADD     *ZEROS        SDRVOL
022700000000     C                   Z-ADD     *ZEROS        SDRPTS
022800000000     C                   Z-ADD     *ZEROS        SDRIRB
022900000000     C                   Z-ADD     *ZEROS        SDRIRR
023000000000     C                   Z-ADD     *ZEROS        SDRINC
023100000000     C                   ADD       1             RECSDR
023200990610     C                   UPDATE    SISDR000
023300000000E   3C                   ENDIF
023400990610     C     KEYSDR        READE     SISDR01L                               99
023500000000E   2C                   ENDDO
023600000000E   1C                   ENDIF
023700000000     C*---
023800990610     C* SISDT -> SALDI FILIALE TRAZIONI
023900000000     C*---
024000000000IF  1C     PARSDT        IFEQ      'S'
024100000000     C                   Z-ADD     PARANN        KDTANN
024200990610     C     KEYSDT        SETLL     SISDT01L
024300990610     C     KEYSDT        READE     SISDT01L                               99
024400000000DO  2C     *IN99         DOWEQ     *OFF
024500000000     C                   MOVEL     'N'           WRECOK
024600000000     C                   EXSR      CHKSDT
024700000000IF  3C     WRECOK        IFEQ      'S'
024800000000     C                   Z-ADD     *ZEROS        SDTNSP
024900000000     C                   Z-ADD     *ZEROS        SDTPKG
025000000000     C                   Z-ADD     *ZEROS        SDTNCO
025100000000     C                   Z-ADD     *ZEROS        SDTVOL
025200000000     C                   Z-ADD     *ZEROS        SDTCTL
025300000000     C                   Z-ADD     *ZEROS        SDTCT1
025400000000     C                   Z-ADD     *ZEROS        SDTCT2
025500000000     C                   ADD       1             RECSDT
025600990610     C                   UPDATE    SISDT000
025700000000E   3C                   ENDIF
025800990610     C     KEYSDT        READE     SISDT01L                               99
025900000000E   2C                   ENDDO
026000000000E   1C                   ENDIF
026100000000     C*---
026200020219     C* ECMOV -> WFILE MOVIMENTI CONTABILI
026300000000     C*---
026400000000IF  1C     PARWFM        IFEQ      'S'
026500020219     C     *LOVAL        SETLL     ECMOV02L
026600020219     C                   READ      ECMOV02L                               99
026700011123DO  2C     *IN99         DOWEQ     *OFF
026800011123     C                   MOVEL     'N'           WRECOK
026900020219     C                   EXSR      CHKMOV
027000011123IF  3C     WRECOK        IFEQ      'S'
027100011123     C                   ADD       1             RECWFM
027200020219     C                   DELETE    ECMOV000
027300011123E   3C                   ENDIF
027400020219     C                   READ      ECMOV02L                               99
027500011123E   2C                   ENDDO
027600000000E   1C                   ENDIF
027700000000     C*---
027800000000     C* ECCET -> COMPETENZE BOLLE TESTATA
027900000000     C*---
028000000000IF  1C     PARCET        IFEQ      'S'
028100020520     C*
028200020520     C/EXEC SQL
028300020520     C+ delete from eccet00f where
028400020520     C+ cetaas= :PARANN and cetmes>= :PARMMI and cetmes<= :PARMMF
028500020520     C/END-EXEC
028600020520     C*
028700020520     C/EXEC SQL
028800020520     C+ delete from eccetP0f where
028900020520     C+ cetaas= :PARANN and cetmes>= :PARMMI and cetmes<= :PARMMF
029000020520     C/END-EXEC
029100000000E   1C                   ENDIF
029200000000     C*---
029300000000     C* ECCED -> COMPETENZE BOLLE DETTAGLIO
029400000000     C*---
029500000000IF  1C     PARCED        IFEQ      'S'
029600020520     C*
029700020520     C/EXEC SQL
029800020520     C+ delete from ecced00f where
029900020520     C+ cedaas= :PARANN and cedmes>= :PARMMI and cedmes<= :PARMMF
030000020520     C/END-EXEC
030100020520     C*
030200020520     C/EXEC SQL
030300020520     C+ delete from eccedP0f where
030400020520     C+ cedaas= :PARANN and cedmes>= :PARMMI and cedmes<= :PARMMF
030500020520     C/END-EXEC
030600000000E   1C                   ENDIF
030700990219     C*---
030800990219     C* ECFIM -> INGRAGRUPPO MCUBI
030900990219     C*---
031000990219IF  1C     PARFIM        IFEQ      'S'
031100020520     C*
031200020520     C/EXEC SQL
031300020520     C+ delete from ecfim00f where
031400020520     C+ fimaas= :PARANN and fimmes>= :PARMMI and fimmes<= :PARMMF
031500020520     C/END-EXEC
031600020520     C*
031700020520     C/EXEC SQL
031800020520     C+ delete from ecfimP0f where
031900020520     C+ fimaas= :PARANN and fimmes>= :PARMMI and fimmes<= :PARMMF
032000020520     C/END-EXEC
032100990219E   1C                   ENDIF
032200000000     C*
032300000000     C* STAMPA DI CONTROLLO
032400000000     C                   EXCEPT    CHK
032500000000     C*
032600000000     C                   SETON                                            LR
032700000000     C*------------------------------------------------------------------------*
032800000000     C* CHKSDO - CONTROLLA VALIDITA' RECORD
032900000000     C*------------------------------------------------------------------------*
033000000000     C     CHKSDO        BEGSR
033100000000     C*
033200000000     C                   MOVEL     'N'           WRECOK
033300000000     C*
033400000000     C* PERIODO NON RICHIESTO, ESCLUDE
033500000000IF  1C     SDOANN        IFEQ      PARANN
033600000000     C     SDOMES        ANDGE     PARMMI
033700000000     C     SDOMES        ANDLE     PARMMF
033800000000     C                   MOVEL     'S'           WRECOK
033900000000E   1C                   ENDIF
034000000000     C*
034100000000     C                   ENDSR
034200000000     C*------------------------------------------------------------------------*
034300000000     C* CHKSDC - CONTROLLA VALIDITA' RECORD
034400000000     C*------------------------------------------------------------------------*
034500000000     C     CHKSDC        BEGSR
034600000000     C*
034700000000     C                   MOVEL     'N'           WRECOK
034800000000     C*
034900000000     C* PERIODO NON RICHIESTO, ESCLUDE
035000000000IF  1C     SDCANN        IFEQ      PARANN
035100000000     C     SDCMES        ANDGE     PARMMI
035200000000     C     SDCMES        ANDLE     PARMMF
035300000000     C                   MOVEL     'S'           WRECOK
035400000000E   1C                   ENDIF
035500000000     C*
035600000000     C                   ENDSR
035700000000     C*------------------------------------------------------------------------*
035800000000     C* CHKSDA - CONTROLLA VALIDITA' RECORD
035900000000     C*------------------------------------------------------------------------*
036000000000     C     CHKSDA        BEGSR
036100000000     C*
036200000000     C                   MOVEL     'N'           WRECOK
036300000000     C*
036400000000     C* PERIODO NON RICHIESTO, ESCLUDE
036500000000IF  1C     SDAANN        IFEQ      PARANN
036600000000     C     SDAMES        ANDGE     PARMMI
036700000000     C     SDAMES        ANDLE     PARMMF
036800000000     C                   MOVEL     'S'           WRECOK
036900000000E   1C                   ENDIF
037000000000     C*
037100000000     C                   ENDSR
037200000000     C*------------------------------------------------------------------------*
037300000000     C* CHKSDP - CONTROLLA VALIDITA' RECORD
037400000000     C*------------------------------------------------------------------------*
037500000000     C     CHKSDP        BEGSR
037600000000     C*
037700000000     C                   MOVEL     'N'           WRECOK
037800000000     C*
037900000000     C* PERIODO NON RICHIESTO, ESCLUDE
038000000000IF  1C     SDPANN        IFEQ      PARANN
038100000000     C     SDPMES        ANDGE     PARMMI
038200000000     C     SDPMES        ANDLE     PARMMF
038300000000     C                   MOVEL     'S'           WRECOK
038400000000E   1C                   ENDIF
038500000000     C*
038600000000     C                   ENDSR
038700000000     C*------------------------------------------------------------------------*
038800000000     C* CHKSDF - CONTROLLA VALIDITA' RECORD
038900000000     C*------------------------------------------------------------------------*
039000000000     C     CHKSDF        BEGSR
039100000000     C*
039200000000     C                   MOVEL     'N'           WRECOK
039300000000     C*
039400000000     C* PERIODO NON RICHIESTO, ESCLUDE
039500000000IF  1C     SDFANN        IFEQ      PARANN
039600000000     C     SDFMES        ANDGE     PARMMI
039700000000     C     SDFMES        ANDLE     PARMMF
039800000000     C                   MOVEL     'S'           WRECOK
039900000000E   1C                   ENDIF
040000000000     C*
040100000000     C                   ENDSR
040200000000     C*------------------------------------------------------------------------*
040300000000     C* CHKSDR - CONTROLLA VALIDITA' RECORD
040400000000     C*------------------------------------------------------------------------*
040500000000     C     CHKSDR        BEGSR
040600000000     C*
040700000000     C                   MOVEL     'N'           WRECOK
040800000000     C*
040900000000     C* PERIODO NON RICHIESTO, ESCLUDE
041000000000IF  1C     SDRANN        IFEQ      PARANN
041100000000     C     SDRMES        ANDGE     PARMMI
041200000000     C     SDRMES        ANDLE     PARMMF
041300000000     C                   MOVEL     'S'           WRECOK
041400000000E   1C                   ENDIF
041500000000     C*
041600000000     C                   ENDSR
041700000000     C*------------------------------------------------------------------------*
041800000000     C* CHKSDT - CONTROLLA VALIDITA' RECORD
041900000000     C*------------------------------------------------------------------------*
042000000000     C     CHKSDT        BEGSR
042100000000     C*
042200000000     C                   MOVEL     'N'           WRECOK
042300000000     C*
042400000000     C* PERIODO NON RICHIESTO, ESCLUDE
042500000000IF  1C     SDTANN        IFEQ      PARANN
042600000000     C     SDTMES        ANDGE     PARMMI
042700000000     C     SDTMES        ANDLE     PARMMF
042800000000     C                   MOVEL     'S'           WRECOK
042900000000E   1C                   ENDIF
043000000000     C*
043100000000     C                   ENDSR
043200000000     C*------------------------------------------------------------------------*
043300020219     C* CHKMOV - CONTROLLA VALIDITA' RECORD
043400000000     C*------------------------------------------------------------------------*
043500020219     C     CHKMOV        BEGSR
043600000000     C*
043700000000     C                   MOVEL     'N'           WRECOK
043800000000     C*
043900000000     C* PERIODO NON RICHIESTO, ESCLUDE
044000020219IF  1C     MOVDRG        IFGE      PARDAI
044100020219     C     MOVDRG        ANDLE     PARDAF
044200000000     C                   MOVEL     'S'           WRECOK
044300000000E   1C                   ENDIF
044400000000     C*
044500000000     C                   ENDSR
044600000000     C*------------------------------------------------------------------------*
044700000000     C* *INZSR - OPERAZIONI INIZIALI
044800000000     C*------------------------------------------------------------------------*
044900000000     C     *INZSR        BEGSR
045000000000     C*
045100000000     C* RICEVIMENTO PARAMETRI
045200000000     C     *ENTRY        PLIST
045300000000     C                   PARM                    KPJBA
045400000000     C                   MOVEL     KPJBU         PARAM
045500000000     C*
045600000000     C* VARIABILI RIFERITE AL DATABASE
045700990610     C     *LIKE         DEFINE    SDFANN        KDFANN
045800990610     C     *LIKE         DEFINE    SDRANN        KDRANN
045900990610     C     *LIKE         DEFINE    SDTANN        KDTANN
046000990824     C     *LIKE         DEFINE    TASAAS        KASAAS
046100990219     C     *LIKE         DEFINE    TASLNP        KASLNP
046200990219     C     *LIKE         DEFINE    TASNRS        KASNRS
046300990219     C     *LIKE         DEFINE    TASNSP        KASNSP
046400990219     C     *LIKE         DEFINE    TASTBL        KASTBL
046500020219     C     *LIKE         DEFINE    MOVDRG        PARDAI
046600020219     C     *LIKE         DEFINE    MOVDRG        PARDAF
046700000000     C*
046800000000     C* CHIAVI DI LETTURA
046900990610     C     KEYSDF        KLIST
047000000000     C                   KFLD                    KDFANN
047100990610     C     KEYSDR        KLIST
047200000000     C                   KFLD                    KDRANN
047300990610     C     KEYSDT        KLIST
047400000000     C                   KFLD                    KDTANN
047500990824     C     KEYTAS        KLIST
047600990219     C                   KFLD                    KASAAS
047700990219     C                   KFLD                    KASLNP
047800990219     C                   KFLD                    KASNRS
047900990219     C                   KFLD                    KASNSP
048000990219     C                   KFLD                    KASTBL
048100000000     C*
048200000000     C* VARIABILI NON RIFERITE AL DATABASE
048300000000     C                   Z-ADD     *ZEROS        RECSDO            9 0
048400000000     C                   Z-ADD     *ZEROS        RECSDC            9 0
048500000000     C                   Z-ADD     *ZEROS        RECSDA            9 0
048600000000     C                   Z-ADD     *ZEROS        RECSDP            9 0
048700000000     C                   Z-ADD     *ZEROS        RECSDF            9 0
048800000000     C                   Z-ADD     *ZEROS        RECSDR            9 0
048900000000     C                   Z-ADD     *ZEROS        RECSDT            9 0
049000000000     C                   Z-ADD     *ZEROS        RECCET            9 0
049100000000     C                   Z-ADD     *ZEROS        RECCED            9 0
049200000000     C                   Z-ADD     *ZEROS        RECWFM            9 0
049300990219     C                   Z-ADD     *ZEROS        RECFIM            9 0
049400000000     C*
049500000000     C* VARIABILI DI CONTROLLO
049600000000     C                   MOVEL     'N'           WRECOK            1
049700000000     C*
049800000000     C* REPERIMENTO DATI DELL'UTENTE
049900000000     C                   CALL      'XFIL'
050000000000     C                   PARM                    DSFIL
050100000000     C*
050200020219     C* CALCOLA LE DATE DI CONFRONTO PER ECMOV
050300020219     C                   Z-ADD     PARANN        DSANN
050400000000     C                   Z-ADD     PARMMI        DSMES
050500000000     C                   Z-ADD     *ZEROS        DSGIO
050600020219     C                   Z-ADD     DSDAT8        PARDAI
050700020219     C                   Z-ADD     PARANN        DSANN
050800000000     C                   Z-ADD     PARMMF        DSMES
050900000000     C                   Z-ADD     99            DSGIO
051000020219     C                   Z-ADD     DSDAT8        PARDAF
051100000000     C*
051200000000     C                   ENDSR
051300000000     O*------------------------------------------------------------------------*
051400000000     O* STAMPA DI CONTROLLO
051500000000     O*------------------------------------------------------------------------*
051600000000     OQSYSPRT   E            CHK         2  1
051700000000     O                       STM(1)              66
051800000000     O                       STM(2)             132
051900000000     O                       DSFIRS              20
052000000000     O                       PARMMI             107
052100000000     O                       PARMMF             110
052200990610     O                       PARANN             115
052300000000     O                       UDATE         Y    132
052400000000     O          E            CHK         1
052500000000     O                       STM(3)              66
052600000000     O                       RECSDO        2     39
052700000000     O          E            CHK         1
052800000000     O                       STM(4)              66
052900000000     O                       RECSDC        2     39
053000000000     O          E            CHK         1
053100000000     O                       STM(5)              66
053200000000     O                       RECSDA        2     39
053300000000     O          E            CHK         1
053400000000     O                       STM(6)              66
053500000000     O                       RECSDP        2     39
053600000000     O          E            CHK         1
053700000000     O                       STM(7)              66
053800000000     O                       RECSDF        2     39
053900000000     O          E            CHK         1
054000000000     O                       STM(8)              66
054100000000     O                       RECSDR        2     39
054200000000     O          E            CHK         1
054300000000     O                       STM(9)              66
054400000000     O                       RECSDT        2     39
054500000000     O          E            CHK         1
054600000000     O                       STM(10)             66
054700011123     O                       RECWFM        2     39
054800000000     O          E            CHK         1
054900000000     O                       STM(12)             66
055000000000     O                       RECCET        2     39
055100000000     O          E            CHK         1
055200000000     O                       STM(13)             66
055300000000     O                       RECCED        2     39
055400990219     O          E            CHK         1
055500990219     O                       STM(15)             66
055600990219     O                       RECFIM        2     39
055700000000     O*------------------------------------------------------------------------*
055800990219**   STM - RIGHE DI STAMPA
055900000000XXXXXXXXXXXXXXXXXXXX         *** AZZERAMENTO FILES DI C/ECONOMICO 1
056000990610PER RICALCOLO COMPETENZE ***  PERIODO: 99/99 9999         99/99/992
056100990610REC AGGIORNATI SISDO     -> 999.999.999                           3
056200990610REC AGGIORNATI SISDC     -> 999.999 999                           4
056300990610REC AGGIORNATI SISDA     -> 999.999.999                           5
056400990610REC AGGIORNATI SISDP     -> 999.999.999                           6
056500990610REC AGGIORNATI SISDF     -> 999.999.999                           7
056600990610REC AGGIORNATI SISDR     -> 999.999.999                           8
056700990610REC AGGIORNATI SISDT     -> 999.999.999                           9
056800020219REC CANCELLATI ECMOV BAR -> 999.999.999                           10
056900000000REC CANCELLATI ECCET     -> 999.999.999                           12
057000000000REC CANCELLATI ECCED     -> 999.999.999                           13
05710001112714
057200990219REC CANCELLATI ECFIM     -> 999.999.999                           15
