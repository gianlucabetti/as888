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
001100040316     FSISDO01L  UF   E           K DISK
001200990610     FSISDC01L  UF   E           K DISK
001300990610     FSISDA01L  UF   E           K DISK
001400990610     FSISDP01L  UF   E           K DISK
001500990610     FSISDF01L  UF   E           K DISK
001600990610     FSISDR01L  UF   E           K DISK
001700990610     FSISDT01L  UF   E           K DISK
001800040316     FSISDE02L  UF   E           K DISK
001900030123     FSICOP01L  UF   E           K DISK
002000000000     F*---
002100000000     F* WFILE MOVIMENTI CONTABILI
002200000000     F*---
002300020614     FECMOV03L  UF   E           K DISK
002400990219     F*---
002500990219     F* BOLLE
002600990219     F* Solo per controllo
002700990219     F*---
002800990824     FTITAS30C  IF   E           K DISK
002900990824     F                                     IGNORE(TITASP00)
003000000000     F*---
003100000000     F* PRINTER FILE
003200000000     F*---
003300000000     FQSYSPRT   O    F  132        PRINTER
003400000000     D*------------------------------------------------------------------------*
003500000000     D* SCHIERE
003600000000     D*------------------------------------------------------------------------*
003700000000     D*---
003800000000     D* RIGHE DI STAMPA
003900000000     D*---
004000000000     D STM             S             66    DIM(15) CTDATA PERRCD(1)
004100000000     D*------------------------------------------------------------------------*
004200000000     D* INPUT
004300000000     D*------------------------------------------------------------------------*
004400040317     D dar5bnbsde    E DS
004500040322     D dar5bansde    E DS
004600000000     D*-------------------
004700000000     D* DS REPERIMENTO DATI UTENTE
004800000000     D*-------------------
004900020923     D TIBS34DS      E DS
005000020923     D DDATIUTE      E DS
005100020923     D AZUTEDS       E DS                  extname(AZUTE00F)
005200000000     D*---
005300000000     D* DS DATA GENERICA
005400000000     D*---
005500000000     D                 DS
005600020219     D  DSANN                  1      4  0
005700000000     D  DSMES                  5      6  0
005800000000     D  DSGIO                  7      8  0
005900000000     D  DSDAT8                 1      8  0
006000990219     D*---
006100990824     D* DS SCOMPOSIZIONE DATA TITAS
006200990219     D*---
006300990219     D                 DS
006400990219     D  TASMES                 1      2  0
006500990219     D  TASGIO                 3      4  0
006600990219     D  TASMGS                 1      4  0
006700000000     D*---
006800000000     D* PARAMETRI LANCIO
006900000000     D*---
007000000000     D PARAM           DS
007100990610     D  PARANN                 1      4  0
007200990610     D  PARMMI                 5      6  0
007300990610     D  PARMMF                 7      8  0
007400990610     D  PARSDO                 9      9
007500990610     D  PARSDC                10     10
007600990610     D  PARSDA                11     11
007700990610     D  PARSDP                12     12
007800990610     D  PARSDF                13     13
007900990610     D  PARSDR                14     14
008000990610     D  PARSDT                15     15
008100990610     D  PARWFM                16     16
008200990610     D  PARCET                17     17
008300990610     D  PARCED                18     18
008500030123     D  PARCOP                20     20
008600040316     D  PARsde                21     21
008700000000     D*---
008800000000     D* ARCHITETTURA
008900000000     D*---
009000000000     D KPJBA         E DS
009100000000     C*------------------------------------------------------------------------*
009200000000     C* MAIN LINE
009300000000     C*------------------------------------------------------------------------*
009400000000     C*---
009500990610     C* SISDO -> SALDI CLIENTE TESTATA
009600000000     C*---
009700000000IF  1C     PARSDO        IFEQ      'S'
009800990610     C     *LOVAL        SETLL     SISDO01L
009900990610     C                   READ      SISDO01L                               99
010000000000DO  2C     *IN99         DOWEQ     *OFF
010100000000     C                   MOVEL     'N'           WRECOK
010200000000     C                   EXSR      CHKSDO
010300000000IF  3C     WRECOK        IFEQ      'S'
010400000000     C                   Z-ADD     *ZEROS        SDONSP
010500000000     C                   Z-ADD     *ZEROS        SDOPKG
010600000000     C                   Z-ADD     *ZEROS        SDONCO
010700000000     C                   Z-ADD     *ZEROS        SDOVOL
010800000000     C                   Z-ADD     *ZEROS        SDOIRB
010900000000     C                   Z-ADD     *ZEROS        SDOIRR
011000000000     C                   Z-ADD     *ZEROS        SDOINC
011100000000     C                   Z-ADD     *ZEROS        SDOIRP
011200000304     C                   Z-ADD     *ZEROS        SDOIRC
011300000000     C                   Z-ADD     *ZEROS        SDOIRD
011400000000     C                   Z-ADD     *ZEROS        SDONNC
011500000000     C                   MOVEL     *BLANKS       SDOCLN
011600000000     C                   MOVEL     *BLANKS       SDOFTR
011700000000     C                   Z-ADD     *ZEROS        SDODTR
011800000000     C                   ADD       1             RECSDO
011900990610     C                   UPDATE    SISDO000
012000000000E   3C                   ENDIF
012100990610     C                   READ      SISDO01L                               99
012200000000E   2C                   ENDDO
012300000000E   1C                   ENDIF
012400000000     C*---
012500990610     C* SISDC -> SALDI CLIENTE DETTAGLIO
012600000000     C*---
012700000000IF  1C     PARSDC        IFEQ      'S'
012800990610     C     *LOVAL        SETLL     SISDC01L
012900990610     C                   READ      SISDC01L                               99
013000000000DO  2C     *IN99         DOWEQ     *OFF
013100000000     C                   MOVEL     'N'           WRECOK
013200000000     C                   EXSR      CHKSDC
013300000000IF  3C     WRECOK        IFEQ      'S'
013400000000     C                   Z-ADD     *ZEROS        SDCNSP
013500000000     C                   Z-ADD     *ZEROS        SDCPKG
013600990610     C                   Z-ADD     *ZEROS        SDCPCC
013700990610     C                   Z-ADD     *ZEROS        SDCPLS
013800000000     C                   Z-ADD     *ZEROS        SDCNCO
013900000000     C                   Z-ADD     *ZEROS        SDCVOL
014000000000     C                   Z-ADD     *ZEROS        SDCVCC
014100000000     C                   Z-ADD     *ZEROS        SDCVLS
014200000000     C                   Z-ADD     *ZEROS        SDCIRB
014300000000     C                   Z-ADD     *ZEROS        SDCIRR
014400000000     C                   Z-ADD     *ZEROS        SDCINC
014500000000     C                   Z-ADD     *ZEROS        SDCIRP
014600000304     C                   Z-ADD     *ZEROS        SDCIRC
014700000000     C                   Z-ADD     *ZEROS        SDCIRD
014800000000     C                   MOVEL     *BLANKS       SDCFTR
014900000000     C                   Z-ADD     *ZEROS        SDCDTR
015000000000     C                   ADD       1             RECSDC
015100990610     C                   UPDATE    SISDC000
015200000000E   3C                   ENDIF
015300990610     C                   READ      SISDC01L                               99
015400000000E   2C                   ENDDO
015500000000E   1C                   ENDIF
015600000000     C*---
015700990610     C* SISDA -> SALDI CLIENTE AFFIDABILITA'
015800000000     C*---
015900000000IF  1C     PARSDA        IFEQ      'S'
016000990610     C     *LOVAL        SETLL     SISDA01L
016100990610     C                   READ      SISDA01L                               99
016200000000DO  2C     *IN99         DOWEQ     *OFF
016300000000     C                   MOVEL     'N'           WRECOK
016400000000     C                   EXSR      CHKSDA
016500000000IF  3C     WRECOK        IFEQ      'S'
016600000000     C                   Z-ADD     *ZEROS        SDANSP
016700000000     C                   MOVEL     *BLANKS       SDAFTR
016800000000     C                   Z-ADD     *ZEROS        SDADTR
016900000000     C                   ADD       1             RECSDA
017000990610     C                   UPDATE    SISDA000
017100000000E   3C                   ENDIF
017200990610     C                   READ      SISDA01L                               99
017300000000E   2C                   ENDDO
017400000000E   1C                   ENDIF
017500000000     C*---
017600990610     C* SISDP -> SALDI CLIENTE PARTICOLARITA'
017700000000     C*---
017800000000IF  1C     PARSDP        IFEQ      'S'
017900990610     C     *LOVAL        SETLL     SISDP01L
018000990610     C                   READ      SISDP01L                               99
018100000000DO  2C     *IN99         DOWEQ     *OFF
018200000000     C                   MOVEL     'N'           WRECOK
018300000000     C                   EXSR      CHKSDP
018400000000IF  3C     WRECOK        IFEQ      'S'
018500000000     C                   Z-ADD     *ZEROS        SDPNSP
018600000000     C                   Z-ADD     *ZEROS        SDPRCP
018700000000     C                   MOVEL     *BLANKS       SDPFTR
018800000000     C                   Z-ADD     *ZEROS        SDPDTR
018900000000     C                   ADD       1             RECSDP
019000990610     C                   UPDATE    SISDP000
019100000000E   3C                   ENDIF
019200990610     C                   READ      SISDP01L                               99
019300000000E   2C                   ENDDO
019400000000E   1C                   ENDIF
019500000000     C*---
019600990610     C* SISDF -> SALDI FILIALE TESTATA
019700000000     C*---
019800000000IF  1C     PARSDF        IFEQ      'S'
019900000000     C                   Z-ADD     PARANN        KDFANN
020000990610     C     KEYSDF        SETLL     SISDF01L
020100990610     C     KEYSDF        READE     SISDF01L                               99
020200000000DO  2C     *IN99         DOWEQ     *OFF
020300000000     C                   MOVEL     'N'           WRECOK
020400000000     C                   EXSR      CHKSDF
020500000000IF  3C     WRECOK        IFEQ      'S'
020600000000     C                   Z-ADD     *ZEROS        SDFNSP
020700000000     C                   Z-ADD     *ZEROS        SDFPKG
020800000000     C                   Z-ADD     *ZEROS        SDFNCO
020900000000     C                   Z-ADD     *ZEROS        SDFVOL
021000000000     C                   Z-ADD     *ZEROS        SDFPTS
021100000000     C                   Z-ADD     *ZEROS        SDFIRB
021200000000     C                   Z-ADD     *ZEROS        SDFIRR
021300000000     C                   Z-ADD     *ZEROS        SDFINC
021400000000     C                   ADD       1             RECSDF
021500990610     C                   UPDATE    SISDF000
021600000000E   3C                   ENDIF
021700990610     C     KEYSDF        READE     SISDF01L                               99
021800000000E   2C                   ENDDO
021900000000E   1C                   ENDIF
022000000000     C*---
022100990610     C* SISDR -> SALDI FILIALE DETTAGLIO
022200000000     C*---
022300000000IF  1C     PARSDR        IFEQ      'S'
022400000000     C                   Z-ADD     PARANN        KDRANN
022500990610     C     KEYSDR        SETLL     SISDR01L
022600990610     C     KEYSDR        READE     SISDR01L                               99
022700000000DO  2C     *IN99         DOWEQ     *OFF
022800000000     C                   MOVEL     'N'           WRECOK
022900000000     C                   EXSR      CHKSDR
023000000000IF  3C     WRECOK        IFEQ      'S'
023100000000     C                   Z-ADD     *ZEROS        SDRNSP
023200000000     C                   Z-ADD     *ZEROS        SDRPKG
023300000000     C                   Z-ADD     *ZEROS        SDRNCO
023400000000     C                   Z-ADD     *ZEROS        SDRVOL
023500000000     C                   Z-ADD     *ZEROS        SDRPTS
023600000000     C                   Z-ADD     *ZEROS        SDRIRB
023700000000     C                   Z-ADD     *ZEROS        SDRIRR
023800000000     C                   Z-ADD     *ZEROS        SDRINC
023900000000     C                   ADD       1             RECSDR
024000990610     C                   UPDATE    SISDR000
024100000000E   3C                   ENDIF
024200990610     C     KEYSDR        READE     SISDR01L                               99
024300000000E   2C                   ENDDO
024400000000E   1C                   ENDIF
024500000000     C*---
024600990610     C* SISDT -> SALDI FILIALE TRAZIONI
024700000000     C*---
024800000000IF  1C     PARSDT        IFEQ      'S'
024900000000     C                   Z-ADD     PARANN        KDTANN
025000990610     C     KEYSDT        SETLL     SISDT01L
025100990610     C     KEYSDT        READE     SISDT01L                               99
025200000000DO  2C     *IN99         DOWEQ     *OFF
025300000000     C                   MOVEL     'N'           WRECOK
025400000000     C                   EXSR      CHKSDT
025500000000IF  3C     WRECOK        IFEQ      'S'
025600000000     C                   Z-ADD     *ZEROS        SDTNSP
025700000000     C                   Z-ADD     *ZEROS        SDTPKG
025800000000     C                   Z-ADD     *ZEROS        SDTNCO
025900000000     C                   Z-ADD     *ZEROS        SDTVOL
026000000000     C                   Z-ADD     *ZEROS        SDTCTL
026100000000     C                   Z-ADD     *ZEROS        SDTCT1
026200000000     C                   Z-ADD     *ZEROS        SDTCT2
026300040524     C                   Z-ADD     *ZEROS        SDTCT3
026400040524     C                   Z-ADD     *ZEROS        SDTCT4
026500040524     C                   Z-ADD     *ZEROS        SDTCT5
026600000000     C                   ADD       1             RECSDT
026700990610     C                   UPDATE    SISDT000
026800000000E   3C                   ENDIF
026900990610     C     KEYSDT        READE     SISDT01L                               99
027000000000E   2C                   ENDDO
027100000000E   1C                   ENDIF
027200030123     C*---
027300030123     C* SICOP -> SALDI COOPERATIVE
027400030123     C*---
027500030123IF  1C     PARCOP        IFEQ      'S'
027600030123     C     *LOVAL        SETLL     SICOP01L
027700030123     C                   READ      SICOP01L                               99
027800030123DO  2C     *IN99         DOWEQ     *OFF
027900030123     C                   MOVEL     'N'           WRECOK
028000030123     C                   EXSR      CHKCOP
028100030123IF  3C     WRECOK        IFEQ      'S'
028200030123     C                   Z-ADD     *ZEROS        COPIMV
028300030123     C                   Z-ADD     *ZEROS        COPNSP
028400030123     C                   Z-ADD     *ZEROS        COPNCL
028500030123     C                   Z-ADD     *ZEROS        COPPKF
028600030123     C                   Z-ADD     *ZEROS        COPVLF
028700030123     C                   Z-ADD     *ZEROS        COPBAN
028800030123     C                   MOVEL     *BLANKS       COPFL1
028900030123     C                   ADD       1             RECCOP
029000030123     C                   UPDATE    SICOP000
029100030123E   3C                   ENDIF
029200030123     C                   READ      SICOP01L                               99
029300030123E   2C                   ENDDO
029400030123E   1C                   ENDIF
029500040316     C*---
029600040316     C* SISDE -> SALDI estensione
029700040316     C*---
029800040316IF  1C     PARSDE        IFEQ      'S'
029900040316     C     *LOVAL        SETLL     SISDe02L
030000040316     C                   READ      SISDe02L                               99
030100040316DO  2C     *IN99         DOWEQ     *OFF
030200040316     C                   MOVEL     'N'           WRECOK
030300040316     C                   EXSR      CHKSDe
030400040316IF  3C     WRECOK        IFEQ      'S'
030500040322     c                   z-add     0             oksde             1 0
030600040316     c                   select
030700040316     c                   when      sdetrd = 'BNB'
030800040317     c                   clear                   dar5bnbsde
030900040317     C                   MOVEL     dar5bnbsde    SDeuni
031000040322     c                   when      sdetrd = 'BAN'
031100040322     c                   clear                   dar5bansde
031200040322     C                   MOVEL     dar5bansde    SDeuni
031300040322     c                   other
031400040322     c                   z-add     1             oksde
031500040322     c                   except    errsde
031600040316     c                   endsl
031700040322     c                   if        oksde = 0
031800040316     C                   MOVEL     *BLANKS       SDeFTR
031900040316     C                   Z-ADD     *ZEROS        SDeDTR
032000040316     C                   ADD       1             RECSDe
032100040316     C                   UPDATE    SISDe000
032200040322     c                   end
032300040316E   3C                   ENDIF
032400040316     C                   READ      SISDe02L                               99
032500040316E   2C                   ENDDO
032600040316E   1C                   ENDIF
032700000000     C*---
032800020219     C* ECMOV -> WFILE MOVIMENTI CONTABILI
032900000000     C*---
033000000000IF  1C     PARWFM        IFEQ      'S'
033100020614     C     *LOVAL        SETLL     ECMOV03L
033200020614     C                   READ      ECMOV03L                               99
033300011123DO  2C     *IN99         DOWEQ     *OFF
033400011123     C                   MOVEL     'N'           WRECOK
033500020219     C                   EXSR      CHKMOV
033600011123IF  3C     WRECOK        IFEQ      'S'
033700011123     C                   ADD       1             RECWFM
033800020219     C                   DELETE    ECMOV000
033900011123E   3C                   ENDIF
034000020614     C                   READ      ECMOV03L                               99
034100011123E   2C                   ENDDO
034200000000E   1C                   ENDIF
034300000000     C*---
034400000000     C* ECCET -> COMPETENZE BOLLE TESTATA
034500000000     C*---
034600000000IF  1C     PARCET        IFEQ      'S'
034700020520     C*
034800020520     C/EXEC SQL
034900020520     C+ delete from eccet00f where
035000020520     C+ cetaas= :PARANN and cetmes>= :PARMMI and cetmes<= :PARMMF
035100020520     C/END-EXEC
035200020520     C*
035300020520     C/EXEC SQL
035400020520     C+ delete from eccetP0f where
035500020520     C+ cetaas= :PARANN and cetmes>= :PARMMI and cetmes<= :PARMMF
035600020520     C/END-EXEC
035700000000E   1C                   ENDIF
035800000000     C*---
035900000000     C* ECCED -> COMPETENZE BOLLE DETTAGLIO
036000000000     C*---
036100000000IF  1C     PARCED        IFEQ      'S'
036200020520     C*
036300020520     C/EXEC SQL
036400020520     C+ delete from ecced00f where
036500020520     C+ cedaas= :PARANN and cedmes>= :PARMMI and cedmes<= :PARMMF
036600020520     C/END-EXEC
036700020520     C*
036800020520     C/EXEC SQL
036900020520     C+ delete from eccedP0f where
037000020520     C+ cedaas= :PARANN and cedmes>= :PARMMI and cedmes<= :PARMMF
037100020520     C/END-EXEC
037200000000E   1C                   ENDIF
038800030123     C*---
038900030512     C* ECCEV -> VOCI C/E TESTATA
039000030123     C*---
039100030123IF  1C     PARCOP        IFEQ      'S'
039200030123     C*
039300030123     C/EXEC SQL
039400030123     C+ delete from eccev00f where
039500030123     C+ cevaas= :PARANN and cevmes>= :PARMMI and cevmes<= :PARMMF
039600030123     C/END-EXEC
039700030123     C*
039800030123     C/EXEC SQL
039900030123     C+ delete from eccevP0f where
040000030123     C+ cevaas= :PARANN and cevmes>= :PARMMI and cevmes<= :PARMMF
040100030123     C/END-EXEC
040200030123E   1C                   ENDIF
040300030512     C*---
040400030512     C* ECC2V -> VOCI C/E ESTENSIONE
040500030512     C*---
040600030512IF  1C     PARCOP        IFEQ      'S'
040700030512     C*
040800030512     C/EXEC SQL
040900030512     C+ delete from ecc2v00f where
041000030512     C+ c2vaas= :PARANN and c2vmes>= :PARMMI and c2vmes<= :PARMMF
041100030512     C/END-EXEC
041200030512     C*
041300030512     C/EXEC SQL
041400030512     C+ delete from ecc2vP0f where
041500030512     C+ c2vaas= :PARANN and c2vmes>= :PARMMI and c2vmes<= :PARMMF
041600030512     C/END-EXEC
041700030512E   1C                   ENDIF
041800000000     C*
041900000000     C* STAMPA DI CONTROLLO
042000000000     C                   EXCEPT    CHK
042100000000     C*
042200000000     C                   SETON                                            LR
042300000000     C*------------------------------------------------------------------------*
042400000000     C* CHKSDO - CONTROLLA VALIDITA' RECORD
042500000000     C*------------------------------------------------------------------------*
042600000000     C     CHKSDO        BEGSR
042700000000     C*
042800000000     C                   MOVEL     'N'           WRECOK
042900000000     C*
043000000000     C* PERIODO NON RICHIESTO, ESCLUDE
043100000000IF  1C     SDOANN        IFEQ      PARANN
043200000000     C     SDOMES        ANDGE     PARMMI
043300000000     C     SDOMES        ANDLE     PARMMF
043400000000     C                   MOVEL     'S'           WRECOK
043500000000E   1C                   ENDIF
043600000000     C*
043700000000     C                   ENDSR
043800000000     C*------------------------------------------------------------------------*
043900000000     C* CHKSDC - CONTROLLA VALIDITA' RECORD
044000000000     C*------------------------------------------------------------------------*
044100000000     C     CHKSDC        BEGSR
044200000000     C*
044300000000     C                   MOVEL     'N'           WRECOK
044400000000     C*
044500000000     C* PERIODO NON RICHIESTO, ESCLUDE
044600000000IF  1C     SDCANN        IFEQ      PARANN
044700000000     C     SDCMES        ANDGE     PARMMI
044800000000     C     SDCMES        ANDLE     PARMMF
044900000000     C                   MOVEL     'S'           WRECOK
045000000000E   1C                   ENDIF
045100000000     C*
045200000000     C                   ENDSR
045300000000     C*------------------------------------------------------------------------*
045400000000     C* CHKSDA - CONTROLLA VALIDITA' RECORD
045500000000     C*------------------------------------------------------------------------*
045600000000     C     CHKSDA        BEGSR
045700000000     C*
045800000000     C                   MOVEL     'N'           WRECOK
045900000000     C*
046000000000     C* PERIODO NON RICHIESTO, ESCLUDE
046100000000IF  1C     SDAANN        IFEQ      PARANN
046200000000     C     SDAMES        ANDGE     PARMMI
046300000000     C     SDAMES        ANDLE     PARMMF
046400000000     C                   MOVEL     'S'           WRECOK
046500000000E   1C                   ENDIF
046600000000     C*
046700000000     C                   ENDSR
046800000000     C*------------------------------------------------------------------------*
046900000000     C* CHKSDP - CONTROLLA VALIDITA' RECORD
047000000000     C*------------------------------------------------------------------------*
047100000000     C     CHKSDP        BEGSR
047200000000     C*
047300000000     C                   MOVEL     'N'           WRECOK
047400000000     C*
047500000000     C* PERIODO NON RICHIESTO, ESCLUDE
047600000000IF  1C     SDPANN        IFEQ      PARANN
047700000000     C     SDPMES        ANDGE     PARMMI
047800000000     C     SDPMES        ANDLE     PARMMF
047900000000     C                   MOVEL     'S'           WRECOK
048000000000E   1C                   ENDIF
048100000000     C*
048200000000     C                   ENDSR
048300000000     C*------------------------------------------------------------------------*
048400000000     C* CHKSDF - CONTROLLA VALIDITA' RECORD
048500000000     C*------------------------------------------------------------------------*
048600000000     C     CHKSDF        BEGSR
048700000000     C*
048800000000     C                   MOVEL     'N'           WRECOK
048900000000     C*
049000000000     C* PERIODO NON RICHIESTO, ESCLUDE
049100000000IF  1C     SDFANN        IFEQ      PARANN
049200000000     C     SDFMES        ANDGE     PARMMI
049300000000     C     SDFMES        ANDLE     PARMMF
049400000000     C                   MOVEL     'S'           WRECOK
049500000000E   1C                   ENDIF
049600000000     C*
049700000000     C                   ENDSR
049800000000     C*------------------------------------------------------------------------*
049900000000     C* CHKSDR - CONTROLLA VALIDITA' RECORD
050000000000     C*------------------------------------------------------------------------*
050100000000     C     CHKSDR        BEGSR
050200000000     C*
050300000000     C                   MOVEL     'N'           WRECOK
050400000000     C*
050500000000     C* PERIODO NON RICHIESTO, ESCLUDE
050600000000IF  1C     SDRANN        IFEQ      PARANN
050700000000     C     SDRMES        ANDGE     PARMMI
050800000000     C     SDRMES        ANDLE     PARMMF
050900000000     C                   MOVEL     'S'           WRECOK
051000000000E   1C                   ENDIF
051100000000     C*
051200000000     C                   ENDSR
051300000000     C*------------------------------------------------------------------------*
051400000000     C* CHKSDT - CONTROLLA VALIDITA' RECORD
051500000000     C*------------------------------------------------------------------------*
051600000000     C     CHKSDT        BEGSR
051700000000     C*
051800000000     C                   MOVEL     'N'           WRECOK
051900000000     C*
052000000000     C* PERIODO NON RICHIESTO, ESCLUDE
052100000000IF  1C     SDTANN        IFEQ      PARANN
052200000000     C     SDTMES        ANDGE     PARMMI
052300000000     C     SDTMES        ANDLE     PARMMF
052400000000     C                   MOVEL     'S'           WRECOK
052500000000E   1C                   ENDIF
052600000000     C*
052700000000     C                   ENDSR
052800030123     C*------------------------------------------------------------------------*
052900030123     C* CHKCOP - CONTROLLA VALIDITA' RECORD
053000030123     C*------------------------------------------------------------------------*
053100030123     C     CHKCOP        BEGSR
053200030123     C*
053300030123     C                   MOVEL     'N'           WRECOK
053400030131     C*
053500030131     C* PERIODO NON RICHIESTO, ESCLUDE
053600030131IF  1C     COPAAS        IFEQ      PARANN
053700030131     C     COPMES        ANDGE     PARMMI
053800030131     C     COPMES        ANDLE     PARMMF
053900030131     C                   MOVEL     'S'           WRECOK
054000030131E   1C                   ENDIF
054100030123     C*
054200030123     C                   ENDSR
054300040316     C*------------------------------------------------------------------------*
054400040316     C* CHKSDe - CONTROLLA VALIDITA' RECORD
054500040316     C*------------------------------------------------------------------------*
054600040316     C     CHKSDe        BEGSR
054700040316     C*
054800040316     C                   MOVEL     'N'           WRECOK
054900040316     C*
055000040316     C* PERIODO NON RICHIESTO, ESCLUDE
055100040316IF  1C     SDeANN        IFEQ      PARANN
055200040316     C     SDeMES        ANDGE     PARMMI
055300040316     C     SDeMES        ANDLE     PARMMF
055400040316     C                   MOVEL     'S'           WRECOK
055500040316E   1C                   ENDIF
055600040316     C*
055700040316     C                   ENDSR
055800000000     C*------------------------------------------------------------------------*
055900020219     C* CHKMOV - CONTROLLA VALIDITA' RECORD
056000000000     C*------------------------------------------------------------------------*
056100020219     C     CHKMOV        BEGSR
056200000000     C*
056300000000     C                   MOVEL     'N'           WRECOK
056400000000     C*
056500000000     C* PERIODO NON RICHIESTO, ESCLUDE
056600020219IF  1C     MOVDRG        IFGE      PARDAI
056700020219     C     MOVDRG        ANDLE     PARDAF
056800000000     C                   MOVEL     'S'           WRECOK
056900000000E   1C                   ENDIF
057000000000     C*
057100000000     C                   ENDSR
057200020923     C*-------------------------------------------------------------------------
057300020923     C* REPERISCE I DATI UTENTE
057400020923     C*-------------------------------------------------------------------------
057500020923     C     REPDATIUTE    BEGSR
057600020923     C*
057700020923     C* INIZIALIZZA VARIABILI DI WRK
057800020923     C                   CLEAR                   TIBS34DS
057900020923     C                   CLEAR                   AZUTEDS
058000020923     C                   CLEAR                   DDATIUTE
058100020923     C*
058200020923     C     *DTAARA       DEFINE    §azute        azuteds
058300020923     C     *DTAARA       DEFINE    §datiute      ddatiute
058400020923     C                   IN(E)     *DTAARA
058500020923if  1C                   IF        %Error
058600020923     c                   EVAL      I34Tla = 'L'
058700020923     C                   CALL      'TIBS34R'
058800020923     C                   PARM                    Tibs34Ds
058900020923     C                   IN        *DTAARA
059000020923e   1C                   ENDIF
059100020923     C*
059200020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
059300020923     C                   MOVEL(P)  RSUT          DSFIRS           20
059400020923     C*
059500020923     C                   ENDSR
059600000000     C*------------------------------------------------------------------------*
059700000000     C* *INZSR - OPERAZIONI INIZIALI
059800000000     C*------------------------------------------------------------------------*
059900000000     C     *INZSR        BEGSR
060000000000     C*
060100000000     C* RICEVIMENTO PARAMETRI
060200000000     C     *ENTRY        PLIST
060300000000     C                   PARM                    KPJBA
060400000000     C                   MOVEL     KPJBU         PARAM
060500000000     C*
060600000000     C* VARIABILI RIFERITE AL DATABASE
060700990610     C     *LIKE         DEFINE    SDFANN        KDFANN
060800990610     C     *LIKE         DEFINE    SDRANN        KDRANN
060900990610     C     *LIKE         DEFINE    SDTANN        KDTANN
061000990824     C     *LIKE         DEFINE    TASAAS        KASAAS
061100990219     C     *LIKE         DEFINE    TASLNP        KASLNP
061200990219     C     *LIKE         DEFINE    TASNRS        KASNRS
061300990219     C     *LIKE         DEFINE    TASNSP        KASNSP
061400990219     C     *LIKE         DEFINE    TASTBL        KASTBL
061500020219     C     *LIKE         DEFINE    MOVDRG        PARDAI
061600020219     C     *LIKE         DEFINE    MOVDRG        PARDAF
061700000000     C*
061800000000     C* CHIAVI DI LETTURA
061900990610     C     KEYSDF        KLIST
062000000000     C                   KFLD                    KDFANN
062100990610     C     KEYSDR        KLIST
062200000000     C                   KFLD                    KDRANN
062300990610     C     KEYSDT        KLIST
062400000000     C                   KFLD                    KDTANN
062500990824     C     KEYTAS        KLIST
062600990219     C                   KFLD                    KASAAS
062700990219     C                   KFLD                    KASLNP
062800990219     C                   KFLD                    KASNRS
062900990219     C                   KFLD                    KASNSP
063000990219     C                   KFLD                    KASTBL
063100000000     C*
063200000000     C* VARIABILI NON RIFERITE AL DATABASE
063300000000     C                   Z-ADD     *ZEROS        RECSDO            9 0
063400000000     C                   Z-ADD     *ZEROS        RECSDC            9 0
063500000000     C                   Z-ADD     *ZEROS        RECSDA            9 0
063600000000     C                   Z-ADD     *ZEROS        RECSDP            9 0
063700000000     C                   Z-ADD     *ZEROS        RECSDF            9 0
063800000000     C                   Z-ADD     *ZEROS        RECSDR            9 0
063900000000     C                   Z-ADD     *ZEROS        RECSDT            9 0
064000000000     C                   Z-ADD     *ZEROS        RECWFM            9 0
064100030123     C                   Z-ADD     *ZEROS        RECCOP            9 0
064200040316     C                   Z-ADD     *ZEROS        RECSDe            9 0
064300000000     C*
064400000000     C* VARIABILI DI CONTROLLO
064500000000     C                   MOVEL     'N'           WRECOK            1
064600000000     C*
064700000000     C* REPERIMENTO DATI DELL'UTENTE
064800020923     C                   EXSR      REPDATIUTE
064900000000     C*
065000020219     C* CALCOLA LE DATE DI CONFRONTO PER ECMOV
065100020219     C                   Z-ADD     PARANN        DSANN
065200000000     C                   Z-ADD     PARMMI        DSMES
065300000000     C                   Z-ADD     *ZEROS        DSGIO
065400020219     C                   Z-ADD     DSDAT8        PARDAI
065500020219     C                   Z-ADD     PARANN        DSANN
065600000000     C                   Z-ADD     PARMMF        DSMES
065700000000     C                   Z-ADD     99            DSGIO
065800020219     C                   Z-ADD     DSDAT8        PARDAF
065900040322     c                   movel     *all'?'       all              66
066000000000     C*
066100000000     C                   ENDSR
066200000000     O*------------------------------------------------------------------------*
066300000000     O* STAMPA DI CONTROLLO
066400000000     O*------------------------------------------------------------------------*
066500000000     OQSYSPRT   E            CHK         2  1
066600000000     O                       STM(1)              66
066700000000     O                       STM(2)             132
066800000000     O                       DSFIRS              20
066900000000     O                       PARMMI             107
067000000000     O                       PARMMF             110
067100990610     O                       PARANN             115
067200000000     O                       UDATE         Y    132
067300000000     O          E            CHK         1
067400000000     O                       STM(3)              66
067500000000     O                       RECSDO        2     39
067600000000     O          E            CHK         1
067700000000     O                       STM(4)              66
067800000000     O                       RECSDC        2     39
067900000000     O          E            CHK         1
068000000000     O                       STM(5)              66
068100000000     O                       RECSDA        2     39
068200000000     O          E            CHK         1
068300000000     O                       STM(6)              66
068400000000     O                       RECSDP        2     39
068500000000     O          E            CHK         1
068600000000     O                       STM(7)              66
068700000000     O                       RECSDF        2     39
068800000000     O          E            CHK         1
068900000000     O                       STM(8)              66
069000000000     O                       RECSDR        2     39
069100000000     O          E            CHK         1
069200000000     O                       STM(9)              66
069300000000     O                       RECSDT        2     39
069400000000     O          E            CHK         1
069500000000     O                       STM(10)             66
069600011123     O                       RECWFM        2     39
069700030123     O          E            CHK         1
069800030123     O                       STM(11)             66
069900030123     O                       RECCOP        2     39
070000040316     O          E            CHK         1
070100040316     O                       STM(12)             66
070200040316     O                       RECSDe        2     39
070300020617     O          E            CHK         2
070400020617     O                       STM(14)             66
070500020617     O                       STM(15)            132
070600040322     O          E            errsde      2
070700040322     O                       STM(13)             66
070800040322     O                       sdetrd              56
070900040322     o                       all                132
071000000000     O*------------------------------------------------------------------------*
071100990219**   STM - RIGHE DI STAMPA
071200000000XXXXXXXXXXXXXXXXXXXX         *** AZZERAMENTO FILES DI C/ECONOMICO 1
071300990610PER RICALCOLO COMPETENZE ***  PERIODO: 99/99 9999         99/99/992
071400990610REC AGGIORNATI SISDO     -> 999.999.999                           3
071500990610REC AGGIORNATI SISDC     -> 999.999 999                           4
071600990610REC AGGIORNATI SISDA     -> 999.999.999                           5
071700990610REC AGGIORNATI SISDP     -> 999.999.999                           6
071800990610REC AGGIORNATI SISDF     -> 999.999.999                           7
071900990610REC AGGIORNATI SISDR     -> 999.999.999                           8
072000990610REC AGGIORNATI SISDT     -> 999.999.999                           9
072100020617REC CANCELLATI ECMOV     -> 999.999.999                           10
072200030123REC AGGIORNATI SICOP     -> 999.999.999                           11
072300040316REC AGGIORNATI SISDE     -> 999.999.999                           12
072400040322Attenzione tipo record nel file SISDE00F non pulito: xxx          13
072500171107I dati dei record degli archivi ECCET, ECCED se selezionati non so
072600171107no riportati perchè con l'SQL non vengono contati.
