000100921021     H DECEDIT('0,') DATEDIT(*DMY.)
000200050126     H* FNLV59R  *---------------------------------------------------*
000300941110     H*     CARICAMENTO TARIFFE DI UN CLIENTE                         *
000400921021     H*--------------------------------------------------------------*
000500050127     FTNTAM01L  IF   E           K DISK
000600941110     FTABEL00F  IF   E           K DISK    USROPN
000700941104     D*
000800050127     D CTV             S              3    DIM(50)
000900050127     D CTN             S              3    DIM(50)
001000050127     d ctw             s              3    Dim(50)
001100050218     d ctcli           s              3    Dim(50)
001200060503     d
001300050127     d
001400050127     D baV             S              1    DIM(50)
001500050127     D baVdop          S              1    DIM(50)
001600050127     D ban             S              1    DIM(50)
001700050127     D baNdop          S              1    DIM(50)
001800050218     D bacli           S              1    DIM(50)
001900050127     d
002000050127     D vaV             S              1    DIM(50)
002100050218     D vacli           S              1    DIM(50)
002200050127     d
002300050127     D TPV             S              1    DIM(50)
002400050127     D TPN             S              1    DIM(50)
002500050127     d tpw             s              1    Dim(50)
002600050218     d tpcli           s              1    Dim(50)
002700050127     d
002800050127     D SPV             S              1    DIM(50)
002900050127     D SPN             S              1    DIM(50)
003000050127     d spw             s              1    Dim(50)
003100050218     d spcli           s              1    Dim(50)
003200050127     d
003300050127     D PGV             S              3    DIM(50)
003400050127     D PGN             S              3    DIM(50)
003500050218     D PGcli           S              3    DIM(50)
003600050127     d
003700050127     D DIV             S              3    DIM(50)
003800050127     D DIN             S              3    DIM(50)
003900050218     D DIcli           S              3    DIM(50)
004000050127     d
004100050127     D FIV             S              1    DIM(50)
004200050127     D FIN             S              1    DIM(50)
004300050218     D FIcli           S              1    DIM(50)
004400050127
004500050127     d mdv             s              1    Dim(50)
004600050127     d mdn             s              1    Dim(50)
004700050218     d mdcli           s              1    Dim(50)
004800941104     D*
004900941107     D CTA             S              1    DIM(30)
005000941107     D DTA             S             10    DIM(30)
005100060511     D ERR             S             78    DIM(7) CTDATA PERRCD(1)
005200021115
005300021115     d Wctr            s              2  0
005400060516     d Errore          s              1
005500050124     d CNo             s                   like(C)
005600050128     d WcontrCTR       s                   like(Ilv59CTR)
005700050128     d Wblo            s                   like(Ilv59blo)
005800050128     d Wval            s                   like(Ilv59val)
005900050128     d Wta2            s                   like(Ilv59ta2)
006000050128     d Wta3            s                   like(Ilv59ta3)
006100050128     d Wta4            s                   like(Ilv59ta4)
006200050128     d Wta5            s                   like(Ilv59ta5)
006300941104      *
006400050127     D* DS PER FNLV59R - PER CARICAMENTO TARIFFE
006500050127     D fnlv59ds      E DS
006600021115
006700021115      * Ds per confronto dati tariffe FedEx
006800021115     d ddfe          e ds
006900021115     d Tibs02Ds      e ds
007000021115
007100990930     D DSTA01        E DS
007200941104     D* PASSO IN SCELTA COD TARIFFA DI TARIFFA/OFFERTA - TNTA36R -
007300941104     D PARAM4          DS
007400941104     D  PA4COD                 6     12  0
007500941104     D  PA4CTR                13     15
007600941104     D  PA4FLG                16     16
007700941104     D  PA4VPR                17     17
007800941104     D  PA4CCC                18     21  0
007900941104     D  PA4DCV                22     36
008000061220     d  pa4prg                37     39
008100061220     d  pa4dtr                40     47
008200941104     D KPJBA         E DS
008300941104     C*---------------------------------------------------------------*
008400941104     C     *ENTRY        PLIST
008500050128     C                   PARM                    fnlv59ds
008600941104     C*---------------------------------------------------------------*
008700050128     C* TIPO LANCIO, ilv59TLA "C" -           CHIUSO CON LR
008800050128     C* TIPO LANCIO, ilv59TLA "L" - ELABORO E CHIUDO CON LR
008900050128     C* TIPO LANCIO, ilv59TLA " " - ELABORO E CHIUDO IN RETRN
009000941104     C*
009100050128    1C     ilv59tla      IFNE      'C'
009200941107     C*
009300941107     C* PULIZIA CAMPI DI OUTPUT
009400050127     C                   CLEAR                   olv59mSG
009500050127     C                   CLEAR                   olv59ERR
009600050127     C                   CLEAR                   olv59DFS
009700050127     C                   CLEAR                   olv59PRG
009800050127     C                   CLEAR                   olv59DIV
009900050128     C                   CLEAR                   olv59CTR
010000050128     c
010100050128     C                   MOVEL     *BLANKS       olv59CT2
010200050128     C                   MOVEL     *BLANKS       olv59CT3
010300050128     C                   MOVEL     *BLANKS       olv59CT4
010400050128     C                   MOVEL     *BLANKS       olv59CT5
010500050128     C                   MOVEL     *BLANKS       olv59ES2
010600050128     C                   MOVEL     *BLANKS       olv59ES3
010700050128     C                   MOVEL     *BLANKS       olv59ES4
010800050128     C                   MOVEL     *BLANKS       olv59ES5
010900050128     c
011000050128     C                   MOVEL     *BLANKS       olv59CTT
011100050128     C                   MOVEL     *BLANKS       olv59pgT
011200050128     C                   MOVEL     *BLANKS       olv59AT1
011300050128     C                   MOVEL     *BLANKS       olv59AT2
011400050128     C                   MOVEL     *BLANKS       olv59AT3
011500050128     C                   MOVEL     *BLANKS       olv59AT4
011600050128     C                   MOVEL     *BLANKS       olv59AT5
011700050128     C                   MOVEL     *BLANKS       olv59ATt
011800941107     C*
011900941117     C* SOLO CONTROLLO FORMALE
012000050128    2C     ilv59CFO      IFEQ      'S'
012100050128     c                   eval      wcontrCTR=Ilv59CTR
012200941117     C                   EXSR      CTRFOR
012300941117     C*
012400941117   X2C                   ELSE
012500941117     C*
012600050127    3C     ilv59ksc      IFNE      WKSC
012700050127     C     ilv59fie      ORNE      WFIE
012800050127     C     ilv59tbo      ORNE      WTBO
012900050127     C     ilv59dsp      ORNE      WDSP
013000050128     C     ilv59blo      ORNE      Wblo
013100050128     C     ilv59VAL      ORNE      WVAL
013200060515     C**** ilv59ta2      ORNE      Wta2
013300050128     C     ilv59ta3      ORNE      Wta3
013400050128     C     ilv59ta4      ORNE      Wta4
013500050128     C     ilv59ta5      ORNE      Wta5
013600941104     C                   EXSR      CARCTR
013700941231     C                   ELSE
013800941231     C                   EXSR      VISTAR
013900941231    3C                   ENDIF
014000050127     C                   MOVEL     ilv59ksc      WKSC              7 0
014100050127     C                   MOVEL     ilv59dsp      WDSP              8 0
014200050127     C                   MOVEL     ilv59fie      WFIE              1
014300050127     C                   MOVEL     ilv59tbo      WTBO              1
014400050128     C                   MOVEL     ilv59blo      WBLO
014500050128     C                   MOVEL     ilv59val      WVAL
014600060515     C****               MOVEL     ilv59ta2      WTA2
014700050128     C                   MOVEL     ilv59ta3      WTA3
014800050128     C                   MOVEL     ilv59ta4      WTA4
014900050128     C                   MOVEL     ilv59ta5      WTA5
015000941104     C*
015100050127    3C     ilv59ric      IFEQ      'S'
015200941104     C                   EXSR      RICTAR
015300941215    3C                   ENDIF
015400941104     C*
015500050127    3C     ilv59con      IFEQ      'S'
015600050128     c                   if        olv59ctr=*blanks
015700050128     c                   eval      olv59ctr=ilv59ctr
015800050128     c                   endif
015900050128     c
016000941104     C                   EXSR      CTRCTR
016100941215    3C                   ENDIF
016200941215     C*
016300050128     C* DECODIFICA FISSA TARIFFA se non ancora messa
016400050127     C     olv59CTR      IFNE      *BLANKS
016500050202     C     olv59dfs      andEQ     *BLANKS
016600941215     C                   Z-ADD     1             C
016700050127     C                   MOVEL     olv59ctr      UNOCTR            1
016800941215     C     UNOCTR        LOOKUP    CTA(C)                                 30
016900050127     C   30              MOVEL     DTA(C)        olv59dfs
017000941215     C                   ENDIF
017100941215     C*
017200941117    2C                   ENDIF
017300941107     C*
017400941104    1C                   ENDIF
017500941104     C* CHIUSURA PGM
017600050128    1C     ilv59TLA      IFEQ      ' '
017700941104     C                   RETURN
017800941104     C                   ELSE
017900941104     C                   SETON                                        LR
018000941104    1C                   ENDIF
018100050218     c*
018200050218     C* CARICO LE TARIFFE DEL CLIENTE --------------------------------*
018300050218     C     CARCTR        BEGSR
018400050218     C*
018500050218     C* PULISCO SKIERE DELLE TARIFFE
018600050218     C                   CLEAR                   CTV
018700050218     C                   CLEAR                   CTN
018800050218     c                   Clear                   Ctw
018900050218     c                   Clear                   Ctcli
019000050218     C                   CLEAR                   TPN
019100050218     C                   CLEAR                   TPV
019200050218     c                   Clear                   Tpw
019300050218     c                   Clear                   Tpcli
019400050218     C                   CLEAR                   SPN
019500050218     C                   CLEAR                   SPV
019600050218     c                   Clear                   Spw
019700050218     c                   Clear                   Spcli
019800050218     C                   CLEAR                   PGV
019900050218     C                   CLEAR                   PGN
020000050218     C                   CLEAR                   PGcli
020100050218     C                   CLEAR                   DIV
020200050218     C                   CLEAR                   DIN
020300050218     C                   CLEAR                   DIcli
020400050218     C                   CLEAR                   FIV
020500050218     C                   CLEAR                   FIN
020600050218     C                   CLEAR                   FIcli
020700050218     c                   Clear                   mdv
020800050218     c                   Clear                   mdn
020900050218     c                   Clear                   mdcli
021000050218     c                   Clear                   bav
021100050218     c                   Clear                   ban
021200050218     c                   Clear                   bacli
021300050218     c                   Clear                   bavdop
021400050218     c                   Clear                   bandop
021500050218     c                   Clear                   vav
021600050218     c                   Clear                   vacli
021700050218     C*
021800050218     C                   MOVEL     *BLANKS       olv59TKS
021900050218     C*
022000050218     C* MITTENTI CODIFICATI --> CERCO LE TARIFFE
022100050218     C                   Z-ADD     0             B                 3 0
022200050218     C                   Z-ADD     0             C                 3 0
022300050218     C                   Z-ADD     0             D                 3 0
022400050218     C*
022500050218     C* SE 8888 O 9999 NON CARICO NULLA
022600050218     C                   MOVE      ilv59KSC      WCCM              4 0
022700050218     C*
022800050218    1C     WCCM          IFNE      8888
022900050218     C     WCCM          ANDNE     9999
023000050218     C*
023100050218     C     ilv59ksc      SETLL     TNTAM000
023200050218     C     ilv59ksc      READE     TNTAM000                               30
023300050218     C*
023400050218    2C     *IN30         DOWEQ     *OFF
023500050218     C* ESCLUDO SE ANNULLATO
023600050218    3C     TAMATB        IFNE      'A'
023700050218     c
023800050218     C                   MOVEL     TAMFLO        DSTA01
023900050218     C*
024000050218     C* CARICO SOLO TARIFFE ITALIA           --> FIE "I"
024100050218     C* CARICO SOLO TARIFFE Euroexpress      --> FIE "E"
024200050218     C* CARICO SOLO TARIFFE DPD              --> FIE "D"
024300050218     C* CARICO SOLO TARIFFE FEDEX            --> FIE "F"
024400050218     C* CARICO      TARIFFE DPD+FEDEX+EUROEX --> FIE "S"
024500050218     C* CARICO      TARIFFE DPD+FEDEX+ITALIA --> FIE "J"
024600050218     C* CARICO SOLO TARIFFE FEDEX DOCUMENTI  --> FIE "N"
024700050218     C* CARICO SOLO TARIFFE FEDEX MERCI      --> FIE "M"
024800060503    4C***  TAMFIE        IFEQ      ' '
024900060503     C***
025000060503     C***  TAMFIE        OREQ      ilv59fie
025100060503     C***  §TADPD        ANDEQ     ' '
025200060503     C***  §TAFED        ANDEQ     ' '
025300050218     C* SE TARIFFA DPD NON DEVO CARICARE  SE RICHIESTO I/E/F/M/N
025400060503     C***  §TADPD        OREQ      'S'
025500060503     C***  ilv59fie      ANDNE     'I'
025600060503     C***  ilv59fie      ANDNE     'E'
025700060503     C***  ilv59fie      ANDNE     'F'
025800060503     c***  ilv59fie      andne     'M'
025900060503     c***  ilv59fie      andne     'N'
026000050218     C*
026100050218     C* SE TARIFFA FEDEX NON DEVO CARICARE  SE RICHIESTO I/E/D
026200060503     C***  §TAFED        OREQ      'S'
026300060503     C***  ilv59fie      ANDNE     'I'
026400060503     C***  ilv59fie      ANDNE     'E'
026500060503     C***  ilv59fie      ANDNE     'D'
026600050218     C* LE ESTERE LE DEVO CARICARE SOLO PER S (OLTRE CHE DPD E FEDEX)
026700060503     C***  TAMFIE        OREQ      'E'
026800060503     C***  ilv59fie      ANDEQ     'S'
026900050218     C* LE ITALIA LE DEVO CARICARE SOLO PER J (OLTRE CHE DPD E FEDEX)
027000060503     C***  TAMFIE        OREQ      'I'
027100060503     C***  ilv59fie      ANDEQ     'J'
027200050218
027300050218     c                   Move      TamCtr        Wctr
027400050218      * Se Tamfie "M" Solo Tariffe merci FedEx
027500050218    5c                   If        ilv59Fie = 'M' and Wctr >= §DfeDalD
027600060511     c                   GoTo      Notartutto
027700050218    5c                   EndIf
027800050218      * Se Tamfie "N" Solo Tariffe documenti FedEx
027900050218    5c                   If        ilv59Fie = 'N' and Wctr < §DfeDalD
028000060511     c                   GoTo      Notartutto
028100050218    5c                   EndIf
028200050218     c* Elaboro solo tariffe decorrenti o scadute (escludo da decorrere
028300050218     c*  come se non ci fossero)
028400060503    5c***                if        ilv59dsp>=tamddt
028500060503     c* 03/5/06 ES --> preferisco caricare tutto. Se un tariffa ancora
028600060503     c*                da decorrere è già stata immessa, meglio
028700060503     c*                non farla usare in modo improprio, salvo
028800060503     c*                che non sia anche presente come decorrente:allor
028900060503     c*                la ignoro: meglio la decorrente
029000050218     c
029100050218     C                   MOVEL     TAMCTR        COMCT2            3
029200050218     C                   Z-ADD     1             D
029300050218     C     COMCT2        LOOKUP    CTcli(D)                               31
029400060503    6c                   if        (*in31 and ilv59dsp>=tamddt) or
029500050218     c                             (not *in31 and c < 50)
029600050218     C*
029700060511     C* DELLE TARIFFE già presenti, tengo sempre l'ulitma, escluse le da
029800060511     c*  decorrere
029900050218    7C     *IN31         IFEQ      *OFF
030000050218     C                   ADD       1             C
030100050218     C                   MOVEL     TAMCTR        CTcli(C)
030200050218     C                   Z-ADD     C             D
030300050218    7C                   ENDIF
030400050218     C*
030500050218     C* VEDO SE E' TARIFFA decorrente
030600060503     c                   select
030700060503    7C     ilv59dsp      wheneq    TAMDDT
030800050218     C     ilv59dsp      ANDLE     TAMDST
030900060503     c* Decorrente
031000050218     c                   movel     'D'           vacli(d)
031100060503    7C     ilv59dsp      whenlT    TAMDDT
031200060503     c* dA decorrere
031300060503     c                   movel     'A'           vacli(d)
031400060503     c
031500060503     c                   other
031600060503     c* Scaduta
031700050218     c                   movel     'S'           vacli(d)
031800060511    7c                   endsl
031900050218     C*
032000050218     C                   MOVEL     TAMCTR        CTcli(d)
032100050218     C                   MOVEL     TAMTSP        SPcli(d)
032200050218     C                   MOVE      TAMFTP        TPcli(d)
032300050218     C                   MOVE      TAMPRG        PGcli(d)
032400050218     C                   MOVE      §TADIV        DIcli(d)
032500050218     C                   MOVE      TAMFIE        FIcli(d)
032600050218     C                   MOVE      TAMbap        bacli(d)
032700050218     C* TARIFFA DPD
032800050218     C     §TADPD        IFEQ      'S'
032900050218     C                   MOVE      'D'           FIcli(d)
033000050218     C                   ENDIF
033100050218     C* TARIFFA FEDEX
033200050218    7C     §TAFED        IFEQ      'S'
033300050218     C                   MOVE      'F'           FIcli(d)
033400050218     c                   Clear                   mdcli(d)
033500050218     c                   If        ilv59Fie = 'M' and Wctr >= §DfeDalM
033600050218     c                                            and Wctr <= §DfeAlM
033700050218     c                   Eval      mdcli(d) = 'M'
033800050218     c                   EndIf
033900050218     c                   If        ilv59Fie = 'N' and Wctr >= §DfeDalD
034000050218     c                                            and Wctr <= §DfeAlD
034100050218     c                   Eval      mdcli(d) = 'N'
034200050218     c                   EndIf
034300050218    7c                   EndIf
034400050218    6C                   ENDIF
034500060511    5C*********          ENDIF
034600060511    4C*********          ENDIF
034700050218    3C                   ENDIF
034800050218
034900060511     c     Notartutto    Tag
035000050218
035100050218     C*
035200050218     C  N30ilv59ksc      READE     TNTAM000                               30
035300050218     C*
035400050218    2C                   ENDDO
035500050218     c
035600050218     c                   exsr      SELCTR
035700050218    1c                   endif
035800050218     c
035900050218     C                   ENDSR
036000941104     C*
036100050218     C* SELEZIONO le tariffe del cliente in base a quanto passato in DS ---*
036200050218     C     SELCTR        BEGSR
036300941104     C*
036400941104     C* PULISCO SKIERE DELLE TARIFFE
036500941104     C                   CLEAR                   CTV
036600941104     C                   CLEAR                   CTN
036700021119     c                   Clear                   Ctw
036800941104     C                   CLEAR                   TPN
036900941104     C                   CLEAR                   TPV
037000021119     c                   Clear                   Tpw
037100941104     C                   CLEAR                   SPN
037200941104     C                   CLEAR                   SPV
037300021119     c                   Clear                   Spw
037400941104     C                   CLEAR                   PGV
037500941104     C                   CLEAR                   PGN
037600990930     C                   CLEAR                   DIV
037700990930     C                   CLEAR                   DIN
037800000119     C                   CLEAR                   FIV
037900000119     C                   CLEAR                   FIN
038000021115     c                   Clear                   mdv
038100021115     c                   Clear                   mdn
038200050127     c                   Clear                   bav
038300050127     c                   Clear                   ban
038400050128     c                   Clear                   bavdop
038500050128     c                   Clear                   bandop
038600050127     c                   Clear                   vav
038700941104     C*
038800050127     C                   MOVEL     *BLANKS       olv59TKS
038900941104     C*
039000941104     C* MITTENTI CODIFICATI --> CERCO LE TARIFFE
039100941104     C                   Z-ADD     0             B                 3 0
039200941104     C                   Z-ADD     0             C                 3 0
039300941104     C                   Z-ADD     0             D                 3 0
039400941110     C*
039500941110     C* SE 8888 O 9999 NON CARICO NULLA
039600050128     C                   MOVE      ilv59KSC      WCCM              4 0
039700941104     C*
039800050128    1C     WCCM          IFNE      8888
039900941110     C     WCCM          ANDNE     9999
040000941110     C*
040100050127     C     ilv59ksc      SETLL     TNTAM000
040200050127     C     ilv59ksc      READE     TNTAM000                               30
040300941104     C*
040400941104    2C     *IN30         DOWEQ     *OFF
040500941104     C* ESCLUDO SE ANNULLATO
040600941104    3C     TAMATB        IFNE      'A'
040700050127     C* ilv59BLO:  B=solo bloccate " "=solo non bloccate  S=tutte
040800050128    4c                   if         ilv59BLO='S' or
040900050127     c                             (ilv59BLO='B' and tambap='B') or
041000050127     c                             (ilv59BLO=' ' and tambap<>'B')
041100050127     c
041200000119     C                   MOVEL     TAMFLO        DSTA01
041300000126     C*
041400020206     C* CARICO SOLO TARIFFE ITALIA           --> FIE "I"
041500050127     C* CARICO SOLO TARIFFE Euroexpress      --> FIE "E"
041600020206     C* CARICO SOLO TARIFFE DPD              --> FIE "D"
041700020206     C* CARICO SOLO TARIFFE FEDEX            --> FIE "F"
041800021115     C* CARICO      TARIFFE DPD+FEDEX+EUROEX --> FIE "S"
041900020206     C* CARICO      TARIFFE DPD+FEDEX+ITALIA --> FIE "J"
042000021115     C* CARICO SOLO TARIFFE FEDEX DOCUMENTI  --> FIE "N"
042100021115     C* CARICO SOLO TARIFFE FEDEX MERCI      --> FIE "M"
042200050128    5C     TAMFIE        IFEQ      ' '
042300000119     C*
042400050127     C     TAMFIE        OREQ      ilv59fie
042500000119     C     §TADPD        ANDEQ     ' '
042600020206     C     §TAFED        ANDEQ     ' '
042700021115     C* SE TARIFFA DPD NON DEVO CARICARE  SE RICHIESTO I/E/F/M/N
042800000119     C     §TADPD        OREQ      'S'
042900050127     C     ilv59fie      ANDNE     'I'
043000050127     C     ilv59fie      ANDNE     'E'
043100050127     C     ilv59fie      ANDNE     'F'
043200050127     c     ilv59fie      andne     'M'
043300050127     c     ilv59fie      andne     'N'
043400000124     C*
043500020206     C* SE TARIFFA FEDEX NON DEVO CARICARE  SE RICHIESTO I/E/D
043600020206     C     §TAFED        OREQ      'S'
043700050127     C     ilv59fie      ANDNE     'I'
043800050127     C     ilv59fie      ANDNE     'E'
043900050127     C     ilv59fie      ANDNE     'D'
044000020206     C* LE ESTERE LE DEVO CARICARE SOLO PER S (OLTRE CHE DPD E FEDEX)
044100000124     C     TAMFIE        OREQ      'E'
044200050127     C     ilv59fie      ANDEQ     'S'
044300020206     C* LE ITALIA LE DEVO CARICARE SOLO PER J (OLTRE CHE DPD E FEDEX)
044400000126     C     TAMFIE        OREQ      'I'
044500050127     C     ilv59fie      ANDEQ     'J'
044600021115
044700021118     c                   Move      TamCtr        Wctr
044800021115      * Se Tamfie "M" Solo Tariffe merci FedEx
044900050128    6c                   If        ilv59Fie = 'M' and Wctr >= §DfeDalD
045000021121     c                   GoTo      Notar
045100050128    6c                   EndIf
045200021121      * Se Tamfie "N" Solo Tariffe documenti FedEx
045300050128    6c                   If        ilv59Fie = 'N' and Wctr < §DfeDalD
045400021121     c                   GoTo      Notar
045500050128    6c                   EndIf
045600941107     C*
045700050127     C* ilv59TBO P=carico tariffe P/B/" "
045800050127     a*          A=carico tariffe A/B/" "   " "=tutte
045900050128    6C     ilv59tbo      IFEQ      ' '
046000981007     C     TAMBAP        OREQ      ' '
046100050127     C     TAMBAP        OREQ      'B'
046200050127     C     TAMBAP        OREQ      ilv59tbo
046300941104     C*
046400050127     C* VEDO SE E' TARIFFA decorrente
046500050128    7C     ilv59dsp      IFGE      TAMDDT
046600050127     C     ilv59dsp      ANDLE     TAMDST
046700050127     c* solo se richieste (ilv59VAL=D/" ")
046800050128    8c                   if        ilv59VAL=' ' or ilv59val='D'
046900941104     C*
047000941104     C                   ADD       1             B
047100941104     C                   MOVEL     TAMCTR        CTV(B)
047200941104     C                   MOVEL     TAMTSP        SPV(B)
047300941104     C                   MOVE      TAMFTP        TPV(B)
047400941104     C                   MOVE      TAMPRG        PGV(B)
047500990930     C                   MOVE      §TADIV        DIV(B)
047600000119     C                   MOVE      TAMFIE        FIV(B)
047700050124     C                   MOVE      TAMbap        baV(B)
047800050127     c* Carico il campo bap anche mettendo al posto di " " --> "E"
047900050128    9c                   if        tambap=' ' or tambap='B'
048000050127     c                   movel     'E'           bavdop(b)
048100050127     c                   else
048200050127     C                   MOVE      TAMbap        baVdop(B)
048300050128    9c                   endif
048400050127     C                   MOVE      'D'           vaV(B)
048500000119     C* TARIFFA DPD
048600000119     C     §TADPD        IFEQ      'S'
048700000126     C                   MOVE      'D'           FIV(B)
048800000119     C                   ENDIF
048900020206     C* TARIFFA FEDEX
049000050128    9C     §TAFED        IFEQ      'S'
049100020206     C                   MOVE      'F'           FIV(B)
049200021115     c                   Clear                   mdv(b)
049300050128     c                   If        ilv59Fie = 'M' and Wctr >= §DfeDalM
049400050128     c                                            and Wctr <= §DfeAlM
049500021115     c                   Eval      mdv(b) = 'M'
049600021115     c                   EndIf
049700050128     c                   If        ilv59Fie = 'N' and Wctr >= §DfeDalD
049800050128     c                                            and Wctr <= §DfeAlD
049900021115     c                   Eval      mdv(b) = 'N'
050000021115     c                   EndIf
050100050128    9c                   EndIf
050200050127     c
050300050127     C     B             COMP      50                                 30
050400050201   x8c                   else
050500050201     c* Se tariffa decorrente e c'e' anche come scaduta la tolgo
050600050201     C*
050700050201     C                   MOVEL     TAMCTR        COMCT2            3
050800050201     C                   Z-ADD     1             D
050900050201     C     COMCT2        LOOKUP    CTN(D)                                 31
051000050201    9c                   if        *in31
051100050201     c                   clear                   ctn(d)
051200050201     c                   clear                   spn(d)
051300050201     c                   clear                   tpn(d)
051400050201     c                   clear                   pgn(d)
051500050201     c                   clear                   din(d)
051600050201     c                   clear                   fin(d)
051700050201     c                   clear                   ban(d)
051800050201     c                   clear                   bandop(d)
051900050201     c                   clear                   mdn(d)
052000050201     c* Reimposto il contatore a -1
052100050201     c                   sub       1             C
052200050201     c
052300050201    9C                   ENDIF
052400050201    8C                   ENDIF
052500941104     C*
052600050127     C* TARIFFE non decorrente
052700050128   X7C                   ELSE
052800050127     c*         tengo solo le scadute, se richieste (ilv59VAL=S/" ")
052900050128    8c                   if        ilv59dsp>tamdst  and
053000050127     c                             (ilv59VAL=' ' or ilv59VAL='S')
053100941104     C*
053200941104     C                   MOVEL     TAMCTR        COMCT2            3
053300941104     C                   Z-ADD     1             D
053400941104     C     COMCT2        LOOKUP    CTN(D)                                 31
053500050128    9c                   if        *in31   or
053600050127     c                             (not *in31 and c < 50)
053700941104     C*
053800941104     C* DELLE TARIFFE NON VALIDE, TENGO SEMPRE L'ULTIMA
053900050128   10C     *IN31         IFEQ      *OFF
054000941104     C                   ADD       1             C
054100941104     C                   MOVEL     TAMCTR        CTN(C)
054200941104     C                   Z-ADD     C             D
054300050128   10C                   ENDIF
054400941104     C                   MOVEL     TAMTSP        SPN(D)
054500941104     C                   MOVE      TAMFTP        TPN(D)
054600941104     C                   MOVE      TAMPRG        PGN(D)
054700990930     C                   MOVE      §TADIV        DIN(D)
054800000119     C                   MOVE      TAMFIE        FIN(D)
054900050124     C                   MOVE      TAMbap        ban(D)
055000050127     c* Carico il campo bap anche mettendo al posto di " " --> "E"
055100050127     c                   if        tambap=' '   or  tambap='B'
055200050127     c                   movel     'E'           bandop(D)
055300050127     c                   else
055400050127     C                   MOVE      TAMbap        bandop(D)
055500050127     c                   endif
055600000126     C* TARIFFA DPD
055700000126     C     §TADPD        IFEQ      'S'
055800000201     C                   MOVE      'D'           FIN(D)
055900000126     C                   ENDIF
056000020206     C* TARIFFA FEDEX
056100050128   10C     §TAFED        IFEQ      'S'
056200020206     C                   MOVE      'F'           FIN(D)
056300021115     c                   Clear                   mdn(d)
056400050128     c                   If        ilv59Fie = 'M' and Wctr >= §DfeDalM
056500050128     c                                            and Wctr <= §DfeAlM
056600021115     c                   Eval      mdn(d) = 'M'
056700021115     c                   EndIf
056800050128     c                   If        ilv59Fie = 'N' and Wctr >= §DfeDalD
056900050128     c                                            and Wctr <= §DfeAlD
057000021115     c                   Eval      mdn(d) = 'N'
057100021115     c                   EndIf
057200050128   10C                   ENDIF
057300050128    9C                   ENDIF
057400941104     C*
057500050128    8C                   ENDIF
057600050128    7C                   ENDIF
057700050128    6C                   ENDIF
057800050128    5C                   ENDIF
057900050128    4C                   ENDIF
058000050128    3C                   ENDIF
058100021115
058200021121     c     Notar         Tag
058300021121
058400941104     C*
058500050127     C  N30ilv59ksc      READE     TNTAM000                               30
058600981006     C*
058700941104    2C                   ENDDO
058800941104     C*
058900941104     C                   ADD       1             B
059000941104     C*
059100941104     C* CARICO TUTTI I TIPI TARIFFA TROVATI IN UNICA SCHIERA
059200050128     C* SE NON NE HO GIA' TROVATI 50 VALIDI
059300050128    2C     B             IFLE      50
059400941104     C*
059500941104     C*
059600941104     C* SE ESISTE ALMENO UNA TARIFFA VALIDA, NON CONSIDERO LA 'P'
059700941104     C*  DI PREFERENZIALE PER LE TARIFFE SCADUTE
059800941104    3C     B             IFGT      1
059900941104     C                   CLEAR                   TPN
060000941104    3C                   ENDIF
060100941104     C*
060200941104     C                   Z-ADD     1             C
060300941104     C*
060400941104    3C     CTN(C)        DOWNE     *BLANKS
060500050128     C     B             ANDLE     50
060600941104     C*
060700000119     C     CTN(C)        LOOKUP    CTV                                    31
060800941104    4C     *IN31         IFEQ      *OFF
060900941104     C                   MOVEL     CTN(C)        CTV(B)
061000941104     C                   MOVEL     TPN(C)        TPV(B)
061100941104     C                   MOVEL     SPN(C)        SPV(B)
061200941104     C                   MOVEL     PGN(C)        PGV(B)
061300990930     C                   MOVEL     DIN(C)        DIV(B)
061400000119     C                   MOVEL     FIN(C)        FIV(B)
061500021115     c                   Movel     mdn(c)        mdv(b)
061600050124     c                   Movel     ban(c)        bav(b)
061700050127     c                   Movel     bandop(c)     bavdop(b)
061800050127     c* S-scaduta
061900050127     c                   movel     'S'           vav(b)
062000941104     C                   ADD       1             B
062100941104    4C                   ENDIF
062200941104     C*
062300941104     C                   ADD       1             C
062400050128    3C                   ENDdo
062500050128    2C                   ENDif
062600021115     C*
062700941107     C                   EXSR      VISTAR
062800050128    1C                   ENDif
062900941104     C*
063000941104     C                   ENDSR
063100941104     C*
063200941104     C* RICERCA E CARICAMENTO A VIDEO TARIFFE ------------------------*
063300941104     C     RICTAR        BEGSR
063400941104     C                   SETOFF                                       30
063500950125     C                   CLEAR                   W001A
063600050124     c                   clear                   CNo
063700941104     C*
063800941104     C* SE NON CI SONO TARIFFE IMPOSTO LA TARIFFA 000
063900050127    1C     CTV(1)        IFEQ      *BLANKS
064000050127     C                   CLEAR                   olv59tks
064100941215     C*
064200050127     C     ilv59ctr      IFEQ      *BLANKS
064300050127     C                   MOVEL     *ZEROS        olv59CTR
064400941215     C                   ENDIF
064500941215     C*
064600050127   X1C                   ELSE
064700941107     C*
064800941107     C* CODICE TARIFFA IMPOSTATO: PRENDO IL RELATIVO PROGRESSIVO
064900050127    2C     ilv59CTR      IFNE      *BLANKS
065000050127     c                   eval      olv59ctr=ilv59ctr
065100050127    2C                   ENDIF
065200941107     C**
065300050127     c**
065400050127    2C     ilv59CTR      IFEQ      *BLANKS
065500021115
065600050127      * Carico le sk di work per la scelta
065700021115     c                   ExSr      CarSk
065800941104     C*
065900941104     C* SE IMPOSTATO IL TIPO SERVIZIO --> CERCO QUELLO
066000050128    3C     ilv59TSP      IFNE      *BLANKS
066100941104     C* CERCO SE C'E' LA PREFERENZIALE ALTRIMENTI LA 1 CHE TROVO
066200941104     C                   Z-ADD     1             C
066300941104     C                   SETON                                        31
066400941104     C*
066500941104    4C     *IN31         DOWEQ     *ON
066600050128     C     ilv59tSP      LOOKUP    SPw(C)                                 31
066700941104     C* SE TROVATA ED E' PREFERENZIALE ESCO DAL CICLO
066800070109    5c****               if        *in31 and  tpw(C)='P'
066900070109    5c   31              if        tpw(C)='P'
067000941104     C                   SETON                                        30
067100941104     C                   SETOFF                                       31
067200941104     C                   MOVEL     '1'           W001A             1
067300050127   X5C                   ELSE
067400021115     C* SE TROVATA MA NON PREFERENZIALE ACCENDO COMUNQUE L'INDICATORE
067500941104     C*  DI TROVATO MA FACCIO UN ALTRO GIRO
067600941104     C                   SETON                                        30
067700050124     c                   if        CNo=0
067800050124     c                   z-add     C             CNo
067900050127     c                   endif
068000050124     c
068100941104     C                   ADD       1             C
068200050124    5C                   ENDIF
068300941104    4C                   ENDDO
068400941104     C*
068500941104     C* SE HO TROVATO LA TARIFFA MA NON LA PREFERENZIALE, LA RICERCO X
068600941104     C*  PRENDERE LA POSIZIONE
068700941104    4C   30W001A         IFEQ      ' '
068800050124     c                   z-add     CNo           C
068900941104    4C                   ENDIF
069000941104     C*
069100941104    3C                   ENDIF
069200941104     C*
069300941104     C* SE NON TROVATA O NON IMPOSTATO IL TIPO SERVIZIO -->
069400941104     C*  CERCO LA PRIMA PREFERENZIALE
069500050124    3C     *IN30         IFEQ      *OFF
069600050127     C                   Z-ADD     1             C
069700050127     C     'P'           LOOKUP    TPw(C)                                 30
069800050127    3C                   ENDIF
069900941104     C*
070000941104     C* SE NON ESISTE LA PREFERENZIALE METTO LA PRIMA VALIDA TROVATA
070100941104     C  N30              Z-ADD     1             C
070200050127     c
070300050127     c* IMPOSTO CAMPO CODICE TARIFFA
070400050127     C                   MOVEL     CTw(C)        Olv59CTR
070500050127    2c                   endif
070600941104     C*
070700050127      * Cerco la posizione nella sk valida
070800050127     c                   Z-Add     1             c
070900050127     c     olv59CTR      Lookup    Ctv(c)                                 30
071000050127     c
071100050128    2c                   IF        *IN30
071200050127     C                   MOVEL     PGV(C)        OLV59PRG
071300050127     C                   MOVEL     DIV(C)        OLV59DIV
071400050127     C     OLV59DIV      IFEQ      '   '
071500050127     C                   MOVEL     'ITL'         OLV59DIV
071600991026     C                   ENDIF
071700050127
071800050127   x2C                   ELSE
071900050127     C* ERRORE --> COD.TARIFFA NON TROVATO (se passato in input può
072000050127     c*            accadere)
072100050127     C                   MOVEL     '3'           olv59ERR
072200050127     C                   MOVEL     ERR(3)        olv59MSG
072300050127     C                   Z-ADD     10            C
072400050127    2C                   ENDIF
072500941107     C*
072600021115     C                   EXSR      VISTAR
072700941104     C*
072800941104     C* RIMPIAZZO CON COD TARIFFA 5 SOLO SE ESISTE
072900050127    2C     CTv(5)        IFNE      *BLANKS
073000941104     C* SE NELLA 1°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
073100050127    3C     C             IFEQ      1
073200050128     C                   MOVEL     CTv(5)        olv59CT2
073300050128     C                   MOVEL     SPv(5)        olv59ES2
073400050128     C                   MOVE      FIv(5)        olv59ES2
073500050127    3C                   END
073600941104     C* SE NELLA 2°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
073700050127    3C     C             IFEQ      2
073800050128     C                   MOVEL     CTv(5)        olv59CT3
073900050128     C                   MOVEL     SPv(5)        olv59ES3
074000050128     C                   MOVE      FIv(5)        olv59ES3
074100050127    3C                   END
074200941104     C* SE NELLA 3°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
074300050127    3C     C             IFEQ      3
074400050128     C                   MOVEL     CTv(5)        olv59CT4
074500050128     C                   MOVEL     SPv(5)        olv59ES4
074600050128     C                   MOVE      FIv(5)        olv59ES4
074700050127    3C                   END
074800941104     C* SE NELLA 4°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
074900050127    3C     C             IFEQ      4
075000050128     C                   MOVEL     CTv(5)        olv59CT5
075100050128     C                   MOVEL     SPv(5)        olv59ES5
075200050128     C                   MOVE      FIv(5)        olv59ES5
075300050127    3C                   END
075400941104     C*
075500050127    2C                   END
075600050127     C
075700050127    1C                   ENDIF
075800941104     C*
075900941104     C                   ENDSR
076000021115      *--------------------------------------------------------------*
076100050127      * Carico le sk di work per la proposta a video
076200021115      *--------------------------------------------------------------*
076300021115     c     Carsk         BegSr
076400021115
076500050127     c* Se richieste tariffe FEDEX e presenti sia M che N --> propon
076600050127     c*  go "N"-documenti
076700050127     c                   setoff                                       3536
076800050127     c                   setoff                                       3738
076900050127     c                   If        ilv59Fie = 'F'
077000021115     c     'M'           Lookup    mdv                                    35
077100021115     c     'N'           Lookup    mdv                                    36
077200021115     c                   EndIf
077300050127     c
077400050127     c* Se presenti sia tariffe "P" che "A" --> propongo la P o la A
077500050127     c*    in base alla richiesta (ilv59PRP)
077600050128    1c                   if        ilv59tbo=' '  and ilv59PRP<>' '
077700050127     c     'E'           Lookup    bavdop                                 37
077800050128    2c                   if        *in37
077900050127     c                   seton                                        38
078000050128   x2c                   else
078100050127     c     'P'           Lookup    bavdop                                 37
078200050127     c     'A'           Lookup    bavdop                                 38
078300050128    2c                   endif
078400050128    1c                   endif
078500021115
078600021115     c                   Clear                   d
078700021115
078800050128    1c                   Do        50            b
078900021115     c                   If        Ctv(b) = *Blanks
079000021115     c                   Leave
079100021115     c                   EndIf
079200050127     c* per fedex tengo la documenti se presenti entrambe
079300050127     c                   If        ilv59Fie = 'F' and *In35 and *In36
079400021115     c                             and Mdv(b) <> 'N'
079500021115     c                   Iter
079600021115     c                   EndIf
079700050127     c
079800050127     c* Per la proposta impostata e presenti entrambe tariffe (p e A)
079900050127     c*  carico solo la proposta scelta
080000050127     c                   if        ilv59tbo=' ' and ilv59prp<>' ' and
080100050127     c                             *in37 and *in38
080200050127     c                   if        ilv59prp='P' and bavdop(b)='A'
080300050127     c                   iter
080400050127     c                   endif
080500050127     c                   if        ilv59prp='A' and bavdop(b)='P'
080600050127     c                   iter
080700050127     c                   endif
080800050127     c
080900050127     c                   endif
081000021115
081100021115     c                   Add       1             d
081200021115
081300021115     c                   Eval      Ctw(d) = Ctv(b)
081400021115     c                   Eval      Tpw(d) = Tpv(b)
081500021115     c                   Eval      Spw(d) = Spv(b)
081600050128    1c                   EndDo
081700021115
081800021115     c                   EndSr
081900941104     C*
082000941104     C* CONTROLLO CODICE TARIFFA ------------------------------------*
082100941104     C     CTRCTR        BEGSR
082200941215     C* SE ESISTE ALMENO UNA TARIFFA PER IL CLIENTE ANCHE SE SCADUTA
082300941215     C*  LO SEGNALO
082400941215     C     CTV(1)        IFNE      *BLANKS
082500050127     C                   MOVEL     'S'           olv59TKS
082600941215     C                   ENDIF
082700941215     C*
082800050127     C                   MOVE      ilv59ksc      WCCM              4 0
082900941104     C* RICERCA
083000050128     C     '?'           SCAN      olv59ctr                               30
083100941104     C*
083200941104    1C     *IN30         IFEQ      *ON
083300941104     C*
083400941104     C* SOLO SE CODIFICATO DELL'AREA POSSO FARE LA RICERCA
083500941104    2C     WCCM          IFEQ      8888
083600941104     C     WCCM          OREQ      9999
083700050127     C                   MOVEL     '1'           olv59ERR
083800050127     C                   MOVEL     ERR(1)        olv59MSG
083900941104     C                   GOTO      ENDCTR
084000941104    2C                   ENDIF
084100941104     C*
084200941104     C* RICERCA
084300941104     C                   CLEAR                   PARAM4
084400050127     C                   MOVEL     ilv59ksc      PA4COD
084500050127     C                   Z-ADD     ilv59kci      PA4CCC
084600061220     c                   movel     '2'           pa4vpr
084700061220     c                   move      ilv59dsp      pa4dtr
084800941104     C                   MOVEL     PARAM4        KPJBU
084900941104     C                   CALL      'TNTA36R'
085000941104     C                   PARM                    KPJBA
085100941104     C                   MOVEL     KPJBU         PARAM4
085200941104     C*
085300941104    2C     PA4CTR        IFNE      *BLANKS
085400941104     C* SE TROVATO METTO QUELLO SCELTO
085500050128     C                   MOVE      PA4CTR        olv59CTR
085600941104   X2C                   ELSE
085700050128     c* Se non scelto lo abblanco
085800050128     C                   MOVEL     *blanks       olv59CTR
085900941104    2C                   END
086000050127     C                   MOVEL     'R'           olv59ERR
086100941104     C                   GOTO      ENDCTR
086200941104     C*
086300941104   X1C                   ELSE
086400941104     C*
086500941116     C* CONTROLLO FORMALE
086600050128     c                   eval      wcontrCTR=olv59CTR
086700941116     C                   EXSR      CTRFOR
086800941116     C*
086900050127     C     olv59ERR      IFNE      ' '
087000941104     C                   GOTO      ENDCTR
087100941104    2C                   ENDIF
087200941104     C*
087300941104     C* CONTROLLO ESISTENZA TARIFFA
087400941104    2C     CTV(1)        IFNE      *BLANKS
087500941104     C                   Z-ADD     1             B
087600050128     C     olv59CTR      LOOKUP    CTV(B)                                 30
087700050128     C   30              MOVEL     PGV(B)        olv59PRG
087800050128     C   30              MOVEL     DIV(B)        olv59DIV
087900060515    3C   30olv59DIV      IFEQ      '   '
088000050128     C                   MOVEL     'ITL'         olv59DIV
088100060515    3C                   ENDIF
088200060515     c
088300060515     c* Se non trovata tariffa e accetto 1 cod tariffa se bloccato, verifico
088400060515     c*  il network
088500060515    3c                   if        not *in30 and ilv59ta2='B'
088600060515     C                   Z-ADD     1             B
088700060515     C     olv59CTR      LOOKUP    CTcli(b)                               30
088800060515     c*
088900060515    4c                   if        *in30
089000060515    5c                   if        ficli(b)=ilv59fie              or
089100060515     c                             ilv59fie='M' and mdcli(b)='M'  or
089200060515     c                             ilv59fie='N' and mdcli(b)='N'  or
089300060515     c                             ilv59fie='S' and ficli(b)<>'I' or
089400060515     c                             ilv59fie='J' and ficli(b)<>'E'
089500060515     c* Network corrisponde: controllo se bloccata
089600060515    6c                   if        bacli(b)='B' and vacli(b)<>'A'
089700060516     c* se ok imposto i campi del progressino e della divisa
089800060516     C                   MOVEL     PGcli(b)      olv59PRG
089900060516     C                   MOVEL     DIcli(b)      olv59DIV
090000060516    3C     olv59DIV      IFEQ      '   '
090100060516     C                   MOVEL     'ITL'         olv59DIV
090200060516    3C                   ENDIF
090300060515     c                   else
090400060515     c                   setoff                                       30
090500060515    6c                   endif
090600060515   x5c                   else
090700060515     c                   setoff                                       30
090800060515    5c                   endif
090900060515    4c                   endif
091000060515    3c                   endif
091100060515     c
091200060515     c
091300941104     C* INESISTENTE: ERRORE
091400060516    3C**N30CTV(1)        IFNE      *BLANKS
091500060516    c
091600060516     c                   if        not *in30
091700050128     C                   MOVEL     '3'           olv59ERR
091800050128     C                   MOVEL     ERR(3)        olv59MSG
091900941104     C                   GOTO      ENDCTR
092000941104    3C                   END
092100060511     c*
092200060511   x2c                   else
092300060516     c*
092400060511     c* Se non esistono tariffe valide, il cod tariffa impostato non
092500060511     c*  può nemmeno essere uno presente ma non valido con questo
092600060511     c*  richiamo (ad esempio se carico tariffe italia, non può
092700060511     c*  essere un cod tariffa per l'estero)
092800060516     c                   eval      Errore=' '
092900060516     c                   z-add     1             b
093000060516     C     olv59CTR      LOOKUP    CTcli(b)                               30
093100060516     c
093200060516    3c                   if        *in30
093300060516    4c                   if        ficli(b)=ilv59fie              or
093400060516     c                             ilv59fie='M' and mdcli(b)='M'  or
093500060516     c                             ilv59fie='N' and mdcli(b)='N'  or
093600060516     c                             ilv59fie='S' and ficli(b)<>'I' or
093700060516     c                             ilv59fie='J' and ficli(b)<>'E'
093800060516     c* Network corrisponde: controllo se bloccata
093900060516    5c                   if        bacli(b)='B' and ilv59ta2='B'
094000060516     c                             and vacli(b)<>'A'
094100060516     c* se ok imposto i campi del progressino e della divisa
094200060516     C                   MOVEL     PGcli(b)      olv59PRG
094300060516     C                   MOVEL     DIcli(b)      olv59DIV
094400060516    3C     olv59DIV      IFEQ      '   '
094500060516     C                   MOVEL     'ITL'         olv59DIV
094600060516    3C                   ENDIF
094700060516     c                   else
094800060516     c                   eval      errore='S'
094900060516    5c                   endif
095000060516   x4c                   else
095100060516     c                   eval      errore='S'
095200060516    4c                   endif
095300060516    3c                   endif
095400060511     c
095500060516    3c                   if        Errore='S'
095600060511     C                   MOVEL     '5'           olv59ERR
095700060516    4c                   select
095800060511     c                   when      ilv59tbo='P'
095900060511     C                   MOVEL     ERR(5)        olv59MSG
096000060511     c                   when      ilv59tbo='A'
096100060511     C                   MOVEL     ERR(6)        olv59MSG
096200060511     c                   other
096300060511     C                   MOVEL     ERR(7)        olv59MSG
096400060516    4c                   endsl
096500060511     c
096600060511     C                   GOTO      ENDCTR
096700060516    3c                   endif
096800060511     c
096900060516    2C                   END
097000941104    1C                   END
097100941104     C     ENDCTR        ENDSR
097200941107     C*
097300941107     C* CONTROLLO FORMALE CODICE TARIFFA -----------------------------*
097400941107     C     CTRFOR        BEGSR
097500941107     C*
097600050128     C                   MOVEL     wcontrCTR     UNOCTR            1
097700941107     C                   Z-ADD     1             C
097800941107     C     UNOCTR        LOOKUP    CTA(C)                                 30
097900941107     C*
098000050128    1c     *IN30         IFEQ      *OFF
098100941118     C     UNOCTR        OREQ      ' '
098200050127     C                   MOVEL     ERR(2)        olv59MSG
098300050127     C                   MOVEL     '2'           olv59ERR
098400050128    1C                   ENDIF
098500941107     C*
098600050127    1C     olv59ERR      IFEQ      *BLANKS
098700941107     C*
098800050127     C                   MOVEL     DTA(C)        olv59DFS
098900941107     C* CONTROLLO CHE LA 2 E 3 CIFRA SIANO UN NUERO
099000050128     C                   MOVE      wcontrCTR     W002A             2
099100941107     C                   MOVEL     W002A         W001A
099200941107     C*
099300941107    2C                   DO        2
099400941107    3C     W001A         IFNE      '0'
099500941107     C     W001A         ANDNE     '1'
099600941107     C     W001A         ANDNE     '2'
099700941107     C     W001A         ANDNE     '3'
099800941107     C     W001A         ANDNE     '4'
099900941107     C     W001A         ANDNE     '5'
100000941107     C     W001A         ANDNE     '6'
100100941107     C     W001A         ANDNE     '7'
100200941107     C     W001A         ANDNE     '8'
100300941107     C     W001A         ANDNE     '9'
100400050127     C                   MOVEL     ERR(4)        olv59MSG
100500050127     C                   MOVEL     '4'           olv59ERR
100600941107    3C                   ENDIF
100700941107     C*
100800941107     C                   MOVE      W002A         W001A
100900941107    2C                   ENDDO
101000941107    1C                   ENDIF
101100941107     C*
101200941107     C                   ENDSR
101300941104     C*
101400981006     C* SBR INIZIO ---------------------------------------------------*
101500941117     C     *INZSR        BEGSR
101600981006      *
101700981006      * ACCESSO   TABEL
101800981006      *
101900941104     C     KTAB3         KLIST
102000941104     C                   KFLD                    CODUT
102100941104     C                   KFLD                    COD               2
102200981006      *
102300941110     C                   Z-ADD     1             CODUT             1 0
102400981006     C***
102500941104     C* CARICO TABELLA TR : TIPI TARIFFA
102600941104     C***
102700981006     C                   OPEN      TABEL00F
102800981006      *
102900941104     C                   MOVEL     'TR'          COD
103000941104     C                   Z-ADD     1             C
103100941104     C     KTAB3         SETLL     TABEL
103200941104     C     KTAB3         READE     TABEL                                  30
103300941104     C*
103400941104     C     *IN30         DOWEQ     '0'
103500941104     C     TBLFLG        IFEQ      ' '
103600941104     C                   MOVEL     TBLKEY        CTA(C)
103700941104     C                   MOVEL     TBLUNI        DTA(C)
103800941104     C                   ADD       1             C
103900941104     C                   ENDIF
104000941104     C*
104100941104     C     KTAB3         READE     TABEL                                  30
104200941104     C                   ENDDO
104300941104     C*
104400941104     C                   CLOSE     TABEL00F
104500941117     C*
104600941117     C                   CLEAR                   WKSC
104700941117     C                   CLEAR                   WFIE
104800941117     C                   CLEAR                   WTBO
104900050127     c*
105000050127      * Carico tabella DFE
105100050127     c                   Clear                   DDfe
105200050127     c                   Clear                   Tibs02Ds
105300050127     c                   Eval      T02Mod = 'C'
105400050127     c                   Clear                   T02Sif
105500050127     c                   Eval      T02Cod = 'DFE'
105600050127     c                   Movel(p)  'FED'         T02Ke1
105700050127     c                   Call      'TIBS02R'
105800050127     c                   Parm                    Kpjba
105900050127     c                   Parm                    Tibs02Ds
106000050127     c                   If        T02err = *Blanks
106100050127     c                   Movel     T02Uni        DDfe
106200050127     c                   EndIf
106300941104     C*
106400941104     C                   ENDSR
106500941107     C*
106600941107     C* RIEMPO I CAMPI VIDEO-----------------------------------------*
106700941107     C     VISTAR        BEGSR
106800941107     C     CTV(1)        IFNE      *BLANKS
106900050127     C                   MOVEL     CTV(1)        OLV59CT2
107000050127     C                   MOVEL     SPV(1)        OLV59ES2
107100050127     C                   MOVE      Fiv(1)        OLV59ES2
107200941107     C* SE ESISTE ALMENO UNA TARIFFA PER IL CLIENTE ANCHE SE SCADUTA
107300941107     C*  LO SEGNALO
107400050127     C                   MOVEL     'S'           OLV59TKS
107500941107     C                   ENDIF
107600941107     C*
107700941107     C     CTV(2)        IFNE      *BLANKS
107800050127     C                   MOVEL     CTV(2)        OLV59CT3
107900050127     C                   MOVEL     SPV(2)        OLV59ES3
108000050127     C                   MOVE      Fiv(2)        OLV59ES3
108100941107     C                   ENDIF
108200941107     C     CTV(3)        IFNE      *BLANKS
108300050127     C                   MOVEL     CTV(3)        OLV59CT4
108400050127     C                   MOVEL     SPV(3)        OLV59ES4
108500050127     C                   MOVE      Fiv(3)        OLV59ES4
108600941107     C                   ENDIF
108700941107     C     CTV(4)        IFNE      *BLANKS
108800050127     C                   MOVEL     CTV(4)        OLV59CT5
108900050127     C                   MOVEL     SPV(4)        OLV59ES5
109000050127     C                   MOVE      Fiv(4)        OLV59ES5
109100941107     C                   ENDIF
109200050128     c
109300050128     c* Carico le schiere di output
109400050128     c                   if        ctv(1)<>*blanks
109500050128     c                   movea     ctv           olv59CTT
109600050128     c                   movea     pgv           olv59pgT
109700050128     c* Attributi
109800050128     c                   movea     fiv           olv59at1
109900050128     c                   movea     spv           olv59at2
110000050128     c                   movea     bav           olv59at3
110100050128     c                   movea     tpv           olv59at4
110200050128     c                   movea     vav           olv59at5
110300050128     c                   endif
110400050128     c
110500941107     C                   ENDSR
110600941118**
110700941104Per il codice cliente indicato e' impossibile eseguire la ricerca tariffe
110800941104Tipo Tariffa errato
110900060516Codice Tariffa inesistente o non utilizzabile per il cliente indicato
111000941118Codice tariffa errato
111100060511Codice tariffa esistente ma non utilizzabile in Partenza e/o x questo Network
111200060511Codice tariffa esistente ma non utilizzabile in Arrivo e/o x questo Network
111300060511Codice tariffa esistente ma bloccato o non utilizzabile per questo Network
