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
008000941104     D KPJBA         E DS
008100941104     C*---------------------------------------------------------------*
008200941104     C     *ENTRY        PLIST
008300050128     C                   PARM                    fnlv59ds
008400941104     C*---------------------------------------------------------------*
008500050128     C* TIPO LANCIO, ilv59TLA "C" -           CHIUSO CON LR
008600050128     C* TIPO LANCIO, ilv59TLA "L" - ELABORO E CHIUDO CON LR
008700050128     C* TIPO LANCIO, ilv59TLA " " - ELABORO E CHIUDO IN RETRN
008800941104     C*
008900050128    1C     ilv59tla      IFNE      'C'
009000941107     C*
009100941107     C* PULIZIA CAMPI DI OUTPUT
009200050127     C                   CLEAR                   olv59mSG
009300050127     C                   CLEAR                   olv59ERR
009400050127     C                   CLEAR                   olv59DFS
009500050127     C                   CLEAR                   olv59PRG
009600050127     C                   CLEAR                   olv59DIV
009700050128     C                   CLEAR                   olv59CTR
009800050128     c
009900050128     C                   MOVEL     *BLANKS       olv59CT2
010000050128     C                   MOVEL     *BLANKS       olv59CT3
010100050128     C                   MOVEL     *BLANKS       olv59CT4
010200050128     C                   MOVEL     *BLANKS       olv59CT5
010300050128     C                   MOVEL     *BLANKS       olv59ES2
010400050128     C                   MOVEL     *BLANKS       olv59ES3
010500050128     C                   MOVEL     *BLANKS       olv59ES4
010600050128     C                   MOVEL     *BLANKS       olv59ES5
010700050128     c
010800050128     C                   MOVEL     *BLANKS       olv59CTT
010900050128     C                   MOVEL     *BLANKS       olv59pgT
011000050128     C                   MOVEL     *BLANKS       olv59AT1
011100050128     C                   MOVEL     *BLANKS       olv59AT2
011200050128     C                   MOVEL     *BLANKS       olv59AT3
011300050128     C                   MOVEL     *BLANKS       olv59AT4
011400050128     C                   MOVEL     *BLANKS       olv59AT5
011500050128     C                   MOVEL     *BLANKS       olv59ATt
011600941107     C*
011700941117     C* SOLO CONTROLLO FORMALE
011800050128    2C     ilv59CFO      IFEQ      'S'
011900050128     c                   eval      wcontrCTR=Ilv59CTR
012000941117     C                   EXSR      CTRFOR
012100941117     C*
012200941117   X2C                   ELSE
012300941117     C*
012400050127    3C     ilv59ksc      IFNE      WKSC
012500050127     C     ilv59fie      ORNE      WFIE
012600050127     C     ilv59tbo      ORNE      WTBO
012700050127     C     ilv59dsp      ORNE      WDSP
012800050128     C     ilv59blo      ORNE      Wblo
012900050128     C     ilv59VAL      ORNE      WVAL
013000060515     C**** ilv59ta2      ORNE      Wta2
013100050128     C     ilv59ta3      ORNE      Wta3
013200050128     C     ilv59ta4      ORNE      Wta4
013300050128     C     ilv59ta5      ORNE      Wta5
013400941104     C                   EXSR      CARCTR
013500941231     C                   ELSE
013600941231     C                   EXSR      VISTAR
013700941231    3C                   ENDIF
013800050127     C                   MOVEL     ilv59ksc      WKSC              7 0
013900050127     C                   MOVEL     ilv59dsp      WDSP              8 0
014000050127     C                   MOVEL     ilv59fie      WFIE              1
014100050127     C                   MOVEL     ilv59tbo      WTBO              1
014200050128     C                   MOVEL     ilv59blo      WBLO
014300050128     C                   MOVEL     ilv59val      WVAL
014400060515     C****               MOVEL     ilv59ta2      WTA2
014500050128     C                   MOVEL     ilv59ta3      WTA3
014600050128     C                   MOVEL     ilv59ta4      WTA4
014700050128     C                   MOVEL     ilv59ta5      WTA5
014800941104     C*
014900050127    3C     ilv59ric      IFEQ      'S'
015000941104     C                   EXSR      RICTAR
015100941215    3C                   ENDIF
015200941104     C*
015300050127    3C     ilv59con      IFEQ      'S'
015400050128     c                   if        olv59ctr=*blanks
015500050128     c                   eval      olv59ctr=ilv59ctr
015600050128     c                   endif
015700050128     c
015800941104     C                   EXSR      CTRCTR
015900941215    3C                   ENDIF
016000941215     C*
016100050128     C* DECODIFICA FISSA TARIFFA se non ancora messa
016200050127     C     olv59CTR      IFNE      *BLANKS
016300050202     C     olv59dfs      andEQ     *BLANKS
016400941215     C                   Z-ADD     1             C
016500050127     C                   MOVEL     olv59ctr      UNOCTR            1
016600941215     C     UNOCTR        LOOKUP    CTA(C)                                 30
016700050127     C   30              MOVEL     DTA(C)        olv59dfs
016800941215     C                   ENDIF
016900941215     C*
017000941117    2C                   ENDIF
017100941107     C*
017200941104    1C                   ENDIF
017300941104     C* CHIUSURA PGM
017400050128    1C     ilv59TLA      IFEQ      ' '
017500941104     C                   RETURN
017600941104     C                   ELSE
017700941104     C                   SETON                                        LR
017800941104    1C                   ENDIF
017900050218     c*
018000050218     C* CARICO LE TARIFFE DEL CLIENTE --------------------------------*
018100050218     C     CARCTR        BEGSR
018200050218     C*
018300050218     C* PULISCO SKIERE DELLE TARIFFE
018400050218     C                   CLEAR                   CTV
018500050218     C                   CLEAR                   CTN
018600050218     c                   Clear                   Ctw
018700050218     c                   Clear                   Ctcli
018800050218     C                   CLEAR                   TPN
018900050218     C                   CLEAR                   TPV
019000050218     c                   Clear                   Tpw
019100050218     c                   Clear                   Tpcli
019200050218     C                   CLEAR                   SPN
019300050218     C                   CLEAR                   SPV
019400050218     c                   Clear                   Spw
019500050218     c                   Clear                   Spcli
019600050218     C                   CLEAR                   PGV
019700050218     C                   CLEAR                   PGN
019800050218     C                   CLEAR                   PGcli
019900050218     C                   CLEAR                   DIV
020000050218     C                   CLEAR                   DIN
020100050218     C                   CLEAR                   DIcli
020200050218     C                   CLEAR                   FIV
020300050218     C                   CLEAR                   FIN
020400050218     C                   CLEAR                   FIcli
020500050218     c                   Clear                   mdv
020600050218     c                   Clear                   mdn
020700050218     c                   Clear                   mdcli
020800050218     c                   Clear                   bav
020900050218     c                   Clear                   ban
021000050218     c                   Clear                   bacli
021100050218     c                   Clear                   bavdop
021200050218     c                   Clear                   bandop
021300050218     c                   Clear                   vav
021400050218     c                   Clear                   vacli
021500050218     C*
021600050218     C                   MOVEL     *BLANKS       olv59TKS
021700050218     C*
021800050218     C* MITTENTI CODIFICATI --> CERCO LE TARIFFE
021900050218     C                   Z-ADD     0             B                 3 0
022000050218     C                   Z-ADD     0             C                 3 0
022100050218     C                   Z-ADD     0             D                 3 0
022200050218     C*
022300050218     C* SE 8888 O 9999 NON CARICO NULLA
022400050218     C                   MOVE      ilv59KSC      WCCM              4 0
022500050218     C*
022600050218    1C     WCCM          IFNE      8888
022700050218     C     WCCM          ANDNE     9999
022800050218     C*
022900050218     C     ilv59ksc      SETLL     TNTAM000
023000050218     C     ilv59ksc      READE     TNTAM000                               30
023100050218     C*
023200050218    2C     *IN30         DOWEQ     *OFF
023300050218     C* ESCLUDO SE ANNULLATO
023400050218    3C     TAMATB        IFNE      'A'
023500050218     c
023600050218     C                   MOVEL     TAMFLO        DSTA01
023700050218     C*
023800050218     C* CARICO SOLO TARIFFE ITALIA           --> FIE "I"
023900050218     C* CARICO SOLO TARIFFE Euroexpress      --> FIE "E"
024000050218     C* CARICO SOLO TARIFFE DPD              --> FIE "D"
024100050218     C* CARICO SOLO TARIFFE FEDEX            --> FIE "F"
024200050218     C* CARICO      TARIFFE DPD+FEDEX+EUROEX --> FIE "S"
024300050218     C* CARICO      TARIFFE DPD+FEDEX+ITALIA --> FIE "J"
024400050218     C* CARICO SOLO TARIFFE FEDEX DOCUMENTI  --> FIE "N"
024500050218     C* CARICO SOLO TARIFFE FEDEX MERCI      --> FIE "M"
024600060503    4C***  TAMFIE        IFEQ      ' '
024700060503     C***
024800060503     C***  TAMFIE        OREQ      ilv59fie
024900060503     C***  §TADPD        ANDEQ     ' '
025000060503     C***  §TAFED        ANDEQ     ' '
025100050218     C* SE TARIFFA DPD NON DEVO CARICARE  SE RICHIESTO I/E/F/M/N
025200060503     C***  §TADPD        OREQ      'S'
025300060503     C***  ilv59fie      ANDNE     'I'
025400060503     C***  ilv59fie      ANDNE     'E'
025500060503     C***  ilv59fie      ANDNE     'F'
025600060503     c***  ilv59fie      andne     'M'
025700060503     c***  ilv59fie      andne     'N'
025800050218     C*
025900050218     C* SE TARIFFA FEDEX NON DEVO CARICARE  SE RICHIESTO I/E/D
026000060503     C***  §TAFED        OREQ      'S'
026100060503     C***  ilv59fie      ANDNE     'I'
026200060503     C***  ilv59fie      ANDNE     'E'
026300060503     C***  ilv59fie      ANDNE     'D'
026400050218     C* LE ESTERE LE DEVO CARICARE SOLO PER S (OLTRE CHE DPD E FEDEX)
026500060503     C***  TAMFIE        OREQ      'E'
026600060503     C***  ilv59fie      ANDEQ     'S'
026700050218     C* LE ITALIA LE DEVO CARICARE SOLO PER J (OLTRE CHE DPD E FEDEX)
026800060503     C***  TAMFIE        OREQ      'I'
026900060503     C***  ilv59fie      ANDEQ     'J'
027000050218
027100050218     c                   Move      TamCtr        Wctr
027200050218      * Se Tamfie "M" Solo Tariffe merci FedEx
027300050218    5c                   If        ilv59Fie = 'M' and Wctr >= §DfeDalD
027400060511     c                   GoTo      Notartutto
027500050218    5c                   EndIf
027600050218      * Se Tamfie "N" Solo Tariffe documenti FedEx
027700050218    5c                   If        ilv59Fie = 'N' and Wctr < §DfeDalD
027800060511     c                   GoTo      Notartutto
027900050218    5c                   EndIf
028000050218     c* Elaboro solo tariffe decorrenti o scadute (escludo da decorrere
028100050218     c*  come se non ci fossero)
028200060503    5c***                if        ilv59dsp>=tamddt
028300060503     c* 03/5/06 ES --> preferisco caricare tutto. Se un tariffa ancora
028400060503     c*                da decorrere è già stata immessa, meglio
028500060503     c*                non farla usare in modo improprio, salvo
028600060503     c*                che non sia anche presente come decorrente:allor
028700060503     c*                la ignoro: meglio la decorrente
028800050218     c
028900050218     C                   MOVEL     TAMCTR        COMCT2            3
029000050218     C                   Z-ADD     1             D
029100050218     C     COMCT2        LOOKUP    CTcli(D)                               31
029200060503    6c                   if        (*in31 and ilv59dsp>=tamddt) or
029300050218     c                             (not *in31 and c < 50)
029400050218     C*
029500060511     C* DELLE TARIFFE già presenti, tengo sempre l'ulitma, escluse le da
029600060511     c*  decorrere
029700050218    7C     *IN31         IFEQ      *OFF
029800050218     C                   ADD       1             C
029900050218     C                   MOVEL     TAMCTR        CTcli(C)
030000050218     C                   Z-ADD     C             D
030100050218    7C                   ENDIF
030200050218     C*
030300050218     C* VEDO SE E' TARIFFA decorrente
030400060503     c                   select
030500060503    7C     ilv59dsp      wheneq    TAMDDT
030600050218     C     ilv59dsp      ANDLE     TAMDST
030700060503     c* Decorrente
030800050218     c                   movel     'D'           vacli(d)
030900060503    7C     ilv59dsp      whenlT    TAMDDT
031000060503     c* dA decorrere
031100060503     c                   movel     'A'           vacli(d)
031200060503     c
031300060503     c                   other
031400060503     c* Scaduta
031500050218     c                   movel     'S'           vacli(d)
031600060511    7c                   endsl
031700050218     C*
031800050218     C                   MOVEL     TAMCTR        CTcli(d)
031900050218     C                   MOVEL     TAMTSP        SPcli(d)
032000050218     C                   MOVE      TAMFTP        TPcli(d)
032100050218     C                   MOVE      TAMPRG        PGcli(d)
032200050218     C                   MOVE      §TADIV        DIcli(d)
032300050218     C                   MOVE      TAMFIE        FIcli(d)
032400050218     C                   MOVE      TAMbap        bacli(d)
032500050218     C* TARIFFA DPD
032600050218     C     §TADPD        IFEQ      'S'
032700050218     C                   MOVE      'D'           FIcli(d)
032800050218     C                   ENDIF
032900050218     C* TARIFFA FEDEX
033000050218    7C     §TAFED        IFEQ      'S'
033100050218     C                   MOVE      'F'           FIcli(d)
033200050218     c                   Clear                   mdcli(d)
033300050218     c                   If        ilv59Fie = 'M' and Wctr >= §DfeDalM
033400050218     c                                            and Wctr <= §DfeAlM
033500050218     c                   Eval      mdcli(d) = 'M'
033600050218     c                   EndIf
033700050218     c                   If        ilv59Fie = 'N' and Wctr >= §DfeDalD
033800050218     c                                            and Wctr <= §DfeAlD
033900050218     c                   Eval      mdcli(d) = 'N'
034000050218     c                   EndIf
034100050218    7c                   EndIf
034200050218    6C                   ENDIF
034300060511    5C*********          ENDIF
034400060511    4C*********          ENDIF
034500050218    3C                   ENDIF
034600050218
034700060511     c     Notartutto    Tag
034800050218
034900050218     C*
035000050218     C  N30ilv59ksc      READE     TNTAM000                               30
035100050218     C*
035200050218    2C                   ENDDO
035300050218     c
035400050218     c                   exsr      SELCTR
035500050218    1c                   endif
035600050218     c
035700050218     C                   ENDSR
035800941104     C*
035900050218     C* SELEZIONO le tariffe del cliente in base a quanto passato in DS ---*
036000050218     C     SELCTR        BEGSR
036100941104     C*
036200941104     C* PULISCO SKIERE DELLE TARIFFE
036300941104     C                   CLEAR                   CTV
036400941104     C                   CLEAR                   CTN
036500021119     c                   Clear                   Ctw
036600941104     C                   CLEAR                   TPN
036700941104     C                   CLEAR                   TPV
036800021119     c                   Clear                   Tpw
036900941104     C                   CLEAR                   SPN
037000941104     C                   CLEAR                   SPV
037100021119     c                   Clear                   Spw
037200941104     C                   CLEAR                   PGV
037300941104     C                   CLEAR                   PGN
037400990930     C                   CLEAR                   DIV
037500990930     C                   CLEAR                   DIN
037600000119     C                   CLEAR                   FIV
037700000119     C                   CLEAR                   FIN
037800021115     c                   Clear                   mdv
037900021115     c                   Clear                   mdn
038000050127     c                   Clear                   bav
038100050127     c                   Clear                   ban
038200050128     c                   Clear                   bavdop
038300050128     c                   Clear                   bandop
038400050127     c                   Clear                   vav
038500941104     C*
038600050127     C                   MOVEL     *BLANKS       olv59TKS
038700941104     C*
038800941104     C* MITTENTI CODIFICATI --> CERCO LE TARIFFE
038900941104     C                   Z-ADD     0             B                 3 0
039000941104     C                   Z-ADD     0             C                 3 0
039100941104     C                   Z-ADD     0             D                 3 0
039200941110     C*
039300941110     C* SE 8888 O 9999 NON CARICO NULLA
039400050128     C                   MOVE      ilv59KSC      WCCM              4 0
039500941104     C*
039600050128    1C     WCCM          IFNE      8888
039700941110     C     WCCM          ANDNE     9999
039800941110     C*
039900050127     C     ilv59ksc      SETLL     TNTAM000
040000050127     C     ilv59ksc      READE     TNTAM000                               30
040100941104     C*
040200941104    2C     *IN30         DOWEQ     *OFF
040300941104     C* ESCLUDO SE ANNULLATO
040400941104    3C     TAMATB        IFNE      'A'
040500050127     C* ilv59BLO:  B=solo bloccate " "=solo non bloccate  S=tutte
040600050128    4c                   if         ilv59BLO='S' or
040700050127     c                             (ilv59BLO='B' and tambap='B') or
040800050127     c                             (ilv59BLO=' ' and tambap<>'B')
040900050127     c
041000000119     C                   MOVEL     TAMFLO        DSTA01
041100000126     C*
041200020206     C* CARICO SOLO TARIFFE ITALIA           --> FIE "I"
041300050127     C* CARICO SOLO TARIFFE Euroexpress      --> FIE "E"
041400020206     C* CARICO SOLO TARIFFE DPD              --> FIE "D"
041500020206     C* CARICO SOLO TARIFFE FEDEX            --> FIE "F"
041600021115     C* CARICO      TARIFFE DPD+FEDEX+EUROEX --> FIE "S"
041700020206     C* CARICO      TARIFFE DPD+FEDEX+ITALIA --> FIE "J"
041800021115     C* CARICO SOLO TARIFFE FEDEX DOCUMENTI  --> FIE "N"
041900021115     C* CARICO SOLO TARIFFE FEDEX MERCI      --> FIE "M"
042000050128    5C     TAMFIE        IFEQ      ' '
042100000119     C*
042200050127     C     TAMFIE        OREQ      ilv59fie
042300000119     C     §TADPD        ANDEQ     ' '
042400020206     C     §TAFED        ANDEQ     ' '
042500021115     C* SE TARIFFA DPD NON DEVO CARICARE  SE RICHIESTO I/E/F/M/N
042600000119     C     §TADPD        OREQ      'S'
042700050127     C     ilv59fie      ANDNE     'I'
042800050127     C     ilv59fie      ANDNE     'E'
042900050127     C     ilv59fie      ANDNE     'F'
043000050127     c     ilv59fie      andne     'M'
043100050127     c     ilv59fie      andne     'N'
043200000124     C*
043300020206     C* SE TARIFFA FEDEX NON DEVO CARICARE  SE RICHIESTO I/E/D
043400020206     C     §TAFED        OREQ      'S'
043500050127     C     ilv59fie      ANDNE     'I'
043600050127     C     ilv59fie      ANDNE     'E'
043700050127     C     ilv59fie      ANDNE     'D'
043800020206     C* LE ESTERE LE DEVO CARICARE SOLO PER S (OLTRE CHE DPD E FEDEX)
043900000124     C     TAMFIE        OREQ      'E'
044000050127     C     ilv59fie      ANDEQ     'S'
044100020206     C* LE ITALIA LE DEVO CARICARE SOLO PER J (OLTRE CHE DPD E FEDEX)
044200000126     C     TAMFIE        OREQ      'I'
044300050127     C     ilv59fie      ANDEQ     'J'
044400021115
044500021118     c                   Move      TamCtr        Wctr
044600021115      * Se Tamfie "M" Solo Tariffe merci FedEx
044700050128    6c                   If        ilv59Fie = 'M' and Wctr >= §DfeDalD
044800021121     c                   GoTo      Notar
044900050128    6c                   EndIf
045000021121      * Se Tamfie "N" Solo Tariffe documenti FedEx
045100050128    6c                   If        ilv59Fie = 'N' and Wctr < §DfeDalD
045200021121     c                   GoTo      Notar
045300050128    6c                   EndIf
045400941107     C*
045500050127     C* ilv59TBO P=carico tariffe P/B/" "
045600050127     a*          A=carico tariffe A/B/" "   " "=tutte
045700050128    6C     ilv59tbo      IFEQ      ' '
045800981007     C     TAMBAP        OREQ      ' '
045900050127     C     TAMBAP        OREQ      'B'
046000050127     C     TAMBAP        OREQ      ilv59tbo
046100941104     C*
046200050127     C* VEDO SE E' TARIFFA decorrente
046300050128    7C     ilv59dsp      IFGE      TAMDDT
046400050127     C     ilv59dsp      ANDLE     TAMDST
046500050127     c* solo se richieste (ilv59VAL=D/" ")
046600050128    8c                   if        ilv59VAL=' ' or ilv59val='D'
046700941104     C*
046800941104     C                   ADD       1             B
046900941104     C                   MOVEL     TAMCTR        CTV(B)
047000941104     C                   MOVEL     TAMTSP        SPV(B)
047100941104     C                   MOVE      TAMFTP        TPV(B)
047200941104     C                   MOVE      TAMPRG        PGV(B)
047300990930     C                   MOVE      §TADIV        DIV(B)
047400000119     C                   MOVE      TAMFIE        FIV(B)
047500050124     C                   MOVE      TAMbap        baV(B)
047600050127     c* Carico il campo bap anche mettendo al posto di " " --> "E"
047700050128    9c                   if        tambap=' ' or tambap='B'
047800050127     c                   movel     'E'           bavdop(b)
047900050127     c                   else
048000050127     C                   MOVE      TAMbap        baVdop(B)
048100050128    9c                   endif
048200050127     C                   MOVE      'D'           vaV(B)
048300000119     C* TARIFFA DPD
048400000119     C     §TADPD        IFEQ      'S'
048500000126     C                   MOVE      'D'           FIV(B)
048600000119     C                   ENDIF
048700020206     C* TARIFFA FEDEX
048800050128    9C     §TAFED        IFEQ      'S'
048900020206     C                   MOVE      'F'           FIV(B)
049000021115     c                   Clear                   mdv(b)
049100050128     c                   If        ilv59Fie = 'M' and Wctr >= §DfeDalM
049200050128     c                                            and Wctr <= §DfeAlM
049300021115     c                   Eval      mdv(b) = 'M'
049400021115     c                   EndIf
049500050128     c                   If        ilv59Fie = 'N' and Wctr >= §DfeDalD
049600050128     c                                            and Wctr <= §DfeAlD
049700021115     c                   Eval      mdv(b) = 'N'
049800021115     c                   EndIf
049900050128    9c                   EndIf
050000050127     c
050100050127     C     B             COMP      50                                 30
050200050201   x8c                   else
050300050201     c* Se tariffa decorrente e c'e' anche come scaduta la tolgo
050400050201     C*
050500050201     C                   MOVEL     TAMCTR        COMCT2            3
050600050201     C                   Z-ADD     1             D
050700050201     C     COMCT2        LOOKUP    CTN(D)                                 31
050800050201    9c                   if        *in31
050900050201     c                   clear                   ctn(d)
051000050201     c                   clear                   spn(d)
051100050201     c                   clear                   tpn(d)
051200050201     c                   clear                   pgn(d)
051300050201     c                   clear                   din(d)
051400050201     c                   clear                   fin(d)
051500050201     c                   clear                   ban(d)
051600050201     c                   clear                   bandop(d)
051700050201     c                   clear                   mdn(d)
051800050201     c* Reimposto il contatore a -1
051900050201     c                   sub       1             C
052000050201     c
052100050201    9C                   ENDIF
052200050201    8C                   ENDIF
052300941104     C*
052400050127     C* TARIFFE non decorrente
052500050128   X7C                   ELSE
052600050127     c*         tengo solo le scadute, se richieste (ilv59VAL=S/" ")
052700050128    8c                   if        ilv59dsp>tamdst  and
052800050127     c                             (ilv59VAL=' ' or ilv59VAL='S')
052900941104     C*
053000941104     C                   MOVEL     TAMCTR        COMCT2            3
053100941104     C                   Z-ADD     1             D
053200941104     C     COMCT2        LOOKUP    CTN(D)                                 31
053300050128    9c                   if        *in31   or
053400050127     c                             (not *in31 and c < 50)
053500941104     C*
053600941104     C* DELLE TARIFFE NON VALIDE, TENGO SEMPRE L'ULTIMA
053700050128   10C     *IN31         IFEQ      *OFF
053800941104     C                   ADD       1             C
053900941104     C                   MOVEL     TAMCTR        CTN(C)
054000941104     C                   Z-ADD     C             D
054100050128   10C                   ENDIF
054200941104     C                   MOVEL     TAMTSP        SPN(D)
054300941104     C                   MOVE      TAMFTP        TPN(D)
054400941104     C                   MOVE      TAMPRG        PGN(D)
054500990930     C                   MOVE      §TADIV        DIN(D)
054600000119     C                   MOVE      TAMFIE        FIN(D)
054700050124     C                   MOVE      TAMbap        ban(D)
054800050127     c* Carico il campo bap anche mettendo al posto di " " --> "E"
054900050127     c                   if        tambap=' '   or  tambap='B'
055000050127     c                   movel     'E'           bandop(D)
055100050127     c                   else
055200050127     C                   MOVE      TAMbap        bandop(D)
055300050127     c                   endif
055400000126     C* TARIFFA DPD
055500000126     C     §TADPD        IFEQ      'S'
055600000201     C                   MOVE      'D'           FIN(D)
055700000126     C                   ENDIF
055800020206     C* TARIFFA FEDEX
055900050128   10C     §TAFED        IFEQ      'S'
056000020206     C                   MOVE      'F'           FIN(D)
056100021115     c                   Clear                   mdn(d)
056200050128     c                   If        ilv59Fie = 'M' and Wctr >= §DfeDalM
056300050128     c                                            and Wctr <= §DfeAlM
056400021115     c                   Eval      mdn(d) = 'M'
056500021115     c                   EndIf
056600050128     c                   If        ilv59Fie = 'N' and Wctr >= §DfeDalD
056700050128     c                                            and Wctr <= §DfeAlD
056800021115     c                   Eval      mdn(d) = 'N'
056900021115     c                   EndIf
057000050128   10C                   ENDIF
057100050128    9C                   ENDIF
057200941104     C*
057300050128    8C                   ENDIF
057400050128    7C                   ENDIF
057500050128    6C                   ENDIF
057600050128    5C                   ENDIF
057700050128    4C                   ENDIF
057800050128    3C                   ENDIF
057900021115
058000021121     c     Notar         Tag
058100021121
058200941104     C*
058300050127     C  N30ilv59ksc      READE     TNTAM000                               30
058400981006     C*
058500941104    2C                   ENDDO
058600941104     C*
058700941104     C                   ADD       1             B
058800941104     C*
058900941104     C* CARICO TUTTI I TIPI TARIFFA TROVATI IN UNICA SCHIERA
059000050128     C* SE NON NE HO GIA' TROVATI 50 VALIDI
059100050128    2C     B             IFLE      50
059200941104     C*
059300941104     C*
059400941104     C* SE ESISTE ALMENO UNA TARIFFA VALIDA, NON CONSIDERO LA 'P'
059500941104     C*  DI PREFERENZIALE PER LE TARIFFE SCADUTE
059600941104    3C     B             IFGT      1
059700941104     C                   CLEAR                   TPN
059800941104    3C                   ENDIF
059900941104     C*
060000941104     C                   Z-ADD     1             C
060100941104     C*
060200941104    3C     CTN(C)        DOWNE     *BLANKS
060300050128     C     B             ANDLE     50
060400941104     C*
060500000119     C     CTN(C)        LOOKUP    CTV                                    31
060600941104    4C     *IN31         IFEQ      *OFF
060700941104     C                   MOVEL     CTN(C)        CTV(B)
060800941104     C                   MOVEL     TPN(C)        TPV(B)
060900941104     C                   MOVEL     SPN(C)        SPV(B)
061000941104     C                   MOVEL     PGN(C)        PGV(B)
061100990930     C                   MOVEL     DIN(C)        DIV(B)
061200000119     C                   MOVEL     FIN(C)        FIV(B)
061300021115     c                   Movel     mdn(c)        mdv(b)
061400050124     c                   Movel     ban(c)        bav(b)
061500050127     c                   Movel     bandop(c)     bavdop(b)
061600050127     c* S-scaduta
061700050127     c                   movel     'S'           vav(b)
061800941104     C                   ADD       1             B
061900941104    4C                   ENDIF
062000941104     C*
062100941104     C                   ADD       1             C
062200050128    3C                   ENDdo
062300050128    2C                   ENDif
062400021115     C*
062500941107     C                   EXSR      VISTAR
062600050128    1C                   ENDif
062700941104     C*
062800941104     C                   ENDSR
062900941104     C*
063000941104     C* RICERCA E CARICAMENTO A VIDEO TARIFFE ------------------------*
063100941104     C     RICTAR        BEGSR
063200941104     C                   SETOFF                                       30
063300950125     C                   CLEAR                   W001A
063400050124     c                   clear                   CNo
063500941104     C*
063600941104     C* SE NON CI SONO TARIFFE IMPOSTO LA TARIFFA 000
063700050127    1C     CTV(1)        IFEQ      *BLANKS
063800050127     C                   CLEAR                   olv59tks
063900941215     C*
064000050127     C     ilv59ctr      IFEQ      *BLANKS
064100050127     C                   MOVEL     *ZEROS        olv59CTR
064200941215     C                   ENDIF
064300941215     C*
064400050127   X1C                   ELSE
064500941107     C*
064600941107     C* CODICE TARIFFA IMPOSTATO: PRENDO IL RELATIVO PROGRESSIVO
064700050127    2C     ilv59CTR      IFNE      *BLANKS
064800050127     c                   eval      olv59ctr=ilv59ctr
064900050127    2C                   ENDIF
065000941107     C**
065100050127     c**
065200050127    2C     ilv59CTR      IFEQ      *BLANKS
065300021115
065400050127      * Carico le sk di work per la scelta
065500021115     c                   ExSr      CarSk
065600941104     C*
065700941104     C* SE IMPOSTATO IL TIPO SERVIZIO --> CERCO QUELLO
065800050128    3C     ilv59TSP      IFNE      *BLANKS
065900941104     C* CERCO SE C'E' LA PREFERENZIALE ALTRIMENTI LA 1 CHE TROVO
066000941104     C                   Z-ADD     1             C
066100941104     C                   SETON                                        31
066200941104     C*
066300941104    4C     *IN31         DOWEQ     *ON
066400050128     C     ilv59tSP      LOOKUP    SPw(C)                                 31
066500941104     C* SE TROVATA ED E' PREFERENZIALE ESCO DAL CICLO
066600050127    5c                   if        *in31 and  tpw(C)='P'
066700941104     C                   SETON                                        30
066800941104     C                   SETOFF                                       31
066900941104     C                   MOVEL     '1'           W001A             1
067000050127   X5C                   ELSE
067100021115     C* SE TROVATA MA NON PREFERENZIALE ACCENDO COMUNQUE L'INDICATORE
067200941104     C*  DI TROVATO MA FACCIO UN ALTRO GIRO
067300941104     C                   SETON                                        30
067400050124     c                   if        CNo=0
067500050124     c                   z-add     C             CNo
067600050127     c                   endif
067700050124     c
067800941104     C                   ADD       1             C
067900050124    5C                   ENDIF
068000941104    4C                   ENDDO
068100941104     C*
068200941104     C* SE HO TROVATO LA TARIFFA MA NON LA PREFERENZIALE, LA RICERCO X
068300941104     C*  PRENDERE LA POSIZIONE
068400941104    4C   30W001A         IFEQ      ' '
068500050124     c                   z-add     CNo           C
068600941104    4C                   ENDIF
068700941104     C*
068800941104    3C                   ENDIF
068900941104     C*
069000941104     C* SE NON TROVATA O NON IMPOSTATO IL TIPO SERVIZIO -->
069100941104     C*  CERCO LA PRIMA PREFERENZIALE
069200050124    3C     *IN30         IFEQ      *OFF
069300050127     C                   Z-ADD     1             C
069400050127     C     'P'           LOOKUP    TPw(C)                                 30
069500050127    3C                   ENDIF
069600941104     C*
069700941104     C* SE NON ESISTE LA PREFERENZIALE METTO LA PRIMA VALIDA TROVATA
069800941104     C  N30              Z-ADD     1             C
069900050127     c
070000050127     c* IMPOSTO CAMPO CODICE TARIFFA
070100050127     C                   MOVEL     CTw(C)        Olv59CTR
070200050127    2c                   endif
070300941104     C*
070400050127      * Cerco la posizione nella sk valida
070500050127     c                   Z-Add     1             c
070600050127     c     olv59CTR      Lookup    Ctv(c)                                 30
070700050127     c
070800050128    2c                   IF        *IN30
070900050127     C                   MOVEL     PGV(C)        OLV59PRG
071000050127     C                   MOVEL     DIV(C)        OLV59DIV
071100050127     C     OLV59DIV      IFEQ      '   '
071200050127     C                   MOVEL     'ITL'         OLV59DIV
071300991026     C                   ENDIF
071400050127
071500050127   x2C                   ELSE
071600050127     C* ERRORE --> COD.TARIFFA NON TROVATO (se passato in input può
071700050127     c*            accadere)
071800050127     C                   MOVEL     '3'           olv59ERR
071900050127     C                   MOVEL     ERR(3)        olv59MSG
072000050127     C                   Z-ADD     10            C
072100050127    2C                   ENDIF
072200941107     C*
072300021115     C                   EXSR      VISTAR
072400941104     C*
072500941104     C* RIMPIAZZO CON COD TARIFFA 5 SOLO SE ESISTE
072600050127    2C     CTv(5)        IFNE      *BLANKS
072700941104     C* SE NELLA 1°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
072800050127    3C     C             IFEQ      1
072900050128     C                   MOVEL     CTv(5)        olv59CT2
073000050128     C                   MOVEL     SPv(5)        olv59ES2
073100050128     C                   MOVE      FIv(5)        olv59ES2
073200050127    3C                   END
073300941104     C* SE NELLA 2°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
073400050127    3C     C             IFEQ      2
073500050128     C                   MOVEL     CTv(5)        olv59CT3
073600050128     C                   MOVEL     SPv(5)        olv59ES3
073700050128     C                   MOVE      FIv(5)        olv59ES3
073800050127    3C                   END
073900941104     C* SE NELLA 3°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
074000050127    3C     C             IFEQ      3
074100050128     C                   MOVEL     CTv(5)        olv59CT4
074200050128     C                   MOVEL     SPv(5)        olv59ES4
074300050128     C                   MOVE      FIv(5)        olv59ES4
074400050127    3C                   END
074500941104     C* SE NELLA 4°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
074600050127    3C     C             IFEQ      4
074700050128     C                   MOVEL     CTv(5)        olv59CT5
074800050128     C                   MOVEL     SPv(5)        olv59ES5
074900050128     C                   MOVE      FIv(5)        olv59ES5
075000050127    3C                   END
075100941104     C*
075200050127    2C                   END
075300050127     C
075400050127    1C                   ENDIF
075500941104     C*
075600941104     C                   ENDSR
075700021115      *--------------------------------------------------------------*
075800050127      * Carico le sk di work per la proposta a video
075900021115      *--------------------------------------------------------------*
076000021115     c     Carsk         BegSr
076100021115
076200050127     c* Se richieste tariffe FEDEX e presenti sia M che N --> propon
076300050127     c*  go "N"-documenti
076400050127     c                   setoff                                       3536
076500050127     c                   setoff                                       3738
076600050127     c                   If        ilv59Fie = 'F'
076700021115     c     'M'           Lookup    mdv                                    35
076800021115     c     'N'           Lookup    mdv                                    36
076900021115     c                   EndIf
077000050127     c
077100050127     c* Se presenti sia tariffe "P" che "A" --> propongo la P o la A
077200050127     c*    in base alla richiesta (ilv59PRP)
077300050128    1c                   if        ilv59tbo=' '  and ilv59PRP<>' '
077400050127     c     'E'           Lookup    bavdop                                 37
077500050128    2c                   if        *in37
077600050127     c                   seton                                        38
077700050128   x2c                   else
077800050127     c     'P'           Lookup    bavdop                                 37
077900050127     c     'A'           Lookup    bavdop                                 38
078000050128    2c                   endif
078100050128    1c                   endif
078200021115
078300021115     c                   Clear                   d
078400021115
078500050128    1c                   Do        50            b
078600021115     c                   If        Ctv(b) = *Blanks
078700021115     c                   Leave
078800021115     c                   EndIf
078900050127     c* per fedex tengo la documenti se presenti entrambe
079000050127     c                   If        ilv59Fie = 'F' and *In35 and *In36
079100021115     c                             and Mdv(b) <> 'N'
079200021115     c                   Iter
079300021115     c                   EndIf
079400050127     c
079500050127     c* Per la proposta impostata e presenti entrambe tariffe (p e A)
079600050127     c*  carico solo la proposta scelta
079700050127     c                   if        ilv59tbo=' ' and ilv59prp<>' ' and
079800050127     c                             *in37 and *in38
079900050127     c                   if        ilv59prp='P' and bavdop(b)='A'
080000050127     c                   iter
080100050127     c                   endif
080200050127     c                   if        ilv59prp='A' and bavdop(b)='P'
080300050127     c                   iter
080400050127     c                   endif
080500050127     c
080600050127     c                   endif
080700021115
080800021115     c                   Add       1             d
080900021115
081000021115     c                   Eval      Ctw(d) = Ctv(b)
081100021115     c                   Eval      Tpw(d) = Tpv(b)
081200021115     c                   Eval      Spw(d) = Spv(b)
081300050128    1c                   EndDo
081400021115
081500021115     c                   EndSr
081600941104     C*
081700941104     C* CONTROLLO CODICE TARIFFA ------------------------------------*
081800941104     C     CTRCTR        BEGSR
081900941215     C* SE ESISTE ALMENO UNA TARIFFA PER IL CLIENTE ANCHE SE SCADUTA
082000941215     C*  LO SEGNALO
082100941215     C     CTV(1)        IFNE      *BLANKS
082200050127     C                   MOVEL     'S'           olv59TKS
082300941215     C                   ENDIF
082400941215     C*
082500050127     C                   MOVE      ilv59ksc      WCCM              4 0
082600941104     C* RICERCA
082700050128     C     '?'           SCAN      olv59ctr                               30
082800941104     C*
082900941104    1C     *IN30         IFEQ      *ON
083000941104     C*
083100941104     C* SOLO SE CODIFICATO DELL'AREA POSSO FARE LA RICERCA
083200941104    2C     WCCM          IFEQ      8888
083300941104     C     WCCM          OREQ      9999
083400050127     C                   MOVEL     '1'           olv59ERR
083500050127     C                   MOVEL     ERR(1)        olv59MSG
083600941104     C                   GOTO      ENDCTR
083700941104    2C                   ENDIF
083800941104     C*
083900941104     C* RICERCA
084000941104     C                   CLEAR                   PARAM4
084100050127     C                   MOVEL     ilv59ksc      PA4COD
084200050127     C                   Z-ADD     ilv59kci      PA4CCC
084300941104     C                   MOVEL     PARAM4        KPJBU
084400941104     C                   CALL      'TNTA36R'
084500941104     C                   PARM                    KPJBA
084600941104     C                   MOVEL     KPJBU         PARAM4
084700941104     C*
084800941104    2C     PA4CTR        IFNE      *BLANKS
084900941104     C* SE TROVATO METTO QUELLO SCELTO
085000050128     C                   MOVE      PA4CTR        olv59CTR
085100941104   X2C                   ELSE
085200050128     c* Se non scelto lo abblanco
085300050128     C                   MOVEL     *blanks       olv59CTR
085400941104    2C                   END
085500050127     C                   MOVEL     'R'           olv59ERR
085600941104     C                   GOTO      ENDCTR
085700941104     C*
085800941104   X1C                   ELSE
085900941104     C*
086000941116     C* CONTROLLO FORMALE
086100050128     c                   eval      wcontrCTR=olv59CTR
086200941116     C                   EXSR      CTRFOR
086300941116     C*
086400050127     C     olv59ERR      IFNE      ' '
086500941104     C                   GOTO      ENDCTR
086600941104    2C                   ENDIF
086700941104     C*
086800941104     C* CONTROLLO ESISTENZA TARIFFA
086900941104    2C     CTV(1)        IFNE      *BLANKS
087000941104     C                   Z-ADD     1             B
087100050128     C     olv59CTR      LOOKUP    CTV(B)                                 30
087200050128     C   30              MOVEL     PGV(B)        olv59PRG
087300050128     C   30              MOVEL     DIV(B)        olv59DIV
087400060515    3C   30olv59DIV      IFEQ      '   '
087500050128     C                   MOVEL     'ITL'         olv59DIV
087600060515    3C                   ENDIF
087700060515     c
087800060515     c* Se non trovata tariffa e accetto 1 cod tariffa se bloccato, verifico
087900060515     c*  il network
088000060515    3c                   if        not *in30 and ilv59ta2='B'
088100060515     C                   Z-ADD     1             B
088200060515     C     olv59CTR      LOOKUP    CTcli(b)                               30
088300060515     c*
088400060515    4c                   if        *in30
088500060515    5c                   if        ficli(b)=ilv59fie              or
088600060515     c                             ilv59fie='M' and mdcli(b)='M'  or
088700060515     c                             ilv59fie='N' and mdcli(b)='N'  or
088800060515     c                             ilv59fie='S' and ficli(b)<>'I' or
088900060515     c                             ilv59fie='J' and ficli(b)<>'E'
089000060515     c* Network corrisponde: controllo se bloccata
089100060515    6c                   if        bacli(b)='B' and vacli(b)<>'A'
089200060516     c* se ok imposto i campi del progressino e della divisa
089300060516     C                   MOVEL     PGcli(b)      olv59PRG
089400060516     C                   MOVEL     DIcli(b)      olv59DIV
089500060516    3C     olv59DIV      IFEQ      '   '
089600060516     C                   MOVEL     'ITL'         olv59DIV
089700060516    3C                   ENDIF
089800060515     c                   else
089900060515     c                   setoff                                       30
090000060515    6c                   endif
090100060515   x5c                   else
090200060515     c                   setoff                                       30
090300060515    5c                   endif
090400060515    4c                   endif
090500060515    3c                   endif
090600060515     c
090700060515     c
090800941104     C* INESISTENTE: ERRORE
090900060516    3C**N30CTV(1)        IFNE      *BLANKS
091000060516    c
091100060516     c                   if        not *in30
091200050128     C                   MOVEL     '3'           olv59ERR
091300050128     C                   MOVEL     ERR(3)        olv59MSG
091400941104     C                   GOTO      ENDCTR
091500941104    3C                   END
091600060511     c*
091700060511   x2c                   else
091800060516     c*
091900060511     c* Se non esistono tariffe valide, il cod tariffa impostato non
092000060511     c*  può nemmeno essere uno presente ma non valido con questo
092100060511     c*  richiamo (ad esempio se carico tariffe italia, non può
092200060511     c*  essere un cod tariffa per l'estero)
092300060516     c                   eval      Errore=' '
092400060516     c                   z-add     1             b
092500060516     C     olv59CTR      LOOKUP    CTcli(b)                               30
092600060516     c
092700060516    3c                   if        *in30
092800060516    4c                   if        ficli(b)=ilv59fie              or
092900060516     c                             ilv59fie='M' and mdcli(b)='M'  or
093000060516     c                             ilv59fie='N' and mdcli(b)='N'  or
093100060516     c                             ilv59fie='S' and ficli(b)<>'I' or
093200060516     c                             ilv59fie='J' and ficli(b)<>'E'
093300060516     c* Network corrisponde: controllo se bloccata
093400060516    5c                   if        bacli(b)='B' and ilv59ta2='B'
093500060516     c                             and vacli(b)<>'A'
093600060516     c* se ok imposto i campi del progressino e della divisa
093700060516     C                   MOVEL     PGcli(b)      olv59PRG
093800060516     C                   MOVEL     DIcli(b)      olv59DIV
093900060516    3C     olv59DIV      IFEQ      '   '
094000060516     C                   MOVEL     'ITL'         olv59DIV
094100060516    3C                   ENDIF
094200060516     c                   else
094300060516     c                   eval      errore='S'
094400060516    5c                   endif
094500060516   x4c                   else
094600060516     c                   eval      errore='S'
094700060516    4c                   endif
094800060516    3c                   endif
094900060511     c
095000060516    3c                   if        Errore='S'
095100060511     C                   MOVEL     '5'           olv59ERR
095200060516    4c                   select
095300060511     c                   when      ilv59tbo='P'
095400060511     C                   MOVEL     ERR(5)        olv59MSG
095500060511     c                   when      ilv59tbo='A'
095600060511     C                   MOVEL     ERR(6)        olv59MSG
095700060511     c                   other
095800060511     C                   MOVEL     ERR(7)        olv59MSG
095900060516    4c                   endsl
096000060511     c
096100060511     C                   GOTO      ENDCTR
096200060516    3c                   endif
096300060511     c
096400060516    2C                   END
096500941104    1C                   END
096600941104     C     ENDCTR        ENDSR
096700941107     C*
096800941107     C* CONTROLLO FORMALE CODICE TARIFFA -----------------------------*
096900941107     C     CTRFOR        BEGSR
097000941107     C*
097100050128     C                   MOVEL     wcontrCTR     UNOCTR            1
097200941107     C                   Z-ADD     1             C
097300941107     C     UNOCTR        LOOKUP    CTA(C)                                 30
097400941107     C*
097500050128    1c     *IN30         IFEQ      *OFF
097600941118     C     UNOCTR        OREQ      ' '
097700050127     C                   MOVEL     ERR(2)        olv59MSG
097800050127     C                   MOVEL     '2'           olv59ERR
097900050128    1C                   ENDIF
098000941107     C*
098100050127    1C     olv59ERR      IFEQ      *BLANKS
098200941107     C*
098300050127     C                   MOVEL     DTA(C)        olv59DFS
098400941107     C* CONTROLLO CHE LA 2 E 3 CIFRA SIANO UN NUERO
098500050128     C                   MOVE      wcontrCTR     W002A             2
098600941107     C                   MOVEL     W002A         W001A
098700941107     C*
098800941107    2C                   DO        2
098900941107    3C     W001A         IFNE      '0'
099000941107     C     W001A         ANDNE     '1'
099100941107     C     W001A         ANDNE     '2'
099200941107     C     W001A         ANDNE     '3'
099300941107     C     W001A         ANDNE     '4'
099400941107     C     W001A         ANDNE     '5'
099500941107     C     W001A         ANDNE     '6'
099600941107     C     W001A         ANDNE     '7'
099700941107     C     W001A         ANDNE     '8'
099800941107     C     W001A         ANDNE     '9'
099900050127     C                   MOVEL     ERR(4)        olv59MSG
100000050127     C                   MOVEL     '4'           olv59ERR
100100941107    3C                   ENDIF
100200941107     C*
100300941107     C                   MOVE      W002A         W001A
100400941107    2C                   ENDDO
100500941107    1C                   ENDIF
100600941107     C*
100700941107     C                   ENDSR
100800941104     C*
100900981006     C* SBR INIZIO ---------------------------------------------------*
101000941117     C     *INZSR        BEGSR
101100981006      *
101200981006      * ACCESSO   TABEL
101300981006      *
101400941104     C     KTAB3         KLIST
101500941104     C                   KFLD                    CODUT
101600941104     C                   KFLD                    COD               2
101700981006      *
101800941110     C                   Z-ADD     1             CODUT             1 0
101900981006     C***
102000941104     C* CARICO TABELLA TR : TIPI TARIFFA
102100941104     C***
102200981006     C                   OPEN      TABEL00F
102300981006      *
102400941104     C                   MOVEL     'TR'          COD
102500941104     C                   Z-ADD     1             C
102600941104     C     KTAB3         SETLL     TABEL
102700941104     C     KTAB3         READE     TABEL                                  30
102800941104     C*
102900941104     C     *IN30         DOWEQ     '0'
103000941104     C     TBLFLG        IFEQ      ' '
103100941104     C                   MOVEL     TBLKEY        CTA(C)
103200941104     C                   MOVEL     TBLUNI        DTA(C)
103300941104     C                   ADD       1             C
103400941104     C                   ENDIF
103500941104     C*
103600941104     C     KTAB3         READE     TABEL                                  30
103700941104     C                   ENDDO
103800941104     C*
103900941104     C                   CLOSE     TABEL00F
104000941117     C*
104100941117     C                   CLEAR                   WKSC
104200941117     C                   CLEAR                   WFIE
104300941117     C                   CLEAR                   WTBO
104400050127     c*
104500050127      * Carico tabella DFE
104600050127     c                   Clear                   DDfe
104700050127     c                   Clear                   Tibs02Ds
104800050127     c                   Eval      T02Mod = 'C'
104900050127     c                   Clear                   T02Sif
105000050127     c                   Eval      T02Cod = 'DFE'
105100050127     c                   Movel(p)  'FED'         T02Ke1
105200050127     c                   Call      'TIBS02R'
105300050127     c                   Parm                    Kpjba
105400050127     c                   Parm                    Tibs02Ds
105500050127     c                   If        T02err = *Blanks
105600050127     c                   Movel     T02Uni        DDfe
105700050127     c                   EndIf
105800941104     C*
105900941104     C                   ENDSR
106000941107     C*
106100941107     C* RIEMPO I CAMPI VIDEO-----------------------------------------*
106200941107     C     VISTAR        BEGSR
106300941107     C     CTV(1)        IFNE      *BLANKS
106400050127     C                   MOVEL     CTV(1)        OLV59CT2
106500050127     C                   MOVEL     SPV(1)        OLV59ES2
106600050127     C                   MOVE      Fiv(1)        OLV59ES2
106700941107     C* SE ESISTE ALMENO UNA TARIFFA PER IL CLIENTE ANCHE SE SCADUTA
106800941107     C*  LO SEGNALO
106900050127     C                   MOVEL     'S'           OLV59TKS
107000941107     C                   ENDIF
107100941107     C*
107200941107     C     CTV(2)        IFNE      *BLANKS
107300050127     C                   MOVEL     CTV(2)        OLV59CT3
107400050127     C                   MOVEL     SPV(2)        OLV59ES3
107500050127     C                   MOVE      Fiv(2)        OLV59ES3
107600941107     C                   ENDIF
107700941107     C     CTV(3)        IFNE      *BLANKS
107800050127     C                   MOVEL     CTV(3)        OLV59CT4
107900050127     C                   MOVEL     SPV(3)        OLV59ES4
108000050127     C                   MOVE      Fiv(3)        OLV59ES4
108100941107     C                   ENDIF
108200941107     C     CTV(4)        IFNE      *BLANKS
108300050127     C                   MOVEL     CTV(4)        OLV59CT5
108400050127     C                   MOVEL     SPV(4)        OLV59ES5
108500050127     C                   MOVE      Fiv(4)        OLV59ES5
108600941107     C                   ENDIF
108700050128     c
108800050128     c* Carico le schiere di output
108900050128     c                   if        ctv(1)<>*blanks
109000050128     c                   movea     ctv           olv59CTT
109100050128     c                   movea     pgv           olv59pgT
109200050128     c* Attributi
109300050128     c                   movea     fiv           olv59at1
109400050128     c                   movea     spv           olv59at2
109500050128     c                   movea     bav           olv59at3
109600050128     c                   movea     tpv           olv59at4
109700050128     c                   movea     vav           olv59at5
109800050128     c                   endif
109900050128     c
110000941107     C                   ENDSR
110100941118**
110200941104Per il codice cliente indicato e' impossibile eseguire la ricerca tariffe
110300941104Tipo Tariffa errato
110400060516Codice Tariffa inesistente o non utilizzabile per il cliente indicato
110500941118Codice tariffa errato
110600060511Codice tariffa esistente ma non utilizzabile in Partenza e/o x questo Network
110700060511Codice tariffa esistente ma non utilizzabile in Arrivo e/o x questo Network
110800060511Codice tariffa esistente ma bloccato o non utilizzabile per questo Network
