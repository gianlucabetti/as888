000100921021     H DECEDIT('0,') DATEDIT(*DMY.)
000200050126     H* FNLV59R  *---------------------------------------------------*
000300941110     H*     CARICAMENTO TARIFFE DI UN CLIENTE                         *
000400921021     H*--------------------------------------------------------------*
000500050127     FTNTAM01L  IF   E           K DISK
000600941110     FTABEL00F  IF   E           K DISK    USROPN
000700941104     D*
000800050127     D CTV             S              3    DIM(50)
000900160223     D** CTN             S              3    DIM(50)
001000050127     d ctw             s              3    Dim(50)
001100050218     d ctcli           s              3    Dim(50)
001200060503     d
001300050127     d
001400050127     D baV             S              1    DIM(50)
001500050127     D baVdop          S              1    DIM(50)
001600160223     D** ban             S              1    DIM(50)
001700050127     D baNdop          S              1    DIM(50)
001800050218     D bacli           S              1    DIM(50)
001900050127     d
002000050127     D vaV             S              1    DIM(50)
002100050218     D vacli           S              1    DIM(50)
002200050127     d
002300050127     D TPV             S              1    DIM(50)
002400160223     D** TPN             S              1    DIM(50)
002500050127     d tpw             s              1    Dim(50)
002600050218     d tpcli           s              1    Dim(50)
002700050127     d
002800050127     D SPV             S              1    DIM(50)
002900160223     D** SPN             S              1    DIM(50)
003000050127     d spw             s              1    Dim(50)
003100050218     d spcli           s              1    Dim(50)
003200050127     d
003300050127     D PGV             S              3    DIM(50)
003400160223     D** PGN             S              3    DIM(50)
003500050218     D PGcli           S              3    DIM(50)
003600050127     d
003700050127     D DIV             S              3    DIM(50)
003800160223     D** DIN             S              3    DIM(50)
003900050218     D DIcli           S              3    DIM(50)
004000050127     d
004100050127     D FIV             S              1    DIM(50)
004200160223     D** FIN             S              1    DIM(50)
004300160215     D FIW             S              1    DIM(50)
004400050218     D FIcli           S              1    DIM(50)
004500050127
004600050127     d mdv             s              1    Dim(50)
004700160223     d** mdn             s              1    Dim(50)
004800050218     d mdcli           s              1    Dim(50)
004900941104     D*
005000941107     D CTA             S              1    DIM(30)
005100941107     D DTA             S             10    DIM(30)
005200060511     D ERR             S             78    DIM(7) CTDATA PERRCD(1)
005300021115
005400021115     d Wctr            s              2  0
005500170517     d Wctr_n          s              3  0
005600060516     d Errore          s              1
005700170517     d wok             s              1
005800050124     d CNo             s                   like(C)
005900050128     d WcontrCTR       s                   like(Ilv59CTR)
006000050128     d Wblo            s                   like(Ilv59blo)
006100050128     d Wval            s                   like(Ilv59val)
006200050128     d Wta2            s                   like(Ilv59ta2)
006300050128     d Wta3            s                   like(Ilv59ta3)
006400050128     d Wta4            s                   like(Ilv59ta4)
006500050128     d Wta5            s                   like(Ilv59ta5)
006600941104      *
006700050127     D* DS PER FNLV59R - PER CARICAMENTO TARIFFE
006800050127     D fnlv59ds      E DS
006900021115
007000021115      * Ds per confronto dati tariffe FedEx
007100021115     d ddfe          e ds
007200021115     d Tibs02Ds      e ds
007300021115
007400990930     D DSTA01        E DS
007500941104     D* PASSO IN SCELTA COD TARIFFA DI TARIFFA/OFFERTA - TNTA36R -
007600941104     D PARAM4          DS
007700941104     D  PA4COD                 6     12  0
007800941104     D  PA4CTR                13     15
007900941104     D  PA4FLG                16     16
008000941104     D  PA4VPR                17     17
008100941104     D  PA4CCC                18     21  0
008200941104     D  PA4DCV                22     36
008300061220     d  pa4prg                37     39
008400061220     d  pa4dtr                40     47
008500941104     D KPJBA         E DS
008600941104     C*---------------------------------------------------------------*
008700941104     C     *ENTRY        PLIST
008800050128     C                   PARM                    fnlv59ds
008900941104     C*---------------------------------------------------------------*
009000050128     C* TIPO LANCIO, ilv59TLA "C" -           CHIUSO CON LR
009100050128     C* TIPO LANCIO, ilv59TLA "L" - ELABORO E CHIUDO CON LR
009200050128     C* TIPO LANCIO, ilv59TLA " " - ELABORO E CHIUDO IN RETRN
009300941104     C*
009400050128    1C     ilv59tla      IFNE      'C'
009500941107     C*
009600941107     C* PULIZIA CAMPI DI OUTPUT
009700050127     C                   CLEAR                   olv59mSG
009800050127     C                   CLEAR                   olv59ERR
009900050127     C                   CLEAR                   olv59DFS
010000050127     C                   CLEAR                   olv59PRG
010100050127     C                   CLEAR                   olv59DIV
010200050128     C                   CLEAR                   olv59CTR
010300050128     c
010400050128     C                   MOVEL     *BLANKS       olv59CT2
010500050128     C                   MOVEL     *BLANKS       olv59CT3
010600050128     C                   MOVEL     *BLANKS       olv59CT4
010700050128     C                   MOVEL     *BLANKS       olv59CT5
010800050128     C                   MOVEL     *BLANKS       olv59ES2
010900050128     C                   MOVEL     *BLANKS       olv59ES3
011000050128     C                   MOVEL     *BLANKS       olv59ES4
011100050128     C                   MOVEL     *BLANKS       olv59ES5
011200050128     c
011300050128     C                   MOVEL     *BLANKS       olv59CTT
011400050128     C                   MOVEL     *BLANKS       olv59pgT
011500050128     C                   MOVEL     *BLANKS       olv59AT1
011600050128     C                   MOVEL     *BLANKS       olv59AT2
011700050128     C                   MOVEL     *BLANKS       olv59AT3
011800050128     C                   MOVEL     *BLANKS       olv59AT4
011900050128     C                   MOVEL     *BLANKS       olv59AT5
012000050128     C                   MOVEL     *BLANKS       olv59ATt
012100941107     C*
012200941117     C* SOLO CONTROLLO FORMALE
012300050128    2C     ilv59CFO      IFEQ      'S'
012400050128     c                   eval      wcontrCTR=Ilv59CTR
012500941117     C                   EXSR      CTRFOR
012600941117     C*
012700941117   X2C                   ELSE
012800941117     C*
012900160223     c* se modoficato: KSC  e DSP --> rileggo e ricarico tutte le tariffe
013000050127    3C     ilv59ksc      IFNE      WKSC
013100160223     C     ilv59dsp      ORNE      WDSP
013200160223     c                   exsr      CARCTR
013300160223    3c                   endif
013400160223     c
013500160223     c* negli altri casi devo solo rileggere e ricaricare dalle sckiere
013600160223     c* escludo 8888 e 9999
013700160223    3C     ilv59ksc      IFNE      WKSC
013800160223     C     ilv59dsp      ORNE      WDSP
013900050127     C     ilv59fie      ORNE      WFIE
014000050127     C     ilv59tbo      ORNE      WTBO
014100050128     C     ilv59blo      ORNE      Wblo
014200050128     C     ilv59VAL      ORNE      WVAL
014300060515     C**** ilv59ta2      ORNE      Wta2
014400160223     C**** ilv59ta3      ORNE      Wta3
014500160428     C**** ilv59ta4      ORNE      Wta4
014600050128     C     ilv59ta5      ORNE      Wta5
014700160223     c* non lo faccio per 8888/9999
014800160223     c                   if        wccm<>8888 and wccm<>9999
014900160223     C                   EXSR      SELCTR_ntw
015000160223     c                   endif
015100160223     c
015200941231     C                   ELSE
015300941231     C                   EXSR      VISTAR
015400941231    3C                   ENDIF
015500160223    c
015600050127     C                   MOVEL     ilv59ksc      WKSC              7 0
015700050127     C                   MOVEL     ilv59dsp      WDSP              8 0
015800050127     C                   MOVEL     ilv59fie      WFIE              1
015900050127     C                   MOVEL     ilv59tbo      WTBO              1
016000050128     C                   MOVEL     ilv59blo      WBLO
016100050128     C                   MOVEL     ilv59val      WVAL
016200060515     C****               MOVEL     ilv59ta2      WTA2
016300160223     C****               MOVEL     ilv59ta3      WTA3
016400160428     C****               MOVEL     ilv59ta4      WTA4
016500050128     C                   MOVEL     ilv59ta5      WTA5
016600941104     C*
016700050127    3C     ilv59ric      IFEQ      'S'
016800941104     C                   EXSR      RICTAR
016900160428    3C                   else
017000160428
017100160428     c* Se Ilv59TA4 è impostato, verifico se trovo tariffa SOLO
017200160428     c* per il tipo servizio
017300160428     c     ilv59ta4      ifeq      'S'
017400160428     c                   exsr      RICTAR_TSP
017500160428     c                   endif
017600160428     c                   endif
017700941104     C*
017800050127    3C     ilv59con      IFEQ      'S'
017900050128     c                   if        olv59ctr=*blanks
018000050128     c                   eval      olv59ctr=ilv59ctr
018100050128     c                   endif
018200050128     c
018300941104     C                   EXSR      CTRCTR
018400941215    3C                   ENDIF
018500941215     C*
018600050128     C* DECODIFICA FISSA TARIFFA se non ancora messa
018700050127     C     olv59CTR      IFNE      *BLANKS
018800050202     C     olv59dfs      andEQ     *BLANKS
018900941215     C                   Z-ADD     1             C
019000050127     C                   MOVEL     olv59ctr      UNOCTR            1
019100941215     C     UNOCTR        LOOKUP    CTA(C)                                 30
019200050127     C   30              MOVEL     DTA(C)        olv59dfs
019300941215     C                   ENDIF
019400941215     C*
019500941117    2C                   ENDIF
019600941107     C*
019700941104    1C                   ENDIF
019800941104     C* CHIUSURA PGM
019900050128    1C     ilv59TLA      IFEQ      ' '
020000941104     C                   RETURN
020100941104     C                   ELSE
020200941104     C                   SETON                                        LR
020300941104    1C                   ENDIF
020400050218     c*
020500050218     C* CARICO LE TARIFFE DEL CLIENTE --------------------------------*
020600050218     C     CARCTR        BEGSR
020700050218     C*
020800160225     C* PULISCO tutte le SKIERE DELLE TARIFFE
020900050218     c                   Clear                   Ctcli
021000050218     c                   Clear                   Tpcli
021100050218     c                   Clear                   Spcli
021200050218     C                   CLEAR                   PGcli
021300050218     C                   CLEAR                   DIcli
021400050218     C                   CLEAR                   FIcli
021500050218     c                   Clear                   mdcli
021600050218     c                   Clear                   bacli
021700050218     c                   Clear                   vacli
021800160225     C*
021900160225     C                   CLEAR                   CTV
022000160225     c                   Clear                   Ctw
022100160225     C                   CLEAR                   TPV
022200160225     c                   Clear                   Tpw
022300160225     C                   CLEAR                   SPV
022400160225     c                   Clear                   Spw
022500160225     C                   CLEAR                   PGV
022600160225     C                   CLEAR                   DIV
022700160225     C                   CLEAR                   FIV
022800160225     C                   CLEAR                   FIw
022900160225     c                   Clear                   mdv
023000160225     c                   Clear                   bav
023100160225     c                   Clear                   bavdop
023200160225     c                   Clear                   vav
023300160225     c                   Clear                   w_val             1
023400160225     c                   Clear                   TIPOVAL           1
023500050218     C*
023600050218     C                   Z-ADD     0             B                 3 0
023700050218     C                   Z-ADD     0             C                 3 0
023800050218     C                   Z-ADD     0             D                 3 0
023900050218     C*
024000050218     C* SE 8888 O 9999 NON CARICO NULLA
024100050218     C                   MOVE      ilv59KSC      WCCM              4 0
024200050218     C*
024300050218    1C     WCCM          IFNE      8888
024400050218     C     WCCM          ANDNE     9999
024500050218     C*
024600050218     C     ilv59ksc      SETLL     TNTAM000
024700050218     C     ilv59ksc      READE     TNTAM000                               30
024800050218     C*
024900050218    2C     *IN30         DOWEQ     *OFF
025000050218     C* ESCLUDO SE ANNULLATO
025100050218    3C     TAMATB        IFNE      'A'
025200050218     c
025300050218     C                   MOVEL     TAMFLO        DSTA01
025400050218     C*
025500050218     c                   Move      TamCtr        Wctr
025600050218      * Se Tamfie "M" Solo Tariffe merci FedEx
025700160223    5c**                 If        ilv59Fie = 'M' and Wctr >= §DfeDalD
025800160223     c**                 GoTo      Notartutto
025900160223    5c**                 EndIf
026000050218      * Se Tamfie "N" Solo Tariffe documenti FedEx
026100160223    5c**                 If        ilv59Fie = 'N' and Wctr < §DfeDalD
026200160223     c**                 GoTo      Notartutto
026300160223    5c**                 EndIf
026400160223     c**
026500050218     c* Elaboro solo tariffe decorrenti o scadute (escludo da decorrere
026600050218     c*  come se non ci fossero)
026700060503    5c***                if        ilv59dsp>=tamddt
026800060503     c* 03/5/06 ES --> preferisco caricare tutto. Se un tariffa ancora
026900060503     c*                da decorrere è già stata immessa, meglio
027000060503     c*                non farla usare in modo improprio, salvo
027100060503     c*                che non sia anche presente come decorrente:allor
027200060503     c*                la ignoro: meglio la decorrente
027300050218     c
027400050218     C                   MOVEL     TAMCTR        COMCT2            3
027500050218     C                   Z-ADD     1             D
027600050218     C     COMCT2        LOOKUP    CTcli(D)                               31
027700060503    6c                   if        (*in31 and ilv59dsp>=tamddt) or
027800050218     c                             (not *in31 and c < 50)
027900050218     C*
028000160223     C* DELLE TARIFFE già presenti, tengo sempre l'ultima, escluse le da
028100060511     c*  decorrere
028200050218    7C     *IN31         IFEQ      *OFF
028300050218     C                   ADD       1             C
028400050218     C                   MOVEL     TAMCTR        CTcli(C)
028500050218     C                   Z-ADD     C             D
028600050218    7C                   ENDIF
028700050218     C*
028800050218     C* VEDO SE E' TARIFFA decorrente
028900060503     c                   select
029000160223    7C     ilv59dsp      whenge    TAMDDT
029100050218     C     ilv59dsp      ANDLE     TAMDST
029200060503     c* Decorrente
029300050218     c                   movel     'D'           vacli(d)
029400060503    7C     ilv59dsp      whenlT    TAMDDT
029500060503     c* dA decorrere
029600060503     c                   movel     'A'           vacli(d)
029700060503     c
029800060503     c                   other
029900060503     c* Scaduta
030000050218     c                   movel     'S'           vacli(d)
030100060511    7c                   endsl
030200050218     C*
030300050218     C                   MOVEL     TAMCTR        CTcli(d)
030400050218     C                   MOVEL     TAMTSP        SPcli(d)
030500050218     C                   MOVE      TAMFTP        TPcli(d)
030600050218     C                   MOVE      TAMPRG        PGcli(d)
030700050218     C                   MOVE      §TADIV        DIcli(d)
030800050218     C                   MOVE      TAMFIE        FIcli(d)
030900050218     C                   MOVE      TAMbap        bacli(d)
031000050218     C* TARIFFA DPD
031100050218     C     §TADPD        IFEQ      'S'
031200050218     C                   MOVE      'D'           FIcli(d)
031300050218     C                   ENDIF
031400050218     C* TARIFFA FEDEX
031500050218    7C     §TAFED        IFEQ      'S'
031600050218     C                   MOVE      'F'           FIcli(d)
031700050218     c                   Clear                   mdcli(d)
031800160223     c******             If        ilv59Fie = 'M' and Wctr >= §DfeDalM
031900160223     c                   If        Wctr >= §DfeDalM
032000050218     c                                            and Wctr <= §DfeAlM
032100050218     c                   Eval      mdcli(d) = 'M'
032200050218     c                   EndIf
032300160223     c********           If        ilv59Fie = 'N' and Wctr >= §DfeDalD
032400160223     c                   If        Wctr >= §DfeDalD
032500050218     c                                            and Wctr <= §DfeAlD
032600050218     c                   Eval      mdcli(d) = 'N'
032700050218     c                   EndIf
032800050218    7c                   EndIf
032900050218    6C                   ENDIF
033000060511    5C*********          ENDIF
033100060511    4C*********          ENDIF
033200050218    3C                   ENDIF
033300050218
033400060511     c     Notartutto    Tag
033500050218
033600050218     C*
033700050218     C  N30ilv59ksc      READE     TNTAM000                               30
033800050218     C*
033900050218    2C                   ENDDO
034000160223     c
034100050218    1c                   endif
034200050218     c
034300050218     C                   ENDSR
034400160223     C*
034500160223     C* SELEZIONO le tariffe del cliente in base a quanto passato in DS ---*
034600160223     C     SELCTR_ntw    BEGSR
034700160223     C* PULISCO SKIERE DELLE TARIFFE
034800160223     C                   CLEAR                   CTV
034900160223     c                   Clear                   Ctw
035000160223     C                   CLEAR                   TPV
035100160223     c                   Clear                   Tpw
035200160223     C                   CLEAR                   SPV
035300160223     c                   Clear                   Spw
035400160223     C                   CLEAR                   PGV
035500160223     C                   CLEAR                   DIV
035600160223     C                   CLEAR                   FIV
035700160223     C                   CLEAR                   FIw
035800160223     c                   Clear                   mdv
035900160223     c                   Clear                   bav
036000160223     c                   Clear                   bavdop
036100160223     c                   Clear                   vav
036200160223     c                   Clear                   w_val             1
036300160223     c                   Clear                   TIPOVAL           1
036400160223     C                   MOVEL     *BLANKS       olv59TKS
036500160223     c* rileggo le schiere e carico in base ai parametri passati
036600160223     c                   z-add     1             xx                4 0
036700160223     c                   clear                   B
036800160223     c
036900160223     c* 1) ELABORO PRIMA LE DECORRENTI
037000160223     C*     solo se richiesto
037100160223    6c                   if        ilv59VAL=' ' or
037200160223     c                             ilv59VAL='D'
037300160223     C                   EVAL      TIPOVAL='D'
037400160223     C                   EXSR      CARICA_CTR
037500160223     C                   ENDIF
037600160223
037700160223     C* 2) POI ELABORO LE SCADUTE
037800160223     C*     solo se richiesto
037900160223    6c                   if        ilv59VAL=' ' or
038000160223     c                             ilv59VAL='S'
038100160223     c                   z-add     1             xx
038200160223     c* caricata almeno una valida
038300160223     c                   if        b>0
038400160223     c                   eval      w_val='S'
038500160223     c                   endif
038600160223     c
038700160223     C                   EVAL      TIPOVAL='S'
038800160223     C                   EXSR      CARICA_CTR
038900160223     C                   ENDIF
039000160223
039100160223     c                   exsr      VISTAR
039200160223     c                   ENDSR
039300160223     C*
039400160223     C* -------------------------------------------------------------------*
039500160223     C     CARICA_CTR    BEGSR
039600160223
039700160223    1c                   dow       ctcli(xx)<>*blanks and B<50
039800160223     c* carico il tipo di validità richiesto
039900160223    2c                   if        vacli(xx) =TIPOVAL
040000160223
040100160223     C* ilv59BLO:  B=solo bloccate " "=solo non bloccate  S=tutte
040200160223    3c                   if         ilv59BLO='S' or
040300160223     c                             (ilv59BLO='B' and bacli(xx)='B') or
040400160223     c                             (ilv59BLO=' ' and bacli(xx)<>'B')
040500160223
040600160223     C* CARICO SOLO TARIFFE ITALIA           --> FIE "I"
040700160223     C* CARICO SOLO TARIFFE Euroexpress      --> FIE "E"
040800160223     C* CARICO SOLO TARIFFE DPD              --> FIE "D"
040900160223     C* CARICO SOLO TARIFFE FEDEX            --> FIE "F"
041000160223     C* CARICO      TARIFFE DPD+FEDEX+EUROEX --> FIE "S"
041100160223     C* CARICO      TARIFFE DPD+FEDEX+ITALIA --> FIE "J"
041200160223     C* CARICO SOLO TARIFFE FEDEX DOCUMENTI  --> FIE "N"
041300160223     C* CARICO SOLO TARIFFE FEDEX MERCI      --> FIE "M"
041400160223     C* CARICO      TARIFFE DPD+EUROEX       --> FIE "X"
041500160223    4c                   if        ficli(xx)=' ' or
041600160223     c                             ficli(xx)=ilv59fie or
041700160223     c                             (ilv59fie='X' and ficli(XX)='D') or
041800160223     c                             (ilv59fie='X' and ficli(XX)='E') or
041900160223     c                             (ilv59fie='S' and ficli(xx)<>'I') or
042000160223     c                             (ilv59fie='J' and ficli(xx)<>'E') or
042100160223     c                             (ilv59fie='M' and mdcli(xx)= 'M') or
042200160223     c                             (ilv59fie='N' and mdcli(xx)= 'N')
042300160223     c
042400160223     C* ilv59TBO P=carico tariffe P/B/" "
042500160223     c*          A=carico tariffe A/B/" "   " "=tutte
042600160223    5c                   if        ilv59tbo=' ' or
042700160223     c                             bacli(xx)=' ' or
042800160223     c                             bacli(xx)='B' or
042900160223     c                             ilv59tbo=bacli(xx)
043000160223     c
043100160223     C                   ADD       1             B
043200160223     C                   MOVEL     ctcli(xx)     CTV(B)
043300160223     C                   MOVEL     spcli(xx)     SPV(B)
043400160223     c* se caricata almeno una valida non tengo conto della tariffa Prefer
043500160223     c*  per le non valide
043600160223     c                   if        w_val='S'
043700160223     c                   clear                   TPV(B)
043800160223     c                   else
043900160223     C                   MOVE      tpcli(XX)     TPV(B)
044000160223     c                   endif
044100160223     C                   MOVE      pgcli(xx)     PGV(B)
044200160223     C                   MOVE      dicli(xx)     DIV(B)
044300160223     C                   MOVE      ficli(xx)     FIV(B)
044400160223     C                   MOVE      bacli(xx)     BAV(B)
044500160223     c
044600160223     c* Carico il campo bap anche mettendo al posto di " " --> "E"
044700160223     c                   if        bacli(XX)=' ' or bacli(xx)='B'
044800160223     c                   movel     'E'           bavdop(b)
044900160223     c                   else
045000160223     C                   MOVE      bacli(xx)     BAVdop(B)
045100160223     c                   endif
045200160223     c* Validità tariffa
045300160223     C                   MOVE      vacli(xx)     VAV(B)
045400160223     c
045500160223     c* Fedex merci/documenti
045600160223     c                   if        ficli(XX)='F'
045700160223     c                   move      mdcli(XX)     MDV(b)
045800160223     c                   endif
045900160223     c
046000160223    5c                   endif
046100160223    4c                   endif
046200160223    3c                   endif
046300160223    2c                   endif
046400160223     c
046500160223     c                   add       1             xx
046600160223    1c                   enddo
046700160223     c
046800160223     C                   ENDSR
046900941104     C*
047000050218     C* SELEZIONO le tariffe del cliente in base a quanto passato in DS ---*
047100160223     C**   SELCTR        BEGSR
047200160223     C**
047300941104     C* PULISCO SKIERE DELLE TARIFFE
047400160223     C**                 CLEAR                   CTV
047500160223     C**                 CLEAR                   CTN
047600160223     c**                 Clear                   Ctw
047700160223     C**                 CLEAR                   TPN
047800160223     C**                 CLEAR                   TPV
047900160223     c**                 Clear                   Tpw
048000160223     C**                 CLEAR                   SPN
048100160223     C**                 CLEAR                   SPV
048200160223     c**                 Clear                   Spw
048300160223     C**                 CLEAR                   PGV
048400160223     C**                 CLEAR                   PGN
048500160223     C**                 CLEAR                   DIV
048600160223     C**                 CLEAR                   DIN
048700160223     C**                 CLEAR                   FIV
048800160223     C**                 CLEAR                   FIN
048900160223     C**                 CLEAR                   FIw
049000160223     c**                 Clear                   mdv
049100160223     c**                 Clear                   mdn
049200160223     c**                 Clear                   bav
049300160223     c**                 Clear                   ban
049400160223     c**                 Clear                   bavdop
049500160223     c**                 Clear                   bandop
049600160223     c**                 Clear                   vav
049700160223     C**
049800160223     C**                 MOVEL     *BLANKS       olv59TKS
049900160223     C**
050000160223     C**MITTENTI CODIFICATI --> CERCO LE TARIFFE
050100160223     C**                 Z-ADD     0             B                 3 0
050200160223     C**                 Z-ADD     0             C                 3 0
050300160223     C**                 Z-ADD     0             D                 3 0
050400160223     C**
050500160223     C**SE 8888 O 9999 NON CARICO NULLA
050600160223     C**                 MOVE      ilv59KSC      WCCM              4 0
050700160223     C**
050800160223    1C**   WCCM          IFNE      8888
050900160223     C**   WCCM          ANDNE     9999
051000160223     C**
051100160223     C**   ilv59ksc      SETLL     TNTAM000
051200160223     C**   ilv59ksc      READE     TNTAM000                               30
051300160223     C**
051400160223    2C**   *IN30         DOWEQ     *OFF
051500160223     C**ESCLUDO SE ANNULLATO
051600160223    3C**   TAMATB        IFNE      'A'
051700160223     c**
051800160223     C**                 MOVEL     TAMFLO        DSTA01
051900160223     C**
052000160223    5C**   TAMFIE        IFEQ      ' '
052100160223     C**
052200160223     C**   TAMFIE        OREQ      ilv59fie
052300160223     C**   §TADPD        ANDEQ     ' '
052400160223     C**   §TAFED        ANDEQ     ' '
052500160223     C**SE TARIFFA DPD NON DEVO CARICARE  SE RICHIESTO I/E/F/M/N
052600160223     C**   §TADPD        OREQ      'S'
052700160223     C**   ilv59fie      ANDNE     'I'
052800160223     C**   ilv59fie      ANDNE     'E'
052900160223     C**   ilv59fie      ANDNE     'F'
053000160223     c**   ilv59fie      andne     'M'
053100160223     c**   ilv59fie      andne     'N'
053200160223     C**
053300160223     C**SE TARIFFA FEDEX NON DEVO CARICARE  SE RICHIESTO I/E/D/X
053400160223     C**   §TAFED        OREQ      'S'
053500160223     C**   ilv59fie      ANDNE     'I'
053600160223     C**   ilv59fie      ANDNE     'E'
053700160223     C**   ilv59fie      ANDNE     'D'
053800160223     C**   ilv59fie      ANDNE     'X'
053900160223     C**LE ESTERE LE DEVO CARICARE SOLO PER S (OLTRE CHE DPD E FEDEX)
054000160223     C**   TAMFIE        OREQ      'E'
054100160223     C**   ilv59fie      ANDEQ     'S'
054200160223     C**LE EEX    LE DEVO CARICARE anche per X
054300160223     C**   TAMFIE        OREQ      'E'
054400160223     C**   ilv59fie      ANDEQ     'X'
054500160223     C**   §TAFED        ANDEQ     ' '
054600160223     C**LE ITALIA LE DEVO CARICARE SOLO PER J (OLTRE CHE DPD E FEDEX)
054700160223     C**   TAMFIE        OREQ      'I'
054800160223     C**   ilv59fie      ANDEQ     'J'
054900160223     C**
055000160223     c**                 Move      TamCtr        Wctr
055100160223     C**Se Tamfie "M" Solo Tariffe merci FedEx
055200160223    6c**                 If        ilv59Fie = 'M' and Wctr >= §DfeDalD
055300160223     c**                 GoTo      Notar
055400160223    6c**                 EndIf
055500160223     C**Se Tamfie "N" Solo Tariffe documenti FedEx
055600160223    6c**                 If        ilv59Fie = 'N' and Wctr < §DfeDalD
055700160223     c**                 GoTo      Notar
055800160223    6c**                 EndIf
055900160223     C**
056000160223     C**ilv59TBO P=carico tariffe P/B/" "
056100160223     a**         A=carico tariffe A/B/" "   " "=tutte
056200160223    6C**   ilv59tbo      IFEQ      ' '
056300160223     C**   TAMBAP        OREQ      ' '
056400160223     C**   TAMBAP        OREQ      'B'
056500160223     C**   TAMBAP        OREQ      ilv59tbo
056600160223     C**
056700160223     C**VEDO SE E' TARIFFA decorrente
056800160223    7C**   ilv59dsp      IFGE      TAMDDT
056900160223     C**   ilv59dsp      ANDLE     TAMDST
057000160223     c**solo se richieste (ilv59VAL=D/" ")
057100160223    8c**                 if        ilv59VAL=' ' or ilv59val='D'
057200160223     C**
057300160223     C**                 ADD       1             B
057400160223     C**                 MOVEL     TAMCTR        CTV(B)
057500160223     C**                 MOVEL     TAMTSP        SPV(B)
057600160223     C**                 MOVE      TAMFTP        TPV(B)
057700160223     C**                 MOVE      TAMPRG        PGV(B)
057800160223     C**                 MOVE      §TADIV        DIV(B)
057900160223     C**                 MOVE      TAMFIE        FIV(B)
058000160223     C**                 MOVE      TAMbap        baV(B)
058100160223     c**Carico il campo bap anche mettendo al posto di " " --> "E"
058200160223    9c**                 if        tambap=' ' or tambap='B'
058300160223     c**                 movel     'E'           bavdop(b)
058400160223     c**                 else
058500160223     C**                 MOVE      TAMbap        baVdop(B)
058600160223    9c**                 endif
058700160223     C**                 MOVE      'D'           vaV(B)
058800160223     C**TARIFFA DPD
058900160223     C**   §TADPD        IFEQ      'S'
059000160223     C**                 MOVE      'D'           FIV(B)
059100160223     C**                 ENDIF
059200160223     C**TARIFFA FEDEX
059300160223    9C**   §TAFED        IFEQ      'S'
059400160223     C**                 MOVE      'F'           FIV(B)
059500160223     c**                 Clear                   mdv(b)
059600160223     c**                 If        ilv59Fie = 'M' and Wctr >= §DfeDalM
059700160223     c**                                          and Wctr <= §DfeAlM
059800160223     c**                 Eval      mdv(b) = 'M'
059900160223     c**                 EndIf
060000160223     c**                 If        ilv59Fie = 'N' and Wctr >= §DfeDalD
060100160223     c**                                          and Wctr <= §DfeAlD
060200160223     c**                 Eval      mdv(b) = 'N'
060300160223     c**                 EndIf
060400160223    9c**                 EndIf
060500160223     c**
060600160223     C**   B             COMP      50                                 30
060700160223   x8c**                 else
060800160223     c**Se tariffa decorrente e c'e' anche come scaduta la tolgo
060900160223     C**
061000160223     C**                 MOVEL     TAMCTR        COMCT2            3
061100160223     C**                 Z-ADD     1             D
061200160223     C**   COMCT2        LOOKUP    CTN(D)                                 31
061300160223    9c**                 if        *in31
061400160223     c**                 clear                   ctn(d)
061500160223     c**                 clear                   spn(d)
061600160223     c**                 clear                   tpn(d)
061700160223     c**                 clear                   pgn(d)
061800160223     c**                 clear                   din(d)
061900160223     c**                 clear                   fin(d)
062000160223     c**                 clear                   ban(d)
062100160223     c**                 clear                   bandop(d)
062200160223     c**                 clear                   mdn(d)
062300160223     c**Reimposto il contatore a -1
062400160223     c**                 sub       1             C
062500160223     c**
062600160223    9C**                 ENDIF
062700160223    8C**                 ENDIF
062800160223     C**
062900160223     C**TARIFFE non decorrente
063000160223   X7C**                 ELSE
063100160223     c**        tengo solo le scadute, se richieste (ilv59VAL=S/" ")
063200160223    8c**                 if        ilv59dsp>tamdst  and
063300160223     c**                           (ilv59VAL=' ' or ilv59VAL='S')
063400941104     C*
063500160223     C**                 MOVEL     TAMCTR        COMCT2            3
063600160223     C**                 Z-ADD     1             D
063700160223     C**   COMCT2        LOOKUP    CTN(D)                                 31
063800160223    9c**                 if        *in31   or
063900160223     c**                           (not *in31 and c < 50)
064000160223     C**
064100160223     C**DELLE TARIFFE NON VALIDE, TENGO SEMPRE L'ULTIMA
064200160223   10C**   *IN31         IFEQ      *OFF
064300160223     C**                 ADD       1             C
064400160223     C**                 MOVEL     TAMCTR        CTN(C)
064500160223     C**                 Z-ADD     C             D
064600160223   10C**                 ENDIF
064700160223     C**                 MOVEL     TAMTSP        SPN(D)
064800160223     C**                 MOVE      TAMFTP        TPN(D)
064900160223     C**                 MOVE      TAMPRG        PGN(D)
065000160223     C**                 MOVE      §TADIV        DIN(D)
065100160223     C**                 MOVE      TAMFIE        FIN(D)
065200160223     C**                 MOVE      TAMbap        ban(D)
065300160223     c**Carico il campo bap anche mettendo al posto di " " --> "E"
065400160223     c**                 if        tambap=' '   or  tambap='B'
065500160223     c**                 movel     'E'           bandop(D)
065600160223     c**                 else
065700160223     C**                 MOVE      TAMbap        bandop(D)
065800160223     c**                 endif
065900160223     C**TARIFFA DPD
066000160223     C**   §TADPD        IFEQ      'S'
066100160223     C**                 MOVE      'D'           FIN(D)
066200160223     C**                 ENDIF
066300160223     C**TARIFFA FEDEX
066400160223   10C**   §TAFED        IFEQ      'S'
066500160223     C**                 MOVE      'F'           FIN(D)
066600160223     c**                 Clear                   mdn(d)
066700160223     c**                 If        ilv59Fie = 'M' and Wctr >= §DfeDalM
066800160223     c**                                          and Wctr <= §DfeAlM
066900160223     c**                 Eval      mdn(d) = 'M'
067000160223     c**                 EndIf
067100160223     c**                 If        ilv59Fie = 'N' and Wctr >= §DfeDalD
067200160223     c**                                          and Wctr <= §DfeAlD
067300160223     c**                 Eval      mdn(d) = 'N'
067400160223     c**                 EndIf
067500160223   10C**                 ENDIF
067600160223    9C**                 ENDIF
067700160223     C**
067800160223    8C**                 ENDIF
067900160223    7C**                 ENDIF
068000160223    6C**                 ENDIF
068100160223    5C**                 ENDIF
068200160223    4C**                 ENDIF
068300160223    3C**                 ENDIF
068400160223     C**
068500160223     c**   Notar         Tag
068600160223     C**
068700160223     C**
068800160223     C**N30ilv59ksc      READE     TNTAM000                               30
068900160223     C**
069000160223    2C**                 ENDDO
069100160223     C**
069200160223     C**                 ADD       1             B
069300160223     C**
069400160223     C**CARICO TUTTI I TIPI TARIFFA TROVATI IN UNICA SCHIERA
069500160223     C**SE NON NE HO GIA' TROVATI 50 VALIDI
069600160223    2C**   B             IFLE      50
069700160223     C**
069800160223     C**
069900160223     C**SE ESISTE ALMENO UNA TARIFFA VALIDA, NON CONSIDERO LA 'P'
070000160223     C** DI PREFERENZIALE PER LE TARIFFE SCADUTE
070100160223    3C**   B             IFGT      1
070200160223     C**                 CLEAR                   TPN
070300160223    3C**                 ENDIF
070400160223     C**
070500160223     C**                 Z-ADD     1             C
070600160223     C**
070700160223    3C**   CTN(C)        DOWNE     *BLANKS
070800160223     C**   B             ANDLE     50
070900160223     C**
071000160223     C**   CTN(C)        LOOKUP    CTV                                    31
071100160223    4C**   *IN31         IFEQ      *OFF
071200160223     C**                 MOVEL     CTN(C)        CTV(B)
071300160223     C**                 MOVEL     TPN(C)        TPV(B)
071400160223     C**                 MOVEL     SPN(C)        SPV(B)
071500160223     C**                 MOVEL     PGN(C)        PGV(B)
071600160223     C**                 MOVEL     DIN(C)        DIV(B)
071700160223     C**                 MOVEL     FIN(C)        FIV(B)
071800160223     c**                 Movel     mdn(c)        mdv(b)
071900160223     c**                 Movel     ban(c)        bav(b)
072000160223     c**                 Movel     bandop(c)     bavdop(b)
072100160223     c**S-scaduta
072200160223     c**                 movel     'S'           vav(b)
072300160223     C**                 ADD       1             B
072400160223    4C**                 ENDIF
072500160223     C**
072600160223     C**                 ADD       1             C
072700160223    3C**                 ENDdo
072800160223    2C**                 ENDif
072900160223     C**
073000160223     C**                 EXSR      VISTAR
073100160223    1C**                 ENDif
073200160223     C**
073300160223     C**                 ENDSR
073400941104     C*
073500160428     C* RICERCA E CARICAMENTO A VIDEO TARIFFE solo col tipo serivizio-*
073600160428     C     RICTAR_TSP    BEGSR
073700160428     c
073800160428     C                   SETOFF                                       30
073900160428     C                   CLEAR                   W001A
074000160428     c                   clear                   CNo
074100160428     C*
074200160428     C* SE NON CI SONO TARIFFE IMPOSTO LA TARIFFA 000
074300160428    1C     CTV(1)        IFEQ      *BLANKS
074400160428     C                   CLEAR                   olv59tks
074500160428     c
074600160428    1C     ilv59ctr      IFEQ      *BLANKS
074700170517     c* eseguo un loop per proporre un cod tariffa valido ma NON
074800170517     c*  esistente per altri netwokr
074900170517     C                   CLEAR                   WCTR_N            3 0
075000170517     c                   eval      wok='N'
075100170517     c                   dow       wok='N'  AND WCTR_N<15
075200170517     C                   MOVEL     wctr_n        olv59CTR
075300170517     C                   IF        %LOOKUP(olv59ctr:CTCLI)>0
075400170517     C                   ADD       1             WCTR_N
075500170517     c                   else
075600170517     c                   eval      wok='S'
075700170517     C                   ENDIF
075800170517     c                   enddo
075900170517     C
076000170517     C***                MOVEL     *ZEROS        olv59CTR
076100160428    1C                   ENDIF
076200170517    C
076300160428     C*
076400160428   X1C                   ELSE
076500160428      * Carico le sk di work per la scelta
076600160428     c                   ExSr      CarSk
076700160428     C*
076800160428     C* 1) SE IMPOSTATO IL TIPO SERVIZIO --> CERCO QUELLO
076900160428     c*    solo per E e H (non lo faccio per C e D)
077000160428     c                   exsr      Cerca_tsp
077100160428     C*
077200160428     c* Se trovato -->IMPOSTO CAMPO CODICE TARIFFA
077300160428    2c                   if        *in30
077400160428     C                   MOVEL     CTw(C)        Olv59CTR
077500160428     c                   else
077600160428     c* imposto quello di input
077700160428     c                   eval      olv59ctr=ilv59ctr
077800160428    2c                   endif
077900160428     c
078000160428     c                   exsr      DATI
078100160428    1c                   endif
078200160428     C*
078300160428     c                   ENDSR
078400160428     c
078500941104     C* RICERCA E CARICAMENTO A VIDEO TARIFFE ------------------------*
078600941104     C     RICTAR        BEGSR
078700941104     C                   SETOFF                                       30
078800950125     C                   CLEAR                   W001A
078900050124     c                   clear                   CNo
079000941104     C*
079100941104     C* SE NON CI SONO TARIFFE IMPOSTO LA TARIFFA 000
079200050127    1C     CTV(1)        IFEQ      *BLANKS
079300050127     C                   CLEAR                   olv59tks
079400941215     C*
079500050127     C     ilv59ctr      IFEQ      *BLANKS
079600170517     C***                MOVEL     *ZEROS        olv59CTR
079700170517     c
079800170517     C                   CLEAR                   WCTR_N            3 0
079900170517     c                   eval      wok='N'
080000170517     c                   dow       wok='N'  AND WCTR_N<15
080100170517     C                   MOVEL     wctr_n        olv59CTR
080200170517     C                   IF        %LOOKUP(olv59ctr:CTCLI)>0
080300170517     C                   ADD       1             WCTR_N
080400170517     c                   else
080500170517     c                   eval      wok='S'
080600170517     C                   ENDIF
080700170517     c                   enddo
080800941215     C                   ENDIF
080900941215     C*
081000050127   X1C                   ELSE
081100941107     C*
081200941107     C* CODICE TARIFFA IMPOSTATO: PRENDO IL RELATIVO PROGRESSIVO
081300050127    2C     ilv59CTR      IFNE      *BLANKS
081400050127     c                   eval      olv59ctr=ilv59ctr
081500050127    2C                   ENDIF
081600941107     C**
081700050127     c**
081800050127    2C     ilv59CTR      IFEQ      *BLANKS
081900021115
082000050127      * Carico le sk di work per la scelta
082100021115     c                   ExSr      CarSk
082200941104     C*
082300160215     C* 1) SE IMPOSTATO IL TIPO SERVIZIO --> CERCO QUELLO
082400160401     c*    slo per E e H (non lo faccio per C e D)
082500160428     c                   exsr      Cerca_tsp
082600160428
082700941104     C*
082800941104     C* SE NON TROVATA O NON IMPOSTATO IL TIPO SERVIZIO -->
082900160215     c* 2) cerco per network  se passato
083000160215     c*    solo se ho caricato più di un network, altrimenti ignoro
083100160215    3c                   if        not *in30 and ilv59TA3<>' '
083200160215     c                             and (ilv59fie='X' or ilv59fie='S' or
083300160215     c                             ilv59fie='J')
083400160215     C                   CLEAR                   W001A
083500160215     c                   clear                   CNo
083600160215     C                   Z-ADD     1             C
083700160215     C                   SETON                                        31
083800160215     C*
083900160215    4C     *IN31         DOWEQ     *ON
084000160215     C     ilv59TA3      LOOKUP    fiw(C)                                 31
084100160215     C* SE TROVATA ED E' PREFERENZIALE ESCO DAL CICLO
084200160215    5c   31              if        tpw(C)='P'
084300160215     C                   SETON                                        30
084400160215     C                   SETOFF                                       31
084500160215     C                   MOVEL     '1'           W001A             1
084600160215   X5C                   ELSE
084700160215     c
084800160215     C* SE TROVATA MA NON PREFERENZIALE ACCENDO COMUNQUE L'INDICATORE
084900160215     C*  DI TROVATO MA FACCIO UN ALTRO GIRO
085000160215     C                   SETON                                        30
085100160215     c                   if        CNo=0
085200160215     c                   z-add     C             CNo
085300160215     c                   endif
085400160215     c
085500160215     C                   ADD       1             C
085600160215    5C                   ENDIF
085700160215    4C                   ENDDO
085800160215     C*
085900160215     C* SE HO TROVATO LA TARIFFA MA NON LA PREFERENZIALE, LA RICERCO per
086000160215     C*  PRENDERE LA POSIZIONE
086100160215    4C   30W001A         IFEQ      ' '
086200160215     c                   z-add     CNo           C
086300160215    4C                   ENDIF
086400160215     C*
086500160215    3C                   ENDIF
086600160215
086700160215     C* SE NON ancora TROVATA -->
086800160215     C* 3) CERCO LA PRIMA PREFERENZIALE
086900050124    3C     *IN30         IFEQ      *OFF
087000050127     C                   Z-ADD     1             C
087100050127     C     'P'           LOOKUP    TPw(C)                                 30
087200050127    3C                   ENDIF
087300941104     C*
087400160215     C* 4) SE NON ESISTE LA PREFERENZIALE METTO LA PRIMA VALIDA TROVATA
087500941104     C  N30              Z-ADD     1             C
087600050127     c
087700050127     c* IMPOSTO CAMPO CODICE TARIFFA
087800050127     C                   MOVEL     CTw(C)        Olv59CTR
087900050127    2c                   endif
088000160428     c
088100160428     c                   exsr      DATI
088200160428     c
088300050127    1C                   ENDIF
088400941104     C*
088500941104     C                   ENDSR
088600160428      *--------------------------------------------------------------*
088700160428      * Ricerca tariffa per tipo servizio
088800160428      *--------------------------------------------------------------*
088900160428     c     Cerca_tsp     BEGSR
089000160428    3C     ilv59TSP      IFNE      *BLANKS
089100160428     c     ilv59TSP      andne     'C'
089200160428     c     ilv59TSP      andne     'D'
089300160428     C* CERCO SE C'E' LA PREFERENZIALE ALTRIMENTI LA 1 CHE TROVO
089400160428     C                   Z-ADD     1             C
089500160428     C                   SETON                                        31
089600160428     C*
089700160428    4C     *IN31         DOWEQ     *ON
089800160428     C     ilv59tSP      LOOKUP    SPw(C)                                 31
089900160428     C* SE TROVATA ED E' PREFERENZIALE ESCO DAL CICLO
090000160428    5c   31              if        tpw(C)='P'
090100160428     C                   SETON                                        30
090200160428     C                   SETOFF                                       31
090300160428     C                   MOVEL     '1'           W001A             1
090400160428   X5C                   ELSE
090500160428     C* SE TROVATA MA NON PREFERENZIALE ACCENDO COMUNQUE L'INDICATORE
090600160428     C*  DI TROVATO MA FACCIO UN ALTRO GIRO
090700160428     C                   SETON                                        30
090800160428     c                   if        CNo=0
090900160428     c                   z-add     C             CNo
091000160428     c                   endif
091100160428     c
091200160428     C                   ADD       1             C
091300160428    5C                   ENDIF
091400160428    4C                   ENDDO
091500160428     C*
091600160428     C* SE HO TROVATO LA TARIFFA MA NON LA PREFERENZIALE, LA RICERCO X
091700160428     C*  PRENDERE LA POSIZIONE
091800160428    4C   30W001A         IFEQ      ' '
091900160428     c                   z-add     CNo           C
092000160428    4C                   ENDIF
092100160428     C*
092200160428    3C                   ENDIF
092300160428     c                   ENDSR
092400160428      *--------------------------------------------------------------*
092500160428      * Finisco di caricare i dati relativi alla tariffa
092600160428      *--------------------------------------------------------------*
092700160428     c     DATI          BEGSR
092800160428     C*
092900160428      * Cerco la posizione nella sk valida
093000160428     c                   Z-Add     1             c
093100160428     c     olv59CTR      Lookup    Ctv(c)                                 30
093200160428     c
093300160428    2c                   IF        *IN30
093400160428     C                   MOVEL     PGV(C)        OLV59PRG
093500160428     C                   MOVEL     DIV(C)        OLV59DIV
093600160428     C     OLV59DIV      IFEQ      '   '
093700160428     C                   MOVEL     'ITL'         OLV59DIV
093800160428     C                   ENDIF
093900160428
094000160428   x2C                   ELSE
094100160428     C* ERRORE --> COD.TARIFFA NON TROVATO (se passato in input può
094200160428     c*            accadere)
094300160428     C                   MOVEL     '3'           olv59ERR
094400160428     C                   MOVEL     ERR(3)        olv59MSG
094500160428     C                   Z-ADD     10            C
094600160428    2C                   ENDIF
094700160428     C*
094800160428     C                   EXSR      VISTAR
094900160428     C*
095000160428     C* RIMPIAZZO CON COD TARIFFA 5 SOLO SE ESISTE
095100160428    2C     CTv(5)        IFNE      *BLANKS
095200160428     C* SE NELLA 1°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
095300160428    3C     C             IFEQ      1
095400160428     C                   MOVEL     CTv(5)        olv59CT2
095500160428     C                   MOVEL     SPv(5)        olv59ES2
095600160428     C                   MOVE      FIv(5)        olv59ES2
095700160428    3C                   END
095800160428     C* SE NELLA 2°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
095900160428    3C     C             IFEQ      2
096000160428     C                   MOVEL     CTv(5)        olv59CT3
096100160428     C                   MOVEL     SPv(5)        olv59ES3
096200160428     C                   MOVE      FIv(5)        olv59ES3
096300160428    3C                   END
096400160428     C* SE NELLA 3°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
096500160428    3C     C             IFEQ      3
096600160428     C                   MOVEL     CTv(5)        olv59CT4
096700160428     C                   MOVEL     SPv(5)        olv59ES4
096800160428     C                   MOVE      FIv(5)        olv59ES4
096900160428    3C                   END
097000160428     C* SE NELLA 4°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
097100160428    3C     C             IFEQ      4
097200160428     C                   MOVEL     CTv(5)        olv59CT5
097300160428     C                   MOVEL     SPv(5)        olv59ES5
097400160428     C                   MOVE      FIv(5)        olv59ES5
097500160428    3C                   END
097600160428     C*
097700160428    2C                   END
097800160428     c                   ENDSR
097900021115      *--------------------------------------------------------------*
098000050127      * Carico le sk di work per la proposta a video
098100021115      *--------------------------------------------------------------*
098200021115     c     Carsk         BegSr
098300021115
098400050127     c* Se richieste tariffe FEDEX e presenti sia M che N --> propon
098500050127     c*  go "N"-documenti
098600050127     c                   setoff                                       3536
098700050127     c                   setoff                                       3738
098800050127     c                   If        ilv59Fie = 'F'
098900021115     c     'M'           Lookup    mdv                                    35
099000021115     c     'N'           Lookup    mdv                                    36
099100021115     c                   EndIf
099200050127     c
099300050127     c* Se presenti sia tariffe "P" che "A" --> propongo la P o la A
099400050127     c*    in base alla richiesta (ilv59PRP)
099500050128    1c                   if        ilv59tbo=' '  and ilv59PRP<>' '
099600050127     c     'E'           Lookup    bavdop                                 37
099700050128    2c                   if        *in37
099800050127     c                   seton                                        38
099900050128   x2c                   else
100000050127     c     'P'           Lookup    bavdop                                 37
100100050127     c     'A'           Lookup    bavdop                                 38
100200050128    2c                   endif
100300050128    1c                   endif
100400021115
100500021115     c                   Clear                   d
100600021115
100700050128    1c                   Do        50            b
100800021115     c                   If        Ctv(b) = *Blanks
100900021115     c                   Leave
101000021115     c                   EndIf
101100050127     c* per fedex tengo la documenti se presenti entrambe
101200050127     c                   If        ilv59Fie = 'F' and *In35 and *In36
101300021115     c                             and Mdv(b) <> 'N'
101400021115     c                   Iter
101500021115     c                   EndIf
101600050127     c
101700050127     c* Per la proposta impostata e presenti entrambe tariffe (p e A)
101800050127     c*  carico solo la proposta scelta
101900050127     c                   if        ilv59tbo=' ' and ilv59prp<>' ' and
102000050127     c                             *in37 and *in38
102100050127     c                   if        ilv59prp='P' and bavdop(b)='A'
102200050127     c                   iter
102300050127     c                   endif
102400050127     c                   if        ilv59prp='A' and bavdop(b)='P'
102500050127     c                   iter
102600050127     c                   endif
102700050127     c
102800050127     c                   endif
102900021115
103000021115     c                   Add       1             d
103100021115
103200021115     c                   Eval      Ctw(d) = Ctv(b)
103300021115     c                   Eval      Tpw(d) = Tpv(b)
103400021115     c                   Eval      Spw(d) = Spv(b)
103500160215     c                   Eval      fiw(d) = fiv(b)
103600050128    1c                   EndDo
103700021115
103800021115     c                   EndSr
103900941104     C*
104000941104     C* CONTROLLO CODICE TARIFFA ------------------------------------*
104100941104     C     CTRCTR        BEGSR
104200941215     C* SE ESISTE ALMENO UNA TARIFFA PER IL CLIENTE ANCHE SE SCADUTA
104300941215     C*  LO SEGNALO
104400941215     C     CTV(1)        IFNE      *BLANKS
104500050127     C                   MOVEL     'S'           olv59TKS
104600941215     C                   ENDIF
104700941215     C*
104800050127     C                   MOVE      ilv59ksc      WCCM              4 0
104900941104     C* RICERCA
105000050128     C     '?'           SCAN      olv59ctr                               30
105100941104     C*
105200941104    1C     *IN30         IFEQ      *ON
105300941104     C*
105400941104     C* SOLO SE CODIFICATO DELL'AREA POSSO FARE LA RICERCA
105500941104    2C     WCCM          IFEQ      8888
105600941104     C     WCCM          OREQ      9999
105700050127     C                   MOVEL     '1'           olv59ERR
105800050127     C                   MOVEL     ERR(1)        olv59MSG
105900941104     C                   GOTO      ENDCTR
106000941104    2C                   ENDIF
106100941104     C*
106200941104     C* RICERCA
106300941104     C                   CLEAR                   PARAM4
106400050127     C                   MOVEL     ilv59ksc      PA4COD
106500050127     C                   Z-ADD     ilv59kci      PA4CCC
106600061220     c                   movel     '2'           pa4vpr
106700061220     c                   move      ilv59dsp      pa4dtr
106800941104     C                   MOVEL     PARAM4        KPJBU
106900941104     C                   CALL      'TNTA36R'
107000941104     C                   PARM                    KPJBA
107100941104     C                   MOVEL     KPJBU         PARAM4
107200941104     C*
107300941104    2C     PA4CTR        IFNE      *BLANKS
107400941104     C* SE TROVATO METTO QUELLO SCELTO
107500050128     C                   MOVE      PA4CTR        olv59CTR
107600941104   X2C                   ELSE
107700050128     c* Se non scelto lo abblanco
107800050128     C                   MOVEL     *blanks       olv59CTR
107900941104    2C                   END
108000050127     C                   MOVEL     'R'           olv59ERR
108100941104     C                   GOTO      ENDCTR
108200941104     C*
108300941104   X1C                   ELSE
108400941104     C*
108500941116     C* CONTROLLO FORMALE
108600050128     c                   eval      wcontrCTR=olv59CTR
108700941116     C                   EXSR      CTRFOR
108800941116     C*
108900050127     C     olv59ERR      IFNE      ' '
109000941104     C                   GOTO      ENDCTR
109100941104    2C                   ENDIF
109200941104     C*
109300941104     C* CONTROLLO ESISTENZA TARIFFA
109400941104    2C     CTV(1)        IFNE      *BLANKS
109500941104     C                   Z-ADD     1             B
109600050128     C     olv59CTR      LOOKUP    CTV(B)                                 30
109700050128     C   30              MOVEL     PGV(B)        olv59PRG
109800050128     C   30              MOVEL     DIV(B)        olv59DIV
109900060515    3C   30olv59DIV      IFEQ      '   '
110000050128     C                   MOVEL     'ITL'         olv59DIV
110100060515    3C                   ENDIF
110200060515     c
110300060515     c* Se non trovata tariffa e accetto 1 cod tariffa se bloccato, verifico
110400060515     c*  il network
110500060515    3c                   if        not *in30 and ilv59ta2='B'
110600060515     C                   Z-ADD     1             B
110700060515     C     olv59CTR      LOOKUP    CTcli(b)                               30
110800060515     c*
110900060515    4c                   if        *in30
111000060515    5c                   if        ficli(b)=ilv59fie              or
111100060515     c                             ilv59fie='M' and mdcli(b)='M'  or
111200060515     c                             ilv59fie='N' and mdcli(b)='N'  or
111300060515     c                             ilv59fie='S' and ficli(b)<>'I' or
111400160215     c                             ilv59fie='J' and ficli(b)<>'E' or
111500160215     c                             ilv59fie='X' and (ficli(b)='E' or
111600160215     c                             ficli(b)='D')
111700060515     c* Network corrisponde: controllo se bloccata
111800060515    6c                   if        bacli(b)='B' and vacli(b)<>'A'
111900060516     c* se ok imposto i campi del progressino e della divisa
112000060516     C                   MOVEL     PGcli(b)      olv59PRG
112100060516     C                   MOVEL     DIcli(b)      olv59DIV
112200060516    3C     olv59DIV      IFEQ      '   '
112300060516     C                   MOVEL     'ITL'         olv59DIV
112400060516    3C                   ENDIF
112500060515     c                   else
112600060515     c                   setoff                                       30
112700060515    6c                   endif
112800060515   x5c                   else
112900060515     c                   setoff                                       30
113000060515    5c                   endif
113100060515    4c                   endif
113200060515    3c                   endif
113300060515     c
113400060515     c
113500941104     C* INESISTENTE: ERRORE
113600060516    3C**N30CTV(1)        IFNE      *BLANKS
113700060516    c
113800060516     c                   if        not *in30
113900050128     C                   MOVEL     '3'           olv59ERR
114000050128     C                   MOVEL     ERR(3)        olv59MSG
114100941104     C                   GOTO      ENDCTR
114200941104    3C                   END
114300060511     c*
114400060511   x2c                   else
114500060516     c*
114600060511     c* Se non esistono tariffe valide, il cod tariffa impostato non
114700060511     c*  può nemmeno essere uno presente ma non valido con questo
114800060511     c*  richiamo (ad esempio se carico tariffe italia, non può
114900060511     c*  essere un cod tariffa per l'estero)
115000060516     c                   eval      Errore=' '
115100060516     c                   z-add     1             b
115200060516     C     olv59CTR      LOOKUP    CTcli(b)                               30
115300060516     c
115400060516    3c                   if        *in30
115500060516    4c                   if        ficli(b)=ilv59fie              or
115600060516     c                             ilv59fie='M' and mdcli(b)='M'  or
115700060516     c                             ilv59fie='N' and mdcli(b)='N'  or
115800060516     c                             ilv59fie='S' and ficli(b)<>'I' or
115900160215     c                             ilv59fie='J' and ficli(b)<>'E' or
116000160215     c                             ilv59fie='X' and (ficli(b)='E' or
116100160215     c                             ficli(b)='D')
116200060516     c* Network corrisponde: controllo se bloccata
116300060516    5c                   if        bacli(b)='B' and ilv59ta2='B'
116400060516     c                             and vacli(b)<>'A'
116500060516     c* se ok imposto i campi del progressino e della divisa
116600060516     C                   MOVEL     PGcli(b)      olv59PRG
116700060516     C                   MOVEL     DIcli(b)      olv59DIV
116800060516    3C     olv59DIV      IFEQ      '   '
116900060516     C                   MOVEL     'ITL'         olv59DIV
117000060516    3C                   ENDIF
117100060516     c                   else
117200060516     c                   eval      errore='S'
117300060516    5c                   endif
117400060516   x4c                   else
117500060516     c                   eval      errore='S'
117600060516    4c                   endif
117700060516    3c                   endif
117800060511     c
117900060516    3c                   if        Errore='S'
118000060511     C                   MOVEL     '5'           olv59ERR
118100060516    4c                   select
118200060511     c                   when      ilv59tbo='P'
118300060511     C                   MOVEL     ERR(5)        olv59MSG
118400060511     c                   when      ilv59tbo='A'
118500060511     C                   MOVEL     ERR(6)        olv59MSG
118600060511     c                   other
118700060511     C                   MOVEL     ERR(7)        olv59MSG
118800060516    4c                   endsl
118900060511     c
119000060511     C                   GOTO      ENDCTR
119100060516    3c                   endif
119200060511     c
119300060516    2C                   END
119400941104    1C                   END
119500941104     C     ENDCTR        ENDSR
119600941107     C*
119700941107     C* CONTROLLO FORMALE CODICE TARIFFA -----------------------------*
119800941107     C     CTRFOR        BEGSR
119900941107     C*
120000050128     C                   MOVEL     wcontrCTR     UNOCTR            1
120100941107     C                   Z-ADD     1             C
120200941107     C     UNOCTR        LOOKUP    CTA(C)                                 30
120300941107     C*
120400050128    1c     *IN30         IFEQ      *OFF
120500941118     C     UNOCTR        OREQ      ' '
120600050127     C                   MOVEL     ERR(2)        olv59MSG
120700050127     C                   MOVEL     '2'           olv59ERR
120800050128    1C                   ENDIF
120900941107     C*
121000050127    1C     olv59ERR      IFEQ      *BLANKS
121100941107     C*
121200050127     C                   MOVEL     DTA(C)        olv59DFS
121300941107     C* CONTROLLO CHE LA 2 E 3 CIFRA SIANO UN NUERO
121400050128     C                   MOVE      wcontrCTR     W002A             2
121500941107     C                   MOVEL     W002A         W001A
121600941107     C*
121700941107    2C                   DO        2
121800941107    3C     W001A         IFNE      '0'
121900941107     C     W001A         ANDNE     '1'
122000941107     C     W001A         ANDNE     '2'
122100941107     C     W001A         ANDNE     '3'
122200941107     C     W001A         ANDNE     '4'
122300941107     C     W001A         ANDNE     '5'
122400941107     C     W001A         ANDNE     '6'
122500941107     C     W001A         ANDNE     '7'
122600941107     C     W001A         ANDNE     '8'
122700941107     C     W001A         ANDNE     '9'
122800050127     C                   MOVEL     ERR(4)        olv59MSG
122900050127     C                   MOVEL     '4'           olv59ERR
123000941107    3C                   ENDIF
123100941107     C*
123200941107     C                   MOVE      W002A         W001A
123300941107    2C                   ENDDO
123400941107    1C                   ENDIF
123500941107     C*
123600941107     C                   ENDSR
123700941104     C*
123800981006     C* SBR INIZIO ---------------------------------------------------*
123900941117     C     *INZSR        BEGSR
124000981006      *
124100981006      * ACCESSO   TABEL
124200981006      *
124300941104     C     KTAB3         KLIST
124400941104     C                   KFLD                    CODUT
124500941104     C                   KFLD                    COD               2
124600981006      *
124700941110     C                   Z-ADD     1             CODUT             1 0
124800981006     C***
124900941104     C* CARICO TABELLA TR : TIPI TARIFFA
125000941104     C***
125100981006     C                   OPEN      TABEL00F
125200981006      *
125300941104     C                   MOVEL     'TR'          COD
125400941104     C                   Z-ADD     1             C
125500941104     C     KTAB3         SETLL     TABEL
125600941104     C     KTAB3         READE     TABEL                                  30
125700941104     C*
125800941104     C     *IN30         DOWEQ     '0'
125900941104     C     TBLFLG        IFEQ      ' '
126000941104     C                   MOVEL     TBLKEY        CTA(C)
126100941104     C                   MOVEL     TBLUNI        DTA(C)
126200941104     C                   ADD       1             C
126300941104     C                   ENDIF
126400941104     C*
126500941104     C     KTAB3         READE     TABEL                                  30
126600941104     C                   ENDDO
126700941104     C*
126800941104     C                   CLOSE     TABEL00F
126900941117     C*
127000941117     C                   CLEAR                   WKSC
127100941117     C                   CLEAR                   WFIE
127200941117     C                   CLEAR                   WTBO
127300050127     c*
127400050127      * Carico tabella DFE
127500050127     c                   Clear                   DDfe
127600050127     c                   Clear                   Tibs02Ds
127700050127     c                   Eval      T02Mod = 'C'
127800050127     c                   Clear                   T02Sif
127900050127     c                   Eval      T02Cod = 'DFE'
128000050127     c                   Movel(p)  'FED'         T02Ke1
128100050127     c                   Call      'TIBS02R'
128200050127     c                   Parm                    Kpjba
128300050127     c                   Parm                    Tibs02Ds
128400050127     c                   If        T02err = *Blanks
128500050127     c                   Movel     T02Uni        DDfe
128600050127     c                   EndIf
128700941104     C*
128800941104     C                   ENDSR
128900941107     C*
129000941107     C* RIEMPO I CAMPI VIDEO-----------------------------------------*
129100941107     C     VISTAR        BEGSR
129200941107     C     CTV(1)        IFNE      *BLANKS
129300050127     C                   MOVEL     CTV(1)        OLV59CT2
129400050127     C                   MOVEL     SPV(1)        OLV59ES2
129500050127     C                   MOVE      Fiv(1)        OLV59ES2
129600941107     C* SE ESISTE ALMENO UNA TARIFFA PER IL CLIENTE ANCHE SE SCADUTA
129700941107     C*  LO SEGNALO
129800050127     C                   MOVEL     'S'           OLV59TKS
129900941107     C                   ENDIF
130000941107     C*
130100941107     C     CTV(2)        IFNE      *BLANKS
130200050127     C                   MOVEL     CTV(2)        OLV59CT3
130300050127     C                   MOVEL     SPV(2)        OLV59ES3
130400050127     C                   MOVE      Fiv(2)        OLV59ES3
130500941107     C                   ENDIF
130600941107     C     CTV(3)        IFNE      *BLANKS
130700050127     C                   MOVEL     CTV(3)        OLV59CT4
130800050127     C                   MOVEL     SPV(3)        OLV59ES4
130900050127     C                   MOVE      Fiv(3)        OLV59ES4
131000941107     C                   ENDIF
131100941107     C     CTV(4)        IFNE      *BLANKS
131200050127     C                   MOVEL     CTV(4)        OLV59CT5
131300050127     C                   MOVEL     SPV(4)        OLV59ES5
131400050127     C                   MOVE      Fiv(4)        OLV59ES5
131500941107     C                   ENDIF
131600050128     c
131700050128     c* Carico le schiere di output
131800050128     c                   if        ctv(1)<>*blanks
131900050128     c                   movea     ctv           olv59CTT
132000050128     c                   movea     pgv           olv59pgT
132100050128     c* Attributi
132200050128     c                   movea     fiv           olv59at1
132300050128     c                   movea     spv           olv59at2
132400050128     c                   movea     bav           olv59at3
132500050128     c                   movea     tpv           olv59at4
132600050128     c                   movea     vav           olv59at5
132700050128     c                   endif
132800050128     c
132900941107     C                   ENDSR
133000941118**
133100941104Per il codice cliente indicato e' impossibile eseguire la ricerca tariffe
133200941104Tipo Tariffa errato
133300060516Codice Tariffa inesistente o non utilizzabile per il cliente indicato
133400941118Codice tariffa errato
133500060511Codice tariffa esistente ma non utilizzabile in Partenza e/o x questo Network
133600060511Codice tariffa esistente ma non utilizzabile in Arrivo e/o x questo Network
133700060511Codice tariffa esistente ma bloccato o non utilizzabile per questo Network
