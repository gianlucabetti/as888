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
004900170622
005000170622     D edV             S              1    DIM(50)
005100170622     D edcli           S              1    DIM(50)
005200941104     D*
005300941107     D CTA             S              1    DIM(30)
005400941107     D DTA             S             10    DIM(30)
005500060511     D ERR             S             78    DIM(7) CTDATA PERRCD(1)
005600021115
005700021115     d Wctr            s              2  0
005800170517     d Wctr_n          s              3  0
005900060516     d Errore          s              1
006000170517     d wok             s              1
006100050124     d CNo             s                   like(C)
006200050128     d WcontrCTR       s                   like(Ilv59CTR)
006300050128     d Wblo            s                   like(Ilv59blo)
006400050128     d Wval            s                   like(Ilv59val)
006500050128     d Wta2            s                   like(Ilv59ta2)
006600050128     d Wta3            s                   like(Ilv59ta3)
006700050128     d Wta4            s                   like(Ilv59ta4)
006800050128     d Wta5            s                   like(Ilv59ta5)
006900941104      *
007000050127     D* DS PER FNLV59R - PER CARICAMENTO TARIFFE
007100050127     D fnlv59ds      E DS
007200021115
007300021115      * Ds per confronto dati tariffe FedEx
007400021115     d ddfe          e ds
007500021115     d Tibs02Ds      e ds
007600021115
007700990930     D DSTA01        E DS
007800941104     D* PASSO IN SCELTA COD TARIFFA DI TARIFFA/OFFERTA - TNTA36R -
007900941104     D PARAM4          DS
008000941104     D  PA4COD                 6     12  0
008100941104     D  PA4CTR                13     15
008200941104     D  PA4FLG                16     16
008300941104     D  PA4VPR                17     17
008400941104     D  PA4CCC                18     21  0
008500941104     D  PA4DCV                22     36
008600061220     d  pa4prg                37     39
008700061220     d  pa4dtr                40     47
008800941104     D KPJBA         E DS
008900941104     C*---------------------------------------------------------------*
009000941104     C     *ENTRY        PLIST
009100050128     C                   PARM                    fnlv59ds
009200941104     C*---------------------------------------------------------------*
009300050128     C* TIPO LANCIO, ilv59TLA "C" -           CHIUSO CON LR
009400050128     C* TIPO LANCIO, ilv59TLA "L" - ELABORO E CHIUDO CON LR
009500050128     C* TIPO LANCIO, ilv59TLA " " - ELABORO E CHIUDO IN RETRN
009600941104     C*
009700050128    1C     ilv59tla      IFNE      'C'
009800941107     C*
009900941107     C* PULIZIA CAMPI DI OUTPUT
010000050127     C                   CLEAR                   olv59mSG
010100050127     C                   CLEAR                   olv59ERR
010200050127     C                   CLEAR                   olv59DFS
010300050127     C                   CLEAR                   olv59PRG
010400050127     C                   CLEAR                   olv59DIV
010500050128     C                   CLEAR                   olv59CTR
010600050128     c
010700050128     C                   MOVEL     *BLANKS       olv59CT2
010800050128     C                   MOVEL     *BLANKS       olv59CT3
010900050128     C                   MOVEL     *BLANKS       olv59CT4
011000050128     C                   MOVEL     *BLANKS       olv59CT5
011100050128     C                   MOVEL     *BLANKS       olv59ES2
011200050128     C                   MOVEL     *BLANKS       olv59ES3
011300050128     C                   MOVEL     *BLANKS       olv59ES4
011400050128     C                   MOVEL     *BLANKS       olv59ES5
011500050128     c
011600050128     C                   MOVEL     *BLANKS       olv59CTT
011700050128     C                   MOVEL     *BLANKS       olv59pgT
011800050128     C                   MOVEL     *BLANKS       olv59AT1
011900050128     C                   MOVEL     *BLANKS       olv59AT2
012000050128     C                   MOVEL     *BLANKS       olv59AT3
012100050128     C                   MOVEL     *BLANKS       olv59AT4
012200050128     C                   MOVEL     *BLANKS       olv59AT5
012300050128     C                   MOVEL     *BLANKS       olv59ATt
012400941107     C*
012500941117     C* SOLO CONTROLLO FORMALE
012600050128    2C     ilv59CFO      IFEQ      'S'
012700050128     c                   eval      wcontrCTR=Ilv59CTR
012800941117     C                   EXSR      CTRFOR
012900941117     C*
013000941117   X2C                   ELSE
013100941117     C*
013200170622     c* se modificato: KSC  e DSP --> rileggo e ricarico tutte le tariffe
013300170622     c*  o tariffe solo web o tutte
013400050127    3C     ilv59ksc      IFNE      WKSC
013500160223     C     ilv59dsp      ORNE      WDSP
013600160223     c                   exsr      CARCTR
013700160223    3c                   endif
013800160223     c
013900160223     c* negli altri casi devo solo rileggere e ricaricare dalle sckiere
014000160223     c* escludo 8888 e 9999
014100160223    3C     ilv59ksc      IFNE      WKSC
014200160223     C     ilv59dsp      ORNE      WDSP
014300050127     C     ilv59fie      ORNE      WFIE
014400050127     C     ilv59tbo      ORNE      WTBO
014500050128     C     ilv59blo      ORNE      Wblo
014600050128     C     ilv59VAL      ORNE      WVAL
014700060515     C**** ilv59ta2      ORNE      Wta2
014800160223     C**** ilv59ta3      ORNE      Wta3
014900160428     C**** ilv59ta4      ORNE      Wta4
015000170622     C     ilv59ta5      ORNE      Wta5
015100160223     c* non lo faccio per 8888/9999
015200160223     c                   if        wccm<>8888 and wccm<>9999
015300160223     C                   EXSR      SELCTR_ntw
015400160223     c                   endif
015500160223     c
015600941231     C                   ELSE
015700941231     C                   EXSR      VISTAR
015800941231    3C                   ENDIF
015900160223    c
016000050127     C                   MOVEL     ilv59ksc      WKSC              7 0
016100050127     C                   MOVEL     ilv59dsp      WDSP              8 0
016200050127     C                   MOVEL     ilv59fie      WFIE              1
016300050127     C                   MOVEL     ilv59tbo      WTBO              1
016400050128     C                   MOVEL     ilv59blo      WBLO
016500050128     C                   MOVEL     ilv59val      WVAL
016600060515     C****               MOVEL     ilv59ta2      WTA2
016700160223     C****               MOVEL     ilv59ta3      WTA3
016800160428     C****               MOVEL     ilv59ta4      WTA4
016900050128     C                   MOVEL     ilv59ta5      WTA5
017000941104     C*
017100050127    3C     ilv59ric      IFEQ      'S'
017200941104     C                   EXSR      RICTAR
017300160428    3C                   else
017400160428
017500160428     c* Se Ilv59TA4 è impostato, verifico se trovo tariffa SOLO
017600160428     c* per il tipo servizio
017700160428     c     ilv59ta4      ifeq      'S'
017800160428     c                   exsr      RICTAR_TSP
017900160428     c                   endif
018000160428     c                   endif
018100941104     C*
018200050127    3C     ilv59con      IFEQ      'S'
018300050128     c                   if        olv59ctr=*blanks
018400050128     c                   eval      olv59ctr=ilv59ctr
018500050128     c                   endif
018600050128     c
018700941104     C                   EXSR      CTRCTR
018800941215    3C                   ENDIF
018900941215     C*
019000050128     C* DECODIFICA FISSA TARIFFA se non ancora messa
019100050127     C     olv59CTR      IFNE      *BLANKS
019200050202     C     olv59dfs      andEQ     *BLANKS
019300941215     C                   Z-ADD     1             C
019400050127     C                   MOVEL     olv59ctr      UNOCTR            1
019500941215     C     UNOCTR        LOOKUP    CTA(C)                                 30
019600050127     C   30              MOVEL     DTA(C)        olv59dfs
019700941215     C                   ENDIF
019800941215     C*
019900941117    2C                   ENDIF
020000941107     C*
020100941104    1C                   ENDIF
020200941104     C* CHIUSURA PGM
020300050128    1C     ilv59TLA      IFEQ      ' '
020400941104     C                   RETURN
020500941104     C                   ELSE
020600941104     C                   SETON                                        LR
020700941104    1C                   ENDIF
020800050218     c*
020900050218     C* CARICO LE TARIFFE DEL CLIENTE --------------------------------*
021000050218     C     CARCTR        BEGSR
021100050218     C*
021200160225     C* PULISCO tutte le SKIERE DELLE TARIFFE
021300050218     c                   Clear                   Ctcli
021400050218     c                   Clear                   Tpcli
021500050218     c                   Clear                   Spcli
021600050218     C                   CLEAR                   PGcli
021700050218     C                   CLEAR                   DIcli
021800050218     C                   CLEAR                   FIcli
021900050218     c                   Clear                   mdcli
022000050218     c                   Clear                   bacli
022100170622     c                   Clear                   edcli
022200050218     c                   Clear                   vacli
022300160225     C*
022400160225     C                   CLEAR                   CTV
022500160225     c                   Clear                   Ctw
022600160225     C                   CLEAR                   TPV
022700160225     c                   Clear                   Tpw
022800160225     C                   CLEAR                   SPV
022900160225     c                   Clear                   Spw
023000160225     C                   CLEAR                   PGV
023100160225     C                   CLEAR                   DIV
023200160225     C                   CLEAR                   FIV
023300160225     C                   CLEAR                   FIw
023400170622     C                   CLEAR                   edV
023500160225     c                   Clear                   mdv
023600160225     c                   Clear                   bav
023700160225     c                   Clear                   bavdop
023800160225     c                   Clear                   vav
023900160225     c                   Clear                   w_val             1
024000160225     c                   Clear                   TIPOVAL           1
024100050218     C*
024200050218     C                   Z-ADD     0             B                 3 0
024300050218     C                   Z-ADD     0             C                 3 0
024400050218     C                   Z-ADD     0             D                 3 0
024500050218     C*
024600050218     C* SE 8888 O 9999 NON CARICO NULLA
024700050218     C                   MOVE      ilv59KSC      WCCM              4 0
024800050218     C*
024900050218    1C     WCCM          IFNE      8888
025000050218     C     WCCM          ANDNE     9999
025100050218     C*
025200050218     C     ilv59ksc      SETLL     TNTAM000
025300050218     C     ilv59ksc      READE     TNTAM000                               30
025400050218     C*
025500050218    2C     *IN30         DOWEQ     *OFF
025600050218     C* ESCLUDO SE ANNULLATO
025700050218    3C     TAMATB        IFNE      'A'
025800050218     c
025900050218     C                   MOVEL     TAMFLO        DSTA01
026000050218     C*
026100050218     c                   Move      TamCtr        Wctr
026200050218      * Se Tamfie "M" Solo Tariffe merci FedEx
026300160223    5c**                 If        ilv59Fie = 'M' and Wctr >= §DfeDalD
026400160223     c**                 GoTo      Notartutto
026500160223    5c**                 EndIf
026600050218      * Se Tamfie "N" Solo Tariffe documenti FedEx
026700160223    5c**                 If        ilv59Fie = 'N' and Wctr < §DfeDalD
026800160223     c**                 GoTo      Notartutto
026900160223    5c**                 EndIf
027000160223     c**
027100050218     c* Elaboro solo tariffe decorrenti o scadute (escludo da decorrere
027200050218     c*  come se non ci fossero)
027300060503    5c***                if        ilv59dsp>=tamddt
027400060503     c* 03/5/06 ES --> preferisco caricare tutto. Se un tariffa ancora
027500060503     c*                da decorrere è già stata immessa, meglio
027600060503     c*                non farla usare in modo improprio, salvo
027700060503     c*                che non sia anche presente come decorrente:allor
027800060503     c*                la ignoro: meglio la decorrente
027900050218     c
028000050218     C                   MOVEL     TAMCTR        COMCT2            3
028100050218     C                   Z-ADD     1             D
028200050218     C     COMCT2        LOOKUP    CTcli(D)                               31
028300060503    6c                   if        (*in31 and ilv59dsp>=tamddt) or
028400050218     c                             (not *in31 and c < 50)
028500050218     C*
028600160223     C* DELLE TARIFFE già presenti, tengo sempre l'ultima, escluse le da
028700060511     c*  decorrere
028800050218    7C     *IN31         IFEQ      *OFF
028900050218     C                   ADD       1             C
029000050218     C                   MOVEL     TAMCTR        CTcli(C)
029100050218     C                   Z-ADD     C             D
029200050218    7C                   ENDIF
029300050218     C*
029400050218     C* VEDO SE E' TARIFFA decorrente
029500060503     c                   select
029600160223    7C     ilv59dsp      whenge    TAMDDT
029700050218     C     ilv59dsp      ANDLE     TAMDST
029800060503     c* Decorrente
029900050218     c                   movel     'D'           vacli(d)
030000060503    7C     ilv59dsp      whenlT    TAMDDT
030100060503     c* dA decorrere
030200060503     c                   movel     'A'           vacli(d)
030300060503     c
030400060503     c                   other
030500060503     c* Scaduta
030600050218     c                   movel     'S'           vacli(d)
030700060511    7c                   endsl
030800050218     C*
030900050218     C                   MOVEL     TAMCTR        CTcli(d)
031000050218     C                   MOVEL     TAMTSP        SPcli(d)
031100050218     C                   MOVE      TAMFTP        TPcli(d)
031200050218     C                   MOVE      TAMPRG        PGcli(d)
031300050218     C                   MOVE      §TADIV        DIcli(d)
031400050218     C                   MOVE      TAMFIE        FIcli(d)
031500050218     C                   MOVE      TAMbap        bacli(d)
031600170622     C                   MOVE      TAMeds        edcli(d)
031700050218     C* TARIFFA DPD
031800050218     C     §TADPD        IFEQ      'S'
031900050218     C                   MOVE      'D'           FIcli(d)
032000050218     C                   ENDIF
032100050218     C* TARIFFA FEDEX
032200050218    7C     §TAFED        IFEQ      'S'
032300050218     C                   MOVE      'F'           FIcli(d)
032400050218     c                   Clear                   mdcli(d)
032500160223     c******             If        ilv59Fie = 'M' and Wctr >= §DfeDalM
032600160223     c                   If        Wctr >= §DfeDalM
032700050218     c                                            and Wctr <= §DfeAlM
032800050218     c                   Eval      mdcli(d) = 'M'
032900050218     c                   EndIf
033000160223     c********           If        ilv59Fie = 'N' and Wctr >= §DfeDalD
033100160223     c                   If        Wctr >= §DfeDalD
033200050218     c                                            and Wctr <= §DfeAlD
033300050218     c                   Eval      mdcli(d) = 'N'
033400050218     c                   EndIf
033500050218    7c                   EndIf
033600050218    6C                   ENDIF
033700060511    5C*********          ENDIF
033800060511    4C*********          ENDIF
033900050218    3C                   ENDIF
034000050218
034100060511     c     Notartutto    Tag
034200050218
034300050218     C*
034400050218     C  N30ilv59ksc      READE     TNTAM000                               30
034500050218     C*
034600050218    2C                   ENDDO
034700160223     c
034800050218    1c                   endif
034900050218     c
035000050218     C                   ENDSR
035100160223     C*
035200160223     C* SELEZIONO le tariffe del cliente in base a quanto passato in DS ---*
035300160223     C     SELCTR_ntw    BEGSR
035400160223     C* PULISCO SKIERE DELLE TARIFFE
035500160223     C                   CLEAR                   CTV
035600160223     c                   Clear                   Ctw
035700160223     C                   CLEAR                   TPV
035800160223     c                   Clear                   Tpw
035900160223     C                   CLEAR                   SPV
036000160223     c                   Clear                   Spw
036100160223     C                   CLEAR                   PGV
036200160223     C                   CLEAR                   DIV
036300160223     C                   CLEAR                   FIV
036400160223     C                   CLEAR                   FIw
036500160223     c                   Clear                   mdv
036600160223     c                   Clear                   bav
036700160223     c                   Clear                   bavdop
036800160223     c                   Clear                   vav
036900170622     c                   Clear                   edv
037000160223     c                   Clear                   w_val             1
037100160223     c                   Clear                   TIPOVAL           1
037200160223     C                   MOVEL     *BLANKS       olv59TKS
037300160223     c* rileggo le schiere e carico in base ai parametri passati
037400160223     c                   z-add     1             xx                4 0
037500160223     c                   clear                   B
037600160223     c
037700160223     c* 1) ELABORO PRIMA LE DECORRENTI
037800160223     C*     solo se richiesto
037900160223    6c                   if        ilv59VAL=' ' or
038000160223     c                             ilv59VAL='D'
038100160223     C                   EVAL      TIPOVAL='D'
038200160223     C                   EXSR      CARICA_CTR
038300160223     C                   ENDIF
038400160223
038500160223     C* 2) POI ELABORO LE SCADUTE
038600160223     C*     solo se richiesto
038700160223    6c                   if        ilv59VAL=' ' or
038800160223     c                             ilv59VAL='S'
038900160223     c                   z-add     1             xx
039000160223     c* caricata almeno una valida
039100160223     c                   if        b>0
039200160223     c                   eval      w_val='S'
039300160223     c                   endif
039400160223     c
039500160223     C                   EVAL      TIPOVAL='S'
039600160223     C                   EXSR      CARICA_CTR
039700160223     C                   ENDIF
039800160223
039900160223     c                   exsr      VISTAR
040000160223     c                   ENDSR
040100160223     C*
040200160223     C* -------------------------------------------------------------------*
040300160223     C     CARICA_CTR    BEGSR
040400160223
040500160223    1c                   dow       ctcli(xx)<>*blanks and B<50
040600160223     c* carico il tipo di validità richiesto
040700160223    2c                   if        vacli(xx) =TIPOVAL
040800160223
040900160223     C* ilv59BLO:  B=solo bloccate " "=solo non bloccate  S=tutte
041000160223    3c                   if         ilv59BLO='S' or
041100160223     c                             (ilv59BLO='B' and bacli(xx)='B') or
041200160223     c                             (ilv59BLO=' ' and bacli(xx)<>'B')
041300160223
041400160223     C* CARICO SOLO TARIFFE ITALIA           --> FIE "I"
041500160223     C* CARICO SOLO TARIFFE Euroexpress      --> FIE "E"
041600160223     C* CARICO SOLO TARIFFE DPD              --> FIE "D"
041700160223     C* CARICO SOLO TARIFFE FEDEX            --> FIE "F"
041800160223     C* CARICO      TARIFFE DPD+FEDEX+EUROEX --> FIE "S"
041900160223     C* CARICO      TARIFFE DPD+FEDEX+ITALIA --> FIE "J"
042000160223     C* CARICO SOLO TARIFFE FEDEX DOCUMENTI  --> FIE "N"
042100160223     C* CARICO SOLO TARIFFE FEDEX MERCI      --> FIE "M"
042200160223     C* CARICO      TARIFFE DPD+EUROEX       --> FIE "X"
042300160223    4c                   if        ficli(xx)=' ' or
042400160223     c                             ficli(xx)=ilv59fie or
042500160223     c                             (ilv59fie='X' and ficli(XX)='D') or
042600160223     c                             (ilv59fie='X' and ficli(XX)='E') or
042700160223     c                             (ilv59fie='S' and ficli(xx)<>'I') or
042800160223     c                             (ilv59fie='J' and ficli(xx)<>'E') or
042900160223     c                             (ilv59fie='M' and mdcli(xx)= 'M') or
043000160223     c                             (ilv59fie='N' and mdcli(xx)= 'N')
043100160223     c
043200160223     C* ilv59TBO P=carico tariffe P/B/" "
043300160223     c*          A=carico tariffe A/B/" "   " "=tutte
043400160223    5c                   if        ilv59tbo=' ' or
043500160223     c                             bacli(xx)=' ' or
043600160223     c                             bacli(xx)='B' or
043700160223     c                             ilv59tbo=bacli(xx)
043800170622     c* Carico solo per web
043900170622   5ac                   if        ilv59ta5<>'W' or edcli(xx)<>'N'
044000160223     c
044100160223     C                   ADD       1             B
044200160223     C                   MOVEL     ctcli(xx)     CTV(B)
044300160223     C                   MOVEL     spcli(xx)     SPV(B)
044400160223     c* se caricata almeno una valida non tengo conto della tariffa Prefer
044500160223     c*  per le non valide
044600160223     c                   if        w_val='S'
044700160223     c                   clear                   TPV(B)
044800160223     c                   else
044900160223     C                   MOVE      tpcli(XX)     TPV(B)
045000160223     c                   endif
045100160223     C                   MOVE      pgcli(xx)     PGV(B)
045200160223     C                   MOVE      dicli(xx)     DIV(B)
045300160223     C                   MOVE      ficli(xx)     FIV(B)
045400160223     C                   MOVE      bacli(xx)     BAV(B)
045500170622     C                   MOVE      edcli(xx)     EDV(B)
045600160223     c
045700160223     c* Carico il campo bap anche mettendo al posto di " " --> "E"
045800160223     c                   if        bacli(XX)=' ' or bacli(xx)='B'
045900160223     c                   movel     'E'           bavdop(b)
046000160223     c                   else
046100160223     C                   MOVE      bacli(xx)     BAVdop(B)
046200160223     c                   endif
046300160223     c* Validità tariffa
046400160223     C                   MOVE      vacli(xx)     VAV(B)
046500160223     c
046600160223     c* Fedex merci/documenti
046700160223     c                   if        ficli(XX)='F'
046800160223     c                   move      mdcli(XX)     MDV(b)
046900160223     c                   endif
047000160223     c
047100170622   5ac                   endif
047200170622    5c                   endif
047300160223    4c                   endif
047400160223    3c                   endif
047500160223    2c                   endif
047600160223     c
047700160223     c                   add       1             xx
047800160223    1c                   enddo
047900160223     c
048000160223     C                   ENDSR
048100941104     C*
048200050218     C* SELEZIONO le tariffe del cliente in base a quanto passato in DS ---*
048300160223     C**   SELCTR        BEGSR
048400160223     C**
048500941104     C* PULISCO SKIERE DELLE TARIFFE
048600160223     C**                 CLEAR                   CTV
048700160223     C**                 CLEAR                   CTN
048800160223     c**                 Clear                   Ctw
048900160223     C**                 CLEAR                   TPN
049000160223     C**                 CLEAR                   TPV
049100160223     c**                 Clear                   Tpw
049200160223     C**                 CLEAR                   SPN
049300160223     C**                 CLEAR                   SPV
049400160223     c**                 Clear                   Spw
049500160223     C**                 CLEAR                   PGV
049600160223     C**                 CLEAR                   PGN
049700160223     C**                 CLEAR                   DIV
049800160223     C**                 CLEAR                   DIN
049900160223     C**                 CLEAR                   FIV
050000160223     C**                 CLEAR                   FIN
050100160223     C**                 CLEAR                   FIw
050200160223     c**                 Clear                   mdv
050300160223     c**                 Clear                   mdn
050400160223     c**                 Clear                   bav
050500160223     c**                 Clear                   ban
050600160223     c**                 Clear                   bavdop
050700160223     c**                 Clear                   bandop
050800160223     c**                 Clear                   vav
050900160223     C**
051000160223     C**                 MOVEL     *BLANKS       olv59TKS
051100160223     C**
051200160223     C**MITTENTI CODIFICATI --> CERCO LE TARIFFE
051300160223     C**                 Z-ADD     0             B                 3 0
051400160223     C**                 Z-ADD     0             C                 3 0
051500160223     C**                 Z-ADD     0             D                 3 0
051600160223     C**
051700160223     C**SE 8888 O 9999 NON CARICO NULLA
051800160223     C**                 MOVE      ilv59KSC      WCCM              4 0
051900160223     C**
052000160223    1C**   WCCM          IFNE      8888
052100160223     C**   WCCM          ANDNE     9999
052200160223     C**
052300160223     C**   ilv59ksc      SETLL     TNTAM000
052400160223     C**   ilv59ksc      READE     TNTAM000                               30
052500160223     C**
052600160223    2C**   *IN30         DOWEQ     *OFF
052700160223     C**ESCLUDO SE ANNULLATO
052800160223    3C**   TAMATB        IFNE      'A'
052900160223     c**
053000160223     C**                 MOVEL     TAMFLO        DSTA01
053100160223     C**
053200160223    5C**   TAMFIE        IFEQ      ' '
053300160223     C**
053400160223     C**   TAMFIE        OREQ      ilv59fie
053500160223     C**   §TADPD        ANDEQ     ' '
053600160223     C**   §TAFED        ANDEQ     ' '
053700160223     C**SE TARIFFA DPD NON DEVO CARICARE  SE RICHIESTO I/E/F/M/N
053800160223     C**   §TADPD        OREQ      'S'
053900160223     C**   ilv59fie      ANDNE     'I'
054000160223     C**   ilv59fie      ANDNE     'E'
054100160223     C**   ilv59fie      ANDNE     'F'
054200160223     c**   ilv59fie      andne     'M'
054300160223     c**   ilv59fie      andne     'N'
054400160223     C**
054500160223     C**SE TARIFFA FEDEX NON DEVO CARICARE  SE RICHIESTO I/E/D/X
054600160223     C**   §TAFED        OREQ      'S'
054700160223     C**   ilv59fie      ANDNE     'I'
054800160223     C**   ilv59fie      ANDNE     'E'
054900160223     C**   ilv59fie      ANDNE     'D'
055000160223     C**   ilv59fie      ANDNE     'X'
055100160223     C**LE ESTERE LE DEVO CARICARE SOLO PER S (OLTRE CHE DPD E FEDEX)
055200160223     C**   TAMFIE        OREQ      'E'
055300160223     C**   ilv59fie      ANDEQ     'S'
055400160223     C**LE EEX    LE DEVO CARICARE anche per X
055500160223     C**   TAMFIE        OREQ      'E'
055600160223     C**   ilv59fie      ANDEQ     'X'
055700160223     C**   §TAFED        ANDEQ     ' '
055800160223     C**LE ITALIA LE DEVO CARICARE SOLO PER J (OLTRE CHE DPD E FEDEX)
055900160223     C**   TAMFIE        OREQ      'I'
056000160223     C**   ilv59fie      ANDEQ     'J'
056100160223     C**
056200160223     c**                 Move      TamCtr        Wctr
056300160223     C**Se Tamfie "M" Solo Tariffe merci FedEx
056400160223    6c**                 If        ilv59Fie = 'M' and Wctr >= §DfeDalD
056500160223     c**                 GoTo      Notar
056600160223    6c**                 EndIf
056700160223     C**Se Tamfie "N" Solo Tariffe documenti FedEx
056800160223    6c**                 If        ilv59Fie = 'N' and Wctr < §DfeDalD
056900160223     c**                 GoTo      Notar
057000160223    6c**                 EndIf
057100160223     C**
057200160223     C**ilv59TBO P=carico tariffe P/B/" "
057300160223     a**         A=carico tariffe A/B/" "   " "=tutte
057400160223    6C**   ilv59tbo      IFEQ      ' '
057500160223     C**   TAMBAP        OREQ      ' '
057600160223     C**   TAMBAP        OREQ      'B'
057700160223     C**   TAMBAP        OREQ      ilv59tbo
057800160223     C**
057900160223     C**VEDO SE E' TARIFFA decorrente
058000160223    7C**   ilv59dsp      IFGE      TAMDDT
058100160223     C**   ilv59dsp      ANDLE     TAMDST
058200160223     c**solo se richieste (ilv59VAL=D/" ")
058300160223    8c**                 if        ilv59VAL=' ' or ilv59val='D'
058400160223     C**
058500160223     C**                 ADD       1             B
058600160223     C**                 MOVEL     TAMCTR        CTV(B)
058700160223     C**                 MOVEL     TAMTSP        SPV(B)
058800160223     C**                 MOVE      TAMFTP        TPV(B)
058900160223     C**                 MOVE      TAMPRG        PGV(B)
059000160223     C**                 MOVE      §TADIV        DIV(B)
059100160223     C**                 MOVE      TAMFIE        FIV(B)
059200160223     C**                 MOVE      TAMbap        baV(B)
059300160223     c**Carico il campo bap anche mettendo al posto di " " --> "E"
059400160223    9c**                 if        tambap=' ' or tambap='B'
059500160223     c**                 movel     'E'           bavdop(b)
059600160223     c**                 else
059700160223     C**                 MOVE      TAMbap        baVdop(B)
059800160223    9c**                 endif
059900160223     C**                 MOVE      'D'           vaV(B)
060000160223     C**TARIFFA DPD
060100160223     C**   §TADPD        IFEQ      'S'
060200160223     C**                 MOVE      'D'           FIV(B)
060300160223     C**                 ENDIF
060400160223     C**TARIFFA FEDEX
060500160223    9C**   §TAFED        IFEQ      'S'
060600160223     C**                 MOVE      'F'           FIV(B)
060700160223     c**                 Clear                   mdv(b)
060800160223     c**                 If        ilv59Fie = 'M' and Wctr >= §DfeDalM
060900160223     c**                                          and Wctr <= §DfeAlM
061000160223     c**                 Eval      mdv(b) = 'M'
061100160223     c**                 EndIf
061200160223     c**                 If        ilv59Fie = 'N' and Wctr >= §DfeDalD
061300160223     c**                                          and Wctr <= §DfeAlD
061400160223     c**                 Eval      mdv(b) = 'N'
061500160223     c**                 EndIf
061600160223    9c**                 EndIf
061700160223     c**
061800160223     C**   B             COMP      50                                 30
061900160223   x8c**                 else
062000160223     c**Se tariffa decorrente e c'e' anche come scaduta la tolgo
062100160223     C**
062200160223     C**                 MOVEL     TAMCTR        COMCT2            3
062300160223     C**                 Z-ADD     1             D
062400160223     C**   COMCT2        LOOKUP    CTN(D)                                 31
062500160223    9c**                 if        *in31
062600160223     c**                 clear                   ctn(d)
062700160223     c**                 clear                   spn(d)
062800160223     c**                 clear                   tpn(d)
062900160223     c**                 clear                   pgn(d)
063000160223     c**                 clear                   din(d)
063100160223     c**                 clear                   fin(d)
063200160223     c**                 clear                   ban(d)
063300160223     c**                 clear                   bandop(d)
063400160223     c**                 clear                   mdn(d)
063500160223     c**Reimposto il contatore a -1
063600160223     c**                 sub       1             C
063700160223     c**
063800160223    9C**                 ENDIF
063900160223    8C**                 ENDIF
064000160223     C**
064100160223     C**TARIFFE non decorrente
064200160223   X7C**                 ELSE
064300160223     c**        tengo solo le scadute, se richieste (ilv59VAL=S/" ")
064400160223    8c**                 if        ilv59dsp>tamdst  and
064500160223     c**                           (ilv59VAL=' ' or ilv59VAL='S')
064600941104     C*
064700160223     C**                 MOVEL     TAMCTR        COMCT2            3
064800160223     C**                 Z-ADD     1             D
064900160223     C**   COMCT2        LOOKUP    CTN(D)                                 31
065000160223    9c**                 if        *in31   or
065100160223     c**                           (not *in31 and c < 50)
065200160223     C**
065300160223     C**DELLE TARIFFE NON VALIDE, TENGO SEMPRE L'ULTIMA
065400160223   10C**   *IN31         IFEQ      *OFF
065500160223     C**                 ADD       1             C
065600160223     C**                 MOVEL     TAMCTR        CTN(C)
065700160223     C**                 Z-ADD     C             D
065800160223   10C**                 ENDIF
065900160223     C**                 MOVEL     TAMTSP        SPN(D)
066000160223     C**                 MOVE      TAMFTP        TPN(D)
066100160223     C**                 MOVE      TAMPRG        PGN(D)
066200160223     C**                 MOVE      §TADIV        DIN(D)
066300160223     C**                 MOVE      TAMFIE        FIN(D)
066400160223     C**                 MOVE      TAMbap        ban(D)
066500160223     c**Carico il campo bap anche mettendo al posto di " " --> "E"
066600160223     c**                 if        tambap=' '   or  tambap='B'
066700160223     c**                 movel     'E'           bandop(D)
066800160223     c**                 else
066900160223     C**                 MOVE      TAMbap        bandop(D)
067000160223     c**                 endif
067100160223     C**TARIFFA DPD
067200160223     C**   §TADPD        IFEQ      'S'
067300160223     C**                 MOVE      'D'           FIN(D)
067400160223     C**                 ENDIF
067500160223     C**TARIFFA FEDEX
067600160223   10C**   §TAFED        IFEQ      'S'
067700160223     C**                 MOVE      'F'           FIN(D)
067800160223     c**                 Clear                   mdn(d)
067900160223     c**                 If        ilv59Fie = 'M' and Wctr >= §DfeDalM
068000160223     c**                                          and Wctr <= §DfeAlM
068100160223     c**                 Eval      mdn(d) = 'M'
068200160223     c**                 EndIf
068300160223     c**                 If        ilv59Fie = 'N' and Wctr >= §DfeDalD
068400160223     c**                                          and Wctr <= §DfeAlD
068500160223     c**                 Eval      mdn(d) = 'N'
068600160223     c**                 EndIf
068700160223   10C**                 ENDIF
068800160223    9C**                 ENDIF
068900160223     C**
069000160223    8C**                 ENDIF
069100160223    7C**                 ENDIF
069200160223    6C**                 ENDIF
069300160223    5C**                 ENDIF
069400160223    4C**                 ENDIF
069500160223    3C**                 ENDIF
069600160223     C**
069700160223     c**   Notar         Tag
069800160223     C**
069900160223     C**
070000160223     C**N30ilv59ksc      READE     TNTAM000                               30
070100160223     C**
070200160223    2C**                 ENDDO
070300160223     C**
070400160223     C**                 ADD       1             B
070500160223     C**
070600160223     C**CARICO TUTTI I TIPI TARIFFA TROVATI IN UNICA SCHIERA
070700160223     C**SE NON NE HO GIA' TROVATI 50 VALIDI
070800160223    2C**   B             IFLE      50
070900160223     C**
071000160223     C**
071100160223     C**SE ESISTE ALMENO UNA TARIFFA VALIDA, NON CONSIDERO LA 'P'
071200160223     C** DI PREFERENZIALE PER LE TARIFFE SCADUTE
071300160223    3C**   B             IFGT      1
071400160223     C**                 CLEAR                   TPN
071500160223    3C**                 ENDIF
071600160223     C**
071700160223     C**                 Z-ADD     1             C
071800160223     C**
071900160223    3C**   CTN(C)        DOWNE     *BLANKS
072000160223     C**   B             ANDLE     50
072100160223     C**
072200160223     C**   CTN(C)        LOOKUP    CTV                                    31
072300160223    4C**   *IN31         IFEQ      *OFF
072400160223     C**                 MOVEL     CTN(C)        CTV(B)
072500160223     C**                 MOVEL     TPN(C)        TPV(B)
072600160223     C**                 MOVEL     SPN(C)        SPV(B)
072700160223     C**                 MOVEL     PGN(C)        PGV(B)
072800160223     C**                 MOVEL     DIN(C)        DIV(B)
072900160223     C**                 MOVEL     FIN(C)        FIV(B)
073000160223     c**                 Movel     mdn(c)        mdv(b)
073100160223     c**                 Movel     ban(c)        bav(b)
073200160223     c**                 Movel     bandop(c)     bavdop(b)
073300160223     c**S-scaduta
073400160223     c**                 movel     'S'           vav(b)
073500160223     C**                 ADD       1             B
073600160223    4C**                 ENDIF
073700160223     C**
073800160223     C**                 ADD       1             C
073900160223    3C**                 ENDdo
074000160223    2C**                 ENDif
074100160223     C**
074200160223     C**                 EXSR      VISTAR
074300160223    1C**                 ENDif
074400160223     C**
074500160223     C**                 ENDSR
074600941104     C*
074700160428     C* RICERCA E CARICAMENTO A VIDEO TARIFFE solo col tipo serivizio-*
074800160428     C     RICTAR_TSP    BEGSR
074900160428     c
075000160428     C                   SETOFF                                       30
075100160428     C                   CLEAR                   W001A
075200160428     c                   clear                   CNo
075300160428     C*
075400160428     C* SE NON CI SONO TARIFFE IMPOSTO LA TARIFFA 000
075500160428    1C     CTV(1)        IFEQ      *BLANKS
075600160428     C                   CLEAR                   olv59tks
075700160428     c
075800160428    1C     ilv59ctr      IFEQ      *BLANKS
075900170517     c* eseguo un loop per proporre un cod tariffa valido ma NON
076000170517     c*  esistente per altri netwokr
076100170517     C                   CLEAR                   WCTR_N            3 0
076200170517     c                   eval      wok='N'
076300170517     c                   dow       wok='N'  AND WCTR_N<15
076400170517     C                   MOVEL     wctr_n        olv59CTR
076500170517     C                   IF        %LOOKUP(olv59ctr:CTCLI)>0
076600170517     C                   ADD       1             WCTR_N
076700170517     c                   else
076800170517     c                   eval      wok='S'
076900170517     C                   ENDIF
077000170517     c                   enddo
077100170517     C
077200170517     C***                MOVEL     *ZEROS        olv59CTR
077300160428    1C                   ENDIF
077400170517    C
077500160428     C*
077600160428   X1C                   ELSE
077700160428      * Carico le sk di work per la scelta
077800160428     c                   ExSr      CarSk
077900160428     C*
078000160428     C* 1) SE IMPOSTATO IL TIPO SERVIZIO --> CERCO QUELLO
078100160428     c*    solo per E e H (non lo faccio per C e D)
078200160428     c                   exsr      Cerca_tsp
078300160428     C*
078400160428     c* Se trovato -->IMPOSTO CAMPO CODICE TARIFFA
078500160428    2c                   if        *in30
078600160428     C                   MOVEL     CTw(C)        Olv59CTR
078700160428     c                   else
078800160428     c* imposto quello di input
078900160428     c                   eval      olv59ctr=ilv59ctr
079000160428    2c                   endif
079100160428     c
079200160428     c                   exsr      DATI
079300160428    1c                   endif
079400160428     C*
079500160428     c                   ENDSR
079600160428     c
079700941104     C* RICERCA E CARICAMENTO A VIDEO TARIFFE ------------------------*
079800941104     C     RICTAR        BEGSR
079900941104     C                   SETOFF                                       30
080000950125     C                   CLEAR                   W001A
080100050124     c                   clear                   CNo
080200941104     C*
080300941104     C* SE NON CI SONO TARIFFE IMPOSTO LA TARIFFA 000
080400050127    1C     CTV(1)        IFEQ      *BLANKS
080500050127     C                   CLEAR                   olv59tks
080600941215     C*
080700050127     C     ilv59ctr      IFEQ      *BLANKS
080800170517     C***                MOVEL     *ZEROS        olv59CTR
080900170517     c
081000170517     C                   CLEAR                   WCTR_N            3 0
081100170517     c                   eval      wok='N'
081200170517     c                   dow       wok='N'  AND WCTR_N<15
081300170517     C                   MOVEL     wctr_n        olv59CTR
081400170517     C                   IF        %LOOKUP(olv59ctr:CTCLI)>0
081500170517     C                   ADD       1             WCTR_N
081600170517     c                   else
081700170517     c                   eval      wok='S'
081800170517     C                   ENDIF
081900170517     c                   enddo
082000941215     C                   ENDIF
082100941215     C*
082200050127   X1C                   ELSE
082300941107     C*
082400941107     C* CODICE TARIFFA IMPOSTATO: PRENDO IL RELATIVO PROGRESSIVO
082500050127    2C     ilv59CTR      IFNE      *BLANKS
082600050127     c                   eval      olv59ctr=ilv59ctr
082700050127    2C                   ENDIF
082800941107     C**
082900050127     c**
083000050127    2C     ilv59CTR      IFEQ      *BLANKS
083100021115
083200050127      * Carico le sk di work per la scelta
083300021115     c                   ExSr      CarSk
083400941104     C*
083500160215     C* 1) SE IMPOSTATO IL TIPO SERVIZIO --> CERCO QUELLO
083600160401     c*    slo per E e H (non lo faccio per C e D)
083700160428     c                   exsr      Cerca_tsp
083800160428
083900941104     C*
084000941104     C* SE NON TROVATA O NON IMPOSTATO IL TIPO SERVIZIO -->
084100160215     c* 2) cerco per network  se passato
084200160215     c*    solo se ho caricato più di un network, altrimenti ignoro
084300160215    3c                   if        not *in30 and ilv59TA3<>' '
084400160215     c                             and (ilv59fie='X' or ilv59fie='S' or
084500160215     c                             ilv59fie='J')
084600160215     C                   CLEAR                   W001A
084700160215     c                   clear                   CNo
084800160215     C                   Z-ADD     1             C
084900160215     C                   SETON                                        31
085000160215     C*
085100160215    4C     *IN31         DOWEQ     *ON
085200160215     C     ilv59TA3      LOOKUP    fiw(C)                                 31
085300160215     C* SE TROVATA ED E' PREFERENZIALE ESCO DAL CICLO
085400160215    5c   31              if        tpw(C)='P'
085500160215     C                   SETON                                        30
085600160215     C                   SETOFF                                       31
085700160215     C                   MOVEL     '1'           W001A             1
085800160215   X5C                   ELSE
085900160215     c
086000160215     C* SE TROVATA MA NON PREFERENZIALE ACCENDO COMUNQUE L'INDICATORE
086100160215     C*  DI TROVATO MA FACCIO UN ALTRO GIRO
086200160215     C                   SETON                                        30
086300160215     c                   if        CNo=0
086400160215     c                   z-add     C             CNo
086500160215     c                   endif
086600160215     c
086700160215     C                   ADD       1             C
086800160215    5C                   ENDIF
086900160215    4C                   ENDDO
087000160215     C*
087100160215     C* SE HO TROVATO LA TARIFFA MA NON LA PREFERENZIALE, LA RICERCO per
087200160215     C*  PRENDERE LA POSIZIONE
087300160215    4C   30W001A         IFEQ      ' '
087400160215     c                   z-add     CNo           C
087500160215    4C                   ENDIF
087600160215     C*
087700160215    3C                   ENDIF
087800160215
087900160215     C* SE NON ancora TROVATA -->
088000160215     C* 3) CERCO LA PRIMA PREFERENZIALE
088100050124    3C     *IN30         IFEQ      *OFF
088200050127     C                   Z-ADD     1             C
088300050127     C     'P'           LOOKUP    TPw(C)                                 30
088400050127    3C                   ENDIF
088500941104     C*
088600160215     C* 4) SE NON ESISTE LA PREFERENZIALE METTO LA PRIMA VALIDA TROVATA
088700941104     C  N30              Z-ADD     1             C
088800050127     c
088900050127     c* IMPOSTO CAMPO CODICE TARIFFA
089000050127     C                   MOVEL     CTw(C)        Olv59CTR
089100050127    2c                   endif
089200160428     c
089300160428     c                   exsr      DATI
089400160428     c
089500050127    1C                   ENDIF
089600941104     C*
089700941104     C                   ENDSR
089800160428      *--------------------------------------------------------------*
089900160428      * Ricerca tariffa per tipo servizio
090000160428      *--------------------------------------------------------------*
090100160428     c     Cerca_tsp     BEGSR
090200160428    3C     ilv59TSP      IFNE      *BLANKS
090300160428     c     ilv59TSP      andne     'C'
090400160428     c     ilv59TSP      andne     'D'
090500160428     C* CERCO SE C'E' LA PREFERENZIALE ALTRIMENTI LA 1 CHE TROVO
090600160428     C                   Z-ADD     1             C
090700160428     C                   SETON                                        31
090800160428     C*
090900160428    4C     *IN31         DOWEQ     *ON
091000160428     C     ilv59tSP      LOOKUP    SPw(C)                                 31
091100160428     C* SE TROVATA ED E' PREFERENZIALE ESCO DAL CICLO
091200160428    5c   31              if        tpw(C)='P'
091300160428     C                   SETON                                        30
091400160428     C                   SETOFF                                       31
091500160428     C                   MOVEL     '1'           W001A             1
091600160428   X5C                   ELSE
091700160428     C* SE TROVATA MA NON PREFERENZIALE ACCENDO COMUNQUE L'INDICATORE
091800160428     C*  DI TROVATO MA FACCIO UN ALTRO GIRO
091900160428     C                   SETON                                        30
092000160428     c                   if        CNo=0
092100160428     c                   z-add     C             CNo
092200160428     c                   endif
092300160428     c
092400160428     C                   ADD       1             C
092500160428    5C                   ENDIF
092600160428    4C                   ENDDO
092700160428     C*
092800160428     C* SE HO TROVATO LA TARIFFA MA NON LA PREFERENZIALE, LA RICERCO X
092900160428     C*  PRENDERE LA POSIZIONE
093000160428    4C   30W001A         IFEQ      ' '
093100160428     c                   z-add     CNo           C
093200160428    4C                   ENDIF
093300160428     C*
093400160428    3C                   ENDIF
093500160428     c                   ENDSR
093600160428      *--------------------------------------------------------------*
093700160428      * Finisco di caricare i dati relativi alla tariffa
093800160428      *--------------------------------------------------------------*
093900160428     c     DATI          BEGSR
094000160428     C*
094100160428      * Cerco la posizione nella sk valida
094200160428     c                   Z-Add     1             c
094300160428     c     olv59CTR      Lookup    Ctv(c)                                 30
094400160428     c
094500160428    2c                   IF        *IN30
094600160428     C                   MOVEL     PGV(C)        OLV59PRG
094700160428     C                   MOVEL     DIV(C)        OLV59DIV
094800160428     C     OLV59DIV      IFEQ      '   '
094900160428     C                   MOVEL     'ITL'         OLV59DIV
095000160428     C                   ENDIF
095100160428
095200160428   x2C                   ELSE
095300160428     C* ERRORE --> COD.TARIFFA NON TROVATO (se passato in input può
095400160428     c*            accadere)
095500160428     C                   MOVEL     '3'           olv59ERR
095600160428     C                   MOVEL     ERR(3)        olv59MSG
095700160428     C                   Z-ADD     10            C
095800160428    2C                   ENDIF
095900160428     C*
096000160428     C                   EXSR      VISTAR
096100160428     C*
096200160428     C* RIMPIAZZO CON COD TARIFFA 5 SOLO SE ESISTE
096300160428    2C     CTv(5)        IFNE      *BLANKS
096400160428     C* SE NELLA 1°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
096500160428    3C     C             IFEQ      1
096600160428     C                   MOVEL     CTv(5)        olv59CT2
096700160428     C                   MOVEL     SPv(5)        olv59ES2
096800160428     C                   MOVE      FIv(5)        olv59ES2
096900160428    3C                   END
097000160428     C* SE NELLA 2°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
097100160428    3C     C             IFEQ      2
097200160428     C                   MOVEL     CTv(5)        olv59CT3
097300160428     C                   MOVEL     SPv(5)        olv59ES3
097400160428     C                   MOVE      FIv(5)        olv59ES3
097500160428    3C                   END
097600160428     C* SE NELLA 3°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
097700160428    3C     C             IFEQ      3
097800160428     C                   MOVEL     CTv(5)        olv59CT4
097900160428     C                   MOVEL     SPv(5)        olv59ES4
098000160428     C                   MOVE      FIv(5)        olv59ES4
098100160428    3C                   END
098200160428     C* SE NELLA 4°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
098300160428    3C     C             IFEQ      4
098400160428     C                   MOVEL     CTv(5)        olv59CT5
098500160428     C                   MOVEL     SPv(5)        olv59ES5
098600160428     C                   MOVE      FIv(5)        olv59ES5
098700160428    3C                   END
098800160428     C*
098900160428    2C                   END
099000160428     c                   ENDSR
099100021115      *--------------------------------------------------------------*
099200050127      * Carico le sk di work per la proposta a video
099300021115      *--------------------------------------------------------------*
099400021115     c     Carsk         BegSr
099500021115
099600050127     c* Se richieste tariffe FEDEX e presenti sia M che N --> propon
099700050127     c*  go "N"-documenti
099800050127     c                   setoff                                       3536
099900050127     c                   setoff                                       3738
100000050127     c                   If        ilv59Fie = 'F'
100100021115     c     'M'           Lookup    mdv                                    35
100200021115     c     'N'           Lookup    mdv                                    36
100300021115     c                   EndIf
100400050127     c
100500050127     c* Se presenti sia tariffe "P" che "A" --> propongo la P o la A
100600050127     c*    in base alla richiesta (ilv59PRP)
100700050128    1c                   if        ilv59tbo=' '  and ilv59PRP<>' '
100800050127     c     'E'           Lookup    bavdop                                 37
100900050128    2c                   if        *in37
101000050127     c                   seton                                        38
101100050128   x2c                   else
101200050127     c     'P'           Lookup    bavdop                                 37
101300050127     c     'A'           Lookup    bavdop                                 38
101400050128    2c                   endif
101500050128    1c                   endif
101600021115
101700021115     c                   Clear                   d
101800021115
101900050128    1c                   Do        50            b
102000021115     c                   If        Ctv(b) = *Blanks
102100021115     c                   Leave
102200021115     c                   EndIf
102300050127     c* per fedex tengo la documenti se presenti entrambe
102400050127     c                   If        ilv59Fie = 'F' and *In35 and *In36
102500021115     c                             and Mdv(b) <> 'N'
102600021115     c                   Iter
102700021115     c                   EndIf
102800050127     c
102900050127     c* Per la proposta impostata e presenti entrambe tariffe (p e A)
103000050127     c*  carico solo la proposta scelta
103100050127     c                   if        ilv59tbo=' ' and ilv59prp<>' ' and
103200050127     c                             *in37 and *in38
103300050127     c                   if        ilv59prp='P' and bavdop(b)='A'
103400050127     c                   iter
103500050127     c                   endif
103600050127     c                   if        ilv59prp='A' and bavdop(b)='P'
103700050127     c                   iter
103800050127     c                   endif
103900050127     c
104000050127     c                   endif
104100021115
104200021115     c                   Add       1             d
104300021115
104400021115     c                   Eval      Ctw(d) = Ctv(b)
104500021115     c                   Eval      Tpw(d) = Tpv(b)
104600021115     c                   Eval      Spw(d) = Spv(b)
104700160215     c                   Eval      fiw(d) = fiv(b)
104800050128    1c                   EndDo
104900021115
105000021115     c                   EndSr
105100941104     C*
105200941104     C* CONTROLLO CODICE TARIFFA ------------------------------------*
105300941104     C     CTRCTR        BEGSR
105400941215     C* SE ESISTE ALMENO UNA TARIFFA PER IL CLIENTE ANCHE SE SCADUTA
105500941215     C*  LO SEGNALO
105600941215     C     CTV(1)        IFNE      *BLANKS
105700050127     C                   MOVEL     'S'           olv59TKS
105800941215     C                   ENDIF
105900941215     C*
106000050127     C                   MOVE      ilv59ksc      WCCM              4 0
106100941104     C* RICERCA
106200050128     C     '?'           SCAN      olv59ctr                               30
106300941104     C*
106400941104    1C     *IN30         IFEQ      *ON
106500941104     C*
106600941104     C* SOLO SE CODIFICATO DELL'AREA POSSO FARE LA RICERCA
106700941104    2C     WCCM          IFEQ      8888
106800941104     C     WCCM          OREQ      9999
106900050127     C                   MOVEL     '1'           olv59ERR
107000050127     C                   MOVEL     ERR(1)        olv59MSG
107100941104     C                   GOTO      ENDCTR
107200941104    2C                   ENDIF
107300941104     C*
107400941104     C* RICERCA
107500941104     C                   CLEAR                   PARAM4
107600050127     C                   MOVEL     ilv59ksc      PA4COD
107700050127     C                   Z-ADD     ilv59kci      PA4CCC
107800061220     c                   movel     '2'           pa4vpr
107900061220     c                   move      ilv59dsp      pa4dtr
108000941104     C                   MOVEL     PARAM4        KPJBU
108100941104     C                   CALL      'TNTA36R'
108200941104     C                   PARM                    KPJBA
108300941104     C                   MOVEL     KPJBU         PARAM4
108400941104     C*
108500941104    2C     PA4CTR        IFNE      *BLANKS
108600941104     C* SE TROVATO METTO QUELLO SCELTO
108700050128     C                   MOVE      PA4CTR        olv59CTR
108800941104   X2C                   ELSE
108900050128     c* Se non scelto lo abblanco
109000050128     C                   MOVEL     *blanks       olv59CTR
109100941104    2C                   END
109200050127     C                   MOVEL     'R'           olv59ERR
109300941104     C                   GOTO      ENDCTR
109400941104     C*
109500941104   X1C                   ELSE
109600941104     C*
109700941116     C* CONTROLLO FORMALE
109800050128     c                   eval      wcontrCTR=olv59CTR
109900941116     C                   EXSR      CTRFOR
110000941116     C*
110100050127     C     olv59ERR      IFNE      ' '
110200941104     C                   GOTO      ENDCTR
110300941104    2C                   ENDIF
110400941104     C*
110500941104     C* CONTROLLO ESISTENZA TARIFFA
110600941104    2C     CTV(1)        IFNE      *BLANKS
110700941104     C                   Z-ADD     1             B
110800050128     C     olv59CTR      LOOKUP    CTV(B)                                 30
110900050128     C   30              MOVEL     PGV(B)        olv59PRG
111000050128     C   30              MOVEL     DIV(B)        olv59DIV
111100060515    3C   30olv59DIV      IFEQ      '   '
111200050128     C                   MOVEL     'ITL'         olv59DIV
111300060515    3C                   ENDIF
111400060515     c
111500060515     c* Se non trovata tariffa e accetto 1 cod tariffa se bloccato, verifico
111600060515     c*  il network
111700060515    3c                   if        not *in30 and ilv59ta2='B'
111800060515     C                   Z-ADD     1             B
111900060515     C     olv59CTR      LOOKUP    CTcli(b)                               30
112000060515     c*
112100060515    4c                   if        *in30
112200060515    5c                   if        ficli(b)=ilv59fie              or
112300060515     c                             ilv59fie='M' and mdcli(b)='M'  or
112400060515     c                             ilv59fie='N' and mdcli(b)='N'  or
112500060515     c                             ilv59fie='S' and ficli(b)<>'I' or
112600160215     c                             ilv59fie='J' and ficli(b)<>'E' or
112700160215     c                             ilv59fie='X' and (ficli(b)='E' or
112800160215     c                             ficli(b)='D')
112900060515     c* Network corrisponde: controllo se bloccata
113000060515    6c                   if        bacli(b)='B' and vacli(b)<>'A'
113100060516     c* se ok imposto i campi del progressino e della divisa
113200060516     C                   MOVEL     PGcli(b)      olv59PRG
113300060516     C                   MOVEL     DIcli(b)      olv59DIV
113400060516    3C     olv59DIV      IFEQ      '   '
113500060516     C                   MOVEL     'ITL'         olv59DIV
113600060516    3C                   ENDIF
113700060515     c                   else
113800060515     c                   setoff                                       30
113900060515    6c                   endif
114000060515   x5c                   else
114100060515     c                   setoff                                       30
114200060515    5c                   endif
114300060515    4c                   endif
114400060515    3c                   endif
114500060515     c
114600060515     c
114700941104     C* INESISTENTE: ERRORE
114800060516    3C**N30CTV(1)        IFNE      *BLANKS
114900060516    c
115000060516     c                   if        not *in30
115100050128     C                   MOVEL     '3'           olv59ERR
115200050128     C                   MOVEL     ERR(3)        olv59MSG
115300941104     C                   GOTO      ENDCTR
115400941104    3C                   END
115500060511     c*
115600060511   x2c                   else
115700060516     c*
115800060511     c* Se non esistono tariffe valide, il cod tariffa impostato non
115900060511     c*  può nemmeno essere uno presente ma non valido con questo
116000060511     c*  richiamo (ad esempio se carico tariffe italia, non può
116100060511     c*  essere un cod tariffa per l'estero)
116200060516     c                   eval      Errore=' '
116300060516     c                   z-add     1             b
116400060516     C     olv59CTR      LOOKUP    CTcli(b)                               30
116500060516     c
116600060516    3c                   if        *in30
116700060516    4c                   if        ficli(b)=ilv59fie              or
116800060516     c                             ilv59fie='M' and mdcli(b)='M'  or
116900060516     c                             ilv59fie='N' and mdcli(b)='N'  or
117000060516     c                             ilv59fie='S' and ficli(b)<>'I' or
117100160215     c                             ilv59fie='J' and ficli(b)<>'E' or
117200160215     c                             ilv59fie='X' and (ficli(b)='E' or
117300160215     c                             ficli(b)='D')
117400060516     c* Network corrisponde: controllo se bloccata
117500060516    5c                   if        bacli(b)='B' and ilv59ta2='B'
117600060516     c                             and vacli(b)<>'A'
117700060516     c* se ok imposto i campi del progressino e della divisa
117800060516     C                   MOVEL     PGcli(b)      olv59PRG
117900060516     C                   MOVEL     DIcli(b)      olv59DIV
118000060516    3C     olv59DIV      IFEQ      '   '
118100060516     C                   MOVEL     'ITL'         olv59DIV
118200060516    3C                   ENDIF
118300060516     c                   else
118400060516     c                   eval      errore='S'
118500060516    5c                   endif
118600060516   x4c                   else
118700060516     c                   eval      errore='S'
118800060516    4c                   endif
118900060516    3c                   endif
119000060511     c
119100060516    3c                   if        Errore='S'
119200060511     C                   MOVEL     '5'           olv59ERR
119300060516    4c                   select
119400060511     c                   when      ilv59tbo='P'
119500060511     C                   MOVEL     ERR(5)        olv59MSG
119600060511     c                   when      ilv59tbo='A'
119700060511     C                   MOVEL     ERR(6)        olv59MSG
119800060511     c                   other
119900060511     C                   MOVEL     ERR(7)        olv59MSG
120000060516    4c                   endsl
120100060511     c
120200060511     C                   GOTO      ENDCTR
120300060516    3c                   endif
120400060511     c
120500060516    2C                   END
120600941104    1C                   END
120700941104     C     ENDCTR        ENDSR
120800941107     C*
120900941107     C* CONTROLLO FORMALE CODICE TARIFFA -----------------------------*
121000941107     C     CTRFOR        BEGSR
121100941107     C*
121200050128     C                   MOVEL     wcontrCTR     UNOCTR            1
121300941107     C                   Z-ADD     1             C
121400941107     C     UNOCTR        LOOKUP    CTA(C)                                 30
121500941107     C*
121600050128    1c     *IN30         IFEQ      *OFF
121700941118     C     UNOCTR        OREQ      ' '
121800050127     C                   MOVEL     ERR(2)        olv59MSG
121900050127     C                   MOVEL     '2'           olv59ERR
122000050128    1C                   ENDIF
122100941107     C*
122200050127    1C     olv59ERR      IFEQ      *BLANKS
122300941107     C*
122400050127     C                   MOVEL     DTA(C)        olv59DFS
122500941107     C* CONTROLLO CHE LA 2 E 3 CIFRA SIANO UN NUERO
122600050128     C                   MOVE      wcontrCTR     W002A             2
122700941107     C                   MOVEL     W002A         W001A
122800941107     C*
122900941107    2C                   DO        2
123000941107    3C     W001A         IFNE      '0'
123100941107     C     W001A         ANDNE     '1'
123200941107     C     W001A         ANDNE     '2'
123300941107     C     W001A         ANDNE     '3'
123400941107     C     W001A         ANDNE     '4'
123500941107     C     W001A         ANDNE     '5'
123600941107     C     W001A         ANDNE     '6'
123700941107     C     W001A         ANDNE     '7'
123800941107     C     W001A         ANDNE     '8'
123900941107     C     W001A         ANDNE     '9'
124000050127     C                   MOVEL     ERR(4)        olv59MSG
124100050127     C                   MOVEL     '4'           olv59ERR
124200941107    3C                   ENDIF
124300941107     C*
124400941107     C                   MOVE      W002A         W001A
124500941107    2C                   ENDDO
124600941107    1C                   ENDIF
124700941107     C*
124800941107     C                   ENDSR
124900941104     C*
125000981006     C* SBR INIZIO ---------------------------------------------------*
125100941117     C     *INZSR        BEGSR
125200981006      *
125300981006      * ACCESSO   TABEL
125400981006      *
125500941104     C     KTAB3         KLIST
125600941104     C                   KFLD                    CODUT
125700941104     C                   KFLD                    COD               2
125800981006      *
125900941110     C                   Z-ADD     1             CODUT             1 0
126000981006     C***
126100941104     C* CARICO TABELLA TR : TIPI TARIFFA
126200941104     C***
126300981006     C                   OPEN      TABEL00F
126400981006      *
126500941104     C                   MOVEL     'TR'          COD
126600941104     C                   Z-ADD     1             C
126700941104     C     KTAB3         SETLL     TABEL
126800941104     C     KTAB3         READE     TABEL                                  30
126900941104     C*
127000941104     C     *IN30         DOWEQ     '0'
127100941104     C     TBLFLG        IFEQ      ' '
127200941104     C                   MOVEL     TBLKEY        CTA(C)
127300941104     C                   MOVEL     TBLUNI        DTA(C)
127400941104     C                   ADD       1             C
127500941104     C                   ENDIF
127600941104     C*
127700941104     C     KTAB3         READE     TABEL                                  30
127800941104     C                   ENDDO
127900941104     C*
128000941104     C                   CLOSE     TABEL00F
128100941117     C*
128200941117     C                   CLEAR                   WKSC
128300941117     C                   CLEAR                   WFIE
128400941117     C                   CLEAR                   WTBO
128500050127     c*
128600050127      * Carico tabella DFE
128700050127     c                   Clear                   DDfe
128800050127     c                   Clear                   Tibs02Ds
128900050127     c                   Eval      T02Mod = 'C'
129000050127     c                   Clear                   T02Sif
129100050127     c                   Eval      T02Cod = 'DFE'
129200050127     c                   Movel(p)  'FED'         T02Ke1
129300050127     c                   Call      'TIBS02R'
129400050127     c                   Parm                    Kpjba
129500050127     c                   Parm                    Tibs02Ds
129600050127     c                   If        T02err = *Blanks
129700050127     c                   Movel     T02Uni        DDfe
129800050127     c                   EndIf
129900941104     C*
130000941104     C                   ENDSR
130100941107     C*
130200941107     C* RIEMPO I CAMPI VIDEO-----------------------------------------*
130300941107     C     VISTAR        BEGSR
130400941107     C     CTV(1)        IFNE      *BLANKS
130500050127     C                   MOVEL     CTV(1)        OLV59CT2
130600050127     C                   MOVEL     SPV(1)        OLV59ES2
130700050127     C                   MOVE      Fiv(1)        OLV59ES2
130800941107     C* SE ESISTE ALMENO UNA TARIFFA PER IL CLIENTE ANCHE SE SCADUTA
130900941107     C*  LO SEGNALO
131000050127     C                   MOVEL     'S'           OLV59TKS
131100941107     C                   ENDIF
131200941107     C*
131300941107     C     CTV(2)        IFNE      *BLANKS
131400050127     C                   MOVEL     CTV(2)        OLV59CT3
131500050127     C                   MOVEL     SPV(2)        OLV59ES3
131600050127     C                   MOVE      Fiv(2)        OLV59ES3
131700941107     C                   ENDIF
131800941107     C     CTV(3)        IFNE      *BLANKS
131900050127     C                   MOVEL     CTV(3)        OLV59CT4
132000050127     C                   MOVEL     SPV(3)        OLV59ES4
132100050127     C                   MOVE      Fiv(3)        OLV59ES4
132200941107     C                   ENDIF
132300941107     C     CTV(4)        IFNE      *BLANKS
132400050127     C                   MOVEL     CTV(4)        OLV59CT5
132500050127     C                   MOVEL     SPV(4)        OLV59ES5
132600050127     C                   MOVE      Fiv(4)        OLV59ES5
132700941107     C                   ENDIF
132800050128     c
132900050128     c* Carico le schiere di output
133000050128     c                   if        ctv(1)<>*blanks
133100050128     c                   movea     ctv           olv59CTT
133200050128     c                   movea     pgv           olv59pgT
133300050128     c* Attributi
133400050128     c                   movea     fiv           olv59at1
133500050128     c                   movea     spv           olv59at2
133600050128     c                   movea     bav           olv59at3
133700050128     c                   movea     tpv           olv59at4
133800050128     c                   movea     vav           olv59at5
133900050128     c                   endif
134000050128     c
134100941107     C                   ENDSR
134200941118**
134300941104Per il codice cliente indicato e' impossibile eseguire la ricerca tariffe
134400941104Tipo Tariffa errato
134500060516Codice Tariffa inesistente o non utilizzabile per il cliente indicato
134600941118Codice tariffa errato
134700060511Codice tariffa esistente ma non utilizzabile in Partenza e/o x questo Network
134800060511Codice tariffa esistente ma non utilizzabile in Arrivo e/o x questo Network
134900060511Codice tariffa esistente ma bloccato o non utilizzabile per questo Network
