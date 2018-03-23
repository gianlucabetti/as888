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
005500060516     d Errore          s              1
005600050124     d CNo             s                   like(C)
005700050128     d WcontrCTR       s                   like(Ilv59CTR)
005800050128     d Wblo            s                   like(Ilv59blo)
005900050128     d Wval            s                   like(Ilv59val)
006000050128     d Wta2            s                   like(Ilv59ta2)
006100050128     d Wta3            s                   like(Ilv59ta3)
006200050128     d Wta4            s                   like(Ilv59ta4)
006300050128     d Wta5            s                   like(Ilv59ta5)
006400941104      *
006500050127     D* DS PER FNLV59R - PER CARICAMENTO TARIFFE
006600050127     D fnlv59ds      E DS
006700021115
006800021115      * Ds per confronto dati tariffe FedEx
006900021115     d ddfe          e ds
007000021115     d Tibs02Ds      e ds
007100021115
007200990930     D DSTA01        E DS
007300941104     D* PASSO IN SCELTA COD TARIFFA DI TARIFFA/OFFERTA - TNTA36R -
007400941104     D PARAM4          DS
007500941104     D  PA4COD                 6     12  0
007600941104     D  PA4CTR                13     15
007700941104     D  PA4FLG                16     16
007800941104     D  PA4VPR                17     17
007900941104     D  PA4CCC                18     21  0
008000941104     D  PA4DCV                22     36
008100061220     d  pa4prg                37     39
008200061220     d  pa4dtr                40     47
008300941104     D KPJBA         E DS
008400941104     C*---------------------------------------------------------------*
008500941104     C     *ENTRY        PLIST
008600050128     C                   PARM                    fnlv59ds
008700941104     C*---------------------------------------------------------------*
008800050128     C* TIPO LANCIO, ilv59TLA "C" -           CHIUSO CON LR
008900050128     C* TIPO LANCIO, ilv59TLA "L" - ELABORO E CHIUDO CON LR
009000050128     C* TIPO LANCIO, ilv59TLA " " - ELABORO E CHIUDO IN RETRN
009100941104     C*
009200050128    1C     ilv59tla      IFNE      'C'
009300941107     C*
009400941107     C* PULIZIA CAMPI DI OUTPUT
009500050127     C                   CLEAR                   olv59mSG
009600050127     C                   CLEAR                   olv59ERR
009700050127     C                   CLEAR                   olv59DFS
009800050127     C                   CLEAR                   olv59PRG
009900050127     C                   CLEAR                   olv59DIV
010000050128     C                   CLEAR                   olv59CTR
010100050128     c
010200050128     C                   MOVEL     *BLANKS       olv59CT2
010300050128     C                   MOVEL     *BLANKS       olv59CT3
010400050128     C                   MOVEL     *BLANKS       olv59CT4
010500050128     C                   MOVEL     *BLANKS       olv59CT5
010600050128     C                   MOVEL     *BLANKS       olv59ES2
010700050128     C                   MOVEL     *BLANKS       olv59ES3
010800050128     C                   MOVEL     *BLANKS       olv59ES4
010900050128     C                   MOVEL     *BLANKS       olv59ES5
011000050128     c
011100050128     C                   MOVEL     *BLANKS       olv59CTT
011200050128     C                   MOVEL     *BLANKS       olv59pgT
011300050128     C                   MOVEL     *BLANKS       olv59AT1
011400050128     C                   MOVEL     *BLANKS       olv59AT2
011500050128     C                   MOVEL     *BLANKS       olv59AT3
011600050128     C                   MOVEL     *BLANKS       olv59AT4
011700050128     C                   MOVEL     *BLANKS       olv59AT5
011800050128     C                   MOVEL     *BLANKS       olv59ATt
011900941107     C*
012000941117     C* SOLO CONTROLLO FORMALE
012100050128    2C     ilv59CFO      IFEQ      'S'
012200050128     c                   eval      wcontrCTR=Ilv59CTR
012300941117     C                   EXSR      CTRFOR
012400941117     C*
012500941117   X2C                   ELSE
012600941117     C*
012700160223     c* se modoficato: KSC  e DSP --> rileggo e ricarico tutte le tariffe
012800050127    3C     ilv59ksc      IFNE      WKSC
012900160223     C     ilv59dsp      ORNE      WDSP
013000160223     c                   exsr      CARCTR
013100160223    3c                   endif
013200160223     c
013300160223     c* negli altri casi devo solo rileggere e ricaricare dalle sckiere
013400160223     c* escludo 8888 e 9999
013500160223    3C     ilv59ksc      IFNE      WKSC
013600160223     C     ilv59dsp      ORNE      WDSP
013700050127     C     ilv59fie      ORNE      WFIE
013800050127     C     ilv59tbo      ORNE      WTBO
013900050128     C     ilv59blo      ORNE      Wblo
014000050128     C     ilv59VAL      ORNE      WVAL
014100060515     C**** ilv59ta2      ORNE      Wta2
014200160223     C**** ilv59ta3      ORNE      Wta3
014300160428     C**** ilv59ta4      ORNE      Wta4
014400050128     C     ilv59ta5      ORNE      Wta5
014500160223     c* non lo faccio per 8888/9999
014600160223     c                   if        wccm<>8888 and wccm<>9999
014700160223     C                   EXSR      SELCTR_ntw
014800160223     c                   endif
014900160223     c
015000941231     C                   ELSE
015100941231     C                   EXSR      VISTAR
015200941231    3C                   ENDIF
015300160223    c
015400050127     C                   MOVEL     ilv59ksc      WKSC              7 0
015500050127     C                   MOVEL     ilv59dsp      WDSP              8 0
015600050127     C                   MOVEL     ilv59fie      WFIE              1
015700050127     C                   MOVEL     ilv59tbo      WTBO              1
015800050128     C                   MOVEL     ilv59blo      WBLO
015900050128     C                   MOVEL     ilv59val      WVAL
016000060515     C****               MOVEL     ilv59ta2      WTA2
016100160223     C****               MOVEL     ilv59ta3      WTA3
016200160428     C****               MOVEL     ilv59ta4      WTA4
016300050128     C                   MOVEL     ilv59ta5      WTA5
016400941104     C*
016500050127    3C     ilv59ric      IFEQ      'S'
016600941104     C                   EXSR      RICTAR
016700160428    3C                   else
016800160428
016900160428     c* Se Ilv59TA4 è impostato, verifico se trovo tariffa SOLO
017000160428     c* per il tipo servizio
017100160428     c     ilv59ta4      ifeq      'S'
017200160428     c                   exsr      RICTAR_TSP
017300160428     c                   endif
017400160428     c                   endif
017500941104     C*
017600050127    3C     ilv59con      IFEQ      'S'
017700050128     c                   if        olv59ctr=*blanks
017800050128     c                   eval      olv59ctr=ilv59ctr
017900050128     c                   endif
018000050128     c
018100941104     C                   EXSR      CTRCTR
018200941215    3C                   ENDIF
018300941215     C*
018400050128     C* DECODIFICA FISSA TARIFFA se non ancora messa
018500050127     C     olv59CTR      IFNE      *BLANKS
018600050202     C     olv59dfs      andEQ     *BLANKS
018700941215     C                   Z-ADD     1             C
018800050127     C                   MOVEL     olv59ctr      UNOCTR            1
018900941215     C     UNOCTR        LOOKUP    CTA(C)                                 30
019000050127     C   30              MOVEL     DTA(C)        olv59dfs
019100941215     C                   ENDIF
019200941215     C*
019300941117    2C                   ENDIF
019400941107     C*
019500941104    1C                   ENDIF
019600941104     C* CHIUSURA PGM
019700050128    1C     ilv59TLA      IFEQ      ' '
019800941104     C                   RETURN
019900941104     C                   ELSE
020000941104     C                   SETON                                        LR
020100941104    1C                   ENDIF
020200050218     c*
020300050218     C* CARICO LE TARIFFE DEL CLIENTE --------------------------------*
020400050218     C     CARCTR        BEGSR
020500050218     C*
020600160225     C* PULISCO tutte le SKIERE DELLE TARIFFE
020700050218     c                   Clear                   Ctcli
020800050218     c                   Clear                   Tpcli
020900050218     c                   Clear                   Spcli
021000050218     C                   CLEAR                   PGcli
021100050218     C                   CLEAR                   DIcli
021200050218     C                   CLEAR                   FIcli
021300050218     c                   Clear                   mdcli
021400050218     c                   Clear                   bacli
021500050218     c                   Clear                   vacli
021600160225     C*
021700160225     C                   CLEAR                   CTV
021800160225     c                   Clear                   Ctw
021900160225     C                   CLEAR                   TPV
022000160225     c                   Clear                   Tpw
022100160225     C                   CLEAR                   SPV
022200160225     c                   Clear                   Spw
022300160225     C                   CLEAR                   PGV
022400160225     C                   CLEAR                   DIV
022500160225     C                   CLEAR                   FIV
022600160225     C                   CLEAR                   FIw
022700160225     c                   Clear                   mdv
022800160225     c                   Clear                   bav
022900160225     c                   Clear                   bavdop
023000160225     c                   Clear                   vav
023100160225     c                   Clear                   w_val             1
023200160225     c                   Clear                   TIPOVAL           1
023300050218     C*
023400050218     C                   Z-ADD     0             B                 3 0
023500050218     C                   Z-ADD     0             C                 3 0
023600050218     C                   Z-ADD     0             D                 3 0
023700050218     C*
023800050218     C* SE 8888 O 9999 NON CARICO NULLA
023900050218     C                   MOVE      ilv59KSC      WCCM              4 0
024000050218     C*
024100050218    1C     WCCM          IFNE      8888
024200050218     C     WCCM          ANDNE     9999
024300050218     C*
024400050218     C     ilv59ksc      SETLL     TNTAM000
024500050218     C     ilv59ksc      READE     TNTAM000                               30
024600050218     C*
024700050218    2C     *IN30         DOWEQ     *OFF
024800050218     C* ESCLUDO SE ANNULLATO
024900050218    3C     TAMATB        IFNE      'A'
025000050218     c
025100050218     C                   MOVEL     TAMFLO        DSTA01
025200050218     C*
025300050218     c                   Move      TamCtr        Wctr
025400050218      * Se Tamfie "M" Solo Tariffe merci FedEx
025500160223    5c**                 If        ilv59Fie = 'M' and Wctr >= §DfeDalD
025600160223     c**                 GoTo      Notartutto
025700160223    5c**                 EndIf
025800050218      * Se Tamfie "N" Solo Tariffe documenti FedEx
025900160223    5c**                 If        ilv59Fie = 'N' and Wctr < §DfeDalD
026000160223     c**                 GoTo      Notartutto
026100160223    5c**                 EndIf
026200160223     c**
026300050218     c* Elaboro solo tariffe decorrenti o scadute (escludo da decorrere
026400050218     c*  come se non ci fossero)
026500060503    5c***                if        ilv59dsp>=tamddt
026600060503     c* 03/5/06 ES --> preferisco caricare tutto. Se un tariffa ancora
026700060503     c*                da decorrere è già stata immessa, meglio
026800060503     c*                non farla usare in modo improprio, salvo
026900060503     c*                che non sia anche presente come decorrente:allor
027000060503     c*                la ignoro: meglio la decorrente
027100050218     c
027200050218     C                   MOVEL     TAMCTR        COMCT2            3
027300050218     C                   Z-ADD     1             D
027400050218     C     COMCT2        LOOKUP    CTcli(D)                               31
027500060503    6c                   if        (*in31 and ilv59dsp>=tamddt) or
027600050218     c                             (not *in31 and c < 50)
027700050218     C*
027800160223     C* DELLE TARIFFE già presenti, tengo sempre l'ultima, escluse le da
027900060511     c*  decorrere
028000050218    7C     *IN31         IFEQ      *OFF
028100050218     C                   ADD       1             C
028200050218     C                   MOVEL     TAMCTR        CTcli(C)
028300050218     C                   Z-ADD     C             D
028400050218    7C                   ENDIF
028500050218     C*
028600050218     C* VEDO SE E' TARIFFA decorrente
028700060503     c                   select
028800160223    7C     ilv59dsp      whenge    TAMDDT
028900050218     C     ilv59dsp      ANDLE     TAMDST
029000060503     c* Decorrente
029100050218     c                   movel     'D'           vacli(d)
029200060503    7C     ilv59dsp      whenlT    TAMDDT
029300060503     c* dA decorrere
029400060503     c                   movel     'A'           vacli(d)
029500060503     c
029600060503     c                   other
029700060503     c* Scaduta
029800050218     c                   movel     'S'           vacli(d)
029900060511    7c                   endsl
030000050218     C*
030100050218     C                   MOVEL     TAMCTR        CTcli(d)
030200050218     C                   MOVEL     TAMTSP        SPcli(d)
030300050218     C                   MOVE      TAMFTP        TPcli(d)
030400050218     C                   MOVE      TAMPRG        PGcli(d)
030500050218     C                   MOVE      §TADIV        DIcli(d)
030600050218     C                   MOVE      TAMFIE        FIcli(d)
030700050218     C                   MOVE      TAMbap        bacli(d)
030800050218     C* TARIFFA DPD
030900050218     C     §TADPD        IFEQ      'S'
031000050218     C                   MOVE      'D'           FIcli(d)
031100050218     C                   ENDIF
031200050218     C* TARIFFA FEDEX
031300050218    7C     §TAFED        IFEQ      'S'
031400050218     C                   MOVE      'F'           FIcli(d)
031500050218     c                   Clear                   mdcli(d)
031600160223     c******             If        ilv59Fie = 'M' and Wctr >= §DfeDalM
031700160223     c                   If        Wctr >= §DfeDalM
031800050218     c                                            and Wctr <= §DfeAlM
031900050218     c                   Eval      mdcli(d) = 'M'
032000050218     c                   EndIf
032100160223     c********           If        ilv59Fie = 'N' and Wctr >= §DfeDalD
032200160223     c                   If        Wctr >= §DfeDalD
032300050218     c                                            and Wctr <= §DfeAlD
032400050218     c                   Eval      mdcli(d) = 'N'
032500050218     c                   EndIf
032600050218    7c                   EndIf
032700050218    6C                   ENDIF
032800060511    5C*********          ENDIF
032900060511    4C*********          ENDIF
033000050218    3C                   ENDIF
033100050218
033200060511     c     Notartutto    Tag
033300050218
033400050218     C*
033500050218     C  N30ilv59ksc      READE     TNTAM000                               30
033600050218     C*
033700050218    2C                   ENDDO
033800160223     c
033900050218    1c                   endif
034000050218     c
034100050218     C                   ENDSR
034200160223     C*
034300160223     C* SELEZIONO le tariffe del cliente in base a quanto passato in DS ---*
034400160223     C     SELCTR_ntw    BEGSR
034500160223     C* PULISCO SKIERE DELLE TARIFFE
034600160223     C                   CLEAR                   CTV
034700160223     c                   Clear                   Ctw
034800160223     C                   CLEAR                   TPV
034900160223     c                   Clear                   Tpw
035000160223     C                   CLEAR                   SPV
035100160223     c                   Clear                   Spw
035200160223     C                   CLEAR                   PGV
035300160223     C                   CLEAR                   DIV
035400160223     C                   CLEAR                   FIV
035500160223     C                   CLEAR                   FIw
035600160223     c                   Clear                   mdv
035700160223     c                   Clear                   bav
035800160223     c                   Clear                   bavdop
035900160223     c                   Clear                   vav
036000160223     c                   Clear                   w_val             1
036100160223     c                   Clear                   TIPOVAL           1
036200160223     C                   MOVEL     *BLANKS       olv59TKS
036300160223     c* rileggo le schiere e carico in base ai parametri passati
036400160223     c                   z-add     1             xx                4 0
036500160223     c                   clear                   B
036600160223     c
036700160223     c* 1) ELABORO PRIMA LE DECORRENTI
036800160223     C*     solo se richiesto
036900160223    6c                   if        ilv59VAL=' ' or
037000160223     c                             ilv59VAL='D'
037100160223     C                   EVAL      TIPOVAL='D'
037200160223     C                   EXSR      CARICA_CTR
037300160223     C                   ENDIF
037400160223
037500160223     C* 2) POI ELABORO LE SCADUTE
037600160223     C*     solo se richiesto
037700160223    6c                   if        ilv59VAL=' ' or
037800160223     c                             ilv59VAL='S'
037900160223     c                   z-add     1             xx
038000160223     c* caricata almeno una valida
038100160223     c                   if        b>0
038200160223     c                   eval      w_val='S'
038300160223     c                   endif
038400160223     c
038500160223     C                   EVAL      TIPOVAL='S'
038600160223     C                   EXSR      CARICA_CTR
038700160223     C                   ENDIF
038800160223
038900160223     c                   exsr      VISTAR
039000160223     c                   ENDSR
039100160223     C*
039200160223     C* -------------------------------------------------------------------*
039300160223     C     CARICA_CTR    BEGSR
039400160223
039500160223    1c                   dow       ctcli(xx)<>*blanks and B<50
039600160223     c* carico il tipo di validità richiesto
039700160223    2c                   if        vacli(xx) =TIPOVAL
039800160223
039900160223     C* ilv59BLO:  B=solo bloccate " "=solo non bloccate  S=tutte
040000160223    3c                   if         ilv59BLO='S' or
040100160223     c                             (ilv59BLO='B' and bacli(xx)='B') or
040200160223     c                             (ilv59BLO=' ' and bacli(xx)<>'B')
040300160223
040400160223     C* CARICO SOLO TARIFFE ITALIA           --> FIE "I"
040500160223     C* CARICO SOLO TARIFFE Euroexpress      --> FIE "E"
040600160223     C* CARICO SOLO TARIFFE DPD              --> FIE "D"
040700160223     C* CARICO SOLO TARIFFE FEDEX            --> FIE "F"
040800160223     C* CARICO      TARIFFE DPD+FEDEX+EUROEX --> FIE "S"
040900160223     C* CARICO      TARIFFE DPD+FEDEX+ITALIA --> FIE "J"
041000160223     C* CARICO SOLO TARIFFE FEDEX DOCUMENTI  --> FIE "N"
041100160223     C* CARICO SOLO TARIFFE FEDEX MERCI      --> FIE "M"
041200160223     C* CARICO      TARIFFE DPD+EUROEX       --> FIE "X"
041300160223    4c                   if        ficli(xx)=' ' or
041400160223     c                             ficli(xx)=ilv59fie or
041500160223     c                             (ilv59fie='X' and ficli(XX)='D') or
041600160223     c                             (ilv59fie='X' and ficli(XX)='E') or
041700160223     c                             (ilv59fie='S' and ficli(xx)<>'I') or
041800160223     c                             (ilv59fie='J' and ficli(xx)<>'E') or
041900160223     c                             (ilv59fie='M' and mdcli(xx)= 'M') or
042000160223     c                             (ilv59fie='N' and mdcli(xx)= 'N')
042100160223     c
042200160223     C* ilv59TBO P=carico tariffe P/B/" "
042300160223     c*          A=carico tariffe A/B/" "   " "=tutte
042400160223    5c                   if        ilv59tbo=' ' or
042500160223     c                             bacli(xx)=' ' or
042600160223     c                             bacli(xx)='B' or
042700160223     c                             ilv59tbo=bacli(xx)
042800160223     c
042900160223     C                   ADD       1             B
043000160223     C                   MOVEL     ctcli(xx)     CTV(B)
043100160223     C                   MOVEL     spcli(xx)     SPV(B)
043200160223     c* se caricata almeno una valida non tengo conto della tariffa Prefer
043300160223     c*  per le non valide
043400160223     c                   if        w_val='S'
043500160223     c                   clear                   TPV(B)
043600160223     c                   else
043700160223     C                   MOVE      tpcli(XX)     TPV(B)
043800160223     c                   endif
043900160223     C                   MOVE      pgcli(xx)     PGV(B)
044000160223     C                   MOVE      dicli(xx)     DIV(B)
044100160223     C                   MOVE      ficli(xx)     FIV(B)
044200160223     C                   MOVE      bacli(xx)     BAV(B)
044300160223     c
044400160223     c* Carico il campo bap anche mettendo al posto di " " --> "E"
044500160223     c                   if        bacli(XX)=' ' or bacli(xx)='B'
044600160223     c                   movel     'E'           bavdop(b)
044700160223     c                   else
044800160223     C                   MOVE      bacli(xx)     BAVdop(B)
044900160223     c                   endif
045000160223     c* Validità tariffa
045100160223     C                   MOVE      vacli(xx)     VAV(B)
045200160223     c
045300160223     c* Fedex merci/documenti
045400160223     c                   if        ficli(XX)='F'
045500160223     c                   move      mdcli(XX)     MDV(b)
045600160223     c                   endif
045700160223     c
045800160223    5c                   endif
045900160223    4c                   endif
046000160223    3c                   endif
046100160223    2c                   endif
046200160223     c
046300160223     c                   add       1             xx
046400160223    1c                   enddo
046500160223     c
046600160223     C                   ENDSR
046700941104     C*
046800050218     C* SELEZIONO le tariffe del cliente in base a quanto passato in DS ---*
046900160223     C**   SELCTR        BEGSR
047000160223     C**
047100941104     C* PULISCO SKIERE DELLE TARIFFE
047200160223     C**                 CLEAR                   CTV
047300160223     C**                 CLEAR                   CTN
047400160223     c**                 Clear                   Ctw
047500160223     C**                 CLEAR                   TPN
047600160223     C**                 CLEAR                   TPV
047700160223     c**                 Clear                   Tpw
047800160223     C**                 CLEAR                   SPN
047900160223     C**                 CLEAR                   SPV
048000160223     c**                 Clear                   Spw
048100160223     C**                 CLEAR                   PGV
048200160223     C**                 CLEAR                   PGN
048300160223     C**                 CLEAR                   DIV
048400160223     C**                 CLEAR                   DIN
048500160223     C**                 CLEAR                   FIV
048600160223     C**                 CLEAR                   FIN
048700160223     C**                 CLEAR                   FIw
048800160223     c**                 Clear                   mdv
048900160223     c**                 Clear                   mdn
049000160223     c**                 Clear                   bav
049100160223     c**                 Clear                   ban
049200160223     c**                 Clear                   bavdop
049300160223     c**                 Clear                   bandop
049400160223     c**                 Clear                   vav
049500160223     C**
049600160223     C**                 MOVEL     *BLANKS       olv59TKS
049700160223     C**
049800160223     C**MITTENTI CODIFICATI --> CERCO LE TARIFFE
049900160223     C**                 Z-ADD     0             B                 3 0
050000160223     C**                 Z-ADD     0             C                 3 0
050100160223     C**                 Z-ADD     0             D                 3 0
050200160223     C**
050300160223     C**SE 8888 O 9999 NON CARICO NULLA
050400160223     C**                 MOVE      ilv59KSC      WCCM              4 0
050500160223     C**
050600160223    1C**   WCCM          IFNE      8888
050700160223     C**   WCCM          ANDNE     9999
050800160223     C**
050900160223     C**   ilv59ksc      SETLL     TNTAM000
051000160223     C**   ilv59ksc      READE     TNTAM000                               30
051100160223     C**
051200160223    2C**   *IN30         DOWEQ     *OFF
051300160223     C**ESCLUDO SE ANNULLATO
051400160223    3C**   TAMATB        IFNE      'A'
051500160223     c**
051600160223     C**                 MOVEL     TAMFLO        DSTA01
051700160223     C**
051800160223    5C**   TAMFIE        IFEQ      ' '
051900160223     C**
052000160223     C**   TAMFIE        OREQ      ilv59fie
052100160223     C**   §TADPD        ANDEQ     ' '
052200160223     C**   §TAFED        ANDEQ     ' '
052300160223     C**SE TARIFFA DPD NON DEVO CARICARE  SE RICHIESTO I/E/F/M/N
052400160223     C**   §TADPD        OREQ      'S'
052500160223     C**   ilv59fie      ANDNE     'I'
052600160223     C**   ilv59fie      ANDNE     'E'
052700160223     C**   ilv59fie      ANDNE     'F'
052800160223     c**   ilv59fie      andne     'M'
052900160223     c**   ilv59fie      andne     'N'
053000160223     C**
053100160223     C**SE TARIFFA FEDEX NON DEVO CARICARE  SE RICHIESTO I/E/D/X
053200160223     C**   §TAFED        OREQ      'S'
053300160223     C**   ilv59fie      ANDNE     'I'
053400160223     C**   ilv59fie      ANDNE     'E'
053500160223     C**   ilv59fie      ANDNE     'D'
053600160223     C**   ilv59fie      ANDNE     'X'
053700160223     C**LE ESTERE LE DEVO CARICARE SOLO PER S (OLTRE CHE DPD E FEDEX)
053800160223     C**   TAMFIE        OREQ      'E'
053900160223     C**   ilv59fie      ANDEQ     'S'
054000160223     C**LE EEX    LE DEVO CARICARE anche per X
054100160223     C**   TAMFIE        OREQ      'E'
054200160223     C**   ilv59fie      ANDEQ     'X'
054300160223     C**   §TAFED        ANDEQ     ' '
054400160223     C**LE ITALIA LE DEVO CARICARE SOLO PER J (OLTRE CHE DPD E FEDEX)
054500160223     C**   TAMFIE        OREQ      'I'
054600160223     C**   ilv59fie      ANDEQ     'J'
054700160223     C**
054800160223     c**                 Move      TamCtr        Wctr
054900160223     C**Se Tamfie "M" Solo Tariffe merci FedEx
055000160223    6c**                 If        ilv59Fie = 'M' and Wctr >= §DfeDalD
055100160223     c**                 GoTo      Notar
055200160223    6c**                 EndIf
055300160223     C**Se Tamfie "N" Solo Tariffe documenti FedEx
055400160223    6c**                 If        ilv59Fie = 'N' and Wctr < §DfeDalD
055500160223     c**                 GoTo      Notar
055600160223    6c**                 EndIf
055700160223     C**
055800160223     C**ilv59TBO P=carico tariffe P/B/" "
055900160223     a**         A=carico tariffe A/B/" "   " "=tutte
056000160223    6C**   ilv59tbo      IFEQ      ' '
056100160223     C**   TAMBAP        OREQ      ' '
056200160223     C**   TAMBAP        OREQ      'B'
056300160223     C**   TAMBAP        OREQ      ilv59tbo
056400160223     C**
056500160223     C**VEDO SE E' TARIFFA decorrente
056600160223    7C**   ilv59dsp      IFGE      TAMDDT
056700160223     C**   ilv59dsp      ANDLE     TAMDST
056800160223     c**solo se richieste (ilv59VAL=D/" ")
056900160223    8c**                 if        ilv59VAL=' ' or ilv59val='D'
057000160223     C**
057100160223     C**                 ADD       1             B
057200160223     C**                 MOVEL     TAMCTR        CTV(B)
057300160223     C**                 MOVEL     TAMTSP        SPV(B)
057400160223     C**                 MOVE      TAMFTP        TPV(B)
057500160223     C**                 MOVE      TAMPRG        PGV(B)
057600160223     C**                 MOVE      §TADIV        DIV(B)
057700160223     C**                 MOVE      TAMFIE        FIV(B)
057800160223     C**                 MOVE      TAMbap        baV(B)
057900160223     c**Carico il campo bap anche mettendo al posto di " " --> "E"
058000160223    9c**                 if        tambap=' ' or tambap='B'
058100160223     c**                 movel     'E'           bavdop(b)
058200160223     c**                 else
058300160223     C**                 MOVE      TAMbap        baVdop(B)
058400160223    9c**                 endif
058500160223     C**                 MOVE      'D'           vaV(B)
058600160223     C**TARIFFA DPD
058700160223     C**   §TADPD        IFEQ      'S'
058800160223     C**                 MOVE      'D'           FIV(B)
058900160223     C**                 ENDIF
059000160223     C**TARIFFA FEDEX
059100160223    9C**   §TAFED        IFEQ      'S'
059200160223     C**                 MOVE      'F'           FIV(B)
059300160223     c**                 Clear                   mdv(b)
059400160223     c**                 If        ilv59Fie = 'M' and Wctr >= §DfeDalM
059500160223     c**                                          and Wctr <= §DfeAlM
059600160223     c**                 Eval      mdv(b) = 'M'
059700160223     c**                 EndIf
059800160223     c**                 If        ilv59Fie = 'N' and Wctr >= §DfeDalD
059900160223     c**                                          and Wctr <= §DfeAlD
060000160223     c**                 Eval      mdv(b) = 'N'
060100160223     c**                 EndIf
060200160223    9c**                 EndIf
060300160223     c**
060400160223     C**   B             COMP      50                                 30
060500160223   x8c**                 else
060600160223     c**Se tariffa decorrente e c'e' anche come scaduta la tolgo
060700160223     C**
060800160223     C**                 MOVEL     TAMCTR        COMCT2            3
060900160223     C**                 Z-ADD     1             D
061000160223     C**   COMCT2        LOOKUP    CTN(D)                                 31
061100160223    9c**                 if        *in31
061200160223     c**                 clear                   ctn(d)
061300160223     c**                 clear                   spn(d)
061400160223     c**                 clear                   tpn(d)
061500160223     c**                 clear                   pgn(d)
061600160223     c**                 clear                   din(d)
061700160223     c**                 clear                   fin(d)
061800160223     c**                 clear                   ban(d)
061900160223     c**                 clear                   bandop(d)
062000160223     c**                 clear                   mdn(d)
062100160223     c**Reimposto il contatore a -1
062200160223     c**                 sub       1             C
062300160223     c**
062400160223    9C**                 ENDIF
062500160223    8C**                 ENDIF
062600160223     C**
062700160223     C**TARIFFE non decorrente
062800160223   X7C**                 ELSE
062900160223     c**        tengo solo le scadute, se richieste (ilv59VAL=S/" ")
063000160223    8c**                 if        ilv59dsp>tamdst  and
063100160223     c**                           (ilv59VAL=' ' or ilv59VAL='S')
063200941104     C*
063300160223     C**                 MOVEL     TAMCTR        COMCT2            3
063400160223     C**                 Z-ADD     1             D
063500160223     C**   COMCT2        LOOKUP    CTN(D)                                 31
063600160223    9c**                 if        *in31   or
063700160223     c**                           (not *in31 and c < 50)
063800160223     C**
063900160223     C**DELLE TARIFFE NON VALIDE, TENGO SEMPRE L'ULTIMA
064000160223   10C**   *IN31         IFEQ      *OFF
064100160223     C**                 ADD       1             C
064200160223     C**                 MOVEL     TAMCTR        CTN(C)
064300160223     C**                 Z-ADD     C             D
064400160223   10C**                 ENDIF
064500160223     C**                 MOVEL     TAMTSP        SPN(D)
064600160223     C**                 MOVE      TAMFTP        TPN(D)
064700160223     C**                 MOVE      TAMPRG        PGN(D)
064800160223     C**                 MOVE      §TADIV        DIN(D)
064900160223     C**                 MOVE      TAMFIE        FIN(D)
065000160223     C**                 MOVE      TAMbap        ban(D)
065100160223     c**Carico il campo bap anche mettendo al posto di " " --> "E"
065200160223     c**                 if        tambap=' '   or  tambap='B'
065300160223     c**                 movel     'E'           bandop(D)
065400160223     c**                 else
065500160223     C**                 MOVE      TAMbap        bandop(D)
065600160223     c**                 endif
065700160223     C**TARIFFA DPD
065800160223     C**   §TADPD        IFEQ      'S'
065900160223     C**                 MOVE      'D'           FIN(D)
066000160223     C**                 ENDIF
066100160223     C**TARIFFA FEDEX
066200160223   10C**   §TAFED        IFEQ      'S'
066300160223     C**                 MOVE      'F'           FIN(D)
066400160223     c**                 Clear                   mdn(d)
066500160223     c**                 If        ilv59Fie = 'M' and Wctr >= §DfeDalM
066600160223     c**                                          and Wctr <= §DfeAlM
066700160223     c**                 Eval      mdn(d) = 'M'
066800160223     c**                 EndIf
066900160223     c**                 If        ilv59Fie = 'N' and Wctr >= §DfeDalD
067000160223     c**                                          and Wctr <= §DfeAlD
067100160223     c**                 Eval      mdn(d) = 'N'
067200160223     c**                 EndIf
067300160223   10C**                 ENDIF
067400160223    9C**                 ENDIF
067500160223     C**
067600160223    8C**                 ENDIF
067700160223    7C**                 ENDIF
067800160223    6C**                 ENDIF
067900160223    5C**                 ENDIF
068000160223    4C**                 ENDIF
068100160223    3C**                 ENDIF
068200160223     C**
068300160223     c**   Notar         Tag
068400160223     C**
068500160223     C**
068600160223     C**N30ilv59ksc      READE     TNTAM000                               30
068700160223     C**
068800160223    2C**                 ENDDO
068900160223     C**
069000160223     C**                 ADD       1             B
069100160223     C**
069200160223     C**CARICO TUTTI I TIPI TARIFFA TROVATI IN UNICA SCHIERA
069300160223     C**SE NON NE HO GIA' TROVATI 50 VALIDI
069400160223    2C**   B             IFLE      50
069500160223     C**
069600160223     C**
069700160223     C**SE ESISTE ALMENO UNA TARIFFA VALIDA, NON CONSIDERO LA 'P'
069800160223     C** DI PREFERENZIALE PER LE TARIFFE SCADUTE
069900160223    3C**   B             IFGT      1
070000160223     C**                 CLEAR                   TPN
070100160223    3C**                 ENDIF
070200160223     C**
070300160223     C**                 Z-ADD     1             C
070400160223     C**
070500160223    3C**   CTN(C)        DOWNE     *BLANKS
070600160223     C**   B             ANDLE     50
070700160223     C**
070800160223     C**   CTN(C)        LOOKUP    CTV                                    31
070900160223    4C**   *IN31         IFEQ      *OFF
071000160223     C**                 MOVEL     CTN(C)        CTV(B)
071100160223     C**                 MOVEL     TPN(C)        TPV(B)
071200160223     C**                 MOVEL     SPN(C)        SPV(B)
071300160223     C**                 MOVEL     PGN(C)        PGV(B)
071400160223     C**                 MOVEL     DIN(C)        DIV(B)
071500160223     C**                 MOVEL     FIN(C)        FIV(B)
071600160223     c**                 Movel     mdn(c)        mdv(b)
071700160223     c**                 Movel     ban(c)        bav(b)
071800160223     c**                 Movel     bandop(c)     bavdop(b)
071900160223     c**S-scaduta
072000160223     c**                 movel     'S'           vav(b)
072100160223     C**                 ADD       1             B
072200160223    4C**                 ENDIF
072300160223     C**
072400160223     C**                 ADD       1             C
072500160223    3C**                 ENDdo
072600160223    2C**                 ENDif
072700160223     C**
072800160223     C**                 EXSR      VISTAR
072900160223    1C**                 ENDif
073000160223     C**
073100160223     C**                 ENDSR
073200941104     C*
073300160428     C* RICERCA E CARICAMENTO A VIDEO TARIFFE solo col tipo serivizio-*
073400160428     C     RICTAR_TSP    BEGSR
073500160428     c
073600160428     C                   SETOFF                                       30
073700160428     C                   CLEAR                   W001A
073800160428     c                   clear                   CNo
073900160428     C*
074000160428     C* SE NON CI SONO TARIFFE IMPOSTO LA TARIFFA 000
074100160428    1C     CTV(1)        IFEQ      *BLANKS
074200160428     C                   CLEAR                   olv59tks
074300160428     c
074400160428    1C     ilv59ctr      IFEQ      *BLANKS
074500160428     C                   MOVEL     *ZEROS        olv59CTR
074600160428    1C                   ENDIF
074700160428     C*
074800160428   X1C                   ELSE
074900160428      * Carico le sk di work per la scelta
075000160428     c                   ExSr      CarSk
075100160428     C*
075200160428     C* 1) SE IMPOSTATO IL TIPO SERVIZIO --> CERCO QUELLO
075300160428     c*    solo per E e H (non lo faccio per C e D)
075400160428     c                   exsr      Cerca_tsp
075500160428     C*
075600160428     c* Se trovato -->IMPOSTO CAMPO CODICE TARIFFA
075700160428    2c                   if        *in30
075800160428     C                   MOVEL     CTw(C)        Olv59CTR
075900160428     c                   else
076000160428     c* imposto quello di input
076100160428     c                   eval      olv59ctr=ilv59ctr
076200160428    2c                   endif
076300160428     c
076400160428     c                   exsr      DATI
076500160428    1c                   endif
076600160428     C*
076700160428     c                   ENDSR
076800160428     c
076900941104     C* RICERCA E CARICAMENTO A VIDEO TARIFFE ------------------------*
077000941104     C     RICTAR        BEGSR
077100941104     C                   SETOFF                                       30
077200950125     C                   CLEAR                   W001A
077300050124     c                   clear                   CNo
077400941104     C*
077500941104     C* SE NON CI SONO TARIFFE IMPOSTO LA TARIFFA 000
077600050127    1C     CTV(1)        IFEQ      *BLANKS
077700050127     C                   CLEAR                   olv59tks
077800941215     C*
077900050127     C     ilv59ctr      IFEQ      *BLANKS
078000050127     C                   MOVEL     *ZEROS        olv59CTR
078100941215     C                   ENDIF
078200941215     C*
078300050127   X1C                   ELSE
078400941107     C*
078500941107     C* CODICE TARIFFA IMPOSTATO: PRENDO IL RELATIVO PROGRESSIVO
078600050127    2C     ilv59CTR      IFNE      *BLANKS
078700050127     c                   eval      olv59ctr=ilv59ctr
078800050127    2C                   ENDIF
078900941107     C**
079000050127     c**
079100050127    2C     ilv59CTR      IFEQ      *BLANKS
079200021115
079300050127      * Carico le sk di work per la scelta
079400021115     c                   ExSr      CarSk
079500941104     C*
079600160215     C* 1) SE IMPOSTATO IL TIPO SERVIZIO --> CERCO QUELLO
079700160401     c*    slo per E e H (non lo faccio per C e D)
079800160428     c                   exsr      Cerca_tsp
079900160428
080000941104     C*
080100941104     C* SE NON TROVATA O NON IMPOSTATO IL TIPO SERVIZIO -->
080200160215     c* 2) cerco per network  se passato
080300160215     c*    solo se ho caricato più di un network, altrimenti ignoro
080400160215    3c                   if        not *in30 and ilv59TA3<>' '
080500160215     c                             and (ilv59fie='X' or ilv59fie='S' or
080600160215     c                             ilv59fie='J')
080700160215     C                   CLEAR                   W001A
080800160215     c                   clear                   CNo
080900160215     C                   Z-ADD     1             C
081000160215     C                   SETON                                        31
081100160215     C*
081200160215    4C     *IN31         DOWEQ     *ON
081300160215     C     ilv59TA3      LOOKUP    fiw(C)                                 31
081400160215     C* SE TROVATA ED E' PREFERENZIALE ESCO DAL CICLO
081500160215    5c   31              if        tpw(C)='P'
081600160215     C                   SETON                                        30
081700160215     C                   SETOFF                                       31
081800160215     C                   MOVEL     '1'           W001A             1
081900160215   X5C                   ELSE
082000160215     c
082100160215     C* SE TROVATA MA NON PREFERENZIALE ACCENDO COMUNQUE L'INDICATORE
082200160215     C*  DI TROVATO MA FACCIO UN ALTRO GIRO
082300160215     C                   SETON                                        30
082400160215     c                   if        CNo=0
082500160215     c                   z-add     C             CNo
082600160215     c                   endif
082700160215     c
082800160215     C                   ADD       1             C
082900160215    5C                   ENDIF
083000160215    4C                   ENDDO
083100160215     C*
083200160215     C* SE HO TROVATO LA TARIFFA MA NON LA PREFERENZIALE, LA RICERCO per
083300160215     C*  PRENDERE LA POSIZIONE
083400160215    4C   30W001A         IFEQ      ' '
083500160215     c                   z-add     CNo           C
083600160215    4C                   ENDIF
083700160215     C*
083800160215    3C                   ENDIF
083900160215
084000160215     C* SE NON ancora TROVATA -->
084100160215     C* 3) CERCO LA PRIMA PREFERENZIALE
084200050124    3C     *IN30         IFEQ      *OFF
084300050127     C                   Z-ADD     1             C
084400050127     C     'P'           LOOKUP    TPw(C)                                 30
084500050127    3C                   ENDIF
084600941104     C*
084700160215     C* 4) SE NON ESISTE LA PREFERENZIALE METTO LA PRIMA VALIDA TROVATA
084800941104     C  N30              Z-ADD     1             C
084900050127     c
085000050127     c* IMPOSTO CAMPO CODICE TARIFFA
085100050127     C                   MOVEL     CTw(C)        Olv59CTR
085200050127    2c                   endif
085300160428     c
085400160428     c                   exsr      DATI
085500160428     c
085600050127    1C                   ENDIF
085700941104     C*
085800941104     C                   ENDSR
085900160428      *--------------------------------------------------------------*
086000160428      * Ricerca tariffa per tipo servizio
086100160428      *--------------------------------------------------------------*
086200160428     c     Cerca_tsp     BEGSR
086300160428    3C     ilv59TSP      IFNE      *BLANKS
086400160428     c     ilv59TSP      andne     'C'
086500160428     c     ilv59TSP      andne     'D'
086600160428     C* CERCO SE C'E' LA PREFERENZIALE ALTRIMENTI LA 1 CHE TROVO
086700160428     C                   Z-ADD     1             C
086800160428     C                   SETON                                        31
086900160428     C*
087000160428    4C     *IN31         DOWEQ     *ON
087100160428     C     ilv59tSP      LOOKUP    SPw(C)                                 31
087200160428     C* SE TROVATA ED E' PREFERENZIALE ESCO DAL CICLO
087300160428    5c   31              if        tpw(C)='P'
087400160428     C                   SETON                                        30
087500160428     C                   SETOFF                                       31
087600160428     C                   MOVEL     '1'           W001A             1
087700160428   X5C                   ELSE
087800160428     C* SE TROVATA MA NON PREFERENZIALE ACCENDO COMUNQUE L'INDICATORE
087900160428     C*  DI TROVATO MA FACCIO UN ALTRO GIRO
088000160428     C                   SETON                                        30
088100160428     c                   if        CNo=0
088200160428     c                   z-add     C             CNo
088300160428     c                   endif
088400160428     c
088500160428     C                   ADD       1             C
088600160428    5C                   ENDIF
088700160428    4C                   ENDDO
088800160428     C*
088900160428     C* SE HO TROVATO LA TARIFFA MA NON LA PREFERENZIALE, LA RICERCO X
089000160428     C*  PRENDERE LA POSIZIONE
089100160428    4C   30W001A         IFEQ      ' '
089200160428     c                   z-add     CNo           C
089300160428    4C                   ENDIF
089400160428     C*
089500160428    3C                   ENDIF
089600160428     c                   ENDSR
089700160428      *--------------------------------------------------------------*
089800160428      * Finisco di caricare i dati relativi alla tariffa
089900160428      *--------------------------------------------------------------*
090000160428     c     DATI          BEGSR
090100160428     C*
090200160428      * Cerco la posizione nella sk valida
090300160428     c                   Z-Add     1             c
090400160428     c     olv59CTR      Lookup    Ctv(c)                                 30
090500160428     c
090600160428    2c                   IF        *IN30
090700160428     C                   MOVEL     PGV(C)        OLV59PRG
090800160428     C                   MOVEL     DIV(C)        OLV59DIV
090900160428     C     OLV59DIV      IFEQ      '   '
091000160428     C                   MOVEL     'ITL'         OLV59DIV
091100160428     C                   ENDIF
091200160428
091300160428   x2C                   ELSE
091400160428     C* ERRORE --> COD.TARIFFA NON TROVATO (se passato in input può
091500160428     c*            accadere)
091600160428     C                   MOVEL     '3'           olv59ERR
091700160428     C                   MOVEL     ERR(3)        olv59MSG
091800160428     C                   Z-ADD     10            C
091900160428    2C                   ENDIF
092000160428     C*
092100160428     C                   EXSR      VISTAR
092200160428     C*
092300160428     C* RIMPIAZZO CON COD TARIFFA 5 SOLO SE ESISTE
092400160428    2C     CTv(5)        IFNE      *BLANKS
092500160428     C* SE NELLA 1°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
092600160428    3C     C             IFEQ      1
092700160428     C                   MOVEL     CTv(5)        olv59CT2
092800160428     C                   MOVEL     SPv(5)        olv59ES2
092900160428     C                   MOVE      FIv(5)        olv59ES2
093000160428    3C                   END
093100160428     C* SE NELLA 2°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
093200160428    3C     C             IFEQ      2
093300160428     C                   MOVEL     CTv(5)        olv59CT3
093400160428     C                   MOVEL     SPv(5)        olv59ES3
093500160428     C                   MOVE      FIv(5)        olv59ES3
093600160428    3C                   END
093700160428     C* SE NELLA 3°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
093800160428    3C     C             IFEQ      3
093900160428     C                   MOVEL     CTv(5)        olv59CT4
094000160428     C                   MOVEL     SPv(5)        olv59ES4
094100160428     C                   MOVE      FIv(5)        olv59ES4
094200160428    3C                   END
094300160428     C* SE NELLA 4°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
094400160428    3C     C             IFEQ      4
094500160428     C                   MOVEL     CTv(5)        olv59CT5
094600160428     C                   MOVEL     SPv(5)        olv59ES5
094700160428     C                   MOVE      FIv(5)        olv59ES5
094800160428    3C                   END
094900160428     C*
095000160428    2C                   END
095100160428     c                   ENDSR
095200021115      *--------------------------------------------------------------*
095300050127      * Carico le sk di work per la proposta a video
095400021115      *--------------------------------------------------------------*
095500021115     c     Carsk         BegSr
095600021115
095700050127     c* Se richieste tariffe FEDEX e presenti sia M che N --> propon
095800050127     c*  go "N"-documenti
095900050127     c                   setoff                                       3536
096000050127     c                   setoff                                       3738
096100050127     c                   If        ilv59Fie = 'F'
096200021115     c     'M'           Lookup    mdv                                    35
096300021115     c     'N'           Lookup    mdv                                    36
096400021115     c                   EndIf
096500050127     c
096600050127     c* Se presenti sia tariffe "P" che "A" --> propongo la P o la A
096700050127     c*    in base alla richiesta (ilv59PRP)
096800050128    1c                   if        ilv59tbo=' '  and ilv59PRP<>' '
096900050127     c     'E'           Lookup    bavdop                                 37
097000050128    2c                   if        *in37
097100050127     c                   seton                                        38
097200050128   x2c                   else
097300050127     c     'P'           Lookup    bavdop                                 37
097400050127     c     'A'           Lookup    bavdop                                 38
097500050128    2c                   endif
097600050128    1c                   endif
097700021115
097800021115     c                   Clear                   d
097900021115
098000050128    1c                   Do        50            b
098100021115     c                   If        Ctv(b) = *Blanks
098200021115     c                   Leave
098300021115     c                   EndIf
098400050127     c* per fedex tengo la documenti se presenti entrambe
098500050127     c                   If        ilv59Fie = 'F' and *In35 and *In36
098600021115     c                             and Mdv(b) <> 'N'
098700021115     c                   Iter
098800021115     c                   EndIf
098900050127     c
099000050127     c* Per la proposta impostata e presenti entrambe tariffe (p e A)
099100050127     c*  carico solo la proposta scelta
099200050127     c                   if        ilv59tbo=' ' and ilv59prp<>' ' and
099300050127     c                             *in37 and *in38
099400050127     c                   if        ilv59prp='P' and bavdop(b)='A'
099500050127     c                   iter
099600050127     c                   endif
099700050127     c                   if        ilv59prp='A' and bavdop(b)='P'
099800050127     c                   iter
099900050127     c                   endif
100000050127     c
100100050127     c                   endif
100200021115
100300021115     c                   Add       1             d
100400021115
100500021115     c                   Eval      Ctw(d) = Ctv(b)
100600021115     c                   Eval      Tpw(d) = Tpv(b)
100700021115     c                   Eval      Spw(d) = Spv(b)
100800160215     c                   Eval      fiw(d) = fiv(b)
100900050128    1c                   EndDo
101000021115
101100021115     c                   EndSr
101200941104     C*
101300941104     C* CONTROLLO CODICE TARIFFA ------------------------------------*
101400941104     C     CTRCTR        BEGSR
101500941215     C* SE ESISTE ALMENO UNA TARIFFA PER IL CLIENTE ANCHE SE SCADUTA
101600941215     C*  LO SEGNALO
101700941215     C     CTV(1)        IFNE      *BLANKS
101800050127     C                   MOVEL     'S'           olv59TKS
101900941215     C                   ENDIF
102000941215     C*
102100050127     C                   MOVE      ilv59ksc      WCCM              4 0
102200941104     C* RICERCA
102300050128     C     '?'           SCAN      olv59ctr                               30
102400941104     C*
102500941104    1C     *IN30         IFEQ      *ON
102600941104     C*
102700941104     C* SOLO SE CODIFICATO DELL'AREA POSSO FARE LA RICERCA
102800941104    2C     WCCM          IFEQ      8888
102900941104     C     WCCM          OREQ      9999
103000050127     C                   MOVEL     '1'           olv59ERR
103100050127     C                   MOVEL     ERR(1)        olv59MSG
103200941104     C                   GOTO      ENDCTR
103300941104    2C                   ENDIF
103400941104     C*
103500941104     C* RICERCA
103600941104     C                   CLEAR                   PARAM4
103700050127     C                   MOVEL     ilv59ksc      PA4COD
103800050127     C                   Z-ADD     ilv59kci      PA4CCC
103900061220     c                   movel     '2'           pa4vpr
104000061220     c                   move      ilv59dsp      pa4dtr
104100941104     C                   MOVEL     PARAM4        KPJBU
104200941104     C                   CALL      'TNTA36R'
104300941104     C                   PARM                    KPJBA
104400941104     C                   MOVEL     KPJBU         PARAM4
104500941104     C*
104600941104    2C     PA4CTR        IFNE      *BLANKS
104700941104     C* SE TROVATO METTO QUELLO SCELTO
104800050128     C                   MOVE      PA4CTR        olv59CTR
104900941104   X2C                   ELSE
105000050128     c* Se non scelto lo abblanco
105100050128     C                   MOVEL     *blanks       olv59CTR
105200941104    2C                   END
105300050127     C                   MOVEL     'R'           olv59ERR
105400941104     C                   GOTO      ENDCTR
105500941104     C*
105600941104   X1C                   ELSE
105700941104     C*
105800941116     C* CONTROLLO FORMALE
105900050128     c                   eval      wcontrCTR=olv59CTR
106000941116     C                   EXSR      CTRFOR
106100941116     C*
106200050127     C     olv59ERR      IFNE      ' '
106300941104     C                   GOTO      ENDCTR
106400941104    2C                   ENDIF
106500941104     C*
106600941104     C* CONTROLLO ESISTENZA TARIFFA
106700941104    2C     CTV(1)        IFNE      *BLANKS
106800941104     C                   Z-ADD     1             B
106900050128     C     olv59CTR      LOOKUP    CTV(B)                                 30
107000050128     C   30              MOVEL     PGV(B)        olv59PRG
107100050128     C   30              MOVEL     DIV(B)        olv59DIV
107200060515    3C   30olv59DIV      IFEQ      '   '
107300050128     C                   MOVEL     'ITL'         olv59DIV
107400060515    3C                   ENDIF
107500060515     c
107600060515     c* Se non trovata tariffa e accetto 1 cod tariffa se bloccato, verifico
107700060515     c*  il network
107800060515    3c                   if        not *in30 and ilv59ta2='B'
107900060515     C                   Z-ADD     1             B
108000060515     C     olv59CTR      LOOKUP    CTcli(b)                               30
108100060515     c*
108200060515    4c                   if        *in30
108300060515    5c                   if        ficli(b)=ilv59fie              or
108400060515     c                             ilv59fie='M' and mdcli(b)='M'  or
108500060515     c                             ilv59fie='N' and mdcli(b)='N'  or
108600060515     c                             ilv59fie='S' and ficli(b)<>'I' or
108700160215     c                             ilv59fie='J' and ficli(b)<>'E' or
108800160215     c                             ilv59fie='X' and (ficli(b)='E' or
108900160215     c                             ficli(b)='D')
109000060515     c* Network corrisponde: controllo se bloccata
109100060515    6c                   if        bacli(b)='B' and vacli(b)<>'A'
109200060516     c* se ok imposto i campi del progressino e della divisa
109300060516     C                   MOVEL     PGcli(b)      olv59PRG
109400060516     C                   MOVEL     DIcli(b)      olv59DIV
109500060516    3C     olv59DIV      IFEQ      '   '
109600060516     C                   MOVEL     'ITL'         olv59DIV
109700060516    3C                   ENDIF
109800060515     c                   else
109900060515     c                   setoff                                       30
110000060515    6c                   endif
110100060515   x5c                   else
110200060515     c                   setoff                                       30
110300060515    5c                   endif
110400060515    4c                   endif
110500060515    3c                   endif
110600060515     c
110700060515     c
110800941104     C* INESISTENTE: ERRORE
110900060516    3C**N30CTV(1)        IFNE      *BLANKS
111000060516    c
111100060516     c                   if        not *in30
111200050128     C                   MOVEL     '3'           olv59ERR
111300050128     C                   MOVEL     ERR(3)        olv59MSG
111400941104     C                   GOTO      ENDCTR
111500941104    3C                   END
111600060511     c*
111700060511   x2c                   else
111800060516     c*
111900060511     c* Se non esistono tariffe valide, il cod tariffa impostato non
112000060511     c*  può nemmeno essere uno presente ma non valido con questo
112100060511     c*  richiamo (ad esempio se carico tariffe italia, non può
112200060511     c*  essere un cod tariffa per l'estero)
112300060516     c                   eval      Errore=' '
112400060516     c                   z-add     1             b
112500060516     C     olv59CTR      LOOKUP    CTcli(b)                               30
112600060516     c
112700060516    3c                   if        *in30
112800060516    4c                   if        ficli(b)=ilv59fie              or
112900060516     c                             ilv59fie='M' and mdcli(b)='M'  or
113000060516     c                             ilv59fie='N' and mdcli(b)='N'  or
113100060516     c                             ilv59fie='S' and ficli(b)<>'I' or
113200160215     c                             ilv59fie='J' and ficli(b)<>'E' or
113300160215     c                             ilv59fie='X' and (ficli(b)='E' or
113400160215     c                             ficli(b)='D')
113500060516     c* Network corrisponde: controllo se bloccata
113600060516    5c                   if        bacli(b)='B' and ilv59ta2='B'
113700060516     c                             and vacli(b)<>'A'
113800060516     c* se ok imposto i campi del progressino e della divisa
113900060516     C                   MOVEL     PGcli(b)      olv59PRG
114000060516     C                   MOVEL     DIcli(b)      olv59DIV
114100060516    3C     olv59DIV      IFEQ      '   '
114200060516     C                   MOVEL     'ITL'         olv59DIV
114300060516    3C                   ENDIF
114400060516     c                   else
114500060516     c                   eval      errore='S'
114600060516    5c                   endif
114700060516   x4c                   else
114800060516     c                   eval      errore='S'
114900060516    4c                   endif
115000060516    3c                   endif
115100060511     c
115200060516    3c                   if        Errore='S'
115300060511     C                   MOVEL     '5'           olv59ERR
115400060516    4c                   select
115500060511     c                   when      ilv59tbo='P'
115600060511     C                   MOVEL     ERR(5)        olv59MSG
115700060511     c                   when      ilv59tbo='A'
115800060511     C                   MOVEL     ERR(6)        olv59MSG
115900060511     c                   other
116000060511     C                   MOVEL     ERR(7)        olv59MSG
116100060516    4c                   endsl
116200060511     c
116300060511     C                   GOTO      ENDCTR
116400060516    3c                   endif
116500060511     c
116600060516    2C                   END
116700941104    1C                   END
116800941104     C     ENDCTR        ENDSR
116900941107     C*
117000941107     C* CONTROLLO FORMALE CODICE TARIFFA -----------------------------*
117100941107     C     CTRFOR        BEGSR
117200941107     C*
117300050128     C                   MOVEL     wcontrCTR     UNOCTR            1
117400941107     C                   Z-ADD     1             C
117500941107     C     UNOCTR        LOOKUP    CTA(C)                                 30
117600941107     C*
117700050128    1c     *IN30         IFEQ      *OFF
117800941118     C     UNOCTR        OREQ      ' '
117900050127     C                   MOVEL     ERR(2)        olv59MSG
118000050127     C                   MOVEL     '2'           olv59ERR
118100050128    1C                   ENDIF
118200941107     C*
118300050127    1C     olv59ERR      IFEQ      *BLANKS
118400941107     C*
118500050127     C                   MOVEL     DTA(C)        olv59DFS
118600941107     C* CONTROLLO CHE LA 2 E 3 CIFRA SIANO UN NUERO
118700050128     C                   MOVE      wcontrCTR     W002A             2
118800941107     C                   MOVEL     W002A         W001A
118900941107     C*
119000941107    2C                   DO        2
119100941107    3C     W001A         IFNE      '0'
119200941107     C     W001A         ANDNE     '1'
119300941107     C     W001A         ANDNE     '2'
119400941107     C     W001A         ANDNE     '3'
119500941107     C     W001A         ANDNE     '4'
119600941107     C     W001A         ANDNE     '5'
119700941107     C     W001A         ANDNE     '6'
119800941107     C     W001A         ANDNE     '7'
119900941107     C     W001A         ANDNE     '8'
120000941107     C     W001A         ANDNE     '9'
120100050127     C                   MOVEL     ERR(4)        olv59MSG
120200050127     C                   MOVEL     '4'           olv59ERR
120300941107    3C                   ENDIF
120400941107     C*
120500941107     C                   MOVE      W002A         W001A
120600941107    2C                   ENDDO
120700941107    1C                   ENDIF
120800941107     C*
120900941107     C                   ENDSR
121000941104     C*
121100981006     C* SBR INIZIO ---------------------------------------------------*
121200941117     C     *INZSR        BEGSR
121300981006      *
121400981006      * ACCESSO   TABEL
121500981006      *
121600941104     C     KTAB3         KLIST
121700941104     C                   KFLD                    CODUT
121800941104     C                   KFLD                    COD               2
121900981006      *
122000941110     C                   Z-ADD     1             CODUT             1 0
122100981006     C***
122200941104     C* CARICO TABELLA TR : TIPI TARIFFA
122300941104     C***
122400981006     C                   OPEN      TABEL00F
122500981006      *
122600941104     C                   MOVEL     'TR'          COD
122700941104     C                   Z-ADD     1             C
122800941104     C     KTAB3         SETLL     TABEL
122900941104     C     KTAB3         READE     TABEL                                  30
123000941104     C*
123100941104     C     *IN30         DOWEQ     '0'
123200941104     C     TBLFLG        IFEQ      ' '
123300941104     C                   MOVEL     TBLKEY        CTA(C)
123400941104     C                   MOVEL     TBLUNI        DTA(C)
123500941104     C                   ADD       1             C
123600941104     C                   ENDIF
123700941104     C*
123800941104     C     KTAB3         READE     TABEL                                  30
123900941104     C                   ENDDO
124000941104     C*
124100941104     C                   CLOSE     TABEL00F
124200941117     C*
124300941117     C                   CLEAR                   WKSC
124400941117     C                   CLEAR                   WFIE
124500941117     C                   CLEAR                   WTBO
124600050127     c*
124700050127      * Carico tabella DFE
124800050127     c                   Clear                   DDfe
124900050127     c                   Clear                   Tibs02Ds
125000050127     c                   Eval      T02Mod = 'C'
125100050127     c                   Clear                   T02Sif
125200050127     c                   Eval      T02Cod = 'DFE'
125300050127     c                   Movel(p)  'FED'         T02Ke1
125400050127     c                   Call      'TIBS02R'
125500050127     c                   Parm                    Kpjba
125600050127     c                   Parm                    Tibs02Ds
125700050127     c                   If        T02err = *Blanks
125800050127     c                   Movel     T02Uni        DDfe
125900050127     c                   EndIf
126000941104     C*
126100941104     C                   ENDSR
126200941107     C*
126300941107     C* RIEMPO I CAMPI VIDEO-----------------------------------------*
126400941107     C     VISTAR        BEGSR
126500941107     C     CTV(1)        IFNE      *BLANKS
126600050127     C                   MOVEL     CTV(1)        OLV59CT2
126700050127     C                   MOVEL     SPV(1)        OLV59ES2
126800050127     C                   MOVE      Fiv(1)        OLV59ES2
126900941107     C* SE ESISTE ALMENO UNA TARIFFA PER IL CLIENTE ANCHE SE SCADUTA
127000941107     C*  LO SEGNALO
127100050127     C                   MOVEL     'S'           OLV59TKS
127200941107     C                   ENDIF
127300941107     C*
127400941107     C     CTV(2)        IFNE      *BLANKS
127500050127     C                   MOVEL     CTV(2)        OLV59CT3
127600050127     C                   MOVEL     SPV(2)        OLV59ES3
127700050127     C                   MOVE      Fiv(2)        OLV59ES3
127800941107     C                   ENDIF
127900941107     C     CTV(3)        IFNE      *BLANKS
128000050127     C                   MOVEL     CTV(3)        OLV59CT4
128100050127     C                   MOVEL     SPV(3)        OLV59ES4
128200050127     C                   MOVE      Fiv(3)        OLV59ES4
128300941107     C                   ENDIF
128400941107     C     CTV(4)        IFNE      *BLANKS
128500050127     C                   MOVEL     CTV(4)        OLV59CT5
128600050127     C                   MOVEL     SPV(4)        OLV59ES5
128700050127     C                   MOVE      Fiv(4)        OLV59ES5
128800941107     C                   ENDIF
128900050128     c
129000050128     c* Carico le schiere di output
129100050128     c                   if        ctv(1)<>*blanks
129200050128     c                   movea     ctv           olv59CTT
129300050128     c                   movea     pgv           olv59pgT
129400050128     c* Attributi
129500050128     c                   movea     fiv           olv59at1
129600050128     c                   movea     spv           olv59at2
129700050128     c                   movea     bav           olv59at3
129800050128     c                   movea     tpv           olv59at4
129900050128     c                   movea     vav           olv59at5
130000050128     c                   endif
130100050128     c
130200941107     C                   ENDSR
130300941118**
130400941104Per il codice cliente indicato e' impossibile eseguire la ricerca tariffe
130500941104Tipo Tariffa errato
130600060516Codice Tariffa inesistente o non utilizzabile per il cliente indicato
130700941118Codice tariffa errato
130800060511Codice tariffa esistente ma non utilizzabile in Partenza e/o x questo Network
130900060511Codice tariffa esistente ma non utilizzabile in Arrivo e/o x questo Network
131000060511Codice tariffa esistente ma bloccato o non utilizzabile per questo Network
