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
014300050128     C     ilv59ta4      ORNE      Wta4
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
016200050128     C                   MOVEL     ilv59ta4      WTA4
016300050128     C                   MOVEL     ilv59ta5      WTA5
016400941104     C*
016500050127    3C     ilv59ric      IFEQ      'S'
016600941104     C                   EXSR      RICTAR
016700941215    3C                   ENDIF
016800941104     C*
016900050127    3C     ilv59con      IFEQ      'S'
017000050128     c                   if        olv59ctr=*blanks
017100050128     c                   eval      olv59ctr=ilv59ctr
017200050128     c                   endif
017300050128     c
017400941104     C                   EXSR      CTRCTR
017500941215    3C                   ENDIF
017600941215     C*
017700050128     C* DECODIFICA FISSA TARIFFA se non ancora messa
017800050127     C     olv59CTR      IFNE      *BLANKS
017900050202     C     olv59dfs      andEQ     *BLANKS
018000941215     C                   Z-ADD     1             C
018100050127     C                   MOVEL     olv59ctr      UNOCTR            1
018200941215     C     UNOCTR        LOOKUP    CTA(C)                                 30
018300050127     C   30              MOVEL     DTA(C)        olv59dfs
018400941215     C                   ENDIF
018500941215     C*
018600941117    2C                   ENDIF
018700941107     C*
018800941104    1C                   ENDIF
018900941104     C* CHIUSURA PGM
019000050128    1C     ilv59TLA      IFEQ      ' '
019100941104     C                   RETURN
019200941104     C                   ELSE
019300941104     C                   SETON                                        LR
019400941104    1C                   ENDIF
019500050218     c*
019600050218     C* CARICO LE TARIFFE DEL CLIENTE --------------------------------*
019700050218     C     CARCTR        BEGSR
019800050218     C*
019900160225     C* PULISCO tutte le SKIERE DELLE TARIFFE
020000050218     c                   Clear                   Ctcli
020100050218     c                   Clear                   Tpcli
020200050218     c                   Clear                   Spcli
020300050218     C                   CLEAR                   PGcli
020400050218     C                   CLEAR                   DIcli
020500050218     C                   CLEAR                   FIcli
020600050218     c                   Clear                   mdcli
020700050218     c                   Clear                   bacli
020800050218     c                   Clear                   vacli
020900160225     C*
021000160225     C                   CLEAR                   CTV
021100160225     c                   Clear                   Ctw
021200160225     C                   CLEAR                   TPV
021300160225     c                   Clear                   Tpw
021400160225     C                   CLEAR                   SPV
021500160225     c                   Clear                   Spw
021600160225     C                   CLEAR                   PGV
021700160225     C                   CLEAR                   DIV
021800160225     C                   CLEAR                   FIV
021900160225     C                   CLEAR                   FIw
022000160225     c                   Clear                   mdv
022100160225     c                   Clear                   bav
022200160225     c                   Clear                   bavdop
022300160225     c                   Clear                   vav
022400160225     c                   Clear                   w_val             1
022500160225     c                   Clear                   TIPOVAL           1
022600050218     C*
022700050218     C                   Z-ADD     0             B                 3 0
022800050218     C                   Z-ADD     0             C                 3 0
022900050218     C                   Z-ADD     0             D                 3 0
023000050218     C*
023100050218     C* SE 8888 O 9999 NON CARICO NULLA
023200050218     C                   MOVE      ilv59KSC      WCCM              4 0
023300050218     C*
023400050218    1C     WCCM          IFNE      8888
023500050218     C     WCCM          ANDNE     9999
023600050218     C*
023700050218     C     ilv59ksc      SETLL     TNTAM000
023800050218     C     ilv59ksc      READE     TNTAM000                               30
023900050218     C*
024000050218    2C     *IN30         DOWEQ     *OFF
024100050218     C* ESCLUDO SE ANNULLATO
024200050218    3C     TAMATB        IFNE      'A'
024300050218     c
024400050218     C                   MOVEL     TAMFLO        DSTA01
024500050218     C*
024600050218     c                   Move      TamCtr        Wctr
024700050218      * Se Tamfie "M" Solo Tariffe merci FedEx
024800160223    5c**                 If        ilv59Fie = 'M' and Wctr >= §DfeDalD
024900160223     c**                 GoTo      Notartutto
025000160223    5c**                 EndIf
025100050218      * Se Tamfie "N" Solo Tariffe documenti FedEx
025200160223    5c**                 If        ilv59Fie = 'N' and Wctr < §DfeDalD
025300160223     c**                 GoTo      Notartutto
025400160223    5c**                 EndIf
025500160223     c**
025600050218     c* Elaboro solo tariffe decorrenti o scadute (escludo da decorrere
025700050218     c*  come se non ci fossero)
025800060503    5c***                if        ilv59dsp>=tamddt
025900060503     c* 03/5/06 ES --> preferisco caricare tutto. Se un tariffa ancora
026000060503     c*                da decorrere è già stata immessa, meglio
026100060503     c*                non farla usare in modo improprio, salvo
026200060503     c*                che non sia anche presente come decorrente:allor
026300060503     c*                la ignoro: meglio la decorrente
026400050218     c
026500050218     C                   MOVEL     TAMCTR        COMCT2            3
026600050218     C                   Z-ADD     1             D
026700050218     C     COMCT2        LOOKUP    CTcli(D)                               31
026800060503    6c                   if        (*in31 and ilv59dsp>=tamddt) or
026900050218     c                             (not *in31 and c < 50)
027000050218     C*
027100160223     C* DELLE TARIFFE già presenti, tengo sempre l'ultima, escluse le da
027200060511     c*  decorrere
027300050218    7C     *IN31         IFEQ      *OFF
027400050218     C                   ADD       1             C
027500050218     C                   MOVEL     TAMCTR        CTcli(C)
027600050218     C                   Z-ADD     C             D
027700050218    7C                   ENDIF
027800050218     C*
027900050218     C* VEDO SE E' TARIFFA decorrente
028000060503     c                   select
028100160223    7C     ilv59dsp      whenge    TAMDDT
028200050218     C     ilv59dsp      ANDLE     TAMDST
028300060503     c* Decorrente
028400050218     c                   movel     'D'           vacli(d)
028500060503    7C     ilv59dsp      whenlT    TAMDDT
028600060503     c* dA decorrere
028700060503     c                   movel     'A'           vacli(d)
028800060503     c
028900060503     c                   other
029000060503     c* Scaduta
029100050218     c                   movel     'S'           vacli(d)
029200060511    7c                   endsl
029300050218     C*
029400050218     C                   MOVEL     TAMCTR        CTcli(d)
029500050218     C                   MOVEL     TAMTSP        SPcli(d)
029600050218     C                   MOVE      TAMFTP        TPcli(d)
029700050218     C                   MOVE      TAMPRG        PGcli(d)
029800050218     C                   MOVE      §TADIV        DIcli(d)
029900050218     C                   MOVE      TAMFIE        FIcli(d)
030000050218     C                   MOVE      TAMbap        bacli(d)
030100050218     C* TARIFFA DPD
030200050218     C     §TADPD        IFEQ      'S'
030300050218     C                   MOVE      'D'           FIcli(d)
030400050218     C                   ENDIF
030500050218     C* TARIFFA FEDEX
030600050218    7C     §TAFED        IFEQ      'S'
030700050218     C                   MOVE      'F'           FIcli(d)
030800050218     c                   Clear                   mdcli(d)
030900160223     c******             If        ilv59Fie = 'M' and Wctr >= §DfeDalM
031000160223     c                   If        Wctr >= §DfeDalM
031100050218     c                                            and Wctr <= §DfeAlM
031200050218     c                   Eval      mdcli(d) = 'M'
031300050218     c                   EndIf
031400160223     c********           If        ilv59Fie = 'N' and Wctr >= §DfeDalD
031500160223     c                   If        Wctr >= §DfeDalD
031600050218     c                                            and Wctr <= §DfeAlD
031700050218     c                   Eval      mdcli(d) = 'N'
031800050218     c                   EndIf
031900050218    7c                   EndIf
032000050218    6C                   ENDIF
032100060511    5C*********          ENDIF
032200060511    4C*********          ENDIF
032300050218    3C                   ENDIF
032400050218
032500060511     c     Notartutto    Tag
032600050218
032700050218     C*
032800050218     C  N30ilv59ksc      READE     TNTAM000                               30
032900050218     C*
033000050218    2C                   ENDDO
033100160223     c
033200050218    1c                   endif
033300050218     c
033400050218     C                   ENDSR
033500160223     C*
033600160223     C* SELEZIONO le tariffe del cliente in base a quanto passato in DS ---*
033700160223     C     SELCTR_ntw    BEGSR
033800160223     C* PULISCO SKIERE DELLE TARIFFE
033900160223     C                   CLEAR                   CTV
034000160223     c                   Clear                   Ctw
034100160223     C                   CLEAR                   TPV
034200160223     c                   Clear                   Tpw
034300160223     C                   CLEAR                   SPV
034400160223     c                   Clear                   Spw
034500160223     C                   CLEAR                   PGV
034600160223     C                   CLEAR                   DIV
034700160223     C                   CLEAR                   FIV
034800160223     C                   CLEAR                   FIw
034900160223     c                   Clear                   mdv
035000160223     c                   Clear                   bav
035100160223     c                   Clear                   bavdop
035200160223     c                   Clear                   vav
035300160223     c                   Clear                   w_val             1
035400160223     c                   Clear                   TIPOVAL           1
035500160223     C                   MOVEL     *BLANKS       olv59TKS
035600160223     c* rileggo le schiere e carico in base ai parametri passati
035700160223     c                   z-add     1             xx                4 0
035800160223     c                   clear                   B
035900160223     c
036000160223     c* 1) ELABORO PRIMA LE DECORRENTI
036100160223     C*     solo se richiesto
036200160223    6c                   if        ilv59VAL=' ' or
036300160223     c                             ilv59VAL='D'
036400160223     C                   EVAL      TIPOVAL='D'
036500160223     C                   EXSR      CARICA_CTR
036600160223     C                   ENDIF
036700160223
036800160223     C* 2) POI ELABORO LE SCADUTE
036900160223     C*     solo se richiesto
037000160223    6c                   if        ilv59VAL=' ' or
037100160223     c                             ilv59VAL='S'
037200160223     c                   z-add     1             xx
037300160223     c* caricata almeno una valida
037400160223     c                   if        b>0
037500160223     c                   eval      w_val='S'
037600160223     c                   endif
037700160223     c
037800160223     C                   EVAL      TIPOVAL='S'
037900160223     C                   EXSR      CARICA_CTR
038000160223     C                   ENDIF
038100160223
038200160223     c                   exsr      VISTAR
038300160223     c                   ENDSR
038400160223     C*
038500160223     C* -------------------------------------------------------------------*
038600160223     C     CARICA_CTR    BEGSR
038700160223
038800160223    1c                   dow       ctcli(xx)<>*blanks and B<50
038900160223     c* carico il tipo di validità richiesto
039000160223    2c                   if        vacli(xx) =TIPOVAL
039100160223
039200160223     C* ilv59BLO:  B=solo bloccate " "=solo non bloccate  S=tutte
039300160223    3c                   if         ilv59BLO='S' or
039400160223     c                             (ilv59BLO='B' and bacli(xx)='B') or
039500160223     c                             (ilv59BLO=' ' and bacli(xx)<>'B')
039600160223
039700160223     C* CARICO SOLO TARIFFE ITALIA           --> FIE "I"
039800160223     C* CARICO SOLO TARIFFE Euroexpress      --> FIE "E"
039900160223     C* CARICO SOLO TARIFFE DPD              --> FIE "D"
040000160223     C* CARICO SOLO TARIFFE FEDEX            --> FIE "F"
040100160223     C* CARICO      TARIFFE DPD+FEDEX+EUROEX --> FIE "S"
040200160223     C* CARICO      TARIFFE DPD+FEDEX+ITALIA --> FIE "J"
040300160223     C* CARICO SOLO TARIFFE FEDEX DOCUMENTI  --> FIE "N"
040400160223     C* CARICO SOLO TARIFFE FEDEX MERCI      --> FIE "M"
040500160223     C* CARICO      TARIFFE DPD+EUROEX       --> FIE "X"
040600160223    4c                   if        ficli(xx)=' ' or
040700160223     c                             ficli(xx)=ilv59fie or
040800160223     c                             (ilv59fie='X' and ficli(XX)='D') or
040900160223     c                             (ilv59fie='X' and ficli(XX)='E') or
041000160223     c                             (ilv59fie='S' and ficli(xx)<>'I') or
041100160223     c                             (ilv59fie='J' and ficli(xx)<>'E') or
041200160223     c                             (ilv59fie='M' and mdcli(xx)= 'M') or
041300160223     c                             (ilv59fie='N' and mdcli(xx)= 'N')
041400160223     c
041500160223     C* ilv59TBO P=carico tariffe P/B/" "
041600160223     c*          A=carico tariffe A/B/" "   " "=tutte
041700160223    5c                   if        ilv59tbo=' ' or
041800160223     c                             bacli(xx)=' ' or
041900160223     c                             bacli(xx)='B' or
042000160223     c                             ilv59tbo=bacli(xx)
042100160223     c
042200160223     C                   ADD       1             B
042300160223     C                   MOVEL     ctcli(xx)     CTV(B)
042400160223     C                   MOVEL     spcli(xx)     SPV(B)
042500160223     c* se caricata almeno una valida non tengo conto della tariffa Prefer
042600160223     c*  per le non valide
042700160223     c                   if        w_val='S'
042800160223     c                   clear                   TPV(B)
042900160223     c                   else
043000160223     C                   MOVE      tpcli(XX)     TPV(B)
043100160223     c                   endif
043200160223     C                   MOVE      pgcli(xx)     PGV(B)
043300160223     C                   MOVE      dicli(xx)     DIV(B)
043400160223     C                   MOVE      ficli(xx)     FIV(B)
043500160223     C                   MOVE      bacli(xx)     BAV(B)
043600160223     c
043700160223     c* Carico il campo bap anche mettendo al posto di " " --> "E"
043800160223     c                   if        bacli(XX)=' ' or bacli(xx)='B'
043900160223     c                   movel     'E'           bavdop(b)
044000160223     c                   else
044100160223     C                   MOVE      bacli(xx)     BAVdop(B)
044200160223     c                   endif
044300160223     c* Validità tariffa
044400160223     C                   MOVE      vacli(xx)     VAV(B)
044500160223     c
044600160223     c* Fedex merci/documenti
044700160223     c                   if        ficli(XX)='F'
044800160223     c                   move      mdcli(XX)     MDV(b)
044900160223     c                   endif
045000160223     c
045100160223    5c                   endif
045200160223    4c                   endif
045300160223    3c                   endif
045400160223    2c                   endif
045500160223     c
045600160223     c                   add       1             xx
045700160223    1c                   enddo
045800160223     c
045900160223     C                   ENDSR
046000941104     C*
046100050218     C* SELEZIONO le tariffe del cliente in base a quanto passato in DS ---*
046200160223     C**   SELCTR        BEGSR
046300160223     C**
046400941104     C* PULISCO SKIERE DELLE TARIFFE
046500160223     C**                 CLEAR                   CTV
046600160223     C**                 CLEAR                   CTN
046700160223     c**                 Clear                   Ctw
046800160223     C**                 CLEAR                   TPN
046900160223     C**                 CLEAR                   TPV
047000160223     c**                 Clear                   Tpw
047100160223     C**                 CLEAR                   SPN
047200160223     C**                 CLEAR                   SPV
047300160223     c**                 Clear                   Spw
047400160223     C**                 CLEAR                   PGV
047500160223     C**                 CLEAR                   PGN
047600160223     C**                 CLEAR                   DIV
047700160223     C**                 CLEAR                   DIN
047800160223     C**                 CLEAR                   FIV
047900160223     C**                 CLEAR                   FIN
048000160223     C**                 CLEAR                   FIw
048100160223     c**                 Clear                   mdv
048200160223     c**                 Clear                   mdn
048300160223     c**                 Clear                   bav
048400160223     c**                 Clear                   ban
048500160223     c**                 Clear                   bavdop
048600160223     c**                 Clear                   bandop
048700160223     c**                 Clear                   vav
048800160223     C**
048900160223     C**                 MOVEL     *BLANKS       olv59TKS
049000160223     C**
049100160223     C**MITTENTI CODIFICATI --> CERCO LE TARIFFE
049200160223     C**                 Z-ADD     0             B                 3 0
049300160223     C**                 Z-ADD     0             C                 3 0
049400160223     C**                 Z-ADD     0             D                 3 0
049500160223     C**
049600160223     C**SE 8888 O 9999 NON CARICO NULLA
049700160223     C**                 MOVE      ilv59KSC      WCCM              4 0
049800160223     C**
049900160223    1C**   WCCM          IFNE      8888
050000160223     C**   WCCM          ANDNE     9999
050100160223     C**
050200160223     C**   ilv59ksc      SETLL     TNTAM000
050300160223     C**   ilv59ksc      READE     TNTAM000                               30
050400160223     C**
050500160223    2C**   *IN30         DOWEQ     *OFF
050600160223     C**ESCLUDO SE ANNULLATO
050700160223    3C**   TAMATB        IFNE      'A'
050800160223     c**
050900160223     C**                 MOVEL     TAMFLO        DSTA01
051000160223     C**
051100160223    5C**   TAMFIE        IFEQ      ' '
051200160223     C**
051300160223     C**   TAMFIE        OREQ      ilv59fie
051400160223     C**   §TADPD        ANDEQ     ' '
051500160223     C**   §TAFED        ANDEQ     ' '
051600160223     C**SE TARIFFA DPD NON DEVO CARICARE  SE RICHIESTO I/E/F/M/N
051700160223     C**   §TADPD        OREQ      'S'
051800160223     C**   ilv59fie      ANDNE     'I'
051900160223     C**   ilv59fie      ANDNE     'E'
052000160223     C**   ilv59fie      ANDNE     'F'
052100160223     c**   ilv59fie      andne     'M'
052200160223     c**   ilv59fie      andne     'N'
052300160223     C**
052400160223     C**SE TARIFFA FEDEX NON DEVO CARICARE  SE RICHIESTO I/E/D/X
052500160223     C**   §TAFED        OREQ      'S'
052600160223     C**   ilv59fie      ANDNE     'I'
052700160223     C**   ilv59fie      ANDNE     'E'
052800160223     C**   ilv59fie      ANDNE     'D'
052900160223     C**   ilv59fie      ANDNE     'X'
053000160223     C**LE ESTERE LE DEVO CARICARE SOLO PER S (OLTRE CHE DPD E FEDEX)
053100160223     C**   TAMFIE        OREQ      'E'
053200160223     C**   ilv59fie      ANDEQ     'S'
053300160223     C**LE EEX    LE DEVO CARICARE anche per X
053400160223     C**   TAMFIE        OREQ      'E'
053500160223     C**   ilv59fie      ANDEQ     'X'
053600160223     C**   §TAFED        ANDEQ     ' '
053700160223     C**LE ITALIA LE DEVO CARICARE SOLO PER J (OLTRE CHE DPD E FEDEX)
053800160223     C**   TAMFIE        OREQ      'I'
053900160223     C**   ilv59fie      ANDEQ     'J'
054000160223     C**
054100160223     c**                 Move      TamCtr        Wctr
054200160223     C**Se Tamfie "M" Solo Tariffe merci FedEx
054300160223    6c**                 If        ilv59Fie = 'M' and Wctr >= §DfeDalD
054400160223     c**                 GoTo      Notar
054500160223    6c**                 EndIf
054600160223     C**Se Tamfie "N" Solo Tariffe documenti FedEx
054700160223    6c**                 If        ilv59Fie = 'N' and Wctr < §DfeDalD
054800160223     c**                 GoTo      Notar
054900160223    6c**                 EndIf
055000160223     C**
055100160223     C**ilv59TBO P=carico tariffe P/B/" "
055200160223     a**         A=carico tariffe A/B/" "   " "=tutte
055300160223    6C**   ilv59tbo      IFEQ      ' '
055400160223     C**   TAMBAP        OREQ      ' '
055500160223     C**   TAMBAP        OREQ      'B'
055600160223     C**   TAMBAP        OREQ      ilv59tbo
055700160223     C**
055800160223     C**VEDO SE E' TARIFFA decorrente
055900160223    7C**   ilv59dsp      IFGE      TAMDDT
056000160223     C**   ilv59dsp      ANDLE     TAMDST
056100160223     c**solo se richieste (ilv59VAL=D/" ")
056200160223    8c**                 if        ilv59VAL=' ' or ilv59val='D'
056300160223     C**
056400160223     C**                 ADD       1             B
056500160223     C**                 MOVEL     TAMCTR        CTV(B)
056600160223     C**                 MOVEL     TAMTSP        SPV(B)
056700160223     C**                 MOVE      TAMFTP        TPV(B)
056800160223     C**                 MOVE      TAMPRG        PGV(B)
056900160223     C**                 MOVE      §TADIV        DIV(B)
057000160223     C**                 MOVE      TAMFIE        FIV(B)
057100160223     C**                 MOVE      TAMbap        baV(B)
057200160223     c**Carico il campo bap anche mettendo al posto di " " --> "E"
057300160223    9c**                 if        tambap=' ' or tambap='B'
057400160223     c**                 movel     'E'           bavdop(b)
057500160223     c**                 else
057600160223     C**                 MOVE      TAMbap        baVdop(B)
057700160223    9c**                 endif
057800160223     C**                 MOVE      'D'           vaV(B)
057900160223     C**TARIFFA DPD
058000160223     C**   §TADPD        IFEQ      'S'
058100160223     C**                 MOVE      'D'           FIV(B)
058200160223     C**                 ENDIF
058300160223     C**TARIFFA FEDEX
058400160223    9C**   §TAFED        IFEQ      'S'
058500160223     C**                 MOVE      'F'           FIV(B)
058600160223     c**                 Clear                   mdv(b)
058700160223     c**                 If        ilv59Fie = 'M' and Wctr >= §DfeDalM
058800160223     c**                                          and Wctr <= §DfeAlM
058900160223     c**                 Eval      mdv(b) = 'M'
059000160223     c**                 EndIf
059100160223     c**                 If        ilv59Fie = 'N' and Wctr >= §DfeDalD
059200160223     c**                                          and Wctr <= §DfeAlD
059300160223     c**                 Eval      mdv(b) = 'N'
059400160223     c**                 EndIf
059500160223    9c**                 EndIf
059600160223     c**
059700160223     C**   B             COMP      50                                 30
059800160223   x8c**                 else
059900160223     c**Se tariffa decorrente e c'e' anche come scaduta la tolgo
060000160223     C**
060100160223     C**                 MOVEL     TAMCTR        COMCT2            3
060200160223     C**                 Z-ADD     1             D
060300160223     C**   COMCT2        LOOKUP    CTN(D)                                 31
060400160223    9c**                 if        *in31
060500160223     c**                 clear                   ctn(d)
060600160223     c**                 clear                   spn(d)
060700160223     c**                 clear                   tpn(d)
060800160223     c**                 clear                   pgn(d)
060900160223     c**                 clear                   din(d)
061000160223     c**                 clear                   fin(d)
061100160223     c**                 clear                   ban(d)
061200160223     c**                 clear                   bandop(d)
061300160223     c**                 clear                   mdn(d)
061400160223     c**Reimposto il contatore a -1
061500160223     c**                 sub       1             C
061600160223     c**
061700160223    9C**                 ENDIF
061800160223    8C**                 ENDIF
061900160223     C**
062000160223     C**TARIFFE non decorrente
062100160223   X7C**                 ELSE
062200160223     c**        tengo solo le scadute, se richieste (ilv59VAL=S/" ")
062300160223    8c**                 if        ilv59dsp>tamdst  and
062400160223     c**                           (ilv59VAL=' ' or ilv59VAL='S')
062500941104     C*
062600160223     C**                 MOVEL     TAMCTR        COMCT2            3
062700160223     C**                 Z-ADD     1             D
062800160223     C**   COMCT2        LOOKUP    CTN(D)                                 31
062900160223    9c**                 if        *in31   or
063000160223     c**                           (not *in31 and c < 50)
063100160223     C**
063200160223     C**DELLE TARIFFE NON VALIDE, TENGO SEMPRE L'ULTIMA
063300160223   10C**   *IN31         IFEQ      *OFF
063400160223     C**                 ADD       1             C
063500160223     C**                 MOVEL     TAMCTR        CTN(C)
063600160223     C**                 Z-ADD     C             D
063700160223   10C**                 ENDIF
063800160223     C**                 MOVEL     TAMTSP        SPN(D)
063900160223     C**                 MOVE      TAMFTP        TPN(D)
064000160223     C**                 MOVE      TAMPRG        PGN(D)
064100160223     C**                 MOVE      §TADIV        DIN(D)
064200160223     C**                 MOVE      TAMFIE        FIN(D)
064300160223     C**                 MOVE      TAMbap        ban(D)
064400160223     c**Carico il campo bap anche mettendo al posto di " " --> "E"
064500160223     c**                 if        tambap=' '   or  tambap='B'
064600160223     c**                 movel     'E'           bandop(D)
064700160223     c**                 else
064800160223     C**                 MOVE      TAMbap        bandop(D)
064900160223     c**                 endif
065000160223     C**TARIFFA DPD
065100160223     C**   §TADPD        IFEQ      'S'
065200160223     C**                 MOVE      'D'           FIN(D)
065300160223     C**                 ENDIF
065400160223     C**TARIFFA FEDEX
065500160223   10C**   §TAFED        IFEQ      'S'
065600160223     C**                 MOVE      'F'           FIN(D)
065700160223     c**                 Clear                   mdn(d)
065800160223     c**                 If        ilv59Fie = 'M' and Wctr >= §DfeDalM
065900160223     c**                                          and Wctr <= §DfeAlM
066000160223     c**                 Eval      mdn(d) = 'M'
066100160223     c**                 EndIf
066200160223     c**                 If        ilv59Fie = 'N' and Wctr >= §DfeDalD
066300160223     c**                                          and Wctr <= §DfeAlD
066400160223     c**                 Eval      mdn(d) = 'N'
066500160223     c**                 EndIf
066600160223   10C**                 ENDIF
066700160223    9C**                 ENDIF
066800160223     C**
066900160223    8C**                 ENDIF
067000160223    7C**                 ENDIF
067100160223    6C**                 ENDIF
067200160223    5C**                 ENDIF
067300160223    4C**                 ENDIF
067400160223    3C**                 ENDIF
067500160223     C**
067600160223     c**   Notar         Tag
067700160223     C**
067800160223     C**
067900160223     C**N30ilv59ksc      READE     TNTAM000                               30
068000160223     C**
068100160223    2C**                 ENDDO
068200160223     C**
068300160223     C**                 ADD       1             B
068400160223     C**
068500160223     C**CARICO TUTTI I TIPI TARIFFA TROVATI IN UNICA SCHIERA
068600160223     C**SE NON NE HO GIA' TROVATI 50 VALIDI
068700160223    2C**   B             IFLE      50
068800160223     C**
068900160223     C**
069000160223     C**SE ESISTE ALMENO UNA TARIFFA VALIDA, NON CONSIDERO LA 'P'
069100160223     C** DI PREFERENZIALE PER LE TARIFFE SCADUTE
069200160223    3C**   B             IFGT      1
069300160223     C**                 CLEAR                   TPN
069400160223    3C**                 ENDIF
069500160223     C**
069600160223     C**                 Z-ADD     1             C
069700160223     C**
069800160223    3C**   CTN(C)        DOWNE     *BLANKS
069900160223     C**   B             ANDLE     50
070000160223     C**
070100160223     C**   CTN(C)        LOOKUP    CTV                                    31
070200160223    4C**   *IN31         IFEQ      *OFF
070300160223     C**                 MOVEL     CTN(C)        CTV(B)
070400160223     C**                 MOVEL     TPN(C)        TPV(B)
070500160223     C**                 MOVEL     SPN(C)        SPV(B)
070600160223     C**                 MOVEL     PGN(C)        PGV(B)
070700160223     C**                 MOVEL     DIN(C)        DIV(B)
070800160223     C**                 MOVEL     FIN(C)        FIV(B)
070900160223     c**                 Movel     mdn(c)        mdv(b)
071000160223     c**                 Movel     ban(c)        bav(b)
071100160223     c**                 Movel     bandop(c)     bavdop(b)
071200160223     c**S-scaduta
071300160223     c**                 movel     'S'           vav(b)
071400160223     C**                 ADD       1             B
071500160223    4C**                 ENDIF
071600160223     C**
071700160223     C**                 ADD       1             C
071800160223    3C**                 ENDdo
071900160223    2C**                 ENDif
072000160223     C**
072100160223     C**                 EXSR      VISTAR
072200160223    1C**                 ENDif
072300160223     C**
072400160223     C**                 ENDSR
072500941104     C*
072600941104     C* RICERCA E CARICAMENTO A VIDEO TARIFFE ------------------------*
072700941104     C     RICTAR        BEGSR
072800941104     C                   SETOFF                                       30
072900950125     C                   CLEAR                   W001A
073000050124     c                   clear                   CNo
073100941104     C*
073200941104     C* SE NON CI SONO TARIFFE IMPOSTO LA TARIFFA 000
073300050127    1C     CTV(1)        IFEQ      *BLANKS
073400050127     C                   CLEAR                   olv59tks
073500941215     C*
073600050127     C     ilv59ctr      IFEQ      *BLANKS
073700050127     C                   MOVEL     *ZEROS        olv59CTR
073800941215     C                   ENDIF
073900941215     C*
074000050127   X1C                   ELSE
074100941107     C*
074200941107     C* CODICE TARIFFA IMPOSTATO: PRENDO IL RELATIVO PROGRESSIVO
074300050127    2C     ilv59CTR      IFNE      *BLANKS
074400050127     c                   eval      olv59ctr=ilv59ctr
074500050127    2C                   ENDIF
074600941107     C**
074700050127     c**
074800050127    2C     ilv59CTR      IFEQ      *BLANKS
074900021115
075000050127      * Carico le sk di work per la scelta
075100021115     c                   ExSr      CarSk
075200941104     C*
075300160215     C* 1) SE IMPOSTATO IL TIPO SERVIZIO --> CERCO QUELLO
075400160401     c*    slo per E e H (non lo faccio per C e D)
075500050128    3C     ilv59TSP      IFNE      *BLANKS
075600160401     c     ilv59TSP      andne     'C'
075700160401     c     ilv59TSP      andne     'D'
075800941104     C* CERCO SE C'E' LA PREFERENZIALE ALTRIMENTI LA 1 CHE TROVO
075900941104     C                   Z-ADD     1             C
076000941104     C                   SETON                                        31
076100941104     C*
076200941104    4C     *IN31         DOWEQ     *ON
076300050128     C     ilv59tSP      LOOKUP    SPw(C)                                 31
076400941104     C* SE TROVATA ED E' PREFERENZIALE ESCO DAL CICLO
076500070109    5c   31              if        tpw(C)='P'
076600941104     C                   SETON                                        30
076700941104     C                   SETOFF                                       31
076800941104     C                   MOVEL     '1'           W001A             1
076900050127   X5C                   ELSE
077000021115     C* SE TROVATA MA NON PREFERENZIALE ACCENDO COMUNQUE L'INDICATORE
077100941104     C*  DI TROVATO MA FACCIO UN ALTRO GIRO
077200941104     C                   SETON                                        30
077300050124     c                   if        CNo=0
077400050124     c                   z-add     C             CNo
077500050127     c                   endif
077600050124     c
077700941104     C                   ADD       1             C
077800050124    5C                   ENDIF
077900941104    4C                   ENDDO
078000941104     C*
078100941104     C* SE HO TROVATO LA TARIFFA MA NON LA PREFERENZIALE, LA RICERCO X
078200941104     C*  PRENDERE LA POSIZIONE
078300941104    4C   30W001A         IFEQ      ' '
078400050124     c                   z-add     CNo           C
078500941104    4C                   ENDIF
078600941104     C*
078700941104    3C                   ENDIF
078800941104     C*
078900941104     C* SE NON TROVATA O NON IMPOSTATO IL TIPO SERVIZIO -->
079000160215     c* 2) cerco per network  se passato
079100160215     c*    solo se ho caricato più di un network, altrimenti ignoro
079200160215    3c                   if        not *in30 and ilv59TA3<>' '
079300160215     c                             and (ilv59fie='X' or ilv59fie='S' or
079400160215     c                             ilv59fie='J')
079500160215     C                   CLEAR                   W001A
079600160215     c                   clear                   CNo
079700160215     C                   Z-ADD     1             C
079800160215     C                   SETON                                        31
079900160215     C*
080000160215    4C     *IN31         DOWEQ     *ON
080100160215     C     ilv59TA3      LOOKUP    fiw(C)                                 31
080200160215     C* SE TROVATA ED E' PREFERENZIALE ESCO DAL CICLO
080300160215    5c   31              if        tpw(C)='P'
080400160215     C                   SETON                                        30
080500160215     C                   SETOFF                                       31
080600160215     C                   MOVEL     '1'           W001A             1
080700160215   X5C                   ELSE
080800160215     c
080900160215     C* SE TROVATA MA NON PREFERENZIALE ACCENDO COMUNQUE L'INDICATORE
081000160215     C*  DI TROVATO MA FACCIO UN ALTRO GIRO
081100160215     C                   SETON                                        30
081200160215     c                   if        CNo=0
081300160215     c                   z-add     C             CNo
081400160215     c                   endif
081500160215     c
081600160215     C                   ADD       1             C
081700160215    5C                   ENDIF
081800160215    4C                   ENDDO
081900160215     C*
082000160215     C* SE HO TROVATO LA TARIFFA MA NON LA PREFERENZIALE, LA RICERCO per
082100160215     C*  PRENDERE LA POSIZIONE
082200160215    4C   30W001A         IFEQ      ' '
082300160215     c                   z-add     CNo           C
082400160215    4C                   ENDIF
082500160215     C*
082600160215    3C                   ENDIF
082700160215
082800160215     C* SE NON ancora TROVATA -->
082900160215     C* 3) CERCO LA PRIMA PREFERENZIALE
083000050124    3C     *IN30         IFEQ      *OFF
083100050127     C                   Z-ADD     1             C
083200050127     C     'P'           LOOKUP    TPw(C)                                 30
083300050127    3C                   ENDIF
083400941104     C*
083500160215     C* 4) SE NON ESISTE LA PREFERENZIALE METTO LA PRIMA VALIDA TROVATA
083600941104     C  N30              Z-ADD     1             C
083700050127     c
083800050127     c* IMPOSTO CAMPO CODICE TARIFFA
083900050127     C                   MOVEL     CTw(C)        Olv59CTR
084000050127    2c                   endif
084100941104     C*
084200050127      * Cerco la posizione nella sk valida
084300050127     c                   Z-Add     1             c
084400050127     c     olv59CTR      Lookup    Ctv(c)                                 30
084500050127     c
084600050128    2c                   IF        *IN30
084700050127     C                   MOVEL     PGV(C)        OLV59PRG
084800050127     C                   MOVEL     DIV(C)        OLV59DIV
084900050127     C     OLV59DIV      IFEQ      '   '
085000050127     C                   MOVEL     'ITL'         OLV59DIV
085100991026     C                   ENDIF
085200050127
085300050127   x2C                   ELSE
085400050127     C* ERRORE --> COD.TARIFFA NON TROVATO (se passato in input può
085500050127     c*            accadere)
085600050127     C                   MOVEL     '3'           olv59ERR
085700050127     C                   MOVEL     ERR(3)        olv59MSG
085800050127     C                   Z-ADD     10            C
085900050127    2C                   ENDIF
086000941107     C*
086100021115     C                   EXSR      VISTAR
086200941104     C*
086300941104     C* RIMPIAZZO CON COD TARIFFA 5 SOLO SE ESISTE
086400050127    2C     CTv(5)        IFNE      *BLANKS
086500941104     C* SE NELLA 1°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
086600050127    3C     C             IFEQ      1
086700050128     C                   MOVEL     CTv(5)        olv59CT2
086800050128     C                   MOVEL     SPv(5)        olv59ES2
086900050128     C                   MOVE      FIv(5)        olv59ES2
087000050127    3C                   END
087100941104     C* SE NELLA 2°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
087200050127    3C     C             IFEQ      2
087300050128     C                   MOVEL     CTv(5)        olv59CT3
087400050128     C                   MOVEL     SPv(5)        olv59ES3
087500050128     C                   MOVE      FIv(5)        olv59ES3
087600050127    3C                   END
087700941104     C* SE NELLA 3°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
087800050127    3C     C             IFEQ      3
087900050128     C                   MOVEL     CTv(5)        olv59CT4
088000050128     C                   MOVEL     SPv(5)        olv59ES4
088100050128     C                   MOVE      FIv(5)        olv59ES4
088200050127    3C                   END
088300941104     C* SE NELLA 4°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
088400050127    3C     C             IFEQ      4
088500050128     C                   MOVEL     CTv(5)        olv59CT5
088600050128     C                   MOVEL     SPv(5)        olv59ES5
088700050128     C                   MOVE      FIv(5)        olv59ES5
088800050127    3C                   END
088900941104     C*
089000050127    2C                   END
089100050127     C
089200050127    1C                   ENDIF
089300941104     C*
089400941104     C                   ENDSR
089500021115      *--------------------------------------------------------------*
089600050127      * Carico le sk di work per la proposta a video
089700021115      *--------------------------------------------------------------*
089800021115     c     Carsk         BegSr
089900021115
090000050127     c* Se richieste tariffe FEDEX e presenti sia M che N --> propon
090100050127     c*  go "N"-documenti
090200050127     c                   setoff                                       3536
090300050127     c                   setoff                                       3738
090400050127     c                   If        ilv59Fie = 'F'
090500021115     c     'M'           Lookup    mdv                                    35
090600021115     c     'N'           Lookup    mdv                                    36
090700021115     c                   EndIf
090800050127     c
090900050127     c* Se presenti sia tariffe "P" che "A" --> propongo la P o la A
091000050127     c*    in base alla richiesta (ilv59PRP)
091100050128    1c                   if        ilv59tbo=' '  and ilv59PRP<>' '
091200050127     c     'E'           Lookup    bavdop                                 37
091300050128    2c                   if        *in37
091400050127     c                   seton                                        38
091500050128   x2c                   else
091600050127     c     'P'           Lookup    bavdop                                 37
091700050127     c     'A'           Lookup    bavdop                                 38
091800050128    2c                   endif
091900050128    1c                   endif
092000021115
092100021115     c                   Clear                   d
092200021115
092300050128    1c                   Do        50            b
092400021115     c                   If        Ctv(b) = *Blanks
092500021115     c                   Leave
092600021115     c                   EndIf
092700050127     c* per fedex tengo la documenti se presenti entrambe
092800050127     c                   If        ilv59Fie = 'F' and *In35 and *In36
092900021115     c                             and Mdv(b) <> 'N'
093000021115     c                   Iter
093100021115     c                   EndIf
093200050127     c
093300050127     c* Per la proposta impostata e presenti entrambe tariffe (p e A)
093400050127     c*  carico solo la proposta scelta
093500050127     c                   if        ilv59tbo=' ' and ilv59prp<>' ' and
093600050127     c                             *in37 and *in38
093700050127     c                   if        ilv59prp='P' and bavdop(b)='A'
093800050127     c                   iter
093900050127     c                   endif
094000050127     c                   if        ilv59prp='A' and bavdop(b)='P'
094100050127     c                   iter
094200050127     c                   endif
094300050127     c
094400050127     c                   endif
094500021115
094600021115     c                   Add       1             d
094700021115
094800021115     c                   Eval      Ctw(d) = Ctv(b)
094900021115     c                   Eval      Tpw(d) = Tpv(b)
095000021115     c                   Eval      Spw(d) = Spv(b)
095100160215     c                   Eval      fiw(d) = fiv(b)
095200050128    1c                   EndDo
095300021115
095400021115     c                   EndSr
095500941104     C*
095600941104     C* CONTROLLO CODICE TARIFFA ------------------------------------*
095700941104     C     CTRCTR        BEGSR
095800941215     C* SE ESISTE ALMENO UNA TARIFFA PER IL CLIENTE ANCHE SE SCADUTA
095900941215     C*  LO SEGNALO
096000941215     C     CTV(1)        IFNE      *BLANKS
096100050127     C                   MOVEL     'S'           olv59TKS
096200941215     C                   ENDIF
096300941215     C*
096400050127     C                   MOVE      ilv59ksc      WCCM              4 0
096500941104     C* RICERCA
096600050128     C     '?'           SCAN      olv59ctr                               30
096700941104     C*
096800941104    1C     *IN30         IFEQ      *ON
096900941104     C*
097000941104     C* SOLO SE CODIFICATO DELL'AREA POSSO FARE LA RICERCA
097100941104    2C     WCCM          IFEQ      8888
097200941104     C     WCCM          OREQ      9999
097300050127     C                   MOVEL     '1'           olv59ERR
097400050127     C                   MOVEL     ERR(1)        olv59MSG
097500941104     C                   GOTO      ENDCTR
097600941104    2C                   ENDIF
097700941104     C*
097800941104     C* RICERCA
097900941104     C                   CLEAR                   PARAM4
098000050127     C                   MOVEL     ilv59ksc      PA4COD
098100050127     C                   Z-ADD     ilv59kci      PA4CCC
098200061220     c                   movel     '2'           pa4vpr
098300061220     c                   move      ilv59dsp      pa4dtr
098400941104     C                   MOVEL     PARAM4        KPJBU
098500941104     C                   CALL      'TNTA36R'
098600941104     C                   PARM                    KPJBA
098700941104     C                   MOVEL     KPJBU         PARAM4
098800941104     C*
098900941104    2C     PA4CTR        IFNE      *BLANKS
099000941104     C* SE TROVATO METTO QUELLO SCELTO
099100050128     C                   MOVE      PA4CTR        olv59CTR
099200941104   X2C                   ELSE
099300050128     c* Se non scelto lo abblanco
099400050128     C                   MOVEL     *blanks       olv59CTR
099500941104    2C                   END
099600050127     C                   MOVEL     'R'           olv59ERR
099700941104     C                   GOTO      ENDCTR
099800941104     C*
099900941104   X1C                   ELSE
100000941104     C*
100100941116     C* CONTROLLO FORMALE
100200050128     c                   eval      wcontrCTR=olv59CTR
100300941116     C                   EXSR      CTRFOR
100400941116     C*
100500050127     C     olv59ERR      IFNE      ' '
100600941104     C                   GOTO      ENDCTR
100700941104    2C                   ENDIF
100800941104     C*
100900941104     C* CONTROLLO ESISTENZA TARIFFA
101000941104    2C     CTV(1)        IFNE      *BLANKS
101100941104     C                   Z-ADD     1             B
101200050128     C     olv59CTR      LOOKUP    CTV(B)                                 30
101300050128     C   30              MOVEL     PGV(B)        olv59PRG
101400050128     C   30              MOVEL     DIV(B)        olv59DIV
101500060515    3C   30olv59DIV      IFEQ      '   '
101600050128     C                   MOVEL     'ITL'         olv59DIV
101700060515    3C                   ENDIF
101800060515     c
101900060515     c* Se non trovata tariffa e accetto 1 cod tariffa se bloccato, verifico
102000060515     c*  il network
102100060515    3c                   if        not *in30 and ilv59ta2='B'
102200060515     C                   Z-ADD     1             B
102300060515     C     olv59CTR      LOOKUP    CTcli(b)                               30
102400060515     c*
102500060515    4c                   if        *in30
102600060515    5c                   if        ficli(b)=ilv59fie              or
102700060515     c                             ilv59fie='M' and mdcli(b)='M'  or
102800060515     c                             ilv59fie='N' and mdcli(b)='N'  or
102900060515     c                             ilv59fie='S' and ficli(b)<>'I' or
103000160215     c                             ilv59fie='J' and ficli(b)<>'E' or
103100160215     c                             ilv59fie='X' and (ficli(b)='E' or
103200160215     c                             ficli(b)='D')
103300060515     c* Network corrisponde: controllo se bloccata
103400060515    6c                   if        bacli(b)='B' and vacli(b)<>'A'
103500060516     c* se ok imposto i campi del progressino e della divisa
103600060516     C                   MOVEL     PGcli(b)      olv59PRG
103700060516     C                   MOVEL     DIcli(b)      olv59DIV
103800060516    3C     olv59DIV      IFEQ      '   '
103900060516     C                   MOVEL     'ITL'         olv59DIV
104000060516    3C                   ENDIF
104100060515     c                   else
104200060515     c                   setoff                                       30
104300060515    6c                   endif
104400060515   x5c                   else
104500060515     c                   setoff                                       30
104600060515    5c                   endif
104700060515    4c                   endif
104800060515    3c                   endif
104900060515     c
105000060515     c
105100941104     C* INESISTENTE: ERRORE
105200060516    3C**N30CTV(1)        IFNE      *BLANKS
105300060516    c
105400060516     c                   if        not *in30
105500050128     C                   MOVEL     '3'           olv59ERR
105600050128     C                   MOVEL     ERR(3)        olv59MSG
105700941104     C                   GOTO      ENDCTR
105800941104    3C                   END
105900060511     c*
106000060511   x2c                   else
106100060516     c*
106200060511     c* Se non esistono tariffe valide, il cod tariffa impostato non
106300060511     c*  può nemmeno essere uno presente ma non valido con questo
106400060511     c*  richiamo (ad esempio se carico tariffe italia, non può
106500060511     c*  essere un cod tariffa per l'estero)
106600060516     c                   eval      Errore=' '
106700060516     c                   z-add     1             b
106800060516     C     olv59CTR      LOOKUP    CTcli(b)                               30
106900060516     c
107000060516    3c                   if        *in30
107100060516    4c                   if        ficli(b)=ilv59fie              or
107200060516     c                             ilv59fie='M' and mdcli(b)='M'  or
107300060516     c                             ilv59fie='N' and mdcli(b)='N'  or
107400060516     c                             ilv59fie='S' and ficli(b)<>'I' or
107500160215     c                             ilv59fie='J' and ficli(b)<>'E' or
107600160215     c                             ilv59fie='X' and (ficli(b)='E' or
107700160215     c                             ficli(b)='D')
107800060516     c* Network corrisponde: controllo se bloccata
107900060516    5c                   if        bacli(b)='B' and ilv59ta2='B'
108000060516     c                             and vacli(b)<>'A'
108100060516     c* se ok imposto i campi del progressino e della divisa
108200060516     C                   MOVEL     PGcli(b)      olv59PRG
108300060516     C                   MOVEL     DIcli(b)      olv59DIV
108400060516    3C     olv59DIV      IFEQ      '   '
108500060516     C                   MOVEL     'ITL'         olv59DIV
108600060516    3C                   ENDIF
108700060516     c                   else
108800060516     c                   eval      errore='S'
108900060516    5c                   endif
109000060516   x4c                   else
109100060516     c                   eval      errore='S'
109200060516    4c                   endif
109300060516    3c                   endif
109400060511     c
109500060516    3c                   if        Errore='S'
109600060511     C                   MOVEL     '5'           olv59ERR
109700060516    4c                   select
109800060511     c                   when      ilv59tbo='P'
109900060511     C                   MOVEL     ERR(5)        olv59MSG
110000060511     c                   when      ilv59tbo='A'
110100060511     C                   MOVEL     ERR(6)        olv59MSG
110200060511     c                   other
110300060511     C                   MOVEL     ERR(7)        olv59MSG
110400060516    4c                   endsl
110500060511     c
110600060511     C                   GOTO      ENDCTR
110700060516    3c                   endif
110800060511     c
110900060516    2C                   END
111000941104    1C                   END
111100941104     C     ENDCTR        ENDSR
111200941107     C*
111300941107     C* CONTROLLO FORMALE CODICE TARIFFA -----------------------------*
111400941107     C     CTRFOR        BEGSR
111500941107     C*
111600050128     C                   MOVEL     wcontrCTR     UNOCTR            1
111700941107     C                   Z-ADD     1             C
111800941107     C     UNOCTR        LOOKUP    CTA(C)                                 30
111900941107     C*
112000050128    1c     *IN30         IFEQ      *OFF
112100941118     C     UNOCTR        OREQ      ' '
112200050127     C                   MOVEL     ERR(2)        olv59MSG
112300050127     C                   MOVEL     '2'           olv59ERR
112400050128    1C                   ENDIF
112500941107     C*
112600050127    1C     olv59ERR      IFEQ      *BLANKS
112700941107     C*
112800050127     C                   MOVEL     DTA(C)        olv59DFS
112900941107     C* CONTROLLO CHE LA 2 E 3 CIFRA SIANO UN NUERO
113000050128     C                   MOVE      wcontrCTR     W002A             2
113100941107     C                   MOVEL     W002A         W001A
113200941107     C*
113300941107    2C                   DO        2
113400941107    3C     W001A         IFNE      '0'
113500941107     C     W001A         ANDNE     '1'
113600941107     C     W001A         ANDNE     '2'
113700941107     C     W001A         ANDNE     '3'
113800941107     C     W001A         ANDNE     '4'
113900941107     C     W001A         ANDNE     '5'
114000941107     C     W001A         ANDNE     '6'
114100941107     C     W001A         ANDNE     '7'
114200941107     C     W001A         ANDNE     '8'
114300941107     C     W001A         ANDNE     '9'
114400050127     C                   MOVEL     ERR(4)        olv59MSG
114500050127     C                   MOVEL     '4'           olv59ERR
114600941107    3C                   ENDIF
114700941107     C*
114800941107     C                   MOVE      W002A         W001A
114900941107    2C                   ENDDO
115000941107    1C                   ENDIF
115100941107     C*
115200941107     C                   ENDSR
115300941104     C*
115400981006     C* SBR INIZIO ---------------------------------------------------*
115500941117     C     *INZSR        BEGSR
115600981006      *
115700981006      * ACCESSO   TABEL
115800981006      *
115900941104     C     KTAB3         KLIST
116000941104     C                   KFLD                    CODUT
116100941104     C                   KFLD                    COD               2
116200981006      *
116300941110     C                   Z-ADD     1             CODUT             1 0
116400981006     C***
116500941104     C* CARICO TABELLA TR : TIPI TARIFFA
116600941104     C***
116700981006     C                   OPEN      TABEL00F
116800981006      *
116900941104     C                   MOVEL     'TR'          COD
117000941104     C                   Z-ADD     1             C
117100941104     C     KTAB3         SETLL     TABEL
117200941104     C     KTAB3         READE     TABEL                                  30
117300941104     C*
117400941104     C     *IN30         DOWEQ     '0'
117500941104     C     TBLFLG        IFEQ      ' '
117600941104     C                   MOVEL     TBLKEY        CTA(C)
117700941104     C                   MOVEL     TBLUNI        DTA(C)
117800941104     C                   ADD       1             C
117900941104     C                   ENDIF
118000941104     C*
118100941104     C     KTAB3         READE     TABEL                                  30
118200941104     C                   ENDDO
118300941104     C*
118400941104     C                   CLOSE     TABEL00F
118500941117     C*
118600941117     C                   CLEAR                   WKSC
118700941117     C                   CLEAR                   WFIE
118800941117     C                   CLEAR                   WTBO
118900050127     c*
119000050127      * Carico tabella DFE
119100050127     c                   Clear                   DDfe
119200050127     c                   Clear                   Tibs02Ds
119300050127     c                   Eval      T02Mod = 'C'
119400050127     c                   Clear                   T02Sif
119500050127     c                   Eval      T02Cod = 'DFE'
119600050127     c                   Movel(p)  'FED'         T02Ke1
119700050127     c                   Call      'TIBS02R'
119800050127     c                   Parm                    Kpjba
119900050127     c                   Parm                    Tibs02Ds
120000050127     c                   If        T02err = *Blanks
120100050127     c                   Movel     T02Uni        DDfe
120200050127     c                   EndIf
120300941104     C*
120400941104     C                   ENDSR
120500941107     C*
120600941107     C* RIEMPO I CAMPI VIDEO-----------------------------------------*
120700941107     C     VISTAR        BEGSR
120800941107     C     CTV(1)        IFNE      *BLANKS
120900050127     C                   MOVEL     CTV(1)        OLV59CT2
121000050127     C                   MOVEL     SPV(1)        OLV59ES2
121100050127     C                   MOVE      Fiv(1)        OLV59ES2
121200941107     C* SE ESISTE ALMENO UNA TARIFFA PER IL CLIENTE ANCHE SE SCADUTA
121300941107     C*  LO SEGNALO
121400050127     C                   MOVEL     'S'           OLV59TKS
121500941107     C                   ENDIF
121600941107     C*
121700941107     C     CTV(2)        IFNE      *BLANKS
121800050127     C                   MOVEL     CTV(2)        OLV59CT3
121900050127     C                   MOVEL     SPV(2)        OLV59ES3
122000050127     C                   MOVE      Fiv(2)        OLV59ES3
122100941107     C                   ENDIF
122200941107     C     CTV(3)        IFNE      *BLANKS
122300050127     C                   MOVEL     CTV(3)        OLV59CT4
122400050127     C                   MOVEL     SPV(3)        OLV59ES4
122500050127     C                   MOVE      Fiv(3)        OLV59ES4
122600941107     C                   ENDIF
122700941107     C     CTV(4)        IFNE      *BLANKS
122800050127     C                   MOVEL     CTV(4)        OLV59CT5
122900050127     C                   MOVEL     SPV(4)        OLV59ES5
123000050127     C                   MOVE      Fiv(4)        OLV59ES5
123100941107     C                   ENDIF
123200050128     c
123300050128     c* Carico le schiere di output
123400050128     c                   if        ctv(1)<>*blanks
123500050128     c                   movea     ctv           olv59CTT
123600050128     c                   movea     pgv           olv59pgT
123700050128     c* Attributi
123800050128     c                   movea     fiv           olv59at1
123900050128     c                   movea     spv           olv59at2
124000050128     c                   movea     bav           olv59at3
124100050128     c                   movea     tpv           olv59at4
124200050128     c                   movea     vav           olv59at5
124300050128     c                   endif
124400050128     c
124500941107     C                   ENDSR
124600941118**
124700941104Per il codice cliente indicato e' impossibile eseguire la ricerca tariffe
124800941104Tipo Tariffa errato
124900060516Codice Tariffa inesistente o non utilizzabile per il cliente indicato
125000941118Codice tariffa errato
125100060511Codice tariffa esistente ma non utilizzabile in Partenza e/o x questo Network
125200060511Codice tariffa esistente ma non utilizzabile in Arrivo e/o x questo Network
125300060511Codice tariffa esistente ma bloccato o non utilizzabile per questo Network
