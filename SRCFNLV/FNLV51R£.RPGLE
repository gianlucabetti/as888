000100921021     H DECEDIT('0,') DATEDIT(*DMY.)
000200941104     H* FNLV51R  *---------------------------------------------------*
000300941110     H*     CARICAMENTO TARIFFE DI UN CLIENTE                         *
000400921021     H*--------------------------------------------------------------*
000500981006     FTNTAM01L  IF   E           K DISK    USROPN
000600941110     FTABEL00F  IF   E           K DISK    USROPN
000700941104     D*
000800941104     D CTV             S              3    DIM(150)
000900941104     D CTN             S              3    DIM(150)
001000021115     d ctw             s              3    Dim(150)
001100941104     D TPV             S              1    DIM(150)
001200941104     D TPN             S              1    DIM(150)
001300021115     d tpw             s              1    Dim(150)
001400941104     D SPV             S              1    DIM(150)
001500941104     D SPN             S              1    DIM(150)
001600021115     d spw             s              1    Dim(150)
001700941104     D PGV             S              3    DIM(150)
001800941104     D PGN             S              3    DIM(150)
001900021115     d pgw             s              3    Dim(150)
002000990930     D DIV             S              3    DIM(150)
002100990930     D DIN             S              3    DIM(150)
002200021115     d diw             s              3    Dim(150)
002300000126     D FIV             S              1    DIM(150)
002400000126     D FIN             S              1    DIM(150)
002500021115     d fiw             s              1    Dim(150)
002600021115     d mdv             s              1    Dim(150)
002700021115     d mdn             s              1    Dim(150)
002800941104     D*
002900941107     D CTA             S              1    DIM(30)
003000941107     D DTA             S             10    DIM(30)
003100941107     D ERR             S             78    DIM(4) CTDATA PERRCD(1)
003200021115
003300021115     d Wctr            s              2  0
003400941104      *
003500941107     D* DS PER FNLV51R - DS DEI CAMPI PER CARICAMENTO TARIFFE
003600941104     D DSLV51        E DS                  EXTNAME(FNLV51DS)
003700021115
003800021115      * Ds per confronto dati tariffe FedEx
003900021115     d ddfe          e ds
004000021115     d Tibs02Ds      e ds
004100021115
004200981006     D* DS PER XPARUT -
004300981006     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004400990930     D DSTA01        E DS
004500000126     D OG143         E DS
004600990930     D CNCR80        E DS                  EXTNAME(CNCR80)
004700941104     D* PASSO IN SCELTA COD TARIFFA DI TARIFFA/OFFERTA - TNTA36R -
004800941104     D PARAM4          DS
004900941104     D  PA4COD                 6     12  0
005000941104     D  PA4CTR                13     15
005100941104     D  PA4FLG                16     16
005200941104     D  PA4VPR                17     17
005300941104     D  PA4CCC                18     21  0
005400941104     D  PA4DCV                22     36
005500941104     D KPJBA         E DS
005600941104     C*---------------------------------------------------------------*
005700941104     C     *ENTRY        PLIST
005800941104     C                   PARM                    DSLV51
005900941104     C*---------------------------------------------------------------*
006000941104     C* TIPO LANCIO, D51TLA "C" -           CHIUSO CON LR
006100941104     C* TIPO LANCIO, D51TLA "L" - ELABORO E CHIUDO CON LR
006200941104     C* TIPO LANCIO, D51TLA " " - ELABORO E CHIUDO IN RETRN
006300941104     C*
006400941104    1C     D51TLA        IFNE      'C'
006500941107     C*
006600941107     C* PULIZIA CAMPI DI OUTPUT
006700941107     C                   CLEAR                   D51MSG
006800941107     C                   CLEAR                   D51ERR
006900941107     C                   CLEAR                   D51DFS
007000941107     C                   CLEAR                   D51PRG
007100990930     C                   CLEAR                   D51DIV
007200941107     C*
007300941117     C* SOLO CONTROLLO FORMALE
007400941117    2C     D51CFO        IFEQ      'S'
007500941117     C                   EXSR      CTRFOR
007600941117     C*
007700941117   X2C                   ELSE
007800021115
007900021115      * Carico tabella DFE
008000021115     c                   Clear                   DDfe
008100021115     c                   Clear                   Tibs02Ds
008200021115     c                   Eval      T02Mod = 'C'
008300021115     c                   Clear                   T02Sif
008400021115     c                   Eval      T02Cod = 'DFE'
008500021118     c                   Movel(p)  'FED'         T02Ke1
008600021115     c                   Call      'TIBS02R'
008700021115     c                   Parm                    Kpjba
008800021115     c                   Parm                    Tibs02Ds
008900021115     c                   If        T02err = *Blanks
009000021115     c                   Movel     T02Uni        DDfe
009100021115     c                   EndIf
009200021115
009300941117     C*
009400941117    3C     D51KSC        IFNE      WKSC
009500941117     C     D51FIE        ORNE      WFIE
009600941117     C     D51TBO        ORNE      WTBO
009700961007     C     D51DSP        ORNE      WDSP
009800941104     C                   EXSR      CARCTR
009900941231     C                   ELSE
010000941231     C                   EXSR      VISTAR
010100941231    3C                   ENDIF
010200941231     C                   MOVEL     D51KSC        WKSC              7 0
010300961007     C                   MOVEL     D51DSP        WDSP              8 0
010400941231     C                   MOVEL     D51FIE        WFIE              1
010500941231     C                   MOVEL     D51TBO        WTBO              1
010600941104     C*
010700941215    3C     D51RIC        IFEQ      'S'
010800990408    3C     D51RIC        OREQ      'Y'
010900941104     C                   EXSR      RICTAR
011000941215    3C                   ENDIF
011100941104     C*
011200941215    3C     D51CON        IFEQ      'S'
011300941104     C                   EXSR      CTRCTR
011400941215    3C                   ENDIF
011500941215     C*
011600941215     C* DECODIFICA FISSA TARIFFA
011700941215     C     D51CTR        IFNE      *BLANKS
011800941215     C                   Z-ADD     1             C
011900941215     C                   MOVEL     D51CTR        UNOCTR            1
012000941215     C     UNOCTR        LOOKUP    CTA(C)                                 30
012100941215     C   30              MOVEL     DTA(C)        D51DFS
012200941215     C                   ELSE
012300941215     C                   CLEAR                   D51DFS
012400941215     C                   ENDIF
012500941215     C*
012600941117    2C                   ENDIF
012700941107     C*
012800941104    1C                   ENDIF
012900941104     C* CHIUSURA PGM
013000941104    1C     D51TLA        IFEQ      ' '
013100941104     C                   RETURN
013200941104     C                   ELSE
013300941104     C                   SETON                                        LR
013400941104    1C                   ENDIF
013500941104     C*
013600941104     C* CARICO LE TARIFFE DEL CLIENTE --------------------------------*
013700941104     C     CARCTR        BEGSR
013800941104     C*
013900941104     C* PULISCO SKIERE DELLE TARIFFE
014000941104     C                   CLEAR                   CTV
014100941104     C                   CLEAR                   CTN
014200021119     c                   Clear                   Ctw
014300941104     C                   CLEAR                   TPN
014400941104     C                   CLEAR                   TPV
014500021119     c                   Clear                   Tpw
014600941104     C                   CLEAR                   SPN
014700941104     C                   CLEAR                   SPV
014800021119     c                   Clear                   Spw
014900941104     C                   CLEAR                   PGV
015000941104     C                   CLEAR                   PGN
015100021119     c                   Clear                   Pgw
015200990930     C                   CLEAR                   DIV
015300990930     C                   CLEAR                   DIN
015400021119     c                   Clear                   Diw
015500000119     C                   CLEAR                   FIV
015600000119     C                   CLEAR                   FIN
015700021119     c                   Clear                   Fiw
015800021115     c                   Clear                   mdv
015900021115     c                   Clear                   mdn
016000941104     C                   MOVEL     *BLANKS       D51CT2
016100941104     C                   MOVEL     *BLANKS       D51CT3
016200941104     C                   MOVEL     *BLANKS       D51CT4
016300941104     C                   MOVEL     *BLANKS       D51CT5
016400941104     C                   MOVEL     *BLANKS       D51ES2
016500941104     C                   MOVEL     *BLANKS       D51ES3
016600941104     C                   MOVEL     *BLANKS       D51ES4
016700941104     C                   MOVEL     *BLANKS       D51ES5
016800941104     C*
016900941104     C                   MOVEL     *BLANKS       D51TKS
017000941104     C*
017100941104     C                   CLEAR                   D51PRG
017200941104     C*
017300941104     C* MITTENTI CODIFICATI --> CERCO LE TARIFFE
017400941104     C                   Z-ADD     0             B                 3 0
017500941104     C                   Z-ADD     0             C                 3 0
017600941104     C                   Z-ADD     0             D                 3 0
017700941110     C*
017800941110     C* SE 8888 O 9999 NON CARICO NULLA
017900941110     C                   MOVE      D51KSC        WCCM              4 0
018000941104     C*
018100941110     C     WCCM          IFNE      8888
018200941110     C     WCCM          ANDNE     9999
018300941110     C*
018400981006     C     D51KSC        SETLL     TNTAM000
018500981006     C     D51KSC        READE     TNTAM000                               30
018600941104     C*
018700941104    2C     *IN30         DOWEQ     *OFF
018800941104     C* ESCLUDO SE ANNULLATO
018900941104    3C     TAMATB        IFNE      'A'
019000941104     C* ESCLUDO TIPI TARIFFA BLOCCATI O NON DI COMPETENZA
019100990408     C* SOLO SE D51RIC<>'Y'
019200941104     C     TAMBAP        ANDNE     'B'
019300990408     C     TAMBAP        OREQ      'B'
019400990408     C     D51RIC        ANDEQ     'Y'
019500000119     C                   MOVEL     TAMFLO        DSTA01
019600000126     C*
019700020206     C* CARICO SOLO TARIFFE ITALIA           --> FIE "I"
019800020206     C* CARICO SOLO TARIFFE ESTERO           --> FIE "E"
019900020206     C* CARICO SOLO TARIFFE DPD              --> FIE "D"
020000020206     C* CARICO SOLO TARIFFE FEDEX            --> FIE "F"
020100021115     C* CARICO      TARIFFE DPD+FEDEX+EUROEX --> FIE "S"
020200020206     C* CARICO      TARIFFE DPD+FEDEX+ITALIA --> FIE "J"
020300021115     C* CARICO SOLO TARIFFE FEDEX DOCUMENTI  --> FIE "N"
020400021115     C* CARICO SOLO TARIFFE FEDEX MERCI      --> FIE "M"
020500000119   3AC     TAMFIE        IFEQ      ' '
020600000119     C*
020700000120     C     TAMFIE        OREQ      D51FIE
020800000119     C     §TADPD        ANDEQ     ' '
020900020206     C     §TAFED        ANDEQ     ' '
021000021115     C* SE TARIFFA DPD NON DEVO CARICARE  SE RICHIESTO I/E/F/M/N
021100000119     C     §TADPD        OREQ      'S'
021200000124     C     D51FIE        ANDNE     'I'
021300000124     C     D51FIE        ANDNE     'E'
021400020206     C     D51FIE        ANDNE     'F'
021500021115     c     D51Fie        andne     'M'
021600021115     c     D51Fie        andne     'N'
021700000124     C*
021800020206     C* SE TARIFFA FEDEX NON DEVO CARICARE  SE RICHIESTO I/E/D
021900020206     C     §TAFED        OREQ      'S'
022000020206     C     D51FIE        ANDNE     'I'
022100020206     C     D51FIE        ANDNE     'E'
022200020206     C     D51FIE        ANDNE     'D'
022300020206     C* LE ESTERE LE DEVO CARICARE SOLO PER S (OLTRE CHE DPD E FEDEX)
022400000124     C     TAMFIE        OREQ      'E'
022500000124     C     D51FIE        ANDEQ     'S'
022600020206     C* LE ITALIA LE DEVO CARICARE SOLO PER J (OLTRE CHE DPD E FEDEX)
022700000126     C     TAMFIE        OREQ      'I'
022800000126     C     D51FIE        ANDEQ     'J'
022900021115
023000021118     c                   Move      TamCtr        Wctr
023100021115      * Se Tamfie "M" Solo Tariffe merci FedEx
023200021121     c                   If        D51Fie = 'M' and Wctr >= §DfeDalD
023300021121     c                   GoTo      Notar
023400021121     c                   EndIf
023500021121      * Se Tamfie "N" Solo Tariffe documenti FedEx
023600021121     c                   If        D51Fie = 'N' and Wctr < §DfeDalD
023700021121     c                   GoTo      Notar
023800021121     c                   EndIf
023900021115
024000941107     C*
024100990408     C* SE C'E' TIPO BOLLA E D51RIC<>'Y', CARICO SOLO LE TARIFFE DEL
024200990408     C*  TIPO BOLLA IMMESSO
024300981007   3BC     D51TBO        IFEQ      ' '
024400981007     C     TAMBAP        OREQ      ' '
024500990408     C     D51RIC        OREQ      'Y'
024600941107     C     TAMBAP        OREQ      D51TBO
024700941104     C*
024800941104     C* VEDO SE E' TARIFFA VALIDA
024900941104    4C     D51DSP        IFGE      TAMDDT
025000941104     C     D51DSP        ANDLE     TAMDST
025100941104     C*
025200941104     C                   ADD       1             B
025300941104     C                   MOVEL     TAMCTR        CTV(B)
025400941104     C                   MOVEL     TAMTSP        SPV(B)
025500941104     C                   MOVE      TAMFTP        TPV(B)
025600941104     C                   MOVE      TAMPRG        PGV(B)
025700990930     C                   MOVE      §TADIV        DIV(B)
025800000119     C                   MOVE      TAMFIE        FIV(B)
025900000119     C* TARIFFA DPD
026000000119     C     §TADPD        IFEQ      'S'
026100000126     C                   MOVE      'D'           FIV(B)
026200000119     C                   ENDIF
026300020206     C* TARIFFA FEDEX
026400020206     C     §TAFED        IFEQ      'S'
026500020206     C                   MOVE      'F'           FIV(B)
026600021115     c                   Clear                   mdv(b)
026700021115     c                   If        D51Fie = 'M' and Wctr >= §DfeDalM
026800021115     c                                          and Wctr <= §DfeAlM
026900021115     c                   Eval      mdv(b) = 'M'
027000021115     c                   EndIf
027100021115     c                   If        D51Fie = 'N' and Wctr >= §DfeDalD
027200021115     c                                          and Wctr <= §DfeAlD
027300021115     c                   Eval      mdv(b) = 'N'
027400021115     c                   EndIf
027500020206     C                   ENDIF
027600941104     C     B             COMP      150                                30
027700941104     C*
027800941104     C* TARIFFE NON VALIDE
027900941104   X4C                   ELSE
028000941104     C*
028100941104    5C     C             IFLT      150
028200941104     C                   MOVEL     TAMCTR        COMCT2            3
028300941104     C                   Z-ADD     1             D
028400941104     C     COMCT2        LOOKUP    CTN(D)                                 31
028500941104     C*
028600941104     C* DELLE TARIFFE NON VALIDE, TENGO SEMPRE L'ULTIMA
028700941104    6C     *IN31         IFEQ      *OFF
028800941104     C                   ADD       1             C
028900941104     C                   MOVEL     TAMCTR        CTN(C)
029000941104     C                   Z-ADD     C             D
029100941104    6C                   ENDIF
029200941104     C                   MOVEL     TAMTSP        SPN(D)
029300941104     C                   MOVE      TAMFTP        TPN(D)
029400941104     C                   MOVE      TAMPRG        PGN(D)
029500990930     C                   MOVE      §TADIV        DIN(D)
029600000119     C                   MOVE      TAMFIE        FIN(D)
029700000126     C* TARIFFA DPD
029800000126     C     §TADPD        IFEQ      'S'
029900000201     C                   MOVE      'D'           FIN(D)
030000000126     C                   ENDIF
030100020206     C* TARIFFA FEDEX
030200020206     C     §TAFED        IFEQ      'S'
030300020206     C                   MOVE      'F'           FIN(D)
030400021115     c                   Clear                   mdn(d)
030500021115     c                   If        D51Fie = 'M' and Wctr >= §DfeDalM
030600021115     c                                          and Wctr <= §DfeAlM
030700021115     c                   Eval      mdn(d) = 'M'
030800021115     c                   EndIf
030900021115     c                   If        D51Fie = 'N' and Wctr >= §DfeDalD
031000021115     c                                          and Wctr <= §DfeAlD
031100021115     c                   Eval      mdn(d) = 'N'
031200021115     c                   EndIf
031300020206     C                   ENDIF
031400941104    5C                   ENDIF
031500941104     C*
031600941104    4C                   ENDIF
031700981007   3BC                   ENDIF
031800981007   3AC                   ENDIF
031900941107    3C                   ENDIF
032000021115
032100021121     c     Notar         Tag
032200021121
032300941104     C*
032400010316     C  N30D51KSC        READE     TNTAM000                               30
032500981006     C*
032600941104    2C                   ENDDO
032700941104     C*
032800941104     C                   ADD       1             B
032900941104     C*
033000941104     C* CARICO TUTTI I TIPI TARIFFA TROVATI IN UNICA SCHIERA
033100941104     C* SE NON NE HO GIA' TROVATI 150 VALIDI
033200941104    2C     B             IFLE      150
033300941104     C*
033400941104     C*
033500941104     C* SE ESISTE ALMENO UNA TARIFFA VALIDA, NON CONSIDERO LA 'P'
033600941104     C*  DI PREFERENZIALE PER LE TARIFFE SCADUTE
033700941104    3C     B             IFGT      1
033800941104     C                   CLEAR                   TPN
033900941104    3C                   ENDIF
034000941104     C*
034100941104     C                   Z-ADD     1             C
034200941104     C*
034300941104    3C     CTN(C)        DOWNE     *BLANKS
034400941104     C     B             ANDLE     150
034500941104     C*
034600000119     C     CTN(C)        LOOKUP    CTV                                    31
034700941104    4C     *IN31         IFEQ      *OFF
034800941104     C                   MOVEL     CTN(C)        CTV(B)
034900941104     C                   MOVEL     TPN(C)        TPV(B)
035000941104     C                   MOVEL     SPN(C)        SPV(B)
035100941104     C                   MOVEL     PGN(C)        PGV(B)
035200990930     C                   MOVEL     DIN(C)        DIV(B)
035300000119     C                   MOVEL     FIN(C)        FIV(B)
035400021115     c                   Movel     mdn(c)        mdv(b)
035500941104     C                   ADD       1             B
035600941104    4C                   ENDIF
035700941104     C*
035800941104     C                   ADD       1             C
035900941104    3C                   END
036000941104    2C                   END
036100021115     C*
036200941107     C                   EXSR      VISTAR
036300941110    2C                   END
036400941104     C*
036500941104     C                   ENDSR
036600941104     C*
036700941104     C* RICERCA E CARICAMENTO A VIDEO TARIFFE ------------------------*
036800941104     C     RICTAR        BEGSR
036900941104     C                   SETOFF                                       30
037000950125     C                   CLEAR                   W001A
037100941104     C*
037200941104     C* SE NON CI SONO TARIFFE IMPOSTO LA TARIFFA 000
037300941104    2C     CTV(1)        IFEQ      *BLANKS
037400941215     C*
037500941215     C     D51CTR        IFEQ      *BLANKS
037600941104     C                   MOVEL     *ZEROS        D51CTR
037700941107     C                   CLEAR                   D51TKS
037800941215     C                   ENDIF
037900941215     C*
038000941104   X2C                   ELSE
038100941107     C*
038200941107     C* CODICE TARIFFA IMPOSTATO: PRENDO IL RELATIVO PROGRESSIVO
038300941107   2AC     D51CTR        IFNE      *BLANKS
038400941107     C                   Z-ADD     1             C
038500941107     C     D51CTR        LOOKUP    CTV(C)                                 30
038600941219     C   30              MOVEL     PGV(C)        D51PRG
038700990930     C   30              MOVEL     DIV(C)        D51DIV
038800991026     C   30D51DIV        IFEQ      '   '
038900011026     C                   MOVEL     'ITL'         D51DIV
039000991026     C                   ENDIF
039100941219     C*
039200941219     C* ERRORE --> TARIFFA NON TROVATA
039300941219    3C     *IN30         IFEQ      *OFF
039400941219     C                   MOVEL     '3'           D51ERR
039500941219     C                   MOVEL     ERR(3)        D51MSG
039600941219     C                   Z-ADD     10            C
039700941219    3C                   ENDIF
039800941219     C*
039900941219   2AC                   ENDIF
040000941107     C**
040100941107   2AC     D51CTR        IFEQ      *BLANKS
040200021115
040300021115      * Carico le sk di work
040400021115     c                   ExSr      CarSk
040500021115
040600941104     C*
040700941104     C* SE IMPOSTATO IL TIPO SERVIZIO --> CERCO QUELLO
040800941104    3C     D51TSP        IFNE      *BLANKS
040900941104     C* CERCO SE C'E' LA PREFERENZIALE ALTRIMENTI LA 1 CHE TROVO
041000941104     C                   Z-ADD     1             C
041100941104     C                   SETON                                        31
041200941104     C*
041300941104    4C     *IN31         DOWEQ     *ON
041400021115     C     D51TSP        LOOKUP    SPw(C)                                 31
041500941104     C* SE TROVATA ED E' PREFERENZIALE ESCO DAL CICLO
041600021115    5C   31TPw(C)        IFEQ      'P'
041700941104     C                   SETON                                        30
041800941104     C                   SETOFF                                       31
041900941104     C                   MOVEL     '1'           W001A             1
042000941104   X5C                   ELSE
042100021115     C* SE TROVATA MA NON PREFERENZIALE ACCENDO COMUNQUE L'INDICATORE
042200941104     C*  DI TROVATO MA FACCIO UN ALTRO GIRO
042300941104     C                   SETON                                        30
042400941104     C                   ADD       1             C
042500941104    5C                   ENDIF
042600941104    4C                   ENDDO
042700941104     C*
042800941104     C* SE HO TROVATO LA TARIFFA MA NON LA PREFERENZIALE, LA RICERCO X
042900941104     C*  PRENDERE LA POSIZIONE
043000941104    4C   30W001A         IFEQ      ' '
043100941104     C                   Z-ADD     1             C
043200021115     C     D51TSP        LOOKUP    SPw(C)                                 31
043300941104    4C                   ENDIF
043400941104     C*
043500941104    3C                   ENDIF
043600941104     C*
043700941104     C* SE NON TROVATA O NON IMPOSTATO IL TIPO SERVIZIO -->
043800941104     C*  CERCO LA PRIMA PREFERENZIALE
043900941104    3C     *IN30         IFEQ      *OFF
044000941104     C                   Z-ADD     1             C
044100021115     C     'P'           LOOKUP    TPw(C)                                 30
044200941104    3C                   ENDIF
044300941104     C*
044400941104     C* SE NON ESISTE LA PREFERENZIALE METTO LA PRIMA VALIDA TROVATA
044500941104     C  N30              Z-ADD     1             C
044600941104     C*
044700021115     C                   MOVEL     CTw(C)        D51CTR
044800021115     C                   MOVEL     PGw(C)        D51PRG
044900021115     C                   MOVEL     DIw(C)        D51DIV
045000991026     C     D51DIV        IFEQ      '   '
045100011026     C                   MOVEL     'ITL'         D51DIV
045200991026     C                   ENDIF
045300941104    2C                   ENDIF
045400021115      * Cerco la posizione nella sk valida
045500021115     c                   Z-Add     1             c
045600021115     c     D51Ctr        Lookup    Ctv(c)                                 30
045700941219   2AC                   ENDIF
045800941107     C*
045900021115     C                   EXSR      VISTAR
046000941104     C*
046100941104     C* RIMPIAZZO CON COD TARIFFA 5 SOLO SE ESISTE
046200021115    1C     CTv(5)        IFNE      *BLANKS
046300941104     C* SE NELLA 1°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
046400941104    2C     C             IFEQ      1
046500021115     C                   MOVEL     CTv(5)        D51CT2
046600021115     C                   MOVEL     SPv(5)        D51ES2
046700021115     C                   MOVE      FIv(5)        D51ES2
046800941107     C                   ELSE
046900941104    2C                   END
047000941104     C* SE NELLA 2°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
047100941104    2C     C             IFEQ      2
047200021115     C                   MOVEL     CTv(5)        D51CT3
047300021115     C                   MOVEL     SPv(5)        D51ES3
047400021115     C                   MOVE      FIv(5)        D51ES3
047500941104    2C                   END
047600941104     C* SE NELLA 3°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
047700941104    2C     C             IFEQ      3
047800021115     C                   MOVEL     CTv(5)        D51CT4
047900021115     C                   MOVEL     SPv(5)        D51ES4
048000021115     C                   MOVE      FIv(5)        D51ES4
048100941104    2C                   END
048200941104     C* SE NELLA 4°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
048300941104    2C     C             IFEQ      4
048400021115     C                   MOVEL     CTv(5)        D51CT5
048500021115     C                   MOVEL     SPv(5)        D51ES5
048600021115     C                   MOVE      FIv(5)        D51ES5
048700941104    2C                   END
048800941104     C*
048900941104    1C                   END
049000941104     C*
049100941104     C* DECODIFICA FISSA TARIFFA
049200941104     C                   Z-ADD     1             C
049300941104     C                   MOVEL     D51CTR        UNOCTR            1
049400941104     C     UNOCTR        LOOKUP    CTA(C)                                 30
049500941104     C   30              MOVEL     DTA(C)        D51DFS
049600941104     C*
049700941104     C                   ENDSR
049800021115
049900021115      *--------------------------------------------------------------*
050000021115      * Carico le sk di work                                         *
050100021115      *--------------------------------------------------------------*
050200021115     c     Carsk         BegSr
050300021115
050400021115     c                   If        D51Fie = 'F'
050500021115     c     'M'           Lookup    mdv                                    35
050600021115     c     'N'           Lookup    mdv                                    36
050700021115     c                   EndIf
050800021115
050900021115     c                   Clear                   d
051000021115
051100021115     c                   Do        150           b
051200021115     c                   If        Ctv(b) = *Blanks
051300021115     c                   Leave
051400021115     c                   EndIf
051500021115
051600021115     c                   If        D51Fie = 'F' and *In35 and *In36
051700021115     c                             and Mdv(b) <> 'N'
051800021115     c                   Iter
051900021115     c                   EndIf
052000021115
052100021115     c                   Add       1             d
052200021115
052300021115     c                   Eval      Ctw(d) = Ctv(b)
052400021115     c                   Eval      Tpw(d) = Tpv(b)
052500021115     c                   Eval      Spw(d) = Spv(b)
052600021115     c                   Eval      Pgw(d) = Pgv(b)
052700021115     c                   Eval      Diw(d) = Div(b)
052800021115     c                   Eval      Fiw(d) = Fiv(b)
052900021115     c                   EndDo
053000021115
053100021115     c                   EndSr
053200941104     C*
053300941104     C* CONTROLLO CODICE TARIFFA ------------------------------------*
053400941104     C     CTRCTR        BEGSR
053500941215     C* SE ESISTE ALMENO UNA TARIFFA PER IL CLIENTE ANCHE SE SCADUTA
053600941215     C*  LO SEGNALO
053700941215     C     CTV(1)        IFNE      *BLANKS
053800941215     C                   MOVEL     'S'           D51TKS
053900941215     C                   ENDIF
054000941215     C*
054100941110     C                   MOVE      D51KSC        WCCM              4 0
054200941104     C* RICERCA
054300941104     C     '?'           SCAN      D51CTR                                 30
054400941104     C*
054500941104    1C     *IN30         IFEQ      *ON
054600941104     C*
054700941104     C* SOLO SE CODIFICATO DELL'AREA POSSO FARE LA RICERCA
054800941104    2C     WCCM          IFEQ      8888
054900941104     C     WCCM          OREQ      9999
055000941104     C                   MOVEL     '1'           D51ERR
055100941104     C                   MOVEL     ERR(1)        D51MSG
055200941104     C                   GOTO      ENDCTR
055300941104    2C                   ENDIF
055400941104     C*
055500941104     C* RICERCA
055600941104     C                   CLEAR                   PARAM4
055700941104     C                   MOVEL     D51KSC        PA4COD
055800941104     C                   Z-ADD     D51KCI        PA4CCC
055900061213     c                   movel     '2'           pa4vpr
056000941104     C                   MOVEL     PARAM4        KPJBU
056100941104     C                   CALL      'TNTA36R'
056200941104     C                   PARM                    KPJBA
056300941104     C                   MOVEL     KPJBU         PARAM4
056400941104     C*
056500941104    2C     PA4CTR        IFNE      *BLANKS
056600941104     C* SE TROVATO METTO QUELLO SCELTO
056700941104     C                   MOVE      PA4CTR        D51CTR
056800941104     C* DESCRIZIONE FISSA
056900941104     C                   MOVEL     PA4CTR        UNOCTR
057000941104     C                   Z-ADD     1             C
057100941104     C     UNOCTR        LOOKUP    CTA(C)                                 30
057200941104     C   30              MOVEL     DTA(C)        D51DFS
057300941104     C  N30              MOVEL     *BLANKS       D51DFS
057400941104     C*
057500941104   X2C                   ELSE
057600941104     C                   MOVEL     *ZEROS        D51CTR
057700941104    2C                   END
057800941107     C                   MOVEL     'R'           D51ERR
057900941104     C                   GOTO      ENDCTR
058000941104     C*
058100941104   X1C                   ELSE
058200941104     C*
058300941116     C* CONTROLLO FORMALE
058400941116     C                   EXSR      CTRFOR
058500941116     C*
058600941116     C     D51ERR        IFNE      ' '
058700941104     C                   GOTO      ENDCTR
058800941104    2C                   ENDIF
058900941104     C*
059000941104     C* CONTROLLO ESISTENZA TARIFFA
059100941104    2C     CTV(1)        IFNE      *BLANKS
059200941104     C                   Z-ADD     1             B
059300941104     C     D51CTR        LOOKUP    CTV(B)                                 30
059400941219     C   30              MOVEL     PGV(B)        D51PRG
059500990930     C   30              MOVEL     DIV(B)        D51DIV
059600991026     C   30D51DIV        IFEQ      '   '
059700011026     C                   MOVEL     'ITL'         D51DIV
059800991026     C                   ENDIF
059900941104     C* INESISTENTE: ERRORE
060000941104    3C  N30CTV(1)        IFNE      *BLANKS
060100941104     C                   MOVEL     '3'           D51ERR
060200941104     C                   MOVEL     ERR(3)        D51MSG
060300941104     C                   GOTO      ENDCTR
060400941104    3C                   END
060500941104    2C                   END
060600941104    1C                   END
060700941104     C     ENDCTR        ENDSR
060800941107     C*
060900941107     C* CONTROLLO FORMALE CODICE TARIFFA -----------------------------*
061000941107     C     CTRFOR        BEGSR
061100941107     C*
061200941107     C                   MOVEL     D51CTR        UNOCTR            1
061300941107     C                   Z-ADD     1             C
061400941107     C     UNOCTR        LOOKUP    CTA(C)                                 30
061500941107     C*
061600941107    2C     *IN30         IFEQ      *OFF
061700941118     C     UNOCTR        OREQ      ' '
061800941107     C                   MOVEL     ERR(2)        D51MSG
061900941107     C                   MOVEL     '2'           D51ERR
062000941107    2C                   ENDIF
062100941107     C*
062200941107    1C     D51ERR        IFEQ      *BLANKS
062300941107     C*
062400941107     C                   MOVEL     DTA(C)        D51DFS
062500941107     C* CONTROLLO CHE LA 2 E 3 CIFRA SIANO UN NUERO
062600941107     C                   MOVE      D51CTR        W002A             2
062700941107     C                   MOVEL     W002A         W001A
062800941107     C*
062900941107    2C                   DO        2
063000941107    3C     W001A         IFNE      '0'
063100941107     C     W001A         ANDNE     '1'
063200941107     C     W001A         ANDNE     '2'
063300941107     C     W001A         ANDNE     '3'
063400941107     C     W001A         ANDNE     '4'
063500941107     C     W001A         ANDNE     '5'
063600941107     C     W001A         ANDNE     '6'
063700941107     C     W001A         ANDNE     '7'
063800941107     C     W001A         ANDNE     '8'
063900941107     C     W001A         ANDNE     '9'
064000941107     C                   MOVEL     ERR(4)        D51MSG
064100941107     C                   MOVEL     '4'           D51ERR
064200941107    3C                   ENDIF
064300941107     C*
064400941107     C                   MOVE      W002A         W001A
064500941107    2C                   ENDDO
064600941107    1C                   ENDIF
064700941107     C*
064800941107     C                   ENDSR
064900941104     C*
065000981006     C* SBR INIZIO ---------------------------------------------------*
065100941117     C     *INZSR        BEGSR
065200981006      *
065300981006      * ACCESSO   TABEL
065400981006      *
065500941104     C     KTAB3         KLIST
065600941104     C                   KFLD                    CODUT
065700941104     C                   KFLD                    COD               2
065800981006      *
065900941110     C                   Z-ADD     1             CODUT             1 0
066000981006      ***
066100981006      *  DETERMINO SE SONO SEDE O FILIALE ED APRO IL FILE OPPORTUNO
066200981006      ***
066300981006     C                   CALL      'X§PARUT'
066400981006     C                   PARM                    UT§DSE
066500981006      *
066600981006     C                   MOVEL     REC80         CNCR80
066700981006     C                   OPEN      TNTAM01L
066800981006     C***
066900941104     C* CARICO TABELLA TR : TIPI TARIFFA
067000941104     C***
067100981006     C                   OPEN      TABEL00F
067200981006      *
067300941104     C                   MOVEL     'TR'          COD
067400941104     C                   Z-ADD     1             C
067500941104     C     KTAB3         SETLL     TABEL
067600941104     C     KTAB3         READE     TABEL                                  30
067700941104     C*
067800941104     C     *IN30         DOWEQ     '0'
067900941104     C     TBLFLG        IFEQ      ' '
068000941104     C                   MOVEL     TBLKEY        CTA(C)
068100941104     C                   MOVEL     TBLUNI        DTA(C)
068200941104     C                   ADD       1             C
068300941104     C                   ENDIF
068400941104     C*
068500941104     C     KTAB3         READE     TABEL                                  30
068600941104     C                   ENDDO
068700941104     C*
068800941104     C                   CLOSE     TABEL00F
068900941117     C*
069000941117     C                   CLEAR                   WKSC
069100941117     C                   CLEAR                   WFIE
069200941117     C                   CLEAR                   WTBO
069300941104     C*
069400941104     C                   ENDSR
069500941107     C*
069600941107     C* RIEMPO I CAMPI VIDEO-----------------------------------------*
069700941107     C     VISTAR        BEGSR
069800941107     C     CTV(1)        IFNE      *BLANKS
069900941107     C                   MOVEL     CTV(1)        D51CT2
070000941107     C                   MOVEL     SPV(1)        D51ES2
070100021121     C                   MOVE      Fiv(1)        D51ES2
070200941107     C* SE ESISTE ALMENO UNA TARIFFA PER IL CLIENTE ANCHE SE SCADUTA
070300941107     C*  LO SEGNALO
070400941107     C                   MOVEL     'S'           D51TKS
070500941107     C                   ENDIF
070600941107     C*
070700941107     C     CTV(2)        IFNE      *BLANKS
070800941107     C                   MOVEL     CTV(2)        D51CT3
070900941107     C                   MOVEL     SPV(2)        D51ES3
071000021121     C                   MOVE      Fiv(2)        D51ES3
071100941107     C                   ENDIF
071200941107     C     CTV(3)        IFNE      *BLANKS
071300941107     C                   MOVEL     CTV(3)        D51CT4
071400941107     C                   MOVEL     SPV(3)        D51ES4
071500021121     C                   MOVE      Fiv(3)        D51ES4
071600941107     C                   ENDIF
071700941107     C     CTV(4)        IFNE      *BLANKS
071800941107     C                   MOVEL     CTV(4)        D51CT5
071900941107     C                   MOVEL     SPV(4)        D51ES5
072000021121     C                   MOVE      Fiv(4)        D51ES5
072100941107     C                   ENDIF
072200941107     C                   ENDSR
072300941118**
072400941104Per il codice cliente indicato e' impossibile eseguire la ricerca tariffe
072500941104Tipo Tariffa errato
072600941104Codice tariffa inesistente per il cliente indicato
072700941118Codice tariffa errato
