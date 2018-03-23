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
005500061220     d  pa4prg                37     39
005600061220     d  pa4dtr                40     47
005700941104     D KPJBA         E DS
005800941104     C*---------------------------------------------------------------*
005900941104     C     *ENTRY        PLIST
006000941104     C                   PARM                    DSLV51
006100941104     C*---------------------------------------------------------------*
006200941104     C* TIPO LANCIO, D51TLA "C" -           CHIUSO CON LR
006300941104     C* TIPO LANCIO, D51TLA "L" - ELABORO E CHIUDO CON LR
006400941104     C* TIPO LANCIO, D51TLA " " - ELABORO E CHIUDO IN RETRN
006500941104     C*
006600941104    1C     D51TLA        IFNE      'C'
006700941107     C*
006800941107     C* PULIZIA CAMPI DI OUTPUT
006900941107     C                   CLEAR                   D51MSG
007000941107     C                   CLEAR                   D51ERR
007100941107     C                   CLEAR                   D51DFS
007200941107     C                   CLEAR                   D51PRG
007300990930     C                   CLEAR                   D51DIV
007400941107     C*
007500941117     C* SOLO CONTROLLO FORMALE
007600941117    2C     D51CFO        IFEQ      'S'
007700941117     C                   EXSR      CTRFOR
007800941117     C*
007900941117   X2C                   ELSE
008000021115
008100021115      * Carico tabella DFE
008200021115     c                   Clear                   DDfe
008300021115     c                   Clear                   Tibs02Ds
008400021115     c                   Eval      T02Mod = 'C'
008500021115     c                   Clear                   T02Sif
008600021115     c                   Eval      T02Cod = 'DFE'
008700021118     c                   Movel(p)  'FED'         T02Ke1
008800021115     c                   Call      'TIBS02R'
008900021115     c                   Parm                    Kpjba
009000021115     c                   Parm                    Tibs02Ds
009100021115     c                   If        T02err = *Blanks
009200021115     c                   Movel     T02Uni        DDfe
009300021115     c                   EndIf
009400021115
009500941117     C*
009600941117    3C     D51KSC        IFNE      WKSC
009700941117     C     D51FIE        ORNE      WFIE
009800941117     C     D51TBO        ORNE      WTBO
009900961007     C     D51DSP        ORNE      WDSP
010000941104     C                   EXSR      CARCTR
010100941231     C                   ELSE
010200941231     C                   EXSR      VISTAR
010300941231    3C                   ENDIF
010400941231     C                   MOVEL     D51KSC        WKSC              7 0
010500961007     C                   MOVEL     D51DSP        WDSP              8 0
010600941231     C                   MOVEL     D51FIE        WFIE              1
010700941231     C                   MOVEL     D51TBO        WTBO              1
010800941104     C*
010900941215    3C     D51RIC        IFEQ      'S'
011000990408    3C     D51RIC        OREQ      'Y'
011100941104     C                   EXSR      RICTAR
011200941215    3C                   ENDIF
011300941104     C*
011400941215    3C     D51CON        IFEQ      'S'
011500941104     C                   EXSR      CTRCTR
011600941215    3C                   ENDIF
011700941215     C*
011800941215     C* DECODIFICA FISSA TARIFFA
011900941215     C     D51CTR        IFNE      *BLANKS
012000941215     C                   Z-ADD     1             C
012100941215     C                   MOVEL     D51CTR        UNOCTR            1
012200941215     C     UNOCTR        LOOKUP    CTA(C)                                 30
012300941215     C   30              MOVEL     DTA(C)        D51DFS
012400941215     C                   ELSE
012500941215     C                   CLEAR                   D51DFS
012600941215     C                   ENDIF
012700941215     C*
012800941117    2C                   ENDIF
012900941107     C*
013000941104    1C                   ENDIF
013100941104     C* CHIUSURA PGM
013200941104    1C     D51TLA        IFEQ      ' '
013300941104     C                   RETURN
013400941104     C                   ELSE
013500941104     C                   SETON                                        LR
013600941104    1C                   ENDIF
013700941104     C*
013800941104     C* CARICO LE TARIFFE DEL CLIENTE --------------------------------*
013900941104     C     CARCTR        BEGSR
014000941104     C*
014100941104     C* PULISCO SKIERE DELLE TARIFFE
014200941104     C                   CLEAR                   CTV
014300941104     C                   CLEAR                   CTN
014400021119     c                   Clear                   Ctw
014500941104     C                   CLEAR                   TPN
014600941104     C                   CLEAR                   TPV
014700021119     c                   Clear                   Tpw
014800941104     C                   CLEAR                   SPN
014900941104     C                   CLEAR                   SPV
015000021119     c                   Clear                   Spw
015100941104     C                   CLEAR                   PGV
015200941104     C                   CLEAR                   PGN
015300021119     c                   Clear                   Pgw
015400990930     C                   CLEAR                   DIV
015500990930     C                   CLEAR                   DIN
015600021119     c                   Clear                   Diw
015700000119     C                   CLEAR                   FIV
015800000119     C                   CLEAR                   FIN
015900021119     c                   Clear                   Fiw
016000021115     c                   Clear                   mdv
016100021115     c                   Clear                   mdn
016200941104     C                   MOVEL     *BLANKS       D51CT2
016300941104     C                   MOVEL     *BLANKS       D51CT3
016400941104     C                   MOVEL     *BLANKS       D51CT4
016500941104     C                   MOVEL     *BLANKS       D51CT5
016600941104     C                   MOVEL     *BLANKS       D51ES2
016700941104     C                   MOVEL     *BLANKS       D51ES3
016800941104     C                   MOVEL     *BLANKS       D51ES4
016900941104     C                   MOVEL     *BLANKS       D51ES5
017000941104     C*
017100941104     C                   MOVEL     *BLANKS       D51TKS
017200941104     C*
017300941104     C                   CLEAR                   D51PRG
017400941104     C*
017500941104     C* MITTENTI CODIFICATI --> CERCO LE TARIFFE
017600941104     C                   Z-ADD     0             B                 3 0
017700941104     C                   Z-ADD     0             C                 3 0
017800941104     C                   Z-ADD     0             D                 3 0
017900941110     C*
018000941110     C* SE 8888 O 9999 NON CARICO NULLA
018100941110     C                   MOVE      D51KSC        WCCM              4 0
018200941104     C*
018300941110     C     WCCM          IFNE      8888
018400941110     C     WCCM          ANDNE     9999
018500941110     C*
018600981006     C     D51KSC        SETLL     TNTAM000
018700981006     C     D51KSC        READE     TNTAM000                               30
018800941104     C*
018900941104    2C     *IN30         DOWEQ     *OFF
019000941104     C* ESCLUDO SE ANNULLATO
019100941104    3C     TAMATB        IFNE      'A'
019200941104     C* ESCLUDO TIPI TARIFFA BLOCCATI O NON DI COMPETENZA
019300990408     C* SOLO SE D51RIC<>'Y'
019400941104     C     TAMBAP        ANDNE     'B'
019500990408     C     TAMBAP        OREQ      'B'
019600990408     C     D51RIC        ANDEQ     'Y'
019700000119     C                   MOVEL     TAMFLO        DSTA01
019800000126     C*
019900020206     C* CARICO SOLO TARIFFE ITALIA           --> FIE "I"
020000020206     C* CARICO SOLO TARIFFE ESTERO           --> FIE "E"
020100020206     C* CARICO SOLO TARIFFE DPD              --> FIE "D"
020200020206     C* CARICO SOLO TARIFFE FEDEX            --> FIE "F"
020300021115     C* CARICO      TARIFFE DPD+FEDEX+EUROEX --> FIE "S"
020400020206     C* CARICO      TARIFFE DPD+FEDEX+ITALIA --> FIE "J"
020500021115     C* CARICO SOLO TARIFFE FEDEX DOCUMENTI  --> FIE "N"
020600021115     C* CARICO SOLO TARIFFE FEDEX MERCI      --> FIE "M"
020700000119   3AC     TAMFIE        IFEQ      ' '
020800000119     C*
020900000120     C     TAMFIE        OREQ      D51FIE
021000000119     C     §TADPD        ANDEQ     ' '
021100020206     C     §TAFED        ANDEQ     ' '
021200021115     C* SE TARIFFA DPD NON DEVO CARICARE  SE RICHIESTO I/E/F/M/N
021300000119     C     §TADPD        OREQ      'S'
021400000124     C     D51FIE        ANDNE     'I'
021500000124     C     D51FIE        ANDNE     'E'
021600020206     C     D51FIE        ANDNE     'F'
021700021115     c     D51Fie        andne     'M'
021800021115     c     D51Fie        andne     'N'
021900000124     C*
022000020206     C* SE TARIFFA FEDEX NON DEVO CARICARE  SE RICHIESTO I/E/D
022100020206     C     §TAFED        OREQ      'S'
022200020206     C     D51FIE        ANDNE     'I'
022300020206     C     D51FIE        ANDNE     'E'
022400020206     C     D51FIE        ANDNE     'D'
022500020206     C* LE ESTERE LE DEVO CARICARE SOLO PER S (OLTRE CHE DPD E FEDEX)
022600000124     C     TAMFIE        OREQ      'E'
022700000124     C     D51FIE        ANDEQ     'S'
022800020206     C* LE ITALIA LE DEVO CARICARE SOLO PER J (OLTRE CHE DPD E FEDEX)
022900000126     C     TAMFIE        OREQ      'I'
023000000126     C     D51FIE        ANDEQ     'J'
023100021115
023200021118     c                   Move      TamCtr        Wctr
023300021115      * Se Tamfie "M" Solo Tariffe merci FedEx
023400021121     c                   If        D51Fie = 'M' and Wctr >= §DfeDalD
023500021121     c                   GoTo      Notar
023600021121     c                   EndIf
023700021121      * Se Tamfie "N" Solo Tariffe documenti FedEx
023800021121     c                   If        D51Fie = 'N' and Wctr < §DfeDalD
023900021121     c                   GoTo      Notar
024000021121     c                   EndIf
024100021115
024200941107     C*
024300990408     C* SE C'E' TIPO BOLLA E D51RIC<>'Y', CARICO SOLO LE TARIFFE DEL
024400990408     C*  TIPO BOLLA IMMESSO
024500981007   3BC     D51TBO        IFEQ      ' '
024600981007     C     TAMBAP        OREQ      ' '
024700990408     C     D51RIC        OREQ      'Y'
024800941107     C     TAMBAP        OREQ      D51TBO
024900941104     C*
025000941104     C* VEDO SE E' TARIFFA VALIDA
025100941104    4C     D51DSP        IFGE      TAMDDT
025200941104     C     D51DSP        ANDLE     TAMDST
025300941104     C*
025400941104     C                   ADD       1             B
025500941104     C                   MOVEL     TAMCTR        CTV(B)
025600941104     C                   MOVEL     TAMTSP        SPV(B)
025700941104     C                   MOVE      TAMFTP        TPV(B)
025800941104     C                   MOVE      TAMPRG        PGV(B)
025900990930     C                   MOVE      §TADIV        DIV(B)
026000000119     C                   MOVE      TAMFIE        FIV(B)
026100000119     C* TARIFFA DPD
026200000119     C     §TADPD        IFEQ      'S'
026300000126     C                   MOVE      'D'           FIV(B)
026400000119     C                   ENDIF
026500020206     C* TARIFFA FEDEX
026600020206     C     §TAFED        IFEQ      'S'
026700020206     C                   MOVE      'F'           FIV(B)
026800021115     c                   Clear                   mdv(b)
026900021115     c                   If        D51Fie = 'M' and Wctr >= §DfeDalM
027000021115     c                                          and Wctr <= §DfeAlM
027100021115     c                   Eval      mdv(b) = 'M'
027200021115     c                   EndIf
027300021115     c                   If        D51Fie = 'N' and Wctr >= §DfeDalD
027400021115     c                                          and Wctr <= §DfeAlD
027500021115     c                   Eval      mdv(b) = 'N'
027600021115     c                   EndIf
027700020206     C                   ENDIF
027800941104     C     B             COMP      150                                30
027900941104     C*
028000941104     C* TARIFFE NON VALIDE
028100941104   X4C                   ELSE
028200941104     C*
028300941104    5C     C             IFLT      150
028400941104     C                   MOVEL     TAMCTR        COMCT2            3
028500941104     C                   Z-ADD     1             D
028600941104     C     COMCT2        LOOKUP    CTN(D)                                 31
028700941104     C*
028800941104     C* DELLE TARIFFE NON VALIDE, TENGO SEMPRE L'ULTIMA
028900941104    6C     *IN31         IFEQ      *OFF
029000941104     C                   ADD       1             C
029100941104     C                   MOVEL     TAMCTR        CTN(C)
029200941104     C                   Z-ADD     C             D
029300941104    6C                   ENDIF
029400941104     C                   MOVEL     TAMTSP        SPN(D)
029500941104     C                   MOVE      TAMFTP        TPN(D)
029600941104     C                   MOVE      TAMPRG        PGN(D)
029700990930     C                   MOVE      §TADIV        DIN(D)
029800000119     C                   MOVE      TAMFIE        FIN(D)
029900000126     C* TARIFFA DPD
030000000126     C     §TADPD        IFEQ      'S'
030100000201     C                   MOVE      'D'           FIN(D)
030200000126     C                   ENDIF
030300020206     C* TARIFFA FEDEX
030400020206     C     §TAFED        IFEQ      'S'
030500020206     C                   MOVE      'F'           FIN(D)
030600021115     c                   Clear                   mdn(d)
030700021115     c                   If        D51Fie = 'M' and Wctr >= §DfeDalM
030800021115     c                                          and Wctr <= §DfeAlM
030900021115     c                   Eval      mdn(d) = 'M'
031000021115     c                   EndIf
031100021115     c                   If        D51Fie = 'N' and Wctr >= §DfeDalD
031200021115     c                                          and Wctr <= §DfeAlD
031300021115     c                   Eval      mdn(d) = 'N'
031400021115     c                   EndIf
031500020206     C                   ENDIF
031600941104    5C                   ENDIF
031700941104     C*
031800941104    4C                   ENDIF
031900981007   3BC                   ENDIF
032000981007   3AC                   ENDIF
032100941107    3C                   ENDIF
032200021115
032300021121     c     Notar         Tag
032400021121
032500941104     C*
032600010316     C  N30D51KSC        READE     TNTAM000                               30
032700981006     C*
032800941104    2C                   ENDDO
032900941104     C*
033000941104     C                   ADD       1             B
033100941104     C*
033200941104     C* CARICO TUTTI I TIPI TARIFFA TROVATI IN UNICA SCHIERA
033300941104     C* SE NON NE HO GIA' TROVATI 150 VALIDI
033400941104    2C     B             IFLE      150
033500941104     C*
033600941104     C*
033700941104     C* SE ESISTE ALMENO UNA TARIFFA VALIDA, NON CONSIDERO LA 'P'
033800941104     C*  DI PREFERENZIALE PER LE TARIFFE SCADUTE
033900941104    3C     B             IFGT      1
034000941104     C                   CLEAR                   TPN
034100941104    3C                   ENDIF
034200941104     C*
034300941104     C                   Z-ADD     1             C
034400941104     C*
034500941104    3C     CTN(C)        DOWNE     *BLANKS
034600941104     C     B             ANDLE     150
034700941104     C*
034800000119     C     CTN(C)        LOOKUP    CTV                                    31
034900941104    4C     *IN31         IFEQ      *OFF
035000941104     C                   MOVEL     CTN(C)        CTV(B)
035100941104     C                   MOVEL     TPN(C)        TPV(B)
035200941104     C                   MOVEL     SPN(C)        SPV(B)
035300941104     C                   MOVEL     PGN(C)        PGV(B)
035400990930     C                   MOVEL     DIN(C)        DIV(B)
035500000119     C                   MOVEL     FIN(C)        FIV(B)
035600021115     c                   Movel     mdn(c)        mdv(b)
035700941104     C                   ADD       1             B
035800941104    4C                   ENDIF
035900941104     C*
036000941104     C                   ADD       1             C
036100941104    3C                   END
036200941104    2C                   END
036300021115     C*
036400941107     C                   EXSR      VISTAR
036500941110    2C                   END
036600941104     C*
036700941104     C                   ENDSR
036800941104     C*
036900941104     C* RICERCA E CARICAMENTO A VIDEO TARIFFE ------------------------*
037000941104     C     RICTAR        BEGSR
037100941104     C                   SETOFF                                       30
037200950125     C                   CLEAR                   W001A
037300941104     C*
037400941104     C* SE NON CI SONO TARIFFE IMPOSTO LA TARIFFA 000
037500941104    2C     CTV(1)        IFEQ      *BLANKS
037600941215     C*
037700941215     C     D51CTR        IFEQ      *BLANKS
037800941104     C                   MOVEL     *ZEROS        D51CTR
037900941107     C                   CLEAR                   D51TKS
038000941215     C                   ENDIF
038100941215     C*
038200941104   X2C                   ELSE
038300941107     C*
038400941107     C* CODICE TARIFFA IMPOSTATO: PRENDO IL RELATIVO PROGRESSIVO
038500941107   2AC     D51CTR        IFNE      *BLANKS
038600941107     C                   Z-ADD     1             C
038700941107     C     D51CTR        LOOKUP    CTV(C)                                 30
038800941219     C   30              MOVEL     PGV(C)        D51PRG
038900990930     C   30              MOVEL     DIV(C)        D51DIV
039000991026     C   30D51DIV        IFEQ      '   '
039100011026     C                   MOVEL     'ITL'         D51DIV
039200991026     C                   ENDIF
039300941219     C*
039400941219     C* ERRORE --> TARIFFA NON TROVATA
039500941219    3C     *IN30         IFEQ      *OFF
039600941219     C                   MOVEL     '3'           D51ERR
039700941219     C                   MOVEL     ERR(3)        D51MSG
039800941219     C                   Z-ADD     10            C
039900941219    3C                   ENDIF
040000941219     C*
040100941219   2AC                   ENDIF
040200941107     C**
040300941107   2AC     D51CTR        IFEQ      *BLANKS
040400021115
040500021115      * Carico le sk di work
040600021115     c                   ExSr      CarSk
040700021115
040800941104     C*
040900941104     C* SE IMPOSTATO IL TIPO SERVIZIO --> CERCO QUELLO
041000941104    3C     D51TSP        IFNE      *BLANKS
041100941104     C* CERCO SE C'E' LA PREFERENZIALE ALTRIMENTI LA 1 CHE TROVO
041200941104     C                   Z-ADD     1             C
041300941104     C                   SETON                                        31
041400941104     C*
041500941104    4C     *IN31         DOWEQ     *ON
041600021115     C     D51TSP        LOOKUP    SPw(C)                                 31
041700941104     C* SE TROVATA ED E' PREFERENZIALE ESCO DAL CICLO
041800021115    5C   31TPw(C)        IFEQ      'P'
041900941104     C                   SETON                                        30
042000941104     C                   SETOFF                                       31
042100941104     C                   MOVEL     '1'           W001A             1
042200941104   X5C                   ELSE
042300021115     C* SE TROVATA MA NON PREFERENZIALE ACCENDO COMUNQUE L'INDICATORE
042400941104     C*  DI TROVATO MA FACCIO UN ALTRO GIRO
042500941104     C                   SETON                                        30
042600941104     C                   ADD       1             C
042700941104    5C                   ENDIF
042800941104    4C                   ENDDO
042900941104     C*
043000941104     C* SE HO TROVATO LA TARIFFA MA NON LA PREFERENZIALE, LA RICERCO X
043100941104     C*  PRENDERE LA POSIZIONE
043200941104    4C   30W001A         IFEQ      ' '
043300941104     C                   Z-ADD     1             C
043400021115     C     D51TSP        LOOKUP    SPw(C)                                 31
043500941104    4C                   ENDIF
043600941104     C*
043700941104    3C                   ENDIF
043800941104     C*
043900941104     C* SE NON TROVATA O NON IMPOSTATO IL TIPO SERVIZIO -->
044000941104     C*  CERCO LA PRIMA PREFERENZIALE
044100941104    3C     *IN30         IFEQ      *OFF
044200941104     C                   Z-ADD     1             C
044300021115     C     'P'           LOOKUP    TPw(C)                                 30
044400941104    3C                   ENDIF
044500941104     C*
044600941104     C* SE NON ESISTE LA PREFERENZIALE METTO LA PRIMA VALIDA TROVATA
044700941104     C  N30              Z-ADD     1             C
044800941104     C*
044900021115     C                   MOVEL     CTw(C)        D51CTR
045000021115     C                   MOVEL     PGw(C)        D51PRG
045100021115     C                   MOVEL     DIw(C)        D51DIV
045200991026     C     D51DIV        IFEQ      '   '
045300011026     C                   MOVEL     'ITL'         D51DIV
045400991026     C                   ENDIF
045500941104    2C                   ENDIF
045600021115      * Cerco la posizione nella sk valida
045700021115     c                   Z-Add     1             c
045800021115     c     D51Ctr        Lookup    Ctv(c)                                 30
045900941219   2AC                   ENDIF
046000941107     C*
046100021115     C                   EXSR      VISTAR
046200941104     C*
046300941104     C* RIMPIAZZO CON COD TARIFFA 5 SOLO SE ESISTE
046400021115    1C     CTv(5)        IFNE      *BLANKS
046500941104     C* SE NELLA 1°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
046600941104    2C     C             IFEQ      1
046700021115     C                   MOVEL     CTv(5)        D51CT2
046800021115     C                   MOVEL     SPv(5)        D51ES2
046900021115     C                   MOVE      FIv(5)        D51ES2
047000941107     C                   ELSE
047100941104    2C                   END
047200941104     C* SE NELLA 2°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
047300941104    2C     C             IFEQ      2
047400021115     C                   MOVEL     CTv(5)        D51CT3
047500021115     C                   MOVEL     SPv(5)        D51ES3
047600021115     C                   MOVE      FIv(5)        D51ES3
047700941104    2C                   END
047800941104     C* SE NELLA 3°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
047900941104    2C     C             IFEQ      3
048000021115     C                   MOVEL     CTv(5)        D51CT4
048100021115     C                   MOVEL     SPv(5)        D51ES4
048200021115     C                   MOVE      FIv(5)        D51ES4
048300941104    2C                   END
048400941104     C* SE NELLA 4°POSIZIONE HO MESSO LA PREF. METTO IL 5° CODICE
048500941104    2C     C             IFEQ      4
048600021115     C                   MOVEL     CTv(5)        D51CT5
048700021115     C                   MOVEL     SPv(5)        D51ES5
048800021115     C                   MOVE      FIv(5)        D51ES5
048900941104    2C                   END
049000941104     C*
049100941104    1C                   END
049200941104     C*
049300941104     C* DECODIFICA FISSA TARIFFA
049400941104     C                   Z-ADD     1             C
049500941104     C                   MOVEL     D51CTR        UNOCTR            1
049600941104     C     UNOCTR        LOOKUP    CTA(C)                                 30
049700941104     C   30              MOVEL     DTA(C)        D51DFS
049800941104     C*
049900941104     C                   ENDSR
050000021115
050100021115      *--------------------------------------------------------------*
050200021115      * Carico le sk di work                                         *
050300021115      *--------------------------------------------------------------*
050400021115     c     Carsk         BegSr
050500021115
050600021115     c                   If        D51Fie = 'F'
050700021115     c     'M'           Lookup    mdv                                    35
050800021115     c     'N'           Lookup    mdv                                    36
050900021115     c                   EndIf
051000021115
051100021115     c                   Clear                   d
051200021115
051300021115     c                   Do        150           b
051400021115     c                   If        Ctv(b) = *Blanks
051500021115     c                   Leave
051600021115     c                   EndIf
051700021115
051800021115     c                   If        D51Fie = 'F' and *In35 and *In36
051900021115     c                             and Mdv(b) <> 'N'
052000021115     c                   Iter
052100021115     c                   EndIf
052200021115
052300021115     c                   Add       1             d
052400021115
052500021115     c                   Eval      Ctw(d) = Ctv(b)
052600021115     c                   Eval      Tpw(d) = Tpv(b)
052700021115     c                   Eval      Spw(d) = Spv(b)
052800021115     c                   Eval      Pgw(d) = Pgv(b)
052900021115     c                   Eval      Diw(d) = Div(b)
053000021115     c                   Eval      Fiw(d) = Fiv(b)
053100021115     c                   EndDo
053200021115
053300021115     c                   EndSr
053400941104     C*
053500941104     C* CONTROLLO CODICE TARIFFA ------------------------------------*
053600941104     C     CTRCTR        BEGSR
053700941215     C* SE ESISTE ALMENO UNA TARIFFA PER IL CLIENTE ANCHE SE SCADUTA
053800941215     C*  LO SEGNALO
053900941215     C     CTV(1)        IFNE      *BLANKS
054000941215     C                   MOVEL     'S'           D51TKS
054100941215     C                   ENDIF
054200941215     C*
054300941110     C                   MOVE      D51KSC        WCCM              4 0
054400941104     C* RICERCA
054500941104     C     '?'           SCAN      D51CTR                                 30
054600941104     C*
054700941104    1C     *IN30         IFEQ      *ON
054800941104     C*
054900941104     C* SOLO SE CODIFICATO DELL'AREA POSSO FARE LA RICERCA
055000941104    2C     WCCM          IFEQ      8888
055100941104     C     WCCM          OREQ      9999
055200941104     C                   MOVEL     '1'           D51ERR
055300941104     C                   MOVEL     ERR(1)        D51MSG
055400941104     C                   GOTO      ENDCTR
055500941104    2C                   ENDIF
055600941104     C*
055700941104     C* RICERCA
055800941104     C                   CLEAR                   PARAM4
055900941104     C                   MOVEL     D51KSC        PA4COD
056000941104     C                   Z-ADD     D51KCI        PA4CCC
056100061213     c                   movel     '2'           pa4vpr
056200061220     c                   move      d51dsp        pa4dtr
056300941104     C                   MOVEL     PARAM4        KPJBU
056400941104     C                   CALL      'TNTA36R'
056500941104     C                   PARM                    KPJBA
056600941104     C                   MOVEL     KPJBU         PARAM4
056700941104     C*
056800941104    2C     PA4CTR        IFNE      *BLANKS
056900941104     C* SE TROVATO METTO QUELLO SCELTO
057000941104     C                   MOVE      PA4CTR        D51CTR
057100941104     C* DESCRIZIONE FISSA
057200941104     C                   MOVEL     PA4CTR        UNOCTR
057300941104     C                   Z-ADD     1             C
057400941104     C     UNOCTR        LOOKUP    CTA(C)                                 30
057500941104     C   30              MOVEL     DTA(C)        D51DFS
057600941104     C  N30              MOVEL     *BLANKS       D51DFS
057700941104     C*
057800941104   X2C                   ELSE
057900941104     C                   MOVEL     *ZEROS        D51CTR
058000941104    2C                   END
058100941107     C                   MOVEL     'R'           D51ERR
058200941104     C                   GOTO      ENDCTR
058300941104     C*
058400941104   X1C                   ELSE
058500941104     C*
058600941116     C* CONTROLLO FORMALE
058700941116     C                   EXSR      CTRFOR
058800941116     C*
058900941116     C     D51ERR        IFNE      ' '
059000941104     C                   GOTO      ENDCTR
059100941104    2C                   ENDIF
059200941104     C*
059300941104     C* CONTROLLO ESISTENZA TARIFFA
059400941104    2C     CTV(1)        IFNE      *BLANKS
059500941104     C                   Z-ADD     1             B
059600941104     C     D51CTR        LOOKUP    CTV(B)                                 30
059700941219     C   30              MOVEL     PGV(B)        D51PRG
059800990930     C   30              MOVEL     DIV(B)        D51DIV
059900991026     C   30D51DIV        IFEQ      '   '
060000011026     C                   MOVEL     'ITL'         D51DIV
060100991026     C                   ENDIF
060200941104     C* INESISTENTE: ERRORE
060300941104    3C  N30CTV(1)        IFNE      *BLANKS
060400941104     C                   MOVEL     '3'           D51ERR
060500941104     C                   MOVEL     ERR(3)        D51MSG
060600941104     C                   GOTO      ENDCTR
060700941104    3C                   END
060800941104    2C                   END
060900941104    1C                   END
061000941104     C     ENDCTR        ENDSR
061100941107     C*
061200941107     C* CONTROLLO FORMALE CODICE TARIFFA -----------------------------*
061300941107     C     CTRFOR        BEGSR
061400941107     C*
061500941107     C                   MOVEL     D51CTR        UNOCTR            1
061600941107     C                   Z-ADD     1             C
061700941107     C     UNOCTR        LOOKUP    CTA(C)                                 30
061800941107     C*
061900941107    2C     *IN30         IFEQ      *OFF
062000941118     C     UNOCTR        OREQ      ' '
062100941107     C                   MOVEL     ERR(2)        D51MSG
062200941107     C                   MOVEL     '2'           D51ERR
062300941107    2C                   ENDIF
062400941107     C*
062500941107    1C     D51ERR        IFEQ      *BLANKS
062600941107     C*
062700941107     C                   MOVEL     DTA(C)        D51DFS
062800941107     C* CONTROLLO CHE LA 2 E 3 CIFRA SIANO UN NUERO
062900941107     C                   MOVE      D51CTR        W002A             2
063000941107     C                   MOVEL     W002A         W001A
063100941107     C*
063200941107    2C                   DO        2
063300941107    3C     W001A         IFNE      '0'
063400941107     C     W001A         ANDNE     '1'
063500941107     C     W001A         ANDNE     '2'
063600941107     C     W001A         ANDNE     '3'
063700941107     C     W001A         ANDNE     '4'
063800941107     C     W001A         ANDNE     '5'
063900941107     C     W001A         ANDNE     '6'
064000941107     C     W001A         ANDNE     '7'
064100941107     C     W001A         ANDNE     '8'
064200941107     C     W001A         ANDNE     '9'
064300941107     C                   MOVEL     ERR(4)        D51MSG
064400941107     C                   MOVEL     '4'           D51ERR
064500941107    3C                   ENDIF
064600941107     C*
064700941107     C                   MOVE      W002A         W001A
064800941107    2C                   ENDDO
064900941107    1C                   ENDIF
065000941107     C*
065100941107     C                   ENDSR
065200941104     C*
065300981006     C* SBR INIZIO ---------------------------------------------------*
065400941117     C     *INZSR        BEGSR
065500981006      *
065600981006      * ACCESSO   TABEL
065700981006      *
065800941104     C     KTAB3         KLIST
065900941104     C                   KFLD                    CODUT
066000941104     C                   KFLD                    COD               2
066100981006      *
066200941110     C                   Z-ADD     1             CODUT             1 0
066300981006      ***
066400981006      *  DETERMINO SE SONO SEDE O FILIALE ED APRO IL FILE OPPORTUNO
066500981006      ***
066600981006     C                   CALL      'X§PARUT'
066700981006     C                   PARM                    UT§DSE
066800981006      *
066900981006     C                   MOVEL     REC80         CNCR80
067000981006     C                   OPEN      TNTAM01L
067100981006     C***
067200941104     C* CARICO TABELLA TR : TIPI TARIFFA
067300941104     C***
067400981006     C                   OPEN      TABEL00F
067500981006      *
067600941104     C                   MOVEL     'TR'          COD
067700941104     C                   Z-ADD     1             C
067800941104     C     KTAB3         SETLL     TABEL
067900941104     C     KTAB3         READE     TABEL                                  30
068000941104     C*
068100941104     C     *IN30         DOWEQ     '0'
068200941104     C     TBLFLG        IFEQ      ' '
068300941104     C                   MOVEL     TBLKEY        CTA(C)
068400941104     C                   MOVEL     TBLUNI        DTA(C)
068500941104     C                   ADD       1             C
068600941104     C                   ENDIF
068700941104     C*
068800941104     C     KTAB3         READE     TABEL                                  30
068900941104     C                   ENDDO
069000941104     C*
069100941104     C                   CLOSE     TABEL00F
069200941117     C*
069300941117     C                   CLEAR                   WKSC
069400941117     C                   CLEAR                   WFIE
069500941117     C                   CLEAR                   WTBO
069600941104     C*
069700941104     C                   ENDSR
069800941107     C*
069900941107     C* RIEMPO I CAMPI VIDEO-----------------------------------------*
070000941107     C     VISTAR        BEGSR
070100941107     C     CTV(1)        IFNE      *BLANKS
070200941107     C                   MOVEL     CTV(1)        D51CT2
070300941107     C                   MOVEL     SPV(1)        D51ES2
070400021121     C                   MOVE      Fiv(1)        D51ES2
070500941107     C* SE ESISTE ALMENO UNA TARIFFA PER IL CLIENTE ANCHE SE SCADUTA
070600941107     C*  LO SEGNALO
070700941107     C                   MOVEL     'S'           D51TKS
070800941107     C                   ENDIF
070900941107     C*
071000941107     C     CTV(2)        IFNE      *BLANKS
071100941107     C                   MOVEL     CTV(2)        D51CT3
071200941107     C                   MOVEL     SPV(2)        D51ES3
071300021121     C                   MOVE      Fiv(2)        D51ES3
071400941107     C                   ENDIF
071500941107     C     CTV(3)        IFNE      *BLANKS
071600941107     C                   MOVEL     CTV(3)        D51CT4
071700941107     C                   MOVEL     SPV(3)        D51ES4
071800021121     C                   MOVE      Fiv(3)        D51ES4
071900941107     C                   ENDIF
072000941107     C     CTV(4)        IFNE      *BLANKS
072100941107     C                   MOVEL     CTV(4)        D51CT5
072200941107     C                   MOVEL     SPV(4)        D51ES5
072300021121     C                   MOVE      Fiv(4)        D51ES5
072400941107     C                   ENDIF
072500941107     C                   ENDSR
072600941118**
072700941104Per il codice cliente indicato e' impossibile eseguire la ricerca tariffe
072800941104Tipo Tariffa errato
072900941104Codice tariffa inesistente per il cliente indicato
073000941118Codice tariffa errato
