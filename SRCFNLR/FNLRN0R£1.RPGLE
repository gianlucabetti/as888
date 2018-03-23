000100960118     H DECEDIT('0,') DATEDIT(*DMY.)
000200960118     H* FNLRN0R *----------------------------------------------------*
000300960118     H*         RELEASE 11/94                                        *
000400960118     H*                                                              *
000500960118     H*       - TRASMISSIONE NOTE ALLA FILIALE DI PARTENZA      -    *
000600960118     H*--------------------------------------------------------------*
000700960118     FFNAR402L  UF   E           K DISK
000800960118     F*--------
000900960118     FFNAR400T  O    E             DISK    USROPN
001000960118     F                                     RENAME(FNAR4000:FNAR40T)
001100000410     F*--------
001200000410     FFNAR446T  O    E             DISK    USROPN
001300000410     F                                     RENAME(FNAR4000:FNAR4ST)
001400960118     F*--------
001500960126     FFNLBL01L  IF   E           K DISK
001600960126     F*--------
001700960118     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
001800960118     F                                     USROPN
001900960118     F*--------
002000960118     FTABEL00F  IF   E           K DISK    USROPN
002100960118     D*--------------------------------------------------------------*
002200960118     D*  SCHIERE
002300960118     D*--------------------------------------------------------------*
002400971118     D FIL             S              3  0 DIM(500)                             FILIALE
002500971120     D TFE             S              3  0 DIM(500)                             FILIALE ELABORAT.
002600971120     D TFP             S              3  0 DIM(500)                             TERMINAL PARTENZ.
002700020808     D*AS4             S              3  0 DIM(500)                             ELABORATORE
002800971118     D NRN             S              7  0 DIM(500)                             NUMERO RECORD
002900960118     D* Schiere X QCMDEXEC
003000020808     D CMDC            S             49    DIM(1) CTDATA PERRCD(1)              QCAEXEC CHKOBJ
003100020808     D CMDA            S             49    DIM(1) CTDATA PERRCD(1)              QCAEXEC ADDPFM
003200020808     D CMDO            S             49    DIM(1) CTDATA PERRCD(1)              QCAEXEC OVRDBF
003300960118     D* Schiera tipi record da trasmettere in partenza
003400960118     D C7K             S              1    DIM(100)                             Cod.tp.record
003500960126     D T7K             S              1    DIM(100)                             tp.trasmissio
003600960118     D*--------------------------------------------------------------*
003700960118     D*  DS
003800960118     D*--------------------------------------------------------------*
003900960118      *  Ds x definizione tipi record FNAR4
004000960118     D DS7K          E DS
004100960118      *  Ds x trasmissione dati
004200960118     D DSCMZ         E DS
004300960118      *  Dati utente
004400960118     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
004500960118     D CNCR80        E DS
004600960118     D KPJBA         E DS
004700960118     D  LIBSYS                92    101
004800960118      *
004900960118      *  Ds x scomposozione profilo
005000020506     D*           DS
005100020506     D*                                       1   6 §SEI
005200020506     D*                                       1   3 §PRF
005300960118     D PARAM5          DS
005400960118     D  LEGFIL                 1     10
005500020808     D  MBRFIL                11     17
005600020808     D  BLKFIL                18     23
005700960118      *  Ds x scomposizione data
005800960118     D WLBDA8          DS
005900960118     D  G02DAT                 1      8  0
006000960118     D  G02INV                 9     16  0
006100960118     D  G02ERR                17     17
006200960118     D  G02TGI                18     22  0
006300960118      *  Ds x comporre CMD
006400960118     D                 DS
006500960118     D  NOMFIL                13     20
006600020809     D  CMD                    1     49
006700971120      *  Ds x Reperimento librerie e Sif.da/a cui trasmettere
006800020808     D TIBS55        E DS                  EXTNAME(TIBS55DS)
006900971120      *  Ds x reperimento terminal a cui trasmettere
007000971120     D FNLV55        E DS                  EXTNAME(FNLV55DS)
007100960118     I*--------------------------------------------------------------*
007200960118     I*  Definizione campi Azorg
007300960118     I*--------------------------------------------------------------*
007400960118     IAZORG01L  AA
007500960118     I                             P    4    5 0ORGFIL
007600960118     I                                  7    7  ORGFAG
007700960118     I                             P 4281 4282 0ORGFEL
007800960118     C*-----------------------------------------------------------*
007900960118     C*  Ciclo principale
008000960118     C*-----------------------------------------------------------*
008100960118      *  Prima lettura
008200960118     C                   READ      FNAR402L                               31
008300960126     C* Controllo se devo trasmettere record alla filiale di
008400960126     C* partenza oppure alla lnp. originale
008500960126     C                   EXSR      CTRREC                                       Ctr record ok
008600960129     C*  Eseguo Posizionamento sul file dei legami bolla x controlla
008700960129     C*  se devo trasmettere giacenza alla filiale originale
008800960226     C  N31              EXSR      CHNLBL
008900000411      *
009000000411      * No fine file
009100000411     C     *IN31         IFEQ      '0'
009200000411      * Se esiste almeno un record da trasmettere eseguo apertura
009300000411      * membro sede
009400000411     C                   EXSR      OPNSED                                       Nuova lnp
009500000411      *  Loop
009600960118     C     *IN31         DOWEQ     '0'
009700960118      *  Apertura file se cambia lnp x new record da trasm.
009800960118     C                   EXSR      NEWLNP                                       Nuova lnp
009900960118      *  Loop lettura lnp
010000960118     C     *IN31         DOWEQ     '0'
010100971120     C     WSVLNP        ANDEQ     TFE(X)
010200960118      *  gestisco scrittura se record ok
010300960118     C     WTRAOK        IFEQ      'S'
010400960118     C                   EXSR      GESREC
010500960216     C                   ELSE
010600960216      *  Aggiorno archivio note per non leggere più quelle da trasm.
010700960216     C                   Z-ADD     99999999      AR4DTR
010800960216     C                   EXCEPT    AGGTRA
010900960118     C                   END
011000960118     C                   READ      FNAR402L                               31
011100960126     C* Controllo se devo trasmettere record alla filiale di
011200960126     C* partenza oppure alla lnp. originale
011300960126     C                   EXSR      CTRREC                                       Ctr record ok
011400960129     C*  Eseguo Posizionamento sul file dei legami bolla x controlla
011500960129     C*  se devo trasmettere giacenza alla filiale originale
011600960226     C  N31              EXSR      CHNLBL
011700960118      *
011800960118     C                   END                                                    Lnp = Wsvlnp
011900960118      *  chiudo file
012000960118     C                   EXSR      CLOLNP                                       cambia lnp
012100960118     C                   END
012200000411      *
012300000411      * Eseguo chiusura membro sede
012400000411     C                   EXSR      CLOSED
012500960118      *  Sottometto trasmissioni
012600960118     C                   EXSR      SBMTRA
012700000411     C                   END
012800960118      *
012900960118     C     FINE          TAG
013000971120     C* Imposto i dati in BS50
013100020808     C                   CLEAR                   TIBS55
013200971121     C                   MOVEL     'C'           I50TLA
013300020808     C                   CALL      'TIBS55R'
013400020808     C                   PARM                    TIBS55
013500971121     C* Imposto i dati in LV55
013600971121     C                   CLEAR                   FNLV55
013700971121     C                   MOVEL     'L'           D55TLA
013800971121     C                   CALL      'FNLV55R'
013900971121     C                   PARM                    FNLV55
014000971120     C* SETON LR
014100960118     C                   SETON                                        LR
014200960126     C*-----------------------------------------------------------------*
014300960126     C*  Posizionamento su FNLBL
014400960126     C*-----------------------------------------------------------------*
014500960126     C     CHNLBL        BEGSR
014600960126     C*
014700960126     C* Se devo trasmettere alla lnp orignale la cerco
014800960126     C     T7K(W)        IFEQ      'O'
014900960126     C                   Z-ADD     AR4AAS        KAAS
015000960126     C                   Z-ADD     AR4LNP        KLNP
015100960126     C                   Z-ADD     AR4NRS        KNRS
015200960126     C                   Z-ADD     AR4NSP        KNSP
015300960126     C     KLBL          CHAIN     FNLBL01L                           33
015400960126     C                   END
015500960126     C*
015600960126     C* Trasmissione Lnp originale:
015700960126     C     T7K(W)        IFEQ      'O'
015800960126     C     *IN33         ANDEQ     '0'
015900960126     C     LBLLPO        ANDNE     AR4LNP
016000960126     C* Controllo se la filiale di partenza originale è partita col
016100960126     C* nuovo release
016200960126     C                   Z-ADD     1             X
016300960126     C     LBLLPO        LOOKUP    FIL(X)                                 32
016400960126     C                   MOVE      LBLAAO        WAAS
016500960126     C                   MOVE      LBLLPO        WLNP
016600960126     C                   MOVE      LBLNRO        WNRS
016700960126     C                   MOVE      LBLNSO        WNSP
016800960126     C*
016900960126     C                   ELSE
017000960126     C* Se no bolla legata cerco filiale elaboratore da lnp
017100960126     C                   Z-ADD     1             X
017200960126     C     AR4LNP        LOOKUP    FIL(X)                                 32
017300960126     C                   MOVE      AR4AAS        WAAS
017400960126     C                   MOVE      AR4LNP        WLNP
017500960126     C                   MOVE      AR4NRS        WNRS
017600960126     C                   MOVE      AR4NSP        WNSP
017700960126     C                   END
017800960126     C*
017900960126     C                   ENDSR
018000960118     C*-----------------------------------------------------------*
018100960118     C*  Gestisco cambio linea di partenza
018200960118     C*-----------------------------------------------------------*
018300960118     C     NEWLNP        BEGSR
018400960118      *
018500960118      *  Cerco filiale elaboratore
018600960118     C                   MOVEL     'FNAR400T'    WFILE             9
018700000411     C** Filiale per apertura membro
018800020808     C                   MOVE      ')'           VAR               7
018900020808     C*                  MOVEL     TFE(X)        VAR
019000020808     C                   MOVE      TFE(X)        tfea              3
019100020808     c                   eval      var = simfela + tfea + ')'
019200960118     C                   EXSR      CAEX
019300960118     C                   OPEN      FNAR400T
019400960118      *  trovo indice schiera  AS
019500971120     C                   MOVEL     TFE(X)        WSVLNP            3 0
019600960118     C                   Z-ADD     1             Y                 3 0
019700971120     C     TFE(X)        LOOKUP    TFP(Y)                                 32
019800960118      *
019900960118     C                   ENDSR
020000000411     C*-----------------------------------------------------------*
020100000411     C*  Apro membrp per trasmissione note a sede
020200000411     C*-----------------------------------------------------------*
020300000411     C     OPNSED        BEGSR
020400000411      *
020500000411      *  Cerco filiale elaboratore
020600000411     C                   MOVEL     'FNAR446T'    WFILE             9
020700000411     C** Filiale per apertura membro
020800020808     C*                  MOVE      ')'           VAR               4
020900020808     c                   eval      var = simfela + '046'+ ')'
021000020808     C*                  MOVEL     '046'         VAR
021100000411     C                   EXSR      CAEX
021200000411     C                   OPEN      FNAR446T
021300000411      *
021400000411     C                   ENDSR
021500960118     C*-----------------------------------------------------------*
021600960118     C*  Controllo se tipo record rientra fra quelli da trasmett.
021700960118     C*-----------------------------------------------------------*
021800960118     C     CTRREC        BEGSR
021900960118      *
022000960126     C     AR4TRC        IFNE      WSVTRC
022100960126      *  Loop fino a quando non trovo record da trasmett.
022200960126     C                   MOVEL     'N'           WTRAOK            1
022300960126     C     WTRAOK        DOWEQ     'N'
022400960126     C     *IN31         ANDEQ     '0'
022500960126      *  Verifico se tipo rcd. da trasmettere e tipo trasmiss.
022600960126     C                   Z-ADD     1             W                 3 0
022700960126     C                   MOVEL     AR4TRC        WSVTRC            1
022800960126     C     AR4TRC        LOOKUP    C7K(W)                                 32
022900960126     C     *IN32         IFEQ      '1'
023000960126     C                   MOVEL     'S'           WTRAOK
023100960126     C                   ELSE
023200960216      *  Aggiorno archivio note per non leggere più quelle da trasm.
023300960216     C                   Z-ADD     99999999      AR4DTR
023400960216     C                   EXCEPT    AGGTRA
023500960216     C                   READ      FNAR402L                               31
023600960126     C                   END
023700960118      *
023800960126     C                   END
023900960126     C                   END
024000960126      *
024100960118     C                   ENDSR
024200960118     C*-----------------------------------------------------------*
024300960118     C*  Scrittura record x trasmissione
024400960118     C*-----------------------------------------------------------*
024500960118     C     GESREC        BEGSR
024600960118      *
024700960126     C                   Z-ADD     WAAS          AR4AAS
024800960126     C                   Z-ADD     WLNP          AR4LNP
024900960126     C                   Z-ADD     WNRS          AR4NRS
025000960126     C                   Z-ADD     WNSP          AR4NSP
025100040409      *
025200040409      * imposta la data di trasmissione da comparare in arrivo
025300040409     C                   Z-ADD     WOGGI         AR4DTR
025400040409      *
025500000411      * Scrivo record x trasmissione in filiale
025600960118     C                   WRITE     FNAR40T
025700000411     C* Scrivo record x trasmissione in sede
025800000411     C                   WRITE     FNAR4ST
025900960118      *  Aggiorno archivio note da trasmettere
026000960118     C                   MOVEL     'T'           AR4FTR
026100960118     C                   Z-ADD     WOGGI         AR4DTR
026200960118     C                   EXCEPT    AGGTRA
026300960118     C                   ADD       1             NRN(Y)
026400960118      *
026500960118     C                   ENDSR
026600960118     C*-----------------------------------------------------------*
026700000411     C*  Chiudo file trasmissione in filiale
026800960118     C*-----------------------------------------------------------*
026900960118     C     CLOLNP        BEGSR
027000960118      *
027100960118     C                   CLOSE     FNAR400T
027200960118      *
027300960118     C                   ENDSR
027400000411     C*-----------------------------------------------------------*
027500000411     C*  Chiudo file tramissione in sede
027600000411     C*-----------------------------------------------------------*
027700000411     C     CLOSED        BEGSR
027800000411      *
027900000411     C                   CLOSE     FNAR446T
028000000411      *
028100000411     C                   ENDSR
028200960118     C*-------------------------------------------------------------**
028300960118     C* Sottometto trasmissione note
028400960118     C*-------------------------------------------------------------**
028500960118     C     SBMTRA        BEGSR
028600960118      *
028700971120      * Imposto dati per trasmissione locali
028800020808     C*                  MOVEL     'M'           MBRFIL
028900020808     C*                  MOVE      SIMFEL        MBRFIL
029000020808     c                   eval      mbrfil = 'M' +simfela+simfela
029100971120     C                   MOVEL     MBRFIL        LEGFIL
029200971120     C                   MOVEL     'F'           LEGFIL
029300971120     C                   MOVE      '999999'      LEGFIL
029400971120     C* Loop trasmissioni
029500020808     C                   MOVEL     'M'           VAR2              7
029600971118     C                   DO        500           Y
029700960118     C                   MOVE      *BLANKS       CMZAZN                          AZIONE
029800960118     C*
029900960118     C     NRN(Y)        IFNE      0
030000971120     C* Richiamo pgm x reperimento librerie e S.Informativi da/a
030100020808     C                   CLEAR                   TIBS55
030200971120     C                   MOVEL     *BLANKS       I50TLA
030300971120     C                   MOVEL     KNSIF         I50PSI
030400971120     C                   MOVEL     SIMFEL        I50PPO
030500971120     C                   MOVEL     TFP(Y)        I50APO
030600020808     C                   CALL      'TIBS55R'
030700020808     C                   PARM                    TIBS55
030800971120     C* Imposto dati trasmissione
030900020809     C*                  MOVE      TFP(Y)        VAR2
031000020808     C                   MOVE      TFP(Y)        tfpa              3
031100020809     c                   eval      var2 = 'M' + simfela + tfpa
031200020808     c                   eval      mbrfil = 'M' + simfela + tfpa
031300960118     C                   MOVE      'LRN1'        CMZAZN                          AZIONE
031400960118     C                   MOVE      NRN(Y)        CMZNRR                          N.REC.
031500020808     C*                  MOVE      AS4(Y)        CMZDST                          DESTINAZIONE
031600020808     C                   MOVE      TFPa          cmzdst
031700960118     C                   MOVE      VAR2          VAR                             MEMBRO DESTINAZIONE
031800960118     C                   EXSR      X25
031900960118      *
032000960118     C                   END
032100960118     C                   END
032200000411     C*
032300000411     C* Se ho scritto almeno un record gesstisco l'invio dei
032400000411     C* dati in sede
032500000411     C                   XFOOT     NRN           TOTREC            6 0
032600000411     C     TOTREC        IFGT      0
032700000411     C* Richiamo pgm x reperimento librerie e S.Informativi da/a
032800020808     C                   CLEAR                   TIBS55
032900000411     C                   MOVEL     *BLANKS       I50TLA
033000000411     C                   MOVEL     KNSIF         I50PSI
033100000411     C                   MOVEL     SIMFEL        I50PPO
033200000411     C                   MOVEL     '046'         I50APO
033300020808     C                   CALL      'TIBS55R'
033400020808     C                   PARM                    TIBS55
033500000411     C                   EXSR      X25S
033600000411     C                   END
033700960118     C*
033800960118     C                   ENDSR
033900960118     C**----------------------------------
034000960118     C** SOTTOMETTE TRASMISSIONE
034100960118     C**----------------------------------
034200960118     C     X25           BEGSR
034300960118     C                   MOVEL     'FNAR400T'    CMZFLD                          DA: >>>
034400960118     C                   MOVEL     *BLANK        CMZMBD
034500960118     C                   MOVEL     VAR           CMZMBD
034600971120     C                   MOVEL     O50PLA        CMZLBD
034700960118     C                   MOVEL     'FNAR400R'    CMZFLA                          A: >>>
034800020808     C                   MOVEL     *BLANK        CMZMBA
034900020808     C*                  MOVEL     'M   '        VAR
035000020808     C*                  MOVE      SIMFEL        VAR
035100020808     C                   MOVEL     VAR           CMZMBA
035200971120     C                   MOVEL     O50ALA        CMZLBA
035300960118     C                   MOVE      '0000'        CMZORI                          ORA
035400960118     C                   MOVE      '0000000'     CMZDTI                          DATA
035500960118     C* TRASMETTO IN NORMALE SE ORA ATTUALE <= ORA TABELLA
035600960118     C                   MOVE      'N'           CMZURG                          NORMALE
035700960118     C* CON ORARIO ARROTONDATO ALL'UNITA' SUPERIORE
035800960118     C                   TIME                    ORAR              6 0
035900960118     C                   MOVEL     ORAR          AORA              2 0
036000960118     C                   ADD       1             AORA
036100960118     C                   MOVEL     AORA          CMZORI
036200960118     C                   Z-ADD     WOGGI         DATA6             6 0
036300960118     C                   MOVE      DATA6         CMZDTI
036400960118     C*
036500960118     C                   MOVE      'A'           CMZADD                          IN ADD
036600960118     C                   MOVE      'S'           CMZCLR                          CLEAR
036700960118     C                   MOVE      'S'           CMZFTR                          DA TRANS.
036800960118     C                   MOVE      *BLANK        CMZERR                          1= ERR.
036900971120     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
037000971120     C* Imposto passaggio parametri Kpjba x locali
037100971120     C                   MOVEL     PARAM5        W236            236
037200000207     C                   MOVEL     *BLANKS       CMZPRR
037300971120     C* Trasmetto
037400020808     C                   CALL      'TIBS53C'
037500971120     C                   PARM                    O50LCL
037600960118     C                   PARM                    CMZDST
037700960118     C                   PARM                    CMZFLD
037800960118     C                   PARM                    CMZMBD
037900960118     C                   PARM                    CMZLBD
038000960118     C                   PARM                    CMZFLA
038100960118     C                   PARM                    CMZMBA
038200960118     C                   PARM                    CMZLBA
038300960118     C                   PARM                    CMZORI
038400960118     C                   PARM                    CMZDTI
038500960118     C                   PARM                    CMZURG
038600960118     C                   PARM                    CMZADD
038700960118     C                   PARM                    CMZCLR
038800960118     C                   PARM                    CMZFTR
038900960118     C                   PARM                    CMZERR
039000960118     C                   PARM                    CMZAZN
039100960118     C                   PARM                    CMZSYF
039200960118     C                   PARM                    CMZNRR
039300960118     C                   PARM                    CMZPRR
039400971120     C                   PARM                    W236
039500971120     C*
039600960118     C                   ENDSR
039700000411     C**----------------------------------
039800000411     C** SOTTOMETTE TRASMISSIONE
039900000411     C**----------------------------------
040000000411     C     X25S          BEGSR
040100000411     C*
040200000411     C                   MOVE      'TRN1'        CMZAZN                          AZIONE
040300000411     C                   MOVE      TOTREC        CMZNRR                          N.REC.
040400000411     C                   MOVE      '046'         CMZDST                          DESTINAZIONE
040500000411     C                   MOVEL     'FNAR446T'    CMZFLD                          DA: >>>
040600020808     C*                  MOVEL     'M046'        CMZMBD
040700020808     c                   eval      cmzmbd = 'M' + simfela + '046'
040800000411     C                   MOVEL     O50PLA        CMZLBD
040900000411     C                   MOVEL     'FNAR400R'    CMZFLA                          A: >>>
041000020808     C*                  MOVEL     *BLANK        CMZMBA
041100020808     C*                  MOVEL     'M   '        VAR
041200020808     C*                  MOVE      SIMFEL        VAR
041300020808     C*                  MOVEL     VAR           CMZMBA
041400020808     c                   eval      cmzmba = 'M' + simfela + '046'
041500000411     C                   MOVEL     O50ALG        CMZLBA
041600000411     C                   MOVE      '0000'        CMZORI                          ORA
041700000411     C                   MOVE      '0000000'     CMZDTI                          DATA
041800000411     C* TRASMETTO IN NORMALE SE ORA ATTUALE <= ORA TABELLA
041900000411     C                   MOVE      'N'           CMZURG                          NORMALE
042000000411     C* CON ORARIO ARROTONDATO ALL'UNITA' SUPERIORE
042100000411     C                   TIME                    ORAR              6 0
042200000411     C                   MOVEL     ORAR          AORA              2 0
042300000411     C                   ADD       1             AORA
042400000411     C                   MOVEL     AORA          CMZORI
042500000411     C                   Z-ADD     WOGGI         DATA6             6 0
042600000411     C                   MOVE      DATA6         CMZDTI
042700000411     C*
042800000411     C                   MOVE      'A'           CMZADD                          IN ADD
042900000411     C                   MOVE      'S'           CMZCLR                          CLEAR
043000000411     C                   MOVE      'S'           CMZFTR                          DA TRANS.
043100000411     C                   MOVE      *BLANK        CMZERR                          1= ERR.
043200000411     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
043300000411     C* Imposto passaggio parametri Kpjba x locali
043400000411     C                   MOVEL     PARAM5        W236            236
043500000411     C                   MOVEL     *BLANKS       CMZPRR
043600000411     C* Trasmetto
043700020808     C                   CALL      'TIBS53C'
043800000411     C                   PARM                    O50LCL
043900000411     C                   PARM                    CMZDST
044000000411     C                   PARM                    CMZFLD
044100000411     C                   PARM                    CMZMBD
044200000411     C                   PARM                    CMZLBD
044300000411     C                   PARM                    CMZFLA
044400000411     C                   PARM                    CMZMBA
044500000411     C                   PARM                    CMZLBA
044600000411     C                   PARM                    CMZORI
044700000411     C                   PARM                    CMZDTI
044800000411     C                   PARM                    CMZURG
044900000411     C                   PARM                    CMZADD
045000000411     C                   PARM                    CMZCLR
045100000411     C                   PARM                    CMZFTR
045200000411     C                   PARM                    CMZERR
045300000411     C                   PARM                    CMZAZN
045400000411     C                   PARM                    CMZSYF
045500000411     C                   PARM                    CMZNRR
045600000411     C                   PARM                    CMZPRR
045700000411     C                   PARM                    W236
045800000411     C*
045900000411     C                   ENDSR
046000960118     C*-------------------------------------------------------------**
046100960118     C* QCMDXEC membro di FNAR400T oppure su membro FNAR400R
046200960118     C*-------------------------------------------------------------**
046300960118     C     CAEX          BEGSR
046400000411     C*
046500020808     C                   Z-ADD     49            LUNG             15 5
046600960118     C** Controllo se esiste membro
046700960118     C                   SETOFF                                       34
046800960118     C                   MOVE      VAR           CMDC
046900960118     C                   MOVEA     CMDC          CMD
047000960118     C                   MOVEL     WFILE         NOMFIL
047100960118     C                   MOVEL     *BLANKS       COMMAN
047200960118     C                   MOVEL     CMD           COMMAN           80
047300960118     C                   CALL      'QCMDEXC'                            34
047400960118     C                   PARM                    COMMAN
047500960118     C                   PARM                    LUNG
047600960118     C** Se non esiste eseguo addpfm x membro nuovo
047700960118     C     *IN34         IFEQ      '1'
047800960118     C                   MOVE      VAR           CMDA
047900960118     C                   MOVEA     CMDA          CMD
048000960118     C                   MOVEL     WFILE         NOMFIL
048100960118     C                   MOVEL     *BLANKS       COMMAN
048200960118     C                   MOVEL     CMD           COMMAN           80
048300960118     C                   CALL      'QCMDEXC'
048400960118     C                   PARM                    COMMAN
048500960118     C                   PARM                    LUNG
048600960118     C                   END
048700960118     C** Esegue in ogni caso OVRDBF del membro desiderato
048800960118     C                   MOVE      VAR           CMDO
048900960118     C                   MOVEA     CMDO          CMD
049000960118     C                   MOVEL     WFILE         NOMFIL
049100960118     C                   MOVEL     *BLANKS       COMMAN
049200960118     C                   MOVEL     CMD           COMMAN           80
049300960118     C                   CALL      'QCMDEXC'
049400960118     C                   PARM                    COMMAN
049500960118     C                   PARM                    LUNG
049600960118     C*
049700960118     C                   ENDSR
049800960118     C*-----------------------------------------------------------*
049900960118     C*  Operazioni iniziali:
050000960118     C*-----------------------------------------------------------*
050100960118     C     *INZSR        BEGSR
050200960118      *
050300960118     C     *ENTRY        PLIST
050400960118     C                   PARM                    KPJBA
050500960118      *  Richiamo XParut
050600960118     C                   Z-ADD     1             CODUT
050700960118     C                   CALL      'X§PARUT'
050800960118     C                   PARM                    UTEDSE
050900960118     C                   MOVEL     REC80         CNCR80
051000020808     c                   move      simfel        simfela           3
051100960118      *  Definizione chiavi
051200960126     C     KLBL          KLIST
051300960126     C                   KFLD                    KAAS
051400960126     C                   KFLD                    KLNP
051500960126     C                   KFLD                    KNRS
051600960126     C                   KFLD                    KNSP
051700960118     C     KTAB1         KLIST
051800960118     C                   KFLD                    KKUT
051900960118     C                   KFLD                    KCOD
052000960118     C     KTAB2         KLIST
052100960118     C                   KFLD                    KKUT
052200960118     C                   KFLD                    KCOD
052300960118     C                   KFLD                    KKEY
052400960118      *  Definizione variabili
052500960126     C     *LIKE         DEFINE    AR4AAS        WAAS
052600960126     C     *LIKE         DEFINE    AR4LNP        WLNP
052700960126     C     *LIKE         DEFINE    AR4NRS        WNRS
052800960126     C     *LIKE         DEFINE    AR4NSP        WNSP
052900960126     C     *LIKE         DEFINE    AR4AAS        KAAS
053000960126     C     *LIKE         DEFINE    AR4LNP        KLNP
053100960126     C     *LIKE         DEFINE    AR4NRS        KNRS
053200960126     C     *LIKE         DEFINE    AR4NSP        KNSP
053300960118     C     *LIKE         DEFINE    TBLKUT        KKUT
053400960118     C     *LIKE         DEFINE    TBLCOD        KCOD
053500960118     C     *LIKE         DEFINE    TBLKEY        KKEY
053600960126     C     *LIKE         DEFINE    AR4LNP        WEXLNP
053700960118      *
053800960118      *  Reperisco filiale
053900020506     C*                    MOVELKNMUS     §SEI
054000020506     C*          §PRF      IFEQ 'REM'
054100020506     C     SIMTPP        IFEQ      '2'
054200020506     C     SIMTPP        OREQ      *BLANKS
054300020506     C*                    MOVE §SEI      WFLE    30
054400020506     C                   MOVE      SIMPOU        WFLE              3 0
054500960118     C                   ELSE
054600960118     C                   MOVE      SIMFEL        WFLE
054700960118     C                   END
054800960118      *  Data del giorno
054900960118     C                   TIME                    WAMGDT           14 0
055000960118     C                   MOVE      WAMGDT        G02DAT
055100960118     C                   MOVE      *ZEROS        G02INV
055200960118     C                   MOVEL     *BLANK        G02ERR
055300960118     C                   CALL      'XSRDA8'
055400960118     C                   PARM                    WLBDA8
055500960118     C                   Z-ADD     G02INV        WOGGI             8 0
055600960118     C                   TIME                    TIMES             6 0
055700960118      *
055800960118      *  Effettuo caricamento dei tipi record da trasmettere
055900960118     C                   OPEN      TABEL00F
056000960118     C                   Z-ADD     0             X
056100960118     C                   Z-ADD     1             KKUT
056200960118     C                   MOVEL     '7K'          KCOD
056300960118     C     KTAB1         CHAIN     TABEL                              30
056400960118     C     *IN30         DOWEQ     '0'
056500960118     C     X             ANDLT     100
056600960118     C     TBLFLG        IFEQ      *BLANKS
056700960118     C                   MOVEL     TBLUNI        DS7K
056800960126     C     §7KTRA        IFNE      *BLANKS
056900960118     C                   ADD       1             X
057000960118     C                   MOVEL     TBLKEY        C7K(X)
057100960126     C                   MOVEL     §7KTRA        T7K(X)
057200960118     C                   END
057300960118     C                   END
057400960118     C     KTAB1         READE     TABEL                                  30
057500960118     C                   END
057600960118      *
057700960118      * memorizzo per ogni filiale il codice elaboratore
057800960118     C                   Z-ADD     0             X
057900960118     C                   Z-ADD     0             Y
058000960118     C                   Z-ADD     0             NRN
058100960118     C                   OPEN      AZORG01L
058200960118     C                   READ      AZORG01L                               30
058300960118     C     *IN30         DOWEQ     '0'
058400960118     C     ORGFAG        IFEQ      'F'
058500960118     C     ORGFAG        OREQ      'A'
058600960118     C                   ADD       1             X                 3 0
058700960118     C                   MOVE      ORGFIL        FIL(X)
058800971120      *  Reperisco terminal di partenza collegato
058900971121     C                   CLEAR                   FNLV55
059000971121     C* Imposto i dati in LV55
059100971121     C                   MOVEL     'P'           D55TPT
059200971121     C                   MOVEL     WOGGI         D55DRF
059300971121     C                   MOVEL     ORGFIL        D55LNP
059400971121     C                   MOVEL     ORGFIL        D55LIN
059500971121     C                   CALL      'FNLV55R'
059600971121     C                   PARM                    FNLV55
059700971121     C                   MOVE      D55TFP        TFE(X)
059800960118      *  Se primo livello carico schiera AS4 nr.record
059900971124     C     D55TFP        IFEQ      ORGFIL
060000960118     C                   ADD       1             Y                 3 0
060100020808     C*                  MOVE      ORGFEL        AS4(Y)
060200971121     C                   MOVE      D55TFP        TFP(Y)
060300960118     C                   END
060400960118     C                   END
060500960118     C                   READ      AZORG01L                               30
060600960118     C                   END
060700960118     C                   CLOSE     AZORG01L
060800960118     C*
060900960118     C                   ENDSR
061000960118     O*-----------------------------------------------------*
061100960118     O* Excpt x aggiornamento dati da trasmettere
061200960118     O*-----------------------------------------------------*
061300960118     OFNAR4000  E            AGGTRA
061400960118     O                       AR4FTR
061500960118     O                       AR4DTR
061600960118**         CMDC
061700020808CHKOBJ  OBJ(        ) OBJTYPE(*FILE) MBR(M000000)
061800960118**         CMDA
061900020808ADDPFM FILE(        )                MBR(M000000)
062000960118**         CMDO
062100020808OVRDBF FILE(        )                MBR(M000000)
