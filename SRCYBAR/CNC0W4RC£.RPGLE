000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200900606?     *--------------------------------------------------------------*
000300941017      * CNC0V4RC                                                     *
000400941017      *        PREPARO C/ASSEGNI INCASSATI X FIL.PARTENZA            *
000500911030      *        CHIUSURA PER OPERATORE                                *
000600900606?     *--------------------------------------------------------------*
000700940224      * 01 -ON--> PRIMO LIVELLO/OFF --> SECONDO LIVELLO              *
000800941018      * 20 -ON--> SCRIVO FNCAF00R / OFF SCRIVO FNCAF00T              *
000900941018      * 21 -ON--> HO APERTO FNCAF00R / OFF HO APERTO FNCAF00T        *
001000940224      * 91 -CALL A QCMDEXEC                                          *
001100940224?     *--------------------------------------------------------------*
001200941017     FFNCAS00T  IP   E           K DISK
001300941017     FAZORG01L  IF   E           K DISK
001400941017     FFNCAF00T  O    E             DISK    USROPN
001500941017     F                                     RENAME(FNCAF000:FNCAF0T)
001600971121?     *--------------------------------------------------------------*
001700971118     D FIL             S              3  0 DIM(500)                             FILIALE
001900971121     D TFE             S              3  0 DIM(500)                             TERMINAL D
002100971121     D TFP             S              3  0 DIM(500)                             TERMINAL D
002200020920     D CMDC            S             48    DIM(2) CTDATA PERRCD(1)              QCAEXEC CH
002300020920     D CMDA            S             48    DIM(2) CTDATA PERRCD(1)              QCAEXEC AD
002400020920     D CMDO            S             48    DIM(2) CTDATA PERRCD(1)              QCAEXEC OV
002500020920     D CMDD            S             48    DIM(2) CTDATA PERRCD(1)              QCAEXEC OV
002600971121?     *--------------------------------------------------------------*
002700941018     D DSCMZ         E DS
002800910708     D CNCR80        E DS
002900900514     D KPJBA         E DS
003000900514     D  LIBSYS                92    100
003100900514     D  MSGERR               453    502
003200911030      *
003300930510     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003400941018     D PARAM           DS
003500941018     D  LEGFIL                 1     10
003600020925     D  MBRFIL                11     20
003800020920     D TIBS50        E DS                  EXTNAME(TIBS55DS)
003900971121     D FNLV55        E DS                  EXTNAME(FNLV55DS)
004000971121     D WLBDA8          DS
004100971121     D  G08DAT                 1      8  0
004200971121     D  G08INV                 9     16  0
004300971121     D  G08ERR                17     17
004400971121     D  G08TGI                18     22  0
004401020920     d   §membro       ds
004402020920     d  §emme                  1      1    inz('M')
004403020925     d  xfle                   2      4s 0
004404020920     d  §tfe                   5      7s 0
004405020920     d  §mbr                   2      7
004500971121?     *--------------------------------------------------------------*
004600941017      * --  ROTTURA LNP
004700941017     C     CASLNP        IFNE      SAVLNP
004800941017     C                   Z-ADD     1             X
004900941017     C     CASLNP        LOOKUP    FIL(X)                                 19
005000971121      * - controllo se cambia anche il terminal di partenza
005100971121     C   19              DO
005200971121     C     TFE(X)        IFNE      SAVTFE
005300971121     C* se cambia il terminal di partenza la prima volta non
005400971121     C* devo chiudere nessun file
005500991122     C   50              DO
005600991122     C                   MOVEL     *BLANKS       COMMAN
005700991122     C                   MOVEA     CMDD(1)       COMMAN
005800991122     C                   CALL      'QCMDEXC'
005900991122     C                   PARM                    COMMAN
006000991122     C                   PARM                    LUNG
006100991122     C                   CLOSE     FNCAF00T
006200991122     C                   END
006300971121     C  N50              SETON                                        50
006400971121      * controllo se ho già memorizzato il terminal di partenza
006500971125     C                   Z-ADD     1             XX                3 0
006600971125     C     TFE(X)        LOOKUP    TFP(XX)                                19
006700971121     C     *IN19         IFEQ      '0'
006800941017     C                   ADD       1             T                 3 0
007000971121     C                   MOVE      TFE(X)        TFP(T)
007100971121     C                   END                                                    carica tfp
007200941017     C                   EXSR      CAEXC
007300971125     C* salvo nuovo tfe
007400971125     C                   Z-ADD     TFE(X)        SAVTFE
007500971121     C                   END                                                    tfe<>sav
007600971125     C*
007700971121     C                   ENDDO                                                  trv filia
007800941017      *
007900971121     C                   Z-ADD     CASLNP        SAVLNP
008000941017     C                   ENDIF
008100941017      * -- FINE ROTTURA
008200941019     C                   MOVE      CASLNP        CAFLNP
008300941019     C                   MOVE      CASAAS        CAFAAS
008400941019     C                   MOVE      CASNRS        CAFNRS
008500941019     C                   MOVE      CASNSP        CAFNSP
008600941019     C                   MOVE      CASCAS        CAFCAS
008700941019     C                   MOVE      CASVCA        CAFVCA
008800941019     C                   MOVE      CASTPA        CAFTPA
008900941019     C                   MOVE      CASTPI        CAFTPI
009000941019     C                   MOVE      CASDDC        CAFDMI
009100971121     C                   WRITE     FNCAF0T                                      DA TRASM.
009200941017     C*
009300991122     CLR 50              DO
009400991122     CLR                 MOVEL     *BLANKS       COMMAN
009500991122     CLR                 MOVEA     CMDD(1)       COMMAN
009600991122     CLR                 CALL      'QCMDEXC'
009700991122     CLR                 PARM                    COMMAN
009800991122     CLR                 PARM                    LUNG
009900991122     CLR                 CLOSE     FNCAF00T
010000991122     CLR                 END
010100941017     CLR                 EXSR      TRASMI
010200941018      *
010300941017     C**-------------------------------------------------------------**
010400941017     C** QCAEXEC SUL MBR DI FNCAF00T/FNCAF00R
010500941017     C**-------------------------------------------------------------**
010600941017     C     CAEXC         BEGSR
010700941017     C** FILIALE PER APERTURA MEMBRO
010900020920     C                   MOVEL     TFE(X)        §tfe
011100941017      *
011200971121     C** CONTROLLO SE ESISTE IL MEMBRO X FNCAF00T
011300941017     C                   SETOFF                                       60
011600020920     C                   MOVEl(p)  CMDC(1)       COMMAN           80
011601020920     c                   eval      %subst(comman:42:6) = §mbr
011700941017     C                   CALL      'QCMDEXC'                            60
011800941017     C                   PARM                    COMMAN
011900941017     C                   PARM                    LUNG
012000941017     C** ESEGUE ADDPFM DEL MEMBRO NUOVO
012100941017     C   60              DO
012400020920     C                   MOVEl(p)  CMDA(1)       COMMAN
012401020920     c                   eval      %subst(comman:42:6) = §mbr
012500941017     C                   CALL      'QCMDEXC'
012600941017     C                   PARM                    COMMAN
012700941017     C                   PARM                    LUNG
012800941017     C                   END
012900941017     C*+ ESEGUE IN OGNI CASO OVRDBF DEL MEMBRO DESIDERATO
013200020920     C                   MOVEl(P)  CMDO(1)       COMMAN
013201020920     c                   eval      %subst(comman:42:6) = §mbr
013300941017     C                   CALL      'QCMDEXC'
013400941017     C                   PARM                    COMMAN
013500941017     C                   PARM                    LUNG
013600941017      *
013700971121     C                   OPEN      FNCAF00T
013800941017     C                   SETOFF                                       21
013900941017     C                   ENDSR
014000941017     C**---------------------------------------
014100941018     C** TRASMETTE X LR ALLE FILIALI PARTENZA
014200941017     C**---------------------------------------
014300941017     C     TRASMI        BEGSR
014400971121     C*
015100971121     C* trasmetti
015200971121     C                   DO        T             TT                3 0
015201020924     C                   MOVE      TFP(TT)       §tfe
015202020924     C* Imposto parametri per trasmissione locali
015203020924     C                   MOVEL(p)  §membro       MBRFIL
015204020924     C                   MOVEL     MBRFIL        LEGFIL
015205020924     C                   MOVEL     'F'           LEGFIL
015206020924     C                   MOVE      '999'         LEGFIL
015207020924     c*
015500941018     C                   EXSR      TRUL51
015600941018     C                   MOVEL     'FNCAF00T'    FILE
015701020920     C                   MOVEL(P)  §membro       MBR
015800941018     C                   CALL      'TRUL51C'
015900941018     C                   PARM                    LIBR
016000941018     C                   PARM                    FILE
016100941018     C                   PARM                    MBR
016200941018     C                   PARM                    NRR
016300941018     C     NRR           IFGT      0
016400941018     C     NRR           ANDNE     *ALL'9'
016500020920     C* Richiamo TIBS55R per conoscere librerie/S.Informativi
016600971121     C* da/a cui spedire
016700971121     C                   CLEAR                   TIBS50
016800971121     C                   MOVEL     *BLANKS       I50TLA
016900971121     C                   MOVEL     KNSIF         I50PSI
017000020925     C                   MOVEL     SIMFEL        I50PPO
017100971125     C                   MOVEL     TFP(TT)       I50APO
017200020920     C                   CALL      'TIBS55R'
017300971121     C                   PARM                    TIBS50
017400971121     C* eseguo trasmissione
017500941017     C                   EXSR      X25
017600941018     C                   END
017700971125     C*
017800941017     C                   ENDDO
017900971121     C*
018000971121     C* call finale per chiusura con LR
018100971121     C* Imposto i dati in BS50
018200971121     C                   CLEAR                   TIBS50
018300971121     C                   MOVEL     'C'           I50TLA
018400020920     C                   CALL      'TIBS55R'
018500971121     C                   PARM                    TIBS50
018600971121     C* Imposto i dati in LV55
018700971121     C                   CLEAR                   FNLV55
018800971121     C                   MOVEL     'L'           D55TLA
018900971121     C                   CALL      'FNLV55R'
019000971121     C                   PARM                    FNLV55
019100941017     C                   ENDSR
019200941018      *----------------------------------------------------------------
019300941018      * PULIZIA PARAMETRI X TRUL51C
019400941018      *----------------------------------------------------------------
019500941018     C     TRUL51        BEGSR
019600971121     C                   MOVEL     '*LIBL'       LIBR             10
019700941018     C                   MOVEL     *BLANK        FILE             10
019800941018     C                   MOVEL     *BLANK        MBR              10
019900941018     C                   Z-ADD     0             NRR              10 0
020000941018     C                   ENDSR
020100941017     C**---------------------------------------
020200941017     C** SOTTOMETTE TRASMISSIONE
020300941017     C**---------------------------------------
020400941017     C     X25           BEGSR
020500021007     C                   MOVE      tfp(TT)       CMZDST                          DEST.:FIL
020600941017     C                   MOVEL     'FNCAF00T'    CMZFLD                          DA: >>>
020800020920     C                   MOVEL(p)  §membro       CMZMBD
020900971121     C                   MOVEL     O50PLA        CMZLBD
021000941017     C                   MOVEL     'FNCAF00R'    CMZFLA                          A: >>>
021400020920     C                   MOVEL(p)  §membro       CMZMBA
021500971121     C                   MOVEL     O50ALA        CMZLBA
021600941018     C                   MOVE      'LR89'        CMZAZN                          AZIONE
021700971121     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
021800941017     C                   MOVE      NRR           CMZNRR                          N.REC.
021900941017     C                   MOVE      *BLANKS       CMZPRR                          FILE PRE-
022000941017     C                   EXSR      XCMZ
022100941017     C                   ENDSR
022200971121     C**---------------------------------------
022300971121     C** richiamo pgm trasmissione
022400971121     C**---------------------------------------
022500941017     C     XCMZ          BEGSR
022600971121     C*
022700991202      * TEST SE DEVO TRASMETTERE IN IMMED O IN NOTTE
022800991202     C                   TIME                    TIME              6 0
022900991202     C                   MOVEL     TIME          HM                4 0
023000991202     C                   MOVEL     HM            H                 2 0
023100991202     C                   ADD       1             H
023200991202     C                   MOVE      'N'           CMZURG                          NORMALE
023300991202     C                   MOVEL     H             CMZORI
023400991202     C                   Z-ADD     AMGUDT        DATA6             6 0           DATA
023500991202     C                   MOVE      DATA6         CMZDTI                          DATA
023600971121     C                   MOVEL     PARAM         W236            236
023700941017     C                   MOVE      'A'           CMZADD                          IN ADD
023800941017     C                   MOVE      'S'           CMZCLR                          CLEAR
023900941017     C                   MOVE      'S'           CMZFTR                          DA TRANS.
024000941017     C                   MOVE      *BLANK        CMZERR                          1= ERR.
024100021007     C                   CALL      'TIBS53C'
024200971121     C                   PARM                    O50LCL
024300971121     C                   PARM                    CMZDST
024400941017     C                   PARM                    CMZFLD
024500941017     C                   PARM                    CMZMBD
024600941017     C                   PARM                    CMZLBD
024700941017     C                   PARM                    CMZFLA
024800941017     C                   PARM                    CMZMBA
024900941017     C                   PARM                    CMZLBA
025000941017     C                   PARM                    CMZORI
025100941017     C                   PARM                    CMZDTI
025200941017     C                   PARM                    CMZURG
025300941017     C                   PARM                    CMZADD
025400941017     C                   PARM                    CMZCLR
025500941017     C                   PARM                    CMZFTR
025600941017     C                   PARM                    CMZERR
025700941018     C                   PARM                    CMZAZN
025800941017     C                   PARM                    CMZSYF
025900941017     C                   PARM                    CMZNRR
026000941017     C                   PARM                    CMZPRR
026100971121     C                   PARM                    W236
026200941017     C                   ENDSR
026300941017     C*
026400971121      *----------------------------------------------------------------
026500971121      * OPERAZIONI INIZIALI
026600971121      *----------------------------------------------------------------
026700941018     C     *INZSR        BEGSR
026800971121      *
026900941018     C     *ENTRY        PLIST
027000941018     C                   PARM                    KPJBA
027100941018      *
027200941018     C                   Z-ADD     1             CODUT             1 0
027300941018     C                   CALL      'X§PARUT'
027400941018     C                   PARM                    UT§DSE
027500020719     C                   MOVEL     REC80         CNCR80
027600020719     C*
027700020920     C     SIMTPP        COMP      '2'                                    01
027800020920     C   01              MOVE      SIMPOU        §FLE              3 0
027900020920     C  N01              DO
028000020920     C     SIMFEL        COMP      0                                      02
028100020920     C  N02              MOVE      SIMFEL        §FLE
028200020920     C                   END
028201020925     C                   MOVE      SIMFEL        xFLE
028202020924     C                   Z-ADD     48            LUNG             15 5
028300971121     C* Reperisco data del giorno
028400971121     C                   TIME                    WHHUDT           14 0
028500971121     C                   MOVEL     WHHUDT        ORA               6 0
028600971121     C                   MOVE      WHHUDT        G08DAT
028700971121     C                   MOVE      *ZEROS        G08INV
028800971121     C                   MOVEL     *BLANK        G08ERR
028900971121     C                   CALL      'XSRDA8'
029000971121     C                   PARM                    WLBDA8
029100971121     C                   Z-ADD     G08INV        AMGUDT            8 0
029200941018     C*MEMORIZZO PER OGNI FILIALE LA FILIALE ELABORATORE
029300941018     C     *LOVAL        SETLL     AZORG01L
029400941018     C     *IN19         DOUEQ     '1'
029500941018     C                   READ      AZORG01L                               19
029600941018     C  N19ORGFAG        IFEQ      'F'
029700941018     C     ORGFAG        OREQ      'A'
029800971121      *  Reperisco terminal di partenza collegato
029900971121     C                   CLEAR                   FNLV55
030000971121     C* Imposto i dati in LV55
030100971121     C                   MOVEL     'P'           D55TPT
030200971121     C                   MOVEL     AMGUDT        D55DRF
030300971121     C                   MOVEL     ORGFIL        D55LNP
030400971121     C                   MOVEL     ORGFIL        D55LIN
030500971121     C                   CALL      'FNLV55R'
030600971121     C                   PARM                    FNLV55
030700941018     C                   ADD       1             X                 3 0
030800941018     C                   MOVE      ORGFIL        FIL(X)
031000971121     C                   MOVE      D55TFP        TFE(X)
031100941018     C                   END
031200941018     C                   END
031300941018      *
031400941018     C     *LIKE         DEFINE    CASLNP        SAVLNP
031500971121     C     *LIKE         DEFINE    CASLNP        SAVTFE
031600971125     C*
031700971125     C                   Z-ADD     0             T
031800971121     C*
031900941018     C                   ENDSR
032000991122**         CMDC
032100020920CHKOBJ OBJ(FNCAF00T) OBJTYPE(*FILE) MBR(M000000)
032200941017**         CMDA
032300020920ADDPFM FILE(FNCAF00T)               MBR(M000000)
032400941017**         CMDO
032500020920OVRDBF FILE(FNCAF00T)               MBR(M000000)
032600991122**         CMDD
032700991122DLTOVR FILE(FNCAF00T)
