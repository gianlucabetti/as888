000100960605      ***********************************************************************
000200960605      *   TRSMISSIONE TABELLE  E.D.I. ESTERO                                *
000300960605      ***********************************************************************
000400960605     H DECEDIT('0,') DATEDIT(*DMY.)
000500960605     F*---------------------------------------------------------------------*
000600960605     F*  DATA BASE                                                          *
000700960605     F*---------------------------------------------------------------------*
000800960610     FEDTAB02L  UP   E           K DISK
000900960605     F*---------
001000960610     FEDTAB00T  UF A E             DISK    USROPN
001100960605     F                                     RENAME(EDTAB000:EDTABT)
001200960605     D*---------------------------------------------------------------------*
001300960605     D*  SCHIERE                                                            *
001400960605     D*---------------------------------------------------------------------*
001500020930     D CMD1            S             48    DIM(1) CTDATA PERRCD(1)              Addpfm EDTAB
001600020930     D CMD2            S             48    DIM(1) CTDATA PERRCD(1)              Ovrdbf EDTAB
001700020930     D CMD3            S             48    DIM(1) CTDATA PERRCD(1)              Chkobj EDTAB
001800020930     D CMD4            S             48    DIM(1) CTDATA PERRCD(1)              Dltovr EDTAB
001900021014     D NRR             S              6  0
002000960605     D*---------------------------------------------------------------------*
002100960605     D*  DS                                                                 *
002200960605     D*---------------------------------------------------------------------*
002300960610     D KPJBA         E DS
002400960610     D  LIBSYS                92    101
002500960610     D DSCMZ         E DS
002600021014      *
002700960610     D WLBDA8          DS
002800960610     D  G02DAT                 1      8  0
002900960610     D  G02INV                 9     16  0
003000960610     D  G02ERR                17     17
003100960610     D  G02TGI                18     22  0
003200021014      *
003300980114     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
003400980114     D CNCR80        E DS
003500021014      *
003600020930     D TIBS55        E DS                  EXTNAME(TIBS55DS)
003700021002     d Tibs56Ds      e ds
003800021002     d  Poela                  7    756  0 dim(250)
003900021002     D ela             S              3  0 DIM(250)                             *elaboratori
004000021014     D wpo             S                   like(i50ppo)
004100960610     I*--------------------------------------------------------------*
004200960610     I*  FILE
004300960610     I*--------------------------------------------------------------*
004400960610     I*  Rotture di livello file primario
004500960610     IEDTAB000
004600021014     I              tabftt                      xTABFTT       L2
004700021014     I              tabflt                      xTABFLT       L1
004800960605     C*---------------------------------------------------------------------*
004900960605     C*  Ciclo Principale
005000960605     C*---------------------------------------------------------------------*
005100960610     C*  Rottura tipo trasmissione: verifico se devo trasmettere a tutti
005200960610     C   L2              EXSR      INIZ2
005300960610     C*  Rottura codice filiale a cui trasmettere: eseguo OVRPRTF - OPEN
005400960610     C   L1              EXSR      INIZ1
005500960610     C*  Dettaglio tabelle
005600960610     C                   EXSR      DETT
005700960610     C*  Totale filiale a cui trasmettere: eseguo close membri trasmissione
005800960610     CL1   WNOREC        IFEQ      'N'
005900960610     CL1                 EXSR      TOTA1
006000960610     CL1                 END
006100960610     C*  Totale tipo tramissione: eseguo close membro 000
006200960610     CL2   WNOREC        IFEQ      'N'
006300960610     CL2                 EXSR      TOTA2
006400960610     CL2                 END
006500980114     C* Imposto i dati in BS50
006600020930     CLR                 CLEAR                   TIBS55
006700980114     CLR                 MOVEL     'C'           I50TLA
006800020930     CLR                 CALL      'TIBS55R'
006900020930     CLR                 PARM                    TIBS55
007000960610     C*---------------------------------------------------------------------*
007100960610     C*  Iniz2: Rottura tipo trasmissione
007200960610     C*---------------------------------------------------------------------*
007300960610     C     INIZ2         BEGSR
007400960610     C*
007500960610     C*  Se devo trasmettere a tutte le filiali apro membro 000
007600021014     C     xTABFTT       IFEQ      '1'
007700960610     C                   MOVEL     '000'         WMBR
007800960610     C                   EXSR      OPNMBR
007900960610     C                   END
008000960610     C*
008100960610     C                   ENDSR
008200960610     C*---------------------------------------------------------------------*
008300960610     C*  Iniz1: Rottura filiale trasmissione
008400960610     C*---------------------------------------------------------------------*
008500960610     C     INIZ1         BEGSR
008600960610     C*
008700960610     C*  Se tipo trasmissione <> da 1 eseguo OPEN singoli membri
008800021014     C     xTABFTT       IFNE      '1'
008900021014     C                   MOVEL     xTABFLT       WMBR
009000960610     C                   EXSR      OPNMBR
009100960610     C                   END
009200960610     C*
009300960610     C                   ENDSR
009400960610     C*---------------------------------------------------------------------*
009500960610     C*  TOTA1: Finita filiale eseguo close dei file
009600960610     C*---------------------------------------------------------------------*
009700960610     C     TOTA1         BEGSR
009800960610     C*
009900021002     C*  Se avevo trasmesso alla filiale chiudo membro specifico
010000021014     C     xTABFTT       IFNE      '1'
010100021014     C                   MOVEL     xTABFLT       WMBR
010200960610     C                   EXSR      CLOMBR
010300021014     C*
010400021014     C* Controllo se membro singola filiale è pieno
010500021014     C     NRR           IFGT      *ZERO
010600021014      *
010700021014     C* Trasmette a una sola filiale
010800021014     C* Richiamo TIBS55R per conoscere librerie/S.Informativi
010900021014     C* da/a cui spedire
011000021014     C                   CLEAR                   TIBS55
011100021014     C                   MOVEL     *BLANKS       I50TLA
011200021014     C                   MOVEL     KNSIF         I50PSI
011300021014     C                   MOVEL     wpo           I50PPO
011400021014     C                   MOVEL     xtabflt       I50APO
011500021014     C                   CALL      'TIBS55R'
011600021014     C                   PARM                    TIBS55
011700021014      *
011800021014     C* Trasmetto membro singole filali
011900021014     C                   MOVE      xtabflt       §MBR              3             MEMBRO
012000021014     C                   MOVE      xtabflt       §AS4              3             DESTINAT.
012100021014     C                   MOVE      NRR           §NRR              5 0           nrr
012200021014     C                   MOVE      'S'           §CLR              1             CLEAR
012300021014     C                   EXSR      X25T
012400021014     C                   END
012500960610     C*
012600021014     C                   END
012700021014     C*
012800960610     C                   ENDSR
012900960610     C*---------------------------------------------------------------------*
013000960610     C*  Tota2: Rottura tipo trasmissione
013100960610     C*---------------------------------------------------------------------*
013200960610     C     TOTA2         BEGSR
013300960610     C*
013400021002     C*  Se avevo trasmesso a tutte le filiali chiudo membro specifico
013500021014     C     xTABFTT       IFEQ      '1'
013600960610     C                   MOVEL     '000'         WMBR              3
013700960610     C                   EXSR      CLOMBR
013800960610     C*
013900021014     C     NRR           IFGT      *ZERO
014000021014     C*
014100021014     C* Trasmetto a tutti il membro 000
014200021014     c                   do        250           y                 3 0
014300021014     C*
014400021014     c                   if        ela(y) <>0
014500021014     C* Richiamo TIBS55R per conoscere librerie/S.Informativi
014600021014     C* da/a cui spedire
014700021014     C                   CLEAR                   TIBS55
014800021014     C                   MOVEL     *BLANKS       I50TLA
014900021014     C                   MOVEL     KNSIF         I50PSI
015000021014     C                   MOVEL     wpo           I50PPO
015100021014     C                   MOVEL     ela(y)        I50APO
015200021014     C                   CALL      'TIBS55R'
015300021014     C                   PARM                    TIBS55
015400021014      *
015500021014     C                   MOVE      ela(Y)        §AS4
015600021014     C                   MOVE      '000'         §MBR
015700021014     C                   MOVE      NRR           §NRR
015800021014     C                   MOVE      'N'           §CLR                            CLEAR
015900021014     C                   EXSR      X25T
016000021014     c                   endif
016100021014      *
016200021014     C                   END
016300021014      *
016400021014     c                   endif
016500021014     C*
016600021014     C* Cancello records del membro 000
016700021014     C                   EXSR      OPNMBR
016800021014     C                   READ      EDTAB00T                               30
016900021014     C     *IN30         DOWEQ     '0'
017000021014     C                   DELETE    EDTABT
017100021014     C                   READ      EDTAB00T                               30
017200021014     C                   END
017300021014     C                   EXSR      CLOMBR
017400021014     C*
017500021014     C                   END
017600021014     C*
017700960610     C                   ENDSR
017800960610     C*---------------------------------------------------------------------*
017900960610     C*  DETT: Dettaglio singola tabella
018000960610     C*---------------------------------------------------------------------*
018100960610     C     DETT          BEGSR
018200960610     C*
018300960610     C*  Imposto flag file no vuoto
018400960610     C                   MOVEL     'N'           WNOREC            1
018500960610     C*  scrivo record di trasmissione
018600021014     C                   ADD       1             NRR
018700960610     C                   WRITE     EDTABT
018800960610     C*  Se ho annullato record procedo alla sua cancellazione
018900960610     C     TABFLG        IFEQ      '*'
019000960610     C                   DELETE    EDTAB000
019100960610     C                   ELSE
019200960610     C                   MOVEL     'T'           TABFTR
019300960610     C                   Z-ADD     WOGGI         TABDTR
019400960610     C                   EXCEPT    AGGTRA
019500960610     C                   END
019600960610     C*
019700960610     C                   ENDSR
019800960610     C*---------------------------------------------------------------------*
019900960610     C*  OPNMBR: Eseguo open singolo membro
020000960610     C*---------------------------------------------------------------------*
020100960610     C     OPNMBR        BEGSR
020200960610     C*
020300020930     C                   Z-ADD     48            LUNG             15 5
020400960610     C* Controllo se esiste membro
020500960610     C                   SETOFF                                       20
020600960610     C                   MOVEL     WMBR          VAR               4
020700960610     C                   MOVE      ')'           VAR               4
020800960610     C                   MOVE      VAR           CMD3
020900960610     C                   MOVEL     *BLANKS       COMMAN
021000960610     C                   MOVEA     CMD3(1)       COMMAN           80
021100960610     C                   CALL      'QCMDEXC'                            20
021200960610     C                   PARM                    COMMAN
021300960610     C                   PARM                    LUNG
021400960610     C* Se non esiste lo aggiungo
021500960610     C   20              DO
021600960610     C                   MOVE      VAR           CMD1
021700960610     C                   MOVEL     *BLANKS       COMMAN
021800960610     C                   MOVEA     CMD1(1)       COMMAN
021900960610     C                   CALL      'QCMDEXC'
022000960610     C                   PARM                    COMMAN
022100960610     C                   PARM                    LUNG
022200960610     C                   END
022300960610     C* Esegue  OVRDBF del membro nuovo
022400960610     C                   MOVE      VAR           CMD2
022500960610     C                   MOVEL     *BLANKS       COMMAN
022600960610     C                   MOVEA     CMD2(1)       COMMAN
022700960610     C                   CALL      'QCMDEXC'
022800960610     C                   PARM                    COMMAN
022900960610     C                   PARM                    LUNG
023000960610     C* Open file di trasmissione
023100960610     C                   OPEN      EDTAB00T
023200960610     C*
023300960610     C                   ENDSR
023400960610     C*---------------------------------------------------------------------*
023500960610     C*  CLOMBR: Eseguo open singolo membro
023600960610     C*---------------------------------------------------------------------*
023700960610     C     CLOMBR        BEGSR
023800960610     C*
023900960610     C* Close file di trasmissione
024000960610     C                   CLOSE     EDTAB00T
024100960610     C* Controllo se esiste membro
024200960610     C                   MOVEA     CMD4(1)       COMMAN           80
024300960610     C                   CALL      'QCMDEXC'                            20
024400960610     C                   PARM                    COMMAN
024500960610     C                   PARM                    LUNG
024600960610     C*
024700960610     C                   ENDSR
024800960610     C*----------------------------------------------------------
024900960610     C* Sottomette trasmissione
025000960610     C*----------------------------------------------------------
025100960610     C     X25T          BEGSR
025200960610     C*
025300960610     C                   MOVEL     §AS4          CMZDST                          DESTINAT.
025400960610     C                   MOVEL     'EDTAB00T'    CMZFLD                          DA: >>>
025500020930     C                   MOVEL     'M046'        VAR7
025600020930     C                   MOVE      §MBR          VAR7              7
025700960610     C                   MOVEL     *BLANK        CMZMBD
025800020930     C                   MOVEL     VAR7          CMZMBD
025900020930     C                   MOVEL     O50PLG        CMZLBD
026000960610     C                   MOVEL     'EDTAB00R'    CMZFLA                          A: >>>
026100960610     C                   MOVEL     *BLANK        CMZMBA
026200020930     C                   MOVEL     VAR7          CMZMBA
026300020930     C                   MOVEL     O50ALG        CMZLBA
026400960610     C                   MOVE      '0000'        CMZORI                          ORA
026500960610     C                   MOVE      '0000000'     CMZDTI                          DATA
026600960610     C                   MOVE      'T'           CMZURG                          NOTTE
026700960610     C                   MOVE      'A'           CMZADD                          IN ADD
026800960610     C                   MOVE      §CLR          CMZCLR                          CLEAR
026900960610     C                   MOVE      'S'           CMZFTR                          DA TRANS.
027000960610     C                   MOVE      *BLANK        CMZERR                          1= ERR.
027100960610     C                   MOVE      'TC99'        CMZAZN                          AZIONE
027200020930     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
027300980114     C                   MOVE      §NRR          CMZNRR                          N.REC.
027400960610     C                   MOVE      *BLANK        CMZPRR                          PRE-REQ.
027500980114     C* Imposto passaggio parametri Kpjba x locali
027600980114     C                   MOVEL     *BLANKS       W236            236
027700020930     C                   CALL      'TIBS53C'
027800980114     C                   PARM                    O50LCL
027900960610     C                   PARM                    CMZDST
028000960610     C                   PARM                    CMZFLD
028100960610     C                   PARM                    CMZMBD
028200960610     C                   PARM                    CMZLBD
028300960610     C                   PARM                    CMZFLA
028400960610     C                   PARM                    CMZMBA
028500960610     C                   PARM                    CMZLBA
028600960610     C                   PARM                    CMZORI
028700960610     C                   PARM                    CMZDTI
028800960610     C                   PARM                    CMZURG
028900960610     C                   PARM                    CMZADD
029000960610     C                   PARM                    CMZCLR
029100960610     C                   PARM                    CMZFTR
029200960610     C                   PARM                    CMZERR
029300960610     C                   PARM                    CMZAZN
029400960610     C                   PARM                    CMZSYF
029500960610     C                   PARM                    CMZNRR
029600960610     C                   PARM                    CMZPRR
029700980114     C                   PARM                    W236
029800960610     C*
029900960610     C                   ENDSR
030000960610     C*---------------------------------------------------------------------*
030100960610     C*  *INZSR: Operazioni iniziali
030200960610     C*---------------------------------------------------------------------*
030300960610     C     *INZSR        BEGSR
030400960610     C*
030500960610     C     *ENTRY        PLIST
030600960610     C                   PARM                    KPJBA
030700980114     C*  Richiamo Parut
030800980114     C                   Z-ADD     1             CODUT
030900980114     C                   CALL      'X§PARUT'
031000980114     C                   PARM                    UTEDSE
031100980114     C                   MOVEL     REC80         CNCR80
031200980205     C*
031300980205     C     SIMFEL        COMP      0                                  LRLR
031400021014      * imposta il P.O. dell'elaboratore
031500021014     c                   z-add     simfel        wpo
031600021014      * se sede
031700021014     c                   if        simfel=000
031800021014     c                   z-add     46            wpo
031900021014     c                   endif
032000021014     C*
032100960610     C*  Reperisco data del giorno
032200960610     C                   TIME                    WHHDAT           14 0
032300960610     C                   MOVE      WHHDAT        G02DAT
032400960610     C                   MOVEL     *BLANK        G02ERR
032500960610     C                   CALL      'XSRDA8'
032600960610     C                   PARM                    WLBDA8
032700960610     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
032800021014      *
032900960610     C                   Z-ADD     0             NRR
033000960610     C                   Z-ADD     1             X                 3 0
033100960610     C*
033200021002      * Carico tutti i p.o. a cui trasmettere  solo
033300021002      *   esclusivamente dalla sede.
033400021002     c                   if        simtpp ='S'
033500021002     c                   Clear                   Tibs56Ds
033600021014     c                   Eval      I56ppo =  simfel
033700021002     c                   Eval      I56mod = 'GRU'
033800021002     c                   Call      'TIBS56R'
033900021002     c                   Parm                    Tibs56Ds
034000021002     c                   Movea     Poela         Ela
034100021002     c                   end
034200021002      *
034300960610     C                   ENDSR
034400960610     O*-----------------------------------------------------*
034500960610     OEDTAB000  E            AGGTRA
034600960610     O                       TABFTR
034700960610     O                       TABDTR
034800960610**         CMD1
034900020930ADDPFM FILE(EDTAB00T)               MBR(M046000)
035000960605**         CMD2
035100020930OVRDBF FILE(EDTAB00T)               MBR(M046000)
035200960605**         CMD3
035300020930CHKOBJ OBJ(EDTAB00T) OBJTYPE(*FILE) MBR(M046000)
035400960610**         CMD4
035500960610DLTOVR FILE(EDTAB00T)
