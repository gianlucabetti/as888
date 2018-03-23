000100030508      /TITLE Scrittura TITAS00F bolle particolari.
000200030508     ***********************************************************************
000300030508     ** Questo programma scrive 1 record nel file TITAS00F impostando il
000400030508     ** valore dei campi a seconda della funzione chiamante.
000500030508     **
000600030508     ** PARAMETRI DI INPUT
000700030508     **
000800030508     ** PrmFnz     Funzione chiamante
000900030508     **            'IN' = Insoluti
001000030508     ** PrmKsc     Codice cliente
001100030508     ** PrmRMN     Riferimento mittente numerico
001200030508     ** PrmCmt     Controllo sincronia file
001300030508     **            '1' = aprire i file col controllo sincronia.
001400030508     ** PrmTLa     Tipo lancio
001500030508     **            ' ' = esegue elaborazione e chiude con RETURN.
001600030508     **            'L' = esegue elaborazione e chiude con LR.
001700030508     **            'C' = non esegue elaborazione e chiude con LR.
001800030508     **
001900030508     ** PARAMETRI DI OUTPUT
002000030508     **
002100030508     ** PrmEsi     Esito
002200030508     **            '0' = eseguito.
002300030508     **            '1' = errori.
002400030516     ** PRMAAS     ANNO SPEDIZIONE
002500030516     ** PRMLNP     P.O. PARTENZA
002600030516     ** PRMNRS     NUMERO SERIE
002700030516     ** PRMNSP     NUMERO SPEDIZIONE
002800030508     **
002900030508     ***********************************************************************
003000030507
003100030508     H DFTACTGRP(*NO) ACTGRP(*CALLER) DATEDIT(*YMD)
003200030507
003300030507     FTiTas00F  O    E             DISK
003400030508     F                                     COMMIT(PrmCmt)
003500030508     F                                     INFDS(InfTiTas)
003600030508     FFlNuf01L  UF A E           K DISK
003700030508     F                                     COMMIT(PrmCmt)
003800030507
003900030507     D Insoluti        C                   'IN'
004000030507     D Eseguito        C                   '0'
004100030507     D Errore          C                   '1'
004200030508     D Chiudere        C                   'C'
004300030526     D Cliente         C                   'C'
004400030508
004500030508     D                SDS
004600030508     D ProcName          *PROC
004700030508
004800030508     D InfTiTas        DS
004900030508     D  RRNTiTas             397    400I 0
005000030508
005100030508     D NullPtr         S               *
005200030508     D CnDizion      E DS
005300030508     D                                     BASED(NullPtr)
005400030508     D                                     PREFIX(Diz)
005500030507
005600030508     D PrmFnz          S              2                                         Funzione chiamante
005700030508     D PrmKsc          S                                                        Codice cliente
005800030508     D                                     LIKE(Diz$CSC)
005900030508     D PrmRMN          S                                                        Rif. mittente num.
006000030508     D                                     LIKE(Diz$RMN)
006100030508     D PrmCmt          S              1                                         Sincronia file
006200030508     D PrmTLa          S              1                                         Tipo lancio
006300030508     D PrmEsi          S              1                                         Esito
006400030516     D PrmAAS          S
006500030516     D                                     LIKE(Diz$AAS)
006600030516     D PrmLnP          S
006700030516     D                                     LIKE(Diz$LnP)
006800030516     D PrmNrS          S
006900030516     D                                     LIKE(Diz$NrS)
007000030516     D PrmNSp          S
007100030516     D                                     LIKE(Diz$NSp)
007200030508
007300030508     D UT§DSE0F      E DS
007400030508     D                                     INZ
007500030508     D CNCR40        E DS
007600030508     D                                     INZ
007700030508     D TIBS69DS      E DS
007800030508     D                                     INZ
007900030508     D CnAco00F      E DS
008000030508     D                                     INZ
008100030508     D CnInd00F      E DS
008200030508     D                                     INZ
008300030508     D CnClp00F      E DS
008400030508     D                                     INZ
008500030508     D FnCls00F      E DS
008600030508     D                                     INZ
008700030508
008800030508     D/COPY QSYSINC/QRPGLESRC,QUSEC
008900030508
009000030508     D SndPM01         S              7                                         Message id
009100030508     D                                     INZ('CPF9897')
009200030508     D SndPM02         S             20                                         Message file name
009300030508     D                                     INZ('QCPFMSG   *LIBL     ')
009400030508     D SndPM03         S            512                                         Immediate text
009500030508     D SndPM04         S             10I 0                                      Length of text
009600030508     D                                     INZ(%SIZE(SndPM03))                  Length of text
009700030508     D SndPM05         S             10                                         Message type
009800030508     D                                     INZ('*INFO')
009900030508     D SndPM06         S             16                                         Call stack entry
010000030508     D                                     INZ('*')
010100030508     D SndPM07         S             10I 0                                      Call stack counter
010200030508     D SndPM08         S              4                                         Call stack counter
010300030507
010400030507     C     *ENTRY        PLIST
010500030508     C                   PARM                    PrmFnz
010600030508     C                   PARM                    PrmKsc
010700030508     C                   PARM                    PrmRMN
010800030508     C                   PARM                    PrmCmt
010900030508     C                   PARM                    PrmTLa
011000030508     C                   PARM                    PrmEsi
011100030516     C                   PARM                    PrmAAS
011200030516     C                   PARM                    PrmLnP
011300030516     C                   PARM                    PrmNrS
011400030516     C                   PARM                    PrmNSp
011500030507
011600030507     C                   EXSR      ChkPrm
011700030507     C                   EXSR      WrtTiTas
011800030507     C                   EXSR      Uscita
011900030507
012000030507     ***********************************************************************
012100030507     **
012200030507     ** Operazioni iniziali.
012300030507     **
012400030507     ***********************************************************************
012500030507     C     *INZSR        BEGSR
012600030507
012700030507     C     K04Nuf01      KLIST
012800030507     C                   KFLD                    NufAAA
012900030507     C                   KFLD                    NufCNu
013000030507     C                   KFLD                    NufFil
013100030507     C                   KFLD                    NufFl2
013200030508
013300030508     C                   EVAL      NufAAA = UYEAR
013400030508     C                   EVAL      NufCNu = 103
013500030508     C                   EVAL      NufFil = 102
013600030508
013700030526     C*                  CALL      'X§PARUT'
013800030526     C*                  PARM                    UT§DSE0F
013900030526     C*                  EVAL      CNCR40 = Rec40
014000030507
014100030507     C                   ENDSR
014200030507
014300030507     ***********************************************************************
014400030507     **
014500030507     ** Controllo parametri ricevuti.
014600030507     **
014700030507     ***********************************************************************
014800030507     C     ChkPrm        BEGSR
014900030507
015000030508     C                   CLEAR                   PrmEsi
015100030516     C                   CLEAR                   PrmAAS
015200030516     C                   CLEAR                   PrmLnP
015300030516     C                   CLEAR                   PrmNrS
015400030516     C                   CLEAR                   PrmNSp
015500030508
015600030508     C                   IF        PrmTLa = Chiudere
015700030508     C                   EXSR      Uscita
015800030508     C                   ENDIF
015900030507
016000030508     C                   SELECT
016100030508
016200030508     C                   WHEN      PrmFnz = Insoluti
016300030508
016400030508     C                   IF        PrmKsc <= 0
016500030507     C                             OR
016600030508     C                             PrmRMN <= 0
016700030508     C                   EVAL      SndPM03 = ProcName + ' ERRORE'
016800030508     C                             + ' Parametri mancanti o non validi.'
016900030508     C                   EXSR      CalQMHSNDPM
017000030508     C                   EVAL      PrmEsi = Errore
017100030507     C                   EXSR      Uscita
017200030507     C                   ENDIF
017300030508
017400030508     C                   OTHER
017500030508
017600030508     C                   EVAL      SndPM03 = ProcName + ' ERRORE'
017700030508     C                             + ' Funzione chiamante sconosciuta.'
017800030508     C                   EXSR      CalQMHSNDPM
017900030508     C                   EVAL      PrmEsi = Errore
018000030508     C                   EXSR      Uscita
018100030508
018200030508     C                   ENDSL
018300030509
018400030509     ** Scarto i clienti non codificati.
018500030509     C                   IF        %SUBST(%EDITC(PrmKsc:'X'):4:4) = '8888'
018600030509     C                             OR
018700030509     C                             %SUBST(%EDITC(PrmKsc:'X'):4:4) = '9999'
018800030509     C                   EXSR      Uscita
018900030509     C                   ENDIF
019000030507
019100030507     C                   ENDSR
019200030507
019300030507     ***********************************************************************
019400030507     **
019500030507     ** Scrittura TITAS00F.
019600030507     **
019700030507     ***********************************************************************
019800030507     C     WrtTiTas      BEGSR
019900030507
020000030508     C                   EXSR      SetTiTas
020100030516
020200030508     C                   WRITE     TITAS000
020300030516
020400030516     C                   EVAL      PrmAAS = TasAAS
020500030516     C                   EVAL      PrmLnP = TasLnP
020600030516     C                   EVAL      PrmNrS = TasNrS
020700030516     C                   EVAL      PrmNSp = TasNSp
020800030507
020900030507     C                   ENDSR
021000030508
021100030508     ***********************************************************************
021200030508     **
021300030508     ** Impostaziione TITAS00F.
021400030508     **
021500030508     ***********************************************************************
021600030508     C     SetTiTas      BEGSR
021700030508
021800030508     C                   CLEAR                   TiTas000
021900030508
022000030508     C                   SELECT
022100030508
022200030508     C                   WHEN      PrmFnz = Insoluti
022300030526
022400030526     C                   CLEAR                   TIBS69DS
022500030526     C                   EVAL      I69KAc = PrmKsc
022600030526     C                   EVAL      I69KIn = PrmKsc
022700030526     C                   CLEAR                   CnAco00F
022800030526     C                   CLEAR                   CnInd00F
022900030526
023000030526     C                   CALL      'TIBS69R'
023100030526     C                   PARM                    TIBS69DS
023200030526     C                   PARM                    CnAco00F
023300030526     C                   PARM                    CnInd00F
023400030526     C                   PARM                    CnClp00F
023500030526     C                   PARM                    FnCls00F
023600030526
023700030526     C                   IF        O69Err = Errore
023800030526     C                   EVAL      SndPM03 = ProcName + ' ERRORE ' + O69Msg
023900030526     C                   EXSR      CalQMHSNDPM
024000030526     C                   EVAL      PrmEsi = Errore
024100030526     C                   EXSR      Uscita
024200030526     C                   ENDIF
024300030526
024400030508     C                   EVAL      TasAAS = *YEAR
024500030508     C                   EVAL      TasLnP = 102
024600030508     C                   EVAL      TasNrS = 0
024700030508     C                   EXSR      SetTasNSp
024800030526     C                   EVAL      TasTBl = 'A3'
024900030526     C                   EVAL      TasCBo = 'FM'
025000030508     C                   EVAL      TasMGS = (*MONTH * 100) + *DAY
025100030508     C                   EVAL      TasKsc = PrmKsc
025200030609     C                   MOVEL     PrmKsc        TasLnA
025300030508     C                   EVAL      TasNCl = 1
025400030508     C                   EVAL      TasPKB = 0,1
025500030508     C                   EVAL      TasFPB = 'R'
025600030508     C                   EVAL      TasPKF = 0,1
025700030508     C                   EVAL      TasFPF = 'R'
025800030508     C                   EVAL      TasFVB = 'R'
025900030508     C                   EVAL      TasVLB = 0,001
026000030508     C                   EVAL      TasVLF = 0,001
026100030508     C                   EVAL      TasFVF = 'R'
026200030508     C                   EVAL      TasTSP = 'C'
026300030526     C                   EVAL      TasRSD = AcoRag
026400030526     C                   EVAL      TasInD = IndVia
026500030526     C                   EVAL      TasCaD = %EDITC(IndCAP:'X')
026600030526     C                   EVAL      TasLoD = IndCit
026700030526     C                   EVAL      TasPrD = IndPrv
026800030508     C                   EVAL      TasCTS = '40'
026900030516     C                   EVAL      TasFIn = 'C'
027000030526     C                   EVAL      TasCCM = 1020005
027100030508     C                   EVAL      TasRMN = PrmRMN
027200030508     C                   EVAL      TasMCT = '40'
027300030526     C                   EVAL      TasNAS = 'SPESE INSOLUTO'
027400030508     C                   EVAL      TasFlB = 102
027500030508     C                   EVAL      TasTFA = 1
027600030508     C                   EVAL      TasTFP = 1
027700030508     C                   EVAL      TasDIm = *DATE
027800030526     C                   EVAL      TasSOp = '*'
027900030508     C                   EVAL      TasPdr = 1020999
028000030508     C                   EVAL      TasDRt = *DATE
028100030508     C                   EVAL      TasFPP = 'P'
028200030526     C                   EVAL      TasCTM = '4F'
028300030508     C                   EVAL      TasDBr = *DATE
028400030508     C                   EVAL      TasFDN = 'S'
028500030508     C                   EVAL      TasLL1 = 'Y'
028600030526     C                   EVAL      TasCaM = %EDITC(IndCAP:'X')
028700030508     C                   EVAL      TasDiv = 'EUR'
028800030508     C                   EVAL      TasSV1 = 'M'
028900030508     C                   EVAL      TasVa1 = 28,41
029000030508     C                   EVAL      TasIMV = 28,41
029100030508     C                   EVAL      TasSCL = 'B'
029200030508
029300030508     C                   ENDSL
029400030508
029500030508     C                   ENDSR
029600030508
029700030508     ***********************************************************************
029800030508     **
029900030508     ** Impostazione numero spedizione.
030000030508     **
030100030508     ***********************************************************************
030200030508     C     SetTasNSp     BEGSR
030300030508
030400030508     ** Reperisco l'ultimo numero spedizione usato.
030500030508     C                   DO        *HIVAL
030600030508     C     K04Nuf01      CHAIN(E)  FlNuf01L
030700030508     C                   SELECT
030800030508     C                   WHEN      NOT %ERROR
030900030508     C                   LEAVE
031000030508     C                   WHEN      %ERROR AND %STATUS <> 1218
031100030508     C                   EVAL      SndPM03 = ProcName + ' ERRORE'
031200030508     C                             + ' Lettura FLNUF01L non riuscita.'
031300030508     C                   EXSR      CalQMHSNDPM
031400030508     C                   EVAL      PrmEsi = Errore
031500030508     C                   EXSR      Uscita
031600030508     C                   ENDSL
031700030508     C                   ENDDO
031800030508
031900030508     ** Aggiorno il numeratore. Se non esiste lo creo.
032000030508     C                   IF        %FOUND
032100030508     C                   EVAL      NufNum = NufNum + 1
032200030527     C                   EVAL      NufDat = UDATE
032300030508     C                   UPDATE    FlNuf
032400030508     C                   ELSE
032500030508     C     *NOKEY        CLEAR                   FlNuf
032600030508     C                   EVAL      NufNum = 1
032700030508     C                   EVAL      NufDat = UDATE
032800030508     C                   WRITE     FlNuf
032900030508     C                   ENDIF
033000030508
033100030508     C                   EVAL      TasNSp = NufNum
033200030508
033300030508     C                   ENDSR
033400030507
033500030508     ***********************************************************************
033600030508     **
033700030508     ** Send Program Message (QMHSNDPM) API
033800030508     **
033900030508     ***********************************************************************
034000030508     C     CalQMHSNDPM   BEGSR
034100030508
034200030508     C                   CLEAR                   QUSEC
034300030508     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
034400030508
034500030508     C                   CALL      'QMHSNDPM'
034600030508     C                   PARM                    SndPM01
034700030508     C                   PARM                    SndPM02
034800030508     C                   PARM                    SndPM03
034900030508     C                   PARM                    SndPM04
035000030508     C                   PARM                    SndPM05
035100030508     C                   PARM                    SndPM06
035200030508     C                   PARM                    SndPM07
035300030508     C                   PARM                    SndPM08
035400030508     C                   PARM                    QUSEC
035500030508
035600030508     C                   ENDSR
035700030508
035800030507     ***********************************************************************
035900030507     **
036000030507     **
036100030507     **
036200030507     ***********************************************************************
036300030507     C     Uscita        BEGSR
036400030507
036500030508     C                   IF        PrmEsi = *BLANK
036600030508     C                   EVAL      PrmEsi = Eseguito
036700030508     C                   ENDIF
036800030508
036900030508     C                   EVAL      *INLR = (PrmTLa <> *BLANK)
037000030507     C                   RETURN
037100030507
037200030507     C                   ENDSR
