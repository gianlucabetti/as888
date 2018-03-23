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
003600041201     F***FlNuf01L  UF A E           K DISK
003700041201     F***                                  COMMIT(PrmCmt)
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
007300041201     D KPJBA         E DS
007400041201     d trul33ds      e ds
007500030508     D UT§DSE0F      E DS
007600030508     D                                     INZ
007700030508     D CNCR40        E DS
007800030508     D                                     INZ
007900030508     D TIBS69DS      E DS
008000030508     D                                     INZ
008100030508     D CnAco00F      E DS
008200030508     D                                     INZ
008300030508     D CnInd00F      E DS
008400030508     D                                     INZ
008500030508     D CnClp00F      E DS
008600030508     D                                     INZ
008700030508     D FnCls00F      E DS
008800030508     D                                     INZ
008900030508
009000030508     D/COPY QSYSINC/QRPGLESRC,QUSEC
009100030508
009200030508     D SndPM01         S              7                                         Message id
009300030508     D                                     INZ('CPF9897')
009400030508     D SndPM02         S             20                                         Message file name
009500030508     D                                     INZ('QCPFMSG   *LIBL     ')
009600030508     D SndPM03         S            512                                         Immediate text
009700030508     D SndPM04         S             10I 0                                      Length of text
009800030508     D                                     INZ(%SIZE(SndPM03))                  Length of text
009900030508     D SndPM05         S             10                                         Message type
010000030508     D                                     INZ('*INFO')
010100030508     D SndPM06         S             16                                         Call stack entry
010200030508     D                                     INZ('*')
010300030508     D SndPM07         S             10I 0                                      Call stack counter
010400030508     D SndPM08         S              4                                         Call stack counter
010500030507
010600030507     C     *ENTRY        PLIST
010700030508     C                   PARM                    PrmFnz
010800030508     C                   PARM                    PrmKsc
010900030508     C                   PARM                    PrmRMN
011000030508     C                   PARM                    PrmCmt
011100030508     C                   PARM                    PrmTLa
011200030508     C                   PARM                    PrmEsi
011300030516     C                   PARM                    PrmAAS
011400030516     C                   PARM                    PrmLnP
011500030516     C                   PARM                    PrmNrS
011600030516     C                   PARM                    PrmNSp
011700030507
011800030507     C                   EXSR      ChkPrm
011900030507     C                   EXSR      WrtTiTas
012000030507     C                   EXSR      Uscita
012100030507
012200030507     ***********************************************************************
012300030507     **
012400030507     ** Operazioni iniziali.
012500030507     **
012600030507     ***********************************************************************
012700030507     C     *INZSR        BEGSR
012800030507
012900041201     C**   K04Nuf01      KLIST
013000041201     C**                 KFLD                    NufAAA
013100041201     C**                 KFLD                    NufCNu
013200041201     C**                 KFLD                    NufFil
013300041201     C**                 KFLD                    NufFl2
013400030508
013500041201     C**                 EVAL      NufAAA = UYEAR
013600041201     C**                 EVAL      NufCNu = 103
013700041201     C**                 EVAL      NufFil = 102
013800030508
013900030526     C*                  CALL      'X§PARUT'
014000030526     C*                  PARM                    UT§DSE0F
014100030526     C*                  EVAL      CNCR40 = Rec40
014200030507
014300030507     C                   ENDSR
014400030507
014500030507     ***********************************************************************
014600030507     **
014700030507     ** Controllo parametri ricevuti.
014800030507     **
014900030507     ***********************************************************************
015000030507     C     ChkPrm        BEGSR
015100030507
015200030508     C                   CLEAR                   PrmEsi
015300030516     C                   CLEAR                   PrmAAS
015400030516     C                   CLEAR                   PrmLnP
015500030516     C                   CLEAR                   PrmNrS
015600030516     C                   CLEAR                   PrmNSp
015700030508
015800030508     C                   IF        PrmTLa = Chiudere
015900030508     C                   EXSR      Uscita
016000030508     C                   ENDIF
016100030507
016200030508     C                   SELECT
016300030508
016400030508     C                   WHEN      PrmFnz = Insoluti
016500030508
016600030508     C                   IF        PrmKsc <= 0
016700030507     C                             OR
016800030508     C                             PrmRMN <= 0
016900030508     C                   EVAL      SndPM03 = ProcName + ' ERRORE'
017000030508     C                             + ' Parametri mancanti o non validi.'
017100030508     C                   EXSR      CalQMHSNDPM
017200030508     C                   EVAL      PrmEsi = Errore
017300030507     C                   EXSR      Uscita
017400030507     C                   ENDIF
017500030508
017600030508     C                   OTHER
017700030508
017800030508     C                   EVAL      SndPM03 = ProcName + ' ERRORE'
017900030508     C                             + ' Funzione chiamante sconosciuta.'
018000030508     C                   EXSR      CalQMHSNDPM
018100030508     C                   EVAL      PrmEsi = Errore
018200030508     C                   EXSR      Uscita
018300030508
018400030508     C                   ENDSL
018500030509
018600030509     ** Scarto i clienti non codificati.
018700030509     C                   IF        %SUBST(%EDITC(PrmKsc:'X'):4:4) = '8888'
018800030509     C                             OR
018900030509     C                             %SUBST(%EDITC(PrmKsc:'X'):4:4) = '9999'
019000030509     C                   EXSR      Uscita
019100030509     C                   ENDIF
019200030507
019300030507     C                   ENDSR
019400030507
019500030507     ***********************************************************************
019600030507     **
019700030507     ** Scrittura TITAS00F.
019800030507     **
019900030507     ***********************************************************************
020000030507     C     WrtTiTas      BEGSR
020100030507
020200030508     C                   EXSR      SetTiTas
020300030516
020400030508     C                   WRITE     TITAS000
020500030516
020600030516     C                   EVAL      PrmAAS = TasAAS
020700030516     C                   EVAL      PrmLnP = TasLnP
020800030516     C                   EVAL      PrmNrS = TasNrS
020900030516     C                   EVAL      PrmNSp = TasNSp
021000030507
021100030507     C                   ENDSR
021200030508
021300030508     ***********************************************************************
021400030508     **
021500030508     ** Impostaziione TITAS00F.
021600030508     **
021700030508     ***********************************************************************
021800030508     C     SetTiTas      BEGSR
021900030508
022000030508     C                   CLEAR                   TiTas000
022100030508
022200030508     C                   SELECT
022300030508
022400030508     C                   WHEN      PrmFnz = Insoluti
022500030526
022600030526     C                   CLEAR                   TIBS69DS
022700030526     C                   EVAL      I69KAc = PrmKsc
022800030526     C                   EVAL      I69KIn = PrmKsc
022900030526     C                   CLEAR                   CnAco00F
023000030526     C                   CLEAR                   CnInd00F
023100030526
023200030526     C                   CALL      'TIBS69R'
023300030526     C                   PARM                    TIBS69DS
023400030526     C                   PARM                    CnAco00F
023500030526     C                   PARM                    CnInd00F
023600030526     C                   PARM                    CnClp00F
023700030526     C                   PARM                    FnCls00F
023800030526
023900030526     C                   IF        O69Err = Errore
024000030526     C                   EVAL      SndPM03 = ProcName + ' ERRORE ' + O69Msg
024100030526     C                   EXSR      CalQMHSNDPM
024200030526     C                   EVAL      PrmEsi = Errore
024300030526     C                   EXSR      Uscita
024400030526     C                   ENDIF
024500030526
024600030508     C                   EVAL      TasAAS = *YEAR
024700030508     C                   EVAL      TasLnP = 102
024800030508     C                   EVAL      TasNrS = 0
024900030508     C                   EXSR      SetTasNSp
025000030526     C                   EVAL      TasTBl = 'A3'
025100030526     C                   EVAL      TasCBo = 'FM'
025200030508     C                   EVAL      TasMGS = (*MONTH * 100) + *DAY
025300030508     C                   EVAL      TasKsc = PrmKsc
025400030609     C                   MOVEL     PrmKsc        TasLnA
025500030508     C                   EVAL      TasNCl = 1
025600030508     C                   EVAL      TasPKB = 0,1
025700030508     C                   EVAL      TasFPB = 'R'
025800030508     C                   EVAL      TasPKF = 0,1
025900030508     C                   EVAL      TasFPF = 'R'
026000030508     C                   EVAL      TasFVB = 'R'
026100030508     C                   EVAL      TasVLB = 0,001
026200030508     C                   EVAL      TasVLF = 0,001
026300030508     C                   EVAL      TasFVF = 'R'
026400030508     C                   EVAL      TasTSP = 'C'
026500030526     C                   EVAL      TasRSD = AcoRag
026600030526     C                   EVAL      TasInD = IndVia
026700030526     C                   EVAL      TasCaD = %EDITC(IndCAP:'X')
026800030526     C                   EVAL      TasLoD = IndCit
026900030526     C                   EVAL      TasPrD = IndPrv
027000030508     C                   EVAL      TasCTS = '40'
027100030516     C                   EVAL      TasFIn = 'C'
027200030526     C                   EVAL      TasCCM = 1020005
027300030508     C                   EVAL      TasRMN = PrmRMN
027400030508     C                   EVAL      TasMCT = '40'
027500030526     C                   EVAL      TasNAS = 'SPESE INSOLUTO'
027600030508     C                   EVAL      TasFlB = 102
027700030508     C                   EVAL      TasTFA = 1
027800030508     C                   EVAL      TasTFP = 1
027900030508     C                   EVAL      TasDIm = *DATE
028000030526     C                   EVAL      TasSOp = '*'
028100030508     C                   EVAL      TasPdr = 1020999
028200030508     C                   EVAL      TasDRt = *DATE
028300030508     C                   EVAL      TasFPP = 'P'
028400030526     C                   EVAL      TasCTM = '4F'
028500030508     C                   EVAL      TasDBr = *DATE
028600030508     C                   EVAL      TasFDN = 'S'
028700030508     C                   EVAL      TasLL1 = 'Y'
028800030526     C                   EVAL      TasCaM = %EDITC(IndCAP:'X')
028900030508     C                   EVAL      TasDiv = 'EUR'
029000030508     C                   EVAL      TasSV1 = 'M'
029100030508     C                   EVAL      TasVa1 = 28,41
029200030508     C                   EVAL      TasIMV = 28,41
029300030508     C                   EVAL      TasSCL = 'B'
029400030508
029500030508     C                   ENDSL
029600030508
029700030508     C                   ENDSR
029800030508
029900030508     ***********************************************************************
030000030508     **
030100030508     ** Impostazione numero spedizione.
030200030508     **
030300030508     ***********************************************************************
030400030508     C     SetTasNSp     BEGSR
030500030508
030600030508     ** Reperisco l'ultimo numero spedizione usato.
030700041201     C                   DO        *HIVAL
030800041201     C**   K04Nuf01      CHAIN(E)  FlNuf01L
030900041201     c* lo prendo da AZNUM con trul33r
031000041201     C                   CLEAR                   TRUL33DS
031100041201     C                   Z-ADD     103           I33CNU
031200041201     C                   Z-ADD     102           I33PO1
031300041201     C                   Z-ADD     1             I33NUM
031400041201     c                   movel(p)  trul33ds      kpjbu
031500041201     c                   call      'TRUL33R'
031600041201     c                   parm                    kpjba
031700041201     c                   movel     kpjbu         trul33ds
031800041201     c
031900030508     C                   SELECT
032000041201     c                   when      o33err=0
032100041201     c                   leave
032200041201     c
032300041201     c                   when      o33err<>2
032400041201     C                   EVAL      SndPM03 = 'ERRORE '
032500041201     C                             + o33msg
032600041201     C                   EXSR      CalQMHSNDPM
032700041201     C                   EVAL      PrmEsi = Errore
032800041201     C                   EXSR      Uscita
032900041201     c
033000041201     c
033100041201     C**                 WHEN      NOT %ERROR
033200041201     C**                 LEAVE
033300041201     C**                 WHEN      %ERROR AND %STATUS <> 1218
033400041201     C**                 EVAL      SndPM03 = ProcName + ' ERRORE'
033500041201     C**                           + ' Lettura FLNUF01L non riuscita.'
033600041201     C**                 EXSR      CalQMHSNDPM
033700041201     C**                 EVAL      PrmEsi = Errore
033800041201     C**                 EXSR      Uscita
033900030508     C                   ENDSL
034000030508     C                   ENDDO
034100030508
034200041201     ** Aggiorno il numeratore. Se non esiste lo creo.
034300041201     C**                 IF        %FOUND
034400041201     C**                 EVAL      NufNum = NufNum + 1
034500041201     C**                 EVAL      NufDat = UDATE
034600041201     C**                 UPDATE    FlNuf
034700041201     C**                 ELSE
034800041201     C**   *NOKEY        CLEAR                   FlNuf
034900041201     C**                 EVAL      NufNum = 1
035000041201     C**                 EVAL      NufDat = UDATE
035100041201     C**                 WRITE     FlNuf
035200041201     C**                 ENDIF
035300030508
035400041201     C**                 EVAL      TasNSp = NufNum
035500041201     c                   if        o33err=0
035600041201     C                   Z-ADD     O33Nrf        tasnsp
035700041201     c                   endif
035800030508
035900030508     C                   ENDSR
036000030507
036100030508     ***********************************************************************
036200030508     **
036300030508     ** Send Program Message (QMHSNDPM) API
036400030508     **
036500030508     ***********************************************************************
036600030508     C     CalQMHSNDPM   BEGSR
036700030508
036800030508     C                   CLEAR                   QUSEC
036900030508     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
037000030508
037100030508     C                   CALL      'QMHSNDPM'
037200030508     C                   PARM                    SndPM01
037300030508     C                   PARM                    SndPM02
037400030508     C                   PARM                    SndPM03
037500030508     C                   PARM                    SndPM04
037600030508     C                   PARM                    SndPM05
037700030508     C                   PARM                    SndPM06
037800030508     C                   PARM                    SndPM07
037900030508     C                   PARM                    SndPM08
038000030508     C                   PARM                    QUSEC
038100030508
038200030508     C                   ENDSR
038300030508
038400030507     ***********************************************************************
038500030507     **
038600030507     **
038700030507     **
038800030507     ***********************************************************************
038900030507     C     Uscita        BEGSR
039000041201     C*
039100041201     c                   clear                   trul33ds
039200041201     C                   MOVEL     'C'           I33TLA
039300041201     c                   movel(p)  trul33ds      kpjbu
039400041201     C                   CALL      'TRUL33R'
039500041201     C                   PARM                    KPJBA
039600030507
039700030508     C                   IF        PrmEsi = *BLANK
039800030508     C                   EVAL      PrmEsi = Eseguito
039900030508     C                   ENDIF
040000030508
040100030508     C                   EVAL      *INLR = (PrmTLa <> *BLANK)
040200030507     C                   RETURN
040300030507
040400030507     C                   ENDSR
