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
003400160122     FTiTa400F  O    E             DISK
003500030508     F                                     COMMIT(PrmCmt)
003600030508     F                                     INFDS(InfTiTas)
003700041201     F***FlNuf01L  UF A E           K DISK
003800041201     F***                                  COMMIT(PrmCmt)
003900030507
004000030507     D Insoluti        C                   'IN'
004100030507     D Eseguito        C                   '0'
004200030507     D Errore          C                   '1'
004300030508     D Chiudere        C                   'C'
004400030526     D Cliente         C                   'C'
004500030508
004600030508     D                SDS
004700030508     D ProcName          *PROC
004800030508
004900030508     D InfTiTas        DS
005000030508     D  RRNTiTas             397    400I 0
005100030508
005200030508     D NullPtr         S               *
005300030508     D CnDizion      E DS
005400030508     D                                     BASED(NullPtr)
005500030508     D                                     PREFIX(Diz)
005600030507
005700030508     D PrmFnz          S              2                                         Funzione chiamante
005800030508     D PrmKsc          S                                                        Codice cliente
005900030508     D                                     LIKE(Diz$CSC)
006000030508     D PrmRMN          S                                                        Rif. mittente num.
006100030508     D                                     LIKE(Diz$RMN)
006200030508     D PrmCmt          S              1                                         Sincronia file
006300030508     D PrmTLa          S              1                                         Tipo lancio
006400030508     D PrmEsi          S              1                                         Esito
006500030516     D PrmAAS          S
006600030516     D                                     LIKE(Diz$AAS)
006700030516     D PrmLnP          S
006800030516     D                                     LIKE(Diz$LnP)
006900030516     D PrmNrS          S
007000030516     D                                     LIKE(Diz$NrS)
007100030516     D PrmNSp          S
007200030516     D                                     LIKE(Diz$NSp)
007300030508
007400041201     D KPJBA         E DS
007500041201     d trul33ds      e ds
007600160122     d dta4a         e ds
007700030508     D UT§DSE0F      E DS
007800030508     D                                     INZ
007900030508     D CNCR40        E DS
008000030508     D                                     INZ
008100030508     D TIBS69DS      E DS
008200030508     D                                     INZ
008300030508     D CnAco00F      E DS
008400030508     D                                     INZ
008500030508     D CnInd00F      E DS
008600030508     D                                     INZ
008700030508     D CnClp00F      E DS
008800030508     D                                     INZ
008900030508     D FnCls00F      E DS
009000030508     D                                     INZ
009100030508
009200030508     D/COPY QSYSINC/QRPGLESRC,QUSEC
009300030508
009400030508     D SndPM01         S              7                                         Message id
009500030508     D                                     INZ('CPF9897')
009600030508     D SndPM02         S             20                                         Message file name
009700030508     D                                     INZ('QCPFMSG   *LIBL     ')
009800030508     D SndPM03         S            512                                         Immediate text
009900030508     D SndPM04         S             10I 0                                      Length of text
010000030508     D                                     INZ(%SIZE(SndPM03))                  Length of text
010100030508     D SndPM05         S             10                                         Message type
010200030508     D                                     INZ('*INFO')
010300030508     D SndPM06         S             16                                         Call stack entry
010400030508     D                                     INZ('*')
010500030508     D SndPM07         S             10I 0                                      Call stack counter
010600030508     D SndPM08         S              4                                         Call stack counter
010700030507
010800030507     C     *ENTRY        PLIST
010900030508     C                   PARM                    PrmFnz
011000030508     C                   PARM                    PrmKsc
011100030508     C                   PARM                    PrmRMN
011200030508     C                   PARM                    PrmCmt
011300030508     C                   PARM                    PrmTLa
011400030508     C                   PARM                    PrmEsi
011500030516     C                   PARM                    PrmAAS
011600030516     C                   PARM                    PrmLnP
011700030516     C                   PARM                    PrmNrS
011800030516     C                   PARM                    PrmNSp
011900030507
012000030507     C                   EXSR      ChkPrm
012100030507     C                   EXSR      WrtTiTas
012200030507     C                   EXSR      Uscita
012300030507
012400030507     ***********************************************************************
012500030507     **
012600030507     ** Operazioni iniziali.
012700030507     **
012800030507     ***********************************************************************
012900030507     C     *INZSR        BEGSR
013000030507
013100041201     C**   K04Nuf01      KLIST
013200041201     C**                 KFLD                    NufAAA
013300041201     C**                 KFLD                    NufCNu
013400041201     C**                 KFLD                    NufFil
013500041201     C**                 KFLD                    NufFl2
013600030508
013700041201     C**                 EVAL      NufAAA = UYEAR
013800041201     C**                 EVAL      NufCNu = 103
013900041201     C**                 EVAL      NufFil = 102
014000030508
014100030526     C*                  CALL      'X§PARUT'
014200030526     C*                  PARM                    UT§DSE0F
014300030526     C*                  EVAL      CNCR40 = Rec40
014400030507
014500030507     C                   ENDSR
014600030507
014700030507     ***********************************************************************
014800030507     **
014900030507     ** Controllo parametri ricevuti.
015000030507     **
015100030507     ***********************************************************************
015200030507     C     ChkPrm        BEGSR
015300030507
015400030508     C                   CLEAR                   PrmEsi
015500030516     C                   CLEAR                   PrmAAS
015600030516     C                   CLEAR                   PrmLnP
015700030516     C                   CLEAR                   PrmNrS
015800030516     C                   CLEAR                   PrmNSp
015900030508
016000030508     C                   IF        PrmTLa = Chiudere
016100030508     C                   EXSR      Uscita
016200030508     C                   ENDIF
016300030507
016400030508     C                   SELECT
016500030508
016600030508     C                   WHEN      PrmFnz = Insoluti
016700030508
016800030508     C                   IF        PrmKsc <= 0
016900030507     C                             OR
017000030508     C                             PrmRMN <= 0
017100030508     C                   EVAL      SndPM03 = ProcName + ' ERRORE'
017200030508     C                             + ' Parametri mancanti o non validi.'
017300030508     C                   EXSR      CalQMHSNDPM
017400030508     C                   EVAL      PrmEsi = Errore
017500030507     C                   EXSR      Uscita
017600030507     C                   ENDIF
017700030508
017800030508     C                   OTHER
017900030508
018000030508     C                   EVAL      SndPM03 = ProcName + ' ERRORE'
018100030508     C                             + ' Funzione chiamante sconosciuta.'
018200030508     C                   EXSR      CalQMHSNDPM
018300030508     C                   EVAL      PrmEsi = Errore
018400030508     C                   EXSR      Uscita
018500030508
018600030508     C                   ENDSL
018700030509
018800030509     ** Scarto i clienti non codificati.
018900030509     C                   IF        %SUBST(%EDITC(PrmKsc:'X'):4:4) = '8888'
019000030509     C                             OR
019100030509     C                             %SUBST(%EDITC(PrmKsc:'X'):4:4) = '9999'
019200030509     C                   EXSR      Uscita
019300030509     C                   ENDIF
019400030507
019500030507     C                   ENDSR
019600030507
019700030507     ***********************************************************************
019800030507     **
019900030507     ** Scrittura TITAS00F.
020000030507     **
020100030507     ***********************************************************************
020200030507     C     WrtTiTas      BEGSR
020300030507
020400030508     C                   EXSR      SetTiTas
020500030516
020600030508     C                   WRITE     TITAS000
020700030516
020800030516     C                   EVAL      PrmAAS = TasAAS
020900030516     C                   EVAL      PrmLnP = TasLnP
021000030516     C                   EVAL      PrmNrS = TasNrS
021100030516     C                   EVAL      PrmNSp = TasNSp
021200030507
021300030507     C                   ENDSR
021400030508
021500030508     ***********************************************************************
021600030508     **
021700030508     ** Impostaziione TITAS00F.
021800030508     **
021900030508     ***********************************************************************
022000030508     C     SetTiTas      BEGSR
022100030508
022200030508     C                   CLEAR                   TiTas000
022300030508
022400030508     C                   SELECT
022500030508
022600030508     C                   WHEN      PrmFnz = Insoluti
022700030526
022800030526     C                   CLEAR                   TIBS69DS
022900030526     C                   EVAL      I69KAc = PrmKsc
023000030526     C                   EVAL      I69KIn = PrmKsc
023100030526     C                   CLEAR                   CnAco00F
023200030526     C                   CLEAR                   CnInd00F
023300030526
023400030526     C                   CALL      'TIBS69R'
023500030526     C                   PARM                    TIBS69DS
023600030526     C                   PARM                    CnAco00F
023700030526     C                   PARM                    CnInd00F
023800030526     C                   PARM                    CnClp00F
023900030526     C                   PARM                    FnCls00F
024000030526
024100030526     C                   IF        O69Err = Errore
024200030526     C                   EVAL      SndPM03 = ProcName + ' ERRORE ' + O69Msg
024300030526     C                   EXSR      CalQMHSNDPM
024400030526     C                   EVAL      PrmEsi = Errore
024500030526     C                   EXSR      Uscita
024600030526     C                   ENDIF
024700030526
024800030508     C                   EVAL      TasAAS = *YEAR
024900030508     C                   EVAL      TasLnP = 102
025000070108      * impostiamo la serie per evitare errore di record duplicato su TITAS tra queste
025100070108      * bolle e le bolle generate per il pod image
025200070108     C**!!!              EVAL      TasNrS = 0
025300070108     c                   eval      tasnrs = 3
025400030508     C                   EXSR      SetTasNSp
025500030526     C                   EVAL      TasTBl = 'A3'
025600030526     C                   EVAL      TasCBo = 'FM'
025700030508     C                   EVAL      TasMGS = (*MONTH * 100) + *DAY
025800030508     C                   EVAL      TasKsc = PrmKsc
025900030609     C                   MOVEL     PrmKsc        TasLnA
026000030508     C                   EVAL      TasNCl = 1
026100030508     C                   EVAL      TasPKB = 0,1
026200030508     C                   EVAL      TasFPB = 'R'
026300030508     C                   EVAL      TasPKF = 0,1
026400030508     C                   EVAL      TasFPF = 'R'
026500030508     C                   EVAL      TasFVB = 'R'
026600030508     C                   EVAL      TasVLB = 0,001
026700030508     C                   EVAL      TasVLF = 0,001
026800030508     C                   EVAL      TasFVF = 'R'
026900030508     C                   EVAL      TasTSP = 'C'
027000030526     C                   EVAL      TasRSD = AcoRag
027100030526     C                   EVAL      TasInD = IndVia
027200030526     C                   EVAL      TasCaD = %EDITC(IndCAP:'X')
027300030526     C                   EVAL      TasLoD = IndCit
027400030526     C                   EVAL      TasPrD = IndPrv
027500030508     C                   EVAL      TasCTS = '40'
027600030516     C                   EVAL      TasFIn = 'C'
027700030526     C                   EVAL      TasCCM = 1020005
027800030508     C                   EVAL      TasRMN = PrmRMN
027900030508     C                   EVAL      TasMCT = '40'
028000160412     C*******            EVAL      TasNAS = 'SPESE INSOLUTO'
028100030508     C                   EVAL      TasFlB = 102
028200030508     C                   EVAL      TasTFA = 1
028300030508     C                   EVAL      TasTFP = 1
028400030508     C                   EVAL      TasDIm = *DATE
028500030526     C                   EVAL      TasSOp = '*'
028600030508     C                   EVAL      TasPdr = 1020999
028700030508     C                   EVAL      TasDRt = *DATE
028800030508     C                   EVAL      TasFPP = 'P'
028900030526     C                   EVAL      TasCTM = '4F'
029000030508     C                   EVAL      TasDBr = *DATE
029100030508     C                   EVAL      TasFDN = 'S'
029200030508     C                   EVAL      TasLL1 = 'Y'
029300030526     C                   EVAL      TasCaM = %EDITC(IndCAP:'X')
029400030508     C                   EVAL      TasDiv = 'EUR'
029500030508     C                   EVAL      TasSV1 = 'M'
029600030508     C                   EVAL      TasVa1 = 28,41
029700030508     C                   EVAL      TasIMV = 28,41
029800030508     C                   EVAL      TasSCL = 'B'
029900160122     c* scrittura natura merce
030000160122     c                   clear                   tita4000
030100160122     c                   clear                   dta4a
030200160122     c                   Eval      Ta4Aas = TasAas
030300160122     c                   Eval      Ta4Lnp = TasLnp
030400160122     c                   Eval      Ta4Nrs = TasNrs
030500160122     c                   Eval      Ta4Nsp = TasNsp
030600160122      * Tipo record
030700160122     c                   Eval      Ta4Trc = 'A'
030800160122     C                   EVAL      §ta4anas = 'SPESE INSOLUTO'
030900160122     c                   Eval      Ta4Not = dta4a
031000160122     c                   Write     Tita4000
031100030508
031200030508     C                   ENDSL
031300030508
031400030508     C                   ENDSR
031500030508
031600030508     ***********************************************************************
031700030508     **
031800030508     ** Impostazione numero spedizione.
031900030508     **
032000030508     ***********************************************************************
032100030508     C     SetTasNSp     BEGSR
032200030508
032300030508     ** Reperisco l'ultimo numero spedizione usato.
032400041201     C                   DO        *HIVAL
032500041201     C**   K04Nuf01      CHAIN(E)  FlNuf01L
032600041201     c* lo prendo da AZNUM con trul33r
032700041201     C                   CLEAR                   TRUL33DS
032800041201     C                   Z-ADD     103           I33CNU
032900041201     C                   Z-ADD     102           I33PO1
033000041201     C                   Z-ADD     1             I33NUM
033100041201     c                   movel(p)  trul33ds      kpjbu
033200041201     c                   call      'TRUL33R'
033300041201     c                   parm                    kpjba
033400041201     c                   movel     kpjbu         trul33ds
033500041201     c
033600030508     C                   SELECT
033700041201     c                   when      o33err=0
033800041201     c                   leave
033900041201     c
034000041201     c                   when      o33err<>2
034100041201     C                   EVAL      SndPM03 = 'ERRORE '
034200041201     C                             + o33msg
034300041201     C                   EXSR      CalQMHSNDPM
034400041201     C                   EVAL      PrmEsi = Errore
034500041201     C                   EXSR      Uscita
034600041201     c
034700041201     c
034800041201     C**                 WHEN      NOT %ERROR
034900041201     C**                 LEAVE
035000041201     C**                 WHEN      %ERROR AND %STATUS <> 1218
035100041201     C**                 EVAL      SndPM03 = ProcName + ' ERRORE'
035200041201     C**                           + ' Lettura FLNUF01L non riuscita.'
035300041201     C**                 EXSR      CalQMHSNDPM
035400041201     C**                 EVAL      PrmEsi = Errore
035500041201     C**                 EXSR      Uscita
035600030508     C                   ENDSL
035700030508     C                   ENDDO
035800030508
035900041201     ** Aggiorno il numeratore. Se non esiste lo creo.
036000041201     C**                 IF        %FOUND
036100041201     C**                 EVAL      NufNum = NufNum + 1
036200041201     C**                 EVAL      NufDat = UDATE
036300041201     C**                 UPDATE    FlNuf
036400041201     C**                 ELSE
036500041201     C**   *NOKEY        CLEAR                   FlNuf
036600041201     C**                 EVAL      NufNum = 1
036700041201     C**                 EVAL      NufDat = UDATE
036800041201     C**                 WRITE     FlNuf
036900041201     C**                 ENDIF
037000030508
037100041201     C**                 EVAL      TasNSp = NufNum
037200041201     c                   if        o33err=0
037300041201     C                   Z-ADD     O33Nrf        tasnsp
037400041201     c                   endif
037500030508
037600030508     C                   ENDSR
037700030507
037800030508     ***********************************************************************
037900030508     **
038000030508     ** Send Program Message (QMHSNDPM) API
038100030508     **
038200030508     ***********************************************************************
038300030508     C     CalQMHSNDPM   BEGSR
038400030508
038500030508     C                   CLEAR                   QUSEC
038600030508     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
038700030508
038800030508     C                   CALL      'QMHSNDPM'
038900030508     C                   PARM                    SndPM01
039000030508     C                   PARM                    SndPM02
039100030508     C                   PARM                    SndPM03
039200030508     C                   PARM                    SndPM04
039300030508     C                   PARM                    SndPM05
039400030508     C                   PARM                    SndPM06
039500030508     C                   PARM                    SndPM07
039600030508     C                   PARM                    SndPM08
039700030508     C                   PARM                    QUSEC
039800030508
039900030508     C                   ENDSR
040000030508
040100030507     ***********************************************************************
040200030507     **
040300030507     **
040400030507     **
040500030507     ***********************************************************************
040600030507     C     Uscita        BEGSR
040700041201     C*
040800041201     c                   clear                   trul33ds
040900041201     C                   MOVEL     'C'           I33TLA
041000041201     c                   movel(p)  trul33ds      kpjbu
041100041201     C                   CALL      'TRUL33R'
041200041201     C                   PARM                    KPJBA
041300030507
041400030508     C                   IF        PrmEsi = *BLANK
041500030508     C                   EVAL      PrmEsi = Eseguito
041600030508     C                   ENDIF
041700030508
041800030508     C                   EVAL      *INLR = (PrmTLa <> *BLANK)
041900030507     C                   RETURN
042000030507
042100030507     C                   ENDSR
