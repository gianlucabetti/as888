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
003700030507
003800030507     D Insoluti        C                   'IN'
003900030507     D Eseguito        C                   '0'
004000030507     D Errore          C                   '1'
004100030508     D Chiudere        C                   'C'
004200030526     D Cliente         C                   'C'
004300030508
004400030508     D                SDS
004500030508     D ProcName          *PROC
004600030508
004700030508     D InfTiTas        DS
004800030508     D  RRNTiTas             397    400I 0
004900030508
005000030508     D NullPtr         S               *
005100030508     D CnDizion      E DS
005200030508     D                                     BASED(NullPtr)
005300030508     D                                     PREFIX(Diz)
005400030507
005500030508     D PrmFnz          S              2                                         Funzione chiamante
005600030508     D PrmKsc          S                                                        Codice cliente
005700030508     D                                     LIKE(Diz$CSC)
005800030508     D PrmRMN          S                                                        Rif. mittente num.
005900030508     D                                     LIKE(Diz$RMN)
006000030508     D PrmCmt          S              1                                         Sincronia file
006100030508     D PrmTLa          S              1                                         Tipo lancio
006200030508     D PrmEsi          S              1                                         Esito
006300030516     D PrmAAS          S
006400030516     D                                     LIKE(Diz$AAS)
006500030516     D PrmLnP          S
006600030516     D                                     LIKE(Diz$LnP)
006700030516     D PrmNrS          S
006800030516     D                                     LIKE(Diz$NrS)
006900030516     D PrmNSp          S
007000030516     D                                     LIKE(Diz$NSp)
007100030508
007200041201     D KPJBA         E DS
007300041201     d trul33ds      e ds
007400160122     d dta4a         e ds
007500030508     D TIBS69DS      E DS
007600030508     D                                     INZ
007700030508     D CnAco00F      E DS
007800030508     D                                     INZ
007900030508     D CnInd00F      E DS
008000030508     D                                     INZ
008100030508     D CnClp00F      E DS
008200030508     D                                     INZ
008300030508     D FnCls00F      E DS
008400030508     D                                     INZ
008500030508
008600030508     D/COPY QSYSINC/QRPGLESRC,QUSEC
008700030508
008800030508     D SndPM01         S              7                                         Message id
008900030508     D                                     INZ('CPF9897')
009000030508     D SndPM02         S             20                                         Message file name
009100030508     D                                     INZ('QCPFMSG   *LIBL     ')
009200030508     D SndPM03         S            512                                         Immediate text
009300030508     D SndPM04         S             10I 0                                      Length of text
009400030508     D                                     INZ(%SIZE(SndPM03))                  Length of text
009500030508     D SndPM05         S             10                                         Message type
009600030508     D                                     INZ('*INFO')
009700030508     D SndPM06         S             16                                         Call stack entry
009800030508     D                                     INZ('*')
009900030508     D SndPM07         S             10I 0                                      Call stack counter
010000030508     D SndPM08         S              4                                         Call stack counter
010100030507
010200030507     C     *ENTRY        PLIST
010300030508     C                   PARM                    PrmFnz
010400030508     C                   PARM                    PrmKsc
010500030508     C                   PARM                    PrmRMN
010600030508     C                   PARM                    PrmCmt
010700030508     C                   PARM                    PrmTLa
010800030508     C                   PARM                    PrmEsi
010900030516     C                   PARM                    PrmAAS
011000030516     C                   PARM                    PrmLnP
011100030516     C                   PARM                    PrmNrS
011200030516     C                   PARM                    PrmNSp
011300030507
011400030507     C                   EXSR      ChkPrm
011500030507     C                   EXSR      WrtTiTas
011600030507     C                   EXSR      Uscita
011700030507
011800030507     ***********************************************************************
011900030507     **
012000030507     ** Controllo parametri ricevuti.
012100030507     **
012200030507     ***********************************************************************
012300030507     C     ChkPrm        BEGSR
012400030507
012500030508     C                   CLEAR                   PrmEsi
012600030516     C                   CLEAR                   PrmAAS
012700030516     C                   CLEAR                   PrmLnP
012800030516     C                   CLEAR                   PrmNrS
012900030516     C                   CLEAR                   PrmNSp
013000030508
013100030508     C                   IF        PrmTLa = Chiudere
013200030508     C                   EXSR      Uscita
013300030508     C                   ENDIF
013400030507
013500030508     C                   SELECT
013600030508
013700030508     C                   WHEN      PrmFnz = Insoluti
013800030508
013900030508     C                   IF        PrmKsc <= 0
014000030507     C                             OR
014100030508     C                             PrmRMN <= 0
014200030508     C                   EVAL      SndPM03 = ProcName + ' ERRORE'
014300030508     C                             + ' Parametri mancanti o non validi.'
014400030508     C                   EXSR      CalQMHSNDPM
014500030508     C                   EVAL      PrmEsi = Errore
014600030507     C                   EXSR      Uscita
014700030507     C                   ENDIF
014800030508
014900030508     C                   OTHER
015000030508
015100030508     C                   EVAL      SndPM03 = ProcName + ' ERRORE'
015200030508     C                             + ' Funzione chiamante sconosciuta.'
015300030508     C                   EXSR      CalQMHSNDPM
015400030508     C                   EVAL      PrmEsi = Errore
015500030508     C                   EXSR      Uscita
015600030508
015700030508     C                   ENDSL
015800030509
015900030509     ** Scarto i clienti non codificati.
016000030509     C                   IF        %SUBST(%EDITC(PrmKsc:'X'):4:4) = '8888'
016100030509     C                             OR
016200030509     C                             %SUBST(%EDITC(PrmKsc:'X'):4:4) = '9999'
016300030509     C                   EXSR      Uscita
016400030509     C                   ENDIF
016500030507
016600030507     C                   ENDSR
016700030507
016800030507     ***********************************************************************
016900030507     **
017000030507     ** Scrittura TITAS00F.
017100030507     **
017200030507     ***********************************************************************
017300030507     C     WrtTiTas      BEGSR
017400030507
017500030508     C                   EXSR      SetTiTas
017600030516
017700030508     C                   WRITE     TITAS000
017800030516
017900030516     C                   EVAL      PrmAAS = TasAAS
018000030516     C                   EVAL      PrmLnP = TasLnP
018100030516     C                   EVAL      PrmNrS = TasNrS
018200030516     C                   EVAL      PrmNSp = TasNSp
018300030507
018400030507     C                   ENDSR
018500030508
018600030508     ***********************************************************************
018700030508     **
018800030508     ** Impostaziione TITAS00F.
018900030508     **
019000030508     ***********************************************************************
019100030508     C     SetTiTas      BEGSR
019200030508
019300030508     C                   CLEAR                   TiTas000
019400030508
019500030508     C                   SELECT
019600030508
019700030508     C                   WHEN      PrmFnz = Insoluti
019800030526
019900030526     C                   CLEAR                   TIBS69DS
020000030526     C                   EVAL      I69KAc = PrmKsc
020100030526     C                   EVAL      I69KIn = PrmKsc
020200030526     C                   CLEAR                   CnAco00F
020300030526     C                   CLEAR                   CnInd00F
020400030526
020500030526     C                   CALL      'TIBS69R'
020600030526     C                   PARM                    TIBS69DS
020700030526     C                   PARM                    CnAco00F
020800030526     C                   PARM                    CnInd00F
020900030526     C                   PARM                    CnClp00F
021000030526     C                   PARM                    FnCls00F
021100030526
021200030526     C                   IF        O69Err = Errore
021300030526     C                   EVAL      SndPM03 = ProcName + ' ERRORE ' + O69Msg
021400030526     C                   EXSR      CalQMHSNDPM
021500030526     C                   EVAL      PrmEsi = Errore
021600030526     C                   EXSR      Uscita
021700030526     C                   ENDIF
021800030526
021900030508     C                   EVAL      TasAAS = *YEAR
022000030508     C                   EVAL      TasLnP = 102
022100070108      * impostiamo la serie per evitare errore di record duplicato su TITAS tra queste
022200070108      * bolle e le bolle generate per il pod image
022300070108     C**!!!              EVAL      TasNrS = 0
022400070108     c                   eval      tasnrs = 3
022500030508     C                   EXSR      SetTasNSp
022600030526     C                   EVAL      TasTBl = 'A3'
022700030526     C                   EVAL      TasCBo = 'FM'
022800030508     C                   EVAL      TasMGS = (*MONTH * 100) + *DAY
022900030508     C                   EVAL      TasKsc = PrmKsc
023000030609     C                   MOVEL     PrmKsc        TasLnA
023100030508     C                   EVAL      TasNCl = 1
023200030508     C                   EVAL      TasPKB = 0,1
023300030508     C                   EVAL      TasFPB = 'R'
023400030508     C                   EVAL      TasPKF = 0,1
023500030508     C                   EVAL      TasFPF = 'R'
023600030508     C                   EVAL      TasFVB = 'R'
023700030508     C                   EVAL      TasVLB = 0,001
023800030508     C                   EVAL      TasVLF = 0,001
023900030508     C                   EVAL      TasFVF = 'R'
024000030508     C                   EVAL      TasTSP = 'C'
024100030526     C                   EVAL      TasRSD = AcoRag
024200030526     C                   EVAL      TasInD = IndVia
024300030526     C                   EVAL      TasCaD = %EDITC(IndCAP:'X')
024400030526     C                   EVAL      TasLoD = IndCit
024500030526     C                   EVAL      TasPrD = IndPrv
024600160929     C                   EVAL      Tasfap = 'C'
024700030508     C                   EVAL      TasCTS = '40'
024800030516     C                   EVAL      TasFIn = 'C'
024900030526     C                   EVAL      TasCCM = 1020005
025000030508     C                   EVAL      TasRMN = PrmRMN
025100030508     C                   EVAL      TasMCT = '40'
025200160412     C*******            EVAL      TasNAS = 'SPESE INSOLUTO'
025300030508     C                   EVAL      TasFlB = 102
025400030508     C                   EVAL      TasTFA = 1
025500030508     C                   EVAL      TasTFP = 1
025600030508     C                   EVAL      TasDIm = *DATE
025700030526     C                   EVAL      TasSOp = '*'
025800030508     C                   EVAL      TasPdr = 1020999
025900030508     C                   EVAL      TasDRt = *DATE
026000030508     C                   EVAL      TasFPP = 'P'
026100030526     C                   EVAL      TasCTM = '4F'
026200030508     C                   EVAL      TasDBr = *DATE
026300030508     C                   EVAL      TasFDN = 'S'
026400030508     C                   EVAL      TasLL1 = 'Y'
026500030526     C                   EVAL      TasCaM = %EDITC(IndCAP:'X')
026600030508     C                   EVAL      TasDiv = 'EUR'
026700030508     C                   EVAL      TasSV1 = 'M'
026800030508     C                   EVAL      TasVa1 = 28,41
026900030508     C                   EVAL      TasIMV = 28,41
027000030508     C                   EVAL      TasSCL = 'B'
027100160122     c* scrittura natura merce
027200160122     c                   clear                   tita4000
027300160122     c                   clear                   dta4a
027400160122     c                   Eval      Ta4Aas = TasAas
027500160122     c                   Eval      Ta4Lnp = TasLnp
027600160122     c                   Eval      Ta4Nrs = TasNrs
027700160122     c                   Eval      Ta4Nsp = TasNsp
027800160122      * Tipo record
027900160122     c                   Eval      Ta4Trc = 'A'
028000160122     C                   EVAL      §ta4anas = 'SPESE INSOLUTO'
028100160122     c                   Eval      Ta4Not = dta4a
028200160122     c                   Write     Tita4000
028300030508
028400030508     C                   ENDSL
028500030508
028600030508     C                   ENDSR
028700030508
028800030508     ***********************************************************************
028900030508     **
029000030508     ** Impostazione numero spedizione.
029100030508     **
029200030508     ***********************************************************************
029300030508     C     SetTasNSp     BEGSR
029400030508
029500030508     ** Reperisco l'ultimo numero spedizione usato.
029600041201     C                   DO        *HIVAL
029700041201     C**   K04Nuf01      CHAIN(E)  FlNuf01L
029800041201     c* lo prendo da AZNUM con trul33r
029900041201     C                   CLEAR                   TRUL33DS
030000041201     C                   Z-ADD     103           I33CNU
030100041201     C                   Z-ADD     102           I33PO1
030200041201     C                   Z-ADD     1             I33NUM
030300041201     c                   movel(p)  trul33ds      kpjbu
030400041201     c                   call      'TRUL33R'
030500041201     c                   parm                    kpjba
030600041201     c                   movel     kpjbu         trul33ds
030700041201     c
030800030508     C                   SELECT
030900041201     c                   when      o33err=0
031000041201     c                   leave
031100041201     c
031200041201     c                   when      o33err<>2
031300041201     C                   EVAL      SndPM03 = 'ERRORE '
031400041201     C                             + o33msg
031500041201     C                   EXSR      CalQMHSNDPM
031600041201     C                   EVAL      PrmEsi = Errore
031700041201     C                   EXSR      Uscita
031800041201     c
031900041201     c
032000030508     C                   ENDSL
032100030508     C                   ENDDO
032200030508
032300041201     ** Aggiorno il numeratore. Se non esiste lo creo.
032400030508
032500041201     c                   if        o33err=0
032600041201     C                   Z-ADD     O33Nrf        tasnsp
032700041201     c                   endif
032800030508
032900030508     C                   ENDSR
033000030507
033100030508     ***********************************************************************
033200030508     **
033300030508     ** Send Program Message (QMHSNDPM) API
033400030508     **
033500030508     ***********************************************************************
033600030508     C     CalQMHSNDPM   BEGSR
033700030508
033800030508     C                   CLEAR                   QUSEC
033900030508     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
034000030508
034100030508     C                   CALL      'QMHSNDPM'
034200030508     C                   PARM                    SndPM01
034300030508     C                   PARM                    SndPM02
034400030508     C                   PARM                    SndPM03
034500030508     C                   PARM                    SndPM04
034600030508     C                   PARM                    SndPM05
034700030508     C                   PARM                    SndPM06
034800030508     C                   PARM                    SndPM07
034900030508     C                   PARM                    SndPM08
035000030508     C                   PARM                    QUSEC
035100030508
035200030508     C                   ENDSR
035300030508
035400030507     ***********************************************************************
035500030507     **
035600030507     **
035700030507     **
035800030507     ***********************************************************************
035900030507     C     Uscita        BEGSR
036000041201     C*
036100041201     c                   clear                   trul33ds
036200041201     C                   MOVEL     'C'           I33TLA
036300041201     c                   movel(p)  trul33ds      kpjbu
036400041201     C                   CALL      'TRUL33R'
036500041201     C                   PARM                    KPJBA
036600030507
036700030508     C                   IF        PrmEsi = *BLANK
036800030508     C                   EVAL      PrmEsi = Eseguito
036900030508     C                   ENDIF
037000030508
037100030508     C                   EVAL      *INLR = (PrmTLa <> *BLANK)
037200030507     C                   RETURN
037300030507
037400030507     C                   ENDSR
