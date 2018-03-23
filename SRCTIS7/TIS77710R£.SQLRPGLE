000100050307     ***********************************************************************************************
000200050307     **?
000300050310     **?Questo programma reperisce e restituisce la storia della giacenza di una spedizione.
000400050307     **?
000500050307     ***********************************************************************************************
000600060623     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('TIS':'TIBS') DECEDIT(*JOBRUN)
000601160121     H OPTION(*NOUNREF:*NODEBUGIO:*SRCSTMT)
000700050308
000800060623     F*TNTBE01L  IF   E           K DISK
000900060623     F*                                    USROPN
001000060623     F*TABEL00F  IF   E           K DISK
001100060623     F*                                    USROPN
001200050307
001201160121      /COPY GAITRASRC/SRCCONST,TIS778R
001300050307     D Elementi        C                   25
001400050308     D Eseguito        C                   '0'
001500060608
001501160121      /COPY GAITRASRC/SRCPROTOPR,TIS778R
001600060613     D rtvMsgLang      PR          3512A                                        Messaggio in lingua
001700060613     D  msgId                         7A   CONST
001800060613     D  piLinguaISO2                  2A   OPTIONS(*OMIT:*NOPASS)
001900060613     D  piMsgDta                    512A   OPTIONS(*OMIT:*NOPASS:*VARSIZE) CONST
002000060613     D  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
002100060613     D                                     VARYING
002200060613     D  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
002300060613     D                                     VARYING
002400060613     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
002500060613     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
002600060623     D inzLingue       PR            10I 0                                      Inizializza lingue
002700060623     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
002800060623     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
002900060623     D  rpyElementi                  10I 0 OPTIONS(*NOPASS:*OMIT)
003000060623     D cvtLinguaISO2ToTabel...                                                  Conversione lingua
003100060623     D                 PR                  LIKE(linTabel)
003200060623     D  rqsISO2                            LIKE(linISO2)
003300060623     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
003400060623     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
003500060623     D  rpyTabel                           LIKE(linTabel)
003600060623     D                                     OPTIONS(*NOPASS:*OMIT)
003700060623     D cvtLinguaISO2ToTntbe...                                                  Conversione lingua
003800060623     D                 PR                  LIKE(linTntbe)
003900060623     D  rqsISO2                            LIKE(linISO2)
004000060623     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
004100060623     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
004200060623     D  rpyTntbe                           LIKE(linTntbe)
004300060623     D                                     OPTIONS(*NOPASS:*OMIT)
004400060623     D openTabel00f    PR
004500060623     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
004600060623     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
004700060626     D chainTabel00f   PR           109A
004800060626     D  rqsOpCode                    10A   CONST
004900060626     D  rqsKut                        1P 0
005000060626     D  rqsCod                        2A   CONST
005100060626     D  rqsKey                        8A   OPTIONS(*VARSIZE) CONST
005200060626     D  rqsLengthOfKey...
005300060626     D                               10I 0 CONST
005400060626     D  rqsFormat                    10A   CONST
005500060626     D  rpyOpCode                    10A
005600060626     D  rpyEsito                     10I 0
005700060626     D  rpyData                     109A   OPTIONS(*NOPASS:*OMIT)
005800060626     D tibs02r         PR                  EXTPGM('TIBS02R')                    Lettura TNTBE00F
005900060623     D  kpjba                              LIKEDS(kpjba)
006000060623     D  tibs02ds                           LIKEDS(tibs02ds)
006100060623
006200050311     D PSDS           SDS
006300050311     D  ProcName         *PROC
006400050311     D                         1    333
006500050307     D TIS177DSI     E DS
006600050307     D                                     INZ
006700050307     D                                     PREFIX(Rqs)
006800050309     D  RqsLnP                        3S 0
006900050309     D                                     OVERLAY(RqsNSpediz:1)
007000050309     D  RqsNrS                        2S 0
007100050309     D                                     OVERLAY(RqsNSpediz:*NEXT)
007200050309     D  RqsNSp                        7S 0
007300050309     D                                     OVERLAY(RqsNSpediz:*NEXT)
007400050307     D TIS177DSO     E DS
007500050307     D                                     INZ
007600050307     D                                     PREFIX(Rpy)
007700050309     D  AGc                           4S 0
007800050307     D                                     DIM(Elementi)
007900050307     D                                     OVERLAY(RpyAGc)
008000050307     D  FGc                           3S 0
008100050307     D                                     DIM(Elementi)
008200050307     D                                     OVERLAY(RpyFGc)
008300050307     D  NGc                           7S 0
008400050307     D                                     DIM(Elementi)
008500050307     D                                     OVERLAY(RpyNGc)
008600050309     D  FRG                           2S 0
008700050307     D                                     DIM(Elementi)
008800050307     D                                     OVERLAY(RpyFRG)
008900050307     D  DAG                          10
009000050307     D                                     DIM(Elementi)
009100050307     D                                     OVERLAY(RpyDAG)
009200050307     D  CMC                          30
009300050307     D                                     DIM(Elementi)
009400050307     D                                     OVERLAY(RpyCMC)
009500050325     D  CM2                          50
009600050307     D                                     DIM(Elementi)
009700050307     D                                     OVERLAY(RpyCM2)
009800050307     D  DCG                          10
009900050307     D                                     DIM(Elementi)
010000050307     D                                     OVERLAY(RpyDCG)
010100050307     D  DDM                          10
010200050307     D                                     DIM(Elementi)
010300050307     D                                     OVERLAY(RpyDDM)
010400050307     D  DIS                          20
010500050307     D                                     DIM(Elementi)
010600050307     D                                     OVERLAY(RpyDIS)
010700060627     D  ASG                           3
010800050307     D                                     DIM(Elementi)
010900050307     D                                     OVERLAY(RpyASG)
011000050307     D  PSG                          15
011100050307     D                                     DIM(Elementi)
011200050307     D                                     OVERLAY(RpyPSG)
011300050307     D  VCS                           2
011400050307     D                                     DIM(Elementi)
011500050307     D                                     OVERLAY(RpyVCS)
011600050307     D  VCA                           3
011700050307     D                                     DIM(Elementi)
011800050307     D                                     OVERLAY(RpyVCA)
011900050310     D  CAS                          10
012000050307     D                                     DIM(Elementi)
012100050307     D                                     OVERLAY(RpyCAS)
012200050307     D  DMC                         200
012300050307     D                                     DIM(Elementi)
012400050307     D                                     OVERLAY(RpyDMC)
012500160121     D*TIS174DSI     E DS
012600160121     D*                                    INZ
012700160121     D*TIS174DSO     E DS
012800160121     D*                                    INZ
012801160121     D tis778dsi     E DS                  INZ(*EXTDFT)
012803160121     D tis778dso     E DS                  INZ(*EXTDFT)
012900050404     D TIGCP00F      E DS                                                       Giacenza
013000050308     D                                     INZ
013100050404     D TIGNP00F      E DS                                                       Note giacenza
013200050308     D                                     INZ
013300050308     D DICE          E DS                                                       Causale evento
013400050308     D                                     INZ
013500050310     D DS2D          E DS                                                       Disposizione giac.
013600050310     D                                     INZ
013700060623     D azlin00f      E DS                  BASED(nullPtr)
013800060623     D kpjba         E DS                  INZ
013900060623     D tibs02ds      E DS                  INZ
014000060623     D  t02Mod       E                     INZ('C')                             Controllo
014100060623
014200050308     D RqsDta          S           9999                                         Request data da SGI
014300050308     D RpyDta          S           9999                                         Reply data a SGI
014400050309     D nEsito          S              5I 0
014500160121     D*cEsito          S              1
014600050308     D i               S              5I 0
014700050308     D WrkDAG          S              8  0
014800050308     D WrkDtEUR        S               D
014900050308     D                                     DATFMT(*EUR)
015000050310     D WrkCAS          S              9  2
015100050314     D WrkAAS          S              4  0
015200050329     D WrkTpN          S
015300050329     D                                     LIKE(GNPTpN)
015400060623     D rpyOpCode       S             10A
015500060623     D rpyEsito10      S             10I 0
015501160121     D rpyIdMsg        S             10I 0
015600060626     D langTabel       S                   LIKE(linTabel)
015700060623     D langTntbe       S                   LIKE(linTntbe)
015800060623
015900050307     ***********************************************************************************************
016000050307     **?
016100050307     **?Main.
016200050307     **?
016300050307     ***********************************************************************************************
016400050307     C     *ENTRY        PLIST
016500050307     C                   PARM                    RqsDta
016600050311     C                   PARM                    RpyDta
016700050308     C                   PARM                    nEsito
016800050307
016900050307     C                   EXSR      RcvParm
017000050308
017100050308     C/EXEC SQL
017200050308     C+ SET OPTION CLOSQLCSR = *ENDMOD
017300050308     C/END-EXEC
017400050307
017500050308     C                   EXSR      GetBolla
017600050314
017700060623     C*                  OPEN(E)   TNTBE01L
017800060623     C*                  OPEN(E)   TABEL00F
017900050314
018000050314     **?Se non ho ricevuto l'anno provo con l'anno corrente.
018100050314     C                   IF        RqsAAS = 0
018200050314     C                   EVAL      WrkAAS = *YEAR
018300050314     C                   ELSE
018400050314     C                   EVAL      WrkAAS = RqsAAS
018500050314     C                   ENDIF
018600050314
018700050308     C                   EXSR      GetStoGia
018800050314
018900050314     **?Se non ho reperito la storia con l'anno corrente provo con l'anno precedente.
019000050314     C                   IF        RpyEsito = 'A' AND RqsAAS = 0
019100050314     C                   EVAL      WrkAAS = WrkAAS - 1
019200050314     C                   CLEAR                   RpyEsito
019300050314     C                   EXSR      GetStoGia
019400050314     C                   ENDIF
019500050314
019600050307     C                   EXSR      Uscita
019700050307
019800050307     ***********************************************************************************************
019900050307     **?
020000050307     **?Punto di arrivo in caso di errori in SQL.
020100050307     **?
020200050307     ***********************************************************************************************
020300050307     C     ErroreSQL     TAG
020400050308     C                   EVAL      nEsito = SQLCod
020500050307     C                   EXSR      Uscita
020600050307
020700050307     ***********************************************************************************************
020800050307     **?
020900050307     **?Operazioni iniziali.
021000050307     **?
021100050307     ***********************************************************************************************
021200050307     C     *INZSR        BEGSR
021300050310
021400060623     C*    K05TBE01      KLIST
021500060623     C*                  KFLD                    TBECod
021600060623     C*                  KFLD                    TBEKe1
021700060623     C*                  KFLD                    TBEKe2
021800060623     C*                  KFLD                    TBELin
021900060623     C*                  KFLD                    TBESif
022000050310
022100060623     C*    K03TAB00      KLIST
022200060623     C*                  KFLD                    TBLKut
022300060623     C*                  KFLD                    TBLCod
022400060623     C*                  KFLD                    TBLKey
022500050307
022600060623     C*                  EVAL      TBLKut = 1
022700050311     C                   CLEAR                   SQLCA
022800060623     C                   CALLP     openTabel00f()
022900060623     C                   CALLP     inzLingue()
023000060623
023100050307     C                   ENDSR
023200050307
023300050307     ***********************************************************************************************
023400050307     **?
023500050307     **?Ricezione e controllo parametri.
023600050307     **?
023700050307     ***********************************************************************************************
023800050307     C     RcvParm       BEGSR
023900050307
024000050307     C                   CLEAR                   RpyDta
024100050308     C                   CLEAR                   nEsito
024200050307     C                   CLEAR                   TIS177DSO
024300050307
024400050307     **?Parametri obbligatori.
024500050307     C                   IF        RqsDta = *BLANK
024600050308     C                   EVAL      nEsito = -1
024700050307     C                   EXSR      Uscita
024800050307     C                   ELSE
024900050307     C                   EVAL      TIS177DSI = RqsDta
025000050307     C                   ENDIF
025100050307
025200050307     **?Numero spedizione obbligatorio e numerico.
025300050307     C                   IF        RqsNSpediz = *BLANK
025400050308     C                   EVAL      nEsito = -2
025500050307     C                   EXSR      Uscita
025600050307     C                   ELSE
025700050307     C                   TESTN                   RqsNSpediz           01
025800050307     C                   IF        NOT *IN01
025900050308     C                   EVAL      nEsito = -3
026000050307     C                   EXSR      Uscita
026100050307     C                   ENDIF
026200050307     C                   ENDIF
026300060623
026400060623     C                   EVAL      langTabel = cvtLinguaISO2ToTabel(rqsLang)
026500060623     C                   EVAL      langTntbe = cvtLinguaISO2ToTntbe(rqsLang)
026600060623
026700050307     C                   ENDSR
026800050308
026900050308     ***********************************************************************************************
027000050308     **?
027100050308     **?Reperisce bolla.
027200050308     **?
027300050308     ***********************************************************************************************
027400050308     C     GetBolla      BEGSR
027500050308
027600160121     C*                  RESET                   TIS174DSI
027601160121     C                   RESET                   tis778dsI
027700050308     C                   EVAL      NSpedizI74 = RqsNSpediz
027800050308     C                   EVAL      KSCI74 = RqsKSU
027900050308     C                   EVAL      RqsCIdI174 = RqsSUN
028000050308     C                   EVAL      AASI74 = RqsAAS
028100050308     C                   EVAL      CltIPAdI74 = RqsCltIPAd
028200050308     C                   EVAL      SChkCdeI74 = RqsSChkCde
028300060608     C                   EVAL      langI74 = rqsLang
028400160121     C*                  CLEAR                   TIS174DSO
028401160121     C                   RESET                   tis778dsO
028500050308
028600160121     C*                  CALL(E)   'TIS774R'
028700160121     C*                  PARM      *BLANK        cEsito
028800160121     C*                  PARM                    TIS174DSI
028900160121     C*                  PARM                    TIS174DSO
028901160121     C                   CALLP(E)  Tis778_GetBolla(TIS778_RQSOPCODE_GETBOLLA
028902160121     C                             : rpyEsito10 : rpyIdMsg
028903160121     C                             : formatoI74 : tis778dsI : %SIZE(tis778dsI)
028904160121     C                             : formatoO74 : tis778dsO : %SIZE(tis778dsO))
029000050308
029100050314     C                   SELECT
029200050314     C                   WHEN      %ERROR
029300050311     C                   EVAL      nEsito = *LOVAL
029400050311     C                   EXSR      Uscita
029500160121     C*                  WHEN      cEsito <> Eseguito
029501160121     C                   WHEN      rpyEsito10 < *ZERO
029600050314     C                   EVAL      nEsito = -4
029700050314     C                   EXSR      Uscita
029800050314     C                   OTHER
029900050308     C                   EVAL      RpyAAS = RqsAAS
030000050308     C                   EVAL      RpyNSpediz = NSpedizO74
030100050308     C                   EVAL      RpyTBl = RqsTBl
030200160121     C*                  EVAL      RpyRif = RiferO74
030201160121     C                   EVAL      rpyRif = %CHAR(riferO74)
030202160121     C                   EVAL      %SUBST(rpyRif:16) = riferO74a
030300050314     C                   IF        RqsSUN > 0
030400050308     C                   EVAL      RpyMit = MitO74
030500050308     C                   EVAL      RpyDes = DestinO74
030600050314     C                   ELSE
030700050314     C                   EVAL      RpyMit = %TRIMR(MitLocO74) + ' ' + MitNazO74
030800050314     C                   EVAL      RpyDes =  %TRIMR(DesLocO74) + ' ' + DesNazO74
030900050314     C                   ENDIF
031000050308     C                   EVAL      RpyColli = ColliO74
031100050308     C                   EVAL      RpyPeso = PesoO74
031200050316     C                   EVAL      RpyVolume = VolumeO74
031300050308     C                   EVAL      RpyNatura = NaturaO74
031400050314     C                   ENDSL
031500050308
031600050308     C                   ENDSR
031700050308
031800050308     ***********************************************************************************************
031900050308     **?
032000050308     **?Reperisce storia giacenza.
032100050308     **?
032200050308     ***********************************************************************************************
032300050308     C     GetStoGia     BEGSR
032400050329
032500050329     C/EXEC SQL
032600050329     C+ WHENEVER SQLERROR GOTO ErroreSQL
032700050329     C/END-EXEC
032800050308
032900050308     C/EXEC SQL
033000050404     C+ DECLARE STORIA_GIACENZA CURSOR FOR SELECT * FROM TIGCP00F WHERE
033100050314     C+ GCPATB = ' ' AND GCPAAS = :WrkAAS AND GCPLNP = :RqsLnP AND GCPNRS
033200050308     C+ = :RqsNrS AND GCPNSP = :RqsNSp ORDER BY GCPAAS, GCPLNP, GCPNRS,
033300050404     C+ GCPNSP, GCPDUR DESC
033400050308     C/END-EXEC
033500050308
033600050308     C/EXEC SQL
033700050308     C+ OPEN STORIA_GIACENZA
033800050308     C/END-EXEC
033900050330
034000050330     C                   CLEAR                   i
034100050308
034200050308     C                   DO        Elementi
034300050308
034400050308     C/EXEC SQL
034500050404     C+ FETCH NEXT FROM STORIA_GIACENZA INTO :TIGCP00F
034600050308     C/END-EXEC
034700050308
034800050308     C                   SELECT
034900050308     C                   WHEN      SQLCod = 100
035000050308     C                   IF        RpyRighe = 0
035100050308     C                   EVAL      RpyEsito = 'A'
035200050308     C                   ENDIF
035300050308     C                   LEAVE
035400050308     C                   WHEN      SQLCod < 0
035500050308     C                   EVAL      nEsito = SQLCod
035600050308     C                   LEAVE
035700050308     C                   OTHER
035800050308     C                   EXSR      SetRowStoGia
035900050308     C                   ENDSL
036000050308
036100050308     C                   ENDDO
036200050308
036300050308     C/EXEC SQL
036400050308     C+ CLOSE STORIA_GIACENZA
036500050308     C/END-EXEC
036600050308
036700050308     C                   IF        i > 0
036800050308     C                   EVAL      RpyRighe = i
036900050308     C                   EVAL      RpyEsito = Eseguito
037000050314     C                   EVAL      RpyAAS = WrkAAS
037100050308     C                   ENDIF
037200050308
037300050308     C                   ENDSR
037400050308
037500050308     ***********************************************************************************************
037600050308     **?
037700050308     **?Imposta riga storia giacenza.
037800050308     **?
037900050308     ***********************************************************************************************
038000050308     C     SetRowStoGia  BEGSR
038100050308
038200050308     C                   EXSR      GetICE
038300050308
038400050308     C                   IF        §ICEGIA = 'S' AND §ICEVAT = *BLANK
038500050310     C
038600050308     C                   EVAL      i = i + 1
038700050308     C                   EVAL      AGC(i) = GCPAGC
038800050308     C                   EVAL      FGC(i) = GCPFGC
038900050308     C                   EVAL      NGC(i) = GCPNGC
039000050308     C                   EVAL      FRG(i) = GCPFRG
039100050310     C                   EVAL      CMC(i) = §ICEDEI
039200050310     C
039300050311     C                   IF        GCPDUR = 0
039400050308     C                   EVAL      WrkDAG = (GCPAGC * 10000) + GCPMGC
039500050308     C     *ISO          MOVE      WrkDAG        WrkDtEUR
039600050308     C                   MOVE      WrkDtEUR      DAG(i)
039700050311     C                   ELSE
039800050311     C     *ISO          MOVE      GCPDUR        WrkDtEUR
039900050311     C                   MOVE      WrkDtEUR      DAG(i)
040000050311     C                   ENDIF
040100050310     C
040200050309     C                   IF        GCPDCG > 0
040300050309     C     *ISO          MOVE      GCPDCG        WrkDtEUR
040400050309     C                   MOVE      WrkDtEUR      DCG(i)
040500050309     C                   ENDIF
040600050310     C
040700050309     C                   IF        GCPDDM > 0
040800050309     C     *ISO          MOVE      GCPDDM        WrkDtEUR
040900050309     C                   MOVE      WrkDtEUR      DDM(i)
041000050309     C                   ENDIF
041100050310     C
041200050310     C                   EXSR      Get2D
041300050310     C                   EVAL      DIS(i) = §2DDes
041400050310     C
041500050310     C                   SELECT
041600050310     C                   WHEN      GCPASG = 'S'
041700060608     C*                  EVAL      ASG(i) = 'SI'
041800060613     C                   EVAL      ASG(i) =  rtvMsgLang('TIS0690':rqsLang)
041900060608     C                   SELECT
042000050310if  5C                   WHEN      GCPPSG = 'M'
042100060608     C*                  EVAL      PSG(i) = 'al mittente'
042200060613     C                   EVAL      PSG(i) =  rtvMsgLang('TIS0638':rqsLang)
042300060608if  5C                   WHEN      GCPPSG = 'D'
042400060608     C*                  EVAL      PSG(i) = 'al destinatario'
042500060613     C                   EVAL      PSG(i) =  rtvMsgLang('TIS0637':rqsLang)
042600050310e   5C                   ENDSL
042700050310     C                   WHEN      GCPASG = 'N'
042800060608     C*                  EVAL      ASG(i) = 'NO'
042900060613     C                   EVAL      ASG(i) =  rtvMsgLang('TIS0691':rqsLang)
043000050310     C                   ENDSL
043100050310     C
043200050310     C                   IF        GCPVCS = 'S'
043300050308     C                   EVAL      VCS(i) = GCPVCS
043400050310     C                   EVAL      VCA(i) = GCPVCA
043500050310     C                   EVAL      WrkCAS = GCPCAS
043600050310     C                   EVAL      CAS(i) = %TRIML(%EDITC(WrkCAS:'2'))
043700050310     C                   ENDIF
043800050310     C
043900050329     C                   EXSR      GetNotAper
044000050329     C
044100050329     C                   CLEAR                   WrkTpN
044200050329     C                   EXSR      GetNotDisp
044300050329     C                   IF        DMC(i) = *BLANK
044400050329     C                   EVAL      WrkTpN = 'R'                                 Ricevute da cliente
044500050329     C                   EXSR      GetNotDisp
044600050329     C                   ENDIF
044700050310     C
044800050308     C                   ENDIF
044900050308
045000050308     C                   ENDSR
045100050308
045200050308     ***********************************************************************************************
045300050308     **?
045400050310     **?Reperisco tabella ICE causali evento.
045500050308     **?
045600050308     ***********************************************************************************************
045700050308     C     GetICE        BEGSR
045800050308
045900060623     C*                  EVAL      TBECod = 'ICE'
046000060623     C*                  EVAL      TBEKe1 = GCPCMC
046100060623     C*    K05TBE01      CHAIN     TNTBE01L
046200060623     C                   RESET                   tibs02ds
046300060623     C                   EVAL      t02Cod = 'ICE'
046400060623     C                   EVAL      t02Ke1 = gcpCmc
046500060623     C                   EVAL      t02Lin = langTntbe
046600060623     C                   CALLP     tibs02r(kpjba:tibs02ds)
046700060623     C*                  IF        %FOUND AND TBEATB = *BLANK
046800060623     C                   IF        t02Err <> 'E'
046900060623     C*                  EVAL      DICE = TBEUni
047000060623     C                   EVAL      dice = t02Uni
047100050308     C                   ELSE
047200050308     C                   CLEAR                   DICE
047300050308     C                   ENDIF
047400050308
047500050308     C                   ENDSR
047600050310
047700050310     ***********************************************************************************************
047800050310     **?
047900050310     **?Reperisco tabella 2D disposizioni giacenza codificate.
048000050310     **?
048100050310     ***********************************************************************************************
048200050310     C     Get2D         BEGSR
048300050310
048400060623     C*                  EVAL      TBLCod = '2D'
048500060623     C*                  EVAL      TBLKey = GCPDis
048600060623     C*    K03TAB00      CHAIN     TABEL00F
048700060626     C                   EVAL      ds2D = chainTabel00f('CHAIN3':langTabel
048800060626     C                             :'2D':gcpDis:%LEN(gcpDis):'TBLUNI':rpyOpCode
048900060623     C                             :rpyEsito10)
049000060623     C*                  IF        %FOUND AND TBLFlg = *BLANK
049100060626     C*                  EVAL      DS2D = TBLUni
049200060626     C*                  ELSE
049300060626     C*                  CLEAR                   DS2D
049400060626     C*                  ENDIF
049500050310
049600050310     C                   ENDSR
049700050308
049800050308     ***********************************************************************************************
049900050308     **?
050000050329     **?Reperisco nota apertura.
050100050308     **?
050200050308     ***********************************************************************************************
050300050329     C     GetNotAper    BEGSR
050400050329
050500050329     C/EXEC SQL
050600050329     C+ WHENEVER SQLERROR CONTINUE
050700050329     C/END-EXEC
050800050308
050900050308     C/EXEC SQL
051000050404     C+ SELECT * INTO :TIGNP00F FROM TIGNP00F WHERE GNPAGC = :GCPAGC AND
051100050329     C+ GNPFGC = :GCPFGC AND GNPNGC = :GCPNGC AND GNPFRG = :GCPFRG AND
051200050330     C+ GNPTPN = ' ' AND GNPFAS = 10 AND GNPNPR = 1
051300050308     C/END-EXEC
051400050308
051500050308     C                   SELECT
051600050308     C                   WHEN      SQLCod = 100
051700050329     C                   WHEN      SQLCod = 0 OR SQLCod = -811 OR SQLCod > 0
051800050329     C                   EVAL      CM2(i) = GNPDMC
051900050308     C                   WHEN      SQLCod < 0
052000050308     C                   EVAL      nEsito = SQLCod
052100050329     C                   GOTO      ErroreSQL
052200050308     C                   ENDSL
052300050308
052400050308     C                   ENDSR
052500050329
052600050329     ***********************************************************************************************
052700050329     **?
052800050329     **?Reperisco disposizione.
052900050329     **?
053000050329     ***********************************************************************************************
053100050329     C     GetNotDisp    BEGSR
053200050329
053300050329     C/EXEC SQL
053400050329     C+ WHENEVER SQLERROR GOTO ErroreSQL
053500050329     C/END-EXEC
053600050329
053700050329     C/EXEC SQL
053800050404     C+ DECLARE NOTE_DISPOSIZIONE CURSOR FOR SELECT * FROM TIGNP00F WHERE
053900050329     C+ GNPAGC = :GCPAGC AND GNPFGC = :GCPFGC AND GNPNGC = :GCPNGC AND
054000050329     C+ GNPFRG = :GCPFRG AND GNPFAS = 20 AND GNPTPN = :WrkTpN ORDER BY
054100050329     C+ GNPAGC, GNPFGC, GNPNGC, GNPFRG, GNPFAS, GNPNPR FOR READ ONLY
054200050329     C/END-EXEC
054300050329
054400050329     C/EXEC SQL
054500050329     C+ OPEN NOTE_DISPOSIZIONE
054600050329     C/END-EXEC
054700050329
054800050329     C                   DO        *HIVAL
054900050329
055000050329     C/EXEC SQL
055100050404     C+ FETCH NEXT FROM NOTE_DISPOSIZIONE INTO :TIGNP00F
055200050329     C/END-EXEC
055300050329
055400050329     C                   SELECT
055500050329     C
055600050329     C                   WHEN      SQLCod = 100
055700050329     C                   LEAVE
055800050329     C
055900050329     C                   WHEN      SQLCod < 0
056000050329     C                   EVAL      nEsito = SQLCod
056100050329     C                   LEAVE
056200050329     C
056300050329     C                   OTHER
056400050329     C
056500050329     C                   SELECT
056600050329     C                   WHEN      %SUBST(GNPDMC:1:20) = 'DATA DISPOS.CLIENTE:'
056700050329     C                             AND GNPTpN = 'R'
056800050329     C                   CLEAR                   DMC(i)
056900050329     C                   WHEN      %SUBST(DMC(i):1:50) = *BLANK
057000050329     C                   EVAL      %SUBST(DMC(i):1:50) = GNPDMC
057100050329     C                   WHEN      %SUBST(DMC(i):51:50) = *BLANK
057200050329     C                   EVAL      %SUBST(DMC(i):51:50) = GNPDMC
057300050329     C                   WHEN      %SUBST(DMC(i):101:50) = *BLANK
057400050329     C                   EVAL      %SUBST(DMC(i):101:50) = GNPDMC
057500050329     C                   WHEN      %SUBST(DMC(i):151:50) = *BLANK
057600050329     C                   EVAL      %SUBST(DMC(i):151:50) = GNPDMC
057700050329     C                   ENDSL
057800050329     C
057900050329     C                   ENDSL
058000050329
058100050329     C                   ENDDO
058200050329
058300050329     C/EXEC SQL
058400050329     C+ CLOSE NOTE_DISPOSIZIONE
058500050329     C/END-EXEC
058600050329
058700050329     C                   ENDSR
058800060608
058900050307     ***********************************************************************************************
059000050307     **?
059100050307     **?Uscita dal programma.
059200050307     **?
059300050307     ***********************************************************************************************
059400050307     C     Uscita        BEGSR
059500050307
059600060623     C*                  CLOSE(E)  TNTBE01L
059700060623     C*                  CLOSE(E)  TABEL00F
059800050311
059900050311     C                   IF        nEsito >= 0
060000050311     C                   EVAL      RpyDta = TIS177DSO
060100050311     C                   ELSE
060200060613     C*                  EVAL      RpyDta = 'Esito ' +
060300060613     C*                            %TRIML(%EDITC(nEsito:'Q')) + ' da ' +
060400060613     C*                            %TRIMR(ProcName)
060500060613     C                   EVAL      RpyDta = rtvMsgLang('TIS0182':rqsLang
060600060613     C                             :%EDITC(nEsito:'Q')+ProcName)
060700050311     C                   SELECT
060800050311     C                   WHEN      nEsito = *LOVAL
060900050311     C                   EVAL      RpyDta = %TRIMR(RpyDta) + ' ' + PSDS
061000050311     C                   WHEN      SQLCod < 0
061100050311     C                   EVAL      RpyDta = %TRIMR(RpyDta) + ' ' + SQLCA
061200050311     C                   WHEN      nEsito = -1
061300060613     C*                  EVAL      RpyDta = %TRIMR(RpyDta) +
061400060613     C*                            ' Non sono stati ricevuti dei parametri -
061500060613     C*                            obbligatori.'
061600060613     C                   EVAL      RpyDta = %TRIMR(RpyDta) +
061700060613     C                             ' ' + rtvMsgLang('TIS0397':rqsLang)
061800050311     C                   WHEN      nEsito = -2
061900060613     C*                  EVAL      RpyDta = %TRIMR(RpyDta) +
062000060613     C*                            ' Numero spedizione non ricevuto.'
062100060613     C                   EVAL      RpyDta = %TRIMR(RpyDta) +
062200060613     C                             ' ' + rtvMsgLang('TIS0412':rqsLang)
062300050311     C                   WHEN      nEsito = -3
062400060613     C*                  EVAL      RpyDta = %TRIMR(RpyDta) +
062500060613     C*                            ' Il n. spedizione ' + RqsNSpediz + ' non è -
062600060613     C*                            composto da 12 cifre.'
062700060613     C                   EVAL      RpyDta = rtvMsgLang('TIS0222':rqsLang
062800060613     C                             :RqsNSpediz)
062900050314     C                   WHEN      nEsito = -4
063000060613     C*                  EVAL      RpyDta = %TRIMR(RpyDta) +
063100060613     C*                            ' La spedizione ' + RqsNSpediz + ' non è -
063200060613     C*                            visualizzabile.'
063300060613     C                   EVAL      RpyDta = rtvMsgLang('TIS0339':rqsLang
063400060613     C                             :RqsNSpediz)
063500050311     C                   ENDSL
063600050311     C                   ENDIF
063700050311
063800050307     C                   RETURN
063900050307
064000050307     C                   ENDSR
