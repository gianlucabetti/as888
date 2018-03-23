000100060626     H DFTACTGRP(*NO) BNDDIR('TIS':'TIBS')
000200030620
000300030623     FFnOrm01L  IF   E           K DISK
000400160301     fFNORG01L  if   e           k disk
000500160301     fFIPRO11L  if   e           k disk
000600030620     FTiTa432C  IF   E           K DISK
000700030716
000800030620     D Eseguito        C                   '0'
000900030620     D Errore          C                   '1'
001000030620     D Elementi        C                   50
001100030623     D Singola         C                   'S'
001200030623     D Multipla        C                   'M'
001300090216     D ORM_FASE_IMMESSO_E_CONFERMATO...
001400090216     D                 S                   LIKE(orfFar)
001500090216     D                                     INZ(100)
001600160309
001700160309     d ORM_FASE_PDA...
001800160309     d                 s                   like(ORFfar)
001900160309     d                                     inz(499)
002000160309
002100160309     d ORM_FASE_ASSEGNATO...
002200160309     d                 s                   like(ORFfar)
002300160309     d                                     inz(400)
002400160301      /copy gaitrasrc/srcconst,TIO7G31R
002500060626
002600060626     D GetSpeChkCde    PR            10U 0
002700060626     D  Anno                          4
002800060626     D                                     VALUE
002900060626     D  IdSpedizione                 12
003000060626     D                                     VALUE
003100060626     D  ChkCode                      10U 0
003200060626     D  nEsito                        5I 0
003300160301
003400060626     D inzLingue       PR            10I 0
003500060626     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
003600060626     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
003700060626     D  rpyElementi                  10I 0 OPTIONS(*NOPASS:*OMIT)
003800160301
003900060626     D cvtLinguaISO2ToTntbe...
004000060626     D                 PR                  LIKE(linTntbe)
004100060626     D  rqsISO2                            LIKE(linISO2)
004200060626     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
004300060626     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
004400060626     D  rpyTntbe                           LIKE(linTntbe)
004500060626     D                                     OPTIONS(*NOPASS:*OMIT)
004600160301
004700060626     D tibs02r         PR                  EXTPGM('TIBS02R')                    Lettura TNTBE00F
004800060626     D  kpjba                              LIKEDS(kpjba)
004900060626     D  tibs02ds                           LIKEDS(tibs02ds)
005000160301
005100160301       // -?Calcolo orari servizio
005200160301     d trulorsr        pr                  extpgm('TRULORSR')
005300160301     d  kpjba                       502a
005400160301     d  trulorsds                          likeds(TRULORSDS)
005500160301     d  trulor2ds                          likeds(TRULOR2DS)
005600160301     d                                     options (*nopass)
005700160301
005800160301      // - Aggiunge/Toglie gg/mm dalla data
005900160301     d XGIOLAV         pr                  extpgm('XGIOLAV')
006000160301     d  xgiolavds                          likeds(xgiolavds)
006100160301
006200160301      // - Imposto il messaggio
006300160301     d rtvMsgLang      PR          3512A                                        Messaggio in lingua
006400160301     d  msgId                         7A   CONST
006500160301     d  piLinguaISO2                  2A   OPTIONS(*OMIT:*NOPASS)
006600160301     d  piMsgDta                    512A   OPTIONS(*OMIT:*NOPASS:*VARSIZE) CONST
006700160301     d  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
006800160301     d                                     VARYING
006900160301     d  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
007000160301     d                                     VARYING
007100160301     d  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
007200160301     d  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
007300060626
007400060626     D DCMR          E DS                  INZ
007500060626     D DFAR          E DS                  INZ
007600030620     D FnOrf00F      E DS
007700030620     D                                     BASED(NullPtr)
007800030620     D AzOrg00F      E DS
007900030620     D                                     BASED(NullPtr)
008000060626     D azlin00f      E DS
008100060626     D                                     BASED(NullPtr)
008200060626     D kpjba         E DS
008300060626     D                                     INZ
008400060626     D tibs02ds      E DS                  INZ
008500060626     D  t02Mod       E                     INZ('C')                             Controllo
008600160301
008700160301     d TRULORSDS     e ds                  inz
008800160301     d TRULOR2DS     e ds                  inz
008900160301
009000160301     d FIPRORORDS    e ds                  inz
009100160301     d dDSPR         e ds                  inz
009200160301
009300160301      // - Aggiunge/Toglie gg/mm dalla data
009400160301     d XGIOLAVDS     e ds                  inz
009500160301
009600160301     D TIO1G3IDS     E DS                  inz(*extdft)
009700030620     D                                     PREFIX(Inp)
009800030711     D  InpPoe                        3S 0
009900030711     D                                     OVERLAY(InpOrm:1)
010000030711     D  InpNsr                        2S 0
010100030711     D                                     OVERLAY(InpOrm:4)
010200030711     D  InpNor                        7S 0
010300030714     D                                     OVERLAY(InpOrm:6)
010400030711     D  InpNrv                        2S 0
010500030714     D                                     OVERLAY(InpOrm:13)
010600030620
010700160301     D TIO1G3ODS     E DS                  inz(*extdft)
010800030620     D                                     PREFIX(Out)
010900030620     D  FARDES                       35
011000030620     D                                     DIM(Elementi)
011100030620     D                                     OVERLAY(OutFarDes)
011200030620     D  DAE                          10
011300030620     D                                     DIM(Elementi)
011400030620     D                                     OVERLAY(OutDae)
011500030620     D  ORE                           5
011600030620     D                                     DIM(Elementi)
011700030620     D                                     OVERLAY(OutOre)
011800030620     D  POGDES                       20
011900030620     D                                     DIM(Elementi)
012000030620     D                                     OVERLAY(OutPogDes)
012100030716     D  CARDES                       35
012200030716     D                                     DIM(Elementi)
012300030716     D                                     OVERLAY(OutCarDes)
012400030620
012500030620     D WrkRowFasi      DS
012600030620     D                                     OCCURS(Elementi)
012700030620     D  RowPoe
012800030620     D                                     LIKE(OrfPoe)
012900030620     D  RowNsr
013000030620     D                                     LIKE(OrfNsr)
013100030620     D  RowNor
013200030620     D                                     LIKE(OrfNor)
013300030620     D  RowNrv
013400030620     D                                     LIKE(OrfNrv)
013500030620     D  RowDae
013600030620     D                                     LIKE(OrfDae)
013700030620     D  RowOre
013800030620     D                                     LIKE(OrfOre)
013900030716     D  RowCar
014000030716     D                                     LIKE(OrfCar)
014100030620     D  RowPogDes
014200030620     D                                     LIKE(OrgDes)
014300060626     D  RowFar
014400060626     D                                     LIKE(OrfFar)
014500030620
014600060626     D NullPtr         S               *
014700030620     D Esito           S              1
014800030620     D I               S              5I 0
014900030620     D WrkDatEur       S               D
015000030620     D                                     DATFMT(*EUR)
015100030620     D WrkTimEur       S               T
015200030620     D                                     TIMFMT(*EUR)
015300030620     D WrkTimChr       S              8
015400041223     D ChkCode         S             10U 0
015500041223     D nEsito          S              5I 0
015600060626     D langTntbe       S                   LIKE(linTntbe)
015700090216     D esisteFase100   S              1N
015800090218     D fase100Dae      S                   LIKE(orfDae)
015900160301     d wdata           s             10a
016000160301     d wparm           s            512a
016100160309     d OkFaseDopoPda   s              1n   inz
016200160309     d RigheFasi       s                   like(OutRigheFasi)
016300160301
016400160229       //--------------------------------------------------------------
016500160229       //?Definizione prototipi.
016600160229       //--------------------------------------------------------------
016700160229      /copy gaitrasrc/srcprotopr,TIO7G31R
016800160229
016900160229       //--------------------------------------------------------------
017000160229       //?Definizione parametri programma.
017100160229       //--------------------------------------------------------------
017200160302     d Tio7G31_GetDettaglioOrm...
017300160229     d                 PI
017400160229     d prmRqsOpCode...
017500160229     d                               10I 0 CONST
017600160229     d prmRpyOpCode...
017700160229     d                               10I 0
017800160229     d prmRpyIdMsg...
017900160229     d                               10I 0
018000160229     d prmRqsFormato...
018100160229     d                               10A   CONST
018200160229     d prmRqsData...
018300160229     d                            32767A   OPTIONS(*VARSIZE)
018400160229     d prmRqsSize...
018500160229     d                               10I 0 CONST
018600160229     d prmRpyFormato...
018700160229     d                               10A   CONST
018800160229     d prmRpyData...
018900160229     d                            32767A   OPTIONS(*VARSIZE)
019000160229     d prmRpySize...
019100160229     d                               10I 0 CONST
019200160229
019300090216
019400090216     C/EXEC SQL
019500090216     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
019600090216     C/END-EXEC
019700060626
019800030620     ***********************************************************************
019900030620     **
020000030620     ** Main
020100030620     **
020200030620     ***********************************************************************
020300160229     C***  *ENTRY        PLIST
020400160229     C***                PARM                    Esito
020500160229     C***                PARM                    TIO1G3IDS
020600160229     C***                PARM                    TIO1G3ODS
020700030620
020800160301     C                   reset                   TIO1G3ODS
020900030620     C                   CLEAR                   Esito
021000160229
021100160229     C                   EXSR      Controlla
021200060626
021300030623     C                   EXSR      GetOrm
021400160229     C                   EXSR      GetStatus
021500030620     C                   EXSR      GetTas
021600030620     C                   EXSR      GetFasi
021700030620     C                   EXSR      Uscita
021800160229      /free
021900160229       //--------------------------------------------------------------
022000160229       //?Controllo formale dei dati passati.
022100160229       //--------------------------------------------------------------
022200160301       BEGSR  Controlla;
022300160229
022400160229       //?OpCode
022500160302         IF  prmRqsOpCode <> TIO7G31_RQSOPCODE_GETDETTAGLIOORM;
022600160301           prmRpyOpCode = TIO7G31_RPYOPCODE_ERROR ;
022700160301           prmRpyIdMsg  = TIO7G31_ESITO_RQSOPCODE_SCONOSCIUTO ;
022800160301           exsr Uscita;
022900160229         ENDIF;
023000160301
023100160301       //?Formato e size RQS
023200160301         IF  prmRqsSize > 0;
023300160301         ELSE;
023400160301           prmRpyOpCode = TIO7G31_RPYOPCODE_ERROR;
023500160301           prmRpyIdMsg = TIO7G31_ESITO_SIZE_DATA_ERRATO;
023600160301           exsr Uscita;
023700160301         ENDIF;
023800160301         IF  prmRqsFormato = INPformato;
023900160301           TIO1G3IDS = %SUBST(prmRqsData:1:prmRqsSize);
024000160301         ELSE;
024100160301           prmRpyOpCode = TIO7G31_RPYOPCODE_ERROR;
024200160301           prmRpyIdMsg = TIO7G31_ESITO_NOME_FORMATO_SCONOSCIUTO;
024300160301           exsr Uscita;
024400160301         ENDIF;
024500160301
024600160301       //?Formato e size RPY
024700160301         IF  prmRpyFormato = OUTformato;
024800160301         ELSE;
024900160301           prmRpyOpCode = TIO7G31_RPYOPCODE_ERROR;
025000160301           prmRpyIdMsg = TIO7G31_ESITO_NOME_FORMATO_SCONOSCIUTO;
025100160301           exsr Uscita;
025200160301         ENDIF;
025300160301         IF  prmRpySize > 0;
025400160301         ELSE;
025500160301           prmRpyOpCode = TIO7G31_RPYOPCODE_ERROR;
025600160301           prmRpyIdMsg = TIO7G31_ESITO_SIZE_DATA_ERRATO;
025700160301           exsr Uscita;
025800160301         ENDIF;
025900160301
026000160301       //?Se ID lingua non valido o non passato
026100160301       //?assumo italiano
026200160302         IF  INPlang = *blanks or
026300160302            (INPlang <> TIO7G31_ID_LINGUA_IT and
026400160302             INPlang <> TIO7G31_ID_LINGUA_FR and
026500160302             INPlang <> TIO7G31_ID_LINGUA_EN and
026600160302             INPlang <> TIO7G31_ID_LINGUA_DE);
026700160302           INPlang = TIO7G31_ID_LINGUA_IT;
026800160301         ENDIF;
026900160229
027000160229       ENDSR;
027100160301      /end-free
027200030620     ***********************************************************************
027300030620     **
027400030620     ** Reperisco i dati ORM.
027500030620     **
027600030620     ***********************************************************************
027700030623     C     GetOrm        BEGSR
027800030620
027900030623     C                   EVAL      OrmPoe = InpPoe
028000030623     C                   EVAL      OrmNsr = InpNsr
028100030623     C                   EVAL      OrmNor = InpNor
028200030623     C                   EVAL      OrmNrv = InpNrv
028300030620
028400030623     C     K04Orm01      CHAIN     FnOrm01L
028500030620
028600030620     C                   IF        NOT %FOUND
028700030620if  1C                   EVAL      Esito = Errore
028800030620     C                   EXSR      Uscita
028900030620     C                   ENDIF
029000030620
029100030716     C                   EVAL      OutRFA = OrmRFA
029200030623     C                   EVAL      OutRSR = OrmRSR
029300030623     C                   EVAL      OutINR = OrmINR
029400030623     C                   EVAL      OutCAR = OrmCAR
029500030623     C                   EVAL      OutLOR = OrmLOR
029600030623     C                   EVAL      OutPRR = OrmPRR
029700030623     C                   EVAL      OutNAR = OrmNAR
029800030623     C                   EVAL      OutPAG = OrmPAG
029900030620
030000030623     C                   IF        OrmDAR > 0
030100030623     C     *ISO          MOVE      OrmDAR        WrkDatEur
030200030620     C                   MOVE      WrkDatEur     OutDAR
030300030620     C                   ENDIF
030400030620
030500030716     C                   EVAL      OutORR = %EDITW(OrmORR:'  .  ')
030600030623     C                   EVAL      OutNAM = OrmNAM
030700030623     C                   EVAL      OutNCL = OrmNCL
030800030623     C                   EVAL      OutPKG = OrmPKG
030900030623     C                   EVAL      OutVLM = OrmVLM
031000030623     C                   EVAL      OutBNC = OrmBNC
031100030623     C                   EVAL      OutBLC = OrmBLC
031200030623     C                   EVAL      OutATT = OrmATT
031300030623     C                   EVAL      OutMTC = OrmMTC
031400030623     C                   EVAL      OutRER = OrmRER
031500030623     C                   EVAL      OutTER = OrmTER
031600030623     C                   EVAL      OutUPD = OrmTOR
031700030623     C                   EVAL      OutRSC = OrmRSC
031800030623     C                   EVAL      OutINC = OrmINC
031900030623     C                   EVAL      OutCAC = OrmCAC
032000030623     C                   EVAL      OutLOC = OrmLOC
032100030623     C                   EVAL      OutPRC = OrmPRC
032200030623     C                   EVAL      OutNAC = OrmNAC
032300030623     C                   EVAL      OutFFD = OrmFFD
032400030623     C                   EVAL      OutNO1 = OrmNO1
032500030623     C                   EVAL      OutNO2 = OrmNO2
032600030620
032700030620     C                   ENDSR
032800160229      /free
032900160229       //--------------------------------------------------------------
033000160229       //?Reperisco status ORM e relativa descrizione.
033100160229       //--------------------------------------------------------------
033200160229       BEGSR  GetStatus;
033300160301
033400160301       //?da fare solo se ORM ancora da eseguire
033500160309         IF  ORMfao > ORM_FASE_ASSEGNATO;
033600160301           leavesr;
033700160301         ENDIF;
033800160301
033900160301         clear TRULORSDS;
034000160301         clear TRULOR2DS;
034100160301         IOREdta = ORMdar;
034200160301         IOREfil = ORMpor;
034300160301         IOREcap = ORMcar;
034400160301         IOREloc = ORMlor;
034500160301         IOREnar = ORMnar;
034600160301         IOREtser = 'R';
034700160301
034800160301       //?Se ORM in distinta cerco l'eventuale orario inserito da PDA
034900160301         clear FIPRORORDS;
035000160301         IF  ORMndc > 0 and ORMeti <> *blanks;
035100160301           chain (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORG01L;
035200160301           IF  %found(FNORG01L);
035300160301             chain (ORGfgs:ORGndc:'ROR':ORGpdc:ORGpoe:ORgnsr:ORGnor:ORGnrv)
035400160301                   FIPRO11L;
035500160301             IF  %found(FIPRO11L);
035600160301               FIPRORORDS = PROdati;
035700160301               IOREorp = §PROorasti;
035800160301             ENDIF;
035900160301           ENDIF;
036000160301         ENDIF;
036100160301
036200160301         trulorsr (kpjba:trulorsds:trulor2ds);
036300160301
036400160301       //?Imposto gli orari standard di ritro e le stime da PDA
036500160301         clear OUTorstini;
036600161115         IF  ORMrmp = *blanks and OOR2stis > 0;
036700160301           OUTorstini = %editw(OOR2stis:'  .  ');
036800160301         ENDIF;
036900161115         IF  ORMrmp <> *blanks and OOR2miis > 0;
037000161115           OUTorstini = %editw(OOR2miis:'  .  ');
037100161115         ENDIF;
037200160301         clear OUTorstfin;
037300161115         IF  ORMrmp = *blanks and OOR2stfs > 0;
037400160301           OUTorstfin = %editw(OOR2stfs:'  .  ');
037500160301         ENDIF;
037600161115         IF  ORMrmp <> *blanks and OOR2mxfs > 0;
037700161115           OUTorstfin = %editw(OOR2mxfs:'  .  ');
037800161115         ENDIF;
037900160301         clear OUTorsticd;
038000160301         IF  OOR2presd > 0;
038100160301           OUTorsticd = %editw(OOR2presd:'  .  ');
038200160301         ENDIF;
038300160301         clear OUTorstica;
038400160301         IF  OOR2presa > 0;
038500160301           OUTorstica = %editw(OOR2presa:'  .  ');
038600160301         ENDIF;
038700160301
038800160301       //?Imposto gli orari di ritiro
038900160301         IF  OUTorsticd <> *blanks;
039000160301           OUTorincon = OUTorsticd;
039100160301           OUTorficon = OUTorstica;
039200160301         ELSE;
039300160301           OUTorincon = OUTorstini;
039400160301           OUTorficon = OUTorstfin;
039500160301         ENDIF;
039600160301
039700160301       //?Imposto gli stati
039800160309       //?Data ritiro previsto
039900160309         IF  ORMndc > 0 and ORMfao = ORM_FASE_ASSEGNATO;
040000160302           OUTstatus = 'DDC';
040100160309           wdata = %subst(%editc(ORMddc:'X'):7:2) + '.' +
040200160309                   %subst(%editc(ORMddc:'X'):5:2) + '.' +
040300160309                   %subst(%editc(ORMddc:'X'):1:4);
040400160309         ENDIF;
040500160309         IF  ORMfao < ORM_FASE_ASSEGNATO;
040600160301           OUTstatus = 'PRV';
040700160309           wdata = %subst(%editc(ORMdar:'X'):7:2) + '.' +
040800160309                   %subst(%editc(ORMdar:'X'):5:2) + '.' +
040900160309                   %subst(%editc(ORMdar:'X'):1:4);
041000160301         ENDIF;
041100160301
041200160301       //?Imposto le righe da visualizzare nel T&T ORM
041300160301       //?stato
041400160314         clear OUTstat_d1;
041500160314         clear OUTstat_d2;
041600160314         clear OUTstat_n1;
041700160301         clear OUTstat_n2;
041800160301         clear dDSPR;
041900160301         reset TIBS02DS;
042000160301         T02cod = 'SPR';
042100160301         T02ke1 = OUTstatus;
042200160314         langTntbe = cvtLinguaISO2ToTntbe(inpLang);
042300160301         exsr GetTntbe00f;
042400160301         IF  T02err <> 'E';
042500160301           OUTstat_d1 = §SPRdec1;
042600160301           OUTstat_d2 = §SPRdec2;
042700160301           OUTstat_n1 = §SPRnot1;
042800160301           OUTstat_n2 = §SPRnot2;
042900160301         ENDIF;
043000160301       //?Note
043100160301         IF  %subst(OUTstat_n1:1:3) = 'TIS';
043200160301           wparm = wdata + OUTorincon + OUTorficon;
043300160301           OUTstat_n1 = rtvMsgLang(%subst(OUTstat_n1:1:7):
043400160302                        INPlang:wparm);
043500160301         ENDIF;
043600160301
043700160301         IF  %subst(OUTstat_n2:1:3) = 'TIS';
043800160314           wparm = wdata + OUTorincon + OUTorficon;
043900160314           OUTstat_n2 = rtvMsgLang(%subst(OUTstat_n2:1:7):
044000160314                        INPlang:wparm);
044100160301         ENDIF;
044200160229
044300160229       ENDSR;
044400160229      /end-free
044500030620     ***********************************************************************
044600030620     **
044700030620     ** Reperimento riferimento spedizione.
044800030620     **
044900030620     ***********************************************************************
045000030620     C     GetTas        BEGSR
045100030620
045200030620     C                   EVAL      TA4N14 = %EDITC(InpPoe:'X')
045300030620     C                                    + %EDITC(InpNsr:'X')
045400030620     C                                    + %EDITC(InpNor:'X')
045500030620     C                                    + %EDITC(InpNrv:'X')
045600030620
045700030620     C     K01Ta432      CHAIN     TiTa432C
045800030620
045900030620     ** Restituisco il numero spedizione se ce n'è solo una.
046000030620     C                   IF        %FOUND
046100030620     C                   EVAL      OutAAS = Ta4AAS
046200030620     C                   EVAL      OutLnP = Ta4LnP
046300030620     C                   EVAL      OutNrS = Ta4NrS
046400030620     C                   EVAL      OutNSp = Ta4NSp
046500030623     C                   EVAL      OutSpeSing = Singola
046600030620     C     K01Ta432      READE     TiTa432C
046700030620     C                   IF        NOT %EOF
046800030620     C                   CLEAR                   OutAAS
046900030620     C                   CLEAR                   OutLnP
047000030620     C                   CLEAR                   OutNrS
047100030620     C                   CLEAR                   OutNSp
047200030623     C                   EVAL      OutSpeSing = Multipla
047300030620     C                   ENDIF
047400041223     ** Calcolo il codice di controllo.
047500041223     C                   IF        OutNSp > 0
047600041223     C                   EVAL      OutSpeChkCde = GetSpeChkCde(
047700041223     C                             %EDITC(OutAAS:'X'):%EDITC(OutLnP:'X') +
047800041223     C                             %EDITC(OutNrS:'X') + %EDITC(OutNSp:'X'):
047900041223     C                             ChkCode:nEsito)
048000041223     C                   ENDIF
048100030620     C                   ENDIF
048200030620
048300030620     C                   ENDSR
048400030620
048500030620     ***********************************************************************
048600030620     **
048700030620     ** Reperisco le fasi.
048800030620     **
048900030620     ***********************************************************************
049000030620     C     GetFasi       BEGSR
049100030620
049200060626     C                   EVAL      langTntbe = cvtLinguaISO2ToTntbe(inpLang)
049300090216     C                   RESET                   esisteFase100
049400090218     C                   RESET                   fase100Dae
049500060626
049600030620     C/EXEC SQL
049700060626     C+ DECLARE FASI CURSOR FOR SELECT ORFPOE, ORFNSR, ORFNOR, ORFNRV,
049800060626     C+ ORFDAE, ORFORE, ORFCAR, ORGDES AS PO, ORFFAR
049900060626     C+ FROM (FNORF00F JOIN TNTBE00F ON 'FAR' = TBECOD
050000060626     C+ AND DIGITS(ORFFAR) = TBEKE1 AND ' ' = TBEKE2 AND ' ' = TBELIN
050100060626     C+ AND ' ' = TBESIF) JOIN AZORG00F ON
050200060626     C+ ORFPOG = ORGFIL WHERE ORFPOE = :InpPoe AND ORFNSR = :InpNsr AND
050300160302     C+ ORFNOR = :InpNor AND ORFNRV = :InpNrv AND SUBSTR(TBEUNI, 82, 1) =
050400060626     C+ 'S' ORDER BY ORFPOE, ORFNSR, ORFNOR, ORFNRV, ORFDAE DESC, ORFORE
050500160315     C+ DESC, ORFFAR desc FOR READ ONLY
050600030620     C/END-EXEC
050700030620
050800030620     C/EXEC SQL
050900030620     C+ OPEN FASI
051000030620     C/END-EXEC
051100030620
051200110913     C                   IF        sqlCode < *ZERO
051300110913     C                   DUMP(A)
051400110913     C                   EVAL      Esito = Errore
051500110913     C                   LEAVESR
051600110913     C                   ENDIF
051700110913
051800030620     C/EXEC SQL
051900030620     C+ FETCH NEXT FROM FASI FOR 50 ROWS INTO :WrkRowFasi
052000030620     C/END-EXEC
052100030620
052200030620     C                   SELECT
052300030620     C                   WHEN      SQLCod = 100
052400030620     C                   WHEN      SQLCod < 0
052500030620if  1C                   EVAL      Esito = Errore
052600030620     C                   OTHER
052700030620     ** A questo punto nella DS multipla WRKROWFASI ci possono essere
052800030620     ** da 0 a 50 ricorrenze. Il numero è contenuto nella variabile SQLER3.
052900030620     ** Scorro la DS multipla e riempio le schiere.
053000030620     C                   CLEAR                   I
053100160309     c                   reset                   OkFaseDopoPda
053200160309     c                   clear                   RigheFasi
053300030620     C                   DOW       I < SQLEr3
053400090216     C*
053500030620     C                   EVAL      I = I + 1
053600030620     C     I             OCCUR     WrkRowFasi
053700060626     C                   RESET                   tibs02ds
053800160309      /free
053900160309       //?Verifico se fase fatta dopo fasi PDA
054000160309         IF  rowFar > ORM_FASE_PDA;
054100160309           OkFaseDopoPda = *on;
054200160309         ENDIF;
054300160309       //?In questo caso non devo far vedere le fasi del PDA
054400160309         IF  OkFaseDopoPda and rowFar > ORM_FASE_ASSEGNATO and
054500160309                               rowFar <= ORM_FASE_PDA;
054600160309           iter;
054700160309         ENDIF;
054800160309         RigheFasi += 1;
054900160309      /end-free
055000060626     C                   EVAL      t02Cod = 'FAR'
055100060626     C                   EVAL      t02Ke1 = %EDITC(rowFar:'X')
055200060626     C                   EXSR      getTntbe00f
055300090216     C
055400060626     C                   IF        d§FarDva <> *BLANK
055500160309     C                   EVAL      FarDes(RigheFasi) = d§FarDva
055600060626     C                   ELSE
055700160309     C                   EVAL      FarDes(RigheFasi) = d§FarDes
055800060626     C                   ENDIF
055900090216     C
056000160309     C                   EVAL      PogDes(RigheFasi) = RowPogDes
056100030620     C     *ISO          MOVE      RowDae        WrkDatEur
056200160309     C                   MOVE      WrkDatEur     Dae(RigheFasi)
056300030620     C     *ISO          MOVE      RowOre        WrkTimEur
056400030620     C                   MOVE      WrkTimEur     WrkTimChr
056500160309     C                   EVAL      Ore(RigheFasi) = %SUBST(WrkTimChr:1)
056600090216     C
056700030716     c                   IF        RowCar <> *blanks
056800060626     C                   RESET                   tibs02ds
056900060626     C                   EVAL      t02Cod = 'CMR'
057000060626     C                   EVAL      t02Ke1 = rowCar
057100060626     C                   EXSR      getTntbe00f
057200160309     C                   EVAL      CarDes(RigheFasi) = D§CMRDVA
057300030716     c                   ENDIF
057400090216     c
057500090216     C                   IF        rowFar = ORM_FASE_IMMESSO_E_CONFERMATO
057600090216     C                   EVAL      esisteFase100 = *ON
057700090216     C                   ENDIF
057800160309
057900090216     c*
058000030620     C                   ENDDO
058100030620
058200160315     C*******            EVAL      OutRigheFasi = SQLEr3
058300160315     C                   EVAL      OutRigheFasi = RigheFasi
058400030620if  1C                   EVAL      Esito = Eseguito
058500030620
058600030620     C                   ENDSL
058700030620
058800030620     C/EXEC SQL
058900030620     C+ CLOSE FASI
059000030620     C/END-EXEC
059100030620
059200090216     C                   IF        esisteFase100
059300090216     C/EXEC SQL
059400090216     C+ SELECT ORAREF
059500090216     C+      , ORANOTE
059600090218     C+      , ORADAE
059700090218     C+ INTO :outFase100Ref
059800090218     C+    , :outFase100Not
059900090218     C+    , :fase100Dae
060000090216     C+ FROM FNORA00F
060100090216     C+ WHERE ORAPOE = :inpPoe
060200090216     C+   AND ORANSR = :inpNsr
060300090216     C+   AND ORANOR = :inpNor
060400090216     C+   AND ORANRV = :inpNrv
060500090216     C+   AND ORAFAR = :ORM_FASE_IMMESSO_E_CONFERMATO
060600090216     C+ ORDER BY ORAPOE
060700090216     C+        , ORANSR
060800090216     C+        , ORANOR
060900090216     C+        , ORANRV
061000090216     C+        , ORAFAR
061100090216     C+        , ORADAE DESC
061200090216     C+        , ORAORE DESC
061300090216     C+ FETCH FIRST ROW ONLY
061400090216     C/END-EXEC
061500090216     C                   IF        sqlCode < 0
061600090216     C                   DUMP(A)
061700090218     C                   ELSEIF    sqlCode <> 100
061800090218     C                   EVAL      outFase100Dae =
061900090218     C                                           %CHAR(%DATE(fase100Dae) : *EUR)
062000090216     C                   ENDIF
062100090216     C                   ENDIF
062200090216
062300030620     C                   ENDSR
062400030620
062500030620     ***********************************************************************
062600030620     **
062700030620     ** Operazioni iniziali.
062800030620     **
062900030620     ***********************************************************************
063000030620     C     *INZSR        BEGSR
063100030620
063200030623     C     K04Orm01      KLIST
063300030623     C                   KFLD                    OrmPoe
063400030623     C                   KFLD                    OrmNsr
063500030623     C                   KFLD                    OrmNor
063600030623     C                   KFLD                    OrmNrv
063700030620
063800030620     C     K01Ta432      KLIST
063900030620     C                   KFLD                    TA4N14
064000060626
064100060626     C                   CALLP         inzLingue()
064200060626
064300030620     C                   ENDSR
064400030620
064500060626     ***********************************************************************************************
064600060626     **
064700060626     ** Reperisco tabella da TNTBE00F.
064800060626     **
064900060626     ***********************************************************************************************
065000060626     C     getTntbe00f   BEGSR
065100060626     C                   EVAL      t02Lin = langTntbe
065200060626     C                   CALLP     tibs02r(kpjba:tibs02ds)
065300060626     C                   SELECT
065400060626     C                   WHEN      t02Cod = 'FAR'
065500060626     C                   EVAL      dfar = t02Uni
065600060626     C                   WHEN      t02Cod = 'CMR'
065700060626     C                   EVAL      dcmr = t02Uni
065800160301     C                   WHEN      t02Cod = 'SPR'
065900160301     C                   EVAL      ddspr = t02Uni
066000060626     C                   ENDSL
066100060626     C                   ENDSR
066200060626
066300030620     ***********************************************************************
066400030620     **
066500030620     ** Operazioni finali.
066600030620     **
066700030620     ***********************************************************************
066800030620     C     Uscita        BEGSR
066900160301
067000160301      /free
067100160301         IF  prmRpyOpCode <> TIO7G31_RPYOPCODE_DONE;
067200160301           Esito = Errore;
067300160301         ENDIF;
067400160301         %subst(prmRpyData:1:prmRpySize) = TIO1G3ODS;
067500160301      /end-free
067600030620
067700030620     C                   IF        Esito = *BLANK
067800030620     C                   EVAL      Esito = Eseguito
067900030620     C                   ENDIF
068000030620
068100030620     C                   RETURN
068200030620
068300030620     C                   ENDSR
