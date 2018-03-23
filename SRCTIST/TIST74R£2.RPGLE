000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200010509     H*-------------------------------------------------------------------------*
000300031001     H* STATISTICA MANUTENZIONI BOLLE
000400010509     H*-------------------------------------------------------------------------*
000500031001     FWFMAB01L  UF A E           K DISK
000600031001     FTITAS38C  IF   E           K DISK
000700070323     FTITAS26C  IF   E           K DISK    rename(titas000:titas2600)
000800070323     F                                     rename(titas010:titas2610)
000900031001     FFNEVB04L  IF   E           K DISK
001000050310     FTIGCP51L  IF   E           K DISK
001100031001     FAZORG01L  IF   E           K DISK
001200020613     FTABEL00F  IF   E           K DISK
001300040531     FFIAR531C  IF   E           K DISK
001400071105     FTIST74P   O    E             PRINTER OFLIND(*IN01) USROPN
001500001218     D*--------------------
001600031001     D* DS ESTERNE
001700001218     D*--------------------
001800900517     D KPJBA         E DS
001900010509     D DS2A          E DS
002000031001     D DS2W          E DS
002100031001     D DSTB          E DS
002200031010     D OG143         E DS
002300031204     D DS7Q          E DS
002400031204     D DS7W          E DS
002500040227     D TISIT0DS      E DS
002600040408     D DAR5GEN       E DS
002700071127     D FNLV55DS      E DS
002800011112     D*--------------------
002900011112     D* DS DI RIDEFINIZIOINE CAMPO DATA SPEDIZIONE TAS
003000011112     D*--------------------
003100011112     D                 DS
003200011112     D  TASAAS                        4  0
003300011112     D  TASMGS                        4  0
003400070323     D  KTASDSP                1      8  0
003500010509     D*--------------------
003600010509     D* SCHIERA PER CARICAMENTO CODICI E TIPI MANCATE CONSEGNE
003700010509     D*--------------------
003800031001     D SK2AA           S              3    DIM(300)                             cod. eventi L.A.
003900020614     D SK2AG           S              3    DIM(300)                             cod. eventi GIAC.
004000031125     D SK2AN           S              3    DIM(300)                             cod. eventi NN FATTE
004100031204     D*--------------------
004200031204     D* SCHIERA CARICAMENTO CODICI PARTICOLARITA' GIACENZE (7Q) + SUA ESTENSIONE (7W)
004300031204     D*--------------------
004400031204     D SK7QW           S              2    DIM(99)                              partic. NO giacenza
004500031001     D*-------------------
004600031001     D* SCHIERA FLAG SPEDIZIONE/RICAVO E RAGGRUPPAMENTO TIPO BOLLA
004700031001     D*-------------------
004800031001     D RBL             S              1    DIM(5)                               RAGGRUPPAMENTO BOLLA
004900031001     D FSP             S              1    DIM(5)                               FLAG SPEDIZIONE
005000031001     D FRI             S              1    DIM(5)                               FLAG RICAVO
005100031001     D*-------------------
005200031001     D* SCHIERA TIPO BOLLA CON RELATIVI FLAG SPEDIZIONE/RICAVO
005300031001     D*-------------------
005400031001     D TB              S              2    DIM(20)                              TIP0 BOLLA
005500031001     D SP              S              1    DIM(20)                              FLAG SPEDIZIONE
005600031001     D RI              S              1    DIM(20)                              FLAG RICAVO
005700071105     D*------------------
005800071105     D* DS X PASSAGGIO PARAMETRI
005900071105     D*------------------
006000071105     D PARAM           DS                  INZ
006100071105     D  PARTIP                        2
006200071105     D  PARTID                        1
006300071105     D  PARDAD                        8  0
006400071105     D  PARDAA                        8  0
006500071105     D  PARLNA                        3  0
006600071105     D  PARLAV                        1
006700071105     D  PARFGC                        1
006800071105     D  PARNNF                        1
006900071105     D  PARFFDA                       1
007000071105     D  PARGGCA                       1
007100071105     D  PARDCR                        1
007200071105     D  PARSUP                        1
007300071105     D  PARDADINV                     8  0
007400071105     D  PARDAAINV                     8  0
007500071105     D  PARLNADES                    25
007600071106     D  PARRSUT                      20
007700031001     D*------------------
007800031001     D* VARIABILI DI WRK RIFERITE AL DATA-BASE
007900031001     D*------------------
008000070323     D KmabTID         S                   like(mabTID)
008100031001     D KmabDAD         S                   like(mabDAD)
008200031001     D KmabDAA         S                   like(mabDAA)
008300031001     D KmabDIV         S                   like(mabDIV)
008400031001     D KmabARE         S                   like(mabARE)
008500031010     D KmabLNA         S                   like(mabLNA)
008600070323     D KtasDCM         S                   like(tasDCM)
008700071105     D KtasLNA         S                   like(tasLNA)
008800010606     C*---------------------------------------------------------------*
008900001218
009000001218
009100001218
009200920812     C*---------------------------------------------------------------*
009300001218     C* MAIN
009400071105     C*---------------------------------------------------------------*
009500071105     C*
009600071105     C* Se richiesta stampa
009700071105     C                   if        *in41 = *on
009800071105     C                   open      TIST74P
009900071105     C                   eval      stpDAI  = PARDADINV
010000071129     C                   eval      stpDAF  = PARDAAINV
010100071105     C                   eval      stpFIL  = PARLNA
010200071105     C                   eval      stpFILD = PARLNADES
010300071106     C                   eval      DSFIRS  = PARRSUT
010400071105     C                   write     ST74TE
010500071105     C                   endif
010600071105     C*
010700010509     C                   exsr      cartbl
010800071105     C  N41              exsr      delwfile
010900001218     C                   exsr      procedi
011000040227     C                   exsr      chiudi
011100071106     C*
011200071106     C* Se richiesta stampa effettuo considerazioni su elementi d grafica in stampa
011300071106     C                   if        *in41 = *on
011400071106     C*
011500071106     C                   SETOFF                                       7071
011600071106     C                   IF        stpTLAV  > *zeros OR stpTLANP > *zeros
011700071106     C                   SETON                                        70
011800071106     C                   ENDIF
011900071106     C*
012000071106     C                   IF        stpTDCRP > *zeros OR stpTDCRA > *zeros
012100071106     C                   SETON                                        71
012200071106     C                   ENDIF
012300071105     C*
012400071106     C                   write     ST74RI
012500071106     C                   write     ST74TT
012600071106     C                   write     ST74RI
012700071106     C                   write     ST74FI
012800071106     C                   close     TIST74P
012900071106     C                   endif
013000001218     C*
013100001218     C                   seton                                        LR
013200010509     C*---------------------------------------------------------------*
013300001218
013400001218
013500001218
013600001218     C*------------------------------------------------------------------------*
013700031001     C* PROCEDI - Routine principale
013800001218     C*------------------------------------------------------------------------*
013900020613     C     PROCEDI       BEGSR
014000070323     C*
014100070323     C                   SETOFF                                       30
014200001218     C*
014300070323     C* Mi posiziono sul periodo di elaborazione richiesto
014400070323     C                   Z-ADD     PARDAD        KtasDCM
014500070323     C                   Z-ADD     PARDAD        KtasDSP
014600071105     C   41              Z-ADD     PARLNA        KtasLNA
014700071105     C  N41              Z-ADD     *loval        KtasLNA
014800071105     C*
014900070323     C   40KTAS26P       SETLL     TITAS26C
015000070323     C  N40KTAS38P       SETLL     TITAS38C
015100020613     C*
015200020613     C* Se posizionamento riuscito => ciclo ed elaboro
015300070323     C                   IF        %found
015400031001     C                   MOVEL     '0'           flgFINE           1
015500020613     C*
015600020613     C* Prima lettura fuori ciclo
015700070323     C   40              READ      TITAS26C
015800070323     C  N40              READ      TITAS38C
015900070323     C                   DOW       *IN30 = *off AND
016000020614     C                             flgFINE = '0'
016100020613     C*
016200020613     C* Verifico se la bolla è da considerare
016300020613     C                   EXSR      CHKRECTAS
016400020613     C                   IF        flgRECOK = 'S'
016500040408     C*
016600040408     C* Determino subito se vi è 1 modifica in partenza o in arrivo
016700040408     C                   EXSR      REPPARARR
016800020613     C*
016900020613     C* Se la bolla è da considerare => elaboro e scrivo il WRK-FILE
017000020613     C                   EXSR      WRIWFILE
017100071105     C*
017200071105     C* Se lancio in stampa verifico se la bolla è da considerare
017300071105     C                   IF        *IN41 = *on
017400071105     C                   EXSR      CHKRECTASST
017500071105     C                   IF        flgRECOKST = 'S'
017600071105     C                   EXSR      EXESTAMPA
017700071105     C                   ENDIF
017800071105     C                   ENDIF
017900071105     C*
018000020613     C                   ENDIF
018100020613     C*
018200020613     C* Proseguo le letture dentro ciclo
018300070323     C   40              READ      TITAS26C
018400070323     C  N40              READ      TITAS38C
018500020613     C                   ENDDO
018600001218     C*
018700020613     C                   ENDIF
018800020613     C*
018900001218     C                   ENDSR
019000001218     C*------------------------------------------------------------------------*
019100071105
019200071105
019300071105
019400071105
019500071105     C*------------------------------------------------------------------------*
019600071105     C* EXESTAMPA - Routine di stampa dettaglio
019700071105     C*------------------------------------------------------------------------*
019800071105     C     EXESTAMPA     BEGSR
019900071105     C*
020000071105     C* Imposto i dati "fondamentali" della bolla (chiave, destinatario, etc...)
020100071105     C                   EVAL      stpKEY = %trim(%editw(tasAAS:'0    '))+' '+
020200071105     C                                      %trim(%editw(tasLNP:'0   ')) +' '+
020300071105     C                                      %trim(%editw(tasNRS:'0  '))  +' '+
020400071105     C                                      %trim(%editw(tasNSP:'0       '))
020500071105     C                   EVAL      stpRSD = tasRSD
020600071105     C*
020700071105     C* Effettuo considerazioni sui valori "calcolati"
020800071105     C                   EVAL      stpLANP = stpLAV + stpLANP
020900071105     C*
021000071105     C* Incremento i totali
021100071105     C                   EVAL      stpTLAV   = stpTLAV   + stpLAV
021200071105     C                   EVAL      stpTLANP  = stpTLANP  + stpLANP
021300071105     C                   EVAL      stpTFGC   = stpTFGC   + stpFGC
021400071105     C                   EVAL      stpTNNF   = stpTNNF   + stpNNF
021500071105     C                   EVAL      stpTFFDA  = stpTFFDA  + stpFFDA
021600071105     C                   EVAL      stpTGGCA  = stpTGGCA  + stpGGCA
021700071105     C                   EVAL      stpTDCRP  = stpTDCRP  + stpDCRP
021800071105     C                   EVAL      stpTDCRA  = stpTDCRA  + stpDCRA
021900071105     C                   EVAL      stpTSUP   = stpTSUP   + stpSUP
022000071106     C*
022100071106     C* Effettuo considerazioni su elementi d grafica in stampa
022200071106     C                   SETOFF                                       7071
022300071106     C                   IF        stpLAV  > *zeros OR stpLANP > *zeros
022400071106     C                   SETON                                        70
022500071106     C                   ENDIF
022600071106     C*
022700071106     C                   IF        stpDCRP > *zeros OR stpDCRA > *zeros
022800071106     C                   SETON                                        71
022900071106     C                   ENDIF
023000071105     C*
023100071105     C* Verifico il salto pagina
023200071105     C                   IF        *IN01 = *on
023300071105     C                   WRITE     ST74TE
023400071105     C                   SETOFF                                       01
023500071105     C                   WRITE     ST74DE
023600071105     C                   ELSE
023700071105     C                   WRITE     ST74DE
023800071105     C                   ENDIF
023900071105     C*
024000071105     C                   ENDSR
024100071105     C*------------------------------------------------------------------------*
024200031001
024300031001
024400031001
024500031001
024600010627     C*------------------------------------------------------------------------*
024700031001     C* CHKRECTAS - Routine di verifica bolla
024800010627     C*------------------------------------------------------------------------*
024900020613     C     CHKRECTAS     BEGSR
025000010627     C*
025100020613     C                   MOVEL     'S'           flgRECOK          1
025200020613     C*
025300031001     C* Considero solo le bolle valide come spedizione
025400031001     C                   IF        flgRECOK = 'S'
025500031001     C                   Z-ADD     1             J
025600031001     C                   SETOFF                                           28
025700031001     C     tasTBL        LOOKUP    TB(J)                                  28
025800031001     C     SP(J)         IFEQ      'N'
025900031001     C                   MOVEL     'N'           flgRECOK
026000031001     C                   ENDIF
026100031001     C                   ENDIF
026200031010     C*
026300031010     C* Escludo le bolle del network POSTE
026400031010     C                   IF        flgRECOK = 'S'
026500031010     C     tasLNP        CHAIN     AZORG01L
026600031010     C                   IF        %found(AZORG01L)
026700031010     C                   EVAL      OG143 = ORGDE3
026800031010     C                   IF        §OGNTW = 'PPT'
026900031010     C                   MOVEL     'N'           flgRECOK
027000031010     C                   ENDIF
027100031010     C                   ENDIF
027200031010     C                   ENDIF
027300020613     C*
027400020613     C* Testo se la bolla è del periodo richiesto
027500031001     C                   IF        flgRECOK = 'S'
027600070323     C                   IF        *IN40 = *off
027700070323     C                   IF        KtasDSP >= PARDAD AND
027800070323     C                             KtasDSP <= PARDAA
027900020613     C                   ELSE
028000020613     C                   MOVEL     'N'           flgRECOK
028100031001     C                   MOVEL     '1'           flgFINE
028200070323     C                   ENDIF
028300020613     C                   ENDIF
028400070323     C                   IF        *IN40 = *on
028500070323     C                   IF        tasDCM >= PARDAD AND
028600070323     C                             tasDCM <= PARDAA
028700070323     C                   ELSE
028800070323     C                   MOVEL     'N'           flgRECOK
028900070323     C                   MOVEL     '1'           flgFINE
029000070323     C                   ENDIF
029100031001     C                   ENDIF
029200070323     C                   ENDIF
029300070323     C*
029400070323     C* Verifico fine file
029500070323     C                   IF        *IN40 = *off
029600070323     C                   IF        %eof(TITAS38C)
029700070323     C                   SETON                                        30
029800070323     C                   ENDIF
029900070323     C                   ENDIF
030000070323     C                   IF        *IN40 = *on
030100070323     C                   IF        %eof(TITAS26C)
030200070323     C                   SETON                                        30
030300070323     C                   ENDIF
030400070323     C                   ENDIF
030500071105     C*
030600071105     C* Se lancio in stampa verifico anche fine filiale
030700071105     C                   IF        *IN41 = *on
030800080417     C                   IF        tasLNA <> PARLNA
030801080417     C                   MOVEL     'N'           flgRECOK
030900080417     C***                SETON                                        30
031000071105     C                   ENDIF
031100071105     C                   ENDIF
031200010627     C*
031300010627     C                   ENDSR
031400010627     C*------------------------------------------------------------------------*
031500071105
031600071105
031700071105
031800071105
031900071105     C*------------------------------------------------------------------------*
032000071105     C* CHKRECTASST - Routine di verifica bolla x stampa
032100071105     C*------------------------------------------------------------------------*
032200071105     C     CHKRECTASST   BEGSR
032300071105     C*
032400071105     C                   MOVEL     'N'           flgRECOKST        1
032500071105     C*
032600071105     C* Filtro x se nel lancio richiesti L/AVV.
032700071105     C                   IF        flgRECOKST = 'N'
032800071105     C                   IF        wFlgLAV = 'S' and PARLAV = 'S'
032900071105     C                   MOVEL     'S'           flgRECOKST
033000071105     C                   ENDIF
033100071105     C                   ENDIF
033200071105     C*
033300071105     C* Filtro x se nel lancio richiesti Aperture Giacienze
033400071105     C                   IF        flgRECOKST = 'N'
033500071105     C                   IF        wFlgFGC = 'S' and PARFGC = 'S'
033600071105     C                   MOVEL     'S'           flgRECOKST
033700071105     C                   ENDIF
033800071105     C                   ENDIF
033900071105     C*
034000071105     C* Filtro x se nel lancio richiesti Non Fatte
034100071105     C                   IF        flgRECOKST = 'N'
034200071105     C                   IF        wFlgNNF = 'S' and PARNNF = 'S'
034300071105     C                   MOVEL     'S'           flgRECOKST
034400071105     C                   ENDIF
034500071105     C                   ENDIF
034600071105     C*
034700071105     C* Filtro x se nel lancio richiesti Fermi Deposito in Arrivo
034800071105     C                   IF        flgRECOKST = 'N'
034900071105     C                   IF        wFlgFFDA = 'S' and PARFFDA = 'S'
035000071105     C                   MOVEL     'S'           flgRECOKST
035100071105     C                   ENDIF
035200071105     C                   ENDIF
035300071105     C*
035400071105     C* Filtro x se nel lancio richiesti Turni Chiusura in Arrivo
035500071105     C                   IF        flgRECOKST = 'N'
035600071105     C                   IF        wFlgGGCA = 'S' and PARGGCA = 'S'
035700071105     C                   MOVEL     'S'           flgRECOKST
035800071105     C                   ENDIF
035900071105     C                   ENDIF
036000071105     C*
036100071105     C* Filtro x se nel lancio richiesti Date Consegna Richieste
036200071105     C                   IF        flgRECOKST = 'N'
036300071105     C                   IF        wFlgDCR = 'S' and PARDCR = 'S'
036400071105     C                   MOVEL     'S'           flgRECOKST
036500071105     C                   ENDIF
036600071105     C                   ENDIF
036700071105     C*
036800071105     C* Filtro x se nel lancio richiesti Supermercati
036900071105     C                   IF        flgRECOKST = 'N'
037000071105     C                   IF        wFlgSUP = 'S' and PARSUP = 'S'
037100071105     C                   MOVEL     'S'           flgRECOKST
037200071105     C                   ENDIF
037300071105     C                   ENDIF
037400071105     C*
037500071105     C* Filtro x lina di arrivo (lasciare come ultimissimo test in questa routine)!!!!
037600071105     C                   IF        flgRECOKST = 'S'
037700071105     C                   IF        tasLNA <> PARLNA
037800071105     C                   MOVEL     'N'           flgRECOKST
037900071105     C                   ENDIF
038000071105     C                   ENDIF
038100071105     C*
038200071105     C                   ENDSR
038300071105     C*------------------------------------------------------------------------*
038400040408
038500040408
038600040408
038700040408
038800040408     C*------------------------------------------------------------------------*
038900040408     C* REPPARARR - Routine di verifica modifiche bolla in partenza o in arrivo
039000040408     C*------------------------------------------------------------------------*
039100040408     C     REPPARARR     BEGSR
039200040408     C*
039300040408     C* (*IN60 = *ON  => in arrivo)
039400040408     C* (*IN60 = *OFF => in partenza)
039500040408     C                   EVAL      *IN60 = *OFF
039600040408     C*
039700040408     C                   EVAL      ar5TRD = 'GEN'
039800040531     C     KAR530P       CHAIN     fiar531c
039900040531     C                   IF        %found(fiar531c)
040000040408     C                   EVAL      DAR5GEN = ar5UNI
040100040408     C                   IF        §AR5MDP <> *blanks
040200040408     C                   EVAL      *IN60 = *ON
040300040408     C                   ENDIF
040400040408     C                   ENDIF
040500040408     C*
040600040408     C                   ENDSR
040700040408     C*------------------------------------------------------------------------*
040800001218
040900001218
041000010509
041100010627     C*------------------------------------------------------------------------*
041200031001     C* WRIWFILE - Routine di reperimento informazioni e scrittura WRK-FILE
041300010627     C*------------------------------------------------------------------------*
041400020613     C     WRIWFILE      BEGSR
041500071105     C*
041600071105     C* Inizializzo flag d wrk
041700071105     C                   MOVEL     'N'           wFlgLAV           1
041800071105     C                   MOVEL     'N'           wFlgFGC           1
041900071105     C                   MOVEL     'N'           wFlgNNF           1
042000071105     C                   MOVEL     'N'           wFlgFFDA          1
042100071105     C                   MOVEL     'N'           wFlgGGCA          1
042200071105     C                   MOVEL     'N'           wFlgDCR           1
042300071105     C                   MOVEL     'N'           wFlgSUP           1
042400071105     C*
042500071105     C* Se lancio in stampa => inizializzo campi in stampa
042600071105     C   41              CLEAR                   ST74DE
042700031001     C*
042800031010     C* Reperisco distretto e area relativamente alla LNA della bolla
042900031010     C     tasLNA        CHAIN     AZORG01L
043000031001     C                   IF        %found(AZORG01L)
043100031001     C                   EVAL      KmabDIV = ORGFL3
043200031001     C                   EVAL      KmabARE = ORGCAR
043300031001     C                   ENDIF
043400031001     C*
043500031001     C* Valorizzo gli altri campi chiave del wrk-file
043600070323     C                   EVAL      KmabTID = PARTID
043700031001     C                   EVAL      KmabDAD = PARDAD
043800031001     C                   EVAL      KmabDAA = PARDAA
043900031010     C                   EVAL      KmabLNA = tasLNA
044000031001     C*
044100031010     C* Inizializzo il buffer del wrk-file e verifico se già esiste un record
044200031010     C* con medesima chiave (x decidere se andare in WRITE o in UPDATE)
044300031010     C                   CLEAR                   WFMAB000
044400031001     C                   SETOFF                                       50
044500031001     C     KMAB01C       CHAIN     WFMAB01L
044600031001     C                   IF        %found(WFMAB01L)
044700031001     C                   SETON                                        50
044800031001     C                   ELSE
044900070323     C                   EVAL      mabTID = KmabTID
045000031001     C                   EVAL      mabDAD = KmabDAD
045100031001     C                   EVAL      mabDAA = KmabDAA
045200031001     C                   EVAL      mabDIV = KmabDIV
045300031001     C                   EVAL      mabARE = KmabARE
045400031010     C                   EVAL      mabLNA = KmabLNA
045500031001     C                   ENDIF
045600031204     C*
045700031204     C* Eseguo considerazioni relativamente alle particolarità giacenza
045800031204     C                   IF        tasGGA = *BLANKS
045900031204     C                   SETOFF                                       54
046000031204     C                   ELSE
046100031204     C     tasGGA        LOOKUP    SK7QW                                  54
046200031204     C                   ENDIF
046300031001     C*
046400031001     C* Eseguo considerazioni relativamente agli eventi della bolla corrente
046500031001     C     KEVB04P       SETLL     FNEVB04L
046600031001     C                   IF        %equal(FNEVB04L)
046700031001     C     KEVB04P       READE     FNEVB04L
046800031001     C                   DOW       not %eof(FNEVB04L)
046900071127     C*
047000071127     C* Verifico se trattasi d evento "in carico" alla filiale in elaborazione
047100071127     C                   SETOFF                                       45
047200071127     C                   IF        evbFLE = tasLNA
047300071127     C                   SETON                                        45
047400071127     C                   ELSE
047500071127     C                   CLEAR                   FNLV55DS
047600071128     C                   EVAL      D55tpt = '6'
047700071128     C                   EVAL      D55lin = tasLNA
047800071128     C                   EVAL      D55drf = evbDEV
047900071127     C                   CALL      'FNLV55R'
048000071127     C                   PARM                    FNLV55ds
048100071128     C                   IF        D55err = *blanks
048200071128     C                   IF        D55tfa = evbFLE
048300071127     C                   SETON                                        45
048400071127     C                   ENDIF
048500071127     C                   ENDIF
048600071127     C                   ENDIF
048700071127     C*
048800071127     C                   IF        *IN45 = *on
048900031010     C* Determino il tipo di evento (se "Lasciato avviso" oppure "Giacenza")
049000031001     C     evbCEV        LOOKUP    SK2AA                                  55    * evento L/AVV
049100031001     C                   IF        %found
049200071105     C                   MOVEL     'S'           wFlgLAV
049300071105     C                   ADD       1             stpLAV
049400031001     C   50              ADD       1             mabLAV
049500031001     C  N50              Z-ADD     1             mabLAV
049600031001     C                   IF        %subst(tasGVA:1:1) = 'P'                     * L/AVV a privati
049700071105     C                   SUB       1             stpLANP
049800031001     C   50              ADD       1             mabLAP
049900031001     C  N50              Z-ADD     1             mabLAP
050000031001     C                   ENDIF
050100031001     C                   ENDIF
050200031204     C* A seconda che la bolla abbia o meno le particolarità giacenza "NO GIACENZA"
050300031204     C* conteggio gli eventi d tipo giacenza in campi differenti.
050400031001     C     evbCEV        LOOKUP    SK2AG                                  55    * evento GIAC.
050500031001     C                   IF        %found
050600031204     C                   IF        *IN54 = *OFF
050700031010     C   50              ADD       1             mabGIA
050800031010     C  N50              Z-ADD     1             mabGIA
050900031204     C                   ELSE
051000031204     C   50              ADD       1             mabENG
051100031204     C  N50              Z-ADD     1             mabENG
051200031001     C                   ENDIF
051300031204     C                   ENDIF
051400031125     C     evbCEV        LOOKUP    SK2AN                                  55    * evento NN FATTA
051500031125     C                   IF        %found
051600071105     C                   MOVEL     'S'           wFlgNNF
051700071105     C                   ADD       1             stpNNF
051800031125     C   50              ADD       1             mabNNF
051900031125     C  N50              Z-ADD     1             mabNNF
052000031125     C                   ENDIF
052100071127     C*
052200071127     C                   ENDIF
052300071127     C*
052400031001     C     KEVB04P       READE     FNEVB04L
052500031001     C                   ENDDO
052600031001     C                   ENDIF
052700031201     C*
052800031201     C* Eseguo considerazioni relativamente alle aperture/riaperture giacenza
052900050310     C     KGCP51P       SETLL     TIGCP51L
053000050310     C                   IF        %equal(TIGCP51L)
053100050310     C     KGCP51P       READE     TIGCP51L
053200050310     C                   DOW       not %eof(TIGCP51L)
053300071105     C                   MOVEL     'S'           wFlgFGC
053400071105     C                   ADD       1             stpFGC
053500031201     C   50              ADD       1             mabFGC
053600031201     C  N50              Z-ADD     1             mabFGC
053700050310     C     KGCP51P       READE     TIGCP51L
053800031201     C                   ENDDO
053900031201     C                   ENDIF
054000040227     C*
054100031001     C                   IF        tasGC1 <> *blanks OR                         * turni chiusura
054200031001     C                             tasGC2 <> *blanks
054300040408     C                   IF        *IN60 = *ON
054400040408     C                   IF        tasGC1 <> §AR5GC1 OR
054500040408     C                             tasGC2 <> §AR5GC2
054600071105     C                   MOVEL     'S'           wFlgGGCA
054700071105     C                   ADD       1             stpGGCA
054800040408     C   50              ADD       1             mabGGCA
054900040408     C  N50              Z-ADD     1             mabGGCA
055000040408     C                   ELSE
055100040408     C   50              ADD       1             mabGGCP
055200040408     C  N50              Z-ADD     1             mabGGCP
055300040408     C                   ENDIF
055400040408     C                   ELSE
055500040408     C   50              ADD       1             mabGGCP
055600040408     C  N50              Z-ADD     1             mabGGCP
055700031001     C                   ENDIF
055800040408     C                   ENDIF
055900040227     C*
056000040408     C                   IF        tasFFD = 'S'
056100040408     C                   IF        *IN60 = *ON
056200040408     C                   IF        tasFFD <> §AR5FFD
056300071105     C                   MOVEL     'S'           wFlgFFDA
056400071105     C                   ADD       1             stpFFDA
056500040408     C   50              ADD       1             mabFDA
056600040408     C  N50              Z-ADD     1             mabFDA
056700040408     C                   ELSE
056800040408     C   50              ADD       1             mabFDP
056900040408     C  N50              Z-ADD     1             mabFDP
057000040408     C                   ENDIF
057100040408     C                   ELSE
057200031001     C   50              ADD       1             mabFDP
057300031001     C  N50              Z-ADD     1             mabFDP
057400031001     C                   ENDIF
057500040408     C                   ENDIF
057600040227     C*
057700040227     C                   IF        tasFTC = 'S' OR                              * consegna a superm.
057800040227     C                             tasTC2 = 'S'
057900071105     C                   MOVEL     'S'           wFlgSUP
058000071105     C                   ADD       1             stpSUP
058100040408     C*
058200040408     C                   CLEAR                   TISIT0DS                       * consegna a superm.
058300051227     C                   MOVEL     'E'           IT0TLA                         * in anagraf. dest.
058400051227     C                   MOVEL     tasNZD        IT0NAZ                         * particolari
058500040408     C                   MOVEL     tasPRD        IT0PRV
058600040408     C                   MOVEL     tasCAD        IT0CAP
058700040408     C                   MOVEL     tasLOD        IT0LOC
058800040408     C                   MOVEL     tasIND        IT0IND
058900040408     C                   MOVEL     tasRSD        IT0RAG
059000040408     C                   CALL      'TISIT5R'
059100040408     C                   PARM                    TISIT0DS
059200040408     C*
059300040408     C                   IF        OT0ERR = *blanks AND
059400040408     C                             OT0DOS = 'S'
059500040408     C                   IF        *IN60 = *ON
059600040408     C                   IF        tasFTC <> §AR5TC1 OR
059700040408     C                             tasTC2 <> §AR5TC2
059800040408     C   50              ADD       1             mabSUPA
059900040408     C  N50              Z-ADD     1             mabSUPA
060000040408     C                   ELSE
060100040408     C   50              ADD       1             mabSUPP
060200040408     C  N50              Z-ADD     1             mabSUPP
060300040408     C                   ENDIF
060400040408     C                   ELSE
060500040408     C   50              ADD       1             mabSUPP
060600040408     C  N50              Z-ADD     1             mabSUPP
060700040408     C                   ENDIF
060800040408     C*
060900040408     C                   ELSE
061000040408     C                   IF        *IN60 = *ON
061100040408     C                   IF        tasFTC <> §AR5TC1 OR
061200040408     C                             tasTC2 <> §AR5TC2
061300040408     C   50              ADD       1             mabSPSA
061400040408     C  N50              Z-ADD     1             mabSPSA
061500040408     C                   ELSE
061600040408     C   50              ADD       1             mabSPSP
061700040408     C  N50              Z-ADD     1             mabSPSP
061800040408     C                   ENDIF
061900040408     C                   ELSE
062000040408     C   50              ADD       1             mabSPSP
062100040408     C  N50              Z-ADD     1             mabSPSP
062200040408     C                   ENDIF
062300040408     C                   ENDIF
062400040408     C                   ENDIF
062500040408     C*
062600040408     C                   IF        tasDCR > *zeros
062700071105     C                   MOVEL     'S'           wFlgDCR
062800040408     C                   IF        *IN60 = *ON
062900071105     C                   ADD       1             stpDCRA
063000040408     C                   MOVEL     §AR5DCR       Num§AR5DCR        8 0
063100040409     C                   IF        tasDCR <> Num§AR5DCR
063200040408     C   50              ADD       1             mabDCRA
063300040408     C  N50              Z-ADD     1             mabDCRA
063400040408     C                   ELSE
063500040408     C   50              ADD       1             mabDCRP
063600040408     C  N50              Z-ADD     1             mabDCRP
063700040408     C                   ENDIF
063800040408     C                   ELSE
063900071105     C                   ADD       1             stpDCRP
064000040408     C   50              ADD       1             mabDCRP
064100040408     C  N50              Z-ADD     1             mabDCRP
064200040408     C                   ENDIF
064300040408     C                   ENDIF
064400031001     C*
064500031001     C* Conteggio tutte le bolle elaborate
064600071105     C                   ADD       1             stpTBOL
064700031001     C   50              ADD       1             mabTOT
064800031001     C  N50              Z-ADD     1             mabTOT
064900020613     C*
065000071105     C*
065100071105     C* Se lancio x creazione wrk-file....
065200071105     C                   IF        *IN41 = *off
065300031001     C* Al termine scrivo o update il record così valorizzato
065400031001     C   50              UPDATE    WFMAB000
065500031001     C  N50              WRITE     WFMAB000
065600071105     C                   ENDIF
065700020613     C*
065800010627     C                   ENDSR
065900010509     C*------------------------------------------------------------------------*
066000031001
066100031001
066200031001
066300031001     C*------------------------------------------------------------------------*
066400031107     C* DELWFILE - Routine di cancellazione completa del file.
066500031001     C*------------------------------------------------------------------------*
066600031001     C     DELWFILE      BEGSR
066700031001     C*
066800031107     C     *LOVAL        SETLL     WFMAB01L
066900031107     C                   READ      WFMAB01L
067000031107     C                   DOW       not %eof(WFMAB01L)
067100031107     C                   DELETE    WFMAB000
067200031107     C                   READ      WFMAB01L
067300031107     C                   ENDDO
067400031001     C*
067500031001     C                   ENDSR
067600031001     C*------------------------------------------------------------------------*
067700010626
067800010626
067900010626
068000010509     C*------------------------------------------------------------------------*
068100020613     C* CARTBL - Routine di caricamento dati tabellati
068200010509     C*------------------------------------------------------------------------*
068300010509     C     CARTBL        BEGSR
068400010509     C*
068500031001     C*--------------------
068600031001     C* DISTINGUO EVENTI DI TIPO "GIACENZA" ED EVENTI DI TIPO "LASCIATO AVVISO"
068700031001     C*--------------------
068800020614     C                   Z-ADD     0             WA                4 0
068900020614     C                   Z-ADD     0             WG                4 0
069000020614     C                   CLEAR                   SK2AA
069100020614     C                   CLEAR                   SK2AG
069200010509     C                   MOVEL     '2A'          COD
069300020614     C     KTABP         CHAIN     TABEL00F                           31
069400010509     C     *IN31         DOWEQ     '0'
069500010509     C     TBLFLG        IFEQ      ' '
069600010509     C                   MOVEL     TBLUNI        DS2A
069700020613     C     §2AFTC        IFEQ      'A'
069800020614     C                   ADD       1             WA
069900020614     C                   MOVEL     TBLKEY        SK2AA(WA)
070000010509     C                   ENDIF
070100020614     C     §2AFTC        IFEQ      'G'
070200020614     C                   ADD       1             WG
070300020614     C                   MOVEL     TBLKEY        SK2AG(WG)
070400020614     C                   ENDIF
070500031125     C     §2AFTC        IFEQ      'N'
070600031125     C                   ADD       1             WG
070700031125     C                   MOVEL     TBLKEY        SK2AN(WG)
070800031125     C                   ENDIF
070900010509     C                   ENDIF
071000020614     C     KTABP         READE     TABEL                                  31
071100010509     C                   ENDDO
071200031001     C*--------------------
071300031001     C* FLAG SPEDIZIONI /RICAVI PER RAGGRUPPAMENTO TIPO BOLLA
071400031001     C*--------------------
071500031001     C                   Z-ADD     0             I                 3 0
071600031001     C                   MOVEL     '2W'          COD
071700031001     C     KTABP         SETLL     TABEL00F
071800031001     C     KTABP         READE     TABEL00F                               99
071900031001     C     *IN99         DOWEQ     *OFF
072000031001     C     TBLCOD        ANDEQ     '2W'
072100031001     C     TBLFLG        IFEQ      *BLANKS
072200031001     C                   ADD       1             I
072300031001     C                   MOVEL     TBLKEY        RBL(I)                         *RAGGRUPPAMENTO BOLL
072400031001     C                   MOVEL     TBLUNI        DS2W
072500031001     C                   MOVEL     §2WFSP        FSP(I)                         *FLAG SPEDIZIONE
072600031001     C                   MOVEL     §2WFRI        FRI(I)                         *FLAG RICAVO
072700031001     C                   ENDIF
072800031001     C     KTABP         READE     TABEL00F                               99
072900031001     C                   ENDDO
073000031001     C*--------------------
073100031001     C* TIPI BOLLA E RELATIVI FLAGS SPEDIZIONE/RICAVO
073200031001     C*--------------------
073300031001     C                   Z-ADD     0             I
073400031001     C                   MOVEL     'TB'          COD
073500031001     C     KTABP         SETLL     TABEL00F
073600031001     C     KTABP         READE     TABEL00F                               99
073700031001     C     *IN99         DOWEQ     *OFF
073800031001     C     TBLCOD        ANDEQ     'TB'
073900031001     C     TBLFLG        IFEQ      *BLANKS
074000031001     C                   ADD       1             I
074100031001     C                   MOVEL     TBLKEY        TB(I)                          *TIPO BOLLA
074200031001     C                   MOVEL     TBLUNI        DSTB
074300031001     C                   Z-ADD     1             J                 4 0
074400031001     C                   SETOFF                                       28
074500031001     C     §TBRBL        LOOKUP    RBL(J)                                 28
074600031001     C     *IN28         IFEQ      *ON
074700031001     C                   MOVEL     FSP(J)        SP(I)                          *FLAG SPEDIZIONE
074800031001     C                   MOVEL     FRI(J)        RI(I)                          *FLAG RICAVO
074900031001     C                   ELSE
075000031001     C                   MOVEL     'N'           SP(I)
075100031001     C                   MOVEL     'N'           RI(I)
075200031001     C                   ENDIF
075300031001     C                   ENDIF
075400031001     C     KTABP         READE     TABEL00F                               99
075500031001     C                   ENDDO
075600031204     C*--------------------
075700031204     C* CARICA TABELLA GESTIONE PARTICOLARITA' GIACENZA (7Q)
075800031204     C*--------------------
075900031204     C                   Z-ADD     0             I7QW              2 0
076000031204     C                   MOVEL     '7Q'          COD
076100031204     C     KTABP         SETLL     TABEL00F                           99
076200031204     C     KTABP         READE     TABEL00F                               99
076300031204     C     *IN99         DOWEQ     *OFF
076400031204     C     TBLCOD        ANDEQ     '7Q'
076500031204     C     TBLFLG        IFEQ      *BLANKS
076600031204     C                   MOVEL     TBLUNI        DS7Q
076700031204     C                   IF        §7Q4D1 = 'R' OR
076800031204     C                             §7Q4D2 = 'R' OR
076900031204     C                             §7Q4D3 = 'R' OR
077000031204     C                             §7Q4D4 = 'R' OR
077100031204     C                             §7Q4D5 = 'R'
077200031204     C                   ADD       1             I7QW
077300031204     C                   MOVEL     TBLKEY        SK7QW(I7QW)
077400031204     C                   ENDIF
077500031204     C                   ENDIF
077600031204     C     KTABP         READE     TABEL00F                               99
077700031204     C                   ENDDO
077800031204     C*--------------------
077900031204     C* CARICA TABELLA ESTENSINE GESTIONE PARTICOLARITA' GIACENZA (7W)
078000031204     C*--------------------
078100031204     C                   MOVEL     '7W'          COD
078200031204     C     KTABP         SETLL     TABEL00F                           99
078300031204     C     KTABP         READE     TABEL00F                               99
078400031204     C     *IN99         DOWEQ     *OFF
078500031204     C     TBLCOD        ANDEQ     '7W'
078600031204     C     TBLFLG        IFEQ      *BLANKS
078700031204     C                   MOVEL     TBLUNI        DS7W
078800031204     C                   IF        §7W6D1 = 'C' OR
078900031204     C                             §7W6D2 = 'C' OR
079000031204     C                             §7W6D3 = 'C' OR
079100031204     C                             §7W6D4 = 'C' OR
079200031204     C                             §7W6D5 = 'C'
079300031204     C                   ADD       1             I7QW
079400031204     C                   MOVEL     TBLKEY        SK7QW(I7QW)
079500031204     C                   ENDIF
079600031204     C                   ENDIF
079700031204     C     KTABP         READE     TABEL00F                               99
079800031204     C                   ENDDO
079900010509     C*
080000010509     C                   ENDSR
080100010509     C*---------------------------------------------------------------*
080200040227
080300040227
080400040227
080500040227     C*------------------------------------------------------------------------*
080600040227     C* CHIUDI - Routine di chiudura *pgm esterni
080700040227     C*------------------------------------------------------------------------*
080800040227     C     CHIUDI        BEGSR
080900040227     C*
081000040227     C                   CLEAR                   TISIT0DS
081100040227     C                   MOVEL     'C'           IT0TLA
081200040227     C                   CALL      'TISIT5R'
081300040227     C                   PARM                    TISIT0DS
081400040227     C*
081500040227     C                   ENDSR
081600040227     C*------------------------------------------------------------------------*
081700010509
081800010509
081900001218
082000001218     C*------------------------------------------------------------------------*
082100001218     C* *INZSR - ROUTINE INIZIALE
082200001218     C*------------------------------------------------------------------------*
082300001218     C     *INZSR        BEGSR
082400001218     C*
082500010726     C* Ricevo in input il parametro indicante l'ID LANCIO
082600010726     C     *ENTRY        PLIST
082700020613     C                   PARM                    KPJBA
082800020613     C                   MOVEL     KPJBU         PARAM
082900010726     C*
083000001218     C                   Z-ADD     1             CODUT
083100070323     C*
083200070323     C* VERIFICO I PARAMETRI D INPUT
083300070323     C                   IF        PARTID = 'S'
083400070323     C                   SETOFF                                       40
083500070323     C                   ENDIF
083600070323     C                   IF        PARTID = 'C'
083700070323     C                   SETON                                        40
083800070323     C                   ENDIF
083900071105     C                   IF        PARTIP = 'ST'
084000071105     C                   SETON                                        41
084100071105     C                   ELSE
084200071105     C                   SETOFF                                       41
084300071105     C                   ENDIF
084400001218     C*
084500001218     C* Definizioni chiavi
084600070323     C     KTAS26P       KLIST                                                  TITAS26C - Parziale
084700070323     C                   KFLD                    KtasDCM
084800071105     C                   KFLD                    KtasLNA
084900070323     C*
085000031001     C     KTAS38P       KLIST                                                  TITAS38C - Parziale
085100031001     C                   KFLD                    tasAAS
085200031001     C                   KFLD                    tasMGS
085300031001     C*
085400020613     C     KEVB04P       KLIST                                                  FNEVB04L - Parziale
085500020613     C                   KFLD                    tasAAS
085600020613     C                   KFLD                    tasLNP
085700020613     C                   KFLD                    tasNRS
085800020613     C                   KFLD                    tasNSP
085900031201     C*
086000050310     C     KGCP51P       KLIST                                                  TIGCP51L - Parziale
086100031201     C                   KFLD                    tasAAS
086200031201     C                   KFLD                    tasLNP
086300031201     C                   KFLD                    tasNRS
086400031201     C                   KFLD                    tasNSP
086500040408     C*
086600040531     C     KAR530P       KLIST                                                  FIAR531C - Parziale
086700040408     C                   KFLD                    tasAAS
086800040408     C                   KFLD                    tasLNP
086900040408     C                   KFLD                    tasNRS
087000040408     C                   KFLD                    tasNSP
087100040408     C                   KFLD                    ar5TRD
087200010509     C*
087300020614     C     KTABP         KLIST                                                  TABEL00F - Parziale
087400010509     C                   KFLD                    CODUT             1 0
087500010509     C                   KFLD                    COD               2
087600031001     C*
087700031001     C     KMAB01P       KLIST                                                  WFMAB01L - Parziale
087800070323     C                   KFLD                    KmabTID
087900031001     C                   KFLD                    KmabDAD
088000031001     C*
088100031001     C     KMAB01C       KLIST                                                  WFMAB01L - Completa
088200070323     C                   KFLD                    KmabTID
088300031001     C                   KFLD                    KmabDAD
088400031001     C                   KFLD                    KmabDAA
088500031001     C                   KFLD                    KmabDIV
088600031001     C                   KFLD                    KmabARE
088700031010     C                   KFLD                    KmabLNA
088800001218     C*
088900001218     C                   ENDSR
