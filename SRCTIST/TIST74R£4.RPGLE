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
011700081211     c**!!!                        or stptla16 > 0 or stptla23 > 0
011800071106     C                   SETON                                        70
011900071106     C                   ENDIF
012000071106     C*
012100071106     C                   IF        stpTDCRP > *zeros OR stpTDCRA > *zeros
012200071106     C                   SETON                                        71
012300071106     C                   ENDIF
012400071105     C*
012500071106     C                   write     ST74RI
012600071106     C                   write     ST74TT
012700071106     C                   write     ST74RI
012800071106     C                   write     ST74FI
012900071106     C                   close     TIST74P
013000071106     C                   endif
013100001218     C*
013200001218     C                   seton                                        LR
013300010509     C*---------------------------------------------------------------*
013400001218
013500001218
013600001218
013700001218     C*------------------------------------------------------------------------*
013800031001     C* PROCEDI - Routine principale
013900001218     C*------------------------------------------------------------------------*
014000020613     C     PROCEDI       BEGSR
014100070323     C*
014200070323     C                   SETOFF                                       30
014300001218     C*
014400070323     C* Mi posiziono sul periodo di elaborazione richiesto
014500070323     C                   Z-ADD     PARDAD        KtasDCM
014600070323     C                   Z-ADD     PARDAD        KtasDSP
014700071105     C   41              Z-ADD     PARLNA        KtasLNA
014800071105     C  N41              Z-ADD     *loval        KtasLNA
014900071105     C*
015000070323     C   40KTAS26P       SETLL     TITAS26C
015100070323     C  N40KTAS38P       SETLL     TITAS38C
015200020613     C*
015300020613     C* Se posizionamento riuscito => ciclo ed elaboro
015400070323     C                   IF        %found
015500031001     C                   MOVEL     '0'           flgFINE           1
015600020613     C*
015700020613     C* Prima lettura fuori ciclo
015800070323     C   40              READ      TITAS26C
015900070323     C  N40              READ      TITAS38C
016000070323     C                   DOW       *IN30 = *off AND
016100020614     C                             flgFINE = '0'
016200020613     C*
016300020613     C* Verifico se la bolla è da considerare
016400020613     C                   EXSR      CHKRECTAS
016500020613     C                   IF        flgRECOK = 'S'
016600040408     C*
016700040408     C* Determino subito se vi è 1 modifica in partenza o in arrivo
016800040408     C                   EXSR      REPPARARR
016900020613     C*
017000020613     C* Se la bolla è da considerare => elaboro e scrivo il WRK-FILE
017100020613     C                   EXSR      WRIWFILE
017200071105     C*
017300071105     C* Se lancio in stampa verifico se la bolla è da considerare
017400071105     C                   IF        *IN41 = *on
017500071105     C                   EXSR      CHKRECTASST
017600071105     C                   IF        flgRECOKST = 'S'
017700071105     C                   EXSR      EXESTAMPA
017800071105     C                   ENDIF
017900071105     C                   ENDIF
018000071105     C*
018100020613     C                   ENDIF
018200020613     C*
018300020613     C* Proseguo le letture dentro ciclo
018400070323     C   40              READ      TITAS26C
018500070323     C  N40              READ      TITAS38C
018600020613     C                   ENDDO
018700001218     C*
018800020613     C                   ENDIF
018900020613     C*
019000001218     C                   ENDSR
019100001218     C*------------------------------------------------------------------------*
019200071105
019300071105
019400071105
019500071105
019600071105     C*------------------------------------------------------------------------*
019700071105     C* EXESTAMPA - Routine di stampa dettaglio
019800071105     C*------------------------------------------------------------------------*
019900071105     C     EXESTAMPA     BEGSR
020000071105     C*
020100071105     C* Imposto i dati "fondamentali" della bolla (chiave, destinatario, etc...)
020200071105     C                   EVAL      stpKEY = %trim(%editw(tasAAS:'0    '))+' '+
020300071105     C                                      %trim(%editw(tasLNP:'0   ')) +' '+
020400071105     C                                      %trim(%editw(tasNRS:'0  '))  +' '+
020500071105     C                                      %trim(%editw(tasNSP:'0       '))
020600071105     C                   EVAL      stpRSD = tasRSD
020700071105     C*
020800071105     C* Effettuo considerazioni sui valori "calcolati"
020900071105     C                   EVAL      stpLANP = stpLAV + stpLANP
021000071105     C*
021100071105     C* Incremento i totali
021200071105     C                   EVAL      stpTLAV   = stpTLAV   + stpLAV
021300071105     C                   EVAL      stpTLANP  = stpTLANP  + stpLANP
021400071105     C                   EVAL      stpTFGC   = stpTFGC   + stpFGC
021500071105     C                   EVAL      stpTNNF   = stpTNNF   + stpNNF
021600071105     C                   EVAL      stpTFFDA  = stpTFFDA  + stpFFDA
021700071105     C                   EVAL      stpTGGCA  = stpTGGCA  + stpGGCA
021800071105     C                   EVAL      stpTDCRP  = stpTDCRP  + stpDCRP
021900071105     C                   EVAL      stpTDCRA  = stpTDCRA  + stpDCRA
022000071105     C                   EVAL      stpTSUP   = stpTSUP   + stpSUP
022100081211     c**!!!              eval      stptla16  = stptla16  + stpla16
022200081211     c**!!!              eval      stptla23  = stptla23  + stpla23
022300071106     C*
022400071106     C* Effettuo considerazioni su elementi d grafica in stampa
022500071106     C                   SETOFF                                       7071
022600071106     C                   IF        stpLAV  > *zeros OR stpLANP > *zeros
022700081211     c**!!!                        or stpla16 > 0 or stpla23 > 0
022800071106     C                   SETON                                        70
022900071106     C                   ENDIF
023000071106     C*
023100071106     C                   IF        stpDCRP > *zeros OR stpDCRA > *zeros
023200071106     C                   SETON                                        71
023300071106     C                   ENDIF
023400071105     C*
023500071105     C* Verifico il salto pagina
023600071105     C                   IF        *IN01 = *on
023700071105     C                   WRITE     ST74TE
023800071105     C                   SETOFF                                       01
023900071105     C                   WRITE     ST74DE
024000071105     C                   ELSE
024100071105     C                   WRITE     ST74DE
024200071105     C                   ENDIF
024300071105     C*
024400071105     C                   ENDSR
024500071105     C*------------------------------------------------------------------------*
024600031001
024700031001
024800031001
024900031001
025000010627     C*------------------------------------------------------------------------*
025100031001     C* CHKRECTAS - Routine di verifica bolla
025200010627     C*------------------------------------------------------------------------*
025300020613     C     CHKRECTAS     BEGSR
025400010627     C*
025500020613     C                   MOVEL     'S'           flgRECOK          1
025600020613     C*
025700031001     C* Considero solo le bolle valide come spedizione
025800031001     C                   IF        flgRECOK = 'S'
025900031001     C                   Z-ADD     1             J
026000031001     C                   SETOFF                                           28
026100031001     C     tasTBL        LOOKUP    TB(J)                                  28
026200031001     C     SP(J)         IFEQ      'N'
026300031001     C                   MOVEL     'N'           flgRECOK
026400031001     C                   ENDIF
026500031001     C                   ENDIF
026600031010     C*
026700031010     C* Escludo le bolle del network POSTE
026800031010     C                   IF        flgRECOK = 'S'
026900031010     C     tasLNP        CHAIN     AZORG01L
027000031010     C                   IF        %found(AZORG01L)
027100031010     C                   EVAL      OG143 = ORGDE3
027200031010     C                   IF        §OGNTW = 'PPT'
027300031010     C                   MOVEL     'N'           flgRECOK
027400031010     C                   ENDIF
027500031010     C                   ENDIF
027600031010     C                   ENDIF
027700020613     C*
027800020613     C* Testo se la bolla è del periodo richiesto
027900031001     C                   IF        flgRECOK = 'S'
028000070323     C                   IF        *IN40 = *off
028100070323     C                   IF        KtasDSP >= PARDAD AND
028200070323     C                             KtasDSP <= PARDAA
028300020613     C                   ELSE
028400020613     C                   MOVEL     'N'           flgRECOK
028500031001     C                   MOVEL     '1'           flgFINE
028600070323     C                   ENDIF
028700020613     C                   ENDIF
028800070323     C                   IF        *IN40 = *on
028900070323     C                   IF        tasDCM >= PARDAD AND
029000070323     C                             tasDCM <= PARDAA
029100070323     C                   ELSE
029200070323     C                   MOVEL     'N'           flgRECOK
029300070323     C                   MOVEL     '1'           flgFINE
029400070323     C                   ENDIF
029500031001     C                   ENDIF
029600070323     C                   ENDIF
029700070323     C*
029800070323     C* Verifico fine file
029900070323     C                   IF        *IN40 = *off
030000070323     C                   IF        %eof(TITAS38C)
030100070323     C                   SETON                                        30
030200070323     C                   ENDIF
030300070323     C                   ENDIF
030400070323     C                   IF        *IN40 = *on
030500070323     C                   IF        %eof(TITAS26C)
030600070323     C                   SETON                                        30
030700070323     C                   ENDIF
030800070323     C                   ENDIF
030900071105     C*
031000071105     C* Se lancio in stampa verifico anche fine filiale
031100071105     C                   IF        *IN41 = *on
031200080417     C                   IF        tasLNA <> PARLNA
031300080417     C                   MOVEL     'N'           flgRECOK
031400080417     C***                SETON                                        30
031500071105     C                   ENDIF
031600071105     C                   ENDIF
031700010627     C*
031800010627     C                   ENDSR
031900010627     C*------------------------------------------------------------------------*
032000071105
032100071105
032200071105
032300071105
032400071105     C*------------------------------------------------------------------------*
032500071105     C* CHKRECTASST - Routine di verifica bolla x stampa
032600071105     C*------------------------------------------------------------------------*
032700071105     C     CHKRECTASST   BEGSR
032800071105     C*
032900071105     C                   MOVEL     'N'           flgRECOKST        1
033000071105     C*
033100071105     C* Filtro x se nel lancio richiesti L/AVV.
033200071105     C                   IF        flgRECOKST = 'N'
033300071105     C                   IF        wFlgLAV = 'S' and PARLAV = 'S'
033400071105     C                   MOVEL     'S'           flgRECOKST
033500071105     C                   ENDIF
033600071105     C                   ENDIF
033700071105     C*
033800071105     C* Filtro x se nel lancio richiesti Aperture Giacienze
033900071105     C                   IF        flgRECOKST = 'N'
034000071105     C                   IF        wFlgFGC = 'S' and PARFGC = 'S'
034100071105     C                   MOVEL     'S'           flgRECOKST
034200071105     C                   ENDIF
034300071105     C                   ENDIF
034400071105     C*
034500071105     C* Filtro x se nel lancio richiesti Non Fatte
034600071105     C                   IF        flgRECOKST = 'N'
034700071105     C                   IF        wFlgNNF = 'S' and PARNNF = 'S'
034800071105     C                   MOVEL     'S'           flgRECOKST
034900071105     C                   ENDIF
035000071105     C                   ENDIF
035100071105     C*
035200071105     C* Filtro x se nel lancio richiesti Fermi Deposito in Arrivo
035300071105     C                   IF        flgRECOKST = 'N'
035400071105     C                   IF        wFlgFFDA = 'S' and PARFFDA = 'S'
035500071105     C                   MOVEL     'S'           flgRECOKST
035600071105     C                   ENDIF
035700071105     C                   ENDIF
035800071105     C*
035900071105     C* Filtro x se nel lancio richiesti Turni Chiusura in Arrivo
036000071105     C                   IF        flgRECOKST = 'N'
036100071105     C                   IF        wFlgGGCA = 'S' and PARGGCA = 'S'
036200071105     C                   MOVEL     'S'           flgRECOKST
036300071105     C                   ENDIF
036400071105     C                   ENDIF
036500071105     C*
036600071105     C* Filtro x se nel lancio richiesti Date Consegna Richieste
036700071105     C                   IF        flgRECOKST = 'N'
036800071105     C                   IF        wFlgDCR = 'S' and PARDCR = 'S'
036900071105     C                   MOVEL     'S'           flgRECOKST
037000071105     C                   ENDIF
037100071105     C                   ENDIF
037200071105     C*
037300071105     C* Filtro x se nel lancio richiesti Supermercati
037400071105     C                   IF        flgRECOKST = 'N'
037500071105     C                   IF        wFlgSUP = 'S' and PARSUP = 'S'
037600071105     C                   MOVEL     'S'           flgRECOKST
037700071105     C                   ENDIF
037800071105     C                   ENDIF
037900071105     C*
038000071105     C* Filtro x lina di arrivo (lasciare come ultimissimo test in questa routine)!!!!
038100071105     C                   IF        flgRECOKST = 'S'
038200071105     C                   IF        tasLNA <> PARLNA
038300071105     C                   MOVEL     'N'           flgRECOKST
038400071105     C                   ENDIF
038500071105     C                   ENDIF
038600071105     C*
038700071105     C                   ENDSR
038800071105     C*------------------------------------------------------------------------*
038900040408
039000040408
039100040408
039200040408
039300040408     C*------------------------------------------------------------------------*
039400040408     C* REPPARARR - Routine di verifica modifiche bolla in partenza o in arrivo
039500040408     C*------------------------------------------------------------------------*
039600040408     C     REPPARARR     BEGSR
039700040408     C*
039800040408     C* (*IN60 = *ON  => in arrivo)
039900040408     C* (*IN60 = *OFF => in partenza)
040000040408     C                   EVAL      *IN60 = *OFF
040100040408     C*
040200040408     C                   EVAL      ar5TRD = 'GEN'
040300040531     C     KAR530P       CHAIN     fiar531c
040400040531     C                   IF        %found(fiar531c)
040500040408     C                   EVAL      DAR5GEN = ar5UNI
040600040408     C                   IF        §AR5MDP <> *blanks
040700040408     C                   EVAL      *IN60 = *ON
040800040408     C                   ENDIF
040900040408     C                   ENDIF
041000040408     C*
041100040408     C                   ENDSR
041200040408     C*------------------------------------------------------------------------*
041300001218
041400001218
041500010509
041600010627     C*------------------------------------------------------------------------*
041700031001     C* WRIWFILE - Routine di reperimento informazioni e scrittura WRK-FILE
041800010627     C*------------------------------------------------------------------------*
041900020613     C     WRIWFILE      BEGSR
042000071105     C*
042100071105     C* Inizializzo flag d wrk
042200071105     C                   MOVEL     'N'           wFlgLAV           1
042300071105     C                   MOVEL     'N'           wFlgFGC           1
042400071105     C                   MOVEL     'N'           wFlgNNF           1
042500071105     C                   MOVEL     'N'           wFlgFFDA          1
042600071105     C                   MOVEL     'N'           wFlgGGCA          1
042700071105     C                   MOVEL     'N'           wFlgDCR           1
042800071105     C                   MOVEL     'N'           wFlgSUP           1
042900071105     C*
043000071105     C* Se lancio in stampa => inizializzo campi in stampa
043100071105     C   41              CLEAR                   ST74DE
043200031001     C*
043300031010     C* Reperisco distretto e area relativamente alla LNA della bolla
043400031010     C     tasLNA        CHAIN     AZORG01L
043500031001     C                   IF        %found(AZORG01L)
043600031001     C                   EVAL      KmabDIV = ORGFL3
043700031001     C                   EVAL      KmabARE = ORGCAR
043800031001     C                   ENDIF
043900031001     C*
044000031001     C* Valorizzo gli altri campi chiave del wrk-file
044100070323     C                   EVAL      KmabTID = PARTID
044200031001     C                   EVAL      KmabDAD = PARDAD
044300031001     C                   EVAL      KmabDAA = PARDAA
044400031010     C                   EVAL      KmabLNA = tasLNA
044500031001     C*
044600031010     C* Inizializzo il buffer del wrk-file e verifico se già esiste un record
044700031010     C* con medesima chiave (x decidere se andare in WRITE o in UPDATE)
044800031010     C                   CLEAR                   WFMAB000
044900031001     C                   SETOFF                                       50
045000031001     C     KMAB01C       CHAIN     WFMAB01L
045100031001     C                   IF        %found(WFMAB01L)
045200031001     C                   SETON                                        50
045300031001     C                   ELSE
045400070323     C                   EVAL      mabTID = KmabTID
045500031001     C                   EVAL      mabDAD = KmabDAD
045600031001     C                   EVAL      mabDAA = KmabDAA
045700031001     C                   EVAL      mabDIV = KmabDIV
045800031001     C                   EVAL      mabARE = KmabARE
045900031010     C                   EVAL      mabLNA = KmabLNA
046000031001     C                   ENDIF
046100031204     C*
046200031204     C* Eseguo considerazioni relativamente alle particolarità giacenza
046300031204     C                   IF        tasGGA = *BLANKS
046400031204     C                   SETOFF                                       54
046500031204     C                   ELSE
046600031204     C     tasGGA        LOOKUP    SK7QW                                  54
046700031204     C                   ENDIF
046800031001     C*
046900031001     C* Eseguo considerazioni relativamente agli eventi della bolla corrente
047000031001     C     KEVB04P       SETLL     FNEVB04L
047100031001     C                   IF        %equal(FNEVB04L)
047200031001     C     KEVB04P       READE     FNEVB04L
047300031001     C                   DOW       not %eof(FNEVB04L)
047400071127     C*
047500071127     C* Verifico se trattasi d evento "in carico" alla filiale in elaborazione
047600071127     C                   SETOFF                                       45
047700071127     C                   IF        evbFLE = tasLNA
047800071127     C                   SETON                                        45
047900071127     C                   ELSE
048000071127     C                   CLEAR                   FNLV55DS
048100071128     C                   EVAL      D55tpt = '6'
048200071128     C                   EVAL      D55lin = tasLNA
048300071128     C                   EVAL      D55drf = evbDEV
048400071127     C                   CALL      'FNLV55R'
048500071127     C                   PARM                    FNLV55ds
048600071128     C                   IF        D55err = *blanks
048700071128     C                   IF        D55tfa = evbFLE
048800071127     C                   SETON                                        45
048900071127     C                   ENDIF
049000071127     C                   ENDIF
049100071127     C                   ENDIF
049200071127     C*
049300071127     C                   IF        *IN45 = *on
049400031010     C* Determino il tipo di evento (se "Lasciato avviso" oppure "Giacenza")
049500031001     C     evbCEV        LOOKUP    SK2AA                                  55    * evento L/AVV
049600031001     C                   IF        %found
049700071105     C                   MOVEL     'S'           wFlgLAV
049800071105     C                   ADD       1             stpLAV
049900031001     C   50              ADD       1             mabLAV
050000031001     C  N50              Z-ADD     1             mabLAV
050100031001     C                   IF        %subst(tasGVA:1:1) = 'P'                     * L/AVV a privati
050200071105     C                   SUB       1             stpLANP
050300031001     C   50              ADD       1             mabLAP
050400031001     C  N50              Z-ADD     1             mabLAP
050500031001     C                   ENDIF
050600080728      * se rimanda allo svincolo (A16)
050700081211     c**!!!              if        evbcev = 'A16'
050800081211     c**!!!              add       1             stpla16
050900081211     c*! 50              add       1             mabla16
051000081211     c*!n50              z-add     1             mabla16
051100081211     c**!!!              endif
051200080728      * se destinatario chiuso fino al (A23)
051300081211     c**!!!              if        evbcev = 'A23'
051400081211     c**!!!              add       1             stpla23
051500081211     c*! 50              add       1             mabla23
051600081211     c*!n50              z-add     1             mabla16
051700081211     c**!!!              endif
051800031001     C                   ENDIF
051900031204     C* A seconda che la bolla abbia o meno le particolarità giacenza "NO GIACENZA"
052000031204     C* conteggio gli eventi d tipo giacenza in campi differenti.
052100031001     C     evbCEV        LOOKUP    SK2AG                                  55    * evento GIAC.
052200031001     C                   IF        %found
052300031204     C                   IF        *IN54 = *OFF
052400031010     C   50              ADD       1             mabGIA
052500031010     C  N50              Z-ADD     1             mabGIA
052600031204     C                   ELSE
052700031204     C   50              ADD       1             mabENG
052800031204     C  N50              Z-ADD     1             mabENG
052900031001     C                   ENDIF
053000031204     C                   ENDIF
053100031125     C     evbCEV        LOOKUP    SK2AN                                  55    * evento NN FATTA
053200031125     C                   IF        %found
053300071105     C                   MOVEL     'S'           wFlgNNF
053400071105     C                   ADD       1             stpNNF
053500031125     C   50              ADD       1             mabNNF
053600031125     C  N50              Z-ADD     1             mabNNF
053700031125     C                   ENDIF
053800071127     C*
053900071127     C                   ENDIF
054000071127     C*
054100031001     C     KEVB04P       READE     FNEVB04L
054200031001     C                   ENDDO
054300031001     C                   ENDIF
054400031201     C*
054500031201     C* Eseguo considerazioni relativamente alle aperture/riaperture giacenza
054600050310     C     KGCP51P       SETLL     TIGCP51L
054700050310     C                   IF        %equal(TIGCP51L)
054800050310     C     KGCP51P       READE     TIGCP51L
054900050310     C                   DOW       not %eof(TIGCP51L)
055000071105     C                   MOVEL     'S'           wFlgFGC
055100071105     C                   ADD       1             stpFGC
055200031201     C   50              ADD       1             mabFGC
055300031201     C  N50              Z-ADD     1             mabFGC
055400050310     C     KGCP51P       READE     TIGCP51L
055500031201     C                   ENDDO
055600031201     C                   ENDIF
055700040227     C*
055800031001     C                   IF        tasGC1 <> *blanks OR                         * turni chiusura
055900031001     C                             tasGC2 <> *blanks
056000040408     C                   IF        *IN60 = *ON
056100040408     C                   IF        tasGC1 <> §AR5GC1 OR
056200040408     C                             tasGC2 <> §AR5GC2
056300071105     C                   MOVEL     'S'           wFlgGGCA
056400071105     C                   ADD       1             stpGGCA
056500040408     C   50              ADD       1             mabGGCA
056600040408     C  N50              Z-ADD     1             mabGGCA
056700040408     C                   ELSE
056800040408     C   50              ADD       1             mabGGCP
056900040408     C  N50              Z-ADD     1             mabGGCP
057000040408     C                   ENDIF
057100040408     C                   ELSE
057200040408     C   50              ADD       1             mabGGCP
057300040408     C  N50              Z-ADD     1             mabGGCP
057400031001     C                   ENDIF
057500040408     C                   ENDIF
057600040227     C*
057700040408     C                   IF        tasFFD = 'S'
057800040408     C                   IF        *IN60 = *ON
057900040408     C                   IF        tasFFD <> §AR5FFD
058000071105     C                   MOVEL     'S'           wFlgFFDA
058100071105     C                   ADD       1             stpFFDA
058200040408     C   50              ADD       1             mabFDA
058300040408     C  N50              Z-ADD     1             mabFDA
058400040408     C                   ELSE
058500040408     C   50              ADD       1             mabFDP
058600040408     C  N50              Z-ADD     1             mabFDP
058700040408     C                   ENDIF
058800040408     C                   ELSE
058900031001     C   50              ADD       1             mabFDP
059000031001     C  N50              Z-ADD     1             mabFDP
059100031001     C                   ENDIF
059200040408     C                   ENDIF
059300040227     C*
059400040227     C                   IF        tasFTC = 'S' OR                              * consegna a superm.
059500040227     C                             tasTC2 = 'S'
059600071105     C                   MOVEL     'S'           wFlgSUP
059700071105     C                   ADD       1             stpSUP
059800040408     C*
059900040408     C                   CLEAR                   TISIT0DS                       * consegna a superm.
060000051227     C                   MOVEL     'E'           IT0TLA                         * in anagraf. dest.
060100051227     C                   MOVEL     tasNZD        IT0NAZ                         * particolari
060200040408     C                   MOVEL     tasPRD        IT0PRV
060300040408     C                   MOVEL     tasCAD        IT0CAP
060400040408     C                   MOVEL     tasLOD        IT0LOC
060500040408     C                   MOVEL     tasIND        IT0IND
060600040408     C                   MOVEL     tasRSD        IT0RAG
060700040408     C                   CALL      'TISIT5R'
060800040408     C                   PARM                    TISIT0DS
060900040408     C*
061000040408     C                   IF        OT0ERR = *blanks AND
061100040408     C                             OT0DOS = 'S'
061200040408     C                   IF        *IN60 = *ON
061300040408     C                   IF        tasFTC <> §AR5TC1 OR
061400040408     C                             tasTC2 <> §AR5TC2
061500040408     C   50              ADD       1             mabSUPA
061600040408     C  N50              Z-ADD     1             mabSUPA
061700040408     C                   ELSE
061800040408     C   50              ADD       1             mabSUPP
061900040408     C  N50              Z-ADD     1             mabSUPP
062000040408     C                   ENDIF
062100040408     C                   ELSE
062200040408     C   50              ADD       1             mabSUPP
062300040408     C  N50              Z-ADD     1             mabSUPP
062400040408     C                   ENDIF
062500040408     C*
062600040408     C                   ELSE
062700040408     C                   IF        *IN60 = *ON
062800040408     C                   IF        tasFTC <> §AR5TC1 OR
062900040408     C                             tasTC2 <> §AR5TC2
063000040408     C   50              ADD       1             mabSPSA
063100040408     C  N50              Z-ADD     1             mabSPSA
063200040408     C                   ELSE
063300040408     C   50              ADD       1             mabSPSP
063400040408     C  N50              Z-ADD     1             mabSPSP
063500040408     C                   ENDIF
063600040408     C                   ELSE
063700040408     C   50              ADD       1             mabSPSP
063800040408     C  N50              Z-ADD     1             mabSPSP
063900040408     C                   ENDIF
064000040408     C                   ENDIF
064100040408     C                   ENDIF
064200040408     C*
064300040408     C                   IF        tasDCR > *zeros
064400071105     C                   MOVEL     'S'           wFlgDCR
064500040408     C                   IF        *IN60 = *ON
064600071105     C                   ADD       1             stpDCRA
064700040408     C                   MOVEL     §AR5DCR       Num§AR5DCR        8 0
064800040409     C                   IF        tasDCR <> Num§AR5DCR
064900040408     C   50              ADD       1             mabDCRA
065000040408     C  N50              Z-ADD     1             mabDCRA
065100040408     C                   ELSE
065200040408     C   50              ADD       1             mabDCRP
065300040408     C  N50              Z-ADD     1             mabDCRP
065400040408     C                   ENDIF
065500040408     C                   ELSE
065600071105     C                   ADD       1             stpDCRP
065700040408     C   50              ADD       1             mabDCRP
065800040408     C  N50              Z-ADD     1             mabDCRP
065900040408     C                   ENDIF
066000040408     C                   ENDIF
066100031001     C*
066200031001     C* Conteggio tutte le bolle elaborate
066300071105     C                   ADD       1             stpTBOL
066400031001     C   50              ADD       1             mabTOT
066500031001     C  N50              Z-ADD     1             mabTOT
066600020613     C*
066700071105     C*
066800071105     C* Se lancio x creazione wrk-file....
066900071105     C                   IF        *IN41 = *off
067000031001     C* Al termine scrivo o update il record così valorizzato
067100031001     C   50              UPDATE    WFMAB000
067200031001     C  N50              WRITE     WFMAB000
067300071105     C                   ENDIF
067400020613     C*
067500010627     C                   ENDSR
067600010509     C*------------------------------------------------------------------------*
067700031001
067800031001
067900031001
068000031001     C*------------------------------------------------------------------------*
068100031107     C* DELWFILE - Routine di cancellazione completa del file.
068200031001     C*------------------------------------------------------------------------*
068300031001     C     DELWFILE      BEGSR
068400031001     C*
068500031107     C     *LOVAL        SETLL     WFMAB01L
068600031107     C                   READ      WFMAB01L
068700031107     C                   DOW       not %eof(WFMAB01L)
068800031107     C                   DELETE    WFMAB000
068900031107     C                   READ      WFMAB01L
069000031107     C                   ENDDO
069100031001     C*
069200031001     C                   ENDSR
069300031001     C*------------------------------------------------------------------------*
069400010626
069500010626
069600010626
069700010509     C*------------------------------------------------------------------------*
069800020613     C* CARTBL - Routine di caricamento dati tabellati
069900010509     C*------------------------------------------------------------------------*
070000010509     C     CARTBL        BEGSR
070100010509     C*
070200031001     C*--------------------
070300031001     C* DISTINGUO EVENTI DI TIPO "GIACENZA" ED EVENTI DI TIPO "LASCIATO AVVISO"
070400031001     C*--------------------
070500020614     C                   Z-ADD     0             WA                4 0
070600020614     C                   Z-ADD     0             WG                4 0
070700020614     C                   CLEAR                   SK2AA
070800020614     C                   CLEAR                   SK2AG
070900010509     C                   MOVEL     '2A'          COD
071000020614     C     KTABP         CHAIN     TABEL00F                           31
071100010509     C     *IN31         DOWEQ     '0'
071200010509     C     TBLFLG        IFEQ      ' '
071300010509     C                   MOVEL     TBLUNI        DS2A
071400020613     C     §2AFTC        IFEQ      'A'
071500020614     C                   ADD       1             WA
071600020614     C                   MOVEL     TBLKEY        SK2AA(WA)
071700010509     C                   ENDIF
071800020614     C     §2AFTC        IFEQ      'G'
071900020614     C                   ADD       1             WG
072000020614     C                   MOVEL     TBLKEY        SK2AG(WG)
072100020614     C                   ENDIF
072200031125     C     §2AFTC        IFEQ      'N'
072300031125     C                   ADD       1             WG
072400031125     C                   MOVEL     TBLKEY        SK2AN(WG)
072500031125     C                   ENDIF
072600010509     C                   ENDIF
072700020614     C     KTABP         READE     TABEL                                  31
072800010509     C                   ENDDO
072900031001     C*--------------------
073000031001     C* FLAG SPEDIZIONI /RICAVI PER RAGGRUPPAMENTO TIPO BOLLA
073100031001     C*--------------------
073200031001     C                   Z-ADD     0             I                 3 0
073300031001     C                   MOVEL     '2W'          COD
073400031001     C     KTABP         SETLL     TABEL00F
073500031001     C     KTABP         READE     TABEL00F                               99
073600031001     C     *IN99         DOWEQ     *OFF
073700031001     C     TBLCOD        ANDEQ     '2W'
073800031001     C     TBLFLG        IFEQ      *BLANKS
073900031001     C                   ADD       1             I
074000031001     C                   MOVEL     TBLKEY        RBL(I)                         *RAGGRUPPAMENTO BOLL
074100031001     C                   MOVEL     TBLUNI        DS2W
074200031001     C                   MOVEL     §2WFSP        FSP(I)                         *FLAG SPEDIZIONE
074300031001     C                   MOVEL     §2WFRI        FRI(I)                         *FLAG RICAVO
074400031001     C                   ENDIF
074500031001     C     KTABP         READE     TABEL00F                               99
074600031001     C                   ENDDO
074700031001     C*--------------------
074800031001     C* TIPI BOLLA E RELATIVI FLAGS SPEDIZIONE/RICAVO
074900031001     C*--------------------
075000031001     C                   Z-ADD     0             I
075100031001     C                   MOVEL     'TB'          COD
075200031001     C     KTABP         SETLL     TABEL00F
075300031001     C     KTABP         READE     TABEL00F                               99
075400031001     C     *IN99         DOWEQ     *OFF
075500031001     C     TBLCOD        ANDEQ     'TB'
075600031001     C     TBLFLG        IFEQ      *BLANKS
075700031001     C                   ADD       1             I
075800031001     C                   MOVEL     TBLKEY        TB(I)                          *TIPO BOLLA
075900031001     C                   MOVEL     TBLUNI        DSTB
076000031001     C                   Z-ADD     1             J                 4 0
076100031001     C                   SETOFF                                       28
076200031001     C     §TBRBL        LOOKUP    RBL(J)                                 28
076300031001     C     *IN28         IFEQ      *ON
076400031001     C                   MOVEL     FSP(J)        SP(I)                          *FLAG SPEDIZIONE
076500031001     C                   MOVEL     FRI(J)        RI(I)                          *FLAG RICAVO
076600031001     C                   ELSE
076700031001     C                   MOVEL     'N'           SP(I)
076800031001     C                   MOVEL     'N'           RI(I)
076900031001     C                   ENDIF
077000031001     C                   ENDIF
077100031001     C     KTABP         READE     TABEL00F                               99
077200031001     C                   ENDDO
077300031204     C*--------------------
077400031204     C* CARICA TABELLA GESTIONE PARTICOLARITA' GIACENZA (7Q)
077500031204     C*--------------------
077600031204     C                   Z-ADD     0             I7QW              2 0
077700031204     C                   MOVEL     '7Q'          COD
077800031204     C     KTABP         SETLL     TABEL00F                           99
077900031204     C     KTABP         READE     TABEL00F                               99
078000031204     C     *IN99         DOWEQ     *OFF
078100031204     C     TBLCOD        ANDEQ     '7Q'
078200031204     C     TBLFLG        IFEQ      *BLANKS
078300031204     C                   MOVEL     TBLUNI        DS7Q
078400031204     C                   IF        §7Q4D1 = 'R' OR
078500031204     C                             §7Q4D2 = 'R' OR
078600031204     C                             §7Q4D3 = 'R' OR
078700031204     C                             §7Q4D4 = 'R' OR
078800031204     C                             §7Q4D5 = 'R'
078900031204     C                   ADD       1             I7QW
079000031204     C                   MOVEL     TBLKEY        SK7QW(I7QW)
079100031204     C                   ENDIF
079200031204     C                   ENDIF
079300031204     C     KTABP         READE     TABEL00F                               99
079400031204     C                   ENDDO
079500031204     C*--------------------
079600031204     C* CARICA TABELLA ESTENSINE GESTIONE PARTICOLARITA' GIACENZA (7W)
079700031204     C*--------------------
079800031204     C                   MOVEL     '7W'          COD
079900031204     C     KTABP         SETLL     TABEL00F                           99
080000031204     C     KTABP         READE     TABEL00F                               99
080100031204     C     *IN99         DOWEQ     *OFF
080200031204     C     TBLCOD        ANDEQ     '7W'
080300031204     C     TBLFLG        IFEQ      *BLANKS
080400031204     C                   MOVEL     TBLUNI        DS7W
080500031204     C                   IF        §7W6D1 = 'C' OR
080600031204     C                             §7W6D2 = 'C' OR
080700031204     C                             §7W6D3 = 'C' OR
080800031204     C                             §7W6D4 = 'C' OR
080900031204     C                             §7W6D5 = 'C'
081000031204     C                   ADD       1             I7QW
081100031204     C                   MOVEL     TBLKEY        SK7QW(I7QW)
081200031204     C                   ENDIF
081300031204     C                   ENDIF
081400031204     C     KTABP         READE     TABEL00F                               99
081500031204     C                   ENDDO
081600010509     C*
081700010509     C                   ENDSR
081800010509     C*---------------------------------------------------------------*
081900040227
082000040227
082100040227
082200040227     C*------------------------------------------------------------------------*
082300040227     C* CHIUDI - Routine di chiudura *pgm esterni
082400040227     C*------------------------------------------------------------------------*
082500040227     C     CHIUDI        BEGSR
082600040227     C*
082700040227     C                   CLEAR                   TISIT0DS
082800040227     C                   MOVEL     'C'           IT0TLA
082900040227     C                   CALL      'TISIT5R'
083000040227     C                   PARM                    TISIT0DS
083100040227     C*
083200040227     C                   ENDSR
083300040227     C*------------------------------------------------------------------------*
083400010509
083500010509
083600001218
083700001218     C*------------------------------------------------------------------------*
083800001218     C* *INZSR - ROUTINE INIZIALE
083900001218     C*------------------------------------------------------------------------*
084000001218     C     *INZSR        BEGSR
084100001218     C*
084200010726     C* Ricevo in input il parametro indicante l'ID LANCIO
084300010726     C     *ENTRY        PLIST
084400020613     C                   PARM                    KPJBA
084500020613     C                   MOVEL     KPJBU         PARAM
084600010726     C*
084700001218     C                   Z-ADD     1             CODUT
084800070323     C*
084900070323     C* VERIFICO I PARAMETRI D INPUT
085000070323     C                   IF        PARTID = 'S'
085100070323     C                   SETOFF                                       40
085200070323     C                   ENDIF
085300070323     C                   IF        PARTID = 'C'
085400070323     C                   SETON                                        40
085500070323     C                   ENDIF
085600071105     C                   IF        PARTIP = 'ST'
085700071105     C                   SETON                                        41
085800071105     C                   ELSE
085900071105     C                   SETOFF                                       41
086000071105     C                   ENDIF
086100001218     C*
086200001218     C* Definizioni chiavi
086300070323     C     KTAS26P       KLIST                                                  TITAS26C - Parziale
086400070323     C                   KFLD                    KtasDCM
086500071105     C                   KFLD                    KtasLNA
086600070323     C*
086700031001     C     KTAS38P       KLIST                                                  TITAS38C - Parziale
086800031001     C                   KFLD                    tasAAS
086900031001     C                   KFLD                    tasMGS
087000031001     C*
087100020613     C     KEVB04P       KLIST                                                  FNEVB04L - Parziale
087200020613     C                   KFLD                    tasAAS
087300020613     C                   KFLD                    tasLNP
087400020613     C                   KFLD                    tasNRS
087500020613     C                   KFLD                    tasNSP
087600031201     C*
087700050310     C     KGCP51P       KLIST                                                  TIGCP51L - Parziale
087800031201     C                   KFLD                    tasAAS
087900031201     C                   KFLD                    tasLNP
088000031201     C                   KFLD                    tasNRS
088100031201     C                   KFLD                    tasNSP
088200040408     C*
088300040531     C     KAR530P       KLIST                                                  FIAR531C - Parziale
088400040408     C                   KFLD                    tasAAS
088500040408     C                   KFLD                    tasLNP
088600040408     C                   KFLD                    tasNRS
088700040408     C                   KFLD                    tasNSP
088800040408     C                   KFLD                    ar5TRD
088900010509     C*
089000020614     C     KTABP         KLIST                                                  TABEL00F - Parziale
089100010509     C                   KFLD                    CODUT             1 0
089200010509     C                   KFLD                    COD               2
089300031001     C*
089400031001     C     KMAB01P       KLIST                                                  WFMAB01L - Parziale
089500070323     C                   KFLD                    KmabTID
089600031001     C                   KFLD                    KmabDAD
089700031001     C*
089800031001     C     KMAB01C       KLIST                                                  WFMAB01L - Completa
089900070323     C                   KFLD                    KmabTID
090000031001     C                   KFLD                    KmabDAD
090100031001     C                   KFLD                    KmabDAA
090200031001     C                   KFLD                    KmabDIV
090300031001     C                   KFLD                    KmabARE
090400031010     C                   KFLD                    KmabLNA
090500001218     C*
090600001218     C                   ENDSR
