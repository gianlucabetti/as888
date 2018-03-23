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
008800090206      *------------------
008900090206      * ds per campo evbnot file eventi
009000090206      *------------------
009100090206     d dsevbnot        ds
009200090206     d  dsndc                         7
009300090206     d  dsaut                         7
009400010606     C*---------------------------------------------------------------*
009500001218
009600001218
009700001218
009800920812     C*---------------------------------------------------------------*
009900001218     C* MAIN
010000071105     C*---------------------------------------------------------------*
010100071105     C*
010200071105     C* Se richiesta stampa
010300071105     C                   if        *in41 = *on
010400071105     C                   open      TIST74P
010500071105     C                   eval      stpDAI  = PARDADINV
010600071129     C                   eval      stpDAF  = PARDAAINV
010700071105     C                   eval      stpFIL  = PARLNA
010800071105     C                   eval      stpFILD = PARLNADES
010900071106     C                   eval      DSFIRS  = PARRSUT
011000071105     C                   write     ST74TE
011100071105     C                   endif
011200071105     C*
011300010509     C                   exsr      cartbl
011400071105     C  N41              exsr      delwfile
011500001218     C                   exsr      procedi
011600040227     C                   exsr      chiudi
011700071106     C*
011800071106     C* Se richiesta stampa effettuo considerazioni su elementi d grafica in stampa
011900071106     C                   if        *in41 = *on
012000071106     C*
012100071106     C                   SETOFF                                       7071
012200071106     C                   IF        stpTLAV  > *zeros OR stpTLANP > *zeros
012300081211     c**!!!                        or stptla16 > 0 or stptla23 > 0
012400071106     C                   SETON                                        70
012500071106     C                   ENDIF
012600071106     C*
012700071106     C                   IF        stpTDCRP > *zeros OR stpTDCRA > *zeros
012800071106     C                   SETON                                        71
012900071106     C                   ENDIF
013000071105     C*
013100071106     C                   write     ST74RI
013200071106     C                   write     ST74TT
013300071106     C                   write     ST74RI
013400071106     C                   write     ST74FI
013500071106     C                   close     TIST74P
013600071106     C                   endif
013700001218     C*
013800001218     C                   seton                                        LR
013900010509     C*---------------------------------------------------------------*
014000001218
014100001218
014200001218
014300001218     C*------------------------------------------------------------------------*
014400031001     C* PROCEDI - Routine principale
014500001218     C*------------------------------------------------------------------------*
014600020613     C     PROCEDI       BEGSR
014700070323     C*
014800070323     C                   SETOFF                                       30
014900001218     C*
015000070323     C* Mi posiziono sul periodo di elaborazione richiesto
015100070323     C                   Z-ADD     PARDAD        KtasDCM
015200070323     C                   Z-ADD     PARDAD        KtasDSP
015300071105     C   41              Z-ADD     PARLNA        KtasLNA
015400071105     C  N41              Z-ADD     *loval        KtasLNA
015500071105     C*
015600070323     C   40KTAS26P       SETLL     TITAS26C
015700070323     C  N40KTAS38P       SETLL     TITAS38C
015800020613     C*
015900020613     C* Se posizionamento riuscito => ciclo ed elaboro
016000070323     C                   IF        %found
016100031001     C                   MOVEL     '0'           flgFINE           1
016200020613     C*
016300020613     C* Prima lettura fuori ciclo
016400070323     C   40              READ      TITAS26C
016500070323     C  N40              READ      TITAS38C
016600070323     C                   DOW       *IN30 = *off AND
016700020614     C                             flgFINE = '0'
016800020613     C*
016900020613     C* Verifico se la bolla è da considerare
017000020613     C                   EXSR      CHKRECTAS
017100020613     C                   IF        flgRECOK = 'S'
017200040408     C*
017300040408     C* Determino subito se vi è 1 modifica in partenza o in arrivo
017400040408     C                   EXSR      REPPARARR
017500020613     C*
017600020613     C* Se la bolla è da considerare => elaboro e scrivo il WRK-FILE
017700020613     C                   EXSR      WRIWFILE
017800071105     C*
017900071105     C* Se lancio in stampa verifico se la bolla è da considerare
018000071105     C                   IF        *IN41 = *on
018100071105     C                   EXSR      CHKRECTASST
018200071105     C                   IF        flgRECOKST = 'S'
018300071105     C                   EXSR      EXESTAMPA
018400071105     C                   ENDIF
018500071105     C                   ENDIF
018600071105     C*
018700020613     C                   ENDIF
018800020613     C*
018900020613     C* Proseguo le letture dentro ciclo
019000070323     C   40              READ      TITAS26C
019100070323     C  N40              READ      TITAS38C
019200020613     C                   ENDDO
019300001218     C*
019400020613     C                   ENDIF
019500020613     C*
019600001218     C                   ENDSR
019700001218     C*------------------------------------------------------------------------*
019800071105
019900071105
020000071105
020100071105
020200071105     C*------------------------------------------------------------------------*
020300071105     C* EXESTAMPA - Routine di stampa dettaglio
020400071105     C*------------------------------------------------------------------------*
020500071105     C     EXESTAMPA     BEGSR
020600071105     C*
020700071105     C* Imposto i dati "fondamentali" della bolla (chiave, destinatario, etc...)
020800071105     C                   EVAL      stpKEY = %trim(%editw(tasAAS:'0    '))+' '+
020900071105     C                                      %trim(%editw(tasLNP:'0   ')) +' '+
021000071105     C                                      %trim(%editw(tasNRS:'0  '))  +' '+
021100071105     C                                      %trim(%editw(tasNSP:'0       '))
021200071105     C                   EVAL      stpRSD = tasRSD
021300071105     C*
021400071105     C* Effettuo considerazioni sui valori "calcolati"
021500071105     C                   EVAL      stpLANP = stpLAV + stpLANP
021600071105     C*
021700071105     C* Incremento i totali
021800071105     C                   EVAL      stpTLAV   = stpTLAV   + stpLAV
021900071105     C                   EVAL      stpTLANP  = stpTLANP  + stpLANP
022000071105     C                   EVAL      stpTFGC   = stpTFGC   + stpFGC
022100071105     C                   EVAL      stpTNNF   = stpTNNF   + stpNNF
022200071105     C                   EVAL      stpTFFDA  = stpTFFDA  + stpFFDA
022300071105     C                   EVAL      stpTGGCA  = stpTGGCA  + stpGGCA
022400071105     C                   EVAL      stpTDCRP  = stpTDCRP  + stpDCRP
022500071105     C                   EVAL      stpTDCRA  = stpTDCRA  + stpDCRA
022600071105     C                   EVAL      stpTSUP   = stpTSUP   + stpSUP
022700081211     c**!!!              eval      stptla16  = stptla16  + stpla16
022800081211     c**!!!              eval      stptla23  = stptla23  + stpla23
022900071106     C*
023000071106     C* Effettuo considerazioni su elementi d grafica in stampa
023100071106     C                   SETOFF                                       7071
023200071106     C                   IF        stpLAV  > *zeros OR stpLANP > *zeros
023300081211     c**!!!                        or stpla16 > 0 or stpla23 > 0
023400071106     C                   SETON                                        70
023500071106     C                   ENDIF
023600071106     C*
023700071106     C                   IF        stpDCRP > *zeros OR stpDCRA > *zeros
023800071106     C                   SETON                                        71
023900071106     C                   ENDIF
024000071105     C*
024100071105     C* Verifico il salto pagina
024200071105     C                   IF        *IN01 = *on
024300071105     C                   WRITE     ST74TE
024400071105     C                   SETOFF                                       01
024500071105     C                   WRITE     ST74DE
024600071105     C                   ELSE
024700071105     C                   WRITE     ST74DE
024800071105     C                   ENDIF
024900071105     C*
025000071105     C                   ENDSR
025100071105     C*------------------------------------------------------------------------*
025200031001
025300031001
025400031001
025500031001
025600010627     C*------------------------------------------------------------------------*
025700031001     C* CHKRECTAS - Routine di verifica bolla
025800010627     C*------------------------------------------------------------------------*
025900020613     C     CHKRECTAS     BEGSR
026000010627     C*
026100020613     C                   MOVEL     'S'           flgRECOK          1
026200020613     C*
026300031001     C* Considero solo le bolle valide come spedizione
026400031001     C                   IF        flgRECOK = 'S'
026500031001     C                   Z-ADD     1             J
026600031001     C                   SETOFF                                           28
026700031001     C     tasTBL        LOOKUP    TB(J)                                  28
026800031001     C     SP(J)         IFEQ      'N'
026900031001     C                   MOVEL     'N'           flgRECOK
027000031001     C                   ENDIF
027100031001     C                   ENDIF
027200031010     C*
027300031010     C* Escludo le bolle del network POSTE
027400031010     C                   IF        flgRECOK = 'S'
027500031010     C     tasLNP        CHAIN     AZORG01L
027600031010     C                   IF        %found(AZORG01L)
027700031010     C                   EVAL      OG143 = ORGDE3
027800031010     C                   IF        §OGNTW = 'PPT'
027900031010     C                   MOVEL     'N'           flgRECOK
028000031010     C                   ENDIF
028100031010     C                   ENDIF
028200031010     C                   ENDIF
028300020613     C*
028400020613     C* Testo se la bolla è del periodo richiesto
028500031001     C                   IF        flgRECOK = 'S'
028600070323     C                   IF        *IN40 = *off
028700070323     C                   IF        KtasDSP >= PARDAD AND
028800070323     C                             KtasDSP <= PARDAA
028900020613     C                   ELSE
029000020613     C                   MOVEL     'N'           flgRECOK
029100031001     C                   MOVEL     '1'           flgFINE
029200070323     C                   ENDIF
029300020613     C                   ENDIF
029400070323     C                   IF        *IN40 = *on
029500070323     C                   IF        tasDCM >= PARDAD AND
029600070323     C                             tasDCM <= PARDAA
029700070323     C                   ELSE
029800070323     C                   MOVEL     'N'           flgRECOK
029900070323     C                   MOVEL     '1'           flgFINE
030000070323     C                   ENDIF
030100031001     C                   ENDIF
030200070323     C                   ENDIF
030300070323     C*
030400070323     C* Verifico fine file
030500070323     C                   IF        *IN40 = *off
030600070323     C                   IF        %eof(TITAS38C)
030700070323     C                   SETON                                        30
030800070323     C                   ENDIF
030900070323     C                   ENDIF
031000070323     C                   IF        *IN40 = *on
031100070323     C                   IF        %eof(TITAS26C)
031200070323     C                   SETON                                        30
031300070323     C                   ENDIF
031400070323     C                   ENDIF
031500071105     C*
031600071105     C* Se lancio in stampa verifico anche fine filiale
031700071105     C                   IF        *IN41 = *on
031800080417     C                   IF        tasLNA <> PARLNA
031900080417     C                   MOVEL     'N'           flgRECOK
032000080417     C***                SETON                                        30
032100071105     C                   ENDIF
032200071105     C                   ENDIF
032300010627     C*
032400010627     C                   ENDSR
032500010627     C*------------------------------------------------------------------------*
032600071105
032700071105
032800071105
032900071105
033000071105     C*------------------------------------------------------------------------*
033100071105     C* CHKRECTASST - Routine di verifica bolla x stampa
033200071105     C*------------------------------------------------------------------------*
033300071105     C     CHKRECTASST   BEGSR
033400071105     C*
033500071105     C                   MOVEL     'N'           flgRECOKST        1
033600071105     C*
033700071105     C* Filtro x se nel lancio richiesti L/AVV.
033800071105     C                   IF        flgRECOKST = 'N'
033900071105     C                   IF        wFlgLAV = 'S' and PARLAV = 'S'
034000071105     C                   MOVEL     'S'           flgRECOKST
034100071105     C                   ENDIF
034200071105     C                   ENDIF
034300071105     C*
034400071105     C* Filtro x se nel lancio richiesti Aperture Giacienze
034500071105     C                   IF        flgRECOKST = 'N'
034600071105     C                   IF        wFlgFGC = 'S' and PARFGC = 'S'
034700071105     C                   MOVEL     'S'           flgRECOKST
034800071105     C                   ENDIF
034900071105     C                   ENDIF
035000071105     C*
035100071105     C* Filtro x se nel lancio richiesti Non Fatte
035200071105     C                   IF        flgRECOKST = 'N'
035300071105     C                   IF        wFlgNNF = 'S' and PARNNF = 'S'
035400071105     C                   MOVEL     'S'           flgRECOKST
035500071105     C                   ENDIF
035600071105     C                   ENDIF
035700071105     C*
035800071105     C* Filtro x se nel lancio richiesti Fermi Deposito in Arrivo
035900071105     C                   IF        flgRECOKST = 'N'
036000071105     C                   IF        wFlgFFDA = 'S' and PARFFDA = 'S'
036100071105     C                   MOVEL     'S'           flgRECOKST
036200071105     C                   ENDIF
036300071105     C                   ENDIF
036400071105     C*
036500071105     C* Filtro x se nel lancio richiesti Turni Chiusura in Arrivo
036600071105     C                   IF        flgRECOKST = 'N'
036700071105     C                   IF        wFlgGGCA = 'S' and PARGGCA = 'S'
036800071105     C                   MOVEL     'S'           flgRECOKST
036900071105     C                   ENDIF
037000071105     C                   ENDIF
037100071105     C*
037200071105     C* Filtro x se nel lancio richiesti Date Consegna Richieste
037300071105     C                   IF        flgRECOKST = 'N'
037400071105     C                   IF        wFlgDCR = 'S' and PARDCR = 'S'
037500071105     C                   MOVEL     'S'           flgRECOKST
037600071105     C                   ENDIF
037700071105     C                   ENDIF
037800071105     C*
037900071105     C* Filtro x se nel lancio richiesti Supermercati
038000071105     C                   IF        flgRECOKST = 'N'
038100071105     C                   IF        wFlgSUP = 'S' and PARSUP = 'S'
038200071105     C                   MOVEL     'S'           flgRECOKST
038300071105     C                   ENDIF
038400071105     C                   ENDIF
038500071105     C*
038600071105     C* Filtro x lina di arrivo (lasciare come ultimissimo test in questa routine)!!!!
038700071105     C                   IF        flgRECOKST = 'S'
038800071105     C                   IF        tasLNA <> PARLNA
038900071105     C                   MOVEL     'N'           flgRECOKST
039000071105     C                   ENDIF
039100071105     C                   ENDIF
039200071105     C*
039300071105     C                   ENDSR
039400071105     C*------------------------------------------------------------------------*
039500040408
039600040408
039700040408
039800040408
039900040408     C*------------------------------------------------------------------------*
040000040408     C* REPPARARR - Routine di verifica modifiche bolla in partenza o in arrivo
040100040408     C*------------------------------------------------------------------------*
040200040408     C     REPPARARR     BEGSR
040300040408     C*
040400040408     C* (*IN60 = *ON  => in arrivo)
040500040408     C* (*IN60 = *OFF => in partenza)
040600040408     C                   EVAL      *IN60 = *OFF
040700040408     C*
040800040408     C                   EVAL      ar5TRD = 'GEN'
040900040531     C     KAR530P       CHAIN     fiar531c
041000040531     C                   IF        %found(fiar531c)
041100040408     C                   EVAL      DAR5GEN = ar5UNI
041200040408     C                   IF        §AR5MDP <> *blanks
041300040408     C                   EVAL      *IN60 = *ON
041400040408     C                   ENDIF
041500040408     C                   ENDIF
041600040408     C*
041700040408     C                   ENDSR
041800040408     C*------------------------------------------------------------------------*
041900001218
042000001218
042100010509
042200010627     C*------------------------------------------------------------------------*
042300031001     C* WRIWFILE - Routine di reperimento informazioni e scrittura WRK-FILE
042400010627     C*------------------------------------------------------------------------*
042500020613     C     WRIWFILE      BEGSR
042600071105     C*
042700071105     C* Inizializzo flag d wrk
042800071105     C                   MOVEL     'N'           wFlgLAV           1
042900071105     C                   MOVEL     'N'           wFlgFGC           1
043000071105     C                   MOVEL     'N'           wFlgNNF           1
043100071105     C                   MOVEL     'N'           wFlgFFDA          1
043200071105     C                   MOVEL     'N'           wFlgGGCA          1
043300071105     C                   MOVEL     'N'           wFlgDCR           1
043400071105     C                   MOVEL     'N'           wFlgSUP           1
043500071105     C*
043600071105     C* Se lancio in stampa => inizializzo campi in stampa
043700071105     C   41              CLEAR                   ST74DE
043800031001     C*
043900031010     C* Reperisco distretto e area relativamente alla LNA della bolla
044000031010     C     tasLNA        CHAIN     AZORG01L
044100031001     C                   IF        %found(AZORG01L)
044200031001     C                   EVAL      KmabDIV = ORGFL3
044300031001     C                   EVAL      KmabARE = ORGCAR
044400031001     C                   ENDIF
044500031001     C*
044600031001     C* Valorizzo gli altri campi chiave del wrk-file
044700070323     C                   EVAL      KmabTID = PARTID
044800031001     C                   EVAL      KmabDAD = PARDAD
044900031001     C                   EVAL      KmabDAA = PARDAA
045000031010     C                   EVAL      KmabLNA = tasLNA
045100031001     C*
045200031010     C* Inizializzo il buffer del wrk-file e verifico se già esiste un record
045300031010     C* con medesima chiave (x decidere se andare in WRITE o in UPDATE)
045400031010     C                   CLEAR                   WFMAB000
045500090622     C                   SETOFF                                       50
045600090622     c
045700090622     c* Se solo stampa non chaino nemmeno il record di WFMAB perchè
045800090622     c*  non mi serve e per evitare errori di allocazione inutili
045900090622     c                   if        not *in41
046000031001     C     KMAB01C       CHAIN     WFMAB01L
046100031001     C                   IF        %found(WFMAB01L)
046200031001     C                   SETON                                        50
046300031001     C                   ELSE
046400070323     C                   EVAL      mabTID = KmabTID
046500031001     C                   EVAL      mabDAD = KmabDAD
046600031001     C                   EVAL      mabDAA = KmabDAA
046700031001     C                   EVAL      mabDIV = KmabDIV
046800031001     C                   EVAL      mabARE = KmabARE
046900031010     C                   EVAL      mabLNA = KmabLNA
047000031001     C                   ENDIF
047100090622     C                   ENDIF
047200031204     C*
047300031204     C* Eseguo considerazioni relativamente alle particolarità giacenza
047400031204     C                   IF        tasGGA = *BLANKS
047500031204     C                   SETOFF                                       54
047600031204     C                   ELSE
047700031204     C     tasGGA        LOOKUP    SK7QW                                  54
047800031204     C                   ENDIF
047900031001     C*
048000031001     C* Eseguo considerazioni relativamente agli eventi della bolla corrente
048100031001     C     KEVB04P       SETLL     FNEVB04L
048200031001     C                   IF        %equal(FNEVB04L)
048300031001     C     KEVB04P       READE     FNEVB04L
048400031001     C                   DOW       not %eof(FNEVB04L)
048500071127     C*
048600071127     C* Verifico se trattasi d evento "in carico" alla filiale in elaborazione
048700071127     C                   SETOFF                                       45
048800071127     C                   IF        evbFLE = tasLNA
048900071127     C                   SETON                                        45
049000071127     C                   ELSE
049100071127     C                   CLEAR                   FNLV55DS
049200071128     C                   EVAL      D55tpt = '6'
049300071128     C                   EVAL      D55lin = tasLNA
049400071128     C                   EVAL      D55drf = evbDEV
049500071127     C                   CALL      'FNLV55R'
049600071127     C                   PARM                    FNLV55ds
049700071128     C                   IF        D55err = *blanks
049800071128     C                   IF        D55tfa = evbFLE
049900071127     C                   SETON                                        45
050000071127     C                   ENDIF
050100071127     C                   ENDIF
050200071127     C                   ENDIF
050300090206      * se filiale evento non è la stessa della lna della bolla controllo
050400090206      * con la filiale dell'aut memorizzato sull'evento
050500090206     c                   if        not *in45
050600090206     c                   eval      dsevbnot = evbnot
050700090206     c                   if        dsaut <> *blanks and
050800090206     c                             %dec(%subst(dsaut:1:3):3:0) = taslna
050900090206     c                   eval      *in45 = *on
051000090206     c                   endif
051100090206     c                   endif
051200071127     C*
051300071127     C                   IF        *IN45 = *on
051400031010     C* Determino il tipo di evento (se "Lasciato avviso" oppure "Giacenza")
051500031001     C     evbCEV        LOOKUP    SK2AA                                  55    * evento L/AVV
051600031001     C                   IF        %found
051700071105     C                   MOVEL     'S'           wFlgLAV
051800071105     C                   ADD       1             stpLAV
051900031001     C   50              ADD       1             mabLAV
052000031001     C  N50              Z-ADD     1             mabLAV
052100031001     C                   IF        %subst(tasGVA:1:1) = 'P'                     * L/AVV a privati
052200071105     C                   SUB       1             stpLANP
052300031001     C   50              ADD       1             mabLAP
052400031001     C  N50              Z-ADD     1             mabLAP
052500031001     C                   ENDIF
052600080728      * se rimanda allo svincolo (A16)
052700081211     c**!!!              if        evbcev = 'A16'
052800081211     c**!!!              add       1             stpla16
052900081211     c*! 50              add       1             mabla16
053000081211     c*!n50              z-add     1             mabla16
053100081211     c**!!!              endif
053200080728      * se destinatario chiuso fino al (A23)
053300081211     c**!!!              if        evbcev = 'A23'
053400081211     c**!!!              add       1             stpla23
053500081211     c*! 50              add       1             mabla23
053600081211     c*!n50              z-add     1             mabla16
053700081211     c**!!!              endif
053800031001     C                   ENDIF
053900031204     C* A seconda che la bolla abbia o meno le particolarità giacenza "NO GIACENZA"
054000031204     C* conteggio gli eventi d tipo giacenza in campi differenti.
054100031001     C     evbCEV        LOOKUP    SK2AG                                  55    * evento GIAC.
054200031001     C                   IF        %found
054300031204     C                   IF        *IN54 = *OFF
054400031010     C   50              ADD       1             mabGIA
054500031010     C  N50              Z-ADD     1             mabGIA
054600031204     C                   ELSE
054700031204     C   50              ADD       1             mabENG
054800031204     C  N50              Z-ADD     1             mabENG
054900031001     C                   ENDIF
055000031204     C                   ENDIF
055100031125     C     evbCEV        LOOKUP    SK2AN                                  55    * evento NN FATTA
055200031125     C                   IF        %found
055300071105     C                   MOVEL     'S'           wFlgNNF
055400071105     C                   ADD       1             stpNNF
055500031125     C   50              ADD       1             mabNNF
055600031125     C  N50              Z-ADD     1             mabNNF
055700031125     C                   ENDIF
055800071127     C*
055900071127     C                   ENDIF
056000071127     C*
056100031001     C     KEVB04P       READE     FNEVB04L
056200031001     C                   ENDDO
056300031001     C                   ENDIF
056400031201     C*
056500031201     C* Eseguo considerazioni relativamente alle aperture/riaperture giacenza
056600050310     C     KGCP51P       SETLL     TIGCP51L
056700050310     C                   IF        %equal(TIGCP51L)
056800050310     C     KGCP51P       READE     TIGCP51L
056900050310     C                   DOW       not %eof(TIGCP51L)
057000071105     C                   MOVEL     'S'           wFlgFGC
057100071105     C                   ADD       1             stpFGC
057200031201     C   50              ADD       1             mabFGC
057300031201     C  N50              Z-ADD     1             mabFGC
057400050310     C     KGCP51P       READE     TIGCP51L
057500031201     C                   ENDDO
057600031201     C                   ENDIF
057700040227     C*
057800031001     C                   IF        tasGC1 <> *blanks OR                         * turni chiusura
057900031001     C                             tasGC2 <> *blanks
058000040408     C                   IF        *IN60 = *ON
058100040408     C                   IF        tasGC1 <> §AR5GC1 OR
058200040408     C                             tasGC2 <> §AR5GC2
058300071105     C                   MOVEL     'S'           wFlgGGCA
058400071105     C                   ADD       1             stpGGCA
058500040408     C   50              ADD       1             mabGGCA
058600040408     C  N50              Z-ADD     1             mabGGCA
058700040408     C                   ELSE
058800040408     C   50              ADD       1             mabGGCP
058900040408     C  N50              Z-ADD     1             mabGGCP
059000040408     C                   ENDIF
059100040408     C                   ELSE
059200040408     C   50              ADD       1             mabGGCP
059300040408     C  N50              Z-ADD     1             mabGGCP
059400031001     C                   ENDIF
059500040408     C                   ENDIF
059600040227     C*
059700040408     C                   IF        tasFFD = 'S'
059800040408     C                   IF        *IN60 = *ON
059900040408     C                   IF        tasFFD <> §AR5FFD
060000071105     C                   MOVEL     'S'           wFlgFFDA
060100071105     C                   ADD       1             stpFFDA
060200040408     C   50              ADD       1             mabFDA
060300040408     C  N50              Z-ADD     1             mabFDA
060400040408     C                   ELSE
060500040408     C   50              ADD       1             mabFDP
060600040408     C  N50              Z-ADD     1             mabFDP
060700040408     C                   ENDIF
060800040408     C                   ELSE
060900031001     C   50              ADD       1             mabFDP
061000031001     C  N50              Z-ADD     1             mabFDP
061100031001     C                   ENDIF
061200040408     C                   ENDIF
061300040227     C*
061400040227     C                   IF        tasFTC = 'S' OR                              * consegna a superm.
061500040227     C                             tasTC2 = 'S'
061600071105     C                   MOVEL     'S'           wFlgSUP
061700071105     C                   ADD       1             stpSUP
061800040408     C*
061900040408     C                   CLEAR                   TISIT0DS                       * consegna a superm.
062000051227     C                   MOVEL     'E'           IT0TLA                         * in anagraf. dest.
062100051227     C                   MOVEL     tasNZD        IT0NAZ                         * particolari
062200040408     C                   MOVEL     tasPRD        IT0PRV
062300040408     C                   MOVEL     tasCAD        IT0CAP
062400040408     C                   MOVEL     tasLOD        IT0LOC
062500040408     C                   MOVEL     tasIND        IT0IND
062600040408     C                   MOVEL     tasRSD        IT0RAG
062700040408     C                   CALL      'TISIT5R'
062800040408     C                   PARM                    TISIT0DS
062900040408     C*
063000040408     C                   IF        OT0ERR = *blanks AND
063100040408     C                             OT0DOS = 'S'
063200040408     C                   IF        *IN60 = *ON
063300040408     C                   IF        tasFTC <> §AR5TC1 OR
063400040408     C                             tasTC2 <> §AR5TC2
063500040408     C   50              ADD       1             mabSUPA
063600040408     C  N50              Z-ADD     1             mabSUPA
063700040408     C                   ELSE
063800040408     C   50              ADD       1             mabSUPP
063900040408     C  N50              Z-ADD     1             mabSUPP
064000040408     C                   ENDIF
064100040408     C                   ELSE
064200040408     C   50              ADD       1             mabSUPP
064300040408     C  N50              Z-ADD     1             mabSUPP
064400040408     C                   ENDIF
064500040408     C*
064600040408     C                   ELSE
064700040408     C                   IF        *IN60 = *ON
064800040408     C                   IF        tasFTC <> §AR5TC1 OR
064900040408     C                             tasTC2 <> §AR5TC2
065000040408     C   50              ADD       1             mabSPSA
065100040408     C  N50              Z-ADD     1             mabSPSA
065200040408     C                   ELSE
065300040408     C   50              ADD       1             mabSPSP
065400040408     C  N50              Z-ADD     1             mabSPSP
065500040408     C                   ENDIF
065600040408     C                   ELSE
065700040408     C   50              ADD       1             mabSPSP
065800040408     C  N50              Z-ADD     1             mabSPSP
065900040408     C                   ENDIF
066000040408     C                   ENDIF
066100040408     C                   ENDIF
066200040408     C*
066300040408     C                   IF        tasDCR > *zeros
066400071105     C                   MOVEL     'S'           wFlgDCR
066500040408     C                   IF        *IN60 = *ON
066600071105     C                   ADD       1             stpDCRA
066700040408     C                   MOVEL     §AR5DCR       Num§AR5DCR        8 0
066800040409     C                   IF        tasDCR <> Num§AR5DCR
066900040408     C   50              ADD       1             mabDCRA
067000040408     C  N50              Z-ADD     1             mabDCRA
067100040408     C                   ELSE
067200040408     C   50              ADD       1             mabDCRP
067300040408     C  N50              Z-ADD     1             mabDCRP
067400040408     C                   ENDIF
067500040408     C                   ELSE
067600071105     C                   ADD       1             stpDCRP
067700040408     C   50              ADD       1             mabDCRP
067800040408     C  N50              Z-ADD     1             mabDCRP
067900040408     C                   ENDIF
068000040408     C                   ENDIF
068100031001     C*
068200031001     C* Conteggio tutte le bolle elaborate
068300071105     C                   ADD       1             stpTBOL
068400031001     C   50              ADD       1             mabTOT
068500031001     C  N50              Z-ADD     1             mabTOT
068600020613     C*
068700071105     C*
068800071105     C* Se lancio x creazione wrk-file....
068900071105     C                   IF        *IN41 = *off
069000031001     C* Al termine scrivo o update il record così valorizzato
069100031001     C   50              UPDATE    WFMAB000
069200031001     C  N50              WRITE     WFMAB000
069300071105     C                   ENDIF
069400020613     C*
069500010627     C                   ENDSR
069600010509     C*------------------------------------------------------------------------*
069700031001
069800031001
069900031001
070000031001     C*------------------------------------------------------------------------*
070100031107     C* DELWFILE - Routine di cancellazione completa del file.
070200031001     C*------------------------------------------------------------------------*
070300031001     C     DELWFILE      BEGSR
070400031001     C*
070500031107     C     *LOVAL        SETLL     WFMAB01L
070600031107     C                   READ      WFMAB01L
070700031107     C                   DOW       not %eof(WFMAB01L)
070800031107     C                   DELETE    WFMAB000
070900031107     C                   READ      WFMAB01L
071000031107     C                   ENDDO
071100031001     C*
071200031001     C                   ENDSR
071300031001     C*------------------------------------------------------------------------*
071400010626
071500010626
071600010626
071700010509     C*------------------------------------------------------------------------*
071800020613     C* CARTBL - Routine di caricamento dati tabellati
071900010509     C*------------------------------------------------------------------------*
072000010509     C     CARTBL        BEGSR
072100010509     C*
072200031001     C*--------------------
072300031001     C* DISTINGUO EVENTI DI TIPO "GIACENZA" ED EVENTI DI TIPO "LASCIATO AVVISO"
072400031001     C*--------------------
072500020614     C                   Z-ADD     0             WA                4 0
072600020614     C                   Z-ADD     0             WG                4 0
072700020614     C                   CLEAR                   SK2AA
072800020614     C                   CLEAR                   SK2AG
072900010509     C                   MOVEL     '2A'          COD
073000020614     C     KTABP         CHAIN     TABEL00F                           31
073100010509     C     *IN31         DOWEQ     '0'
073200010509     C     TBLFLG        IFEQ      ' '
073300010509     C                   MOVEL     TBLUNI        DS2A
073400020613     C     §2AFTC        IFEQ      'A'
073500020614     C                   ADD       1             WA
073600020614     C                   MOVEL     TBLKEY        SK2AA(WA)
073700010509     C                   ENDIF
073800020614     C     §2AFTC        IFEQ      'G'
073900020614     C                   ADD       1             WG
074000020614     C                   MOVEL     TBLKEY        SK2AG(WG)
074100020614     C                   ENDIF
074200031125     C     §2AFTC        IFEQ      'N'
074300031125     C                   ADD       1             WG
074400031125     C                   MOVEL     TBLKEY        SK2AN(WG)
074500031125     C                   ENDIF
074600010509     C                   ENDIF
074700020614     C     KTABP         READE     TABEL                                  31
074800010509     C                   ENDDO
074900031001     C*--------------------
075000031001     C* FLAG SPEDIZIONI /RICAVI PER RAGGRUPPAMENTO TIPO BOLLA
075100031001     C*--------------------
075200031001     C                   Z-ADD     0             I                 3 0
075300031001     C                   MOVEL     '2W'          COD
075400031001     C     KTABP         SETLL     TABEL00F
075500031001     C     KTABP         READE     TABEL00F                               99
075600031001     C     *IN99         DOWEQ     *OFF
075700031001     C     TBLCOD        ANDEQ     '2W'
075800031001     C     TBLFLG        IFEQ      *BLANKS
075900031001     C                   ADD       1             I
076000031001     C                   MOVEL     TBLKEY        RBL(I)                         *RAGGRUPPAMENTO BOLL
076100031001     C                   MOVEL     TBLUNI        DS2W
076200031001     C                   MOVEL     §2WFSP        FSP(I)                         *FLAG SPEDIZIONE
076300031001     C                   MOVEL     §2WFRI        FRI(I)                         *FLAG RICAVO
076400031001     C                   ENDIF
076500031001     C     KTABP         READE     TABEL00F                               99
076600031001     C                   ENDDO
076700031001     C*--------------------
076800031001     C* TIPI BOLLA E RELATIVI FLAGS SPEDIZIONE/RICAVO
076900031001     C*--------------------
077000031001     C                   Z-ADD     0             I
077100031001     C                   MOVEL     'TB'          COD
077200031001     C     KTABP         SETLL     TABEL00F
077300031001     C     KTABP         READE     TABEL00F                               99
077400031001     C     *IN99         DOWEQ     *OFF
077500031001     C     TBLCOD        ANDEQ     'TB'
077600031001     C     TBLFLG        IFEQ      *BLANKS
077700031001     C                   ADD       1             I
077800031001     C                   MOVEL     TBLKEY        TB(I)                          *TIPO BOLLA
077900031001     C                   MOVEL     TBLUNI        DSTB
078000031001     C                   Z-ADD     1             J                 4 0
078100031001     C                   SETOFF                                       28
078200031001     C     §TBRBL        LOOKUP    RBL(J)                                 28
078300031001     C     *IN28         IFEQ      *ON
078400031001     C                   MOVEL     FSP(J)        SP(I)                          *FLAG SPEDIZIONE
078500031001     C                   MOVEL     FRI(J)        RI(I)                          *FLAG RICAVO
078600031001     C                   ELSE
078700031001     C                   MOVEL     'N'           SP(I)
078800031001     C                   MOVEL     'N'           RI(I)
078900031001     C                   ENDIF
079000031001     C                   ENDIF
079100031001     C     KTABP         READE     TABEL00F                               99
079200031001     C                   ENDDO
079300031204     C*--------------------
079400031204     C* CARICA TABELLA GESTIONE PARTICOLARITA' GIACENZA (7Q)
079500031204     C*--------------------
079600031204     C                   Z-ADD     0             I7QW              2 0
079700031204     C                   MOVEL     '7Q'          COD
079800031204     C     KTABP         SETLL     TABEL00F                           99
079900031204     C     KTABP         READE     TABEL00F                               99
080000031204     C     *IN99         DOWEQ     *OFF
080100031204     C     TBLCOD        ANDEQ     '7Q'
080200031204     C     TBLFLG        IFEQ      *BLANKS
080300031204     C                   MOVEL     TBLUNI        DS7Q
080400031204     C                   IF        §7Q4D1 = 'R' OR
080500031204     C                             §7Q4D2 = 'R' OR
080600031204     C                             §7Q4D3 = 'R' OR
080700031204     C                             §7Q4D4 = 'R' OR
080800031204     C                             §7Q4D5 = 'R'
080900031204     C                   ADD       1             I7QW
081000031204     C                   MOVEL     TBLKEY        SK7QW(I7QW)
081100031204     C                   ENDIF
081200031204     C                   ENDIF
081300031204     C     KTABP         READE     TABEL00F                               99
081400031204     C                   ENDDO
081500031204     C*--------------------
081600031204     C* CARICA TABELLA ESTENSINE GESTIONE PARTICOLARITA' GIACENZA (7W)
081700031204     C*--------------------
081800031204     C                   MOVEL     '7W'          COD
081900031204     C     KTABP         SETLL     TABEL00F                           99
082000031204     C     KTABP         READE     TABEL00F                               99
082100031204     C     *IN99         DOWEQ     *OFF
082200031204     C     TBLCOD        ANDEQ     '7W'
082300031204     C     TBLFLG        IFEQ      *BLANKS
082400031204     C                   MOVEL     TBLUNI        DS7W
082500031204     C                   IF        §7W6D1 = 'C' OR
082600031204     C                             §7W6D2 = 'C' OR
082700031204     C                             §7W6D3 = 'C' OR
082800031204     C                             §7W6D4 = 'C' OR
082900031204     C                             §7W6D5 = 'C'
083000031204     C                   ADD       1             I7QW
083100031204     C                   MOVEL     TBLKEY        SK7QW(I7QW)
083200031204     C                   ENDIF
083300031204     C                   ENDIF
083400031204     C     KTABP         READE     TABEL00F                               99
083500031204     C                   ENDDO
083600010509     C*
083700010509     C                   ENDSR
083800010509     C*---------------------------------------------------------------*
083900040227
084000040227
084100040227
084200040227     C*------------------------------------------------------------------------*
084300040227     C* CHIUDI - Routine di chiudura *pgm esterni
084400040227     C*------------------------------------------------------------------------*
084500040227     C     CHIUDI        BEGSR
084600040227     C*
084700040227     C                   CLEAR                   TISIT0DS
084800040227     C                   MOVEL     'C'           IT0TLA
084900040227     C                   CALL      'TISIT5R'
085000040227     C                   PARM                    TISIT0DS
085100040227     C*
085200040227     C                   ENDSR
085300040227     C*------------------------------------------------------------------------*
085400010509
085500010509
085600001218
085700001218     C*------------------------------------------------------------------------*
085800001218     C* *INZSR - ROUTINE INIZIALE
085900001218     C*------------------------------------------------------------------------*
086000001218     C     *INZSR        BEGSR
086100001218     C*
086200010726     C* Ricevo in input il parametro indicante l'ID LANCIO
086300010726     C     *ENTRY        PLIST
086400020613     C                   PARM                    KPJBA
086500020613     C                   MOVEL     KPJBU         PARAM
086600010726     C*
086700001218     C                   Z-ADD     1             CODUT
086800070323     C*
086900070323     C* VERIFICO I PARAMETRI D INPUT
087000070323     C                   IF        PARTID = 'S'
087100070323     C                   SETOFF                                       40
087200070323     C                   ENDIF
087300070323     C                   IF        PARTID = 'C'
087400070323     C                   SETON                                        40
087500070323     C                   ENDIF
087600071105     C                   IF        PARTIP = 'ST'
087700071105     C                   SETON                                        41
087800071105     C                   ELSE
087900071105     C                   SETOFF                                       41
088000071105     C                   ENDIF
088100001218     C*
088200001218     C* Definizioni chiavi
088300070323     C     KTAS26P       KLIST                                                  TITAS26C - Parziale
088400070323     C                   KFLD                    KtasDCM
088500071105     C                   KFLD                    KtasLNA
088600070323     C*
088700031001     C     KTAS38P       KLIST                                                  TITAS38C - Parziale
088800031001     C                   KFLD                    tasAAS
088900031001     C                   KFLD                    tasMGS
089000031001     C*
089100020613     C     KEVB04P       KLIST                                                  FNEVB04L - Parziale
089200020613     C                   KFLD                    tasAAS
089300020613     C                   KFLD                    tasLNP
089400020613     C                   KFLD                    tasNRS
089500020613     C                   KFLD                    tasNSP
089600031201     C*
089700050310     C     KGCP51P       KLIST                                                  TIGCP51L - Parziale
089800031201     C                   KFLD                    tasAAS
089900031201     C                   KFLD                    tasLNP
090000031201     C                   KFLD                    tasNRS
090100031201     C                   KFLD                    tasNSP
090200040408     C*
090300040531     C     KAR530P       KLIST                                                  FIAR531C - Parziale
090400040408     C                   KFLD                    tasAAS
090500040408     C                   KFLD                    tasLNP
090600040408     C                   KFLD                    tasNRS
090700040408     C                   KFLD                    tasNSP
090800040408     C                   KFLD                    ar5TRD
090900010509     C*
091000020614     C     KTABP         KLIST                                                  TABEL00F - Parziale
091100010509     C                   KFLD                    CODUT             1 0
091200010509     C                   KFLD                    COD               2
091300031001     C*
091400031001     C     KMAB01P       KLIST                                                  WFMAB01L - Parziale
091500070323     C                   KFLD                    KmabTID
091600031001     C                   KFLD                    KmabDAD
091700031001     C*
091800031001     C     KMAB01C       KLIST                                                  WFMAB01L - Completa
091900070323     C                   KFLD                    KmabTID
092000031001     C                   KFLD                    KmabDAD
092100031001     C                   KFLD                    KmabDAA
092200031001     C                   KFLD                    KmabDIV
092300031001     C                   KFLD                    KmabARE
092400031010     C                   KFLD                    KmabLNA
092500001218     C*
092600001218     C                   ENDSR
