000100000000     H*------------------------------------------------------------------------*
000200150211     H* STAtistica moviMENTI GABBIE E BANCALI         ESEGUE STAMPA            *
000300950206     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500950206     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800030124     FAZMGB04L  IF   E           K DISK
000900030124     FAZAGB01L  IF   E           K DISK
001000150211     FAZcGB01L  IF   E           K DISK
001100030124     FCNACO00F  IF   E           K DISK
001200150414     FCNind00F  IF   E           K DISK
001300150402     Ffnspe01l  iF   E           K DISK
001400150504     Ffnsp201l  iF   E           K DISK
001500150414     Ftfntc01l  iF   E           K DISK
001600030124     FAZORG01L  IF   E           K DISK
001700030124     FTNTBE01L  IF   E           K DISK
001800150409     FTabel00f  IF   E           K DISK
001900150413     FTISIG17p  O    E             PRINTER OFLIND(*IN01)
002000150413     Fwfbas00f  O    E             disk    usropn
002100000000     D*------------------------------------------------------------------------*
002200000000     D* SCHIERE
002300000000     D*------------------------------------------------------------------------*
002400030124     D*------------
002500030124     D* SCHIERA ARTICOLI
002600030124     D*------------
002700150216     D SKCODART        S             15    DIM(100)
002800030127     D SKDESART        S             30    DIM(100)
002900000808     D*------------------
003000000808     D* DS CONTROLLO DATA (8)
003100000808     D*------------------
003200000808     D WLBDA8          DS                  INZ
003300000808     D  G08DAT                 1      8  0
003400000808     D  G08INV                 9     16  0
003500000808     D  G08ERR                17     17
003600000808     D  G08TGI                18     22  0
003700000000     D*------------------
003800000000     D* ARCHITETTURA
003900000000     D*------------------
004000000000     D KPJBA         E DS
004100000000     D*------------------
004200020718     D* DS X PASSAGGIO PARAMETRI
004300020718     D*------------------
004400030124     D PARAM           DS                  INZ
004500030124     D  PARTP1                        1
004600030124     D  PARSO1                        8
004700030124     D  PARCO1                       15
004800150211     D  PARPO1                        3
004900150402     D  PARfile                       1
005000150402     D  PARpulf                       1
005100150409     D  PARDAl                        8s 0
005200150409     D  PARal                         8s 0
005300150409     D  PARarea                       3
005400150410     D  PARtsc                        1
005500020923     D*-------------------
005600020923     D* DS REPERIMENTO DATI UTENTE
005700020923     D*-------------------
005800020923     D TIBS34DS      E DS                                                       *Profili utente
005900020923     D DDATIUTE      E DS                                                       *Dati utente
006000020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006100150409     D Ds05          E DS                                                       *Dati utente
006200030127     D*------------------
006300030127     D* VARIABILI DI TOTALIZZAZIONE
006400030127     D*------------------
006500150217     D iso             S               d
006600150217     D eur             S               d   datfmt(*eur)
006700150409     D wcod3           S              3
006800150220     D nostp           S              1
006900150219     D per             S             10  0 INZ(*zeros)
007000150216     D TotSOGe         S             10  0 INZ(*zeros)
007100150216     D TotSOGu         S             10  0 INZ(*zeros)
007200150217     D TotSOGGENe      S             13  0 INZ(*zeros)
007300150217     D TotSOGGENu      S             13  0 INZ(*zeros)
007400150216     D TotCODe         S             10  0 INZ(*zeros)
007500150216     D TotCODu         S             10  0 INZ(*zeros)
007600030127     D DepTIP          S                   LIKE(mgbTIP)
007700030127     D DepSOG          S                   LIKE(mgbSOG)
007800030311     D DepCOD          S                   LIKE(mgbCOD)
007900150402     d coman           s             99
008000150402     d luncmd          s             15  5
008100020722     D*------------------
008200030124     D* VARIABILI DI WRK
008300020722     D*------------------
008400020722     D I               S              4  0 INZ
008500030124     D WDA8            S              8  0 INZ
008600030124     D DAW8            S              8  0 INZ
008700030124     D W_Fine          S              1    INZ('0')
008800030124     D WRECOKSAL       S              1    INZ('S')
008900150218     d azcgbds       e ds                  extname(azcgb00f)
009000150402     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
009100000000     C*------------------------------------------------------------------------*
009200000000     C*?MAIN LINE
009300000000     C*------------------------------------------------------------------------*
009400020718     C*
009500020718     C* STAMPO LA TESTATA E I PARAMETRI DI LANCIO
009600150211     C                   EXsr      statess
009700150211     C*
009800150211     C                   EVAL      W_Fine = '0'
009900150211     C*
010000150211     C* POSIZIONAMENTO SUL FILE MOVIMENTI
010100150211     C                   EVAL      MGBTIP = PARTP1
010200150211     C                   EVAL      MGBSOG = PARSO1
010300150211     C                   EVAL      MGBCOD = PARCO1
010400150211     C     KEYMGBP       SETLL     AZMGB04L                           99
010500150211     C                   IF        *IN99 = *OFF
010600150211     C                   READ      AZMGB04L
010700150211     C                   DOW       not %eof(AZMGB04L) AND W_Fine = '0'
010800150211     C*
010900150211     C* ESEGUO CONTROLLI DI VALIDITA RISPETTO AI PARAMETRI DI LANCIO
011000150211     C                   EXSR      CHKREC
011100150211     C*
011200150211     C* SE RECORD OK ESEGUO TOTALIZZAZIONE X CLIENTE / ARTICOLO
011300150211     C                   IF        WRECOKSAL = 'S'
011400150211     C                   EXSR      TOTROT
011500150211     C                   ENDIF
011600150211     C                   READ      AZMGB04L
011700150211     C                   ENDDO
011800150211     C*
011900150211     C* A FINE CICLO SCARICO LE MEMORIZZAZIONI SOSPESE (SE RICHIESTO NEL LANCIO)
012000150220     c                   if        nostp = ' '
012100150220     c                   if        contaart > 1
012200150220     c                   setoff                                       15
012300150220     c                   end
012400150219     C                   EXSR      STASALCODTOT
012500150220     C*                  EXSR      STASALSOGTOT
012600150220     c                   end
012700150211     C                   EXSR      STASALGENTOT
012800150211     C                   ENDIF
012900020523     C*
013000020523     C                   SETON                                        LR
013100030127     C*------------------------------------------------------------------------*
013200030127     C*?STATESS - STAMPA LA TESTATA ED I PARAMETRI DI LANCIO DELLA STAMPA SALDI MOVIMENTI
013300030127     C*------------------------------------------------------------------------*
013400030127     C     STATESS       BEGSR
013500030127     C*
013600150211     C                   WRITE     testa
013700030127     C*
013800030305     C                   IF        PARSO1 <> *zeros
013900030127     C                   MOVEL     PARSO1        STPSOG                         * Soggetto
014000030127     C                   IF        PARTP1 = 'C'
014100030127     C                   EVAL      STPDE1 = '(Cliente)'
014200030127     C                   EVAL      ACOKUT = 1
014300030127     C                   EVAL      ACOKCC = DUTKCI
014400030127     C                   MOVE      PARSO1        ACOKSC
014500030127     C     KEYACO        CHAIN     CNACO00F
014600030127     C                   IF        %found(CNACO00F)
014700030127     C                   EVAL      STPDSO = ACORAG
014800030127     C                   ELSE
014900030127     C                   EVAL      STPDSO = *ALL'*'
015000030127     C                   ENDIF
015100030127     C                   ENDIF
015200030127     C*
015300030127     C                   IF        PARTP1 = 'F'
015400150210     C                   EVAL      STPDE1 = '(Fil.)'
015500150416     C                   MOVE      stpsog        ORGFIL
015600030127     C     KEYORG        CHAIN     AZORG01L
015700030127     C                   IF        %found(AZORG01L)
015800030127     C                   EVAL      STPDSO = ORGDES
015900030127     C                   ELSE
016000030127     C                   EVAL      STPDSO = *ALL'*'
016100030127     C                   ENDIF
016200030127     C                   ENDIF
016300030127     C*
016400030127     C                   ELSE
016500030128     C                   EVAL      STPSOG = 'Tutti'
016600030127     C                   IF        PARTP1 = 'C'
016700030128     C                   EVAL      STPDSO = 'i clienti'
016800030305     C                   IF        PARPO1 <> *zeros
016900150210     C                   EVAL      STPDSO = %trim(STPDSO)+' del Fil. '+PARPO1
017000030305     C                   ENDIF
017100030127     C                   ENDIF
017200030127     C                   IF        PARTP1 = 'F'
017300150210     C                   EVAL      STPDSO = 'i Fil.'
017400030127     C                   ENDIF
017500030127     C                   ENDIF
017600030127     C*
017700030127     C                   IF        PARCO1 <> *blanks
017800030127     C                   MOVEL     PARCO1        STPCOD                         * Articolo
017900030127     C                   Z-ADD     1             I
018000030127     C                   SETOFF                                       55
018100030127     C     STPCOD        LOOKUP    SKCODART(I)                            55
018200030127     C                   IF        %found
018300030127     C                   EVAL      STPDCO = SKDESART(I)
018400030127     C                   ELSE
018500030128     C                   EVAL      STPDCO = *ALL'*'
018600030127     C                   ENDIF
018700030127     C*
018800030127     C                   ELSE
018900030128     C                   EVAL      STPCOD = 'Tutti'
019000030127     C                   ENDIF
019100030127     C*
019200150422     c*                  if        pardet = ' ' or pardet = 'N'
019300150422     c*                  eval      stpdet = 'No'
019400150422     c*                  else
019500150422     C*                  EVAL      STPdet  = 'Si'
019600150422     c*                  end
019700150422     c*    pardet        comp      'S'                                    11
019800030127     C*
019900150211     C                   WRITE     parametri
020000150220     c                   seton                                        0115
020100030127     C*
020200030127     C                   ENDSR
020300150211     C*------------------------------------------------------------------------*
020400150211     C*?CHKREC - ROUTINE DI CONTROLLO VALIDITA' RECORD
020500150211     C*------------------------------------------------------------------------*
020600150211     C     CHKREC        BEGSR
020700150211     C*
020800150211     C                   MOVEL     'S'           WRECOKSAL
020900150414     C*
021000150414     C* Forzatura considero al momento solo la causale specificata
021100150414     c                   if        partp1 = 'C'
021200150422     C                   IF        MGBCAU = 'COMS' or mgbcau = 'RETS'
021300150414     C                   ELSE
021400150414     C                   MOVEL     'N'           WRECOKSAL
021500150414     C                   ENDIF
021600150414     C                   ENDIF
021700150414     c                   if        partp1 = 'F'
021800150414     C                   IF        MGBCAU = 'FIE '
021900150414     C                   ELSE
022000150414     C                   MOVEL     'N'           WRECOKSAL
022100150414     C                   ENDIF
022200150414     C                   ENDIF
022300150211     C*
022400150211     C* SE RICHIESTO UN TIPO/SOGGETTO CONTROLLO
022500150216     C                   IF        PARTP1 <> *BLANKS AND
022600150216     C                             MGBTIP = PARTP1
022700150211     C                   ELSE
022800150211     C                   MOVEL     'N'           WRECOKSAL
022900150211     C                   ENDIF
023000150216     C*
023100150211     C                   IF        PARTP1 =  'C'     AND
023200150211     C                             PARPO1 <> *ZEROS  AND
023300150211     C                             PARPO1 <> *BLANKS
023400150211     C                   IF        MGBTIP = PARTP1 AND
023500150211     C                             %subst(MGBSOG:2:3) = PARPO1
023600150211     C                   ELSE
023700150211     C                   MOVEL     'N'           WRECOKSAL
023800150211     C                   ENDIF
023900150211     C                   ENDIF
024000150216     C*
024100150417     C                   IF        PARSO1 <> *ZEROS and
024200150417     C                             PARSO1 <> *blank
024300150211     C                   IF        MGBTIP = PARTP1 AND
024400150211     C                             MGBSOG = PARSO1
024500150211     C                   ELSE
024600150211     C                   EVAL      W_Fine = '1'
024700150211     C                   MOVEL     'N'           WRECOKSAL
024800150211     C                   ENDIF
024900150211     C                   ENDIF
025000150211     C*
025100150211     C* SE RICHIESTO UN ARTICOLO CONTROLLO
025200150216     C                   IF        PARCO1 <> *BLANKS
025300150216     C                   if        MGBCOD = PARCO1
025400150211     C                   ELSE
025500150211     C                   MOVEL     'N'           WRECOKSAL
025600150211     C                   ENDIF
025700150216     C                   ENDIF
025800150409     C* SE RICHIESTO UN codice area
025900150409     C                   IF        PARarea <> *BLANKS
026000150409     c                   if        mgbtip = 'F'
026100150409     c                   move      mgbsog        wcod3
026200150409     c                   else
026300150409     c                   eval      wcod3 = %subst(mgbsog:2:3)
026400150409     c                   endif
026500150409     c                   move      wcod3         orgfil
026600150409     c     keyorg        chain     azorg01l
026700150409     C                   if        orgcar = arean
026800150409     C                   ELSE
026900150409     C                   MOVEL     'N'           WRECOKSAL
027000150409     C                   ENDIF
027100150409     C                   ENDIF
027200150409     C* SE RICHIESTO UN ARTICOLO CONTROLLO
027300150410     C                   IF        PARtsc <> *BLANKS
027400150410     C                   if        %subst(MGBCOD:1:1) = partsc
027500150409     C                   ELSE
027600150409     C                   MOVEL     'N'           WRECOKSAL
027700150409     C                   ENDIF
027800150409     C                   ENDIF
027900150211     C*
028000150408     C* SE RICHIESTO periodo
028100150409     C                   IF        PARDAL > 0 and PARAL > 0
028200150409     C                   if        MGBDAT >= PARDAl and mgbdat <= PARAL
028300150409     C                   ELSE
028400150409     C                   MOVEL     'N'           WRECOKSAL
028500150409     C                   ENDIF
028600150409     C                   ENDIF
028700150211     C*
028800150211     C                   ENDSR
028900150216     C*------------------------------------------------------------------------*
029000150216     C*?TOTROT - ROUTINE DI VERIFICA ROTTURE DI CODICE
029100150216     C*------------------------------------------------------------------------*
029200150216     C     TOTROT        BEGSR
029300150219     c*
029400150216     C* VERIFICO ROTTURA X SOGGETTO
029500150216     C                   IF        mgbTIP = DepTIP AND
029600150410     C                             mgbSOG = DepSOG and
029700150410     c                             %subst(mgbcod:1:1) = %subst(depcod:1:1)
029800150216     C                   ELSE
029900150216     C                   IF        DepTIP <> *blanks AND DepSOG <> *blanks
030000150220     c                   if        contaart > 1
030100150220     c                   setoff                                       15
030200150220     c                   end
030300150220     c                   clear                   contaart          2 0
030400150216     C                   EXSR      STASALCODTOT
030500150220     c                   seton                                        15
030600150220     C*                  EXSR      STASALSOGTOT
030700150216     C                   ENDIF
030800150216     C                   EVAL      DepTIP = mgbTIP
030900150216     C                   EVAL      DepSOG = mgbSOG
031000150216     C                   EVAL      DepCOD = mgbCOD
031100150216     C*
031200150216     C                   EXSR      REPSALSOG
031300150220     c                   add       1             contaart
031400150216     C                   EXSR      REPSALCOD
031500150216     C                   ENDIF
031600150216     C*
031700150216     C* VERIFICO ROTTURA X ARTICOLO
031800150216     C                   IF        mgbCOD <> DepCOD
031900150216     C                   IF        DepCOD <> *blanks
032000150220     c* spengo il 15 se ho già stampato un primo art. con tutti i rif.
032100150220     c                   if        contaart > 1
032200150220     c                   setoff                                       15
032300150220     c                   end
032400150216     C                   EXSR      STASALCODTOT
032500150216     C                   ENDIF
032600150216     C                   EVAL      DepCOD = mgbCOD
032700150216     C*
032800150220     c                   add       1             contaart
032900150216     C                   EXSR      REPSALCOD
033000150216     C                   ENDIF
033100150218     C* dettaglio movimento
033200150422     c*                  if        pardet = 'S'
033300150422     c*                  move      mgbdat        iso
033400150422     c*                  move      iso           eur
033500150422     c*                  move      eur           stddat
033600150422     c*                  if        mgbqta > 0
033700150422     C*                  EVAL      Stdsogu= mgbqta
033800150422     c*                  clear                   stdsoge
033900150422     c*                  else
034000150422     C*                  EVAL      stdsoge= mgbqta * -1
034100150422     c*                  clear                   stdsogu
034200150422     c*                  end
034300150422     C*                  EVAL      Stddif = stdsoge - stdsogu
034400150422     C*                  seton                                        11
034500150422     C*                  exsr      stampa
034600150422     c*                  setoff                                       15
034700150422     c*                  end
034800150216     C* TOTALIZZO
034900150216     C                   EXSR      MEMDAT
035000150216     C*
035100150216     C                   ENDSR
035200030128     C*------------------------------------------------------------------------*
035300030128     C*?MEMDAT - ROUTINE DI TOTALIZZAZIONE X SALDI
035400030128     C*------------------------------------------------------------------------*
035500030128     C     MEMDAT        BEGSR
035600030128     C* TOTALIZZO
035700150220     c*                  if        mgbqta > 0
035800150220     C*                  EVAL      TotSOGu= TotSOGu+ MGBQTA
035900150220     c*                  else
036000150220     C*                  EVAL      TotSOGe= TotSOGe+ (MGBQTA *-1)
036100150220     c*                  end
036200150218     c                   if        mgbqta > 0
036300150218     C                   EVAL      TotCODu= TotCODu+ MGBQTA
036400150211     c                   else
036500150218     C                   EVAL      TotCODe= TotCODe+ (MGBQTA *-1)
036600150211     c                   end
036700030128     C*
036800030128     C                   ENDSR
036900030127     C*------------------------------------------------------------------------*
037000030127     C*?REPSALSOG - ROUTINE DI REPERIMENTO DESCRIZIONI SOGGETTO X STAMPA SALDI
037100030127     C*------------------------------------------------------------------------*
037200030127     C     REPSALSOG     BEGSR
037300030127     C*
037400150220     C                   MOVEL     DepSOG        STDSog
037500150410     C                   MOVEL     Depcod        tipoges
037600150220     c                   clear                   azcgbds
037700150410     c     kcgb          setll     azcgb01l
037800150220     c                   do        *hival
037900150410     c     kcgb          reade     azcgb01l
038000150220     c                   if        %eof(azcgb01l)
038100150220     c                   leave
038200150220     c                   end
038300150220     c                   enddo
038400150220     c* se richiesti solo clienti con eccedenza o oltre sogli
038500150220     c                   exsr      srtotsog
038600030127     C* REPERISCO DESCRIZIONE SOGGETTO
038700030127     C                   IF        DepTIP = 'C'
038800150218     C*                  EVAL      STDDE2 = '(Cli)'
038900030127     C                   EVAL      ACOKUT = 1
039000030127     C                   EVAL      ACOKCC = DUTKCI
039100030127     C                   MOVE      STDSOG        ACOKSC
039200030127     C     KEYACO        CHAIN     CNACO00F
039300030127     C                   IF        %found(CNACO00F)
039400030127     C                   EVAL      STDDSO = ACORAG
039500030127     C                   ELSE
039600030127     C                   EVAL      STDDSO = *ALL'*'
039700030127     C                   ENDIF
039800030127     C                   ENDIF
039900030127     C                   IF        DepTIP = 'F'
040000150417     C                   MOVE      stdsog        ORGFIL
040100030127     C     KEYORG        CHAIN     AZORG01L
040200030127     C                   IF        %found(AZORG01L)
040300030127     C                   EVAL      STDDSO = ORGDES
040400030127     C                   ELSE
040500030127     C                   EVAL      STDDSO = *ALL'*'
040600030127     C                   ENDIF
040700030127     C                   ENDIF
040800030127     C*
040900150220     C                   ENDSR
041000150220     C*------------------------------------------------------------------------*
041100150220     C*?srtotsog  calcola i totali per cliente                         A SALDI
041200150220     C*------------------------------------------------------------------------*
041300150220     C     SRTOTSOG      BEGSR
041400150220     C*
041500150220     c                   clear                   saldo            13 0
041600150220     c                   clear                   nostp
041700150220     c                   clear                   aster
041800150422     C*EXEC SQL
041900150422     C* SELECT sum(MGBQTA) INTO :saldo FROM azmgb00f WHERE MGBSOG =
042000150422     C* :depsog and MGBdat <= :parda1 and substr(mgbcod, 1, 1) = :cgbtsc
042100150422     C*END-EXEC
042200150220     c* i valori sono negativi quando il cliente da a brt bancali e
042300150220     c* positivi quando rendo al cliente
042400150414     c*                  if        saldo < 0
042500150414     c*                  eval      saldo = saldo * -1
042600150414     C*                  EVAL(H)   Stdsco = saldo * cgbsco/100
042700150414     c*                  eval      stdsaldo = saldo - stdsco
042800150414     c*                  else
042900150414     c*                  eval      stdsaldo = saldo *-1
043000150414     c*                  end
043100150220     c* oltre soglia
043200150414     c*                  if        stdsaldo > cgbqta and
043300150414     c*                            cgbqta > 0
043400150414     c*                  eval      aster = '*'
043500150414     c*                  end
043600150220     C*
043700030127     C                   ENDSR
043800030127     C*------------------------------------------------------------------------*
043900030127     C*?REPSALCOD - ROUTINE DI REPERIMENTO DESCRIZIONI ARTICOLO X STAMPA SALDI
044000030127     C*------------------------------------------------------------------------*
044100030127     C     REPSALCOD     BEGSR
044200030127     C*
044300030127     C                   EVAL      STDCOD = DepCOD
044400030127     C* REPERISCO DESCRIZOINE ARTICOLO
044500030127     C                   Z-ADD     1             I
044600030127     C                   SETOFF                                       55
044700030127     C     STDCOD        LOOKUP    SKCODART(I)                            55
044800030127     C                   IF        %found
044900030127     C                   EVAL      STDDCO = SKDESART(I)
045000030127     C                   ELSE
045100030127     C                   EVAL      STDDCO = *ALL'*'
045200030127     C                   ENDIF
045300030127     C*
045400030127     C                   ENDSR
045500030127     C*------------------------------------------------------------------------*
045600030127     C*?STASALSOGTOT - ROUTINE DI STAMPA TOTALE SALDO X SOGGETTO
045700030127     C*------------------------------------------------------------------------*
045800030127     C     STASALSOGTOT  BEGSR
045900030127     C*
046000150218     C                   EVAL      stdsoge= TotSOGe
046100150218     C                   EVAL      Stdsogu= TotSOGu
046200150216     C                   EVAL      Stddif = TotSOGe - totsogu
046300150218     c                   if        stddif > 0
046400150216     C                   EVAL(H)   Stdsco = (TotSOGe - totsogu)* cgbsco/100
046500150216     c                   eval      stdsaldo = TotSOGe - totsogu - stdsco
046600150218     c                   else
046700150218     c                   clear                   stdsco
046800150218     c                   clear                   stdsaldo
046900150218     c                   end
047000030127     C*
047100150218     C                   seton                                        12
047200150211     c                   exsr      stampa
047300030127     C*
047400150211     C                   Z-ADD     *zeros        TotSOGe
047500150211     C                   Z-ADD     *zeros        TotSOGu
047600030127     C*
047700030127     C                   ENDSR
047800030127     C*------------------------------------------------------------------------*
047900030127     C*?STASALCODTOT - ROUTINE DI STAMPA TOTALE SALDO X ARTICOLO
048000030127     C*------------------------------------------------------------------------*
048100030127     C     STASALCODTOT  BEGSR
048200030127     C*
048300150220     c                   if        nostp = ' '
048400150218     C                   EVAL      STdsoge= TotCODe
048500150218     C                   EVAL      STdsogu= TotCODu
048600150216     C*
048700150216     C                   EVAL      Stddif = Totcode - totcodu
048800150218     c                   if        stddif > 0
048900150216     C                   EVAL(H)   Stdsco = (Totcode - totcodu)* cgbsco/100
049000150216     c                   eval      stdsaldo = Totcode - totcodu - stdsco
049100150218     c                   else
049200150218     c                   clear                   stdsco
049300150218     c                   clear                   stdsaldo
049400150218     c                   end
049500150220     C                   Z-ADD     *zeros        stddat
049600150220     C                   add       Stdsco        stdscog
049700150220     c                   add       stdsaldo      stdsaldog
049800150220     C                   add       stdsoge       TotSOGGENe
049900150220     C                   add       Stdsogu       TotSOGGENu
050000030127     C*
050100150220     c                   seton                                        13
050200150211     C                   exsr      stampa
050300150220     c                   end
050400030127     C*
050500150211     C                   Z-ADD     *zeros        TotCODe
050600150211     C                   Z-ADD     *zeros        TotCODu
050700030127     C*
050800030127     C                   ENDSR
050900150211     C*------------------------------------------------------------------------*
051000150211     C*?STASALGENTOT - ROUTINE DI STAMPA TOTALE SALDO GENERALE SOGGETTO
051100150211     C*------------------------------------------------------------------------*
051200150211     C     STASALGENTOT  BEGSR
051300150211     C*
051400150218     C                   EVAL      STdSALe= TotSOGGENe
051500150218     C                   EVAL      STdSALu= TotSOGGENu
051600150216     C*
051700150216     C                   EVAL      Stddifg= stdsale - stdsalu
051800150211     C*
051900150217     C                   seton                                        14
052000150211     c                   exsr      stampa
052100150211     C*
052200150211     C                   Z-ADD     *zeros        TotSOGGENe
052300150211     C                   Z-ADD     *zeros        TotSOGGENu
052400150211     C*
052500150211     C                   ENDSR
052600150402     C*------------------------------------------------------------------------*
052700150402     C     scriviWF      begsr
052800150402     C*------------------------------------------------------------------------*
052900150413     c                   clear                   wfbas000
053000150402     c                   eval      WGBKSU  =  STDSOG
053100150402     c                   eval      WGBRAGS =  STDDSO
053200150402     c                   eval      WGBCOD1 =  STDCOD
053300150417     c                   eval      WGBban  =  STDsogu
053400150408     c     wgbcod1       Chain     azagb01l
053500150408     c                   eval      WGBcodd  =  agbdes
053600150402     c                   if        partp1 = 'C'
053700150402     c                   move      WGBKSU        kspecli
053800150402     c     kFnspe        Chain     Fnspe01l
053900150402     c                   If        %Found(Fnspe01l) and speflg = *Blanks
054000150413     c                   move      '888'         wpetip
054100150402     c                   eval      WPERAG  =  SPERAG
054200150402     c                   eval      WPEIND  =  SPEIND
054300150414     c                   movel     specap        WPECAP
054400150402     c                   eval      WPELOC  =  SPELOC
054500150402     c                   eval      WPEPRO  =  SPEPRO
054600150402     c                   eval      WPENAZ  =  SPENAZ
054700150402     c                   eval      WPETEL  =  spetel
054800150402     c                   eval      WPEPER  =  speper
054900150504     c     ksp2          Chain     Fnsp201l
055000150508     c                   If        %Found(Fnsp201l) and sp2flg = *Blanks
055100150504     c                   movel     sp2est        wmail888
055200150504     c                   endif
055300150413     c                   else
055400150413     C                   EVAL      ACOKUT = 1
055500150413     C                   EVAL      ACOKCC = DUTKCI
055600150413     C                   MOVE      wgbksu        ACOKSC
055700150413     C     KEYACO        CHAIN     CNACO00F
055800150413     C                   IF        %found(CNACO00F)
055900150414     C     KEYACO        CHAIN     CNind00f
056000150413     c                   eval      NTCAPL = 'C'
056100150413     c                   movel     acokcc        ntcnk1
056200150413     c                   movel     acoksc        ntcnk1
056300150414     c                   eval      WPERAG  =  acoRAG
056400150414     c                   eval      WPEIND  =  indvia
056500150414     c                   movel     indcap        WPECAP
056600150414     c                   eval      WPELOC  =  indcit
056700150414     c                   eval      WPEPRO  =  indprv
056800150414     c                   movel     indsta        WPENAZ
056900150413     c                   eval      NTCNK2 = *blank
057000150413     c                   eval      NTCTNT = '05'
057100150414     C     Kntc          CHAIN     tfntc01l
057200150414     C                   IF        %found(tfntc01l)
057300150414     c                   movel     ntcrnt        wpeper
057400150413     c                   EndIf
057500150402     c                   EndIf
057600150402     c                   EndIf
057700150414     c                   EndIf
057800150413     c                   write     wfbas000
057900150402     C                   endsr
058000030124     C*------------------------------------------------------------------------*
058100150211     C* stampa
058200030124     C*------------------------------------------------------------------------*
058300150211     C     stampa        BEGSR
058400150211     c                   if        *in01
058500150211     c                   write     testa
058600150217     c                   write     testata
058700150211     c                   setoff                                       01
058800150220     c  n12              seton                                        15
058900150211     c                   end
059000150220     c*
059100150217     c                   if        *in11 or *in12 or *in13
059200150220     c                   if        *in11
059300150220     c                   setoff                                       16
059400150220     c                   end
059500150220     c     stdsaldo      comp      0                                  16
059600150220     c                   if        *in13 and *in15
059700150220     c                   write     salto
059800150220     c                   end
059900150402     c                   if        parfile = 'S'
060000150402     c                   exsr      scriviWF
060100150402     c                   endif
060200150217     c                   write     DETTAGLIO
060300150218     c                   end
060400150218     C* totali
060500150218     c                   if        *in14
060600150218     c                   write     salto
060700150218     c                   write     fine
060800150218     c                   end
060900150218     c*
061000150217     c                   setoff                                       111213
061100150220     c                   setoff                                       14
061200150211     C                   ENDSR
061300150211     C*------------------------------------------------------------------------*
061400150211     C* CHKDA8 - CONTROLLA LA DATA (8)
061500150211     C*------------------------------------------------------------------------*
061600150211     C     CHKDA8        BEGSR
061700030124     C*
061800030124     C                   IF        DAW8 > *ZEROS
061900030124     C                   Z-ADD     *ZEROS        G08DAT
062000030124     C                   Z-ADD     DAW8          G08INV
062100030124     C                   MOVEL     '3'           G08ERR
062200030124     C                   CALL      'XSRDA8'
062300030124     C                   PARM                    WLBDA8
062400030124     C                   Z-ADD     G08DAT        WDA8
062500030124     C                   Z-ADD     G08INV        DAW8
062600030124     C                   ELSE
062700030124     C                   Z-ADD     WDA8          G08DAT
062800030124     C                   Z-ADD     *ZEROS        G08INV
062900030124     C                   MOVEL     '0'           G08ERR
063000030124     C                   CALL      'XSRDA8'
063100030124     C                   PARM                    WLBDA8
063200030124     C                   Z-ADD     G08DAT        WDA8
063300030124     C                   Z-ADD     G08INV        DAW8
063400030124     C                   ENDIF
063500030124     C*
063600030124     C                   ENDSR
063700020923     C*--------------------------------------------------------------------------------------------*
063800020923     C* REPERISCE I DATI UTENTE
063900020923     C*--------------------------------------------------------------------------------------------*
064000020923     C     REPDATIUTE    BEGSR
064100020923     C*
064200020923     C* INIZIALIZZA VARIABILI DI WRK
064300020923     C                   CLEAR                   TIBS34DS
064400020923     C                   CLEAR                   AZUTEDS
064500020923     C                   CLEAR                   DDATIUTE
064600020923     C*
064700020923     C     *DTAARA       DEFINE    §azute        azuteds
064800020923     C     *DTAARA       DEFINE    §datiute      ddatiute
064900020923     C                   IN(E)     *DTAARA
065000030124     C                   IF        %Error
065100030124     C                   EVAL      I34Tla = 'L'
065200020923     C                   CALL      'TIBS34R'
065300020923     C                   PARM                    Tibs34Ds
065400020923     C                   IN        *DTAARA
065500030124     C                   ENDIF
065600020923     C*
065700020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
065800020923     C                   MOVEL(P)  RSUT          DSFIRS           20
065900020923     C*
066000020923     C                   ENDSR
066100150211     C*--------------------------------------------------------------------------------------------*
066200150211     C* CARTAB - CARICAMENTO DATI TABELLATI
066300150211     C*--------------------------------------------------------------------------------------------*
066400150211     C     CARTAB        BEGSR
066500030124     C*
066600030124     C* CARICAMENTO ARTICOLI
066700030124     C                   Z-ADD     *ZEROS        I
066800030124     C     *LOVAL        SETLL     AZAGB01L                           55
066900030124     C                   IF        *IN55 = *OFF
067000030124     C                   READ      AZAGB01L
067100030124     C                   DOW       not %eof(AZAGB01L)
067200030124     C                   ADD       1             I
067300030124     C                   EVAL      SKCODART(I) = agbCOD
067400030124     C                   EVAL      SKDESART(I) = agbDES
067500030124     C                   READ      AZAGB01L
067600030124     C                   ENDDO
067700030124     C                   ENDIF
067800030124     C*
067900030124     C                   ENDSR
068000150402     c*-------------------------------------------------
068100150402     c     comandi       begsr
068200150402     c*-------------------------------------------------
068300150413     C                   close     wfbas00f
068400150402      *
068500150402     C                   z-add     99            luncmd
068600150402     C                   CALL      'QCMDEXC'
068700150402     C                   parm                    coman
068800150402     C                   parm                    luncmd
068900150402      *
069000150402     c                   endsr
069100000000     C*------------------------------------------------------------------------*
069200000000     C*?*INZSR - IMPOSTAZIONI INIZIALI
069300000000     C*------------------------------------------------------------------------*
069400000000     C     *INZSR        BEGSR
069500000000     C*
069600000000     C* PARAMETRI IN INPUT AL PGM
069700000000     C     *ENTRY        PLIST
069800020719     C                   PARM                    KPJBA
069900030124     C                   MOVEL     KPJBU         PARAM
070000020923     C*
070100150416     C* CHIAVE LETTURA AZMGB04L - PARZIALE
070200150416     C     KEYMGBP       KLIST
070300150416     C                   KFLD                    MGBTIP
070400150416     C                   KFLD                    MGBSOG
070500150416     C                   KFLD                    MGBCOD
070600150416     C*
070700150416     C     Kcgb          KLIST
070800150416     C                   KFLD                    depsog
070900150416     C                   KFLD                    tipoges           1
071000150416      *
071100150416     C     Ktab          KLIST
071200150416     C                   KFLD                    tblkut
071300150416     C                   KFLD                    tblcod
071400150416     C                   KFLD                    tblkey
071500150416     C                   z-add     1             tblkut
071600150416     C*luoghi
071700150416     c     kFnspe        Klist
071800150416     c                   Kfld                    kspefls           1
071900150416     c                   Kfld                    kspecli           7 0
072000150416     c                   Kfld                    kspecod           3
072100150416     c                   move      'L'           kspefls
072200150416     c                   move      '888'         kspecod
072300150504     c     ksp2          Klist
072400150504     c                   Kfld                    kspecli           7 0
072500150504     c                   Kfld                    kspecod           3
072600150504     c                   Kfld                    ktpe              2
072700150504     c                   move      'EM'          ktpe
072800150416     C* CHIAVE LETTURA tfntc00F - COMPLETA
072900150416     C     Kntc          KLIST
073000150416     C                   KFLD                    NTCAPL
073100150416     C                   KFLD                    NTCNK1
073200150416     C                   KFLD                    NTCNK2
073300150416     C                   KFLD                    NTCTNT
073400150416     C*
073500150416     C* CHIAVE LETTURA CNACO00F - COMPLETA
073600150416     C     KEYACO        KLIST
073700150416     C                   KFLD                    ACOKUT
073800150416     C                   KFLD                    ACOKCC
073900150416     C                   KFLD                    ACOKSC
074000150416     C*
074100150416     C* CHIAVE LETTURA TNTBE01L - PARZIALE
074200150416     C     KEYTBEP       KLIST
074300150416     C                   KFLD                    TBECOD
074400150416     C                   KFLD                    TBEKE1
074500150416     C*
074600150416     C* CHIAVE LETTURA AZORG01L - COMPLETA
074700150416     C     KEYORG        KLIST
074800150416     C                   KFLD                    ORGFIL
074900020923     C* REPERISCE I DATI DELL'UTENTE
075000020923     C                   EXSR      REPDATIUTE
075100030124     C*
075200030124     C* CARICA I DATI TABELLATI
075300030124     C                   EXSR      CARTAB
075400030128     C*
075500030128     C* ACCENDO SUBITO QUI INDICATORE X STAMPA DESCRIZIONE SOGGETTO IN GRASSETTO
075600030128     C                   SETON                                        80
075700030127     C*
075800030127     C* REPERISCO DATA E ORA CORRENTI
075900030127     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
076000030127     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
076100030127     C                   MOVEL     WN14          ORACOR            6 0          *ORA  (8) IN H/M/SS
076200030127     C                   Z-ADD     WN8           G08DAT
076300030127     C                   Z-ADD     *ZEROS        G08INV
076400030127     C                   MOVEL     '0'           G08ERR
076500030127     C                   CALL      'XSRDA8'
076600030127     C                   PARM                    WLBDA8
076700030127     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
076800150409      * decodifica area se selezionata
076900150409     c                   if        pararea <> *blank
077000150409     C                   MOVE      pararea       arean             3 0
077100150409     C                   MOVE      pararea       stparea
077200150409     C                   MOVEL     '05'          tblcod
077300150409     C                   MOVEL     pararea       tblkey
077400150409     C     KTAB          CHAIN     TABEL00f
077500150409     C                   IF        %found(tabel00f)
077600150409     C                   MOVEL     tbluni        ds05
077700150409     C                   movel     §05des        stparead
077800150409     C                   Endif
077900150409     c                   endif
078000150409     c                   if        pardal > 0 and paral > 0
078100150409     c                   seton                                        66
078200150409     c                   movel     pardal        iso
078300150409     c                   move      iso           eur
078400150409     c                   move      eur           stpdalp
078500150409     c                   move      paral         iso
078600150409     c                   move      iso           eur
078700150409     c                   move      eur           stpalp
078800150409     c                   endif
078900150410     c                   if        partsc  <> *blank
079000150410     C                   move      partsc        stpges
079100150410     c                   endif
079200150402      *clear file
079300150402     c                   if        parpulf = 'S'
079400150402     c                   movea     cmd1          coman
079500150402     c                   exsr      comandi
079600150402     c                   endif
079700150402     c                   if        parfile = 'S'
079800150413     C                   OPEN      wfbas00f
079900150402     c                   endif
080000030124     C*
080100000000     C                   ENDSR
080200150402**  CMD1
080300150413CLRPFM    FILE(WFBAS00F)
