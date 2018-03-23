000100000000     H*------------------------------------------------------------------------*
000200150211     H* STAtistica moviMENTI GABBIE E BANCALI         ESEGUE STAMPA            *
000300950206     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500950206     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800030618     FAZMGB03L  IF   E           K DISK    RENAME(AZMGB000:AZMGB003) PREFIX(C_)
000900030124     FAZMGB04L  IF   E           K DISK
001000030124     FAZAGB01L  IF   E           K DISK
001100150211     FAZcGB01L  IF   E           K DISK
001200030124     FCNACO00F  IF   E           K DISK
001300150402     Ffnspe01l  iF   E           K DISK
001400030124     FAZORG01L  IF   E           K DISK
001500030124     FTNTBE01L  IF   E           K DISK
001600150409     FTabel00f  IF   E           K DISK
001700150211     FTISIG15p  O    E             PRINTER OFLIND(*IN01)
001800150402     Fwfbar00f  O    E             disk    usropn
001900000000     D*------------------------------------------------------------------------*
002000000000     D* SCHIERE
002100000000     D*------------------------------------------------------------------------*
002200030124     D*------------
002300030124     D* SCHIERA ARTICOLI
002400030124     D*------------
002500150216     D SKCODART        S             15    DIM(100)
002600030127     D SKDESART        S             30    DIM(100)
002700000808     D*------------------
002800000808     D* DS CONTROLLO DATA (8)
002900000808     D*------------------
003000000808     D WLBDA8          DS                  INZ
003100000808     D  G08DAT                 1      8  0
003200000808     D  G08INV                 9     16  0
003300000808     D  G08ERR                17     17
003400000808     D  G08TGI                18     22  0
003500000000     D*------------------
003600000000     D* ARCHITETTURA
003700000000     D*------------------
003800000000     D KPJBA         E DS
003900000000     D*------------------
004000020718     D* DS X PASSAGGIO PARAMETRI
004100020718     D*------------------
004200030124     D PARAM           DS                  INZ
004300150211     D  PARDA1                        8s 0
004400030124     D  PARTP1                        1
004500030124     D  PARSO1                        8
004600030124     D  PARCO1                       15
004700150211     D  PARPO1                        3
004800150211     D  PARper                        3s 0
004900150211     D  PARgia                        1
005000150218     D  PARdet                        1
005100150402     D  PARfile                       1
005200150402     D  PARpulf                       1
005300150409     D  PARDAl                        8s 0
005400150409     D  PARal                         8s 0
005500150409     D  PARarea                       3
005600150410     D  PARtsc                        1
005700020923     D*-------------------
005800020923     D* DS REPERIMENTO DATI UTENTE
005900020923     D*-------------------
006000020923     D TIBS34DS      E DS                                                       *Profili utente
006100020923     D DDATIUTE      E DS                                                       *Dati utente
006200020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006300150409     D Ds05          E DS                                                       *Dati utente
006400030127     D*------------------
006500030127     D* VARIABILI DI TOTALIZZAZIONE
006600030127     D*------------------
006700150217     D iso             S               d
006800150217     D eur             S               d   datfmt(*eur)
006900150409     D wcod3           S              3
007000150220     D nostp           S              1
007100150219     D per             S             10  0 INZ(*zeros)
007200150216     D TotSOGe         S             10  0 INZ(*zeros)
007300150216     D TotSOGu         S             10  0 INZ(*zeros)
007400150217     D TotSOGGENe      S             13  0 INZ(*zeros)
007500150217     D TotSOGGENu      S             13  0 INZ(*zeros)
007600150216     D TotCODe         S             10  0 INZ(*zeros)
007700150216     D TotCODu         S             10  0 INZ(*zeros)
007800030127     D DepTIP          S                   LIKE(mgbTIP)
007900030127     D DepSOG          S                   LIKE(mgbSOG)
008000030311     D DepCOD          S                   LIKE(mgbCOD)
008100150402     d coman           s             99
008200150402     d luncmd          s             15  5
008300020722     D*------------------
008400030124     D* VARIABILI DI WRK
008500020722     D*------------------
008600020722     D I               S              4  0 INZ
008700030124     D WDA8            S              8  0 INZ
008800030124     D DAW8            S              8  0 INZ
008900030124     D W_Fine          S              1    INZ('0')
009000030124     D WRECOKSAL       S              1    INZ('S')
009100150218     d azcgbds       e ds                  extname(azcgb00f)
009200150402     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
009300000000     C*------------------------------------------------------------------------*
009400000000     C*?MAIN LINE
009500000000     C*------------------------------------------------------------------------*
009600020718     C*
009700020718     C* STAMPO LA TESTATA E I PARAMETRI DI LANCIO
009800150211     C                   EXsr      statess
009900150211     C*
010000150211     C                   EVAL      W_Fine = '0'
010100150211     C*
010200150211     C* POSIZIONAMENTO SUL FILE MOVIMENTI
010300150211     C                   EVAL      MGBTIP = PARTP1
010400150211     C                   EVAL      MGBSOG = PARSO1
010500150211     C                   EVAL      MGBCOD = PARCO1
010600150211     C     KEYMGBP       SETLL     AZMGB04L                           99
010700150211     C                   IF        *IN99 = *OFF
010800150211     C                   READ      AZMGB04L
010900150211     C                   DOW       not %eof(AZMGB04L) AND W_Fine = '0'
011000150211     C*
011100150211     C* ESEGUO CONTROLLI DI VALIDITA RISPETTO AI PARAMETRI DI LANCIO
011200150211     C                   EXSR      CHKREC
011300150211     C*
011400150211     C* SE RECORD OK ESEGUO TOTALIZZAZIONE X CLIENTE / ARTICOLO
011500150211     C                   IF        WRECOKSAL = 'S'
011600150211     C                   EXSR      TOTROT
011700150211     C                   ENDIF
011800150211     C                   READ      AZMGB04L
011900150211     C                   ENDDO
012000150211     C*
012100150211     C* A FINE CICLO SCARICO LE MEMORIZZAZIONI SOSPESE (SE RICHIESTO NEL LANCIO)
012200150220     c                   if        nostp = ' '
012300150220     c                   if        contaart > 1
012400150220     c                   setoff                                       15
012500150220     c                   end
012600150219     C                   EXSR      STASALCODTOT
012700150220     C*                  EXSR      STASALSOGTOT
012800150220     c                   end
012900150211     C                   EXSR      STASALGENTOT
013000150211     C                   ENDIF
013100020523     C*
013200020523     C                   SETON                                        LR
013300030127     C*------------------------------------------------------------------------*
013400030127     C*?STATESS - STAMPA LA TESTATA ED I PARAMETRI DI LANCIO DELLA STAMPA SALDI MOVIMENTI
013500030127     C*------------------------------------------------------------------------*
013600030127     C     STATESS       BEGSR
013700030127     C*
013800150211     C                   WRITE     testa
013900030618     C*
014000030127     C                   IF        PARDA1 > *zeros
014100030127     C                   Z-ADD     PARDA1        DAW8                           * Saldo al
014200030127     C                   ELSE
014300030127     C                   Z-ADD     datcor        DAW8
014400030127     C                   ENDIF
014500030127     C                   EXSR      CHKDA8
014600030127     C                   EVAL      STPDAL = WDA8
014700030127     C*
014800030305     C                   IF        PARSO1 <> *zeros
014900030127     C                   MOVEL     PARSO1        STPSOG                         * Soggetto
015000030127     C                   IF        PARTP1 = 'C'
015100030127     C                   EVAL      STPDE1 = '(Cliente)'
015200030127     C                   EVAL      ACOKUT = 1
015300030127     C                   EVAL      ACOKCC = DUTKCI
015400030127     C                   MOVE      PARSO1        ACOKSC
015500030127     C     KEYACO        CHAIN     CNACO00F
015600030127     C                   IF        %found(CNACO00F)
015700030127     C                   EVAL      STPDSO = ACORAG
015800030127     C                   ELSE
015900030127     C                   EVAL      STPDSO = *ALL'*'
016000030127     C                   ENDIF
016100030127     C                   ENDIF
016200030127     C*
016300030127     C                   IF        PARTP1 = 'F'
016400150210     C                   EVAL      STPDE1 = '(Fil.)'
016500030612     C                   MOVE      PARSO1        ORGFIL
016600030127     C     KEYORG        CHAIN     AZORG01L
016700030127     C                   IF        %found(AZORG01L)
016800030127     C                   EVAL      STPDSO = ORGDES
016900030127     C                   ELSE
017000030127     C                   EVAL      STPDSO = *ALL'*'
017100030127     C                   ENDIF
017200030127     C                   ENDIF
017300030127     C*
017400030127     C                   ELSE
017500030128     C                   EVAL      STPSOG = 'Tutti'
017600030127     C                   IF        PARTP1 = 'C'
017700030128     C                   EVAL      STPDSO = 'i clienti'
017800030305     C                   IF        PARPO1 <> *zeros
017900150210     C                   EVAL      STPDSO = %trim(STPDSO)+' del Fil. '+PARPO1
018000030305     C                   ENDIF
018100030127     C                   ENDIF
018200030127     C                   IF        PARTP1 = 'F'
018300150210     C                   EVAL      STPDSO = 'i Fil.'
018400030127     C                   ENDIF
018500030127     C                   ENDIF
018600030127     C*
018700030127     C                   IF        PARCO1 <> *blanks
018800030127     C                   MOVEL     PARCO1        STPCOD                         * Articolo
018900030127     C                   Z-ADD     1             I
019000030127     C                   SETOFF                                       55
019100030127     C     STPCOD        LOOKUP    SKCODART(I)                            55
019200030127     C                   IF        %found
019300030127     C                   EVAL      STPDCO = SKDESART(I)
019400030127     C                   ELSE
019500030128     C                   EVAL      STPDCO = *ALL'*'
019600030127     C                   ENDIF
019700030127     C*
019800030127     C                   ELSE
019900030128     C                   EVAL      STPCOD = 'Tutti'
020000030127     C                   ENDIF
020100030127     C*
020200150211     C                   EVAL      STPper  = parper
020300150219     c                   if        pardet = ' ' or pardet = 'N'
020400150219     c                   eval      stpdet = 'No'
020500150219     c                   else
020600150219     C                   EVAL      STPdet  = 'Si'
020700150219     c                   end
020800150216     c     pargia        comp      'S'                                    40
020900150219     c     pardet        comp      'S'                                    11
021000030127     C*
021100150211     C                   WRITE     parametri
021200150220     c                   seton                                        0115
021300030127     C*
021400030127     C                   ENDSR
021500150211     C*------------------------------------------------------------------------*
021600150211     C*?CHKREC - ROUTINE DI CONTROLLO VALIDITA' RECORD
021700150211     C*------------------------------------------------------------------------*
021800150211     C     CHKREC        BEGSR
021900150211     C*
022000150211     C                   MOVEL     'S'           WRECOKSAL
022100150211     C*
022200150211     C* SE RICHIESTO UN TIPO/SOGGETTO CONTROLLO
022300150216     C                   IF        PARTP1 <> *BLANKS AND
022400150216     C                             MGBTIP = PARTP1
022500150211     C                   ELSE
022600150211     C                   MOVEL     'N'           WRECOKSAL
022700150211     C                   ENDIF
022800150216     C*
022900150211     C                   IF        PARTP1 =  'C'     AND
023000150211     C                             PARPO1 <> *ZEROS  AND
023100150211     C                             PARPO1 <> *BLANKS
023200150211     C                   IF        MGBTIP = PARTP1 AND
023300150211     C                             %subst(MGBSOG:2:3) = PARPO1
023400150211     C                   ELSE
023500150211     C                   MOVEL     'N'           WRECOKSAL
023600150211     C                   ENDIF
023700150211     C                   ENDIF
023800150216     C*
023900150211     C                   IF        PARSO1 <> *ZEROS
024000150211     C                   IF        MGBTIP = PARTP1 AND
024100150211     C                             MGBSOG = PARSO1
024200150211     C                   ELSE
024300150211     C                   EVAL      W_Fine = '1'
024400150211     C                   MOVEL     'N'           WRECOKSAL
024500150211     C                   ENDIF
024600150211     C                   ENDIF
024700150211     C*
024800150211     C* SE RICHIESTO UN ARTICOLO CONTROLLO
024900150216     C                   IF        PARCO1 <> *BLANKS
025000150216     C                   if        MGBCOD = PARCO1
025100150211     C                   ELSE
025200150211     C                   MOVEL     'N'           WRECOKSAL
025300150211     C                   ENDIF
025400150216     C                   ENDIF
025500150409     C* SE RICHIESTO UN codice area
025600150409     C                   IF        PARarea <> *BLANKS
025700150409     c                   if        mgbtip = 'F'
025800150409     c                   move      mgbsog        wcod3
025900150409     c                   else
026000150409     c                   eval      wcod3 = %subst(mgbsog:2:3)
026100150409     c                   endif
026200150409     c                   move      wcod3         orgfil
026300150409     c     keyorg        chain     azorg01l
026400150409     C                   if        orgcar = arean
026500150409     C                   ELSE
026600150409     C                   MOVEL     'N'           WRECOKSAL
026700150409     C                   ENDIF
026800150409     C                   ENDIF
026900150409     C* SE RICHIESTO UN ARTICOLO CONTROLLO
027000150410     C                   IF        PARtsc <> *BLANKS
027100150410     C                   if        %subst(MGBCOD:1:1) = partsc
027200150409     C                   ELSE
027300150409     C                   MOVEL     'N'           WRECOKSAL
027400150409     C                   ENDIF
027500150409     C                   ENDIF
027600150211     C*
027700150211     C* SE RICHIESTA UNA DATA "SALDO AL" CONTROLLO
027800150216     C                   IF        PARDA1 > *ZEROS
027900150216     C                   if        MGBDAT <= PARDA1
028000150211     C                   ELSE
028100150211     C                   MOVEL     'N'           WRECOKSAL
028200150211     C                   ENDIF
028300150216     C                   ENDIF
028400150408     C* SE RICHIESTO periodo
028500150409     C                   IF        PARDAL > 0 and PARAL > 0
028600150409     C                   if        MGBDAT >= PARDAl and mgbdat <= PARAL
028700150409     C                   ELSE
028800150409     C                   MOVEL     'N'           WRECOKSAL
028900150409     C                   ENDIF
029000150409     C                   ENDIF
029100150211     C*
029200150211     C                   ENDSR
029300150216     C*------------------------------------------------------------------------*
029400150216     C*?TOTROT - ROUTINE DI VERIFICA ROTTURE DI CODICE
029500150216     C*------------------------------------------------------------------------*
029600150216     C     TOTROT        BEGSR
029700150219     c*
029800150216     C* VERIFICO ROTTURA X SOGGETTO
029900150216     C                   IF        mgbTIP = DepTIP AND
030000150410     C                             mgbSOG = DepSOG and
030100150410     c                             %subst(mgbcod:1:1) = %subst(depcod:1:1)
030200150216     C                   ELSE
030300150216     C                   IF        DepTIP <> *blanks AND DepSOG <> *blanks
030400150220     c                   if        contaart > 1
030500150220     c                   setoff                                       15
030600150220     c                   end
030700150220     c                   clear                   contaart          2 0
030800150216     C                   EXSR      STASALCODTOT
030900150220     c                   seton                                        15
031000150220     C*                  EXSR      STASALSOGTOT
031100150216     C                   ENDIF
031200150216     C                   EVAL      DepTIP = mgbTIP
031300150216     C                   EVAL      DepSOG = mgbSOG
031400150216     C                   EVAL      DepCOD = mgbCOD
031500150216     C*
031600150216     C                   EXSR      REPSALSOG
031700150220     c                   add       1             contaart
031800150216     C                   EXSR      REPSALCOD
031900150216     C                   ENDIF
032000150216     C*
032100150216     C* VERIFICO ROTTURA X ARTICOLO
032200150216     C                   IF        mgbCOD <> DepCOD
032300150216     C                   IF        DepCOD <> *blanks
032400150220     c* spengo il 15 se ho già stampato un primo art. con tutti i rif.
032500150220     c                   if        contaart > 1
032600150220     c                   setoff                                       15
032700150220     c                   end
032800150216     C                   EXSR      STASALCODTOT
032900150216     C                   ENDIF
033000150216     C                   EVAL      DepCOD = mgbCOD
033100150216     C*
033200150220     c                   add       1             contaart
033300150216     C                   EXSR      REPSALCOD
033400150216     C                   ENDIF
033500150218     C* dettaglio movimento
033600150219     c                   if        pardet = 'S'
033700150218     c                   move      mgbdat        iso
033800150218     c                   move      iso           eur
033900150218     c                   move      eur           stddat
034000150218     c                   if        mgbqta > 0
034100150218     C                   EVAL      Stdsogu= mgbqta
034200150218     c                   clear                   stdsoge
034300150218     c                   else
034400150218     C                   EVAL      stdsoge= mgbqta * -1
034500150218     c                   clear                   stdsogu
034600150218     c                   end
034700150218     C                   EVAL      Stddif = stdsoge - stdsogu
034800150218     C                   seton                                        11
034900150218     C                   exsr      stampa
035000150220     c                   setoff                                       15
035100150218     c                   end
035200150216     C* TOTALIZZO
035300150216     C                   EXSR      MEMDAT
035400150216     C*
035500150216     C                   ENDSR
035600030128     C*------------------------------------------------------------------------*
035700030128     C*?MEMDAT - ROUTINE DI TOTALIZZAZIONE X SALDI
035800030128     C*------------------------------------------------------------------------*
035900030128     C     MEMDAT        BEGSR
036000030128     C* TOTALIZZO
036100150220     c*                  if        mgbqta > 0
036200150220     C*                  EVAL      TotSOGu= TotSOGu+ MGBQTA
036300150220     c*                  else
036400150220     C*                  EVAL      TotSOGe= TotSOGe+ (MGBQTA *-1)
036500150220     c*                  end
036600150218     c                   if        mgbqta > 0
036700150218     C                   EVAL      TotCODu= TotCODu+ MGBQTA
036800150211     c                   else
036900150218     C                   EVAL      TotCODe= TotCODe+ (MGBQTA *-1)
037000150211     c                   end
037100030128     C*
037200030128     C                   ENDSR
037300030127     C*------------------------------------------------------------------------*
037400030127     C*?REPSALSOG - ROUTINE DI REPERIMENTO DESCRIZIONI SOGGETTO X STAMPA SALDI
037500030127     C*------------------------------------------------------------------------*
037600030127     C     REPSALSOG     BEGSR
037700030127     C*
037800150220     C                   MOVEL     DepSOG        STDSog
037900150410     C                   MOVEL     Depcod        tipoges
038000150220     c                   clear                   azcgbds
038100150410     c     kcgb          setll     azcgb01l
038200150220     c                   do        *hival
038300150410     c     kcgb          reade     azcgb01l
038400150220     c                   if        %eof(azcgb01l)
038500150220     c                   leave
038600150220     c                   end
038700150220     c                   if        cgbdti<=parda1 and cgbdtf>=parda1
038800150220     c                   leave
038900150220     c                   else
039000150220     c                   clear                   azcgbds
039100150220     c                   end
039200150220     c                   enddo
039300150220     c* se richiesti solo clienti con eccedenza o oltre sogli
039400150220     c                   exsr      srtotsog
039500030127     C* REPERISCO DESCRIZIONE SOGGETTO
039600030127     C                   IF        DepTIP = 'C'
039700150218     C*                  EVAL      STDDE2 = '(Cli)'
039800030127     C                   EVAL      ACOKUT = 1
039900030127     C                   EVAL      ACOKCC = DUTKCI
040000030127     C                   MOVE      STDSOG        ACOKSC
040100030127     C     KEYACO        CHAIN     CNACO00F
040200030127     C                   IF        %found(CNACO00F)
040300030127     C                   EVAL      STDDSO = ACORAG
040400030127     C                   ELSE
040500030127     C                   EVAL      STDDSO = *ALL'*'
040600030127     C                   ENDIF
040700030127     C                   ENDIF
040800030127     C                   IF        DepTIP = 'F'
040900150218     C*                  EVAL      STDDE2 = '(Fil.)'
041000150219     C                   MOVE      PARSO1        ORGFIL
041100030127     C     KEYORG        CHAIN     AZORG01L
041200030127     C                   IF        %found(AZORG01L)
041300030127     C                   EVAL      STDDSO = ORGDES
041400030127     C                   ELSE
041500030127     C                   EVAL      STDDSO = *ALL'*'
041600030127     C                   ENDIF
041700030127     C                   ENDIF
041800030127     C*
041900150220     C                   ENDSR
042000150220     C*------------------------------------------------------------------------*
042100150220     C*?srtotsog  calcola i totali per cliente                         A SALDI
042200150220     C*------------------------------------------------------------------------*
042300150220     C     SRTOTSOG      BEGSR
042400150220     C*
042500150220     c                   clear                   saldo            13 0
042600150220     c                   clear                   nostp
042700150220     c                   clear                   aster
042800150220     C/EXEC SQL
042900150220     C+ SELECT sum(MGBQTA) INTO :saldo FROM azmgb00f WHERE MGBSOG =
043000150410     C+ :depsog and MGBQTA <= :parda1 and substr(mgbcod, 1, 1) = :cgbtsc
043100150220     C/END-EXEC
043200150220     c* i valori sono negativi quando il cliente da a brt bancali e
043300150220     c* positivi quando rendo al cliente
043400150220     c                   if        saldo < 0
043500150220     c                   eval      saldo = saldo * -1
043600150220     C                   EVAL(H)   Stdsco = saldo * cgbsco/100
043700150220     c                   eval      stdsaldo = saldo - stdsco
043800150220     c                   else
043900150220     c                   eval      stdsaldo = saldo *-1
044000150220     c                   end
044100150220     c* oltre soglia
044200150410     c                   if        stdsaldo > cgbqta and
044300150410     c                             cgbqta > 0
044400150220     c                   eval      aster = '*'
044500150220     c                   end
044600150220     c* se devo stampare solo quelli oltre soglia
044700150220     c                   if        pargia = 'S'  and stdsaldo <= cgbqta
044800150220     c                   eval      nostp = 'S'
044900150220     c                   end
045000150220     c* se devo stampare solo quelli oltre la % di diff. richiesta e la
045100150220     c* soglia è 0 non stampo
045200150220     c                   if        stpper <>0 and cgbqta = 0
045300150220     c                   eval      nostp = 'S'
045400150220     c                   end
045500150220     c* se devo stampare solo quelli oltre la % di diff. richiesta
045600150220     c                   if        stpper <>0
045700150220     c                             and (stdsaldo - cgbqta) > 0 and cgbqta>0
045800150220     c                   eval(h)   per = 100 *(stdsaldo-cgbqta)/ cgbqta
045900150220     c                   if        per < stpper
046000150220     c                   eval      nostp = 'S'
046100150220     c                   end
046200150220     c                   end
046300150220     C*
046400030127     C                   ENDSR
046500030127     C*------------------------------------------------------------------------*
046600030127     C*?REPSALCOD - ROUTINE DI REPERIMENTO DESCRIZIONI ARTICOLO X STAMPA SALDI
046700030127     C*------------------------------------------------------------------------*
046800030127     C     REPSALCOD     BEGSR
046900030127     C*
047000030127     C                   EVAL      STDCOD = DepCOD
047100030127     C* REPERISCO DESCRIZOINE ARTICOLO
047200030127     C                   Z-ADD     1             I
047300030127     C                   SETOFF                                       55
047400030127     C     STDCOD        LOOKUP    SKCODART(I)                            55
047500030127     C                   IF        %found
047600030127     C                   EVAL      STDDCO = SKDESART(I)
047700030127     C                   ELSE
047800030127     C                   EVAL      STDDCO = *ALL'*'
047900030127     C                   ENDIF
048000030127     C*
048100030127     C                   ENDSR
048200030127     C*------------------------------------------------------------------------*
048300030127     C*?STASALSOGTOT - ROUTINE DI STAMPA TOTALE SALDO X SOGGETTO
048400030127     C*------------------------------------------------------------------------*
048500030127     C     STASALSOGTOT  BEGSR
048600030127     C*
048700150218     C                   EVAL      stdsoge= TotSOGe
048800150218     C                   EVAL      Stdsogu= TotSOGu
048900150216     C                   EVAL      Stddif = TotSOGe - totsogu
049000150218     c                   if        stddif > 0
049100150216     C                   EVAL(H)   Stdsco = (TotSOGe - totsogu)* cgbsco/100
049200150216     c                   eval      stdsaldo = TotSOGe - totsogu - stdsco
049300150218     c                   else
049400150218     c                   clear                   stdsco
049500150218     c                   clear                   stdsaldo
049600150218     c                   end
049700030127     C*
049800150218     C                   seton                                        12
049900150211     c                   exsr      stampa
050000030127     C*
050100150211     C                   Z-ADD     *zeros        TotSOGe
050200150211     C                   Z-ADD     *zeros        TotSOGu
050300030127     C*
050400030127     C                   ENDSR
050500030127     C*------------------------------------------------------------------------*
050600030127     C*?STASALCODTOT - ROUTINE DI STAMPA TOTALE SALDO X ARTICOLO
050700030127     C*------------------------------------------------------------------------*
050800030127     C     STASALCODTOT  BEGSR
050900030127     C*
051000150220     c                   if        nostp = ' '
051100150218     C                   EVAL      STdsoge= TotCODe
051200150218     C                   EVAL      STdsogu= TotCODu
051300150216     C*
051400150216     C                   EVAL      Stddif = Totcode - totcodu
051500150218     c                   if        stddif > 0
051600150216     C                   EVAL(H)   Stdsco = (Totcode - totcodu)* cgbsco/100
051700150216     c                   eval      stdsaldo = Totcode - totcodu - stdsco
051800150218     c                   else
051900150218     c                   clear                   stdsco
052000150218     c                   clear                   stdsaldo
052100150218     c                   end
052200150220     C                   Z-ADD     *zeros        stddat
052300150220     C                   add       Stdsco        stdscog
052400150220     c                   add       stdsaldo      stdsaldog
052500150220     C                   add       stdsoge       TotSOGGENe
052600150220     C                   add       Stdsogu       TotSOGGENu
052700030127     C*
052800150220     c                   seton                                        13
052900150211     C                   exsr      stampa
053000150220     c                   end
053100030127     C*
053200150211     C                   Z-ADD     *zeros        TotCODe
053300150211     C                   Z-ADD     *zeros        TotCODu
053400030127     C*
053500030127     C                   ENDSR
053600150211     C*------------------------------------------------------------------------*
053700150211     C*?STASALGENTOT - ROUTINE DI STAMPA TOTALE SALDO GENERALE SOGGETTO
053800150211     C*------------------------------------------------------------------------*
053900150211     C     STASALGENTOT  BEGSR
054000150211     C*
054100150218     C                   EVAL      STdSALe= TotSOGGENe
054200150218     C                   EVAL      STdSALu= TotSOGGENu
054300150216     C*
054400150216     C                   EVAL      Stddifg= stdsale - stdsalu
054500150211     C*
054600150217     C                   seton                                        14
054700150211     c                   exsr      stampa
054800150211     C*
054900150211     C                   Z-ADD     *zeros        TotSOGGENe
055000150211     C                   Z-ADD     *zeros        TotSOGGENu
055100150211     C*
055200150211     C                   ENDSR
055300150402     C*------------------------------------------------------------------------*
055400150402     C     scriviWF      begsr
055500150402     C*------------------------------------------------------------------------*
055600150402     c                   clear                   wfbar000
055700150402     c                   eval      WGBKSU  =  STDSOG
055800150402     c                   eval      WGBRAGS =  STDDSO
055900150402     c                   eval      WGBCOD1 =  STDCOD
056000150408     c                   eval      WGBban  =  STDsaldo
056100150408     c     wgbcod1       Chain     azagb01l
056200150408     c                   eval      WGBcodd  =  agbdes
056300150402     c                   if        partp1 = 'C'
056400150402     c                   move      WGBKSU        kspecli
056500150402     c     kFnspe        Chain     Fnspe01l
056600150402     c                   If        %Found(Fnspe01l) and speflg = *Blanks
056700150402     c                   eval      WPERAG  =  SPERAG
056800150402     c                   eval      WPEIND  =  SPEIND
056900150402     c                   eval      WPECAP  =  SPECAP
057000150402     c                   eval      WPELOC  =  SPELOC
057100150402     c                   eval      WPEPRO  =  SPEPRO
057200150402     c                   eval      WPENAZ  =  SPENAZ
057300150402     c                   eval      WPETEL  =  spetel
057400150402     c                   eval      WPEPER  =  speper
057500150402     c                   EndIf
057600150402     c                   EndIf
057700150402     c                   write     wfbar000
057800150402     C                   endsr
057900030124     C*------------------------------------------------------------------------*
058000150211     C* stampa
058100030124     C*------------------------------------------------------------------------*
058200150211     C     stampa        BEGSR
058300150211     c                   if        *in01
058400150211     c                   write     testa
058500150217     c                   write     testata
058600150211     c                   setoff                                       01
058700150220     c  n12              seton                                        15
058800150211     c                   end
058900150220     c*
059000150217     c                   if        *in11 or *in12 or *in13
059100150220     c                   if        *in11
059200150220     c                   setoff                                       16
059300150220     c                   end
059400150220     c     stdsaldo      comp      0                                  16
059500150220     c                   if        *in13 and *in15
059600150220     c                   write     salto
059700150220     c                   end
059800150402     c                   if        parfile = 'S'
059900150402     c                   exsr      scriviWF
060000150402     c                   endif
060100150217     c                   write     DETTAGLIO
060200150218     c                   end
060300150218     C* totali
060400150218     c                   if        *in14
060500150218     c                   write     salto
060600150218     c                   write     fine
060700150218     c                   end
060800150218     c*
060900150217     c                   setoff                                       111213
061000150220     c                   setoff                                       14
061100150211     C                   ENDSR
061200150211     C*------------------------------------------------------------------------*
061300150211     C* CHKDA8 - CONTROLLA LA DATA (8)
061400150211     C*------------------------------------------------------------------------*
061500150211     C     CHKDA8        BEGSR
061600030124     C*
061700030124     C                   IF        DAW8 > *ZEROS
061800030124     C                   Z-ADD     *ZEROS        G08DAT
061900030124     C                   Z-ADD     DAW8          G08INV
062000030124     C                   MOVEL     '3'           G08ERR
062100030124     C                   CALL      'XSRDA8'
062200030124     C                   PARM                    WLBDA8
062300030124     C                   Z-ADD     G08DAT        WDA8
062400030124     C                   Z-ADD     G08INV        DAW8
062500030124     C                   ELSE
062600030124     C                   Z-ADD     WDA8          G08DAT
062700030124     C                   Z-ADD     *ZEROS        G08INV
062800030124     C                   MOVEL     '0'           G08ERR
062900030124     C                   CALL      'XSRDA8'
063000030124     C                   PARM                    WLBDA8
063100030124     C                   Z-ADD     G08DAT        WDA8
063200030124     C                   Z-ADD     G08INV        DAW8
063300030124     C                   ENDIF
063400030124     C*
063500030124     C                   ENDSR
063600020923     C*--------------------------------------------------------------------------------------------*
063700020923     C* REPERISCE I DATI UTENTE
063800020923     C*--------------------------------------------------------------------------------------------*
063900020923     C     REPDATIUTE    BEGSR
064000020923     C*
064100020923     C* INIZIALIZZA VARIABILI DI WRK
064200020923     C                   CLEAR                   TIBS34DS
064300020923     C                   CLEAR                   AZUTEDS
064400020923     C                   CLEAR                   DDATIUTE
064500020923     C*
064600020923     C     *DTAARA       DEFINE    §azute        azuteds
064700020923     C     *DTAARA       DEFINE    §datiute      ddatiute
064800020923     C                   IN(E)     *DTAARA
064900030124     C                   IF        %Error
065000030124     C                   EVAL      I34Tla = 'L'
065100020923     C                   CALL      'TIBS34R'
065200020923     C                   PARM                    Tibs34Ds
065300020923     C                   IN        *DTAARA
065400030124     C                   ENDIF
065500020923     C*
065600020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
065700020923     C                   MOVEL(P)  RSUT          DSFIRS           20
065800020923     C*
065900020923     C                   ENDSR
066000150211     C*--------------------------------------------------------------------------------------------*
066100150211     C* CARTAB - CARICAMENTO DATI TABELLATI
066200150211     C*--------------------------------------------------------------------------------------------*
066300150211     C     CARTAB        BEGSR
066400030124     C*
066500030124     C* CARICAMENTO ARTICOLI
066600030124     C                   Z-ADD     *ZEROS        I
066700030124     C     *LOVAL        SETLL     AZAGB01L                           55
066800030124     C                   IF        *IN55 = *OFF
066900030124     C                   READ      AZAGB01L
067000030124     C                   DOW       not %eof(AZAGB01L)
067100030124     C                   ADD       1             I
067200030124     C                   EVAL      SKCODART(I) = agbCOD
067300030124     C                   EVAL      SKDESART(I) = agbDES
067400030124     C                   READ      AZAGB01L
067500030124     C                   ENDDO
067600030124     C                   ENDIF
067700030124     C*
067800030124     C                   ENDSR
067900150402     c*-------------------------------------------------
068000150402     c     comandi       begsr
068100150402     c*-------------------------------------------------
068200150402     C                   close     wfbar00f
068300150402      *
068400150402     C                   z-add     99            luncmd
068500150402     C                   CALL      'QCMDEXC'
068600150402     C                   parm                    coman
068700150402     C                   parm                    luncmd
068800150402      *
068900150402     c                   endsr
069000000000     C*------------------------------------------------------------------------*
069100000000     C*?*INZSR - IMPOSTAZIONI INIZIALI
069200000000     C*------------------------------------------------------------------------*
069300000000     C     *INZSR        BEGSR
069400000000     C*
069500000000     C* PARAMETRI IN INPUT AL PGM
069600000000     C     *ENTRY        PLIST
069700020719     C                   PARM                    KPJBA
069800030124     C                   MOVEL     KPJBU         PARAM
069900020923     C*
070000020923     C* REPERISCE I DATI DELL'UTENTE
070100020923     C                   EXSR      REPDATIUTE
070200030124     C*
070300030124     C* CARICA I DATI TABELLATI
070400030124     C                   EXSR      CARTAB
070500030128     C*
070600030128     C* ACCENDO SUBITO QUI INDICATORE X STAMPA DESCRIZIONE SOGGETTO IN GRASSETTO
070700030128     C                   SETON                                        80
070800030127     C*
070900030127     C* REPERISCO DATA E ORA CORRENTI
071000030127     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
071100030127     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
071200030127     C                   MOVEL     WN14          ORACOR            6 0          *ORA  (8) IN H/M/SS
071300030127     C                   Z-ADD     WN8           G08DAT
071400030127     C                   Z-ADD     *ZEROS        G08INV
071500030127     C                   MOVEL     '0'           G08ERR
071600030127     C                   CALL      'XSRDA8'
071700030127     C                   PARM                    WLBDA8
071800030127     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
071900150409      * decodifica area se selezionata
072000150409     c                   if        pararea <> *blank
072100150409     C                   MOVE      pararea       arean             3 0
072200150409     C                   MOVE      pararea       stparea
072300150409     C                   MOVEL     '05'          tblcod
072400150409     C                   MOVEL     pararea       tblkey
072500150409     C     KTAB          CHAIN     TABEL00f
072600150409     C                   IF        %found(tabel00f)
072700150409     C                   MOVEL     tbluni        ds05
072800150409     C                   movel     §05des        stparead
072900150409     C                   Endif
073000150409     c                   endif
073100150409     c                   if        pardal > 0 and paral > 0
073200150409     c                   seton                                        66
073300150409     c                   movel     pardal        iso
073400150409     c                   move      iso           eur
073500150409     c                   move      eur           stpdalp
073600150409     c                   move      paral         iso
073700150409     c                   move      iso           eur
073800150409     c                   move      eur           stpalp
073900150409     c                   endif
074000150410     c                   if        partsc  <> *blank
074100150410     C                   move      partsc        stpges
074200150410     c                   endif
074300150402      *clear file
074400150402     c                   if        parpulf = 'S'
074500150402     c                   movea     cmd1          coman
074600150402     c                   exsr      comandi
074700150402     c                   endif
074800150402     c                   if        parfile = 'S'
074900150402     C                   OPEN      wfbar00f
075000150402     c                   endif
075100030124     C*
075200030124     C* CHIAVE LETTURA AZMGB04L - PARZIALE
075300030124     C     KEYMGBP       KLIST
075400030124     C                   KFLD                    MGBTIP
075500030124     C                   KFLD                    MGBSOG
075600030124     C                   KFLD                    MGBCOD
075700020718     C*
075800150410     C     Kcgb          KLIST
075900150410     C                   KFLD                    depsog
076000150410     C                   KFLD                    tipoges           1
076100150410      *
076200150409     C     Ktab          KLIST
076300150409     C                   KFLD                    tblkut
076400150409     C                   KFLD                    tblcod
076500150409     C                   KFLD                    tblkey
076600150409     C                   z-add     1             tblkut
076700150402     C*luoghi
076800150402     c     kFnspe        Klist
076900150402     c                   Kfld                    kspefls           1
077000150402     c                   Kfld                    kspecli           7 0
077100150402     c                   Kfld                    kspecod           3
077200150402     c                   move      'L'           kspefls
077300150402     c                   move      '888'         kspecod
077400020718     C* CHIAVE LETTURA CNACO00F - COMPLETA
077500020718     C     KEYACO        KLIST
077600020718     C                   KFLD                    ACOKUT
077700020718     C                   KFLD                    ACOKCC
077800020718     C                   KFLD                    ACOKSC
077900020722     C*
078000030124     C* CHIAVE LETTURA TNTBE01L - PARZIALE
078100030124     C     KEYTBEP       KLIST
078200030124     C                   KFLD                    TBECOD
078300030124     C                   KFLD                    TBEKE1
078400030124     C*
078500030124     C* CHIAVE LETTURA AZORG01L - COMPLETA
078600030124     C     KEYORG        KLIST
078700030124     C                   KFLD                    ORGFIL
078800000000     C                   ENDSR
078900150402**  CMD1
079000150402CLRPFM    FILE(WFBAR00F)
