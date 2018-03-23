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
023900150430     C                   IF        PARSO1 <> *ZEROS and
024000150430     C                             PARSO1 <> *blank
024100150211     C                   IF        MGBTIP = PARTP1 AND
024200150211     C                             MGBSOG = PARSO1
024300150211     C                   ELSE
024400150211     C                   EVAL      W_Fine = '1'
024500150211     C                   MOVEL     'N'           WRECOKSAL
024600150211     C                   ENDIF
024700150211     C                   ENDIF
024800150211     C*
024900150211     C* SE RICHIESTO UN ARTICOLO CONTROLLO
025000150216     C                   IF        PARCO1 <> *BLANKS
025100150216     C                   if        MGBCOD = PARCO1
025200150211     C                   ELSE
025300150211     C                   MOVEL     'N'           WRECOKSAL
025400150211     C                   ENDIF
025500150216     C                   ENDIF
025600150409     C* SE RICHIESTO UN codice area
025700150409     C                   IF        PARarea <> *BLANKS
025800150409     c                   if        mgbtip = 'F'
025900150409     c                   move      mgbsog        wcod3
026000150409     c                   else
026100150409     c                   eval      wcod3 = %subst(mgbsog:2:3)
026200150409     c                   endif
026300150409     c                   move      wcod3         orgfil
026400150409     c     keyorg        chain     azorg01l
026500150409     C                   if        orgcar = arean
026600150409     C                   ELSE
026700150409     C                   MOVEL     'N'           WRECOKSAL
026800150409     C                   ENDIF
026900150409     C                   ENDIF
027000150409     C* SE RICHIESTO UN ARTICOLO CONTROLLO
027100150410     C                   IF        PARtsc <> *BLANKS
027200150410     C                   if        %subst(MGBCOD:1:1) = partsc
027300150409     C                   ELSE
027400150409     C                   MOVEL     'N'           WRECOKSAL
027500150409     C                   ENDIF
027600150409     C                   ENDIF
027700150211     C*
027800150211     C* SE RICHIESTA UNA DATA "SALDO AL" CONTROLLO
027900150216     C                   IF        PARDA1 > *ZEROS
028000150216     C                   if        MGBDAT <= PARDA1
028100150211     C                   ELSE
028200150211     C                   MOVEL     'N'           WRECOKSAL
028300150211     C                   ENDIF
028400150216     C                   ENDIF
028500150408     C* SE RICHIESTO periodo
028600150409     C                   IF        PARDAL > 0 and PARAL > 0
028700150409     C                   if        MGBDAT >= PARDAl and mgbdat <= PARAL
028800150409     C                   ELSE
028900150409     C                   MOVEL     'N'           WRECOKSAL
029000150409     C                   ENDIF
029100150409     C                   ENDIF
029200150211     C*
029300150211     C                   ENDSR
029400150216     C*------------------------------------------------------------------------*
029500150216     C*?TOTROT - ROUTINE DI VERIFICA ROTTURE DI CODICE
029600150216     C*------------------------------------------------------------------------*
029700150216     C     TOTROT        BEGSR
029800150219     c*
029900150216     C* VERIFICO ROTTURA X SOGGETTO
030000150216     C                   IF        mgbTIP = DepTIP AND
030100150410     C                             mgbSOG = DepSOG and
030200150410     c                             %subst(mgbcod:1:1) = %subst(depcod:1:1)
030300150216     C                   ELSE
030400150216     C                   IF        DepTIP <> *blanks AND DepSOG <> *blanks
030500150220     c                   if        contaart > 1
030600150220     c                   setoff                                       15
030700150220     c                   end
030800150220     c                   clear                   contaart          2 0
030900150216     C                   EXSR      STASALCODTOT
031000150220     c                   seton                                        15
031100150220     C*                  EXSR      STASALSOGTOT
031200150216     C                   ENDIF
031300150216     C                   EVAL      DepTIP = mgbTIP
031400150216     C                   EVAL      DepSOG = mgbSOG
031500150216     C                   EVAL      DepCOD = mgbCOD
031600150216     C*
031700150216     C                   EXSR      REPSALSOG
031800150220     c                   add       1             contaart
031900150216     C                   EXSR      REPSALCOD
032000150216     C                   ENDIF
032100150216     C*
032200150216     C* VERIFICO ROTTURA X ARTICOLO
032300150216     C                   IF        mgbCOD <> DepCOD
032400150216     C                   IF        DepCOD <> *blanks
032500150220     c* spengo il 15 se ho già stampato un primo art. con tutti i rif.
032600150220     c                   if        contaart > 1
032700150220     c                   setoff                                       15
032800150220     c                   end
032900150216     C                   EXSR      STASALCODTOT
033000150216     C                   ENDIF
033100150216     C                   EVAL      DepCOD = mgbCOD
033200150216     C*
033300150220     c                   add       1             contaart
033400150216     C                   EXSR      REPSALCOD
033500150216     C                   ENDIF
033600150218     C* dettaglio movimento
033700150219     c                   if        pardet = 'S'
033800150218     c                   move      mgbdat        iso
033900150218     c                   move      iso           eur
034000150218     c                   move      eur           stddat
034100150218     c                   if        mgbqta > 0
034200150218     C                   EVAL      Stdsogu= mgbqta
034300150218     c                   clear                   stdsoge
034400150218     c                   else
034500150218     C                   EVAL      stdsoge= mgbqta * -1
034600150218     c                   clear                   stdsogu
034700150218     c                   end
034800150218     C                   EVAL      Stddif = stdsoge - stdsogu
034900150218     C                   seton                                        11
035000150218     C                   exsr      stampa
035100150220     c                   setoff                                       15
035200150218     c                   end
035300150216     C* TOTALIZZO
035400150216     C                   EXSR      MEMDAT
035500150216     C*
035600150216     C                   ENDSR
035700030128     C*------------------------------------------------------------------------*
035800030128     C*?MEMDAT - ROUTINE DI TOTALIZZAZIONE X SALDI
035900030128     C*------------------------------------------------------------------------*
036000030128     C     MEMDAT        BEGSR
036100030128     C* TOTALIZZO
036200150220     c*                  if        mgbqta > 0
036300150220     C*                  EVAL      TotSOGu= TotSOGu+ MGBQTA
036400150220     c*                  else
036500150220     C*                  EVAL      TotSOGe= TotSOGe+ (MGBQTA *-1)
036600150220     c*                  end
036700150218     c                   if        mgbqta > 0
036800150218     C                   EVAL      TotCODu= TotCODu+ MGBQTA
036900150211     c                   else
037000150218     C                   EVAL      TotCODe= TotCODe+ (MGBQTA *-1)
037100150211     c                   end
037200030128     C*
037300030128     C                   ENDSR
037400030127     C*------------------------------------------------------------------------*
037500030127     C*?REPSALSOG - ROUTINE DI REPERIMENTO DESCRIZIONI SOGGETTO X STAMPA SALDI
037600030127     C*------------------------------------------------------------------------*
037700030127     C     REPSALSOG     BEGSR
037800030127     C*
037900150220     C                   MOVEL     DepSOG        STDSog
038000150410     C                   MOVEL     Depcod        tipoges
038100150220     c                   clear                   azcgbds
038200150522     c     kcgb          chain     azcgb01l
038300150522     c                   if        not %found(azcgb01l)
038400150522     c                   clear                   azcgbds
038500150522     c                   endif
038600150522     c*    kcgb          setll     azcgb01l
038700150522     c*                  do        *hival
038800150522     c*    kcgb          reade     azcgb01l
038900150522     c*                  if        %eof(azcgb01l)
039000150522     c*                  leave
039100150522     c*                  end
039200150522     c*                  if        cgbdti<=parda1 and cgbdtf>=parda1
039300150522     c*                  leave
039400150522     c*                  else
039500150522     c*                  clear                   azcgbds
039600150522     c*                  end
039700150522     c*                  enddo
039800150220     c* se richiesti solo clienti con eccedenza o oltre sogli
039900150220     c                   exsr      srtotsog
040000030127     C* REPERISCO DESCRIZIONE SOGGETTO
040100030127     C                   IF        DepTIP = 'C'
040200150218     C*                  EVAL      STDDE2 = '(Cli)'
040300030127     C                   EVAL      ACOKUT = 1
040400030127     C                   EVAL      ACOKCC = DUTKCI
040500030127     C                   MOVE      STDSOG        ACOKSC
040600030127     C     KEYACO        CHAIN     CNACO00F
040700030127     C                   IF        %found(CNACO00F)
040800030127     C                   EVAL      STDDSO = ACORAG
040900030127     C                   ELSE
041000030127     C                   EVAL      STDDSO = *ALL'*'
041100030127     C                   ENDIF
041200030127     C                   ENDIF
041300030127     C                   IF        DepTIP = 'F'
041400150218     C*                  EVAL      STDDE2 = '(Fil.)'
041500150504     C                   MOVE      stdsog        ORGFIL
041600030127     C     KEYORG        CHAIN     AZORG01L
041700030127     C                   IF        %found(AZORG01L)
041800030127     C                   EVAL      STDDSO = ORGDES
041900030127     C                   ELSE
042000030127     C                   EVAL      STDDSO = *ALL'*'
042100030127     C                   ENDIF
042200030127     C                   ENDIF
042300030127     C*
042400150220     C                   ENDSR
042500150220     C*------------------------------------------------------------------------*
042600150220     C*?srtotsog  calcola i totali per cliente                         A SALDI
042700150220     C*------------------------------------------------------------------------*
042800150220     C     SRTOTSOG      BEGSR
042900150220     C*
043000150220     c                   clear                   saldo            13 0
043100150220     c                   clear                   nostp
043200150220     c                   clear                   aster
043300150220     C/EXEC SQL
043400150220     C+ SELECT sum(MGBQTA) INTO :saldo FROM azmgb00f WHERE MGBSOG =
043500150414     C+ :depsog and MGBDAT <= :parda1 and substr(mgbcod, 1, 1) = :cgbtsc
043600150220     C/END-EXEC
043700150220     c* i valori sono negativi quando il cliente da a brt bancali e
043800150220     c* positivi quando rendo al cliente
043900150220     c                   if        saldo < 0
044000150220     c                   eval      saldo = saldo * -1
044100150220     C                   EVAL(H)   Stdsco = saldo * cgbsco/100
044200150220     c                   eval      stdsaldo = saldo - stdsco
044300150220     c                   else
044400150220     c                   eval      stdsaldo = saldo *-1
044500150220     c                   end
044600150220     c* oltre soglia
044700150410     c                   if        stdsaldo > cgbqta and
044800150410     c                             cgbqta > 0
044900150220     c                   eval      aster = '*'
045000150220     c                   end
045100150220     c* se devo stampare solo quelli oltre soglia
045200150220     c                   if        pargia = 'S'  and stdsaldo <= cgbqta
045300150220     c                   eval      nostp = 'S'
045400150220     c                   end
045500150220     c* se devo stampare solo quelli oltre la % di diff. richiesta e la
045600150220     c* soglia è 0 non stampo
045700150220     c                   if        stpper <>0 and cgbqta = 0
045800150220     c                   eval      nostp = 'S'
045900150220     c                   end
046000150220     c* se devo stampare solo quelli oltre la % di diff. richiesta
046100150220     c                   if        stpper <>0
046200150220     c                             and (stdsaldo - cgbqta) > 0 and cgbqta>0
046300150220     c                   eval(h)   per = 100 *(stdsaldo-cgbqta)/ cgbqta
046400150220     c                   if        per < stpper
046500150220     c                   eval      nostp = 'S'
046600150220     c                   end
046700150220     c                   end
046800150220     C*
046900030127     C                   ENDSR
047000030127     C*------------------------------------------------------------------------*
047100030127     C*?REPSALCOD - ROUTINE DI REPERIMENTO DESCRIZIONI ARTICOLO X STAMPA SALDI
047200030127     C*------------------------------------------------------------------------*
047300030127     C     REPSALCOD     BEGSR
047400030127     C*
047500030127     C                   EVAL      STDCOD = DepCOD
047600030127     C* REPERISCO DESCRIZOINE ARTICOLO
047700030127     C                   Z-ADD     1             I
047800030127     C                   SETOFF                                       55
047900030127     C     STDCOD        LOOKUP    SKCODART(I)                            55
048000030127     C                   IF        %found
048100030127     C                   EVAL      STDDCO = SKDESART(I)
048200030127     C                   ELSE
048300030127     C                   EVAL      STDDCO = *ALL'*'
048400030127     C                   ENDIF
048500030127     C*
048600030127     C                   ENDSR
048700030127     C*------------------------------------------------------------------------*
048800030127     C*?STASALSOGTOT - ROUTINE DI STAMPA TOTALE SALDO X SOGGETTO
048900030127     C*------------------------------------------------------------------------*
049000030127     C     STASALSOGTOT  BEGSR
049100030127     C*
049200150218     C                   EVAL      stdsoge= TotSOGe
049300150218     C                   EVAL      Stdsogu= TotSOGu
049400150216     C                   EVAL      Stddif = TotSOGe - totsogu
049500150218     c                   if        stddif > 0
049600150216     C                   EVAL(H)   Stdsco = (TotSOGe - totsogu)* cgbsco/100
049700150216     c                   eval      stdsaldo = TotSOGe - totsogu - stdsco
049800150218     c                   else
049900150218     c                   clear                   stdsco
050000150218     c                   clear                   stdsaldo
050100150218     c                   end
050200030127     C*
050300150218     C                   seton                                        12
050400150211     c                   exsr      stampa
050500030127     C*
050600150211     C                   Z-ADD     *zeros        TotSOGe
050700150211     C                   Z-ADD     *zeros        TotSOGu
050800030127     C*
050900030127     C                   ENDSR
051000030127     C*------------------------------------------------------------------------*
051100030127     C*?STASALCODTOT - ROUTINE DI STAMPA TOTALE SALDO X ARTICOLO
051200030127     C*------------------------------------------------------------------------*
051300030127     C     STASALCODTOT  BEGSR
051400030127     C*
051500150220     c                   if        nostp = ' '
051600150218     C                   EVAL      STdsoge= TotCODe
051700150218     C                   EVAL      STdsogu= TotCODu
051800150216     C*
051900150216     C                   EVAL      Stddif = Totcode - totcodu
052000150218     c                   if        stddif > 0
052100150216     C                   EVAL(H)   Stdsco = (Totcode - totcodu)* cgbsco/100
052200150216     c                   eval      stdsaldo = Totcode - totcodu - stdsco
052300150218     c                   else
052400150218     c                   clear                   stdsco
052500150218     c                   clear                   stdsaldo
052600150218     c                   end
052700150220     C                   Z-ADD     *zeros        stddat
052800150220     C                   add       Stdsco        stdscog
052900150220     c                   add       stdsaldo      stdsaldog
053000150220     C                   add       stdsoge       TotSOGGENe
053100150220     C                   add       Stdsogu       TotSOGGENu
053200030127     C*
053300150220     c                   seton                                        13
053400150211     C                   exsr      stampa
053500150220     c                   end
053600030127     C*
053700150211     C                   Z-ADD     *zeros        TotCODe
053800150211     C                   Z-ADD     *zeros        TotCODu
053900030127     C*
054000030127     C                   ENDSR
054100150211     C*------------------------------------------------------------------------*
054200150211     C*?STASALGENTOT - ROUTINE DI STAMPA TOTALE SALDO GENERALE SOGGETTO
054300150211     C*------------------------------------------------------------------------*
054400150211     C     STASALGENTOT  BEGSR
054500150211     C*
054600150218     C                   EVAL      STdSALe= TotSOGGENe
054700150218     C                   EVAL      STdSALu= TotSOGGENu
054800150216     C*
054900150216     C                   EVAL      Stddifg= stdsale - stdsalu
055000150211     C*
055100150217     C                   seton                                        14
055200150211     c                   exsr      stampa
055300150211     C*
055400150211     C                   Z-ADD     *zeros        TotSOGGENe
055500150211     C                   Z-ADD     *zeros        TotSOGGENu
055600150211     C*
055700150211     C                   ENDSR
055800150402     C*------------------------------------------------------------------------*
055900150402     C     scriviWF      begsr
056000150402     C*------------------------------------------------------------------------*
056100150402     c                   clear                   wfbar000
056200150402     c                   eval      WGBKSU  =  STDSOG
056300150402     c                   eval      WGBRAGS =  STDDSO
056400150402     c                   eval      WGBCOD1 =  STDCOD
056500150408     c                   eval      WGBban  =  STDsaldo
056600150408     c     wgbcod1       Chain     azagb01l
056700150408     c                   eval      WGBcodd  =  agbdes
056800150402     c                   if        partp1 = 'C'
056900150402     c                   move      WGBKSU        kspecli
057000150402     c     kFnspe        Chain     Fnspe01l
057100150402     c                   If        %Found(Fnspe01l) and speflg = *Blanks
057200150402     c                   eval      WPERAG  =  SPERAG
057300150402     c                   eval      WPEIND  =  SPEIND
057400150402     c                   eval      WPECAP  =  SPECAP
057500150402     c                   eval      WPELOC  =  SPELOC
057600150402     c                   eval      WPEPRO  =  SPEPRO
057700150402     c                   eval      WPENAZ  =  SPENAZ
057800150402     c                   eval      WPETEL  =  spetel
057900150402     c                   eval      WPEPER  =  speper
058000150402     c                   EndIf
058100150402     c                   EndIf
058200150402     c                   write     wfbar000
058300150402     C                   endsr
058400030124     C*------------------------------------------------------------------------*
058500150211     C* stampa
058600030124     C*------------------------------------------------------------------------*
058700150211     C     stampa        BEGSR
058800150211     c                   if        *in01
058900150211     c                   write     testa
059000150217     c                   write     testata
059100150211     c                   setoff                                       01
059200150220     c  n12              seton                                        15
059300150211     c                   end
059400150220     c*
059500150217     c                   if        *in11 or *in12 or *in13
059600150220     c                   if        *in11
059700150220     c                   setoff                                       16
059800150220     c                   end
059900150220     c     stdsaldo      comp      0                                  16
060000150220     c                   if        *in13 and *in15
060100150220     c                   write     salto
060200150220     c                   end
060300150402     c                   if        parfile = 'S'
060400150402     c                   exsr      scriviWF
060500150402     c                   endif
060600150217     c                   write     DETTAGLIO
060700150218     c                   end
060800150218     C* totali
060900150218     c                   if        *in14
061000150218     c                   write     salto
061100150414     c                   write     fine
061200150218     c                   end
061300150218     c*
061400150217     c                   setoff                                       111213
061500150220     c                   setoff                                       14
061600150211     C                   ENDSR
061700150211     C*------------------------------------------------------------------------*
061800150211     C* CHKDA8 - CONTROLLA LA DATA (8)
061900150211     C*------------------------------------------------------------------------*
062000150211     C     CHKDA8        BEGSR
062100030124     C*
062200030124     C                   IF        DAW8 > *ZEROS
062300030124     C                   Z-ADD     *ZEROS        G08DAT
062400030124     C                   Z-ADD     DAW8          G08INV
062500030124     C                   MOVEL     '3'           G08ERR
062600030124     C                   CALL      'XSRDA8'
062700030124     C                   PARM                    WLBDA8
062800030124     C                   Z-ADD     G08DAT        WDA8
062900030124     C                   Z-ADD     G08INV        DAW8
063000030124     C                   ELSE
063100030124     C                   Z-ADD     WDA8          G08DAT
063200030124     C                   Z-ADD     *ZEROS        G08INV
063300030124     C                   MOVEL     '0'           G08ERR
063400030124     C                   CALL      'XSRDA8'
063500030124     C                   PARM                    WLBDA8
063600030124     C                   Z-ADD     G08DAT        WDA8
063700030124     C                   Z-ADD     G08INV        DAW8
063800030124     C                   ENDIF
063900030124     C*
064000030124     C                   ENDSR
064100020923     C*--------------------------------------------------------------------------------------------*
064200020923     C* REPERISCE I DATI UTENTE
064300020923     C*--------------------------------------------------------------------------------------------*
064400020923     C     REPDATIUTE    BEGSR
064500020923     C*
064600020923     C* INIZIALIZZA VARIABILI DI WRK
064700020923     C                   CLEAR                   TIBS34DS
064800020923     C                   CLEAR                   AZUTEDS
064900020923     C                   CLEAR                   DDATIUTE
065000020923     C*
065100020923     C     *DTAARA       DEFINE    §azute        azuteds
065200020923     C     *DTAARA       DEFINE    §datiute      ddatiute
065300020923     C                   IN(E)     *DTAARA
065400030124     C                   IF        %Error
065500030124     C                   EVAL      I34Tla = 'L'
065600020923     C                   CALL      'TIBS34R'
065700020923     C                   PARM                    Tibs34Ds
065800020923     C                   IN        *DTAARA
065900030124     C                   ENDIF
066000020923     C*
066100020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
066200020923     C                   MOVEL(P)  RSUT          DSFIRS           20
066300020923     C*
066400020923     C                   ENDSR
066500150211     C*--------------------------------------------------------------------------------------------*
066600150211     C* CARTAB - CARICAMENTO DATI TABELLATI
066700150211     C*--------------------------------------------------------------------------------------------*
066800150211     C     CARTAB        BEGSR
066900030124     C*
067000030124     C* CARICAMENTO ARTICOLI
067100030124     C                   Z-ADD     *ZEROS        I
067200030124     C     *LOVAL        SETLL     AZAGB01L                           55
067300030124     C                   IF        *IN55 = *OFF
067400030124     C                   READ      AZAGB01L
067500030124     C                   DOW       not %eof(AZAGB01L)
067600030124     C                   ADD       1             I
067700030124     C                   EVAL      SKCODART(I) = agbCOD
067800030124     C                   EVAL      SKDESART(I) = agbDES
067900030124     C                   READ      AZAGB01L
068000030124     C                   ENDDO
068100030124     C                   ENDIF
068200030124     C*
068300030124     C                   ENDSR
068400150402     c*-------------------------------------------------
068500150402     c     comandi       begsr
068600150402     c*-------------------------------------------------
068700150402     C                   close     wfbar00f
068800150402      *
068900150402     C                   z-add     99            luncmd
069000150402     C                   CALL      'QCMDEXC'
069100150402     C                   parm                    coman
069200150402     C                   parm                    luncmd
069300150402      *
069400150402     c                   endsr
069500000000     C*------------------------------------------------------------------------*
069600000000     C*?*INZSR - IMPOSTAZIONI INIZIALI
069700000000     C*------------------------------------------------------------------------*
069800000000     C     *INZSR        BEGSR
069900000000     C*
070000000000     C* PARAMETRI IN INPUT AL PGM
070100000000     C     *ENTRY        PLIST
070200020719     C                   PARM                    KPJBA
070300030124     C                   MOVEL     KPJBU         PARAM
070400020923     C*
070500150416     C* CHIAVE LETTURA AZMGB04L - PARZIALE
070600150416     C     KEYMGBP       KLIST
070700150416     C                   KFLD                    MGBTIP
070800150416     C                   KFLD                    MGBSOG
070900150416     C                   KFLD                    MGBCOD
071000150416     C*
071100150416     C     Kcgb          KLIST
071200150416     C                   KFLD                    depsog
071300150416     C                   KFLD                    tipoges           1
071400150416      *
071500150416     C     Ktab          KLIST
071600150416     C                   KFLD                    tblkut
071700150416     C                   KFLD                    tblcod
071800150416     C                   KFLD                    tblkey
071900150416     C                   z-add     1             tblkut
072000150416     C*luoghi
072100150416     c     kFnspe        Klist
072200150416     c                   Kfld                    kspefls           1
072300150416     c                   Kfld                    kspecli           7 0
072400150416     c                   Kfld                    kspecod           3
072500150416     c                   move      'L'           kspefls
072600150416     c                   move      '888'         kspecod
072700150416     C* CHIAVE LETTURA CNACO00F - COMPLETA
072800150416     C     KEYACO        KLIST
072900150416     C                   KFLD                    ACOKUT
073000150416     C                   KFLD                    ACOKCC
073100150416     C                   KFLD                    ACOKSC
073200150416     C*
073300150416     C* CHIAVE LETTURA TNTBE01L - PARZIALE
073400150416     C     KEYTBEP       KLIST
073500150416     C                   KFLD                    TBECOD
073600150416     C                   KFLD                    TBEKE1
073700150416     C*
073800150416     C* CHIAVE LETTURA AZORG01L - COMPLETA
073900150416     C     KEYORG        KLIST
074000150416     C                   KFLD                    ORGFIL
074100020923     C* REPERISCE I DATI DELL'UTENTE
074200020923     C                   EXSR      REPDATIUTE
074300030124     C*
074400030124     C* CARICA I DATI TABELLATI
074500030124     C                   EXSR      CARTAB
074600030128     C*
074700030128     C* ACCENDO SUBITO QUI INDICATORE X STAMPA DESCRIZIONE SOGGETTO IN GRASSETTO
074800030128     C                   SETON                                        80
074900030127     C*
075000030127     C* REPERISCO DATA E ORA CORRENTI
075100030127     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
075200030127     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
075300030127     C                   MOVEL     WN14          ORACOR            6 0          *ORA  (8) IN H/M/SS
075400030127     C                   Z-ADD     WN8           G08DAT
075500030127     C                   Z-ADD     *ZEROS        G08INV
075600030127     C                   MOVEL     '0'           G08ERR
075700030127     C                   CALL      'XSRDA8'
075800030127     C                   PARM                    WLBDA8
075900030127     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
076000150409      * decodifica area se selezionata
076100150409     c                   if        pararea <> *blank
076200150409     C                   MOVE      pararea       arean             3 0
076300150409     C                   MOVE      pararea       stparea
076400150409     C                   MOVEL     '05'          tblcod
076500150409     C                   MOVEL     pararea       tblkey
076600150409     C     KTAB          CHAIN     TABEL00f
076700150409     C                   IF        %found(tabel00f)
076800150409     C                   MOVEL     tbluni        ds05
076900150409     C                   movel     §05des        stparead
077000150409     C                   Endif
077100150409     c                   endif
077200150409     c                   if        pardal > 0 and paral > 0
077300150409     c                   seton                                        66
077400150409     c                   movel     pardal        iso
077500150409     c                   move      iso           eur
077600150409     c                   move      eur           stpdalp
077700150409     c                   move      paral         iso
077800150409     c                   move      iso           eur
077900150409     c                   move      eur           stpalp
078000150409     c                   endif
078100150410     c                   if        partsc  <> *blank
078200150410     C                   move      partsc        stpges
078300150410     c                   endif
078400150402      *clear file
078500150402     c                   if        parpulf = 'S'
078600150402     c                   movea     cmd1          coman
078700150402     c                   exsr      comandi
078800150402     c                   endif
078900150402     c                   if        parfile = 'S'
079000150402     C                   OPEN      wfbar00f
079100150402     c                   endif
079200030124     C*
079300000000     C                   ENDSR
079400150402**  CMD1
079500150402CLRPFM    FILE(WFBAR00F)
