000100010925      *PARMS DYNUSRPRF(*OWNER)
000200970526     H*------------------------------------------------------------------------*
000300000907     H* Gestione codici unificanti
000400970526     H*------------------------------------------------------------------------*
000500000920     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900020208     Ftis7c4P   O    E             PRINTER OFLIND(*IN05)
001000140616     FAZCMM01L  IF   E           K DISK
001100970526     D*------------------------------------------------------------------------*
001200970526     D* SCHIERE
001300970526     D*------------------------------------------------------------------------*
001400991214     D*------------
001500970526     D* ARCHITETTURA
001600970526     D*------------
001700970526     D KPJBA         E DS
001800000310     D*---------------------
001900000310     D* DS PER REPERIRE I PARAMETRI UTENTE
002000000310     D*---------------------
002100000310     D CARCNT          DS
002200000310     D  FL12CN                 1      2
002300000310     D  FL34CN                 3      4
002400000310     D  TIPCNT                 5      6
002500000310     D  FLU                   66     97
002600000310     D                                     DIM(32)                              FLAG PAR.UTENT
002700000310     D  TCU                  398    697
002800000310     D                                     DIM(50)                              *TAB FLG 8  TP.CN
002900000310     D  KCU                  698    847P 0
003000000310     D                                     DIM(50)                              *TAB CAPICONTO
003100000310     D                                     PACKEVEN
003200991214     D*------------------
003300991214     D* DS X PASSAGGIO PARAMETRI
003400991214     D*------------------
003500991214     D PARAM           DS                  INZ
003600011205     D  PARDDE                 1      8  0
003700011205     D  PARTLE                 9     10
003800011205     D  PARDLE                11     40
003900011205     D  PARFIL                41     43  0
004000011205     D  PARDFI                44     73
004100011205     D  PARFIT                74     74
004200011205     D  PAREDD                75     82  0
004300011206     D  PARFIC                83     83  0
004400011206     D  PARFIR                84    104
004500020208     D  PARANO               105    105
004600000929     D*-------------------
004700020208     D* DS DI LETTURA RECORDSET DEL "PIANO DEI CONTI" VIA SQL
004800000929     D*-------------------
004900020208     D DSACO         E DS                  EXTNAME(cnaco00f) PREFIX(s_)
005000020208     D*-------------------
005100020208     D* DS REPERIMENTO PADRE/FIGLI
005200020208     D*-------------------
005300020208     D TIBS10DS      E DS
005400020208     D*-------------------
005500020208     D* DS REPERIMENTO PADRE/FIGLI
005600020208     D*-------------------
005700020208     D bs69ds        E DS                  EXTNAME(tibs69ds) INZ
005800020208     D acods         E DS                  EXTNAME(cnaco00f) INZ
005900020208     D indds         E DS                  EXTNAME(cnind00f) INZ
006000020208     D clpds         E DS                  EXTNAME(cnclp00f) INZ
006100020208     D clsds         E DS                  EXTNAME(fncls00f) INZ
006200000810     D*-------------------
006300000810     D* FILIALI IN GESTIONE
006400000810     D*-------------------
006500020903     D L1              S              3  0 DIM(250)                             *FILIALI IN GESTIONE
006600000905     D*------------------
006700000901     D* DS "XSRDA8" - CONTROLLA DATA (8)
006800000901     D*------------------
006900000901     D WLBDA8          DS                  INZ
007000000901     D  G08DAT                 1      8  0
007100000901     D  G08INV                 9     16  0
007200000901     D  G08ERR                17     17
007300000901     D  G08TGI                18     22  0
007400011206     D*-------------------
007500011206     D* VARIABILI DI WRK
007600011206     D*-------------------
007700020208     D  ERRl00         s              1    INZ(*zeros)
007800011206     D  I              s              9  0 INZ(*zeros)
007900011206     D  X              s              9  0 INZ(*zeros)
008000020903     D*-------------
008100020903     D* DS DI WRK
008200020903     D*-------------
008300020903     D TIBS34DS      E DS                                                       *Profili utente
008400020903     D DDATIUTE      E DS                                                       *Dati utente
008500020903     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008600020903     D TRUL06DS      E DS                                                       *caricamento £x
008700020903     D  LIA                    1     90  0 DIM(30)                               -filiali
008800020903     D TRUL26DS      E DS                                                       *caricamento P.O.
008900020903     D  SKPO                  11    760  0 dim(250)
009000020903     D  SKPOT                761   1510  0 dim(250)
009100970526     C*------------------------------------------------------------------------*
009200991214     C* MAIN LINES
009300970526     C*------------------------------------------------------------------------*
009400970526     C*
009500991214     C* ESEGUE ROUTINE DI CREAZIONE E STAMPA
009600991214     C                   EXSR      ESEGUI
009700020208     C                   EXSR      FINSR
009800000306     C*
009900970526     C                   SETON                                        LR
010000970526     C*------------------------------------------------------------------------*
010100991217     C* ESEGUI - OPERAZIONI PRINCIPALI
010200970526     C*------------------------------------------------------------------------*
010300991214     C     ESEGUI        BEGSR
010400991214     C*
010500011205     C* Per prima cosa valorizzo i parametri di lancio in stampa
010600011205     C                   exsr      valPAR
010700011205     C*
010800011205     C* Poi stampo l'intestazione
010900011205     C                   seton                                        05
011000011205     C                   exsr      stpTES
011100020207     C*
011200020207     C* Per ogni filiale presente in £1 eseguo distintamente le operazioni di elaborazione
011300020903     C     1             do        250           i
011400020207     C*
011500020903     C                   if        L1(i) > *zeros
011600020903     C                   movel     L1(i)         WrkCurrFil        3
011700011205     C* Quindi la routine di elaborazione
011800020212     C                   exsr      exeACO
011900020207     C                   endif
012000020207     C*
012100020207     C                   enddo
012200011205     C*
012300011205     C* Al termine chiudo il prospetto
012400020208     C                   write     s7c4RI
012500020208     C                   write     s7c4FI
012600970526     C*
012700970526     C                   ENDSR
012800011205     C*------------------------------------------------------------------------*
012900011205     C* VALPAR - VALORIZZA I PARAMETRI DI LANCIO IN STAMPA
013000011205     C*------------------------------------------------------------------------*
013100011205     C     VALPAR        BEGSR
013200011205     C*
013300011206     C                   movel     PARFIL        STPPOP
013400011205     C                   if        PARFIL  = *ZEROS
013500011205     C                   eval      STPPOPD = 'TUTTI I P.O. DELL AREA'
013600011205     C                   else
013700011205     C                   eval      STPPOPD = PARDFI
013800011205     C                   endif
013900011205     C                   eval      STPDDE  = PAREDD
014000011205     C                   eval      STPTLE  = PARTLE
014100011205     C                   eval      STPTLED = PARDLE
014200011206     C                   eval      STPSIF  = PARFIR
014300020208     C                   if        PARANO = 'S'
014400020208     C                   eval      STPTMEX = '(Solo anomalie)'
014500020208     C                   endif
014600011205     C*
014700011205     C                   ENDSR
014800011205     C*------------------------------------------------------------------------*
014900011205     C* STPTES - ESEGUE LA STAMPA DELLA TESTATA QUANDO NECESSARIO
015000011205     C*------------------------------------------------------------------------*
015100011205     C     STPTES        BEGSR
015200011205     C*
015300011205     C                   if        *in05 = *on
015400020208     C   15              write     s7c4LE
015500020208     C                   write     s7c4TE
015600011205     C                   setoff                                       05
015700011205     C                   endif
015800011205     C*
015900011205     C                   ENDSR
016000000929     C*------------------------------------------------------------------------*
016100020208     C* EXEACO - Routine di selezione record del CNACO00F secondo parametri di lancio
016200000929     C*------------------------------------------------------------------------*
016300020208     C     EXEACO        BEGSR
016400011205     C*
016500011205     C                   movel     *loval        depCop           11 0
016600000929     C*
016700000929     C/EXEC SQL
016800000929     C+ declare C1 cursor for
016900020208     C+ select * from cnaco00f where
017000020208     C+ acokut = 1    and
017100020924     C+ acokcc = :DUTKCI and
017200020208     C+ substr(digits(acoksc), 1, 3) = :WrkCurrFil
017300020208     C+ order by acorag
017400000929     C+ for read only
017500000929     C/END-EXEC
017600000929     C
017700000929     C/EXEC SQL
017800000929     C+ open C1
017900000929     C/END-EXEC
018000000929     C
018100000929     C/EXEC SQL
018200020208     C+ Fetch C1 into :DSACO
018300000929     C/END-EXEC
018400000929     C*
018500000929     C                   dow       sqlcod = *zeros
018600011205     C                   exsr      chkcli
018700011205     C                   if        errl00 = '0'
018800020208     C                   exsr      exePRT
018900011205     C                   endif
019000000929     C*
019100000929     C/EXEC SQL
019200020208     C+ Fetch C1 into :DSACO
019300000929     C/END-EXEC
019400000929     C*
019500000929     C                   enddo
019600011206     C*
019700011206     C* A fine file rieseguo la routine di stampa x scaricare l'ultimo record letto
019800020208     C                   exsr      exePRT
019900000929     C*
020000000929     C/EXEC SQL
020100000929     C+ close C1
020200000929     C/END-EXEC
020300000929     C
020400000929     C*
020500000929     C                   ENDSR
020600011205     C*------------------------------------------------------------------------*
020700020208     C* EXEPRT - STAMPA IL DETTAGLIO
020800011205     C*------------------------------------------------------------------------*
020900020208     C     EXEPRT        BEGSR
021000011205     C*
021100020208     C                   clear                   s7c4de
021200011205     C                   exsr      repinfo
021300020208     C                   exsr      repmess
021400020208     C                   if        stpmess <> *blanks or
021500020208     C                             PARANO = 'N'
021600020315     C                   exsr      stpTES
021700020208     C                   write     s7c4DE
021800020208     C                   seton                                        15
021900020208     C                   endif
022000020208     C                   clear                   s7c4DE
022100011206     C*
022200011205     C                   ENDSR
022300011205     C*------------------------------------------------------------------------*
022400020208     C* REPINFO - REPERISCE TUTTE LE INFORMAZIONI NECESSARIE
022500011205     C*------------------------------------------------------------------------*
022600011205     C     REPINFO       BEGSR
022700020208     C*
022800020208     C* Valorizzo i dati del cliente "semplice" che ho già
022900020208     C                   eval      stpcli  = s_acoksc
023000020208     C                   eval      stpdcli = s_acorag
023100020208     C*
023200020208     C* Reperisco le informazioni anagrafiche
023300020208     C                   clear                   bs69ds
023400020208     C                   clear                   acods
023500020208     C                   clear                   indds
023600020208     C                   clear                   clpds
023700020208     C                   clear                   clsds
023800020208     C                   movel     knsif         i69sif
023900020208     C                   z-add     s_acoksc      I69KAC
024000020208     C                   z-add     s_acoksc      I69KCP
024100020208     C                   z-add     s_acoksc      I69KIN
024200020208     C                   call      'TIBS69R'
024300020208     C                   parm                    bs69ds
024400020208     C                   parm                    acods
024500020208     C                   parm                    indds
024600020208     C                   parm                    clpds
024700020208     C                   parm                    clsds
024800020208     C*
024900020208     C     o69err        ifne      '1'
025000020208     C                   eval      stpcliprv = indprv
025100020208     C                   eval      stpcliiva = indiva
025200020208     C* Reperisco le informazioni dell'agente commerciale
025300140616     C                   EVAL      CMMCod = clpage
025400140616     C     CMMCod        CHAIN     AZCMM01l
025500140616     C                   IF        %found(AZCMM01L)
025600140616     C                   eval      stpcliageu = cmmuni
025700020208     C                   endif
025800020208     C                   eval      stpcliclif = clpscf
025900020208     C                   eval      stpfat     = clpscf
026000020208     C                   endif
026100020208     C*
026200020208     C                   clear                   tibs10ds
026300020208     C                   eval      d10drf = PARDDE                              *data riferimento
026400020208     C                   eval      d10tle = PARTLE                              *tipo legame
026500020208     C                   eval      d10paf = 'P'                                 *cerca il padre
026600020208     C                   eval      d10cod = s_acoksc
026700020208     C                   call      'TIBS10R'
026800020208     C                   parm                    tibs10ds
026900020208     C                   if        d10err = *blanks                             *Figlio
027000020208     C                   z-add     d10cop        stpclicliu
027100020208     C                   z-add     d10cop        stpuni
027200020208     C                   endif
027300020208     C*
027400020208     C* Valorizzo i dati del cliente di fatturazione se a questo punto ho già
027500020208     C                   if        stpfat > *zeros
027600020208     C*
027700020208     C* Reperisco le informazioni anagrafiche del cliente di fatturazione
027800020208     C                   clear                   bs69ds
027900020208     C                   clear                   acods
028000020208     C                   clear                   indds
028100020208     C                   clear                   clpds
028200020208     C                   clear                   clsds
028300020208     C                   movel     knsif         i69sif
028400020208     C                   z-add     stpfat        I69KAC
028500020208     C                   z-add     stpfat        I69KCP
028600020208     C                   z-add     stpfat        I69KIN
028700020208     C                   call      'TIBS69R'
028800020208     C                   parm                    bs69ds
028900020208     C                   parm                    acods
029000020208     C                   parm                    indds
029100020208     C                   parm                    clpds
029200020208     C                   parm                    clsds
029300020208     C*
029400020208     C     o69err        ifne      '1'
029500020208     C                   eval      stpdfat = acorag
029600020208     C                   eval      stpfatprv = indprv
029700020208     C                   eval      stpfativa = indiva
029800140616     C* Reperisco le informazioni dell'agente commerciale
029900140616     C                   EVAL      CMMCod = clpage
030000140616     C     CMMCod        CHAIN     AZCMM01l
030100140616     C                   IF        %found(AZCMM01L)
030200140616     C                   eval      stpfatageu = cmmuni
030300140616     C                   endif
030400020208     C                   eval      stpfatclif = clpscf
030500020208     C                   endif
030600020208     C*
030700020208     C                   clear                   tibs10ds
030800020208     C                   eval      d10drf = PARDDE                              *data riferimento
030900020208     C                   eval      d10tle = PARTLE                              *tipo legame
031000020208     C                   eval      d10paf = 'P'                                 *cerca il padre
031100020208     C                   eval      d10cod = stpfat
031200020208     C                   call      'TIBS10R'
031300020208     C                   parm                    tibs10ds
031400020208     C                   if        d10err = *blanks                             *Figlio
031500020208     C                   z-add     d10cop        stpfatcliu
031600020208     C                   endif
031700020208     C                   endif
031800020208     C*
031900020208     C* Valorizzo i dati del cliente unificante se a questo punto ho già
032000020208     C                   if        stpuni > *zeros
032100020208     C*
032200020208     C* Reperisco le informazioni anagrafiche del cliente unificante
032300020208     C                   clear                   bs69ds
032400020208     C                   clear                   acods
032500020208     C                   clear                   indds
032600020208     C                   clear                   clpds
032700020208     C                   clear                   clsds
032800020208     C                   movel     knsif         i69sif
032900020208     C                   z-add     stpuni        I69KAC
033000020208     C                   z-add     stpuni        I69KCP
033100020208     C                   z-add     stpuni        I69KIN
033200020208     C                   call      'TIBS69R'
033300020208     C                   parm                    bs69ds
033400020208     C                   parm                    acods
033500020208     C                   parm                    indds
033600020208     C                   parm                    clpds
033700020208     C                   parm                    clsds
033800020208     C*
033900020208     C     o69err        ifne      '1'
034000020208     C                   eval      stpduni = acorag
034100140616     C* Reperisco le informazioni dell'agente commerciale
034200140616     C                   EVAL      CMMCod = clpage
034300140616     C     CMMCod        CHAIN     AZCMM01l
034400140616     C                   IF        %found(AZCMM01L)
034500140616     C                   eval      stpuniageu = cmmuni
034600140616     C                   endif
034700020208     C                   eval      stpuniclif = clpscf
034800020208     C                   endif
034900020208     C                   endif
035000011206     C*
035100011205     C                   ENDSR
035200020208     C*------------------------------------------------------------------------*
035300020208     C* REPMESS - EFFETTUA CONSIDERAZIONI E DETERMINA IL MESAGGIO DA ESPORRE
035400020208     C*------------------------------------------------------------------------*
035500020208     C     REPMESS       BEGSR
035600020208     C*
035700020208     C                   if        stpcliiva <> *blanks and
035800020208     C                             stpfativa <> *blanks
035900020208     C                   if        stpcliiva <> stpfativa
036000020208     C                   eval      stpmess = %trim(stpmess) + ' 1'
036100020208     C                   endif
036200020208     C                   endif
036300020208     C*
036400020208     C                   if        stpcliageu > *zeros and
036500020208     C                             stpfatageu > *zeros
036600020208     C                   if        stpcliageu <> stpfatageu
036700020208     C                   eval      stpmess = %trim(stpmess) + ' 2'
036800020208     C                   endif
036900020208     C                   endif
037000020208     C*
037100020208     C                   if        stpcliageu > *zeros and
037200020208     C                             stpuniageu > *zeros
037300020208     C                   if        stpcliageu <> stpuniageu
037400020208     C                   eval      stpmess = %trim(stpmess) + ' 3'
037500020208     C                   endif
037600020208     C                   endif
037700020208     C*
037800020208     C                   eval      stpmess = %trim(stpmess)
037900020208     C*
038000020208     C                   ENDSR
038100000306     C*------------------------------------------------------------------------*
038200011205     C* CHKCLI - CONTROLLA IL CLIENTE RISPETTO ALLA FILIALE "CHIAMANTE" e/o SELEZIONATA
038300000306     C*------------------------------------------------------------------------*
038400000306     C     CHKCLI        BEGSR
038500000306     C*
038600011205     C                   MOVEL     '0'           ERRl00
038700000306     C*
038800020208     C                   MOVEL     s_acoksc      WrkFil            3 0
038900011205     C*
039000011205     C                   IF        PARFIL > *zeros
039100020208     C                   IF        WrkFil = PARFIL
039200011205     C                   ELSE
039300011205     C                   MOVEL     '1'           ERRl00
039400011205     C                   ENDIF
039500011205     C                   ELSE
039600020903     C     WrkFil        LOOKUP    L1                                     20
039700020208     C                   IF        *in20=*on
039800011205     C                   ELSE
039900011205     C                   MOVEL     '1'           ERRl00
040000000306     C                   ENDIF
040100011205     C                   ENDIF
040200000306     C*
040300000306     C                   ENDSR
040400000810     C*------------------------------------------------------------------------*
040500000810     C* CARTAB - CARICA LE TABELLE OCCORRENTI
040600000810     C*------------------------------------------------------------------------*
040700000810     C     CARTAB        BEGSR
040800020903     C*
040900020903     C* REPERISCE I DATI UTENTE
041000020903     C                   EXSR      REPDATIUTE
041100020903     C*
041200020903     C* INIZIALIZZO LE VARIABILI DI WRK RELATIVE ALLE FILIALI IN GESTIONE
041300020903     C                   CLEAR                   L1
041400020903     C                   CLEAR                   TRUL06DS
041500020903     C                   CLEAR                   LIA
041600020903     C                   CLEAR                   TRUL26DS
041700020903     C                   CLEAR                   SKPO
041800020903     C                   CLEAR                   SKPOT
041900020903     C*
042000020903     C* CARICA LE FILIALI IN GESTIONE
042100020903     C                   IF        DUTLPO = '1'                                 *1° LIVELLO
042200020903     C                   EVAL      DEPKPJBU = KPJBU
042300020903     C                   MOVEL     'L'           D06TLA
042400020903     C                   MOVEL     '£1'          D06COD
042500021007     C                   MOVEL     duttfp        D06KEY
042600020903     C                   MOVEL     TRUL06DS      KPJBU
042700020903     C                   CALL      'TRUL06R'
042800020903     C                   PARM                    KPJBA
042900020903     C                   MOVEL     KPJBU         TRUL06DS
043000020903     C                   MOVEA     LIA           L1                              -P.O. terminal
043100020903     C                   EVAL      KPJBU = DEPKPJBU
043200020903     C                   ENDIF
043300020903     C*
043400020903     C                   IF        DUTLPO = '2'                                 *2° LIVELLO
043500020906     C                   MOVEL     DUTPOU        L1(1)                           -P.O. utente
043600020903     C                   ENDIF
043700020903     C*
043800020903     C                   IF        DUTLPO = 'S'                                 *SEDE
043900020903     C                   EVAL      I26TLA = 'L'
044000020903     C                   EVAL      I26TPT = 'P'
044100020903     C                   CALL      'TRUL26R'
044200020903     C                   PARM                    TRUL26DS
044300020903     C                   MOVEA     SKPO          L1                              -tutti i P.O.
044400020903     C                   ENDIF
044500000810     C*
044600000810     C                   ENDSR
044700020903     C*--------------------------------------------------------------------------------------------*
044800020903     C* REPERISCE I DATI UTENTE
044900020903     C*--------------------------------------------------------------------------------------------*
045000020903     C     REPDATIUTE    BEGSR
045100020903     C*
045200020903     C* INIZIALIZZA VARIABILI DI WRK
045300020903     C                   CLEAR                   TIBS34DS
045400020903     C                   CLEAR                   AZUTEDS
045500020903     C                   CLEAR                   DDATIUTE
045600020903     C*
045700020903     C     *DTAARA       DEFINE    §azute        azuteds
045800020903     C     *DTAARA       DEFINE    §datiute      ddatiute
045900020903     C                   IN(E)     *DTAARA
046000020903if  1C                   IF        %Error
046100020903     c                   EVAL      I34Tla = 'L'
046200020903     C                   CALL      'TIBS34R'
046300020903     C                   PARM                    Tibs34Ds
046400020903     C                   IN        *DTAARA
046500020903e   1C                   ENDIF
046600020903     C*
046700020903     C                   ENDSR
046800020208     C*------------------------------------------------------------------------*
046900020208     C* FINSR - OPERAZIONI FINALI
047000020208     C*------------------------------------------------------------------------*
047100020208     C     FINSR         BEGSR
047200020208     C*
047300020208     C* CHIUDE I FILE DEI PGM CHIAMATI
047400020208     C                   CLEAR                   bs69ds
047500020208     C                   CLEAR                   acods
047600020208     C                   CLEAR                   indds
047700020208     C                   CLEAR                   clpds
047800020208     C                   CLEAR                   clsds
047900020208     C                   MOVEL     'C'           i69tla
048000020208     C                   MOVEL     knsif         i69sif
048100020208     C                   CALL      'TIBS69R'
048200020208     C                   PARM                    bs69ds
048300020208     C                   PARM                    acods
048400020208     C                   PARM                    indds
048500020208     C                   PARM                    clpds
048600020208     C                   PARM                    clsds
048700020208     C*
048800020208     C                   ENDSR
048900970526     C*------------------------------------------------------------------------*
049000970526     C* *INZSR - OPERAZIONI INIZIALI
049100970526     C*------------------------------------------------------------------------*
049200970526     C     *INZSR        BEGSR
049300970526     C*---
049400970526     C* RICEVIMENTO PARAMETRI
049500970526     C*---
049600970526     C     *ENTRY        PLIST
049700970612     C                   PARM                    KPJBA
049800991214     C                   MOVEL     KPJBU         PARAM
049900020903     C*---
050000020903     C* VARIABILI RIFERITE AL DATA BASE
050100020903     C*---
050200020903     C     *LIKE         DEFINE    Kpjbu         DEPKPJBU                       *depositi
050300011205     C*
050400000810     C* CARICA LE TABELLE OCCORRENTI
050500000810     C                   EXSR      CARTAB
050600970526     C*---
050700991214     C* CHIAVI DI LETTURA
050800991214     C*---
050900991214     C*
051000970526     C                   ENDSR
