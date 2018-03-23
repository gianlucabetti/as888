000100010925      *PARMS DYNUSRPRF(*OWNER)
000200970526     H*------------------------------------------------------------------------*
000300000907     H* Gestione codici unificanti
000400970526     H*------------------------------------------------------------------------*
000500000920     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900020208     Ftis7c4P   O    E             PRINTER OFLIND(*IN05)
001000000810     Ftabel00f  IF   E           K DISK
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
005000011205     D*-------------------
005100011206     D* DS DI PROCEDURA X LETTURA TABELLA "01" DEGLI AGENTI UNIFICANTI
005200011205     D*-------------------
005300011205     D DS01          E DS
005400020208     D*-------------------
005500020208     D* DS REPERIMENTO PADRE/FIGLI
005600020208     D*-------------------
005700020208     D TIBS10DS      E DS
005800020208     D*-------------------
005900020208     D* DS REPERIMENTO PADRE/FIGLI
006000020208     D*-------------------
006100020208     D bs69ds        E DS                  EXTNAME(tibs69ds) INZ
006200020208     D acods         E DS                  EXTNAME(cnaco00f) INZ
006300020208     D indds         E DS                  EXTNAME(cnind00f) INZ
006400020208     D clpds         E DS                  EXTNAME(cnclp00f) INZ
006500020208     D clsds         E DS                  EXTNAME(fncls00f) INZ
006600000810     D*-------------------
006700000810     D* FILIALI IN GESTIONE
006800000810     D*-------------------
006900020903     D L1              S              3  0 DIM(250)                             *FILIALI IN GESTIONE
007000000905     D*------------------
007100000901     D* DS "XSRDA8" - CONTROLLA DATA (8)
007200000901     D*------------------
007300000901     D WLBDA8          DS                  INZ
007400000901     D  G08DAT                 1      8  0
007500000901     D  G08INV                 9     16  0
007600000901     D  G08ERR                17     17
007700000901     D  G08TGI                18     22  0
007800011206     D*-------------------
007900011206     D* VARIABILI DI WRK
008000011206     D*-------------------
008100020208     D  ERRl00         s              1    INZ(*zeros)
008200011206     D  I              s              9  0 INZ(*zeros)
008300011206     D  X              s              9  0 INZ(*zeros)
008400020903     D*-------------
008500020903     D* DS DI WRK
008600020903     D*-------------
008700020903     D TIBS34DS      E DS                                                       *Profili utente
008800020903     D DDATIUTE      E DS                                                       *Dati utente
008900020903     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009000020903     D TRUL06DS      E DS                                                       *caricamento £x
009100020903     D  LIA                    1     90  0 DIM(30)                               -filiali
009200020903     D TRUL26DS      E DS                                                       *caricamento P.O.
009300020903     D  SKPO                  11    760  0 dim(250)
009400020903     D  SKPOT                761   1510  0 dim(250)
009500970526     C*------------------------------------------------------------------------*
009600991214     C* MAIN LINES
009700970526     C*------------------------------------------------------------------------*
009800970526     C*
009900991214     C* ESEGUE ROUTINE DI CREAZIONE E STAMPA
010000991214     C                   EXSR      ESEGUI
010100020208     C                   EXSR      FINSR
010200000306     C*
010300970526     C                   SETON                                        LR
010400970526     C*------------------------------------------------------------------------*
010500991217     C* ESEGUI - OPERAZIONI PRINCIPALI
010600970526     C*------------------------------------------------------------------------*
010700991214     C     ESEGUI        BEGSR
010800991214     C*
010900011205     C* Per prima cosa valorizzo i parametri di lancio in stampa
011000011205     C                   exsr      valPAR
011100011205     C*
011200011205     C* Poi stampo l'intestazione
011300011205     C                   seton                                        05
011400011205     C                   exsr      stpTES
011500020207     C*
011600020207     C* Per ogni filiale presente in £1 eseguo distintamente le operazioni di elaborazione
011700020903     C     1             do        250           i
011800020207     C*
011900020903     C                   if        L1(i) > *zeros
012000020903     C                   movel     L1(i)         WrkCurrFil        3
012100011205     C* Quindi la routine di elaborazione
012200020212     C                   exsr      exeACO
012300020207     C                   endif
012400020207     C*
012500020207     C                   enddo
012600011205     C*
012700011205     C* Al termine chiudo il prospetto
012800020208     C                   write     s7c4RI
012900020208     C                   write     s7c4FI
013000970526     C*
013100970526     C                   ENDSR
013200011205     C*------------------------------------------------------------------------*
013300011205     C* VALPAR - VALORIZZA I PARAMETRI DI LANCIO IN STAMPA
013400011205     C*------------------------------------------------------------------------*
013500011205     C     VALPAR        BEGSR
013600011205     C*
013700011206     C                   movel     PARFIL        STPPOP
013800011205     C                   if        PARFIL  = *ZEROS
013900011205     C                   eval      STPPOPD = 'TUTTI I P.O. DELL AREA'
014000011205     C                   else
014100011205     C                   eval      STPPOPD = PARDFI
014200011205     C                   endif
014300011205     C                   eval      STPDDE  = PAREDD
014400011205     C                   eval      STPTLE  = PARTLE
014500011205     C                   eval      STPTLED = PARDLE
014600011206     C                   eval      STPSIF  = PARFIR
014700020208     C                   if        PARANO = 'S'
014800020208     C                   eval      STPTMEX = '(Solo anomalie)'
014900020208     C                   endif
015000011205     C*
015100011205     C                   ENDSR
015200011205     C*------------------------------------------------------------------------*
015300011205     C* STPTES - ESEGUE LA STAMPA DELLA TESTATA QUANDO NECESSARIO
015400011205     C*------------------------------------------------------------------------*
015500011205     C     STPTES        BEGSR
015600011205     C*
015700011205     C                   if        *in05 = *on
015800020208     C   15              write     s7c4LE
015900020208     C                   write     s7c4TE
016000011205     C                   setoff                                       05
016100011205     C                   endif
016200011205     C*
016300011205     C                   ENDSR
016400000929     C*------------------------------------------------------------------------*
016500020208     C* EXEACO - Routine di selezione record del CNACO00F secondo parametri di lancio
016600000929     C*------------------------------------------------------------------------*
016700020208     C     EXEACO        BEGSR
016800011205     C*
016900011205     C                   movel     *loval        depCop           11 0
017000000929     C*
017100000929     C/EXEC SQL
017200000929     C+ declare C1 cursor for
017300020208     C+ select * from cnaco00f where
017400020208     C+ acokut = 1    and
017500020924     C+ acokcc = :DUTKCI and
017600020208     C+ substr(digits(acoksc), 1, 3) = :WrkCurrFil
017700020208     C+ order by acorag
017800000929     C+ for read only
017900000929     C/END-EXEC
018000000929     C
018100000929     C/EXEC SQL
018200000929     C+ open C1
018300000929     C/END-EXEC
018400000929     C
018500000929     C/EXEC SQL
018600020208     C+ Fetch C1 into :DSACO
018700000929     C/END-EXEC
018800000929     C*
018900000929     C                   dow       sqlcod = *zeros
019000011205     C                   exsr      chkcli
019100011205     C                   if        errl00 = '0'
019200020208     C                   exsr      exePRT
019300011205     C                   endif
019400000929     C*
019500000929     C/EXEC SQL
019600020208     C+ Fetch C1 into :DSACO
019700000929     C/END-EXEC
019800000929     C*
019900000929     C                   enddo
020000011206     C*
020100011206     C* A fine file rieseguo la routine di stampa x scaricare l'ultimo record letto
020200020208     C                   exsr      exePRT
020300000929     C*
020400000929     C/EXEC SQL
020500000929     C+ close C1
020600000929     C/END-EXEC
020700000929     C
020800000929     C*
020900000929     C                   ENDSR
021000011205     C*------------------------------------------------------------------------*
021100020208     C* EXEPRT - STAMPA IL DETTAGLIO
021200011205     C*------------------------------------------------------------------------*
021300020208     C     EXEPRT        BEGSR
021400011205     C*
021500020208     C                   clear                   s7c4de
021600011205     C                   exsr      repinfo
021700020208     C                   exsr      repmess
021800020208     C                   if        stpmess <> *blanks or
021900020208     C                             PARANO = 'N'
022000020315     C                   exsr      stpTES
022100020208     C                   write     s7c4DE
022200020208     C                   seton                                        15
022300020208     C                   endif
022400020208     C                   clear                   s7c4DE
022500011206     C*
022600011205     C                   ENDSR
022700011205     C*------------------------------------------------------------------------*
022800020208     C* REPINFO - REPERISCE TUTTE LE INFORMAZIONI NECESSARIE
022900011205     C*------------------------------------------------------------------------*
023000011205     C     REPINFO       BEGSR
023100020208     C*
023200020208     C* Valorizzo i dati del cliente "semplice" che ho già
023300020208     C                   eval      stpcli  = s_acoksc
023400020208     C                   eval      stpdcli = s_acorag
023500020208     C*
023600020208     C* Reperisco le informazioni anagrafiche
023700020208     C                   clear                   bs69ds
023800020208     C                   clear                   acods
023900020208     C                   clear                   indds
024000020208     C                   clear                   clpds
024100020208     C                   clear                   clsds
024200020208     C                   movel     knsif         i69sif
024300020208     C                   z-add     s_acoksc      I69KAC
024400020208     C                   z-add     s_acoksc      I69KCP
024500020208     C                   z-add     s_acoksc      I69KIN
024600020208     C                   call      'TIBS69R'
024700020208     C                   parm                    bs69ds
024800020208     C                   parm                    acods
024900020208     C                   parm                    indds
025000020208     C                   parm                    clpds
025100020208     C                   parm                    clsds
025200020208     C*
025300020208     C     o69err        ifne      '1'
025400020208     C                   eval      stpcliprv = indprv
025500020208     C                   eval      stpcliiva = indiva
025600020208     C* Reperisco le informazioni dell'agente commerciale
025700020208     C                   clear                   DS01
025800020208     C                   z-add     PARFIC        kblkut
025900020208     C                   movel     '01'          kblcod
026000020208     C                   movel     clpage        kblkey
026100020208     C     keytab        chain     tabel00f
026200020208     C                   if        %found(tabel00f)
026300020208     C                   movel     tbluni        DS01
026400020208     C                   eval      stpcliageu = §01RGF
026500020208     C                   endif
026600020208     C                   eval      stpcliclif = clpscf
026700020208     C                   eval      stpfat     = clpscf
026800020208     C                   endif
026900020208     C*
027000020208     C                   clear                   tibs10ds
027100020208     C                   eval      d10drf = PARDDE                              *data riferimento
027200020208     C                   eval      d10tle = PARTLE                              *tipo legame
027300020208     C                   eval      d10paf = 'P'                                 *cerca il padre
027400020208     C                   eval      d10cod = s_acoksc
027500020208     C                   call      'TIBS10R'
027600020208     C                   parm                    tibs10ds
027700020208     C                   if        d10err = *blanks                             *Figlio
027800020208     C                   z-add     d10cop        stpclicliu
027900020208     C                   z-add     d10cop        stpuni
028000020208     C                   endif
028100020208     C*
028200020208     C* Valorizzo i dati del cliente di fatturazione se a questo punto ho già
028300020208     C                   if        stpfat > *zeros
028400020208     C*
028500020208     C* Reperisco le informazioni anagrafiche del cliente di fatturazione
028600020208     C                   clear                   bs69ds
028700020208     C                   clear                   acods
028800020208     C                   clear                   indds
028900020208     C                   clear                   clpds
029000020208     C                   clear                   clsds
029100020208     C                   movel     knsif         i69sif
029200020208     C                   z-add     stpfat        I69KAC
029300020208     C                   z-add     stpfat        I69KCP
029400020208     C                   z-add     stpfat        I69KIN
029500020208     C                   call      'TIBS69R'
029600020208     C                   parm                    bs69ds
029700020208     C                   parm                    acods
029800020208     C                   parm                    indds
029900020208     C                   parm                    clpds
030000020208     C                   parm                    clsds
030100020208     C*
030200020208     C     o69err        ifne      '1'
030300020208     C                   eval      stpdfat = acorag
030400020208     C                   eval      stpfatprv = indprv
030500020208     C                   eval      stpfativa = indiva
030600020208     C* Reperisco le informazioini dell'agente commerciale
030700020208     C                   clear                   DS01
030800020208     C                   z-add     PARFIC        kblkut
030900020208     C                   movel     '01'          kblcod
031000020208     C                   movel     clpage        kblkey
031100020208     C     keytab        chain     tabel00f
031200020208     C                   if        %found(tabel00f)
031300020208     C                   movel     tbluni        DS01
031400020208     C                   eval      stpfatageu = §01RGF
031500020208     C                   endif
031600020208     C                   eval      stpfatclif = clpscf
031700020208     C                   endif
031800020208     C*
031900020208     C                   clear                   tibs10ds
032000020208     C                   eval      d10drf = PARDDE                              *data riferimento
032100020208     C                   eval      d10tle = PARTLE                              *tipo legame
032200020208     C                   eval      d10paf = 'P'                                 *cerca il padre
032300020208     C                   eval      d10cod = stpfat
032400020208     C                   call      'TIBS10R'
032500020208     C                   parm                    tibs10ds
032600020208     C                   if        d10err = *blanks                             *Figlio
032700020208     C                   z-add     d10cop        stpfatcliu
032800020208     C                   endif
032900020208     C                   endif
033000020208     C*
033100020208     C* Valorizzo i dati del cliente unificante se a questo punto ho già
033200020208     C                   if        stpuni > *zeros
033300020208     C*
033400020208     C* Reperisco le informazioni anagrafiche del cliente unificante
033500020208     C                   clear                   bs69ds
033600020208     C                   clear                   acods
033700020208     C                   clear                   indds
033800020208     C                   clear                   clpds
033900020208     C                   clear                   clsds
034000020208     C                   movel     knsif         i69sif
034100020208     C                   z-add     stpuni        I69KAC
034200020208     C                   z-add     stpuni        I69KCP
034300020208     C                   z-add     stpuni        I69KIN
034400020208     C                   call      'TIBS69R'
034500020208     C                   parm                    bs69ds
034600020208     C                   parm                    acods
034700020208     C                   parm                    indds
034800020208     C                   parm                    clpds
034900020208     C                   parm                    clsds
035000020208     C*
035100020208     C     o69err        ifne      '1'
035200020208     C                   eval      stpduni = acorag
035300020208     C* Reperisco le informazioini dell'agente commerciale
035400020208     C                   clear                   DS01
035500020208     C                   z-add     PARFIC        kblkut
035600020208     C                   movel     '01'          kblcod
035700020208     C                   movel     clpage        kblkey
035800020208     C     keytab        chain     tabel00f
035900020208     C                   if        %found(tabel00f)
036000020208     C                   movel     tbluni        DS01
036100020208     C                   eval      stpuniageu = §01RGF
036200020208     C                   endif
036300020208     C                   eval      stpuniclif = clpscf
036400020208     C                   endif
036500020208     C                   endif
036600011206     C*
036700011205     C                   ENDSR
036800020208     C*------------------------------------------------------------------------*
036900020208     C* REPMESS - EFFETTUA CONSIDERAZIONI E DETERMINA IL MESAGGIO DA ESPORRE
037000020208     C*------------------------------------------------------------------------*
037100020208     C     REPMESS       BEGSR
037200020208     C*
037300020208     C                   if        stpcliiva <> *blanks and
037400020208     C                             stpfativa <> *blanks
037500020208     C                   if        stpcliiva <> stpfativa
037600020208     C                   eval      stpmess = %trim(stpmess) + ' 1'
037700020208     C                   endif
037800020208     C                   endif
037900020208     C*
038000020208     C                   if        stpcliageu > *zeros and
038100020208     C                             stpfatageu > *zeros
038200020208     C                   if        stpcliageu <> stpfatageu
038300020208     C                   eval      stpmess = %trim(stpmess) + ' 2'
038400020208     C                   endif
038500020208     C                   endif
038600020208     C*
038700020208     C                   if        stpcliageu > *zeros and
038800020208     C                             stpuniageu > *zeros
038900020208     C                   if        stpcliageu <> stpuniageu
039000020208     C                   eval      stpmess = %trim(stpmess) + ' 3'
039100020208     C                   endif
039200020208     C                   endif
039300020208     C*
039400020208     C                   eval      stpmess = %trim(stpmess)
039500020208     C*
039600020208     C                   ENDSR
039700000306     C*------------------------------------------------------------------------*
039800011205     C* CHKCLI - CONTROLLA IL CLIENTE RISPETTO ALLA FILIALE "CHIAMANTE" e/o SELEZIONATA
039900000306     C*------------------------------------------------------------------------*
040000000306     C     CHKCLI        BEGSR
040100000306     C*
040200011205     C                   MOVEL     '0'           ERRl00
040300000306     C*
040400020208     C                   MOVEL     s_acoksc      WrkFil            3 0
040500011205     C*
040600011205     C                   IF        PARFIL > *zeros
040700020208     C                   IF        WrkFil = PARFIL
040800011205     C                   ELSE
040900011205     C                   MOVEL     '1'           ERRl00
041000011205     C                   ENDIF
041100011205     C                   ELSE
041200020903     C     WrkFil        LOOKUP    L1                                     20
041300020208     C                   IF        *in20=*on
041400011205     C                   ELSE
041500011205     C                   MOVEL     '1'           ERRl00
041600000306     C                   ENDIF
041700011205     C                   ENDIF
041800000306     C*
041900000306     C                   ENDSR
042000000810     C*------------------------------------------------------------------------*
042100000810     C* CARTAB - CARICA LE TABELLE OCCORRENTI
042200000810     C*------------------------------------------------------------------------*
042300000810     C     CARTAB        BEGSR
042400020903     C*
042500020903     C* REPERISCE I DATI UTENTE
042600020903     C                   EXSR      REPDATIUTE
042700020903     C*
042800020903     C* INIZIALIZZO LE VARIABILI DI WRK RELATIVE ALLE FILIALI IN GESTIONE
042900020903     C                   CLEAR                   L1
043000020903     C                   CLEAR                   TRUL06DS
043100020903     C                   CLEAR                   LIA
043200020903     C                   CLEAR                   TRUL26DS
043300020903     C                   CLEAR                   SKPO
043400020903     C                   CLEAR                   SKPOT
043500020903     C*
043600020903     C* CARICA LE FILIALI IN GESTIONE
043700020903     C                   IF        DUTLPO = '1'                                 *1° LIVELLO
043800020903     C                   EVAL      DEPKPJBU = KPJBU
043900020903     C                   MOVEL     'L'           D06TLA
044000020903     C                   MOVEL     '£1'          D06COD
044100021007     C                   MOVEL     duttfp        D06KEY
044200020903     C                   MOVEL     TRUL06DS      KPJBU
044300020903     C                   CALL      'TRUL06R'
044400020903     C                   PARM                    KPJBA
044500020903     C                   MOVEL     KPJBU         TRUL06DS
044600020903     C                   MOVEA     LIA           L1                              -P.O. terminal
044700020903     C                   EVAL      KPJBU = DEPKPJBU
044800020903     C                   ENDIF
044900020903     C*
045000020903     C                   IF        DUTLPO = '2'                                 *2° LIVELLO
045100020906     C                   MOVEL     DUTPOU        L1(1)                           -P.O. utente
045200020903     C                   ENDIF
045300020903     C*
045400020903     C                   IF        DUTLPO = 'S'                                 *SEDE
045500020903     C                   EVAL      I26TLA = 'L'
045600020903     C                   EVAL      I26TPT = 'P'
045700020903     C                   CALL      'TRUL26R'
045800020903     C                   PARM                    TRUL26DS
045900020903     C                   MOVEA     SKPO          L1                              -tutti i P.O.
046000020903     C                   ENDIF
046100000810     C*
046200000810     C                   ENDSR
046300020903     C*--------------------------------------------------------------------------------------------*
046400020903     C* REPERISCE I DATI UTENTE
046500020903     C*--------------------------------------------------------------------------------------------*
046600020903     C     REPDATIUTE    BEGSR
046700020903     C*
046800020903     C* INIZIALIZZA VARIABILI DI WRK
046900020903     C                   CLEAR                   TIBS34DS
047000020903     C                   CLEAR                   AZUTEDS
047100020903     C                   CLEAR                   DDATIUTE
047200020903     C*
047300020903     C     *DTAARA       DEFINE    §azute        azuteds
047400020903     C     *DTAARA       DEFINE    §datiute      ddatiute
047500020903     C                   IN(E)     *DTAARA
047600020903if  1C                   IF        %Error
047700020903     c                   EVAL      I34Tla = 'L'
047800020903     C                   CALL      'TIBS34R'
047900020903     C                   PARM                    Tibs34Ds
048000020903     C                   IN        *DTAARA
048100020903e   1C                   ENDIF
048200020903     C*
048300020903     C                   ENDSR
048400020208     C*------------------------------------------------------------------------*
048500020208     C* FINSR - OPERAZIONI FINALI
048600020208     C*------------------------------------------------------------------------*
048700020208     C     FINSR         BEGSR
048800020208     C*
048900020208     C* CHIUDE I FILE DEI PGM CHIAMATI
049000020208     C                   CLEAR                   bs69ds
049100020208     C                   CLEAR                   acods
049200020208     C                   CLEAR                   indds
049300020208     C                   CLEAR                   clpds
049400020208     C                   CLEAR                   clsds
049500020208     C                   MOVEL     'C'           i69tla
049600020208     C                   MOVEL     knsif         i69sif
049700020208     C                   CALL      'TIBS69R'
049800020208     C                   PARM                    bs69ds
049900020208     C                   PARM                    acods
050000020208     C                   PARM                    indds
050100020208     C                   PARM                    clpds
050200020208     C                   PARM                    clsds
050300020208     C*
050400020208     C                   ENDSR
050500970526     C*------------------------------------------------------------------------*
050600970526     C* *INZSR - OPERAZIONI INIZIALI
050700970526     C*------------------------------------------------------------------------*
050800970526     C     *INZSR        BEGSR
050900970526     C*---
051000970526     C* RICEVIMENTO PARAMETRI
051100970526     C*---
051200970526     C     *ENTRY        PLIST
051300970612     C                   PARM                    KPJBA
051400991214     C                   MOVEL     KPJBU         PARAM
051500020903     C*---
051600020903     C* VARIABILI RIFERITE AL DATA BASE
051700020903     C*---
051800020903     C     *LIKE         DEFINE    Kpjbu         DEPKPJBU                       *depositi
051900011205     C*
052000000810     C* CARICA LE TABELLE OCCORRENTI
052100000810     C                   EXSR      CARTAB
052200970526     C*---
052300991214     C* CHIAVI DI LETTURA
052400991214     C*---
052500000810     C*
052600000810     C* LETTURA tabel00f - PARZIALE
052700011205     C     KEYtab        KLIST
052800000810     C                   KFLD                    kblkut            1 0          *UTENTE
052900000810     C                   KFLD                    kblcod            2            *TABELLA
053000011205     C                   KFLD                    kblkey            8            *CODICE
053100011205     C     KE2tab        KLIST
053200011205     C                   KFLD                    kblkut            1 0          *UTENTE
053300011205     C                   KFLD                    kblcod            2            *TABELLA
053400991214     C*
053500970526     C                   ENDSR
