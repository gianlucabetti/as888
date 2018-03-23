000100010925      *PARMS DYNUSRPRF(*OWNER)
000200970526     H*------------------------------------------------------------------------*
000300021127     H* STAMPA UNIFICAZIONI PADRI/FIGLI
000400970526     H*------------------------------------------------------------------------*
000500000920     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900011205     Ftis7k8P   O    E             PRINTER OFLIND(*IN05)
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
004500021127     D  PARKUN               105    111  0
004600000929     D*-------------------
004700000929     D* DS DI LETTURA RECORDSET DEL KUN VIA SQL
004800000929     D*-------------------
004900011205     D DSKUN         E DS                  EXTNAME(tikun00f)
005000011205     D*-------------------
005100011205     D* DS VARIE DI REPERIMENTO DATI CLIENTI
005200011205     D*-------------------
005300000929     D BS69DS        e ds                  extname(TIBS69DS)
005400000929     D ACODS         e ds                  extname(cnaco00f)
005500000929     D INDDS         e ds                  extname(cnind00f)
005600000929     D CLPDS         e ds                  extname(cnclp00f)
005700000929     D CLSDS         e ds                  extname(fncls00f)
005800000810     D*-------------------
005900000810     D* FILIALI IN GESTIONE
006000000810     D*-------------------
006100020903     D L1              S              3  0 DIM(250)                             *FILIALI IN GESTIONE
006200000905     D*------------------
006300000901     D* DS "XSRDA8" - CONTROLLA DATA (8)
006400000901     D*------------------
006500000901     D WLBDA8          DS                  INZ
006600000901     D  G08DAT                 1      8  0
006700000901     D  G08INV                 9     16  0
006800000901     D  G08ERR                17     17
006900000901     D  G08TGI                18     22  0
007000011206     D*-------------------
007100011206     D* VARIABILI DI WRK
007200011206     D*-------------------
007300011206     D  ERRl00         s              1    INZ(*zeros)
007400011206     D  countCof       s              9  0 INZ(*zeros)
007500011206     D  I              s              9  0 INZ(*zeros)
007600011206     D  X              s              9  0 INZ(*zeros)
007700011206     D  wrkcli         s             11  0 INZ(*zeros)
007800020903     D*-------------
007900020903     D* DS DI WRK
008000020903     D*-------------
008100020903     D TIBS34DS      E DS                                                       *Profili utente
008200020903     D DDATIUTE      E DS                                                       *Dati utente
008300020903     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008400020903     D TRUL06DS      E DS                                                       *caricamento £x
008500020903     D  LIA                    1     90  0 DIM(30)                               -filiali
008600020903     D TRUL26DS      E DS                                                       *caricamento P.O.
008700020903     D  SKPO                  11    760  0 dim(250)
008800020903     D  SKPOT                761   1510  0 dim(250)
008900970526     C*------------------------------------------------------------------------*
009000991214     C* MAIN LINES
009100970526     C*------------------------------------------------------------------------*
009200970526     C*
009300991214     C* ESEGUE ROUTINE DI CREAZIONE E STAMPA
009400991214     C                   EXSR      ESEGUI
009500011205     C                   EXSR      FINSR
009600000306     C*
009700970526     C                   SETON                                        LR
009800011205     C*------------------------------------------------------------------------*
009900011205     C* FINSR - OPERAZIONI FINALI
010000011205     C*------------------------------------------------------------------------*
010100011205     C     FINSR         BEGSR
010200011205     C*
010300011205     C* CHIUDE I FILE DEI PGM CHIAMATI
010400011205     C                   CLEAR                   bs69ds
010500011205     C                   CLEAR                   acods
010600011205     C                   CLEAR                   indds
010700011205     C                   CLEAR                   clpds
010800011205     C                   CLEAR                   clsds
010900011205     C                   MOVEL     'C'           i69tla
011000011205     C                   MOVEL     knsif         i69sif
011100011205     C                   CALL      'TIBS69R'
011200011205     C                   PARM                    bs69ds
011300011205     C                   PARM                    acods
011400011205     C                   PARM                    indds
011500011205     C                   PARM                    clpds
011600011205     C                   PARM                    clsds
011700011205     C*
011800011205     C                   ENDSR
011900970526     C*------------------------------------------------------------------------*
012000991217     C* ESEGUI - OPERAZIONI PRINCIPALI
012100970526     C*------------------------------------------------------------------------*
012200991214     C     ESEGUI        BEGSR
012300991214     C*
012400011205     C* Per prima cosa valorizzo i parametri di lancio in stampa
012500011205     C                   exsr      valPAR
012600011205     C*
012700011205     C* Poi stampo l'intestazione
012800011205     C                   seton                                        05
012900011205     C                   exsr      stpTES
013000020207     C*
013100020905     C* Per ogni filiale in gestione eseguo distintamente le operazioni di elaborazione
013200020903     C     1             do        250           i
013300020207     C*
013400020903     C                   if        L1(i) > *zeros
013500020903     C                   movel     L1(i)         WrkCurrFil        3
013600021127     C*
013700021127     C                   if        L1(i) = PARFIL OR
013800021127     C                             PARFIL = *zeros
013900011205     C* Quindi la routine di elaborazione
014000020212     C                   exsr      exeKUN
014100020207     C                   endif
014200021127     C                   endif
014300020207     C*
014400020207     C                   enddo
014500011205     C*
014600011205     C* Al termine chiudo il prospetto
014700011205     C                   write     s7k8RI
014800011205     C                   write     s7k8FI
014900970526     C*
015000970526     C                   ENDSR
015100011205     C*------------------------------------------------------------------------*
015200011205     C* VALPAR - VALORIZZA I PARAMETRI DI LANCIO IN STAMPA
015300011205     C*------------------------------------------------------------------------*
015400011205     C     VALPAR        BEGSR
015500011205     C*
015600011206     C                   movel     PARFIL        STPPOP
015700011205     C                   if        PARFIL  = *ZEROS
015800011205     C                   eval      STPPOPD = 'TUTTI I P.O. DELL AREA'
015900011205     C                   else
016000011205     C                   eval      STPPOPD = PARDFI
016100011205     C                   endif
016200011205     C                   eval      STPDDE  = PAREDD
016300011205     C                   eval      STPTLE  = PARTLE
016400011205     C                   eval      STPTLED = PARDLE
016500011206     C                   eval      STPSIF  = PARFIR
016600011205     C*
016700011205     C                   ENDSR
016800011205     C*------------------------------------------------------------------------*
016900011205     C* STPTES - ESEGUE LA STAMPA DELLA TESTATA QUANDO NECESSARIO
017000011205     C*------------------------------------------------------------------------*
017100011205     C     STPTES        BEGSR
017200011205     C*
017300011205     C                   if        *in05 = *on
017400011205     C                   write     s7k8TE
017500011205     C                   setoff                                       05
017600011205     C                   endif
017700011205     C*
017800011205     C                   ENDSR
017900000929     C*------------------------------------------------------------------------*
018000011205     C* EXEKUN - Routine di selezione record del TIKUN00F secondo parametri di lancio
018100000929     C*------------------------------------------------------------------------*
018200011205     C     EXEKUN        BEGSR
018300011205     C*
018400011205     C                   movel     *loval        depCop           11 0
018500000929     C*
018600000929     C/EXEC SQL
018700000929     C+ declare C1 cursor for
018800011205     C+ select * from tikun00f where
018900011205     C+ kunvat  = ' '     and
019000011205     C+ kundde <= :PARDDE and
019100011206     C+ kundsc >= :PARDDE and
019200020207     C+ kuntle  = :PARTLE and
019300020207     C+ substr(digits(kuncop), 5, 3) = :WrkCurrFil
019400011205     C+ order by kuncop, kuncof
019500000929     C+ for read only
019600000929     C/END-EXEC
019700000929     C
019800000929     C/EXEC SQL
019900000929     C+ open C1
020000000929     C/END-EXEC
020100000929     C
020200000929     C/EXEC SQL
020300011205     C+ Fetch C1 into :DSKUN
020400000929     C/END-EXEC
020500000929     C*
020600000929     C                   dow       sqlcod = *zeros
020700011205     C                   exsr      chkcli
020800011205     C                   if        errl00 = '0'
020900011205     C                   exsr      exePRTP
021000011205     C                   endif
021100000929     C*
021200000929     C/EXEC SQL
021300011205     C+ Fetch C1 into :DSKUN
021400000929     C/END-EXEC
021500000929     C*
021600000929     C                   enddo
021700011206     C*
021800011206     C* A fine file rieseguo la routine di stampa x scaricare l'ultimo record letto
021900011206     C                   exsr      exePRTP
022000000929     C*
022100000929     C/EXEC SQL
022200000929     C+ close C1
022300000929     C/END-EXEC
022400000929     C
022500000929     C*
022600000929     C                   ENDSR
022700011205     C*------------------------------------------------------------------------*
022800011205     C* EXEPRTP - STAMPA IL DETTAGLIO PADRI
022900011205     C*------------------------------------------------------------------------*
023000011205     C     EXEPRTP       BEGSR
023100011205     C*
023200011205     C* Verifico l'avvenuta rottura x cliente "padre"
023300011205     C                   if        kuncop = depCop
023400011205     C                   eval      countCof = countCof + 1
023500011205     C*
023600011205     C* Eseguo operazioni per i clienti "figli" legati
023700011205     C                   exsr      exePRTF
023800011205     C                   else
023900011206     C*
024000011206     C* Verifico se ci sono ancora dei figli da stampare del padre precedente
024100011206     C                   if        wrkPosCof > 0
024200011206     C                   exsr      stpTES
024300011206     C                   write     s7k8DE
024400011206     C                   clear                   s7k8DE
024500011206     C                   endif
024600011206     C*
024700011206     C* A rottira di cliente "padre" lascio una riga vuota e re-inizializzo il contatore
024800011206     C                   eval      countCof = 1
024900011205     C*
025000011205     C* Reperisco le informazioni del cliente "padre"
025100011205     C                   eval      depCop = kuncop
025200011205     C                   eval      wrkCli = depCop
025300011205     C                   exsr      repinfo
025400011205     C                   eval      stpCOP = depCop
025500011205     C                   eval      stpDCOP = ACORAG
025600140616     C                   eval      stpAGEUP = cmmuni
025700011205     C*
025800011205     C* Eseguo operazioni per i clienti "figli" legati
025900011205     C                   exsr      exePRTF
026000011205     C                   endif
026100011205     C
026200011205     C*
026300011205     C                   ENDSR
026400011205     C*------------------------------------------------------------------------*
026500011205     C* EXEPRTF - STAMPA IL DETTAGLIO FIGLI
026600011205     C*------------------------------------------------------------------------*
026700011205     C     EXEPRTF       BEGSR
026800011205     C*
026900011205     C                   eval      wrkCli = kuncof
027000011205     C                   exsr      repinfo
027100011205     C*
027200011205     C     countCof      DIV       3             wrkNumCof         9 0
027300011205     C                   MVR                     wrkPosCof         1 0
027400011205     C*
027500011205     C                   if        wrkPosCof = 1
027600011205     C                   eval      stpCOF1 = kuncof
027700011205     C                   eval      stpDCOF1 = ACORAG
027800140616     C                   eval      stpAGEUF1 = cmmuni
027900011205     C                   endif
028000011205     C                   if        wrkPosCof = 2
028100011205     C                   eval      stpCOF2 = kuncof
028200011205     C                   eval      stpDCOF2 = ACORAG
028300140616     C                   eval      stpAGEUF2 = cmmuni
028400011205     C                   endif
028500011206     C                   if        wrkPosCof = 0
028600011205     C                   eval      stpCOF3 = kuncof
028700011205     C                   eval      stpDCOF3 = ACORAG
028800140616     C                   eval      stpAGEUF3 = cmmuni
028900011205     C                   endif
029000011205     C*
029100011206     C                   if        wrkPosCof = 0
029200011206     C                   exsr      stpTES
029300011206     C                   write     s7k8DE
029400011206     C                   clear                   s7k8DE
029500011206     C                   endif
029600011206     C*
029700011205     C                   ENDSR
029800011205     C*------------------------------------------------------------------------*
029900011206     C* REPINFO - REPERISCE TUTTE LE INFORMAZIONI NECESSARIO
030000011205     C*------------------------------------------------------------------------*
030100011205     C     REPINFO       BEGSR
030200011205     C*
030300011206     C* Reperisco le informazioni anagrafiche
030400011205     C                   CLEAR                   bs69ds
030500011205     C                   CLEAR                   acods
030600011205     C                   CLEAR                   indds
030700011205     C                   CLEAR                   clpds
030800011205     C                   CLEAR                   clsds
030900011205     C                   MOVEL     knsif         i69sif
031000011205     C                   Z-ADD     wrkcli        I69KAC
031100011205     C                   Z-ADD     wrkcli        I69KCP
031200011205     C                   CALL      'TIBS69R'
031300011205     C                   PARM                    bs69ds
031400011205     C                   PARM                    acods
031500011205     C                   PARM                    indds
031600011205     C                   PARM                    clpds
031700011205     C                   PARM                    clsds
031800011205     C*
031900011205     C     O69ERR        IFNE      '1'
032000011206     C*
032100140616     C* Reperisco le informazioni dell'agente commerciale
032200140616     C                   EVAL      CMMCod = clpage
032300140616     C     CMMCod        CHAIN     AZCMM01l
032400140616     C                   IF        not %found(AZCMM01L)
032500140616     C                   RESET                   AZCMM000
032600011206     C                   ENDIF
032700011205     C                   ELSE
032800011205     C                   MOVEL     *ALL'*'       ACORAG
032900011205     C                   ENDIF
033000011206     C*
033100011205     C                   ENDSR
033200000306     C*------------------------------------------------------------------------*
033300011205     C* CHKCLI - CONTROLLA IL CLIENTE RISPETTO ALLA FILIALE "CHIAMANTE" e/o SELEZIONATA
033400000306     C*------------------------------------------------------------------------*
033500000306     C     CHKCLI        BEGSR
033600000306     C*
033700011205     C                   MOVEL     '0'           ERRl00
033800000306     C*
033900011206     C                   MOVEL     kuncop        wfilCop           7 0
034000011206     C                   MOVEL     kuncof        wfilCof           7 0
034100011206     C                   MOVE      wfilCop       filCop            3 0
034200011206     C                   MOVE      wfilCof       filCof            3 0
034300011205     C*
034400011205     C                   IF        PARFIL > *zeros
034500011205     C                   IF        filCop = PARFIL or
034600011205     C                             filCof = PARFIL
034700011205     C                   ELSE
034800011205     C                   MOVEL     '1'           ERRl00
034900011205     C                   ENDIF
035000011205     C                   ELSE
035100020903     C     filCop        LOOKUP    L1                                     20
035200020903     C     filCof        LOOKUP    L1                                     21
035300011205     C                   IF        *in20=*on or
035400011205     C                             *in21=*on
035500011205     C                   ELSE
035600011205     C                   MOVEL     '1'           ERRl00
035700000306     C                   ENDIF
035800011205     C                   ENDIF
035900021127     C*
036000021127     C* VERIFICO SE RICHIESTO NEL LANCIO 1 SPECIFICO CLIENTE UNIFICATE
036100021127     C                   IF        PARKUN > *zeros
036200021127     C                   IF        KUNcop <> PARKUN
036300021127     C                   MOVEL     '1'           ERRl00
036400021127     C                   ENDIF
036500021127     C                   ENDIF
036600000306     C*
036700000306     C                   ENDSR
036800000810     C*------------------------------------------------------------------------*
036900000810     C* CARTAB - CARICA LE TABELLE OCCORRENTI
037000000810     C*------------------------------------------------------------------------*
037100000810     C     CARTAB        BEGSR
037200020903     C*
037300020903     C* REPERISCE I DATI UTENTE
037400020903     C                   EXSR      REPDATIUTE
037500020903     C*
037600020903     C* INIZIALIZZO LE VARIABILI DI WRK RELATIVE ALLE FILIALI IN GESTIONE
037700020903     C                   CLEAR                   L1
037800020903     C                   CLEAR                   TRUL06DS
037900020903     C                   CLEAR                   LIA
038000020903     C                   CLEAR                   TRUL26DS
038100020903     C                   CLEAR                   SKPO
038200020903     C                   CLEAR                   SKPOT
038300020903     C*
038400020903     C* CARICA LE FILIALI IN GESTIONE
038500020903     C                   IF        DUTLPO = '1'                                 *1° LIVELLO
038600020903     C                   EVAL      DEPKPJBU = KPJBU
038700020903     C                   MOVEL     'L'           D06TLA
038800020903     C                   MOVEL     '£1'          D06COD
038900021007     C                   MOVEL     duttfp        D06KEY
039000020903     C                   MOVEL     TRUL06DS      KPJBU
039100020903     C                   CALL      'TRUL06R'
039200020903     C                   PARM                    KPJBA
039300020903     C                   MOVEL     KPJBU         TRUL06DS
039400020903     C                   MOVEA     LIA           L1                              -P.O. terminal
039500020903     C                   EVAL      KPJBU = DEPKPJBU
039600020903     C                   ENDIF
039700020903     C*
039800020903     C                   IF        DUTLPO = '2'                                 *2° LIVELLO
039900020906     C                   MOVEL     DUTPOU        L1(1)                           -P.O. utente
040000020903     C                   ENDIF
040100020903     C*
040200020903     C                   IF        DUTLPO = 'S'                                 *SEDE
040300020903     C                   EVAL      I26TLA = 'L'
040400020903     C                   EVAL      I26TPT = 'P'
040500020903     C                   CALL      'TRUL26R'
040600020903     C                   PARM                    TRUL26DS
040700020903     C                   MOVEA     SKPO          L1                              -tutti i P.O.
040800020903     C                   ENDIF
040900000810     C*
041000000810     C                   ENDSR
041100020903     C*--------------------------------------------------------------------------------------------*
041200020903     C* REPERISCE I DATI UTENTE
041300020903     C*--------------------------------------------------------------------------------------------*
041400020903     C     REPDATIUTE    BEGSR
041500020903     C*
041600020903     C* INIZIALIZZA VARIABILI DI WRK
041700020903     C                   CLEAR                   TIBS34DS
041800020903     C                   CLEAR                   AZUTEDS
041900020903     C                   CLEAR                   DDATIUTE
042000020903     C*
042100020903     C     *DTAARA       DEFINE    §azute        azuteds
042200020903     C     *DTAARA       DEFINE    §datiute      ddatiute
042300020903     C                   IN(E)     *DTAARA
042400020903if  1C                   IF        %Error
042500020903     c                   EVAL      I34Tla = 'L'
042600020903     C                   CALL      'TIBS34R'
042700020903     C                   PARM                    Tibs34Ds
042800020903     C                   IN        *DTAARA
042900020903e   1C                   ENDIF
043000020903     C*
043100020903     C                   ENDSR
043200970526     C*------------------------------------------------------------------------*
043300970526     C* *INZSR - OPERAZIONI INIZIALI
043400970526     C*------------------------------------------------------------------------*
043500970526     C     *INZSR        BEGSR
043600970526     C*---
043700970526     C* RICEVIMENTO PARAMETRI
043800970526     C*---
043900970526     C     *ENTRY        PLIST
044000970612     C                   PARM                    KPJBA
044100991214     C                   MOVEL     KPJBU         PARAM
044200020903     C*---
044300020903     C* VARIABILI RIFERITE AL DATA BASE
044400020903     C*---
044500020903     C     *LIKE         DEFINE    Kpjbu         DEPKPJBU                       *depositi
044600011205     C*
044700000810     C* CARICA LE TABELLE OCCORRENTI
044800000810     C                   EXSR      CARTAB
044900970526     C*---
045000991214     C* CHIAVI DI LETTURA
045100991214     C*---
045200000810     C*
045300000810     C* LETTURA tabel00f - PARZIALE
045400011205     C     KEYtab        KLIST
045500000810     C                   KFLD                    kblkut            1 0          *UTENTE
045600000810     C                   KFLD                    kblcod            2            *TABELLA
045700011205     C                   KFLD                    kblkey            8            *CODICE
045800011205     C     KE2tab        KLIST
045900011205     C                   KFLD                    kblkut            1 0          *UTENTE
046000011205     C                   KFLD                    kblcod            2            *TABELLA
046100991214     C*
046200970526     C                   ENDSR
