000100010925      *PARMS DYNUSRPRF(*OWNER)
000200970526     H*------------------------------------------------------------------------*
000300021127     H* STAMPA UNIFICAZIONI PADRI/FIGLI
000400970526     H*------------------------------------------------------------------------*
000500000920     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900011205     Ftis7k8P   O    E             PRINTER OFLIND(*IN05)
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
004500021127     D  PARKUN               105    111  0
004600000929     D*-------------------
004700000929     D* DS DI LETTURA RECORDSET DEL KUN VIA SQL
004800000929     D*-------------------
004900011205     D DSKUN         E DS                  EXTNAME(tikun00f)
005000011205     D*-------------------
005100011206     D* DS DI PROCEDURA X LETTURA TABELLA "01" DEGLI AGENTI UNIFICANTI
005200011205     D*-------------------
005300011205     D DS01          E DS
005400011205     D*-------------------
005500011205     D* DS VARIE DI REPERIMENTO DATI CLIENTI
005600011205     D*-------------------
005700000929     D BS69DS        e ds                  extname(TIBS69DS)
005800000929     D ACODS         e ds                  extname(cnaco00f)
005900000929     D INDDS         e ds                  extname(cnind00f)
006000000929     D CLPDS         e ds                  extname(cnclp00f)
006100000929     D CLSDS         e ds                  extname(fncls00f)
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
007700011206     D  ERRl00         s              1    INZ(*zeros)
007800011206     D  countCof       s              9  0 INZ(*zeros)
007900011206     D  I              s              9  0 INZ(*zeros)
008000011206     D  X              s              9  0 INZ(*zeros)
008100011206     D  wrkcli         s             11  0 INZ(*zeros)
008200020903     D*-------------
008300020903     D* DS DI WRK
008400020903     D*-------------
008500020903     D TIBS34DS      E DS                                                       *Profili utente
008600020903     D DDATIUTE      E DS                                                       *Dati utente
008700020903     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008800020903     D TRUL06DS      E DS                                                       *caricamento £x
008900020903     D  LIA                    1     90  0 DIM(30)                               -filiali
009000020903     D TRUL26DS      E DS                                                       *caricamento P.O.
009100020903     D  SKPO                  11    760  0 dim(250)
009200020903     D  SKPOT                761   1510  0 dim(250)
009300970526     C*------------------------------------------------------------------------*
009400991214     C* MAIN LINES
009500970526     C*------------------------------------------------------------------------*
009600970526     C*
009700991214     C* ESEGUE ROUTINE DI CREAZIONE E STAMPA
009800991214     C                   EXSR      ESEGUI
009900011205     C                   EXSR      FINSR
010000000306     C*
010100970526     C                   SETON                                        LR
010200011205     C*------------------------------------------------------------------------*
010300011205     C* FINSR - OPERAZIONI FINALI
010400011205     C*------------------------------------------------------------------------*
010500011205     C     FINSR         BEGSR
010600011205     C*
010700011205     C* CHIUDE I FILE DEI PGM CHIAMATI
010800011205     C                   CLEAR                   bs69ds
010900011205     C                   CLEAR                   acods
011000011205     C                   CLEAR                   indds
011100011205     C                   CLEAR                   clpds
011200011205     C                   CLEAR                   clsds
011300011205     C                   MOVEL     'C'           i69tla
011400011205     C                   MOVEL     knsif         i69sif
011500011205     C                   CALL      'TIBS69R'
011600011205     C                   PARM                    bs69ds
011700011205     C                   PARM                    acods
011800011205     C                   PARM                    indds
011900011205     C                   PARM                    clpds
012000011205     C                   PARM                    clsds
012100011205     C*
012200011205     C                   ENDSR
012300970526     C*------------------------------------------------------------------------*
012400991217     C* ESEGUI - OPERAZIONI PRINCIPALI
012500970526     C*------------------------------------------------------------------------*
012600991214     C     ESEGUI        BEGSR
012700991214     C*
012800011205     C* Per prima cosa valorizzo i parametri di lancio in stampa
012900011205     C                   exsr      valPAR
013000011205     C*
013100011205     C* Poi stampo l'intestazione
013200011205     C                   seton                                        05
013300011205     C                   exsr      stpTES
013400020207     C*
013500020905     C* Per ogni filiale in gestione eseguo distintamente le operazioni di elaborazione
013600020903     C     1             do        250           i
013700020207     C*
013800020903     C                   if        L1(i) > *zeros
013900020903     C                   movel     L1(i)         WrkCurrFil        3
014000021127     C*
014100021127     C                   if        L1(i) = PARFIL OR
014200021127     C                             PARFIL = *zeros
014300011205     C* Quindi la routine di elaborazione
014400020212     C                   exsr      exeKUN
014500020207     C                   endif
014600021127     C                   endif
014700020207     C*
014800020207     C                   enddo
014900011205     C*
015000011205     C* Al termine chiudo il prospetto
015100011205     C                   write     s7k8RI
015200011205     C                   write     s7k8FI
015300970526     C*
015400970526     C                   ENDSR
015500011205     C*------------------------------------------------------------------------*
015600011205     C* VALPAR - VALORIZZA I PARAMETRI DI LANCIO IN STAMPA
015700011205     C*------------------------------------------------------------------------*
015800011205     C     VALPAR        BEGSR
015900011205     C*
016000011206     C                   movel     PARFIL        STPPOP
016100011205     C                   if        PARFIL  = *ZEROS
016200011205     C                   eval      STPPOPD = 'TUTTI I P.O. DELL AREA'
016300011205     C                   else
016400011205     C                   eval      STPPOPD = PARDFI
016500011205     C                   endif
016600011205     C                   eval      STPDDE  = PAREDD
016700011205     C                   eval      STPTLE  = PARTLE
016800011205     C                   eval      STPTLED = PARDLE
016900011206     C                   eval      STPSIF  = PARFIR
017000011205     C*
017100011205     C                   ENDSR
017200011205     C*------------------------------------------------------------------------*
017300011205     C* STPTES - ESEGUE LA STAMPA DELLA TESTATA QUANDO NECESSARIO
017400011205     C*------------------------------------------------------------------------*
017500011205     C     STPTES        BEGSR
017600011205     C*
017700011205     C                   if        *in05 = *on
017800011205     C                   write     s7k8TE
017900011205     C                   setoff                                       05
018000011205     C                   endif
018100011205     C*
018200011205     C                   ENDSR
018300000929     C*------------------------------------------------------------------------*
018400011205     C* EXEKUN - Routine di selezione record del TIKUN00F secondo parametri di lancio
018500000929     C*------------------------------------------------------------------------*
018600011205     C     EXEKUN        BEGSR
018700011205     C*
018800011205     C                   movel     *loval        depCop           11 0
018900000929     C*
019000000929     C/EXEC SQL
019100000929     C+ declare C1 cursor for
019200011205     C+ select * from tikun00f where
019300011205     C+ kunvat  = ' '     and
019400011205     C+ kundde <= :PARDDE and
019500011206     C+ kundsc >= :PARDDE and
019600020207     C+ kuntle  = :PARTLE and
019700020207     C+ substr(digits(kuncop), 5, 3) = :WrkCurrFil
019800011205     C+ order by kuncop, kuncof
019900000929     C+ for read only
020000000929     C/END-EXEC
020100000929     C
020200000929     C/EXEC SQL
020300000929     C+ open C1
020400000929     C/END-EXEC
020500000929     C
020600000929     C/EXEC SQL
020700011205     C+ Fetch C1 into :DSKUN
020800000929     C/END-EXEC
020900000929     C*
021000000929     C                   dow       sqlcod = *zeros
021100011205     C                   exsr      chkcli
021200011205     C                   if        errl00 = '0'
021300011205     C                   exsr      exePRTP
021400011205     C                   endif
021500000929     C*
021600000929     C/EXEC SQL
021700011205     C+ Fetch C1 into :DSKUN
021800000929     C/END-EXEC
021900000929     C*
022000000929     C                   enddo
022100011206     C*
022200011206     C* A fine file rieseguo la routine di stampa x scaricare l'ultimo record letto
022300011206     C                   exsr      exePRTP
022400000929     C*
022500000929     C/EXEC SQL
022600000929     C+ close C1
022700000929     C/END-EXEC
022800000929     C
022900000929     C*
023000000929     C                   ENDSR
023100011205     C*------------------------------------------------------------------------*
023200011205     C* EXEPRTP - STAMPA IL DETTAGLIO PADRI
023300011205     C*------------------------------------------------------------------------*
023400011205     C     EXEPRTP       BEGSR
023500011205     C*
023600011205     C* Verifico l'avvenuta rottura x cliente "padre"
023700011205     C                   if        kuncop = depCop
023800011205     C                   eval      countCof = countCof + 1
023900011205     C*
024000011205     C* Eseguo operazioni per i clienti "figli" legati
024100011205     C                   exsr      exePRTF
024200011205     C                   else
024300011206     C*
024400011206     C* Verifico se ci sono ancora dei figli da stampare del padre precedente
024500011206     C                   if        wrkPosCof > 0
024600011206     C                   exsr      stpTES
024700011206     C                   write     s7k8DE
024800011206     C                   clear                   s7k8DE
024900011206     C                   endif
025000011206     C*
025100011206     C* A rottira di cliente "padre" lascio una riga vuota e re-inizializzo il contatore
025200011206     C                   eval      countCof = 1
025300011205     C*
025400011205     C* Reperisco le informazioni del cliente "padre"
025500011205     C                   eval      depCop = kuncop
025600011205     C                   eval      wrkCli = depCop
025700011205     C                   exsr      repinfo
025800011205     C                   eval      stpCOP = depCop
025900011205     C                   eval      stpDCOP = ACORAG
026000011206     C                   eval      stpAGEUP = §01RGF
026100011205     C*
026200011205     C* Eseguo operazioni per i clienti "figli" legati
026300011205     C                   exsr      exePRTF
026400011205     C                   endif
026500011205     C
026600011205     C*
026700011205     C                   ENDSR
026800011205     C*------------------------------------------------------------------------*
026900011205     C* EXEPRTF - STAMPA IL DETTAGLIO FIGLI
027000011205     C*------------------------------------------------------------------------*
027100011205     C     EXEPRTF       BEGSR
027200011205     C*
027300011205     C                   eval      wrkCli = kuncof
027400011205     C                   exsr      repinfo
027500011205     C*
027600011205     C     countCof      DIV       3             wrkNumCof         9 0
027700011205     C                   MVR                     wrkPosCof         1 0
027800011205     C*
027900011205     C                   if        wrkPosCof = 1
028000011205     C                   eval      stpCOF1 = kuncof
028100011205     C                   eval      stpDCOF1 = ACORAG
028200011206     C                   eval      stpAGEUF1 = §01RGF
028300011205     C                   endif
028400011205     C                   if        wrkPosCof = 2
028500011205     C                   eval      stpCOF2 = kuncof
028600011205     C                   eval      stpDCOF2 = ACORAG
028700011206     C                   eval      stpAGEUF2 = §01RGF
028800011205     C                   endif
028900011206     C                   if        wrkPosCof = 0
029000011205     C                   eval      stpCOF3 = kuncof
029100011205     C                   eval      stpDCOF3 = ACORAG
029200011206     C                   eval      stpAGEUF3 = §01RGF
029300011205     C                   endif
029400011205     C*
029500011206     C                   if        wrkPosCof = 0
029600011206     C                   exsr      stpTES
029700011206     C                   write     s7k8DE
029800011206     C                   clear                   s7k8DE
029900011206     C                   endif
030000011206     C*
030100011205     C                   ENDSR
030200011205     C*------------------------------------------------------------------------*
030300011206     C* REPINFO - REPERISCE TUTTE LE INFORMAZIONI NECESSARIO
030400011205     C*------------------------------------------------------------------------*
030500011205     C     REPINFO       BEGSR
030600011205     C*
030700011206     C* Reperisco le informazioni anagrafiche
030800011205     C                   CLEAR                   bs69ds
030900011205     C                   CLEAR                   acods
031000011205     C                   CLEAR                   indds
031100011205     C                   CLEAR                   clpds
031200011205     C                   CLEAR                   clsds
031300011205     C                   MOVEL     knsif         i69sif
031400011205     C                   Z-ADD     wrkcli        I69KAC
031500011205     C                   Z-ADD     wrkcli        I69KCP
031600011205     C                   CALL      'TIBS69R'
031700011205     C                   PARM                    bs69ds
031800011205     C                   PARM                    acods
031900011205     C                   PARM                    indds
032000011205     C                   PARM                    clpds
032100011205     C                   PARM                    clsds
032200011205     C*
032300011205     C     O69ERR        IFNE      '1'
032400011206     C*
032500011206     C* Reperisco le informazioini dell'agente commerciale
032600011206     C                   CLEAR                   DS01
032700011206     C                   Z-ADD     PARFIC        KBLKUT
032800011206     C                   MOVEL     '01'          KBLCOD
032900011206     C                   MOVEL     clpAGE        KBLKEY
033000011206     C     KEYTAB        CHAIN     TABEL00F
033100011206     C                   IF        %found(TABEL00F)
033200011206     C                   MOVEL     TBLUNI        DS01
033300011206     C                   ENDIF
033400011205     C                   ELSE
033500011205     C                   MOVEL     *ALL'*'       ACORAG
033600011205     C                   ENDIF
033700011206     C*
033800011205     C                   ENDSR
033900000306     C*------------------------------------------------------------------------*
034000011205     C* CHKCLI - CONTROLLA IL CLIENTE RISPETTO ALLA FILIALE "CHIAMANTE" e/o SELEZIONATA
034100000306     C*------------------------------------------------------------------------*
034200000306     C     CHKCLI        BEGSR
034300000306     C*
034400011205     C                   MOVEL     '0'           ERRl00
034500000306     C*
034600011206     C                   MOVEL     kuncop        wfilCop           7 0
034700011206     C                   MOVEL     kuncof        wfilCof           7 0
034800011206     C                   MOVE      wfilCop       filCop            3 0
034900011206     C                   MOVE      wfilCof       filCof            3 0
035000011205     C*
035100011205     C                   IF        PARFIL > *zeros
035200011205     C                   IF        filCop = PARFIL or
035300011205     C                             filCof = PARFIL
035400011205     C                   ELSE
035500011205     C                   MOVEL     '1'           ERRl00
035600011205     C                   ENDIF
035700011205     C                   ELSE
035800020903     C     filCop        LOOKUP    L1                                     20
035900020903     C     filCof        LOOKUP    L1                                     21
036000011205     C                   IF        *in20=*on or
036100011205     C                             *in21=*on
036200011205     C                   ELSE
036300011205     C                   MOVEL     '1'           ERRl00
036400000306     C                   ENDIF
036500011205     C                   ENDIF
036600021127     C*
036700021127     C* VERIFICO SE RICHIESTO NEL LANCIO 1 SPECIFICO CLIENTE UNIFICATE
036800021127     C                   IF        PARKUN > *zeros
036900021127     C                   IF        KUNcop <> PARKUN
037000021127     C                   MOVEL     '1'           ERRl00
037100021127     C                   ENDIF
037200021127     C                   ENDIF
037300000306     C*
037400000306     C                   ENDSR
037500000810     C*------------------------------------------------------------------------*
037600000810     C* CARTAB - CARICA LE TABELLE OCCORRENTI
037700000810     C*------------------------------------------------------------------------*
037800000810     C     CARTAB        BEGSR
037900020903     C*
038000020903     C* REPERISCE I DATI UTENTE
038100020903     C                   EXSR      REPDATIUTE
038200020903     C*
038300020903     C* INIZIALIZZO LE VARIABILI DI WRK RELATIVE ALLE FILIALI IN GESTIONE
038400020903     C                   CLEAR                   L1
038500020903     C                   CLEAR                   TRUL06DS
038600020903     C                   CLEAR                   LIA
038700020903     C                   CLEAR                   TRUL26DS
038800020903     C                   CLEAR                   SKPO
038900020903     C                   CLEAR                   SKPOT
039000020903     C*
039100020903     C* CARICA LE FILIALI IN GESTIONE
039200020903     C                   IF        DUTLPO = '1'                                 *1° LIVELLO
039300020903     C                   EVAL      DEPKPJBU = KPJBU
039400020903     C                   MOVEL     'L'           D06TLA
039500020903     C                   MOVEL     '£1'          D06COD
039600021007     C                   MOVEL     duttfp        D06KEY
039700020903     C                   MOVEL     TRUL06DS      KPJBU
039800020903     C                   CALL      'TRUL06R'
039900020903     C                   PARM                    KPJBA
040000020903     C                   MOVEL     KPJBU         TRUL06DS
040100020903     C                   MOVEA     LIA           L1                              -P.O. terminal
040200020903     C                   EVAL      KPJBU = DEPKPJBU
040300020903     C                   ENDIF
040400020903     C*
040500020903     C                   IF        DUTLPO = '2'                                 *2° LIVELLO
040600020906     C                   MOVEL     DUTPOU        L1(1)                           -P.O. utente
040700020903     C                   ENDIF
040800020903     C*
040900020903     C                   IF        DUTLPO = 'S'                                 *SEDE
041000020903     C                   EVAL      I26TLA = 'L'
041100020903     C                   EVAL      I26TPT = 'P'
041200020903     C                   CALL      'TRUL26R'
041300020903     C                   PARM                    TRUL26DS
041400020903     C                   MOVEA     SKPO          L1                              -tutti i P.O.
041500020903     C                   ENDIF
041600000810     C*
041700000810     C                   ENDSR
041800020903     C*--------------------------------------------------------------------------------------------*
041900020903     C* REPERISCE I DATI UTENTE
042000020903     C*--------------------------------------------------------------------------------------------*
042100020903     C     REPDATIUTE    BEGSR
042200020903     C*
042300020903     C* INIZIALIZZA VARIABILI DI WRK
042400020903     C                   CLEAR                   TIBS34DS
042500020903     C                   CLEAR                   AZUTEDS
042600020903     C                   CLEAR                   DDATIUTE
042700020903     C*
042800020903     C     *DTAARA       DEFINE    §azute        azuteds
042900020903     C     *DTAARA       DEFINE    §datiute      ddatiute
043000020903     C                   IN(E)     *DTAARA
043100020903if  1C                   IF        %Error
043200020903     c                   EVAL      I34Tla = 'L'
043300020903     C                   CALL      'TIBS34R'
043400020903     C                   PARM                    Tibs34Ds
043500020903     C                   IN        *DTAARA
043600020903e   1C                   ENDIF
043700020903     C*
043800020903     C                   ENDSR
043900970526     C*------------------------------------------------------------------------*
044000970526     C* *INZSR - OPERAZIONI INIZIALI
044100970526     C*------------------------------------------------------------------------*
044200970526     C     *INZSR        BEGSR
044300970526     C*---
044400970526     C* RICEVIMENTO PARAMETRI
044500970526     C*---
044600970526     C     *ENTRY        PLIST
044700970612     C                   PARM                    KPJBA
044800991214     C                   MOVEL     KPJBU         PARAM
044900020903     C*---
045000020903     C* VARIABILI RIFERITE AL DATA BASE
045100020903     C*---
045200020903     C     *LIKE         DEFINE    Kpjbu         DEPKPJBU                       *depositi
045300011205     C*
045400000810     C* CARICA LE TABELLE OCCORRENTI
045500000810     C                   EXSR      CARTAB
045600970526     C*---
045700991214     C* CHIAVI DI LETTURA
045800991214     C*---
045900000810     C*
046000000810     C* LETTURA tabel00f - PARZIALE
046100011205     C     KEYtab        KLIST
046200000810     C                   KFLD                    kblkut            1 0          *UTENTE
046300000810     C                   KFLD                    kblcod            2            *TABELLA
046400011205     C                   KFLD                    kblkey            8            *CODICE
046500011205     C     KE2tab        KLIST
046600011205     C                   KFLD                    kblkut            1 0          *UTENTE
046700011205     C                   KFLD                    kblcod            2            *TABELLA
046800991214     C*
046900970526     C                   ENDSR
