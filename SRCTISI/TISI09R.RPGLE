000100970613     H*------------------------------------------------------------------------*
000200970613     H* CAPPARIO - STAMPA CAPPARIO - ESEGUE
000300970613     H*------------------------------------------------------------------------*
000400000221     H* COMPILARE IL PRINTER-FILE CON I SEGUENTI PARAMETRI:
000500000221     H* OPZ. PDM => C9
000600000222     H* PAGESIZE => 88
000700000221     H*             118
000800000221     H* LPI      => 8
000900000221     H* CPI      => 15
001000000222     H* OVRFLW   => 88
001100000221     H*------------------------------------------------------------------------*
001200970613     H DECEDIT('0,') DATEDIT(*DMY.)
001300970613     F*------------------------------------------------------------------------*
001400970613     F* DATA BASE
001500970613     F*------------------------------------------------------------------------*
001600000221     FAZCPC01L  IF   E           K DISK
001700010118     FAZCPL01L  IF   E           K DISK
001800970613     FTABEL00F  IF   E           K DISK
001900000221     FTISI09P   O    E             PRINTER OFLIND(*IN01)
002000970613     D*------------------------------------------------------------------------*
002100970613     D* SCHIERE
002200970613     D*------------------------------------------------------------------------*
002300970613     D*------------------
002400000221     D* DESCRIZIONI
002500970613     D*------------------
002600000221     D DES             S             60    DIM(10) CTDATA PERRCD(1)
002700970613     D*-------------------
002800970613     D* PROVINCE
002900970613     D*-------------------
003000970613     D PRV             S              2    DIM(300)                             *CODICE
003100970613     D DPRV            S             35    DIM(300)                             *DESCRIZIONE
003200000223     D*------------------
003300000223     D* SCHIERE PER MEMORIZZAZIONE LOCALITA/ISOLE DA STAMPARE
003400000223     D*------------------
003500010119     D s_riga          s             42    dim(5000)
003600970613     D*------------------------------------------------------------------------*
003700970613     D* INPUT
003800970613     D*------------------------------------------------------------------------*
003900010118     D*------------------
004000010118     D* DS DI RIDEFINIZIONE SCHIERA DI STAMPA
004100010118     D*------------------
004200010118     D stpriga1        DS
004300010118     D  stploc1                1     28
004400010119     D  stpcap1               29     38
004500010119     D  stpprv1               39     42
004600010118     D*
004700010118     D stpriga2        DS
004800010118     D  stploc2                1     28
004900010119     D  stpcap2               29     38
005000010119     D  stpprv2               39     42
005100010118     D*
005200010118     D stpriga3        DS
005300010118     D  stploc3                1     28
005400010119     D  stpcap3               29     38
005500010119     D  stpprv3               39     42
005600970613     D*------------------
005700970613     D* DS PROVINCIE
005800970613     D*------------------
005900970613     D DSPR          E DS
006000970613     D*------------------
006100970613     D* DS "XSRDA8" - CONTROLLA DATA (8)
006200970613     D*------------------
006300970613     D WLBDA8          DS                  INZ
006400970613     D  G08DAT                 1      8  0
006500970613     D  G08INV                 9     16  0
006600970613     D  G08ERR                17     17
006700970613     D  G08TGI                18     22  0
006800970613     D*-------------------
006900970613     D* DS PARAMETRI LANCIO
007000970613     D*-------------------
007100970613     D PARAM           DS
007200000221     D  PARDAT                 1      8  0
007300000221     D  PARVER                 9     13  0
007400000221     D  PARPRI                14     14
007500000221     D  PARLDI                15     15
007600000221     D  PARISO                16     16
007700000221     D  PARCAD                17     25
007800000221     D  PARCAA                26     34
007900000221     D  PARPRD                35     36
008000000221     D  PARPRA                37     38
008100000221     D  PARLAD                39     41  0
008200000221     D  PARLAA                42     44  0
008300000223     D*-------------------
008400000223     D* ARCHITETTURA
008500000223     D*-------------------
008600000223     D KPJBA         E DS
008700970613     C*------------------------------------------------------------------------*
008800970613     C* MAIN LINE
008900970613     C*------------------------------------------------------------------------*
009000970613     C*
009100970613     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
009200970613     C                   EXSR      SETFIL
009300970613     C*
009400970613     C* CICLO FINO A FINE FILE
009500970613DO  1C     FINL00        DOWEQ     '0'
009600000221     C*
009700000221     C* MEMORIZZAZIONE DATI DA STAMPARE
009800000221     C                   EXSR      MEMDAT
009900970613     C*
010000970613     C* LEGGE IL RECORD SUCCESSIVO
010100970613     C                   EXSR      LETFIL
010200970613E   1C                   ENDDO                                                  *FINE CAP
010300000221     C*
010400000221     C* EFFETTUA LA STAMPA
010500000221     C                   EXSR      STAMPA
010600000221     C*
010700970613     C*
010800970613     C* STAMPA RIGA DI FINE PROSPETTO
010900000221     C                   WRITE     SI09FI                                       *RIGA FINE STAMPA
011000970613     C*
011100970613     C                   SETON                                        LR
011200970613     C*------------------------------------------------------------------------*
011300970613     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE
011400970613     C*------------------------------------------------------------------------*
011500970613     C     SETFIL        BEGSR
011600970613     C*
011700000221     C* POSIZIONAMENTO SUL FILE
011800010118     C     PARVER        SETLL     AZCPL01L                           99
011900970613     C*
012000000221     C* LETTURA RECORD
012100970613IF  1C     *IN99         IFEQ      *OFF                                         *RECORD TROVATO
012200970613     C                   EXSR      LETFIL                                       *LETTURA RECORD
012300970613X   1C                   ELSE                                                   *FINE FILE
012400970613     C                   MOVEL     '1'           FINL00                         *FINE PGM
012500970613E   1C                   ENDIF
012600970613     C*
012700970613     C                   ENDSR
012800970613     C*------------------------------------------------------------------------*
012900970613     C* LETFIL - LETTURA SUCESSIVA RECORD
013000970613     C*------------------------------------------------------------------------*
013100970613     C     LETFIL        BEGSR
013200970613     C*
013300970613     C* LEGGE FINO A:
013400970613     C*      -FINE FILE
013500970613     C*      -TROVATO RECORD VALIDO
013600970613     C                   MOVEL     'N'           WRECOK
013700970613DO  1C     *IN99         DOWEQ     *OFF
013800970613     C     WRECOK        ANDEQ     'N'
013900010118     C                   READ      AZCPL01L                               99
014000970613     C*
014100970613IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
014200001214     C     CPLVER        ORNE      PARVER
014300970613     C                   MOVEL     '1'           FINL00                         *FINE PROGRAMMA
014400970613X   2C                   ELSE                                                   *RECORD TROVATO
014500970613     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
014600970613E   2C                   ENDIF
014700970613E   1C                   ENDDO
014800970613     C*
014900970613     C                   ENDSR
015000970613     C*------------------------------------------------------------------------*
015100000221     C* CHKREC - CONTROLLA VALIDITA' RECORD
015200970613     C*------------------------------------------------------------------------*
015300970613     C     CHKREC        BEGSR
015400970613     C*
015500970613     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
015600010118     C*
015700010118     C* VERIFICO A ROTTURA DI CAP SE SONO STATE CONSIDERATE LOCALITA'
015800010118     C                   IF        CPLCAP <> DEPCAP
015900010118     C                   IF        DEPCAP <> *BLANKS AND
016000010118     C                             COUNTLOC = *ZEROS AND
016100010118     C                             FLGCAP = '1'
016200010119     C     PARPRI        IFEQ      'S'
016300010119     C                   EXSR      MEMCAP
016400010118     C                   ENDIF
016500010119     C                   ENDIF
016600010118     C                   MOVEL     CPLCAP        DEPCAP
016700010118     C                   Z-ADD     *ZEROS        COUNTLOC
016800010118     C                   ENDIF
016900000221     C*
017000010118     C* REPERISCO IL RECORD CORRELATO SUL "CAPPARIO" E VERIFICO PARTICOLARITA'
017100000221     C                   Z-ADD     PARVER        KCCver
017200000221     C                   MOVEL     CPLNAR        KCCnar
017300000221     C                   MOVEL     CPLCAP        KCCcap
017400001214     C     KEYCPC        CHAIN     AZCPC01L
017500001214     C                   IF        NOT %FOUND(AZCPC01L)
017600001214     C                   MOVEL     'N'           WRECOK
017700010118     C                   ELSE
017800010118     C                   IF        (PARLDI = 'S' AND CPCISO = 'D') OR
017900010118     C                             (PARISO = 'S' AND CPCISO = 'I')
018000010118     C                   MOVEL     '1'           FLGCAP
018100010119     C                   MOVEL     '*'           FLGDIS
018200010118     C                   ELSE
018300010118     C                   MOVEL     '0'           FLGCAP
018400010119     C                   MOVEL     *blanks       FLGDIS
018500010118     C                   ENDIF
018600010118     C                   ENDIF
018700970613     C*
018800971003     C* CAP ANNULLATO, ESCLUDE
018900000221     C     WRECOK        IFEQ      'S'
019000000228IF  1C     CPLATB        IFEQ      *BLANKS
019100000228     C     CPCATB        ANDEQ     *BLANKS
019200000228     C     CPLFI1        ANDEQ     *BLANKS
019300000228     C     CPCFI1        ANDEQ     *BLANKS
019400000228     C                   ELSE
019500970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
019600970613E   1C                   ENDIF
019700000221     C                   ENDIF
019800000221     C*
019900000221     C* SOLO LOCALITA' PRINCIPALI
020000000221     C     WRECOK        IFEQ      'S'
020100000221     C     PARPRI        IFEQ      'S'
020200001009     C     CPLDLV        ANDNE     'S'
020300000221     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
020400000221     C                   ENDIF
020500000221     C                   ENDIF
020600010117     C*
020700010117     C* SE FINO A QUESTO PUNTO E' OK ==> INVERTO IL TEST DI INCLUSIONE
020800010117     C     WRECOK        IFEQ      'S'
020900010117     C                   MOVEL     'N'           WRECOK
021000010118     C*
021100010117     C* LOCALITA' DISAGIATE
021200010119     C                   IF        (PARLDI='S' AND CPLISO='D') OR
021300010119     C                             (PARLDI='S' AND CPCISO='D'  AND
021400050418     C                              PARPRI='S' AND CPLDLV='S'  AND
021500100512     C                              CPLLIV='L')                OR
021600010119     C                             (PARLDI='S' AND CPCISO='D'  AND
021700050418     C                              PARPRI='N' AND CPLLIV='C')
021800010119     C
021900010117     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
022000000221     C                   ENDIF
022100000221     C*
022200010117     C* ISOLE
022300010119     C                   IF        (PARISO='S' AND CPLISO='I') OR
022400010119     C                             (PARISO='S' AND CPCISO='I'  AND
022500050418     C                              PARPRI='S' AND CPLDLV='S'  AND
022600100512     C                              CPLLIV='L')                OR
022700010119     C                             (PARISO='S' AND CPCISO='I'  AND
022800050418     C                              PARPRI='N' AND CPLLIV='C')
022900010119     C
023000010119     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
023100010119     C                   ENDIF
023200010117     C*
023300000221     C                   ENDIF
023400970613     C*
023500970613     C* CAP <> DA QUELLI SELEZIONATI, ESCLUDE
023600000221     C     WRECOK        IFEQ      'S'
023700010117     C     PARCAD        IFNE      *BLANKS
023800010117     C     CPLCAP        IFGE      PARCAD
023900000221     C     CPLCAP        ANDLE     PARCAA
024000010117     C                   ELSE
024100970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
024200010117     C                   ENDIF
024300010117     C                   ENDIF
024400000221     C                   ENDIF
024500970613     C*
024600970613     C* PROVINCIA <> DA QUELLE SELEZIONATE, ESCLUDE
024700000221     C     WRECOK        IFEQ      'S'
024800010117     C     PARPRD        IFNE      *BLANKS
024900010117     C     CPCPRV        IFGE      PARPRD
025000000221     C     CPCPRV        ANDLE     PARPRA
025100010117     C                   ELSE
025200970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
025300010117     C                   ENDIF
025400010117     C                   ENDIF
025500000221     C                   ENDIF
025600970613     C*
025700980616     C* LINEA ARRIVO (STANDARD SE BAR, OLTRE SE SDI) <> DA QUELLE SELEZIONATE, ESCLUDE
025800000221     C     WRECOK        IFEQ      'S'
025900010117     C     PARLAD        IFGT      *ZEROS
026000100512     c                   if        (cplliv='L' and
026100100512     C                              CPLLNA >= PARLAD AND CPLLNA <= PARLAA) OR
026200100512     c                             (cplliv='L' and
026300100512     C                              CPLLOL >= PARLAD AND CPLLOL <= PARLAA) OR
026400100512     c                             (cplliv='L' and
026500100512     C                              CPLLO2 >= PARLAD AND CPLLO2 <= PARLAA) or
026600100512     c                             (cplliv='C' and
026700100512     C                              CPcLNA >= PARLAD AND CPcLNA <= PARLAA) OR
026800100512     c                             (cplliv='C' and
026900100512     C                              CPcLOL >= PARLAD AND CPcLOL <= PARLAA) OR
027000100512     c                             (cplliv='C' and
027100100512     C                              CPcLO2 >= PARLAD AND CPcLO2 <= PARLAA)
027200010117     C                   ELSE
027300970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
027400010117     C                   ENDIF
027500010117     C                   ENDIF
027600000221     C                   ENDIF
027700970613     C*
027800010118     C                   IF        WRECOK = 'S'
027900010118     C                   Z-ADD     1             COUNTLOC
028000010118     C                   ENDIF
028100010118     C*
028200970613     C                   ENDSR
028300000221     C*------------------------------------------------------------------------*
028400000221     C* MEMDAT - MEMORIZZA DATI X STAMPA
028500000221     C*------------------------------------------------------------------------*
028600000221     C     MEMDAT        BEGSR
028700000221     C*
028800010118     C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
028900000221     C*
029000000221     C                   add       1             x
029100010118     C                   eval      stploc1   = %trim(CPLLOC)
029200010119     C                   eval      stpcap1   = %trim(CPLCAP) + FLGDIS
029300010118     C                   eval      stpprv1   = '('+CPCPRV+')'
029400010118     C                   eval      s_riga(x) = stpriga1
029500000221     C*
029600000221     C                   ENDIF
029700000221     C*
029800000221     C                   ENDSR
029900010118     C*------------------------------------------------------------------------*
030000010118     C* MEMCAP - MEMORIZZA DATI X STAMPA RELATIVI A CAP DIS/ISO SENZA LOC DIS/ISO
030100010118     C*------------------------------------------------------------------------*
030200010118     C     MEMCAP        BEGSR
030300010118     C*
030400010118     C                   add       1             x
030500010118     C                   eval      stploc1   = *blanks
030600010118     C                   eval      stpcap1   = %trim(CPCCAP)
030700010118     C                   eval      stpprv1   = '('+CPCPRV+')'
030800010118     C                   eval      s_riga(x) = stpriga1
030900010118     C*
031000010118     C                   ENDSR
031100970613     C*------------------------------------------------------------------------*
031200010119     C* STAMPA - ESEGUE MATERIALMENTE LA STAMPA
031300970613     C*------------------------------------------------------------------------*
031400000221     C     STAMPA        BEGSR
031500000221     C*
031600000221     C* STAMPA INNANZITUTTO LA TESTATA
031700000221     C                   SETOFF                                       01
031800000221     C                   WRITE     SI09TE                                       *TESTATA
031900010118     C*
032000010118     C* ORDINO LA SCHIERA DA STAMPARE
032100010118     C                   SORTA     s_riga
032200010118     C*
032300010118     C* A QUESTO PUNTO HO NECESSITA' DI CONOSCERE L'INDICE DEL PRIMO ELEMENTO PIENO
032400010118     C                   Z-ADD     1             X
032500010118     C                   DOW       X <= 5000
032600010118     C                   IF        s_riga(x) <> *blanks
032700010118     C                   Z-ADD     X             DEPIND            5 0
032800010118     C                   LEAVE
032900010118     C                   ENDIF
033000010118     C                   ADD       1             X
033100010118     C                   ENDDO
033200000221     C*
033300000221     C* CICLA SULLE SCHIERE E STAMPA LE RIGHE DI DETTAGLIO
033400000221     C*=====================
033500000221     C* NOTE: - IL VALORE "70" SOTTOINDICATO IDENTIFICA IL NUMERO DI RIGHE DI DETTAGLIO
033600000221     C*         CHE SI VUOLE STAMPARE IN 1 PAGINA
033700000221     C*       - IL VALORE "*X" SOTTOINDICATO IDENTIFICA IL NUMERO DI COLONNA OLTRE LA PRIMA
033800000221     C*=====================
033900010118     C                   Z-ADD     66            righe             5 0
034000010118     C                   Z-ADD     DEPIND        x
034100000221     C                   Z-ADD     *zeros        x2                5 0
034200000221     C                   Z-ADD     *zeros        x3                5 0
034300000222     C                   Z-ADD     *zeros        conta             5 0
034400010118     C                   Z-ADD     *zeros        giro              5 0
034500010118     C*
034600010725     C                   DOW       x <= 5000 and
034700010725     C                             x  > *zeros
034800000221     C                   eval      x2=x+(righe*1)                                  *2° COLONNA
034900010118     C                   eval      x3=x+(righe*2)                                  *3° COLONNA
035000010118     C                   clear                   SI09ST
035100010118     C                   if        x<=5000
035200010118     C                   movel(p)  s_riga(x)     stpriga1
035300010118     C                   movel     stpcap1       STCAP1
035400010118     C                   eval      STLOC1=%trim(stploc1)+stpprv1
035500010118     C                   endif
035600010118     C                   if        x2<=5000
035700010118     C                   movel(p)  s_riga(x2)    stpriga2
035800010118     C                   movel     stpcap2       STCAP2
035900010118     C                   eval      STLOC2=%trim(stploc2)+stpprv2
036000010118     C                   endif
036100010118     C                   if        x3<=5000
036200010118     C                   movel(p)  s_riga(x3)    stpriga3
036300010118     C                   movel     stpcap3       STCAP3
036400010118     C                   eval      STLOC3=%trim(stploc3)+stpprv3
036500010118     C                   endif
036600000222     C*
036700000222     C* VERIFICO N° EFETTIVO DI RIGHE STAMPATE
036800000222     C     conta         ifeq      righe
036900000222     C                   seton                                        01
037000000222     C                   z-add     *zeros        conta
037100000222     C                   endif
037200970613     C*
037300000221     C     *IN01         IFEQ      *ON
037400000221     C                   WRITE     SI09TE                                       *TESTATA
037500970613     C                   SETOFF                                       01
037600000221     C                   ENDIF
037700000222     C
037800010118     C* STAMPA DETTAGLIO SE ALMENO UNA DELLE TRE COLONNE E' VALORIZZATA
037900010118     C                   IF        STCAP1 <> *blanks OR
038000010118     C                             STCAP2 <> *blanks OR
038100010118     C                             STCAP3 <> *blanks
038200000221     C                   WRITE     SI09ST                                       *DETTAGLIO
038300010118     C* INCREMENTO IL CONTATORE DI STAMPA
038400010118     C                   ADD       1             conta
038500010118     C                   ENDIF
038600010118     C*
038700010118     C* INCREMENTO L'INDICE
038800010118     C                   add       1             x
038900000221     C*
039000000221     C* VERIFICO COSTANTEMENTE IL VALORE DELL'INDICE "x" PER EVITARE DI "SFUMARE"
039100000221     C* LE RIGHE DI STAMPA
039200000221     C* -    2 = n° di colonne oltre la prima per pagina
039300010118     C                   if        x>righe+(righe*3*giro)+DEPIND-1
039400000221     C                   eval      x=x+(righe*2)
039500000221     C                   add       1             giro
039600000221     C                   endif
039700000221     C*
039800000221     C                   ENDDO
039900000221     C*
040000970613     C                   ENDSR
040100970613     C*------------------------------------------------------------------------*
040200970613     C* CARTAB - CARICA LE TABELLE
040300970613     C*------------------------------------------------------------------------*
040400970613     C     CARTAB        BEGSR
040500970613     C*--------------------
040600970613     C* PROVINCE
040700970613     C*--------------------
040800000221     C                   Z-ADD     *ZEROS        I                 5 0
040900020906     C                   Z-ADD     1             KTBKUT
041000970613     C                   MOVEL     'PR'          KTBCOD
041100970613     C     KE2TAB        SETLL     TABEL00F
041200970613     C     KE2TAB        READE     TABEL00F                               99
041300970613DO  1C     *IN99         DOWEQ     *OFF
041400970613IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
041500970613     C                   ADD       1             I
041600970613     C                   MOVEL     TBLKEY        PRV(I)                         *CODICE
041700970613     C                   MOVEL     TBLUNI        DSPR
041800970613     C                   MOVEL     §PRDES        DPRV(I)                        *DESCRIZIONE
041900970613E   2C                   ENDIF
042000970613     C     KE2TAB        READE     TABEL00F                               99
042100970613E   1C                   ENDDO
042200970613     C*
042300970613     C                   ENDSR
042400970613     C*------------------------------------------------------------------------*
042500970613     C* *INZSR - OPERAZIONI INIZIALI
042600970613     C*------------------------------------------------------------------------*
042700970613     C     *INZSR        BEGSR
042800970613     C*
042900970613     C* RICEVIMENTO PARAMETRI
043000970613     C     *ENTRY        PLIST
043100970613     C                   PARM                    KPJBA
043200000223     C                   MOVEL     KPJBU         PARAM
043300000221     C*
043400000221     C* IN BASE AI PARAMETRI DI LANCIO RICHIESTI => IMPOSTA IL "TITOLO" DI STAMPA
043500000221     C* ---- SOLO LOCALITA' DISAGIATE ----
043600000221     C     PARLDI        IFEQ      'S'
043700000221     C     PARISO        ANDEQ     'N'
043800000221     C                   MOVEL     DES(1)        STATES
043900010119     C                   MOVEL     DES(4)        STANOT
044000000221     C                   ENDIF
044100000221     C* ---- SOLO ISOLE ----
044200000221     C     PARLDI        IFEQ      'N'
044300000221     C     PARISO        ANDEQ     'S'
044400000221     C                   MOVEL     DES(3)        STATES
044500010119     C                   MOVEL     DES(6)        STANOT
044600000221     C                   ENDIF
044700000221     C* ---- SIA LOCALITA' DISAGIATE CHE ISOLE ----
044800000221     C     PARLDI        IFEQ      'S'
044900000221     C     PARISO        ANDEQ     'S'
045000000221     C                   MOVEL     DES(2)        STATES
045100010119     C                   MOVEL     DES(5)        STANOT
045200000221     C                   ENDIF
045300970613     C*
045400000221     C* VARIABILI RIFERITE AL DATA BASE
045500000221     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
045600970613     C     *LIKE         DEFINE    TBLCOD        KTBCOD
045700970613     C     *LIKE         DEFINE    TBLKEY        KTBKEY
045800000221     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPL00F
045900000221     C     *LIKE         DEFINE    CPCNAR        KCCNAR
046000000221     C     *LIKE         DEFINE    CPCCAP        KCCCAP
046100970613     C*
046200970613     C* CHIAVI DI LETTURA
046300970613     C* TABEL00F - PARZIALE
046400970613     C     KE2TAB        KLIST
046500970613     C                   KFLD                    KTBKUT                         *UTENTE
046600970613     C                   KFLD                    KTBCOD                         *TABELLA
046700000221     C* AZCPC01L - COMPLETA
046800000221     C     KEYCPC        KLIST
046900000221     C                   KFLD                    KCCVER                         *VERSIONE
047000000221     C                   KFLD                    KCCNAR                         *NAZIONE
047100000221     C                   KFLD                    KCCCAP                         *PROVINCIA
047200970613     C*
047300970613     C* VARIABILI DI CONTROLLO
047400970613     C                   MOVEL     '0'           FINL00            1            *FINE PGM
047500970613     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
047600970613     C                   MOVEL     '1'           *IN01                          *OVERFLOW
047700010118     C                   MOVEL     '0'           FLGCAP            1
047800010119     C                   MOVEL     *BLANKS       FLGDIS            1
047900010118     C                   MOVEL     *BLANKS       DEPCAP            9
048000010118     C                   Z-ADD     *ZEROS        COUNTLOC          1 0
048100970613     C*
048200970613     C* REPERIMENTO DATA E ORA CORRENTE
048300970613     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
048400970613     C                   MOVEL     WN14          ORA               6 0          *ORA  (6)
048500970613     C                   MOVE      WN14          DATA              8 0          *DATA (8) IN G/M/AA
048600970613     C*
048700970613     C* CARICA LE TABELLE OCCORRENTI
048800970613     C                   EXSR      CARTAB
048900970613     C*
049000000221     C* OPERAZIONI INIZIALI SULLE SCHIERE (INIZIALIZZAZIONE ELEMENTI E INDICE)
049100010118     C                   CLEAR                   s_riga
049200000221     C                   Z-ADD     *zeros        x                 5 0
049300000221     C*
049400970613     C                   ENDSR
049500970613     C*------------------------------------------------------------------------*
049600000221**   DES - DESCRIZIONI DI TESTA
049700010119 *** STAMPA CAP/LOCALITA' DISAGIATE ***                     1
049800010119 ** STAMPA CAP/LOCALITA' DISAGIATE E ISOLE **               2
049900010119         *** STAMPA ISOLE ***                               3
050000010119 (* = tutto il CAP è DISAGIATO)                             4
050100010119 (* = tutto il CAP è DISAGIATO o ISOLA)                     5
050200010119 (* = tutto il CAP è ISOLA)                                 6
