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
021400010119     C                              PARPRI='S' AND CPLDLV='S') OR
021500010119     C                             (PARLDI='S' AND CPCISO='D'  AND
021600010119     C                              PARPRI='N')
021700010119     C
021800010117     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
021900000221     C                   ENDIF
022000000221     C*
022100010117     C* ISOLE
022200010119     C                   IF        (PARISO='S' AND CPLISO='I') OR
022300010119     C                             (PARISO='S' AND CPCISO='I'  AND
022400010119     C                              PARPRI='S' AND CPLDLV='S') OR
022500010119     C                             (PARISO='S' AND CPCISO='I'  AND
022600010119     C                              PARPRI='N')
022700010119     C
022800010119     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
022900010119     C                   ENDIF
023000010117     C*
023100000221     C                   ENDIF
023200970613     C*
023300970613     C* CAP <> DA QUELLI SELEZIONATI, ESCLUDE
023400000221     C     WRECOK        IFEQ      'S'
023500010117     C     PARCAD        IFNE      *BLANKS
023600010117     C     CPLCAP        IFGE      PARCAD
023700000221     C     CPLCAP        ANDLE     PARCAA
023800010117     C                   ELSE
023900970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
024000010117     C                   ENDIF
024100010117     C                   ENDIF
024200000221     C                   ENDIF
024300970613     C*
024400970613     C* PROVINCIA <> DA QUELLE SELEZIONATE, ESCLUDE
024500000221     C     WRECOK        IFEQ      'S'
024600010117     C     PARPRD        IFNE      *BLANKS
024700010117     C     CPCPRV        IFGE      PARPRD
024800000221     C     CPCPRV        ANDLE     PARPRA
024900010117     C                   ELSE
025000970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
025100010117     C                   ENDIF
025200010117     C                   ENDIF
025300000221     C                   ENDIF
025400970613     C*
025500980616     C* LINEA ARRIVO (STANDARD SE BAR, OLTRE SE SDI) <> DA QUELLE SELEZIONATE, ESCLUDE
025600000221     C     WRECOK        IFEQ      'S'
025700010117     C     PARLAD        IFGT      *ZEROS
025800010118     C                   IF        (CPLLNA >= PARLAD AND CPLLNA <= PARLAA) OR
025900010118     C                             (CPLLOL >= PARLAD AND CPLLOL <= PARLAA) OR
026000010118     C                             (CPLLO2 >= PARLAD AND CPLLO2 <= PARLAA)
026100010117     C                   ELSE
026200970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
026300010117     C                   ENDIF
026400010117     C                   ENDIF
026500000221     C                   ENDIF
026600970613     C*
026700010118     C                   IF        WRECOK = 'S'
026800010118     C                   Z-ADD     1             COUNTLOC
026900010118     C                   ENDIF
027000010118     C*
027100970613     C                   ENDSR
027200000221     C*------------------------------------------------------------------------*
027300000221     C* MEMDAT - MEMORIZZA DATI X STAMPA
027400000221     C*------------------------------------------------------------------------*
027500000221     C     MEMDAT        BEGSR
027600000221     C*
027700010118     C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
027800000221     C*
027900000221     C                   add       1             x
028000010118     C                   eval      stploc1   = %trim(CPLLOC)
028100010119     C                   eval      stpcap1   = %trim(CPLCAP) + FLGDIS
028200010118     C                   eval      stpprv1   = '('+CPCPRV+')'
028300010118     C                   eval      s_riga(x) = stpriga1
028400000221     C*
028500000221     C                   ENDIF
028600000221     C*
028700000221     C                   ENDSR
028800010118     C*------------------------------------------------------------------------*
028900010118     C* MEMCAP - MEMORIZZA DATI X STAMPA RELATIVI A CAP DIS/ISO SENZA LOC DIS/ISO
029000010118     C*------------------------------------------------------------------------*
029100010118     C     MEMCAP        BEGSR
029200010118     C*
029300010118     C                   add       1             x
029400010118     C                   eval      stploc1   = *blanks
029500010118     C                   eval      stpcap1   = %trim(CPCCAP)
029600010118     C                   eval      stpprv1   = '('+CPCPRV+')'
029700010118     C                   eval      s_riga(x) = stpriga1
029800010118     C*
029900010118     C                   ENDSR
030000970613     C*------------------------------------------------------------------------*
030100010119     C* STAMPA - ESEGUE MATERIALMENTE LA STAMPA
030200970613     C*------------------------------------------------------------------------*
030300000221     C     STAMPA        BEGSR
030400000221     C*
030500000221     C* STAMPA INNANZITUTTO LA TESTATA
030600000221     C                   SETOFF                                       01
030700000221     C                   WRITE     SI09TE                                       *TESTATA
030800010118     C*
030900010118     C* ORDINO LA SCHIERA DA STAMPARE
031000010118     C                   SORTA     s_riga
031100010118     C*
031200010118     C* A QUESTO PUNTO HO NECESSITA' DI CONOSCERE L'INDICE DEL PRIMO ELEMENTO PIENO
031300010118     C                   Z-ADD     1             X
031400010118     C                   DOW       X <= 5000
031500010118     C                   IF        s_riga(x) <> *blanks
031600010118     C                   Z-ADD     X             DEPIND            5 0
031700010118     C                   LEAVE
031800010118     C                   ENDIF
031900010118     C                   ADD       1             X
032000010118     C                   ENDDO
032100000221     C*
032200000221     C* CICLA SULLE SCHIERE E STAMPA LE RIGHE DI DETTAGLIO
032300000221     C*=====================
032400000221     C* NOTE: - IL VALORE "70" SOTTOINDICATO IDENTIFICA IL NUMERO DI RIGHE DI DETTAGLIO
032500000221     C*         CHE SI VUOLE STAMPARE IN 1 PAGINA
032600000221     C*       - IL VALORE "*X" SOTTOINDICATO IDENTIFICA IL NUMERO DI COLONNA OLTRE LA PRIMA
032700000221     C*=====================
032800010118     C                   Z-ADD     66            righe             5 0
032900010118     C                   Z-ADD     DEPIND        x
033000000221     C                   Z-ADD     *zeros        x2                5 0
033100000221     C                   Z-ADD     *zeros        x3                5 0
033200000222     C                   Z-ADD     *zeros        conta             5 0
033300010118     C                   Z-ADD     *zeros        giro              5 0
033400010118     C*
033500010725     C                   DOW       x <= 5000 and
033600010725     C                             x  > *zeros
033700000221     C                   eval      x2=x+(righe*1)                                  *2° COLONNA
033800010118     C                   eval      x3=x+(righe*2)                                  *3° COLONNA
033900010118     C                   clear                   SI09ST
034000010118     C                   if        x<=5000
034100010118     C                   movel(p)  s_riga(x)     stpriga1
034200010118     C                   movel     stpcap1       STCAP1
034300010118     C                   eval      STLOC1=%trim(stploc1)+stpprv1
034400010118     C                   endif
034500010118     C                   if        x2<=5000
034600010118     C                   movel(p)  s_riga(x2)    stpriga2
034700010118     C                   movel     stpcap2       STCAP2
034800010118     C                   eval      STLOC2=%trim(stploc2)+stpprv2
034900010118     C                   endif
035000010118     C                   if        x3<=5000
035100010118     C                   movel(p)  s_riga(x3)    stpriga3
035200010118     C                   movel     stpcap3       STCAP3
035300010118     C                   eval      STLOC3=%trim(stploc3)+stpprv3
035400010118     C                   endif
035500000222     C*
035600000222     C* VERIFICO N° EFETTIVO DI RIGHE STAMPATE
035700000222     C     conta         ifeq      righe
035800000222     C                   seton                                        01
035900000222     C                   z-add     *zeros        conta
036000000222     C                   endif
036100970613     C*
036200000221     C     *IN01         IFEQ      *ON
036300000221     C                   WRITE     SI09TE                                       *TESTATA
036400970613     C                   SETOFF                                       01
036500000221     C                   ENDIF
036600000222     C
036700010118     C* STAMPA DETTAGLIO SE ALMENO UNA DELLE TRE COLONNE E' VALORIZZATA
036800010118     C                   IF        STCAP1 <> *blanks OR
036900010118     C                             STCAP2 <> *blanks OR
037000010118     C                             STCAP3 <> *blanks
037100000221     C                   WRITE     SI09ST                                       *DETTAGLIO
037200010118     C* INCREMENTO IL CONTATORE DI STAMPA
037300010118     C                   ADD       1             conta
037400010118     C                   ENDIF
037500010118     C*
037600010118     C* INCREMENTO L'INDICE
037700010118     C                   add       1             x
037800000221     C*
037900000221     C* VERIFICO COSTANTEMENTE IL VALORE DELL'INDICE "x" PER EVITARE DI "SFUMARE"
038000000221     C* LE RIGHE DI STAMPA
038100000221     C* -    2 = n° di colonne oltre la prima per pagina
038200010118     C                   if        x>righe+(righe*3*giro)+DEPIND-1
038300000221     C                   eval      x=x+(righe*2)
038400000221     C                   add       1             giro
038500000221     C                   endif
038600000221     C*
038700000221     C                   ENDDO
038800000221     C*
038900970613     C                   ENDSR
039000970613     C*------------------------------------------------------------------------*
039100970613     C* CARTAB - CARICA LE TABELLE
039200970613     C*------------------------------------------------------------------------*
039300970613     C     CARTAB        BEGSR
039400970613     C*--------------------
039500970613     C* PROVINCE
039600970613     C*--------------------
039700000221     C                   Z-ADD     *ZEROS        I                 5 0
039800020906     C                   Z-ADD     1             KTBKUT
039900970613     C                   MOVEL     'PR'          KTBCOD
040000970613     C     KE2TAB        SETLL     TABEL00F
040100970613     C     KE2TAB        READE     TABEL00F                               99
040200970613DO  1C     *IN99         DOWEQ     *OFF
040300970613IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
040400970613     C                   ADD       1             I
040500970613     C                   MOVEL     TBLKEY        PRV(I)                         *CODICE
040600970613     C                   MOVEL     TBLUNI        DSPR
040700970613     C                   MOVEL     §PRDES        DPRV(I)                        *DESCRIZIONE
040800970613E   2C                   ENDIF
040900970613     C     KE2TAB        READE     TABEL00F                               99
041000970613E   1C                   ENDDO
041100970613     C*
041200970613     C                   ENDSR
041300970613     C*------------------------------------------------------------------------*
041400970613     C* *INZSR - OPERAZIONI INIZIALI
041500970613     C*------------------------------------------------------------------------*
041600970613     C     *INZSR        BEGSR
041700970613     C*
041800970613     C* RICEVIMENTO PARAMETRI
041900970613     C     *ENTRY        PLIST
042000970613     C                   PARM                    KPJBA
042100000223     C                   MOVEL     KPJBU         PARAM
042200000221     C*
042300000221     C* IN BASE AI PARAMETRI DI LANCIO RICHIESTI => IMPOSTA IL "TITOLO" DI STAMPA
042400000221     C* ---- SOLO LOCALITA' DISAGIATE ----
042500000221     C     PARLDI        IFEQ      'S'
042600000221     C     PARISO        ANDEQ     'N'
042700000221     C                   MOVEL     DES(1)        STATES
042800010119     C                   MOVEL     DES(4)        STANOT
042900000221     C                   ENDIF
043000000221     C* ---- SOLO ISOLE ----
043100000221     C     PARLDI        IFEQ      'N'
043200000221     C     PARISO        ANDEQ     'S'
043300000221     C                   MOVEL     DES(3)        STATES
043400010119     C                   MOVEL     DES(6)        STANOT
043500000221     C                   ENDIF
043600000221     C* ---- SIA LOCALITA' DISAGIATE CHE ISOLE ----
043700000221     C     PARLDI        IFEQ      'S'
043800000221     C     PARISO        ANDEQ     'S'
043900000221     C                   MOVEL     DES(2)        STATES
044000010119     C                   MOVEL     DES(5)        STANOT
044100000221     C                   ENDIF
044200970613     C*
044300000221     C* VARIABILI RIFERITE AL DATA BASE
044400000221     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
044500970613     C     *LIKE         DEFINE    TBLCOD        KTBCOD
044600970613     C     *LIKE         DEFINE    TBLKEY        KTBKEY
044700000221     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPL00F
044800000221     C     *LIKE         DEFINE    CPCNAR        KCCNAR
044900000221     C     *LIKE         DEFINE    CPCCAP        KCCCAP
045000970613     C*
045100970613     C* CHIAVI DI LETTURA
045200970613     C* TABEL00F - PARZIALE
045300970613     C     KE2TAB        KLIST
045400970613     C                   KFLD                    KTBKUT                         *UTENTE
045500970613     C                   KFLD                    KTBCOD                         *TABELLA
045600000221     C* AZCPC01L - COMPLETA
045700000221     C     KEYCPC        KLIST
045800000221     C                   KFLD                    KCCVER                         *VERSIONE
045900000221     C                   KFLD                    KCCNAR                         *NAZIONE
046000000221     C                   KFLD                    KCCCAP                         *PROVINCIA
046100970613     C*
046200970613     C* VARIABILI DI CONTROLLO
046300970613     C                   MOVEL     '0'           FINL00            1            *FINE PGM
046400970613     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
046500970613     C                   MOVEL     '1'           *IN01                          *OVERFLOW
046600010118     C                   MOVEL     '0'           FLGCAP            1
046700010119     C                   MOVEL     *BLANKS       FLGDIS            1
046800010118     C                   MOVEL     *BLANKS       DEPCAP            9
046900010118     C                   Z-ADD     *ZEROS        COUNTLOC          1 0
047000970613     C*
047100970613     C* REPERIMENTO DATA E ORA CORRENTE
047200970613     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
047300970613     C                   MOVEL     WN14          ORA               6 0          *ORA  (6)
047400970613     C                   MOVE      WN14          DATA              8 0          *DATA (8) IN G/M/AA
047500970613     C*
047600970613     C* CARICA LE TABELLE OCCORRENTI
047700970613     C                   EXSR      CARTAB
047800970613     C*
047900000221     C* OPERAZIONI INIZIALI SULLE SCHIERE (INIZIALIZZAZIONE ELEMENTI E INDICE)
048000010118     C                   CLEAR                   s_riga
048100000221     C                   Z-ADD     *zeros        x                 5 0
048200000221     C*
048300970613     C                   ENDSR
048400970613     C*------------------------------------------------------------------------*
048500000221**   DES - DESCRIZIONI DI TESTA
048600010119 *** STAMPA CAP/LOCALITA' DISAGIATE ***                     1
048700010119 ** STAMPA CAP/LOCALITA' DISAGIATE E ISOLE **               2
048800010119         *** STAMPA ISOLE ***                               3
048900010119 (* = tutto il CAP è DISAGIATO)                             4
049000010119 (* = tutto il CAP è DISAGIATO o ISOLA)                     5
049100010119 (* = tutto il CAP è ISOLA)                                 6
