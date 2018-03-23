000100000000     H*------------------------------------------------------------------------*
000200050318     H* STAMPA PICKUP   TIME - CREAZIONE FILE                                  *
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     FTABEL00F  IF   E           K DISK
000900020926     FTNTBE01L  UF   E           K DISK
001000970707     FAZCPC01L  IF   E           K DISK
001100980210     FAZCPL01L  IF   E           K DISK
001200010329     FAZCPT01L  IF   E           K DISK
001300051117     ***FWAPKU00F  UF A E           K DISK    USROPN
001400051117     fWAPKU00F  o    e             disk    usropn
001500000000     D*------------------------------------------------------------------------*
001600000000     D* SCHIERE
001700000000     D*------------------------------------------------------------------------*
001800000000     D*-------------------
001900000000     D* COMANDI CLP
002000000000     D*-------------------
002100020926     D CMD             S             80    DIM(1) CTDATA PERRCD(1)
002200051117      *-------------------
002300051117      * PROVINCIE
002400051117      *-------------------
002500051117     d PRV             s                   dim(600) inz like(PKUprv)            *PROVINCIA
002600051117     d Pcts            s                   dim(600) inz like(§PRcts)            *CODICE TASSAZIONE
002700051117     d Pfcr            s                   dim(600) inz like(PKUfcr)            *CAPOLUOGO DI REGION
002800051117     d Prap            s                   dim(600) inz like(PKUrap)            *REGIONE APPARTENENZ
002900051117     d Pcor            s                   dim(600) inz like(PKUcor)            *ORDINAMENTO
003000000000     D*------------------------------------------------------------------------*
003100000000     D* INPUT
003200000000     D*------------------------------------------------------------------------*
003300980210     D*-------------------
003400980210     D* DS REPERIMENTO DATI UTENTE
003500980210     D*-------------------
003600020919     D TIBS34DS      E DS                                                       *Profili utente
003700020919     D DDATIUTE      E DS                                                       *Dati utente
003800020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
003900971020     D*------------------
004000960709     D* DS PROVINCIE
004100960709     D*-------------------
004200960709     D DSPR          E DS
004300000000     D*-------------------
004400000000     D* DS CODICI TASSAZIONE
004500000000     D*-------------------
004600000000     D DSCT          E DS
004700020926     D*-------------------
004800020926     D* DS STATISTICHE DATI DIVERSI
004900020926     D*-------------------
005000020926     D DSDD          E DS
005100980210     D*------------------
005200980210     D* DS "XSRDA8" - CONTROLLA DATA (8)
005300980210     D*------------------
005400980210     D WLBDA8          DS
005500980210     D  G08DAT                 1      8  0
005600980210     D  G08INV                 9     16  0
005700980210     D  G08ERR                17     17
005800980210     D  G08TGI                18     22  0
005900000000     D*-------------------
006000000000     D* DS PARAMETRI DI LANCIO
006100000000     D*-------------------
006200980210     D PARAM           DS
006300980210     D  PARLNP                 1      3
006400980210     D  PARTEM                 4      4
006500980210     D  PARTSP                 5      5
006600980210     D  PARISO                 6      6
006700980210     D  PARDIS                 7      7
006800980210     D  PARSSA                 8      8
006900980210     D  PARND1                 9     83
007000980210     D  PARND2                84    158
007100980210     D  PARND3               159    233
007200010509     D  PARORE               234    235  0
007300010509     D  PARRD1               236    310
007400010509     D  PARRD2               311    385
007500010509     D  PARRD3               386    460
007600011016     D  PARPRI               461    461
007700980210     D*-------------------
007800980210     D* ARCHITETTURA
007900980210     D*-------------------
008000980210     D KPJBA         E DS
008100051117      *
008200051117      * V A R I A B I L I
008300051117      *
008400051117     d XX              s              3  0 inz
008500000000     C*------------------------------------------------------------------------*
008600000000     C* MAIN LINE
008700000000     C*------------------------------------------------------------------------*
008800051117     C*
008900051117     C* CARICA LE TABELLE OCCORRENTI
009000051117     C                   EXSR      CARTAB
009100020926     C*
009200050318     C* PULISCE E RICREA IL FILE SOLO SE:
009300020926     C* data corrente > data ultima creazione workfile
009400020926IF  0c     DATCOR        IFGT      §SDDPKU
009500020926     C*
009600050318     C* OPERAZIONI INIZIALI APERTURA FILE
009700020926     C                   EXSR      OPNFIL
009800000000     C*
009900980216     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA -CAP-
010000980216IF  1C     $ERR          IFEQ      '0'                                          *NO USCITA
010100000000     C                   EXSR      SETFIL
010200000000     C*
010300000000     C* CICLO FINO A FINE FILE
010400000000DO  2C     $FINE         DOWEQ     '0'
010500980216     C*
010600980216     C* OPERAZIONI PER NUOVO CAP
010700980216     C                   EXSR      NEWCAP
010800980216     C*
010900980216     C* CICLO FINO A ROTTURA CAP
011000980216DO  3C     $FINE         DOWEQ     '0'
011100980216     C     CPCNAR        ANDEQ     DEPNAR
011200980216     C     CPCCAP        ANDEQ     DEPCAP
011300980302     C*
011400980211     C* SCRIVE IL RECORD DEL WORKFILE
011500980216     C                   MOVEL     *BLANKS       WLOC
011600980216     C                   MOVEL     CPCISO        WISO
011700010329     C                   MOVEL     *ALL'9'       WFI2                           *CODICE TEMPO RITIRO
011800010329     C                   MOVE      CPCFI2        WFI2                           *CODICE TEMPO RITIRO
011900980219     C                   MOVEL     'N'           WDLV
012000010402     C                   Z-ADD     CPCLNA        DEPLNP
012100010329     C                   EXSR      CREPKU
012200980216     C*
012300980216     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA -LOCALITA'-
012400980216     C                   EXSR      SETCPL
012500980216     C*
012600980216     C* CICLO FINO A FINE FILE
012700980216DO  4C     $FINL         DOWEQ     '0'
012800980216     C*
012900980216     C* SCRIVE IL RECORD DEL WORKFILE
013000010329     C                   MOVEL     'L'           $PKU
013100980216     C                   MOVEL     CPLLOC        WLOC
013200980220     C                   MOVEL     CPLDLV        WDLV
013300980302     C*
013400010329     C* EFFETTUA CONSIDERAZOINI SUL LIVELLO DI GESTIONE DELLA LOCALITA'
013500011016     C     CPLLIV        IFEQ      'L'                                          *LIVELLO LOCALITA'
013600010402     C                   MOVEL     CPLISO        WISO                           *SE CITTA/PROV/ISOLA
013700010329     C                   MOVEL     *ALL'9'       WFI2                           *CODICE TEMPO RITIRO
013800010329     C                   MOVE      CPLFI2        WFI2                           *CODICE TEMPO RITIRO
013900010402     C                   MOVEL     CPLDLV        WDLV                           *STAMPARE IN DELIVER
014000010402     C                   Z-ADD     CPLLNA        DEPLNP
014100011016     C                   ELSE                                                   *LIVELLO CAP
014200011016     C                   MOVEL     CPCISO        WISO                           *DATI CAP
014300011016     C                   MOVEL     *ALL'9'       WFI2                           *CODICE TEMPO RITIRO
014400011016     C                   MOVE      CPCFI2        WFI2                           *CODICE TEMPO RITIRO
014500011016     C                   Z-ADD     CPCLNA        DEPLNP
014600011016     C                   ENDIF
014700010329     C                   EXSR      CREPKU
014800980216     C*
014900980216     C* LETTURA RECORD SUCCESSIVO -LOCALITA'-
015000980216     C                   EXSR      LETCPL
015100980216E   4C                   ENDDO                                                  *FINE -LOCALITA'-
015200000000     C*
015300980216     C* LETTURA RECORD SUCCESSIVO -CAP-
015400000000     C                   EXSR      LETFIL
015500980216E   3C                   ENDDO                                                  *ROTTURA CAP
015600980216E   2C                   ENDDO                                                  *FINE -CAP-
015700000000E   1C                   ENDIF
015800000000     C*
015900000000     C* OPERAZIONI DI CHIUSURA
016000971020     C                   EXSR      FINSR
016100020926E   0C                   ENDIF
016200000000     C*
016300000000     C                   SETON                                        LR
016400000000     C*------------------------------------------------------------------------*
016500980216     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE -CAP-
016600000000     C*------------------------------------------------------------------------*
016700000000     C     SETFIL        BEGSR
016800000000     C*
016900970707     C                   Z-ADD     P96VER        KCCVER
017000970707     C     KEYCPC        SETLL     AZCPC01L                           99
017100010402     C     *IN99         IFEQ      *ON
017200000000     C                   MOVEL     '1'           $FINE                          *FINE PGM
017300010402     C                   ELSE
017400010402     C                   EXSR      LETFIL                                       *LETTURA RECORD
017500010402     C                   ENDIF
017600000000     C*
017700000000     C                   ENDSR
017800000000     C*------------------------------------------------------------------------*
017900980216     C* LETFIL - LETTURA PROSSIMO RECORD -CAP-
018000000000     C*------------------------------------------------------------------------*
018100000000     C     LETFIL        BEGSR
018200000000     C*
018300000000     C* LEGGE FINO A:
018400000000     C*     - FINE FILE
018500000000     C*     - TROVATO RECORD VALIDO
018600000000     C                   MOVEL     'N'           WRECOK
018700010402     C     $FINE         DOWEQ     '0'
018800000000     C     WRECOK        ANDEQ     'N'
018900970707     C     KEYCPC        READE     AZCPC01L                               99
019000010402     C     *IN99         IFEQ      *ON
019100000000     C                   MOVEL     '1'           $FINE                          *FINE PGM
019200010402     C                   ELSE
019300000000     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
019400010402     C                   ENDIF
019500010402     C                   ENDDO
019600960701     C*
019700000000     C                   ENDSR
019800000000     C*------------------------------------------------------------------------*
019900980216     C* CHKREC - CONTROLLA VALIDITA' RECORD -CAP-
020000000000     C*------------------------------------------------------------------------*
020100000000     C     CHKREC        BEGSR
020200000000     C*
020300000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
020400000000     C*
020500000000     C* CAP ESTERO, ESCLUDE
020600010402     C     CPCNAR        IFNE      *BLANKS
020700000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
020800010402     C                   ENDIF
020900000000     C*
021000000000     C* CAP FITTIZIO, ESCLUDE
021100010402     C     CPCFIT        IFEQ      'S'
021200010402     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
021300010402     C                   ENDIF
021400980217     C*
021500980217     C* CAP OBSOLETO, ESCLUDE
021600010402     C     CPCFI1        IFEQ      'O'
021700980217     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
021800010402     C                   ENDIF
021900000000     C*
022000000000     C                   ENDSR
022100980216     C*------------------------------------------------------------------------*
022200980216     C* NEWCAP - OPERAZIONI PER NUOVO CAP
022300980216     C*------------------------------------------------------------------------*
022400980216     C     NEWCAP        BEGSR
022500980216     C*
022600980216     C* MEMORIZZA IL NUOVO CAP -E ALCUNI SUOI DATI- IN UN DEPOSITO
022700980216     C                   MOVEL     CPCNAR        DEPNAR                         *NAZIONE
022800980216     C                   MOVEL     CPCCAP        DEPCAP                         *CAP
022900980216     C                   MOVEL     CPCPRV        DEPPRV                         *PROVINCIA
023000980216     C*
023100980216     C* RECUPERA I DATI DALLA PROVINCIA DEL CAP
023200980216     C                   Z-ADD     1             I
023300980216     C                   SETOFF                                           28
023400010402     C     CPCPRV        LOOKUP    PRV(I)                                 28
023500010402     C     *IN28         IFEQ      *ON                                          *TROVATO
023600980216     C                   MOVEL     PFCR(I)       DEPFCR                         *CAPOLUOGO DI REGION
023700980216     C                   MOVEL     PRAP(I)       DEPRAP                         *REGIONE APPARTENENZ
023800980216     C                   MOVEL     PCOR(I)       DEPCOR                         *CODICE ORDINAMENTO
023900010402     C                   ELSE                                                   *NON TROVATO
024000980216     C                   MOVEL     *BLANKS       DEPFCR
024100980216     C                   MOVEL     *BLANKS       DEPRAP
024200010402     C                   MOVEL     *BLANKS       DEPCOR
024300010402     C                   ENDIF
024400980216     C*
024500980216     C                   ENDSR
024600980216     C*------------------------------------------------------------------------*
024700980216     C* SETCPL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE -LOCALITA'-
024800980216     C*------------------------------------------------------------------------*
024900980216     C     SETCPL        BEGSR
025000980216     C*
025100980216     C* REIMPOSTA LE VARIABILI DI LAVORO
025200980216     C                   MOVEL     '0'           $FINL
025300980216     C*
025400010329     C                   Z-ADD     P96VER        KCLVER
025500980216     C                   MOVEL     DEPNAR        KCLNAR
025600980216     C                   MOVEL     DEPCAP        KCLCAP
025700980216     C     KEYCPL        SETLL     AZCPL01L                           98
025800010402     C     *IN98         IFEQ      *ON
025900980216     C                   MOVEL     '1'           $FINL                          *FINE PGM
026000010402     C                   ELSE
026100980216     C                   EXSR      LETCPL                                       *LETTURA RECORD
026200010402     C                   ENDIF
026300980216     C*
026400980216     C                   ENDSR
026500980216     C*------------------------------------------------------------------------*
026600980216     C* LETCPL - LETTURA PROSSIMO RECORD -LOCALITA'-
026700980216     C*------------------------------------------------------------------------*
026800980216     C     LETCPL        BEGSR
026900980216     C*
027000980216     C* LEGGE FINO A:
027100980216     C*     - FINE FILE
027200980216     C*     - TROVATO RECORD VALIDO
027300980216     C                   MOVEL     'N'           WRECOK
027400010402     C     $FINL         DOWEQ     '0'
027500980216     C     WRECOK        ANDEQ     'N'
027600980216     C     KEYCPL        READE     AZCPL01L                               98
027700010402     C     *IN98         IFEQ      *ON
027800980216     C                   MOVEL     '1'           $FINL                          *FINE PGM
027900010402     C                   ELSE
028000980216     C                   EXSR      CHKCPL                                       *CONTROLLA RECORD
028100010402     C                   ENDIF
028200010402     C                   ENDDO
028300980216     C*
028400980216     C                   ENDSR
028500980216     C*------------------------------------------------------------------------*
028600980216     C* CHKCPL - CONTROLLA VALIDITA' RECORD -LOCALITA'-
028700980216     C*------------------------------------------------------------------------*
028800980216     C     CHKCPL        BEGSR
028900980216     C*
029000980216     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
029100980216     C*
029200980217     C* LOCALITA' OBSOLETA, ESCLUDE
029300010402     C     CPLFI1        IFEQ      'O'
029400980217     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
029500010402     C                   ENDIF
029600980216     C*
029700980216     C                   ENDSR
029800980216     C*------------------------------------------------------------------------*
029900010329     C* CREPKU - CREA IL RECORD SUL WORKFILE
030000980216     C*------------------------------------------------------------------------*
030100010329     C     CREPKU        BEGSR
030200980216     C*
030300050318     C                   CLEAR                   WAPKU000
030400980216     C*---
030500980216     C* DATI DEDOTTI
030600980216     C*---
030700010402     C                   MOVEL     DEPLNP        PKULNP                         *LINEA PARTENZA
030800010329     C                   MOVEL     DEPFCR        PKUFCR                         *CAPOLUOGO DI REGION
030900010329     C                   MOVEL     DEPRAP        PKURAP                         *REGIONE APPARTENENZ
031000010329     C                   MOVEL     DEPCOR        PKUCOR                         *CODICE ORDINAMENTO
031100980216     C*---
031200980216     C* DATI CAPPARIO
031300980216     C*---
031400010329     C                   MOVEL     DEPNAR        PKUNAR                         *NAZIONE
031500010329     C                   MOVEL     DEPCAP        PKUCAP                         *CAP
031600010329     C                   MOVEL     DEPPRV        PKUPRV                         *PROVINCIA
031700010329     C                   MOVEL     WLOC          PKULOC                         *LOCALITA'
031800010329     C                   MOVEL     WISO          PKUISO                         *INOLTRO
031900010329     C                   MOVEL     WDLV          PKUDLV                         *STAMPA LOCALITA'
032000980216     C*---
032100010402     C* REPERISCE TEMPI DI RITIRO
032200980216     C*---
032300010329     C*
032400010329     C* CORRIERE
032500010329     C                   MOVEL     'C'           PKUTSP                         *TIPO SERVIZIO
032600010329     C                   MOVEL     'C'           KCTTSP                         *TIPO SERVIZIO
032700010329     C                   Z-ADD     WFI2          KCTTTC                         *CODICE TIPO TEMPO
032800010329     C                   Z-ADD     P96VER        KCTVER
032900010329     C     KEYCPT        CHAIN     AZCPT01L                           55
033000010402     C     *IN55         IFEQ      *OFF
033100010402     C                   Z-ADD     CPT0IN        PKURIM
033200010402     C                   Z-ADD     CPT1IN        PKURIP
033300010402     C                   Z-ADD     CPT0CN        PKURCM
033400010402     C                   Z-ADD     CPT1CN        PKURCP
033500050318     C                   WRITE     WAPKU000                                     I N S E R I M E N T
033600010402     C                   ENDIF
033700010402     C*
033800010402     C* ESPRESSO
033900010402     C                   MOVEL     'E'           PKUTSP                         *TIPO SERVIZIO
034000010402     C                   MOVEL     'E'           KCTTSP                         *TIPO SERVIZIO
034100010402     C                   Z-ADD     WFI2          KCTTTC                         *CODICE TIPO TEMPO
034200010402     C                   Z-ADD     P96VER        KCTVER
034300010402     C     KEYCPT        CHAIN     AZCPT01L                           55
034400010402     C     *IN55         IFEQ      *OFF
034500010402     C                   Z-ADD     CPT0IN        PKURIM
034600010402     C                   Z-ADD     CPT1IN        PKURIP
034700010402     C                   Z-ADD     CPT0CN        PKURCM
034800010402     C                   Z-ADD     CPT1CN        PKURCP
034900050318     C                   WRITE     WAPKU000                                     I N S E R I M E N T
035000010402     C                   ENDIF
035100980216     C*
035200980216     C                   ENDSR
035300000000     C*------------------------------------------------------------------------*
035400971020     C* FINSR - OPERAZIONI FINALI
035500000000     C*------------------------------------------------------------------------*
035600971020     C     FINSR         BEGSR
035700971020     C*
035800971020     C* CHIUDE I FILE DI LAVORO
035900050318     C                   CLOSE     WAPKU00F
036000020926     C*
036100020926     C* AGGIORNA LA DATA ULTIMA CREAZIONE WORKFILE CON LA DATA CORRENTE
036200020926     C                   MOVEL(P)  'SDD'         KBECOD
036300020926     C                   MOVEL(P)  '1'           KBEKE1
036400020926     C     KEYTBE        CHAIN     TNTBE01L
036500020926IF  1C                   IF        %found(TNTBE01L)
036600020926     C                   Z-ADD     DATCOR        §SDDPKU
036700020926     C                   MOVEL     DSDD          TBEUNI
036800020926     C                   UPDATE    TNTBE000
036900020926E   1C                   ENDIF
037000971020     C*
037100000000     C                   ENDSR
037200000000     C*------------------------------------------------------------------------*
037300050318     C* OPNFIL - OPERAZIONI INIZIALI APERTURA FILE
037400000000     C*------------------------------------------------------------------------*
037500000000     C     OPNFIL        BEGSR
037600020926     c*
037700050318     c* Pulisce il file
037800020926     C                   Z-ADD     80            LENGH            15 5
037900050318     C                   CALL      'QCAEXEC'                                    *PULISCE FILE
038000020926     C                   PARM                    CMD(1)
038100020926     C                   PARM                    LENGH
038200020926     C*
038300050318     C* Apre il file
038400050318     C                   OPEN      WAPKU00F                                     *APRE FILE
038500960701     C*
038600000000     C                   ENDSR
038700960709     C*------------------------------------------------------------------------*
038800980211     C* CARTAB - CARICA LE TABELLE OCCORRENTI
038900960709     C*------------------------------------------------------------------------*
039000960709     C     CARTAB        BEGSR
039100020926     C*---
039200020926     C* Dati statistiche
039300020926     C*---
039400020926     C                   CLEAR                   DSDD
039500020926     C                   MOVEL(P)  'SDD'         KBECOD
039600020926     C                   MOVEL(P)  '1'           KBEKE1
039700020926     C     KEYTBE        CHAIN(N)  TNTBE01L
039800020926     C                   IF        %found(TNTBE01L)
039900020926     C                   MOVEL     TBEUNI        DSDD
040000020926     C                   ENDIF
040100980211     C*---
040200980211     C* PROVINCIE
040300980211     C*---
040400980216     C                   Z-ADD     *ZEROS        I                 5 0
040500020919     C                   Z-ADD     1             KTBKUT
040600960709     C                   MOVEL     'PR'          KTBCOD
040700960709     C     KE2TAB        SETLL     TABEL00F
040800960709     C     KE2TAB        READE     TABEL00F                               99
040900010402     C     *IN99         DOWEQ     *OFF
041000010402     C     TBLFLG        IFEQ      *BLANKS
041100960709     C                   ADD       1             I
041200960709     C                   MOVEL     TBLUNI        DSPR
041300960709     C                   MOVEL     TBLKEY        PRV(I)                         *PROVINCIA
041400960709     C                   MOVEL     §PRCTS        PCTS(I)                        *CODICE TASSAZIONE
041500051117     c                   movel     §PRfcr        Pfcr(I)                        *Capoluogo Regione
041600010402     C                   ENDIF
041700960709     C     KE2TAB        READE     TABEL00F                               99
041800010402     C                   ENDDO
041900051117      *
042000051117      * Imposta il progressivo per le province (diverse) con lo stesso
042100051117      *   codice tassazione (da cui viene reperito il cod. ordinamento
042200051117      *   - che NON può essere uguale per due province diverse:
042300051117      *   ha causato confusione nel reperimento dei tempi di consegna
042400051117      *   e/o nell'elenco delle località)
042500051117     c                   clear                   xx
042600051117do  1c                   DOW       xx       <  %elem(PRV)
042700051117     c                   add       1             xx
042800051117if  2c                   if        Prv(xx)  =  *blanks
042900051117     c                   leave
043000051117e   2c                   endif
043100051117      * - scarto le province per le quali è già stata impostata la
043200051117      *   "sigla" finale della provincia per il codice ordinamento
043300051117     c                   if        Pcor(xx) <> *blanks
043400051117     c                   iter
043500051117     c                   endif
043600051117      * - cerco le altre province con lo stesso codice tassazione
043700051117     c                   eval      I        =  xx
043800051117do  2c                   DOW       I        <  %elem(PRV)
043900051117     c                   add       1             I
044000051117     c                   eval      *in28    =  *off
044100051117     c     Pcts(xx)      lookup    Pcts(I)                                28
044200051117if  3c                   if        *in28    =  *off
044300051117     c                   leave
044400051117e   3c                   endif
044500051117      * - e per ognuna di esse (tranne che per il capoluogo regionale)
044600051117      *   memorizzo la provincia stessa - da posporre al codice
044700051117      *   ordinamento
044800051117      *   Esempio finale: "CCA  " = Milano    "CCALO" = Lodi
044900051117      *                   "HHCFO" = Forlì     "HHCRN" = Rimini
045000051117      *   (ora memorizzo "   LO", "   FO", "   RN")
045100051117if  3c                   if        Pfcr(xx) =  *blanks
045200051117     c                   move      Prv(xx)       Pcor(xx)
045300051117e   3c                   endif
045400051117if  3c                   if        Pfcr(I)  =  *blanks
045500051117     c                   move      Prv(I)        Pcor(I)
045600051117e   3c                   endif
045700051117e   2c                   ENDDO
045800051117e   1c                   ENDDO
045900980211     C*---
046000960709     C* DATI CODICI TASSAZIONE
046100980211     C*---
046200020919     C                   Z-ADD     1             KTBKUT
046300960709     C                   MOVEL     'CT'          KTBCOD
046400041213     C     1             DO        600           I
046500010402     C     PRV(I)        IFNE      *BLANKS
046600960709     C                   MOVEL(P)  PCTS(I)       KTBKEY
046700010402     C     KEYTAB        CHAIN     TABEL00F                           99
046800010402     C     *IN99         IFEQ      *OFF
046900960709     C     TBLFLG        ANDEQ     *BLANKS
047000960709     C                   MOVEL     TBLUNI        DSCT
047100960709     C                   MOVEL     §CTRAP        PRAP(I)                        *REGIONE APPARTENENZ
047200960709     C                   MOVEL     §CTCOR        PCOR(I)                        *CODICE ORDINAMENTO
047300010402     C                   ENDIF
047400010402     C                   ENDIF
047500010402     C                   ENDDO
047600960709     C*
047700960709     C                   ENDSR
047800020919     C*--------------------------------------------------------------------------------------------*
047900020919     C* REPERISCE I DATI UTENTE
048000020919     C*--------------------------------------------------------------------------------------------*
048100020919     C     REPDATIUTE    BEGSR
048200020919     C*
048300020919     C* INIZIALIZZA VARIABILI DI WRK
048400020919     C                   CLEAR                   TIBS34DS
048500020919     C                   CLEAR                   AZUTEDS
048600020919     C                   CLEAR                   DDATIUTE
048700020919     C*
048800020919     C     *DTAARA       DEFINE    §azute        azuteds
048900020919     C     *DTAARA       DEFINE    §datiute      ddatiute
049000020919     C                   IN(E)     *DTAARA
049100020919if  1C                   IF        %Error
049200020919     c                   EVAL      I34Tla = 'L'
049300020919     C                   CALL      'TIBS34R'
049400020919     C                   PARM                    Tibs34Ds
049500020919     C                   IN        *DTAARA
049600020919e   1C                   ENDIF
049700020919     C*
049800020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
049900020919     C                   MOVEL(P)  RSUT          DSFIRS           20
050000020919     C*
050100020919     C                   ENDSR
050200000000     C*------------------------------------------------------------------------*
050300000000     C* *INZSR - OPERAZIONI INIZIALI
050400000000     C*------------------------------------------------------------------------*
050500000000     C     *INZSR        BEGSR
050600980211     C*---
050700000000     C* RICEVIMENTO PARAMETRI
050800980211     C*---
050900000000     C     *ENTRY        PLIST
051000000000     C                   PARM                    KPJBA
051100010509     C*---
051200010509     C* DEFINISCO LA *LDA
051300010509     C*---
051400010509     C     *DTAARA       DEFINE    *LDA          PARAM
051500010509     C                   IN        PARAM
051600980211     C*---
051700000000     C* VARIABILI RIFERITE AL DATABASE
051800980211     C*---
051900000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
052000000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
052100000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
052200020926     C     *LIKE         DEFINE    TBECOD        KBECOD                         *TNTBE01L
052300020926     C     *LIKE         DEFINE    TBEKE1        KBEKE1
052400980211     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
052500980216     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL00F
052600980216     C     *LIKE         DEFINE    CPLNAR        KCLNAR
052700980216     C     *LIKE         DEFINE    CPLCAP        KCLCAP
052800010329     C     *LIKE         DEFINE    CPTVER        KCTVER                         *AZCPT00F
052900010329     C     *LIKE         DEFINE    CPTTTC        KCTTTC
053000010329     C     *LIKE         DEFINE    CPTTSP        KCTTSP
053100010402     C     *LIKE         DEFINE    CPCLNA        DEPLNP                         *DEPOSITI
053200980216     C     *LIKE         DEFINE    CPCNAR        DEPNAR
053300980216     C     *LIKE         DEFINE    CPCCAP        DEPCAP
053400980216     C     *LIKE         DEFINE    CPCPRV        DEPPRV
053500010329     C     *LIKE         DEFINE    PKUFCR        DEPFCR
053600010329     C     *LIKE         DEFINE    PKURAP        DEPRAP
053700010329     C     *LIKE         DEFINE    PKUCOR        DEPCOR
053800980216     C     *LIKE         DEFINE    CPLLOC        WLOC                           *DI LAVORO
053900980216     C     *LIKE         DEFINE    CPCISO        WISO
054000980216     C     *LIKE         DEFINE    CPCLNA        WLNA
054100980219     C     *LIKE         DEFINE    CPLDLV        WDLV
054200980302     C     *LIKE         DEFINE    CPCLNA        WILNA
054300980302     C     *LIKE         DEFINE    CPCLOL        WILOL
054400980302     C     *LIKE         DEFINE    CPCLNA        WOLIN
054500980211     C*---
054600000000     C* CHIAVI DI LETTURA
054700980211     C*---
054800980211     C     KEYTAB        KLIST                                                  *TABEL00F
054900980211     C                   KFLD                    KTBKUT                          -UTENTE
055000980211     C                   KFLD                    KTBCOD                          -TABELLA
055100980211     C                   KFLD                    KTBKEY                          -CHIAVE
055200980211     C     KE2TAB        KLIST                                                  *TABEL00F
055300980211     C                   KFLD                    KTBKUT                          -UTENTE
055400980211     C                   KFLD                    KTBCOD                          -TEBELLA
055500020926     C     KEYTBE        KLIST                                                  *TNTBE01L
055600020926     C                   KFLD                    KBECOD                          -TABELLA
055700020926     C                   KFLD                    KBEKE1                          -CHIAVE
055800980211     C     KEYCPC        KLIST                                                  *AZCPC00F
055900980211     C                   KFLD                    KCCVER                          -VERSIONE
056000980211     C     KEYCPL        KLIST                                                  *AZCPL00F
056100980211     C                   KFLD                    KCLVER                          -VERSIONE
056200980216     C                   KFLD                    KCLNAR                          -NAZIONE
056300980216     C                   KFLD                    KCLCAP                          -CAP
056400010329     C     KEYCPT        KLIST                                                  *AZCPT00F
056500010329     C                   KFLD                    KCTVER                          -VERSIONE
056600010329     C                   KFLD                    KCTTTC                          -CODICE TEMPO
056700010329     C                   KFLD                    KCTTSP                          -TIPO SERVIZIO
056800980211     C*---
056900010329     C* VARIABILI DI WRK
057000980211     C*---
057100980216     C                   MOVEL     '0'           $ERR              1            *ERRORE GENERICO
057200980216     C                   MOVEL     '0'           $FINE             1            *FINE LETTURA CAP
057300980216     C                   MOVEL     '0'           $FINL             1            *FINE LETTURA LOCALI
057400980220     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
057500050318     C                   MOVEL     *BLANKS       $PKU              1            *CREAZIONE WAPKU (C/
057600010329     C                   Z-ADD     *ZEROS        WFI2              5 0          *DEPOSITO CODICE TIP
057700020926     C*
057800020926     C* CALCOLA LA DATA CORRENTE -DAL JOB-
057900020926     C                   MOVE      *DATE         N8                8 0          *DATA JOB IN G/M/AA
058000020926     C                   Z-ADD     N8            G08DAT
058100020926     C                   Z-ADD     *ZEROS        G08INV
058200020926     C                   MOVEL     '0'           G08ERR
058300020926     C                   CALL      'XSRDA8'
058400020926     C                   PARM                    WLBDA8
058500020926     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA JOB IN AA/M/G
058600000000     C*
058700000000     C* REPERISCE I DATI UTENTE
058800020919     C                   EXSR      REPDATIUTE
058900020926     C*
059000020926     C* FILIALE --> REPERISCE L'ULTIMA VERSIONE DEL CAPPARIO
059100020926     C* SEDE    --> PEZZA: REPERISCE LA VERSIONE ALLA DATA DEL JOB -COSI' LUCIANO LA PUO' CAMBIARE-
059200020926IF  1C     DUTLPO        IFNE      'S'                                          F I L I A L E
059300020926     C                   Z-ADD     *ZEROS        P96DRI            8 0
059400020926X   1C                   ELSE                                                   S E D E
059500020926     C                   Z-ADD     DATCOR        P96DRI            8 0
059600020926E   1C                   ENDIF
059700020926     C                   Z-ADD     *ZEROS        P96VER            5 0
059800020926     C                   CALL      'TISI96R'
059900020926     C                   PARM                    P96DRI
060000020926     C                   PARM                    P96VER
060100960709     C*
060200000000     C                   ENDSR
060300000000     C*------------------------------------------------------------------------*
060400000000**   CMD -COMANDI CLP-
060500050318CLRPFM FILE(WAPKU00F)
