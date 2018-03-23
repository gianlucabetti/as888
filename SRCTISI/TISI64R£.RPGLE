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
001300050318     FWAPKU00F  UF A E           K DISK    USROPN
001400000000     D*------------------------------------------------------------------------*
001500000000     D* SCHIERE
001600000000     D*------------------------------------------------------------------------*
001700000000     D*-------------------
001800000000     D* COMANDI CLP
001900000000     D*-------------------
002000020926     D CMD             S             80    DIM(1) CTDATA PERRCD(1)
002100960709     D*-------------------
002200980216     D* PROVINCIE
002300960709     D*-------------------
002400041213     D PRV             S              2    DIM(600)                             *PROVINCIA
002500041213     D PCTS            S              2    DIM(600)                             *CODICE TASSAZIONE
002600041213     D PFCR            S              1    DIM(600)                             *CAPOLUOGO DI REGION
002700041213     D PRAP            S              2    DIM(600)                             *REGIONE APPARTENENZ
002800041213     D PCOR            S              3    DIM(600)                             *ORDINAMENTO
002900000000     D*------------------------------------------------------------------------*
003000000000     D* INPUT
003100000000     D*------------------------------------------------------------------------*
003200980210     D*-------------------
003300980210     D* DS REPERIMENTO DATI UTENTE
003400980210     D*-------------------
003500020919     D TIBS34DS      E DS                                                       *Profili utente
003600020919     D DDATIUTE      E DS                                                       *Dati utente
003700020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
003800971020     D*------------------
003900960709     D* DS PROVINCIE
004000960709     D*-------------------
004100960709     D DSPR          E DS
004200000000     D*-------------------
004300000000     D* DS CODICI TASSAZIONE
004400000000     D*-------------------
004500000000     D DSCT          E DS
004600020926     D*-------------------
004700020926     D* DS STATISTICHE DATI DIVERSI
004800020926     D*-------------------
004900020926     D DSDD          E DS
005000980210     D*------------------
005100980210     D* DS "XSRDA8" - CONTROLLA DATA (8)
005200980210     D*------------------
005300980210     D WLBDA8          DS
005400980210     D  G08DAT                 1      8  0
005500980210     D  G08INV                 9     16  0
005600980210     D  G08ERR                17     17
005700980210     D  G08TGI                18     22  0
005800000000     D*-------------------
005900000000     D* DS PARAMETRI DI LANCIO
006000000000     D*-------------------
006100980210     D PARAM           DS
006200980210     D  PARLNP                 1      3
006300980210     D  PARTEM                 4      4
006400980210     D  PARTSP                 5      5
006500980210     D  PARISO                 6      6
006600980210     D  PARDIS                 7      7
006700980210     D  PARSSA                 8      8
006800980210     D  PARND1                 9     83
006900980210     D  PARND2                84    158
007000980210     D  PARND3               159    233
007100010509     D  PARORE               234    235  0
007200010509     D  PARRD1               236    310
007300010509     D  PARRD2               311    385
007400010509     D  PARRD3               386    460
007500011016     D  PARPRI               461    461
007600980210     D*-------------------
007700980210     D* ARCHITETTURA
007800980210     D*-------------------
007900980210     D KPJBA         E DS
008000000000     C*------------------------------------------------------------------------*
008100000000     C* MAIN LINE
008200000000     C*------------------------------------------------------------------------*
008300020926     C*
008400050318     C* PULISCE E RICREA IL FILE SOLO SE:
008500020926     C* data corrente > data ultima creazione workfile
008600020926IF  0c     DATCOR        IFGT      §SDDPKU
008700020926     C*
008800050318     C* OPERAZIONI INIZIALI APERTURA FILE
008900020926     C                   EXSR      OPNFIL
009000000000     C*
009100980216     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA -CAP-
009200980216IF  1C     $ERR          IFEQ      '0'                                          *NO USCITA
009300000000     C                   EXSR      SETFIL
009400000000     C*
009500000000     C* CICLO FINO A FINE FILE
009600000000DO  2C     $FINE         DOWEQ     '0'
009700980216     C*
009800980216     C* OPERAZIONI PER NUOVO CAP
009900980216     C                   EXSR      NEWCAP
010000980216     C*
010100980216     C* CICLO FINO A ROTTURA CAP
010200980216DO  3C     $FINE         DOWEQ     '0'
010300980216     C     CPCNAR        ANDEQ     DEPNAR
010400980216     C     CPCCAP        ANDEQ     DEPCAP
010500980302     C*
010600980211     C* SCRIVE IL RECORD DEL WORKFILE
010700980216     C                   MOVEL     *BLANKS       WLOC
010800980216     C                   MOVEL     CPCISO        WISO
010900010329     C                   MOVEL     *ALL'9'       WFI2                           *CODICE TEMPO RITIRO
011000010329     C                   MOVE      CPCFI2        WFI2                           *CODICE TEMPO RITIRO
011100980219     C                   MOVEL     'N'           WDLV
011200010402     C                   Z-ADD     CPCLNA        DEPLNP
011300010329     C                   EXSR      CREPKU
011400980216     C*
011500980216     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA -LOCALITA'-
011600980216     C                   EXSR      SETCPL
011700980216     C*
011800980216     C* CICLO FINO A FINE FILE
011900980216DO  4C     $FINL         DOWEQ     '0'
012000980216     C*
012100980216     C* SCRIVE IL RECORD DEL WORKFILE
012200010329     C                   MOVEL     'L'           $PKU
012300980216     C                   MOVEL     CPLLOC        WLOC
012400980220     C                   MOVEL     CPLDLV        WDLV
012500980302     C*
012600010329     C* EFFETTUA CONSIDERAZOINI SUL LIVELLO DI GESTIONE DELLA LOCALITA'
012700011016     C     CPLLIV        IFEQ      'L'                                          *LIVELLO LOCALITA'
012800010402     C                   MOVEL     CPLISO        WISO                           *SE CITTA/PROV/ISOLA
012900010329     C                   MOVEL     *ALL'9'       WFI2                           *CODICE TEMPO RITIRO
013000010329     C                   MOVE      CPLFI2        WFI2                           *CODICE TEMPO RITIRO
013100010402     C                   MOVEL     CPLDLV        WDLV                           *STAMPARE IN DELIVER
013200010402     C                   Z-ADD     CPLLNA        DEPLNP
013300011016     C                   ELSE                                                   *LIVELLO CAP
013400011016     C                   MOVEL     CPCISO        WISO                           *DATI CAP
013500011016     C                   MOVEL     *ALL'9'       WFI2                           *CODICE TEMPO RITIRO
013600011016     C                   MOVE      CPCFI2        WFI2                           *CODICE TEMPO RITIRO
013700011016     C                   Z-ADD     CPCLNA        DEPLNP
013800011016     C                   ENDIF
013900010329     C                   EXSR      CREPKU
014000980216     C*
014100980216     C* LETTURA RECORD SUCCESSIVO -LOCALITA'-
014200980216     C                   EXSR      LETCPL
014300980216E   4C                   ENDDO                                                  *FINE -LOCALITA'-
014400000000     C*
014500980216     C* LETTURA RECORD SUCCESSIVO -CAP-
014600000000     C                   EXSR      LETFIL
014700980216E   3C                   ENDDO                                                  *ROTTURA CAP
014800980216E   2C                   ENDDO                                                  *FINE -CAP-
014900000000E   1C                   ENDIF
015000000000     C*
015100000000     C* OPERAZIONI DI CHIUSURA
015200971020     C                   EXSR      FINSR
015300020926E   0C                   ENDIF
015400000000     C*
015500000000     C                   SETON                                        LR
015600000000     C*------------------------------------------------------------------------*
015700980216     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE -CAP-
015800000000     C*------------------------------------------------------------------------*
015900000000     C     SETFIL        BEGSR
016000000000     C*
016100970707     C                   Z-ADD     P96VER        KCCVER
016200970707     C     KEYCPC        SETLL     AZCPC01L                           99
016300010402     C     *IN99         IFEQ      *ON
016400000000     C                   MOVEL     '1'           $FINE                          *FINE PGM
016500010402     C                   ELSE
016600010402     C                   EXSR      LETFIL                                       *LETTURA RECORD
016700010402     C                   ENDIF
016800000000     C*
016900000000     C                   ENDSR
017000000000     C*------------------------------------------------------------------------*
017100980216     C* LETFIL - LETTURA PROSSIMO RECORD -CAP-
017200000000     C*------------------------------------------------------------------------*
017300000000     C     LETFIL        BEGSR
017400000000     C*
017500000000     C* LEGGE FINO A:
017600000000     C*     - FINE FILE
017700000000     C*     - TROVATO RECORD VALIDO
017800000000     C                   MOVEL     'N'           WRECOK
017900010402     C     $FINE         DOWEQ     '0'
018000000000     C     WRECOK        ANDEQ     'N'
018100970707     C     KEYCPC        READE     AZCPC01L                               99
018200010402     C     *IN99         IFEQ      *ON
018300000000     C                   MOVEL     '1'           $FINE                          *FINE PGM
018400010402     C                   ELSE
018500000000     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
018600010402     C                   ENDIF
018700010402     C                   ENDDO
018800960701     C*
018900000000     C                   ENDSR
019000000000     C*------------------------------------------------------------------------*
019100980216     C* CHKREC - CONTROLLA VALIDITA' RECORD -CAP-
019200000000     C*------------------------------------------------------------------------*
019300000000     C     CHKREC        BEGSR
019400000000     C*
019500000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
019600000000     C*
019700000000     C* CAP ESTERO, ESCLUDE
019800010402     C     CPCNAR        IFNE      *BLANKS
019900000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
020000010402     C                   ENDIF
020100000000     C*
020200000000     C* CAP FITTIZIO, ESCLUDE
020300010402     C     CPCFIT        IFEQ      'S'
020400010402     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
020500010402     C                   ENDIF
020600980217     C*
020700980217     C* CAP OBSOLETO, ESCLUDE
020800010402     C     CPCFI1        IFEQ      'O'
020900980217     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
021000010402     C                   ENDIF
021100000000     C*
021200000000     C                   ENDSR
021300980216     C*------------------------------------------------------------------------*
021400980216     C* NEWCAP - OPERAZIONI PER NUOVO CAP
021500980216     C*------------------------------------------------------------------------*
021600980216     C     NEWCAP        BEGSR
021700980216     C*
021800980216     C* MEMORIZZA IL NUOVO CAP -E ALCUNI SUOI DATI- IN UN DEPOSITO
021900980216     C                   MOVEL     CPCNAR        DEPNAR                         *NAZIONE
022000980216     C                   MOVEL     CPCCAP        DEPCAP                         *CAP
022100980216     C                   MOVEL     CPCPRV        DEPPRV                         *PROVINCIA
022200980216     C*
022300980216     C* RECUPERA I DATI DALLA PROVINCIA DEL CAP
022400980216     C                   Z-ADD     1             I
022500980216     C                   SETOFF                                           28
022600010402     C     CPCPRV        LOOKUP    PRV(I)                                 28
022700010402     C     *IN28         IFEQ      *ON                                          *TROVATO
022800980216     C                   MOVEL     PFCR(I)       DEPFCR                         *CAPOLUOGO DI REGION
022900980216     C                   MOVEL     PRAP(I)       DEPRAP                         *REGIONE APPARTENENZ
023000980216     C                   MOVEL     PCOR(I)       DEPCOR                         *CODICE ORDINAMENTO
023100010402     C                   ELSE                                                   *NON TROVATO
023200980216     C                   MOVEL     *BLANKS       DEPFCR
023300980216     C                   MOVEL     *BLANKS       DEPRAP
023400010402     C                   MOVEL     *BLANKS       DEPCOR
023500010402     C                   ENDIF
023600980216     C*
023700980216     C                   ENDSR
023800980216     C*------------------------------------------------------------------------*
023900980216     C* SETCPL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE -LOCALITA'-
024000980216     C*------------------------------------------------------------------------*
024100980216     C     SETCPL        BEGSR
024200980216     C*
024300980216     C* REIMPOSTA LE VARIABILI DI LAVORO
024400980216     C                   MOVEL     '0'           $FINL
024500980216     C*
024600010329     C                   Z-ADD     P96VER        KCLVER
024700980216     C                   MOVEL     DEPNAR        KCLNAR
024800980216     C                   MOVEL     DEPCAP        KCLCAP
024900980216     C     KEYCPL        SETLL     AZCPL01L                           98
025000010402     C     *IN98         IFEQ      *ON
025100980216     C                   MOVEL     '1'           $FINL                          *FINE PGM
025200010402     C                   ELSE
025300980216     C                   EXSR      LETCPL                                       *LETTURA RECORD
025400010402     C                   ENDIF
025500980216     C*
025600980216     C                   ENDSR
025700980216     C*------------------------------------------------------------------------*
025800980216     C* LETCPL - LETTURA PROSSIMO RECORD -LOCALITA'-
025900980216     C*------------------------------------------------------------------------*
026000980216     C     LETCPL        BEGSR
026100980216     C*
026200980216     C* LEGGE FINO A:
026300980216     C*     - FINE FILE
026400980216     C*     - TROVATO RECORD VALIDO
026500980216     C                   MOVEL     'N'           WRECOK
026600010402     C     $FINL         DOWEQ     '0'
026700980216     C     WRECOK        ANDEQ     'N'
026800980216     C     KEYCPL        READE     AZCPL01L                               98
026900010402     C     *IN98         IFEQ      *ON
027000980216     C                   MOVEL     '1'           $FINL                          *FINE PGM
027100010402     C                   ELSE
027200980216     C                   EXSR      CHKCPL                                       *CONTROLLA RECORD
027300010402     C                   ENDIF
027400010402     C                   ENDDO
027500980216     C*
027600980216     C                   ENDSR
027700980216     C*------------------------------------------------------------------------*
027800980216     C* CHKCPL - CONTROLLA VALIDITA' RECORD -LOCALITA'-
027900980216     C*------------------------------------------------------------------------*
028000980216     C     CHKCPL        BEGSR
028100980216     C*
028200980216     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
028300980216     C*
028400980217     C* LOCALITA' OBSOLETA, ESCLUDE
028500010402     C     CPLFI1        IFEQ      'O'
028600980217     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
028700010402     C                   ENDIF
028800980216     C*
028900980216     C                   ENDSR
029000980216     C*------------------------------------------------------------------------*
029100010329     C* CREPKU - CREA IL RECORD SUL WORKFILE
029200980216     C*------------------------------------------------------------------------*
029300010329     C     CREPKU        BEGSR
029400980216     C*
029500050318     C                   CLEAR                   WAPKU000
029600980216     C*---
029700980216     C* DATI DEDOTTI
029800980216     C*---
029900010402     C                   MOVEL     DEPLNP        PKULNP                         *LINEA PARTENZA
030000010329     C                   MOVEL     DEPFCR        PKUFCR                         *CAPOLUOGO DI REGION
030100010329     C                   MOVEL     DEPRAP        PKURAP                         *REGIONE APPARTENENZ
030200010329     C                   MOVEL     DEPCOR        PKUCOR                         *CODICE ORDINAMENTO
030300980216     C*---
030400980216     C* DATI CAPPARIO
030500980216     C*---
030600010329     C                   MOVEL     DEPNAR        PKUNAR                         *NAZIONE
030700010329     C                   MOVEL     DEPCAP        PKUCAP                         *CAP
030800010329     C                   MOVEL     DEPPRV        PKUPRV                         *PROVINCIA
030900010329     C                   MOVEL     WLOC          PKULOC                         *LOCALITA'
031000010329     C                   MOVEL     WISO          PKUISO                         *INOLTRO
031100010329     C                   MOVEL     WDLV          PKUDLV                         *STAMPA LOCALITA'
031200980216     C*---
031300010402     C* REPERISCE TEMPI DI RITIRO
031400980216     C*---
031500010329     C*
031600010329     C* CORRIERE
031700010329     C                   MOVEL     'C'           PKUTSP                         *TIPO SERVIZIO
031800010329     C                   MOVEL     'C'           KCTTSP                         *TIPO SERVIZIO
031900010329     C                   Z-ADD     WFI2          KCTTTC                         *CODICE TIPO TEMPO
032000010329     C                   Z-ADD     P96VER        KCTVER
032100010329     C     KEYCPT        CHAIN     AZCPT01L                           55
032200010402     C     *IN55         IFEQ      *OFF
032300010402     C                   Z-ADD     CPT0IN        PKURIM
032400010402     C                   Z-ADD     CPT1IN        PKURIP
032500010402     C                   Z-ADD     CPT0CN        PKURCM
032600010402     C                   Z-ADD     CPT1CN        PKURCP
032700050318     C                   WRITE     WAPKU000                                     I N S E R I M E N T
032800010402     C                   ENDIF
032900010402     C*
033000010402     C* ESPRESSO
033100010402     C                   MOVEL     'E'           PKUTSP                         *TIPO SERVIZIO
033200010402     C                   MOVEL     'E'           KCTTSP                         *TIPO SERVIZIO
033300010402     C                   Z-ADD     WFI2          KCTTTC                         *CODICE TIPO TEMPO
033400010402     C                   Z-ADD     P96VER        KCTVER
033500010402     C     KEYCPT        CHAIN     AZCPT01L                           55
033600010402     C     *IN55         IFEQ      *OFF
033700010402     C                   Z-ADD     CPT0IN        PKURIM
033800010402     C                   Z-ADD     CPT1IN        PKURIP
033900010402     C                   Z-ADD     CPT0CN        PKURCM
034000010402     C                   Z-ADD     CPT1CN        PKURCP
034100050318     C                   WRITE     WAPKU000                                     I N S E R I M E N T
034200010402     C                   ENDIF
034300980216     C*
034400980216     C                   ENDSR
034500000000     C*------------------------------------------------------------------------*
034600971020     C* FINSR - OPERAZIONI FINALI
034700000000     C*------------------------------------------------------------------------*
034800971020     C     FINSR         BEGSR
034900971020     C*
035000971020     C* CHIUDE I FILE DI LAVORO
035100050318     C                   CLOSE     WAPKU00F
035200020926     C*
035300020926     C* AGGIORNA LA DATA ULTIMA CREAZIONE WORKFILE CON LA DATA CORRENTE
035400020926     C                   MOVEL(P)  'SDD'         KBECOD
035500020926     C                   MOVEL(P)  '1'           KBEKE1
035600020926     C     KEYTBE        CHAIN     TNTBE01L
035700020926IF  1C                   IF        %found(TNTBE01L)
035800020926     C                   Z-ADD     DATCOR        §SDDPKU
035900020926     C                   MOVEL     DSDD          TBEUNI
036000020926     C                   UPDATE    TNTBE000
036100020926E   1C                   ENDIF
036200971020     C*
036300000000     C                   ENDSR
036400000000     C*------------------------------------------------------------------------*
036500050318     C* OPNFIL - OPERAZIONI INIZIALI APERTURA FILE
036600000000     C*------------------------------------------------------------------------*
036700000000     C     OPNFIL        BEGSR
036800020926     c*
036900050318     c* Pulisce il file
037000020926     C                   Z-ADD     80            LENGH            15 5
037100050318     C                   CALL      'QCAEXEC'                                    *PULISCE FILE
037200020926     C                   PARM                    CMD(1)
037300020926     C                   PARM                    LENGH
037400020926     C*
037500050318     C* Apre il file
037600050318     C                   OPEN      WAPKU00F                                     *APRE FILE
037700960701     C*
037800000000     C                   ENDSR
037900960709     C*------------------------------------------------------------------------*
038000980211     C* CARTAB - CARICA LE TABELLE OCCORRENTI
038100960709     C*------------------------------------------------------------------------*
038200960709     C     CARTAB        BEGSR
038300020926     C*---
038400020926     C* Dati statistiche
038500020926     C*---
038600020926     C                   CLEAR                   DSDD
038700020926     C                   MOVEL(P)  'SDD'         KBECOD
038800020926     C                   MOVEL(P)  '1'           KBEKE1
038900020926     C     KEYTBE        CHAIN(N)  TNTBE01L
039000020926     C                   IF        %found(TNTBE01L)
039100020926     C                   MOVEL     TBEUNI        DSDD
039200020926     C                   ENDIF
039300980211     C*---
039400980211     C* PROVINCIE
039500980211     C*---
039600980216     C                   Z-ADD     *ZEROS        I                 5 0
039700020919     C                   Z-ADD     1             KTBKUT
039800960709     C                   MOVEL     'PR'          KTBCOD
039900960709     C     KE2TAB        SETLL     TABEL00F
040000960709     C     KE2TAB        READE     TABEL00F                               99
040100010402     C     *IN99         DOWEQ     *OFF
040200010402     C     TBLFLG        IFEQ      *BLANKS
040300960709     C                   ADD       1             I
040400960709     C                   MOVEL     TBLUNI        DSPR
040500960709     C                   MOVEL     TBLKEY        PRV(I)                         *PROVINCIA
040600960709     C                   MOVEL     §PRCTS        PCTS(I)                        *CODICE TASSAZIONE
040700010402     C                   ENDIF
040800960709     C     KE2TAB        READE     TABEL00F                               99
040900010402     C                   ENDDO
041000980211     C*---
041100960709     C* DATI CODICI TASSAZIONE
041200980211     C*---
041300020919     C                   Z-ADD     1             KTBKUT
041400960709     C                   MOVEL     'CT'          KTBCOD
041500041213     C     1             DO        600           I
041600010402     C     PRV(I)        IFNE      *BLANKS
041700960709     C                   MOVEL(P)  PCTS(I)       KTBKEY
041800010402     C     KEYTAB        CHAIN     TABEL00F                           99
041900010402     C     *IN99         IFEQ      *OFF
042000960709     C     TBLFLG        ANDEQ     *BLANKS
042100960709     C                   MOVEL     TBLUNI        DSCT
042200960709     C                   MOVEL     §CTFCR        PFCR(I)                        *CAPOLUOGO DI REGION
042300960709     C                   MOVEL     §CTRAP        PRAP(I)                        *REGIONE APPARTENENZ
042400960709     C                   MOVEL     §CTCOR        PCOR(I)                        *CODICE ORDINAMENTO
042500010402     C                   ENDIF
042600010402     C                   ENDIF
042700010402     C                   ENDDO
042800960709     C*
042900960709     C                   ENDSR
043000020919     C*--------------------------------------------------------------------------------------------*
043100020919     C* REPERISCE I DATI UTENTE
043200020919     C*--------------------------------------------------------------------------------------------*
043300020919     C     REPDATIUTE    BEGSR
043400020919     C*
043500020919     C* INIZIALIZZA VARIABILI DI WRK
043600020919     C                   CLEAR                   TIBS34DS
043700020919     C                   CLEAR                   AZUTEDS
043800020919     C                   CLEAR                   DDATIUTE
043900020919     C*
044000020919     C     *DTAARA       DEFINE    §azute        azuteds
044100020919     C     *DTAARA       DEFINE    §datiute      ddatiute
044200020919     C                   IN(E)     *DTAARA
044300020919if  1C                   IF        %Error
044400020919     c                   EVAL      I34Tla = 'L'
044500020919     C                   CALL      'TIBS34R'
044600020919     C                   PARM                    Tibs34Ds
044700020919     C                   IN        *DTAARA
044800020919e   1C                   ENDIF
044900020919     C*
045000020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
045100020919     C                   MOVEL(P)  RSUT          DSFIRS           20
045200020919     C*
045300020919     C                   ENDSR
045400000000     C*------------------------------------------------------------------------*
045500000000     C* *INZSR - OPERAZIONI INIZIALI
045600000000     C*------------------------------------------------------------------------*
045700000000     C     *INZSR        BEGSR
045800980211     C*---
045900000000     C* RICEVIMENTO PARAMETRI
046000980211     C*---
046100000000     C     *ENTRY        PLIST
046200000000     C                   PARM                    KPJBA
046300010509     C*---
046400010509     C* DEFINISCO LA *LDA
046500010509     C*---
046600010509     C     *DTAARA       DEFINE    *LDA          PARAM
046700010509     C                   IN        PARAM
046800980211     C*---
046900000000     C* VARIABILI RIFERITE AL DATABASE
047000980211     C*---
047100000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
047200000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
047300000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
047400020926     C     *LIKE         DEFINE    TBECOD        KBECOD                         *TNTBE01L
047500020926     C     *LIKE         DEFINE    TBEKE1        KBEKE1
047600980211     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
047700980216     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL00F
047800980216     C     *LIKE         DEFINE    CPLNAR        KCLNAR
047900980216     C     *LIKE         DEFINE    CPLCAP        KCLCAP
048000010329     C     *LIKE         DEFINE    CPTVER        KCTVER                         *AZCPT00F
048100010329     C     *LIKE         DEFINE    CPTTTC        KCTTTC
048200010329     C     *LIKE         DEFINE    CPTTSP        KCTTSP
048300010402     C     *LIKE         DEFINE    CPCLNA        DEPLNP                         *DEPOSITI
048400980216     C     *LIKE         DEFINE    CPCNAR        DEPNAR
048500980216     C     *LIKE         DEFINE    CPCCAP        DEPCAP
048600980216     C     *LIKE         DEFINE    CPCPRV        DEPPRV
048700010329     C     *LIKE         DEFINE    PKUFCR        DEPFCR
048800010329     C     *LIKE         DEFINE    PKURAP        DEPRAP
048900010329     C     *LIKE         DEFINE    PKUCOR        DEPCOR
049000980216     C     *LIKE         DEFINE    CPLLOC        WLOC                           *DI LAVORO
049100980216     C     *LIKE         DEFINE    CPCISO        WISO
049200980216     C     *LIKE         DEFINE    CPCLNA        WLNA
049300980219     C     *LIKE         DEFINE    CPLDLV        WDLV
049400980302     C     *LIKE         DEFINE    CPCLNA        WILNA
049500980302     C     *LIKE         DEFINE    CPCLOL        WILOL
049600980302     C     *LIKE         DEFINE    CPCLNA        WOLIN
049700980211     C*---
049800000000     C* CHIAVI DI LETTURA
049900980211     C*---
050000980211     C     KEYTAB        KLIST                                                  *TABEL00F
050100980211     C                   KFLD                    KTBKUT                          -UTENTE
050200980211     C                   KFLD                    KTBCOD                          -TABELLA
050300980211     C                   KFLD                    KTBKEY                          -CHIAVE
050400980211     C     KE2TAB        KLIST                                                  *TABEL00F
050500980211     C                   KFLD                    KTBKUT                          -UTENTE
050600980211     C                   KFLD                    KTBCOD                          -TEBELLA
050700020926     C     KEYTBE        KLIST                                                  *TNTBE01L
050800020926     C                   KFLD                    KBECOD                          -TABELLA
050900020926     C                   KFLD                    KBEKE1                          -CHIAVE
051000980211     C     KEYCPC        KLIST                                                  *AZCPC00F
051100980211     C                   KFLD                    KCCVER                          -VERSIONE
051200980211     C     KEYCPL        KLIST                                                  *AZCPL00F
051300980211     C                   KFLD                    KCLVER                          -VERSIONE
051400980216     C                   KFLD                    KCLNAR                          -NAZIONE
051500980216     C                   KFLD                    KCLCAP                          -CAP
051600010329     C     KEYCPT        KLIST                                                  *AZCPT00F
051700010329     C                   KFLD                    KCTVER                          -VERSIONE
051800010329     C                   KFLD                    KCTTTC                          -CODICE TEMPO
051900010329     C                   KFLD                    KCTTSP                          -TIPO SERVIZIO
052000980211     C*---
052100010329     C* VARIABILI DI WRK
052200980211     C*---
052300980216     C                   MOVEL     '0'           $ERR              1            *ERRORE GENERICO
052400980216     C                   MOVEL     '0'           $FINE             1            *FINE LETTURA CAP
052500980216     C                   MOVEL     '0'           $FINL             1            *FINE LETTURA LOCALI
052600980220     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
052700050318     C                   MOVEL     *BLANKS       $PKU              1            *CREAZIONE WAPKU (C/
052800010329     C                   Z-ADD     *ZEROS        WFI2              5 0          *DEPOSITO CODICE TIP
052900020926     C*
053000020926     C* CALCOLA LA DATA CORRENTE -DAL JOB-
053100020926     C                   MOVE      *DATE         N8                8 0          *DATA JOB IN G/M/AA
053200020926     C                   Z-ADD     N8            G08DAT
053300020926     C                   Z-ADD     *ZEROS        G08INV
053400020926     C                   MOVEL     '0'           G08ERR
053500020926     C                   CALL      'XSRDA8'
053600020926     C                   PARM                    WLBDA8
053700020926     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA JOB IN AA/M/G
053800000000     C*
053900000000     C* REPERISCE I DATI UTENTE
054000020919     C                   EXSR      REPDATIUTE
054100020926     C*
054200020926     C* FILIALE --> REPERISCE L'ULTIMA VERSIONE DEL CAPPARIO
054300020926     C* SEDE    --> PEZZA: REPERISCE LA VERSIONE ALLA DATA DEL JOB -COSI' LUCIANO LA PUO' CAMBIARE-
054400020926IF  1C     DUTLPO        IFNE      'S'                                          F I L I A L E
054500020926     C                   Z-ADD     *ZEROS        P96DRI            8 0
054600020926X   1C                   ELSE                                                   S E D E
054700020926     C                   Z-ADD     DATCOR        P96DRI            8 0
054800020926E   1C                   ENDIF
054900020926     C                   Z-ADD     *ZEROS        P96VER            5 0
055000020926     C                   CALL      'TISI96R'
055100020926     C                   PARM                    P96DRI
055200020926     C                   PARM                    P96VER
055300980211     C*
055400980211     C* CARICA LE TABELLE OCCORRENTI
055500980211     C                   EXSR      CARTAB
055600960709     C*
055700000000     C                   ENDSR
055800000000     C*------------------------------------------------------------------------*
055900000000**   CMD -COMANDI CLP-
056000050318CLRPFM FILE(WAPKU00F)
