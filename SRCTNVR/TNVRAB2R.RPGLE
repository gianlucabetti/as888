000100980127     H DECEDIT('0,') DATEDIT(*YMD.)
000200980127      *---------------------------------------------------------------------*
000300980127      *  Ritorno date arrivo ai partner  - EUROPOLITAN -                    *
000400980127      *---------------------------------------------------------------------*
000500981106      * Indicatori utilizzati dal programma
000600981106      *-------------------------------------
000700981106      *  22 = SFLNXTCHG
000800981106      *  28 = Gestione messaggi di errore nel formato video
000900981106      *---------------------------------------------------------------------*
001000980127     FCNACO00F  IF   E           K DISK
001100980127     FTFNTC01L  IF   E           K DISK
001200961009     FTABEL00F  IF   E           K DISK
001300980127     FFNBLP40L  IF   E           K DISK
001400980128     FFNBL901L  IF   E           K DISK
001500980127     FFNEUR02L  IF   E           K DISK
001600980127     FAMZEVE    O    E             DISK
001700980128     FFIEU03D   CF   E             WORKSTN SFILE(EU03S02:NRR2)
001800980127      *---------------------------------------------------------------------*
001900980127      *  SCHIERE
002000980127      *---------------------------------------------------------------------*
002100980127      * Tabella 3K x codici con tipo invio 'EU'
002200981202     D KSC             S              7  0 DIM(600)
002300980128      * Ragione sociale
002400981202     D RSC             S             20    DIM(600)
002500980128      * Descrizione
002600981202     D DEC             S             20    DIM(600)
002700980128      * Data arrivo
002800981202     D SDA             S              8  0 DIM(600)
002900980128      * Ora arrivo
003000981202     D SOA             S              4  0 DIM(600)
003100980128      * Ora arrivo
003200981202     D SRI             S              1    DIM(600)
003300980128      * Tabella 3A solo tipi porto assegnato
003400981202     D SPA             S              2    DIM(50)
003500980128      * Errori
003600980127     D ERR             S             70    DIM(1) CTDATA PERRCD(1)
003700961003      *----------------------------------------------------------------
003800980128      * CAMPI DI COMODO
003900980127      *---------------------------------------------------------------------*
004000980128     D WTPVID          S              1
004100980128     D WFINE           S              1
004200980128     D §ORA            S              2  0
004300980128     D §MIN            S              2  0
004400980128     D JJ              S              3  0
004500980128     D XX              S              3  0
004600980128     D CONTA           S              3  0
004700980128     D NRR2            S              4  0
004800980128     D DATTIM          S             14  0
004900980128     D §§DAEL          S                   LIKE(BLPDSE)
005000980128     D G§§DAT          S                   LIKE(BLPDSE)
005100980128     D UDATE8          S                   LIKE(BLPDSE)
005200980128     D KKUT            S                   LIKE(ACOKUT)
005300980128     D KCOD1           S                   LIKE(TBLCOD)
005400980128     D KKCI            S                   LIKE(ACOKCC)
005500980128     D KKSC            S                   LIKE(ACOKSC)
005600980128     D KNK1            S                   LIKE(NTCNK1)
005700980128     D KNK2            S                   LIKE(NTCNK2)
005800980128     D KAPL            S                   LIKE(NTCAPL)
005900980128     D KTNT            S                   LIKE(NTCTNT)
006000980128     D KAAS            S                   LIKE(BLPAAS)
006100980128     D KMGS            S                   LIKE(BLPMGS)
006200980128     D KLNP            S                   LIKE(BLPLNP)
006300980128     D KNRS            S                   LIKE(BLPNRS)
006400980128     D KNSP            S                   LIKE(BLPNSP)
006500980128      *----------------------------------------------------------------
006600980128      * DS
006700980128      *---------------------------------------------------------------------*
006800961003     D KPJBA         E DS
006900980128     D DS3K          E DS
007000980128     D DS3A          E DS
007100980128     D TRUL52DS      E DS
007200980128     D CNCR80        E DS
007300980128     D UT§DSE0F      E DS
007400980128     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
007500980128     D  KCU                  698    847P 0 DIM(50)     PACKEVEN                 Capiconto
007600980127      * Ds scomposzione tipo capoconti
007700961003     D TCUDS           DS
007800961003     D  F34                    3      4
007900961003     D  F56                    5      6
008000980127      *
008100980127      *  Ds per inversione data
008200961003     D WLBDA8          DS
008300980127     D  G08DAT                 1      8  0
008400980127     D  G08INV                 9     16  0
008500980127     D  G08ERR                17     17
008600980127     D  G08TGI                18     22  0
008700961003     D                SDS
008800961003     D  NOMPGM                 1     10
008900981106      *---------------------------------------------------------------------*
009000980127      * Ciclo principale
009100980127      *---------------------------------------------------------------------*
009200980127      * Inizializzo prima videata
009300961003     C                   EXSR      INZ01
009400961003      * Loop gestione videate
009500961003     C     WFINE         DOWEQ     'N'
009600980127     C     WTPVID        CASEQ     '1'           GESV01                         Parametri elaborazio
009700961008     C     WTPVID        CASEQ     '2'           GESS02                         Subfile
009800961003     C                   END
009900961003     C                   END
010000961003      * Fine Lavoro
010100961003     C     FINE          TAG
010200980128     C                   EVAL      *INLR = *ON
010300980127      *---------------------------------------------------------------*
010400980127      *  INZ01: Inizializzo prima videata
010500980127      *---------------------------------------------------------------*
010600961003     C     INZ01         BEGSR
010700980127      *
010800980128     C                   EVAL      *IN28 = *OFF
010900980128     C                   MOVEA     '000'         *IN(61)
011000961003     C                   MOVEL     *BLANKS       $MSG
011100980128     C                   MOVEL     '1'           WTPVID
011200961003     C                   MOVEL     NOMPGM        V1CPGM
011300980127     C                   Z-ADD     UDATE8        V1DAEL
011400980127     C                   Z-ADD     UDATE8        V1DAAR
011500980127     C                   MOVEL     *ZEROS        V1ORAR
011600980127      *
011700961003     C                   ENDSR
011800980127      *---------------------------------------------------------------*
011900980127      *  INZ02: Inizializzo seconda videata
012000980127      *---------------------------------------------------------------*
012100961003     C     INZ02         BEGSR
012200980127      *
012300980128     C                   EVAL      *IN28 = *OFF
012400980128     C                   MOVEA     '00'          *IN(62)
012500961003     C                   MOVEL     *BLANKS       $MSG
012600961003     C                   MOVEL     NOMPGM        V2CPGM
012700961008     C                   MOVEL     '2'           WTPVID
012800980127     C                   Z-ADD     V1DAEL        V2DAEL
012900980127      *  Pulisco subfile
013000980128     C                   Z-ADD     *ZEROS        NRR2
013100980128     C                   MOVEA     '00'          *IN(20)
013200980113     C                   WRITE     EU03C02
013300980128     C                   MOVEA     '11'          *IN(20)
013400980127      *  Eseguo caricamento del subfile
013500980128     C                   Z-ADD     *ZEROS        NRR2
013600980127     C     1             DO        CONTA         XX
013700980127     C                   EXSR      CARSF
013800961008     C                   END
013900961003     C*
014000961003     C                   ENDSR
014100980127      *---------------------------------------------------------------*
014200980127      *  GESV01: Gestione prima videata
014300980127      *---------------------------------------------------------------*
014400980127     C     GESV01        BEGSR
014500980127      *
014600980113     C                   EXFMT     EU03D01
014700980128     C                   EVAL      *IN28 = *OFF
014800980127      *  Fine Lavoro
014900980128     C                   if        *INKC = *ON
015000961003     C                   MOVEL     'S'           WFINE
015100961003     C                   GOTO      FINVD1
015200980128     C                   endif
015300980127      *  Controlli
015400961003     C                   EXSR      CTR01
015500961007     C   28              GOTO      FINVD1
015600980127      *  Caricamento subfile
015700961007     C                   EXSR      INZ02
015800980128     C                   Z-ADD     1             REC2
015900980127      *
016000961003     C     FINVD1        ENDSR
016100980127      *---------------------------------------------------------------*
016200980127      *  GESS02: Gestione subfile pieno
016300980127      *---------------------------------------------------------------*
016400961003     C     GESS02        BEGSR
016500980127      *
016600980113     C                   WRITE     EU03T02
016700980113     C                   EXFMT     EU03C02
016800980128     C                   EVAL      *IN28 = *OFF
016900980128     C                   MOVEA     '00'          *IN(62)
017000980127      *  Fine Lavoro
017100980128     C                   if        *INKC = *ON
017200961003     C                   MOVEL     'S'           WFINE
017300961003     C                   GOTO      FINVS2
017400961003     C                   END
017500980127      *  Ritorno
017600980128     C                   if        *INKL = *ON
017700980128     C                   EVAL      *IN28 = *OFF
017800980128     C                   MOVEA     '000'         *IN(61)
017900980127     C                   MOVEL     *BLANKS       $MSG
018000980128     C                   MOVEL     '1'           WTPVID
018100961003     C                   GOTO      FINVS2
018200961003     C                   END
018300980127      *  Controlli
018400961003     C                   EXSR      CTR02
018500980127      *  Conferma
018600980128     C                   if        *INKF = *ON  and  *IN28 = *OFF
018700980127     C                   EXSR      ELABOR
018800980127     C                   MOVEL     'S'           WFINE
018900980127     C                   END
019000980127      *
019100961003     C     FINVS2        ENDSR
019200980127      *---------------------------------------------------------------*
019300980127      *  CTR01: Controlli prima videata
019400980127      *---------------------------------------------------------------*
019500961003     C     CTR01         BEGSR
019600980127      *
019700980128     C                   MOVEA     '000'         *IN(61)
019800980127      *
019900980127      *  Controllo se la data di elaborazione è valida
020000980127      *
020100980128     C                   IF        V1DAEL = *ZEROS
020200980128     C                   EVAL      *IN28 = *ON
020300980128     C                   EVAL      *IN61 = *ON
020400980127     C                   MOVEL     ERR(1)        $MSG
020500980127     C                   GOTO      FINCT1
020600980127     C                   ELSE
020700980127      *
020800980127     C                   Z-ADD     V1DAEL        G08DAT
020900980127     C                   MOVEL     *BLANKS       G08ERR
021000980127     C                   CALL      'XSRDA8'
021100980127     C                   PARM                    WLBDA8
021200980128     C                   IF        G08ERR = '1'
021300980128     C                   EVAL      *IN28 = *ON
021400980128     C                   EVAL      *IN61 = *ON
021500980127     C                   MOVEL     ERR(1)        $MSG
021600980127     C                   GOTO      FINCT1
021700980127     C                   ELSE
021800980127     C                   Z-ADD     G08DAT        V1DAEL
021900980128     C                   Z-ADD     G08INV        §§DAEL
022000980127     C                   END
022100980127     C                   END
022200980127      *
022300980127      *  Controllo se la data di arrivo è valida
022400980127      *
022500980128     C                   IF        V1DAAR = *ZEROS
022600980128     C                   EVAL      *IN28 = *ON
022700980128     C                   EVAL      *IN62 = *ON
022800980127     C                   MOVEL     ERR(1)        $MSG
022900980127     C                   GOTO      FINCT1
023000980127     C                   ELSE
023100980127      *
023200980127     C                   Z-ADD     V1DAAR        G08DAT
023300980127     C                   MOVEL     *BLANKS       G08ERR
023400980127     C                   CALL      'XSRDA8'
023500980127     C                   PARM                    WLBDA8
023600980128     C                   IF        G08ERR = '1'
023700980128     C                   EVAL      *IN28 = *ON
023800980128     C                   EVAL      *IN62 = *ON
023900980127     C                   MOVEL     ERR(1)        $MSG
024000980127     C                   GOTO      FINCT1
024100980127     C                   ELSE
024200980127     C                   Z-ADD     G08DAT        V1DAAR
024300980127     C                   END
024400980127     C                   END
024500980127      *
024600980127      *  Controllo se l'ora di arrivo è valida
024700980127      *
024800980128     C                   IF        V1ORAR = *ZEROS
024900980128     C                   EVAL      *IN28 = *ON
025000980128     C                   EVAL      *IN63 = *ON
025100980127     C                   MOVEL     ERR(1)        $MSG
025200980127     C                   GOTO      FINCT1
025300980127     C                   ELSE
025400980127      *
025500980128     c                   movel     v1orar        §ORA
025600980128     c                   move      v1orar        §MIN
025700980128     c                   eval      *IN63 = (§ORA > 24)
025800980128     c  N63              eval      *IN63 = (§MIN > 60)
025900980128     C                   IF        *IN63 = '1'
026000980128     C                   EVAL      *IN28 = *ON
026100980127     C                   MOVEL     ERR(1)        $MSG
026200980127     C                   GOTO      FINCT1
026300980127     C                   END
026400980127     C                   END
026500980127      *
026600961003     C     FINCT1        ENDSR
026700980127      *----------------------------------------------------*
026800980127      *   Caricamento subfiles                             *
026900980127      *----------------------------------------------------*
027000980127     C     CARSF         BEGSR
027100980127      *
027200980127     C                   CLEAR                   EU03S02
027300980127      *
027400980127     C                   Z-ADD     V1DAAR        V2DAAR
027500980127     C                   Z-ADD     V1ORAR        V2ORAR
027600980127     C                   MOVEL     *BLANKS       V2RITA
027700980127     C                   Z-ADD     KSC(XX)       V2CLIE
027800980127     C                   MOVEL     RSC(XX)       V2DECL
027900980127     C                   MOVEL     DEC(XX)       V2TACL
028000980127     C                   Z-ADD     XX            V2CNRR
028100980127     C                   ADD       1             NRR2
028200980127     C                   WRITE     EU03S02
028300980127      *
028400980127     C                   ENDSR
028500980127      *---------------------------------------------------------------*
028600980127      *  CTR02: Controlli seconda videata
028700980127      *---------------------------------------------------------------*
028800961003     C     CTR02         BEGSR
028900980127      *
029000980127      *  Controllo campi imput
029100980127      *
029200980127     C                   Z-ADD     1             NRR2
029300980113     C                   READC     EU03S02                                29
029400980127      *
029500961003     C     *IN29         DOWEQ     '0'
029600980128     C                   EXSR      CTR021
029700980128     C   28              EVAL      *IN29 = *ON
029800980127     C  N28              READC     EU03S02                                29
029900961003     C                   END
030000980127      *
030100961003     C                   ENDSR
030200980127      *---------------------------------------------------------------*
030300980127      *  CTR021: Controlli seconda videata campi imput
030400980127      *---------------------------------------------------------------*
030500980128     C     CTR021        BEGSR
030600980127      *
030700980128     C                   clear                   G§§DAT
030800980127      *
030900980127      *  Controllo se la data di arrivo è valida
031000980127      *
031100980128     c                   if        v2daar > *zeros
031200980127     C                   Z-ADD     V2DAAR        G08DAT
031300980127     C                   MOVEL     *BLANKS       G08ERR
031400980127     C                   CALL      'XSRDA8'
031500980127     C                   PARM                    WLBDA8
031600980128     c                   if        g08err = '1'
031700980128     c                   eval      *in28 = *on
031800980128     c                   eval      *in62 = *on
031900980127     C                   MOVEL     ERR(1)        $MSG
032000980127     C                   GOTO      FINCT2
032100980127     C                   ELSE
032200980127     C                   Z-ADD     G08DAT        G§§DAT
032300980127     C                   END
032400980127     C                   END
032500980127      *
032600980127      *  Controllo se l'ora di arrivo è valida
032700980127      *
032800980128     c                   if        v2orar > *zeros
032900980128     c                   movel     v2orar        §ORA
033000980128     c                   move      v2orar        §MIN
033100980128     c                   eval      *IN63 = (§ORA > 24)
033200980128     c  N63              eval      *IN63 = (§MIN > 60)
033300980128     C                   IF        *IN63 = '1'
033400980128     c                   eval      *in28 = *on
033500980127     C                   MOVEL     ERR(1)        $MSG
033600980127     C                   GOTO      FINCT2
033700980127     C                   END
033800980127     C                   END
033900980127      *
034000980127      *  Controlli di congruità
034100980127      *
034200980128     C                   IF        (V2DAAR > *ZEROS and V2ORAR = *ZEROS)  or
034300980128     C                             (V2DAAR = *ZEROS and V2ORAR > *ZEROS)
034400980128     c                   eval      *in28 = *on
034500980128     C                   MOVEA     '11'          *IN(62)
034600980127     C                   MOVEL     ERR(1)        $MSG
034700980127     C                   GOTO      FINCT2
034800980127     C                   END
034900980127      *
035000980127     C     FINCT2        TAG
035100980128     C                   EVAL      *IN22 = (*IN28 = *ON)
035200980128     C   28              z-add     NRR2          REC2
035300980128      *
035400980127     C     NRR2          CHAIN     EU03S02                            01
035500980128     C                   IF        *IN01 = *OFF   and  G§§DAT > *zeros
035600980127     C                   Z-ADD     G§§DAT        V2DAAR
035700980127     C                   END
035800980127     C  N01              UPDATE    EU03S02
035900980127      *
036000980127     C                   ENDSR
036100980127      *----------------------------------------------------*
036200980127      *   Elaboro spedizioni                               *
036300980127      *----------------------------------------------------*
036400980127     C     ELABOR        BEGSR
036500980127      *
036600980127      *  Scarico dal subfile la data e l'ora d'arrivo (AMG) per ogni cliente
036700980127      *
036800980128     C                   clear                   KSC
036900980128     C                   CLEAR                   JJ
037000980127      *
037100980127     C     1             DO        CONTA         XX
037200980127     C                   Z-ADD     XX            NRR2
037300980127     C     NRR2          CHAIN     EU03S02                            01
037400980127      *
037500980128     C                   IF        *in01 = *off  AND  V2DAAR > *zeros
037600980127     C                   Z-ADD     V2DAAR        G08DAT
037700980127     C                   MOVEL     *BLANKS       G08ERR
037800980127     C                   CALL      'XSRDA8'
037900980127     C                   PARM                    WLBDA8
038000980127      *
038100980127     C                   ADD       1             JJ
038200980127     C                   Z-ADD     G08INV        SDA(JJ)
038300980127     C                   Z-ADD     V2ORAR        SOA(JJ)
038400980127     C                   Z-ADD     V2CLIE        KSC(JJ)
038500980128     C                   MOVEL     V2RITA        SRI(JJ)
038600980127     C                   END
038700980128     C                   END
038800980127      *
038900980128     c                   if        JJ > *ZEROS
039000980127      *
039100980128      *  Elaboro le  spedizioni con data bolla uguale alla data elaborazione
039200980127      *
039300980127     C                   MOVEL     §§DAEL        KAAS
039400980127     C                   MOVE      §§DAEL        KMGS
039500980127      *
039600980127     C     KBLP          SETLL     FNBLP000
039700980127      *
039800980128     C                   DO        *HIVAL
039900980127     C     KBLP          READE     FNBLP000                               01
040000980127      *
040100980128      * Elabora solo se letto almeno un segnacollo della spedizione
040200980128      *
040300980128     c                   IF        *IN01 = *OFF and BLPDSE > *ZEROS
040400980128      *
040500980128      * Determino se franco o assegnato per stabilire quale codice cliente controllare
040600980128      *
040700980128     C     BLPCBO        LOOKUP    SPA                                    02
040800980128      *
040900980128     c                   if        (*in02 = *on and BLPCCM > *zeros)  or
041000980128     c                             (*in02 = *off and BLPKSC > *zeros)
041100980128     C                   movea     '00'          *IN(31)
041200980128     C                   z-add     1             JJ
041300980128     C   02BLPCCM        LOOKUP    KSC(JJ)                                31               Assegnati
041400980128     C  N02BLPKSC        LOOKUP    KSC(JJ)                                32                 Franchi
041500980128      *
041600980128      * Se il codice mittente è uno di quelli in schiera e la spedizione è stata inviata
041700980128      *  da EUROPOLITAN creo l'evento di spedizione arrivata.
041800980128      *
041900980128     c                   if        *in31 = *on  or *IN32 = *on
042000980128     C                   MOVEL     BLPAAS        KAAS
042100980128     C                   MOVEL     BLPLNP        KLNP
042200980128     C                   MOVEL     BLPNRS        KNRS
042300980128     C                   MOVEL     BLPNSP        KNSP
042400980128     C     KEUR          CHAIN     FNEUR000                           30
042500980128     C                   IF        *IN30 = *OFF
042600980128     C                   EXSR      CRTEVE_AR                                    Sped. arrivata
042700980128      *
042800980128      * Se la spedizione è in assegnato o con C/Assegno creo evento di fermo deposito
042900980128      *
043000980128     C                   IF        *IN31 = *ON                                  Assegnati
043100980128     C                   EXSR      CRTEVE_FD                                    Fermo Deposito
043200980128     C                   ELSE
043300980128     C     KEUR          SETLL     FNBL9000                               30    C/Assegno
043400980128     C   30              EXSR      CRTEVE_FD                                    Fermo Deposito
043500980128     C                   ENDIF
043600980128     C                   ENDIF
043700980128      *
043800980128     C                   END
043900980128     C                   END
044000980128     C                   END
044100980127     C  N01              END
044200980128      *
044300980128     C                   END
044400980127      *
044500980127     C     ENDELA        ENDSR
044600980128      *----------------------------------------------------------------
044700980128      * CREAZIONE RCD EVENTO SPEDIZIONE ARRIVATA
044800980128      *----------------------------------------------------------------
044900980128     C     CRTEVE_AR     BEGSR
045000980128      *
045100980128     C                   CLEAR                   EEVEF0
045200980128      *
045300980128     C                   Z-ADD     eurJOUR       EVJOUR                         Giorno
045400980128     C                   Z-ADD     eurMOIS       EVMOIS                         Mese
045500980128     C                   Z-ADD     eurANNE       EVANNE                         Anno
045600980128     C                   MOVEL     eurPAYE       EVPAYE                         Nazio. part.
045700980128     C                   MOVEL     eurFILE       EVFILE                         Corr. part.
045800980128     C                   Z-ADD     eurAGCE       EVAGCE                         Filiale
045900980128     C                   Z-ADD     eurNORD       EVNORD                         Progressivo
046000980312     C                   MOVEL     '380'         EVPAYV                         Naz. evento
046100980128     C                   MOVEL     'MZ'          EVFILV                         Cor. evento
046200981106     C                   Z-ADD     SIMFEL        EVAGCV                         Fil. evento
046300980128     C                   MOVEL     SDA(JJ)       EVDATE                         Data evento
046400980128     C                   MOVEL     SOA(JJ)       EVHEUR                         Ora  evento
046500980128     C                   MOVEL(P)  'SDI'         EVPERS                         Utente
046600980128     C                   Z-ADD     *ZEROS        EVNCOL                         Colli
046700980128     C                   Z-ADD     *ZEROS        EVDREP                         Data appunt.
046800980128      *
046900980128      * Assegno status ritardo o arrivo normale
047000980128      *
047100980128     c                   IF        SRI(JJ) = 'R'
047200980128     C                   MOVE      '35'          EVSTAT                         Status
047300980128     C                   MOVE      '11'          EVEVEN                         Cod. evento
047400980128     C                   ELSE
047500980128     C                   MOVE      '30'          EVSTAT                         Status
047600980128     C                   MOVE      '30'          EVEVEN                         Cod. evento
047700980128     C                   END
047800980128      *
047900980128     C                   WRITE     EEVEF0
048000980128      *
048100980128     C                   ENDSR
048200980128      *----------------------------------------------------------------
048300980128      * CREAZIONE RCD EVENTO SPEDIZIONE IN FERMO DEPOSITO
048400980128      *----------------------------------------------------------------
048500980128     C     CRTEVE_FD     BEGSR
048600980128      *
048700980128     C                   CLEAR                   EEVEF0
048800980128      *
048900980128     C                   Z-ADD     eurJOUR       EVJOUR                         Giorno
049000980128     C                   Z-ADD     eurMOIS       EVMOIS                         Mese
049100980128     C                   Z-ADD     eurANNE       EVANNE                         Anno
049200980128     C                   MOVEL     eurPAYE       EVPAYE                         Nazio. part.
049300980128     C                   MOVEL     eurFILE       EVFILE                         Corr. part.
049400980128     C                   Z-ADD     eurAGCE       EVAGCE                         Filiale
049500980128     C                   Z-ADD     eurNORD       EVNORD                         Progressivo
049600980128     C                   MOVE      '55'          EVSTAT                         Status
049700980312     C                   MOVEL     '380'         EVPAYV                         Naz. evento
049800980128     C                   MOVEL     'MZ'          EVFILV                         Cor. evento
049900981106     C                   Z-ADD     SIMFEL        EVAGCV                         Fil. evento
050000980128     C                   MOVEL     SDA(JJ)       EVDATE                         Data evento
050100980128     C                   MOVEL     SOA(JJ)       EVHEUR                         Ora  evento
050200980128     C                   MOVE      '65'          EVEVEN                         Cod. evento
050300980128     C                   MOVEL(P)  'SDI'         EVPERS                         Utente
050400980128     C                   Z-ADD     *ZEROS        EVNCOL                         Colli
050500980128     C                   Z-ADD     *ZEROS        EVDREP                         Data appunt.
050600980128      *
050700980128     C                   WRITE     EEVEF0
050800980128      *
050900980128     C                   ENDSR
051000980127      *----------------------------------------------------*
051100980127      *   Decodifica Ragione Sociale Cliente               *
051200980127      *----------------------------------------------------*
051300980127     C     DECCLI        BEGSR
051400980127      *
051500980127     C                   Z-ADD     KSC(XX)       KKSC
051600980127     C     KACO          CHAIN     CNACO000                           30
051700980128      * Per codice cliente annullato annullo il valore schiera e ripristino l'indice
051800980128     c                   if        *in30 = *off AND acoflg <> ' '
051900980127     C                   Z-ADD     *ZEROS        KSC(XX)
052000980127     C                   SUB       1             XX
052100980127   X C                   ELSE
052200980127     C                   MOVEL     ACORAG        RSC(XX)
052300980127     C                   EXSR      DECCOD
052400980127     C                   END
052500980127      *
052600980127     C                   ENDSR
052700980127      *----------------------------------------------------*
052800980127      *   Decodifica descrizione codice                    *
052900980127      *----------------------------------------------------*
053000980127     C     DECCOD        BEGSR
053100980127      *
053200980127     C                   MOVEL     'C'           KAPL
053300980127     C                   MOVEL     'DC'          KTNT
053400980127     C                   MOVE      ACOKSC        KNK1
053500980127     C                   MOVEL     KKCI          KNK1
053600980127     C     KNTC          CHAIN     TFNTC01L                           30
053700980127     C  N30              MOVEL     NTCRNT        DEC(XX)
053800980127      *
053900980127     C                   ENDSR
054000980127      *----------------------------------------------------*
054100980127      *   *INZSR: Operazioni iniziali                      *
054200980127      *----------------------------------------------------*
054300980127     C     *INZSR        BEGSR
054400980127      *
054500961003     C     *ENTRY        PLIST
054600961003     C                   PARM                    KPJBA
054700980127      *
054800980127      *  Richiamo XPARUT
054900980127      *
055000961003     C                   Z-ADD     1             CODUT
055100961003     C                   CALL      'X§PARUT'
055200980128     C                   PARM                    UT§DSE0F
055300961003     C                   MOVEL     REC80         CNCR80
055400980127      *
055500980127      *  Ricerca capoconti
055600980127      *
055700980128     C                   DO        50            JJ
055800980127     C                   MOVE      TCU(JJ)       TCUDS
055900980128     c                   if        F56 = 'CG' and F34 = '01'
056000980127     C                   Z-ADD     KCU(JJ)       KKCI
056100961003     C                   END
056200961003     C                   END
056300961003     C                   MOVEL     RAGUT         RSUT
056400980127      *
056500980127      *  Chiavi di accesso
056600980127      *
056700961009     C     KTAB1         KLIST
056800961009     C                   KFLD                    KKUT
056900961104     C                   KFLD                    KCOD1
057000980127      *
057100980127     C     KACO          KLIST
057200980127     C                   KFLD                    CODUT
057300980127     C                   KFLD                    KKCI
057400980127     C                   KFLD                    KKSC
057500980127      *
057600980127     C     KNTC          KLIST
057700980127     C                   KFLD                    KAPL
057800980127     C                   KFLD                    KNK1
057900980127     C                   KFLD                    KNK2
058000980127     C                   KFLD                    KTNT
058100980128      *
058200980128     C     KBLP          KLIST
058300980128     C                   KFLD                    KAAS
058400980128     C                   KFLD                    KMGS
058500980128      *
058600980128     C     KEUR          KLIST
058700980128     C                   KFLD                    KAAS
058800980128     C                   KFLD                    KLNP
058900980128     C                   KFLD                    KNRS
059000980128     C                   KFLD                    KNSP
059100980127      *
059200980127      * REPERISCO LA DATA DELL'ULTIMO GIORNO LAVORATIVO
059300980127      *
059400980128     C                   TIME                    DATTIM
059500980128     C                   MOVE      DATTIM        UDATE8
059600980127      *
059700980127      *  GIRO UDATE8
059800980127      *
059900980127     C                   Z-ADD     UDATE8        G08DAT
060000980127     C                   MOVEL     *BLANKS       G08ERR
060100980127     C                   Z-ADD     *ZEROS        G08INV
060200980127     C                   CALL      'XSRDA8'
060300980127     C                   PARM                    WLBDA8
060400980127      *
060500980128     C                   CLEAR                   TRUL52DS
060600980127     C                   Z-ADD     G08INV        D52DIN
060700980127     C                   MOVEL     '-'           D52SGN
060800980127     C                   Z-ADD     1             D52GIO
060900980128     C                   CALL      'TRUL52R'
061000980128     C                   PARM                    TRUL52DS
061100980128      *
061200980128     C                   IF        D52ERR = *BLANKS
061300980127     C                   Z-ADD     D52DFI        UDATE8
061400980127     C                   Z-ADD     UDATE8        G08INV
061500980127     C                   MOVEL     '3'           G08ERR
061600980127     C                   Z-ADD     *ZEROS        G08DAT
061700980127     C                   CALL      'XSRDA8'
061800980127     C                   PARM                    WLBDA8
061900980127     C                   Z-ADD     G08DAT        UDATE8
062000980127     C                   ELSE
062100980127     C                   Z-ADD     *ZEROS        UDATE8
062200980127     C                   END
062300980127      *
062400980127      * Caricamento Tabella 3K - Solo tipo invio 'EU', memorizzo il numero di elementi
062500980127      *
062600980128     C                   Z-ADD     0             XX
062700961029     C                   Z-ADD     1             KKUT
062800980128     C                   Z-ADD     0             KKSC
062900980127     C                   MOVEL     '3K'          KCOD1
063000961029     C     KTAB1         CHAIN     TABEL00F                           30
063100961029     C     *IN30         DOWEQ     '0'
063200980128     c                   IF        TBLFLG = *BLANKS
063300980127     C                   MOVEL     TBLUNI        DS3K
063400980128     c                   IF        §3KCSD = 'EU'
063500980127     C                   ADD       1             XX
063600980127     C                   MOVEL     TBLKEY        KSC(XX)
063700980127     C                   EXSR      DECCLI                                       Decod. cliente
063800961029     C                   END
063900980127     C                   END
064000961029     C     KTAB1         READE     TABEL00F                               30
064100961029     C                   END
064200980128     C                   Z-ADD     XX            CONTA
064300980128      *
064400980128      * Caricamento Tabella 3A - Solo tipo porto assegnato
064500980128      *
064600980128     C                   Z-ADD     0             XX
064700980128     C                   Z-ADD     1             KKUT
064800980128     C                   Z-ADD     0             KKSC
064900980128     C                   MOVEL     '3A'          KCOD1
065000980128     C     KTAB1         CHAIN     TABEL00F                           30
065100980128     C     *IN30         DOWEQ     '0'
065200980128     c                   IF        TBLFLG = *BLANKS
065300980128     C                   MOVEL     TBLUNI        DS3A
065400980128     c                   IF        %SUBST(§3ATB1:1:1) = 'A'
065500980128     C                   ADD       1             XX
065600980128     C                   MOVEL     TBLKEY        SPA(XX)
065700980128     C                   END
065800980128     C                   END
065900980128     C     KTAB1         READE     TABEL00F                               30
066000980128     C                   END
066100980128      *
066200980128      *  Inizializzo variabili
066300980128      *
066400980128     C                   MOVEL     'N'           WFINE
066500980128     C                   EVAL      *IN23 = *ON
066600980128      *
066700961003     C                   ENDSR
066800980127      *----------------------------------------------------------------------
066900980127**
067000980127Immessi dati non validi.                                               01
