000100141219     /*END
000200980904     H*------------------------------------------------------------------------*
000300980904     H* REPORT COMMERCIALE - CREAZIONE REPORT - PARTE 2^                       *
000400980904     H*------------------------------------------------------------------------*
000500980904     H DECEDIT('0,') DATEDIT(*DMY.)
000600980904     F*------------------------------------------------------------------------*
000700980904     F* DATA BASE
000800980904     F*------------------------------------------------------------------------*
000900030807     F*---
001000980904     F* WORKFILE
001100980904     F*---
001200140613     FAZCMM01L  IF   E           K DISK    usropn
001300050408     FWFREP11L  UF   E           K DISK
001400980904     F*---
001500980904     F* BOLLE
001600980904     F*---
001700990802     FTITAS38C  IF   E           K DISK
001800990802     F                                     IGNORE(TITAS000)
001900000519     F                                     IGNORE(TITASP00)
002000980904     F*---
002100980904     F* ORGANIGRAMMA
002200980904     F*---
002300980904     FAZORG01L  IF   E           K DISK
002400980904     F*---
002500980904     F* TABELLE
002600980904     F*---
002700980904     FTABEL00F  IF   E           K DISK
002800020702     FTNTBE01L  IF   E           K DISK
002900050414     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
003000980904     D*------------------------------------------------------------------------*
003100980904     D* SCHIERE
003200980904     D*------------------------------------------------------------------------*
003300980904     D*---
003400980904     D* PERIODO DA ELABORARE
003500980904     D*---
003600980904     D AAA             S              4  0 DIM(12)                              *ANNI
003700980904     D MMM             S              2  0 DIM(12)                              *MESI
003800980904     D*---
003900980904     D* ORGANIGRAMMA
004000980904     D*---
004100980904     D FIL             S              3  0 DIM(500)                             *FILIALE
004200030805     D FDPD            S              3  0 DIM(500)                             *DPD
004300980904     D FFL1            S              1    DIM(500)                             *ITALIA/ESTERO
004400980904     D FARE            S              3  0 DIM(500)                             *AREA
004500980904     D FDIV            S              1    DIM(500)                             *DIVISIONE
004600980904     D*---
004700980904     D* TIPI BOLLA
004800980904     D*---
004900980904     D TBL             S              2    DIM(20)                              *TIPO BOLLA
005000980904     D TRBL            S              1    DIM(20)                              *RAGGRUPPAMENTO
005100980904     D TFSP            S              1    DIM(20)                              *FLAG SPEDIZIONI (S/
005200980904     D TFRI            S              1    DIM(20)                              *FLAG RICAVI     (S/
005300030805     D*---
005400030805     D* CODICI TRATTAMENTO MERCE
005500030805     D*---
005600030805     D TMC             S              2    DIM(100)
005700030805     D TMT             S              1    DIM(100)
005800980904     D*---
005900980904     D* COMMERCIALI UNIFICANTI
006000980904     D*---
006100150107     d c_Max_CmmU      c                   const(5000)
006200150107     D RGF             S              7  0 DIM(c_Max_CmmU)                      *CODICE
006300150107     D DRGF            S             25    DIM(c_Max_CmmU)                      *DESCRIZIONE
006400980904     D*---
006500980904     D* COMMERCIALI
006600980904     D*---
006700150107     d c_Max_Cmm       c                   const(9000)
006800141219     D AGE             S              7  0 DIM(c_Max_Cmm)                       *AGENTE
006900141219     D ARGF            S              7  0 DIM(c_Max_Cmm)                       *AGENTE UNIFICANTE
007000980904     D*---
007100980904     D* SCHIERE DI MEMORIZZAZIONE                        -MESI-
007200980904     D*---
007300150107     D* MULTIPLE                                         -5000 COMMERCIALI UNIF-
007400980904     D*------------------------------------------------------------------------*
007500980904     D* INPUT
007600980904     D*------------------------------------------------------------------------*
007700980904     D* (11:2)+1=6,5 ; (6x12) = 72
007800980904     D*---
007900980904     D* DS MULTIPLA FASCE DI PESO PER TOTALI GENERALI     - 10 FASCE X 12 MESI -
008000980904     D*---
008100150107     D UDAT            DS                  OCCURS(c_Max_CmmU)  INZ
008200980904     D  $CTR                   1     12
008300981111     D                                     DIM(12)                              *CONTROLLO MEMORIZZA
008400980904     D  ISDN                  13     84P 0
008500980904     D                                     DIM(12)                              *SPEDIZIONI NON DDT
008600980904     D  ISAB                  85    156P 0
008700980904     D                                     DIM(12)                              *SPEDIZIONI DISK A/B
008800980904     D  ICDB                 157    228P 0
008900980904     D                                     DIM(12)                              *COLLI DISK B
009000980904     D  ESDN                 229    300P 0
009100980904     D                                     DIM(12)                              *SPEDIZIONI NON DDT
009200980904     D  ESAB                 301    372P 0
009300980904     D                                     DIM(12)                              *SPEDIZIONI DISK A/B
009400980904     D  ECDB                 373    444P 0
009500980904     D                                     DIM(12)                              *COLLI DISK B
009600030805     D  ISCB                 445    516P 0
009700030805     D                                     DIM(12)                              *SPED. DISK C BART.
009800030805     D  ESCB                 517    588P 0
009900030805     D                                     DIM(12)                              *SPED. DISK C BART.
010000030805     D  ISCC                 589    660P 0
010100030805     D                                     DIM(12)                              *SPED. DISK C CLI.
010200030805     D  ESCC                 661    732P 0
010300030805     D                                     DIM(12)                              *SPED. DISK C CLI.
010400050408     D  IcCB                 733    804P 0
010500050408     D                                     DIM(12)                              *SPED. DISK C BART.
010600050408     D  EcCB                 805    876P 0
010700050408     D                                     DIM(12)                              *SPED. DISK C BART.
010800050408     D  IcCC                 877    948P 0
010900050408     D                                     DIM(12)                              *SPED. DISK C CLI.
011000050408     D  EcCC                 949   1020P 0
011100050408     D                                     DIM(12)                              *SPED. DISK C CLI.
011200980904     D*---
011300980904     D* DS AGENTI
011400980904     D*---
011500140613     D****** DS01          E DS
011600980904     D*---
011700980904     D* DS TIBI BOLLA
011800980904     D*---
011900980904     D DSTB          E DS
012000980904     D*---
012100980904     D* DS FLAG TIPI BOLLA
012200980904     D*---
012300980904     D DS2W          E DS
012400980904     D*---
012500980904     D* DS REPERIMENTO DATI CLIENTE
012600980904     D*---
012700980904     D BS69DS        E DS                  EXTNAME(TIBS69DS)
012800980904     D ACODS         E DS                  EXTNAME(CNACO00F)
012900980904     D INDDS         E DS                  EXTNAME(CNIND00F)
013000980904     D CLPDS         E DS                  EXTNAME(CNCLP00F)
013100980904     D CLSDS         E DS                  EXTNAME(FNCLS00F)
013200980904     D*---
013300980904     D* DS COMPOSIZIONE DATA(6) -TFSAC-
013400980904     D*---
013500981007     D                 DS                  INZ
013600980904     D  TASAAS                 1      4  0
013700980904     D  TASMGS                 5      8  0
013800980904     D  TASMES                 5      6  0
013900980904     D  TASDAT                 1      6  0
014000980904     D*---
014100980904     D* DS PARAMETRI INPUT
014200980904     D*---
014300980904     D PARAM           DS
014400980904     D  PARAMI                 1      6  0
014500980904     D  PARAAI                 1      4  0
014600980904     D  PARMMI                 5      6  0
014700980904     D  PARAMF                 7     12  0
014800980904     D  PARAAF                 7     10  0
014900980904     D  PARMMF                11     12  0
015000050414     D  PARsta               256    256
015100980904     D*---
015200980904     D* ARCHITETTURA
015300980904     D*---
015400980904     D KPJBA         E DS
015500000216     D*---
015600000216     D* DS X LETTURA DESCRIZIONE 143 ORGANIGRAMMA (X DPD)
015700000216     D*---
015800000216     D OG143         E DS
015900020403     D*---
016000020403     D* DS X REPERIMENTO/TEST CLIENTI PADRI/FIGLI
016100020403     D*---
016200020403     D TIBS10        E DS                  EXTNAME(TIBS10DS)
016300020702     D*------------------
016400020702     D* DS PER LETTURA TABELLA "NTW"
016500020702     D*------------------
016600020702     D DNTW          E DS
016700030805     D*---
016800030805     D* DS CODICI TRATTAMENTO MERCE
016900030805     D*---
017000030805     D DS1B          E DS
017100030805     D*---
017200030805     D* DS TABELLA INVIO DATI CLIENTI
017300030805     D*---
017400030805     D DS3C          E DS
017500980904     C*------------------------------------------------------------------------*
017600980904     C* MAIN LINE
017700980904     C*------------------------------------------------------------------------*
017800980904     C*
017900980904     C* POSIZIONAMENTO E PRIMA LETTURA BOLLE
018000980904     C                   EXSR      SETFIL
018100980904     C*
018200980904     C* CICLO FINO A FINE FILE
018300980904DO  1C     $FINE         DOWEQ     'N'
018400980904     C*
018500980904     C* MEMORIZZA LE BOLLE PER COMMERCIALE
018600980904     C                   EXSR      MEMTAS
018700980904     C*
018800980904     C* LETTURA SUCCESSIVA BOLLE
018900980904     C                   EXSR      LETFIL
019000980904E   1C                   ENDDO                                                  *FINE FILE
019100980904     C*
019200980904     C* AGGIORNA IL REPORT
019300980904     C                   EXSR      SCRREP
019400980904     C*
019500980904     C* OPERAZIONI FINALI
019600980904     C                   EXSR      FINSR
019700980904     C*
019800980904     C                   SETON                                        LR
019900980904     C*------------------------------------------------------------------------*
020000980904     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA                          -BOLLE-
020100980904     C*------------------------------------------------------------------------*
020200980904     C     SETFIL        BEGSR
020300980904     C*
020400981215     C                   Z-ADD     PARAAI        KASAAS                         *ANNO INIZIALE
020500981215     C                   MOVEL     PARMMI        KASMGS                         *MM/GG INIZIALE
020600990802     C     KEYTAS        SETLL     TITAS38C                           99        *FINE ARCHIVIO
020700980904IF  1C     *IN99         IFEQ      *ON
020800980904     C                   MOVEL     'S'           $FINE                          *FINE PGM
020900980904X   1C                   ELSE
021000980904     C                   EXSR      LETFIL                                       *LETTURA RECORD
021100980904E   1C                   ENDIF
021200980904     C*
021300980904     C                   ENDSR
021400980904     C*------------------------------------------------------------------------*
021500980904     C* LETFIL - LETTURA PROSSIMO RECORD                                 -BOLLE-
021600980904     C*------------------------------------------------------------------------*
021700980904     C     LETFIL        BEGSR
021800980904     C*
021900980904     C* LETTURA FINO A:
022000980904     C*  - FINE FILE
022100980904     C*  - TROVATO RECORD VALIDO
022200980904     C                   MOVEL     'N'           WRECOK
022300980904DO  1C     $FINE         DOWEQ     'N'
022400980904     C     WRECOK        ANDEQ     'N'
022500990802     C                   READ      TITAS38C                               99
022600980904IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
022700980904     C                   MOVEL     'S'           $FINE
022800980904X   2C                   ELSE
022900030807     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
023000980904E   2C                   ENDIF
023100980904E   1C                   ENDDO
023200980904     C*
023300980904     C                   ENDSR
023400980904     C*------------------------------------------------------------------------*
023500980904     C* CHKREC - CONTROLLA VALIDITA' RECORD                              -BOLLE-
023600980904     C*------------------------------------------------------------------------*
023700980904     C     CHKREC        BEGSR
023800980904     C*
023900980904     C* REIMPOSTA LE VARIABILI DI LAVORO
024000980904     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
024100980904     C                   MOVEL     *BLANKS       TASFIE            1
024200020403     C*
024300020403     C* PERIODO OLTRE IL RICHIESTO, ESCE
024400020403     C     TASDAT        IFGT      PARAMF
024500020403     C                   MOVEL     'S'           $FINE
024600020403     C                   ENDIF
024700980904     C*
024800980904     C* PERIODO NON RICHIESTO, ESCLUDE
024900980904IF  1C     TASDAT        IFGE      PARAMI
025000980904     C     TASDAT        ANDLE     PARAMF
025100980904X   1C                   ELSE
025200980904     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
025300980904E   1C                   ENDIF
025400980904     C*
025500980904     C* BOLLA NON VALIDA COME SPEDIZIONE, ESCLUDE
025600980904     C                   SETOFF                                           28
025700980904     C                   Z-ADD     1             I
025800980904     C     TASTBL        LOOKUP    TBL(I)                                 28
025900980904IF  1C     *IN28         IFEQ      *OFF                                         *NON TROVATO
026000980904     C     TFSP(I)       ORNE      'S'
026100980904     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
026200980904E   1C                   ENDIF
026300020403     C*
026400020403     C* RECUPERA IL PADRE DEL CLIENTE CORRENTE
026500020405IF  1C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
026600020404     C*
026700020404     C* VERIFICO LA ROTTURA DEL CLIENTE X OTTIMIZZARE L'ELABORAZIONE
026800020404     C                   IF        TASKSC <> DEPKSC
026900020404     C                   Z-ADD     TASKSC        DEPKSC
027000020403     C                   CLEAR                   TIBS10
027100020403     C                   Z-ADD     PARAMGF       D10DRF                         *DATA RIFERIMENTO
027200020403     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
027300020403     C                   MOVEL     'P'           D10PAF                         *CERCO IL SUO PADRE
027400020404     C                   Z-ADD     DEPKSC        D10COD                         *CODICE CLIENTE
027500020403     C                   CALL      'TIBS10R'
027600020403     C                   PARM                    TIBS10
027700020405     C                   PARM      'A'           PARTIPEXE
027800020403     C     D10ERR        IFEQ      *BLANKS                                      *SE PADRE
027900020405     C                   Z-ADD     D10COP        DEPCOP                         *CLIENTI FIGLI
028000020404     C                   ELSE
028100020404     C                   Z-ADD     DEPKSC        DEPCOP
028200020404     C                   ENDIF
028300980904     C*
028400980904     C* REPERICE I DATI DEL CLIENTE
028500981014     C                   CLEAR                   BS69DS
028600980904     C                   CLEAR                   CLPDS
028700980904     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
028800020404     C                   Z-ADD     DEPCOP        I69KCP                         *CODICE X CNCLP
028900980904     C                   CALL      'TIBS69R'
029000980904     C                   PARM                    BS69DS
029100980904     C                   PARM                    ACODS
029200980904     C                   PARM                    INDDS
029300980904     C                   PARM                    CLPDS
029400980904     C                   PARM                    CLSDS
029500980904IF  2C     O69ERR        IFEQ      '1'                                          *ERRORE
029600980904     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
029700980904E   2C                   ENDIF
029800980904     C*
029900980904     C* RECUPERA IL COMMERCIALE UNIFICANTE
030000980904     C                   Z-ADD     1             I
030100980904     C                   SETOFF                                           28
030200980904     C     CLPAGE        LOOKUP    AGE(I)                                 28
030300980904IF  2C     *IN28         IFEQ      *ON                                          *TROVATO
030400980904     C                   Z-ADD     1             C                 4 0
030500980904     C                   SETOFF                                           28
030600980904     C     ARGF(I)       LOOKUP    RGF(C)                                 28
030700980904IF  3C     *IN28         IFEQ      *ON                                          *TROVATO
030800980904     C                   Z-ADD     C             INDRGF
030900050414     c                   movel     rgf(C)        wpo               3 0
031000020405     C                   ELSE
031100020405     C                   Z-ADD     *ZEROS        INDRGF                         *NON TROVATO
031200980904E   3C                   ENDIF
031300020405     C                   ELSE
031400020405     C                   Z-ADD     *ZEROS        INDRGF                         *NON TROVATO
031500980904E   2C                   ENDIF
031600020405     C                   ENDIF
031700020405E   1C                   ENDIF
031800020405     C*
031900020405IF  1C     INDRGF        IFEQ      *ZEROS                                       *ERRORI
032000020405     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
032100020405E   1C                   ENDIF
032200980904     C*
032300980904     C* RECUPERA FLAG ITALIA/ESTERO
032400980904IF  1C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
032500980904     C                   Z-ADD     1             I
032600980904     C                   SETOFF                                           28
032700980904     C     TASLNP        LOOKUP    FIL(I)                                 28
032800980904IF  2C     *IN28         IFEQ      *ON                                          *TROVATO
032900980904     C                   Z-ADD     1             J                 4 0
033000980904     C                   SETOFF                                           28
033100980904     C     TASLNA        LOOKUP    FIL(J)                                 28
033200980904IF  3C     *IN28         IFEQ      *ON                                          *TROVATO
033300980904IF  4C     FFL1(I)       IFEQ      'E'                                          *LNP ESTERO  O
033400980904     C     FFL1(J)       OREQ      'E'                                          *LNA ESTERO
033500980904     C                   MOVEL     'E'           TASFIE                         *BOLLA ESTERO
033600980904X   4C                   ELSE
033700980904     C                   MOVEL     'I'           TASFIE                         *BOLLA ITALIA
033800980904E   4C                   ENDIF
033900980904E   3C                   ENDIF
034000980904E   2C                   ENDIF
034100980904E   1C                   ENDIF
034200980904IF  1C     TASFIE        IFEQ      *BLANKS                                      *ERRORI
034300980904     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
034400980904E   1C                   ENDIF
034500980904     C*
034600980904     C                   ENDSR
034700980904     C*------------------------------------------------------------------------*
034800980904     C* MEMTAS - MEMORIZZA I DATI                                        -BOLLE-
034900980904     C*------------------------------------------------------------------------*
035000980904     C     MEMTAS        BEGSR
035100980904     C*
035200980904     C* RECUPERA IL MESE
035300980904     C                   SETOFF                                           28
035400980904     C                   Z-ADD     1             M                 2 0
035500980904     C     TASMES        LOOKUP    MMM(M)                                 28
035600980904IF  1C     *IN28         IFEQ      *ON                                          *TROVATO
035700980904     C*
035800980904     C* SI POSIZIONA SUL COMMERCIALE UNIFICANTE
035900980904     C     INDRGF        OCCUR     UDAT                                         *POSIZIONAMENTO
036000981008     C                   MOVEL     'S'           $CTR(M)                        *MEMORIZZATO QC
036100980904     C*---
036200030805     C* ITALIA
036300980904     C*---
036400980904IF  2C     TASFIE        IFEQ      'I'
036500980904     C* SPEDIZIONI NO DDT
036600980904IF  3C     TASLL1        IFNE      'C'
036700980904     C     TASLL1        ANDNE     'S'
036800980904     C                   ADD       1             ISDN(M)
036900980904E   3C                   ENDIF
037000030805     C* SPEDIZIONI DISK A/B/C
037100030805     C*
037200030805     C* VERIFICO SE BOLLA DISK C (E CHE TIPO DI DISK C: C=CLIENTE B=BARTOLINI) O A+C
037300030805     C                   Z-ADD     1             II
037400030805     C                   CLEAR                   WTMT
037500030805     C     TASCTM        LOOKUP    TMC(II)                                36
037600030805     C   36              MOVE      TMT(II)       WTMT              1
037700030805     C*
037800030805     C* VERIFICO PARTICOLARITA' IMPORT DPD (OVVERO LINEA DI PARTENZA DPD)
037900030805     C     TASLNP        LOOKUP    FDPD                                   55
038000030805     C                   IF        *IN55 = *ON
038100030805     C     TASCCM        IFGT      *ZEROS
038200030805     C                   MOVEL(P)  TASCCM        KTBKEY
038300030805     C                   ELSE
038400030805     C                   MOVEL(P)  TASKSC        KTBKEY
038500030805     C                   ENDIF
038600030805     C     KEYSAV        IFNE      KTBKEY
038700030805     C                   MOVE      KTBKEY        KEYSAV
038800030805     C                   MOVEL     '3C'          KTBCOD
038900030805     C     KEYTAB        CHAIN     TABEL00F                           98
039000030805     C  N98              MOVEL     TBLUNI        DS3C
039100030805     C   98              CLEAR                   DS3C
039200030805     C                   ENDIF
039300030805     C     §3CDKC        IFEQ      'S'
039400030805     C                   SETON                                        36
039500030805     C                   MOVEL     'C'           WTMT
039600030805     C                   ENDIF
039700030805     C                   ENDIF
039800030807     C*
039900030807     C* VERIFICO SE NO DISK
040000030807     C     TASSOP        IFNE      '§'
040100030807     C     TASSOP        ANDNE     '%'
040200030807     C     *IN36         ANDEQ     *OFF
040300030807     C                   ELSE
040400030805     C*
040500030805     C* VERIFICO SE DISK B (OVVERO E' PRESENTE LA SERIE)
040600030805     C                   IF        TASNRS <> *ZEROS
040700030805     C                   ADD       1             ISAB(M)
040800030807     C                   ADD       TASNCL        ICDB(M)
040900030805     C                   ELSE
041000030805     C* VERIFICO SE DISK A
041100030805     C                   IF        *IN36 = *OFF
041200030805     C                   ADD       1             ISAB(M)
041300030805     C                   ELSE
041400030805     C* VERIFICO SE DISK C BARTOLINI
041500030805     C     WTMT          IFEQ      'B'
041600030805     C     TASSOP        IFEQ      ':'
041700030805     C                   ADD       1             ISAB(M)
041800030805     C                   ADD       1             ISCB(M)
041900050408     C                   ADD       TASNCL        IccB(M)
042000030805     C                   ELSE
042100030805     C                   ADD       1             ISCB(M)
042200050408     C                   ADD       TASNCL        IccB(M)
042300030805     C                   ENDIF
042400030805     C                   ELSE
042500030805     C* VERIFICO SE DISK C CLIENTE
042600030805     C                   ADD       1             ISAB(M)
042700030805     C                   ADD       1             ISCC(M)
042800050408     C                   ADD       TASNCL        Iccc(M)
042900050414     c                   if        parsta='S'
043000050414     c                   if        wpo=71 or wpo=72 or wpo=74 or wpo=63 or
043100050414     c                             wpo=103 or wpo=109 or wpo=114 or wpo=123 or
043200050414     c                             wpo=124 or wpo=255
043300050414     c                   except    bolSCC
043400030805     C                   ENDIF
043500050414     C                   ENDIF
043600050414     c*
043700050414     C                   ENDIF
043800030805     C                   ENDIF
043900030805     C                   ENDIF
044000030805E   3C                   ENDIF
044100030807E   2C                   ENDIF
044200980904     C*---
044300980904     C* ESTERO
044400980904     C*---
044500980904IF  2C     TASFIE        IFEQ      'E'
044600980904     C* SPEDIZIONI NO DDT
044700980904IF  3C     TASLL1        IFNE      'C'
044800980904     C     TASLL1        ANDNE     'S'
044900980904     C                   ADD       1             ESDN(M)
045000980904E   3C                   ENDIF
045100030807     C* SPEDIZIONI DISK A/B/C
045200030805     C*
045300030805     C* VERIFICO SE BOLLA DISK C (E CHE TIPO DI DISK C: C=CLIENTE B=BARTOLINI) O A+C
045400030805     C                   Z-ADD     1             II
045500030805     C                   CLEAR                   WTMT
045600030805     C     TASCTM        LOOKUP    TMC(II)                                36
045700030805     C   36              MOVE      TMT(II)       WTMT              1
045800030805     C*
045900030805     C* VERIFICO PARTICOLARITA' IMPORT DPD (OVVERO LINEA DI PARTENZA DPD)
046000030805     C     TASLNP        LOOKUP    FDPD                                   55
046100030805     C                   IF        *IN55 = *ON
046200030805     C     TASCCM        IFGT      *ZEROS
046300030805     C                   MOVEL(P)  TASCCM        KTBKEY
046400030805     C                   ELSE
046500030805     C                   MOVEL(P)  TASKSC        KTBKEY
046600030805     C                   ENDIF
046700030805     C     KEYSAV        IFNE      KTBKEY
046800030805     C                   MOVE      KTBKEY        KEYSAV
046900030805     C                   MOVEL     '3C'          KTBCOD
047000030805     C     KEYTAB        CHAIN     TABEL00F                           98
047100030805     C  N98              MOVEL     TBLUNI        DS3C
047200030805     C   98              CLEAR                   DS3C
047300030805     C                   ENDIF
047400030805     C     §3CDKC        IFEQ      'S'
047500030805     C                   SETON                                        36
047600030805     C                   MOVEL     'C'           WTMT
047700030805     C                   ENDIF
047800030805     C                   ENDIF
047900030807     C*
048000030807     C* VERIFICO SE NO DISK
048100030807     C     TASSOP        IFNE      '§'
048200030807     C     TASSOP        ANDNE     '%'
048300030807     C     *IN36         ANDEQ     *OFF
048400030807     C                   ELSE
048500030805     C*
048600030805     C* VERIFICO SE DISK B (OVVERO E' PRESENTE LA SERIE)
048700030805     C                   IF        TASNRS <> *ZEROS
048800030805     C                   ADD       1             ESAB(M)
048900030807     C                   ADD       TASNCL        ECDB(M)
049000030805     C                   ELSE
049100030805     C* VERIFICO SE DISK A
049200030805     C                   IF        *IN36 = *OFF
049300030805     C                   ADD       1             ESAB(M)
049400030805     C                   ELSE
049500030805     C* VERIFICO SE DISK C BARTOLINI
049600030805     C     WTMT          IFEQ      'B'
049700030805     C     TASSOP        IFEQ      ':'
049800030805     C                   ADD       1             ESAB(M)
049900030805     C                   ADD       1             ESCB(M)
050000050408     C                   ADD       TASNCL        ECcB(M)
050100030805     C                   ELSE
050200030805     C                   ADD       1             ESCB(M)
050300050408     C                   ADD       TASNCL        ECcB(M)
050400030805     C                   ENDIF
050500030805     C                   ELSE
050600030805     C* VERIFICO SE DISK C CLIENTE
050700030805     C                   ADD       1             ESAB(M)
050800030805     C                   ADD       1             ESCC(M)
050900050408     C                   ADD       TASNCL        ECcc(M)
051000050414     c                   if        parsta='S'
051100050414     c                   if        wpo=71 or wpo=72 or wpo=74 or wpo=63 or
051200050414     c                             wpo=103 or wpo=109 or wpo=114 or wpo=123 or
051300050414     c                             wpo=124 or wpo=255
051400050414     c                   except    bolSCC
051500050414     C                   ENDIF
051600050414     C                   ENDIF
051700050414     c
051800030805     C                   ENDIF
051900030805     C                   ENDIF
052000030805     C                   ENDIF
052100980904E   3C                   ENDIF
052200980904E   2C                   ENDIF
052300980904E   1C                   ENDIF
052400980904     C*
052500980904     C                   ENDSR
052600980904     C*------------------------------------------------------------------------*
052700980904     C* SCRREP - SCRIVE UN RECORD PER IL COMMERCIALE UNIFICANTE
052800980904     C*------------------------------------------------------------------------*
052900980904     C     SCRREP        BEGSR
053000980904     C*
053100980904     C* PER CIASCUN COMMERCIALE UNIFICANTE MEMORIZZATO
053200150107DO  1C     1             DO        c_Max_CmmU    C
053300980904IF  2C     RGF(C)        IFNE      *ZEROS
053400980904     C     C             OCCUR     UDAT                                         *POSIZIONAMENTO
053500980904     C*
053600980904     C* IMPOSTA LA CHIAVE DI LETTURA
053700980904     C                   EXSR      REPAGE                                       *RECUPERA DATI AGENT
053800980904     C                   MOVEL     DEPDIV        KEPDIV                         *DIVISIONE
053900980904     C                   Z-ADD     DEPARE        KEPARE                         *AREA
054000980904     C                   Z-ADD     DEPFIL        KEPFIL                         *FILIALE
054100980904     C                   Z-ADD     RGF(C)        KEPRGF                         *COMMERCIALE
054200980904     C*
054300981117     C* PER CIASCUN MESE MEMORIZZATO, SCRIVE I DATI SOLO SE ESISTE
054400980904DO  3C     1             DO        12            M
054500980904IF  4C     MMM(M)        IFNE      *ZEROS
054600981117IF  5C     $CTR(M)       IFEQ      'S'                                          *MEMORIZZATO QC.
054700980904     C                   Z-ADD     AAA(M)        KEPANN                         *ANNO
054800980904     C                   Z-ADD     MMM(M)        KEPMES                         *MESE
054900050408     C     KEYREP        CHAIN     WFREP11L                           97
055000981117IF  6C     *IN97         IFEQ      *OFF                                         *MODIFICA
055100980904     C                   EXSR      IMPREP                                       *IMPOSTA CAMPI
055200980904     C                   UPDATE    WFREP000                                     *AGGIORNA
055300981117E   6C                   ENDIF
055400981117E   5C                   ENDIF
055500980904E   4C                   ENDIF
055600980904E   3C                   ENDDO                                                  *FINE PERIODO
055700980904E   2C                   ENDIF
055800980904E   1C                   ENDDO                                                  *FINE COMMERCIALI
055900980904     C*
056000980904     C                   ENDSR
056100980904     C*------------------------------------------------------------------------*
056200980904     C* IMPREP - IMPOSTA UN RECORD PER IL REPORT
056300980904     C*------------------------------------------------------------------------*
056400980904     C     IMPREP        BEGSR
056500981013     C* ITALIA
056600980904     C                   Z-ADD     ISDN(M)       REISDN
056700980904     C                   Z-ADD     ISAB(M)       REISAB
056800980904     C                   Z-ADD     ICDB(M)       REICDB
056900030807     C                   Z-ADD     ISCB(M)       REISCB
057000030807     C                   Z-ADD     ISCC(M)       REISCC
057100050408     C                   Z-ADD     IcCB(M)       REIcCB
057200050408     C                   Z-ADD     IcCC(M)       REIcCC
057300980904     C* ESTERO
057400980904     C                   Z-ADD     ESDN(M)       REESDN                         *CAMPI DA VALORIZZAR
057500980904     C                   Z-ADD     ESAB(M)       REESAB
057600980904     C                   Z-ADD     ECDB(M)       REECDB
057700030807     C                   Z-ADD     ESCB(M)       REESCB
057800030807     C                   Z-ADD     ESCC(M)       REESCC
057900050408     C                   Z-ADD     EcCB(M)       REEcCB
058000050408     C                   Z-ADD     EcCC(M)       REEcCC
058100980904     C*
058200980904     C                   ENDSR
058300980904     C*------------------------------------------------------------------------*
058400980904     C* REPAGE - RECUPERA I DATI DELL'AGENTE
058500980904     C*------------------------------------------------------------------------*
058600980904     C     REPAGE        BEGSR
058700980904     C*
058800980904     C* REIMPOSTA LE VARIABILI DI LAVORO
058900980904     C                   MOVEL     *BLANKS       DEPDIV
059000980904     C                   Z-ADD     *ZEROS        DEPARE
059100980904     C                   Z-ADD     *ZEROS        DEPFIL
059200980904     C*
059300980904     C                   Z-ADD     1             I
059400980904     C                   SETOFF                                           28
059500980904     C                   MOVEL     RGF(C)        DEPFIL                         *FILIALE
059600980904     C     DEPFIL        LOOKUP    FIL(I)                                 28
059700980904IF  1C     *IN28         IFEQ      *ON                                          *TROVATO
059800980904     C                   Z-ADD     FARE(I)       DEPARE                         *AREA
059900980904     C                   MOVEL     FDIV(I)       DEPDIV                         *DIVISIONE
060000980904E   1C                   ENDIF
060100980904     C*
060200980904     C                   ENDSR
060300980904     C*------------------------------------------------------------------------*
060400980904     C* CARTAB - CARICAMENTO TABELLE OCCORRENTI
060500980904     C*------------------------------------------------------------------------*
060600980904     C     CARTAB        BEGSR
060700980904     C*---
060800980904     C* ORGANIGRAMMA
060900980904     C*---
061000980904     C                   Z-ADD     *ZEROS        I                 4 0
061100980904     C     *LOVAL        SETLL     AZORG01L
061200980904     C                   READ      AZORG01L                               99
061300980904DO  1C     *IN99         DOWEQ     *OFF
061400980904IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
061500980904IF  3C     ORGFAG        IFNE      'V'                                          *NO "VARIE"
061600980904     C                   ADD       1             I
061700980904     C                   Z-ADD     ORGFIL        FIL(I)                         *FILIALE
061800000216     C                   CLEAR                   OG143
061900000216     C                   MOVEL     ORGDE3        OG143
062000020702     C*
062100020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
062200020702     C                   CLEAR                   DNTW
062300020702     C*
062400020702     C                   MOVEL(P)  'NTW'         KTECOD
062500020702     C                   MOVEL(P)  §OGNTW        KTEKE1
062600020702     C     KEYTBE        CHAIN     TNTBE01L
062700020702     C                   IF        %found(TNTBE01L)
062800020702     C                   MOVEL     TBEUNI        DNTW
062900020702     C                   ENDIF
063000020702     C*
063100020702     C                   MOVEL     §NTWFIE       FFL1(I)
063200020702     C                   IF        §OGNTW = 'DPD'
063300030805     C                   Z-ADD     ORGFIL        FDPD(I)                        *FILIALE DPD
063400020702     C                   ENDIF
063500020702     C*
063600980904     C                   Z-ADD     ORGCAR        FARE(I)                        *AREA
063700980904     C                   MOVEL     ORGFL3        FDIV(I)                        *DIVISIONE
063800980904E   3C                   ENDIF
063900980904E   2C                   ENDIF
064000980904     C                   READ      AZORG01L                               99
064100980904E   1C                   ENDDO
064200980904     C*---
064300980904     C* AGENTI UNIFICANTI
064400980904     C*---
064500140613     C********           Z-ADD     *ZEROS        I                 4 0
064600140613     C********           Z-ADD     1             KTBKUT
064700140613     C********           MOVEL     '01'          KTBCOD
064800140613     C***  KE2TAB        SETLL     TABEL00F
064900140613     C***  KE2TAB        READE     TABEL00F                               99
065000140613DO  1C***  *IN99         DOWEQ     *OFF
065100140613IF  2C***  TBLFLG        IFEQ      *BLANKS
065200140613     C********           MOVEL     TBLKEY        N7                7 0
065300140613     C********           MOVEL     TBLUNI        DS01
065400140613IF  3C***  N7            IFEQ      §01RGF
065500140613     C********           ADD       1             I
065600140613     C********           Z-ADD     §01RGF        RGF(I)                         *AGENTE UNIFICANTE
065700140613     C********           MOVEL     §01AGE        DRGF(I)                        *DESCRIZIONE
065800140613E   3C********           ENDIF
065900140613E   2C********           ENDIF
066000140613     C***  KE2TAB        READE     TABEL00F                               99
066100140613E   1C********           ENDDO
066200980904     C*---
066300980904     C* AGENTI
066400980904     C*---
066500140613     C*********          Z-ADD     *ZEROS        I                 4 0
066600140613     C*********          Z-ADD     1             KTBKUT
066700140613     C*********          MOVEL     '01'          KTBCOD
066800140613     C***  KE2TAB        SETLL     TABEL00F
066900140613     C***  KE2TAB        READE     TABEL00F                               99
067000140613DO  1C***  *IN99         DOWEQ     *OFF
067100140613IF  2C***  TBLFLG        IFEQ      *BLANKS
067200140613     C*********          ADD       1             I
067300140613     C*********          MOVEL     TBLUNI        DS01
067400140613     C*********          MOVEL     TBLKEY        AGE(I)                         *AGENTE
067500140613     C*********          Z-ADD     §01RGF        ARGF(I)                        *AGENTE UNIFICANTE
067600140613E   2C*********          ENDIF
067700140613     C***  KE2TAB        READE     TABEL00F                               99
067800140613E   1C*********          ENDDO
067900140613      *********
068000140613     C*---
068100140613     C* AGENTI e UNIFICANTI
068200140613     C*---
068300140613     C                   Z-ADD     *ZEROS        I                 4 0
068400140613     C                   Z-ADD     *ZEROS        U                 4 0
068500140613     C                   open      azCMM01l
068600140613      *-
068700140613     C     *loval        SETLL     azCMM01l
068800140613     C                   READ      azCMM01l
068900140613DO  1C                   DOW         not %Eof(azCMM01l)
069000140613IF  2C                   IF           cmmATB = *blank
069100140613     C                   ADD       1             I
069200140613     C                   z-add     cmmCOD        AGE(I)                         *AGENTE
069300140613     C                   Z-ADD     cmmUNI        ARGF(I)                        *AGENTE UNIFICANTE
069400140613      *-
069500140613IF  3C     cmmCOD        IFEQ      cmmUNI
069600140613     C                   ADD       1             U
069700140613     C                   Z-ADD     cmmUNI        RGF(U)                         *AGENTE UNIFICANTE
069800140613     C                   MOVEL     cmmDES        DRGF(U)                        *DESCRIZIONE
069900140613E   3C                   ENDIF
070000140613      *-
070100140613E   2C                   ENDIF
070200140613     C                   READ      azCMM01l
070300140613E   1C                   ENDDO
070400140613     C                   close     azCMM01l
070500980904     C*---
070600980904     C* TIPI BOLLA
070700980904     C*---
070800980904     C                   Z-ADD     *ZEROS        I                 4 0
070900020906     C                   Z-ADD     1             KTBKUT
071000980904     C                   MOVEL     'TB'          KTBCOD
071100980904     C     KE2TAB        SETLL     TABEL00F
071200980904     C     KE2TAB        READE     TABEL00F                               99
071300980904DO  1C     *IN99         DOWEQ     *OFF
071400980904IF  2C     TBLFLG        IFEQ      *BLANKS
071500980904     C                   MOVEL     TBLUNI        DSTB
071600980904     C                   ADD       1             I
071700980904     C                   MOVEL     TBLKEY        TBL(I)
071800980904     C                   MOVEL     §TBRBL        TRBL(I)
071900980904E   2C                   ENDIF
072000980904     C     KE2TAB        READE     TABEL00F                               99
072100980904E   1C                   ENDDO
072200980904     C*---
072300980904     C* FLAG TIPI BOLLA
072400980904     C*---
072500980904DO  1C     1             DO        20            I
072600980904IF  2C     TBL(I)        IFNE      *BLANKS
072700020906     C                   Z-ADD     1             KTBKUT
072800980904     C                   MOVEL     '2W'          KTBCOD
072900980904     C                   MOVEL(P)  TRBL(I)       KTBKEY
073000980904     C     KEYTAB        CHAIN     TABEL00F                           99
073100980904IF  3C     *IN99         IFEQ      *OFF
073200980904     C                   MOVEL     TBLUNI        DS2W
073300980904     C                   MOVEL     §2WFSP        TFSP(I)                        *FLAG SPEDIZIONI
073400980904     C                   MOVEL     §2WFRI        TFRI(I)                        *FLAG RICAVI
073500980904E   3C                   ENDIF
073600980904E   2C                   ENDIF
073700980904E   1C                   ENDDO                                                  *FINE TIPI BOLLA
073800030805     C*
073900030805     C* CARICO TABELLA CTM DISK C
074000030805     C                   Z-ADD     1             KTBKUT
074100030805     C                   MOVE      '1B'          KTBCOD
074200030805     C                   CLEAR                   II
074300030805     C     KE2TAB        SETLL     TABEL00F
074400030805     C     KE2TAB        READE     TABEL00F                               98
074500030805     C     *IN98         DOWEQ     *OFF
074600030805     C                   MOVEL     TBLUNI        DS1B
074700030805     C     §1BDKC        IFEQ      'B'
074800030805     C     §1BDKC        OREQ      'C'
074900030805     C                   ADD       1             II                3 0
075000030805     C                   MOVEL     TBLKEY        TMC(II)
075100030805     C                   MOVEL     §1BDKC        TMT(II)
075200030805     C                   ENDIF
075300030805     C     KE2TAB        READE     TABEL00F                               98
075400030805     C                   ENDDO
075500980904     C*
075600980904     C                   ENDSR
075700980904     C*------------------------------------------------------------------------*
075800980904     C* CARMES - CARICA I MESI/ANNI DA ELEBORARE
075900980904     C*------------------------------------------------------------------------*
076000980904     C     CARMES        BEGSR
076100980904     C*
076200980904     C* REIMPOSTA LE VARIABILI DI LAVORO
076300980904     C                   CLEAR                   AAA                            *ANNI
076400980904     C                   CLEAR                   MMM                            *MESI
076500980904     C                   Z-ADD     *ZEROS        J                 4 0          *N° MESI DA ELABORAR
076600980904     C                   Z-ADD     PARAAF        WAA4              4 0          *ANNO FINALE LAVORO
076700980904     C                   Z-ADD     PARMMF        WMM2              2 0          *MESE FINALE LAVORO
076800980904     C                   Z-ADD     PARAAI        WAA               4 0          *ANNO INIZIALE LAVOR
076900980904     C                   Z-ADD     PARMMI        WMM               2 0          *MESE INIZIALE LAVOR
077000980904     C*
077100980904     C* ANNO INIZIALE UGUALE AD ANNO FINALE
077200980904IF  1C     WAA4          IFEQ      WAA
077300980904     C*
077400980904     C* CALCOLO I MESI DI DIFFERENZA
077500980904DO  2C     WMM           DOWLE     WMM2
077600980904     C                   ADD       1             J
077700980904     C                   Z-ADD     WAA           AAA(J)
077800980904     C                   Z-ADD     WMM           MMM(J)
077900980904     C                   ADD       1             WMM
078000980904E   2C                   ENDDO
078100980904     C*
078200980904     C* IL RESTO DELLA SCHIERA A ZERO
078300980904DO  2C     J             DOWLT     12
078400980904     C                   ADD       1             J
078500980904     C                   Z-ADD     *ZEROS        AAA(J)
078600980904     C                   Z-ADD     *ZEROS        MMM(J)
078700980904E   2C                   ENDDO
078800980904     C*
078900980904     C* ANNO FINALE DIVERSO DA ANNO INIZIALE
079000980904X   1C                   ELSE
079100980904     C*
079200980904     C* MEMORIZZO I MESI DELL'ANNO INIZIALE
079300980904DO  2C     WMM           DOWLE     12
079400980904     C                   ADD       1             J
079500980904     C                   Z-ADD     WAA           AAA(J)
079600980904     C                   Z-ADD     WMM           MMM(J)
079700980904     C                   ADD       1             WMM
079800980904E   2C                   ENDDO
079900980904     C*
080000980904     C* MEMORIZZO I MESI DELL'ANNO FINALE
080100980904     C                   Z-ADD     01            WMM
080200980904DO  2C     WMM           DOWLE     WMM2
080300980904     C                   ADD       1             J
080400980904     C                   Z-ADD     WAA4          AAA(J)
080500980904     C                   Z-ADD     WMM           MMM(J)
080600980904     C                   ADD       1             WMM
080700980904E   2C                   ENDDO
080800980904     C*
080900980904     C* IL RESTO DELLA SCHIERA A ZERO
081000980904DO  2C     J             DOWLT     12
081100980904     C                   ADD       1             J
081200980904     C                   Z-ADD     *ZEROS        AAA(J)
081300980904     C                   Z-ADD     *ZEROS        MMM(J)
081400980904E   2C                   ENDDO
081500980904E   1C                   ENDIF
081600980904     C*
081700980904     C                   ENDSR
081800980904     C*------------------------------------------------------------------------*
081900980904     C* FINSR - OPERAZIONI FINALI
082000980904     C*------------------------------------------------------------------------*
082100980904     C     FINSR         BEGSR
082200980904     C*
082300980904     C* CHIUDE I FILE DEI PGM CHIAMATI
082400980904     C                   CLEAR                   BS69DS
082500980904     C                   CLEAR                   ACODS
082600980904     C                   CLEAR                   INDDS
082700980904     C                   CLEAR                   CLPDS
082800980904     C                   CLEAR                   CLSDS
082900980904     C                   MOVEL     'C'           I69TLA                         *TIPO LANCIO
083000980904     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
083100980904     C                   CALL      'TIBS69R'
083200980904     C                   PARM                    BS69DS
083300980904     C                   PARM                    ACODS
083400980904     C                   PARM                    INDDS
083500980904     C                   PARM                    CLPDS
083600980904     C                   PARM                    CLSDS
083700020405     C*
083800020405     C                   CLEAR                   TIBS10
083900020405     C                   CALL      'TIBS10R'
084000020405     C                   PARM                    TIBS10
084100020405     C                   PARM      'C'           PARTIPEXE         1
084200980904     C*
084300980904     C                   ENDSR
084400980904     C*------------------------------------------------------------------------*
084500980904     C* *INZSR - OPERAZIONI INIZIALI
084600980904     C*------------------------------------------------------------------------*
084700980904     C     *INZSR        BEGSR
084800980904     C*
084900980904     C* RICEVIMENTO PARAMETRI
085000980904     C     *ENTRY        PLIST
085100980904     C                   PARM                    KPJBA
085200980904     C                   MOVEL     KPJBU         PARAM
085300980904     C*
085400980904     C* VARIABILI RIFERITE AL DATABASE
085500980904     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
085600980904     C     *LIKE         DEFINE    TBLCOD        KTBCOD
085700980904     C     *LIKE         DEFINE    TBLKEY        KTBKEY
085800990802     C     *LIKE         DEFINE    TASAAS        KASAAS                         *TITAS00F
085900981215     C     *LIKE         DEFINE    TASMGS        KASMGS
086000050408     C     *LIKE         DEFINE    REPDIV        KEPDIV                         *WFREP10F
086100980904     C     *LIKE         DEFINE    REPARE        KEPARE
086200980904     C     *LIKE         DEFINE    REPFIL        KEPFIL
086300980904     C     *LIKE         DEFINE    REPRGF        KEPRGF
086400980904     C     *LIKE         DEFINE    REPANN        KEPANN
086500980904     C     *LIKE         DEFINE    REPMES        KEPMES
086600980904     C     *LIKE         DEFINE    REPDIV        DEPDIV                         *DEPOSITI
086700980904     C     *LIKE         DEFINE    REPARE        DEPARE
086800980904     C     *LIKE         DEFINE    REPFIL        DEPFIL
086900020404     C     *LIKE         DEFINE    TASKSC        DEPKSC
087000020404     C     *LIKE         DEFINE    TASKSC        DEPCOP
087100020702     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
087200020702     C     *LIKE         DEFINE    TBEKE1        KTEKE1
087300030805     C     *LIKE         DEFINE    KTBKEY        KEYSAV
087400980904     C*
087500030805     C* CHIAVI DI LETTURA
087600980904     C     KEYTAB        KLIST                                                  *TABEL00F
087700980904     C                   KFLD                    KTBKUT                          -UTENTE
087800980904     C                   KFLD                    KTBCOD                          -TABELLA
087900980904     C                   KFLD                    KTBKEY                          -UTENTE
088000980904     C     KE2TAB        KLIST                                                  *TABEL00F
088100980904     C                   KFLD                    KTBKUT                          -UTENTE
088200980904     C                   KFLD                    KTBCOD                          -TABELLA
088300050408     C     KEYTAS        KLIST                                                  *WFREP10F
088400981215     C                   KFLD                    KASAAS                          -ANNO
088500981215     C                   KFLD                    KASMGS                          -MESE/GIORNO
088600050408     C     KEYREP        KLIST                                                  *WFREP10F
088700980904     C                   KFLD                    KEPDIV                          -DIVISIONE
088800980904     C                   KFLD                    KEPARE                          -AREA
088900980904     C                   KFLD                    KEPFIL                          -FILIALE
089000980904     C                   KFLD                    KEPRGF                          -AGENTE
089100980904     C                   KFLD                    KEPANN                          -ANNO
089200980904     C                   KFLD                    KEPMES                          -MESE
089300020702     C     KEYTBE        KLIST                                                  *TNTBE01L
089400020702     C                   KFLD                    KTECOD                          -CODICE
089500020702     C                   KFLD                    KTEKE1                          -CHIAVE
089600980904     C*
089700980904     C* VARIABILI DI CONTROLLO
089800980904     C                   MOVEL     'N'           $FINE             1            *FINE PGM
089900980904     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
090000020405     C                   Z-ADD     *ZEROS        INDRGF            4 0
090100020403     C*
090200020403     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
090300020403     C                   MOVEL     PARAMF        PARAMGF           8 0
090400020403     C                   MOVE      '28'          PARAMGF
090500081119     C*
090600081119     C*------- GIORNO    di    F I N E    M E S E ---------------------------*
090700081119     C*
090800081119     C*  Ultimo giorno del mese
090900081119     C                   MOVE      PARAMGF       MMGG_4            4 0
091000081119     C                   MOVEL     MMGG_4        Mm_2              2 0
091100081119     C                   MOVEl     PARAMGF       Anno_4            4 0
091200081119      * se è febbraio
091300081119     c                   If        Mm_2 = 2
091400081119      *
091500081119      *  potrebbe essere bisestile 29 gg.
091600081119     c     Anno_4        div       4             risulta           3 0
091700081119     c                   mvr                     resto             3 0
091800081119      *  Se bisestile
091900081119     c                   If        resto = 0
092000081119     C                   MOVE      '29'          PARAMGF
092100081119     c                   end
092200081119      *
092300081119     c                   else
092400081119      *
092500081119      *  se invece Aprile, Giugno, Settembre, Novembre con 30 gg.
092600081119     c                   If           Mm_2 = 4 or Mm_2 = 6 or
092700081119     c                                Mm_2 = 9 or Mm_2 = 11
092800081119     C                   MOVE      '30'          PARAMGF
092900081119     c                   else
093000081119     C                   MOVE      '31'          PARAMGF
093100081119     c                   end
093200081119      *
093300081119     c                   endIf
093400081119     C*
093500081119     C*----------------------- F I N E    M E S E ---------------------------*
093600081119     C*
093700980904     C*
093800980904     C* CARICAMENTO TABELLE OCCORRENTI
093900980904     C                   EXSR      CARTAB
094000980904     C*
094100980904     C* CARICAMENTO MESI/ANNI DA ELABORARE
094200980904     C                   EXSR      CARMES
094300980904     C*
094400980904     C                   ENDSR
094500050414     OQSYSPRT   E            bolSCC      1
094600050414     O                                        +   0 'Bolle di REISCC e REESCC  '
094700050414     O                       tasAAS            +  2
094800050414     O                       tasLNP            +  1
094900050414     O                       tasNRS            +  1
095000050414     O                       tasNSP        2   +  1
095100050414     O                       taslna            +  1
095200050414     O                       tasmgs            +  1 '  /  '
095300050414     O                       tasksc            +  1
095400050414     o                                         +  1 'Colli'
095500050414     O                       tasncl            +  1
