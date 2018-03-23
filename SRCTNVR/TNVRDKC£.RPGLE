000100980904     H*------------------------------------------------------------------------*
000200980904     H* REPORT COMMERCIALE - CREAZIONE REPORT - PARTE 2^                       *
000300980904     H*------------------------------------------------------------------------*
000400980904     H DECEDIT('0,') DATEDIT(*DMY.)
000500980904     F*------------------------------------------------------------------------*
000600980904     F* DATA BASE
000700980904     F*------------------------------------------------------------------------*
000800030807     F*---
000900980904     F* WORKFILE
001000980904     F*---
001100050408     FWFREP11L  UF   E           K DISK
001200980904     F*---
001300980904     F* BOLLE
001400980904     F*---
001500990802     FTITAS38C  IF   E           K DISK
001600990802     F                                     IGNORE(TITAS000)
001700000519     F                                     IGNORE(TITASP00)
001800980904     F*---
001900980904     F* ORGANIGRAMMA
002000980904     F*---
002100980904     FAZORG01L  IF   E           K DISK
002200980904     F*---
002300980904     F* TABELLE
002400980904     F*---
002500980904     FTABEL00F  IF   E           K DISK
002600020702     FTNTBE01L  IF   E           K DISK
002700050414     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002800980904     D*------------------------------------------------------------------------*
002900980904     D* SCHIERE
003000980904     D*------------------------------------------------------------------------*
003100980904     D*---
003200980904     D* PERIODO DA ELABORARE
003300980904     D*---
003400980904     D AAA             S              4  0 DIM(12)                              *ANNI
003500980904     D MMM             S              2  0 DIM(12)                              *MESI
003600980904     D*---
003700980904     D* ORGANIGRAMMA
003800980904     D*---
003900980904     D FIL             S              3  0 DIM(500)                             *FILIALE
004000030805     D FDPD            S              3  0 DIM(500)                             *DPD
004100980904     D FFL1            S              1    DIM(500)                             *ITALIA/ESTERO
004200980904     D FARE            S              3  0 DIM(500)                             *AREA
004300980904     D FDIV            S              1    DIM(500)                             *DIVISIONE
004400980904     D*---
004500980904     D* TIPI BOLLA
004600980904     D*---
004700980904     D TBL             S              2    DIM(20)                              *TIPO BOLLA
004800980904     D TRBL            S              1    DIM(20)                              *RAGGRUPPAMENTO
004900980904     D TFSP            S              1    DIM(20)                              *FLAG SPEDIZIONI (S/
005000980904     D TFRI            S              1    DIM(20)                              *FLAG RICAVI     (S/
005100030805     D*---
005200030805     D* CODICI TRATTAMENTO MERCE
005300030805     D*---
005400030805     D TMC             S              2    DIM(100)
005500030805     D TMT             S              1    DIM(100)
005600980904     D*---
005700980904     D* COMMERCIALI UNIFICANTI
005800980904     D*---
005900100202     D RGF             S              7  0 DIM(5000)                            *CODICE
006000100202     D DRGF            S             25    DIM(5000)                            *DESCRIZIONE
006100980904     D*---
006200980904     D* COMMERCIALI
006300980904     D*---
006400100202     D AGE             S              7  0 DIM(5000)                            *AGENTE
006500100202     D ARGF            S              7  0 DIM(5000)                            *AGENTE UNIFICANTE
006600980904     D*---
006700980904     D* SCHIERE DI MEMORIZZAZIONE                        -MESI-
006800980904     D*---
006900980904     D* MULTIPLE                                         -2000 COMMERCIALI-
007000980904     D*------------------------------------------------------------------------*
007100980904     D* INPUT
007200980904     D*------------------------------------------------------------------------*
007300980904     D* (11:2)+1=6,5 ; (6x12) = 72
007400980904     D*---
007500980904     D* DS MULTIPLA FASCE DI PESO PER TOTALI GENERALI     - 10 FASCE X 12 MESI -
007600980904     D*---
007700980904     D UDAT            DS                  OCCURS(2000) INZ
007800980904     D  $CTR                   1     12
007900981111     D                                     DIM(12)                              *CONTROLLO MEMORIZZA
008000980904     D  ISDN                  13     84P 0
008100980904     D                                     DIM(12)                              *SPEDIZIONI NON DDT
008200980904     D  ISAB                  85    156P 0
008300980904     D                                     DIM(12)                              *SPEDIZIONI DISK A/B
008400980904     D  ICDB                 157    228P 0
008500980904     D                                     DIM(12)                              *COLLI DISK B
008600980904     D  ESDN                 229    300P 0
008700980904     D                                     DIM(12)                              *SPEDIZIONI NON DDT
008800980904     D  ESAB                 301    372P 0
008900980904     D                                     DIM(12)                              *SPEDIZIONI DISK A/B
009000980904     D  ECDB                 373    444P 0
009100980904     D                                     DIM(12)                              *COLLI DISK B
009200030805     D  ISCB                 445    516P 0
009300030805     D                                     DIM(12)                              *SPED. DISK C BART.
009400030805     D  ESCB                 517    588P 0
009500030805     D                                     DIM(12)                              *SPED. DISK C BART.
009600030805     D  ISCC                 589    660P 0
009700030805     D                                     DIM(12)                              *SPED. DISK C CLI.
009800030805     D  ESCC                 661    732P 0
009900030805     D                                     DIM(12)                              *SPED. DISK C CLI.
010000050408     D  IcCB                 733    804P 0
010100050408     D                                     DIM(12)                              *SPED. DISK C BART.
010200050408     D  EcCB                 805    876P 0
010300050408     D                                     DIM(12)                              *SPED. DISK C BART.
010400050408     D  IcCC                 877    948P 0
010500050408     D                                     DIM(12)                              *SPED. DISK C CLI.
010600050408     D  EcCC                 949   1020P 0
010700050408     D                                     DIM(12)                              *SPED. DISK C CLI.
010800980904     D*---
010900980904     D* DS AGENTI
011000980904     D*---
011100980904     D DS01          E DS
011200980904     D*---
011300980904     D* DS TIBI BOLLA
011400980904     D*---
011500980904     D DSTB          E DS
011600980904     D*---
011700980904     D* DS FLAG TIPI BOLLA
011800980904     D*---
011900980904     D DS2W          E DS
012000980904     D*---
012100980904     D* DS REPERIMENTO DATI CLIENTE
012200980904     D*---
012300980904     D BS69DS        E DS                  EXTNAME(TIBS69DS)
012400980904     D ACODS         E DS                  EXTNAME(CNACO00F)
012500980904     D INDDS         E DS                  EXTNAME(CNIND00F)
012600980904     D CLPDS         E DS                  EXTNAME(CNCLP00F)
012700980904     D CLSDS         E DS                  EXTNAME(FNCLS00F)
012800980904     D*---
012900980904     D* DS COMPOSIZIONE DATA(6) -TFSAC-
013000980904     D*---
013100981007     D                 DS                  INZ
013200980904     D  TASAAS                 1      4  0
013300980904     D  TASMGS                 5      8  0
013400980904     D  TASMES                 5      6  0
013500980904     D  TASDAT                 1      6  0
013600980904     D*---
013700980904     D* DS PARAMETRI INPUT
013800980904     D*---
013900980904     D PARAM           DS
014000980904     D  PARAMI                 1      6  0
014100980904     D  PARAAI                 1      4  0
014200980904     D  PARMMI                 5      6  0
014300980904     D  PARAMF                 7     12  0
014400980904     D  PARAAF                 7     10  0
014500980904     D  PARMMF                11     12  0
014600050414     D  PARsta               256    256
014700980904     D*---
014800980904     D* ARCHITETTURA
014900980904     D*---
015000980904     D KPJBA         E DS
015100000216     D*---
015200000216     D* DS X LETTURA DESCRIZIONE 143 ORGANIGRAMMA (X DPD)
015300000216     D*---
015400000216     D OG143         E DS
015500020403     D*---
015600020403     D* DS X REPERIMENTO/TEST CLIENTI PADRI/FIGLI
015700020403     D*---
015800020403     D TIBS10        E DS                  EXTNAME(TIBS10DS)
015900020702     D*------------------
016000020702     D* DS PER LETTURA TABELLA "NTW"
016100020702     D*------------------
016200020702     D DNTW          E DS
016300030805     D*---
016400030805     D* DS CODICI TRATTAMENTO MERCE
016500030805     D*---
016600030805     D DS1B          E DS
016700030805     D*---
016800030805     D* DS TABELLA INVIO DATI CLIENTI
016900030805     D*---
017000030805     D DS3C          E DS
017100980904     C*------------------------------------------------------------------------*
017200980904     C* MAIN LINE
017300980904     C*------------------------------------------------------------------------*
017400980904     C*
017500980904     C* POSIZIONAMENTO E PRIMA LETTURA BOLLE
017600980904     C                   EXSR      SETFIL
017700980904     C*
017800980904     C* CICLO FINO A FINE FILE
017900980904DO  1C     $FINE         DOWEQ     'N'
018000980904     C*
018100980904     C* MEMORIZZA LE BOLLE PER COMMERCIALE
018200980904     C                   EXSR      MEMTAS
018300980904     C*
018400980904     C* LETTURA SUCCESSIVA BOLLE
018500980904     C                   EXSR      LETFIL
018600980904E   1C                   ENDDO                                                  *FINE FILE
018700980904     C*
018800980904     C* AGGIORNA IL REPORT
018900980904     C                   EXSR      SCRREP
019000980904     C*
019100980904     C* OPERAZIONI FINALI
019200980904     C                   EXSR      FINSR
019300980904     C*
019400980904     C                   SETON                                        LR
019500980904     C*------------------------------------------------------------------------*
019600980904     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA                          -BOLLE-
019700980904     C*------------------------------------------------------------------------*
019800980904     C     SETFIL        BEGSR
019900980904     C*
020000981215     C                   Z-ADD     PARAAI        KASAAS                         *ANNO INIZIALE
020100981215     C                   MOVEL     PARMMI        KASMGS                         *MM/GG INIZIALE
020200990802     C     KEYTAS        SETLL     TITAS38C                           99        *FINE ARCHIVIO
020300980904IF  1C     *IN99         IFEQ      *ON
020400980904     C                   MOVEL     'S'           $FINE                          *FINE PGM
020500980904X   1C                   ELSE
020600980904     C                   EXSR      LETFIL                                       *LETTURA RECORD
020700980904E   1C                   ENDIF
020800980904     C*
020900980904     C                   ENDSR
021000980904     C*------------------------------------------------------------------------*
021100980904     C* LETFIL - LETTURA PROSSIMO RECORD                                 -BOLLE-
021200980904     C*------------------------------------------------------------------------*
021300980904     C     LETFIL        BEGSR
021400980904     C*
021500980904     C* LETTURA FINO A:
021600980904     C*  - FINE FILE
021700980904     C*  - TROVATO RECORD VALIDO
021800980904     C                   MOVEL     'N'           WRECOK
021900980904DO  1C     $FINE         DOWEQ     'N'
022000980904     C     WRECOK        ANDEQ     'N'
022100990802     C                   READ      TITAS38C                               99
022200980904IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
022300980904     C                   MOVEL     'S'           $FINE
022400980904X   2C                   ELSE
022500030807     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
022600980904E   2C                   ENDIF
022700980904E   1C                   ENDDO
022800980904     C*
022900980904     C                   ENDSR
023000980904     C*------------------------------------------------------------------------*
023100980904     C* CHKREC - CONTROLLA VALIDITA' RECORD                              -BOLLE-
023200980904     C*------------------------------------------------------------------------*
023300980904     C     CHKREC        BEGSR
023400980904     C*
023500980904     C* REIMPOSTA LE VARIABILI DI LAVORO
023600980904     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
023700980904     C                   MOVEL     *BLANKS       TASFIE            1
023800020403     C*
023900020403     C* PERIODO OLTRE IL RICHIESTO, ESCE
024000020403     C     TASDAT        IFGT      PARAMF
024100020403     C                   MOVEL     'S'           $FINE
024200020403     C                   ENDIF
024300980904     C*
024400980904     C* PERIODO NON RICHIESTO, ESCLUDE
024500980904IF  1C     TASDAT        IFGE      PARAMI
024600980904     C     TASDAT        ANDLE     PARAMF
024700980904X   1C                   ELSE
024800980904     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
024900980904E   1C                   ENDIF
025000980904     C*
025100980904     C* BOLLA NON VALIDA COME SPEDIZIONE, ESCLUDE
025200980904     C                   SETOFF                                           28
025300980904     C                   Z-ADD     1             I
025400980904     C     TASTBL        LOOKUP    TBL(I)                                 28
025500980904IF  1C     *IN28         IFEQ      *OFF                                         *NON TROVATO
025600980904     C     TFSP(I)       ORNE      'S'
025700980904     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
025800980904E   1C                   ENDIF
025900020403     C*
026000020403     C* RECUPERA IL PADRE DEL CLIENTE CORRENTE
026100020405IF  1C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
026200020404     C*
026300020404     C* VERIFICO LA ROTTURA DEL CLIENTE X OTTIMIZZARE L'ELABORAZIONE
026400020404     C                   IF        TASKSC <> DEPKSC
026500020404     C                   Z-ADD     TASKSC        DEPKSC
026600020403     C                   CLEAR                   TIBS10
026700020403     C                   Z-ADD     PARAMGF       D10DRF                         *DATA RIFERIMENTO
026800020403     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
026900020403     C                   MOVEL     'P'           D10PAF                         *CERCO IL SUO PADRE
027000020404     C                   Z-ADD     DEPKSC        D10COD                         *CODICE CLIENTE
027100020403     C                   CALL      'TIBS10R'
027200020403     C                   PARM                    TIBS10
027300020405     C                   PARM      'A'           PARTIPEXE
027400020403     C     D10ERR        IFEQ      *BLANKS                                      *SE PADRE
027500020405     C                   Z-ADD     D10COP        DEPCOP                         *CLIENTI FIGLI
027600020404     C                   ELSE
027700020404     C                   Z-ADD     DEPKSC        DEPCOP
027800020404     C                   ENDIF
027900980904     C*
028000980904     C* REPERICE I DATI DEL CLIENTE
028100981014     C                   CLEAR                   BS69DS
028200980904     C                   CLEAR                   CLPDS
028300980904     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
028400020404     C                   Z-ADD     DEPCOP        I69KCP                         *CODICE X CNCLP
028500980904     C                   CALL      'TIBS69R'
028600980904     C                   PARM                    BS69DS
028700980904     C                   PARM                    ACODS
028800980904     C                   PARM                    INDDS
028900980904     C                   PARM                    CLPDS
029000980904     C                   PARM                    CLSDS
029100980904IF  2C     O69ERR        IFEQ      '1'                                          *ERRORE
029200980904     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
029300980904E   2C                   ENDIF
029400980904     C*
029500980904     C* RECUPERA IL COMMERCIALE UNIFICANTE
029600980904     C                   Z-ADD     1             I
029700980904     C                   SETOFF                                           28
029800980904     C     CLPAGE        LOOKUP    AGE(I)                                 28
029900980904IF  2C     *IN28         IFEQ      *ON                                          *TROVATO
030000980904     C                   Z-ADD     1             C                 4 0
030100980904     C                   SETOFF                                           28
030200980904     C     ARGF(I)       LOOKUP    RGF(C)                                 28
030300980904IF  3C     *IN28         IFEQ      *ON                                          *TROVATO
030400980904     C                   Z-ADD     C             INDRGF
030500050414     c                   movel     rgf(C)        wpo               3 0
030600020405     C                   ELSE
030700020405     C                   Z-ADD     *ZEROS        INDRGF                         *NON TROVATO
030800980904E   3C                   ENDIF
030900020405     C                   ELSE
031000020405     C                   Z-ADD     *ZEROS        INDRGF                         *NON TROVATO
031100980904E   2C                   ENDIF
031200020405     C                   ENDIF
031300020405E   1C                   ENDIF
031400020405     C*
031500020405IF  1C     INDRGF        IFEQ      *ZEROS                                       *ERRORI
031600020405     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
031700020405E   1C                   ENDIF
031800980904     C*
031900980904     C* RECUPERA FLAG ITALIA/ESTERO
032000980904IF  1C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
032100980904     C                   Z-ADD     1             I
032200980904     C                   SETOFF                                           28
032300980904     C     TASLNP        LOOKUP    FIL(I)                                 28
032400980904IF  2C     *IN28         IFEQ      *ON                                          *TROVATO
032500980904     C                   Z-ADD     1             J                 4 0
032600980904     C                   SETOFF                                           28
032700980904     C     TASLNA        LOOKUP    FIL(J)                                 28
032800980904IF  3C     *IN28         IFEQ      *ON                                          *TROVATO
032900980904IF  4C     FFL1(I)       IFEQ      'E'                                          *LNP ESTERO  O
033000980904     C     FFL1(J)       OREQ      'E'                                          *LNA ESTERO
033100980904     C                   MOVEL     'E'           TASFIE                         *BOLLA ESTERO
033200980904X   4C                   ELSE
033300980904     C                   MOVEL     'I'           TASFIE                         *BOLLA ITALIA
033400980904E   4C                   ENDIF
033500980904E   3C                   ENDIF
033600980904E   2C                   ENDIF
033700980904E   1C                   ENDIF
033800980904IF  1C     TASFIE        IFEQ      *BLANKS                                      *ERRORI
033900980904     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
034000980904E   1C                   ENDIF
034100980904     C*
034200980904     C                   ENDSR
034300980904     C*------------------------------------------------------------------------*
034400980904     C* MEMTAS - MEMORIZZA I DATI                                        -BOLLE-
034500980904     C*------------------------------------------------------------------------*
034600980904     C     MEMTAS        BEGSR
034700980904     C*
034800980904     C* RECUPERA IL MESE
034900980904     C                   SETOFF                                           28
035000980904     C                   Z-ADD     1             M                 2 0
035100980904     C     TASMES        LOOKUP    MMM(M)                                 28
035200980904IF  1C     *IN28         IFEQ      *ON                                          *TROVATO
035300980904     C*
035400980904     C* SI POSIZIONA SUL COMMERCIALE UNIFICANTE
035500980904     C     INDRGF        OCCUR     UDAT                                         *POSIZIONAMENTO
035600981008     C                   MOVEL     'S'           $CTR(M)                        *MEMORIZZATO QC
035700980904     C*---
035800030805     C* ITALIA
035900980904     C*---
036000980904IF  2C     TASFIE        IFEQ      'I'
036100980904     C* SPEDIZIONI NO DDT
036200980904IF  3C     TASLL1        IFNE      'C'
036300980904     C     TASLL1        ANDNE     'S'
036400980904     C                   ADD       1             ISDN(M)
036500980904E   3C                   ENDIF
036600030805     C* SPEDIZIONI DISK A/B/C
036700030805     C*
036800030805     C* VERIFICO SE BOLLA DISK C (E CHE TIPO DI DISK C: C=CLIENTE B=BARTOLINI) O A+C
036900030805     C                   Z-ADD     1             II
037000030805     C                   CLEAR                   WTMT
037100030805     C     TASCTM        LOOKUP    TMC(II)                                36
037200030805     C   36              MOVE      TMT(II)       WTMT              1
037300030805     C*
037400030805     C* VERIFICO PARTICOLARITA' IMPORT DPD (OVVERO LINEA DI PARTENZA DPD)
037500030805     C     TASLNP        LOOKUP    FDPD                                   55
037600030805     C                   IF        *IN55 = *ON
037700030805     C     TASCCM        IFGT      *ZEROS
037800030805     C                   MOVEL(P)  TASCCM        KTBKEY
037900030805     C                   ELSE
038000030805     C                   MOVEL(P)  TASKSC        KTBKEY
038100030805     C                   ENDIF
038200030805     C     KEYSAV        IFNE      KTBKEY
038300030805     C                   MOVE      KTBKEY        KEYSAV
038400030805     C                   MOVEL     '3C'          KTBCOD
038500030805     C     KEYTAB        CHAIN     TABEL00F                           98
038600030805     C  N98              MOVEL     TBLUNI        DS3C
038700030805     C   98              CLEAR                   DS3C
038800030805     C                   ENDIF
038900030805     C     §3CDKC        IFEQ      'S'
039000030805     C                   SETON                                        36
039100030805     C                   MOVEL     'C'           WTMT
039200030805     C                   ENDIF
039300030805     C                   ENDIF
039400030807     C*
039500030807     C* VERIFICO SE NO DISK
039600030807     C     TASSOP        IFNE      '§'
039700030807     C     TASSOP        ANDNE     '%'
039800030807     C     *IN36         ANDEQ     *OFF
039900030807     C                   ELSE
040000030805     C*
040100030805     C* VERIFICO SE DISK B (OVVERO E' PRESENTE LA SERIE)
040200030805     C                   IF        TASNRS <> *ZEROS
040300030805     C                   ADD       1             ISAB(M)
040400030807     C                   ADD       TASNCL        ICDB(M)
040500030805     C                   ELSE
040600030805     C* VERIFICO SE DISK A
040700030805     C                   IF        *IN36 = *OFF
040800030805     C                   ADD       1             ISAB(M)
040900030805     C                   ELSE
041000030805     C* VERIFICO SE DISK C BARTOLINI
041100030805     C     WTMT          IFEQ      'B'
041200030805     C     TASSOP        IFEQ      ':'
041300030805     C                   ADD       1             ISAB(M)
041400030805     C                   ADD       1             ISCB(M)
041500050408     C                   ADD       TASNCL        IccB(M)
041600030805     C                   ELSE
041700030805     C                   ADD       1             ISCB(M)
041800050408     C                   ADD       TASNCL        IccB(M)
041900030805     C                   ENDIF
042000030805     C                   ELSE
042100030805     C* VERIFICO SE DISK C CLIENTE
042200030805     C                   ADD       1             ISAB(M)
042300030805     C                   ADD       1             ISCC(M)
042400050408     C                   ADD       TASNCL        Iccc(M)
042500050414     c                   if        parsta='S'
042600050414     c                   if        wpo=71 or wpo=72 or wpo=74 or wpo=63 or
042700050414     c                             wpo=103 or wpo=109 or wpo=114 or wpo=123 or
042800050414     c                             wpo=124 or wpo=255
042900050414     c                   except    bolSCC
043000030805     C                   ENDIF
043100050414     C                   ENDIF
043200050414     c*
043300050414     C                   ENDIF
043400030805     C                   ENDIF
043500030805     C                   ENDIF
043600030805E   3C                   ENDIF
043700030807E   2C                   ENDIF
043800980904     C*---
043900980904     C* ESTERO
044000980904     C*---
044100980904IF  2C     TASFIE        IFEQ      'E'
044200980904     C* SPEDIZIONI NO DDT
044300980904IF  3C     TASLL1        IFNE      'C'
044400980904     C     TASLL1        ANDNE     'S'
044500980904     C                   ADD       1             ESDN(M)
044600980904E   3C                   ENDIF
044700030807     C* SPEDIZIONI DISK A/B/C
044800030805     C*
044900030805     C* VERIFICO SE BOLLA DISK C (E CHE TIPO DI DISK C: C=CLIENTE B=BARTOLINI) O A+C
045000030805     C                   Z-ADD     1             II
045100030805     C                   CLEAR                   WTMT
045200030805     C     TASCTM        LOOKUP    TMC(II)                                36
045300030805     C   36              MOVE      TMT(II)       WTMT              1
045400030805     C*
045500030805     C* VERIFICO PARTICOLARITA' IMPORT DPD (OVVERO LINEA DI PARTENZA DPD)
045600030805     C     TASLNP        LOOKUP    FDPD                                   55
045700030805     C                   IF        *IN55 = *ON
045800030805     C     TASCCM        IFGT      *ZEROS
045900030805     C                   MOVEL(P)  TASCCM        KTBKEY
046000030805     C                   ELSE
046100030805     C                   MOVEL(P)  TASKSC        KTBKEY
046200030805     C                   ENDIF
046300030805     C     KEYSAV        IFNE      KTBKEY
046400030805     C                   MOVE      KTBKEY        KEYSAV
046500030805     C                   MOVEL     '3C'          KTBCOD
046600030805     C     KEYTAB        CHAIN     TABEL00F                           98
046700030805     C  N98              MOVEL     TBLUNI        DS3C
046800030805     C   98              CLEAR                   DS3C
046900030805     C                   ENDIF
047000030805     C     §3CDKC        IFEQ      'S'
047100030805     C                   SETON                                        36
047200030805     C                   MOVEL     'C'           WTMT
047300030805     C                   ENDIF
047400030805     C                   ENDIF
047500030807     C*
047600030807     C* VERIFICO SE NO DISK
047700030807     C     TASSOP        IFNE      '§'
047800030807     C     TASSOP        ANDNE     '%'
047900030807     C     *IN36         ANDEQ     *OFF
048000030807     C                   ELSE
048100030805     C*
048200030805     C* VERIFICO SE DISK B (OVVERO E' PRESENTE LA SERIE)
048300030805     C                   IF        TASNRS <> *ZEROS
048400030805     C                   ADD       1             ESAB(M)
048500030807     C                   ADD       TASNCL        ECDB(M)
048600030805     C                   ELSE
048700030805     C* VERIFICO SE DISK A
048800030805     C                   IF        *IN36 = *OFF
048900030805     C                   ADD       1             ESAB(M)
049000030805     C                   ELSE
049100030805     C* VERIFICO SE DISK C BARTOLINI
049200030805     C     WTMT          IFEQ      'B'
049300030805     C     TASSOP        IFEQ      ':'
049400030805     C                   ADD       1             ESAB(M)
049500030805     C                   ADD       1             ESCB(M)
049600050408     C                   ADD       TASNCL        ECcB(M)
049700030805     C                   ELSE
049800030805     C                   ADD       1             ESCB(M)
049900050408     C                   ADD       TASNCL        ECcB(M)
050000030805     C                   ENDIF
050100030805     C                   ELSE
050200030805     C* VERIFICO SE DISK C CLIENTE
050300030805     C                   ADD       1             ESAB(M)
050400030805     C                   ADD       1             ESCC(M)
050500050408     C                   ADD       TASNCL        ECcc(M)
050600050414     c                   if        parsta='S'
050700050414     c                   if        wpo=71 or wpo=72 or wpo=74 or wpo=63 or
050800050414     c                             wpo=103 or wpo=109 or wpo=114 or wpo=123 or
050900050414     c                             wpo=124 or wpo=255
051000050414     c                   except    bolSCC
051100050414     C                   ENDIF
051200050414     C                   ENDIF
051300050414     c
051400030805     C                   ENDIF
051500030805     C                   ENDIF
051600030805     C                   ENDIF
051700980904E   3C                   ENDIF
051800980904E   2C                   ENDIF
051900980904E   1C                   ENDIF
052000980904     C*
052100980904     C                   ENDSR
052200980904     C*------------------------------------------------------------------------*
052300980904     C* SCRREP - SCRIVE UN RECORD PER IL COMMERCIALE UNIFICANTE
052400980904     C*------------------------------------------------------------------------*
052500980904     C     SCRREP        BEGSR
052600980904     C*
052700980904     C* PER CIASCUN COMMERCIALE UNIFICANTE MEMORIZZATO
052800100202DO  1C     1             DO        2000          C
052900980904IF  2C     RGF(C)        IFNE      *ZEROS
053000980904     C     C             OCCUR     UDAT                                         *POSIZIONAMENTO
053100980904     C*
053200980904     C* IMPOSTA LA CHIAVE DI LETTURA
053300980904     C                   EXSR      REPAGE                                       *RECUPERA DATI AGENT
053400980904     C                   MOVEL     DEPDIV        KEPDIV                         *DIVISIONE
053500980904     C                   Z-ADD     DEPARE        KEPARE                         *AREA
053600980904     C                   Z-ADD     DEPFIL        KEPFIL                         *FILIALE
053700980904     C                   Z-ADD     RGF(C)        KEPRGF                         *COMMERCIALE
053800980904     C*
053900981117     C* PER CIASCUN MESE MEMORIZZATO, SCRIVE I DATI SOLO SE ESISTE
054000980904DO  3C     1             DO        12            M
054100980904IF  4C     MMM(M)        IFNE      *ZEROS
054200981117IF  5C     $CTR(M)       IFEQ      'S'                                          *MEMORIZZATO QC.
054300980904     C                   Z-ADD     AAA(M)        KEPANN                         *ANNO
054400980904     C                   Z-ADD     MMM(M)        KEPMES                         *MESE
054500050408     C     KEYREP        CHAIN     WFREP11L                           97
054600981117IF  6C     *IN97         IFEQ      *OFF                                         *MODIFICA
054700980904     C                   EXSR      IMPREP                                       *IMPOSTA CAMPI
054800980904     C                   UPDATE    WFREP000                                     *AGGIORNA
054900981117E   6C                   ENDIF
055000981117E   5C                   ENDIF
055100980904E   4C                   ENDIF
055200980904E   3C                   ENDDO                                                  *FINE PERIODO
055300980904E   2C                   ENDIF
055400980904E   1C                   ENDDO                                                  *FINE COMMERCIALI
055500980904     C*
055600980904     C                   ENDSR
055700980904     C*------------------------------------------------------------------------*
055800980904     C* IMPREP - IMPOSTA UN RECORD PER IL REPORT
055900980904     C*------------------------------------------------------------------------*
056000980904     C     IMPREP        BEGSR
056100981013     C* ITALIA
056200980904     C                   Z-ADD     ISDN(M)       REISDN
056300980904     C                   Z-ADD     ISAB(M)       REISAB
056400980904     C                   Z-ADD     ICDB(M)       REICDB
056500030807     C                   Z-ADD     ISCB(M)       REISCB
056600030807     C                   Z-ADD     ISCC(M)       REISCC
056700050408     C                   Z-ADD     IcCB(M)       REIcCB
056800050408     C                   Z-ADD     IcCC(M)       REIcCC
056900980904     C* ESTERO
057000980904     C                   Z-ADD     ESDN(M)       REESDN                         *CAMPI DA VALORIZZAR
057100980904     C                   Z-ADD     ESAB(M)       REESAB
057200980904     C                   Z-ADD     ECDB(M)       REECDB
057300030807     C                   Z-ADD     ESCB(M)       REESCB
057400030807     C                   Z-ADD     ESCC(M)       REESCC
057500050408     C                   Z-ADD     EcCB(M)       REEcCB
057600050408     C                   Z-ADD     EcCC(M)       REEcCC
057700980904     C*
057800980904     C                   ENDSR
057900980904     C*------------------------------------------------------------------------*
058000980904     C* REPAGE - RECUPERA I DATI DELL'AGENTE
058100980904     C*------------------------------------------------------------------------*
058200980904     C     REPAGE        BEGSR
058300980904     C*
058400980904     C* REIMPOSTA LE VARIABILI DI LAVORO
058500980904     C                   MOVEL     *BLANKS       DEPDIV
058600980904     C                   Z-ADD     *ZEROS        DEPARE
058700980904     C                   Z-ADD     *ZEROS        DEPFIL
058800980904     C*
058900980904     C                   Z-ADD     1             I
059000980904     C                   SETOFF                                           28
059100980904     C                   MOVEL     RGF(C)        DEPFIL                         *FILIALE
059200980904     C     DEPFIL        LOOKUP    FIL(I)                                 28
059300980904IF  1C     *IN28         IFEQ      *ON                                          *TROVATO
059400980904     C                   Z-ADD     FARE(I)       DEPARE                         *AREA
059500980904     C                   MOVEL     FDIV(I)       DEPDIV                         *DIVISIONE
059600980904E   1C                   ENDIF
059700980904     C*
059800980904     C                   ENDSR
059900980904     C*------------------------------------------------------------------------*
060000980904     C* CARTAB - CARICAMENTO TABELLE OCCORRENTI
060100980904     C*------------------------------------------------------------------------*
060200980904     C     CARTAB        BEGSR
060300980904     C*---
060400980904     C* ORGANIGRAMMA
060500980904     C*---
060600980904     C                   Z-ADD     *ZEROS        I                 4 0
060700980904     C     *LOVAL        SETLL     AZORG01L
060800980904     C                   READ      AZORG01L                               99
060900980904DO  1C     *IN99         DOWEQ     *OFF
061000980904IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
061100980904IF  3C     ORGFAG        IFNE      'V'                                          *NO "VARIE"
061200980904     C                   ADD       1             I
061300980904     C                   Z-ADD     ORGFIL        FIL(I)                         *FILIALE
061400000216     C                   CLEAR                   OG143
061500000216     C                   MOVEL     ORGDE3        OG143
061600020702     C*
061700020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
061800020702     C                   CLEAR                   DNTW
061900020702     C*
062000020702     C                   MOVEL(P)  'NTW'         KTECOD
062100020702     C                   MOVEL(P)  §OGNTW        KTEKE1
062200020702     C     KEYTBE        CHAIN     TNTBE01L
062300020702     C                   IF        %found(TNTBE01L)
062400020702     C                   MOVEL     TBEUNI        DNTW
062500020702     C                   ENDIF
062600020702     C*
062700020702     C                   MOVEL     §NTWFIE       FFL1(I)
062800020702     C                   IF        §OGNTW = 'DPD'
062900020702     C                   MOVEL     'E'           FFL1(I)
063000030805     C                   Z-ADD     ORGFIL        FDPD(I)                        *FILIALE DPD
063100020702     C                   ENDIF
063200020702     C*
063300980904     C                   Z-ADD     ORGCAR        FARE(I)                        *AREA
063400980904     C                   MOVEL     ORGFL3        FDIV(I)                        *DIVISIONE
063500980904E   3C                   ENDIF
063600980904E   2C                   ENDIF
063700980904     C                   READ      AZORG01L                               99
063800980904E   1C                   ENDDO
063900980904     C*---
064000980904     C* AGENTI UNIFICANTI
064100980904     C*---
064200980904     C                   Z-ADD     *ZEROS        I                 4 0
064300020906     C                   Z-ADD     1             KTBKUT
064400980904     C                   MOVEL     '01'          KTBCOD
064500981210     C     KE2TAB        SETLL     TABEL00F
064600981210     C     KE2TAB        READE     TABEL00F                               99
064700980904DO  1C     *IN99         DOWEQ     *OFF
064800980904IF  2C     TBLFLG        IFEQ      *BLANKS
064900980904     C                   MOVEL     TBLKEY        N7                7 0
065000980904     C                   MOVEL     TBLUNI        DS01
065100980904IF  3C     N7            IFEQ      §01RGF
065200980904     C                   ADD       1             I
065300980904     C                   Z-ADD     §01RGF        RGF(I)                         *AGENTE UNIFICANTE
065400981013     C                   MOVEL     §01AGE        DRGF(I)                        *DESCRIZIONE
065500980904E   3C                   ENDIF
065600980904E   2C                   ENDIF
065700981210     C     KE2TAB        READE     TABEL00F                               99
065800980904E   1C                   ENDDO
065900980904     C*---
066000980904     C* AGENTI
066100980904     C*---
066200980904     C                   Z-ADD     *ZEROS        I                 4 0
066300020906     C                   Z-ADD     1             KTBKUT
066400980904     C                   MOVEL     '01'          KTBCOD
066500981210     C     KE2TAB        SETLL     TABEL00F
066600981210     C     KE2TAB        READE     TABEL00F                               99
066700980904DO  1C     *IN99         DOWEQ     *OFF
066800980904IF  2C     TBLFLG        IFEQ      *BLANKS
066900980904     C                   ADD       1             I
067000980904     C                   MOVEL     TBLUNI        DS01
067100980904     C                   MOVEL     TBLKEY        AGE(I)                         *AGENTE
067200980904     C                   Z-ADD     §01RGF        ARGF(I)                        *AGENTE UNIFICANTE
067300980904E   2C                   ENDIF
067400981210     C     KE2TAB        READE     TABEL00F                               99
067500980904E   1C                   ENDDO
067600980904     C*---
067700980904     C* TIPI BOLLA
067800980904     C*---
067900980904     C                   Z-ADD     *ZEROS        I                 4 0
068000020906     C                   Z-ADD     1             KTBKUT
068100980904     C                   MOVEL     'TB'          KTBCOD
068200980904     C     KE2TAB        SETLL     TABEL00F
068300980904     C     KE2TAB        READE     TABEL00F                               99
068400980904DO  1C     *IN99         DOWEQ     *OFF
068500980904IF  2C     TBLFLG        IFEQ      *BLANKS
068600980904     C                   MOVEL     TBLUNI        DSTB
068700980904     C                   ADD       1             I
068800980904     C                   MOVEL     TBLKEY        TBL(I)
068900980904     C                   MOVEL     §TBRBL        TRBL(I)
069000980904E   2C                   ENDIF
069100980904     C     KE2TAB        READE     TABEL00F                               99
069200980904E   1C                   ENDDO
069300980904     C*---
069400980904     C* FLAG TIPI BOLLA
069500980904     C*---
069600980904DO  1C     1             DO        20            I
069700980904IF  2C     TBL(I)        IFNE      *BLANKS
069800020906     C                   Z-ADD     1             KTBKUT
069900980904     C                   MOVEL     '2W'          KTBCOD
070000980904     C                   MOVEL(P)  TRBL(I)       KTBKEY
070100980904     C     KEYTAB        CHAIN     TABEL00F                           99
070200980904IF  3C     *IN99         IFEQ      *OFF
070300980904     C                   MOVEL     TBLUNI        DS2W
070400980904     C                   MOVEL     §2WFSP        TFSP(I)                        *FLAG SPEDIZIONI
070500980904     C                   MOVEL     §2WFRI        TFRI(I)                        *FLAG RICAVI
070600980904E   3C                   ENDIF
070700980904E   2C                   ENDIF
070800980904E   1C                   ENDDO                                                  *FINE TIPI BOLLA
070900030805     C*
071000030805     C* CARICO TABELLA CTM DISK C
071100030805     C                   Z-ADD     1             KTBKUT
071200030805     C                   MOVE      '1B'          KTBCOD
071300030805     C                   CLEAR                   II
071400030805     C     KE2TAB        SETLL     TABEL00F
071500030805     C     KE2TAB        READE     TABEL00F                               98
071600030805     C     *IN98         DOWEQ     *OFF
071700030805     C                   MOVEL     TBLUNI        DS1B
071800030805     C     §1BDKC        IFEQ      'B'
071900030805     C     §1BDKC        OREQ      'C'
072000030805     C                   ADD       1             II                3 0
072100030805     C                   MOVEL     TBLKEY        TMC(II)
072200030805     C                   MOVEL     §1BDKC        TMT(II)
072300030805     C                   ENDIF
072400030805     C     KE2TAB        READE     TABEL00F                               98
072500030805     C                   ENDDO
072600980904     C*
072700980904     C                   ENDSR
072800980904     C*------------------------------------------------------------------------*
072900980904     C* CARMES - CARICA I MESI/ANNI DA ELEBORARE
073000980904     C*------------------------------------------------------------------------*
073100980904     C     CARMES        BEGSR
073200980904     C*
073300980904     C* REIMPOSTA LE VARIABILI DI LAVORO
073400980904     C                   CLEAR                   AAA                            *ANNI
073500980904     C                   CLEAR                   MMM                            *MESI
073600980904     C                   Z-ADD     *ZEROS        J                 4 0          *N° MESI DA ELABORAR
073700980904     C                   Z-ADD     PARAAF        WAA4              4 0          *ANNO FINALE LAVORO
073800980904     C                   Z-ADD     PARMMF        WMM2              2 0          *MESE FINALE LAVORO
073900980904     C                   Z-ADD     PARAAI        WAA               4 0          *ANNO INIZIALE LAVOR
074000980904     C                   Z-ADD     PARMMI        WMM               2 0          *MESE INIZIALE LAVOR
074100980904     C*
074200980904     C* ANNO INIZIALE UGUALE AD ANNO FINALE
074300980904IF  1C     WAA4          IFEQ      WAA
074400980904     C*
074500980904     C* CALCOLO I MESI DI DIFFERENZA
074600980904DO  2C     WMM           DOWLE     WMM2
074700980904     C                   ADD       1             J
074800980904     C                   Z-ADD     WAA           AAA(J)
074900980904     C                   Z-ADD     WMM           MMM(J)
075000980904     C                   ADD       1             WMM
075100980904E   2C                   ENDDO
075200980904     C*
075300980904     C* IL RESTO DELLA SCHIERA A ZERO
075400980904DO  2C     J             DOWLT     12
075500980904     C                   ADD       1             J
075600980904     C                   Z-ADD     *ZEROS        AAA(J)
075700980904     C                   Z-ADD     *ZEROS        MMM(J)
075800980904E   2C                   ENDDO
075900980904     C*
076000980904     C* ANNO FINALE DIVERSO DA ANNO INIZIALE
076100980904X   1C                   ELSE
076200980904     C*
076300980904     C* MEMORIZZO I MESI DELL'ANNO INIZIALE
076400980904DO  2C     WMM           DOWLE     12
076500980904     C                   ADD       1             J
076600980904     C                   Z-ADD     WAA           AAA(J)
076700980904     C                   Z-ADD     WMM           MMM(J)
076800980904     C                   ADD       1             WMM
076900980904E   2C                   ENDDO
077000980904     C*
077100980904     C* MEMORIZZO I MESI DELL'ANNO FINALE
077200980904     C                   Z-ADD     01            WMM
077300980904DO  2C     WMM           DOWLE     WMM2
077400980904     C                   ADD       1             J
077500980904     C                   Z-ADD     WAA4          AAA(J)
077600980904     C                   Z-ADD     WMM           MMM(J)
077700980904     C                   ADD       1             WMM
077800980904E   2C                   ENDDO
077900980904     C*
078000980904     C* IL RESTO DELLA SCHIERA A ZERO
078100980904DO  2C     J             DOWLT     12
078200980904     C                   ADD       1             J
078300980904     C                   Z-ADD     *ZEROS        AAA(J)
078400980904     C                   Z-ADD     *ZEROS        MMM(J)
078500980904E   2C                   ENDDO
078600980904E   1C                   ENDIF
078700980904     C*
078800980904     C                   ENDSR
078900980904     C*------------------------------------------------------------------------*
079000980904     C* FINSR - OPERAZIONI FINALI
079100980904     C*------------------------------------------------------------------------*
079200980904     C     FINSR         BEGSR
079300980904     C*
079400980904     C* CHIUDE I FILE DEI PGM CHIAMATI
079500980904     C                   CLEAR                   BS69DS
079600980904     C                   CLEAR                   ACODS
079700980904     C                   CLEAR                   INDDS
079800980904     C                   CLEAR                   CLPDS
079900980904     C                   CLEAR                   CLSDS
080000980904     C                   MOVEL     'C'           I69TLA                         *TIPO LANCIO
080100980904     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
080200980904     C                   CALL      'TIBS69R'
080300980904     C                   PARM                    BS69DS
080400980904     C                   PARM                    ACODS
080500980904     C                   PARM                    INDDS
080600980904     C                   PARM                    CLPDS
080700980904     C                   PARM                    CLSDS
080800020405     C*
080900020405     C                   CLEAR                   TIBS10
081000020405     C                   CALL      'TIBS10R'
081100020405     C                   PARM                    TIBS10
081200020405     C                   PARM      'C'           PARTIPEXE         1
081300980904     C*
081400980904     C                   ENDSR
081500980904     C*------------------------------------------------------------------------*
081600980904     C* *INZSR - OPERAZIONI INIZIALI
081700980904     C*------------------------------------------------------------------------*
081800980904     C     *INZSR        BEGSR
081900980904     C*
082000980904     C* RICEVIMENTO PARAMETRI
082100980904     C     *ENTRY        PLIST
082200980904     C                   PARM                    KPJBA
082300980904     C                   MOVEL     KPJBU         PARAM
082400980904     C*
082500980904     C* VARIABILI RIFERITE AL DATABASE
082600980904     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
082700980904     C     *LIKE         DEFINE    TBLCOD        KTBCOD
082800980904     C     *LIKE         DEFINE    TBLKEY        KTBKEY
082900990802     C     *LIKE         DEFINE    TASAAS        KASAAS                         *TITAS00F
083000981215     C     *LIKE         DEFINE    TASMGS        KASMGS
083100050408     C     *LIKE         DEFINE    REPDIV        KEPDIV                         *WFREP10F
083200980904     C     *LIKE         DEFINE    REPARE        KEPARE
083300980904     C     *LIKE         DEFINE    REPFIL        KEPFIL
083400980904     C     *LIKE         DEFINE    REPRGF        KEPRGF
083500980904     C     *LIKE         DEFINE    REPANN        KEPANN
083600980904     C     *LIKE         DEFINE    REPMES        KEPMES
083700980904     C     *LIKE         DEFINE    REPDIV        DEPDIV                         *DEPOSITI
083800980904     C     *LIKE         DEFINE    REPARE        DEPARE
083900980904     C     *LIKE         DEFINE    REPFIL        DEPFIL
084000020404     C     *LIKE         DEFINE    TASKSC        DEPKSC
084100020404     C     *LIKE         DEFINE    TASKSC        DEPCOP
084200020702     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
084300020702     C     *LIKE         DEFINE    TBEKE1        KTEKE1
084400030805     C     *LIKE         DEFINE    KTBKEY        KEYSAV
084500980904     C*
084600030805     C* CHIAVI DI LETTURA
084700980904     C     KEYTAB        KLIST                                                  *TABEL00F
084800980904     C                   KFLD                    KTBKUT                          -UTENTE
084900980904     C                   KFLD                    KTBCOD                          -TABELLA
085000980904     C                   KFLD                    KTBKEY                          -UTENTE
085100980904     C     KE2TAB        KLIST                                                  *TABEL00F
085200980904     C                   KFLD                    KTBKUT                          -UTENTE
085300980904     C                   KFLD                    KTBCOD                          -TABELLA
085400050408     C     KEYTAS        KLIST                                                  *WFREP10F
085500981215     C                   KFLD                    KASAAS                          -ANNO
085600981215     C                   KFLD                    KASMGS                          -MESE/GIORNO
085700050408     C     KEYREP        KLIST                                                  *WFREP10F
085800980904     C                   KFLD                    KEPDIV                          -DIVISIONE
085900980904     C                   KFLD                    KEPARE                          -AREA
086000980904     C                   KFLD                    KEPFIL                          -FILIALE
086100980904     C                   KFLD                    KEPRGF                          -AGENTE
086200980904     C                   KFLD                    KEPANN                          -ANNO
086300980904     C                   KFLD                    KEPMES                          -MESE
086400020702     C     KEYTBE        KLIST                                                  *TNTBE01L
086500020702     C                   KFLD                    KTECOD                          -CODICE
086600020702     C                   KFLD                    KTEKE1                          -CHIAVE
086700980904     C*
086800980904     C* VARIABILI DI CONTROLLO
086900980904     C                   MOVEL     'N'           $FINE             1            *FINE PGM
087000980904     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
087100020405     C                   Z-ADD     *ZEROS        INDRGF            4 0
087200020403     C*
087300020403     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
087400020403     C                   MOVEL     PARAMF        PARAMGF           8 0
087500020403     C                   MOVE      '28'          PARAMGF
087600081119     C*
087700081119     C*------- GIORNO    di    F I N E    M E S E ---------------------------*
087800081119     C*
087900081119     C*  Ultimo giorno del mese
088000081119     C                   MOVE      PARAMGF       MMGG_4            4 0
088100081119     C                   MOVEL     MMGG_4        Mm_2              2 0
088200081119     C                   MOVEl     PARAMGF       Anno_4            4 0
088300081119      * se è febbraio
088400081119     c                   If        Mm_2 = 2
088500081119      *
088600081119      *  potrebbe essere bisestile 29 gg.
088700081119     c     Anno_4        div       4             risulta           3 0
088800081119     c                   mvr                     resto             3 0
088900081119      *  Se bisestile
089000081119     c                   If        resto = 0
089100081119     C                   MOVE      '29'          PARAMGF
089200081119     c                   end
089300081119      *
089400081119     c                   else
089500081119      *
089600081119      *  se invece Aprile, Giugno, Settembre, Novembre con 30 gg.
089700081119     c                   If           Mm_2 = 4 or Mm_2 = 6 or
089800081119     c                                Mm_2 = 9 or Mm_2 = 11
089900081119     C                   MOVE      '30'          PARAMGF
090000081119     c                   else
090100081119     C                   MOVE      '31'          PARAMGF
090200081119     c                   end
090300081119      *
090400081119     c                   endIf
090500081119     C*
090600081119     C*----------------------- F I N E    M E S E ---------------------------*
090700081119     C*
090800980904     C*
090900980904     C* CARICAMENTO TABELLE OCCORRENTI
091000980904     C                   EXSR      CARTAB
091100980904     C*
091200980904     C* CARICAMENTO MESI/ANNI DA ELABORARE
091300980904     C                   EXSR      CARMES
091400980904     C*
091500980904     C                   ENDSR
091600050414     OQSYSPRT   E            bolSCC      1
091700050414     O                                        +   0 'Bolle di REISCC e REESCC  '
091800050414     O                       tasAAS            +  2
091900050414     O                       tasLNP            +  1
092000050414     O                       tasNRS            +  1
092100050414     O                       tasNSP        2   +  1
092200050414     O                       taslna            +  1
092300050414     O                       tasmgs            +  1 '  /  '
092400050414     O                       tasksc            +  1
092500050414     o                                         +  1 'Colli'
092600050414     O                       tasncl            +  1
