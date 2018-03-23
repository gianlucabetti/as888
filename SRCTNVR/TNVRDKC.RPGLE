000100980904     H*------------------------------------------------------------------------*
000200101021     H* conteggio disk "C"
000300980904     H*------------------------------------------------------------------------*
000400980904     H DECEDIT('0,') DATEDIT(*DMY.)
000500980904     F*------------------------------------------------------------------------*
000600980904     F* DATA BASE
000700980904     F*------------------------------------------------------------------------*
001500101021     FTITAS39C  IF   E           K DISK
002100980904     FAZORG01L  IF   E           K DISK
002500980904     FTABEL00F  IF   E           K DISK
002700050414     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002800980904     D*------------------------------------------------------------------------*
002900980904     D* SCHIERE
003000980904     D*------------------------------------------------------------------------*
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
005501101021     d
005502101021     D FIL             S              3  0 DIM(3)
005503101021     D diskc           S              9  0 DIM(3)
011200980904     D*---
011300980904     D* DS TIBI BOLLA
011400980904     D*---
011500980904     D DSTB          E DS
011600980904     D*---
011700980904     D* DS FLAG TIPI BOLLA
011800980904     D*---
011900980904     D DS2W          E DS
012800980904     D*---
012900980904     D* DS COMPOSIZIONE DATA(6) -TFSAC-
013000980904     D*---
013100981007     D                 DS                  INZ
013200980904     D  TASAAS                 1      4  0
013300980904     D  TASMGS                 5      8  0
013500101021     D  TASDAT                 1      8  0
013600980904     D*---
013700980904     D* DS PARAMETRI INPUT
013800980904     D*---
013900980904     D PARAM           DS
014000101021     D  PARAMI                 1      8  0
014300101021     D  PARAMF                 9     16  0
014700980904     D*---
014800980904     D* ARCHITETTURA
014900980904     D*---
015000980904     D KPJBA         E DS
016300030805     D*---
016400030805     D* DS CODICI TRATTAMENTO MERCE
016500030805     D*---
016600030805     D DS1B          E DS
016700030805     D*---
017100980904     C*------------------------------------------------------------------------*
017200980904     C* MAIN LINE
017300980904     C*------------------------------------------------------------------------*
017400980904     C*
017700980904     C*
017800980904     C* CICLO FINO A FINE FILE
017801101021     c                   z-add     1             xx                2 0
017802101021     c                   do        3
017803101021     c                   if        xx= 1
017804101021     c                   eval      kfil=005
017805101021     c                   eval      fil(1)=005
017806101021     c                   endif
017807101021     c                   if        xx= 2
017808101021     c                   eval      kfil=050
017809101021     c                   eval      fil(2)=050
017810101021     c                   endif
017811101021     c                   if        xx= 3
017812101021     c                   eval      kfil=089
017813101021     c                   eval      fil(3)=089
017814101021     c                   endif
017815101021     c
017816101021     c                   eval      $fine='N'
017817101021     c
017818101021     C* POSIZIONAMENTO E PRIMA LETTURA BOLLE
017819101021     C                   EXSR      SETFIL
017820101021     c
017900980904DO  1C     $FINE         DOWEQ     'N'
018000980904     C*
018100980904     C* MEMORIZZA LE BOLLE PER COMMERCIALE
018200980904     C                   EXSR      MEMTAS
018300980904     C*
018400980904     C* LETTURA SUCCESSIVA BOLLE
018500980904     C                   EXSR      LETFIL
018600980904E   1C                   ENDDO                                                  *FINE FILE
018601101021     c
018602101021     c                   eval      xx=xx+1
018603101021     c                   enddo
019000980904     C*
019100980904     C* OPERAZIONI FINALI
019200101021     C                   except    Stampa
019300980904     C*
019400980904     C                   SETON                                        LR
019500980904     C*------------------------------------------------------------------------*
019600980904     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA                          -BOLLE-
019700980904     C*------------------------------------------------------------------------*
019800980904     C     SETFIL        BEGSR
019900980904     C*
020000101021     C                   movel     PARAmI        KASAAS                         *ANNO INIZIALE
020100101021     C                   MOVE      PARami        KASMGS                         *MM/GG INIZIALE
020200101021     C     Ktas          SETLL     TITAS39C                           99        *FINE ARCHIVIO
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
022100101021     C     kfil          READe     TITAS39C                               99
022101101021
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
023800020403     C*
023900020403     C* PERIODO OLTRE IL RICHIESTO, ESCE
024000020403     C     TASDAT        IFGT      PARAMF
024100020403     C                   MOVEL     'S'           $FINE
024200020403     C                   ENDIF
024201101021     c*
024202101021     c* elaboro solo linee 05 50 89
024203101021     c                   if        taslnp<>005 and taslnp<>50 and taslnp<>89
024204101021     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
024205101021E   1C                   ENDIF
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
034100980904     C*
034200980904     C                   ENDSR
034300980904     C*------------------------------------------------------------------------*
034400980904     C* MEMTAS - MEMORIZZA I DATI                                        -BOLLE-
034500980904     C*------------------------------------------------------------------------*
034600980904     C     MEMTAS        BEGSR
036700030805     C*
036800101021     C* VERIFICO SE BOLLA DISK C : C=CLIENTE
036900030805     C                   Z-ADD     1             II
037100030805     C     TASCTM        LOOKUP    TMC(II)                                36
037101101021     c                   if        *in36
037102101021     c                   add       tasncl        diskc(xx)
037103101021     c                   endif
052000980904     C*
052100980904     C                   ENDSR
067600980904     C*---
067700980904     C* TIPI BOLLA
067800980904     C*---
067801101021     c     CARTAB        BEGSR
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
071900101021     C     §1BDKC        ifeq      'C'
072000030805     C                   ADD       1             II                3 0
072100030805     C                   MOVEL     TBLKEY        TMC(II)
072200030805     C                   MOVEL     §1BDKC        TMT(II)
072300030805     C                   ENDIF
072400030805     C     KE2TAB        READE     TABEL00F                               98
072500030805     C                   ENDDO
072600980904     C*
072700980904     C                   ENDSR
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
084500980904     C*
084600030805     C* CHIAVI DI LETTURA
084700980904     C     KEYTAB        KLIST                                                  *TABEL00F
084800980904     C                   KFLD                    KTBKUT                          -UTENTE
084900980904     C                   KFLD                    KTBCOD                          -TABELLA
085000980904     C                   KFLD                    KTBKEY                          -UTENTE
085100980904     C     KE2TAB        KLIST                                                  *TABEL00F
085200980904     C                   KFLD                    KTBKUT                          -UTENTE
085300980904     C                   KFLD                    KTBCOD                          -TABELLA
085400101021     C     KTAS          KLIST                                                  *WFREP10F
085500101021     C                   KFLD                    Kfil              3 0           -ANNO
085501101021     C                   KFLD                    KASAAS                          -ANNO
085600981215     C                   KFLD                    KASMGS                          -MESE/GIORNO
086700980904     C*
086800980904     C* VARIABILI DI CONTROLLO
086900980904     C                   MOVEL     'N'           $FINE             1            *FINE PGM
087000980904     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
090700081119     C*
090800980904     C*
090900980904     C* CARICAMENTO TABELLE OCCORRENTI
091000980904     C                   EXSR      CARTAB
091400980904     C*
091500980904     C                   ENDSR
091600101021     OQSYSPRT   E            Stampa      1
091700101021     O                                        +  10 'Colli disk C dal '
091701101021     O                       parami            +  1 '    /  /  '
091702101021     o                                         +  2 'al '
091704101021     O                       paramf            +  1 '    /  /  '
091705101021     OQSYSPRT   E            Stampa      1
091800101021     O                                         +  3 '005-Mi Mecenate  Colli'
091900101021     O                       diskc(1)      2   +  1
092000101021     O                                         +  3 '050-Mi Sedriano  Colli'
092100101021     O                       diskc(2)      2   +  1
092200101021     O                                         +  3 '089-Mi Bovisa    Colli'
092300101021     O                       diskc(3)      2   +  1
