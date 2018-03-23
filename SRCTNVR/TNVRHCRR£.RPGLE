000100980904     H*------------------------------------------------------------------------*
000200980904     H DECEDIT('0,') DATEDIT(*DMY.)
000300980904     F*------------------------------------------------------------------------*
000400980904     F* DATA BASE
000500980904     F*------------------------------------------------------------------------*
000600020410     FTITAS38C  UF   E           K DISK
000700020422     F*                                    IGNORE(TITAS000)
000800000519     F                                     IGNORE(TITASP00)
000900020410     FPRTF198   O    F  198        PRINTER
001000020410     D*------------
001100020410     D* DS DI WRK
001200020410     D*------------
001300020410     D                 DS
001400020410     D TASAAS                  1      4  0
001500020410     D XXXSP1                  5      5
001600020410     D TASMGS                  6      9  0
001700020410     D XXXSP2                 10     10
001800020410     D TASLNP                 11     13  0
001900020410     D XXXSP3                 14     14
002000020410     D TASNRS                 15     16  0
002100020410     D XXXSP4                 17     17
002200020410     D TASNSP                 18     24  0
002300020410     D XXXSP5                 17     17
002400020410     D TASTBL                 26     27
002500020410     D XXXSP6                 28     28
002600020410     D TASSV1                 29     29
002700020410     D XXXSP7                 30     30
002800020410     D TASVA1                 31     41  3
002900020410     D XXXSP8                 42     42
003000020410     D TASKSC                 43     49  0
003100020410     D XXXSP9                 50     50
003200020410     D TASCCM                 51     57  0
003300020410     D prtriga                 1     57
003400020422
003500020422     I*********
003600020422     I* INDICATORI PER VERIFICA FORMATO RECORD CORRENTE (X AGGIORNAMENTO)
003700020422     I*********
003800020422     ITITAS000      01
003900020422     ITITAS010      02
004000020422     I*TITASP00      03
004100020422     I*
004200980904     C*------------------------------------------------------------------------*
004300980904     C* MAIN LINE
004400980904     C*------------------------------------------------------------------------*
004500980904     C*
004600980904     C* POSIZIONAMENTO E PRIMA LETTURA BOLLE
004700980904     C                   EXSR      SETFIL
004800980904     C*
004900980904     C* CICLO FINO A FINE FILE
005000980904DO  1C     $FINE         DOWEQ     'N'
005100980904     C*
005200020410     C* VERIFICA LA VARIA 1 DI TITAS QUANDO E' "N" E L'IMPORTO E' DI 88.888.888 ED SE SI => AZZERA I
005300020410     C                   EXSR      EXETAS
005400980904     C*
005500980904     C* LETTURA SUCCESSIVA BOLLE
005600980904     C                   EXSR      LETFIL
005700980904E   1C                   ENDDO                                                  *FINE FILE
005800980904     C*
005900020410     C     COUNTMOD      DSPLY
006000020410     C*
006100980904     C                   SETON                                        LR
006200980904     C*------------------------------------------------------------------------*
006300980904     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA                          -BOLLE-
006400980904     C*------------------------------------------------------------------------*
006500980904     C     SETFIL        BEGSR
006600980904     C*
006700020410     C                   Z-ADD     2002          KASAAS                         *ANNO INIZIALE
006800020723     C                   Z-ADD     0100          KASMGS                         *MM/GG INIZIALE
006900990802     C     KEYTAS        SETLL     TITAS38C                           99        *FINE ARCHIVIO
007000980904IF  1C     *IN99         IFEQ      *ON
007100980904     C                   MOVEL     'S'           $FINE                          *FINE PGM
007200980904X   1C                   ELSE
007300980904     C                   EXSR      LETFIL                                       *LETTURA RECORD
007400980904E   1C                   ENDIF
007500980904     C*
007600980904     C                   ENDSR
007700980904     C*------------------------------------------------------------------------*
007800980904     C* LETFIL - LETTURA PROSSIMO RECORD                                 -BOLLE-
007900980904     C*------------------------------------------------------------------------*
008000980904     C     LETFIL        BEGSR
008100980904     C*
008200980904     C* LETTURA FINO A:
008300980904     C*  - FINE FILE
008400980904     C*  - TROVATO RECORD VALIDO
008500980904     C                   MOVEL     'N'           WRECOK
008600980904DO  1C     $FINE         DOWEQ     'N'
008700980904     C     WRECOK        ANDEQ     'N'
008800020422     C                   SETOFF                                       010203
008900990802     C                   READ      TITAS38C                               99
009000980904IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
009100980904     C                   MOVEL     'S'           $FINE
009200980904X   2C                   ELSE
009300980904     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
009400980904E   2C                   ENDIF
009500980904E   1C                   ENDDO
009600980904     C*
009700980904     C                   ENDSR
009800980904     C*------------------------------------------------------------------------*
009900980904     C* CHKREC - CONTROLLA VALIDITA' RECORD                              -BOLLE-
010000980904     C*------------------------------------------------------------------------*
010100980904     C     CHKREC        BEGSR
010200020410     C*
010300020410     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
010400020403     C*
010500020403     C* PERIODO OLTRE IL RICHIESTO, ESCE
010600020410     C                   IF        TASAAS  = 2002 AND
010700020723     C                             TASMGS >= 0100 AND
010800020723     C                             TASMGS <= 0600
010900020410     C                   ELSE
011000020403     C                   MOVEL     'S'           $FINE
011100020410     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
011200020403     C                   ENDIF
011300020410     C*
011400020410     C* CONSIDERO SOLO TIPI BOLLA SOLO "AS"
011500020410     C                   IF        TASTBL = 'AS'
011600020410     C                   ELSE
011700020410     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
011800020410     C                   ENDIF
011900980904     C*
012000020410     C* VERIFICA IL TIPO VARIA 1 SULLA BOLLA
012100020405IF  1C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
012200020410     C                   IF        TASSV1 = 'N' AND
012300020410     C                             TASVA1 = 88888888,000
012400020410     C                   ADD       1             COUNTMOD
012500020410     C                   ELSE
012600020410     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
012700020410     C                   ENDIF
012800980904E   1C                   ENDIF
012900980904     C*
013000980904     C                   ENDSR
013100980904     C*------------------------------------------------------------------------*
013200020410     C* EXETAS - AGGIORNA/LISTA MODIFICHE IMPORTO VARIA -N-              -BOLLE-
013300980904     C*------------------------------------------------------------------------*
013400020410     C     EXETAS        BEGSR
013500980904     C*
013600020422     C                   EXCEPT    listato
013700020410     C                   EVAL      TASVA1 = *zeros
013800020422     C   01              UPDATE    TITAS000
013900020422     C   02              UPDATE    TITAS010
014000020422     C*  03              UPDATE    TITASP00
014100020410     C*
014200980904     C                   ENDSR
014300980904     C*------------------------------------------------------------------------*
014400980904     C* *INZSR - OPERAZIONI INIZIALI
014500980904     C*------------------------------------------------------------------------*
014600980904     C     *INZSR        BEGSR
014700980904     C*
014800980904     C* VARIABILI RIFERITE AL DATABASE
014900990802     C     *LIKE         DEFINE    TASAAS        KASAAS                         *TITAS00F
015000981215     C     *LIKE         DEFINE    TASMGS        KASMGS
015100020410     C*
015200020410     C* VARIABILI DI WRK
015300020410     C                   MOVEL     'N'           $FINE             1            *FINE PGM
015400020410     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
015500020410     C                   Z-ADD     *zeros        COUNTMOD          6 0          *CONTA BOLLE MODIF.
015600980904     C*
015700980904     C* CHAIVI DI LETTURA
015800981215     C     KEYTAS        KLIST                                                  *WFREP00F
015900981215     C                   KFLD                    KASAAS                          -ANNO
016000981215     C                   KFLD                    KASMGS                          -MESE/GIORNO
016100980904     C*
016200980904     C                   ENDSR
016300020410
016400020410     OPRTF198   E            listato     2
016500020410     O                       prtriga
