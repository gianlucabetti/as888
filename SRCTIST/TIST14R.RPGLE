000100981118     H*------------------------------------------------------------------------*
000200981118     H* REPORT COMMERCIALE - SISTEMA ORGANIGRAMMA REPORT                       *
000300981118     H*------------------------------------------------------------------------*
000400981118     H DECEDIT('0,') DATEDIT(*DMY.)
000500981118     F*------------------------------------------------------------------------*
000600981118     F* DATA BASE
000700981118     F*------------------------------------------------------------------------*
000800981118     F*---
000900981118     F* WORKFILE
001000981118     F*---
001100050408     FWFREP11L  UF   E           K DISK
001200981118     F*---
001300981118     F* ORGANIGRAMMA
001400981118     F*---
001500981118     FAZORG01L  IF   E           K DISK
001600981118     D*------------------------------------------------------------------------*
001700981118     D* SCHIERE
001800981118     D*------------------------------------------------------------------------*
001900981118     D*---
002000981118     D* ORGANIGRAMMA
002100981118     D*---
002200981118     D FIL             S              3  0 DIM(500)                             *FILIALE
002300981118     D FARE            S              3  0 DIM(500)                             *AREA
002400981118     D FDIV            S              1    DIM(500)                             *DIVISIONE
002500981118     D*------------------------------------------------------------------------*
002600981118     D* INPUT
002700981118     D*------------------------------------------------------------------------*
002800981118     D*---
002900981118     D* DS PARAMETRI INPUT
003000981118     D*---
003100981118     D PARAM           DS
003200981118     D  PARAMI                 1      6  0
003300981118     D  PARAAI                 1      4  0
003400981118     D  PARMMI                 5      6  0
003500981118     D  PARAMF                 7     12  0
003600981118     D  PARAAF                 7     10  0
003700981118     D  PARMMF                11     12  0
003800981118     D*---
003900981118     D* ARCHITETTURA
004000981118     D*---
004100981118     D KPJBA         E DS
004200981118     C*------------------------------------------------------------------------*
004300981118     C* MAIN LINE
004400981118     C*------------------------------------------------------------------------*
004500981118     C*
004600981118     C* POSIZIONAMENTO E PRIMA LETTURA REPORT COMMERCIALE
004700981118     C                   EXSR      SETFIL
004800981118     C*
004900981118     C* CICLO FINO A FINE FILE
005000981118DO  1C     $FINE         DOWEQ     'N'
005100981118     C*
005200981118     C* AGGIORNA IL REPORT
005300981118     C                   EXSR      SCRREP
005400981118     C*
005500981118     C* LETTURA SUCCESSIVA REPORT COMMERCIALE
005600981118     C                   EXSR      LETFIL
005700981118E   1C                   ENDDO                                                  *FINE FILE
005800981118     C*
005900981118     C                   SETON                                        LR
006000981118     C*------------------------------------------------------------------------*
006100981118     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA             -REPORT COMMERCIALE-
006200981118     C*------------------------------------------------------------------------*
006300981118     C     SETFIL        BEGSR
006400981118     C*
006500050408     C     *LOVAL        SETLL     WFREP11L                           99        *FINE ARCHIVIO
006600981118IF  1C     *IN99         IFEQ      *ON
006700981118     C                   MOVEL     'S'           $FINE                          *FINE PGM
006800981118X   1C                   ELSE
006900981118     C                   EXSR      LETFIL                                       *LETTURA RECORD
007000981118E   1C                   ENDIF
007100981118     C*
007200981118     C                   ENDSR
007300981118     C*------------------------------------------------------------------------*
007400981118     C* LETFIL - LETTURA PROSSIMA RECORD                    -REPORT COMMERCIALE-
007500981118     C*------------------------------------------------------------------------*
007600981118     C     LETFIL        BEGSR
007700981118     C*
007800981118     C* LETTURA FINO A:
007900981118     C*  - FINE FILE
008000981118     C*  - TROVATO RECORD VALIDO
008100981118     C                   MOVEL     'N'           WRECOK
008200981118DO  1C     $FINE         DOWEQ     'N'
008300981118     C     WRECOK        ANDEQ     'N'
008400050408     C                   READ      WFREP11L                               99
008500981118IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
008600981118     C                   MOVEL     'S'           $FINE
008700981118X   2C                   ELSE
008800981118     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
008900981118E   2C                   ENDIF
009000981118E   1C                   ENDDO
009100981118     C*
009200981118     C                   ENDSR
009300981118     C*------------------------------------------------------------------------*
009400981118     C* CHKREC - CONTROLLA VALIDITA' RECORD                 -REPORT COMMERCIALE-
009500981118     C*------------------------------------------------------------------------*
009600981118     C     CHKREC        BEGSR
009700981118     C*
009800981118     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
009900981118     C*
010000981118     C                   ENDSR
010100981118     C*------------------------------------------------------------------------*
010200981118     C* SCRREP - AGGIORNA RECORD                            -REPORT COMMERCIALE-
010300981118     C*------------------------------------------------------------------------*
010400981118     C     SCRREP        BEGSR
010500981118     C*
010600981118     C* RECUPERA ALCUNI DATI DELL'AGENTE
010700981118     C                   Z-ADD     1             I
010800981118     C                   SETOFF                                           28
010900981118     C     REPFIL        LOOKUP    FIL(I)                                 28
011000981118IF  1C     *IN28         IFEQ      *ON                                          *TROVATO
011100981118     C                   MOVEL     FDIV(I)       WDIV                           *DIVISIONE
011200981118     C                   Z-ADD     FARE(I)       WARE                           *AREA
011300981118     C*
011400981118     C* AGGIORNA IL REPORT, SE DATI MODIFICATI
011500981118IF  2C     REPDIV        IFNE      WDIV                                         *MODIFICATO QC.
011600981118     C     REPARE        ORNE      WARE
011700981118     C                   MOVEL     WDIV          REPDIV                         *DIVISIONE
011800981118     C                   Z-ADD     WARE          REPARE                         *AREA
011900981118     C                   EXCEPT    AGGREP                                       *AGGIORNA
012000981118E   2C                   ENDIF
012100981118E   1C                   ENDIF
012200981118     C*
012300981118     C                   ENDSR
012400981118     C*------------------------------------------------------------------------*
012500981118     C* CARTAB - CARICAMENTO TABELLE OCCORRENTI
012600981118     C*------------------------------------------------------------------------*
012700981118     C     CARTAB        BEGSR
012800981118     C*---
012900981118     C* ORGANIGRAMMA
013000981118     C*---
013100981118     C                   Z-ADD     *ZEROS        I                 4 0
013200981118     C     *LOVAL        SETLL     AZORG01L
013300981118     C                   READ      AZORG01L                               99
013400981118DO  1C     *IN99         DOWEQ     *OFF
013500981118IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
013600981118IF  3C*---       ORGFAG    IFNE 'V'                        *NO "VARIE"
013700981118     C                   ADD       1             I
013800981118     C                   Z-ADD     ORGFIL        FIL(I)                         *FILIALE
013900981118     C                   Z-ADD     ORGCAR        FARE(I)                        *AREA
014000981118     C                   MOVEL     ORGFL3        FDIV(I)                        *DIVISIONE
014100981118E   3C*---                 ENDIF
014200981118E   2C                   ENDIF
014300981118     C                   READ      AZORG01L                               99
014400981118E   1C                   ENDDO
014500981118     C*
014600981118     C                   ENDSR
014700981118     C*------------------------------------------------------------------------*
014800981118     C* *INZSR - OPERAZIONI INIZIALI
014900981118     C*------------------------------------------------------------------------*
015000981118     C     *INZSR        BEGSR
015100981118     C*
015200981118     C* RICEVIMENTO PARAMETRI
015300981118     C     *ENTRY        PLIST
015400981118     C                   PARM                    KPJBA
015500981118     C                   MOVEL     KPJBU         PARAM
015600981118     C*
015700981118     C* VARIABILI RIFERITE AL DATA BASE
015800981118     C     *LIKE         DEFINE    REPDIV        WDIV                           *DI LAROVO
015900981118     C     *LIKE         DEFINE    REPARE        WARE
016000981118     C*
016100981118     C* VARIABILI DI CONTROLLO
016200981118     C                   MOVEL     'N'           $FINE             1            *FINE PGM
016300981118     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
016400981118     C*
016500981118     C* CARICAMENTO TABELLE OCCORRENTI
016600981118     C                   EXSR      CARTAB
016700981118     C*
016800981118     C                   ENDSR
016900981118     O*------------------------------------------------------------------------*
017000981118     O* AGGIORNAMENTO FILES
017100981118     O*------------------------------------------------------------------------*
017200981118     OWFREP000  E            AGGREP
017300981118     O                       REPDIV
017400981118     O                       REPARE
