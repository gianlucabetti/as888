000100030407      *------------------------------------------------------------------------*
000200030407      * Lancia C/E per errori SPOT
000300030407      *------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500030407      *------------------------------------------------------------------------*
000600030407      * DATA BASE
000700030407      *------------------------------------------------------------------------*
000800030407     FECERR02L  UF   E           K DISK
000900030407      *------------------------------------------------------------------------*
001000030407      * DATA STRUCTURE
001100030407      *------------------------------------------------------------------------*
001200000000      * DS ESTERNA PER GUFO
001300000000     D DS£ECO        E DS
001400000000      * DS VALORIZZAZIONE IMMAGINE ERRORE DA TITAS
001500000000     D TRECDS        E DS
001600000000     D  D$PDS1                 1    250
001700000000     D  D$PDS2               251    500
001800000000     D  D$DDS1               501    750
001900000000     D  D$DDS2               751   1000
002000000000      * DS VALORIZZAZIONE IMMAGINE ERRORE DA TNASC
002100000000     D DS$CSB        E DS
002200000000     D  DCPDS1                 1    250
002300000000     D  DCPDS2               251    500
002400000000     D  DCDDS1               501    750
002500000000     D  DCDDS2               751   1000
002600000000      * DS VALORIZZAZIONE IMMAGINE ERRORE DA FIAR5 -TRANSITI-
002700000000     D DS$AR5T       E DS
002800000000     D  D$PAT1                 1    250
002900000000     D  D$DAT1               251    500
003000000000      * DS VALORIZZAZIONE IMMAGINE ERRORE DA FIAR5 -BANCALI-
003100000000     D DS$AR5B       E DS
003200000000     D  D$PAB1                 1    250
003300000000     D  D$DAB1               251    500
003400000000      * DS LETTURA CAMPO UNICO DI FIAR500F RECORD TRANSITI
003500000000     D DAR5TRS       E DS
003600000000      * DS LETTURA CAMPO UNICO DI FIAR500F RECORD BANCALI
003700000000     D DAR5BAN       E DS
003800030407      *------------------------------------------------------------------------*
003900030407      * MAIN LINES
004000030407      *------------------------------------------------------------------------*
004100030407      *
004200030407      * CICLO FINO A FINE FILE ERRORI
004300030407     C     *LOVAL        SETLL     ECERR02L
004400030407     C                   READ      ECERR02L                               99
004500000000DO  1C     *IN99         DOWEQ     *OFF
004600030407      *
004700030407      * RILANCIA IL CONTO ECONOMICO -NO SE FIAR5-
004800030407IF  2C     ERRFIL        IFNE      'FIAR500F'
004900000000     C                   EXSR      GUFO
005000030407E   2C                   ENDIF
005100030407      *
005200030407      * CANCELLA L'ERRORE LANCIATO -ANCHE SE FIAR5-
005300030407     C                   DELETE    ECERR
005400030407      *
005500030407      * LETTURA SUCCESSIVA ERRORI
005600030407     C                   READ      ECERR02L                               99
005700030407E   1C                   ENDDO
005800030407      *
005900000000     C                   SETON                                        LR
006000030407     C*------------------------------------------------------------------------*
006100030407     C* RILANCIA IL C/E
006200030407     C*------------------------------------------------------------------------*
006300030407     C     GUFO          BEGSR
006400030407     C*
006500030407     C* LANCIA IL PROGRAMMA DI ACCODAMENTO
006600030407     C                   MOVEL     ERRSTS        D£GSTS
006700030407     C                   Z-ADD     ERRDAM        D£GDAT
006800030407     C                   Z-ADD     ERRORM        D£GTIM
006900030407     C                   MOVEL     ERRUSR        D£GUSR
007000030407     C                   MOVEL     'TREC90R'     D£GPGF
007100030407     C                   MOVEL     ERRFIL        D£GFIL
007200030407     C                   MOVEL     ERRMBR        D£GMBR
007300030407     C                   MOVEL     ERRLIB        D£GLIB
007400030407     C                   MOVEL     'S'           D£GPDE
007500030407     C                   MOVEL     'B'           D£GTJB
007600030407     C                   MOVEL     ERRRP1        D£PRE1
007700030407     C                   MOVEL     ERRRP2        D£PRE2
007800030407     C                   MOVEL     ERRRD1        D£DRE1
007900030407     C                   MOVEL     ERRRD2        D£DRE2
008000030407     C                   CALL      'TRECD2R'
008100030407     C                   PARM                    DS£ECO
008200030407     C*
008300030407     C                   ENDSR
008400030407      *------------------------------------------------------------------------*
008500030407      * ROUTINE INIZIALE -SOLO LA PRIMA VOLTA-
008600030407      *------------------------------------------------------------------------*
008700000000     C     *INZSR        BEGSR
008800030407      *
008900000000     C                   ENDSR
