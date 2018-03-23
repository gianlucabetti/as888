000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200011212     H* TRMZ22R *-OC/38-*--------------------------------------------*
000300000000     H*-----------------*                                            *
000400000000     H*             - PULIZIA PICCOLA MANUTENZIONE -                 *
000500000000     H*--------------------------------------------------------------*
000600011212     FmzIMP00F  UF   E           K DISK
000700000000     D PARAM2          DS
000800000000     D  VDAT                   1      6  0
000900000000     D                 DS
001000000000     D*-----INVERSIONE DATE
001100000000     D  ANNODS                 1      2  0
001200000000     D  MESEDS                 3      4  0
001300000000     D  GIORDS                 5      6  0
001400000000     D  DATAIN                 1      6  0
001500000000     D KPJBA         E DS
001600000000     D  LIBSYS                92    100
001700000000     D  MSGERR               453    502
001800000000     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
001900000000     D  TCU                  398    697
002000000000     D                                     DIM(50)                              TAB FLAG CNT
002100000000     D  KCU                  698    847P 0
002200000000     D                                     DIM(50)                              TAB CAPO CONTO
002300000000     D                                     PACKEVEN
002400000000     D  DTU                  848    895P 0
002500000000     D                                     DIM(12)                              DATE PARAM
002600000000     D                                     PACKEVEN
002700000000     I/SPACE 3
002800000000     C*---------------------------------------------------------------*
002900000000     C     *ENTRY        PLIST
003000000000     C                   PARM                    KPJBA
003100000000     C                   Z-ADD     1             CODUT
003200000000     C                   CALL      'XPARUT'
003300000000     C                   PARM                    UTEDSE
003400000000     C     CODUT         CABEQ     -1            FINE
003500000000     C                   MOVEL     RAGUT         RSUT             20
003600000000     C                   MOVEL     RAGUT         RSUT2            25
003700000000     C*---------------------------------------------------------------*
003800000000     C                   MOVEL     KPJBU         PARAM2                         *
003900000000     C*ROVESCIO IN AAMMGG LA DATA RICHIESTA A VIDEO
004000000000     C                   Z-ADD     VDAT          DATAIN
004100000000     C                   EXSR      ROVDAT
004200000000     C                   Z-ADD     DATAIN        VDAT1             6 0
004300000223     C                   MOVEL     DATAIN        ANNO2             2 0
004400000000     C                   MOVE      VDAT1         DATA              8 0
004500000223     C     ANNO2         IFGT      40
004600000000     C                   MOVEL     19            DATA
004700000223     C                   ELSE
004800000223     C                   MOVEL     20            DATA
004900000223     C                   END
005000000000     C*
005100000000     C     SU            TAG
005200011212     C                   READ      mzIMP000                               06
005300000000     C   06              GOTO      FINE
005400000000     C     IMPDAT        CABGT     DATA          SU
005500011212     C                   DELETE    mzIMP000
005600000000     C                   GOTO      SU
005700000000     C     FINE          TAG
005800000000     C                   SETON                                        LR
005900000000     C***------------------------------------------------***
006000000000     CSR   ROVDAT        BEGSR                                                  *
006100000000     C     GIORDS        SUB       ANNODS        COMOD1            2 0          *
006200000000     C     COMOD1        MULT      9999          COMOD2            6 0          *
006300000000     C     COMOD2        ADD       DATAIN        DATAIN                         *
006400000000     CSR                 ENDSR                                                  *
006500000000     C***------------------------------------------------***
