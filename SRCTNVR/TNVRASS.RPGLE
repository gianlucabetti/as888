000100000601     H DECEDIT('0,') DATEDIT(*DMY.)
000200000601      *
000300050111     FFNARB05L  UF   E           K DISK
000400050111     FFIAR601L  UF   E           K DISK
000500050111     FFIAR701L  UF   E           K DISK
000600000602      *--------------------------------------------------
000700050111     C     *LOVAL        SETLL     FNARB05L                           21
000800050111     C                   READ      FNARB05L                               21
000900000601      *
001000000601     C     *IN21         DOWEQ     *OFF
001100050111     C                   IF        ARBDCM=0
001200050111     C                   IF        ARBCBO='2 ' OR ARBCBO='6 ' OR ARBCBO='E ' OR
001300050111     C                             ARBCBO='F*' OR ARBCBO='FO' OR ARBCBO='FT' OR
001400050111     C                             ARBCBO='FW' OR ARBCBO='FY'
001500050111     C* CANCELLO LA TASSAZIONE
001600050111     C     KARB          DELETE    FIAR601L
001700050111     C     KARB          SETLL     FIAR701L
001800050111     C     KARB          READE     FIAR701L                               22
001900050111     C     *IN22         DOWEQ     *OFF
002000050111     C                   DELETE    FIAR7000
002100050111     C     KARB          READE     FIAR701L                               22
002200050111     C                   ENDDO
002300050111     C                   CLEAR                   ARBACA
002400050111     C                   CLEAR                   ARBICA
002500050111     C                   UPDATE    FNARB000
002600050111     C                   ENDIF
002700050111     C                   ENDIF
002800050111      *
002900050111     C                   READ      FNARB05L                               21
003000000601     C                   ENDDO
003100000601      *
003200950201     C                   SETON                                        LR
003300000601      *---------------------------------------------------------
003400000601      *   IMPOSTAI PUNTI DI ARRIVO (AD ORA VERONA E TORINO)
003500000601      *---------------------------------------------------------
003600050111     C     *INZSR        BEGSR
003700000601      *
003800000601     C     KARB          KLIST
003900000601     C                   KFLD                    ARBAAS
004000000601     C                   KFLD                    ARBLNP
004100000601     C                   KFLD                    ARBNRS
004200000601     C                   KFLD                    ARBNSP
004300050111     C                   ENDSR
