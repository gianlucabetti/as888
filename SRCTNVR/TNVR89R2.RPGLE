000100010205     H DECEDIT('0,') DATEDIT(*DMY.)
000200020807     FECERR00F  IF   E             DISK
000300020807     FECCET30C  UF   E           K DISK
000400020807     FTITAS30C  UF   E           K DISK
000500020807     F                                     IGNORE(TITASP00)
000600020807     ITITAS000      01
000700020807     ITITAS010      02
000800010205     C*------------------------------------------------------------------------*
000900010205     C* MAIN LINES
001000010205     C*------------------------------------------------------------------------*
001100010205     C***
001200010205     C* LEGGE GLI ERRORI
001300010205     C***
001400020807     C
001500020807     C                   READ      ECERR00F                               99
001600010205DO  1C     *IN99         DOWEQ     *OFF
001700010205     C*
001800010205     C* CONTROLLA SE ERRORE DA CONSIDERARE
001900010205     C                   EXSR      CHKERR
002000010205IF  2C     WRECOK        IFEQ      'S'
002100020807     C***
002200020807     C* AGGIORNA LE COMPETENZE
002300020807     C***
002400020807     C     KEYERR        CHAIN     ECCET30C                           98
002500020807if  3c     *in98         ifeq      *off
002600020807     C                   MOVEL     'N'           CETDET
002700020807     C                   UPDATE    ECCET000
002800020807     C                   SETOFF                                       0102
002900020807     C     KEYERR        CHAIN     TITAS30C                           97
003000020807if  4c     *in97         ifeq      *off
003100020807     C                   MOVEL     '.'           TASLL2
003200020807     C   01              UPDATE    TITAS010
003300020807     C   02              UPDATE    TITAS010
003400020807E   4C                   ENDIF
003500020807E   3C                   ENDIF
003600020807E   2C                   ENDIF
003700010205     C*
003800010205     C* LETTURA SUCCESSIVA ERRORI DA RILANCIARE
003900020807     C                   READ      ECERR00F                               99
004000010205E   1C                   ENDDO
004100010205     C*
004200010205     C                   SETON                                        LR
004300010205     C*------------------------------------------------------------------------*
004400010205     C* CHKERR - CONTROLLA VALIDITA' ERRORE
004500010205     C*------------------------------------------------------------------------*
004600010205     C     CHKERR        BEGSR
004700010205     C*
004800010205     C                   MOVEL     'S'           WRECOK            1
004900020805     C*
005000020807IF  1C     ERRCER        IFEQ      'NCED'
005100020807X   1C                   ELSE
005200020807     C                   MOVEL     'N'           WRECOK
005300020807E   1C                   ENDIF
005400010205     C*
005500010205     C                   ENDSR
005600010205     C*------------------------------------------------------------------------*
005700010205     C*  *INZSR - ROUTINE INIZIALE
005800010205     C*------------------------------------------------------------------------*
005900010205     C     *INZSR        BEGSR
006000020807     C     KEYERR        KLIST
006100020807     C                   KFLD                    ERRAAS
006200020807     C                   KFLD                    ERRLNP
006300020807     C                   KFLD                    ERRNRS
006400020807     C                   KFLD                    ERRNSP
006500020807     C                   KFLD                    ERRTBL
006600010205     C*
006700010205     C                   ENDSR
