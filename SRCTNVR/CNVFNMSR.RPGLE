000100990611     H*------------------------------------------------------------------------*
000200011018     H* CONVERSIONE FILES DEL MACCHINONE                                       *
000300990611     H*------------------------------------------------------------------------*
000400990611     H DECEDIT('0,') DATEDIT(*DMY.)
000500011018     FFNMSA01L  UF   E           K DISK
000600011018     FFNMSS01L  UF   E           K DISK
000700990611     C*---
000800011018     C* A seconda della società chiodo l'applicazione
000900990611     C*---
001000011018      *
001100011018      * SOCIETA' 043
001200011018     C     SOC           IFEQ      '043'
001300011018      * Impostare l'applicazione principale per la filiale
001400011018     C                   move      'V'           APLDFT            1
001500011018      *
001600011018     c                   move      SOC           FIL               3 0
001700011018     c                   move      ' '           APL               1
001800011018      * Cicli di lettura/aggiornamento
001900011018     C     KEY2          SETLL     FNMSA01L
002000011018     C     KEY2          READE     FNMSA01L                               99
002100990611DO  1C     *IN99         DOWEQ     *OFF
002200011018     C                   MOVE      APLDFT        MSAAPL
002300011018IF  2C                   UPDATE    FNMSA000
002400011018     C     KEY2          READE     FNMSA01L                               99
002500011018     C                   ENDDO
002600011018      *
002700011018     C     KEY2          SETLL     FNMSS01L
002800011018     C     KEY2          READE     FNMSS01L                               99
002900011018DO  1C     *IN99         DOWEQ     *OFF
003000011018     C                   MOVE      APLDFT        MSSAPL
003100011018IF  2C                   UPDATE    FNMSS000
003200011018     C     KEY2          READE     FNMSS01L                               99
003300011018     C                   ENDDO
003400011018     C                   ENDIF
003500011018      *
003600011018      * SOCIETA' 065
003700011018     C     SOC           IFEQ      '065'
003800011018      * Impostare l'applicazione principale per la filiale
003900011018     C                   move      'R'           APLDFT
004000011018      *
004100011018     c                   move      SOC           FIL               3 0
004200011018     C                   MOVE      ' '           APL               1
004300011018      * Cicli di lettura/aggiornamento
004400011018     C     KEY2          SETLL     FNMSA01L
004500011018     C     KEY2          READE     FNMSA01L                               99
004600011018DO  1C     *IN99         DOWEQ     *OFF
004700011018     C                   MOVE      APLDFT        MSAAPL            1
004800011018IF  2C                   UPDATE    FNMSA000
004900011018     C     KEY2          READE     FNMSA01L                               99
005000011018     C                   ENDDO
005100011018      *
005200011018     C     KEY2          SETLL     FNMSS01L
005300011018     C     KEY2          READE     FNMSS01L                               99
005400011018DO  1C     *IN99         DOWEQ     *OFF
005500011018     C                   MOVE      APLDFT        MSSAPL            1
005600011018IF  2C                   UPDATE    FNMSS000
005700011018     C     KEY2          READE     FNMSS01L                               99
005800011018     C                   ENDDO
005900011018     C                   ENDIF
006000990611     C*
006100011018     C* chiavi di lettura
006200011018     c     KEY2          KLIST                                                  *tntbe01l
006300011018     c                   KFLD                    APL                             -tabella
006400011018     c                   KFLD                    FIL                             -chiave uno
006500011018     C*
006600011018     c* ricevimento parametri
006700011018     C     *ENTRY        PLIST
006800011018     C                   PARM                    SOC               3
006900011018      *
007000990611     C                   SETON                                            LR
