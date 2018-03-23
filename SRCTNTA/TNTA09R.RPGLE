000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200970122     H* TNTA09R *----------------------------------------------------*
000300000000     H*-----------------*                                            *
000400011001     H*         ELIMINAZIONE FISICA TEETC/TEETT/TEETS ANNULLATI      *
000500000000     H*--------------------------------------------------------------*
000600011001     FTEETC00F  UF   E             DISK
000700011001     FTEETC01L  IF   E           K DISK
000800011001     F                                     RENAME(TEETC000:TEETC001)
000900970115     F                                     INFDS(ETC1)
001000011001     FTEETT00F  UF   E             DISK
001100011001     FTEETT01L  IF   E           K DISK
001200011001     F                                     RENAME(TEETT000:TEETT001)
001300970115     F                                     INFDS(ETT1)
001400011001     FTEETS00F  UF   E             DISK
001500011001     FTEETS01L  IF   E           K DISK
001600011001     F                                     RENAME(TEETS000:TEETS001)
001700970115     F                                     INFDS(ETS1)
001701061009     FTEETS10F  UF   E             DISK
001702061006     F                                     RENAME(TEETS000:TEETS102)
001703061009     FTEETS11L  IF   E           K DISK
001704061006     F                                     RENAME(TEETS000:TEETS101)
001705061006     F                                     INFDS(ETS2)
001800970115     D ETC1            DS
001900970115     D  ETCNRR               397    400B 0
002000970115     D ETT1            DS
002100970115     D  ETTNRR               397    400B 0
002200970115     D ETS1            DS
002300970115     D  ETSNRR               397    400B 0
002301061006     D ETS2            DS
002302061006     D  ETSNRR1              397    400B 0
002400921013     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002500000000     D KPJBA         E DS
002600921015     C*****************************************************************
002700921015     C* RIEPILOGO INDICATORI
002800921015     C*****************************************************************
002900921015     C* 10    - UTILIZZATO PER GESTIRE L'ALLOCAZIONE DEL RECORD : SE
003000970115     C*         IL RECORD E' ALLOCATO (CIOE' *IN10 ACCESO) LO SALTO
003100970115     C*         E LO DELETERO' LA PROSSIMA VOLTA
003200940713     C* 30/32 - DI COMODO
003300921015     C*****************************************************************
003400000000     C     *ENTRY        PLIST
003500000000     C                   PARM                    KPJBA
003600950207     C                   Z-ADD     1             CODUT
003700921013     C*
003800921013     C                   CALL      'X§PARUT'
003900921013     C                   PARM                    UT§DSE
004000000000     C*---------------------------------------------------------------*
004100011001     C****  PULIZIA  TEETC  ****
004200011001     C     *LOVAL        SETLL     TEETC001
004300011001     C                   READ      TEETC001                               32
004400970115     C*
004500970115    1C     *IN32         DOWEQ     *OFF
004600040729     C* ELABORO SOLO I RECORD ANNULLATI
004700970115    2C     ETCATB        IFNE      ' '
004800011001     C     ETCNRR        CHAIN     TEETC000                           3010
004900011001     C  N30
005000011001     CANN10              DELETE    TEETC000
005100970115    2C                   ENDIF
005200921028     C*
005300011001     C                   READ      TEETC001                               32
005400970115    1C                   ENDDO
005500921016     C*
005600011001     C****  PULIZIA  TEETT  ****
005700011001     C     *LOVAL        SETLL     TEETT001
005800011001     C                   READ      TEETT001                               32
005900970115     C*
006000970115    1C     *IN32         DOWEQ     *OFF
006100040729     C* ELABORO SOLO I RECORD ANNULLATI
006200970115    2C     ETTATB        IFNE      ' '
006300011001     C     ETTNRR        CHAIN     TEETT000                           3010
006400011001     C  N30
006500011001     CANN10              DELETE    TEETT000
006600970115    2C                   ENDIF
006700921028     C*
006800011001     C                   READ      TEETT001                               32
006900970115    1C                   ENDDO
007000921012     C*
007100061006     C****  PULIZIA  TEETS00****
007200011001     C     *LOVAL        SETLL     TEETS001
007300011001     C                   READ      TEETS001                               32
007400970115    1C     *IN32         DOWEQ     *OFF
007500040729     C* ELABORO SOLO I RECORD ANNULLATI
007600970115    2C     ETSATB        IFNE      ' '
007700011001     C     ETSNRR        CHAIN     TEETS000                           3010
007800011001     C  N30
007900011001     CANN10              DELETE    TEETS000
008000970115    2C                   ENDIF
008100921028     C*
008200011001     C                   READ      TEETS001                               32
008300970115    1C                   ENDDO
008301061006     C****  PULIZIA  TEETS10****
008302061006     C     *LOVAL        SETLL     TEETS101
008303061006     C                   READ      TEETS101                               32
008304061006    1C     *IN32         DOWEQ     *OFF
008305061006     C* ELABORO SOLO I RECORD ANNULLATI
008306061009    2C     ETSATB        IFNE      ' '
008307061006     C     ETSNRR1       CHAIN     TEETS102                           3010
008308061006     C  N30
008309061006     CANN10              DELETE    TEETS102
008310061006    2C                   ENDIF
008311061006     C*
008312061006     C                   READ      TEETS101                               32
008313061006    1C                   ENDDO
008400921012     C*
008500000000     C                   SETON                                        LR
