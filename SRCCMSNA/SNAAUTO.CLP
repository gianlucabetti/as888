000100000000     FSNAJOB  O   E                    DISK
000200000000     F            SNAJOB                            KRENAMERECORD
000300000000     E                    SC      1   4 80
000400000000     ISNAPAR    E DS
000500000000     C           *NAMVAR   DEFN *LDA      SNAPAR
000600000000     C                     IN   SNAPAR
000700000000     C                     MOVELSNAPAR    SC,3
000800000000     C                     DO   4         I       10
000900000000     C                     MOVELSC,I      SRCDTA
001000000000     C                     WRITERECORD
001100000000     C                     END
001200000000     C                     SETON                     LR
001300000000**
001400000000//JOB      JOB(SNAJOB) RQSDTA('call snaric')
001500000000// DATA
001600000000
001700000000// ENDJOB
