000100001129     H*------------------------------------------------------------------------*
000200001129     H* SIMULAZIONE CONTO ECONOMICO - SISTEMA VOCI E TARIFFE
000300001129     H*------------------------------------------------------------------------*
000400001129     H DECEDIT('0,') DATEDIT(*DMY.)
000500001129     F*------------------------------------------------------------------------*
000600001129     F* DATA BASE
000700001129     F*------------------------------------------------------------------------*
000800001129     F*---
000900001129     F* Tariffe
001000001129     F*---
001100140114     FECEVD06L  UF A E           K DISK
001200001129     D*------------------------------------------------------------------------*
001300001129     D* DATA STRUCTURE
001400001129     D*------------------------------------------------------------------------*
001500001129     D*-------------------
001600001129     D* CONTROLLO DATA (8)
001700001129     D*-------------------
001800001129     D WLBDA8          DS                  INZ
001900001129     D  G08DAT                 1      8  0
002000001129     D  G08INV                 9     16  0
002100001129     D  G08ERR                17     17
002200001129     D  G08TGI                18     22  0
002300001129     C*------------------------------------------------------------------------*
002400001129     C* MAIN LINE
002500001129     C*------------------------------------------------------------------------*
002600001129     C*---
002700001201     C* TARIFFE CLIENTI
002800001129     C*---
002900001129     C*
003000001201     C* FACCIO SCADERE VECCHIA VOCE 20
003100001201     C                   Z-ADD     20            KVDVOC
003200001129     C                   Z-ADD     1             KVDPRG
003300001201     C                   MOVEL     'K'           KVDTTV
003400140114     C     KEYEVD        SETLL     ECEVD06L
003500140114     C     KEYEVD        READE     ECEVD06L                               99
003600001129DO  1C     *IN99         DOWEQ     *OFF
003700001129IF  2C     EVDATB        IFEQ      *BLANKS
003800001129IF  3C     DATCOR        IFGE      EVDDDT
003900001129     C     DATCOR        ANDLE     EVDDST
004000001129     C                   Z-ADD     20001231      EVDDST
004100001129     C                   MOVEL     *BLANKS       EVDFTR
004200010809     C                   Z-ADD     DATCOR        EVDDUV
004300010809     C                   UPDATE    ECEVD000
004400001129E   3C                   ENDIF
004500001129E   2C                   ENDIF
004600140114     C     KEYEVD        READE     ECEVD06L                               99
004700001129E   1C                   ENDDO
004800001129     C*
004900001201     C* COPIA DA VOCE 20 A VOCE 23 TUTTI I K
005000001201     C                   Z-ADD     20            KVDVOC
005100001129     C                   Z-ADD     1             KVDPRG
005200001201     C                   MOVEL     'K'           KVDTTV
005300140114     C     KEYEVD        SETLL     ECEVD06L
005400140114     C     KEYEVD        READE     ECEVD06L                               99
005500001129DO  1C     *IN99         DOWEQ     *OFF
005600001129IF  2C     EVDATB        IFEQ      *BLANKS
005700001129IF  3C     DATCOR        IFGE      EVDDDT
005800001129     C     DATCOR        ANDLE     EVDDST
005900001201     C                   Z-ADD     23            EVDVOC
006000001129     C                   Z-ADD     20010101      EVDDDT
006100001129     C                   Z-ADD     20091231      EVDDST
006200001129     C                   Z-ADD     *ZEROS        EVDDTR
006300001129     C                   MOVEL     *BLANKS       EVDFTR
006400010809     C                   Z-ADD     DATCOR        EVDDIR
006500010809     C                   WRITE     ECEVD000
006600001129E   3C                   ENDIF
006700001129E   2C                   ENDIF
006800140114     C     KEYEVD        READE     ECEVD06L                               99
006900001129E   1C                   ENDDO
007000001129     C*
007100001129     C                   SETON                                            LR
007200001129     C*------------------------------------------------------------------------*
007300001129     C* *INZSR - OPERAZIONI INIZIALI
007400001129     C*------------------------------------------------------------------------*
007500001129     C     *INZSR        BEGSR
007600001129     C*
007700001129     C* VARIABILI RIFERITE AL DATABASE
007800001129     C     *LIKE         DEFINE    EVDVOC        KVDVOC
007900001129     C     *LIKE         DEFINE    EVDPRG        KVDPRG
008000001129     C     *LIKE         DEFINE    EVDTTV        KVDTTV
008100001129     C*
008200001129     C* CHIAVI DI LETTURA
008300001129     C     KEYEVD        KLIST
008400001129     C                   KFLD                    KVDVOC
008500001129     C                   KFLD                    KVDPRG
008600001129     C                   KFLD                    KVDTTV
008700001129     C*
008800001129     C* IMPOSTA DATA E ORA CORRENTE
008900001129     C                   TIME                    N14              14 0
009000001129     C                   MOVE      N14           N8                8 0
009100001129     C                   Z-ADD     N8            G08DAT
009200001129     C                   Z-ADD     *ZEROS        G08INV
009300001129     C                   MOVEL     '0'           G08ERR
009400001129     C                   CALL      'XSRDA8'
009500001129     C                   PARM                    WLBDA8
009600001129     C                   Z-ADD     G08INV        DATCOR            8 0
009700001129     C                   MOVE      00            DATCO0            8 0
009800001129     C*
009900001129     C                   ENDSR
