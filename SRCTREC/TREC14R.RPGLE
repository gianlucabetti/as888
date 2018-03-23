000100000000     H*------------------------------------------------------------------------*
000200991117     H* SIMULAZIONE CONTO ECONOMICO - SITEMA VOCI E TARIFFE
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800991117     F*---
000900991117     F* Competenze
001000991117     F*---
001100991117     FECECT01L  UF   E           K DISK
001200991117     F*---
001300991117     F* Conti
001400991117     F*---
001500991117     FECECD01L  UF   E           K DISK
001600991117     F*---
001700991117     F* Tariffe
001800991117     F*---
001900140114     FECEVD06L  UF A E           K DISK
002000991117     D*------------------------------------------------------------------------*
002100991117     D* DATA STRUCTURE
002200991117     D*------------------------------------------------------------------------*
002300991117     D*-------------------
002400991117     D* CONTROLLO DATA (8)
002500991117     D*-------------------
002600991117     D WLBDA8          DS                  INZ
002700991117     D  G08DAT                 1      8  0
002800991117     D  G08INV                 9     16  0
002900991117     D  G08ERR                17     17
003000991117     D  G08TGI                18     22  0
003100000000     C*------------------------------------------------------------------------*
003200000000     C* MAIN LINE
003300000000     C*------------------------------------------------------------------------*
003400991117     C*---
003500991117     C* COMPETENZE
003600991117     C*---
003700991117     C* -SPOSTA LA SCADENZA AL 01/12/2009
003800991117     C     00037         SETLL     ECECT01L
003900991117     C     00037         READE     ECECT01L                               99
004000991117DO  1C     *IN99         DOWEQ     *OFF
004100991117IF  2C     ECTATB        IFEQ      *BLANKS
004200991119IF  3C     DATCO0        IFGE      ECTDDC
004300991119     C     DATCO0        ANDLE     ECTDSC
004400991117     C                   Z-ADD     20091200      ECTDSC
004500991117     C                   Z-ADD     *ZEROS        ECTDTR
004600991117     C                   MOVEL     *BLANKS       ECTFTR
004700991117     C                   UPDATE    ECECT
004800991119E   3C                   ENDIF
004900991119E   2C                   ENDIF
005000991117     C     00037         READE     ECECT01L                               99
005100991117E   1C                   ENDDO
005200991117     C*---
005300991117     C* CONTI DI COMPETENZA
005400991117     C*---
005500991117     C* -SPOSTA LA SCADENZA AL 00/12/2009
005600991117     C     00037         SETLL     ECECD01L
005700991117     C     00037         READE     ECECD01L                               99
005800991117DO  1C     *IN99         DOWEQ     *OFF
005900991117IF  2C     ECDATB        IFEQ      *BLANKS
006000991119IF  3C     DATCO0        IFGE      ECDDDO
006100991119     C     DATCO0        ANDLE     ECDDSO
006200991117     C                   Z-ADD     20091200      ECDDSO
006300991117     C                   Z-ADD     *ZEROS        ECDDTR
006400991117     C                   MOVEL     *BLANKS       ECDFTR
006500991117     C                   UPDATE    ECECD
006600991119E   3C                   ENDIF
006700991119E   2C                   ENDIF
006800991117     C     00037         READE     ECECD01L                               99
006900991117E   1C                   ENDDO
007000991117     C*---
007100991117     C* TARIFFE
007200991117     C*---
007300991117     C* -FA SCADERE AL 31/21/1999 LE TARIFFE DELLA VOCE 210/1
007400991117     C* -CREA NUOVE TARIFFE CON DECORRENZA 01/01/2000 E SCADENZA 31/12/2009
007500991117     C*  MAGGIORANDO L'IMPORTO TARIFFA DEL 3%  (SOLO FILIALE PARTENZA BARTOLINI)
007600991117     C                   Z-ADD     210           KVDVOC
007700991117     C                   Z-ADD     1             KVDPRG
007800991117     C                   MOVEL     'F'           KVDTTV
007900140114     C     KEYEVD        SETLL     ECEVD06L
008000140114     C     KEYEVD        READE     ECEVD06L                               99
008100991117DO  1C     *IN99         DOWEQ     *OFF
008200991117IF  2C     EVDATB        IFEQ      *BLANKS
008300991117IF  3C     DATCOR        IFGE      EVDDDT
008400991117     C     DATCOR        ANDLE     EVDDST
008500991117     C* MODIFICA
008600991117     C                   Z-ADD     19991231      EVDDST
008700991117     C                   MOVEL     *BLANKS       EVDFTR
008800010809     C                   Z-ADD     DATCOR        EVDDUV
008900010809     C                   UPDATE    ECEVD000
009000991117     C* INSERISCE
009100991117IF  4C     EVDFT1        IFLT      200
009200991117IF  5C     EVDITR        IFGT      *ZEROS
009300991117     C     EVDITR        MULT      103           N13              13 0
009400991117     C     N13           DIV       100           N13
009500991117     C                   Z-ADD     N13           EVDITR
009600991117E   5C                   ENDIF
009700991117     C                   Z-ADD     20000101      EVDDDT
009800991117     C                   Z-ADD     20091231      EVDDST
009900991117     C                   Z-ADD     *ZEROS        EVDDTR
010000991117     C                   MOVEL     *BLANKS       EVDFTR
010100010809     C                   Z-ADD     DATCOR        EVDDIR
010200010809     C                   WRITE     ECEVD000
010300991117E   4C                   ENDIF
010400991117E   3C                   ENDIF
010500991117E   2C                   ENDIF
010600140114     C     KEYEVD        READE     ECEVD06L                               99
010700991117E   1C                   ENDDO
010800991117     C*
010900991117     C* -FA SCADERE AL 31/21/1999 LE TARIFFE DELLA VOCE 211/1
011000991117     C* -CREA NUOVE TARIFFE CON DECORRENZA 01/01/2000 E SCADENZA 31/12/2009
011100991117     C*  DIMINUENDO L'IMPORTO TARIFFA DEL 7%
011200991117     C                   Z-ADD     211           KVDVOC
011300991117     C                   Z-ADD     1             KVDPRG
011400991117     C                   MOVEL     'F'           KVDTTV
011500140114     C     KEYEVD        SETLL     ECEVD06L
011600140114     C     KEYEVD        READE     ECEVD06L                               99
011700991117DO  1C     *IN99         DOWEQ     *OFF
011800991117IF  2C     EVDATB        IFEQ      *BLANKS
011900991117IF  3C     DATCOR        IFGE      EVDDDT
012000991117     C     DATCOR        ANDLE     EVDDST
012100991117     C* MODIFICA
012200991117     C                   Z-ADD     19991231      EVDDST
012300991117     C                   MOVEL     *BLANKS       EVDFTR
012400010809     C                   Z-ADD     DATCOR        EVDDUV
012500010809     C                   UPDATE    ECEVD000
012600991117     C* INSERISCE
012700991117IF  4C     EVDITR        IFGT      *ZEROS
012800991117     C     EVDITR        MULT      93            N13              13 0
012900991117     C     N13           DIV       100           N13
013000991117     C                   Z-ADD     N13           EVDITR
013100991117E   4C                   ENDIF
013200991117     C                   Z-ADD     20000101      EVDDDT
013300991117     C                   Z-ADD     20091231      EVDDST
013400991117     C                   Z-ADD     *ZEROS        EVDDTR
013500991117     C                   MOVEL     *BLANKS       EVDFTR
013600010809     C                   Z-ADD     DATCOR        EVDDIR
013700010809     C                   WRITE     ECEVD000
013800991117E   3C                   ENDIF
013900991117E   2C                   ENDIF
014000140114     C     KEYEVD        READE     ECEVD06L                               99
014100991117E   1C                   ENDDO
014200991117     C*
014300000000     C                   SETON                                            LR
014400000000     C*------------------------------------------------------------------------*
014500000000     C* *INZSR - OPERAZIONI INIZIALI
014600000000     C*------------------------------------------------------------------------*
014700000000     C     *INZSR        BEGSR
014800991117     C*
014900991117     C* VARIABILI RIFERITE AL DATABASE
015000991117     C     *LIKE         DEFINE    EVDVOC        KVDVOC
015100991117     C     *LIKE         DEFINE    EVDPRG        KVDPRG
015200991117     C     *LIKE         DEFINE    EVDTTV        KVDTTV
015300991117     C*
015400991117     C* CHIAVI DI LETTURA
015500991117     C     KEYEVD        KLIST
015600991117     C                   KFLD                    KVDVOC
015700991117     C                   KFLD                    KVDPRG
015800991117     C                   KFLD                    KVDTTV
015900991117     C*
016000991117     C* IMPOSTA DATA E ORA CORRENTE
016100991117     C                   TIME                    N14              14 0
016200991117     C                   MOVE      N14           N8                8 0
016300991117     C                   Z-ADD     N8            G08DAT
016400991117     C                   Z-ADD     *ZEROS        G08INV
016500991117     C                   MOVEL     '0'           G08ERR
016600991117     C                   CALL      'XSRDA8'
016700991117     C                   PARM                    WLBDA8
016800991117     C                   Z-ADD     G08INV        DATCOR            8 0
016900991119     C                   MOVE      00            DATCO0            8 0
017000991117     C*
017100000000     C                   ENDSR
