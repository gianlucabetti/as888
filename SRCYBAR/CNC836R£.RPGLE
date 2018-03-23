000100900528      *****************************************************************
000200900528      *                                                               *
000300900528      * ANNULLAMENTO RECORD VECCHI IN CNABI00F                        *
000400900528      *                                                        LC.    *
000500900528      *****************************************************************
000600000000     H DECEDIT('0,') DATEDIT(*DMY.)
000700900528     FCNABI00F  IF   E           K DISK
000800900529     FCNABI01L  UF   E           K DISK
000900900528     F                                     RENAME(CNABI000:CNABI)
001000900528     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001100900529     D KPJBA         E DS
001200900226     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
001300900315     D  TCU                  398    697
001400900315     D                                     DIM(50)                              TAB FLAG C
001500900315     D  KCU                  698    847P 0
001600900315     D                                     DIM(50)                              TAB CAPO C
001700900315     D                                     PACKEVEN
001800900315     D  DTU                  848    895P 0
001900900315     D                                     DIM(12)                              DATE PARAM
002000900315     D                                     PACKEVEN
002100900528      *
002200900529     ICNABI000
002300900530     I              ABIIST                      ISTXXX
002400900530     I              ABIAGE                      AGEXXX
002500030806     I              ABIANN                      ANNXXX
002600900226     I/SPACE 3
002700900528      *---------------------------------------------------------------*
002800900226     C     *ENTRY        PLIST
002900900226     C                   PARM                    KPJBA
003000900226     C                   Z-ADD     1             CODUT
003100900226     C                   CALL      'XPARUT'
003200900226     C                   PARM                    UTEDSE
003300900226     C     CODUT         CABEQ     -1            FINE
003400900226     C                   MOVEL     RAGUT         RSUT             20
003500900226     C                   MOVEL     RAGUT         RSUT2            25
003600900528      *---------------------------------------------------------------*
003700900315     C     KABI          KLIST
003800900528     C                   KFLD                    ABIABI
003900900528     C                   KFLD                    ABICAB
004000900528      *---------------------------------------------------------------*
004100900529     C     *LOVAL        SETLL     CNABI
004200900529     C                   READ      CNABI                                  10
004300900528     C                   SETON                                        85
004400900528     C                   Z-ADD     0             CONT              5 0
004500900528     C                   MOVEL     *ALL'-'       TRATT           132
004600900528      *--------------------
004700900528     C     *IN10         DOWEQ     '0'
004800900530      *    COD. CHE DEVONO RIMANERE
004900080328      * - Codice avviamento postale 99999 (boh);
005000080328      * - Sportelli esteri;
005100080328      * - Sportelli inseriti/modificati oggi e non ancora trasmessi.
005200900530     C     ABICAP        IFEQ      99999
005300000321     C     ABIABI        ORGE      90000
005400080328     C     ABIFTR        OREQ      *BLANK
005500900530     C                   GOTO      SOTTO
005600900530     C                   END
005700080328     ** Non annullo le righe trasmesse (cioè inserite o modificate) nell'ultimo mese.
005800080328     ** Potrebbero essere degli sportelli inseriti manualmente e non ancora
005900080328     ** presenti nel file scaricato dalla banca.
006000080328     C     *YMD          TEST(ED)                abiDtr
006100080328     C                   IF        NOT %ERROR() AND
006200080328     C                             %DATE(abiDtr:*YMD) > %DATE() - %MONTHS(1)
006300080328     C                   GOTO      sotto
006400080328     C                   ENDIF
006500080328     C*
006600900528     C     KABI          CHAIN     CNABI00F                           80
006700900528      *-------------
006800080328     C                   IF        *IN80 OR ANNXXX = '*'
006900900528     C   80              DO
007000900528     C                   MOVE      '*'           ABIANN
007100900529     C                   MOVE      ' '           ABIFTR
007200900529     C                   MOVE      *ZEROS        ABIDTR
007300900528     C                   UPDATE    CNABI
007400900528     C                   ADD       1             CONT
007500030806     C                   ENDDO
007600900528      **************
007700900528      * PRIMA VOLTA
007800900528     C   85              EXCEPT    TESTA
007900900528     C   85              SETOFF                                       85
008000900528      **************
008100900528     C   OF              EXCEPT    TESTA
008200900528     C                   EXCEPT    RIGA
008300030806     C                   ENDIF
008400900528      *-------------
008500900530     C     SOTTO         TAG
008600900529     C                   READ      CNABI                                  10
008700900528     C                   END
008800900528      *--------------------
008900900528     C   OF              EXCEPT    TESTA
009000900528     C                   EXCEPT    CODA
009100900528      *
009200900316     C     FINE          TAG
009300900316     C                   SETON                                        LR
009400900528      *---------------------------------------------------------------*
009500900316     OQSYSPRT   E            TESTA            01
009600900316     O                       RSUT                20
009700900528     O                                           60 '* COD.ABI VECCHI'
009800900528     O                                         +  0 ' ANNULLATI *'
009900900528     O                                          108 '-CNC836R-'
010000900316     O                       UDATE         Y    118
010100900528     O                                          128 'Pag.'
010200900316     O                       PAGE               132
010300900528      *
010400900528     O          E            TESTA            02
010500900528     O                       TRATT             +  0
010600900528      *
010700900528     O          E            RIGA        1
010800900530     O                       ABIABI        Z   +  0
010900900530     O                       ABICAB        Z   +  1
011000900530     O                       ABIIST            +  1
011100900530     O                       ABIAGE            +  0
011200900528      *
011300900528     O          E            CODA        2
011400900528     O                       TRATT             +  0
011500900528      *
011600900528     O          E            CODA        1
011700900528     O                                         +  0 'Annullati'
011800900528     O                       CONT          Z   +  0
011900900528     O                                         +  2 'record'
012000900528      *
012100900528     O          E            CODA        1
012200900528     O                       TRATT             +  0
