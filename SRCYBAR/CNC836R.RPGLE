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
005200111110      * - Sportelli già annullati.
005300900530     C     ABICAP        IFEQ      99999
005400000321     C     ABIABI        ORGE      90000
005500080328     C     ABIFTR        OREQ      *BLANK
005600111110     C     abiAnn        OREQ      '*'                                          Già annull
005700900530     C                   GOTO      SOTTO
005800900530     C                   END
005900080328     ** Non annullo le righe trasmesse (cioè inserite o modificate) nell'ultimo mese.
006000080328     ** Potrebbero essere degli sportelli inseriti manualmente e non ancora
006100080328     ** presenti nel file scaricato dalla banca.
006200080328     C     *YMD          TEST(ED)                abiDtr
006300080328     C                   IF        NOT %ERROR() AND
006400080328     C                             %DATE(abiDtr:*YMD) > %DATE() - %MONTHS(1)
006500080328     C                   GOTO      sotto
006600080328     C                   ENDIF
006700080328     C*
006800900528     C     KABI          CHAIN     CNABI00F                           80
006900900528      *-------------
007000080328     C                   IF        *IN80 OR ANNXXX = '*'
007100900528     C   80              DO
007200900528     C                   MOVE      '*'           ABIANN
007300900529     C                   MOVE      ' '           ABIFTR
007400900529     C                   MOVE      *ZEROS        ABIDTR
007500900528     C                   UPDATE    CNABI
007600900528     C                   ADD       1             CONT
007700030806     C                   ENDDO
007800900528      **************
007900900528      * PRIMA VOLTA
008000900528     C   85              EXCEPT    TESTA
008100900528     C   85              SETOFF                                       85
008200900528      **************
008300900528     C   OF              EXCEPT    TESTA
008400900528     C                   EXCEPT    RIGA
008500030806     C                   ENDIF
008600900528      *-------------
008700900530     C     SOTTO         TAG
008800900529     C                   READ      CNABI                                  10
008900900528     C                   END
009000900528      *--------------------
009100900528     C   OF              EXCEPT    TESTA
009200900528     C                   EXCEPT    CODA
009300900528      *
009400900316     C     FINE          TAG
009500900316     C                   SETON                                        LR
009600900528      *---------------------------------------------------------------*
009700900316     OQSYSPRT   E            TESTA            01
009800900316     O                       RSUT                20
009900900528     O                                           60 '* COD.ABI VECCHI'
010000900528     O                                         +  0 ' ANNULLATI *'
010100900528     O                                          108 '-CNC836R-'
010200900316     O                       UDATE         Y    118
010300900528     O                                          128 'Pag.'
010400900316     O                       PAGE               132
010500900528      *
010600900528     O          E            TESTA            02
010700900528     O                       TRATT             +  0
010800900528      *
010900900528     O          E            RIGA        1
011000900530     O                       ABIABI        Z   +  0
011100900530     O                       ABICAB        Z   +  1
011200900530     O                       ABIIST            +  1
011300900530     O                       ABIAGE            +  0
011400900528      *
011500900528     O          E            CODA        2
011600900528     O                       TRATT             +  0
011700900528      *
011800900528     O          E            CODA        1
011900900528     O                                         +  0 'Annullati'
012000900528     O                       CONT          Z   +  0
012100900528     O                                         +  2 'record'
012200900528      *
012300900528     O          E            CODA        1
012400900528     O                       TRATT             +  0
