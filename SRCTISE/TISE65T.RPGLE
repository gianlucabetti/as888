000100000000     C*------------------------------------------------------------------------*
000200000000     C*?ROUTINE DI EDITAZIONE CAMPI ALFANUMERICI (EDTCDE = N)
000300000000     C*------------------------------------------------------------------------*
000400000000     C* QUESTA ROUT VUOLE IN INPUT IL VALORE DA EDITARE E IL NUMERO DI DECIMALI;
000500000000     C* FORNISCE IN OUTPUT UN VALORE ALFANUMERICO CONTENENTE IL NUMERO EDITATO .
000600000000     C*
000700000000     C* INPUT :  $NUM - VALORE NUMERICO DA EDITARE (16,3)
000800000000     C*          $DEC - NUMERO DI DECIMALI (0÷3)   (1,0)
000900000000     C* OUTPUT:  $ALF - VALORE ALFANUMERICO CONTENENTE IL NUMERO EDITATO (22)
001000000000     C*
001100000000     C* DEFINIRE INOLTRE, ALL'INTERNO DEL PGM, LE SEGUENTI SCHIERE:
001200000000     C*          $N$  - 16 ELEMENTI ALFANUMERICI LUNGHI UNO
001300000000     C*          $A$  - 22 ELEMENTI ALFANUMERICI LUNGHI UNO
001400000000     C*------------------------------------------------------------------------*
001500000000     C*?IMPOSTAZIONI INIZIALI
001600000000     C*------------------------------------------------------------------------*
001700000000     C                   Z-ADD     *ZEROS        $C                3 0
001800000000     C                   Z-ADD     *ZEROS        $I                3 0
001900000000     C                   Z-ADD     *ZEROS        $A                3 0
002000000000     C                   Z-ADD     *ZEROS        $P                3 0
002100000000     C                   MOVEL     '0'           $F                1
002200000000     C                   MOVEL     '0'           $FLAG             1
002300000000     C                   MOVEL     *BLANKS       $ALF             22
002400000000     C                   MOVEL     *ZEROS        $COM             16
002500000000     C                   CLEAR                   $N$
002600000000     C                   CLEAR                   $A$
002700000000     C*------------------------------------------------------------------------*
002800000000     C*?MAIN LINE
002900000000     C*------------------------------------------------------------------------*
003000000000     C*
003100000000     C* SE IL NUMERO E' NEGATIVO LO TRASFORMO IN POSITIVO
003200000000IF  1C     $NUM          IFLT      *ZEROS
003300000000     C     $NUM          MULT      -1            $NUM
003400000000     C                   MOVEL     '1'           $FLAG                          *NUMERO NEGATIVO
003500000000E   1C                   ENDIF
003600000000     C*
003700000000     C                   MOVE      $NUM          $COM                           *ALLINEAMENTO A Dx
003800000000     C                   MOVEA     $COM          $N$                            *CAMPO --> SCHIERA
003900000000     C*
004000000000     C                   Z-ADD     4             $C
004100000000     C     $C            SUB       $DEC          $C
004200000000     C***
004300000000     C* IMPOSTO LA PARTE DECIMALE
004400000000     C***
004500000000     C                   Z-ADD     22            $A
004600000000     C                   Z-ADD     17            $I
004700000000     C     $I            SUB       $C            $I
004800000000     C*
004900000000IF  1C     $C            IFNE      4                                            *CHIESTI I DECIMALI
005000000000IF  2C     $C            DOWLT     4                                            *MASSIMO TRE DECIMAL
005100000000     C                   MOVEL     $N$($I)       $A$($A)
005200000000     C                   ADD       1             $C
005300000000     C                   SUB       1             $A
005400000000     C                   SUB       1             $I
005500000000E   2C                   ENDDO
005600000000     C                   MOVEL     ','           $A$($A)
005700000000     C                   SUB       1             $A
005800000000E   1C                   ENDIF
005900000000     C***
006000000000     C* IMPOSTO LA PARTE INTERA
006100000000     C***
006200000000DO  1C     $I            DOWGT     *ZEROS
006300000000     C                   MOVEL     $N$($I)       $A$($A)
006400000000     C                   SUB       1             $A
006500000000     C                   SUB       1             $I
006600000000     C                   ADD       1             $P
006700000000IF  2C     $P            IFEQ      3                                            *OGNI 3 NUMERI 1 '.'
006800000000     C     $I            ANDNE     *ZEROS                                       *TRANNE ULTIMA CIFRA
006900000000     C                   Z-ADD     *ZEROS        $P
007000000000     C                   MOVEL     '.'           $A$($A)
007100000000     C                   SUB       1             $A
007200000000E   2C                   ENDIF
007300000000E   1C                   ENDDO
007400000000     C***
007500000000     C* TOLGO GLI ZERI NON SIGNIFICATIVI E I PUNTI IN PIU'
007600000000     C* NON TOLGO LO ZERO PRIMA DELLA VIRGOLA
007700000000     C***
007800000000     C                   MOVEL     '0'           $F
007900000000     C                   Z-ADD     1             $A
008000000000DO  1C     $F            DOWEQ     '0'
008100000000     C     $A            ANDLE     22
008200000000     C*
008300000000IF  2C     $A            IFNE      22
008400000000     C     $A            ADD       1             $C
008500000000E   2C                   ENDIF
008600000000     C*
008700000000IF  2C     $A$($A)       IFGT      *ZEROS
008800000000     C     $A$($C)       OREQ      ','
008900000000     C                   MOVEL     '1'           $F
009000000000     C                   SUB       1             $A
009100000000X   2C                   ELSE
009200000000     C                   MOVEL     *BLANKS       $A$($A)
009300000000     C                   ADD       1             $A
009400000000E   2C                   ENDIF
009500000000E   1C                   ENDDO
009600000000     C***
009700000000     C* IMPOSTO IL SEGNO MENO
009800000000     C***
009900000000IF  1C     $FLAG         IFEQ      '1'                                          *NUMERO NEGATIVO
010000000000     C                   MOVEL     '-'           $A$($A)
010100000000E   1C                   ENDIF
010200000000     C***
010300000000     C* IMPOSTO VARIABILIE DI OUTPUT
010400000000     C***
010500000000     C                   MOVEA     $A$           $ALF
010600000000     C*------------------------------------------------------------------------*
