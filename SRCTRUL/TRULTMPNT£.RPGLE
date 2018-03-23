000100130122     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200130122
000300130123     ***************************************************************************
000400130123     **
000500130123     ** Costanti.
000600130123     **
000700130123     ***************************************************************************
000800130122     D REAL_PRN        C                   '0'
000900130122     D PARITY_NONE     C                   '0'
001000130123     D GAITRATMP       C                   'GAITRATMP'
001100130123     D OBJECT_IN_LIBRARY_FOUND...
001200130123     D                 C                   *BLANK
001300130123     D OBJECT_IN_LIBRARY_NOT_FOUND...
001400130123     D                 C                   'CPF9801'
001500130122
001600130123     ***************************************************************************
001700130123     **
001800130123     ** Strutture dati.
001900130123     **
002000130123     ***************************************************************************
002100130123     D/COPY QSYSINC/QRPGLESRC,QUSROBJD
002200130122     D/COPY QSYSINC/QRPGLESRC,QUSEC
002300130122     D valData         DS
002400130122     D                                1A   INZ('0')
002500130122     D                                1A   INZ('1')
002600130122     D                                1A   INZ('2')
002700130122     D                                1A   INZ('3')
002800130122     D                                1A   INZ('4')
002900130122     D                                1A   INZ('5')
003000130122     D                                1A   INZ('6')
003100130122     D                                1A   INZ('7')
003200130122     D                                1A   INZ('8')
003300130122     D                                1A   INZ('9')
003400130122     D                                1A   INZ('Q')
003500130122     D                                1A   INZ('W')
003600130122     D                                1A   INZ('E')
003700130122     D                                1A   INZ('R')
003800130122     D                                1A   INZ('T')
003900130122     D                                1A   INZ('Y')
004000130122     D                                1A   INZ('U')
004100130122     D                                1A   INZ('I')
004200130122     D                                1A   INZ('O')
004300130122     D                                1A   INZ('P')
004400130122     D                                1A   INZ('L')
004500130122     D                                1A   INZ('K')
004600130122     D                                1A   INZ('J')
004700130122     D                                1A   INZ('H')
004800130122     D                                1A   INZ('G')
004900130122     D                                1A   INZ('F')
005000130122     D                                1A   INZ('D')
005100130122     D                                1A   INZ('D')
005200130122     D                                1A   INZ('S')
005300130122     D                                1A   INZ('Z')
005400130122     D                                1A   INZ('X')
005500130122     D                                1A   INZ('C')
005600130122     D                                1A   INZ('V')
005700130122     D                                1A   INZ('B')
005800130122     D                                1A   INZ('N')
005900130122     D                                1A   INZ('M')
006000130123     D  val                           1A   DIM(36) OVERLAY(valData)
006100130122
006200130123     ***************************************************************************
006300130123     **
006400130123     ** Campi.
006500130123     **
006600130123     ***************************************************************************
006700130122     D p               S              3U 0
006800130122     D randint         S             10U 0
006900130122
007000130123     ***************************************************************************
007100130123     **
007200130123     ** Prototipi.
007300130123     **
007400130123     ***************************************************************************
007500130123     D/COPY GAITRASRC/SRCPROTOPR,QUSROBJD
007600130123     D/COPY GAITRASRC/SRCPROTOPR,QC3GENPRNS
007700130123     D/COPY GAITRASRC/SRCPROTOPR,TRULTMPN
007800130123
007900130122     D*--------------------------------------------------
008000130122     D* Procedure name: trulTmpN
008100130122     D* Purpose:        Genera un nome oggetto temporaneo inesistente in GA...
008200130122     D*                          ITRATMP.
008300130122     D* Returns:
008400130122     D* Parameter:      priTipoOggetto => Tipo oggetto.
008500130122     D* Parameter:      priNomeOggetto
008600130122     D*--------------------------------------------------
008700130123     D TrulTmpN        PI
008800130122     D priTipoOggetto                 8A   CONST
008900130122     D priNomeOggetto                10A
009000130122
009100130122
009200130122      /FREE
009300130122
009400130122       CLEAR priNomeOggetto;
009500130122
009600130123       DOU priNomeOggetto <> *BLANK;
009700130123
009800130122         FOR p = 1 TO %SIZE(priNomeOggetto);
009900130122           CLEAR qusec;
010000130122           qusbprv = %SIZE(qusec);
010100130122           generatePseudorandomNumbers( %ADDR(randint)
010200130122                                      : %SIZE(randint)
010300130122                                      : REAL_PRN
010400130122                                      : PARITY_NONE
010500130122                                      : qusec );
010600130123           %SUBST(priNomeOggetto : p : 1) = val(1 + %REM(randint : %ELEM(val)));
010700130122         ENDFOR;
010800130123
010900130123         SELECT;
011000130123           WHEN priTipoOggetto = '*FILE';
011100130123             %SUBST(priNomeOggetto : 1 : 1) = 'F';
011200130123           WHEN priTipoOggetto = '*DTAARA';
011300130123             %SUBST(priNomeOggetto : 1 : 1) = 'A';
011400130123           WHEN priTipoOggetto = '*USRSPC';
011500130123             %SUBST(priNomeOggetto : 1 : 1) = 'U';
011600130123           OTHER;
011700130123             %SUBST(priNomeOggetto : 1 : 1) = 'X';
011800130123         ENDSL;
011900130123
012000130123         CLEAR qusec;
012100130123         qusbprv = %SIZE(qusec);
012200130123         RetrieveObjectDescription( QUSD0100
012300130123                                  : %SIZE(QUSD0100)
012400130123                                  : 'OBJD0100'
012500130123                                  : priNomeOggetto + GAITRATMP
012600130123                                  : qusec
012700130123                                  );
012800130123
012900130123         SELECT;
013000130123           WHEN qusei = OBJECT_IN_LIBRARY_NOT_FOUND;
013100130123             LEAVE;
013200130123           WHEN qusei = OBJECT_IN_LIBRARY_FOUND;
013300130123             CLEAR priNomeOggetto;
013400130123           OTHER;
013500130123             CLEAR priNomeOggetto;
013600130123             LEAVE;
013700130123        ENDSL;
013800130123
013900130123       ENDDO;
014000130122
014100130122       RETURN;
014200130122
014300130122      /END-FREE
014400130122
