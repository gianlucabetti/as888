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
001100130123     D QSYS            C                   'QSYS'
001200130123     D OBJECT_IN_LIBRARY_FOUND...
001300130123     D                 C                   *BLANK
001400130123     D OBJECT_IN_LIBRARY_NOT_FOUND...
001500130123     D                 C                   'CPF9801'
001600130123     D FILE_IN_LIBRARY_NOT_FOUND...
001700130123     D                 C                   'CPF9812'
001800130123     D LIBRARY_NOT_FOUND...
001900130123     D                 C                   'CPF9810'
002000130122
002100130123     ***************************************************************************
002200130123     **
002300130123     ** Strutture dati.
002400130123     **
002500130123     ***************************************************************************
002600130123     D/COPY QSYSINC/QRPGLESRC,QUSROBJD
002700130122     D/COPY QSYSINC/QRPGLESRC,QUSEC
002800130122     D valData         DS
002900130122     D                                1A   INZ('0')
003000130122     D                                1A   INZ('1')
003100130122     D                                1A   INZ('2')
003200130122     D                                1A   INZ('3')
003300130122     D                                1A   INZ('4')
003400130122     D                                1A   INZ('5')
003500130122     D                                1A   INZ('6')
003600130122     D                                1A   INZ('7')
003700130122     D                                1A   INZ('8')
003800130122     D                                1A   INZ('9')
003900130122     D                                1A   INZ('Q')
004000130122     D                                1A   INZ('W')
004100130122     D                                1A   INZ('E')
004200130122     D                                1A   INZ('R')
004300130122     D                                1A   INZ('T')
004400130122     D                                1A   INZ('Y')
004500130122     D                                1A   INZ('U')
004600130122     D                                1A   INZ('I')
004700130122     D                                1A   INZ('O')
004800130122     D                                1A   INZ('P')
004900130122     D                                1A   INZ('L')
005000130122     D                                1A   INZ('K')
005100130122     D                                1A   INZ('J')
005200130122     D                                1A   INZ('H')
005300130122     D                                1A   INZ('G')
005400130122     D                                1A   INZ('F')
005500130122     D                                1A   INZ('D')
005600130122     D                                1A   INZ('D')
005700130122     D                                1A   INZ('S')
005800130122     D                                1A   INZ('Z')
005900130122     D                                1A   INZ('X')
006000130122     D                                1A   INZ('C')
006100130122     D                                1A   INZ('V')
006200130122     D                                1A   INZ('B')
006300130122     D                                1A   INZ('N')
006400130122     D                                1A   INZ('M')
006500130123     D  val                           1A   DIM(36) OVERLAY(valData)
006600130122
006700130123     ***************************************************************************
006800130123     **
006900130123     ** Campi.
007000130123     **
007100130123     ***************************************************************************
007200130122     D p               S              3U 0
007300130122     D randint         S             10U 0
007400130123     D nomeOggetto9    S              9A
007500130122
007600130123     ***************************************************************************
007700130123     **
007800130123     ** Prototipi.
007900130123     **
008000130123     ***************************************************************************
008100130123     D/COPY GAITRASRC/SRCPROTOPR,QUSROBJD
008200130123     D/COPY GAITRASRC/SRCPROTOPR,QC3GENPRNS
008300130123     D/COPY GAITRASRC/SRCPROTOPR,TRULTMPN
008400130123
008500130122     D*--------------------------------------------------
008600130122     D* Procedure name: trulTmpN
008700130123     D* Purpose:        Genera un nome oggetto temporaneo inesistente in
008800130123     D*                 GAIITRATMP.
008900130123     D* Returns:
009000130123     D* Parameter:      priTipoOggetto => Tipo oggetto.
009100130123     D* Parameter:      priPrefissoNomeOggetto => Una lettera A-Z da mettere
009200130123     D*                 nella prima posizione del nome.
009300130123     D*                 Se passato ' ' viene assunto 'F' per *FILE,
009400130123     D*                 'A' per *DTAARA, 'S' per *USRSPC, 'L' per *LIB
009500130123     D*                 e 'Z' per altri tipi.
009600130123     D* Parameter:      priLibreria  => *BLANK = 'GAITRATMP'.
009700130123     D* Parameter:      priNomeOggetto  => output.
009800130123     D*                 Se l'elaborazione termina in modo anomalo ritorna
009900130123     D*                 un '*' seguito dal ID errore (p.e. '*CPF9810').
010000130123     D*--------------------------------------------------
010100130123     D TrulTmpN        PI
010200130123     D  priTipoOggetto...
010300130123     D                               10A   CONST
010400130123     D  priPrefissoNomeOggetto...
010500130123     D                                1A   CONST
010600130123     D  priLibreria...
010700130123     D                               10A
010800130123     D  priNomeOggetto...
010900130123     D                               10A
011000130122
011100130122      /FREE
011200130123
011300130123       IF priLibreria = *BLANK;
011400130123         priLibreria = GAITRATMP;
011500130123       ENDIF;
011600130123
011700130123       CLEAR priNomeOggetto;
011800130123       CLEAR nomeOggetto9;
011900130122
012000130123       DOU priNomeOggetto <> *BLANK;
012100130123
012200130123         FOR p = 1 TO %SIZE(nomeOggetto9);
012300130123           RESET qusec;
012400130122           generatePseudorandomNumbers( %ADDR(randint)
012500130122                                      : %SIZE(randint)
012600130122                                      : REAL_PRN
012700130122                                      : PARITY_NONE
012800130122                                      : qusec );
012900130123           IF qusei <> *BLANK;
013000130123             DUMP(A);
013100130123             priNomeOggetto = '*' + qusei;
013200130123             RETURN;
013300130123           ENDIF;
013400130123           %SUBST(nomeOggetto9 : p : 1) = val(1 + %REM(randint : %ELEM(val)));
013500130122         ENDFOR;
013600130123
013700130123         SELECT;
013800130123           WHEN priPrefissoNomeOggetto <> *BLANK;
013900130123             priNomeOggetto = priPrefissoNomeOggetto + nomeOggetto9;
014000130123           WHEN priTipoOggetto = '*FILE';
014100130123             priNomeOggetto = 'F' + nomeOggetto9;
014200130123           WHEN priTipoOggetto = '*DTAARA';
014300130123             priNomeOggetto = 'A' + nomeOggetto9;
014400130123           WHEN priTipoOggetto = '*USRSPC';
014500130123             priNomeOggetto = 'S' + nomeOggetto9;
014600130123           WHEN priTipoOggetto = '*LIB';
014700130123             priNomeOggetto = 'L' + nomeOggetto9;
014800130123             priLibreria = QSYS;
014900130123           OTHER;
015000130123             priNomeOggetto = 'Z' + nomeOggetto9;
015100130123         ENDSL;
015200130123
015300130123         RESET qusec;
015400130123         RetrieveObjectDescription( QUSD0100
015500130123                                  : %SIZE(QUSD0100)
015600130123                                  : 'OBJD0100'
015700130123                                  : priNomeOggetto + priLibreria
015800130123                                  : priTipoOggetto
015900130123                                  : qusec
016000130123                                  );
016100130123
016200130123         SELECT;
016300130123           WHEN qusei = OBJECT_IN_LIBRARY_NOT_FOUND
016400130123             OR qusei = FILE_IN_LIBRARY_NOT_FOUND
016500130123             OR (priLibreria = QSYS AND qusei = LIBRARY_NOT_FOUND);
016600130123             LEAVE;
016700130123           WHEN qusei = OBJECT_IN_LIBRARY_FOUND;
016800130123             CLEAR priNomeOggetto;
016900130123           OTHER;
017000130123             priNomeOggetto = '*' + qusei;
017100130123             LEAVE;
017200130123        ENDSL;
017300130123
017400130123       ENDDO;
017500130122
017600130122       RETURN;
017700130122
017800130123       //***********************************************************************
017900130123       //
018000130123       // Inizializzazioni.
018100130123       //
018200130123       //***********************************************************************
018300130123
018400130123       BEGSR *INZSR;
018500130123
018600130123         CLEAR qusec;
018700130123         qusbprv = %SIZE(qusec);
018800130123
018900130123       ENDSR;
019000130123
019100130122      /END-FREE
019200130122
