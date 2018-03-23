000100120509     ***************************************************************************
000200120509     **
000300120509     ** Tester di YCOCTZOUT.
000400120509     **
000500120509     ***************************************************************************
000600120509     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('YCOCTZ')
000700120509
000800120509     ***************************************************************************
000900120509     **
001000120509     ** Costanti.
001100120509     **
001200120509     ***************************************************************************
001300120509      /COPY GAITRASRC/SRCCONST,YCOCTZ
001400120509      /COPY GAITRASRC/SRCCONST,YCOCTZOUT
001500120509
001600120509     ***************************************************************************
001700120509     **
001800120509     ** Strutture dati.
001900120509     **
002000120509     ***************************************************************************
002100120509     D kpjba         E DS                  QUALIFIED
002200120509
002300120509     ***************************************************************************
002400120509     **
002500120509     ** Campi.
002600120509     **
002700120509     ***************************************************************************
002800120509     D esito           S             10I 0
002900120511     D partitaIVA      S             20A   INZ('00527500540')
003000120511     D codiceFiscale   S             16A   INZ('00527500540')
003100120509     D societa         S              3A   INZ('201')
003200120511     D idCliente       S              8A   INZ('00880224')
003300120509     D capoConto       S              6A   INZ('000151')
003400120509     D dataPartita     S               D   INZ(*SYS)
003500120511     D numeroPartita   S              9P 0
003600120511     D seriePartita    S              4A
003700120511     D saldoPartita    S             15P 2
003800120509     D ctb             S              2A   INZ('CG')
003900120509
004000120509     ***************************************************************************
004100120509     **
004200120509     ** Prototipi.
004300120509     **
004400120509     ***************************************************************************
004500120509      /COPY GAITRASRC/SRCPROTOPR,YCOCTZOUT
004600120509
004700120509     ***************************************************************************
004800120509     **
004900120509     ** Parametri.
005000120509     **
005100120509     ***************************************************************************
005200120509     C     *ENTRY        PLIST
005300120509     C                   PARM                    kpjba
005400120509
005500120509     ***************************************************************************
005600120509     **
005700120509     ** Main.
005800120509     **
005900120509     ***************************************************************************
006000120509
006100120509      /FREE
006200120509
006300120509       *INLR = *ON;
006400120509
006500120509       esito = YCOCTZOUT_Init(kpjba);
006600120509
006700120510       esito = YCOCTZOUT_NewPratica( YCOCTZOUT_OPERAZIONE_INSERIRE
006800120510                                   : societa
006900120510                                   );
007000120509
007100120509       esito = YCOCTZOUT_SetSoggetto( partitaIVA
007200120509                                    : codiceFiscale
007300120509                                    : societa
007400120509                                    : idCliente
007500120509                                    );
007600120511
007700120511       dataPartita = D'2010-12-31';
007800120511       numeroPartita = 392580;
007900120511       saldoPartita = 16247,28;
008000120511
008100120511       esito = YCOCTZOUT_AddPartitaAlContenzioso( societa
008200120511                                                : ctb
008300120511                                                : capoConto
008400120511                                                : idCliente
008500120511                                                : dataPartita
008600120511                                                : numeroPartita
008700120511                                                : seriePartita
008800120511                                                : saldoPartita
008900120511                                                );
009000120509
009100120511       idCliente = '01440474';
009200120511       dataPartita = D'2012-03-31';
009300120516       numeroPartita = 39713;
009400120516       saldoPartita = -5,20;
009500120511
009600120511       esito = YCOCTZOUT_AddPartitaAlContenzioso( societa
009700120509                                                : ctb
009800120509                                                : capoConto
009900120509                                                : idCliente
010000120509                                                : dataPartita
010100120509                                                : numeroPartita
010200120509                                                : seriePartita
010300120509                                                : saldoPartita
010400120509                                                );
010500120509
010600120511       idCliente = '00850265';
010700120511       dataPartita = D'2011-10-17';
010800120511       numeroPartita = 22088;
010900120516       saldoPartita = -35,79;
011000120509
011100120509       esito = YCOCTZOUT_AddPartitaAlContenzioso( societa
011200120509                                                : ctb
011300120509                                                : capoConto
011400120509                                                : idCliente
011500120509                                                : dataPartita
011600120509                                                : numeroPartita
011700120509                                                : seriePartita
011800120509                                                : saldoPartita
011900120509                                                );
012000120510
012100120511       idCliente = '00850265';
012200120511       dataPartita = D'2011-11-03';
012300120511       numeroPartita = 39581;
012400120511       saldoPartita = 7,80;
012500120511
012600120510       esito = YCOCTZOUT_AddPartitaAlContenzioso( societa
012700120510                                                : ctb
012800120510                                                : capoConto
012900120510                                                : idCliente
013000120510                                                : dataPartita
013100120510                                                : numeroPartita
013200120510                                                : seriePartita
013300120510                                                : saldoPartita
013400120510                                                );
013500120511
013600120511       idCliente = '00850265';
013700120511       dataPartita = D'2011-11-03';
013800120511       numeroPartita = 53374;
013900120511       saldoPartita = 7,80;
014000120511
014100120511       esito = YCOCTZOUT_AddPartitaAlContenzioso( societa
014200120511                                                : ctb
014300120511                                                : capoConto
014400120511                                                : idCliente
014500120511                                                : dataPartita
014600120511                                                : numeroPartita
014700120511                                                : seriePartita
014800120511                                                : saldoPartita
014900120511                                                );
015000120511
015100120511       idCliente = '00850265';
015200120511       dataPartita = D'2011-12-31';
015300120511       numeroPartita = 67178;
015400120511       saldoPartita = 15,61;
015500120511
015600120511       esito = YCOCTZOUT_AddPartitaAlContenzioso( societa
015700120511                                                : ctb
015800120511                                                : capoConto
015900120511                                                : idCliente
016000120511                                                : dataPartita
016100120511                                                : numeroPartita
016200120511                                                : seriePartita
016300120511                                                : saldoPartita
016400120511                                                );
016500120511
016600120511       idCliente = '00850265';
016700120511       dataPartita = D'2012-02-28';
016800120511       numeroPartita = 8695;
016900120511       saldoPartita = 4,76;
017000120511
017100120511       esito = YCOCTZOUT_AddPartitaAlContenzioso( societa
017200120511                                                : ctb
017300120511                                                : capoConto
017400120511                                                : idCliente
017500120511                                                : dataPartita
017600120511                                                : numeroPartita
017700120511                                                : seriePartita
017800120511                                                : saldoPartita
017900120511                                                );
018000120511
018100120511       idCliente = '00850265';
018200120511       dataPartita = D'2012-02-29';
018300120511       numeroPartita = 23821;
018400120511       saldoPartita = 4,76;
018500120511
018600120511       esito = YCOCTZOUT_AddPartitaAlContenzioso( societa
018700120511                                                : ctb
018800120511                                                : capoConto
018900120511                                                : idCliente
019000120511                                                : dataPartita
019100120511                                                : numeroPartita
019200120511                                                : seriePartita
019300120511                                                : saldoPartita
019400120511                                                );
019500120509
019600120509       esito = YCOCTZOUT_EndPratica(*ON);
019700120509
019800120509       YCOCTZOUT_Finalize();
019900120509
020000120509      /END-FREE
