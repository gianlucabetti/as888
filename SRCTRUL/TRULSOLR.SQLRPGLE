000100090610     ***************************************************************************
000200090610     **
000300090611     ** Per Proj
000400090611     **
000500090611     ** Passare  i dati col formato TRULSOLI0.
000600090611     ** Chiedere i dati col formato TRULSOLO0.
000700090611     **
000800090611     ** Restituisce
000900090611     **
001000090611     ** TRULSOLO0.CLISOLL     = '1' se il cliente     è da sollecitare.
001100090611     **                       = '0' se il cliente non è da sollecitare.
001200090611     ** TRULSOLO0.CLISOLLMOD  = '1' se il valore di CLISOLL     è modificabile.
001300090611     **                       = '0' se il valore di CLISOLL non è modificabile.
001400090611     **
001500090611     **-------------------------------------------------------------------------
001600090611     **
001700090611     ** Per Orion
001800090611     **
001900090611     ** Passare  i dati col formato TRULSOLI1.
002000090611     ** Chiedere i dati col formato TRULSOLO1.
002100090611     **
002200090611     ** Restituisce
002300090611     **
002400090611     ** TRULSOLO1.SOL         = 'S' se il cliente     è da sollecitare.
002500090611     **                       = 'N' se il cliente non è da sollecitare.
002600090611     ** TRULSOLO1.SOLMOD      = 'S' se il valore di SOL     è modificabile.
002700090611     **                       = 'N' se il valore di SOL non è modificabile.
002800090610     **
002900090611     **-------------------------------------------------------------------------
003000090611     **
003100090611     ** Restituisce RPYESITO negativo in caso di elaborazione non eseguita.
003200090611     **
003300090611     ***************************************************************************
003400090610     H DFTACTGRP(*NO) BNDDIR('TIBS')
003500090610
003600090610     ***************************************************************************
003700090610     **
003800090610     ** Costanti.
003900090610     **
004000090610     ***************************************************************************
004100090610     D CLIENTI_ASSEGNATI...
004200090610     D                 C                   '9999'
004300090610     D CLIENTI_FRANCHI_VARI...
004400090610     D                 C                   '8888'
004500090617     D*CLIENTE_SOTTO_OSSERVAZIONE...
004600090617     D*                C                   ' 30'
004700090610     D ESITO_ERROR_PARAMETRI...
004800090610     D                 C                   -1
004900090610     D ESITO_ERROR_FILIALE...
005000090610     D                 C                   -2
005100090611     D*NETWORK_DPD...
005200090611     D*                C                   'DPD'
005300090611     D*NETWORK_ESPRESSO...
005400090611     D*                C                   'COR'
005500090611     D*NETWORK_MESSAGGERIA...
005600090611     D*                C                   'MES'
005700090610     D NO...
005800090610     D                 C                   'N'
005900090610     D SI...
006000090610     D                 C                   'S'
006100090610     D SOCIETA_BARTOLINI...
006200090610     D                 C                   '201'
006300090610     D TIPO_FILIALE_VARIE...
006400090610     D                 C                   'V'
006500090610
006600090610     ***************************************************************************
006700090610     **
006800090610     ** Prototipi procedure esterne.
006900090610     **
007000090610     ***************************************************************************
007100090610     D chkContoIncassiAttribuire...
007200090610     D                 PR            10I 0
007300090610     D  societa                            LIKE(andiz00f.societa)
007400090610     D  kcc                                LIKE(andiz00f.kcc)
007500090610     D  ksc                                LIKE(andiz00f.ksc)
007600090610     D  esito                        10I 0
007700090610
007800090610     ***************************************************************************
007900090610     **
008000090610     ** Strutture dati.
008100090610     **
008200090610     ***************************************************************************
008300090610     D andiz00f      E DS                  QUALIFIED
008400090610     D                                     BASED(nullPtr)
008500090610     D cndizion      E DS                  QUALIFIED
008600090610     D                                     BASED(nullPtr)
008700090610     D trulSolI0     E DS                  QUALIFIED
008800090610     D trulSolI1     E DS                  QUALIFIED
008900090610     D trulSolO0     E DS                  QUALIFIED
009000090610     D                                     INZ
009100090610     D  cliSoll      E                     INZ(*ON)
009200090611     D  cliSollMod   E                     INZ(*ON)
009300090610     D trulSolO1     E DS                  QUALIFIED
009400090610     D                                     INZ
009500090610     D  sol          E                     INZ(SI)
009600090611     D  solMod       E                     INZ(SI)
009700090610
009800090610     ***************************************************************************
009900090610     **
010000090610     ** Variabili.
010100090610     **
010200090610     ***************************************************************************
010300090610     D capoConto...
010400090610     D                 S                   LIKE(andiz00f.kcc)
010500090610     D clientiVari...
010600090610     D                 S              4A
010700090610     D esito...
010800090610     D                 S             10I 0
010900090610     D filiale...
011000090610     D                 S                   LIKE(cndizion.$fil)
011100090610     D orgFag...
011200090610     D                 S              1A
011300090611     D*orgNtw...
011400090611     D*                S              3A
011500090610     D rqsFormatName...
011600090610     D                 S             10A
011700090610     D rqsData...
011800090610     D                 S            256A
011900090610     D rqsDataSize...
012000090610     D                 S             10I 0
012100090610     D rqsOpCode...
012200090610     D                 S             10A
012300090610     D rpyFormatName...
012400090610     D                 S             10A
012500090610     D rpyData...
012600090610     D                 S            256A
012700090610     D rpyDataSize...
012800090610     D                 S             10I 0
012900090610     D rpyEsito...
013000090610     D                 S             10I 0
013100090610     D societa...
013200090610     D                 S                   LIKE(andiz00f.societa)
013300090610     D sottoConto...
013400090610     D                 S                   LIKE(andiz00f.ksc)
013500090610
013600090610     ***************************************************************************
013700090610     **
013800090610     ** Parametri.
013900090610     **
014000090610     ***************************************************************************
014100090610     C     *ENTRY        PLIST
014200090610     C                   PARM                    rqsOpCode
014300090610     C                   PARM                    rpyEsito
014400090610     C                   PARM                    rqsFormatName
014500090610     C                   PARM                    rqsData
014600090610     C                   PARM                    rqsDataSize
014700090610     C                   PARM                    rpyFormatName
014800090610     C                   PARM                    rpyData
014900090610     C                   PARM                    rpyDataSize
015000090610
015100090610     ***************************************************************************
015200090610     **
015300090610     ** Main.
015400090610     **
015500090610     ***************************************************************************
015600090610
015700090610      /FREE
015800090610
015900101102       CLEAR rpyEsito;
016000090610
016100090610       // Controllo parametri ricevuti.
016200090610
016300090610       IF rqsDataSize < 1 OR rpyDataSize < 1;
016400090610         rpyEsito = ESITO_ERROR_PARAMETRI;
016500090610         RETURN;
016600090610       ENDIF;
016700090610
016800090610       RESET clientiVari;
016900090610       RESET filiale;
017000090610
017100090610       SELECT;
017200090610         WHEN rqsFormatName = 'TRULSOLI0';
017300090610           trulSolI0 = %SUBST(rqsData : 1 : rqsDataSize);
017400090610           IF trulSolI0.ksc = *BLANK;
017500090610             rpyEsito = ESITO_ERROR_PARAMETRI;
017600090610             RETURN;
017700090610           ENDIF;
017800090610           clientiVari = %SUBST(trulSolI0.ksc : 5);
017900090610           MONITOR;
018000090610             filiale = %DEC(%SUBST(trulSolI0.ksc : 2 : 3) : 3 : 0);
018100090610           ON-ERROR;
018200090610             rpyEsito = ESITO_ERROR_FILIALE;
018300090610             RETURN;
018400090610           ENDMON;
018500090610         WHEN rqsFormatName = 'TRULSOLI1';
018600090610           trulSolI1 = %SUBST(rqsData : 1 : rqsDataSize);
018700090610           IF trulSolI1.ksc < 1;
018800090610             rpyEsito = ESITO_ERROR_PARAMETRI;
018900090610             RETURN;
019000090610           ENDIF;
019100090610           clientiVari = %SUBST(%EDITC(trulSolI1.ksc : 'Z') : 4);
019200090610           filiale = trulSolI1.ksc / 10000;
019300090610         OTHER;
019400090610           rpyEsito = ESITO_ERROR_PARAMETRI;
019500090610           RETURN;
019600090610       ENDSL;
019700090610
019800090610       SELECT;
019900090611         WHEN rpyFormatName = 'TRULSOLO0';
020000090610           RESET trulSolO0;
020100090611         WHEN rpyFormatName = 'TRULSOLO1';
020200090610           RESET trulSolO1;
020300090610         OTHER;
020400090610           rpyEsito = ESITO_ERROR_PARAMETRI;
020500090610           RETURN;
020600090610       ENDSL;
020700090610
020800090610       // Mi interessano solo i clienti della società Bartolini.
020900090610
021000090610       SELECT;
021100090610         WHEN rqsFormatName = 'TRULSOLI0';
021200090610           IF trulSolI0.societa = *BLANK;
021300090610             trulSolI0.societa = SOCIETA_BARTOLINI;
021400090610           ELSEIF trulSolI0.societa <> SOCIETA_BARTOLINI;
021500090610             %SUBST(rpyData : 1 : rpyDataSize) = trulSolO0;
021600090610             RETURN;
021700090610           ENDIF;
021800090610         WHEN rqsFormatName = 'TRULSOLI1';
021900090610           // In Orion i clienti sono solo della società Bartolini.
022000090610       ENDSL;
022100090610
022200090610       // Franchi vari e assegnati, non sollecitare.
022300090610
022400090610       IF clientiVari = CLIENTI_FRANCHI_VARI OR clientiVari = CLIENTI_ASSEGNATI;
022500090610         SELECT;
022600090611           WHEN rpyFormatName = 'TRULSOLO0';
022700090610             trulSolO0.cliSoll = *OFF;
022800090611             trulSolO0.cliSollMod = *OFF;
022900090610             %SUBST(rpyData : 1 : rpyDataSize) = trulSolO0;
023000090611           WHEN rpyFormatName = 'TRULSOLO1';
023100090610             trulSolO1.sol = NO;
023200090611             trulSolO1.solMod = NO;
023300090610             %SUBST(rpyData : 1 : rpyDataSize) = trulSolO1;
023400090610         ENDSL;
023500090610         RETURN;
023600090610       ENDIF;
023700090610
023800090610       // Reperisco l'anagrafica della filiale cliente.
023900090610
024000090610       EXEC SQL
024100090610         SELECT AZORG00F.ORGFAG
024200090610       --     , AZORG00F.SUBSTR(ORGDE3, 11, 3)
024300090610           INTO :orgFag
024400090610       --     , :orgNtw
024500090610           FROM AZORG00F
024600090610           WHERE AZORG00F.ORGFIL = :filiale
024700090610       ;
024800090610
024900090610       SELECT;
025000090610         WHEN sqlCode = 100;
025100090610           rpyEsito = ESITO_ERROR_FILIALE;
025200090610           RETURN;
025300090610         WHEN sqlCode < *ZERO;
025400090610           DUMP(A);
025500090610           rpyEsito = sqlCode;
025600090610           RETURN;
025700090610       ENDSL;
025800090610
025900090610       // Clienti di filiali varie, non sollecitare.
026000090610
026100090610       IF orgFag = TIPO_FILIALE_VARIE;
026200090610         SELECT;
026300090611           WHEN rpyFormatName = 'TRULSOLO0';
026400090610             trulSolO0.cliSoll = *OFF;
026500090611             trulSolO0.cliSollMod = *OFF;
026600090610             %SUBST(rpyData : 1 : rpyDataSize) = trulSolO0;
026700090611           WHEN rpyFormatName = 'TRULSOLO1';
026800090610             trulSolO1.sol = NO;
026900090611             trulSolO1.solMod = NO;
027000090610             %SUBST(rpyData : 1 : rpyDataSize) = trulSolO1;
027100090610         ENDSL;
027200090610         RETURN;
027300090610       ENDIF;
027400090610
027500090610       // Filiali di network strani, non sollecitare.
027600090610
027700090610       //IF orgNtw <> NETWORK_ESPRESSO
027800090610       //AND orgNtw <> NETWORK_MESSAGGERIA
027900090610       //AND orgNtw <> NETWORK_DPD;
028000090610       //  SELECT;
028100090610       //    WHEN rqsFormatName = 'TRULSOLO0';
028200090610       //      trulSolO0.cliSoll = *OFF;
028300090611       //      trulSolO0.cliSollMod = *OFF;
028400090610       //      %SUBST(rpyData : 1 : rpyDataSize) = trulSolO0;
028500090610       //    WHEN rqsFormatName = 'TRULSOLO1';
028600090610       //      trulSolO1.sol = NO;
028700090611       //      trulSolO1.solMod = NO;
028800090610       //      %SUBST(rpyData : 1 : rpyDataSize) = trulSolO1;
028900090610       //  ENDSL;
029000090610       //  RETURN;
029100090610       //ENDIF;
029200090610
029300090610       // Incassi da attribuire, non sollecito.
029400090610
029500090610       SELECT;
029600090611         WHEN rpyFormatName = 'TRULSOLO0';
029700090610           societa = trulSolI0.societa;
029800090610           capoConto = trulSolI0.kcc;
029900090610           sottoConto = trulSolI0.ksc;
030000090611         WHEN rpyFormatName = 'TRULSOLO1';
030100090610           societa = SOCIETA_BARTOLINI;
030200090610           capoConto = '00' + %EDITC(trulSolI1.kcc : 'X');
030300090610           sottoConto = '0' + %EDITC(trulSolI1.ksc : 'X');
030400090610       ENDSL;
030500090610
030600090610       IF chkContoIncassiAttribuire( societa
030700090610                                   : capoConto
030800090610                                   : sottoConto
030900090610                                   : esito
031000090610                                   ) = 1;
031100090610         SELECT;
031200090611           WHEN rpyFormatName = 'TRULSOLO0';
031300090610             trulSolO0.cliSoll = *OFF;
031400090611             trulSolO0.cliSollMod = *OFF;
031500090610             %SUBST(rpyData : 1 : rpyDataSize) = trulSolO0;
031600090611           WHEN rpyFormatName = 'TRULSOLO1';
031700090610             trulSolO1.sol = NO;
031800090611             trulSolO1.solMod = NO;
031900090610             %SUBST(rpyData : 1 : rpyDataSize) = trulSolO1;
032000090610         ENDSL;
032100090610         RETURN;
032200090610       ENDIF;
032300090611
032400090611       // I clienti sotto osservazione devono essere sollecitati.
032500090617       //
032600090617       //SELECT;
032700090617       //  WHEN rpyFormatName = 'TRULSOLO0';
032800090617       //    IF trulSolI0.ctgAn03 = CLIENTE_SOTTO_OSSERVAZIONE;
032900090617       //      trulSolO0.cliSollMod = *OFF;
033000090617       //      %SUBST(rpyData : 1 : rpyDataSize) = trulSolO0;
033100090617       //      RETURN;
033200090617       //    ENDIF;
033300090617       //  WHEN rpyFormatName = 'TRULSOLO1';
033400090617       //    IF trulSolI1.con = CLIENTE_SOTTO_OSSERVAZIONE;
033500090617       //      trulSolO1.solMod = NO;
033600090617       //      %SUBST(rpyData : 1 : rpyDataSize) = trulSolO1;
033700090617       //      RETURN;
033800090617       //    ENDIF;
033900090617       //ENDSL;
034000090610
034100090610       // Se arrivo qui significa che è andato tutto bene ... forse ...
034200090610
034300090610       SELECT;
034400090611         WHEN rpyFormatName = 'TRULSOLO0';
034500090610           %SUBST(rpyData : 1 : rpyDataSize) = trulSolO0;
034600090611         WHEN rpyFormatName = 'TRULSOLO1';
034700090610           %SUBST(rpyData : 1 : rpyDataSize) = trulSolO1;
034800090610       ENDSL;
034900090610
035000090610       RETURN;
035100090610
035200090610       //***********************************************************************
035300090610       //
035400090610       //
035500090610       //
035600090610       //***********************************************************************
035700090610
035800090610       BEGSR *INZSR;
035900090610
036000090610       ENDSR;
036100090610
036200090610      /END-FREE
