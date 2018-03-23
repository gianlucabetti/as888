000100030806      /TITLE Aggiornamento CNABI00F da ANAB900F.
000200030806
000300120413     H DATEDIT(*DMY) DFTACTGRP(*NO)
000400030806
000500030806     FPrtF198   O    F  198        PRINTER
000600030806     F                                     USROPN
000700030806     FCnAbi00F  UF A E           K DISK
000800030806     FAnAb900F  IP   E             DISK
000900030806
001000120416     D ATTIVO          C                   '0'
001100120416     D ASSORBITO       C                   '1'
001200120416     D CHIUSO          C                   '2'
001300120416     D ANNULLATO       C                   '*'
001400080206     D ITALIA          C                   *BLANK
001500080206     D SANMARINO       C                   'SM'
001600160912     D*SANMARINO_CAP_4789...
001700160912     D*                C                   '4789'
001800160912     D*SANMARINO_CAP_47031...
001900160912     D*                C                   '47031'
001901160912     D SANMARINO_CAB...
001902160912     D                 C                   '098'
002000030806
002100030806     D ANAB911DS     E DS
002200030806     D                                     INZ
002300030806     D ANAB921DS     E DS
002400030806     D                                     INZ
002500030806     D ANAB931DS     E DS
002600030806     D                                     INZ
002700030806
002800030806     D Stampare        S              1
002900030806     D Operazione      S             10
003000080206     D wStato          S              4A
003100120413
003200120413     D*--------------------------------------------------
003300120413     D* Procedure name: SetDescrizioneIstituto
003400120413     D* Purpose:        Imposta la descrizione dell'istituto su tutti gli s...
003500120413     D*                          portelli.
003600120413     D* Returns:        Esito.
003700120413     D* Parameter:      priDescrizioneIstituto => Descrizione istituto
003800120413     D*--------------------------------------------------
003900120413     D SetDescrizioneIstituto...
004000120413     D                 PR            10I 0
004100120413     D  priAbi...
004200120413     D                                5A   CONST
004300120413     D  priDescrizioneIstituto...
004400120413     D                               80A   CONST
004500120413
004600120413     D*--------------------------------------------------
004700120413     D* Procedure name: SetIstitutoChiusoAssorbito
004800120413     D* Purpose:        Annulla gli sportelli di un istituto chiuso o assor...
004900120413     D*                          bito.
005000120413     D* Returns:        Esito.
005100120413     D* Parameter:      priAbi
005200120413     D*--------------------------------------------------
005300120413     D SetIstitutoChiusoAssorbito...
005400120413     D                 PR            10I 0
005500120413     D  priAbi                        5A   CONST
005600120416     D  priStatus                     1A   CONST
005700120416     D  priAssorbitoDaAbi...
005800120416     D                                5A   CONST
005900120416
006000120413
006100030806     ***********************************************************************
006200030806     **
006300030806     **
006400030806     **
006500030806     ***********************************************************************
006600030806     C     *ENTRY        PLIST
006700030806     C                   PARM                    Stampare
006800030806
006900030807     ** Per ogni sportello esistono 3 records nel seguente ordine:
007000030807     ** tipo '11' - Descrizione istituto;
007100030807     ** tipo '21' - Indirizzo sportello;
007200030807     ** tipo '31' - Descrizione sportello.
007300030806     C                   SELECT
007400030806     C
007500030806     C                   WHEN      AB9TR = '11'
007600030806     C                   EVAL      ANAB911DS = AB9F01
007700120413     C
007800120413     C                   SELECT
007900120416     C                   WHEN      AB9Tipo = ASSORBITO
008000120416     C                   EVAL      %SUBST(AB9DenIst:45) = '(ASSORBITO DA '
008100120413     C                             + %EDITC(AB9BanSeg:'X') + ')'
008200120416     C                   WHEN      AB9Tipo = CHIUSO
008300120416     C                   EVAL      %SUBST(AB9DenIst:45) = '(CHIUSO)'
008400120413     C                   ENDSL
008500030806     C
008600120416     C                   CALLP     SetDescrizioneIstituto(ab9abi : ab9denIst)
008700120413     C
008800120416     C                   IF        ab9tipo <> ATTIVO
008900120416     C                   CALLP     SetIstitutoChiusoAssorbito(ab9abi : ab9tipo
009000120416     C                             : %EDITC(ab9banSeg:'X'))
009100120413     C                   ENDIF
009200120413     C
009300030806     C                   WHEN      AB9TR = '21'
009400030806     C                   EVAL      ANAB921DS = AB9F01
009500030806     C                   MOVE      AB9ABI        ABIABI
009600030806     C                   MOVE      AB9CAB        ABICAB
009700030806     C
009800030806     C                   WHEN      AB9TR = '31'
009900030806     C                   EVAL      ANAB931DS = AB9F01
010000030806     C                   EXSR      UpdCnAbi
010100030806     C
010200030806     C                   ENDSL
010300030806
010400030806     ***********************************************************************
010500030806     **
010600030806     **
010700030806     **
010800030806     ***********************************************************************
010900030806     C     *INZSR        BEGSR
011000030806
011100030806     C                   IF        Stampare = *ON
011200030806     C                   OPEN      PrtF198
011300030806     C                   EXCEPT    TesSta
011400030806     C                   ENDIF
011500030806
011600030806     C                   ENDSR
011700030806
011800030806     ***********************************************************************
011900030806     **
012000030806     ** Aggiornamento CNABI00F.
012100030806     **
012200030806     ***********************************************************************
012300030806     C     UpdCnAbi      BEGSR
012400030806
012500030806     C     K02Abi00      KLIST
012600030806     C                   KFLD                    ABIABI
012700030806     C                   KFLD                    ABICAB
012800080206
012900080206     C                   IF        ab9Provinc = SANMARINO
013000080206     C                   EVAL      wStato = SANMARINO
013100160912     C*                  ELSEIF    %SUBST(ab9Cap : 1 : 4) = SANMARINO_CAP_4789
013200160912     C*                            OR ab9Cap = SANMARINO_CAP_47031
013201160912     C                   ELSEIF    %SUBST(ab9Cab:1:3) = SANMARINO_CAB
013300080206     C                   EVAL      wStato = SANMARINO
013400080206     C                   EVAL      ab9Provinc = SANMARINO
013500080206     C                   ELSE
013600080206     C                   EVAL      wStato = ITALIA
013700080206     C                   ENDIF
013800030806
013900030807     C     *NOKEY        CLEAR                   CnAbi000
014000030807
014100030806     C     K02Abi00      CHAIN     CnAbi00F
014200030806
014300030806     C                   IF        NOT %FOUND
014400030806     C                             OR
014500030807     C                             ABIIst <> AB9DenIst
014600030806     C                             OR
014700030806     C                             ABIAge <> AB9Denom
014800030806     C                             OR
014900030806     C                             ABInd  <> AB9Indir
015000030806     C                             OR
015100030806     C                             ABILoc <> AB9Localit
015200030806     C                             OR
015300080206     C                             %SUBST(ABICom:1:36) <> %SUBST(AB9Comune:1:36)
015400030806     C                             OR
015500030806     C                             %EDITC(ABICAP:'X') <> AB9CAP
015600030806     C                             OR
015700030806     C                             ABIPro <> AB9Provinc
015800080206     C                             OR
015900080206     C                             %SUBST(ABICom:37) <> wStato
016000030806     C                             OR
016100120416     C                             (AB9Tipo <> ATTIVO AND ABIAnn = *BLANK)
016200030808     C                             OR
016300120416     C                             (AB9Tipo = ATTIVO AND ABIAnn = ANNULLATO)
016400030806
016500030806     C                   EVAL      ABIIst = AB9DenIst
016600030806     C                   EVAL      ABIAge = AB9Denom
016700030806     C                   EVAL      ABInd  = AB9Indir
016800030806     C                   EVAL      ABILoc = AB9Localit
016900080206     C                   EVAL      ABICom = AB9Comune
017000080206     C                   EVAL      %SUBST(ABICom:37) = wStato
017100030806     C                   MOVE      AB9CAP        ABICAP
017200030806     C                   EVAL      ABIPro = AB9Provinc
017300030806     C                   CLEAR                   ABIDTr
017400030806     C                   CLEAR                   ABIFTr
017500030808     C                   CLEAR                   ABIAnn
017600030806
017700030806     C                   SELECT
017800120416     C                   WHEN      AB9Tipo = ASSORBITO
017900030806     C                   EVAL      Operazione = 'ASSORBITO'
018000120416     C                   WHEN      AB9Tipo = CHIUSO
018100030806     C                   EVAL      Operazione = 'CHIUSO'
018200120416     C                   EVAL      ABIAnn = ANNULLATO
018300030806     C                   WHEN      %FOUND
018400030806     C                   EVAL      Operazione = 'AGGIORNATO'
018500030806     C                   WHEN      NOT %FOUND
018600030806     C                   EVAL      Operazione = 'NUOVO'
018700030806     C                   ENDSL
018800030806
018900030806     C                   IF        %FOUND
019000030806     C                   UPDATE    CnAbi000
019100030806     C                   ELSE
019200030806     C                   WRITE     CnAbi000
019300030806     C                   ENDIF
019400030806
019500030806     C                   IF        Stampare = *ON
019600030806     C                   EXCEPT    Rig001
019700030806     C                   ENDIF
019800030806
019900030806     C                   ELSE
020000030806
020100030806     C                   UNLOCK    CnAbi00F
020200030806
020300030806     C                   ENDIF
020400030806
020500030806     C                   ENDSR
020600030806
020700030806     ***********************************************************************
020800030806     **
020900030806     **
021000030806     **
021100030806     ***********************************************************************
021200030806     OPrtF198   E            TesSta            1
021300030806     O                                              'Aggiornamento CNABI00F del'
021400030806     O                       UDATE         Y     +1
021500030806
021600030806     OPrtF198   E            Rig001      2
021700030806     O                       Operazione
021800030806     O                       ABIABI              16
021900030807     O                       ABICAB              22
022000030807     O                       ANAB911DS           +1
022100030807     O
022200030806     OPrtF198   E            Rig001      1
022300030807     O                                           22 ' '
022400030807     O                       ANAB921DS           +1
022500030807     O
022600030807     OPrtF198   E            Rig001      1
022700030807     O                                           22 ' '
022800030807     O                       ANAB931DS           +1
022900120413
023000120413     P*--------------------------------------------------
023100120413     P* Procedure name: SetDescrizioneIstituto
023200120413     P* Purpose:        Imposta la descrizione dell'istituto su tutti gli s...
023300120413     P*                          portelli.
023400120413     P* Returns:        Esito.
023500120413     P* Parameter:      priDescrizioneIstituto => Descrizione istituto
023600120413     P*--------------------------------------------------
023700120413     P SetDescrizioneIstituto...
023800120413     P                 B
023900120413     D SetDescrizioneIstituto...
024000120413     D                 PI            10I 0
024100120413     D  priAbi...
024200120413     D                                5A   CONST
024300120413     D  priDescrizioneIstituto...
024400120413     D                               80A   CONST
024500120413
024600120413     D retField        S             10I 0 STATIC
024700120416     D local           DS                  QUALIFIED STATIC
024800120416     D  desIst                       44A
024900120416     D  desAgIs                      36A
025000120413
025100120413      /FREE
025200120413
025300120413       CLEAR retField;
025400120416       CLEAR local;
025500120413
025600120413       IF priAbi = *BLANK OR priDescrizioneIstituto = *BLANK;
025700120413         RETURN -1;
025800120413       ENDIF;
025900120413
026000120413       EXEC SQL
026100120413         UPDATE cnabi00f
026200120413           SET abiIst = :priDescrizioneIstituto
026300120413             , abiDtr = 0
026400120413             , abiFtr = ''
026500120413           WHERE abiAbi = :priAbi
026600120413             AND abiIst <> :priDescrizioneIstituto
026700120413           WITH NC
026800120413       ;
026900120413
027000120413       IF sqlCode < *ZERO;
027100120413         DUMP(A);
027200120413         retField = sqlCode;
027300120413       ENDIF;
027400120416
027500120416       local.desIst = priDescrizioneIstituto;
027600120416       local.desAgIs = %SUBST(priDescrizioneIstituto : 45);
027700120416
027800120416       EXEC SQL
027900120416         UPDATE anact00f
028000120416           SET actDesIst = :local.desIst
028100120416             , actDesAgIs = :local.desAgIs
028200120416           WHERE actAbi = :priAbi
028300120416             AND (actDesIst <> :local.desIst OR actDesAgIs <> :local.desAgIs)
028400120416           WITH NC
028500120416       ;
028600120416
028700120416       IF sqlCode < *ZERO;
028800120416         DUMP(A);
028900120416         retField = sqlCode;
029000120416       ENDIF;
029100120413
029200120413       RETURN retField;
029300120413
029400120413      /END-FREE
029500120413     P SetDescrizioneIstituto...
029600120413     P                 E
029700120413
029800120413
029900120413     P*--------------------------------------------------
030000120413     P* Procedure name: SetIstitutoChiusoAssorbito
030100120413     P* Purpose:        Annulla gli sportelli di un istituto chiuso o assor...
030200120413     P*                          bito.
030300120413     P* Returns:        Esito.
030400120413     P* Parameter:      priAbi
030500120413     P*--------------------------------------------------
030600120413     P SetIstitutoChiusoAssorbito...
030700120413     P                 B
030800120413     D SetIstitutoChiusoAssorbito...
030900120413     D                 PI            10I 0
031000120413     D  priAbi                        5A   CONST
031100120416     D  priStatus                     1A   CONST
031200120416     D  priAssorbitoDaAbi...
031300120416     D                                5A   CONST
031400120413
031500120413     D retField        S             10I 0 STATIC
031600120416     D local           DS                  QUALIFIED STATIC
031700120416     D  status                        1A
031800120416     D  assorbitoDaAbi...
031900120416     D                                5A
032000120413
032100120413      /FREE
032200120413
032300120413       CLEAR retField;
032400120416       CLEAR local;
032500120413
032600120413       EXEC SQL
032700120413         UPDATE cnabi00f
032800120413           SET abiAnn = '*'
032900120413             , abiDtr = 0
033000120413             , abiFtr = ''
033100120413           WHERE abiAbi = :priAbi
033200120413             AND abiAnn = ''
033300120413           WITH NC
033400120413       ;
033500120413
033600120413       IF sqlCode < *ZERO;
033700120413         DUMP(A);
033800120413         retField = sqlCode;
033900120413       ENDIF;
034000120416
034100120416       IF priStatus <> ATTIVO;
034200120416         local.status = priStatus;
034300120416       ENDIF;
034400120416
034500120416       IF priStatus = ASSORBITO;
034600120416         local.assorbitoDaAbi = priAssorbitoDaAbi;
034700120416       ENDIF;
034800120416
034900120416       EXEC SQL
035000120416         UPDATE anact00f
035100120416           SET actStatus = :local.status
035200120416             , actAssorDa = :local.assorbitoDaAbi
035300120416           WHERE actAbi = :priAbi
035400120416           WITH NC
035500120416       ;
035600120416
035700120416       IF sqlCode < *ZERO;
035800120416         DUMP(A);
035900120416         retField = sqlCode;
036000120416       ENDIF;
036100120413
036200120413       RETURN retField;
036300120413
036400120413      /END-FREE
036500120413     P SetIstitutoChiusoAssorbito...
036600120413     P                 E
036700120413
