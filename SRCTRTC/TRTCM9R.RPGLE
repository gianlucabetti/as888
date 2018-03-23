000100000000     H*--------------------------------------------------------------------------------------------*
000200050214     H* Reperimento/impostazione stato log "multi-spool"
000300000000     H*--------------------------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*--------------------------------------------------------------------------------------------*
000600000000     F* Data base
000700000000     F*--------------------------------------------------------------------------------------------*
000800040819     Ftimal01l  UF   E           K DISK
000900080904     F                                     infsr(*pssr)
001000050214     D*------------------
001100050214     D* DS di procedura
001200050214     D*------------------
001300050222     D TRTCM9DS      E DS
001400050214     D*------------------
001500050214     D* Variabili d wrk
001600050214     D*------------------
001700050214     D depSTS          S              1    INZ(*blanks)
001800050214     D*------------------
001900000000     C*--------------------------------------------------------------------------------------------*
002000000000     C* Main lines
002100000000     C*--------------------------------------------------------------------------------------------*
002200050222     C                   EVAL      §CM9STSOUT = *blanks
002300050222     C                   EVAL      §CM9ALL    = *blanks
002400050222     C                   EVAL      §CM9ERR    = *blanks
002500050214     C*
002600050214     C* Innanzitutto verifico il tipo d chiamata richiesta
002700050214     C* - se chiamata C => solo x chiusura
002800050214     C                   IF        §CM9TIP = 'C'
002900050214     C                   SETON                                        LR
003000050214     C                   ELSE
003100000000     C*
003200050214     C* Leggo tutti i record del progressivo ricevuto in input
003300050214     C     §CM9PROG      SETLL     timal01l
003400040819     C                   IF        %found(timal01l)
003500050214     C     §CM9PROG      READE     timal01l
003600040819     C                   DOW       not %eof(timal01l)
003700050214     C*
003800050214     C* Se passato lo stato "IN" => richiesta valorizzazione tutti log del progressivo a stato IN
003900050214     C                   IF        §CM9STSIN <> *blanks
004000050214     C                   EVAL      malSTS = §CM9STSIN
004100050222     C                   EVAL      malMSG = §CM9MSGIN
004200050214     C                   UPDATE(e) timal000
004300050214     C                   IF        %error
004400050214     C                   EVAL      §CM9ERR = '1'
004500050214     C                   LEAVE
004600050214     C                   ENDIF
004700050214     C*
004800050214     C* Se NN passato lo stato "IN" => richiesta attribuzione d 1 stato complessivo
004900050214     C* > se almeno 1 record del progressivo richiesto è in errore considero in errore tutti i log
005000050214     C*   dello stesso progressivo
005100050214     C                   ELSE
005200050214     C                   IF        malSTS = '2' or
005300050214     C                             malSTS = '3'
005400050214     C                   EVAL      §CM9STSOUT = '2'
005500050214     C                   ENDIF
005600050214     C*
005700050214     C* Verifico inoltre se tutti i record d log dello stesso progressivo hanno già medesimo stato
005800050214     C                   IF        depSTS = *blanks
005900050214     C                   EVAL      depSTS = malSTS
006000050214     C                   ENDIF
006100050214     C                   IF        malSTS <> depSTS
006200050214     C                   EVAL      §CM9ALL = 'N'
006300050214     C                   ENDIF
006400050214     C                   ENDIF
006500040406     C*
006600050214     C     §CM9PROG      READE     timal01l
006700040406     C                   ENDDO
006800040406     C                   ENDIF
006900040406     C*
007000050222     C                   IF        §CM9TIP = 'E'
007100050222     C                   SETON                                        LR
007200050222     C                   ELSE
007300050214     C                   SETON                                        RT
007400050222     C                   ENDIF
007500050222     C*
007600050214     C                   ENDIF
007700080904     C*--------------------------------------------------------------------------------------------*
007800080904     C     *pssr         BEGSR
007900080904     C*
008000080904     C                   eval      §CM9ERR = '1'
008100080904     C                   seton                                        LR
008200080904     C*
008300080904     C                   ENDSR     '*CANCL'
008400040406     C*--------------------------------------------------------------------------------------------*
008500000000     C* *inzsr - operazioni iniziali
008600040406     C*--------------------------------------------------------------------------------------------*
008700000000     C     *inzsr        BEGSR
008800040406     C*
008900000000     C* ricevimento parametri
009000040406     C     *ENTRY        PLIST
009100080904     C                   PARM                    TRTCM9DS
009200050214     C*
009300000000     C                   ENDSR
