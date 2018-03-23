000100990910      /TITLE Upload via Internet: decisore.
000200990908     H dftactgrp(*yes)
000300990908
000400031015     Ftivlt11l  uf   e           k disk
000500000626
000600030512     C*
000700030512     C* Verifico esistenza file/membro richiesti da operazione di DWNLD in file spia
000800030512     C* file/membri in uso da FTP.
000900030512     C                   EXSR      chkFTP
001000000626     C*
001100011129     C                   SETON                                        LR
001200000626
001300000623
001400030512      /TITLE Lettura TIVLT11L.
001500030512     C     chkFTP        BEGSR
001600000623     C*
001700030512     C                   EVAL      check = '0'
001800031015     C                   MOVEL     '0'           wChk              1
001900030512     C*
002000031015     C* Ciclo finchè il record esiste nel file spia allocazioni da FTP/e-mail e
002100031015     C* trattasi d inizio allocazione, oppure fino a che nn è + presente il record;
002200031015     C* fino ad un massimo d 2 minuti => altrimenti blocco la traduzione
002300031015     C                   SETOFF                                       50
002400031015     C                   DOW       *IN50 = *OFF
002500031015     C     KEYFTP        CHAIN     tivlt11l                           50
002600030512     C                   IF        %found(tivlt11l)
002700031015     C*
002800031015     C* Se trattasi d fine allocazione da procedura FTP/e-mail
002900031015     C                   IF        %subst(vltfl1:1:1) = '1'                     * fine allocazione
003000031015     C*
003100031015     C* Accendo l'indicatore che determina l'uscita dal ciclo
003200031015     C                   SETON                                        50
003300031015     C*
003400031015     C* Se trovo l'indicazione del file/membro e l'allocazione su server FTP è terminata
003500031015     C* verifico se il file/membro contiene dati
003600031015     C                   CALL(e)   'TIS7T0C1'
003700031015     C                   PARM                    file
003800031015     C                   PARM                    membro
003900031015     C                   PARM                    numrec           10 0
004000031015     C*
004100031015     C* Se nn contiene record => elimino il record d spia x allocazione da FTP/e-mail
004200031015     C                   IF        %error OR numrec = *zeros
004300031015     C                   DELETE    tivlt11l
004400031015     C                   ELSE
004500031015     C*
004600031015     C* Se invece contiene record => blocco l'operazione d traduzione
004700031015     C                   EVAL      check = '1'
004800031015     C                   ENDIF
004900031015     C                   ELSE
005000031126     C*
005100031126     C* Se invece trattasi d inizio allocazione da procedura FTP/e-mail =>
005200031126     C* verifico cmq se esiste o meno il file/membro
005300031126     C                   CALL(e)   'TIS7T0C1'
005400031126     C                   PARM                    file
005500031126     C                   PARM                    membro
005600031126     C                   PARM                    numrec           10 0
005700031126     C*
005800031126     C* Se nn contiene record => elimino il record d spia x allocazione da FTP/e-mail
005900031126     C                   IF        %error OR numrec = *zeros
006000031126     C                   DELETE    tivlt11l
006100031126     C                   ELSE
006200031015     C*
006300031126     C* Se invece il file/membro contiene record => rilascio subito il record ed attendo 2 minuti
006400031015     C                   IF        wChk = '0'
006500031015     C                   UNLOCK    tivlt11l
006600031015     C                   CALL(e)   'TIS7T0C2'
006700031015     C                   PARM                    file
006800031015     C                   PARM                    membro
006900031015     C                   PARM      '1'           operazione        1
007000031015     C                   PARM                    esito             1
007100031015     C*
007200031015     C* Valorizzo la varibile che mi indica che ho già aspettato 2 minuti
007300031015     C                   MOVEL     '1'           wChk              1
007400031015     C                   ELSE
007500031015     C*
007600031015     C* Se ho già aspettato 2 minuti esco dal ciclo e blocco la traduzione
007700031015     C                   SETON                                        50
007800031015     C                   EVAL      check = '1'
007900031015     C                   ENDIF
008000031126     C                   ENDIF
008100031015     C                   ENDIF
008200031015     C                   ELSE
008300031015     C*
008400031015     C* Se nn trovo il record relativo al file/membro corrente esco semplicemente dal ciclo
008500031015     C                   SETON                                        50
008600031015     C                   ENDIF
008700031015     C                   ENDDO
008800000630     C*
008900011129     C                   ENDSR
009000000711
009100000711
009200000626
009300990908      /TITLE Operazioni iniziali.
009400011129     C     *inzsr        BEGSR
009500990908     C*
009600011129     C     *entry        PLIST
009700030512     C                   PARM                    file             10
009800030512     C                   PARM                    membro           10
009900030512     C                   PARM                    check             1
010000011129     C*
010100030512     C* Chiavi di lettura
010200031008     C     KEYFTP        KLIST                                                  *TIVLT11L - completa
010300030512     C                   KFLD                    file
010400030512     C                   KFLD                    membro
010500030512     C*
010600030512     C                   ENDSR
