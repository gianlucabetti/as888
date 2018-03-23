000100170118     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200990908
000300031015     Ftivlt11l  uf   e           k disk
000400000626
000500030512     C*
000600031126     C* Scorre tutto il file delle spie allocazioni file/membri in DOWNLOAD procedura FTP/e-mail
000700030512     C                   EXSR      chkFTP
000800000626     C*
000900011129     C                   SETON                                        LR
001000000626
001100000623
001200030512      /TITLE Lettura TIVLT11L.
001300030512     C     chkFTP        BEGSR
001400000623     C*
001500031126     C     *START        SETLL     tivlt11l
001600031126     C                   READ      tivlt11l
001700031126     C                   DOW       not %eof(tivlt11l)
001800031015     C*
001900031015     C* Se trattasi d fine allocazione da procedura FTP/e-mail
002000031015     C                   IF        %subst(vltfl1:1:1) = '1'                     * fine allocazione
002100031015     C*
002200031015     C* Se trovo l'indicazione del file/membro e l'allocazione su server FTP è terminata
002300031015     C* verifico se il file/membro contiene dati
002400031015     C                   CALL(e)   'TIS7T0C1'
002500031126     C                   PARM                    vltFLD
002600031126     C                   PARM                    vltMBD
002700031015     C                   PARM                    numrec           10 0
002800031015     C*
002900031015     C* Se nn contiene record => elimino il record d spia x allocazione da FTP/e-mail
003000031015     C                   IF        %error OR numrec = *zeros
003100031015     C                   DELETE    tivlt11l
003200031126     C                   ENDIF
003300031126     C                   ENDIF
003400031126     C*
003500031126     C* Proseguo la lettura del file spie
003600031126     C                   READ      tivlt11l
003700031015     C                   ENDDO
003800000630     C*
003900011129     C                   ENDSR
004000000711
004100000711
004200000626
004300990908      /TITLE Operazioni iniziali.
004400011129     C     *inzsr        BEGSR
004500030512     C*
004600031126     C*
004700030512     C                   ENDSR
