000100051019     D**************
000200051019     D* DS ESTERNE DI PROCEDURE
000300051019     D**************
000400051021     D XSPLATRDS     E DS
000500051019     D XSPLATRDS1    E DS
000600051021     D*
000700051021     D                 DS
000800051021     D  LENDTA                 1      4B 0
000900051021     D  STRPOS                 5      8B 0
001000051021     D  SPALEN                 9     12B 0
001100051021     D  SPASNR                13     16B 0
001200051019     D*
001300051019     D**************
001400051019     D* DS ESTERNE DI SISTEMA X GESTIONE RITORNO ERRORI API
001500051019     D**************
001600051019     D/COPY QSYSINC/QRPGLESRC,QUSEC
001700051019     D*
001800051017     C*---------------------------------------------------------------*
001900970929     C     *ENTRY        PLIST
002000051021     C                   PARM                    XSPLATRDS
002100051021     C
002200051021     C                   IF        §SPLA_QJOB <> *blanks AND
002300051021     C                             §SPLA_SNAM <> *blanks AND
002400051021     C                             §SPLA_SNUM <> *zeros
002500051021     C                   EVAL      §SPLA_JID = *blanks
002600051021     C                   EVAL      §SPLA_SID = *blanks
002700051021     C                   ENDIF
002800051021     C*
002900051021     C                   IF        §SPLA_JID <> *blanks AND
003000051021     C                             §SPLA_SID <> *blanks
003100051021     C                   EVAL      §SPLA_QJOB = '*INT'
003200051021     C                   EVAL      §SPLA_SNAM = '*INT'
003300051021     C                   EVAL      §SPLA_SNUM = *zeros
003400051021     C                   ENDIF
003500051021     C*
003600051021     C                   IF        §SPLA_FMTN = *blanks
003700051021     C                   EVAL      §SPLA_FMTN = 'SPLA0100'
003800051021     C                   ENDIF
003900051021     C*
004000051021     C* A seconda del formato attributi d out richiesto in input valorizzo determinate variabili d procedura
004100051021     C                   IF        §SPLA_FMTN = 'SPLA0100'
004200051021     C                   EVAL      SPALEN = %size(XSPLATRDS1)
004300051021     C*
004400051021     C* Reperisco gli attributi dello spool x il formato richiesto
004500051021     C                   CLEAR                   QUSEC
004600051025     C                   CLEAR                   XSPLATRDS1
004700051021     C                   EVAL      QUSBPRV = %size(QUSEC)
004800051021     C                   CALL      'QUSRSPLA'
004900051021     C                   PARM                    XSPLATRDS1
005000051021     C                   PARM                    SPALEN
005100051021     C                   PARM                    §SPLA_FMTN
005200051021     C                   PARM                    §SPLA_QJOB
005300051021     C                   PARM                    §SPLA_JID
005400051021     C                   PARM                    §SPLA_SID
005500051021     C                   PARM                    §SPLA_SNAM
005600051021     C                   PARM                    §SPLA_SNUM
005700051021     C                   PARM                    QUSEC
005800051021     C                   ENDIF
005900051021     C*
006000051021     C* Gestisco l'errore e quindi esco ritornando esito negativo della richiesta
006100051021     C                   IF        %error OR QUSEI <> *blanks
006200051021     C                   EVAL      §SPLA_ERR = 'E'
006300051021     C                   ELSE
006400051021     C                   EVAL      §SPLA_ATTR = XSPLATRDS1
006500051021     C                   ENDIF
006600051017     C*
006700051017     C* Ed esco
006800910702     C                   SETON                                          LR
