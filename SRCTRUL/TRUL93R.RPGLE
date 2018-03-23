000100020618     H*----------------------------------------------------------------------------------------------------------------*
000200020618     H* Reperisce numero record presenti in TITASQUE e manda un messaggio a video
000300020618     H*----------------------------------------------------------------------------------------------------------------*
000400020618     H DATEDIT(*YMD)
000500020618     D*----------------------------------------------------------------------------------------------------------------*
000600020618     D* Data structure
000700020618     D*----------------------------------------------------------------------------------------------------------------*
000800020618      * Schiere
000900020618     D cmd             S             70    DIM(1) CTDATA PERRCD(1)              *comandi CLP
001000020618      * Variabili a programma
001100020619     d n9              S              9  0                                      *numerico 9
001200020618     d n14             S             14  0                                      *numerico 14
001300020618     d cordat          S              8  0                                      *data corrente g/m/a
001400020618     d corora          S              6  0                                      *ora  corrente h:m:s
001500020618     d $lenght         S             15  5 INZ(80)                              *comando -lunghezza-
001600020618     d $command        S             80                                         *comando -contenuto-
001700020618      * Informazioni lavoro
001800020618     d                SDS
001900020618     d  JOB                  244    253                                         *lavoro
002000020618     d  USER                 254    263                                         *utente
002100020618      * Comandi CLP
002200020618     d                 DS
002300020619     d  DRec                  16     24                                          -numero record
002400020619     d  DJob                  35     44                                          -Jobq
002500020618     d DSComando               1     80                                         *comando
002600020618      * API per reperire numero record
002700020618     D RMbrD01         S                   LIKE(QUSM0200)                       Receiver variable
002800020618     D RMbrD02         S              9B 0 INZ(%SIZE(RMbrD01))                  Length of receiver v
002900020618     D RMbrD03         S              8    INZ('MBRD0200')                      Format name
003000020618     D RMbrD04         S             20    INZ('TITASQUE  *LIBL     ')          Qualified database f
003100020618     D RMbrD05         S             10    INZ('*FIRST')                        Database member name
003200020618     D RMbrD06         S              1    INZ('0')                             Override processing
003300020618
003400020618      /COPY QSYSINC/QRPGLESRC,QUSRMBRD
003500020618      /COPY QSYSINC/QRPGLESRC,QUSEC
003600020618
003700020618      *----------------------------------------------------------------------------------------------------------------*
003800020618      * Main linese
003900020618      *----------------------------------------------------------------------------------------------------------------*
004000020618     C                   CLEAR                   QUSEC
004100020618     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
004200020618      *
004300020618     C                   CALL      'QUSRMBRD'
004400020618     C     QUSM0200      PARM      *BLANK        RMbrD01
004500020618     C                   PARM                    RMbrD02
004600020618     C                   PARM                    RMbrD03
004700020618     C                   PARM                    RMbrD04
004800020618     C                   PARM                    RMbrD05
004900020618     C                   PARM                    RMbrD06
005000020618     C                   PARM                    QUSEC
005100020618     C*
005200020618     C* Manda un messaggio all'utente per visualizzarne l'esito
005300020618     C                   EVAL      $command = cmd(1)
005400020618     C                   MOVEA     cmd(1)        DSComando
005500020619     C                   Z-ADD     QUSNCRU       n9                             *N° record DS
005600020619     C                   MOVEL     n9            DRec
005700020618     C                   MOVEL     Job           DJob
005800020618     C                   EVAL      $command = DsComando
005900020618     C                   CALL      'QCMDEXC'
006000020618     C                   PARM                    $command
006100020618     C                   PARM                    $lenght
006200020618
006300020618     ** A questo punto se la chiamata è stata eseguita correttamente
006400020618     ** il campo QUSEI è vuoto e in QUSNCRU c'è il numero di record
006500020618     ** presenti nel membro.
006600020618
006700020618     C                   EVAL      *INLR = *ON
006800020618     C*----------------------------------------------------------------------------------------------------------------*
006900020618     C* Operazioni iniziali
007000020618     C*----------------------------------------------------------------------------------------------------------------*
007100020618     C     *inzsr        BEGSR
007200020618     C*
007300020618     C* reperisce la data corrente
007400020618     C                   TIME                    n14                            *ora (6)+ data (8)
007500020618     C                   MOVEL     n14           corora                         *ora  in h:m:s
007600020618     C                   MOVE      n14           cordat                         *data in g/m/aa
007700020618     C*
007800020618     C                   ENDSR
007900020618     O*----------------------------------------------------------------------------------------------------------------*
008000020618**   CMD - Comandi
008100020619SNDBRKMSG MSG('XXXXXXXXX') TOMSGQ(XXXXXXXXXX)
