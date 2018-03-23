000100020515      /EJECT
000200020515     D*****************************************************************
000300020515     D*Data Definitions
000400020515     D*****************************************************************
000500020515     D* Numero di riche dello stack da ricevere
000600020515     D W_Line          s              3  0 INZ(100)
000700020515     D* Call Stack
000800020515     D DSStack         ds
000900020515     D  £Stack                       10    Dim(100)
001000020515     D* Contatore dello Stack
001100020515     D  $PCounter      s              9b 0
001200020515     D* Tempo di attesa x il ricevimento del messaggio
001300020515     D  $PWait         s              9b 0
001400020515     D* Definizione formato RCVM0200
001500020515     D RCVM0200        DS
001600020607     D RCVPgm                111    120                                         Receive Program Name
001700020607     D*
001800020515     D RCVM0200Len     s              9b 0 INZ(%Size(RCVM0200))
001900020515     D*****
002000020515     D* API Error Data structure
002100020515     D*****
002200020515     D DSERR           DS           256
002300020515     D  ERBYTP                 1      4B 0 INZ(256)
002400020515     D  ERBYTA                 5      8B 0
002500020515     D  ERMSID                 9     15
002600020515     D  ERMSDT                17    256
002700020515     D*****************************************************************
002800020515     D* MAIN
002900020515     D*****************************************************************
003000020515     C* Parametri di ingresso
003100020515     C* (la dimensione del parametro PStack è data da 10 x il numero di richiami da reperire => 10 x 100)
003200020515     C     *Entry        Plist
003300020515     C                   Parm                    PStack         1000
003400020515     C                   Parm                    PStartPoint       3 0
003500020515     C                   Parm                    PEntries          3 0
003600020515     C*****
003700020515     C* Inizializzazione parametri
003800020515     C*****
003900020515     C                   Move      *Blanks       PStack
004000020515     C                   Z-Add     *Zero         PEntries
004100020515     C                   Z-Add     PStartPoint   StartPoint        3 0
004200020515     C     StartPoint    IfLt      1
004300020515     C                   Z-Add     1             StartPoint
004400020515     C                   EndIf
004500020515     C                   Z-Add     *Zero         S                 3 0
004600020515     C*****
004700020515     C* Ciclo x il numero di righe impostate
004800020515     C*****
004900020515     C     S             DOUEQ     W_Line
005000020515     C     ERBYTA        ORGT      *ZERO
005100020515     C                   Add       1             S
005200020515     C     StartPoint    Add       S             $PCounter
005300020515     D*****
005400020515     D* Spedisce un messagio alla DTAQ specifica
005500020515     D*****
005600020515     C                   Call      'QMHSNDPM'
005700020515     C                   Parm                    $PMsId            7
005800020515     C                   Parm                    $PMsgf           20
005900020515     C                   Parm      'SLIC'        $PMSDT            4
006000020515     C                   Parm      X'00000004'   $PLen             4
006100020515     C                   Parm      '*INFO'       $PType           10
006200020515     C                   Parm      '*'           $PMSGQ           10
006300020515     C                   Parm                    $PCounter
006400020515     C                   Parm                    $PKey             4
006500020515     C                   Parm                    DSERR
006600020515     D*****
006700020515     D* Riceve il messaggio
006800020515     D*****
006900020515     C                   Call      'QMHRCVPM'
007000020515     C                   Parm                    RCVM0200
007100020515     C                   Parm                    RCVM0200Len
007200020515     C                   Parm      'RCVM0200'    $PFormat          8
007300020515     C                   Parm      '*'           $PMSGQ
007400020515     C                   Parm      *ZERO         $PCounter
007500020515     C                   Parm      '*ANY      '  $PType
007600020515     C                   Parm                    $PKey
007700020515     C                   Parm      *Zero         $PWait
007800020515     C                   Parm      '*REMOVE'     $PAction         10
007900020515     C                   Parm                    DSERR
008000020515     C*****
008100020515     C* Se non ci sono errori ritorno lo stato dello stack
008200020515     C*****
008300020515     C     ERBYTA        IFeq      *ZERO
008400020515     C                   MOVE      RCVPgm        £Stack(S)
008500020515     C                   EndIf
008600020515     C*
008700020515     C                   ENDDO
008800020515     C*****
008900020515     C* Ritorno la DS ed il numero di entrate processate
009000020515     C*****
009100020515     C                   Move      DSSTack       PStack
009200020515     C     S             Sub       1             PEntries
009300020515     C*
009400020515     C                   RETURN
