000100020621     h DECEDIT('0,') DATEDIT(*DMY.)
000200020621
000300020621      ***==========================================================***
000400020621      *** FNLV67R2 *** Copia dei record specifici per P.O.         ***
000500020621      ***              dal file FLTR350F  ai file FLTR3xxx         ***
000600020621      ***==========================================================***
000700020621
000800020621     fFLTR351L  if   e           k disk
000900020621
001000020621      *
001100020621      * === COSTANTI =============================================== *
001200020621      *
001300020621      *
001400020621      * === SCHIERE A TEMPO DI COMPILAZIONE ======================== *
001500020621      *
001600020621     d Cmd             s             80    dim(6) ctdata perrcd(1)
001700020621      *
001800020621      * === STRUTTURE DATI DEFINITE ESTERNAMENTE =================== *
001900020621      *
002000020621      * Passaggio Parametri (per ora non ne servono !!!)
002100020621     d*** KPJBA         e ds
002200020621      *
002300020621      * === STRUTTURE DATI DEFINITE INTERNAMENTE =================== *
002400020621      *
002500020621      *
002600020621      * === VARIABILI ============================================== *
002700020621      *
002800020621      * Parametri per pgm QCMDEXC
002900020621     d Comman          s            160    inz
003000020621     d Lung            s             15  5 inz(160)
003100020709      * Parametri per pgm TRUL29C
003200020709     d W29obj          s             10    inz('FLTR300F')
003300020709     d W29type         s              8    inz('*FILE')
003400020709     d W29lib          s             10    inz
003500020621
003600020621      *--------------------------------------------------------------*
003700020621      * === MAIN LINE PROGRAM ====================================== *
003800020621      *--------------------------------------------------------------*
003900020621
004000020621     c***  *entry        plist
004100020621     c***                parm                    KPJBA
004200020621      *
004300020621      * Reperisco la libreria in cui è reperibile il file 00F
004400020709     c                   call      'TRUL29C'
004500020709     c                   parm                    w29obj
004600020709     c                   parm                    w29type
004700020709     c                   parm                    w29lib
004800020621      * In caso di errore (file non trovato) chiudo direttamente
004900020709    0c                   IF        W29lib <> *blanks
005000020621      *
005100020621      * Prima creo il "vecchio" file ------------- FLTR300F
005200020621      * - Non controllo se esiste (do per scontato che esista)
005300020621      * - Copio tutti i dati
005400020621     c                   reset                   Comman
005500020621     c                   eval      %subst(Cmd(4):42:3) = '00F'
005600020621     c                   eval      Comman = %trim(Cmd(4)) + ' '
005700020621     c                                    + %trim(Cmd(6))
005800020621     c                   call      'QCMDEXC'
005900020621     c                   parm                    Comman
006000020621     c                   parm                    Lung
006100020621      *
006200020621      * Poi creo i file nuovi specifici per P.O. - FLTR3xxx
006300020621      *  per ogni P.O. letto nel completo FLTR350F
006400020621     c     *loval        setll     FLTR3500
006500020621     c                   read      FLTR3500
006600020621do  1c                   dow       not %eof(FLTR351L)
006700020621      * - Controllo se esiste il file per il P.O.
006800020621     c                   reset                   Comman
006900020621     c                   eval      %subst(Cmd(1):20:3) = TR3pog
007000020621     c                   call      'QCMDEXC'                            40
007100020621     c                   parm      Cmd(1)        Comman
007200020621     c                   parm                    Lung
007300020621if  2c                   if        *in40
007400020621     c                   reset                   Comman
007500020709     c                   eval      %subst(Cmd(2):33:10) = W29lib
007600020621     c                   eval      %subst(Cmd(3):23:3)  = TR3pog
007700020621     c                   eval      Comman = %trim(Cmd(2)) + ' '
007800020621     c                                    + %trim(Cmd(3))
007900020621     c                   call      'QCMDEXC'
008000020621     c                   parm                    Comman
008100020621     c                   parm                    Lung
008200020621e   2c                   endif
008300020621      * - Copio i dati specifici del P.O.
008400020621     c                   reset                   Comman
008500020621     c                   eval      %subst(Cmd(4):42:3) = TR3pog
008600020621     c                   eval      %subst(Cmd(5):34:3) = TR3pog
008700020621     c                   eval      Comman = %trim(Cmd(4)) + ' '
008800020621     c                                    + %trim(Cmd(5)) + ' '
008900020621     c                                    + %trim(Cmd(6))
009000020621     c                   call      'QCMDEXC'
009100020621     c                   parm                    Comman
009200020621     c                   parm                    Lung
009300020621      * - passaggio ai dati del P.O. successivo
009400020621     c     TR3pog        setgt     FLTR3500
009500020621     c                   read      FLTR3500
009600020621e   1c                   enddo
009700020621      *
009800020709    0C                   ENDIF
009900020709      *
010000020621     c                   eval      *INLR = *on
010100020621
010200020621** CMD
010300020621CHKOBJ    OBJ(FLTR3...) OBJTYPE(*FILE)
010400020621CRTDUPOBJ OBJ(FLTR300F) FROMLIB(..........) OBJTYPE(*FILE) TOLIB(*FROMLIB)
010500020621          NEWOBJ(FLTR3...) DATA(*NO)
010600020621CPYF      FROMFILE(FLTR350F) TOFILE(FLTR3...) MBROPT(*REPLACE)
010700020621          INCREL((*IF TR3POG *EQ ...))
010800020621          FMTOPT(*MAP *DROP)
