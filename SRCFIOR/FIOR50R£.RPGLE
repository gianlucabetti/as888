000100050929     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200050929
000300050929      *------------------------------------------------------------------------*
000400050929      *                                                                        *
000500050929      *                 BLOCCO ANAGRAFICO CLIENTI RITIRO   ?                   *
000600050929      *                                                                        *
000700050929      *------------------------------------------------------------------------*
000800050929
000900050929     fFnacr01l  uf   e           k disk
001000050929
001100050929      *------------------------------------------------------------------------*
001200050929
001300050929      *   V A R I A B I L I
001400050929     d kacr            s                   like(acrcro)
001500050929     d kksc            s              7  0
001600050929     d wok             s              1    inz('0')
001700050929     d w0070           s              7  0
001800050929
001900050929      *   S C H I E R E
002000050929     d msg             s             78    Dim(20) ctdata perrcd(1)
002100050929
002200050929      *   D S   I N T E R N E / E S T E R N E
002300050929     d Fior50ds      e ds
002400050929
002500050929      *------------------------------------------------------------------------*
002600050929
002700050929      * Il cliente deve essere passato
002800050929     c                   If        i50ksc = *Zeros or i50ksc = *Blanks
002900050929     c                   Eval      o50err = '1'
003000050929     c                   Eval      o50msg = msg(1)
003100050929     c                   Goto      fine
003200050929     c                   EndIf
003300050929
003400050929      * Per ora prevediamo solo il blocco delle anagrafiche
003500050929     c                   If        i50abl = *Blanks
003600050929     c                   Goto      fine
003700050929     c                   EndIf
003800050929
003900050929     c                   Eval      wok = *Off
004000050929     c                   Clear                   kacr
004100050929     c                   Movel     i50ksc        kacr
004200050929     c                   Movel     i50ksc        kksc
004300050929      * Mi posiziono con il cliente passato e luogo 000
004400050929     c     kacr          Setll     Fnacr01l
004500050929     c                   Do        *Hival
004600050929     c                   Read      Fnacr01l
004700050929      * Fine file
004800050929     c                   If        %Eof
004900050929     c                   Leave
005000050929     c                   EndIf
005100050929      * Escludo gli annullati
005200050929     c                   If        acratb <> *Blanks
005300050929     c                   Iter
005400050929     c                   EndIf
005500050929      * Se cliente (i primi 7 byte) diverso vado a fine file
005600050929     c                   Movel     acrcro        w0070
005700050929     c                   If        w0070 <> kksc
005800050929     c                   Leave
005900050929     c                   EndIf
006000050929     c                   Eval      wok = *On
006100050929     c                   Eval      acrtcr = 'M'
006200050929     c                   Update    fnacr000
006300050929     c                   EndDo
006400050929
006500050929      * Se non ho aggiornato ritorno l'errore al chiamante
006600050929     c                   If        wok = *Off
006700050929     c                   Eval      o50err = '1'
006800050929     c                   Eval      o50msg = msg(2)
006900050929     c                   Goto      fine
007000050929     c                   EndIf
007100050929
007200050929     c     fine          Tag
007300050929
007400050929     c                   Eval      *InLr = *On
007500050929
007600050929      *------------------------------------------------------------------------*
007700050929      * ROUTINE INIZIALE
007800050929      *------------------------------------------------------------------------*
007900050929     c     *Inzsr        BegSr
008000050929
008100050929     c     *Entry        Plist
008200050929     c                   Parm                    Fior50ds
008300050929
008400050929     c                   EndSr
008500050929
008600050929      *------------------------------------------------------------------------*
008700050929
008800050929** MSG  Lungh. 78                                                            *
008900050929Blocco anagrafico ritiri non eseguito per mancanza del codice cliente         01
009000050929Blocco anagrafico ritiri non eseguito. Non trovati clienti validi             02
