000100050929     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200050929
000300050929      *------------------------------------------------------------------------*
000400050929      *                                                                        *
000500140113      *         BLOCCO/SBLOCCO ANAGRAFICO CLIENTI RITIRO   ?                   *
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
002400140113
002500140113     d dACR01        e ds
002600050929
002700050929      *------------------------------------------------------------------------*
002800050929
002900050929      * Il cliente deve essere passato
003000050929     c                   If        i50ksc = *Zeros or i50ksc = *Blanks
003100050929     c                   Eval      o50err = '1'
003200050929     c                   Eval      o50msg = msg(1)
003300050929     c                   Goto      fine
003400050929     c                   EndIf
003500050929
003600140113      * Il flag di blocco/sblocco deve essere passato
003700050929     c                   If        i50abl = *Blanks
003800050929     c                   Goto      fine
003900050929     c                   EndIf
004000050929
004100050929     c                   Eval      wok = *Off
004200050929     c                   Clear                   kacr
004300050929     c                   Movel     i50ksc        kacr
004400050929     c                   Movel     i50ksc        kksc
004500050929      * Mi posiziono con il cliente passato e luogo 000
004600050929     c     kacr          Setll     Fnacr01l
004700050929     c                   Do        *Hival
004800050929     c                   Read      Fnacr01l
004900050929      * Fine file
005000050929     c                   If        %Eof
005100050929     c                   Leave
005200050929     c                   EndIf
005300050929      * Escludo gli annullati
005400050929     c                   If        acratb <> *Blanks
005500050929     c                   Iter
005600050929     c                   EndIf
005700140113
005800140113     c                   eval      dACR01 = ACRmai
005900140113
006000050929      * Se cliente (i primi 7 byte) diverso vado a fine file
006100050929     c                   Movel     acrcro        w0070
006200050929     c                   If        w0070 <> kksc
006300050929     c                   Leave
006400050929     c                   EndIf
006500050929     c                   Eval      wok = *On
006600140113
006700140113       //?BLOCCO ANAGRAFICA
006800140114     c                   IF        I50abl = 'B' and I50auto = 'S'
006900140113     c                   eval      你CRstcr = ACRtcr
007000140113     c                   eval      你CRctcr = 'C'
007100140113     c                   ENDIF
007200140114     c                   IF        I50abl = 'B'
007300140114     c                   Eval      acrtcr = 'M'
007400140113     c                   ENDIF
007500140113
007600140113       //?SBLOCCO ANAGRAFICA solo da richiamo contabile
007700140113     c                   IF        I50abl = 'S' and I50auto = 'S'
007800140113     c                   IF        你CRstcr <> *blanks and 你CRctcr = 'C'
007900140113     c                   eval      ACRtcr = 你CRstcr
008000140113     c                   clear                   你CRstcr
008100140113     c                   clear                   你CRctcr
008200140113     c                   ENDIF
008300140113     c                   ENDIF
008400140113
008500140113     c                   eval      ACRmai = dACR01
008600050929     c                   Update    fnacr000
008700050929     c                   EndDo
008800050929
008900050929      * Se non ho aggiornato ritorno l'errore al chiamante
009000050929     c                   If        wok = *Off
009100050929     c                   Eval      o50err = '1'
009200050929     c                   Eval      o50msg = msg(2)
009300050929     c                   Goto      fine
009400050929     c                   EndIf
009500050929
009600050929     c     fine          Tag
009700050929
009800050929     c                   Eval      *InLr = *On
009900050929
010000050929      *------------------------------------------------------------------------*
010100050929      * ROUTINE INIZIALE
010200050929      *------------------------------------------------------------------------*
010300050929     c     *Inzsr        BegSr
010400050929
010500050929     c     *Entry        Plist
010600050929     c                   Parm                    Fior50ds
010700050929
010800050929     c                   EndSr
010900050929
011000050929      *------------------------------------------------------------------------*
011100050929
011200050929** MSG  Lungh. 78                                                            *
011300050929Blocco anagrafico ritiri non eseguito per mancanza del codice cliente         01
011400050929Blocco anagrafico ritiri non eseguito. Non trovati clienti validi             02
