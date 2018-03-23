000100070801     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200070801
000300070801      *------------------------------------------------------------------------*
000400070801      *
000500070801      *         Controllo se ORM assegnabile
000600070801      *
000700070801      *------------------------------------------------------------------------*
000800070801     ffnorm01l  if   e           k disk
000900070801     ffnorg01l  if   e           k disk
001000070801
001100070801      *------------------------------------------------------------------------*
001200070801      *  RIEPILOGO INDICATORI
001300070801      *------------------------------------------------------------------------*
001400070801
001500070801      *------------------------------------------------------------------------*
001600070801      *   V A R I A B I L I
001700070801      *------------------------------------------------------------------------*
001800070801     d datadfv         s              8  0
001900070801     d dataiso         s               d
002000070801
002100070801      *------------------------------------------------------------------------*
002200070801      *   S C H I E R E
002300070801      *------------------------------------------------------------------------*
002400070801
002500070801      *------------------------------------------------------------------------*
002600070801      *   D S   I N T E R N E / E S T E R N E
002700070801      *------------------------------------------------------------------------*
002800070801     d dskeyosr        ds             5
002900070801     d  ormpoe                 1      3  0
003000070801     d  ormnsr                 4      5  0
003100070801
003200070801     d dosr          e ds
003300070801     d fior64ds      e ds
003400070801     d kpjba         e ds
003500070801     d tibs02ds      e ds
003600070801
003700070801      *------------------------------------------------------------------------*
003800070801
003900070801     c     *entry        plist
004000070801     c                   parm                    kpjba
004100070801     c                   parm                    fior64ds
004200070801
004300070801     c     korm          klist
004400070801     c                   kfld                    or64poei
004500070801     c                   kfld                    or64nsri
004600070801     c                   kfld                    or64nori
004700070801     c                   kfld                    or64nrvi
004800070801
004900070801      * controllo validità tipo lancio
005000070801     c                   if        or64tla <> 'C' and or64tla <> 'L' and
005100070801     c                             or64tla <> *blanks
005200070801     c                   clear                   or64tla
005300070801     c                   endif
005400070801
005500070801      * se tipo lancio 'C' --> non elabora....chiude solo i file
005600070801     c                   if        or64tla <> 'C'
005700070801     c                   exsr      elabora
005800070801     c                   endif
005900070801
006000070801     c                   if        or64tla <> *blanks
006100070801     c                   eval      *inlr = *on
006200070801     c                   else
006300070801     c                   eval      *inrt = *on
006400070801     c                   endif
006500070801
006600070801      *------------------------------------------------------------------------*
006700070801      * ELABORAZIONE
006800070801      *------------------------------------------------------------------------*
006900070801     c     elabora       begsr
007000070801
007100070801      * pulisco i campi di output
007200070801     c                   clear                   or64asso
007300070801     c                   clear                   or64msgo
007400070801
007500070801      * se non passata imposto la data del giorno
007600070801     c                   if        or64dfvi = *zeros
007700070801     c                   move      *date         dataiso
007800070801     c                   else
007900070801     c                   move      or64dfvi      dataiso
008000070801     c                   endif
008100070801     c                   move      dataiso       datadfv
008200070801
008300070801      * il numero orm è obbligatorio
008400070801     c                   if        or64nori = *zeros
008500070801     c                   eval      or64msgo= 'ORM non trovato'
008600070801     c                   eval      or64asso = 'N'
008700070801     c                   leavesr
008800070801     c                   endif
008900070801
009000070801      * deve essere un ORM valido su fnorm
009100070801     c     korm          chain     fnorm01l
009200070801     c                   if        not %found(fnorm01l)
009300070801     c                   eval      or64msgo= 'ORM non trovato'
009400070801     c                   eval      or64asso = 'N'
009500070801     c                   leavesr
009600070801     c                   endif
009700070801
009800070801      * deve essere un ORM valido su fnorg
009900070801     c     korm          chain     fnorg01l
010000070801     c                   if        not %found(fnorg01l)
010100070801     c                   eval      or64msgo= 'ORM non trovato'
010200070801     c                   eval      or64asso = 'N'
010300070801     c                   leavesr
010400070801     c                   endif
010500070801
010600070801      * se ORM RC (con serie) controllo con la tabella OSR
010700070801     c                   clear                   dosr
010800070801     c                   clear                   tibs02ds
010900070801     c                   if        ormnsr <> *zeros
011000070801     c                   eval      t02mod = 'C'
011100070801     c                   eval      t02sif = knsif
011200070801     c                   eval      t02cod = 'OSR'
011300070801     c                   eval      t02ke1 = dskeyosr
011400070801     c                   call      'TIBS02R'
011500070801     c                   parm                    kpjba
011600070801     c                   parm                    tibs02ds
011700070801     c                   if        t02err <> *blanks
011800070801     c                   eval      or64msgo= 'ORM con Serie ' +
011900070801     c                             %editc(ormnsr:'X') +
012000070801     c                             ' non gestita.  AVVISARE IL CED'
012100070801     c                   eval      or64asso = 'N'
012200070801     c                   leavesr
012300070801     c                   endif
012400070801     c                   eval      dosr = t02uni
012500070801     c                   if        d§osrass = 'S'
012600070801     c                   eval      or64msgo= 'ORM con Serie ' +
012700070801     c                             %editc(ormnsr:'X') +
012800070801     c                             ' non assegnabile'
012900070801     c                   eval      or64asso = 'N'
013000070801     c                   leavesr
013100070801     c                   endif
013200070801     c                   endif
013300070801
013400070801      * controllo il flag di fase assegnabile in simulazione
013500070801     c                   if        or64etii = 'S' and ormeti <> 'S' and
013600070801     c                             ormeti <> 'X'
013700070801     c                   eval      or64msgo= 'ORM in fase non assegnabile'
013800070801     c                   eval      or64asso = 'N'
013900070801     c                   leavesr
014000070801     c                   endif
014100070801      * controllo il flag di fase assegnabile
014200070801     c                   if        or64etii = *blanks and ormeti <> 'S'
014300070801     c                   eval      or64msgo= 'ORM in fase non assegnabile'
014400070801     c                   eval      or64asso = 'N'
014500070801     c                   leavesr
014600070801     c                   endif
014700070801
014800070801      * controllo se già in distinta su ORG
014900070801     c                   if        (or64ndci = 'G' or or64ndci = 'E') and
015000070801     c                              orgndc <> *zeros
015100070801     c                   eval      or64msgo= 'ORM già in distinta'
015200070801     c                   eval      or64asso = 'N'
015300070801     c                   leavesr
015400070801     c                   endif
015500070801
015600070801      * controllo se già in distinta su ORM
015700070801     c                   if        (or64ndci = 'O' or or64ndci = 'E') and
015800070802     c                              ormndc <> *zeros
015900070801     c                   eval      or64msgo= 'ORM già in distinta'
016000070801     c                   eval      or64asso = 'N'
016100070801     c                   leavesr
016200070801     c                   endif
016300070801
016400070801      * data ritiro (solo se richiesto)
016500070801     c                   if        or64dari = 'S' and
016600070801     c                             ormdar > datadfv
016700070801     c                   eval      or64msgo= 'Data ritiro superiore alla ' +
016800070801     c                             'data distinta'
016900070801     c                   eval      or64asso = 'N'
017000070801     c                   leavesr
017100070801     c                   endif
017200070801
017300070801     c                   eval      or64asso = 'S'
017400070801
017500070801     c                   endsr
