000100070801     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200070801
000300070801      *------------------------------------------------------------------------*
000400070801      *
000500070801      *         Controllo se ORM assegnabile
000600070801      *
000700070801      *------------------------------------------------------------------------*
000800070801     ffnorm01l  if   e           k disk
000900070801     ffnorg01l  if   e           k disk
001000130121     fTNTBE01L  if   e           k disk
001100070801
001200070801      *------------------------------------------------------------------------*
001300070801      *  RIEPILOGO INDICATORI
001400070801      *------------------------------------------------------------------------*
001500070801
001600070801      *------------------------------------------------------------------------*
001700070801      *   V A R I A B I L I
001800070801      *------------------------------------------------------------------------*
001900070801     d datadfv         s              8  0
002000070801     d dataiso         s               d
002100130121     d wORMcor         s              7s 0 inz
002200070801
002300070801      *------------------------------------------------------------------------*
002400070801      *   S C H I E R E
002500070801      *------------------------------------------------------------------------*
002600070801
002700070801      *------------------------------------------------------------------------*
002800070801      *   D S   I N T E R N E / E S T E R N E
002900070801      *------------------------------------------------------------------------*
003000130121     d******* dskeyosr        ds             5
003100130121     d*******  ormpoe                 1      3  0
003200130121     d*******  ormnsr                 4      5  0
003300070801
003400070801     d dosr          e ds
003500070801     d fior64ds      e ds
003600070801     d kpjba         e ds
003700130121     d******** tibs02ds      e ds
003800070801
003900070801      *------------------------------------------------------------------------*
004000070801
004100070801     c     *entry        plist
004200070801     c                   parm                    kpjba
004300070801     c                   parm                    fior64ds
004400070801
004500070801     c     korm          klist
004600070801     c                   kfld                    or64poei
004700070801     c                   kfld                    or64nsri
004800070801     c                   kfld                    or64nori
004900070801     c                   kfld                    or64nrvi
005000070801
005100070801      * controllo validità tipo lancio
005200070801     c                   if        or64tla <> 'C' and or64tla <> 'L' and
005300070801     c                             or64tla <> *blanks
005400070801     c                   clear                   or64tla
005500070801     c                   endif
005600070801
005700070801      * se tipo lancio 'C' --> non elabora....chiude solo i file
005800070801     c                   if        or64tla <> 'C'
005900070801     c                   exsr      elabora
006000070801     c                   endif
006100070801
006200070801     c                   if        or64tla <> *blanks
006300070801     c                   eval      *inlr = *on
006400070801     c                   else
006500070801     c                   eval      *inrt = *on
006600070801     c                   endif
006700070801
006800070801      *------------------------------------------------------------------------*
006900070801      * ELABORAZIONE
007000070801      *------------------------------------------------------------------------*
007100070801     c     elabora       begsr
007200070801
007300070801      * pulisco i campi di output
007400070801     c                   clear                   or64asso
007500070801     c                   clear                   or64msgo
007600070801
007700070801      * se non passata imposto la data del giorno
007800070801     c                   if        or64dfvi = *zeros
007900070801     c                   move      *date         dataiso
008000070801     c                   else
008100070801     c                   move      or64dfvi      dataiso
008200070801     c                   endif
008300070801     c                   move      dataiso       datadfv
008400070801
008500070801      * il numero orm è obbligatorio
008600070801     c                   if        or64nori = *zeros
008700070801     c                   eval      or64msgo= 'ORM non trovato'
008800070801     c                   eval      or64asso = 'N'
008900070801     c                   leavesr
009000070801     c                   endif
009100070801
009200070801      * deve essere un ORM valido su fnorm
009300070801     c     korm          chain     fnorm01l
009400070801     c                   if        not %found(fnorm01l)
009500070801     c                   eval      or64msgo= 'ORM non trovato'
009600070801     c                   eval      or64asso = 'N'
009700070801     c                   leavesr
009800070801     c                   endif
009900070801
010000070801      * deve essere un ORM valido su fnorg
010100070801     c     korm          chain     fnorg01l
010200070801     c                   if        not %found(fnorg01l)
010300070801     c                   eval      or64msgo= 'ORM non trovato'
010400070801     c                   eval      or64asso = 'N'
010500070801     c                   leavesr
010600070801     c                   endif
010700070801
010800070801      * se ORM RC (con serie) controllo con la tabella OSR
010900070801     c                   clear                   dosr
011000130121     c**********         clear                   tibs02ds
011100070801     c                   if        ormnsr <> *zeros
011200130121     c                   exsr      sr_TABosr
011300130121     c**********         eval      t02mod = 'C'
011400130121     c**********         eval      t02sif = knsif
011500130121     c**********         eval      t02cod = 'OSR'
011600130121     c**********         eval      t02ke1 = dskeyosr
011700130121     c**********         call      'TIBS02R'
011800130121     c**********         parm                    kpjba
011900130121     c**********         parm                    tibs02ds
012000130121     c**********         if        t02err <> *blanks
012100130121     c**********         eval      or64msgo= 'ORM con Serie ' +
012200130121     c**********                   %editc(ormnsr:'X') +
012300130121     c**********                   ' non gestita.  AVVISARE IL CED'
012400130121     c**********         eval      or64asso = 'N'
012500130121     c**********         leavesr
012600130121     c**********         endif
012700130121     c**********         eval      dosr = t02uni
012800130121     c**********         if        d§osrass = 'S'
012900130121     c**********         eval      or64msgo= 'ORM con Serie ' +
013000130121     c**********                   %editc(ormnsr:'X') +
013100130121     c**********                   ' non assegnabile'
013200130121     c**********         eval      or64asso = 'N'
013300130121     c**********         leavesr
013400130121     c**********         endif
013500070801     c                   endif
013600070801
013700070801      * controllo il flag di fase assegnabile in simulazione
013800070801     c                   if        or64etii = 'S' and ormeti <> 'S' and
013900070801     c                             ormeti <> 'X'
014000070801     c                   eval      or64msgo= 'ORM in fase non assegnabile'
014100070801     c                   eval      or64asso = 'N'
014200070801     c                   leavesr
014300070801     c                   endif
014400070801      * controllo il flag di fase assegnabile
014500070801     c                   if        or64etii = *blanks and ormeti <> 'S'
014600070801     c                   eval      or64msgo= 'ORM in fase non assegnabile'
014700070801     c                   eval      or64asso = 'N'
014800070801     c                   leavesr
014900070801     c                   endif
015000070801
015100070801      * controllo se già in distinta su ORG
015200070801     c                   if        (or64ndci = 'G' or or64ndci = 'E') and
015300070801     c                              orgndc <> *zeros
015400070801     c                   eval      or64msgo= 'ORM già in distinta'
015500070801     c                   eval      or64asso = 'N'
015600070801     c                   leavesr
015700070801     c                   endif
015800070801
015900070801      * controllo se già in distinta su ORM
016000070801     c                   if        (or64ndci = 'O' or or64ndci = 'E') and
016100070802     c                              ormndc <> *zeros
016200070801     c                   eval      or64msgo= 'ORM già in distinta'
016300070801     c                   eval      or64asso = 'N'
016400070801     c                   leavesr
016500070801     c                   endif
016600070801
016700070801      * data ritiro (solo se richiesto)
016800070801     c                   if        or64dari = 'S' and
016900070801     c                             ormdar > datadfv
017000070801     c                   eval      or64msgo= 'Data ritiro superiore alla ' +
017100070801     c                             'data distinta'
017200070801     c                   eval      or64asso = 'N'
017300070801     c                   leavesr
017400070801     c                   endif
017500070801
017600070801     c                   eval      or64asso = 'S'
017700070801
017800070801     c                   endsr
017900130121
018000130121      /free
018100130121       //--------------------------------------------------------------
018200130121       //?Controllo tabella OSR
018300130121       //--------------------------------------------------------------
018400130121       BEGSR sr_TABosr;
018500130121
018600130121       //?Controllo se il cliente è in tabella OSR
018700130121         clear dOSR;
018800130121         wORMcor = %dec(%subst(%editc(ORMcor:'X'):1:7):7:0);
018900130121       //?leggo tutta la tabella OSR
019000130121         TBEcod = 'OSR';
019100130121         setll TBEcod tntbe01l;
019200130121         reade TBEcod tntbe01l;
019300130121
019400130121         DOW not %eof(tntbe01l);
019500130121           dosr = tbeuni;
019600130122           IF  TBEatb = *blanks;
019700130121           IF d§osrcli  = wORMcor or d§OSRcl2  = wORMcor or
019800130121              d§osrcl3  = wORMcor or d§OSRcl4  = wORMcor or
019900130121              d§osrcl5  = wORMcor or d§OSRcl6  = wORMcor or
020000130121              d§osrcl7  = wORMcor or d§OSRcl8  = wORMcor or
020100130121              d§osrcl9  = wORMcor or d§OSRcl10 = wORMcor or
020200130121              d§osrcl11 = wORMcor or d§OSRcl12 = wORMcor or
020300130121              d§osrcl13 = wORMcor or d§OSRcl14 = wORMcor or
020400130121              d§osrcl15 = wORMcor or d§OSRcl16 = wORMcor;
020500130121             IF  d§OSRass <> *blanks;
020600130121               OR64msgo = 'ORM con Serie ' +
020700130121                           %editc(ormnsr:'X') +
020800130121                          ' non assegnabile';
020900130121               OR64asso = 'N';
021000130121             ENDIF;
021100130121             leavesr;
021200130121           ENDIF;
021300130122           ENDIF;
021400130121           reade TBEcod tntbe01l;
021500130121         ENDDO;
021600130121
021700130121       //?se arrivo qua vuol dire che non ho trovato il cliente in tabella OSR
021800130121         OR64msgo= 'ORM con Serie, Ordinante ' +
021900130121                    %editc(wORMcor:'X') +
022000130121                   ' non abilitato in tab. OSR. Avvisare IL CED';
022100130121         OR64asso = 'N';
022200130121
022300130121       ENDSR;
022400130121      /end-free
