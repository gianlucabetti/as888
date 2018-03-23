000100071205     h Decedit('0,') Datedit(*ymd/) Option(*nodebugio)
000200061227
000300061227      *------------------------------------------------------------------------*
000400061227      *
000500080311      *          Quadratura DATI PER PDA AUT
000600061227      *
000700061227      *------------------------------------------------------------------------*
000800080227     ffnarb70l  if   e           k disk
000900080311     ffnarb01l  if   e           k disk    rename(fnarb000:fnarb1)
001000080311     ffipdc01t  if   e           k disk
001100061227
001200061227      *------------------------------------------------------------------------*
001300061227      *  RIEPILOGO INDICATORI
001400061227      *------------------------------------------------------------------------*
001500071205
001600061227      *------------------------------------------------------------------------*
001700080307     d tmp             s             14
001800061227
001900061227      *   S C H I E R E
002000061227
002100061227     d wlbdat          ds
002200061227     d  g02dat                 1      8  0
002300061227     d  g02inv                 9     16  0
002400061227     d  g02err                17     17
002500061227     d  g02tgi                18     22  0
002600061227
002700080311     d fidg43ds      e ds
002800080227     d fidg42ds      e ds
002900080409     d fidg44ds      e ds
003000080311     d fnarbds       e ds                  extname(fnarb00f)
003100061227     d kpjba         e ds
003200080227      * ?_______________________________________________________________________
003300080701      * solo se non richiesta l'esclusione del controllo sulla distinta x forzarla
003400080701      * (significa che un precedente programma ha già eseguito il controllo)
003500080701      * altrimenti il pgm può essere richiamato da menù quindi deve eseguire
003600080701      * questo controllo.
003700080701     c                   if        co43ExlChk = *blank
003800080409     c                   clear                   fidg44ds
003900080409     c                   eval      CO44FGS = CO43FGS
004000080409     c                   eval      CO44DDC = CO43DDC
004100080409     c                   eval      CO44NDC = CO43NDC
004200080409     c                   eval      CO44wnd = 'S'
004300080409     c                   eval      kpjbu =  fidg44ds
004400080409     c                   call      'FIDG44R'
004500080409     c                   parm                    kpjba
004600080409     c                   eval      fidg44ds = kpjbu
004700080701     c                   end
004800080701      *
004900080701      * Se Passato il controllo del FIDG44R
005000080701      * oppure
005100080701      * se è stato escluso il controllo del FIDG44R
005200080701      *   deve eseguire la quadratura:
005300080701     c                   if        (CO44FCUS <> 'N' and co44err = *blank)
005400080701      *                            or co43ExlChk <> *blank
005500080701      *
005600080311      * verifica spedizioni presenti su arb e non presenti su pdc
005700080311     c                   exsr      elaborARB
005800080311      * verifica spedizioni presenti su PDC e non presenti su arb/distinta
005900080311     c                   exsr      elaborPDC
006000080409     c                   else
006100080409     c                   eval      co43msg = co44msg
006200080409     c                   eval      co43err = co44err
006300080227     c                   endif
006400080227
006500080311     c     fine          tag
006600080409     c                   eval      kpjbu =  fidg43ds
006700061227     c                   eval      *inlr = *on
006800080227      * ?_____________________________________________________
006900080311     c     elaborarb     begsr
007000080227      * ?_____________________________________________________
007100080409     c                   z-add     co43fgs       ifp
007200080409     c                   z-add     co43ndc       ndc
007300080227     c     karb          setll     fnarb70l
007400080227     c                   do        *hival
007500080227     c     karb          reade     fnarb70l
007600080227     c                   if        %eof(fnarb70l)
007700080227     c                   leave
007800080227     c                   endif
007900080311     c     kfipdc        chain     fipdc01t
008000080311      *spedizione non trovata aggiunge
008100080319     c                   if        not %found(fipdc01t) or
008200080319     c                             %found(fipdc01t) and pdcatb = 'A'
008300080311     c                   eval      CO42TLA = 'I'
008400080227     c                   eval      CO42FGS =  arbifp
008500080227     c                   eval      CO42NDC =  arbndc
008600080227     c                   eval      CO42DDC =  arbddc
008700080227     c                   eval      CO42AAS =  arbaas
008800080227     c                   eval      CO42LNP =  arblnp
008900080227     c                   eval      CO42Nrs =  arbnrs
009000080227     c                   eval      CO42NSP =  arbnsp
009100101109     c                   clear                   co42ord
009200080227     c                   movel     fidg42ds      kpjbu
009300080227     c                   call      'FIDG42R'
009400080227     c                   parm                    kpjba
009500080311     c                   parm                    fnarbds
009600080227     c                   movel     kpjbu         fidg42ds
009700080311
009800080227     c                   if        co42err <> ' '
009900080227     c                   endif
010000080311     c                   endif
010100061227
010200080227     c                   enddo
010300080227     c                   endsr
010400080311      * ?_____________________________________________________
010500080311     c     elaborPDC     begsr
010600080311      * ?_____________________________________________________
010700080318     c     kpdc          setll     fipdc01t
010800080318     c                   do        *hival
010900080318     c     kpdc          reade     fipdc01t
011000080318     c                   if        %eof(fipdc01t)
011100080318     c                   leave
011200080318     c                   endif
011300080318     c     karb01        chain     fnarb01l
011400080318      *spedizione non trovata annulla pdc
011500080318     c                   if        %found(fnarb01l) and arbndc <> pdcndc
011600080319     c                             and pdcatb = *blank
011700080318     c                   eval      CO42TLA = 'A'
011800080318     c                   eval      CO42FGS =  PDCfgs
011900080318     c                   eval      CO42NDC =  PDCndc
012000080409     c                   eval      CO42DDC =  co43ddc
012100080318     c                   eval      CO42AAS =  arbaas
012200080318     c                   eval      CO42LNP =  arblnp
012300080318     c                   eval      CO42Nrs =  arbnrs
012400080318     c                   eval      CO42NSP =  arbnsp
012500080318     c                   movel     fidg42ds      kpjbu
012600101109     c                   clear                   co42ord
012700080318     c                   call      'FIDG42R'
012800080318     c                   parm                    kpjba
012900080318     c                   parm                    fnarbds
013000080318     c                   movel     kpjbu         fidg42ds
013100080318     c                   if        co42err <> ' '
013200080318     c                   endif
013300080318     c                   endif
013400080318
013500080318     c                   enddo
013600080318     c                   endsr
013700080227      * ?_____________________________________________________
013800061227     c     *inzsr        begsr
013900080227      * ?_____________________________________________________
014000061227     c     *entry        plist
014100061227     c                   parm                    kpjba
014200061227
014300080311     c                   clear                   fidg43ds
014400080311     c                   eval      fidg43ds = kpjbu
014500070124
014600070122      * Richiamato da preparazione dati di massa
014700070115
014800080311     c                   clear                   co43err
014900080311     c                   clear                   co43msg
015000061227
015100080311     c     kPDC          klist
015200080409     c                   kfld                    co43FGS
015300080409     c                   kfld                    co43ndc
015400080311
015500080311     c     kfipdc        klist
015600080409     c                   kfld                    co43FGS
015700080409     c                   kfld                    co43ndc
015800080311     c                   kfld                    arbAAS
015900080311     c                   kfld                    arbLNP
016000080311     c                   kfld                    arbNRS
016100080311     c                   kfld                    arbNSP
016200080221
016300080225     c     karb          klist
016400080227     c                   kfld                    ifp               9 0
016500080227     c                   kfld                    ndc               7 0
016600080311
016700080311     c     karb01        klist
016800080311     c                   kfld                    pdcAAS
016900080311     c                   kfld                    pdcLNP
017000080311     c                   kfld                    pdcNRS
017100080311     c                   kfld                    pdcNSP
017200061227
017300061227     c                   endsr
017400061227
