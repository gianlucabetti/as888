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
003400080701      * (significa che un precedente programma ha gi� eseguito il controllo)
003500080701      * altrimenti il pgm pu� essere richiamato da men� quindi deve eseguire
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
005100080701      * se � stato escluso il controllo del FIDG44R
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
009100080227     c                   movel     fidg42ds      kpjbu
009200080227     c                   call      'FIDG42R'
009300080227     c                   parm                    kpjba
009400080311     c                   parm                    fnarbds
009500080227     c                   movel     kpjbu         fidg42ds
009600080311
009700080227     c                   if        co42err <> ' '
009800080227     c                   endif
009900080311     c                   endif
010000061227
010100080227     c                   enddo
010200080227     c                   endsr
010300080311      * ?_____________________________________________________
010400080311     c     elaborPDC     begsr
010500080311      * ?_____________________________________________________
010600080318     c     kpdc          setll     fipdc01t
010700080318     c                   do        *hival
010800080318     c     kpdc          reade     fipdc01t
010900080318     c                   if        %eof(fipdc01t)
011000080318     c                   leave
011100080318     c                   endif
011200080318     c     karb01        chain     fnarb01l
011300080318      *spedizione non trovata annulla pdc
011400080318     c                   if        %found(fnarb01l) and arbndc <> pdcndc
011500080319     c                             and pdcatb = *blank
011600080318     c                   eval      CO42TLA = 'A'
011700080318     c                   eval      CO42FGS =  PDCfgs
011800080318     c                   eval      CO42NDC =  PDCndc
011900080409     c                   eval      CO42DDC =  co43ddc
012000080318     c                   eval      CO42AAS =  arbaas
012100080318     c                   eval      CO42LNP =  arblnp
012200080318     c                   eval      CO42Nrs =  arbnrs
012300080318     c                   eval      CO42NSP =  arbnsp
012400080318     c                   movel     fidg42ds      kpjbu
012500080318     c                   call      'FIDG42R'
012600080318     c                   parm                    kpjba
012700080318     c                   parm                    fnarbds
012800080318     c                   movel     kpjbu         fidg42ds
012900080318     c                   if        co42err <> ' '
013000080318     c                   endif
013100080318     c                   endif
013200080318
013300080318     c                   enddo
013400080318     c                   endsr
013500080227      * ?_____________________________________________________
013600061227     c     *inzsr        begsr
013700080227      * ?_____________________________________________________
013800061227     c     *entry        plist
013900061227     c                   parm                    kpjba
014000061227
014100080311     c                   clear                   fidg43ds
014200080311     c                   eval      fidg43ds = kpjbu
014300070124
014400070122      * Richiamato da preparazione dati di massa
014500070115
014600080311     c                   clear                   co43err
014700080311     c                   clear                   co43msg
014800061227
014900080311     c     kPDC          klist
015000080409     c                   kfld                    co43FGS
015100080409     c                   kfld                    co43ndc
015200080311
015300080311     c     kfipdc        klist
015400080409     c                   kfld                    co43FGS
015500080409     c                   kfld                    co43ndc
015600080311     c                   kfld                    arbAAS
015700080311     c                   kfld                    arbLNP
015800080311     c                   kfld                    arbNRS
015900080311     c                   kfld                    arbNSP
016000080221
016100080225     c     karb          klist
016200080227     c                   kfld                    ifp               9 0
016300080227     c                   kfld                    ndc               7 0
016400080311
016500080311     c     karb01        klist
016600080311     c                   kfld                    pdcAAS
016700080311     c                   kfld                    pdcLNP
016800080311     c                   kfld                    pdcNRS
016900080311     c                   kfld                    pdcNSP
017000061227
017100061227     c                   endsr
017200061227
