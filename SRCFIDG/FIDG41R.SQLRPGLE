000100071205     h Decedit('0,') Datedit(*ymd/) Option(*nodebugio)
000200061227
000300061227      *------------------------------------------------------------------------*
000400061227      *
000500061227      *          PREPARAZIONE DATI PER PDA AUT
000600061227      *
000700061227      *------------------------------------------------------------------------*
000800080227     ffnarb70l  if   e           k disk
000900080227     ffidst08l  if   e           k disk
001000080325     ffidsf02l  uf a e           k disk
001100080311     Fprtf100   O    F  100        PRINTER prtctl(prtds)
001200061227
001300061227      *------------------------------------------------------------------------*
001400061227      *  RIEPILOGO INDICATORI
001500061227      *------------------------------------------------------------------------*
001600071205
001700061227      *------------------------------------------------------------------------*
001800080307     d tmp             s             14
001900101109     d conta           s              4  0 inz
002000061227
002100061227      *   S C H I E R E
002200061227
002300061227     d wlbdat          ds
002400061227     d  g02dat                 1      8  0
002500061227     d  g02inv                 9     16  0
002600061227     d  g02err                17     17
002700061227     d  g02tgi                18     22  0
002800061227
002900160504     d ddstflr       e ds
002901160504     d fidg41ds      e ds
003000080227     d fidg42ds      e ds
003100080311     d fnarbds       e ds                  extname(fnarb00f)
003200061227     d kpjba         e ds
003300080311     d prtds           ds
003400080311     d   spab                         3  0
003500080311     d   spaa                         3  0
003600080311     d   skab                         3  0
003700080311     d   skaa                         3  0
003800080311     d   line                         3  0
003900080227      * ?_______________________________________________________________________
004000070301
004100080311     c                   z-add     99            line
004200080227     c     kfidst        setll     fidst08l
004300080227     c                   do        *hival
004400080227     c                   read      fidst08l
004500080312     c                   clear                   carico            1
004501160504     c                   eval      ddstflr = dstflr
004600080306      *fine file
004700080227     c                   if        %eof(fidst08l)
004800080227     c                   leave
004900080227     c                   endif
005000080306      *cambio p.o
005100080306     c                   if        dstfgs <> co41fgs
005200080306     c                   leave
005300080306     c                   endif
005400080306      *cambio data
005500080306     c                   if        dstdfv > co41ddc
005600080306     c                   leave
005700080306     c                   endif
005800080306      *range di distinte
005900080227     c                   if        co41ndcd <> *zeros
006000080306     c                   if        dstnfv > co41ndca
006100080227     c                   leave
006200080227     c                   endif
006300080227     c                   endif
006400080310      *Se il lancio viene effettuato non per totale quindi PGM richiamato la
006500080310      * distinta è un parametro obbligatorio altrimenti non elaboro
006600080310     c                   if        co41tla <> 'T' and co41ndcd = *zeros
006700080310     c                   eval      co41err = 'E'
006800080310     c                   eval      co41msg = 'Richiamo non corretto manca il -
006900080313     c                             Nr.della distinta x elaborazione  -
007000080313     c                             non a totale'
007100080310     c                   leave
007200080310     c                   endif
007201120517      * verifica se foglio chiuso o annullato
007202120517     c                   if        dstatb <> *blank or
007203120517     c                             dstfcf <> *blank
007204120517     c                   iter
007205120517     c                   endif
007300080313      * verifica parzializzazione mattina/pomeriggio
007400080313     c                   if        co41tusc <> *blank and
007500080313     c                             co41tusc <> dstfpp
007600080313     c                   iter
007700080313     c                   endif
007800080313      * verifica parzializzazione Parcel
007900080313     c                   if        co41tdis = 'P' and dsttpm <> *blank
008000080313     c                   iter
008100080313     c                   endif
008200080313      *  parzializzazione Messaggerie
008300080313     c                   if        co41tdis = 'M' and dsttpm <> 'M'
008400080313     c                   iter
008500080313     c                   endif
008502160504     c* turno solo per parcel
008503160504     c                   if        (co41tur1 = ' ' or co41tur2 = ' ')
008504160504     c                             and co41tur1 <> 'U' and co41tur2 <> 'U'
008505160504     c                             and dsttpm = ' '
008506160504     c                   select
008507160504     c                   when      (co41tur1 = '1' or
008508160504     c                              co41tur2 = '1') and
008509160504     c                             §dstturno <> ' '
008510160504     C                   iter
008511160504     c                   when      (co41tur1 = '2' or
008512160504     c                              co41tur2 = '2') and
008513160504     c                             §dstturno <> '2'
008514160504     C                   iter
008515160504     c                   endsl
008516160504     c                   end
008600080313      * distinta gestita con il PDA relativamente alle consegne
008700080227     c                   if        (dstpda <> 'E' and dstpda <> 'C')
008800080227     c                   iter
008900080227     c                   else
009000080307      * verifico se distinta già scaricata di massa sul PDA
009100080310      * se già scaricata posso solo richiamare in inserimento o in annullamento
009200080325     c     kdsf          chain     fidsf02l
009300080325     c                   if        %found(fidsf02l) and co41tla = 'T'
009400080311     c                   if        line > 60
009500120614     c*mm                except    testa
009600080311     c                   endif
009700120614     c*mm                except    esistegia
009800080307     c                   iter
009900080307     c                   endif
010000080409      * elaborazione distinta
010100080227     c                   exsr      elabora
010200080227     c                   endif
010300080227     c                   enddo
010400080227
010500061227     c                   eval      *inlr = *on
010600080227      * ?_____________________________________________________
010700080227     c     elabora       begsr
010800080227      * ?_____________________________________________________
010900080227     c                   z-add     dstfgs        ifp
011000080227     c                   z-add     dstnfv        ndc
011100101109     c                   clear                   conta
011200101108     C/EXEC SQL
011300101108     C+ DECLARE B1 CURSOR FOR SELECT fnarb00f.* FROM fnarb00f join
011400101108     C+ fiarg00f on arbaas = argaas and arblnp = arglnp and arbnrs =
011500101108     C+ argnrs and arbnsp = argnsp WHERE arbifp = :ifp and arbndc = :ndc
011600101108     C+ ORDER BY arblod, argcgi, arbrsd
011700101108     C/END-EXEC
011800101108     c*
011900101108     C/EXEC SQL
012000101108     C+ OPEN b1
012100101108     C/END-EXEC
012200101108
012300101108     C                   do        *hival
012400101108
012500101108     C/EXEC SQL
012600101108     C+ FETCH NEXT FROM b1 INTO :fnarbds
012700101108     C/END-EXEC
012800101108     C                   select
012900101108     C                   when      sqlcod = 100
013000101108     c                   leave
013100101108     C                   WHEN      SqlCod < 0
013200101108     c                   other
013300101109     c                   if        co41tla = 'T'
013400101109     c                   add       1             conta
013500101109     c                   end
013600101109     c                   eval      co42ord = conta
013700080310     c                   eval      CO42TLA = co41tla
013800080227     c                   eval      CO42FGS =  arbifp
013900080227     c                   eval      CO42NDC =  arbndc
014000080227     c                   eval      CO42DDC =  arbddc
014100080227     c                   eval      CO42AAS =  arbaas
014200080227     c                   eval      CO42LNP =  arblnp
014300080227     c                   eval      CO42Nrs =  arbnrs
014400080227     c                   eval      CO42NSP =  arbnsp
014500080227     c                   movel     fidg42ds      kpjbu
014600080227     c                   call      'FIDG42R'
014700080227     c                   parm                    kpjba
014800080311     c                   parm                    fnarbds
014900080227     c                   movel     kpjbu         fidg42ds
015000080307      * se non c'è errore scrive rek su dsf per segnalare avvenuto scarico distinta su PDA
015100080310      * solo se il lancio è per 'T' scarico distinta totale
015200080227     c                   if        co42err <> ' '
015300080307     c                   else
015400080312     c                   if        co41tla = 'T' and carico = *blank
015500080307     c                   clear                   fidsf000
015600080307     c                   eval      DSFfgs    =   dstfgs
015700080307     c                   eval      DSFnpg    =   dstnpg
015800080307     c                   eval      DSFnfv    =   dstnfv
015900080307     c                   eval      DSFtrd    =   fase
016000080307     c                   eval      tmp= %char(%timestamp: *iso0)
016100080307     c                   move      tmp           DSFdtorin
016200080307     c                   eval      DSFPDAAS4 = 'A'
016300080311     c                   eval      carico = 'X'
016400080307     c                   write     fidsf000
016500080310     c                   endif
016600080227     c                   endif
016700061227
016800101108     C                   endsl
016900101108     C*
017000101108     C                   enddo
017100101108
017200101108     C/EXEC SQL
017300101108     C+ CLOSE b1
017400101108     C/END-EXEC
017500080227     c                   endsr
017600080227      * ?_____________________________________________________
017700061227     c     *inzsr        begsr
017800080227      * ?_____________________________________________________
017900061227     c     *entry        plist
018000061227     c                   parm                    kpjba
018100061227
018200080225     c                   clear                   fidg41ds
018300080227     c                   eval      fidg41ds = kpjbu
018400070124
018500070122      * Richiamato da preparazione dati di massa
018600070115
018700080225     c                   clear                   co41err
018800080225     c                   clear                   co41msg
018900080227      * se numero distinta dal maggiore di zero e distinta al = 0 lo imposto
019000080227     c                   if        co41ndcd <> *zeros and co41ndca = *zeros
019100080227     c                   eval      co41ndca = co41ndcd
019200080227     c                   endif
019300061227
019400080221
019500080227     c     kfidst        klist
019600080225     c                   kfld                    co41fgs
019700080227     c                   kfld                    cat4              1 0
019800080227     c                   kfld                    co41ddc
019900080227     c                   kfld                    co41ndcd
020000080227     c                   z-add     4             cat4
020100061227
020200080307     c     kdsf          klist
020300080307     c                   kfld                    dstnpg
020400080307     c                   kfld                    dstnfv
020500080307     c                   kfld                    dstfgs
020600080307     c                   kfld                    fase
020700080319     c                   move      'PDC'         fase              3
020800080307
020900080225     c     karb          klist
021000080227     c                   kfld                    ifp               9 0
021100080227     c                   kfld                    ndc               7 0
021200061227
021300061227     c                   endsr
021400061227
021500080311     C*--------------------------------------------------------------------
021600080311     Oprtf100   E            testa            02
021700080311     o                                           10 'FIDG41R'
021800080311     o                       knmus               22
021900080312     O          E            testa       1
022000080311     o                                           60 'Anomalie trasferimento da-
022100080311     o                                              ti a PDA'
022200080312     O          E            esistegia   1
022300080311     o                                        +   3 'La distinta'
022400080311     o                       dsfnfv        z  +   2
022500080312     o                                        +   1 'è già stata scaricata '
022600080312     o                                        +   0 'sul PDA  '
