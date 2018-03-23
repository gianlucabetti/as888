000100050705     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200050705
000300050705      *------------------------------------------------------------------------*
000400050705      *                                                                        *
000500050705      *                 COPIA TARIFFA/OFFERTA              ?                   *
000600050705      *                                                                        *
000700050705      *------------------------------------------------------------------------*
000800050705
000900050713     fTntam01l  uf a e           k disk
001000050713     fTitad04l  uf a e           k disk
001100050713     fTitpt01l  uf a e           k disk
001200050713     fTitpd01l  uf a e           k disk
001300050713     fTitgc01l  uf a e           k disk
001400050712     fTnofm01l  uf a e           k disk    usropn
001500050711     f                                     rename(tntam000:tnofm)
001600050712     fTiofd01l  uf a e           k disk    usropn
001700050711     f                                     rename(titad000:tiofd)
001800050712     fTiopt01l  uf a e           k disk    usropn
001900050711     f                                     rename(titpt000:tiopt)
002000050712     fTiopd01l  uf a e           k disk    usropn
002100050712     f                                     rename(titpd000:tiopd)
002200050712     fTiogc01l  uf a e           k disk    usropn
002300050712     f                                     rename(titgc000:tiogc)
002400050715     fTitav01l  if a e           k disk    usropn
002500050713     fTabel00f  if   e           k disk
002600090916     fTivis05l  if   e           k disk    usropn
002700100127     ftivof01l  if a e           k disk    usropn
002800101015     ftisis01l  if   e           k disk
002900140109     FTidpb01l  IF   E           K DISK
003000050705
003100050705      *------------------------------------------------------------------------*
003200050705      *  RIEPILOGO INDICATORI
003300050705      *------------------------------------------------------------------------*
003400050712      * 01 - Copio da Offerta  N01 - Copio da Tariffa
003500050712      * 02 - Copio  a Offerta  N02 - Copio  a Tariffa
003600050712      * 03 - Copio verso un nuovo progressivo
003700050714      * 04 - Copia da tariffa di cartello
003800050714      * 05 - Copia  a tariffa di cartello
003900050714      * 06 - Nuova tariffa è a valore
004000061012      * 07 - Duplica dettaglio con codice di tassazione
004100090916      * 08 - richiamato da trattative
004200050715      * 30 - Comodo
004300050705      *------------------------------------------------------------------------*
004400050705
004500050705      *   V A R I A B I L I
004600050715     d comlnp          s                   like(tadlnp)
004700050714     d kcap            s                   like(tadcap)
004800050713     d kcod            s                   like(tblcod)
004900050805     d kcod1           s                   like(tblcod)
005000050713     d kctr            s                   like(tamctr)
005100050725     d kftc            s                   like(tptftc)
005200050713     d kkey            s                   like(tblkey)
005300050805     d kkey1           s                   like(tblkey)
005400050713     d kksc            s                   like(tamksc)
005500050713     d kkut            s                   like(tblkut) inz(1)
005600050714     d klnp            s                   like(tadlnp)
005700050714     d knaz            s                   like(tadnaz)
005800050714     d korp            s                   like(tadorp)
005900050713     d kprg            s                   like(tamprg)
006000050714     d ksgl            s                   like(tadsgl)
006100120124     d pi              s              4  0
006200101015     d savrct          s                   like(tamrct)
006300050802     d savin01         s              1    inz('0')
006400050802     d savin04         s              1    inz('0')
006500061012     d wcap            s              1    inz('0')
006600050715     d wcts            s             20
006700050713     d wdata           s               d   datfmt(*iso)
006800050817     d wftc            s                   like(tptftc)
006900050713     d wnewprg         s                   like(tamprg)
007000050715     d wokcts          s              1    inz('0')
007100050715     d woklnp          s              1    inz('0')
007200050715     d wokrec          s              1    inz('0')
007300050715     d woksgl          s              1    inz('0')
007400061012     d worpo           s                   like(tadorp)
007500061012     d worpn           s                   like(tadorp)
007600050715     d wscts           s              1    inz('0')
007700050715     d wssgl           s              1    inz('0')
007800050713     d wtamdst         s                   like(tamdst)
007900050715     d wtav            s              1    inz('0')
008000050727     d wtpg            s                   like(tpttpg)
008100050714     d w001a           s              1
008200050715     d w002a           s              2
008300050714     d w005a           s              5
008400050714     d w0050           s              5  0
008500050728     d w0080           s              8  0
008600050713     d w0140           s             14  0
008700050713     d w0150           s             15  0
008800050713     d w0151           s             15  1
008900050713     d w0152           s             15  2
009000050713     d w0153           s             15  3
009100050920     d xx              s              3  0
009200101015     d $EoF            s              1n   inz(*off)
009300150219     d wVMA            s               n   inz(*off)
009400050819
009500050705      *   S C H I E R E
009600050802     d msg             s             78    Dim(10) ctdata perrcd(1)
009700101015     d dia             s              8  0 Dim(999)
009800080609      * schiera filiali abilitate al lasciato avviso
009900080609     d sklav           s              3  0 dim(999)
010000050705
010100050705      *   D S   I N T E R N E / E S T E R N E
010200050705
010300050705     d Azuteds       e ds                  Extname(Azute00f)
010400050705     d dDatiute      e ds
010500050715     d dsct          e ds
010600050706     d dsta01        e ds
010700050727     d dstr          e ds
010800050725     d ds1p          e ds
010900050729     d ds2g          e ds
011000050705     d Kpjba         e ds
011100050708     d Tnta25ds      e ds
011200050921     d  skcts                 41     54    dim(7)
011300050921     d  sklnpo                69    218  0 dim(50)
011400050921     d  sklnpn               219    368  0 dim(50)
011500050921     d  sktpt                369    468    dim(50)
011600050714     d Tnta25tad     e ds
011700050714     d Tnta25tgc     e ds
011800050714     d Tnta25tpd     e ds
011900050714     d Tnta25tpt     e ds
012000050801     d Tnta31ds      e ds
012100050801     d Tnta32ds      e ds
012200050725     d Tibs02ds      e ds
012300050714     d Tibs34ds      e ds
012400160229     d Trulc7ds      e ds
012500080609     d dlav          e ds
012600140227     d dtpt01        e ds
012700080609     d tntbeds       e ds                  extname(tntbe00f)
012800150219     d dVMA          e ds
012900050713
013000050713      * Reperimento dati del PGM
013100050713     d pgmstatus      sds
013200050713     d job_name              244    253
013300050713     d user                  254    263
013400150219
013500150219      //---------------------------------------------------------------
013600150219      //?Prototipi.
013700150219      //---------------------------------------------------------------
013800150219      /copy gaitrasrc/srcprotopr,TIBS02R
013900150219
014000150219      //---------------------------------------------------------------
014100090916
014200090916     itivis000
014300090916     i              visesi                      wisesi
014400050705
014500050705      *------------------------------------------------------------------------*
014600080609
014700080609     c/exec sql
014800080609     c+ set option dynusrprf = *owner, closqlcsr = *endmod
014900080609     c/end-exec
015000080609
015100080609      * carico tabella filiali attive al lasciato avviso
015200080609     c                   exsr      carlav
015300050727
015400050727      * Controllo subito se la nuova tariffa è una tariffa a valore
015500050727     c                   Movel     ta25ctrn      w001a
015600050727     c                   ExSr      Sr_Chkctr
015700050727     c                   Eval      *In06 = (§trtap = 'S')
015800050819
015900050713      * Copio il TNTAM
016000050801if  1c                   If        ta25tam <> 'N'
016100050713     c                   ExSr      Sr_Tntam
016200050725      * Aggancio il TNTAM
016300050725   x1c                   Else
016400050725     c                   Clear                   dsta01
016500050725     c                   Eval      kksc = ta25kscn
016600050725     c                   Move      ta25ctrn      kctr
016700050725     c                   Move      ta25prgn      kprg
016800050817     c  n02kTntam01      Chain(n)  Tntam01l
016900050817     c   02kTntam01      Chain(n)  Tnofm01l
017000050725     c                   If        %Found
017100050725     c                   Eval      dsta01 = tamflo
017200050725     c                   EndIf
017300050725    1c                   EndIf
017400050713
017500050713      * Copio il TITAD
017600050713     c                   If        ta25tad <> 'N'
017700050713     c                   ExSr      Sr_Titad
017800050713     c                   EndIf
017900050725
018000050725      * Copio il TITPT
018100050725     c                   If        ta25tpt <> 'N'
018200050725     c                   ExSr      Sr_Titpt
018300050725     c                   EndIf
018400050725
018500050725      * Copio il TITPD
018600050725     c                   If        ta25tpd <> 'N'
018700050725     c                   ExSr      Sr_Titpd
018800050725     c                   EndIf
018900050729
019000050729      * Copio il TITGC
019100050729     c                   If        ta25tgc <> 'N'
019200050729     c                   ExSr      Sr_Titgc
019300050729     c                   EndIf
019400050801
019500050801      * Copio i dati tipo
019600050801     c                   If        ta25cat <> 'N'
019700050801     c                   ExSr      Sr_Cat
019800050801     c                   EndIf
019900050801
020000050801      * Integro a totale la tariffa
020100080521     c                   If        ta25inglo = 'S'
020200050801     c                   ExSr      Sr_Inglo
020300050801     c                   EndIf
020400080521      * Integro la tariffa con la tariffa 'f' = Fuel
020500080521     c                   if        ta25inglo = 'f'
020600080522     c                   exsr      sr_fuel
020700080522     c                   endif
020800061012
020900061012      * Duplica dettaglio da Tnta01r2 F13
021000061012     c                   If        ta25dupd = 'S'
021100061012     c                   ExSr      Sr_Duptad
021200061012     c                   EndIf
021300050706
021400050819     c     Fine          Tag
021500050713
021600050713     c                   Eval      *InLr = *On
021700050712
021800050712      *------------------------------------------------------------------------*
021900050712      * CREO TNTAM
022000050712      *------------------------------------------------------------------------*
022100050712     c     Sr_Tntam      BegSr
022200050713
022300050713     c                   Clear                   wtamdst
022400050713     c                   Eval      kksc = ta25kscn
022500050713     c                   Move      ta25ctrn      kctr
022600050713
022700050713      * Controllo se esistono progressivi precedenti a quello da creare
022800050713     c  n02kTntam        Setgt     Tntam01l
022900101222     c   02kTntam        Setgt     Tivof01l
023000050818     c     leggi         tag
023100051125     c  n02kTntam        Readpe(n) Tntam01l
023200101222     c   02kTntam        Readpe(n) Tivof01l
023300050713      * se fine file vuol dire che non ho trovato nessun progressivo
023400050713      * precedente
023500050921if  1c                   If        %eof
023600050713     c                   Clear                   wnewprg
023700050921   x1c                   Else
023800050818      * se il progressivo appena letto è annullato torno a leggere
023900050818     c                   If        tamatb <> *Blanks
024000050818     c                   Goto      leggi
024100050818     c                   EndIf
024200050713      * se non è fine file incremento il progressivo
024300101222     c  n02              Eval      wnewprg = tamprg + 1
024400101222     c   02              Eval      wnewprg = vofprg + 1
024500050713      * mi salvo la data scadenza
024600101222     c  n02              Eval      wtamdst = tamdst
024700101222     c   02              clear                   wtamdst
024800050921    1c                   EndIf
024900050713
025000050713      * Controllo se esiste già la nuova tariffa
025100050713     c                   Eval      kprg = wnewprg
025200050713     c  n02kTntam01      Chain     Tntam01l
025300050713     c   02kTntam01      Chain     Tnofm01l
025400050714if  1c                   If        %Found
025500050713      * esiste e non è annullata esco dal pgm con msg di errore
025600050713     c                   If        Tamatb = *Blanks
025700050713     c                   Eval      ta25err = '1'
025800050713     c                   Eval      ta25msg = msg(01)
025900050713     c                   Eval      ta25tad = 'N'
026000050713     c                   Eval      ta25tpt = 'N'
026100050713     c                   Eval      ta25tgc = 'N'
026200050713     c                   Eval      ta25cat = 'N'
026300050713     c                   Goto      Fine
026400050713     c                   EndIf
026500050713      * esiste ma è annullata la cancello
026600050713     c  n02              Delete    tntam000
026700050713     c   02              Delete    tnofm
026800050714    1c                   EndIf
026900050713
027000050713      * Aggancio la testata della tariffa da cui copiare
027100050713     c                   Eval      kksc = ta25ksco
027200050713     c                   Move      ta25ctro      kctr
027300050713     c                   Move      ta25prgo      kprg
027400051125     c  n01kTntam01      Chain(n)  Tntam01l
027500051125     c   01kTntam01      Chain(n)  Tnofm01l
027600050714if  1c                   If        Not %Found or tamatb <> *Blanks
027700050713     c                   Eval      ta25err = '1'
027800050713     c                   Eval      ta25msg = msg(02)
027900050713     c                   Eval      ta25tad = 'N'
028000050713     c                   Eval      ta25tpt = 'N'
028100050713     c                   Eval      ta25tgc = 'N'
028200050713     c                   Eval      ta25cat = 'N'
028300050713     c                   Goto      Fine
028400050714    1c                   EndIf
028500050725     c                   Eval      dsta01 = tamflo
028600050714
028700050715      * --> SCRIVO TNTAM
028800050713     c                   Eval      tamksc = ta25kscn
028900050713     c                   Move      ta25ctrn      tamctr
029000050713     c                   Eval      tamprg = wnewprg
029100050803      * se copia da tariffa di cartello e tamfie non è impostato metto
029200050803      * quello che viene passato nella tnta25ds
029300050803      * (per ora vale solo se copia dalla 8888830 in quanto è l'unica che
029400050803      *  non ha il tamfie impostato)
029500050803     c                   If        *In04 and tamfie = *Blanks
029600050803     c                   Eval      tamfie = ta25fie
029700050803     c                   EndIf
029800050713      * se ho trovato progressivi precedenti imposto
029900050713      * --> data decorrenza = data scadenza prg precedente + 1 gg
030000050713      * --> data scadenza = data scadenza tariffa da cui copiare
030100050713      *     se data scadenza old >= data decorrenza new
030200050713      * --> data scadenza = data decorrenza nuova tariffa
030300050713      *     se data scadenza old < data decorrenza new
030400090916      * ma solo se non è nuova offerta
030500090916     c                   If        wnewprg <> *Zeros and not *in02
030600050713     c     *iso          Movel     wtamdst       wdata
030700050713     c                   adddur    1:*d          wdata
030800050713     c                   Move      wdata         tamddt
030900050713     c                   If        tamdst < tamddt
031000050713     c                   Eval      tamdst = tamddt
031100050713     c                   EndIf
031200050713     c                   EndIf
031300050713      * se pgm richiamato da Tnta01r imposto lo scaglione ISTAT
031400050922      * se il progressivo precedente non ha lo scaglione impostato
031500050922     c                   If        ta25ta01 = '1' and
031600101015     c                             tamrct = *Zeros and tampri = *Zeros
031700101015     c                   Eval      tamrct = savrct
031800050713     c                   Eval      tampri = 100
031900050713     c                   EndIf
032000050713      * se tariffa a valore (tipo 4) arrotondo
032100050803     c                   If        *In06 and ta25sad = 'S'
032200050713     c                   Z-add     tamata        w0153
032300050713     c                   ExSr      Sr_Arroto
032400050713     c                   Z-add     w0153         tamata
032500050713     c                   EndIf
032600151002      /free
032700151002       //?Se non è copia richiamato da TNTA01R (F9)
032800151002       //?non riporto l'applicazione peso desunto quando è:
032900151002       //?duplica su potenziale
033000151002       //?duplica su tariffa diversa
033100151002       //?duplica su ksc diverso
033200151002       //?duplica da offerta su altra offerta
033300151005         IF  TA25ta01 = *blanks;
033400151005           SELECT;
033500151002       //?duplica su potenziale
033600151002           WHEN  *in02 and VISksc = 0;
033700151002             clear TAMtpr;
033800151002       //?duplica su tariffa diversa
033900151002           WHEN  TA25ctro <> TA25ctrn;
034000151002             clear TAMtpr;
034100151023       //?duplica su ksc diverso (da tariffa a tariffa)
034200151002           WHEN  not *in02 and TA25ksco <> TA25kscn;
034300151002             clear TAMtpr;
034400151023       //?duplica su ksc diverso (da tariffa a offerta)
034500151023           WHEN  not *in01 and *in02 and TA25ksco <> VISksc;
034600151002             clear TAMtpr;
034700151002       //?duplica da offerta su altra offerta
034800151002           WHEN  *in01 and *in02 and TA25ksco <> TA25kscn;
034900151002             clear TAMtpr;
035000151002           ENDSL;
035100151002         ENDIF;
035200151002      /end-free
035300151002
035400050713     c                   Eval      tamduv = *date
035500050713     c                   Eval      tamdtr = *date
035600050713     c                   Clear                   tamftr
035700050713      * scrivo la nuova testata
035800050713     c  n02              Write     Tntam000
035900050713     c   02              Write     Tnofm
036000050714
036100050714      * imposto il nuovo progressivo creato da ritornare al chiamante
036200050714     c                   Move      tamprg        ta25prg
036300090916
036400090916      * scrivo il TIVOF in caso di creazione nuova offerta
036500101222     c                   if        *in02
036600090916     c                   clear                   tivof000
036700090916     c                   eval      vofnrv = ta25kscn
036800090916     c                   move      ta25ctrn      vofctr
036900090916     c                   eval      vofprg = %dec(wnewprg:1:0)
037000090916     c                   select
037100090916     c                   when      §tadpd = 'S'
037200090916     c                   eval      voftpt = 'D'
037300090916     c                   when      §tafed = 'S'
037400090916     c                   eval      voftpt = 'F'
037500090916     c                   other
037600090916     c                   eval      voftpt = tamfie
037700090916     c                   endsl
037800100120      * valorizzo data presentazione offerta con la data del giorno
037900100120     c                   eval      vofdpo = *date
038000090916     c                   write     tivof000
038100090916     c                   endif
038200050714
038300050715      * --> SCRIVO TITAV se copia verso tariffa
038400050714if  1c                   If        Not *In02
038500050713     c                   Clear                   Titav000
038600050715     c                   Eval      tavksc = tamksc
038700050715     c                   Eval      tavctr = tamctr
038800050715     c                   Eval      tavprg = tamprg
038900050715     c                   Eval      tavtrd = 'TES'
039000050715     c                   Eval      tavcta = 'D'
039100050715     c                   Eval      tavdav = *date
039200050713     c                   Time                    w0140
039300050715     c                   Movel     w0140         tavorv
039400050715     c                   Eval      tavnoj = job_name
039500050715     c                   Eval      tavpru = user
039600050715     c                   Eval      tavdtr = *date
039700050713     c                   Write     Titav000
039800050714    1c                   EndIf
039900050713
040000050712     c                   EndSr
040100050713
040200050713      *------------------------------------------------------------------------*
040300050713      * CREO TITAD
040400050713      *------------------------------------------------------------------------*
040500050713     c     Sr_Titad      BegSr
040600050714
040700050715      * --> COPIA A DETTAGLIO
040800050714if  1c                   If        ta25tad = 'D'
040900050715      * controllo che non esista già un record uguale
041000050714     c                   Eval      kksc = ta25kscn
041100050714     c                   Eval      kctr = ta25ctrn
041200050802     c  n03              Move      ta25prgn      kprg
041300050802     c   03              Move      ta25prg       kprg
041400050920     c                   Eval      klnp = ta25lnprn
041500050714     c                   Eval      korp = ta25orpr
041600050714     c                   Eval      knaz = ta25nazr
041700050714     c                   Eval      kcap = ta25capr
041800050714     c                   Eval      ksgl = ta25sglr
041900050805     c  n02kTitad04      Chain     Titad04l
042000050805     c   02kTitad04      Chain     Tiofd01l
042100050714if  2c                   If        %Found
042200050714      * esiste e non è annullata esco dal pgm con msg di errore
042300050729     c                   If        tadatb = *Blanks
042400050714     c                   Eval      ta25err = '1'
042500050714     c                   Eval      ta25msg = msg(03)
042600050715     c                   Eval      ta25tad = 'N'
042700050715     c                   Eval      ta25tpt = 'N'
042800050715     c                   Eval      ta25tgc = 'N'
042900050715     c                   Eval      ta25cat = 'N'
043000050714     c                   Goto      Fine
043100050714     c                   EndIf
043200050714      * esiste ma è annullata la cancello
043300050714     c  n02              Delete    titad000
043400050714     c   02              Delete    tiofd
043500050714    2c                   EndIf
043600050715
043700050715      * scrivo il nuovo dettaglio in base a quanto passato nella tnta25tad
043800050714     c                   Clear                   tadatb
043900050714     c                   Eval      tadksc = ta25kscn
044000050714     c                   Eval      tadctr = ta25ctrn
044100050802     c  n03              Move      ta25prgn      tadprg
044200050802     c   03              Move      ta25prg       tadprg
044300050920     c                   Eval      tadlnp = ta25lnprn
044400050714     c                   Eval      tadcts = ta25ctsr
044500050714     c                   Eval      tadnaz = ta25nazr
044600050714     c                   Eval      tadcap = ta25capr
044700050714     c                   Eval      tadsgl = ta25sglr
044800050714     c                   Eval      taditr = ta25itrr
044900050714     c                   Eval      tadino = ta25inor
045000050714     c                   Eval      tadrpv = ta25rpvr
045100050714     c                   Eval      tadorp = ta25orpr
045200050714     c                   Eval      tadmin = ta25minr
045300050714     c                   Eval      tadmax = ta25maxr
045400050714     c                   Clear                   tadftr
045500050714     c                   Eval      taddtr = *date
045600050714      * prima della write eseguo gli arrotondamenti
045700050803     c                   If        ta25sad = 'S'
045800050714     c                   ExSr      Sr_Arrtad
045900050803     c                   EndIf
046000050714     c  n02              Write     titad000
046100050714     c   02              Write     tiofd
046200050804
046300050715      * --> SCRIVO TITAV se copia verso tariffa
046400050804      *                  e se non è già stato scritto in un richiamo precedente
046500050912if  2c                   If        Not *In02
046600050715     c                   Eval      wtav = *Off
046700050715     c                   Eval      tavksc = tamksc
046800050715     c                   Eval      tavctr = tamctr
046900050715     c                   Eval      tavprg = tamprg
047000050715     c                   Eval      tavtrd = 'DET'
047100050715     c                   Eval      tavcta = 'D'
047200050715     c                   Eval      tavdav = *date
047300050715      *            Controllo se non è già stato scritto in precedenza
047400050715     c     kTitav        Setll     Titav01l
047500050715do  3c                   Do        *Hival
047600050715     c     kTitav        Reade     Titav01l
047700050715     c                   If        %Eof(Titav01l)
047800050715     c                   Leave
047900050715     c                   EndIf
048000050715if  4c                   If        tavnoj = job_name and tavpru = user
048100050715     c                   Eval      wtav = *On
048200050715     c                   Leave
048300050715    4c                   EndIf
048400050715    3c                   EndDo
048500050715      *            Non è stato scritto scrivo ora
048600050715if  3c                   If        wtav = *Off
048700050715     c                   Clear                   Titav000
048800050715     c                   Eval      tavksc = tamksc
048900050715     c                   Eval      tavctr = tamctr
049000050715     c                   Eval      tavprg = tamprg
049100050715     c                   Eval      tavtrd = 'DET'
049200050715     c                   Eval      tavcta = 'D'
049300050715     c                   Eval      tavdav = *date
049400050715     c                   Time                    w0140
049500050715     c                   Movel     w0140         tavorv
049600050715     c                   Eval      tavnoj = job_name
049700050715     c                   Eval      tavpru = user
049800050715     c                   Eval      tavdtr = *date
049900050715     c                   Write     Titav000
050000050715    3c                   EndIf
050100050715    2c                   EndIf
050200050714    1c                   EndIf
050300050714
050400050715      * --> COPIA TOTALE
050500050715if  1c                   If        ta25tad = 'T'
050600050715      * controllo che non esista già
050700050715     c                   Eval      kksc = ta25kscn
050800050715     c                   Eval      kctr = ta25ctrn
050900050802     c  n03              Move      ta25prgn      kprg
051000050802     c   03              Move      ta25prg       kprg
051100050921      * Per aggiunta nuovo progressivo da tnta01r leggo tutta la tariffa
051200050922      * o se sto copiando da cartello a cartello
051300061012if  2c                   If        ta25ta01 = '1' or
051400061012     c                             (*In04 and *In05)
051500050921     c     kTntam01      Setll     Titad04l
051600050921do  3c                   Do        *Hival
051700050921     c     kTntam01      Reade     Titad04l
051800050921     c                   If        %Eof
051900050921     c                   Leave
052000050921     c                   EndIf
052100050921      * esiste e non è annullata esco dal pgm con msg di errore
052200050921if  4c                   If        tadatb = *Blanks
052300050921     c                   Eval      ta25err = '1'
052400050921     c                   Eval      ta25msg = msg(03)
052500050921     c                   Eval      ta25tad = 'N'
052600050921     c                   Eval      ta25tpt = 'N'
052700050921     c                   Eval      ta25tgc = 'N'
052800050921     c                   Eval      ta25cat = 'N'
052900050921     c                   Goto      Fine
053000050921    4c                   EndIf
053100050921      * esiste ma è annullata la cancello
053200050921     c                   Delete    titad000
053300050921    3c                   EndDo
053400050921   x2c                   Else
053500050920      * Ciclo per le linee di partenza
053600050921do  3c                   Do        50            xx
053700050920     c                   If        sklnpn(xx) = *Zeros
053800050920     c                   Leave
053900050920     c                   EndIf
054000050920     c                   Eval      klnp = sklnpn(xx)
054100050805     c  n02kTitad        Setll     Titad04l
054200050805     c   02kTitad        Setll     Tiofd01l
054300050921do  4c                   Do        *Hival
054400050805     c  n02kTitad        Reade     Titad04l
054500050805     c   02kTitad        Reade     Tiofd01l
054600050920     c                   If        %Eof
054700050715     c                   Leave
054800050715     c                   EndIf
054900050715      * Controllo le inclusioni/omissioni
055000050728     c                   ExSr      Sr_Seltad
055100050921if  5c                   If        wokrec = *On
055200050715     c                   Iter
055300050921    5c                   EndIf
055400050715      * esiste e non è annullata esco dal pgm con msg di errore
055500050921if  5c                   If        tadatb = *Blanks
055600050715     c                   Eval      ta25err = '1'
055700050715     c                   Eval      ta25msg = msg(03)
055800050715     c                   Eval      ta25tad = 'N'
055900050715     c                   Eval      ta25tpt = 'N'
056000050715     c                   Eval      ta25tgc = 'N'
056100050715     c                   Eval      ta25cat = 'N'
056200050715     c                   Goto      Fine
056300050921    5c                   EndIf
056400050715      * esiste ma è annullata la cancello
056500050715     c  n02              Delete    titad000
056600050715     c   02              Delete    tiofd
056700050921    4c                   EndDo
056800050921    3c                   EndDo
056900050921    2c                   EndIf
057000050715
057100050715      * Aggancio il dettaglio della tariffa da cui copiare
057200050715     c                   Eval      kksc = ta25ksco
057300050715     c                   Move      ta25ctro      kctr
057400050715     c                   Move      ta25prgo      kprg
057500050921      * Per aggiunta nuovo progressivo da tnta01r leggo tutta la tariffa
057600050922      * o se sto copiando da cartello a cartello
057700050922if  2c                   If        ta25ta01 = '1' or
057800050922     c                             (*In04 and *In05)
057900050921     c     kTntam01      Setll     Titad04l
058000050921do  3c                   Do        *Hival
058100051125     c     kTntam01      Reade(n)  Titad04l
058200050921     c                   If        %Eof
058300050921     c                   Leave
058400050921     c                   EndIf
058500050921      * escludo annullati
058600050921     c                   If        tadatb <> *Blanks
058700050921     c                   Iter
058800050921     c                   EndIf
058900050921      * scrivo il nuovo dettaglio
059000050921     c                   Eval      tadksc = ta25kscn
059100050921     c                   Eval      tadctr = ta25ctrn
059200050921     c  n03              Move      ta25prgn      tadprg
059300050921     c   03              Move      ta25prg       tadprg
059400050921     c                   Clear                   tadftr
059500050921     c                   Eval      taddtr = *date
059600050921     c                   Write     titad000
059700050921    3c                   EndDo
059800050921   x2c                   Else
059900050920      * Ciclo per le linee di partenza
060000050921do  3c                   Do        50            xx
060100050920     c                   If        sklnpo(xx) = *Zeros
060200050920     c                   Leave
060300050920     c                   EndIf
060400050920     c                   Eval      klnp = sklnpo(xx)
060500050921     c  n01kTitad        Setll     Titad04l
060600050921     c   01kTitad        Setll     Tiofd01l
060700050921do  4c                   Do        *Hival
060800051125     c  n01kTitad        Reade(n)  Titad04l
060900051125     c   01kTitad        Reade(n)  Tiofd01l
061000050715     c                   If        %Eof
061100050715     c                   Leave
061200050715     c                   EndIf
061300050715      * escludo annullati
061400050715     c                   If        tadatb <> *Blanks
061500050715     c                   Iter
061600050715     c                   EndIf
061700050715      * Controllo le inclusioni/omissioni
061800050728     c                   ExSr      Sr_Seltad
061900050921if  5c                   If        wokrec = *On
062000050715     c                   Iter
062100050921    5c                   EndIf
062200050715      * scrivo il nuovo dettaglio
062300050715     c                   Eval      tadksc = ta25kscn
062400050715     c                   Eval      tadctr = ta25ctrn
062500050802     c  n03              Move      ta25prgn      tadprg
062600050802     c   03              Move      ta25prg       tadprg
062700050920     c                   Move      sklnpn(xx)    tadlnp
062800050715     c                   Clear                   tadftr
062900050715     c                   Eval      taddtr = *date
063000050715      * prima della write eseguo gli arrotondamenti
063100050803     c                   If        ta25sad = 'S'
063200050715     c                   ExSr      Sr_Arrtad
063300050803     c                   EndIf
063400050715     c  n02              Write     titad000
063500050715     c   02              Write     tiofd
063600050921    4c                   EndDo
063700050921    3c                   EndDo
063800050921    2c                   EndIf
063900050804
064000050715      * --> SCRIVO TITAV se copia verso tariffa
064100050912if  2c                   If        Not *In02
064200050715     c                   Clear                   Titav000
064300050715     c                   Eval      tavksc = tamksc
064400050715     c                   Eval      tavctr = tamctr
064500050715     c                   Eval      tavprg = tamprg
064600050715     c                   Eval      tavtrd = 'DET'
064700050715     c                   Eval      tavcta = 'D'
064800050715     c                   Eval      tavdav = *date
064900050715     c                   Time                    w0140
065000050715     c                   Movel     w0140         tavorv
065100050715     c                   Eval      tavnoj = job_name
065200050715     c                   Eval      tavpru = user
065300050715     c                   Eval      tavdtr = *date
065400050715     c                   Write     Titav000
065500050715    2c                   EndIf
065600050715    1c                   EndIf
065700050713
065800050713     c                   EndSr
065900050725
066000050725      *------------------------------------------------------------------------*
066100050725      * CREO TITPT
066200050725      *------------------------------------------------------------------------*
066300050725     c     Sr_Titpt      BegSr
066400050725
066500050725      * --> COPIA A DETTAGLIO
066600050725if  1c                   If        ta25tpt = 'D'
066700050725      * controllo che non esista già un record uguale
066800050725     c                   Eval      kksc = ta25kscn
066900050725     c                   Eval      kctr = ta25ctrn
067000050802     c  n03              Move      ta25prgn      kprg
067100050802     c   03              Move      ta25prg       kprg
067200050727     c                   Eval      kftc = ta25ftcr
067300050727     c  n02kTitpt        Chain     Titpt01l
067400050727     c   02kTitpt        Chain     Tiopt01l
067500050725if  2c                   If        %Found
067600050725      * esiste e non è annullata esco dal pgm con msg di errore
067700050802      * se non sto inglobando
067800050803if  3c                   If        tptatb = *Blanks and
067900080521     c                             ta25inglo = 'N'
068000050725     c                   Eval      ta25err = '1'
068100050727     c                   Eval      ta25msg = msg(04)
068200050725     c                   Eval      ta25tpt = 'N'
068300050725     c                   Eval      ta25tgc = 'N'
068400050725     c                   Eval      ta25cat = 'N'
068500050725     c                   Goto      Fine
068600050803    3c                   EndIf
068700050803      * esiste e non è annullata e sto inglobando esco dalla routine
068800050803if  3c                   If        tptatb = *Blanks and
068900080521     c                             ta25inglo <> 'N'
069000050803     c                   Leavesr
069100050803    3c                   EndIf
069200050725      * esiste ma è annullata la cancello
069300050802     c                   If        tptatb <> *Blanks
069400050727     c  n02              Delete    titpt000
069500050727     c   02              Delete    tiopt
069600050802     c                   EndIf
069700050725    2c                   EndIf
069800050725
069900050727      * prima di scrivere la testata della tariffa particolare vado a scrivere
070000050727      * TUTTO il dettaglio della tariffa particolare
070100050805      * se non ho chiesta la copia del dettaglio delle tariffe particolari
070200050922      * o se sto inglobando
070300050816     c                   If        ta25tpd = 'N' or
070400050805     c                             ta25inglo = 'S'
070500050727     c                   Eval      ta25tpd = 'T'
070600050727     c                   Eval      wtpg = ta25tpgr
070700050817     c                   Eval      wftc = ta25ftcr
070800050727     c                   ExSr      Sr_Titpd
070900050803     c                   Eval      ta25tpd = 'N'
071000050805     c                   EndIf
071100050727
071200050727      * scrivo la tariffa particolare in base a quanto passato nella tnta25tpt
071300050727     c                   Clear                   tptatb
071400050727     c                   Eval      tptksc = ta25kscn
071500050727     c                   Eval      tptctr = ta25ctrn
071600050802     c  n03              Move      ta25prgn      tptprg
071700050802     c   03              Move      ta25prg       tptprg
071800050727     c                   Eval      tptduv = *date
071900050727     c                   Eval      tptftc = ta25ftcr
072000050727     c                   Eval      tpttpg = ta25tpgr
072100050727     c                   Eval      tptarl = ta25arlr
072200050727     c                   Eval      tptarf = ta25arfr
072300050727     c                   Eval      tptaro = ta25aror
072400050727     c                   Eval      tptrpv = ta25rpvrt
072500050727     c                   Eval      tptvlm = ta25vlmr
072600050727     c                   Eval      tptvvm = ta25vvmr
072700050727     c                   Eval      tptfvm = ta25fvmr
072800050727     c                   Eval      tpttma = ta25tmar
072900050727     c                   Eval      tptapl = ta25aplr
073000060802     c                   eval      tptdpb = ta25dpbr
073100060803      * la data inserimento devo aggiornarla con la data del giorno
073200060803     c                   eval      tptdit = *date
073300060802     c                   eval      tptflo = ta25flor
073400050727     c                   Clear                   tptftr
073500050727     c                   Eval      tptdtr = *date
073600050727      * se tariffa a valore (tipo 4-T-V) arrotondo
073700050727     c                   Eval      w001a = tpttpg
073800050727     c                   ExSr      Sr_Chkctr
073900050803if  2c                   If        §trtap = 'S' and ta25sad = 'S'
074000050727     c                   Z-add     tptvlm        w0153
074100050727     c                   ExSr      Sr_Arroto
074200050727     c                   Z-add     w0153         tptvlm
074300050727    2c                   EndIf
074400050727     c  n02              Write     titpt000
074500050727     c   02              Write     tiopt
074600050804
074700050725      * --> SCRIVO TITAV se copia verso tariffa
074800050912if  2c                   If        Not *In02
074900050727     c                   Clear                   Titav000
075000050725     c                   Eval      tavksc = tamksc
075100050725     c                   Eval      tavctr = tamctr
075200050725     c                   Eval      tavprg = tamprg
075300050727     c                   Eval      tavtrd = 'PAR'
075400050725     c                   Eval      tavcta = 'D'
075500050727     c                   Eval      tavftc = tptftc
075600050725     c                   Eval      tavdav = *date
075700050725     c                   Time                    w0140
075800050725     c                   Movel     w0140         tavorv
075900050725     c                   Eval      tavnoj = job_name
076000050725     c                   Eval      tavpru = user
076100110913      * se sto inglobando metto 'I'
076200110913     c                   If        ta25inglo <> 'N'
076300110913     c                   Eval      tavcta = 'I'
076400110913     c                   EndIf
076500050725     c                   Eval      tavdtr = *date
076600050725     c                   Write     Titav000
076700050725    2c                   EndIf
076800050725    1c                   EndIf
076900050725
077000050725      * --> COPIA TOTALE
077100050725if  1c                   If        ta25tpt = 'T'
077200050725      * controllo che non esista già
077300050725     c                   Eval      kksc = ta25kscn
077400050725     c                   Eval      kctr = ta25ctrn
077500050802     c  n03              Move      ta25prgn      kprg
077600050802     c   03              Move      ta25prg       kprg
077700050725     c  n02kTntam01      Setll     Titpt01l
077800050725     c   02kTntam01      Setll     Tiopt01l
077900050725do  2c                   Do        *Hival
078000050725     c  n02kTntam01      Reade     Titpt01l
078100050725     c   02kTntam01      Reade     Tiopt01l
078200050725if  3c                   If        %Eof
078300050725     c                   Leave
078400050725    3c                   EndIf
078500050725      * controllo se la tariffa particolare è gestibile o meno
078600050725     c                   Eval      w002a = tptftc
078700050725     c                   ExSr      Sr_Chktpt
078800050725if  3c                   If        wokrec = *On
078900050725     c                   Iter
079000050725    3c                   EndIf
079100050921      * se non è aggiunta tariffa da tnta01r
079200050922      * o se non è copia da cartello a cartello
079300050921      * controllo se è una delle tariffe particolari da copiare
079400050922if  3c                   If        ta25ta01 <> '1' and Not *In04 and
079500050922     c                             Not *In05
079600050921     c     tptftc        Lookup    sktpt                                  30
079700050921      * non è presente nella sk torno a leggere
079800050921     c  n30              Iter
079900050921    3c                   EndIf
080000050725      * esiste e non è annullata esco dal pgm con msg di errore
080100050729if  3c                   If        tptatb = *Blanks
080200050725     c                   Eval      ta25err = '1'
080300050725     c                   Eval      ta25msg = msg(04)
080400050725     c                   Eval      ta25tpt = 'N'
080500050725     c                   Eval      ta25tgc = 'N'
080600050725     c                   Eval      ta25cat = 'N'
080700050725     c                   Goto      Fine
080800050725    3c                   EndIf
080900050725      * esiste ma è annullata la cancello
081000050725     c  n02              Delete    titpt000
081100050725     c   02              Delete    tiopt
081200050725    2c                   EndDo
081300050725
081400050725      * Aggancio le tariffe particolari da cui copiare
081500050725     c                   Eval      kksc = ta25ksco
081600050725     c                   Move      ta25ctro      kctr
081700050725     c                   Move      ta25prgo      kprg
081800050725     c  n01kTntam01      Setll     Titpt01l
081900050725     c   01kTntam01      Setll     Tiopt01l
082000050725do  2c                   Do        *Hival
082100051125     c  n01kTntam01      Reade(n)  Titpt01l
082200051125     c   01kTntam01      Reade(n)  Tiopt01l
082300050725     c                   If        %Eof
082400050725     c                   Leave
082500050725     c                   EndIf
082600050725      * escludo annullati
082700050725     c                   If        tptatb <> *Blanks
082800050725     c                   Iter
082900050725     c                   EndIf
083000050725      * controllo se la tariffa particolare è gestibile o meno
083100050725     c                   Eval      w002a = tptftc
083200050725     c                   ExSr      Sr_Chktpt
083300050725if  3c                   If        wokrec = *On
083400050725     c                   Iter
083500050725    3c                   EndIf
083600050921      * se non è aggiunta tariffa da tnta01r
083700050921      * controllo se è una delle tariffe particolari da copiare
083800050921if  3c                   If        ta25ta01 <> '1'
083900050921     c     tptftc        Lookup    sktpt                                  30
084000050921      * non è presente nella sk torno a leggere
084100050921     c  n30              Iter
084200050921    3c                   EndIf
084300050727
084400050727      * prima di scrivere la testata della tariffa particolare vado a scrivere
084500050727      * TUTTO il dettaglio della tariffa particolare
084600050727     c                   Eval      ta25tpd = 'T'
084700050803     c                   Eval      wtpg = tpttpg
084800050817     c                   Eval      wftc = tptftc
084900050727     c                   ExSr      Sr_Titpd
085000050803     c                   Eval      ta25tpd = 'N'
085100050725
085200050727      * scrivo la nuova tariffa particolare
085300050727     c                   Eval      tptksc = ta25kscn
085400050727     c                   Eval      tptctr = ta25ctrn
085500050802     c  n03              Move      ta25prgn      tptprg
085600050802     c   03              Move      ta25prg       tptprg
085700050727     c                   Eval      tptduv = *date
085800061130     c                   eval      tptdit = *date
085900050727     c                   Clear                   tptftr
086000050727     c                   Eval      tptdtr = *date
086100050727      * se tariffa a valore (tipo 4-T-V) arrotondo
086200050727     c                   Eval      w001a = tpttpg
086300050727     c                   ExSr      Sr_Chkctr
086400050803if  3c                   If        §trtap = 'S' and ta25sad = 'S'
086500050727     c                   Z-add     tptvlm        w0153
086600050727     c                   ExSr      Sr_Arroto
086700050727     c                   Z-add     w0153         tptvlm
086800050727    3c                   EndIf
086900050803     c  n02              Write     titpt000
087000050803     c   02              Write     tiopt
087100050804
087200050725      * --> SCRIVO TITAV se copia verso tariffa
087300050912if  3c                   If        Not *In02
087400050725     c                   Clear                   Titav000
087500050725     c                   Eval      tavksc = tamksc
087600050725     c                   Eval      tavctr = tamctr
087700050725     c                   Eval      tavprg = tamprg
087800050727     c                   Eval      tavtrd = 'PAR'
087900050725     c                   Eval      tavcta = 'D'
088000050727     c                   Eval      tavftc = tptftc
088100050725     c                   Eval      tavdav = *date
088200050725     c                   Time                    w0140
088300050725     c                   Movel     w0140         tavorv
088400050725     c                   Eval      tavnoj = job_name
088500050725     c                   Eval      tavpru = user
088600050725     c                   Eval      tavdtr = *date
088700050725     c                   Write     Titav000
088800050804    3c                   EndIf
088900050804    2c                   EndDo
089000050725    1c                   EndIf
089100050725
089200050725     c                   EndSr
089300050727
089400050727      *------------------------------------------------------------------------*
089500050727      * CREO TITPD
089600050727      *------------------------------------------------------------------------*
089700050727     c     Sr_Titpd      BegSr
089800050727
089900050727      * --> COPIA A DETTAGLIO
090000050727if  1c                   If        ta25tpd = 'D'
090100050727      * controllo che non esista già un record uguale
090200050727     c                   Eval      kksc = ta25kscn
090300050727     c                   Eval      kctr = ta25ctrn
090400050802     c  n03              Move      ta25prgn      kprg
090500050802     c   03              Move      ta25prg       kprg
090600050728     c                   Eval      kftc = ta25ftcrd
090700050728     c                   Eval      korp = ta25orprd
090800050728     c                   Eval      ksgl = ta25sglrd
090900050728     c  n02kTitpd        Chain     Titpd01l
091000050728     c   02kTitpd        Chain     Tiopd01l
091100050727if  2c                   If        %Found
091200050727      * esiste e non è annullata esco dal pgm con msg di errore
091300050802     c                   If        tpdatb = *Blanks
091400050727     c                   Eval      ta25err = '1'
091500050728     c                   Eval      ta25msg = msg(05)
091600050728     c                   Eval      %subst(ta25msg:37:2) = ta25ftcrd
091700050727     c                   Eval      ta25tpt = 'N'
091800050727     c                   Eval      ta25tgc = 'N'
091900050727     c                   Eval      ta25cat = 'N'
092000050727     c                   Goto      Fine
092100050727     c                   EndIf
092200050727      * esiste ma è annullata la cancello
092300050728     c  n02              Delete    titpd000
092400050728     c   02              Delete    tiopd
092500050727    2c                   EndIf
092600050727
092700050728      * scrivo il nuovo dettaglio in base a quanto passato nella tnta25tpd
092800050728     c                   Clear                   tpdatb
092900050728     c                   Eval      tpdksc = ta25kscn
093000050728     c                   Eval      tpdctr = ta25ctrn
093100050802     c  n03              Move      ta25prgn      tpdprg
093200050802     c   03              Move      ta25prg       tpdprg
093300050728     c                   Eval      tpdftc = ta25ftcrd
093400050728     c                   Eval      tpdcts = ta25ctsrd
093500050728     c                   Eval      tpdsgl = ta25sglrd
093600050728     c                   Eval      tpditr = ta25itrrd
093700050728     c                   Eval      tpdorp = ta25orprd
093800050728     c                   Eval      tpdmin = ta25minrd
093900050728     c                   Eval      tpdmax = ta25maxrd
094000050728     c                   Eval      tpdain = ta25ainr
094100050728     c                   Clear                   tpdftr
094200050728     c                   Eval      tpddtr = *date
094300050727      * prima della write eseguo gli arrotondamenti
094400050803     c                   If        ta25sad = 'S'
094500050728     c                   ExSr      Sr_Arrtpd
094600050803     c                   EndIf
094700050728     c  n02              Write     titpd000
094800050728     c   02              Write     tiopd
094900050804
095000050727      * --> SCRIVO TITAV se copia verso tariffa
095100050804      *                  e se non è già stato scritto in un richiamo precedente
095200050912if  2c                   If        Not *In02
095300050727     c                   Eval      wtav = *Off
095400050727     c                   Eval      tavksc = tamksc
095500050727     c                   Eval      tavctr = tamctr
095600050727     c                   Eval      tavprg = tamprg
095700050728     c                   Eval      tavtrd = 'PAR'
095800050728     c                   Eval      tavftc = tpdftc
095900050727     c                   Eval      tavcta = 'D'
096000050727     c                   Eval      tavdav = *date
096100050727      *            Controllo se non è già stato scritto in precedenza
096200050727     c     kTitav        Setll     Titav01l
096300050727do  3c                   Do        *Hival
096400050727     c     kTitav        Reade     Titav01l
096500050727     c                   If        %Eof(Titav01l)
096600050727     c                   Leave
096700050727     c                   EndIf
096800050727if  4c                   If        tavnoj = job_name and tavpru = user
096900050727     c                   Eval      wtav = *On
097000050727     c                   Leave
097100050727    4c                   EndIf
097200050727    3c                   EndDo
097300050727      *            Non è stato scritto scrivo ora
097400050727if  3c                   If        wtav = *Off
097500050727     c                   Clear                   Titav000
097600050727     c                   Eval      tavksc = tamksc
097700050727     c                   Eval      tavctr = tamctr
097800050727     c                   Eval      tavprg = tamprg
097900050728     c                   Eval      tavtrd = 'PAR'
098000050727     c                   Eval      tavcta = 'D'
098100050728     c                   Eval      tavftc = tpdftc
098200050727     c                   Eval      tavdav = *date
098300050727     c                   Time                    w0140
098400050727     c                   Movel     w0140         tavorv
098500050727     c                   Eval      tavnoj = job_name
098600050727     c                   Eval      tavpru = user
098700050727     c                   Eval      tavdtr = *date
098800050727     c                   Write     Titav000
098900050727    3c                   EndIf
099000050727    2c                   EndIf
099100050727    1c                   EndIf
099200050727
099300050727      * --> COPIA TOTALE
099400050727if  1c                   If        ta25tpd = 'T'
099500050727      * controllo che non esista già
099600050727     c                   Eval      kksc = ta25kscn
099700050727     c                   Eval      kctr = ta25ctrn
099800050802     c  n03              Move      ta25prgn      kprg
099900050802     c   03              Move      ta25prg       kprg
100000050817     c                   Eval      kftc = wftc
100100050727     c  n02kTitpt        Setll     Titpd01l
100200050727     c   02kTitpt        Setll     Tiopd01l
100300050727do  2c                   Do        *Hival
100400050727     c  n02kTitpt        Reade     Titpd01l
100500050727     c   02kTitpt        Reade     Tiopd01l
100600050727if  3c                   If        %Eof
100700050727     c                   Leave
100800050727     c                   EndIf
100900050727      * controllo se il dettaglio della tariffa particolare è da copiare o no
101000050727     c                   Eval      w002a = tpdcts
101100050727     c                   ExSr      Sr_Chktpd
101200050727if  3c                   If        wokrec = *On
101300050727     c                   Iter
101400050727    3c                   EndIf
101500050727      * esiste e non è annullata esco dal pgm con msg di errore
101600050802      * se non sto inglobando
101700050803if  3c                   If        tpdatb = *Blanks and
101800080521     c                             ta25inglo = 'N'
101900050727     c                   Eval      ta25err = '1'
102000050727     c                   Eval      ta25msg = msg(05)
102100050803     c                   Eval      %subst(ta25msg:37:2) = tptftc
102200050727     c                   Eval      ta25tpt = 'N'
102300050727     c                   Eval      ta25tgc = 'N'
102400050727     c                   Eval      ta25cat = 'N'
102500050727     c                   Goto      Fine
102600050727    3c                   EndIf
102700050803      * esiste e non è annullata e sto inglobando leggo rcd sucessivo
102800050803if  3c                   If        tpdatb = *Blanks and
102900080521     c                             ta25inglo <> 'N'
103000050803     c                   Iter
103100050803    3c                   EndIf
103200050727      * esiste ma è annullata la cancello
103300050802     c                   If        tpdatb <> *Blanks
103400050727     c  n02              Delete    titpd000
103500050727     c   02              Delete    tiopd
103600050802     c                   EndIf
103700050727    2c                   EndDo
103800050727
103900050727      * Aggancio il dettaglio della tariffa particolare da cui copiare
104000050727     c                   Eval      kksc = ta25ksco
104100050727     c                   Move      ta25ctro      kctr
104200050727     c                   Move      ta25prgo      kprg
104300050817     c                   Eval      kftc = wftc
104400050727     c  n01kTitpt        Setll     Titpd01l
104500050727     c   01kTitpt        Setll     Tiopd01l
104600050727do  2c                   Do        *Hival
104700051125     c  n01kTitpt        Reade(n)  Titpd01l
104800051125     c   01kTitpt        Reade(n)  Tiopd01l
104900050727     c                   If        %Eof
105000050727     c                   Leave
105100050727     c                   EndIf
105200050727      * escludo annullati
105300050727     c                   If        tpdatb <> *Blanks
105400050727     c                   Iter
105500050727     c                   EndIf
105600050727      * controllo se il dettaglio della tariffa particolare è da copiare o no
105700050727     c                   Eval      w002a = tpdcts
105800050727     c                   ExSr      Sr_Chktpd
105900050727if  3c                   If        wokrec = *On
106000050727     c                   Iter
106100050727    3c                   EndIf
106200050727      * scrivo il nuovo dettaglio della tariffa particolare
106300050727     c                   Eval      tpdksc = ta25kscn
106400050727     c                   Eval      tpdctr = ta25ctrn
106500050802     c  n03              Move      ta25prgn      tpdprg
106600050802     c   03              Move      ta25prg       tpdprg
106700050727     c                   Clear                   tpdftr
106800050727     c                   Eval      tpddtr = *date
106900050727      * prima della write eseguo gli arrotondamenti
107000050803     c                   If        ta25sad = 'S'
107100050727     c                   ExSr      Sr_Arrtpd
107200050803     c                   EndIf
107300050727     c  n02              Write     titpd000
107400050727     c   02              Write     tiopd
107500050727    2c                   EndDo
107600050727      * --> NON SCRIVO TITAV xchè lo scrivo una volta solo dopo che ho scritto
107700050727      *                      la testata della tariffa particolare (TITPT)
107800050727    1c                   EndIf
107900050727
108000050727     c                   EndSr
108100050729
108200050729      *------------------------------------------------------------------------*
108300050729      * CREO TITGC
108400050729      *------------------------------------------------------------------------*
108500050729     c     Sr_Titgc      BegSr
108600050729
108700050729      * --> COPIA A DETTAGLIO
108800050729if  1c                   If        ta25tgc = 'D'
108900050729      * controllo che non esista già un record uguale
109000050729     c                   Eval      kksc = ta25kscn
109100050729     c                   Eval      kctr = ta25ctrn
109200050802     c  n03              Move      ta25prgn      kprg
109300050802     c   03              Move      ta25prg       kprg
109400050729     c  n02kTntam01      Chain     Titgc01l
109500050729     c   02kTntam01      Chain     Tiogc01l
109600050729if  2c                   If        %Found
109700050729      * esiste e non è annullata esco dal pgm con msg di errore
109800050729     c                   If        tgcatb = *Blanks
109900050729     c                   Eval      ta25err = '1'
110000050729     c                   Eval      ta25msg = msg(06)
110100050729     c                   Eval      ta25tgc = 'N'
110200050729     c                   Eval      ta25cat = 'N'
110300050729     c                   Goto      Fine
110400050729     c                   EndIf
110500050729      * esiste ma è annullata la cancello
110600050729     c  n02              Delete    titgc000
110700050729     c   02              Delete    tiogc
110800050729    2c                   EndIf
110900050729
111000050729      * scrivo il nuovo dettaglio in base a quanto passato nella tnta25tgc
111100050729     c                   Clear                   tgcatb
111200050729     c                   Eval      tgcksc = ta25kscn
111300050729     c                   Eval      tgcctr = ta25ctrn
111400050802     c  n03              Move      ta25prgn      tgcprg
111500050802     c   03              Move      ta25prg       tgcprg
111600050729     c                   Eval      tgcsgv = ta25sgvr
111700050729     c                   Eval      tgcsgr = ta25sgrr
111800050729     c                   Eval      tgcsgp = ta25sgpr
111900050729     c                   Eval      tgcsgd = ta25sgdr
112000050729     c                   Eval      tgcsg1 = ta25sg1r
112100050729     c                   Eval      tgcsg2 = ta25sg2r
112200050729     c                   Eval      tgcsg3 = ta25sg3r
112300050729     c                   Eval      tgcst1 = ta25st1r
112400050729     c                   Eval      tgcst2 = ta25st2r
112500050729     c                   Eval      tgcst3 = ta25st3r
112600050729     c                   Eval      tgcstm = ta25stmr
112700050729     c                   Eval      tgcrpv = ta25rpvrg
112800050729     c                   Eval      tgcfaf = ta25fafr
112900050729     c                   Eval      tgcsgf = ta25sgfr
113000050729     c                   Eval      tgcggr = ta25ggrr
113100050729     c                   Eval      tgctcm = ta25tcmr
113200050729     c                   Eval      tgctfg = ta25tfgr
113300050729     c                   Eval      tgcduv = *date
113400050729     c                   Clear                   tgcftr
113500050729     c                   Eval      tgcdtr = *date
113600050729      * prima della write eseguo gli arrotondamenti
113700050803     c                   If        ta25sad = 'S'
113800050729     c                   ExSr      Sr_Arrtgc
113900050803     c                   EndIF
114000050729     c  n02              Write     titgc000
114100050729     c   02              Write     tiogc
114200050804
114300050729      * --> SCRIVO TITAV se copia verso tariffa
114400050912if  2c                   If        Not *In02
114500050729     c                   Clear                   Titav000
114600050729     c                   Eval      tavksc = tamksc
114700050729     c                   Eval      tavctr = tamctr
114800050729     c                   Eval      tavprg = tamprg
114900050729     c                   Eval      tavtrd = 'GIA'
115000050729     c                   Eval      tavcta = 'D'
115100050729     c                   Eval      tavdav = *date
115200050729     c                   Time                    w0140
115300050729     c                   Movel     w0140         tavorv
115400050729     c                   Eval      tavnoj = job_name
115500050729     c                   Eval      tavpru = user
115600050729     c                   Eval      tavdtr = *date
115700050729     c                   Write     Titav000
115800050804    2c                   EndIf
115900050729    1c                   EndIf
116000050729
116100050729      * --> COPIA TOTALE
116200050729if  1c                   If        ta25tgc = 'T'
116300050729      * controllo che non esista già
116400050729     c                   Eval      kksc = ta25kscn
116500050729     c                   Eval      kctr = ta25ctrn
116600050802     c  n03              Move      ta25prgn      kprg
116700050802     c   03              Move      ta25prg       kprg
116800050729     c  n02kTntam01      Chain     Titgc01l
116900050729     c   02kTntam01      Chain     Tiogc01l
117000050729if  2c                   If        %Found
117100050729      * esiste e non è annullata esco dal pgm con msg di errore
117200050802      * se non sto inglobando
117300050803if  3c                   If        tgcatb = *Blanks and
117400080521     c                             ta25inglo = 'N'
117500050729     c                   Eval      ta25err = '1'
117600050729     c                   Eval      ta25msg = msg(06)
117700050729     c                   Eval      ta25tgc = 'N'
117800050729     c                   Eval      ta25cat = 'N'
117900050729     c                   Goto      Fine
118000050803    3c                   EndIf
118100050803      * esiste e non è annullata e sto inglobando esco dalla ruotine
118200050803if  3c                   If        tgcatb = *Blanks and
118300080521     c                             ta25inglo <> 'N'
118400050803     c                   Leavesr
118500050803    3c                   EndIf
118600050729      * esiste ma è annullata la cancello
118700050802     c                   If        tgcatb <> *Blanks
118800050729     c  n02              Delete    titgc000
118900050729     c   02              Delete    tiogc
119000050802     c                   EndIf
119100050729    2c                   EndIf
119200050729
119300050729      * Aggancio le tariffe di giacenza da cui copiare
119400050729     c                   Eval      kksc = ta25ksco
119500050729     c                   Move      ta25ctro      kctr
119600050729     c                   Move      ta25prgo      kprg
119700051125     c  n01kTntam01      Chain(n)  Titgc01l
119800051125     c   01kTntam01      Chain(n)  Tiogc01l
119900050729      * scrivo il nuovo dettaglio
120000050729     c                   Eval      tgcksc = ta25kscn
120100050729     c                   Eval      tgcctr = ta25ctrn
120200050802     c  n03              Move      ta25prgn      tgcprg
120300050802     c   03              Move      ta25prg       tgcprg
120400050729     c                   Eval      tgcduv = *date
120500050729     c                   Clear                   tgcftr
120600050729     c                   Eval      tgcdtr = *date
120700050729      * prima della write eseguo gli arrotondamenti
120800050803     c                   If        ta25sad = 'S'
120900050729     c                   ExSr      Sr_Arrtgc
121000050803     c                   EndIf
121100050729      * se copia da tariffa di cartello
121200050802if  2c                   If        *In04
121300050729      * i giorni massimi di rientro per una tariffa estera vengono recuperati
121400050729      * dalla tabella 2G
121500050729     c                   If        tamfie = 'E'
121600050729     c                   Eval      tgcggr = §2ggre
121700050729     c                   EndIf
121800050729      * i giorni massimi di rientro per una tariffa FEDEX vengono recuperati
121900050729      * dalla tabella 2G
122000050729     c                   If        §tafed = 'S'
122100050729     c                   Eval      tgcggr = §2ggfe
122200050729     c                   EndIf
122300050729      * i giorni massimi di rientro per una tariffa DPD vengono recuperati
122400050729      * dalla tabella 2G
122500050729     c                   If        §tadpd = 'S'
122600050729     c                   Eval      tgcggr = §2ggdp
122700050729     c                   EndIf
122800050802    2c                   EndIf
122900050729     c  n02              Write     titgc000
123000050729     c   02              Write     tiogc
123100050729      * --> SCRIVO TITAV se copia verso tariffa
123200050912if  2c                   If        Not *In02
123300050729     c                   Clear                   Titav000
123400050729     c                   Eval      tavksc = tamksc
123500050729     c                   Eval      tavctr = tamctr
123600050729     c                   Eval      tavprg = tamprg
123700050729     c                   Eval      tavtrd = 'GIA'
123800050729     c                   Eval      tavcta = 'D'
123900050802      * se sto inglobando metto 'I'
124000050802     c                   If        ta25inglo <> 'N'
124100050802     c                   Eval      tavcta = 'I'
124200050802     c                   EndIf
124300050729     c                   Eval      tavdav = *date
124400050729     c                   Time                    w0140
124500050729     c                   Movel     w0140         tavorv
124600050729     c                   Eval      tavnoj = job_name
124700050729     c                   Eval      tavpru = user
124800050729     c                   Eval      tavdtr = *date
124900050729     c                   Write     Titav000
125000050729    2c                   EndIf
125100050729    1c                   EndIf
125200050729
125300050729     c                   EndSr
125400050801
125500050801      *------------------------------------------------------------------------*
125600050801      * CREO DATI TIPO
125700050801      *------------------------------------------------------------------------*
125800050801     c     Sr_Cat        BegSr
125900050802
126000050802      * I due pgm che vengono richiamti per la copia del CAT non prevedono
126100050802      * nessun tipo di errore di ritorno.....
126200050801
126300050801      * Se copia verso offerta
126400050801if  1c                   If        *In02
126500050801     c                   Clear                   tnta32ds
126600050801      * --> copio da tariffa
126700050801if  2c                   If        Not *In01
126800050801     c                   Eval      d32cto = 'T'
126900050801   x2c                   Else
127000050801      * --> copio da offerta
127100050801     c                   Eval      d32cto = 'O'
127200050801    2c                   EndIf
127300061009     c                   Eval      d32dsf = 'S'
127400050801     c                   Eval      d32tla = 'L'
127500050801     c                   Eval      d32ksc = ta25ksco
127600050801     c                   Eval      d32ctr = ta25ctro
127700050801     c                   Eval      d32prg = ta25prgo
127800050801     c                   Eval      d32div = 'EUR'
127900050801     c                   Eval      d32nrv = ta25kscn
128000050801     c                   Eval      d32ct2 = ta25ctrn
128100100310     c  n03              move      ta25prgn      d32pr2
128200100310     c   03              move      ta25prg       d32pr2
128300050801     c                   Eval      d32din = 'EUR'
128400050801     c                   Eval      d32lnp = comlnp
128500050801     c                   Eval      kpjbu = tnta32ds
128600050801     c                   Call      'TNTA32C'
128700050801     c                   Parm                    kpjba
128800050801    1c                   EndIf
128900050801
129000050801      * Se copia verso tariffa
129100050801if  1c                   If        Not *In02
129200050801     c                   Clear                   tnta31ds
129300050801     c                   Eval      d31tla = 'L'
129400050801     c                   Eval      d31ksc = ta25ksco
129500050801     c                   Eval      d31ctr = ta25ctro
129600050801     c                   Eval      d31prg = ta25prgo
129700050801     c                   Eval      d31div = 'EUR'
129800050801     c                   Eval      d31ksn = ta25kscn
129900050801     c                   Eval      d31ctn = ta25ctrn
130000050802     c  n03              Move      ta25prgn      d31prn
130100050802     c   03              Move      ta25prg       d31prn
130200050801     c                   Eval      d31din = 'EUR'
130300050801     c                   Eval      kpjbu = tnta31ds
130400050801     c                   Call      'TNTA31R'
130500050801     c                   Parm                    kpjba
130600050801    1c                   EndIf
130700050801
130800050801     c                   EndSr
130900050801
131000050801      *------------------------------------------------------------------------*
131100050801      * INTEGRO A TOTALE LA TARIFFA/OFFERTA
131200050801      *------------------------------------------------------------------------*
131300050801     c     Sr_Inglo      BegSr
131400050802
131500050802     c                   Eval      savin01 = *In01
131600050802     c                   Eval      savin04 = *In04
131700050801
131800050802      * Cerco la cartello da cui copiare
131900050802     c                   ExSr      Sr_Tarcar
132000070214
132100070214      * Accendo l'indicatore 04 per indicare che sto copiando da cartello
132200070214     c                   eval      *in04 = *on
132300061011
132400061011      * se non ho ricevuto la tariffa particolare da duplicare
132500061011      * devo inglobare tutte quelle che sono indicate in tabella
132600061011      * altrimenti è una duplica tariffe particolari di cartello
132700061011    0c                   if        ta25ftcr = *blanks
132800050801
132900050801      * Leggo la tabella 1P per recuperare solo le tariffe particolari
133000050801      * da inglobare dalla cartello
133100050801     c                   Clear                   ds1p
133200050805     c                   Eval      kcod1 = '1P'
133300050801     c     kTabel        Setll     Tabel00f
133400050801if  1c                   Do        *Hival
133500050801     c     kTabel        Reade     Tabel00f
133600050801      * Fine file
133700050801     c                   If        %Eof(Tabel00f)
133800050801     c                   Leave
133900050801     c                   EndIf
134000050801      * Escludo annullati
134100050801     c                   If        tblflg <> *Blanks
134200050801     c                   Iter
134300050801     c                   EndIf
134400050801     c                   Eval      ds1p = tbluni
134500050801      * Inglobo solo le tariffe particolari che hanno flag = 'I'
134600050801s   2c                   select
134700050801      * Tariffa con tipo servizio poste
134800050801     c                   When      tamtsp = 'P' and §1pfpt <> 'I'
134900050801     c                   Iter
135000050801      * Tariffa con Network DPD
135100050801     c                   When      §tadpd = 'S' and §1pfdp <> 'I'
135200050801     c                   Iter
135300050801      * Tariffa con Network FedEx
135400050801     c                   When      §tafed = 'S' and §1pffe <> 'I'
135500050801     c                   Iter
135600050801      * Tariffa Italia
135700050801     c                   When      tamfie = 'I' and §1ptco <> 'I'
135800050801     c                   Iter
135900050801      * Per tariffa Estero controllo flag §1pfie
136000050801     c                   When      tamfie = 'E' and §1pfie <> 'I'
136100050801     c                   Iter
136200050801      * Utilizzabile in tariffa
136300050802     c                   When      §1puta <> 'S'
136400050801     c                   Iter
136500050802    2c                   EndSl
136600050923      * salvo la tariffa particolare appena letta
136700050923     c                   Eval      kkey1 = tblkey
136800050802      * Leggo la tariffa particolare da copiare dalla cartello
136900160229     c                   Eval      kksc = oc7kscc
137000160229     c                   Eval      kctr = oc7ctrc
137100160229     c                   Move      oc7prgc       kprg
137200050802     c                   Eval      kftc = tblkey
137300050802     c     kTitpt        Setll     Titpt01l
137400050802do  2c                   Do        *Hival
137500050802     c     kTitpt        Reade(n)  Titpt01l
137600050802      * Fine file
137700050802     c                   If        %Eof(Titpt01l)
137800050802     c                   Leave
137900050802     c                   EndIf
138000050802      * Escludo annullati
138100050802     c                   If        tptatb <> *Blanks
138200050802     c                   Iter
138300050802     c                   EndIf
138400050802      * Imposto i dati della tnta25tpt e della tnta25ds
138500050802      * x copiare la tariffa particolare
138600050802     c                   Eval      *In01 = *Off
138700160229     c                   Eval      ta25ksco = oc7kscc
138800160229     c                   Eval      ta25ctro = oc7ctrc
138900160229     c                   Move      oc7prgc       ta25prgo
139000050802     c                   Eval      ta25ftcr = tptftc
139100050802     c                   Eval      ta25tpgr = tpttpg
139200050802     c                   Eval      ta25arlr = tptarl
139300050802     c                   Eval      ta25arfr = tptarf
139400050802     c                   Eval      ta25aror = tptaro
139500050802     c                   Eval      ta25rpvrt = tptrpv
139600050802     c                   Eval      ta25vlmr = tptvlm
139700050802     c                   Eval      ta25vvmr = tptvvm
139800050802     c                   Eval      ta25fvmr = tptfvm
139900050802     c                   Eval      ta25tmar = tpttma
140000050802     c                   Eval      ta25aplr = tptapl
140100060802     c                   eval      ta25dpbr = tptdpb
140200060803      * la data inserimento devo aggiornarla con la data del giorno
140300060803     c                   eval      ta25ditr = *date
140400060802     c                   eval      ta25flor = tptflo
140500050802      * Richiamo la routine per copiare la tariffa particolare a dettaglio
140600050803     c                   Eval      ta25tpt = 'D'
140700050802     c                   ExSr      Sr_Titpt
140800050802    2c                   EndDo
140900050805      * Mi riposiziono sulla tabella 1P
141000050805     c     kTabel1       Setgt     Tabel00f
141100050805     c
141200050801    1c                   EndDo
141300050802
141400050802      * Inglobo anche la tariffa di giacenza della cartello
141500050802     c                   Eval      *In04 = *On
141600050802      * Richiamo la routine per copiare la tariffa di giacenza
141700050802     c                   Eval      ta25tgc = 'T'
141800050802     c                   Eval      *In01 = *Off
141900160229     c                   Eval      ta25ksco = oc7kscc
142000160229     c                   Eval      ta25ctro = oc7ctrc
142100160229     c                   Move      oc7prgc       ta25prgo
142200050802     c                   ExSr      Sr_Titgc
142300061011
142400061011      * duplica tariffe particolari di cartello
142500061011   x0c                   else
142600061011      * leggo la tariffa particolare da copiare dalla cartello
142700160229     c                   eval      kksc = oc7kscc
142800160229     c                   eval      kctr = oc7ctrc
142900160229     c                   move      oc7prgc       kprg
143000061011     c                   eval      kftc = ta25ftcr
143100061011     c     ktitpt        setll     titpt01l
143200061011do  1c                   do        *hival
143300061011     c     ktitpt        reade(n)  titpt01l
143400061011      * fine file
143500061011     c                   if        %eof(titpt01l)
143600061011     c                   leave
143700061011     c                   endif
143800061011      * escludo annullati
143900061011     c                   if        tptatb <> *blanks
144000061011     c                   iter
144100061011     c                   endif
144200061011      * imposto i dati della tnta25tpt e della tnta25ds
144300061011      * x copiare la tariffa particolare
144400061011     c                   eval      *in01 = *off
144500160229     c                   eval      ta25ksco = oc7kscc
144600160229     c                   eval      ta25ctro = oc7ctrc
144700160229     c                   move      oc7prgc       ta25prgo
144800061011     c                   eval      ta25ftcr = tptftc
144900061011     c                   eval      ta25tpgr = tpttpg
145000061011     c                   eval      ta25arlr = tptarl
145100061011     c                   eval      ta25arfr = tptarf
145200061011     c                   eval      ta25aror = tptaro
145300061011     c                   eval      ta25rpvrt = tptrpv
145400061011     c                   eval      ta25vlmr = tptvlm
145500061011     c                   eval      ta25vvmr = tptvvm
145600061011     c                   eval      ta25fvmr = tptfvm
145700061011     c                   eval      ta25tmar = tpttma
145800061011     c                   eval      ta25aplr = tptapl
145900061011      * richiamo la routine per copiare la tariffa particolare a dettaglio
146000061011     c                   eval      ta25tpt = 'D'
146100061011     c                   exsr      sr_titpt
146200061011    1c                   enddo
146300061011    0c                   endif
146400150219
146500150219      /free
146600150219       //?Alla fine dei giochi controllo se il cliente è presente in
146700150219       //?tabella VMA e se la data decorrenza tariffa è compresa nelle
146800150219       //?date impostate
146900150219       //?in questo caso devo impostare NO VMA sulla tariffa particolare
147000150219       //?fuel...SEMPRE!!!!
147100150219         exsr Sr_NoVMA;
147200150219      /end-free
147300050802
147400050802     c                   Eval      *In01 = savin01
147500050802     c                   Eval      *In04 = savin04
147600050801
147700050801     c                   EndSr
147800080521
147900080521      *------------------------------------------------------------------------*
148000080521      * INTEGRO LA TARIFFA/OFFERTA CON TARIFFA PARTICOLARE FUEL
148100080521      *------------------------------------------------------------------------*
148200080521     c     sr_fuel       begsr
148300080521
148400080521      * Leggo la tabella 1P per controllare se devo creare la tariffa
148500080521      * Fuel per la tariffa interessata
148600080521     c                   clear                   ds1p
148700080521     c                   eval      kcod = '1P'
148800080522     c                   eval      kkey = 'f '
148900080521     c     ktabel01      chain     tabel00f
149000080521     c                   if        %found(tabel00f) and tblflg = *blanks
149100080521     c                   eval      ds1p = tbluni
149200080521     c                   endif
149300080521      * Creo solo se è possibile
149400080522    1c                   select
149500080521      * Tariffa con tipo servizio poste
149600080522     c                   when      tamtsp = 'P' and §1pfpt = 'N'
149700080521     c                   leavesr
149800080521      * Tariffa con Network DPD
149900080522     c                   when      §tadpd = 'S' and §1pfdp = 'N'
150000080521     c                   leavesr
150100080521      * Tariffa con Network FedEx
150200080522     c                   when      §tafed = 'S' and §1pffe = 'N'
150300080521     c                   leavesr
150400080521      * Tariffa Italia
150500080522     c                   when      tamfie = 'I' and §1ptco = 'N'
150600080521     c                   leavesr
150700080521      * Per tariffa Estero controllo flag §1pfie
150800080522     c                   when      tamfie = 'E' and §1pfie = 'N'
150900080521     c                   leavesr
151000080521      * Utilizzabile in tariffa
151100080521     c                   when      §1puta <> 'S'
151200080521     c                   leavesr
151300080522    1c                   endsl
151400080521
151500080521      * controllo che non esista già la tariffa - Testata
151600080521     c                   eval      kksc = ta25kscn
151700080521     c                   eval      kctr = ta25ctrn
151800080521     c                   move      ta25prgn      kprg
151900080522     c                   eval      kftc = 'f '
152000080522     c  n02ktitpt        chain     titpt01l
152100080522     c   02ktitpt        chain     tiopt01l
152200080522    1c                   if        %found
152300080521      * esiste e non è annullata esco dalla routine
152400080522     c                   if        tptatb = *blanks
152500080521     c                   leavesr
152600080522     c                   endif
152700080521      * esiste ma è annullata la cancello
152800080522    2c                   if        tptatb <> *blanks
152900080521     c  n02              delete    titpt000
153000080521     c   02              delete    tiopt
153100080522      * e cancello anche il relativo dettaglio
153200080522     c  n02ktitpt        setll     titpd01l
153300080522     c   02ktitpt        setll     tiopd01l
153400080522    3c                   do        *hival
153500080522     c  n02ktitpt        reade     titpd01l
153600080522     c   02ktitpt        reade     tiopd01l
153700080522     c                   if        %eof
153800080522     c                   leave
153900080522     c                   endif
154000080522     c  n02              delete    titpd000
154100080522     c   02              delete    tiopd
154200080522    3c                   enddo
154300080522    2c                   endif
154400080522    1c                   endif
154500080521
154600080522      * imposto il codice tassazione e relativo ordinamento in base
154700080522      * alla tariffa italia/estera/DPD/FedEx
154800080522     c                   select
154900080522      * Tariffa con Network DPD
155000080522     c                   when      §tadpd = 'S'
155100080522     c                   eval      w002a = 'IT'
155200080522      * Tariffa con Network FedEx
155300080522     c                   when      §tafed = 'S'
155400080522     c                   eval      w002a = 'EE'
155500080522      * Tariffa Italia
155600080522     c                   when      tamfie = 'I'
155700080522     c                   eval      w002a = 'IT'
155800080522      * Tariffa Estero
155900080522     c                   when      tamfie = 'E'
156000080522     c                   eval      w002a = 'EE'
156100080522     c                   endsl
156200080522      * cerco l'ordinamento dalla tabella CT
156300080522     c                   exsr      sr_chkcts
156400080522
156500080522      * scrivo il dettaglio della tariffa particolare
156600080522      * se non esiste già
156700080522      * (forse controllo inutile visto che prima verifico che non esista
156800080522      *  già la testata, ma così evito la rottura del programma)
156900080604     c                   evalr     korp = §ctcor
157000080522     c                   eval      ksgl = 9999999999
157100080522     c  n02ktitpd        chain     titpd01l
157200080522     c   02ktitpd        chain     tiopd01l
157300080522      * scrivo il nuovo dettaglio se non esiste
157400080522    1c                   if        not %found
157500080522     c  n02              clear                   titpd000
157600080522     c   02              clear                   tiopd
157700080522     c                   eval      tpdksc = kksc
157800080522     c                   eval      tpdctr = kctr
157900080522     c                   eval      tpdprg = kprg
158000080522     c                   eval      tpdftc = kftc
158100080522     c                   eval      tpdcts = w002a
158200080522     c                   eval      tpdorp = korp
158300080522     c                   eval      tpdsgl = ksgl
158400110525     c                   eval      tpditr = 0,5
158500080522     c                   eval      tpddtr = *date
158600080522     c  n02              write     titpd000
158700080522     c   02              write     tiopd
158800080522    1c                   endif
158900080522
159000080522      * scrivo la testata della tariffa particolare
159100080522      * (qua non controllo se esiste già...l'ho già fatto prima)
159200080522     c  n02              clear                   titpt000
159300080522     c   02              clear                   tiopt
159400140227     c                   clear                   dtpt01
159500080522     c                   eval      tptksc = kksc
159600080522     c                   eval      tptctr = kctr
159700080522     c                   eval      tptprg = kprg
159800080522     c                   eval      tptduv = *date
159900080522     c                   eval      tptftc = kftc
160000080522     c                   eval      tpttpg = 'V'
160100080522     c                   eval      tptdtr = *date
160200140109      * la data riferimento prezzo base carburante viene recuperata dal file
160300140109      * di riferimento TIDPB00F
160400140109      * verifico se esiste il file del prezzo base da impostare in base alla
160500140109      * data validità altrimenti imposto la data decorrenza tariffa
160600140109     c     *date         setll     tidpb01l
160700140109     c                   read      tidpb01l
160800140109     c                   if        %found(tidpb01l)
160900140109     c                   eval      tptdpb = dpbdpb
161000140227     c                   eval      §tptpma = dpbpma
161100140109     c                   else
161200140109     c                   eval      tptdpb = tamddt
161300140227     c                   eval      §tptpma = *zeros
161400140109     c                   endif
161500140227     c                   eval      tptflo = dtpt01
161600080522      * la data inserimento = data del giorno
161700080521     c                   eval      tptdit = *date
161800080522     c  n02              write     titpt000
161900080522     c   02              write     tiopt
162000080522
162100080522      * --> SCRIVO TITAV se copia verso tariffa
162200080522    1c                   if        not *in02
162300080522     c                   clear                   titav000
162400080522     c                   eval      tavksc = tamksc
162500080522     c                   eval      tavctr = tamctr
162600080522     c                   eval      tavprg = tamprg
162700080522     c                   eval      tavtrd = 'PAR'
162800080522     c                   eval      tavftc = tpdftc
162900080522     c                   eval      tavcta = 'I'
163000080522     c                   eval      tavdav = *date
163100080522     c                   time                    w0140
163200080522     c                   movel     w0140         tavorv
163300080522     c                   eval      tavnoj = job_name
163400080522     c                   eval      tavpru = user
163500080522     c                   eval      %subst(tavpru:10:1) = 'A'
163600080522     c                   eval      tavdtr = *date
163700080522     c                   write     titav000
163800080522    1c                   endif
163900150219
164000150219      /free
164100150219       //?Alla fine dei giochi controllo se il cliente è presente in
164200150219       //?tabella VMA e se la data decorrenza tariffa è compresa nelle
164300150219       //?date impostate
164400150219       //?in questo caso devo impostare NO VMA sulla tariffa particolare
164500150219       //?fuel...SEMPRE!!!!
164600150219         exsr Sr_NoVMA;
164700150219      /end-free
164800080521
164900080521     c                   endsr
165000061012
165100061012      *------------------------------------------------------------------------*
165200061012      * DUPLICA DETTAGLIO TARIFFARIO
165300061012      *------------------------------------------------------------------------*
165400061012     c     sr_duptad     begsr
165500061012
165600061012     c                   eval      wcap = *off
165700061012      * aggancio il dettaglio della tariffa da cui copiare
165800061012     c                   eval      kksc = ta25ksco
165900061012     c                   move      ta25ctro      kctr
166000061012     c                   move      ta25prgo      kprg
166100061012     c                   move      ta25olnp      klnp
166200061012    1c                   if        not *in07
166300061012     c  n01ktitad        setll     titad04l
166400061012     c   01ktitad        setll     tiofd01l
166500061012     c  n01ktitad        reade     titad04l
166600061012     c   01ktitad        reade     tiofd01l
166700061012    1c                   endif
166800061012    1c                   if        *in07
166900061012     c                   move      worpo         korp
167000061012     c  n01ktitad01      setll     titad04l
167100061012     c   01ktitad01      setll     tiofd01l
167200061012     c  n01ktitad01      reade     titad04l
167300061012     c   01ktitad01      reade     tiofd01l
167400061012    1c                   endif
167500061012    1c                   dow       not %eof
167600061012      * escludo annullati
167700061012    2c                   if        tadatb = *blanks
167800061012      * se richiesto anche il codice di tassazione controllo il cap
167900061012    3c                   if        *in07 and tadcap <> *blanks
168000061012     c                   eval      wcap = *on
168100061012     c                   goto      noduptad
168200061012    3c                   endif
168300061012      * la nuova tariffa se esiste la devo cancellare
168400061012     c                   eval      kksc = ta25kscn
168500061012     c                   move      ta25ctrn      kctr
168600061012     c                   move      ta25prgn      kprg
168700061012     c                   move      ta25nlnp      klnp
168800061012     c  n07              move      tadorp        korp
168900061012     c   07              move      worpn         korp
169000061012     c                   eval      knaz = tadnaz
169100061012     c                   eval      kcap = tadcap
169200061012     c                   eval      ksgl = tadsgl
169300061012     c  n02ktitad04      chain     titad04l
169400061012     c   02ktitad04      chain     tiofd01l
169500061012    3c                   if        %found
169600061012     c  n02              delete    titad000
169700061012     c   02              delete    tiofd
169800061012    3c                   endif
169900061012      * riaggancio il record da cui devo copiare
170000061012     c                   eval      kksc = ta25ksco
170100061012     c                   move      ta25ctro      kctr
170200061012     c                   move      ta25prgo      kprg
170300061012     c                   move      ta25olnp      klnp
170400061012     c  n07              move      tadorp        korp
170500061012     c   07              move      worpo         korp
170600061012     c  n01ktitad04      chain     titad04l
170700061012     c   01ktitad04      chain     tiofd01l
170800061012    3c                   if        %found
170900061012      * scrivo il nuovo dettaglio
171000061012     c                   eval      tadksc = ta25kscn
171100061012     c                   eval      tadctr = ta25ctrn
171200061012     c                   move      ta25prgn      tadprg
171300061012     c                   move      ta25nlnp      tadlnp
171400061012     c   07              move      ta25ncts      tadcts
171500061012     c   07              move      worpn         tadorp
171600061012     c                   clear                   tadftr
171700061012     c                   eval      taddtr = *date
171800061012     c  n02              write     titad000
171900061012     c   02              write     tiofd
172000061012    3c                   endif
172100061012    2c                   endif
172200061012     c     noduptad      tag
172300061012     c  n07
172400061012     cann01ktitad        reade     titad04l
172500061012     c  n07
172600061012     can 01ktitad        reade     tiofd01l
172700061012     c   07
172800061012     cann01ktitad01      reade     titad04l
172900061012     c   07
173000061012     can 01ktitad01      reade     tiofd01l
173100061012    1c                   enddo
173200061012
173300061012      * --> SCRIVO TITAV se copia verso tariffa
173400061012    1c                   if        not *in02
173500061012     c                   clear                   titav000
173600061012     c                   eval      tavksc = tamksc
173700061012     c                   eval      tavctr = tamctr
173800061012     c                   eval      tavprg = tamprg
173900061012     c                   eval      tavtrd = 'DET'
174000061012     c                   eval      tavcta = 'D'
174100061012     c                   eval      tavdav = *date
174200061012     c                   time                    w0140
174300061012     c                   movel     w0140         tavorv
174400061012     c                   eval      tavnoj = job_name
174500061012     c                   eval      tavpru = user
174600061012     c                   eval      tavdtr = *date
174700070220     c                   write     Titav000                             99
174800061012    1c                   endif
174900061012
175000061012      * se duplica con codice di tassazione ed ho trovato almeno un cap
175100061012      * messaggio info al chiamante
175200061012    1c                   if        wcap = *on
175300061012     c                   eval      ta25err = '1'
175400061012     c                   eval      ta25msg = msg(07)
175500061012    1c                   endif
175600061012
175700061012     c                   endsr
175800050727
175900050727      *------------------------------------------------------------------------*
176000050727      * CONTROLLO IL CODICE TARIFFA
176100050727      *------------------------------------------------------------------------*
176200050727     c     Sr_Chkctr     BegSr
176300050727
176400050727     c                   Clear                   dstr
176500050727     c                   Eval      kcod = 'TR'
176600050727     c                   Eval      kkey = w001a
176700050803     c     kTabel01      Chain     Tabel00f
176800050727     c                   If        %Found(Tabel00f) and tblflg = *Blanks
176900050727     c                   Eval      dstr = tbluni
177000050727     c                   EndIf
177100050727
177200050727     c                   EndSr
177300050725
177400050715      *------------------------------------------------------------------------*
177500050715      * CONTROLLO IL CODICE TASSAZIONE IN TABELLA
177600050715      *------------------------------------------------------------------------*
177700050715     c     Sr_Chkcts     BegSr
177800050715
177900050715     c                   Clear                   dsct
178000050715     c                   Eval      kcod = 'CT'
178100050715     c                   Eval      kkey = w002a
178200050803     c     kTabel01      Chain     Tabel00f
178300050715     c                   If        %Found(Tabel00f) and tblflg = *Blanks
178400050715     c                   Eval      dsct = tbluni
178500050715     c                   EndIf
178600050715
178700050715     c                   EndSr
178800050727
178900050727      *------------------------------------------------------------------------*
179000050727      * CONTROLLO SE LA TARIFFA PARTICOLARE E' GESTIBILE
179100050727      *------------------------------------------------------------------------*
179200050727     c     Sr_Chktpt     BegSr
179300050727
179400050727     c                   Eval      wokrec = *Off
179500050727
179600050727     c                   Clear                   ds1p
179700050727     c                   Eval      kcod = '1P'
179800050727     c                   Eval      kkey = w002a
179900050803     c     kTabel01      Chain     Tabel00f
180000050727     c                   If        %Found(Tabel00f) and tblflg = *Blanks
180100050727     c                   Eval      ds1p = tbluni
180200050727     c                   EndIf
180300050727
180400050727      * se tariffa FedEx e non sto copiando verso una tariffa di cartello e
180500050728      * la tariffa particolare FedEx è valida solo per la cartello non copio
180600050727if  1c                   If        §tafed = 'S' and Not *In05 and
180700050727     c                             §1pffe = 'C'
180800050727     c                   Eval      wokrec = *On
180900050727     c                   Leavesr
181000050727    1c                   EndIf
181100050727
181200050727      * copio solo le tariffe particolari che hanno il flag di gestione <> 'N'
181300050727s   1c                   Select
181400050727      * tipo servizio poste
181500050727     c                   When      tamtsp = 'P' and §1pfpt = 'N'
181600050727     c                   Eval      wokrec = *On
181700050727      * tariffa DPD
181800050727     c                   When      §tadpd = 'S' and §1pfdp = 'N'
181900050727     c                   Eval      wokrec = *On
182000050727      * tariffa FedEx
182100050727     c                   When      §tafed = 'S' and (§1pffe = 'N' or
182200050727     c                                               §1pffe = 'C')
182300050727     c                   Eval      wokrec = *On
182400050727      * tariffa Italia
182500050727     c                   When      tamfie = 'I' and §1ptco = 'N'
182600050727     c                   Eval      wokrec = *On
182700050727      * tariffa Estero
182800050727     c                   When      tamfie = 'E' and §1pfie = 'N'
182900050727     c                   Eval      wokrec = *On
183000050727    1c                   EndSl
183100050727
183200050727      * Controllo se tariffa particolare in scadenza da eliminare non copio
183300050728if  1c                   If        §1peli <> *Blanks
183400050728     c                   Move      §1peli        w0080
183500050728     c                   If        tamdst >= w0080
183600050727     c                   Eval      wokrec = *On
183700050728     c                   EndIf
183800050727    1c                   EndIf
183900051019
184000080609      * Se tariffa particolare 'c ' lasciato avviso e non sto copiando
184100080609      * verso una tariffa di cartello o verso un cliente relativo ad
184200080609      * una delle filiali abilitate non copio
184300080609if  1c                   If        w002a = 'c ' and Not *In05
184400080609     c                   if        %lookup(comlnp:sklav) = *zeros
184500080609     c                             and %lookup(999:sklav) = *zeros
184600051019     c                   Eval      wokrec = *On
184700080609    1c                   endif
184800051019    1c                   EndIf
184900050727     c                   EndSr
185000050727
185100050727      *------------------------------------------------------------------------*
185200050727      * CONTROLLO SE IL DETTAGLIO DELLA TARIFFA PARTICOLARE E' DA COPIARE
185300050727      *------------------------------------------------------------------------*
185400050727     c     Sr_Chktpd     BegSr
185500050727
185600050727     c                   Eval      wokrec = *Off
185700050727
185800050727     c                   ExSr      Sr_Chkcts
185900050727
186000050727      * copio solo se il codice di tassazione è congruente
186100050727if  1c                   If        (tamfie = 'I' and w002a = 'EE') or
186200050727     c                             (tamfie = 'E' and w002a = 'IT')
186300050727     c                   Eval      wokrec = *On
186400050727     c                   Leavesr
186500050727    1c                   EndIf
186600050727
186700050727      * se tariffa FedEx copio solo i codici di tassazione previsti
186800050727      * per FedEx
186900050727if  1c                   If        §tafed = 'S' and §ctutf <> 'S'
187000050727     c                   Eval      wokrec = *On
187100050727     c                   Leavesr
187200050727    1c                   EndIf
187300050727
187400050727      * se non è tariffa FedEx devo copiare solo i codici di tassazione
187500050727      * non previsti per FedEx
187600050727if  1c                   If        §tafed <> 'S' and §ctutf = 'S'
187700050727     c                   Eval      wokrec = *On
187800050727     c                   Leavesr
187900050727    1c                   EndIf
188000050727
188100050727      * se sto copiando da una tariffa di cartello
188200050727     c                   If        *In04
188300050727s   1c                   Select
188400050727      * Se tariffa estero copio solo estero
188500050727     c                   When      tamfie = 'E' and §ctest = *Blanks
188600050727     c                   Eval      wokrec = *On
188700050727      * Se tariffa DPD copio solo estero
188800050728      * tranne il codice generico IT xchè va bene
188900050727     c                   When      §tadpd = 'S' and §ctest = *Blanks and
189000050727     c                             tpdcts <> 'IT'
189100050727     c                   Eval      wokrec = *On
189200050727      * Se tariffa italia copio solo italia
189300050727     c                   When      tamfie = 'I' and §tadpd <> 'S' and
189400050727     c                                              §ctest <> *Blanks
189500050727     c                   Eval      wokrec = *On
189600050727     c                   EndSl
189700050728    1c                   EndIf
189800050727
189900050727     c                   EndSr
190000050727
190100050727      *------------------------------------------------------------------------*
190200050728      * CONTROLLO INCLUSIONI/OMISSIONI DETTAGLIO TARIFFARIO
190300050727      *------------------------------------------------------------------------*
190400050728     c     Sr_Seltad     BegSr
190500050727
190600050727     c                   Eval      wokrec = *Off
190700050729     c                   Eval      wokcts = *Off
190800050729     c                   Eval      woksgl = *Off
190900050727
191000050727      * Controllo le inclusioni
191100050727      * --> Codici di tassazione
191200050727if  1c                   If        wcts <> *Blanks
191300050727     c     tadcts        Lookup    skcts                                  30
191400050727      *     incluso
191500050727if  2c                   If        *In30
191600050727     c                   Eval      wokcts = *On
191700050727   x2c                   Else
191800050727      *     non incluso
191900050727if  3c                   If        Not *In30 and ta25io2 = 'I'
192000050727     c                   Eval      wokrec = *On
192100050727    3c                   EndIf
192200050727    2c                   EndIf
192300050727    1c                   EndIf
192400050727      * --> Scaglione
192500050727if  1c                   If        ta25sgl > *Zeros
192600050727      *     incluso
192700050727if  2c                   If        tadsgl = ta25sgl
192800050727     c                   Eval      woksgl = *On
192900050727   x2c                   Else
193000050727      *     non incluso
193100050727if  3c                   If        ta25sgl <> tadsgl and ta25io3 = 'I'
193200050727     c                   Eval      wokrec = *On
193300050727    3c                   EndIf
193400050727    2c                   EndIf
193500050727    1c                   EndIf
193600050727
193700050727      * Controllo le omissioni
193800050727      * --> Codici di tassazione
193900050729if  1c                   If        wscts = *On and wokcts = *On
194000050727     c                   Eval      wokrec = *On
194100050727    1c                   EndIf
194200050727      * --> Scaglione
194300050727if  1c                   If        wssgl = *On and woksgl = *On
194400050727     c                   Eval      wokrec = *On
194500050727    1c                   EndIf
194600050727
194700050727     c                   If        wokrec = *On
194800050727     c                   Leavesr
194900050727     c                   EndIf
195000050727
195100050727      * Se non sto creando una nuova tariffa di cartello
195200050727if  1c                   If        Not *In05
195300050727      * Controlli x copia da tariffa di cartello e copia totale
195400050727      * e non è la cartello DPD o FedEx
195500050727      * quindi questo vale per la creazione di una nuova tariffa
195600050727      * (non di cartello) copiando dalla 8888830
195700050727      * dove sono presenti sia codici di tassazione Italia che Estero
195800050727if  2c                   If        *In04 and ta25tad = 'T' and
195900050727     c                             §tadpd <> 'S' and §tafed <> 'S'
196000050727     c                   Eval      w002a = tadcts
196100050727     c                   ExSr      Sr_Chkcts
196200050727     c                   Select
196300050727      * Se tariffa estero copio solo estero
196400050727     c                   When      ta25fie = 'E' and §ctest = *Blanks
196500050727     c                   Eval      wokrec = *On
196600050727      * Se tariffa italia copio solo italia
196700050727     c                   When      ta25fie = 'I' and §ctest <> *Blanks
196800050727     c                   Eval      wokrec = *On
196900050727     c                   EndSl
197000050727     c                   If        wokrec = *On
197100050727     c                   Leavesr
197200050727     c                   EndIf
197300050727    2c                   EndIf
197400050727    1c                   EndIf
197500050727
197600050727     c                   EndSr
197700050728
197800050727      *------------------------------------------------------------------------*
197900050727      * ARROTONDAMENTI DETTAGLIO TARIFFARIO
198000050727      *------------------------------------------------------------------------*
198100050727     c     Sr_Arrtad     BegSr
198200050727
198300050727      * se tariffa a valore arrotondo solo lo scaglione
198400050727if  1c                   If        *In06
198500050727      * se non è 99999
198600050727     c                   Z-add     tadsgl        w0050
198700050727     c                   If        w0050 <> 99999
198800050727     c                   Z-add     tadsgl        w0153
198900050727     c                   ExSr      Sr_Arroto
199000050727     c                   Z-add     w0153         tadsgl
199100050727     c                   EndIf
199200050727      * altrimenti arrotondo città + inoltro
199300050727   x1c                   Else
199400050727     c                   Z-add     taditr        w0153
199500050727     c                   ExSr      Sr_Arroto
199600050727     c                   Z-add     w0153         taditr
199700050727     c                   Z-add     tadino        w0153
199800050727     c                   ExSr      Sr_Arroto
199900050727     c                   Z-add     w0153         tadino
200000050727    1c                   EndIf
200100050727
200200050727      * arrotondo sempre il minimo e il massimo
200300050727     c                   Z-add     tadmin        w0153
200400050727     c                   ExSr      Sr_Arroto
200500050727     c                   Z-add     w0153         tadmin
200600050727     c                   Z-add     tadmax        w0153
200700050727     c                   ExSr      Sr_Arroto
200800050727     c                   Z-add     w0153         tadmax
200900050727
201000050727     c                   EndSr
201100050727
201200050727      *------------------------------------------------------------------------*
201300050727      * ARROTONDAMENTI DETTAGLIO TARIFFE PARTICOLARI
201400050727      *------------------------------------------------------------------------*
201500050727     c     Sr_Arrtpd     BegSr
201600050727
201700050727      * se tariffa a valore arrotondo solo lo scaglione
201800050727     c                   Eval      w001a = wtpg
201900050727     c                   ExSr      Sr_Chkctr
202000050727if  1c                   If        §trtap = 'S'
202100050727      * se non è 99999
202200050727     c                   Z-add     tpdsgl        w0050
202300050727     c                   If        w0050 <> 99999
202400050727     c                   Z-add     tpdsgl        w0153
202500050727     c                   ExSr      Sr_Arroto
202600050727     c                   Z-add     w0153         tpdsgl
202700050727     c                   EndIf
202800050727      * altrimenti arrotondo la tariffa
202900050727   x1c                   Else
203000050727     c                   Z-add     tpditr        w0153
203100050727     c                   ExSr      Sr_Arroto
203200050727     c                   Z-add     w0153         tpditr
203300050728    1c                   EndIf
203400050727
203500050727      * arrotondo sempre il minimo e il massimo
203600050727     c                   Z-add     tpdmin        w0153
203700050727     c                   ExSr      Sr_Arroto
203800050727     c                   Z-add     w0153         tpdmin
203900050727     c                   Z-add     tpdmax        w0153
204000050727     c                   ExSr      Sr_Arroto
204100050727     c                   Z-add     w0153         tpdmax
204200050727
204300050727     c                   EndSr
204400050729
204500050729      *------------------------------------------------------------------------*
204600050729      * ARROTONDAMENTI TARIFFE DI GIACENZA
204700050729      *------------------------------------------------------------------------*
204800050729     c     Sr_Arrtgc     BegSr
204900050729
205000050729     c                   Z-add     tgcsgv        w0153
205100050729     c                   ExSr      Sr_Arroto
205200050729     c                   Z-add     w0153         tgcsgv
205300050729     c                   Z-add     tgcsgr        w0153
205400050729     c                   ExSr      Sr_Arroto
205500050729     c                   Z-add     w0153         tgcsgr
205600050729     c                   Z-add     tgcsgp        w0153
205700050729     c                   ExSr      Sr_Arroto
205800050729     c                   Z-add     w0153         tgcsgp
205900050729     c                   Z-add     tgcsgd        w0153
206000050729     c                   ExSr      Sr_Arroto
206100050729     c                   Z-add     w0153         tgcsgd
206200050729     c                   Z-add     tgcst1        w0153
206300050729     c                   ExSr      Sr_Arroto
206400050729     c                   Z-add     w0153         tgcst1
206500050729     c                   Z-add     tgcst2        w0153
206600050729     c                   ExSr      Sr_Arroto
206700050729     c                   Z-add     w0153         tgcst2
206800050729     c                   Z-add     tgcst3        w0153
206900050729     c                   ExSr      Sr_Arroto
207000050729     c                   Z-add     w0153         tgcst3
207100050729     c                   Z-add     tgcstm        w0153
207200050729     c                   ExSr      Sr_Arroto
207300050729     c                   Z-add     w0153         tgcstm
207400050729
207500050729     c                   EndSr
207600050727
207700050727      *------------------------------------------------------------------------*
207800050727      * ARROTONDAMENTI
207900050727      *------------------------------------------------------------------------*
208000050727     c     Sr_Arroto     BegSr
208100050727
208200050803if  1c                   If        w0153 <> *Zeros
208300050727     c                   Select
208400050727     c                   When      ta25dec = 0
208500050727     c                   Z-add(h)  w0153         w0150
208600050727     c                   Z-add     w0150         w0153
208700050727     c                   When      ta25dec = 1
208800050727     c                   Z-add(h)  w0153         w0151
208900050727     c                   Z-add     w0151         w0153
209000050727     c                   When      ta25dec = 2
209100050727     c                   Z-add(h)  w0153         w0152
209200050727     c                   Z-add     w0152         w0153
209300050727     c                   EndSl
209400050727    1c                   EndIf
209500050727
209600050727     c                   EndSr
209700050801
209800050801      *------------------------------------------------------------------------*
209900050801      * RECUPERO LA TARIFFA DI CARTELLO
210000050801      *------------------------------------------------------------------------*
210100050801     c     Sr_Tarcar     BegSr
210200050801
210300160229     c                   Clear                   Trulc7ds
210400160229     c                   Eval      ic7tla = 'L'
210500050801s   1c                   Select
211000050801      * Ntw DPD
211100050801     c                   When      §tadpd = 'S'
211200160229     c                   Eval      ic7tntw = 'DPD'
211300050801      * Ntw FedEx
211400050801     c                   When      §tafed = 'S'
211500160229     c                   Eval      ic7tntw = 'FED'
211600050801      * passo anche il codice tariffa per recuperare se merci o documenti
211700160229     c                   Move      ta25ctrn      ic7ctb
211800050801      * Tariffa estero
211900050801     c                   When      tamfie = 'E'
212000160229     c                   Eval      ic7tntw = 'EEX'
212100050801      * Tariffa italia
212200050801     c                   When      tamfie = 'I'
212300160229     c                   Eval      ic7tntw = 'COR'
212400050922      * se nessuna di queste (mi sa che è solo x la cartello 8888830)
212500160229      * passo in fie = I
212600050922     c                   Other
212700160229     c                   Eval      ic7tfie = 'I'
212800050801    1c                   Endsl
212900050802      * Passo la data decorrenza per recuperare anche il progressivo della
213000050802      * tariffa di cartello
213100111031      * se nella ds TNTA25DS il campo TA25drc = 'S' devo prendere per recuperare
213200111031      * la tariffa di cartello la data maggiore tra oggi e la data decorrenza
213300111031      * della tariffa che riceve la copia
213400111031     c                   If        Ta25drc = 'S' and tamddt < *date
213500160229     c                   Eval      ic7dta = *date
213600111031     c                   Else
213700160229     c                   Eval      ic7dta = tamddt
213800111031     c                   Endif
213900111026
214000160229     c                   Call      'TRULC7R'
214100160229     c                   Parm                    Trulc7ds
214200050802
214300160229     c                   If        oc7err <> *Blanks
214400050802     c                   Goto      Fine
214500050802     c                   EndIf
214600050801
214700050801     c                   EndSr
214800080609
214900080609      *------------------------------------------------------------------------*
215000080609      * Carico filiali abilitate al lasciato avviso
215100080609      *------------------------------------------------------------------------*
215200080609     c     carlav        begsr
215300080609
215400080609     c                   clear                   sklav
215500080609     c                   clear                   xx
215600080609
215700080609     c/exec sql
215800080609     c+ declare lav cursor for select tntbe01l.*
215900080609     c+ from tntbe01l
216000080609     c+ where tbecod='LAV' and tbeatb = ''
216100080609     c/end-exec
216200080609
216300080609     c/exec sql
216400080609     c+ open lav
216500080609     c/end-exec
216600080609
216700080609     c                   do        *hival
216800080609
216900080609     c/exec sql
217000080609     c+ fetch next from lav into :tntbeds
217100080609     c/end-exec
217200080609
217300080609     c                   select
217400080609     c                   when      sqlcod = 100
217500080609     c                   leave
217600080609     c                   when      sqlcod < 0
217700080609     c                   other
217800080609     c                   eval      dlav = tbeuni
217900080609      * carico la schiera della filiali abilitate
218000080616     c                   if        d§lavtar > *date
218100080609     c                   iter
218200080609     c                   endif
218300080609     c                   eval      xx = xx + 1
218400080609     c                   eval      sklav(xx) = %dec(tbeke1:3:0)
218500080609
218600080609     c                   endsl
218700080609
218800080609     c                   enddo
218900080609
219000080609     c/exec sql
219100080609     c+ close lav
219200080609     c/end-exec
219300080609
219400080609     c                   endsr
219500150219      /free
219600150219       //--------------------------------------------------------------
219700150219       //?VMA Negato.
219800150219       //--------------------------------------------------------------
219900150219       BEGSR sr_NoVMA;
220000150219
220100150219         wVMA = *off;
220200150219
220300150219         clear TIBS02DS;
220400150219         clear dVMA;
220500150219         T02mod = 'C';
220600150219         T02cod = 'VMA';
220700150219       //?copia verso tariffa
220800150219         IF  not *in02;
220900150219           T02ke1 = %editc(TA25kscn:'X');
221000150219         ENDIF;
221100150219       //?copia verso offerta di cliente codificato
221200150219         IF  *in02;
221300150219           T02ke1 = %editc(VISksc:'X');
221400150219         ENDIF;
221500150219         T02sif = KNSIF;
221600150219         TNTBE_RicercaControllo  (kpjba : tibs02ds);
221700150219         IF  T02err = *blanks;
221800150219           dVMA = T02uni;
221900150219           IF  TAMddt >= §VMAddt and
222000150219               TAMddt <= §VMAdst;
222100150219             wVMA = *on;
222200150219           ENDIF;
222300150219         ENDIF;
222400150219       //?Aggangio la tariffa particolare fuel
222500150219         kksc = TA25kscn;
222600150219         kctr = TA25ctrn;
222700150219         kftc = 'f ';
222800150219       //?Nuovo progressivo
222900150219         IF  *in03;
223000150219           kprg = %dec(TA25prg:3:0);
223100150219         ENDIF;
223200150219       //?Progressivo già esistente
223300150219         IF  not *in03;
223400150219           kprg = %dec(TA25prgn:3:0);
223500150219         ENDIF;
223600150219       //?copia verso tariffa
223700150219         IF  not *in02;
223800150219           chain (kksc:kctr:kprg:kftc) TITPT01L;
223900150219         ENDIF;
224000150219       //?copia verso offerta di cliente codificato
224100150219         IF  *in02;
224200150219           chain (kksc:kctr:kprg:kftc) TIOPT01L;
224300150219         ENDIF;
224400150219       //?non trovo la tariffa vado via
224500150219         IF  not %found;
224600150219           leavesr;
224700150219         ENDIF;
224800150219         dtpt01 = TPTflo;
224900150219         IF  wVMA;
225000150219           §TPTvma = 'N';
225100150219         ELSE;
225200150219           clear §TPTvma;
225300150219         ENDIF;
225400150219         TPTflo = dtpt01;
225500150219       //?copia verso tariffa
225600150219         IF  not *in02;
225700150219           update  TITPT000;
225800150219         ENDIF;
225900150219       //?copia verso offerta di cliente codificato
226000150219         IF  *in02;
226100150219           update  TIOPT;
226200150219         ENDIF;
226300150219
226400150219       ENDSR;
226500050715
226600050715      *------------------------------------------------------------------------*
226700050715      * ROUTINE INIZIALE
226800050715      *------------------------------------------------------------------------*
226900050715     c     *Inzsr        BegSr
227000050715
227100050715     c     *Entry        Plist
227200050715     c                   Parm                    Kpjba
227300050715     c                   Parm                    Tnta25ds
227400050715     c                   Parm                    Tnta25tad
227500050715     c                   Parm                    Tnta25tgc
227600050715     c                   Parm                    Tnta25tpd
227700050715     c                   Parm                    Tnta25tpt
227800050715
227900050715      * se richiamato da Tnta01r carico gli scaglioni ISTAT
228000050802if  1c                   If        ta25ta01 = '1'
228100101015     c                   Clear                   savrct
228200101015
228300101015      /free
228400101015       //?Carico scatti ISTAT
228500101015         SISsca = 0;
228600101015         clear DIA;
228700101015         setll (SISsca) TISIS01L;
228800101015
228900101015         DOW  not $EoF;
229000101015
229100101015           read TISIS01L;
229200101015
229300101015           IF  %Eof(TISIS01L);
229400101015             $EoF = *on;
229500101015             leave;
229600101015           ENDIF;
229700101015
229800101015           DIA(SISsca) = SISdata;
229900101015
230000101015         ENDDO;
230100101015
230200101015      /end-free
230300101015
230400050715      * verifico se esiste lo scaglione ISTAT in base alla data odierna
230500101015do  2c                   Do        999           pi
230600050715     c                   If        *date <= dia(pi)
230700101015     c                   Eval      savrct = pi
230800100723      * appena trovo la prima data maggiore o uguale ad oggi esco dal do
230900100723     c                   leave
231000050715     c                   EndIf
231100050715    2c                   EndDo
231200050715      * se non trovo lo scaglione errore al tnta01r
231300101015     c                   If        savrct = *Zeros
231400050715     c                   Move      'IST'         kpjbu
231500050715     c                   Goto      Fine
231600050715     c                   EndIf
231700050715    1c                   EndIf
231800050729
231900050729      * Aggancio la tabella 2G per recuperare i giorni massimi
232000050729      * di rientro
232100050729     c                   Clear                   ds2g
232200050729     c                   Eval      kcod = '2G'
232300050729     c                   Eval      kkey = '1'
232400050803     c     kTabel01      Chain     Tabel00f
232500050729     c                   If        %Found(Tabel00f) and tblflg = *Blanks
232600050729     c                   Eval      ds2g = tbluni
232700050729     c                   Else
232800050729     c                   z-add     99            §2ggre
232900050729     c                   z-add     99            §2ggdp
233000050729     c                   z-add     99            §2ggfe
233100050729     c                   EndIf
233200050715
233300050715      * se copia di tutto il dettaglio tariffario la ds relativa la pulisco
233400050715     c                   If        ta25tad = 'T'
233500050715     c                   Clear                   Tnta25tad
233600050715     c                   EndIf
233700050715
233800050715      * se copia di tutte le tariffe particolari la ds relativa la pulisco
233900050715     c                   If        ta25tpt = 'T'
234000050715     c                   Clear                   Tnta25tpt
234100050715     c                   Clear                   Tnta25tpd
234200050715     c                   EndIf
234300050715
234400050715      * se copia delle tariffe di giacenza senza vis. la ds relativa la pulisco
234500050715     c                   If        ta25tgc = 'T'
234600050715     c                   Clear                   Tnta25tgc
234700050715     c                   EndIf
234800050715
234900050715      * se copia verso tariffa apro TITAV
235000050715     c                   If        ta25tipn = 'T'
235100050715     c                   Open      Titav01l
235200050715     c                   EndIf
235300050715
235400050715      * Copio da OFFERTA
235500050715     c                   Eval      *In01 = (ta25tipo = 'O')
235600050715      * Copio  a OFFERTA
235700050715     c                   Eval      *In02 = (ta25tipn = 'O')
235800050715      * Copio verso un nuovo progressivo
235900050715     c                   Eval      *In03 = (ta25prgn = *Blanks)
236000050715      * Copia da tariffa di cartello
236100050715     c                   If        Not *In01
236200050715     c                   Movel     ta25ksco      w005a
236300050715     c                   Eval      *In04 = (w005a = '88888')
236400050715     c                   EndIf
236500050715      * Copia  a tariffa di cartello
236600050715     c                   If        Not *In02
236700050715     c                   Movel     ta25kscn      w005a
236800050715     c                   Eval      *In05 = (w005a = '88888')
236900050715     c                   EndIf
237000050715      * sk dei codici di tassazione da includere o da omettere
237100050715     c                   Movea     skcts         wcts
237200050715     c                   If        ta25io2 = 'O' and wcts <> *Blanks
237300050715     c                   Eval      wscts = *On
237400050715     c                   EndIf
237500050715      * scaglione da includere o da omettere
237600050715     c                   If        ta25io3 = 'O' and ta25sgl > *Zeros
237700050715     c                   Eval      wssgl = *On
237800050715     c                   EndIf
237900061012      * se duplica dettaglio tariffario controllo se c'è il codice di tassazione
238000061012     c                   if        ta25dupd = 'S'
238100061012     c                   eval      *in07 = (ta25octs <> *blanks)
238200061012     c                   endif
238300061012      * se c'è il codice di tassazione cerco il relativo codice di ordinamento
238400061012     c                   if        *in07
238500061012      * prima il vecchio codice di tassazione
238600061012     c                   clear                   dsct
238700061012     c                   clear                   worpo
238800061012     c                   eval      kcod = 'CT'
238900061012     c                   eval      kkey = ta25octs
239000061012     c     ktabel01      chain     tabel00f
239100061012     c                   if        %found(tabel00f) and tblflg = *blanks
239200061012     c                   eval      dsct = tbluni
239300061012     c                   endif
239400061012     c                   move      §ctcor        worpo
239500061012      * prima il vecchio codice di tassazione
239600061012     c                   clear                   dsct
239700061012     c                   clear                   worpn
239800061012     c                   eval      kcod = 'CT'
239900061012     c                   eval      kkey = ta25ncts
240000061012     c     ktabel01      chain     tabel00f
240100061012     c                   if        %found(tabel00f) and tblflg = *blanks
240200061012     c                   eval      dsct = tbluni
240300061012     c                   endif
240400061012     c                   move      §ctcor        worpn
240500061012     c                   endif
240600050715
240700050715     c     *dtaara       define    §azute        azuteds
240800050715     c     *dtaara       define    §datiute      ddatiute
240900050715     c                   in(E)     *dtaara
241000050715     c                   If        %error  or RSUT = *blanks
241100050715     c                   Clear                   Tibs34ds
241200050715     c                   Call      'TIBS34R'
241300050715     c                   Parm                    Tibs34ds
241400050715     c                   In        *dtaara
241500050715     c                   EndIf
241600050715
241700050715      * Controllo se sono in sede
241800050715     c                   Eval      *In12 = (simfel = *zeros)
241900050715      * se non sono in sede apro i file delle offerte
242000050715     c                   If        Not *In12
242100050715     c                   Open      Tnofm01l
242200050715     c                   Open      Tiofd01l
242300050715     c                   Open      Tiopt01l
242400050715     c                   Open      Tiopd01l
242500050715     c                   Open      Tiogc01l
242600101222     c                   Open      Tivis05l
242700101222     c                   Open      Tivof01l
242800050715     c                   EndIf
242900050715
243000050801     c                   Clear                   comlnp
243100050801
243200051019      * se non sto copiando verso una tariffa di cartello
243300051019      * cerco la lnp del cliente
243400051019if  1c                   If        Not *In05
243500050715      * copia verso tariffa
243600050715     c  n02              Movel     ta25kscn      comlnp
243700050715      * copia verso offerta
243800050715if  2c                   If        *In02
243900101222     c     ta25kscn      Chain     Tivis05l
244000090916if  3c                   If        %Found
244100050715      *         offerta di cliente codificato
244200050715if  4c                   If        visksc > *Zeros
244300050715     c                   Movel     visksc        comlnp
244400050715      *         offerta di cliente non codificato
244500050715   x4c                   Else
244600050715     c                   Movel     viscmm        comlnp
244700050715    4c                   EndIf
244800050715    3c                   EndIf
244900050715    2c                   EndIf
245000050715    1c                   EndIf
245100050715
245200050715      * KLIST
245300050803     c     kTabel        klist
245400050805     c                   kfld                    kkut
245500050805     c                   kfld                    kcod1
245600050805
245700050805     c     kTabel1       klist
245800050805     c                   kfld                    kkut
245900050805     c                   kfld                    kcod1
246000050805     c                   kfld                    kkey1
246100050803
246200050803     c     kTabel01      klist
246300050715     c                   kfld                    kkut
246400050715     c                   kfld                    kcod
246500050715     c                   kfld                    kkey
246600050715
246700050715     c     kTntam        klist
246800050715     c                   kfld                    kksc
246900050715     c                   kfld                    kctr
247000050715
247100050715     c     kTntam01      klist
247200050715     c                   kfld                    kksc
247300050715     c                   kfld                    kctr
247400050715     c                   kfld                    kprg
247500050805
247600050805     c     kTitad        klist
247700050805     c                   kfld                    kksc
247800050805     c                   kfld                    kctr
247900050805     c                   kfld                    kprg
248000050805     c                   kfld                    klnp
248100061012
248200061012     c     kTitad01      klist
248300061012     c                   kfld                    kksc
248400061012     c                   kfld                    kctr
248500061012     c                   kfld                    kprg
248600061012     c                   kfld                    klnp
248700061012     c                   kfld                    korp
248800050715
248900050805     c     kTitad04      klist
249000050715     c                   kfld                    kksc
249100050715     c                   kfld                    kctr
249200050715     c                   kfld                    kprg
249300050715     c                   kfld                    klnp
249400050715     c                   kfld                    korp
249500050715     c                   kfld                    knaz
249600050715     c                   kfld                    kcap
249700050715     c                   kfld                    ksgl
249800050725
249900050728     c     kTitpt        klist
250000050725     c                   kfld                    kksc
250100050725     c                   kfld                    kctr
250200050725     c                   kfld                    kprg
250300050725     c                   kfld                    kftc
250400050728
250500050728     c     kTitpd        klist
250600050728     c                   kfld                    kksc
250700050728     c                   kfld                    kctr
250800050728     c                   kfld                    kprg
250900050728     c                   kfld                    kftc
251000050728     c                   kfld                    korp
251100050728     c                   kfld                    ksgl
251200050715
251300050715     c     kTitav        klist
251400050715     c                   kfld                    tavksc
251500050715     c                   kfld                    tavctr
251600050715     c                   kfld                    tavprg
251700050715     c                   kfld                    tavtrd
251800050715     c                   kfld                    tavftc
251900050715     c                   kfld                    tavcta
252000050715     c                   kfld                    tavdav
252100050715
252200050715     c                   EndSr
252300050715      *------------------------------------------------------------------------*
252400050705
252500050705** MSG  Lungh. 78                                                            *
252600050713Nuova Tariffa/Offerta già presente                                            01
252700050713Tariffa/Offerta da cui copiare non trovata                                    02
252800050714Dettaglio tariffario già presente                                             03
252900050725Tariffe particolari già presenti                                              04
253000050727Dettaglio della tariffa particolare    già presente                           05
253100050729Tariffe di giacenza già presenti                                              06
253200061012ATTENZIONE !! Non sono state duplicate le tariffe con il C.A.P. inserito      07
