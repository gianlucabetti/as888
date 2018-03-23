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
012400050802     d Trul27ds      e ds
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
032600050713     c                   Eval      tamduv = *date
032700050713     c                   Eval      tamdtr = *date
032800050713     c                   Clear                   tamftr
032900050713      * scrivo la nuova testata
033000050713     c  n02              Write     Tntam000
033100050713     c   02              Write     Tnofm
033200050714
033300050714      * imposto il nuovo progressivo creato da ritornare al chiamante
033400050714     c                   Move      tamprg        ta25prg
033500090916
033600090916      * scrivo il TIVOF in caso di creazione nuova offerta
033700101222     c                   if        *in02
033800090916     c                   clear                   tivof000
033900090916     c                   eval      vofnrv = ta25kscn
034000090916     c                   move      ta25ctrn      vofctr
034100090916     c                   eval      vofprg = %dec(wnewprg:1:0)
034200090916     c                   select
034300090916     c                   when      §tadpd = 'S'
034400090916     c                   eval      voftpt = 'D'
034500090916     c                   when      §tafed = 'S'
034600090916     c                   eval      voftpt = 'F'
034700090916     c                   other
034800090916     c                   eval      voftpt = tamfie
034900090916     c                   endsl
035000100120      * valorizzo data presentazione offerta con la data del giorno
035100100120     c                   eval      vofdpo = *date
035200090916     c                   write     tivof000
035300090916     c                   endif
035400050714
035500050715      * --> SCRIVO TITAV se copia verso tariffa
035600050714if  1c                   If        Not *In02
035700050713     c                   Clear                   Titav000
035800050715     c                   Eval      tavksc = tamksc
035900050715     c                   Eval      tavctr = tamctr
036000050715     c                   Eval      tavprg = tamprg
036100050715     c                   Eval      tavtrd = 'TES'
036200050715     c                   Eval      tavcta = 'D'
036300050715     c                   Eval      tavdav = *date
036400050713     c                   Time                    w0140
036500050715     c                   Movel     w0140         tavorv
036600050715     c                   Eval      tavnoj = job_name
036700050715     c                   Eval      tavpru = user
036800050715     c                   Eval      tavdtr = *date
036900050713     c                   Write     Titav000
037000050714    1c                   EndIf
037100050713
037200050712     c                   EndSr
037300050713
037400050713      *------------------------------------------------------------------------*
037500050713      * CREO TITAD
037600050713      *------------------------------------------------------------------------*
037700050713     c     Sr_Titad      BegSr
037800050714
037900050715      * --> COPIA A DETTAGLIO
038000050714if  1c                   If        ta25tad = 'D'
038100050715      * controllo che non esista già un record uguale
038200050714     c                   Eval      kksc = ta25kscn
038300050714     c                   Eval      kctr = ta25ctrn
038400050802     c  n03              Move      ta25prgn      kprg
038500050802     c   03              Move      ta25prg       kprg
038600050920     c                   Eval      klnp = ta25lnprn
038700050714     c                   Eval      korp = ta25orpr
038800050714     c                   Eval      knaz = ta25nazr
038900050714     c                   Eval      kcap = ta25capr
039000050714     c                   Eval      ksgl = ta25sglr
039100050805     c  n02kTitad04      Chain     Titad04l
039200050805     c   02kTitad04      Chain     Tiofd01l
039300050714if  2c                   If        %Found
039400050714      * esiste e non è annullata esco dal pgm con msg di errore
039500050729     c                   If        tadatb = *Blanks
039600050714     c                   Eval      ta25err = '1'
039700050714     c                   Eval      ta25msg = msg(03)
039800050715     c                   Eval      ta25tad = 'N'
039900050715     c                   Eval      ta25tpt = 'N'
040000050715     c                   Eval      ta25tgc = 'N'
040100050715     c                   Eval      ta25cat = 'N'
040200050714     c                   Goto      Fine
040300050714     c                   EndIf
040400050714      * esiste ma è annullata la cancello
040500050714     c  n02              Delete    titad000
040600050714     c   02              Delete    tiofd
040700050714    2c                   EndIf
040800050715
040900050715      * scrivo il nuovo dettaglio in base a quanto passato nella tnta25tad
041000050714     c                   Clear                   tadatb
041100050714     c                   Eval      tadksc = ta25kscn
041200050714     c                   Eval      tadctr = ta25ctrn
041300050802     c  n03              Move      ta25prgn      tadprg
041400050802     c   03              Move      ta25prg       tadprg
041500050920     c                   Eval      tadlnp = ta25lnprn
041600050714     c                   Eval      tadcts = ta25ctsr
041700050714     c                   Eval      tadnaz = ta25nazr
041800050714     c                   Eval      tadcap = ta25capr
041900050714     c                   Eval      tadsgl = ta25sglr
042000050714     c                   Eval      taditr = ta25itrr
042100050714     c                   Eval      tadino = ta25inor
042200050714     c                   Eval      tadrpv = ta25rpvr
042300050714     c                   Eval      tadorp = ta25orpr
042400050714     c                   Eval      tadmin = ta25minr
042500050714     c                   Eval      tadmax = ta25maxr
042600050714     c                   Clear                   tadftr
042700050714     c                   Eval      taddtr = *date
042800050714      * prima della write eseguo gli arrotondamenti
042900050803     c                   If        ta25sad = 'S'
043000050714     c                   ExSr      Sr_Arrtad
043100050803     c                   EndIf
043200050714     c  n02              Write     titad000
043300050714     c   02              Write     tiofd
043400050804
043500050715      * --> SCRIVO TITAV se copia verso tariffa
043600050804      *                  e se non è già stato scritto in un richiamo precedente
043700050912if  2c                   If        Not *In02
043800050715     c                   Eval      wtav = *Off
043900050715     c                   Eval      tavksc = tamksc
044000050715     c                   Eval      tavctr = tamctr
044100050715     c                   Eval      tavprg = tamprg
044200050715     c                   Eval      tavtrd = 'DET'
044300050715     c                   Eval      tavcta = 'D'
044400050715     c                   Eval      tavdav = *date
044500050715      *            Controllo se non è già stato scritto in precedenza
044600050715     c     kTitav        Setll     Titav01l
044700050715do  3c                   Do        *Hival
044800050715     c     kTitav        Reade     Titav01l
044900050715     c                   If        %Eof(Titav01l)
045000050715     c                   Leave
045100050715     c                   EndIf
045200050715if  4c                   If        tavnoj = job_name and tavpru = user
045300050715     c                   Eval      wtav = *On
045400050715     c                   Leave
045500050715    4c                   EndIf
045600050715    3c                   EndDo
045700050715      *            Non è stato scritto scrivo ora
045800050715if  3c                   If        wtav = *Off
045900050715     c                   Clear                   Titav000
046000050715     c                   Eval      tavksc = tamksc
046100050715     c                   Eval      tavctr = tamctr
046200050715     c                   Eval      tavprg = tamprg
046300050715     c                   Eval      tavtrd = 'DET'
046400050715     c                   Eval      tavcta = 'D'
046500050715     c                   Eval      tavdav = *date
046600050715     c                   Time                    w0140
046700050715     c                   Movel     w0140         tavorv
046800050715     c                   Eval      tavnoj = job_name
046900050715     c                   Eval      tavpru = user
047000050715     c                   Eval      tavdtr = *date
047100050715     c                   Write     Titav000
047200050715    3c                   EndIf
047300050715    2c                   EndIf
047400050714    1c                   EndIf
047500050714
047600050715      * --> COPIA TOTALE
047700050715if  1c                   If        ta25tad = 'T'
047800050715      * controllo che non esista già
047900050715     c                   Eval      kksc = ta25kscn
048000050715     c                   Eval      kctr = ta25ctrn
048100050802     c  n03              Move      ta25prgn      kprg
048200050802     c   03              Move      ta25prg       kprg
048300050921      * Per aggiunta nuovo progressivo da tnta01r leggo tutta la tariffa
048400050922      * o se sto copiando da cartello a cartello
048500061012if  2c                   If        ta25ta01 = '1' or
048600061012     c                             (*In04 and *In05)
048700050921     c     kTntam01      Setll     Titad04l
048800050921do  3c                   Do        *Hival
048900050921     c     kTntam01      Reade     Titad04l
049000050921     c                   If        %Eof
049100050921     c                   Leave
049200050921     c                   EndIf
049300050921      * esiste e non è annullata esco dal pgm con msg di errore
049400050921if  4c                   If        tadatb = *Blanks
049500050921     c                   Eval      ta25err = '1'
049600050921     c                   Eval      ta25msg = msg(03)
049700050921     c                   Eval      ta25tad = 'N'
049800050921     c                   Eval      ta25tpt = 'N'
049900050921     c                   Eval      ta25tgc = 'N'
050000050921     c                   Eval      ta25cat = 'N'
050100050921     c                   Goto      Fine
050200050921    4c                   EndIf
050300050921      * esiste ma è annullata la cancello
050400050921     c                   Delete    titad000
050500050921    3c                   EndDo
050600050921   x2c                   Else
050700050920      * Ciclo per le linee di partenza
050800050921do  3c                   Do        50            xx
050900050920     c                   If        sklnpn(xx) = *Zeros
051000050920     c                   Leave
051100050920     c                   EndIf
051200050920     c                   Eval      klnp = sklnpn(xx)
051300050805     c  n02kTitad        Setll     Titad04l
051400050805     c   02kTitad        Setll     Tiofd01l
051500050921do  4c                   Do        *Hival
051600050805     c  n02kTitad        Reade     Titad04l
051700050805     c   02kTitad        Reade     Tiofd01l
051800050920     c                   If        %Eof
051900050715     c                   Leave
052000050715     c                   EndIf
052100050715      * Controllo le inclusioni/omissioni
052200050728     c                   ExSr      Sr_Seltad
052300050921if  5c                   If        wokrec = *On
052400050715     c                   Iter
052500050921    5c                   EndIf
052600050715      * esiste e non è annullata esco dal pgm con msg di errore
052700050921if  5c                   If        tadatb = *Blanks
052800050715     c                   Eval      ta25err = '1'
052900050715     c                   Eval      ta25msg = msg(03)
053000050715     c                   Eval      ta25tad = 'N'
053100050715     c                   Eval      ta25tpt = 'N'
053200050715     c                   Eval      ta25tgc = 'N'
053300050715     c                   Eval      ta25cat = 'N'
053400050715     c                   Goto      Fine
053500050921    5c                   EndIf
053600050715      * esiste ma è annullata la cancello
053700050715     c  n02              Delete    titad000
053800050715     c   02              Delete    tiofd
053900050921    4c                   EndDo
054000050921    3c                   EndDo
054100050921    2c                   EndIf
054200050715
054300050715      * Aggancio il dettaglio della tariffa da cui copiare
054400050715     c                   Eval      kksc = ta25ksco
054500050715     c                   Move      ta25ctro      kctr
054600050715     c                   Move      ta25prgo      kprg
054700050921      * Per aggiunta nuovo progressivo da tnta01r leggo tutta la tariffa
054800050922      * o se sto copiando da cartello a cartello
054900050922if  2c                   If        ta25ta01 = '1' or
055000050922     c                             (*In04 and *In05)
055100050921     c     kTntam01      Setll     Titad04l
055200050921do  3c                   Do        *Hival
055300051125     c     kTntam01      Reade(n)  Titad04l
055400050921     c                   If        %Eof
055500050921     c                   Leave
055600050921     c                   EndIf
055700050921      * escludo annullati
055800050921     c                   If        tadatb <> *Blanks
055900050921     c                   Iter
056000050921     c                   EndIf
056100050921      * scrivo il nuovo dettaglio
056200050921     c                   Eval      tadksc = ta25kscn
056300050921     c                   Eval      tadctr = ta25ctrn
056400050921     c  n03              Move      ta25prgn      tadprg
056500050921     c   03              Move      ta25prg       tadprg
056600050921     c                   Clear                   tadftr
056700050921     c                   Eval      taddtr = *date
056800050921     c                   Write     titad000
056900050921    3c                   EndDo
057000050921   x2c                   Else
057100050920      * Ciclo per le linee di partenza
057200050921do  3c                   Do        50            xx
057300050920     c                   If        sklnpo(xx) = *Zeros
057400050920     c                   Leave
057500050920     c                   EndIf
057600050920     c                   Eval      klnp = sklnpo(xx)
057700050921     c  n01kTitad        Setll     Titad04l
057800050921     c   01kTitad        Setll     Tiofd01l
057900050921do  4c                   Do        *Hival
058000051125     c  n01kTitad        Reade(n)  Titad04l
058100051125     c   01kTitad        Reade(n)  Tiofd01l
058200050715     c                   If        %Eof
058300050715     c                   Leave
058400050715     c                   EndIf
058500050715      * escludo annullati
058600050715     c                   If        tadatb <> *Blanks
058700050715     c                   Iter
058800050715     c                   EndIf
058900050715      * Controllo le inclusioni/omissioni
059000050728     c                   ExSr      Sr_Seltad
059100050921if  5c                   If        wokrec = *On
059200050715     c                   Iter
059300050921    5c                   EndIf
059400050715      * scrivo il nuovo dettaglio
059500050715     c                   Eval      tadksc = ta25kscn
059600050715     c                   Eval      tadctr = ta25ctrn
059700050802     c  n03              Move      ta25prgn      tadprg
059800050802     c   03              Move      ta25prg       tadprg
059900050920     c                   Move      sklnpn(xx)    tadlnp
060000050715     c                   Clear                   tadftr
060100050715     c                   Eval      taddtr = *date
060200050715      * prima della write eseguo gli arrotondamenti
060300050803     c                   If        ta25sad = 'S'
060400050715     c                   ExSr      Sr_Arrtad
060500050803     c                   EndIf
060600050715     c  n02              Write     titad000
060700050715     c   02              Write     tiofd
060800050921    4c                   EndDo
060900050921    3c                   EndDo
061000050921    2c                   EndIf
061100050804
061200050715      * --> SCRIVO TITAV se copia verso tariffa
061300050912if  2c                   If        Not *In02
061400050715     c                   Clear                   Titav000
061500050715     c                   Eval      tavksc = tamksc
061600050715     c                   Eval      tavctr = tamctr
061700050715     c                   Eval      tavprg = tamprg
061800050715     c                   Eval      tavtrd = 'DET'
061900050715     c                   Eval      tavcta = 'D'
062000050715     c                   Eval      tavdav = *date
062100050715     c                   Time                    w0140
062200050715     c                   Movel     w0140         tavorv
062300050715     c                   Eval      tavnoj = job_name
062400050715     c                   Eval      tavpru = user
062500050715     c                   Eval      tavdtr = *date
062600050715     c                   Write     Titav000
062700050715    2c                   EndIf
062800050715    1c                   EndIf
062900050713
063000050713     c                   EndSr
063100050725
063200050725      *------------------------------------------------------------------------*
063300050725      * CREO TITPT
063400050725      *------------------------------------------------------------------------*
063500050725     c     Sr_Titpt      BegSr
063600050725
063700050725      * --> COPIA A DETTAGLIO
063800050725if  1c                   If        ta25tpt = 'D'
063900050725      * controllo che non esista già un record uguale
064000050725     c                   Eval      kksc = ta25kscn
064100050725     c                   Eval      kctr = ta25ctrn
064200050802     c  n03              Move      ta25prgn      kprg
064300050802     c   03              Move      ta25prg       kprg
064400050727     c                   Eval      kftc = ta25ftcr
064500050727     c  n02kTitpt        Chain     Titpt01l
064600050727     c   02kTitpt        Chain     Tiopt01l
064700050725if  2c                   If        %Found
064800050725      * esiste e non è annullata esco dal pgm con msg di errore
064900050802      * se non sto inglobando
065000050803if  3c                   If        tptatb = *Blanks and
065100080521     c                             ta25inglo = 'N'
065200050725     c                   Eval      ta25err = '1'
065300050727     c                   Eval      ta25msg = msg(04)
065400050725     c                   Eval      ta25tpt = 'N'
065500050725     c                   Eval      ta25tgc = 'N'
065600050725     c                   Eval      ta25cat = 'N'
065700050725     c                   Goto      Fine
065800050803    3c                   EndIf
065900050803      * esiste e non è annullata e sto inglobando esco dalla routine
066000050803if  3c                   If        tptatb = *Blanks and
066100080521     c                             ta25inglo <> 'N'
066200050803     c                   Leavesr
066300050803    3c                   EndIf
066400050725      * esiste ma è annullata la cancello
066500050802     c                   If        tptatb <> *Blanks
066600050727     c  n02              Delete    titpt000
066700050727     c   02              Delete    tiopt
066800050802     c                   EndIf
066900050725    2c                   EndIf
067000050725
067100050727      * prima di scrivere la testata della tariffa particolare vado a scrivere
067200050727      * TUTTO il dettaglio della tariffa particolare
067300050805      * se non ho chiesta la copia del dettaglio delle tariffe particolari
067400050922      * o se sto inglobando
067500050816     c                   If        ta25tpd = 'N' or
067600050805     c                             ta25inglo = 'S'
067700050727     c                   Eval      ta25tpd = 'T'
067800050727     c                   Eval      wtpg = ta25tpgr
067900050817     c                   Eval      wftc = ta25ftcr
068000050727     c                   ExSr      Sr_Titpd
068100050803     c                   Eval      ta25tpd = 'N'
068200050805     c                   EndIf
068300050727
068400050727      * scrivo la tariffa particolare in base a quanto passato nella tnta25tpt
068500050727     c                   Clear                   tptatb
068600050727     c                   Eval      tptksc = ta25kscn
068700050727     c                   Eval      tptctr = ta25ctrn
068800050802     c  n03              Move      ta25prgn      tptprg
068900050802     c   03              Move      ta25prg       tptprg
069000050727     c                   Eval      tptduv = *date
069100050727     c                   Eval      tptftc = ta25ftcr
069200050727     c                   Eval      tpttpg = ta25tpgr
069300050727     c                   Eval      tptarl = ta25arlr
069400050727     c                   Eval      tptarf = ta25arfr
069500050727     c                   Eval      tptaro = ta25aror
069600050727     c                   Eval      tptrpv = ta25rpvrt
069700050727     c                   Eval      tptvlm = ta25vlmr
069800050727     c                   Eval      tptvvm = ta25vvmr
069900050727     c                   Eval      tptfvm = ta25fvmr
070000050727     c                   Eval      tpttma = ta25tmar
070100050727     c                   Eval      tptapl = ta25aplr
070200060802     c                   eval      tptdpb = ta25dpbr
070300060803      * la data inserimento devo aggiornarla con la data del giorno
070400060803     c                   eval      tptdit = *date
070500060802     c                   eval      tptflo = ta25flor
070600050727     c                   Clear                   tptftr
070700050727     c                   Eval      tptdtr = *date
070800050727      * se tariffa a valore (tipo 4-T-V) arrotondo
070900050727     c                   Eval      w001a = tpttpg
071000050727     c                   ExSr      Sr_Chkctr
071100050803if  2c                   If        §trtap = 'S' and ta25sad = 'S'
071200050727     c                   Z-add     tptvlm        w0153
071300050727     c                   ExSr      Sr_Arroto
071400050727     c                   Z-add     w0153         tptvlm
071500050727    2c                   EndIf
071600050727     c  n02              Write     titpt000
071700050727     c   02              Write     tiopt
071800050804
071900050725      * --> SCRIVO TITAV se copia verso tariffa
072000050912if  2c                   If        Not *In02
072100050727     c                   Clear                   Titav000
072200050725     c                   Eval      tavksc = tamksc
072300050725     c                   Eval      tavctr = tamctr
072400050725     c                   Eval      tavprg = tamprg
072500050727     c                   Eval      tavtrd = 'PAR'
072600050725     c                   Eval      tavcta = 'D'
072700050727     c                   Eval      tavftc = tptftc
072800050725     c                   Eval      tavdav = *date
072900050725     c                   Time                    w0140
073000050725     c                   Movel     w0140         tavorv
073100050725     c                   Eval      tavnoj = job_name
073200050725     c                   Eval      tavpru = user
073300110913      * se sto inglobando metto 'I'
073400110913     c                   If        ta25inglo <> 'N'
073500110913     c                   Eval      tavcta = 'I'
073600110913     c                   EndIf
073700050725     c                   Eval      tavdtr = *date
073800050725     c                   Write     Titav000
073900050725    2c                   EndIf
074000050725    1c                   EndIf
074100050725
074200050725      * --> COPIA TOTALE
074300050725if  1c                   If        ta25tpt = 'T'
074400050725      * controllo che non esista già
074500050725     c                   Eval      kksc = ta25kscn
074600050725     c                   Eval      kctr = ta25ctrn
074700050802     c  n03              Move      ta25prgn      kprg
074800050802     c   03              Move      ta25prg       kprg
074900050725     c  n02kTntam01      Setll     Titpt01l
075000050725     c   02kTntam01      Setll     Tiopt01l
075100050725do  2c                   Do        *Hival
075200050725     c  n02kTntam01      Reade     Titpt01l
075300050725     c   02kTntam01      Reade     Tiopt01l
075400050725if  3c                   If        %Eof
075500050725     c                   Leave
075600050725    3c                   EndIf
075700050725      * controllo se la tariffa particolare è gestibile o meno
075800050725     c                   Eval      w002a = tptftc
075900050725     c                   ExSr      Sr_Chktpt
076000050725if  3c                   If        wokrec = *On
076100050725     c                   Iter
076200050725    3c                   EndIf
076300050921      * se non è aggiunta tariffa da tnta01r
076400050922      * o se non è copia da cartello a cartello
076500050921      * controllo se è una delle tariffe particolari da copiare
076600050922if  3c                   If        ta25ta01 <> '1' and Not *In04 and
076700050922     c                             Not *In05
076800050921     c     tptftc        Lookup    sktpt                                  30
076900050921      * non è presente nella sk torno a leggere
077000050921     c  n30              Iter
077100050921    3c                   EndIf
077200050725      * esiste e non è annullata esco dal pgm con msg di errore
077300050729if  3c                   If        tptatb = *Blanks
077400050725     c                   Eval      ta25err = '1'
077500050725     c                   Eval      ta25msg = msg(04)
077600050725     c                   Eval      ta25tpt = 'N'
077700050725     c                   Eval      ta25tgc = 'N'
077800050725     c                   Eval      ta25cat = 'N'
077900050725     c                   Goto      Fine
078000050725    3c                   EndIf
078100050725      * esiste ma è annullata la cancello
078200050725     c  n02              Delete    titpt000
078300050725     c   02              Delete    tiopt
078400050725    2c                   EndDo
078500050725
078600050725      * Aggancio le tariffe particolari da cui copiare
078700050725     c                   Eval      kksc = ta25ksco
078800050725     c                   Move      ta25ctro      kctr
078900050725     c                   Move      ta25prgo      kprg
079000050725     c  n01kTntam01      Setll     Titpt01l
079100050725     c   01kTntam01      Setll     Tiopt01l
079200050725do  2c                   Do        *Hival
079300051125     c  n01kTntam01      Reade(n)  Titpt01l
079400051125     c   01kTntam01      Reade(n)  Tiopt01l
079500050725     c                   If        %Eof
079600050725     c                   Leave
079700050725     c                   EndIf
079800050725      * escludo annullati
079900050725     c                   If        tptatb <> *Blanks
080000050725     c                   Iter
080100050725     c                   EndIf
080200050725      * controllo se la tariffa particolare è gestibile o meno
080300050725     c                   Eval      w002a = tptftc
080400050725     c                   ExSr      Sr_Chktpt
080500050725if  3c                   If        wokrec = *On
080600050725     c                   Iter
080700050725    3c                   EndIf
080800050921      * se non è aggiunta tariffa da tnta01r
080900050921      * controllo se è una delle tariffe particolari da copiare
081000050921if  3c                   If        ta25ta01 <> '1'
081100050921     c     tptftc        Lookup    sktpt                                  30
081200050921      * non è presente nella sk torno a leggere
081300050921     c  n30              Iter
081400050921    3c                   EndIf
081500050727
081600050727      * prima di scrivere la testata della tariffa particolare vado a scrivere
081700050727      * TUTTO il dettaglio della tariffa particolare
081800050727     c                   Eval      ta25tpd = 'T'
081900050803     c                   Eval      wtpg = tpttpg
082000050817     c                   Eval      wftc = tptftc
082100050727     c                   ExSr      Sr_Titpd
082200050803     c                   Eval      ta25tpd = 'N'
082300050725
082400050727      * scrivo la nuova tariffa particolare
082500050727     c                   Eval      tptksc = ta25kscn
082600050727     c                   Eval      tptctr = ta25ctrn
082700050802     c  n03              Move      ta25prgn      tptprg
082800050802     c   03              Move      ta25prg       tptprg
082900050727     c                   Eval      tptduv = *date
083000061130     c                   eval      tptdit = *date
083100050727     c                   Clear                   tptftr
083200050727     c                   Eval      tptdtr = *date
083300050727      * se tariffa a valore (tipo 4-T-V) arrotondo
083400050727     c                   Eval      w001a = tpttpg
083500050727     c                   ExSr      Sr_Chkctr
083600050803if  3c                   If        §trtap = 'S' and ta25sad = 'S'
083700050727     c                   Z-add     tptvlm        w0153
083800050727     c                   ExSr      Sr_Arroto
083900050727     c                   Z-add     w0153         tptvlm
084000050727    3c                   EndIf
084100050803     c  n02              Write     titpt000
084200050803     c   02              Write     tiopt
084300050804
084400050725      * --> SCRIVO TITAV se copia verso tariffa
084500050912if  3c                   If        Not *In02
084600050725     c                   Clear                   Titav000
084700050725     c                   Eval      tavksc = tamksc
084800050725     c                   Eval      tavctr = tamctr
084900050725     c                   Eval      tavprg = tamprg
085000050727     c                   Eval      tavtrd = 'PAR'
085100050725     c                   Eval      tavcta = 'D'
085200050727     c                   Eval      tavftc = tptftc
085300050725     c                   Eval      tavdav = *date
085400050725     c                   Time                    w0140
085500050725     c                   Movel     w0140         tavorv
085600050725     c                   Eval      tavnoj = job_name
085700050725     c                   Eval      tavpru = user
085800050725     c                   Eval      tavdtr = *date
085900050725     c                   Write     Titav000
086000050804    3c                   EndIf
086100050804    2c                   EndDo
086200050725    1c                   EndIf
086300050725
086400050725     c                   EndSr
086500050727
086600050727      *------------------------------------------------------------------------*
086700050727      * CREO TITPD
086800050727      *------------------------------------------------------------------------*
086900050727     c     Sr_Titpd      BegSr
087000050727
087100050727      * --> COPIA A DETTAGLIO
087200050727if  1c                   If        ta25tpd = 'D'
087300050727      * controllo che non esista già un record uguale
087400050727     c                   Eval      kksc = ta25kscn
087500050727     c                   Eval      kctr = ta25ctrn
087600050802     c  n03              Move      ta25prgn      kprg
087700050802     c   03              Move      ta25prg       kprg
087800050728     c                   Eval      kftc = ta25ftcrd
087900050728     c                   Eval      korp = ta25orprd
088000050728     c                   Eval      ksgl = ta25sglrd
088100050728     c  n02kTitpd        Chain     Titpd01l
088200050728     c   02kTitpd        Chain     Tiopd01l
088300050727if  2c                   If        %Found
088400050727      * esiste e non è annullata esco dal pgm con msg di errore
088500050802     c                   If        tpdatb = *Blanks
088600050727     c                   Eval      ta25err = '1'
088700050728     c                   Eval      ta25msg = msg(05)
088800050728     c                   Eval      %subst(ta25msg:37:2) = ta25ftcrd
088900050727     c                   Eval      ta25tpt = 'N'
089000050727     c                   Eval      ta25tgc = 'N'
089100050727     c                   Eval      ta25cat = 'N'
089200050727     c                   Goto      Fine
089300050727     c                   EndIf
089400050727      * esiste ma è annullata la cancello
089500050728     c  n02              Delete    titpd000
089600050728     c   02              Delete    tiopd
089700050727    2c                   EndIf
089800050727
089900050728      * scrivo il nuovo dettaglio in base a quanto passato nella tnta25tpd
090000050728     c                   Clear                   tpdatb
090100050728     c                   Eval      tpdksc = ta25kscn
090200050728     c                   Eval      tpdctr = ta25ctrn
090300050802     c  n03              Move      ta25prgn      tpdprg
090400050802     c   03              Move      ta25prg       tpdprg
090500050728     c                   Eval      tpdftc = ta25ftcrd
090600050728     c                   Eval      tpdcts = ta25ctsrd
090700050728     c                   Eval      tpdsgl = ta25sglrd
090800050728     c                   Eval      tpditr = ta25itrrd
090900050728     c                   Eval      tpdorp = ta25orprd
091000050728     c                   Eval      tpdmin = ta25minrd
091100050728     c                   Eval      tpdmax = ta25maxrd
091200050728     c                   Eval      tpdain = ta25ainr
091300050728     c                   Clear                   tpdftr
091400050728     c                   Eval      tpddtr = *date
091500050727      * prima della write eseguo gli arrotondamenti
091600050803     c                   If        ta25sad = 'S'
091700050728     c                   ExSr      Sr_Arrtpd
091800050803     c                   EndIf
091900050728     c  n02              Write     titpd000
092000050728     c   02              Write     tiopd
092100050804
092200050727      * --> SCRIVO TITAV se copia verso tariffa
092300050804      *                  e se non è già stato scritto in un richiamo precedente
092400050912if  2c                   If        Not *In02
092500050727     c                   Eval      wtav = *Off
092600050727     c                   Eval      tavksc = tamksc
092700050727     c                   Eval      tavctr = tamctr
092800050727     c                   Eval      tavprg = tamprg
092900050728     c                   Eval      tavtrd = 'PAR'
093000050728     c                   Eval      tavftc = tpdftc
093100050727     c                   Eval      tavcta = 'D'
093200050727     c                   Eval      tavdav = *date
093300050727      *            Controllo se non è già stato scritto in precedenza
093400050727     c     kTitav        Setll     Titav01l
093500050727do  3c                   Do        *Hival
093600050727     c     kTitav        Reade     Titav01l
093700050727     c                   If        %Eof(Titav01l)
093800050727     c                   Leave
093900050727     c                   EndIf
094000050727if  4c                   If        tavnoj = job_name and tavpru = user
094100050727     c                   Eval      wtav = *On
094200050727     c                   Leave
094300050727    4c                   EndIf
094400050727    3c                   EndDo
094500050727      *            Non è stato scritto scrivo ora
094600050727if  3c                   If        wtav = *Off
094700050727     c                   Clear                   Titav000
094800050727     c                   Eval      tavksc = tamksc
094900050727     c                   Eval      tavctr = tamctr
095000050727     c                   Eval      tavprg = tamprg
095100050728     c                   Eval      tavtrd = 'PAR'
095200050727     c                   Eval      tavcta = 'D'
095300050728     c                   Eval      tavftc = tpdftc
095400050727     c                   Eval      tavdav = *date
095500050727     c                   Time                    w0140
095600050727     c                   Movel     w0140         tavorv
095700050727     c                   Eval      tavnoj = job_name
095800050727     c                   Eval      tavpru = user
095900050727     c                   Eval      tavdtr = *date
096000050727     c                   Write     Titav000
096100050727    3c                   EndIf
096200050727    2c                   EndIf
096300050727    1c                   EndIf
096400050727
096500050727      * --> COPIA TOTALE
096600050727if  1c                   If        ta25tpd = 'T'
096700050727      * controllo che non esista già
096800050727     c                   Eval      kksc = ta25kscn
096900050727     c                   Eval      kctr = ta25ctrn
097000050802     c  n03              Move      ta25prgn      kprg
097100050802     c   03              Move      ta25prg       kprg
097200050817     c                   Eval      kftc = wftc
097300050727     c  n02kTitpt        Setll     Titpd01l
097400050727     c   02kTitpt        Setll     Tiopd01l
097500050727do  2c                   Do        *Hival
097600050727     c  n02kTitpt        Reade     Titpd01l
097700050727     c   02kTitpt        Reade     Tiopd01l
097800050727if  3c                   If        %Eof
097900050727     c                   Leave
098000050727     c                   EndIf
098100050727      * controllo se il dettaglio della tariffa particolare è da copiare o no
098200050727     c                   Eval      w002a = tpdcts
098300050727     c                   ExSr      Sr_Chktpd
098400050727if  3c                   If        wokrec = *On
098500050727     c                   Iter
098600050727    3c                   EndIf
098700050727      * esiste e non è annullata esco dal pgm con msg di errore
098800050802      * se non sto inglobando
098900050803if  3c                   If        tpdatb = *Blanks and
099000080521     c                             ta25inglo = 'N'
099100050727     c                   Eval      ta25err = '1'
099200050727     c                   Eval      ta25msg = msg(05)
099300050803     c                   Eval      %subst(ta25msg:37:2) = tptftc
099400050727     c                   Eval      ta25tpt = 'N'
099500050727     c                   Eval      ta25tgc = 'N'
099600050727     c                   Eval      ta25cat = 'N'
099700050727     c                   Goto      Fine
099800050727    3c                   EndIf
099900050803      * esiste e non è annullata e sto inglobando leggo rcd sucessivo
100000050803if  3c                   If        tpdatb = *Blanks and
100100080521     c                             ta25inglo <> 'N'
100200050803     c                   Iter
100300050803    3c                   EndIf
100400050727      * esiste ma è annullata la cancello
100500050802     c                   If        tpdatb <> *Blanks
100600050727     c  n02              Delete    titpd000
100700050727     c   02              Delete    tiopd
100800050802     c                   EndIf
100900050727    2c                   EndDo
101000050727
101100050727      * Aggancio il dettaglio della tariffa particolare da cui copiare
101200050727     c                   Eval      kksc = ta25ksco
101300050727     c                   Move      ta25ctro      kctr
101400050727     c                   Move      ta25prgo      kprg
101500050817     c                   Eval      kftc = wftc
101600050727     c  n01kTitpt        Setll     Titpd01l
101700050727     c   01kTitpt        Setll     Tiopd01l
101800050727do  2c                   Do        *Hival
101900051125     c  n01kTitpt        Reade(n)  Titpd01l
102000051125     c   01kTitpt        Reade(n)  Tiopd01l
102100050727     c                   If        %Eof
102200050727     c                   Leave
102300050727     c                   EndIf
102400050727      * escludo annullati
102500050727     c                   If        tpdatb <> *Blanks
102600050727     c                   Iter
102700050727     c                   EndIf
102800050727      * controllo se il dettaglio della tariffa particolare è da copiare o no
102900050727     c                   Eval      w002a = tpdcts
103000050727     c                   ExSr      Sr_Chktpd
103100050727if  3c                   If        wokrec = *On
103200050727     c                   Iter
103300050727    3c                   EndIf
103400050727      * scrivo il nuovo dettaglio della tariffa particolare
103500050727     c                   Eval      tpdksc = ta25kscn
103600050727     c                   Eval      tpdctr = ta25ctrn
103700050802     c  n03              Move      ta25prgn      tpdprg
103800050802     c   03              Move      ta25prg       tpdprg
103900050727     c                   Clear                   tpdftr
104000050727     c                   Eval      tpddtr = *date
104100050727      * prima della write eseguo gli arrotondamenti
104200050803     c                   If        ta25sad = 'S'
104300050727     c                   ExSr      Sr_Arrtpd
104400050803     c                   EndIf
104500050727     c  n02              Write     titpd000
104600050727     c   02              Write     tiopd
104700050727    2c                   EndDo
104800050727      * --> NON SCRIVO TITAV xchè lo scrivo una volta solo dopo che ho scritto
104900050727      *                      la testata della tariffa particolare (TITPT)
105000050727    1c                   EndIf
105100050727
105200050727     c                   EndSr
105300050729
105400050729      *------------------------------------------------------------------------*
105500050729      * CREO TITGC
105600050729      *------------------------------------------------------------------------*
105700050729     c     Sr_Titgc      BegSr
105800050729
105900050729      * --> COPIA A DETTAGLIO
106000050729if  1c                   If        ta25tgc = 'D'
106100050729      * controllo che non esista già un record uguale
106200050729     c                   Eval      kksc = ta25kscn
106300050729     c                   Eval      kctr = ta25ctrn
106400050802     c  n03              Move      ta25prgn      kprg
106500050802     c   03              Move      ta25prg       kprg
106600050729     c  n02kTntam01      Chain     Titgc01l
106700050729     c   02kTntam01      Chain     Tiogc01l
106800050729if  2c                   If        %Found
106900050729      * esiste e non è annullata esco dal pgm con msg di errore
107000050729     c                   If        tgcatb = *Blanks
107100050729     c                   Eval      ta25err = '1'
107200050729     c                   Eval      ta25msg = msg(06)
107300050729     c                   Eval      ta25tgc = 'N'
107400050729     c                   Eval      ta25cat = 'N'
107500050729     c                   Goto      Fine
107600050729     c                   EndIf
107700050729      * esiste ma è annullata la cancello
107800050729     c  n02              Delete    titgc000
107900050729     c   02              Delete    tiogc
108000050729    2c                   EndIf
108100050729
108200050729      * scrivo il nuovo dettaglio in base a quanto passato nella tnta25tgc
108300050729     c                   Clear                   tgcatb
108400050729     c                   Eval      tgcksc = ta25kscn
108500050729     c                   Eval      tgcctr = ta25ctrn
108600050802     c  n03              Move      ta25prgn      tgcprg
108700050802     c   03              Move      ta25prg       tgcprg
108800050729     c                   Eval      tgcsgv = ta25sgvr
108900050729     c                   Eval      tgcsgr = ta25sgrr
109000050729     c                   Eval      tgcsgp = ta25sgpr
109100050729     c                   Eval      tgcsgd = ta25sgdr
109200050729     c                   Eval      tgcsg1 = ta25sg1r
109300050729     c                   Eval      tgcsg2 = ta25sg2r
109400050729     c                   Eval      tgcsg3 = ta25sg3r
109500050729     c                   Eval      tgcst1 = ta25st1r
109600050729     c                   Eval      tgcst2 = ta25st2r
109700050729     c                   Eval      tgcst3 = ta25st3r
109800050729     c                   Eval      tgcstm = ta25stmr
109900050729     c                   Eval      tgcrpv = ta25rpvrg
110000050729     c                   Eval      tgcfaf = ta25fafr
110100050729     c                   Eval      tgcsgf = ta25sgfr
110200050729     c                   Eval      tgcggr = ta25ggrr
110300050729     c                   Eval      tgctcm = ta25tcmr
110400050729     c                   Eval      tgctfg = ta25tfgr
110500050729     c                   Eval      tgcduv = *date
110600050729     c                   Clear                   tgcftr
110700050729     c                   Eval      tgcdtr = *date
110800050729      * prima della write eseguo gli arrotondamenti
110900050803     c                   If        ta25sad = 'S'
111000050729     c                   ExSr      Sr_Arrtgc
111100050803     c                   EndIF
111200050729     c  n02              Write     titgc000
111300050729     c   02              Write     tiogc
111400050804
111500050729      * --> SCRIVO TITAV se copia verso tariffa
111600050912if  2c                   If        Not *In02
111700050729     c                   Clear                   Titav000
111800050729     c                   Eval      tavksc = tamksc
111900050729     c                   Eval      tavctr = tamctr
112000050729     c                   Eval      tavprg = tamprg
112100050729     c                   Eval      tavtrd = 'GIA'
112200050729     c                   Eval      tavcta = 'D'
112300050729     c                   Eval      tavdav = *date
112400050729     c                   Time                    w0140
112500050729     c                   Movel     w0140         tavorv
112600050729     c                   Eval      tavnoj = job_name
112700050729     c                   Eval      tavpru = user
112800050729     c                   Eval      tavdtr = *date
112900050729     c                   Write     Titav000
113000050804    2c                   EndIf
113100050729    1c                   EndIf
113200050729
113300050729      * --> COPIA TOTALE
113400050729if  1c                   If        ta25tgc = 'T'
113500050729      * controllo che non esista già
113600050729     c                   Eval      kksc = ta25kscn
113700050729     c                   Eval      kctr = ta25ctrn
113800050802     c  n03              Move      ta25prgn      kprg
113900050802     c   03              Move      ta25prg       kprg
114000050729     c  n02kTntam01      Chain     Titgc01l
114100050729     c   02kTntam01      Chain     Tiogc01l
114200050729if  2c                   If        %Found
114300050729      * esiste e non è annullata esco dal pgm con msg di errore
114400050802      * se non sto inglobando
114500050803if  3c                   If        tgcatb = *Blanks and
114600080521     c                             ta25inglo = 'N'
114700050729     c                   Eval      ta25err = '1'
114800050729     c                   Eval      ta25msg = msg(06)
114900050729     c                   Eval      ta25tgc = 'N'
115000050729     c                   Eval      ta25cat = 'N'
115100050729     c                   Goto      Fine
115200050803    3c                   EndIf
115300050803      * esiste e non è annullata e sto inglobando esco dalla ruotine
115400050803if  3c                   If        tgcatb = *Blanks and
115500080521     c                             ta25inglo <> 'N'
115600050803     c                   Leavesr
115700050803    3c                   EndIf
115800050729      * esiste ma è annullata la cancello
115900050802     c                   If        tgcatb <> *Blanks
116000050729     c  n02              Delete    titgc000
116100050729     c   02              Delete    tiogc
116200050802     c                   EndIf
116300050729    2c                   EndIf
116400050729
116500050729      * Aggancio le tariffe di giacenza da cui copiare
116600050729     c                   Eval      kksc = ta25ksco
116700050729     c                   Move      ta25ctro      kctr
116800050729     c                   Move      ta25prgo      kprg
116900051125     c  n01kTntam01      Chain(n)  Titgc01l
117000051125     c   01kTntam01      Chain(n)  Tiogc01l
117100050729      * scrivo il nuovo dettaglio
117200050729     c                   Eval      tgcksc = ta25kscn
117300050729     c                   Eval      tgcctr = ta25ctrn
117400050802     c  n03              Move      ta25prgn      tgcprg
117500050802     c   03              Move      ta25prg       tgcprg
117600050729     c                   Eval      tgcduv = *date
117700050729     c                   Clear                   tgcftr
117800050729     c                   Eval      tgcdtr = *date
117900050729      * prima della write eseguo gli arrotondamenti
118000050803     c                   If        ta25sad = 'S'
118100050729     c                   ExSr      Sr_Arrtgc
118200050803     c                   EndIf
118300050729      * se copia da tariffa di cartello
118400050802if  2c                   If        *In04
118500050729      * i giorni massimi di rientro per una tariffa estera vengono recuperati
118600050729      * dalla tabella 2G
118700050729     c                   If        tamfie = 'E'
118800050729     c                   Eval      tgcggr = §2ggre
118900050729     c                   EndIf
119000050729      * i giorni massimi di rientro per una tariffa FEDEX vengono recuperati
119100050729      * dalla tabella 2G
119200050729     c                   If        §tafed = 'S'
119300050729     c                   Eval      tgcggr = §2ggfe
119400050729     c                   EndIf
119500050729      * i giorni massimi di rientro per una tariffa DPD vengono recuperati
119600050729      * dalla tabella 2G
119700050729     c                   If        §tadpd = 'S'
119800050729     c                   Eval      tgcggr = §2ggdp
119900050729     c                   EndIf
120000050802    2c                   EndIf
120100050729     c  n02              Write     titgc000
120200050729     c   02              Write     tiogc
120300050729      * --> SCRIVO TITAV se copia verso tariffa
120400050912if  2c                   If        Not *In02
120500050729     c                   Clear                   Titav000
120600050729     c                   Eval      tavksc = tamksc
120700050729     c                   Eval      tavctr = tamctr
120800050729     c                   Eval      tavprg = tamprg
120900050729     c                   Eval      tavtrd = 'GIA'
121000050729     c                   Eval      tavcta = 'D'
121100050802      * se sto inglobando metto 'I'
121200050802     c                   If        ta25inglo <> 'N'
121300050802     c                   Eval      tavcta = 'I'
121400050802     c                   EndIf
121500050729     c                   Eval      tavdav = *date
121600050729     c                   Time                    w0140
121700050729     c                   Movel     w0140         tavorv
121800050729     c                   Eval      tavnoj = job_name
121900050729     c                   Eval      tavpru = user
122000050729     c                   Eval      tavdtr = *date
122100050729     c                   Write     Titav000
122200050729    2c                   EndIf
122300050729    1c                   EndIf
122400050729
122500050729     c                   EndSr
122600050801
122700050801      *------------------------------------------------------------------------*
122800050801      * CREO DATI TIPO
122900050801      *------------------------------------------------------------------------*
123000050801     c     Sr_Cat        BegSr
123100050802
123200050802      * I due pgm che vengono richiamti per la copia del CAT non prevedono
123300050802      * nessun tipo di errore di ritorno.....
123400050801
123500050801      * Se copia verso offerta
123600050801if  1c                   If        *In02
123700050801     c                   Clear                   tnta32ds
123800050801      * --> copio da tariffa
123900050801if  2c                   If        Not *In01
124000050801     c                   Eval      d32cto = 'T'
124100050801   x2c                   Else
124200050801      * --> copio da offerta
124300050801     c                   Eval      d32cto = 'O'
124400050801    2c                   EndIf
124500061009     c                   Eval      d32dsf = 'S'
124600050801     c                   Eval      d32tla = 'L'
124700050801     c                   Eval      d32ksc = ta25ksco
124800050801     c                   Eval      d32ctr = ta25ctro
124900050801     c                   Eval      d32prg = ta25prgo
125000050801     c                   Eval      d32div = 'EUR'
125100050801     c                   Eval      d32nrv = ta25kscn
125200050801     c                   Eval      d32ct2 = ta25ctrn
125300100310     c  n03              move      ta25prgn      d32pr2
125400100310     c   03              move      ta25prg       d32pr2
125500050801     c                   Eval      d32din = 'EUR'
125600050801     c                   Eval      d32lnp = comlnp
125700050801     c                   Eval      kpjbu = tnta32ds
125800050801     c                   Call      'TNTA32C'
125900050801     c                   Parm                    kpjba
126000050801    1c                   EndIf
126100050801
126200050801      * Se copia verso tariffa
126300050801if  1c                   If        Not *In02
126400050801     c                   Clear                   tnta31ds
126500050801     c                   Eval      d31tla = 'L'
126600050801     c                   Eval      d31ksc = ta25ksco
126700050801     c                   Eval      d31ctr = ta25ctro
126800050801     c                   Eval      d31prg = ta25prgo
126900050801     c                   Eval      d31div = 'EUR'
127000050801     c                   Eval      d31ksn = ta25kscn
127100050801     c                   Eval      d31ctn = ta25ctrn
127200050802     c  n03              Move      ta25prgn      d31prn
127300050802     c   03              Move      ta25prg       d31prn
127400050801     c                   Eval      d31din = 'EUR'
127500050801     c                   Eval      kpjbu = tnta31ds
127600050801     c                   Call      'TNTA31R'
127700050801     c                   Parm                    kpjba
127800050801    1c                   EndIf
127900050801
128000050801     c                   EndSr
128100050801
128200050801      *------------------------------------------------------------------------*
128300050801      * INTEGRO A TOTALE LA TARIFFA/OFFERTA
128400050801      *------------------------------------------------------------------------*
128500050801     c     Sr_Inglo      BegSr
128600050802
128700050802     c                   Eval      savin01 = *In01
128800050802     c                   Eval      savin04 = *In04
128900050801
129000050802      * Cerco la cartello da cui copiare
129100050802     c                   ExSr      Sr_Tarcar
129200070214
129300070214      * Accendo l'indicatore 04 per indicare che sto copiando da cartello
129400070214     c                   eval      *in04 = *on
129500061011
129600061011      * se non ho ricevuto la tariffa particolare da duplicare
129700061011      * devo inglobare tutte quelle che sono indicate in tabella
129800061011      * altrimenti è una duplica tariffe particolari di cartello
129900061011    0c                   if        ta25ftcr = *blanks
130000050801
130100050801      * Leggo la tabella 1P per recuperare solo le tariffe particolari
130200050801      * da inglobare dalla cartello
130300050801     c                   Clear                   ds1p
130400050805     c                   Eval      kcod1 = '1P'
130500050801     c     kTabel        Setll     Tabel00f
130600050801if  1c                   Do        *Hival
130700050801     c     kTabel        Reade     Tabel00f
130800050801      * Fine file
130900050801     c                   If        %Eof(Tabel00f)
131000050801     c                   Leave
131100050801     c                   EndIf
131200050801      * Escludo annullati
131300050801     c                   If        tblflg <> *Blanks
131400050801     c                   Iter
131500050801     c                   EndIf
131600050801     c                   Eval      ds1p = tbluni
131700050801      * Inglobo solo le tariffe particolari che hanno flag = 'I'
131800050801s   2c                   select
131900050801      * Tariffa con tipo servizio poste
132000050801     c                   When      tamtsp = 'P' and §1pfpt <> 'I'
132100050801     c                   Iter
132200050801      * Tariffa con Network DPD
132300050801     c                   When      §tadpd = 'S' and §1pfdp <> 'I'
132400050801     c                   Iter
132500050801      * Tariffa con Network FedEx
132600050801     c                   When      §tafed = 'S' and §1pffe <> 'I'
132700050801     c                   Iter
132800050801      * Tariffa Italia
132900050801     c                   When      tamfie = 'I' and §1ptco <> 'I'
133000050801     c                   Iter
133100050801      * Per tariffa Estero controllo flag §1pfie
133200050801     c                   When      tamfie = 'E' and §1pfie <> 'I'
133300050801     c                   Iter
133400050801      * Utilizzabile in tariffa
133500050802     c                   When      §1puta <> 'S'
133600050801     c                   Iter
133700050802    2c                   EndSl
133800050923      * salvo la tariffa particolare appena letta
133900050923     c                   Eval      kkey1 = tblkey
134000050802      * Leggo la tariffa particolare da copiare dalla cartello
134100050802     c                   Eval      kksc = o27ksc
134200050802     c                   Eval      kctr = o27ctr
134300050802     c                   Move      o27prg        kprg
134400050802     c                   Eval      kftc = tblkey
134500050802     c     kTitpt        Setll     Titpt01l
134600050802do  2c                   Do        *Hival
134700050802     c     kTitpt        Reade(n)  Titpt01l
134800050802      * Fine file
134900050802     c                   If        %Eof(Titpt01l)
135000050802     c                   Leave
135100050802     c                   EndIf
135200050802      * Escludo annullati
135300050802     c                   If        tptatb <> *Blanks
135400050802     c                   Iter
135500050802     c                   EndIf
135600050802      * Imposto i dati della tnta25tpt e della tnta25ds
135700050802      * x copiare la tariffa particolare
135800050802     c                   Eval      *In01 = *Off
135900050802     c                   Eval      ta25ksco = o27ksc
136000050802     c                   Eval      ta25ctro = o27ctr
136100050802     c                   Move      o27prg        ta25prgo
136200050802     c                   Eval      ta25ftcr = tptftc
136300050802     c                   Eval      ta25tpgr = tpttpg
136400050802     c                   Eval      ta25arlr = tptarl
136500050802     c                   Eval      ta25arfr = tptarf
136600050802     c                   Eval      ta25aror = tptaro
136700050802     c                   Eval      ta25rpvrt = tptrpv
136800050802     c                   Eval      ta25vlmr = tptvlm
136900050802     c                   Eval      ta25vvmr = tptvvm
137000050802     c                   Eval      ta25fvmr = tptfvm
137100050802     c                   Eval      ta25tmar = tpttma
137200050802     c                   Eval      ta25aplr = tptapl
137300060802     c                   eval      ta25dpbr = tptdpb
137400060803      * la data inserimento devo aggiornarla con la data del giorno
137500060803     c                   eval      ta25ditr = *date
137600060802     c                   eval      ta25flor = tptflo
137700050802      * Richiamo la routine per copiare la tariffa particolare a dettaglio
137800050803     c                   Eval      ta25tpt = 'D'
137900050802     c                   ExSr      Sr_Titpt
138000050802    2c                   EndDo
138100050805      * Mi riposiziono sulla tabella 1P
138200050805     c     kTabel1       Setgt     Tabel00f
138300050805     c
138400050801    1c                   EndDo
138500050802
138600050802      * Inglobo anche la tariffa di giacenza della cartello
138700050802     c                   Eval      *In04 = *On
138800050802      * Richiamo la routine per copiare la tariffa di giacenza
138900050802     c                   Eval      ta25tgc = 'T'
139000050802     c                   Eval      *In01 = *Off
139100050802     c                   Eval      ta25ksco = o27ksc
139200050802     c                   Eval      ta25ctro = o27ctr
139300050802     c                   Move      o27prg        ta25prgo
139400050802     c                   ExSr      Sr_Titgc
139500061011
139600061011      * duplica tariffe particolari di cartello
139700061011   x0c                   else
139800061011      * leggo la tariffa particolare da copiare dalla cartello
139900061011     c                   eval      kksc = o27ksc
140000061011     c                   eval      kctr = o27ctr
140100061011     c                   move      o27prg        kprg
140200061011     c                   eval      kftc = ta25ftcr
140300061011     c     ktitpt        setll     titpt01l
140400061011do  1c                   do        *hival
140500061011     c     ktitpt        reade(n)  titpt01l
140600061011      * fine file
140700061011     c                   if        %eof(titpt01l)
140800061011     c                   leave
140900061011     c                   endif
141000061011      * escludo annullati
141100061011     c                   if        tptatb <> *blanks
141200061011     c                   iter
141300061011     c                   endif
141400061011      * imposto i dati della tnta25tpt e della tnta25ds
141500061011      * x copiare la tariffa particolare
141600061011     c                   eval      *in01 = *off
141700061011     c                   eval      ta25ksco = o27ksc
141800061011     c                   eval      ta25ctro = o27ctr
141900061011     c                   move      o27prg        ta25prgo
142000061011     c                   eval      ta25ftcr = tptftc
142100061011     c                   eval      ta25tpgr = tpttpg
142200061011     c                   eval      ta25arlr = tptarl
142300061011     c                   eval      ta25arfr = tptarf
142400061011     c                   eval      ta25aror = tptaro
142500061011     c                   eval      ta25rpvrt = tptrpv
142600061011     c                   eval      ta25vlmr = tptvlm
142700061011     c                   eval      ta25vvmr = tptvvm
142800061011     c                   eval      ta25fvmr = tptfvm
142900061011     c                   eval      ta25tmar = tpttma
143000061011     c                   eval      ta25aplr = tptapl
143100061011      * richiamo la routine per copiare la tariffa particolare a dettaglio
143200061011     c                   eval      ta25tpt = 'D'
143300061011     c                   exsr      sr_titpt
143400061011    1c                   enddo
143500061011    0c                   endif
143600150219
143700150219      /free
143800150219       //?Alla fine dei giochi controllo se il cliente è presente in
143900150219       //?tabella VMA e se la data decorrenza tariffa è compresa nelle
144000150219       //?date impostate
144100150219       //?in questo caso devo impostare NO VMA sulla tariffa particolare
144200150219       //?fuel...SEMPRE!!!!
144300150219         exsr Sr_NoVMA;
144400150219      /end-free
144500050802
144600050802     c                   Eval      *In01 = savin01
144700050802     c                   Eval      *In04 = savin04
144800050801
144900050801     c                   EndSr
145000080521
145100080521      *------------------------------------------------------------------------*
145200080521      * INTEGRO LA TARIFFA/OFFERTA CON TARIFFA PARTICOLARE FUEL
145300080521      *------------------------------------------------------------------------*
145400080521     c     sr_fuel       begsr
145500080521
145600080521      * Leggo la tabella 1P per controllare se devo creare la tariffa
145700080521      * Fuel per la tariffa interessata
145800080521     c                   clear                   ds1p
145900080521     c                   eval      kcod = '1P'
146000080522     c                   eval      kkey = 'f '
146100080521     c     ktabel01      chain     tabel00f
146200080521     c                   if        %found(tabel00f) and tblflg = *blanks
146300080521     c                   eval      ds1p = tbluni
146400080521     c                   endif
146500080521      * Creo solo se è possibile
146600080522    1c                   select
146700080521      * Tariffa con tipo servizio poste
146800080522     c                   when      tamtsp = 'P' and §1pfpt = 'N'
146900080521     c                   leavesr
147000080521      * Tariffa con Network DPD
147100080522     c                   when      §tadpd = 'S' and §1pfdp = 'N'
147200080521     c                   leavesr
147300080521      * Tariffa con Network FedEx
147400080522     c                   when      §tafed = 'S' and §1pffe = 'N'
147500080521     c                   leavesr
147600080521      * Tariffa Italia
147700080522     c                   when      tamfie = 'I' and §1ptco = 'N'
147800080521     c                   leavesr
147900080521      * Per tariffa Estero controllo flag §1pfie
148000080522     c                   when      tamfie = 'E' and §1pfie = 'N'
148100080521     c                   leavesr
148200080521      * Utilizzabile in tariffa
148300080521     c                   when      §1puta <> 'S'
148400080521     c                   leavesr
148500080522    1c                   endsl
148600080521
148700080521      * controllo che non esista già la tariffa - Testata
148800080521     c                   eval      kksc = ta25kscn
148900080521     c                   eval      kctr = ta25ctrn
149000080521     c                   move      ta25prgn      kprg
149100080522     c                   eval      kftc = 'f '
149200080522     c  n02ktitpt        chain     titpt01l
149300080522     c   02ktitpt        chain     tiopt01l
149400080522    1c                   if        %found
149500080521      * esiste e non è annullata esco dalla routine
149600080522     c                   if        tptatb = *blanks
149700080521     c                   leavesr
149800080522     c                   endif
149900080521      * esiste ma è annullata la cancello
150000080522    2c                   if        tptatb <> *blanks
150100080521     c  n02              delete    titpt000
150200080521     c   02              delete    tiopt
150300080522      * e cancello anche il relativo dettaglio
150400080522     c  n02ktitpt        setll     titpd01l
150500080522     c   02ktitpt        setll     tiopd01l
150600080522    3c                   do        *hival
150700080522     c  n02ktitpt        reade     titpd01l
150800080522     c   02ktitpt        reade     tiopd01l
150900080522     c                   if        %eof
151000080522     c                   leave
151100080522     c                   endif
151200080522     c  n02              delete    titpd000
151300080522     c   02              delete    tiopd
151400080522    3c                   enddo
151500080522    2c                   endif
151600080522    1c                   endif
151700080521
151800080522      * imposto il codice tassazione e relativo ordinamento in base
151900080522      * alla tariffa italia/estera/DPD/FedEx
152000080522     c                   select
152100080522      * Tariffa con Network DPD
152200080522     c                   when      §tadpd = 'S'
152300080522     c                   eval      w002a = 'IT'
152400080522      * Tariffa con Network FedEx
152500080522     c                   when      §tafed = 'S'
152600080522     c                   eval      w002a = 'EE'
152700080522      * Tariffa Italia
152800080522     c                   when      tamfie = 'I'
152900080522     c                   eval      w002a = 'IT'
153000080522      * Tariffa Estero
153100080522     c                   when      tamfie = 'E'
153200080522     c                   eval      w002a = 'EE'
153300080522     c                   endsl
153400080522      * cerco l'ordinamento dalla tabella CT
153500080522     c                   exsr      sr_chkcts
153600080522
153700080522      * scrivo il dettaglio della tariffa particolare
153800080522      * se non esiste già
153900080522      * (forse controllo inutile visto che prima verifico che non esista
154000080522      *  già la testata, ma così evito la rottura del programma)
154100080604     c                   evalr     korp = §ctcor
154200080522     c                   eval      ksgl = 9999999999
154300080522     c  n02ktitpd        chain     titpd01l
154400080522     c   02ktitpd        chain     tiopd01l
154500080522      * scrivo il nuovo dettaglio se non esiste
154600080522    1c                   if        not %found
154700080522     c  n02              clear                   titpd000
154800080522     c   02              clear                   tiopd
154900080522     c                   eval      tpdksc = kksc
155000080522     c                   eval      tpdctr = kctr
155100080522     c                   eval      tpdprg = kprg
155200080522     c                   eval      tpdftc = kftc
155300080522     c                   eval      tpdcts = w002a
155400080522     c                   eval      tpdorp = korp
155500080522     c                   eval      tpdsgl = ksgl
155600110525     c                   eval      tpditr = 0,5
155700080522     c                   eval      tpddtr = *date
155800080522     c  n02              write     titpd000
155900080522     c   02              write     tiopd
156000080522    1c                   endif
156100080522
156200080522      * scrivo la testata della tariffa particolare
156300080522      * (qua non controllo se esiste già...l'ho già fatto prima)
156400080522     c  n02              clear                   titpt000
156500080522     c   02              clear                   tiopt
156600140227     c                   clear                   dtpt01
156700080522     c                   eval      tptksc = kksc
156800080522     c                   eval      tptctr = kctr
156900080522     c                   eval      tptprg = kprg
157000080522     c                   eval      tptduv = *date
157100080522     c                   eval      tptftc = kftc
157200080522     c                   eval      tpttpg = 'V'
157300080522     c                   eval      tptdtr = *date
157400140109      * la data riferimento prezzo base carburante viene recuperata dal file
157500140109      * di riferimento TIDPB00F
157600140109      * verifico se esiste il file del prezzo base da impostare in base alla
157700140109      * data validità altrimenti imposto la data decorrenza tariffa
157800140109     c     *date         setll     tidpb01l
157900140109     c                   read      tidpb01l
158000140109     c                   if        %found(tidpb01l)
158100140109     c                   eval      tptdpb = dpbdpb
158200140227     c                   eval      §tptpma = dpbpma
158300140109     c                   else
158400140109     c                   eval      tptdpb = tamddt
158500140227     c                   eval      §tptpma = *zeros
158600140109     c                   endif
158700140227     c                   eval      tptflo = dtpt01
158800080522      * la data inserimento = data del giorno
158900080521     c                   eval      tptdit = *date
159000080522     c  n02              write     titpt000
159100080522     c   02              write     tiopt
159200080522
159300080522      * --> SCRIVO TITAV se copia verso tariffa
159400080522    1c                   if        not *in02
159500080522     c                   clear                   titav000
159600080522     c                   eval      tavksc = tamksc
159700080522     c                   eval      tavctr = tamctr
159800080522     c                   eval      tavprg = tamprg
159900080522     c                   eval      tavtrd = 'PAR'
160000080522     c                   eval      tavftc = tpdftc
160100080522     c                   eval      tavcta = 'I'
160200080522     c                   eval      tavdav = *date
160300080522     c                   time                    w0140
160400080522     c                   movel     w0140         tavorv
160500080522     c                   eval      tavnoj = job_name
160600080522     c                   eval      tavpru = user
160700080522     c                   eval      %subst(tavpru:10:1) = 'A'
160800080522     c                   eval      tavdtr = *date
160900080522     c                   write     titav000
161000080522    1c                   endif
161100150219
161200150219      /free
161300150219       //?Alla fine dei giochi controllo se il cliente è presente in
161400150219       //?tabella VMA e se la data decorrenza tariffa è compresa nelle
161500150219       //?date impostate
161600150219       //?in questo caso devo impostare NO VMA sulla tariffa particolare
161700150219       //?fuel...SEMPRE!!!!
161800150219         exsr Sr_NoVMA;
161900150219      /end-free
162000080521
162100080521     c                   endsr
162200061012
162300061012      *------------------------------------------------------------------------*
162400061012      * DUPLICA DETTAGLIO TARIFFARIO
162500061012      *------------------------------------------------------------------------*
162600061012     c     sr_duptad     begsr
162700061012
162800061012     c                   eval      wcap = *off
162900061012      * aggancio il dettaglio della tariffa da cui copiare
163000061012     c                   eval      kksc = ta25ksco
163100061012     c                   move      ta25ctro      kctr
163200061012     c                   move      ta25prgo      kprg
163300061012     c                   move      ta25olnp      klnp
163400061012    1c                   if        not *in07
163500061012     c  n01ktitad        setll     titad04l
163600061012     c   01ktitad        setll     tiofd01l
163700061012     c  n01ktitad        reade     titad04l
163800061012     c   01ktitad        reade     tiofd01l
163900061012    1c                   endif
164000061012    1c                   if        *in07
164100061012     c                   move      worpo         korp
164200061012     c  n01ktitad01      setll     titad04l
164300061012     c   01ktitad01      setll     tiofd01l
164400061012     c  n01ktitad01      reade     titad04l
164500061012     c   01ktitad01      reade     tiofd01l
164600061012    1c                   endif
164700061012    1c                   dow       not %eof
164800061012      * escludo annullati
164900061012    2c                   if        tadatb = *blanks
165000061012      * se richiesto anche il codice di tassazione controllo il cap
165100061012    3c                   if        *in07 and tadcap <> *blanks
165200061012     c                   eval      wcap = *on
165300061012     c                   goto      noduptad
165400061012    3c                   endif
165500061012      * la nuova tariffa se esiste la devo cancellare
165600061012     c                   eval      kksc = ta25kscn
165700061012     c                   move      ta25ctrn      kctr
165800061012     c                   move      ta25prgn      kprg
165900061012     c                   move      ta25nlnp      klnp
166000061012     c  n07              move      tadorp        korp
166100061012     c   07              move      worpn         korp
166200061012     c                   eval      knaz = tadnaz
166300061012     c                   eval      kcap = tadcap
166400061012     c                   eval      ksgl = tadsgl
166500061012     c  n02ktitad04      chain     titad04l
166600061012     c   02ktitad04      chain     tiofd01l
166700061012    3c                   if        %found
166800061012     c  n02              delete    titad000
166900061012     c   02              delete    tiofd
167000061012    3c                   endif
167100061012      * riaggancio il record da cui devo copiare
167200061012     c                   eval      kksc = ta25ksco
167300061012     c                   move      ta25ctro      kctr
167400061012     c                   move      ta25prgo      kprg
167500061012     c                   move      ta25olnp      klnp
167600061012     c  n07              move      tadorp        korp
167700061012     c   07              move      worpo         korp
167800061012     c  n01ktitad04      chain     titad04l
167900061012     c   01ktitad04      chain     tiofd01l
168000061012    3c                   if        %found
168100061012      * scrivo il nuovo dettaglio
168200061012     c                   eval      tadksc = ta25kscn
168300061012     c                   eval      tadctr = ta25ctrn
168400061012     c                   move      ta25prgn      tadprg
168500061012     c                   move      ta25nlnp      tadlnp
168600061012     c   07              move      ta25ncts      tadcts
168700061012     c   07              move      worpn         tadorp
168800061012     c                   clear                   tadftr
168900061012     c                   eval      taddtr = *date
169000061012     c  n02              write     titad000
169100061012     c   02              write     tiofd
169200061012    3c                   endif
169300061012    2c                   endif
169400061012     c     noduptad      tag
169500061012     c  n07
169600061012     cann01ktitad        reade     titad04l
169700061012     c  n07
169800061012     can 01ktitad        reade     tiofd01l
169900061012     c   07
170000061012     cann01ktitad01      reade     titad04l
170100061012     c   07
170200061012     can 01ktitad01      reade     tiofd01l
170300061012    1c                   enddo
170400061012
170500061012      * --> SCRIVO TITAV se copia verso tariffa
170600061012    1c                   if        not *in02
170700061012     c                   clear                   titav000
170800061012     c                   eval      tavksc = tamksc
170900061012     c                   eval      tavctr = tamctr
171000061012     c                   eval      tavprg = tamprg
171100061012     c                   eval      tavtrd = 'DET'
171200061012     c                   eval      tavcta = 'D'
171300061012     c                   eval      tavdav = *date
171400061012     c                   time                    w0140
171500061012     c                   movel     w0140         tavorv
171600061012     c                   eval      tavnoj = job_name
171700061012     c                   eval      tavpru = user
171800061012     c                   eval      tavdtr = *date
171900070220     c                   write     Titav000                             99
172000061012    1c                   endif
172100061012
172200061012      * se duplica con codice di tassazione ed ho trovato almeno un cap
172300061012      * messaggio info al chiamante
172400061012    1c                   if        wcap = *on
172500061012     c                   eval      ta25err = '1'
172600061012     c                   eval      ta25msg = msg(07)
172700061012    1c                   endif
172800061012
172900061012     c                   endsr
173000050727
173100050727      *------------------------------------------------------------------------*
173200050727      * CONTROLLO IL CODICE TARIFFA
173300050727      *------------------------------------------------------------------------*
173400050727     c     Sr_Chkctr     BegSr
173500050727
173600050727     c                   Clear                   dstr
173700050727     c                   Eval      kcod = 'TR'
173800050727     c                   Eval      kkey = w001a
173900050803     c     kTabel01      Chain     Tabel00f
174000050727     c                   If        %Found(Tabel00f) and tblflg = *Blanks
174100050727     c                   Eval      dstr = tbluni
174200050727     c                   EndIf
174300050727
174400050727     c                   EndSr
174500050725
174600050715      *------------------------------------------------------------------------*
174700050715      * CONTROLLO IL CODICE TASSAZIONE IN TABELLA
174800050715      *------------------------------------------------------------------------*
174900050715     c     Sr_Chkcts     BegSr
175000050715
175100050715     c                   Clear                   dsct
175200050715     c                   Eval      kcod = 'CT'
175300050715     c                   Eval      kkey = w002a
175400050803     c     kTabel01      Chain     Tabel00f
175500050715     c                   If        %Found(Tabel00f) and tblflg = *Blanks
175600050715     c                   Eval      dsct = tbluni
175700050715     c                   EndIf
175800050715
175900050715     c                   EndSr
176000050727
176100050727      *------------------------------------------------------------------------*
176200050727      * CONTROLLO SE LA TARIFFA PARTICOLARE E' GESTIBILE
176300050727      *------------------------------------------------------------------------*
176400050727     c     Sr_Chktpt     BegSr
176500050727
176600050727     c                   Eval      wokrec = *Off
176700050727
176800050727     c                   Clear                   ds1p
176900050727     c                   Eval      kcod = '1P'
177000050727     c                   Eval      kkey = w002a
177100050803     c     kTabel01      Chain     Tabel00f
177200050727     c                   If        %Found(Tabel00f) and tblflg = *Blanks
177300050727     c                   Eval      ds1p = tbluni
177400050727     c                   EndIf
177500050727
177600050727      * se tariffa FedEx e non sto copiando verso una tariffa di cartello e
177700050728      * la tariffa particolare FedEx è valida solo per la cartello non copio
177800050727if  1c                   If        §tafed = 'S' and Not *In05 and
177900050727     c                             §1pffe = 'C'
178000050727     c                   Eval      wokrec = *On
178100050727     c                   Leavesr
178200050727    1c                   EndIf
178300050727
178400050727      * copio solo le tariffe particolari che hanno il flag di gestione <> 'N'
178500050727s   1c                   Select
178600050727      * tipo servizio poste
178700050727     c                   When      tamtsp = 'P' and §1pfpt = 'N'
178800050727     c                   Eval      wokrec = *On
178900050727      * tariffa DPD
179000050727     c                   When      §tadpd = 'S' and §1pfdp = 'N'
179100050727     c                   Eval      wokrec = *On
179200050727      * tariffa FedEx
179300050727     c                   When      §tafed = 'S' and (§1pffe = 'N' or
179400050727     c                                               §1pffe = 'C')
179500050727     c                   Eval      wokrec = *On
179600050727      * tariffa Italia
179700050727     c                   When      tamfie = 'I' and §1ptco = 'N'
179800050727     c                   Eval      wokrec = *On
179900050727      * tariffa Estero
180000050727     c                   When      tamfie = 'E' and §1pfie = 'N'
180100050727     c                   Eval      wokrec = *On
180200050727    1c                   EndSl
180300050727
180400050727      * Controllo se tariffa particolare in scadenza da eliminare non copio
180500050728if  1c                   If        §1peli <> *Blanks
180600050728     c                   Move      §1peli        w0080
180700050728     c                   If        tamdst >= w0080
180800050727     c                   Eval      wokrec = *On
180900050728     c                   EndIf
181000050727    1c                   EndIf
181100051019
181200080609      * Se tariffa particolare 'c ' lasciato avviso e non sto copiando
181300080609      * verso una tariffa di cartello o verso un cliente relativo ad
181400080609      * una delle filiali abilitate non copio
181500080609if  1c                   If        w002a = 'c ' and Not *In05
181600080609     c                   if        %lookup(comlnp:sklav) = *zeros
181700080609     c                             and %lookup(999:sklav) = *zeros
181800051019     c                   Eval      wokrec = *On
181900080609    1c                   endif
182000051019    1c                   EndIf
182100050727     c                   EndSr
182200050727
182300050727      *------------------------------------------------------------------------*
182400050727      * CONTROLLO SE IL DETTAGLIO DELLA TARIFFA PARTICOLARE E' DA COPIARE
182500050727      *------------------------------------------------------------------------*
182600050727     c     Sr_Chktpd     BegSr
182700050727
182800050727     c                   Eval      wokrec = *Off
182900050727
183000050727     c                   ExSr      Sr_Chkcts
183100050727
183200050727      * copio solo se il codice di tassazione è congruente
183300050727if  1c                   If        (tamfie = 'I' and w002a = 'EE') or
183400050727     c                             (tamfie = 'E' and w002a = 'IT')
183500050727     c                   Eval      wokrec = *On
183600050727     c                   Leavesr
183700050727    1c                   EndIf
183800050727
183900050727      * se tariffa FedEx copio solo i codici di tassazione previsti
184000050727      * per FedEx
184100050727if  1c                   If        §tafed = 'S' and §ctutf <> 'S'
184200050727     c                   Eval      wokrec = *On
184300050727     c                   Leavesr
184400050727    1c                   EndIf
184500050727
184600050727      * se non è tariffa FedEx devo copiare solo i codici di tassazione
184700050727      * non previsti per FedEx
184800050727if  1c                   If        §tafed <> 'S' and §ctutf = 'S'
184900050727     c                   Eval      wokrec = *On
185000050727     c                   Leavesr
185100050727    1c                   EndIf
185200050727
185300050727      * se sto copiando da una tariffa di cartello
185400050727     c                   If        *In04
185500050727s   1c                   Select
185600050727      * Se tariffa estero copio solo estero
185700050727     c                   When      tamfie = 'E' and §ctest = *Blanks
185800050727     c                   Eval      wokrec = *On
185900050727      * Se tariffa DPD copio solo estero
186000050728      * tranne il codice generico IT xchè va bene
186100050727     c                   When      §tadpd = 'S' and §ctest = *Blanks and
186200050727     c                             tpdcts <> 'IT'
186300050727     c                   Eval      wokrec = *On
186400050727      * Se tariffa italia copio solo italia
186500050727     c                   When      tamfie = 'I' and §tadpd <> 'S' and
186600050727     c                                              §ctest <> *Blanks
186700050727     c                   Eval      wokrec = *On
186800050727     c                   EndSl
186900050728    1c                   EndIf
187000050727
187100050727     c                   EndSr
187200050727
187300050727      *------------------------------------------------------------------------*
187400050728      * CONTROLLO INCLUSIONI/OMISSIONI DETTAGLIO TARIFFARIO
187500050727      *------------------------------------------------------------------------*
187600050728     c     Sr_Seltad     BegSr
187700050727
187800050727     c                   Eval      wokrec = *Off
187900050729     c                   Eval      wokcts = *Off
188000050729     c                   Eval      woksgl = *Off
188100050727
188200050727      * Controllo le inclusioni
188300050727      * --> Codici di tassazione
188400050727if  1c                   If        wcts <> *Blanks
188500050727     c     tadcts        Lookup    skcts                                  30
188600050727      *     incluso
188700050727if  2c                   If        *In30
188800050727     c                   Eval      wokcts = *On
188900050727   x2c                   Else
189000050727      *     non incluso
189100050727if  3c                   If        Not *In30 and ta25io2 = 'I'
189200050727     c                   Eval      wokrec = *On
189300050727    3c                   EndIf
189400050727    2c                   EndIf
189500050727    1c                   EndIf
189600050727      * --> Scaglione
189700050727if  1c                   If        ta25sgl > *Zeros
189800050727      *     incluso
189900050727if  2c                   If        tadsgl = ta25sgl
190000050727     c                   Eval      woksgl = *On
190100050727   x2c                   Else
190200050727      *     non incluso
190300050727if  3c                   If        ta25sgl <> tadsgl and ta25io3 = 'I'
190400050727     c                   Eval      wokrec = *On
190500050727    3c                   EndIf
190600050727    2c                   EndIf
190700050727    1c                   EndIf
190800050727
190900050727      * Controllo le omissioni
191000050727      * --> Codici di tassazione
191100050729if  1c                   If        wscts = *On and wokcts = *On
191200050727     c                   Eval      wokrec = *On
191300050727    1c                   EndIf
191400050727      * --> Scaglione
191500050727if  1c                   If        wssgl = *On and woksgl = *On
191600050727     c                   Eval      wokrec = *On
191700050727    1c                   EndIf
191800050727
191900050727     c                   If        wokrec = *On
192000050727     c                   Leavesr
192100050727     c                   EndIf
192200050727
192300050727      * Se non sto creando una nuova tariffa di cartello
192400050727if  1c                   If        Not *In05
192500050727      * Controlli x copia da tariffa di cartello e copia totale
192600050727      * e non è la cartello DPD o FedEx
192700050727      * quindi questo vale per la creazione di una nuova tariffa
192800050727      * (non di cartello) copiando dalla 8888830
192900050727      * dove sono presenti sia codici di tassazione Italia che Estero
193000050727if  2c                   If        *In04 and ta25tad = 'T' and
193100050727     c                             §tadpd <> 'S' and §tafed <> 'S'
193200050727     c                   Eval      w002a = tadcts
193300050727     c                   ExSr      Sr_Chkcts
193400050727     c                   Select
193500050727      * Se tariffa estero copio solo estero
193600050727     c                   When      ta25fie = 'E' and §ctest = *Blanks
193700050727     c                   Eval      wokrec = *On
193800050727      * Se tariffa italia copio solo italia
193900050727     c                   When      ta25fie = 'I' and §ctest <> *Blanks
194000050727     c                   Eval      wokrec = *On
194100050727     c                   EndSl
194200050727     c                   If        wokrec = *On
194300050727     c                   Leavesr
194400050727     c                   EndIf
194500050727    2c                   EndIf
194600050727    1c                   EndIf
194700050727
194800050727     c                   EndSr
194900050728
195000050727      *------------------------------------------------------------------------*
195100050727      * ARROTONDAMENTI DETTAGLIO TARIFFARIO
195200050727      *------------------------------------------------------------------------*
195300050727     c     Sr_Arrtad     BegSr
195400050727
195500050727      * se tariffa a valore arrotondo solo lo scaglione
195600050727if  1c                   If        *In06
195700050727      * se non è 99999
195800050727     c                   Z-add     tadsgl        w0050
195900050727     c                   If        w0050 <> 99999
196000050727     c                   Z-add     tadsgl        w0153
196100050727     c                   ExSr      Sr_Arroto
196200050727     c                   Z-add     w0153         tadsgl
196300050727     c                   EndIf
196400050727      * altrimenti arrotondo città + inoltro
196500050727   x1c                   Else
196600050727     c                   Z-add     taditr        w0153
196700050727     c                   ExSr      Sr_Arroto
196800050727     c                   Z-add     w0153         taditr
196900050727     c                   Z-add     tadino        w0153
197000050727     c                   ExSr      Sr_Arroto
197100050727     c                   Z-add     w0153         tadino
197200050727    1c                   EndIf
197300050727
197400050727      * arrotondo sempre il minimo e il massimo
197500050727     c                   Z-add     tadmin        w0153
197600050727     c                   ExSr      Sr_Arroto
197700050727     c                   Z-add     w0153         tadmin
197800050727     c                   Z-add     tadmax        w0153
197900050727     c                   ExSr      Sr_Arroto
198000050727     c                   Z-add     w0153         tadmax
198100050727
198200050727     c                   EndSr
198300050727
198400050727      *------------------------------------------------------------------------*
198500050727      * ARROTONDAMENTI DETTAGLIO TARIFFE PARTICOLARI
198600050727      *------------------------------------------------------------------------*
198700050727     c     Sr_Arrtpd     BegSr
198800050727
198900050727      * se tariffa a valore arrotondo solo lo scaglione
199000050727     c                   Eval      w001a = wtpg
199100050727     c                   ExSr      Sr_Chkctr
199200050727if  1c                   If        §trtap = 'S'
199300050727      * se non è 99999
199400050727     c                   Z-add     tpdsgl        w0050
199500050727     c                   If        w0050 <> 99999
199600050727     c                   Z-add     tpdsgl        w0153
199700050727     c                   ExSr      Sr_Arroto
199800050727     c                   Z-add     w0153         tpdsgl
199900050727     c                   EndIf
200000050727      * altrimenti arrotondo la tariffa
200100050727   x1c                   Else
200200050727     c                   Z-add     tpditr        w0153
200300050727     c                   ExSr      Sr_Arroto
200400050727     c                   Z-add     w0153         tpditr
200500050728    1c                   EndIf
200600050727
200700050727      * arrotondo sempre il minimo e il massimo
200800050727     c                   Z-add     tpdmin        w0153
200900050727     c                   ExSr      Sr_Arroto
201000050727     c                   Z-add     w0153         tpdmin
201100050727     c                   Z-add     tpdmax        w0153
201200050727     c                   ExSr      Sr_Arroto
201300050727     c                   Z-add     w0153         tpdmax
201400050727
201500050727     c                   EndSr
201600050729
201700050729      *------------------------------------------------------------------------*
201800050729      * ARROTONDAMENTI TARIFFE DI GIACENZA
201900050729      *------------------------------------------------------------------------*
202000050729     c     Sr_Arrtgc     BegSr
202100050729
202200050729     c                   Z-add     tgcsgv        w0153
202300050729     c                   ExSr      Sr_Arroto
202400050729     c                   Z-add     w0153         tgcsgv
202500050729     c                   Z-add     tgcsgr        w0153
202600050729     c                   ExSr      Sr_Arroto
202700050729     c                   Z-add     w0153         tgcsgr
202800050729     c                   Z-add     tgcsgp        w0153
202900050729     c                   ExSr      Sr_Arroto
203000050729     c                   Z-add     w0153         tgcsgp
203100050729     c                   Z-add     tgcsgd        w0153
203200050729     c                   ExSr      Sr_Arroto
203300050729     c                   Z-add     w0153         tgcsgd
203400050729     c                   Z-add     tgcst1        w0153
203500050729     c                   ExSr      Sr_Arroto
203600050729     c                   Z-add     w0153         tgcst1
203700050729     c                   Z-add     tgcst2        w0153
203800050729     c                   ExSr      Sr_Arroto
203900050729     c                   Z-add     w0153         tgcst2
204000050729     c                   Z-add     tgcst3        w0153
204100050729     c                   ExSr      Sr_Arroto
204200050729     c                   Z-add     w0153         tgcst3
204300050729     c                   Z-add     tgcstm        w0153
204400050729     c                   ExSr      Sr_Arroto
204500050729     c                   Z-add     w0153         tgcstm
204600050729
204700050729     c                   EndSr
204800050727
204900050727      *------------------------------------------------------------------------*
205000050727      * ARROTONDAMENTI
205100050727      *------------------------------------------------------------------------*
205200050727     c     Sr_Arroto     BegSr
205300050727
205400050803if  1c                   If        w0153 <> *Zeros
205500050727     c                   Select
205600050727     c                   When      ta25dec = 0
205700050727     c                   Z-add(h)  w0153         w0150
205800050727     c                   Z-add     w0150         w0153
205900050727     c                   When      ta25dec = 1
206000050727     c                   Z-add(h)  w0153         w0151
206100050727     c                   Z-add     w0151         w0153
206200050727     c                   When      ta25dec = 2
206300050727     c                   Z-add(h)  w0153         w0152
206400050727     c                   Z-add     w0152         w0153
206500050727     c                   EndSl
206600050727    1c                   EndIf
206700050727
206800050727     c                   EndSr
206900050801
207000050801      *------------------------------------------------------------------------*
207100050801      * RECUPERO LA TARIFFA DI CARTELLO
207200050801      *------------------------------------------------------------------------*
207300050801     c     Sr_Tarcar     BegSr
207400050801
207500050801     c                   Clear                   Trul27ds
207600050801     c                   Eval      i27tla = 'L'
207700050801s   1c                   Select
207800050801      * se tipo servizio poste passo come ntw PPT così il trul recupera la
207900050801      * cartello PPT ( per ora uguale alla cartello italia)
208000050801     c                   When      tamtsp = 'P'
208100050801     c                   Eval      i27ntw = 'PPT'
208200050801      * Ntw DPD
208300050801     c                   When      §tadpd = 'S'
208400050801     c                   Eval      i27ntw = 'DPD'
208500050801      * Ntw FedEx
208600050801     c                   When      §tafed = 'S'
208700050801     c                   Eval      i27ntw = 'FED'
208800050801      * passo anche il codice tariffa per recuperare se merci o documenti
208900050802     c                   Move      ta25ctrn      i27ctb
209000050801      * Tariffa estero
209100050801     c                   When      tamfie = 'E'
209200050801     c                   Eval      i27ntw = 'EEX'
209300050801      * Tariffa italia
209400050801     c                   When      tamfie = 'I'
209500050801     c                   Eval      i27ntw = 'COR'
209600050922      * se nessuna di queste (mi sa che è solo x la cartello 8888830)
209700050922      * passo il cliente
209800050922     c                   Other
209900050922     c                   Eval      i27cli = ta25kscn
210000050801    1c                   Endsl
210100050802      * Passo la data decorrenza per recuperare anche il progressivo della
210200050802      * tariffa di cartello
210300111031      * se nella ds TNTA25DS il campo TA25drc = 'S' devo prendere per recuperare
210400111031      * la tariffa di cartello la data maggiore tra oggi e la data decorrenza
210500111031      * della tariffa che riceve la copia
210600111031     c                   If        Ta25drc = 'S' and tamddt < *date
210700111031     c                   Eval      i27dta = *date
210800111031     c                   Else
210900050802     c                   Eval      i27dta = tamddt
211000111031     c                   Endif
211100111026
211200111026     c                   Call      'TRUL27R'
211300050802     c                   Parm                    Trul27ds
211400050802
211500050802     c                   If        o27err <> *Blanks
211600050802     c                   Goto      Fine
211700050802     c                   EndIf
211800050801
211900050801     c                   EndSr
212000080609
212100080609      *------------------------------------------------------------------------*
212200080609      * Carico filiali abilitate al lasciato avviso
212300080609      *------------------------------------------------------------------------*
212400080609     c     carlav        begsr
212500080609
212600080609     c                   clear                   sklav
212700080609     c                   clear                   xx
212800080609
212900080609     c/exec sql
213000080609     c+ declare lav cursor for select tntbe01l.*
213100080609     c+ from tntbe01l
213200080609     c+ where tbecod='LAV' and tbeatb = ''
213300080609     c/end-exec
213400080609
213500080609     c/exec sql
213600080609     c+ open lav
213700080609     c/end-exec
213800080609
213900080609     c                   do        *hival
214000080609
214100080609     c/exec sql
214200080609     c+ fetch next from lav into :tntbeds
214300080609     c/end-exec
214400080609
214500080609     c                   select
214600080609     c                   when      sqlcod = 100
214700080609     c                   leave
214800080609     c                   when      sqlcod < 0
214900080609     c                   other
215000080609     c                   eval      dlav = tbeuni
215100080609      * carico la schiera della filiali abilitate
215200080616     c                   if        d§lavtar > *date
215300080609     c                   iter
215400080609     c                   endif
215500080609     c                   eval      xx = xx + 1
215600080609     c                   eval      sklav(xx) = %dec(tbeke1:3:0)
215700080609
215800080609     c                   endsl
215900080609
216000080609     c                   enddo
216100080609
216200080609     c/exec sql
216300080609     c+ close lav
216400080609     c/end-exec
216500080609
216600080609     c                   endsr
216700150219      /free
216800150219       //--------------------------------------------------------------
216900150219       //?VMA Negato.
217000150219       //--------------------------------------------------------------
217100150219       BEGSR sr_NoVMA;
217200150219
217300150219         wVMA = *off;
217400150219
217500150219         clear TIBS02DS;
217600150219         clear dVMA;
217700150219         T02mod = 'C';
217800150219         T02cod = 'VMA';
217900150219       //?copia verso tariffa
218000150219         IF  not *in02;
218100150219           T02ke1 = %editc(TA25kscn:'X');
218200150219         ENDIF;
218300150219       //?copia verso offerta di cliente codificato
218400150219         IF  *in02;
218500150219           T02ke1 = %editc(VISksc:'X');
218600150219         ENDIF;
218700150219         T02sif = KNSIF;
218800150219         TNTBE_RicercaControllo  (kpjba : tibs02ds);
218900150219         IF  T02err = *blanks;
219000150219           dVMA = T02uni;
219100150219           IF  TAMddt >= §VMAddt and
219200150219               TAMddt <= §VMAdst;
219300150219             wVMA = *on;
219400150219           ENDIF;
219500150219         ENDIF;
219600150219       //?Aggangio la tariffa particolare fuel
219700150219         kksc = TA25kscn;
219800150219         kctr = TA25ctrn;
219900150219         kftc = 'f ';
220000150219       //?Nuovo progressivo
220100150219         IF  *in03;
220200150219           kprg = %dec(TA25prg:3:0);
220300150219         ENDIF;
220400150219       //?Progressivo già esistente
220500150219         IF  not *in03;
220600150219           kprg = %dec(TA25prgn:3:0);
220700150219         ENDIF;
220800150219       //?copia verso tariffa
220900150219         IF  not *in02;
221000150219           chain (kksc:kctr:kprg:kftc) TITPT01L;
221100150219         ENDIF;
221200150219       //?copia verso offerta di cliente codificato
221300150219         IF  *in02;
221400150219           chain (kksc:kctr:kprg:kftc) TIOPT01L;
221500150219         ENDIF;
221600150219       //?non trovo la tariffa vado via
221700150219         IF  not %found;
221800150219           leavesr;
221900150219         ENDIF;
222000150219         dtpt01 = TPTflo;
222100150219         IF  wVMA;
222200150219           §TPTvma = 'N';
222300150219         ELSE;
222400150219           clear §TPTvma;
222500150219         ENDIF;
222600150219         TPTflo = dtpt01;
222700150219       //?copia verso tariffa
222800150219         IF  not *in02;
222900150219           update  TITPT000;
223000150219         ENDIF;
223100150219       //?copia verso offerta di cliente codificato
223200150219         IF  *in02;
223300150219           update  TIOPT;
223400150219         ENDIF;
223500150219
223600150219       ENDSR;
223700050715
223800050715      *------------------------------------------------------------------------*
223900050715      * ROUTINE INIZIALE
224000050715      *------------------------------------------------------------------------*
224100050715     c     *Inzsr        BegSr
224200050715
224300050715     c     *Entry        Plist
224400050715     c                   Parm                    Kpjba
224500050715     c                   Parm                    Tnta25ds
224600050715     c                   Parm                    Tnta25tad
224700050715     c                   Parm                    Tnta25tgc
224800050715     c                   Parm                    Tnta25tpd
224900050715     c                   Parm                    Tnta25tpt
225000050715
225100050715      * se richiamato da Tnta01r carico gli scaglioni ISTAT
225200050802if  1c                   If        ta25ta01 = '1'
225300101015     c                   Clear                   savrct
225400101015
225500101015      /free
225600101015       //?Carico scatti ISTAT
225700101015         SISsca = 0;
225800101015         clear DIA;
225900101015         setll (SISsca) TISIS01L;
226000101015
226100101015         DOW  not $EoF;
226200101015
226300101015           read TISIS01L;
226400101015
226500101015           IF  %Eof(TISIS01L);
226600101015             $EoF = *on;
226700101015             leave;
226800101015           ENDIF;
226900101015
227000101015           DIA(SISsca) = SISdata;
227100101015
227200101015         ENDDO;
227300101015
227400101015      /end-free
227500101015
227600050715      * verifico se esiste lo scaglione ISTAT in base alla data odierna
227700101015do  2c                   Do        999           pi
227800050715     c                   If        *date <= dia(pi)
227900101015     c                   Eval      savrct = pi
228000100723      * appena trovo la prima data maggiore o uguale ad oggi esco dal do
228100100723     c                   leave
228200050715     c                   EndIf
228300050715    2c                   EndDo
228400050715      * se non trovo lo scaglione errore al tnta01r
228500101015     c                   If        savrct = *Zeros
228600050715     c                   Move      'IST'         kpjbu
228700050715     c                   Goto      Fine
228800050715     c                   EndIf
228900050715    1c                   EndIf
229000050729
229100050729      * Aggancio la tabella 2G per recuperare i giorni massimi
229200050729      * di rientro
229300050729     c                   Clear                   ds2g
229400050729     c                   Eval      kcod = '2G'
229500050729     c                   Eval      kkey = '1'
229600050803     c     kTabel01      Chain     Tabel00f
229700050729     c                   If        %Found(Tabel00f) and tblflg = *Blanks
229800050729     c                   Eval      ds2g = tbluni
229900050729     c                   Else
230000050729     c                   z-add     99            §2ggre
230100050729     c                   z-add     99            §2ggdp
230200050729     c                   z-add     99            §2ggfe
230300050729     c                   EndIf
230400050715
230500050715      * se copia di tutto il dettaglio tariffario la ds relativa la pulisco
230600050715     c                   If        ta25tad = 'T'
230700050715     c                   Clear                   Tnta25tad
230800050715     c                   EndIf
230900050715
231000050715      * se copia di tutte le tariffe particolari la ds relativa la pulisco
231100050715     c                   If        ta25tpt = 'T'
231200050715     c                   Clear                   Tnta25tpt
231300050715     c                   Clear                   Tnta25tpd
231400050715     c                   EndIf
231500050715
231600050715      * se copia delle tariffe di giacenza senza vis. la ds relativa la pulisco
231700050715     c                   If        ta25tgc = 'T'
231800050715     c                   Clear                   Tnta25tgc
231900050715     c                   EndIf
232000050715
232100050715      * se copia verso tariffa apro TITAV
232200050715     c                   If        ta25tipn = 'T'
232300050715     c                   Open      Titav01l
232400050715     c                   EndIf
232500050715
232600050715      * Copio da OFFERTA
232700050715     c                   Eval      *In01 = (ta25tipo = 'O')
232800050715      * Copio  a OFFERTA
232900050715     c                   Eval      *In02 = (ta25tipn = 'O')
233000050715      * Copio verso un nuovo progressivo
233100050715     c                   Eval      *In03 = (ta25prgn = *Blanks)
233200050715      * Copia da tariffa di cartello
233300050715     c                   If        Not *In01
233400050715     c                   Movel     ta25ksco      w005a
233500050715     c                   Eval      *In04 = (w005a = '88888')
233600050715     c                   EndIf
233700050715      * Copia  a tariffa di cartello
233800050715     c                   If        Not *In02
233900050715     c                   Movel     ta25kscn      w005a
234000050715     c                   Eval      *In05 = (w005a = '88888')
234100050715     c                   EndIf
234200050715      * sk dei codici di tassazione da includere o da omettere
234300050715     c                   Movea     skcts         wcts
234400050715     c                   If        ta25io2 = 'O' and wcts <> *Blanks
234500050715     c                   Eval      wscts = *On
234600050715     c                   EndIf
234700050715      * scaglione da includere o da omettere
234800050715     c                   If        ta25io3 = 'O' and ta25sgl > *Zeros
234900050715     c                   Eval      wssgl = *On
235000050715     c                   EndIf
235100061012      * se duplica dettaglio tariffario controllo se c'è il codice di tassazione
235200061012     c                   if        ta25dupd = 'S'
235300061012     c                   eval      *in07 = (ta25octs <> *blanks)
235400061012     c                   endif
235500061012      * se c'è il codice di tassazione cerco il relativo codice di ordinamento
235600061012     c                   if        *in07
235700061012      * prima il vecchio codice di tassazione
235800061012     c                   clear                   dsct
235900061012     c                   clear                   worpo
236000061012     c                   eval      kcod = 'CT'
236100061012     c                   eval      kkey = ta25octs
236200061012     c     ktabel01      chain     tabel00f
236300061012     c                   if        %found(tabel00f) and tblflg = *blanks
236400061012     c                   eval      dsct = tbluni
236500061012     c                   endif
236600061012     c                   move      §ctcor        worpo
236700061012      * prima il vecchio codice di tassazione
236800061012     c                   clear                   dsct
236900061012     c                   clear                   worpn
237000061012     c                   eval      kcod = 'CT'
237100061012     c                   eval      kkey = ta25ncts
237200061012     c     ktabel01      chain     tabel00f
237300061012     c                   if        %found(tabel00f) and tblflg = *blanks
237400061012     c                   eval      dsct = tbluni
237500061012     c                   endif
237600061012     c                   move      §ctcor        worpn
237700061012     c                   endif
237800050715
237900050715     c     *dtaara       define    §azute        azuteds
238000050715     c     *dtaara       define    §datiute      ddatiute
238100050715     c                   in(E)     *dtaara
238200050715     c                   If        %error  or RSUT = *blanks
238300050715     c                   Clear                   Tibs34ds
238400050715     c                   Call      'TIBS34R'
238500050715     c                   Parm                    Tibs34ds
238600050715     c                   In        *dtaara
238700050715     c                   EndIf
238800050715
238900050715      * Controllo se sono in sede
239000050715     c                   Eval      *In12 = (simfel = *zeros)
239100050715      * se non sono in sede apro i file delle offerte
239200050715     c                   If        Not *In12
239300050715     c                   Open      Tnofm01l
239400050715     c                   Open      Tiofd01l
239500050715     c                   Open      Tiopt01l
239600050715     c                   Open      Tiopd01l
239700050715     c                   Open      Tiogc01l
239800101222     c                   Open      Tivis05l
239900101222     c                   Open      Tivof01l
240000050715     c                   EndIf
240100050715
240200050801     c                   Clear                   comlnp
240300050801
240400051019      * se non sto copiando verso una tariffa di cartello
240500051019      * cerco la lnp del cliente
240600051019if  1c                   If        Not *In05
240700050715      * copia verso tariffa
240800050715     c  n02              Movel     ta25kscn      comlnp
240900050715      * copia verso offerta
241000050715if  2c                   If        *In02
241100101222     c     ta25kscn      Chain     Tivis05l
241200090916if  3c                   If        %Found
241300050715      *         offerta di cliente codificato
241400050715if  4c                   If        visksc > *Zeros
241500050715     c                   Movel     visksc        comlnp
241600050715      *         offerta di cliente non codificato
241700050715   x4c                   Else
241800050715     c                   Movel     viscmm        comlnp
241900050715    4c                   EndIf
242000050715    3c                   EndIf
242100050715    2c                   EndIf
242200050715    1c                   EndIf
242300050715
242400050715      * KLIST
242500050803     c     kTabel        klist
242600050805     c                   kfld                    kkut
242700050805     c                   kfld                    kcod1
242800050805
242900050805     c     kTabel1       klist
243000050805     c                   kfld                    kkut
243100050805     c                   kfld                    kcod1
243200050805     c                   kfld                    kkey1
243300050803
243400050803     c     kTabel01      klist
243500050715     c                   kfld                    kkut
243600050715     c                   kfld                    kcod
243700050715     c                   kfld                    kkey
243800050715
243900050715     c     kTntam        klist
244000050715     c                   kfld                    kksc
244100050715     c                   kfld                    kctr
244200050715
244300050715     c     kTntam01      klist
244400050715     c                   kfld                    kksc
244500050715     c                   kfld                    kctr
244600050715     c                   kfld                    kprg
244700050805
244800050805     c     kTitad        klist
244900050805     c                   kfld                    kksc
245000050805     c                   kfld                    kctr
245100050805     c                   kfld                    kprg
245200050805     c                   kfld                    klnp
245300061012
245400061012     c     kTitad01      klist
245500061012     c                   kfld                    kksc
245600061012     c                   kfld                    kctr
245700061012     c                   kfld                    kprg
245800061012     c                   kfld                    klnp
245900061012     c                   kfld                    korp
246000050715
246100050805     c     kTitad04      klist
246200050715     c                   kfld                    kksc
246300050715     c                   kfld                    kctr
246400050715     c                   kfld                    kprg
246500050715     c                   kfld                    klnp
246600050715     c                   kfld                    korp
246700050715     c                   kfld                    knaz
246800050715     c                   kfld                    kcap
246900050715     c                   kfld                    ksgl
247000050725
247100050728     c     kTitpt        klist
247200050725     c                   kfld                    kksc
247300050725     c                   kfld                    kctr
247400050725     c                   kfld                    kprg
247500050725     c                   kfld                    kftc
247600050728
247700050728     c     kTitpd        klist
247800050728     c                   kfld                    kksc
247900050728     c                   kfld                    kctr
248000050728     c                   kfld                    kprg
248100050728     c                   kfld                    kftc
248200050728     c                   kfld                    korp
248300050728     c                   kfld                    ksgl
248400050715
248500050715     c     kTitav        klist
248600050715     c                   kfld                    tavksc
248700050715     c                   kfld                    tavctr
248800050715     c                   kfld                    tavprg
248900050715     c                   kfld                    tavtrd
249000050715     c                   kfld                    tavftc
249100050715     c                   kfld                    tavcta
249200050715     c                   kfld                    tavdav
249300050715
249400050715     c                   EndSr
249500050715      *------------------------------------------------------------------------*
249600050705
249700050705** MSG  Lungh. 78                                                            *
249800050713Nuova Tariffa/Offerta già presente                                            01
249900050713Tariffa/Offerta da cui copiare non trovata                                    02
250000050714Dettaglio tariffario già presente                                             03
250100050725Tariffe particolari già presenti                                              04
250200050727Dettaglio della tariffa particolare    già presente                           05
250300050729Tariffe di giacenza già presenti                                              06
250400061012ATTENZIONE !! Non sono state duplicate le tariffe con il C.A.P. inserito      07
