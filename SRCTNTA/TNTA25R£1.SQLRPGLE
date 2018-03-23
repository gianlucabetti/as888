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
002801140109     FTidpb01l  IF   E           K DISK
002900050705
003000050705      *------------------------------------------------------------------------*
003100050705      *  RIEPILOGO INDICATORI
003200050705      *------------------------------------------------------------------------*
003300050712      * 01 - Copio da Offerta  N01 - Copio da Tariffa
003400050712      * 02 - Copio  a Offerta  N02 - Copio  a Tariffa
003500050712      * 03 - Copio verso un nuovo progressivo
003600050714      * 04 - Copia da tariffa di cartello
003700050714      * 05 - Copia  a tariffa di cartello
003800050714      * 06 - Nuova tariffa è a valore
003900061012      * 07 - Duplica dettaglio con codice di tassazione
004000090916      * 08 - richiamato da trattative
004100050715      * 30 - Comodo
004200050705      *------------------------------------------------------------------------*
004300050705
004400050705      *   V A R I A B I L I
004500050715     d comlnp          s                   like(tadlnp)
004600050714     d kcap            s                   like(tadcap)
004700050713     d kcod            s                   like(tblcod)
004800050805     d kcod1           s                   like(tblcod)
004900050713     d kctr            s                   like(tamctr)
005000050725     d kftc            s                   like(tptftc)
005100050713     d kkey            s                   like(tblkey)
005200050805     d kkey1           s                   like(tblkey)
005300050713     d kksc            s                   like(tamksc)
005400050713     d kkut            s                   like(tblkut) inz(1)
005500050714     d klnp            s                   like(tadlnp)
005600050714     d knaz            s                   like(tadnaz)
005700050714     d korp            s                   like(tadorp)
005800050713     d kprg            s                   like(tamprg)
005900050714     d ksgl            s                   like(tadsgl)
006000120124     d pi              s              4  0
006100101015     d savrct          s                   like(tamrct)
006200050802     d savin01         s              1    inz('0')
006300050802     d savin04         s              1    inz('0')
006400061012     d wcap            s              1    inz('0')
006500050715     d wcts            s             20
006600050713     d wdata           s               d   datfmt(*iso)
006700050817     d wftc            s                   like(tptftc)
006800050713     d wnewprg         s                   like(tamprg)
006900050715     d wokcts          s              1    inz('0')
007000050715     d woklnp          s              1    inz('0')
007100050715     d wokrec          s              1    inz('0')
007200050715     d woksgl          s              1    inz('0')
007300061012     d worpo           s                   like(tadorp)
007400061012     d worpn           s                   like(tadorp)
007500050715     d wscts           s              1    inz('0')
007600050715     d wssgl           s              1    inz('0')
007700050713     d wtamdst         s                   like(tamdst)
007800050715     d wtav            s              1    inz('0')
007900050727     d wtpg            s                   like(tpttpg)
008000050714     d w001a           s              1
008100050715     d w002a           s              2
008200050714     d w005a           s              5
008300050714     d w0050           s              5  0
008400050728     d w0080           s              8  0
008500050713     d w0140           s             14  0
008600050713     d w0150           s             15  0
008700050713     d w0151           s             15  1
008800050713     d w0152           s             15  2
008900050713     d w0153           s             15  3
009000050920     d xx              s              3  0
009100101015     d $EoF            s              1n   inz(*off)
009200050819
009300050705      *   S C H I E R E
009400050802     d msg             s             78    Dim(10) ctdata perrcd(1)
009500101015     d dia             s              8  0 Dim(999)
009600080609      * schiera filiali abilitate al lasciato avviso
009700080609     d sklav           s              3  0 dim(999)
009800050705
009900050705      *   D S   I N T E R N E / E S T E R N E
010000050705
010100050705     d Azuteds       e ds                  Extname(Azute00f)
010200050705     d dDatiute      e ds
010300050715     d dsct          e ds
010400050706     d dsta01        e ds
010500050727     d dstr          e ds
010600050725     d ds1p          e ds
010700050729     d ds2g          e ds
010800050705     d Kpjba         e ds
010900050708     d Tnta25ds      e ds
011000050921     d  skcts                 41     54    dim(7)
011100050921     d  sklnpo                69    218  0 dim(50)
011200050921     d  sklnpn               219    368  0 dim(50)
011300050921     d  sktpt                369    468    dim(50)
011400050714     d Tnta25tad     e ds
011500050714     d Tnta25tgc     e ds
011600050714     d Tnta25tpd     e ds
011700050714     d Tnta25tpt     e ds
011800050801     d Tnta31ds      e ds
011900050801     d Tnta32ds      e ds
012000050725     d Tibs02ds      e ds
012100050714     d Tibs34ds      e ds
012200050802     d Trul27ds      e ds
012300080609     d dlav          e ds
012301140227     d dtpt01        e ds
012400080609     d tntbeds       e ds                  extname(tntbe00f)
012500050713
012600050713      * Reperimento dati del PGM
012700050713     d pgmstatus      sds
012800050713     d job_name              244    253
012900050713     d user                  254    263
013000090916
013100090916     itivis000
013200090916     i              visesi                      wisesi
013300050705
013400050705      *------------------------------------------------------------------------*
013500080609
013600080609     c/exec sql
013700080609     c+ set option dynusrprf = *owner, closqlcsr = *endmod
013800080609     c/end-exec
013900080609
014000080609      * carico tabella filiali attive al lasciato avviso
014100080609     c                   exsr      carlav
014200050727
014300050727      * Controllo subito se la nuova tariffa è una tariffa a valore
014400050727     c                   Movel     ta25ctrn      w001a
014500050727     c                   ExSr      Sr_Chkctr
014600050727     c                   Eval      *In06 = (§trtap = 'S')
014700050819
014800050713      * Copio il TNTAM
014900050801if  1c                   If        ta25tam <> 'N'
015000050713     c                   ExSr      Sr_Tntam
015100050725      * Aggancio il TNTAM
015200050725   x1c                   Else
015300050725     c                   Clear                   dsta01
015400050725     c                   Eval      kksc = ta25kscn
015500050725     c                   Move      ta25ctrn      kctr
015600050725     c                   Move      ta25prgn      kprg
015700050817     c  n02kTntam01      Chain(n)  Tntam01l
015800050817     c   02kTntam01      Chain(n)  Tnofm01l
015900050725     c                   If        %Found
016000050725     c                   Eval      dsta01 = tamflo
016100050725     c                   EndIf
016200050725    1c                   EndIf
016300050713
016400050713      * Copio il TITAD
016500050713     c                   If        ta25tad <> 'N'
016600050713     c                   ExSr      Sr_Titad
016700050713     c                   EndIf
016800050725
016900050725      * Copio il TITPT
017000050725     c                   If        ta25tpt <> 'N'
017100050725     c                   ExSr      Sr_Titpt
017200050725     c                   EndIf
017300050725
017400050725      * Copio il TITPD
017500050725     c                   If        ta25tpd <> 'N'
017600050725     c                   ExSr      Sr_Titpd
017700050725     c                   EndIf
017800050729
017900050729      * Copio il TITGC
018000050729     c                   If        ta25tgc <> 'N'
018100050729     c                   ExSr      Sr_Titgc
018200050729     c                   EndIf
018300050801
018400050801      * Copio i dati tipo
018500050801     c                   If        ta25cat <> 'N'
018600050801     c                   ExSr      Sr_Cat
018700050801     c                   EndIf
018800050801
018900050801      * Integro a totale la tariffa
019000080521     c**!!!              If        ta25inglo <> 'N'
019100080521     c                   If        ta25inglo = 'S'
019200050801     c                   ExSr      Sr_Inglo
019300050801     c                   EndIf
019400080521      * Integro la tariffa con la tariffa 'f' = Fuel
019500080521     c                   if        ta25inglo = 'f'
019600080522     c                   exsr      sr_fuel
019700080522     c                   endif
019800061012
019900061012      * Duplica dettaglio da Tnta01r2 F13
020000061012     c                   If        ta25dupd = 'S'
020100061012     c                   ExSr      Sr_Duptad
020200061012     c                   EndIf
020300050706
020400050819     c     Fine          Tag
020500050713
020600050713     c                   Eval      *InLr = *On
020700050712
020800050712      *------------------------------------------------------------------------*
020900050712      * CREO TNTAM
021000050712      *------------------------------------------------------------------------*
021100050712     c     Sr_Tntam      BegSr
021200050713
021300050713     c                   Clear                   wtamdst
021400050713     c                   Eval      kksc = ta25kscn
021500050713     c                   Move      ta25ctrn      kctr
021600050713
021700050713      * Controllo se esistono progressivi precedenti a quello da creare
021800050713     c  n02kTntam        Setgt     Tntam01l
021900101222     c** 02
022000101222     c*nn08kTntam        Setgt     Tnofm01l
022100101222     c** 08
022200101222     c   02kTntam        Setgt     Tivof01l
022300050818     c     leggi         tag
022400051125     c  n02kTntam        Readpe(n) Tntam01l
022500101222     c** 02
022600101222     c**n08kTntam        Readpe(n) Tnofm01l
022700101222     c** 08
022800101222     c   02kTntam        Readpe(n) Tivof01l
022900050713      * se fine file vuol dire che non ho trovato nessun progressivo
023000050713      * precedente
023100050921if  1c                   If        %eof
023200050713     c                   Clear                   wnewprg
023300050921   x1c                   Else
023400050818      * se il progressivo appena letto è annullato torno a leggere
023500050818     c                   If        tamatb <> *Blanks
023600050818     c                   Goto      leggi
023700050818     c                   EndIf
023800050713      * se non è fine file incremento il progressivo
023900101222     c**n08              Eval      wnewprg = tamprg + 1
024000101222     c** 08              Eval      wnewprg = vofprg + 1
024100101222     c  n02              Eval      wnewprg = tamprg + 1
024200101222     c   02              Eval      wnewprg = vofprg + 1
024300050713      * mi salvo la data scadenza
024400101222     c**n08              Eval      wtamdst = tamdst
024500101222     c** 08              clear                   wtamdst
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
033700101222     c**                 if        *in02 and *in08
033800101222     c                   if        *in02
033900090916     c                   clear                   tivof000
034000090916     c                   eval      vofnrv = ta25kscn
034100090916     c                   move      ta25ctrn      vofctr
034200090916     c                   eval      vofprg = %dec(wnewprg:1:0)
034300090916     c****                         %dec(%subst(%editc(wnewprg:'X'):3:1):1:0)
034400090916     c                   select
034500090916     c                   when      §tadpd = 'S'
034600090916     c                   eval      voftpt = 'D'
034700090916     c                   when      §tafed = 'S'
034800090916     c                   eval      voftpt = 'F'
034900090916     c                   other
035000090916     c                   eval      voftpt = tamfie
035100090916     c                   endsl
035200100120      * valorizzo data presentazione offerta con la data del giorno
035300100120     c                   eval      vofdpo = *date
035400090916     c                   write     tivof000
035500090916     c                   endif
035600050714
035700050715      * --> SCRIVO TITAV se copia verso tariffa
035800050714if  1c                   If        Not *In02
035900050713     c                   Clear                   Titav000
036000050715     c                   Eval      tavksc = tamksc
036100050715     c                   Eval      tavctr = tamctr
036200050715     c                   Eval      tavprg = tamprg
036300050715     c                   Eval      tavtrd = 'TES'
036400050715     c                   Eval      tavcta = 'D'
036500050715     c                   Eval      tavdav = *date
036600050713     c                   Time                    w0140
036700050715     c                   Movel     w0140         tavorv
036800050715     c                   Eval      tavnoj = job_name
036900050715     c                   Eval      tavpru = user
037000050715     c                   Eval      tavdtr = *date
037100050713     c                   Write     Titav000
037200050714    1c                   EndIf
037300050713
037400050712     c                   EndSr
037500050713
037600050713      *------------------------------------------------------------------------*
037700050713      * CREO TITAD
037800050713      *------------------------------------------------------------------------*
037900050713     c     Sr_Titad      BegSr
038000050714
038100050715      * --> COPIA A DETTAGLIO
038200050714if  1c                   If        ta25tad = 'D'
038300050715      * controllo che non esista già un record uguale
038400050714     c                   Eval      kksc = ta25kscn
038500050714     c                   Eval      kctr = ta25ctrn
038600050802     c  n03              Move      ta25prgn      kprg
038700050802     c   03              Move      ta25prg       kprg
038800050920     c                   Eval      klnp = ta25lnprn
038900050714     c                   Eval      korp = ta25orpr
039000050714     c                   Eval      knaz = ta25nazr
039100050714     c                   Eval      kcap = ta25capr
039200050714     c                   Eval      ksgl = ta25sglr
039300050805     c  n02kTitad04      Chain     Titad04l
039400050805     c   02kTitad04      Chain     Tiofd01l
039500050714if  2c                   If        %Found
039600050714      * esiste e non è annullata esco dal pgm con msg di errore
039700050729     c                   If        tadatb = *Blanks
039800050714     c                   Eval      ta25err = '1'
039900050714     c                   Eval      ta25msg = msg(03)
040000050715     c                   Eval      ta25tad = 'N'
040100050715     c                   Eval      ta25tpt = 'N'
040200050715     c                   Eval      ta25tgc = 'N'
040300050715     c                   Eval      ta25cat = 'N'
040400050714     c                   Goto      Fine
040500050714     c                   EndIf
040600050714      * esiste ma è annullata la cancello
040700050714     c  n02              Delete    titad000
040800050714     c   02              Delete    tiofd
040900050714    2c                   EndIf
041000050715
041100050715      * scrivo il nuovo dettaglio in base a quanto passato nella tnta25tad
041200050714     c                   Clear                   tadatb
041300050714     c                   Eval      tadksc = ta25kscn
041400050714     c                   Eval      tadctr = ta25ctrn
041500050802     c  n03              Move      ta25prgn      tadprg
041600050802     c   03              Move      ta25prg       tadprg
041700050920     c                   Eval      tadlnp = ta25lnprn
041800050714     c                   Eval      tadcts = ta25ctsr
041900050714     c                   Eval      tadnaz = ta25nazr
042000050714     c                   Eval      tadcap = ta25capr
042100050714     c                   Eval      tadsgl = ta25sglr
042200050714     c                   Eval      taditr = ta25itrr
042300050714     c                   Eval      tadino = ta25inor
042400050714     c                   Eval      tadrpv = ta25rpvr
042500050714     c                   Eval      tadorp = ta25orpr
042600050714     c                   Eval      tadmin = ta25minr
042700050714     c                   Eval      tadmax = ta25maxr
042800050714     c                   Clear                   tadftr
042900050714     c                   Eval      taddtr = *date
043000050714      * prima della write eseguo gli arrotondamenti
043100050803     c                   If        ta25sad = 'S'
043200050714     c                   ExSr      Sr_Arrtad
043300050803     c                   EndIf
043400050714     c  n02              Write     titad000
043500050714     c   02              Write     tiofd
043600050804
043700050715      * --> SCRIVO TITAV se copia verso tariffa
043800050804      *                  e se non è già stato scritto in un richiamo precedente
043900050912if  2c                   If        Not *In02
044000050715     c                   Eval      wtav = *Off
044100050715     c                   Eval      tavksc = tamksc
044200050715     c                   Eval      tavctr = tamctr
044300050715     c                   Eval      tavprg = tamprg
044400050715     c                   Eval      tavtrd = 'DET'
044500050715     c                   Eval      tavcta = 'D'
044600050715     c                   Eval      tavdav = *date
044700050715      *            Controllo se non è già stato scritto in precedenza
044800050715     c     kTitav        Setll     Titav01l
044900050715do  3c                   Do        *Hival
045000050715     c     kTitav        Reade     Titav01l
045100050715     c                   If        %Eof(Titav01l)
045200050715     c                   Leave
045300050715     c                   EndIf
045400050715if  4c                   If        tavnoj = job_name and tavpru = user
045500050715     c                   Eval      wtav = *On
045600050715     c                   Leave
045700050715    4c                   EndIf
045800050715    3c                   EndDo
045900050715      *            Non è stato scritto scrivo ora
046000050715if  3c                   If        wtav = *Off
046100050715     c                   Clear                   Titav000
046200050715     c                   Eval      tavksc = tamksc
046300050715     c                   Eval      tavctr = tamctr
046400050715     c                   Eval      tavprg = tamprg
046500050715     c                   Eval      tavtrd = 'DET'
046600050715     c                   Eval      tavcta = 'D'
046700050715     c                   Eval      tavdav = *date
046800050715     c                   Time                    w0140
046900050715     c                   Movel     w0140         tavorv
047000050715     c                   Eval      tavnoj = job_name
047100050715     c                   Eval      tavpru = user
047200050715     c                   Eval      tavdtr = *date
047300050715     c                   Write     Titav000
047400050715    3c                   EndIf
047500050715    2c                   EndIf
047600050714    1c                   EndIf
047700050714
047800050715      * --> COPIA TOTALE
047900050715if  1c                   If        ta25tad = 'T'
048000050715      * controllo che non esista già
048100050715     c                   Eval      kksc = ta25kscn
048200050715     c                   Eval      kctr = ta25ctrn
048300050802     c  n03              Move      ta25prgn      kprg
048400050802     c   03              Move      ta25prg       kprg
048500050921      * Per aggiunta nuovo progressivo da tnta01r leggo tutta la tariffa
048600050922      * o se sto copiando da cartello a cartello
048700061012if  2c                   If        ta25ta01 = '1' or
048800061012     c                             (*In04 and *In05)
048900050921     c     kTntam01      Setll     Titad04l
049000050921do  3c                   Do        *Hival
049100050921     c     kTntam01      Reade     Titad04l
049200050921     c                   If        %Eof
049300050921     c                   Leave
049400050921     c                   EndIf
049500050921      * esiste e non è annullata esco dal pgm con msg di errore
049600050921if  4c                   If        tadatb = *Blanks
049700050921     c                   Eval      ta25err = '1'
049800050921     c                   Eval      ta25msg = msg(03)
049900050921     c                   Eval      ta25tad = 'N'
050000050921     c                   Eval      ta25tpt = 'N'
050100050921     c                   Eval      ta25tgc = 'N'
050200050921     c                   Eval      ta25cat = 'N'
050300050921     c                   Goto      Fine
050400050921    4c                   EndIf
050500050921      * esiste ma è annullata la cancello
050600050921     c                   Delete    titad000
050700050921    3c                   EndDo
050800050921   x2c                   Else
050900050920      * Ciclo per le linee di partenza
051000050921do  3c                   Do        50            xx
051100050920     c                   If        sklnpn(xx) = *Zeros
051200050920     c                   Leave
051300050920     c                   EndIf
051400050920     c                   Eval      klnp = sklnpn(xx)
051500050805     c  n02kTitad        Setll     Titad04l
051600050805     c   02kTitad        Setll     Tiofd01l
051700050921do  4c                   Do        *Hival
051800050805     c  n02kTitad        Reade     Titad04l
051900050805     c   02kTitad        Reade     Tiofd01l
052000050920     c                   If        %Eof
052100050715     c                   Leave
052200050715     c                   EndIf
052300050715      * Controllo le inclusioni/omissioni
052400050728     c                   ExSr      Sr_Seltad
052500050921if  5c                   If        wokrec = *On
052600050715     c                   Iter
052700050921    5c                   EndIf
052800050715      * esiste e non è annullata esco dal pgm con msg di errore
052900050921if  5c                   If        tadatb = *Blanks
053000050715     c                   Eval      ta25err = '1'
053100050715     c                   Eval      ta25msg = msg(03)
053200050715     c                   Eval      ta25tad = 'N'
053300050715     c                   Eval      ta25tpt = 'N'
053400050715     c                   Eval      ta25tgc = 'N'
053500050715     c                   Eval      ta25cat = 'N'
053600050715     c                   Goto      Fine
053700050921    5c                   EndIf
053800050715      * esiste ma è annullata la cancello
053900050715     c  n02              Delete    titad000
054000050715     c   02              Delete    tiofd
054100050921    4c                   EndDo
054200050921    3c                   EndDo
054300050921    2c                   EndIf
054400050715
054500050715      * Aggancio il dettaglio della tariffa da cui copiare
054600050715     c                   Eval      kksc = ta25ksco
054700050715     c                   Move      ta25ctro      kctr
054800050715     c                   Move      ta25prgo      kprg
054900050921      * Per aggiunta nuovo progressivo da tnta01r leggo tutta la tariffa
055000050922      * o se sto copiando da cartello a cartello
055100050922if  2c                   If        ta25ta01 = '1' or
055200050922     c                             (*In04 and *In05)
055300050921     c     kTntam01      Setll     Titad04l
055400050921do  3c                   Do        *Hival
055500051125     c     kTntam01      Reade(n)  Titad04l
055600050921     c                   If        %Eof
055700050921     c                   Leave
055800050921     c                   EndIf
055900050921      * escludo annullati
056000050921     c                   If        tadatb <> *Blanks
056100050921     c                   Iter
056200050921     c                   EndIf
056300050921      * scrivo il nuovo dettaglio
056400050921     c                   Eval      tadksc = ta25kscn
056500050921     c                   Eval      tadctr = ta25ctrn
056600050921     c  n03              Move      ta25prgn      tadprg
056700050921     c   03              Move      ta25prg       tadprg
056800050921     c                   Clear                   tadftr
056900050921     c                   Eval      taddtr = *date
057000050921     c                   Write     titad000
057100050921    3c                   EndDo
057200050921   x2c                   Else
057300050920      * Ciclo per le linee di partenza
057400050921do  3c                   Do        50            xx
057500050920     c                   If        sklnpo(xx) = *Zeros
057600050920     c                   Leave
057700050920     c                   EndIf
057800050920     c                   Eval      klnp = sklnpo(xx)
057900050921     c  n01kTitad        Setll     Titad04l
058000050921     c   01kTitad        Setll     Tiofd01l
058100050921do  4c                   Do        *Hival
058200051125     c  n01kTitad        Reade(n)  Titad04l
058300051125     c   01kTitad        Reade(n)  Tiofd01l
058400050715     c                   If        %Eof
058500050715     c                   Leave
058600050715     c                   EndIf
058700050715      * escludo annullati
058800050715     c                   If        tadatb <> *Blanks
058900050715     c                   Iter
059000050715     c                   EndIf
059100050715      * Controllo le inclusioni/omissioni
059200050728     c                   ExSr      Sr_Seltad
059300050921if  5c                   If        wokrec = *On
059400050715     c                   Iter
059500050921    5c                   EndIf
059600050715      * scrivo il nuovo dettaglio
059700050715     c                   Eval      tadksc = ta25kscn
059800050715     c                   Eval      tadctr = ta25ctrn
059900050802     c  n03              Move      ta25prgn      tadprg
060000050802     c   03              Move      ta25prg       tadprg
060100050920     c                   Move      sklnpn(xx)    tadlnp
060200050715     c                   Clear                   tadftr
060300050715     c                   Eval      taddtr = *date
060400050715      * prima della write eseguo gli arrotondamenti
060500050803     c                   If        ta25sad = 'S'
060600050715     c                   ExSr      Sr_Arrtad
060700050803     c                   EndIf
060800050715     c  n02              Write     titad000
060900050715     c   02              Write     tiofd
061000050921    4c                   EndDo
061100050921    3c                   EndDo
061200050921    2c                   EndIf
061300050804
061400050715      * --> SCRIVO TITAV se copia verso tariffa
061500050912if  2c                   If        Not *In02
061600050715     c                   Clear                   Titav000
061700050715     c                   Eval      tavksc = tamksc
061800050715     c                   Eval      tavctr = tamctr
061900050715     c                   Eval      tavprg = tamprg
062000050715     c                   Eval      tavtrd = 'DET'
062100050715     c                   Eval      tavcta = 'D'
062200050715     c                   Eval      tavdav = *date
062300050715     c                   Time                    w0140
062400050715     c                   Movel     w0140         tavorv
062500050715     c                   Eval      tavnoj = job_name
062600050715     c                   Eval      tavpru = user
062700050715     c                   Eval      tavdtr = *date
062800050715     c                   Write     Titav000
062900050715    2c                   EndIf
063000050715    1c                   EndIf
063100050713
063200050713     c                   EndSr
063300050725
063400050725      *------------------------------------------------------------------------*
063500050725      * CREO TITPT
063600050725      *------------------------------------------------------------------------*
063700050725     c     Sr_Titpt      BegSr
063800050725
063900050725      * --> COPIA A DETTAGLIO
064000050725if  1c                   If        ta25tpt = 'D'
064100050725      * controllo che non esista già un record uguale
064200050725     c                   Eval      kksc = ta25kscn
064300050725     c                   Eval      kctr = ta25ctrn
064400050802     c  n03              Move      ta25prgn      kprg
064500050802     c   03              Move      ta25prg       kprg
064600050727     c                   Eval      kftc = ta25ftcr
064700050727     c  n02kTitpt        Chain     Titpt01l
064800050727     c   02kTitpt        Chain     Tiopt01l
064900050725if  2c                   If        %Found
065000050725      * esiste e non è annullata esco dal pgm con msg di errore
065100050802      * se non sto inglobando
065200050803if  3c                   If        tptatb = *Blanks and
065300080521     c**!!!                        ta25inglo <> 'S'
065400080521     c                             ta25inglo = 'N'
065500050725     c                   Eval      ta25err = '1'
065600050727     c                   Eval      ta25msg = msg(04)
065700050725     c                   Eval      ta25tpt = 'N'
065800050725     c                   Eval      ta25tgc = 'N'
065900050725     c                   Eval      ta25cat = 'N'
066000050725     c                   Goto      Fine
066100050803    3c                   EndIf
066200050803      * esiste e non è annullata e sto inglobando esco dalla routine
066300050803if  3c                   If        tptatb = *Blanks and
066400080521     c**!!!                        ta25inglo = 'S'
066500080521     c                             ta25inglo <> 'N'
066600050803     c                   Leavesr
066700050803    3c                   EndIf
066800050725      * esiste ma è annullata la cancello
066900050802     c                   If        tptatb <> *Blanks
067000050727     c  n02              Delete    titpt000
067100050727     c   02              Delete    tiopt
067200050802     c                   EndIf
067300050725    2c                   EndIf
067400050725
067500050727      * prima di scrivere la testata della tariffa particolare vado a scrivere
067600050727      * TUTTO il dettaglio della tariffa particolare
067700050805      * se non ho chiesta la copia del dettaglio delle tariffe particolari
067800050922      * o se sto inglobando
067900050816     c                   If        ta25tpd = 'N' or
068000050805     c                             ta25inglo = 'S'
068100050727     c                   Eval      ta25tpd = 'T'
068200050727     c                   Eval      wtpg = ta25tpgr
068300050817     c                   Eval      wftc = ta25ftcr
068400050727     c                   ExSr      Sr_Titpd
068500050803     c                   Eval      ta25tpd = 'N'
068600050805     c                   EndIf
068700050727
068800050727      * scrivo la tariffa particolare in base a quanto passato nella tnta25tpt
068900050727     c                   Clear                   tptatb
069000050727     c                   Eval      tptksc = ta25kscn
069100050727     c                   Eval      tptctr = ta25ctrn
069200050802     c  n03              Move      ta25prgn      tptprg
069300050802     c   03              Move      ta25prg       tptprg
069400050727     c                   Eval      tptduv = *date
069500050727     c                   Eval      tptftc = ta25ftcr
069600050727     c                   Eval      tpttpg = ta25tpgr
069700050727     c                   Eval      tptarl = ta25arlr
069800050727     c                   Eval      tptarf = ta25arfr
069900050727     c                   Eval      tptaro = ta25aror
070000050727     c                   Eval      tptrpv = ta25rpvrt
070100050727     c                   Eval      tptvlm = ta25vlmr
070200050727     c                   Eval      tptvvm = ta25vvmr
070300050727     c                   Eval      tptfvm = ta25fvmr
070400050727     c                   Eval      tpttma = ta25tmar
070500050727     c                   Eval      tptapl = ta25aplr
070600060802     c                   eval      tptdpb = ta25dpbr
070700060803      * la data inserimento devo aggiornarla con la data del giorno
070800060803     c                   eval      tptdit = *date
070900060802     c                   eval      tptflo = ta25flor
071000050727     c                   Clear                   tptftr
071100050727     c                   Eval      tptdtr = *date
071200050727      * se tariffa a valore (tipo 4-T-V) arrotondo
071300050727     c                   Eval      w001a = tpttpg
071400050727     c                   ExSr      Sr_Chkctr
071500050803if  2c                   If        §trtap = 'S' and ta25sad = 'S'
071600050727     c                   Z-add     tptvlm        w0153
071700050727     c                   ExSr      Sr_Arroto
071800050727     c                   Z-add     w0153         tptvlm
071900050727    2c                   EndIf
072000050727     c  n02              Write     titpt000
072100050727     c   02              Write     tiopt
072200050804
072300050725      * --> SCRIVO TITAV se copia verso tariffa
072400050912if  2c                   If        Not *In02
072500050727     c                   Clear                   Titav000
072600050725     c                   Eval      tavksc = tamksc
072700050725     c                   Eval      tavctr = tamctr
072800050725     c                   Eval      tavprg = tamprg
072900050727     c                   Eval      tavtrd = 'PAR'
073000050725     c                   Eval      tavcta = 'D'
073100050727     c                   Eval      tavftc = tptftc
073200050725     c                   Eval      tavdav = *date
073300050725     c                   Time                    w0140
073400050725     c                   Movel     w0140         tavorv
073500050725     c                   Eval      tavnoj = job_name
073600050725     c                   Eval      tavpru = user
073700110913      * se sto inglobando metto 'I'
073800110913     c                   If        ta25inglo <> 'N'
073900110913     c                   Eval      tavcta = 'I'
074000110913      * se sto inglobando la Q = ZTL metto come per il fuel la A di automatico
074100120312     c***                if        tptftc = 'Q '
074200120312     c***                eval      %subst(tavpru:10:1) = 'A'
074300120312     c***                endif
074400110913     c                   EndIf
074500050725     c                   Eval      tavdtr = *date
074600050725     c                   Write     Titav000
074700050725    2c                   EndIf
074800050725    1c                   EndIf
074900050725
075000050725      * --> COPIA TOTALE
075100050725if  1c                   If        ta25tpt = 'T'
075200050725      * controllo che non esista già
075300050725     c                   Eval      kksc = ta25kscn
075400050725     c                   Eval      kctr = ta25ctrn
075500050802     c  n03              Move      ta25prgn      kprg
075600050802     c   03              Move      ta25prg       kprg
075700050725     c  n02kTntam01      Setll     Titpt01l
075800050725     c   02kTntam01      Setll     Tiopt01l
075900050725do  2c                   Do        *Hival
076000050725     c  n02kTntam01      Reade     Titpt01l
076100050725     c   02kTntam01      Reade     Tiopt01l
076200050725if  3c                   If        %Eof
076300050725     c                   Leave
076400050725    3c                   EndIf
076500050725      * controllo se la tariffa particolare è gestibile o meno
076600050725     c                   Eval      w002a = tptftc
076700050725     c                   ExSr      Sr_Chktpt
076800050725if  3c                   If        wokrec = *On
076900050725     c                   Iter
077000050725    3c                   EndIf
077100050921      * se non è aggiunta tariffa da tnta01r
077200050922      * o se non è copia da cartello a cartello
077300050921      * controllo se è una delle tariffe particolari da copiare
077400050922if  3c                   If        ta25ta01 <> '1' and Not *In04 and
077500050922     c                             Not *In05
077600050921     c     tptftc        Lookup    sktpt                                  30
077700050921      * non è presente nella sk torno a leggere
077800050921     c  n30              Iter
077900050921    3c                   EndIf
078000050725      * esiste e non è annullata esco dal pgm con msg di errore
078100050729if  3c                   If        tptatb = *Blanks
078200050725     c                   Eval      ta25err = '1'
078300050725     c                   Eval      ta25msg = msg(04)
078400050725     c                   Eval      ta25tpt = 'N'
078500050725     c                   Eval      ta25tgc = 'N'
078600050725     c                   Eval      ta25cat = 'N'
078700050725     c                   Goto      Fine
078800050725    3c                   EndIf
078900050725      * esiste ma è annullata la cancello
079000050725     c  n02              Delete    titpt000
079100050725     c   02              Delete    tiopt
079200050725    2c                   EndDo
079300050725
079400050725      * Aggancio le tariffe particolari da cui copiare
079500050725     c                   Eval      kksc = ta25ksco
079600050725     c                   Move      ta25ctro      kctr
079700050725     c                   Move      ta25prgo      kprg
079800050725     c  n01kTntam01      Setll     Titpt01l
079900050725     c   01kTntam01      Setll     Tiopt01l
080000050725do  2c                   Do        *Hival
080100051125     c  n01kTntam01      Reade(n)  Titpt01l
080200051125     c   01kTntam01      Reade(n)  Tiopt01l
080300050725     c                   If        %Eof
080400050725     c                   Leave
080500050725     c                   EndIf
080600050725      * escludo annullati
080700050725     c                   If        tptatb <> *Blanks
080800050725     c                   Iter
080900050725     c                   EndIf
081000050725      * controllo se la tariffa particolare è gestibile o meno
081100050725     c                   Eval      w002a = tptftc
081200050725     c                   ExSr      Sr_Chktpt
081300050725if  3c                   If        wokrec = *On
081400050725     c                   Iter
081500050725    3c                   EndIf
081600050921      * se non è aggiunta tariffa da tnta01r
081700050921      * controllo se è una delle tariffe particolari da copiare
081800050921if  3c                   If        ta25ta01 <> '1'
081900050921     c     tptftc        Lookup    sktpt                                  30
082000050921      * non è presente nella sk torno a leggere
082100050921     c  n30              Iter
082200050921    3c                   EndIf
082300050727
082400050727      * prima di scrivere la testata della tariffa particolare vado a scrivere
082500050727      * TUTTO il dettaglio della tariffa particolare
082600050727     c                   Eval      ta25tpd = 'T'
082700050803     c                   Eval      wtpg = tpttpg
082800050817     c                   Eval      wftc = tptftc
082900050727     c                   ExSr      Sr_Titpd
083000050803     c                   Eval      ta25tpd = 'N'
083100050725
083200050727      * scrivo la nuova tariffa particolare
083300050727     c                   Eval      tptksc = ta25kscn
083400050727     c                   Eval      tptctr = ta25ctrn
083500050802     c  n03              Move      ta25prgn      tptprg
083600050802     c   03              Move      ta25prg       tptprg
083700050727     c                   Eval      tptduv = *date
083800061130     c                   eval      tptdit = *date
083900050727     c                   Clear                   tptftr
084000050727     c                   Eval      tptdtr = *date
084100050727      * se tariffa a valore (tipo 4-T-V) arrotondo
084200050727     c                   Eval      w001a = tpttpg
084300050727     c                   ExSr      Sr_Chkctr
084400050803if  3c                   If        §trtap = 'S' and ta25sad = 'S'
084500050727     c                   Z-add     tptvlm        w0153
084600050727     c                   ExSr      Sr_Arroto
084700050727     c                   Z-add     w0153         tptvlm
084800050727    3c                   EndIf
084900050803     c  n02              Write     titpt000
085000050803     c   02              Write     tiopt
085100050804
085200050725      * --> SCRIVO TITAV se copia verso tariffa
085300050912if  3c                   If        Not *In02
085400050725     c                   Clear                   Titav000
085500050725     c                   Eval      tavksc = tamksc
085600050725     c                   Eval      tavctr = tamctr
085700050725     c                   Eval      tavprg = tamprg
085800050727     c                   Eval      tavtrd = 'PAR'
085900050725     c                   Eval      tavcta = 'D'
086000050727     c                   Eval      tavftc = tptftc
086100050725     c                   Eval      tavdav = *date
086200050725     c                   Time                    w0140
086300050725     c                   Movel     w0140         tavorv
086400050725     c                   Eval      tavnoj = job_name
086500050725     c                   Eval      tavpru = user
086600050725     c                   Eval      tavdtr = *date
086700050725     c                   Write     Titav000
086800050804    3c                   EndIf
086900050804    2c                   EndDo
087000050725    1c                   EndIf
087100050725
087200050725     c                   EndSr
087300050727
087400050727      *------------------------------------------------------------------------*
087500050727      * CREO TITPD
087600050727      *------------------------------------------------------------------------*
087700050727     c     Sr_Titpd      BegSr
087800050727
087900050727      * --> COPIA A DETTAGLIO
088000050727if  1c                   If        ta25tpd = 'D'
088100050727      * controllo che non esista già un record uguale
088200050727     c                   Eval      kksc = ta25kscn
088300050727     c                   Eval      kctr = ta25ctrn
088400050802     c  n03              Move      ta25prgn      kprg
088500050802     c   03              Move      ta25prg       kprg
088600050728     c                   Eval      kftc = ta25ftcrd
088700050728     c                   Eval      korp = ta25orprd
088800050728     c                   Eval      ksgl = ta25sglrd
088900050728     c  n02kTitpd        Chain     Titpd01l
089000050728     c   02kTitpd        Chain     Tiopd01l
089100050727if  2c                   If        %Found
089200050727      * esiste e non è annullata esco dal pgm con msg di errore
089300050802     c                   If        tpdatb = *Blanks
089400050727     c                   Eval      ta25err = '1'
089500050728     c                   Eval      ta25msg = msg(05)
089600050728     c                   Eval      %subst(ta25msg:37:2) = ta25ftcrd
089700050727     c                   Eval      ta25tpt = 'N'
089800050727     c                   Eval      ta25tgc = 'N'
089900050727     c                   Eval      ta25cat = 'N'
090000050727     c                   Goto      Fine
090100050727     c                   EndIf
090200050727      * esiste ma è annullata la cancello
090300050728     c  n02              Delete    titpd000
090400050728     c   02              Delete    tiopd
090500050727    2c                   EndIf
090600050727
090700050728      * scrivo il nuovo dettaglio in base a quanto passato nella tnta25tpd
090800050728     c                   Clear                   tpdatb
090900050728     c                   Eval      tpdksc = ta25kscn
091000050728     c                   Eval      tpdctr = ta25ctrn
091100050802     c  n03              Move      ta25prgn      tpdprg
091200050802     c   03              Move      ta25prg       tpdprg
091300050728     c                   Eval      tpdftc = ta25ftcrd
091400050728     c                   Eval      tpdcts = ta25ctsrd
091500050728     c                   Eval      tpdsgl = ta25sglrd
091600050728     c                   Eval      tpditr = ta25itrrd
091700050728     c                   Eval      tpdorp = ta25orprd
091800050728     c                   Eval      tpdmin = ta25minrd
091900050728     c                   Eval      tpdmax = ta25maxrd
092000050728     c                   Eval      tpdain = ta25ainr
092100050728     c                   Clear                   tpdftr
092200050728     c                   Eval      tpddtr = *date
092300050727      * prima della write eseguo gli arrotondamenti
092400050803     c                   If        ta25sad = 'S'
092500050728     c                   ExSr      Sr_Arrtpd
092600050803     c                   EndIf
092700050728     c  n02              Write     titpd000
092800050728     c   02              Write     tiopd
092900050804
093000050727      * --> SCRIVO TITAV se copia verso tariffa
093100050804      *                  e se non è già stato scritto in un richiamo precedente
093200050912if  2c                   If        Not *In02
093300050727     c                   Eval      wtav = *Off
093400050727     c                   Eval      tavksc = tamksc
093500050727     c                   Eval      tavctr = tamctr
093600050727     c                   Eval      tavprg = tamprg
093700050728     c                   Eval      tavtrd = 'PAR'
093800050728     c                   Eval      tavftc = tpdftc
093900050727     c                   Eval      tavcta = 'D'
094000050727     c                   Eval      tavdav = *date
094100050727      *            Controllo se non è già stato scritto in precedenza
094200050727     c     kTitav        Setll     Titav01l
094300050727do  3c                   Do        *Hival
094400050727     c     kTitav        Reade     Titav01l
094500050727     c                   If        %Eof(Titav01l)
094600050727     c                   Leave
094700050727     c                   EndIf
094800050727if  4c                   If        tavnoj = job_name and tavpru = user
094900050727     c                   Eval      wtav = *On
095000050727     c                   Leave
095100050727    4c                   EndIf
095200050727    3c                   EndDo
095300050727      *            Non è stato scritto scrivo ora
095400050727if  3c                   If        wtav = *Off
095500050727     c                   Clear                   Titav000
095600050727     c                   Eval      tavksc = tamksc
095700050727     c                   Eval      tavctr = tamctr
095800050727     c                   Eval      tavprg = tamprg
095900050728     c                   Eval      tavtrd = 'PAR'
096000050727     c                   Eval      tavcta = 'D'
096100050728     c                   Eval      tavftc = tpdftc
096200050727     c                   Eval      tavdav = *date
096300050727     c                   Time                    w0140
096400050727     c                   Movel     w0140         tavorv
096500050727     c                   Eval      tavnoj = job_name
096600050727     c                   Eval      tavpru = user
096700050727     c                   Eval      tavdtr = *date
096800050727     c                   Write     Titav000
096900050727    3c                   EndIf
097000050727    2c                   EndIf
097100050727    1c                   EndIf
097200050727
097300050727      * --> COPIA TOTALE
097400050727if  1c                   If        ta25tpd = 'T'
097500050727      * controllo che non esista già
097600050727     c                   Eval      kksc = ta25kscn
097700050727     c                   Eval      kctr = ta25ctrn
097800050802     c  n03              Move      ta25prgn      kprg
097900050802     c   03              Move      ta25prg       kprg
098000050817     c                   Eval      kftc = wftc
098100050727     c  n02kTitpt        Setll     Titpd01l
098200050727     c   02kTitpt        Setll     Tiopd01l
098300050727do  2c                   Do        *Hival
098400050727     c  n02kTitpt        Reade     Titpd01l
098500050727     c   02kTitpt        Reade     Tiopd01l
098600050727if  3c                   If        %Eof
098700050727     c                   Leave
098800050727     c                   EndIf
098900050728      * Controllo le inclusioni/omissioni
099000050728     c                   ExSr      Sr_Seltpd
099100050728if  3c                   If        wokrec = *On
099200050728     c                   Iter
099300050728    3c                   EndIf
099400050727      * controllo se il dettaglio della tariffa particolare è da copiare o no
099500050727     c                   Eval      w002a = tpdcts
099600050727     c                   ExSr      Sr_Chktpd
099700050727if  3c                   If        wokrec = *On
099800050727     c                   Iter
099900050727    3c                   EndIf
100000050727      * esiste e non è annullata esco dal pgm con msg di errore
100100050802      * se non sto inglobando
100200050803if  3c                   If        tpdatb = *Blanks and
100300080521     c**!!!                        ta25inglo <> 'S'
100400080521     c                             ta25inglo = 'N'
100500050727     c                   Eval      ta25err = '1'
100600050727     c                   Eval      ta25msg = msg(05)
100700050803     c                   Eval      %subst(ta25msg:37:2) = tptftc
100800050727     c                   Eval      ta25tpt = 'N'
100900050727     c                   Eval      ta25tgc = 'N'
101000050727     c                   Eval      ta25cat = 'N'
101100050727     c                   Goto      Fine
101200050727    3c                   EndIf
101300050803      * esiste e non è annullata e sto inglobando leggo rcd sucessivo
101400050803if  3c                   If        tpdatb = *Blanks and
101500080521     c**!!!                        ta25inglo = 'S'
101600080521     c                             ta25inglo <> 'N'
101700050803     c                   Iter
101800050803    3c                   EndIf
101900050727      * esiste ma è annullata la cancello
102000050802     c                   If        tpdatb <> *Blanks
102100050727     c  n02              Delete    titpd000
102200050727     c   02              Delete    tiopd
102300050802     c                   EndIf
102400050727    2c                   EndDo
102500050727
102600050727      * Aggancio il dettaglio della tariffa particolare da cui copiare
102700050727     c                   Eval      kksc = ta25ksco
102800050727     c                   Move      ta25ctro      kctr
102900050727     c                   Move      ta25prgo      kprg
103000050817     c                   Eval      kftc = wftc
103100050727     c  n01kTitpt        Setll     Titpd01l
103200050727     c   01kTitpt        Setll     Tiopd01l
103300050727do  2c                   Do        *Hival
103400051125     c  n01kTitpt        Reade(n)  Titpd01l
103500051125     c   01kTitpt        Reade(n)  Tiopd01l
103600050727     c                   If        %Eof
103700050727     c                   Leave
103800050727     c                   EndIf
103900050727      * escludo annullati
104000050727     c                   If        tpdatb <> *Blanks
104100050727     c                   Iter
104200050727     c                   EndIf
104300050728      * Controllo le inclusioni/omissioni
104400050728     c                   ExSr      Sr_Seltpd
104500050728if  3c                   If        wokrec = *On
104600050728     c                   Iter
104700050728    3c                   EndIf
104800050727      * controllo se il dettaglio della tariffa particolare è da copiare o no
104900050727     c                   Eval      w002a = tpdcts
105000050727     c                   ExSr      Sr_Chktpd
105100050727if  3c                   If        wokrec = *On
105200050727     c                   Iter
105300050727    3c                   EndIf
105400050727      * scrivo il nuovo dettaglio della tariffa particolare
105500050727     c                   Eval      tpdksc = ta25kscn
105600050727     c                   Eval      tpdctr = ta25ctrn
105700050802     c  n03              Move      ta25prgn      tpdprg
105800050802     c   03              Move      ta25prg       tpdprg
105900050727     c                   Clear                   tpdftr
106000050727     c                   Eval      tpddtr = *date
106100050727      * prima della write eseguo gli arrotondamenti
106200050803     c                   If        ta25sad = 'S'
106300050727     c                   ExSr      Sr_Arrtpd
106400050803     c                   EndIf
106500050727     c  n02              Write     titpd000
106600050727     c   02              Write     tiopd
106700050727    2c                   EndDo
106800050727      * --> NON SCRIVO TITAV xchè lo scrivo una volta solo dopo che ho scritto
106900050727      *                      la testata della tariffa particolare (TITPT)
107000050727    1c                   EndIf
107100050727
107200050727     c                   EndSr
107300050729
107400050729      *------------------------------------------------------------------------*
107500050729      * CREO TITGC
107600050729      *------------------------------------------------------------------------*
107700050729     c     Sr_Titgc      BegSr
107800050729
107900050729      * --> COPIA A DETTAGLIO
108000050729if  1c                   If        ta25tgc = 'D'
108100050729      * controllo che non esista già un record uguale
108200050729     c                   Eval      kksc = ta25kscn
108300050729     c                   Eval      kctr = ta25ctrn
108400050802     c  n03              Move      ta25prgn      kprg
108500050802     c   03              Move      ta25prg       kprg
108600050729     c  n02kTntam01      Chain     Titgc01l
108700050729     c   02kTntam01      Chain     Tiogc01l
108800050729if  2c                   If        %Found
108900050729      * esiste e non è annullata esco dal pgm con msg di errore
109000050729     c                   If        tgcatb = *Blanks
109100050729     c                   Eval      ta25err = '1'
109200050729     c                   Eval      ta25msg = msg(06)
109300050729     c                   Eval      ta25tgc = 'N'
109400050729     c                   Eval      ta25cat = 'N'
109500050729     c                   Goto      Fine
109600050729     c                   EndIf
109700050729      * esiste ma è annullata la cancello
109800050729     c  n02              Delete    titgc000
109900050729     c   02              Delete    tiogc
110000050729    2c                   EndIf
110100050729
110200050729      * scrivo il nuovo dettaglio in base a quanto passato nella tnta25tgc
110300050729     c                   Clear                   tgcatb
110400050729     c                   Eval      tgcksc = ta25kscn
110500050729     c                   Eval      tgcctr = ta25ctrn
110600050802     c  n03              Move      ta25prgn      tgcprg
110700050802     c   03              Move      ta25prg       tgcprg
110800050729     c                   Eval      tgcsgv = ta25sgvr
110900050729     c                   Eval      tgcsgr = ta25sgrr
111000050729     c                   Eval      tgcsgp = ta25sgpr
111100050729     c                   Eval      tgcsgd = ta25sgdr
111200050729     c                   Eval      tgcsg1 = ta25sg1r
111300050729     c                   Eval      tgcsg2 = ta25sg2r
111400050729     c                   Eval      tgcsg3 = ta25sg3r
111500050729     c                   Eval      tgcst1 = ta25st1r
111600050729     c                   Eval      tgcst2 = ta25st2r
111700050729     c                   Eval      tgcst3 = ta25st3r
111800050729     c                   Eval      tgcstm = ta25stmr
111900050729     c                   Eval      tgcrpv = ta25rpvrg
112000050729     c                   Eval      tgcfaf = ta25fafr
112100050729     c                   Eval      tgcsgf = ta25sgfr
112200050729     c                   Eval      tgcggr = ta25ggrr
112300050729     c                   Eval      tgctcm = ta25tcmr
112400050729     c                   Eval      tgctfg = ta25tfgr
112500050729     c                   Eval      tgcduv = *date
112600050729     c                   Clear                   tgcftr
112700050729     c                   Eval      tgcdtr = *date
112800050729      * prima della write eseguo gli arrotondamenti
112900050803     c                   If        ta25sad = 'S'
113000050729     c                   ExSr      Sr_Arrtgc
113100050803     c                   EndIF
113200050729     c  n02              Write     titgc000
113300050729     c   02              Write     tiogc
113400050804
113500050729      * --> SCRIVO TITAV se copia verso tariffa
113600050912if  2c                   If        Not *In02
113700050729     c                   Clear                   Titav000
113800050729     c                   Eval      tavksc = tamksc
113900050729     c                   Eval      tavctr = tamctr
114000050729     c                   Eval      tavprg = tamprg
114100050729     c                   Eval      tavtrd = 'GIA'
114200050729     c                   Eval      tavcta = 'D'
114300050729     c                   Eval      tavdav = *date
114400050729     c                   Time                    w0140
114500050729     c                   Movel     w0140         tavorv
114600050729     c                   Eval      tavnoj = job_name
114700050729     c                   Eval      tavpru = user
114800050729     c                   Eval      tavdtr = *date
114900050729     c                   Write     Titav000
115000050804    2c                   EndIf
115100050729    1c                   EndIf
115200050729
115300050729      * --> COPIA TOTALE
115400050729if  1c                   If        ta25tgc = 'T'
115500050729      * controllo che non esista già
115600050729     c                   Eval      kksc = ta25kscn
115700050729     c                   Eval      kctr = ta25ctrn
115800050802     c  n03              Move      ta25prgn      kprg
115900050802     c   03              Move      ta25prg       kprg
116000050729     c  n02kTntam01      Chain     Titgc01l
116100050729     c   02kTntam01      Chain     Tiogc01l
116200050729if  2c                   If        %Found
116300050729      * esiste e non è annullata esco dal pgm con msg di errore
116400050802      * se non sto inglobando
116500050803if  3c                   If        tgcatb = *Blanks and
116600080521     c**!!!                        ta25inglo <> 'S'
116700080521     c                             ta25inglo = 'N'
116800050729     c                   Eval      ta25err = '1'
116900050729     c                   Eval      ta25msg = msg(06)
117000050729     c                   Eval      ta25tgc = 'N'
117100050729     c                   Eval      ta25cat = 'N'
117200050729     c                   Goto      Fine
117300050803    3c                   EndIf
117400050803      * esiste e non è annullata e sto inglobando esco dalla ruotine
117500050803if  3c                   If        tgcatb = *Blanks and
117600080521     c**!!!                        ta25inglo = 'S'
117700080521     c                             ta25inglo <> 'N'
117800050803     c                   Leavesr
117900050803    3c                   EndIf
118000050729      * esiste ma è annullata la cancello
118100050802     c                   If        tgcatb <> *Blanks
118200050729     c  n02              Delete    titgc000
118300050729     c   02              Delete    tiogc
118400050802     c                   EndIf
118500050729    2c                   EndIf
118600050729
118700050729      * Aggancio le tariffe di giacenza da cui copiare
118800050729     c                   Eval      kksc = ta25ksco
118900050729     c                   Move      ta25ctro      kctr
119000050729     c                   Move      ta25prgo      kprg
119100051125     c  n01kTntam01      Chain(n)  Titgc01l
119200051125     c   01kTntam01      Chain(n)  Tiogc01l
119300050729      * scrivo il nuovo dettaglio
119400050729     c                   Eval      tgcksc = ta25kscn
119500050729     c                   Eval      tgcctr = ta25ctrn
119600050802     c  n03              Move      ta25prgn      tgcprg
119700050802     c   03              Move      ta25prg       tgcprg
119800050729     c                   Eval      tgcduv = *date
119900050729     c                   Clear                   tgcftr
120000050729     c                   Eval      tgcdtr = *date
120100050729      * prima della write eseguo gli arrotondamenti
120200050803     c                   If        ta25sad = 'S'
120300050729     c                   ExSr      Sr_Arrtgc
120400050803     c                   EndIf
120500050729      * se copia da tariffa di cartello
120600050802if  2c                   If        *In04
120700050729      * i giorni massimi di rientro per una tariffa estera vengono recuperati
120800050729      * dalla tabella 2G
120900050729     c                   If        tamfie = 'E'
121000050729     c                   Eval      tgcggr = §2ggre
121100050729     c                   EndIf
121200050729      * i giorni massimi di rientro per una tariffa FEDEX vengono recuperati
121300050729      * dalla tabella 2G
121400050729     c                   If        §tafed = 'S'
121500050729     c                   Eval      tgcggr = §2ggfe
121600050729     c                   EndIf
121700050729      * i giorni massimi di rientro per una tariffa DPD vengono recuperati
121800050729      * dalla tabella 2G
121900050729     c                   If        §tadpd = 'S'
122000050729     c                   Eval      tgcggr = §2ggdp
122100050729     c                   EndIf
122200050802    2c                   EndIf
122300050729     c  n02              Write     titgc000
122400050729     c   02              Write     tiogc
122500050729      * --> SCRIVO TITAV se copia verso tariffa
122600050912if  2c                   If        Not *In02
122700050729     c                   Clear                   Titav000
122800050729     c                   Eval      tavksc = tamksc
122900050729     c                   Eval      tavctr = tamctr
123000050729     c                   Eval      tavprg = tamprg
123100050729     c                   Eval      tavtrd = 'GIA'
123200050729     c                   Eval      tavcta = 'D'
123300050802      * se sto inglobando metto 'I'
123400050802     c                   If        ta25inglo <> 'N'
123500050802     c                   Eval      tavcta = 'I'
123600050802     c                   EndIf
123700050729     c                   Eval      tavdav = *date
123800050729     c                   Time                    w0140
123900050729     c                   Movel     w0140         tavorv
124000050729     c                   Eval      tavnoj = job_name
124100050729     c                   Eval      tavpru = user
124200050729     c                   Eval      tavdtr = *date
124300050729     c                   Write     Titav000
124400050729    2c                   EndIf
124500050729    1c                   EndIf
124600050729
124700050729     c                   EndSr
124800050801
124900050801      *------------------------------------------------------------------------*
125000050801      * CREO DATI TIPO
125100050801      *------------------------------------------------------------------------*
125200050801     c     Sr_Cat        BegSr
125300050802
125400050802      * I due pgm che vengono richiamti per la copia del CAT non prevedono
125500050802      * nessun tipo di errore di ritorno.....
125600050801
125700050801      * Se copia verso offerta
125800050801if  1c                   If        *In02
125900050801     c                   Clear                   tnta32ds
126000050801      * --> copio da tariffa
126100050801if  2c                   If        Not *In01
126200050801     c                   Eval      d32cto = 'T'
126300050801   x2c                   Else
126400050801      * --> copio da offerta
126500050801     c                   Eval      d32cto = 'O'
126600050801    2c                   EndIf
126700061009     c                   Eval      d32dsf = 'S'
126800050801     c                   Eval      d32tla = 'L'
126900050801     c                   Eval      d32ksc = ta25ksco
127000050801     c                   Eval      d32ctr = ta25ctro
127100050801     c                   Eval      d32prg = ta25prgo
127200050801     c                   Eval      d32div = 'EUR'
127300050801     c                   Eval      d32nrv = ta25kscn
127400050801     c                   Eval      d32ct2 = ta25ctrn
127500100310     c  n03              move      ta25prgn      d32pr2
127600100310     c   03              move      ta25prg       d32pr2
127700050801     c                   Eval      d32din = 'EUR'
127800050801     c                   Eval      d32lnp = comlnp
127900050801     c                   Eval      kpjbu = tnta32ds
128000050801     c                   Call      'TNTA32C'
128100050801     c                   Parm                    kpjba
128200050801    1c                   EndIf
128300050801
128400050801      * Se copia verso tariffa
128500050801if  1c                   If        Not *In02
128600050801     c                   Clear                   tnta31ds
128700050801     c                   Eval      d31tla = 'L'
128800050801     c                   Eval      d31ksc = ta25ksco
128900050801     c                   Eval      d31ctr = ta25ctro
129000050801     c                   Eval      d31prg = ta25prgo
129100050801     c                   Eval      d31div = 'EUR'
129200050801     c                   Eval      d31ksn = ta25kscn
129300050801     c                   Eval      d31ctn = ta25ctrn
129400050802     c  n03              Move      ta25prgn      d31prn
129500050802     c   03              Move      ta25prg       d31prn
129600050801     c                   Eval      d31din = 'EUR'
129700050801     c                   Eval      kpjbu = tnta31ds
129800050801     c                   Call      'TNTA31R'
129900050801     c                   Parm                    kpjba
130000050801    1c                   EndIf
130100050801
130200050801     c                   EndSr
130300050801
130400050801      *------------------------------------------------------------------------*
130500050801      * INTEGRO A TOTALE LA TARIFFA/OFFERTA
130600050801      *------------------------------------------------------------------------*
130700050801     c     Sr_Inglo      BegSr
130800050802
130900050802     c                   Eval      savin01 = *In01
131000050802     c                   Eval      savin04 = *In04
131100050801
131200050802      * Cerco la cartello da cui copiare
131300050802     c                   ExSr      Sr_Tarcar
131400070214
131500070214      * Accendo l'indicatore 04 per indicare che sto copiando da cartello
131600070214     c                   eval      *in04 = *on
131700061011
131800061011      * se non ho ricevuto la tariffa particolare da duplicare
131900061011      * devo inglobare tutte quelle che sono indicate in tabella
132000061011      * altrimenti è una duplica tariffe particolari di cartello
132100061011    0c                   if        ta25ftcr = *blanks
132200050801
132300050801      * Leggo la tabella 1P per recuperare solo le tariffe particolari
132400050801      * da inglobare dalla cartello
132500050801     c                   Clear                   ds1p
132600050805     c                   Eval      kcod1 = '1P'
132700050801     c     kTabel        Setll     Tabel00f
132800050801if  1c                   Do        *Hival
132900050801     c     kTabel        Reade     Tabel00f
133000050801      * Fine file
133100050801     c                   If        %Eof(Tabel00f)
133200050801     c                   Leave
133300050801     c                   EndIf
133400050801      * Escludo annullati
133500050801     c                   If        tblflg <> *Blanks
133600050801     c                   Iter
133700050801     c                   EndIf
133800050801     c                   Eval      ds1p = tbluni
133900050801      * Inglobo solo le tariffe particolari che hanno flag = 'I'
134000050801s   2c                   select
134100050801      * Tariffa con tipo servizio poste
134200050801     c                   When      tamtsp = 'P' and §1pfpt <> 'I'
134300050801     c                   Iter
134400050801      * Tariffa con Network DPD
134500050801     c                   When      §tadpd = 'S' and §1pfdp <> 'I'
134600050801     c                   Iter
134700050801      * Tariffa con Network FedEx
134800050801     c                   When      §tafed = 'S' and §1pffe <> 'I'
134900050801     c                   Iter
135000050801      * Tariffa Italia
135100050801     c                   When      tamfie = 'I' and §1ptco <> 'I'
135200050801     c                   Iter
135300050801      * Per tariffa Estero controllo flag §1pfie
135400050801     c                   When      tamfie = 'E' and §1pfie <> 'I'
135500050801     c                   Iter
135600050801      * Utilizzabile in tariffa
135700050802     c                   When      §1puta <> 'S'
135800050801     c                   Iter
135900050801      * Tariffa particolare espresso e la tariffa ha tipo servizio espresso
136000050801      * non devo inglobare
136100090929      * EDPES --> dal 6/10/09 si ingloba sempre
136200090929     c*****              When      %subst(tblkey:1:1) = 'E' and tamtsp = 'E'
136300090929     c*****              Iter
136400050802    2c                   EndSl
136500050923      * salvo la tariffa particolare appena letta
136600050923     c                   Eval      kkey1 = tblkey
136700050802      * Leggo la tariffa particolare da copiare dalla cartello
136800050802     c                   Eval      kksc = o27ksc
136900050802     c                   Eval      kctr = o27ctr
137000050802     c                   Move      o27prg        kprg
137100050802     c                   Eval      kftc = tblkey
137200050802     c     kTitpt        Setll     Titpt01l
137300050802do  2c                   Do        *Hival
137400050802     c     kTitpt        Reade(n)  Titpt01l
137500050802      * Fine file
137600050802     c                   If        %Eof(Titpt01l)
137700050802     c                   Leave
137800050802     c                   EndIf
137900050802      * Escludo annullati
138000050802     c                   If        tptatb <> *Blanks
138100050802     c                   Iter
138200050802     c                   EndIf
138300050802      * Imposto i dati della tnta25tpt e della tnta25ds
138400050802      * x copiare la tariffa particolare
138500050802     c                   Eval      *In01 = *Off
138600050802     c                   Eval      ta25ksco = o27ksc
138700050802     c                   Eval      ta25ctro = o27ctr
138800050802     c                   Move      o27prg        ta25prgo
138900050802     c                   Eval      ta25ftcr = tptftc
139000050802     c                   Eval      ta25tpgr = tpttpg
139100050802     c                   Eval      ta25arlr = tptarl
139200050802     c                   Eval      ta25arfr = tptarf
139300050802     c                   Eval      ta25aror = tptaro
139400050802     c                   Eval      ta25rpvrt = tptrpv
139500050802     c                   Eval      ta25vlmr = tptvlm
139600050802     c                   Eval      ta25vvmr = tptvvm
139700050802     c                   Eval      ta25fvmr = tptfvm
139800050802     c                   Eval      ta25tmar = tpttma
139900050802     c                   Eval      ta25aplr = tptapl
140000060802     c                   eval      ta25dpbr = tptdpb
140100060803      * la data inserimento devo aggiornarla con la data del giorno
140200060803     c                   eval      ta25ditr = *date
140300060802     c                   eval      ta25flor = tptflo
140400050802      * Richiamo la routine per copiare la tariffa particolare a dettaglio
140500050803     c                   Eval      ta25tpt = 'D'
140600050802     c                   ExSr      Sr_Titpt
140700050802    2c                   EndDo
140800050805      * Mi riposiziono sulla tabella 1P
140900050805     c     kTabel1       Setgt     Tabel00f
141000050805     c
141100050801    1c                   EndDo
141200050802
141300050802      * Inglobo anche la tariffa di giacenza della cartello
141400050802     c                   Eval      *In04 = *On
141500050802      * Richiamo la routine per copiare la tariffa di giacenza
141600050802     c                   Eval      ta25tgc = 'T'
141700050802     c                   Eval      *In01 = *Off
141800050802     c                   Eval      ta25ksco = o27ksc
141900050802     c                   Eval      ta25ctro = o27ctr
142000050802     c                   Move      o27prg        ta25prgo
142100050802     c                   ExSr      Sr_Titgc
142200061011
142300061011      * duplica tariffe particolari di cartello
142400061011   x0c                   else
142500061011      * leggo la tariffa particolare da copiare dalla cartello
142600061011     c                   eval      kksc = o27ksc
142700061011     c                   eval      kctr = o27ctr
142800061011     c                   move      o27prg        kprg
142900061011     c                   eval      kftc = ta25ftcr
143000061011     c     ktitpt        setll     titpt01l
143100061011do  1c                   do        *hival
143200061011     c     ktitpt        reade(n)  titpt01l
143300061011      * fine file
143400061011     c                   if        %eof(titpt01l)
143500061011     c                   leave
143600061011     c                   endif
143700061011      * escludo annullati
143800061011     c                   if        tptatb <> *blanks
143900061011     c                   iter
144000061011     c                   endif
144100061011      * imposto i dati della tnta25tpt e della tnta25ds
144200061011      * x copiare la tariffa particolare
144300061011     c                   eval      *in01 = *off
144400061011     c                   eval      ta25ksco = o27ksc
144500061011     c                   eval      ta25ctro = o27ctr
144600061011     c                   move      o27prg        ta25prgo
144700061011     c                   eval      ta25ftcr = tptftc
144800061011     c                   eval      ta25tpgr = tpttpg
144900061011     c                   eval      ta25arlr = tptarl
145000061011     c                   eval      ta25arfr = tptarf
145100061011     c                   eval      ta25aror = tptaro
145200061011     c                   eval      ta25rpvrt = tptrpv
145300061011     c                   eval      ta25vlmr = tptvlm
145400061011     c                   eval      ta25vvmr = tptvvm
145500061011     c                   eval      ta25fvmr = tptfvm
145600061011     c                   eval      ta25tmar = tpttma
145700061011     c                   eval      ta25aplr = tptapl
145800061011      * richiamo la routine per copiare la tariffa particolare a dettaglio
145900061011     c                   eval      ta25tpt = 'D'
146000061011     c                   exsr      sr_titpt
146100061011    1c                   enddo
146200061011    0c                   endif
146300050802
146400050802     c                   Eval      *In01 = savin01
146500050802     c                   Eval      *In04 = savin04
146600050801
146700050801     c                   EndSr
146800080521
146900080521      *------------------------------------------------------------------------*
147000080521      * INTEGRO LA TARIFFA/OFFERTA CON TARIFFA PARTICOLARE FUEL
147100080521      *------------------------------------------------------------------------*
147200080521     c     sr_fuel       begsr
147300080521
147400080521      * Leggo la tabella 1P per controllare se devo creare la tariffa
147500080521      * Fuel per la tariffa interessata
147600080521     c                   clear                   ds1p
147700080521     c                   eval      kcod = '1P'
147800080522     c                   eval      kkey = 'f '
147900080521     c     ktabel01      chain     tabel00f
148000080521     c                   if        %found(tabel00f) and tblflg = *blanks
148100080521     c                   eval      ds1p = tbluni
148200080521     c                   endif
148300080521      * Creo solo se è possibile
148400080522    1c                   select
148500080521      * Tariffa con tipo servizio poste
148600080522     c                   when      tamtsp = 'P' and §1pfpt = 'N'
148700080521     c                   leavesr
148800080521      * Tariffa con Network DPD
148900080522     c                   when      §tadpd = 'S' and §1pfdp = 'N'
149000080521     c                   leavesr
149100080521      * Tariffa con Network FedEx
149200080522     c                   when      §tafed = 'S' and §1pffe = 'N'
149300080521     c                   leavesr
149400080521      * Tariffa Italia
149500080522     c                   when      tamfie = 'I' and §1ptco = 'N'
149600080521     c                   leavesr
149700080521      * Per tariffa Estero controllo flag §1pfie
149800080522     c                   when      tamfie = 'E' and §1pfie = 'N'
149900080521     c                   leavesr
150000080521      * Utilizzabile in tariffa
150100080521     c                   when      §1puta <> 'S'
150200080521     c                   leavesr
150300080522    1c                   endsl
150400080521
150500080521      * controllo che non esista già la tariffa - Testata
150600080521     c                   eval      kksc = ta25kscn
150700080521     c                   eval      kctr = ta25ctrn
150800080521     c                   move      ta25prgn      kprg
150900080522     c                   eval      kftc = 'f '
151000080522     c  n02ktitpt        chain     titpt01l
151100080522     c   02ktitpt        chain     tiopt01l
151200080522    1c                   if        %found
151300080521      * esiste e non è annullata esco dalla routine
151400080522     c                   if        tptatb = *blanks
151500080521     c                   leavesr
151600080522     c                   endif
151700080521      * esiste ma è annullata la cancello
151800080522    2c                   if        tptatb <> *blanks
151900080521     c  n02              delete    titpt000
152000080521     c   02              delete    tiopt
152100080522      * e cancello anche il relativo dettaglio
152200080522     c  n02ktitpt        setll     titpd01l
152300080522     c   02ktitpt        setll     tiopd01l
152400080522    3c                   do        *hival
152500080522     c  n02ktitpt        reade     titpd01l
152600080522     c   02ktitpt        reade     tiopd01l
152700080522     c                   if        %eof
152800080522     c                   leave
152900080522     c                   endif
153000080522     c  n02              delete    titpd000
153100080522     c   02              delete    tiopd
153200080522    3c                   enddo
153300080522    2c                   endif
153400080522    1c                   endif
153500080521
153600080522      * imposto il codice tassazione e relativo ordinamento in base
153700080522      * alla tariffa italia/estera/DPD/FedEx
153800080522     c                   select
153900080522      * Tariffa con Network DPD
154000080522     c                   when      §tadpd = 'S'
154100080522     c                   eval      w002a = 'IT'
154200080522      * Tariffa con Network FedEx
154300080522     c                   when      §tafed = 'S'
154400080522     c                   eval      w002a = 'EE'
154500080522      * Tariffa Italia
154600080522     c                   when      tamfie = 'I'
154700080522     c                   eval      w002a = 'IT'
154800080522      * Tariffa Estero
154900080522     c                   when      tamfie = 'E'
155000080522     c                   eval      w002a = 'EE'
155100080522     c                   endsl
155200080522      * cerco l'ordinamento dalla tabella CT
155300080522     c                   exsr      sr_chkcts
155400080522
155500080522      * scrivo il dettaglio della tariffa particolare
155600080522      * se non esiste già
155700080522      * (forse controllo inutile visto che prima verifico che non esista
155800080522      *  già la testata, ma così evito la rottura del programma)
155900080604     c                   evalr     korp = §ctcor
156000080522     c                   eval      ksgl = 9999999999
156100080522     c  n02ktitpd        chain     titpd01l
156200080522     c   02ktitpd        chain     tiopd01l
156300080522      * scrivo il nuovo dettaglio se non esiste
156400080522    1c                   if        not %found
156500080522     c  n02              clear                   titpd000
156600080522     c   02              clear                   tiopd
156700080522     c                   eval      tpdksc = kksc
156800080522     c                   eval      tpdctr = kctr
156900080522     c                   eval      tpdprg = kprg
157000080522     c                   eval      tpdftc = kftc
157100080522     c                   eval      tpdcts = w002a
157200080522     c                   eval      tpdorp = korp
157300080522     c                   eval      tpdsgl = ksgl
157400110525     c                   eval      tpditr = 0,5
157500080522     c                   eval      tpddtr = *date
157600080522     c  n02              write     titpd000
157700080522     c   02              write     tiopd
157800080522    1c                   endif
157900080522
158000080522      * scrivo la testata della tariffa particolare
158100080522      * (qua non controllo se esiste già...l'ho già fatto prima)
158200080522     c  n02              clear                   titpt000
158300080522     c   02              clear                   tiopt
158301140227     c                   clear                   dtpt01
158400080522     c                   eval      tptksc = kksc
158500080522     c                   eval      tptctr = kctr
158600080522     c                   eval      tptprg = kprg
158700080522     c                   eval      tptduv = *date
158800080522     c                   eval      tptftc = kftc
158900080522     c                   eval      tpttpg = 'V'
159000080522     c                   eval      tptdtr = *date
159100140109      * la data riferimento prezzo base carburante viene recuperata dal file
159101140109      * di riferimento TIDPB00F
159102140109      * verifico se esiste il file del prezzo base da impostare in base alla
159103140109      * data validità altrimenti imposto la data decorrenza tariffa
159104140109     c     *date         setll     tidpb01l
159105140109     c                   read      tidpb01l
159106140109     c                   if        %found(tidpb01l)
159107140109     c                   eval      tptdpb = dpbdpb
159108140227     c                   eval      §tptpma = dpbpma
159109140109     c                   else
159110140109     c                   eval      tptdpb = tamddt
159111140227     c                   eval      §tptpma = *zeros
159112140109     c                   endif
159113140227     c                   eval      tptflo = dtpt01
159300080522      * la data inserimento = data del giorno
159400080521     c                   eval      tptdit = *date
159500080522     c  n02              write     titpt000
159600080522     c   02              write     tiopt
159700080522
159800080522      * --> SCRIVO TITAV se copia verso tariffa
159900080522    1c                   if        not *in02
160000080522     c                   clear                   titav000
160100080522     c                   eval      tavksc = tamksc
160200080522     c                   eval      tavctr = tamctr
160300080522     c                   eval      tavprg = tamprg
160400080522     c                   eval      tavtrd = 'PAR'
160500080522     c                   eval      tavftc = tpdftc
160600080522     c                   eval      tavcta = 'I'
160700080522     c                   eval      tavdav = *date
160800080522     c                   time                    w0140
160900080522     c                   movel     w0140         tavorv
161000080522     c                   eval      tavnoj = job_name
161100080522     c                   eval      tavpru = user
161200080522     c                   eval      %subst(tavpru:10:1) = 'A'
161300080522     c                   eval      tavdtr = *date
161400080522     c                   write     titav000
161500080522    1c                   endif
161600080521
161700080521     c                   endsr
161800061012
161900061012      *------------------------------------------------------------------------*
162000061012      * DUPLICA DETTAGLIO TARIFFARIO
162100061012      *------------------------------------------------------------------------*
162200061012     c     sr_duptad     begsr
162300061012
162400061012     c                   eval      wcap = *off
162500061012      * aggancio il dettaglio della tariffa da cui copiare
162600061012     c                   eval      kksc = ta25ksco
162700061012     c                   move      ta25ctro      kctr
162800061012     c                   move      ta25prgo      kprg
162900061012     c                   move      ta25olnp      klnp
163000061012    1c                   if        not *in07
163100061012     c  n01ktitad        setll     titad04l
163200061012     c   01ktitad        setll     tiofd01l
163300061012     c  n01ktitad        reade     titad04l
163400061012     c   01ktitad        reade     tiofd01l
163500061012    1c                   endif
163600061012    1c                   if        *in07
163700061012     c                   move      worpo         korp
163800061012     c  n01ktitad01      setll     titad04l
163900061012     c   01ktitad01      setll     tiofd01l
164000061012     c  n01ktitad01      reade     titad04l
164100061012     c   01ktitad01      reade     tiofd01l
164200061012    1c                   endif
164300061012    1c                   dow       not %eof
164400061012      * escludo annullati
164500061012    2c                   if        tadatb = *blanks
164600061012      * se richiesto anche il codice di tassazione controllo il cap
164700061012    3c                   if        *in07 and tadcap <> *blanks
164800061012     c                   eval      wcap = *on
164900061012     c                   goto      noduptad
165000061012    3c                   endif
165100061012      * la nuova tariffa se esiste la devo cancellare
165200061012     c                   eval      kksc = ta25kscn
165300061012     c                   move      ta25ctrn      kctr
165400061012     c                   move      ta25prgn      kprg
165500061012     c                   move      ta25nlnp      klnp
165600061012     c  n07              move      tadorp        korp
165700061012     c   07              move      worpn         korp
165800061012     c                   eval      knaz = tadnaz
165900061012     c                   eval      kcap = tadcap
166000061012     c                   eval      ksgl = tadsgl
166100061012     c  n02ktitad04      chain     titad04l
166200061012     c   02ktitad04      chain     tiofd01l
166300061012    3c                   if        %found
166400061012     c  n02              delete    titad000
166500061012     c   02              delete    tiofd
166600061012    3c                   endif
166700061012      * riaggancio il record da cui devo copiare
166800061012     c                   eval      kksc = ta25ksco
166900061012     c                   move      ta25ctro      kctr
167000061012     c                   move      ta25prgo      kprg
167100061012     c                   move      ta25olnp      klnp
167200061012     c  n07              move      tadorp        korp
167300061012     c   07              move      worpo         korp
167400061012     c  n01ktitad04      chain     titad04l
167500061012     c   01ktitad04      chain     tiofd01l
167600061012    3c                   if        %found
167700061012      * scrivo il nuovo dettaglio
167800061012     c                   eval      tadksc = ta25kscn
167900061012     c                   eval      tadctr = ta25ctrn
168000061012     c                   move      ta25prgn      tadprg
168100061012     c                   move      ta25nlnp      tadlnp
168200061012     c   07              move      ta25ncts      tadcts
168300061012     c   07              move      worpn         tadorp
168400061012     c                   clear                   tadftr
168500061012     c                   eval      taddtr = *date
168600061012     c  n02              write     titad000
168700061012     c   02              write     tiofd
168800061012    3c                   endif
168900061012    2c                   endif
169000061012     c     noduptad      tag
169100061012     c  n07
169200061012     cann01ktitad        reade     titad04l
169300061012     c  n07
169400061012     can 01ktitad        reade     tiofd01l
169500061012     c   07
169600061012     cann01ktitad01      reade     titad04l
169700061012     c   07
169800061012     can 01ktitad01      reade     tiofd01l
169900061012    1c                   enddo
170000061012
170100061012      * --> SCRIVO TITAV se copia verso tariffa
170200061012    1c                   if        not *in02
170300061012     c                   clear                   titav000
170400061012     c                   eval      tavksc = tamksc
170500061012     c                   eval      tavctr = tamctr
170600061012     c                   eval      tavprg = tamprg
170700061012     c                   eval      tavtrd = 'DET'
170800061012     c                   eval      tavcta = 'D'
170900061012     c                   eval      tavdav = *date
171000061012     c                   time                    w0140
171100061012     c                   movel     w0140         tavorv
171200061012     c                   eval      tavnoj = job_name
171300061012     c                   eval      tavpru = user
171400061012     c                   eval      tavdtr = *date
171500070220     c                   write     Titav000                             99
171600061012    1c                   endif
171700061012
171800061012      * se duplica con codice di tassazione ed ho trovato almeno un cap
171900061012      * messaggio info al chiamante
172000061012    1c                   if        wcap = *on
172100061012     c                   eval      ta25err = '1'
172200061012     c                   eval      ta25msg = msg(07)
172300061012    1c                   endif
172400061012
172500061012     c                   endsr
172600050727
172700050727      *------------------------------------------------------------------------*
172800050727      * CONTROLLO IL CODICE TARIFFA
172900050727      *------------------------------------------------------------------------*
173000050727     c     Sr_Chkctr     BegSr
173100050727
173200050727     c                   Clear                   dstr
173300050727     c                   Eval      kcod = 'TR'
173400050727     c                   Eval      kkey = w001a
173500050803     c     kTabel01      Chain     Tabel00f
173600050727     c                   If        %Found(Tabel00f) and tblflg = *Blanks
173700050727     c                   Eval      dstr = tbluni
173800050727     c                   EndIf
173900050727
174000050727     c                   EndSr
174100050725
174200050715      *------------------------------------------------------------------------*
174300050715      * CONTROLLO IL CODICE TASSAZIONE IN TABELLA
174400050715      *------------------------------------------------------------------------*
174500050715     c     Sr_Chkcts     BegSr
174600050715
174700050715     c                   Clear                   dsct
174800050715     c                   Eval      kcod = 'CT'
174900050715     c                   Eval      kkey = w002a
175000050803     c     kTabel01      Chain     Tabel00f
175100050715     c                   If        %Found(Tabel00f) and tblflg = *Blanks
175200050715     c                   Eval      dsct = tbluni
175300050715     c                   EndIf
175400050715
175500050715     c                   EndSr
175600050727
175700050727      *------------------------------------------------------------------------*
175800050727      * CONTROLLO SE LA TARIFFA PARTICOLARE E' GESTIBILE
175900050727      *------------------------------------------------------------------------*
176000050727     c     Sr_Chktpt     BegSr
176100050727
176200050727     c                   Eval      wokrec = *Off
176300050727
176400050727     c                   Clear                   ds1p
176500050727     c                   Eval      kcod = '1P'
176600050727     c                   Eval      kkey = w002a
176700050803     c     kTabel01      Chain     Tabel00f
176800050727     c                   If        %Found(Tabel00f) and tblflg = *Blanks
176900050727     c                   Eval      ds1p = tbluni
177000050727     c                   EndIf
177100050727
177200050727      * se tariffa FedEx e non sto copiando verso una tariffa di cartello e
177300050728      * la tariffa particolare FedEx è valida solo per la cartello non copio
177400050727if  1c                   If        §tafed = 'S' and Not *In05 and
177500050727     c                             §1pffe = 'C'
177600050727     c                   Eval      wokrec = *On
177700050727     c                   Leavesr
177800050727    1c                   EndIf
177900050727
178000050727      * copio solo le tariffe particolari che hanno il flag di gestione <> 'N'
178100050727s   1c                   Select
178200050727      * tipo servizio poste
178300050727     c                   When      tamtsp = 'P' and §1pfpt = 'N'
178400050727     c                   Eval      wokrec = *On
178500050727      * tariffa DPD
178600050727     c                   When      §tadpd = 'S' and §1pfdp = 'N'
178700050727     c                   Eval      wokrec = *On
178800050727      * tariffa FedEx
178900050727     c                   When      §tafed = 'S' and (§1pffe = 'N' or
179000050727     c                                               §1pffe = 'C')
179100050727     c                   Eval      wokrec = *On
179200050727      * tariffa Italia
179300050727     c                   When      tamfie = 'I' and §1ptco = 'N'
179400050727     c                   Eval      wokrec = *On
179500050727      * tariffa Estero
179600050727     c                   When      tamfie = 'E' and §1pfie = 'N'
179700050727     c                   Eval      wokrec = *On
179800050727    1c                   EndSl
179900050727
180000050727      * Controllo se tariffa particolare in scadenza da eliminare non copio
180100050728if  1c                   If        §1peli <> *Blanks
180200050728     c                   Move      §1peli        w0080
180300050728     c                   If        tamdst >= w0080
180400050727     c                   Eval      wokrec = *On
180500050728     c                   EndIf
180600050727    1c                   EndIf
180700051019
180800080609      * Se tariffa particolare 'c ' lasciato avviso e non sto copiando
180900080609      * verso una tariffa di cartello o verso un cliente relativo ad
181000080609      * una delle filiali abilitate non copio
181100080609if  1c                   If        w002a = 'c ' and Not *In05
181200080609     c                   if        %lookup(comlnp:sklav) = *zeros
181300080609     c                             and %lookup(999:sklav) = *zeros
181400051019     c                   Eval      wokrec = *On
181500080609    1c                   endif
181600051019    1c                   EndIf
181700050727     c                   EndSr
181800050727
181900050727      *------------------------------------------------------------------------*
182000050727      * CONTROLLO SE IL DETTAGLIO DELLA TARIFFA PARTICOLARE E' DA COPIARE
182100050727      *------------------------------------------------------------------------*
182200050727     c     Sr_Chktpd     BegSr
182300050727
182400050727     c                   Eval      wokrec = *Off
182500050727
182600050727     c                   ExSr      Sr_Chkcts
182700050727
182800050727      * copio solo se il codice di tassazione è congruente
182900050727if  1c                   If        (tamfie = 'I' and w002a = 'EE') or
183000050727     c                             (tamfie = 'E' and w002a = 'IT')
183100050727     c                   Eval      wokrec = *On
183200050727     c                   Leavesr
183300050727    1c                   EndIf
183400050727
183500050727      * se tariffa FedEx copio solo i codici di tassazione previsti
183600050727      * per FedEx
183700050727if  1c                   If        §tafed = 'S' and §ctutf <> 'S'
183800050727     c                   Eval      wokrec = *On
183900050727     c                   Leavesr
184000050727    1c                   EndIf
184100050727
184200050727      * se non è tariffa FedEx devo copiare solo i codici di tassazione
184300050727      * non previsti per FedEx
184400050727if  1c                   If        §tafed <> 'S' and §ctutf = 'S'
184500050727     c                   Eval      wokrec = *On
184600050727     c                   Leavesr
184700050727    1c                   EndIf
184800050727
184900050727      * se sto copiando da una tariffa di cartello
185000050727     c                   If        *In04
185100050727s   1c                   Select
185200050727      * Se tariffa estero copio solo estero
185300050727     c                   When      tamfie = 'E' and §ctest = *Blanks
185400050727     c                   Eval      wokrec = *On
185500050727      * Se tariffa DPD copio solo estero
185600050728      * tranne il codice generico IT xchè va bene
185700050727     c                   When      §tadpd = 'S' and §ctest = *Blanks and
185800050727     c                             tpdcts <> 'IT'
185900050727     c                   Eval      wokrec = *On
186000050727      * Se tariffa italia copio solo italia
186100050727     c                   When      tamfie = 'I' and §tadpd <> 'S' and
186200050727     c                                              §ctest <> *Blanks
186300050727     c                   Eval      wokrec = *On
186400050727     c                   EndSl
186500050728    1c                   EndIf
186600050727
186700050727     c                   EndSr
186800050727
186900050727      *------------------------------------------------------------------------*
187000050728      * CONTROLLO INCLUSIONI/OMISSIONI DETTAGLIO TARIFFARIO
187100050727      *------------------------------------------------------------------------*
187200050728     c     Sr_Seltad     BegSr
187300050727
187400050727     c                   Eval      wokrec = *Off
187500050729     c                   Eval      wokcts = *Off
187600050729     c                   Eval      woksgl = *Off
187700050727
187800050727      * Controllo le inclusioni
187900050727      * --> Codici di tassazione
188000050727if  1c                   If        wcts <> *Blanks
188100050727     c     tadcts        Lookup    skcts                                  30
188200050727      *     incluso
188300050727if  2c                   If        *In30
188400050727     c                   Eval      wokcts = *On
188500050727   x2c                   Else
188600050727      *     non incluso
188700050727if  3c                   If        Not *In30 and ta25io2 = 'I'
188800050727     c                   Eval      wokrec = *On
188900050727    3c                   EndIf
189000050727    2c                   EndIf
189100050727    1c                   EndIf
189200050727      * --> Scaglione
189300050727if  1c                   If        ta25sgl > *Zeros
189400050727      *     incluso
189500050727if  2c                   If        tadsgl = ta25sgl
189600050727     c                   Eval      woksgl = *On
189700050727   x2c                   Else
189800050727      *     non incluso
189900050727if  3c                   If        ta25sgl <> tadsgl and ta25io3 = 'I'
190000050727     c                   Eval      wokrec = *On
190100050727    3c                   EndIf
190200050727    2c                   EndIf
190300050727    1c                   EndIf
190400050727
190500050727      * Controllo le omissioni
190600050727      * --> Codici di tassazione
190700050729if  1c                   If        wscts = *On and wokcts = *On
190800050727     c                   Eval      wokrec = *On
190900050727    1c                   EndIf
191000050727      * --> Scaglione
191100050727if  1c                   If        wssgl = *On and woksgl = *On
191200050727     c                   Eval      wokrec = *On
191300050727    1c                   EndIf
191400050727
191500050727     c                   If        wokrec = *On
191600050727     c                   Leavesr
191700050727     c                   EndIf
191800050727
191900050727      * Se non sto creando una nuova tariffa di cartello
192000050727if  1c                   If        Not *In05
192100050727      * Controlli x copia da tariffa di cartello e copia totale
192200050727      * e non è la cartello DPD o FedEx
192300050727      * quindi questo vale per la creazione di una nuova tariffa
192400050727      * (non di cartello) copiando dalla 8888830
192500050727      * dove sono presenti sia codici di tassazione Italia che Estero
192600050727if  2c                   If        *In04 and ta25tad = 'T' and
192700050727     c                             §tadpd <> 'S' and §tafed <> 'S'
192800050727     c                   Eval      w002a = tadcts
192900050727     c                   ExSr      Sr_Chkcts
193000050727     c                   Select
193100050727      * Se tariffa estero copio solo estero
193200050727     c                   When      ta25fie = 'E' and §ctest = *Blanks
193300050727     c                   Eval      wokrec = *On
193400050727      * Se tariffa italia copio solo italia
193500050727     c                   When      ta25fie = 'I' and §ctest <> *Blanks
193600050727     c                   Eval      wokrec = *On
193700050727     c                   EndSl
193800050727     c                   If        wokrec = *On
193900050727     c                   Leavesr
194000050727     c                   EndIf
194100050727    2c                   EndIf
194200050727    1c                   EndIf
194300050727
194400050727     c                   EndSr
194500050728
194600050728      *------------------------------------------------------------------------*
194700050728      * CONTROLLO INCLUSIONI/OMISSIONI DETTAGLIO TARIFFE PARTICOLARI
194800050728      *------------------------------------------------------------------------*
194900050728     c     Sr_Seltpd     BegSr
195000050728
195100050728     c                   Eval      wokrec = *Off
195200050729     c                   Eval      woklnp = *Off
195300050729     c                   Eval      wokcts = *Off
195400050729     c                   Eval      woksgl = *Off
195500050728
195600050728      * Controllo le inclusioni
195700050728      * --> Codici di tassazione
195800050728if  1c                   If        wcts <> *Blanks
195900050728     c     tpdcts        Lookup    skcts                                  30
196000050728      *     incluso
196100050728if  2c                   If        *In30
196200050728     c                   Eval      wokcts = *On
196300050728   x2c                   Else
196400050728      *     non incluso
196500050728if  3c                   If        Not *In30 and ta25io2 = 'I'
196600050728     c                   Eval      wokrec = *On
196700050728    3c                   EndIf
196800050728    2c                   EndIf
196900050728    1c                   EndIf
197000050728      * --> Scaglione
197100050728if  1c                   If        ta25sgl > *Zeros
197200050728      *     incluso
197300050728if  2c                   If        tpdsgl = ta25sgl
197400050728     c                   Eval      woksgl = *On
197500050728   x2c                   Else
197600050728      *     non incluso
197700050728if  3c                   If        ta25sgl <> tpdsgl and ta25io3 = 'I'
197800050728     c                   Eval      wokrec = *On
197900050728    3c                   EndIf
198000050728    2c                   EndIf
198100050728    1c                   EndIf
198200050728
198300050728      * Controllo le omissioni
198400050728      * --> Codici di tassazione
198500050729if  1c                   If        wscts = *On and wokcts = *On
198600050728     c                   Eval      wokrec = *On
198700050728    1c                   EndIf
198800050728      * --> Scaglione
198900050728if  1c                   If        wssgl = *On and woksgl = *On
199000050728     c                   Eval      wokrec = *On
199100050728    1c                   EndIf
199200050728
199300050728     c                   EndSr
199400050727
199500050727      *------------------------------------------------------------------------*
199600050727      * ARROTONDAMENTI DETTAGLIO TARIFFARIO
199700050727      *------------------------------------------------------------------------*
199800050727     c     Sr_Arrtad     BegSr
199900050727
200000050727      * se tariffa a valore arrotondo solo lo scaglione
200100050727if  1c                   If        *In06
200200050727      * se non è 99999
200300050727     c                   Z-add     tadsgl        w0050
200400050727     c                   If        w0050 <> 99999
200500050727     c                   Z-add     tadsgl        w0153
200600050727     c                   ExSr      Sr_Arroto
200700050727     c                   Z-add     w0153         tadsgl
200800050727     c                   EndIf
200900050727      * altrimenti arrotondo città + inoltro
201000050727   x1c                   Else
201100050727     c                   Z-add     taditr        w0153
201200050727     c                   ExSr      Sr_Arroto
201300050727     c                   Z-add     w0153         taditr
201400050727     c                   Z-add     tadino        w0153
201500050727     c                   ExSr      Sr_Arroto
201600050727     c                   Z-add     w0153         tadino
201700050727    1c                   EndIf
201800050727
201900050727      * arrotondo sempre il minimo e il massimo
202000050727     c                   Z-add     tadmin        w0153
202100050727     c                   ExSr      Sr_Arroto
202200050727     c                   Z-add     w0153         tadmin
202300050727     c                   Z-add     tadmax        w0153
202400050727     c                   ExSr      Sr_Arroto
202500050727     c                   Z-add     w0153         tadmax
202600050727
202700050727     c                   EndSr
202800050727
202900050727      *------------------------------------------------------------------------*
203000050727      * ARROTONDAMENTI DETTAGLIO TARIFFE PARTICOLARI
203100050727      *------------------------------------------------------------------------*
203200050727     c     Sr_Arrtpd     BegSr
203300050727
203400050727      * se tariffa a valore arrotondo solo lo scaglione
203500050727     c                   Eval      w001a = wtpg
203600050727     c                   ExSr      Sr_Chkctr
203700050727if  1c                   If        §trtap = 'S'
203800050727      * se non è 99999
203900050727     c                   Z-add     tpdsgl        w0050
204000050727     c                   If        w0050 <> 99999
204100050727     c                   Z-add     tpdsgl        w0153
204200050727     c                   ExSr      Sr_Arroto
204300050727     c                   Z-add     w0153         tpdsgl
204400050727     c                   EndIf
204500050727      * altrimenti arrotondo la tariffa
204600050727   x1c                   Else
204700050727     c                   Z-add     tpditr        w0153
204800050727     c                   ExSr      Sr_Arroto
204900050727     c                   Z-add     w0153         tpditr
205000050728    1c                   EndIf
205100050727
205200050727      * arrotondo sempre il minimo e il massimo
205300050727     c                   Z-add     tpdmin        w0153
205400050727     c                   ExSr      Sr_Arroto
205500050727     c                   Z-add     w0153         tpdmin
205600050727     c                   Z-add     tpdmax        w0153
205700050727     c                   ExSr      Sr_Arroto
205800050727     c                   Z-add     w0153         tpdmax
205900050727
206000050727     c                   EndSr
206100050729
206200050729      *------------------------------------------------------------------------*
206300050729      * ARROTONDAMENTI TARIFFE DI GIACENZA
206400050729      *------------------------------------------------------------------------*
206500050729     c     Sr_Arrtgc     BegSr
206600050729
206700050729     c                   Z-add     tgcsgv        w0153
206800050729     c                   ExSr      Sr_Arroto
206900050729     c                   Z-add     w0153         tgcsgv
207000050729     c                   Z-add     tgcsgr        w0153
207100050729     c                   ExSr      Sr_Arroto
207200050729     c                   Z-add     w0153         tgcsgr
207300050729     c                   Z-add     tgcsgp        w0153
207400050729     c                   ExSr      Sr_Arroto
207500050729     c                   Z-add     w0153         tgcsgp
207600050729     c                   Z-add     tgcsgd        w0153
207700050729     c                   ExSr      Sr_Arroto
207800050729     c                   Z-add     w0153         tgcsgd
207900050729     c                   Z-add     tgcst1        w0153
208000050729     c                   ExSr      Sr_Arroto
208100050729     c                   Z-add     w0153         tgcst1
208200050729     c                   Z-add     tgcst2        w0153
208300050729     c                   ExSr      Sr_Arroto
208400050729     c                   Z-add     w0153         tgcst2
208500050729     c                   Z-add     tgcst3        w0153
208600050729     c                   ExSr      Sr_Arroto
208700050729     c                   Z-add     w0153         tgcst3
208800050729     c                   Z-add     tgcstm        w0153
208900050729     c                   ExSr      Sr_Arroto
209000050729     c                   Z-add     w0153         tgcstm
209100050729
209200050729     c                   EndSr
209300050727
209400050727      *------------------------------------------------------------------------*
209500050727      * ARROTONDAMENTI
209600050727      *------------------------------------------------------------------------*
209700050727     c     Sr_Arroto     BegSr
209800050727
209900050803if  1c                   If        w0153 <> *Zeros
210000050727     c                   Select
210100050727     c                   When      ta25dec = 0
210200050727     c                   Z-add(h)  w0153         w0150
210300050727     c                   Z-add     w0150         w0153
210400050727     c                   When      ta25dec = 1
210500050727     c                   Z-add(h)  w0153         w0151
210600050727     c                   Z-add     w0151         w0153
210700050727     c                   When      ta25dec = 2
210800050727     c                   Z-add(h)  w0153         w0152
210900050727     c                   Z-add     w0152         w0153
211000050727     c                   EndSl
211100050727    1c                   EndIf
211200050727
211300050727     c                   EndSr
211400050801
211500050801      *------------------------------------------------------------------------*
211600050801      * RECUPERO LA TARIFFA DI CARTELLO
211700050801      *------------------------------------------------------------------------*
211800050801     c     Sr_Tarcar     BegSr
211900050801
212000050801     c                   Clear                   Trul27ds
212100050801     c                   Eval      i27tla = 'L'
212200050801s   1c                   Select
212300050801      * se tipo servizio poste passo come ntw PPT così il trul recupera la
212400050801      * cartello PPT ( per ora uguale alla cartello italia)
212500050801     c                   When      tamtsp = 'P'
212600050801     c                   Eval      i27ntw = 'PPT'
212700050801      * Ntw DPD
212800050801     c                   When      §tadpd = 'S'
212900050801     c                   Eval      i27ntw = 'DPD'
213000050801      * Ntw FedEx
213100050801     c                   When      §tafed = 'S'
213200050801     c                   Eval      i27ntw = 'FED'
213300050801      * passo anche il codice tariffa per recuperare se merci o documenti
213400050802     c                   Move      ta25ctrn      i27ctb
213500050801      * Tariffa estero
213600050801     c                   When      tamfie = 'E'
213700050801     c                   Eval      i27ntw = 'EEX'
213800050801      * Tariffa italia
213900050801     c                   When      tamfie = 'I'
214000050801     c                   Eval      i27ntw = 'COR'
214100050922      * se nessuna di queste (mi sa che è solo x la cartello 8888830)
214200050922      * passo il cliente
214300050922     c                   Other
214400050922     c                   Eval      i27cli = ta25kscn
214500050801    1c                   Endsl
214600050802      * Passo la data decorrenza per recuperare anche il progressivo della
214700050802      * tariffa di cartello
214800111031      * se nella ds TNTA25DS il campo TA25drc = 'S' devo prendere per recuperare
214900111031      * la tariffa di cartello la data maggiore tra oggi e la data decorrenza
215000111031      * della tariffa che riceve la copia
215100111031     c                   If        Ta25drc = 'S' and tamddt < *date
215200111031     c                   Eval      i27dta = *date
215300111031     c                   Else
215400050802     c                   Eval      i27dta = tamddt
215500111031     c                   Endif
215600111026
215700111026     c                   Call      'TRUL27R'
215800050802     c                   Parm                    Trul27ds
215900050802
216000050802     c                   If        o27err <> *Blanks
216100050802     c                   Goto      Fine
216200050802     c                   EndIf
216300050801
216400050801     c                   EndSr
216500080609
216600080609      *------------------------------------------------------------------------*
216700080609      * Carico filiali abilitate al lasciato avviso
216800080609      *------------------------------------------------------------------------*
216900080609     c     carlav        begsr
217000080609
217100080609     c                   clear                   sklav
217200080609     c                   clear                   xx
217300080609
217400080609     c/exec sql
217500080609     c+ declare lav cursor for select tntbe01l.*
217600080609     c+ from tntbe01l
217700080609     c+ where tbecod='LAV' and tbeatb = ''
217800080609     c/end-exec
217900080609
218000080609     c/exec sql
218100080609     c+ open lav
218200080609     c/end-exec
218300080609
218400080609     c                   do        *hival
218500080609
218600080609     c/exec sql
218700080609     c+ fetch next from lav into :tntbeds
218800080609     c/end-exec
218900080609
219000080609     c                   select
219100080609     c                   when      sqlcod = 100
219200080609     c                   leave
219300080609     c                   when      sqlcod < 0
219400080609     c                   other
219500080609     c                   eval      dlav = tbeuni
219600080609      * carico la schiera della filiali abilitate
219700080616     c                   if        d§lavtar > *date
219800080609     c                   iter
219900080609     c                   endif
220000080609     c                   eval      xx = xx + 1
220100080609     c                   eval      sklav(xx) = %dec(tbeke1:3:0)
220200080609
220300080609     c                   endsl
220400080609
220500080609     c                   enddo
220600080609
220700080609     c/exec sql
220800080609     c+ close lav
220900080609     c/end-exec
221000080609
221100080609     c                   endsr
221200050715
221300050715      *------------------------------------------------------------------------*
221400050715      * ROUTINE INIZIALE
221500050715      *------------------------------------------------------------------------*
221600050715     c     *Inzsr        BegSr
221700050715
221800050715     c     *Entry        Plist
221900050715     c                   Parm                    Kpjba
222000050715     c                   Parm                    Tnta25ds
222100050715     c                   Parm                    Tnta25tad
222200050715     c                   Parm                    Tnta25tgc
222300050715     c                   Parm                    Tnta25tpd
222400050715     c                   Parm                    Tnta25tpt
222500090916
222600090916      * controllo se richiamato da trattativa
222700101222     c**                 if        %subst(kpjbu:1:1) = 'T'
222800101222     c**                 eval      *in08 = *on
222900101222     c**                 else
223000101222     c**                 eval      *in08 = *off
223100101222     c**                 endif
223200050715
223300050715      * se richiamato da Tnta01r carico gli scaglioni ISTAT
223400050802if  1c                   If        ta25ta01 = '1'
223500101015     c                   Clear                   savrct
223600101015
223700101015      /free
223800101015       //?Carico scatti ISTAT
223900101015         SISsca = 0;
224000101015         clear DIA;
224100101015         setll (SISsca) TISIS01L;
224200101015
224300101015         DOW  not $EoF;
224400101015
224500101015           read TISIS01L;
224600101015
224700101015           IF  %Eof(TISIS01L);
224800101015             $EoF = *on;
224900101015             leave;
225000101015           ENDIF;
225100101015
225200101015           DIA(SISsca) = SISdata;
225300101015
225400101015         ENDDO;
225500101015
225600101015      /end-free
225700101015
225800050715      * verifico se esiste lo scaglione ISTAT in base alla data odierna
225900101015do  2c                   Do        999           pi
226000050715     c                   If        *date <= dia(pi)
226100101015     c                   Eval      savrct = pi
226200100723      * appena trovo la prima data maggiore o uguale ad oggi esco dal do
226300100723     c                   leave
226400050715     c                   EndIf
226500050715    2c                   EndDo
226600050715      * se non trovo lo scaglione errore al tnta01r
226700101015     c                   If        savrct = *Zeros
226800050715     c                   Move      'IST'         kpjbu
226900050715     c                   Goto      Fine
227000050715     c                   EndIf
227100050715    1c                   EndIf
227200050729
227300050729      * Aggancio la tabella 2G per recuperare i giorni massimi
227400050729      * di rientro
227500050729     c                   Clear                   ds2g
227600050729     c                   Eval      kcod = '2G'
227700050729     c                   Eval      kkey = '1'
227800050803     c     kTabel01      Chain     Tabel00f
227900050729     c                   If        %Found(Tabel00f) and tblflg = *Blanks
228000050729     c                   Eval      ds2g = tbluni
228100050729     c                   Else
228200050729     c                   z-add     99            §2ggre
228300050729     c                   z-add     99            §2ggdp
228400050729     c                   z-add     99            §2ggfe
228500050729     c                   EndIf
228600050715
228700050715      * se copia di tutto il dettaglio tariffario la ds relativa la pulisco
228800050715     c                   If        ta25tad = 'T'
228900050715     c                   Clear                   Tnta25tad
229000050715     c                   EndIf
229100050715
229200050715      * se copia di tutte le tariffe particolari la ds relativa la pulisco
229300050715     c                   If        ta25tpt = 'T'
229400050715     c                   Clear                   Tnta25tpt
229500050715     c                   Clear                   Tnta25tpd
229600050715     c                   EndIf
229700050715
229800050715      * se copia delle tariffe di giacenza senza vis. la ds relativa la pulisco
229900050715     c                   If        ta25tgc = 'T'
230000050715     c                   Clear                   Tnta25tgc
230100050715     c                   EndIf
230200050715
230300050715      * se copia verso tariffa apro TITAV
230400050715     c                   If        ta25tipn = 'T'
230500050715     c                   Open      Titav01l
230600050715     c                   EndIf
230700050715
230800050715      * Copio da OFFERTA
230900050715     c                   Eval      *In01 = (ta25tipo = 'O')
231000050715      * Copio  a OFFERTA
231100050715     c                   Eval      *In02 = (ta25tipn = 'O')
231200050715      * Copio verso un nuovo progressivo
231300050715     c                   Eval      *In03 = (ta25prgn = *Blanks)
231400050715      * Copia da tariffa di cartello
231500050715     c                   If        Not *In01
231600050715     c                   Movel     ta25ksco      w005a
231700050715     c                   Eval      *In04 = (w005a = '88888')
231800050715     c                   EndIf
231900050715      * Copia  a tariffa di cartello
232000050715     c                   If        Not *In02
232100050715     c                   Movel     ta25kscn      w005a
232200050715     c                   Eval      *In05 = (w005a = '88888')
232300050715     c                   EndIf
232400050715      * sk dei codici di tassazione da includere o da omettere
232500050715     c                   Movea     skcts         wcts
232600050715     c                   If        ta25io2 = 'O' and wcts <> *Blanks
232700050715     c                   Eval      wscts = *On
232800050715     c                   EndIf
232900050715      * scaglione da includere o da omettere
233000050715     c                   If        ta25io3 = 'O' and ta25sgl > *Zeros
233100050715     c                   Eval      wssgl = *On
233200050715     c                   EndIf
233300061012      * se duplica dettaglio tariffario controllo se c'è il codice di tassazione
233400061012     c                   if        ta25dupd = 'S'
233500061012     c                   eval      *in07 = (ta25octs <> *blanks)
233600061012     c                   endif
233700061012      * se c'è il codice di tassazione cerco il relativo codice di ordinamento
233800061012     c                   if        *in07
233900061012      * prima il vecchio codice di tassazione
234000061012     c                   clear                   dsct
234100061012     c                   clear                   worpo
234200061012     c                   eval      kcod = 'CT'
234300061012     c                   eval      kkey = ta25octs
234400061012     c     ktabel01      chain     tabel00f
234500061012     c                   if        %found(tabel00f) and tblflg = *blanks
234600061012     c                   eval      dsct = tbluni
234700061012     c                   endif
234800061012     c                   move      §ctcor        worpo
234900061012      * prima il vecchio codice di tassazione
235000061012     c                   clear                   dsct
235100061012     c                   clear                   worpn
235200061012     c                   eval      kcod = 'CT'
235300061012     c                   eval      kkey = ta25ncts
235400061012     c     ktabel01      chain     tabel00f
235500061012     c                   if        %found(tabel00f) and tblflg = *blanks
235600061012     c                   eval      dsct = tbluni
235700061012     c                   endif
235800061012     c                   move      §ctcor        worpn
235900061012     c                   endif
236000050715
236100050715     c     *dtaara       define    §azute        azuteds
236200050715     c     *dtaara       define    §datiute      ddatiute
236300050715     c                   in(E)     *dtaara
236400050715     c                   If        %error  or RSUT = *blanks
236500050715     c                   Clear                   Tibs34ds
236600050715     c                   Call      'TIBS34R'
236700050715     c                   Parm                    Tibs34ds
236800050715     c                   In        *dtaara
236900050715     c                   EndIf
237000050715
237100050715      * Controllo se sono in sede
237200050715     c                   Eval      *In12 = (simfel = *zeros)
237300050715      * se non sono in sede apro i file delle offerte
237400050715     c                   If        Not *In12
237500050715     c                   Open      Tnofm01l
237600050715     c                   Open      Tiofd01l
237700050715     c                   Open      Tiopt01l
237800050715     c                   Open      Tiopd01l
237900050715     c                   Open      Tiogc01l
238000101222     c                   Open      Tivis05l
238100101222     c                   Open      Tivof01l
238200050715     c                   EndIf
238300050715
238400050801     c                   Clear                   comlnp
238500050801
238600051019      * se non sto copiando verso una tariffa di cartello
238700051019      * cerco la lnp del cliente
238800051019if  1c                   If        Not *In05
238900050715      * copia verso tariffa
239000050715     c  n02              Movel     ta25kscn      comlnp
239100050715      * copia verso offerta
239200050715if  2c                   If        *In02
239300101222     c     ta25kscn      Chain     Tivis05l
239400090916if  3c                   If        %Found
239500050715      *         offerta di cliente codificato
239600050715if  4c                   If        visksc > *Zeros
239700050715     c                   Movel     visksc        comlnp
239800050715      *         offerta di cliente non codificato
239900050715   x4c                   Else
240000050715     c                   Movel     viscmm        comlnp
240100050715    4c                   EndIf
240200050715    3c                   EndIf
240300050715    2c                   EndIf
240400050715    1c                   EndIf
240500050715
240600050715      * KLIST
240700050803     c     kTabel        klist
240800050805     c                   kfld                    kkut
240900050805     c                   kfld                    kcod1
241000050805
241100050805     c     kTabel1       klist
241200050805     c                   kfld                    kkut
241300050805     c                   kfld                    kcod1
241400050805     c                   kfld                    kkey1
241500050803
241600050803     c     kTabel01      klist
241700050715     c                   kfld                    kkut
241800050715     c                   kfld                    kcod
241900050715     c                   kfld                    kkey
242000050715
242100050715     c     kTntam        klist
242200050715     c                   kfld                    kksc
242300050715     c                   kfld                    kctr
242400050715
242500050715     c     kTntam01      klist
242600050715     c                   kfld                    kksc
242700050715     c                   kfld                    kctr
242800050715     c                   kfld                    kprg
242900050805
243000050805     c     kTitad        klist
243100050805     c                   kfld                    kksc
243200050805     c                   kfld                    kctr
243300050805     c                   kfld                    kprg
243400050805     c                   kfld                    klnp
243500061012
243600061012     c     kTitad01      klist
243700061012     c                   kfld                    kksc
243800061012     c                   kfld                    kctr
243900061012     c                   kfld                    kprg
244000061012     c                   kfld                    klnp
244100061012     c                   kfld                    korp
244200050715
244300050805     c     kTitad04      klist
244400050715     c                   kfld                    kksc
244500050715     c                   kfld                    kctr
244600050715     c                   kfld                    kprg
244700050715     c                   kfld                    klnp
244800050715     c                   kfld                    korp
244900050715     c                   kfld                    knaz
245000050715     c                   kfld                    kcap
245100050715     c                   kfld                    ksgl
245200050725
245300050728     c     kTitpt        klist
245400050725     c                   kfld                    kksc
245500050725     c                   kfld                    kctr
245600050725     c                   kfld                    kprg
245700050725     c                   kfld                    kftc
245800050728
245900050728     c     kTitpd        klist
246000050728     c                   kfld                    kksc
246100050728     c                   kfld                    kctr
246200050728     c                   kfld                    kprg
246300050728     c                   kfld                    kftc
246400050728     c                   kfld                    korp
246500050728     c                   kfld                    ksgl
246600050715
246700050715     c     kTitav        klist
246800050715     c                   kfld                    tavksc
246900050715     c                   kfld                    tavctr
247000050715     c                   kfld                    tavprg
247100050715     c                   kfld                    tavtrd
247200050715     c                   kfld                    tavftc
247300050715     c                   kfld                    tavcta
247400050715     c                   kfld                    tavdav
247500050715
247600050715     c                   EndSr
247700050715      *------------------------------------------------------------------------*
247800050705
247900050705** MSG  Lungh. 78                                                            *
248000050713Nuova Tariffa/Offerta già presente                                            01
248100050713Tariffa/Offerta da cui copiare non trovata                                    02
248200050714Dettaglio tariffario già presente                                             03
248300050725Tariffe particolari già presenti                                              04
248400050727Dettaglio della tariffa particolare    già presente                           05
248500050729Tariffe di giacenza già presenti                                              06
248600061012ATTENZIONE !! Non sono state duplicate le tariffe con il C.A.P. inserito      07
