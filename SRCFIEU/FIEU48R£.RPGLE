000100061017     h decedit('0,') datedit(*YMD.) option(*nodebugio)
000200061017
000300061017      * Fieu48r *--------------------------------------------------------------*
000400061017      *                                                                        *
000500061017      * In base al nrr del fnvao che mi viene passato                          *
000600061017      * genero un nuovo ORM che subito chiudo con fase 999 e causale 80        *
000700061017      * questo succede quando DPD ci chiede di annullare un loro ORM           *
000800061017      * ancora presente su FNVAO00F                                            *
000900061017      *------------------------------------------------------------------------*
001000061017
001100061017     ffnvao00f  if   e             disk    infds(vaods)
001200070716     ftabel00f  if   e           k disk
001300061018     ffnorm00f  o    e             disk
001400061018     ffnorf00f  o    e             disk
001500061121     ffnorn00f  o    e             disk
001600070716     ffnorg00f  o    e             disk
001700050915
001800061017      *------------------------------------------------------------------------*
001900061017      *   V A R I A B I L I
002000061017      *------------------------------------------------------------------------*
002100070716     d codut           s              1  0 inz(1)
002200070716     d kcod            s                   like(tblcod)
002300070716     d kkey            s                   like(tblkey)
002400061018     d wora            s               t
002500070716     d wpkg            s             10  1
002600070716     d wvlm            s             10  3
002700001116
002800061017      *------------------------------------------------------------------------*
002900061017      *   D S   I N T E R N E / E S T E R N E
003000061017      *------------------------------------------------------------------------*
003100070716     d dfar          e ds
003200070716     d dsqt1         e ds
003300061017     d fieu48ds      e ds
003400061017     d kpjba         e ds
003500070716     d tibs02ds      e ds
003600061018     d trul33ds      e ds
003700061018     d trul82ds      e ds
003800010205
003900061017     d vaods           ds
004000061017     d  vaonrr               397    400b 0
004100061017
004200061017      *------------------------------------------------------------------------*
004300061018      *   C O S T A N T I
004400061018      *------------------------------------------------------------------------*
004500061018     d msgjob          C                   CONST('Si sta bloccando la conferma -
004600061018     d                                     automatica dell''ORM: USCIRE SUBITO -
004700061018     d                                     dal lavoro!')
004800000927
004900061018      *------------------------------------------------------------------------*
005000061018
005100061018     c     id48nrr       chain(e)  fnvao00f
005200061018      * se il record è allocato mando messaggio info all'utente che alloca il rcd
005300061018     c                   if        %error
005400061018     c                   clear                   trul82ds
005500061018     c                   eval      ul82§rrn = vaonrr
005600061018     c                   eval      ul82§fil = 'FNVAO00F'
005700061018     c                   eval      ul82§win = 'N'
005800061018     c                   eval      ul82§f7  = 'S'
005900061018     c                   eval      ul82§num = 5
006000061018     c                   eval      ul82§att = 60
006100061018     c                   eval      ul82§mss = msgjob
006200061018      * chiamo il pgm che manda il messaggio info all'utente
006300061018     c                   call(e)   'TRUL82R'
006400061018     c                   parm                    trul82ds
006500061018      * riaggancio il rcd
006600061018     c     vaonrr        chain(e)  fnvao00f
006700061018      * se dopo 5 tentativi è ancora allocato ritorno al chiamante con errore
006800061018      * così facendo il VAO viene annullato (vaotab = 'A') ma l'orm non viene creato
006900061018    2c                   if        %error
007000061018     c                   eval      od48esito = '1'
007100061018     c                   goto      fine
007200061018     c                   endif
007300061018e   2c                   endif
007400061018      * non trovo il rcd sul VAO...strano ma lo prevedo
007500061018      * ritorno al chiamante con errore
007600061018     c                   if        not %found (fnvao00f)
007700061018     c                   eval      od48esito = '1'
007800061018     c                   goto      fine
007900061018     c                   endif
008000061018      * se arrivo qua posso procedere con la generazione dell'ORM
008100061018     c                   clear                   fnorm000
008200061018      * stacco un nuovo numero ORM
008300061018     c                   exsr      sr_numeraorm
008400061018      * scrivo l'ORM
008500061018     c                   exsr      sr_scriviorm
008600061018      * scrivo la fase di immissione e di chiusura
008700061018     c                   exsr      sr_scrivifase
008800070716      * scrivo estensione ORM --> FNORG00F
008900070716     c                   exsr      sr_scriviorg
009000061018
009100061018     c     fine          tag
009200061018
009300061018     c                   eval      *inlr = *on
009400040909
009500061018      *------------------------------------------------------------------------*
009600061018      * stacco un nuovo numero ORM
009700061018      *------------------------------------------------------------------------*
009800061018     c     sr_numeraorm  begsr
009900061018
010000061018     c                   clear                   trul33ds
010100061018     c                   eval      i33cnu = 300
010200061018     c                   eval      i33po1 = vaopoe
010300061018     c                   eval      i33num = 1
010400061018     c                   movel(p)  trul33ds      kpjbu
010500061018     c                   call      'TRUL33R'
010600061018     c                   parm                    kpjba
010700061018     c                   movel     kpjbu         trul33ds
010800061018
010900061018    1c                   select
011000061018      * numeratore non trovato
011100061018     c                   when      o33err =  01
011200061018     c                   eval      od48esito = '1'
011300061018     c                   goto      fine
011400061018      * numeratore allocato
011500061018     c                   when      o33err =  02
011600061018     c                   eval      od48esito = '1'
011700061018     c                   goto      fine
011800061018      * altro errore
011900061018     c                   when      O33err <> *zeros
012000061018     c                   eval      od48esito = '1'
012100061018     c                   goto      fine
012200061018    1c                   endsl
012300061018
012400061018     c                   z-add     o33nrf        ormnor
012500061018
012600061018     c                   endsr
012700061018
012800061018      *------------------------------------------------------------------------*
012900061018      * scrivo il nuovo ORM
013000061018      *------------------------------------------------------------------------*
013100061018     c     sr_scriviorm  begsr
013200061018
013300061018     c                   eval      ormpoe = vaopoe
013400061018     c                   eval      ormtor = vaotor
013500061018     c                   eval      ormtco = vaotco
013600061018     c                   eval      ormdao = vaodao
013700061018     c                   eval      ormoao = vaooao
013800061018     c                   eval      ormcor = vaocor
013900061018     c                   eval      ormrso = vaorso
014000061018     c                   eval      ormino = vaoino
014100061018     c                   eval      ormcao = vaocao
014200061018     c                   eval      ormloo = vaoloo
014300061018     c                   eval      ormpro = vaopro
014400061018     c                   eval      ormnao = vaonao
014500061018     c                   eval      ormcra = vaocra
014600061018     c                   eval      ormrsr = vaorsr
014700061018     c                   eval      orminr = vaoinr
014800061018     c                   eval      ormcar = vaocar
014900061018     c                   eval      ormlor = vaolor
015000061018     c                   eval      ormprr = vaoprr
015100061018     c                   eval      ormnar = vaonar
015200061018     c                   eval      ormrer = vaorer
015300061018     c                   eval      ormter = vaoter
015400061018     c                   eval      ormcrc = vaocrc
015500061018     c                   eval      ormrsc = vaorsc
015600061018     c                   eval      orminc = vaoinc
015700061018     c                   eval      ormloc = vaoloc
015800061018     c                   eval      ormcac = vaocac
015900061018     c                   eval      ormprc = vaoprc
016000061018     c                   eval      ormnac = vaonac
016100061018     c                   eval      ormffd = vaoffd
016200061018     c                   eval      ormdar = vaodar
016300061018     c                   eval      ormorr = vaoorr
016400061018     c                   eval      ormrmp = vaormp
016500061018     c                   eval      ormnam = vaonam
016600061018     c                   eval      ormncl = vaoncl
016700061018     c                   eval      ormpkg = vaopkg
016800061018     c                   eval      ormvlm = vaovlm
016900061018     c                   eval      ormbnc = vaobnc
017000061018     c                   eval      ormblc = vaoblc
017100061018     c                   eval      ormatt = vaoatt
017200061018     c                   eval      ormmtc = vaomtc
017300061018     c                   eval      ormpag = vaopag
017400061018     c                   eval      ormksc = vaoksc
017500061018     c                   eval      ormctr = vaoctr
017600061018     c                   eval      ormpor = vaopor
017700061018     c                   eval      ormzor = vaozor
017800061018     c                   eval      ormpoc = vaopoc
017900061018     c                   eval      ormno1 = vaono1
018000061018     c                   eval      ormno2 = vaono2
018100061018     c                   eval      ormddt = vaoddt
018200061018     c                   eval      ormdfo = *date
018300061018     c                   time                    ormofo
018400061018     c                   eval      ormfao = 999
018500061018     c                   eval      ormsto = vaosto
018600061018     c                   eval      ormrfa = vaorfa
018700061018     c                   eval      ormspi = vaospi
018800061018     c                   eval      ormflo = vaoflo
018900061018     c                   eval      ormdtt = *date
019000070716      * imposto il campo ORMETI
019100070716     c                   clear                   tibs02ds
019200070716     c                   eval      t02mod = 'C'
019300070716     c                   eval      t02sif = knsif
019400070716     c                   eval      t02cod = 'FAR'
019500070716     c                   movel(p)  ormfao        t02ke1
019600070716     c                   call      'TIBS02R'
019700070716     c                   parm                    kpjba
019800070716     c                   parm                    tibs02ds
019900070716     c                   eval      dfar = t02uni
020000070716     c                   eval      ormeti = d§farass
020100061018     c                   write     fnorm000
020200061018
020300061018     c                   endsr
020400061018
020500061018      *------------------------------------------------------------------------*
020600061018      * scrivo le fasi dell'ORM
020700061018      *------------------------------------------------------------------------*
020800061018     c     sr_scrivifase begsr
020900061018
021000061018      * scrivo la fase 50
021100061018     c                   clear                   fnorf000
021200061018     c                   eval      orfpoe = ormpoe
021300061018     c                   eval      orfnsr = ormnsr
021400061018     c                   eval      orfnor = ormnor
021500061018     c                   eval      orfnrv = ormnrv
021600061018     c                   eval      orfpog = ormpoe
021700061018     c                   eval      orfdae = ormdfo
021800061018     c                   move      ormofo        wora
021900061018     c                   subdur    1:*s          wora
022000061018     c                   move      wora          orfore
022100061018     c                   eval      orffar = 50
022200061121     c                   eval      orfpue = 'ANNULL-DPD'
022300061018     c                   write     fnorf000
022400061018
022500061018      * scrivo la fase 999 con causale 80
022600061018     c                   eval      orfore = ormofo
022700061018     c                   eval      orffar = ormfao
022800061018     c                   eval      orfcar = '80'
022900061018     c                   write     fnorf000
023000061121
023100061121      * scrivo nota
023200061121     c                   eval      ornpoe = ormpoe
023300061121     c                   eval      ornnsr = ormnsr
023400061121     c                   eval      ornnor = ormnor
023500061121     c                   eval      ornnrv = ormnrv
023600061121     c                   eval      ornfar = ormfao
023700061121     c                   eval      orndai = ormdfo
023800061121     c                   eval      ornori = ormofo
023900061121     c                   eval      ornprg = 1
024000061121     c                   eval      ornpue = orfpue
024100061121     c                   eval      ornpon = ormpoe
024200061121     c                   eval      ornsns = 'I'
024300061121     c                   eval      ornnot = '** ANNULLATO PRIMA DELLA ' +
024400061121     c                             'CONFERMA **'
024500061121     c                   eval      ornftr = 'S'
024600061121     c                   eval      orndtr = ormdfo
024700061121     c                   write     fnorn000
024800061018
024900061018     c                   endsr
025000070716
025100070716      *------------------------------------------------------------------------*
025200070716      * scrivo FNORG00F
025300070716      *------------------------------------------------------------------------*
025400070716     c     sr_scriviorg  begsr
025500070716
025600070716     c                   eval      wpkg = ormpkg
025700070716     c                   eval      wvlm = ormvlm
025800070716
025900070716      * se non inserito peso a video lo calcolo per memorizzarlo su fnorg00f
026000070716     c                   if        wpkg = *zeros
026100070716     c                   select
026200070716 b2  c                   when      ormvlm <> *zeros
026300070716     c                   eval      wpkg = ormvlm * §qtkpm
026400070716 b3  c                   when      ormbnc <> *zeros
026500070716     c                   eval      wpkg = ormbnc * §qtkpm
026600070716 e3  c                   endsl
026700070716      * se troppo alto il peso lo imposto al massimo
026800070716     c                   if        wpkg > 999999,9
026900070716     c                   eval      wpkg = 999999,9
027000070716     c                   endif
027100070716     c                   endif
027200070716
027300070716      * se non inserito volume a video lo calcolo per memorizzarlo su fnorg00f
027400070716     c                   if        wvlm = *zeros
027500070716     c                   select
027600070716 b2  c                   when      ormpkg <> *zeros and §qtkpm <> *zeros
027700070716     c                   eval(h)   wvlm = ormpkg / §qtkpm
027800070716 b2  c                   when      ormpkg <> *zeros
027900070716     c                   eval(h)   wvlm = ormpkg / 200
028000070716 b3  c                   when      ormbnc <> *zeros
028100070716     c                   eval      wvlm = ormbnc
028200070716 e3  c                   endsl
028300070716      * se troppo alto il volume lo imposto al massimo
028400070716     c                   if        wvlm > 99,999
028500070716     c                   eval      wvlm = 99,990
028600070716     c                   endif
028700070716     c                   endif
028800070716
028900070716     c                   clear                   fnorg000
029000070716     c                   eval      orgpoe = ormpoe
029100070716     c                   eval      orgnsr = ormnsr
029200070716     c                   eval      orgnor = ormnor
029300070716     c                   eval      orgnrv = ormnrv
029400070716     c                   eval      orgpor = ormpor
029500070716     c                   eval      orgpkg = wpkg
029600070716     c                   eval      orgvlm = wvlm
029700070716     c                   write     fnorg000
029800070716
029900070716     c                   endsr
030000061017
030100061017      *------------------------------------------------------------------------*
030200061017      * routine iniziale
030300061017      *------------------------------------------------------------------------*
030400061018     c     *inzsr        begsr
030500061017
030600061018     c     *entry        plist
030700061018     c                   parm                    fieu48ds
030800070716
030900070716      * recupero rapporto peso volume
031000070716     c                   clear                   dsqt1
031100070716     c                   eval      kcod = 'QT'
031200070716     c                   eval      kkey = '1'
031300070716     c     ktab          chain     tabel00f
031400070716     c                   if        %found(tabel00f)
031500070716     c                             and tblflg = *blanks
031600070716     c                   eval      dsqt1 = tbluni
031700070716     c                   endif
031800070716
031900070716     c     ktab          klist
032000070716     c                   kfld                    codut
032100070716     c                   kfld                    kcod
032200070716     c                   kfld                    kkey
032300061018
032400061018     c                   endsr
