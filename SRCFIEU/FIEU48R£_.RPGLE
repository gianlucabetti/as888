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
001200061018     ffnorm00f  o    e             disk
001300061018     ffnorf00f  o    e             disk
001400061121     ffnorn00f  o    e             disk
001500050915
001600061017      *------------------------------------------------------------------------*
001700061017      *   V A R I A B I L I
001800061017      *------------------------------------------------------------------------*
001900061018     d wora            s               t
002000001116
002100061017      *------------------------------------------------------------------------*
002200061017      *   D S   I N T E R N E / E S T E R N E
002300061017      *------------------------------------------------------------------------*
002400061017     d fieu48ds      e ds
002500061017     d kpjba         e ds
002600061018     d trul33ds      e ds
002700061018     d trul82ds      e ds
002800010205
002900061017     d vaods           ds
003000061017     d  vaonrr               397    400b 0
003100061017
003200061017      *------------------------------------------------------------------------*
003300061018      *   C O S T A N T I
003400061018      *------------------------------------------------------------------------*
003500061018     d msgjob          C                   CONST('Si sta bloccando la conferma -
003600061018     d                                     automatica dell''ORM: USCIRE SUBITO -
003700061018     d                                     dal lavoro!')
003800000927
003900061018      *------------------------------------------------------------------------*
004000061018
004100061018     c     id48nrr       chain(e)  fnvao00f
004200061018      * se il record è allocato mando messaggio info all'utente che alloca il rcd
004300061018     c                   if        %error
004400061018     c                   clear                   trul82ds
004500061018     c                   eval      ul82§rrn = vaonrr
004600061018     c                   eval      ul82§fil = 'FNVAO00F'
004700061018     c                   eval      ul82§win = 'N'
004800061018     c                   eval      ul82§f7  = 'S'
004900061018     c                   eval      ul82§num = 5
005000061018     c                   eval      ul82§att = 60
005100061018     c                   eval      ul82§mss = msgjob
005200061018      * chiamo il pgm che manda il messaggio info all'utente
005300061018     c                   call(e)   'TRUL82R'
005400061018     c                   parm                    trul82ds
005500061018      * riaggancio il rcd
005600061018     c     vaonrr        chain(e)  fnvao00f
005700061018      * se dopo 5 tentativi è ancora allocato ritorno al chiamante con errore
005800061018      * così facendo il VAO viene annullato (vaotab = 'A') ma l'orm non viene creato
005900061018    2c                   if        %error
006000061018     c                   eval      od48esito = '1'
006100061018     c                   goto      fine
006200061018     c                   endif
006300061018e   2c                   endif
006400061018      * non trovo il rcd sul VAO...strano ma lo prevedo
006500061018      * ritorno al chiamante con errore
006600061018     c                   if        not %found (fnvao00f)
006700061018     c                   eval      od48esito = '1'
006800061018     c                   goto      fine
006900061018     c                   endif
007000061018      * se arrivo qua posso procedere con la generazione dell'ORM
007100061018     c                   clear                   fnorm000
007200061018      * stacco un nuovo numero ORM
007300061018     c                   exsr      sr_numeraorm
007400061018      * scrivo l'ORM
007500061018     c                   exsr      sr_scriviorm
007600061018      * scrivo la fase di immissione e di chiusura
007700061018     c                   exsr      sr_scrivifase
007800061018
007900061018     c     fine          tag
008000061018
008100061018     c                   eval      *inlr = *on
008200040909
008300061018      *------------------------------------------------------------------------*
008400061018      * stacco un nuovo numero ORM
008500061018      *------------------------------------------------------------------------*
008600061018     c     sr_numeraorm  begsr
008700061018
008800061018     c                   clear                   trul33ds
008900061018     c                   eval      i33cnu = 300
009000061018     c                   eval      i33po1 = vaopoe
009100061018     c                   eval      i33num = 1
009200061018     c                   movel(p)  trul33ds      kpjbu
009300061018     c                   call      'TRUL33R'
009400061018     c                   parm                    kpjba
009500061018     c                   movel     kpjbu         trul33ds
009600061018
009700061018    1c                   select
009800061018      * numeratore non trovato
009900061018     c                   when      o33err =  01
010000061018     c                   eval      od48esito = '1'
010100061018     c                   goto      fine
010200061018      * numeratore allocato
010300061018     c                   when      o33err =  02
010400061018     c                   eval      od48esito = '1'
010500061018     c                   goto      fine
010600061018      * altro errore
010700061018     c                   when      O33err <> *zeros
010800061018     c                   eval      od48esito = '1'
010900061018     c                   goto      fine
011000061018    1c                   endsl
011100061018
011200061018     c                   z-add     o33nrf        ormnor
011300061018
011400061018     c                   endsr
011500061018
011600061018      *------------------------------------------------------------------------*
011700061018      * scrivo il nuovo ORM
011800061018      *------------------------------------------------------------------------*
011900061018     c     sr_scriviorm  begsr
012000061018
012100061018     c                   eval      ormpoe = vaopoe
012200061018     c                   eval      ormtor = vaotor
012300061018     c                   eval      ormtco = vaotco
012400061018     c                   eval      ormdao = vaodao
012500061018     c                   eval      ormoao = vaooao
012600061018     c                   eval      ormcor = vaocor
012700061018     c                   eval      ormrso = vaorso
012800061018     c                   eval      ormino = vaoino
012900061018     c                   eval      ormcao = vaocao
013000061018     c                   eval      ormloo = vaoloo
013100061018     c                   eval      ormpro = vaopro
013200061018     c                   eval      ormnao = vaonao
013300061018     c                   eval      ormcra = vaocra
013400061018     c                   eval      ormrsr = vaorsr
013500061018     c                   eval      orminr = vaoinr
013600061018     c                   eval      ormcar = vaocar
013700061018     c                   eval      ormlor = vaolor
013800061018     c                   eval      ormprr = vaoprr
013900061018     c                   eval      ormnar = vaonar
014000061018     c                   eval      ormrer = vaorer
014100061018     c                   eval      ormter = vaoter
014200061018     c                   eval      ormcrc = vaocrc
014300061018     c                   eval      ormrsc = vaorsc
014400061018     c                   eval      orminc = vaoinc
014500061018     c                   eval      ormloc = vaoloc
014600061018     c                   eval      ormcac = vaocac
014700061018     c                   eval      ormprc = vaoprc
014800061018     c                   eval      ormnac = vaonac
014900061018     c                   eval      ormffd = vaoffd
015000061018     c                   eval      ormdar = vaodar
015100061018     c                   eval      ormorr = vaoorr
015200061018     c                   eval      ormrmp = vaormp
015300061018     c                   eval      ormnam = vaonam
015400061018     c                   eval      ormncl = vaoncl
015500061018     c                   eval      ormpkg = vaopkg
015600061018     c                   eval      ormvlm = vaovlm
015700061018     c                   eval      ormbnc = vaobnc
015800061018     c                   eval      ormblc = vaoblc
015900061018     c                   eval      ormatt = vaoatt
016000061018     c                   eval      ormmtc = vaomtc
016100061018     c                   eval      ormpag = vaopag
016200061018     c                   eval      ormksc = vaoksc
016300061018     c                   eval      ormctr = vaoctr
016400061018     c                   eval      ormpor = vaopor
016500061018     c                   eval      ormzor = vaozor
016600061018     c                   eval      ormpoc = vaopoc
016700061018     c                   eval      ormno1 = vaono1
016800061018     c                   eval      ormno2 = vaono2
016900061018     c                   eval      ormddt = vaoddt
017000061018     c                   eval      ormdfo = *date
017100061018     c                   time                    ormofo
017200061018     c                   eval      ormfao = 999
017300061018     c                   eval      ormsto = vaosto
017400061018     c                   eval      ormrfa = vaorfa
017500061018     c                   eval      ormspi = vaospi
017600061018     c                   eval      ormflo = vaoflo
017700061018     c                   eval      ormdtt = *date
017800061018     c                   write     fnorm000
017900061018
018000061018     c                   endsr
018100061018
018200061018      *------------------------------------------------------------------------*
018300061018      * scrivo le fasi dell'ORM
018400061018      *------------------------------------------------------------------------*
018500061018     c     sr_scrivifase begsr
018600061018
018700061018      * scrivo la fase 50
018800061018     c                   clear                   fnorf000
018900061018     c                   eval      orfpoe = ormpoe
019000061018     c                   eval      orfnsr = ormnsr
019100061018     c                   eval      orfnor = ormnor
019200061018     c                   eval      orfnrv = ormnrv
019300061018     c                   eval      orfpog = ormpoe
019400061018     c                   eval      orfdae = ormdfo
019500061018     c                   move      ormofo        wora
019600061018     c                   subdur    1:*s          wora
019700061018     c                   move      wora          orfore
019800061018     c                   eval      orffar = 50
019900061121     c                   eval      orfpue = 'ANNULL-DPD'
020000061018     c                   write     fnorf000
020100061018
020200061018      * scrivo la fase 999 con causale 80
020300061018     c                   eval      orfore = ormofo
020400061018     c                   eval      orffar = ormfao
020500061018     c                   eval      orfcar = '80'
020600061018     c                   write     fnorf000
020700061121
020800061121      * scrivo nota
020900061121     c                   eval      ornpoe = ormpoe
021000061121     c                   eval      ornnsr = ormnsr
021100061121     c                   eval      ornnor = ormnor
021200061121     c                   eval      ornnrv = ormnrv
021300061121     c                   eval      ornfar = ormfao
021400061121     c                   eval      orndai = ormdfo
021500061121     c                   eval      ornori = ormofo
021600061121     c                   eval      ornprg = 1
021700061121     c                   eval      ornpue = orfpue
021800061121     c                   eval      ornpon = ormpoe
021900061121     c                   eval      ornsns = 'I'
022000061121     c                   eval      ornnot = '** ANNULLATO PRIMA DELLA ' +
022100061121     c                             'CONFERMA **'
022200061121     c                   eval      ornftr = 'S'
022300061121     c                   eval      orndtr = ormdfo
022400061121     c                   write     fnorn000
022500061018
022600061018     c                   endsr
022700061017
022800061017      *------------------------------------------------------------------------*
022900061017      * routine iniziale
023000061017      *------------------------------------------------------------------------*
023100061018     c     *inzsr        begsr
023200061017
023300061018     c     *entry        plist
023400061018     c                   parm                    fieu48ds
023500061018
023600061018     c                   endsr
