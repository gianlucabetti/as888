000100030604     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200140811     H* FNLRY02R *---------------------------------------------------*
000300140523     H* - Gestione disposizioni di consegna
000400070403     H*--------------------------------------------------------------*
000500140811     FfnLRY02d  CF   E             WORKSTN
000600140811     f                                     sfile(LRY02s01:snrr1)
000700140812     Ffnarb01l  if   E           k disk
000800170111     Ffnblp01l  if   E           k disk    prefix(arb:3)
000900070316     Fazorg01l  if   E           k disk
001000070201     Ftabel00f  if   E           k disk
001100141027     F*fiarg01l  if   E           k disk
001200140909     Ftiidc01l  if   E           k disk
001300140812     Ftiidc05l  if   E           k disk    rename(tiidc000:tiidc005)
001400070504     d
001500070504     D sk8             S              1    DIM(8)
001600011228      *
001700011228     D Kpjba         E DS
001800140813     d
001900140930     D fnlry02ds     E DS
002000141028     D fnlry03ds2    E DS
002100011228      *
002200070522     D fidg09ds      E DS
002300070201     D fidn12ds      E DS
002400070315     d azuteds       e ds                  extname(azute00f)
002500070315     d ddatiute      e ds
002600141014     d dLat          e ds
002700141014     d dute01        e ds
002800141014     D tibs02ds      E DS
002900141014     D TRUL31DS      E DS
003000141014     D TRUL31DS2     E DS
003100070315     d tibs34ds      e ds                  inz
003200070131     D trul06ds      E DS
003300070131     D  lin                    1     90  0 dim(30)
003400151119     d*** TRULVPODS     e ds
003500151119     D***  skFilOk               16    765    dim(250)
003600141030
003700151119     D*** skFilOkdir      s                   like(skFilOk) dim(250) inz
003800070206     D* DATI X RICHIAMO PGM INTERR. BOLLE ARRIVI
003900070206     D PARAM1          DS                  INZ
004000070206     D  PA1AAS                14     17  0
004100070206     D  PA1LNP                18     20  0
004200070206     D  PA1NRS                21     22  0
004300070206     D  PA1NSP                23     29  0
004400070206     D  PA1F03                30     30
004500070206     D  PA1FLG                31     31
004600070206     D* SE =1 NON RICH. FNLG30R IN MODO RICORSIVO DA INTERR.BOLLE
004700070206     D  PA1GIA               144    144
004800070330     d
004900070330     D                 DS
005000070330     D  ARBAAS                 1      4  0
005100070330     D  ARBMGS                 5      8  0
005200070330     D  ARBDSP                 1      8  0
005300070330     d
005400070201     d ds3a          e ds
005500011228      *
005600140813     D savprg          S                   like(idcprg)
005700140813     D wtit2           S                   like(v1ctit2)
005800070327     d K_aas           s                   like(arbaas)
005900070327     d K_lnp           s                   like(arblnp)
006000070327     d K_nrs           s                   like(arbnrs)
006100070327     d K_nsp           s                   like(arbnsp)
006200140909     d K_ELADATA       s                   like(idceladata) inz
006300070430     D wprima_v        S              1
006400140813     D savpiu          S              1
006500030604     D snrr1           S              5i 0
006600070129     D lasnrr1         S              5i 0
006700030613     D WLBDA8          DS
006800030613     D  G02DAT                 1      8  0
006900030613     D  G02INV                 9     16  0
007000030613     D  G02ERR                17     17
007100030613     D  G02TGI                18     22  0
007200070319
007300070315     d  l6             s              3  0 dim(30)
007400070315     d  l1             s              3  0 dim(30)
007500070320     D nog             S              3  0 DIM(30)                              FIL.GESTITE
007600070330     d Dataiso         s               d   datfmt(*iso)
007700070406     d Datadmy         s               d   datfmt(*dmy)
007800140813     D wpos            S              1s 0
007900070504     D xx              S              1  0
008000141014     D savkpjbu        S                   LIKE(kpjbu)
008100141014     d wabi            s                   like(UteAut)
008200141014     D $pog            s              3    dim(250) inz(*zeros)
008300141014     D w003a           s              3
008400141015     D noaut           s              1    inz
008500141027     D s_v1cdir        s                   like(v1cdir)
008600070316      *---------------------------------------------------------------------
008700140930     D cosapp          C                   CONST('         Consegna Ri-
008800140919     D                                     chiesta       ')
008900070321     D cosffd          C                   CONST('         Fermo Depos-
009000070321     D                                     ito           ')
009100140919     D cosind          C                   CONST('       Indirizzo A-
009200140919     D                                     lternativo    ')
009300140919     D cosaltro        C                   CONST('         Altre Indic-
009400140919     D                                     azioni        ')
009500140527     D msg             S             78    DIM(16) CTDATA PERRCD(1)
009600071128     d weditora        c                   '  :  '
009700071128     d weditdta        c                   '  /  '
009800071204     d ord_ragprv      c                   'F11=Ord.xRagSoc/Prv'
009900140812     d ord_prvrag      c                   'F11=Ord.xDataInser.'
010000071204     d w_prvrsoc       C                   1
010100071204     d w_rsocprv       C                   2
010200071204     d W_ord           S              1  0
010300070125      *---------------------------------------------------------------------
010400070125      * specifiche per ordinamento forzato sfl
010500070125     d NotUsed         S             16A
010600070125     d ReturnSize      S              9B 0
010700070125     d SizeList        S              9B 0
010800070125     d*---
010900070125     d MaxKey          C                   2
011000070125     d Ascendente      C                   1
011100070125     d Discendente     C                   2
011200070125     d Carattere       C                   6
011300070125     d Put             C                   1
011400070125     d EndPut          C                   2
011500070125     d Get             C                   3
011600070125     d Numerico        C                   8
011700141028     D digits          c                   '0123456789'
011800170111
011900170111     D redUL           C                   CONST(X'2C')
012000170111     D UL              C                   CONST(X'24')
012100170111     d c_DspAtrNorm    c                   const(x'20')
012200070125     d*---
012300070125     d SflRcd          DS
012400070316     d  v1srsd
012500071203     d  v1sprd
012600140812     d  V1hinsdata
012700070316     d  V1sAAS
012800080908     d  V1hLNP
012900080908     d  V1hNRS
013000080908     d  V1hNSP
013100140919     d  V1sinsd
013200140919     d  V1sinso
013300070330     d  V1sdsp
013400070504     d  V1sabi
013500071031     d  V1sast
013600070402     d  V1snt1
013700140813     d  V1hprg
013800140813     d  V1hblk
013900070125     d  Selected                      1A
014000070125
014100070125      /COPY QSYSINC/QRPGLESRC,QLGSORT
014200070125     d QLGKL                         16    DIM(MaxKey)
014300070125     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
014400070125     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
014500070125     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
014600070125     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
014700070125
014800070125      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
014900070125      /COPY QSYSINC/QRPGLESRC,QUSEC
015000141027
015100070502     c                   exsr      inzfgs
015200141030     c                   exsr      sr_impodir
015300000608      *
015400141028     c* Gestione subfile
015500070316     C                   EXSR      ges_s01
015600070402     c
015700140930    c
015800070402     c
015900011227      *
016000141014
016100141014     c     fine          tag
016200141028
016300141028    4c                   if        ilry02fgs<>*blanks and *in28
016400141028     c                   eval      OLRY02MSG=v1cmsg
016500141028     c                   eval      OLRY02err='1'
016600141028     c                   endif
016700070321      * Chiusura con lr
016800140813     c                   eval      ilry03tla='C'
016900141028     c                   eval      kpjbu=fnlry03ds2
017000140813     c                   call      'FNLRY03R'
017100140813     c                   parm                    kpjba
017200140930     c
017300140930     c                   movel(p)  fnlry02ds     kpjbu
017400140813     c*
017500070315     C                   seton                                        lr
017600070502     c**********************************************************************
017700070502     c* inizializza p.o. in gestione
017800070502     c**********************************************************************
017900070502     c     inzfgs        begsr
018000140930     c
018100140930     c* se passata la filiale gestione la imposto, altrimenti prendo lo stesso dutpou
018200141028     c                   if        ILRY02fgs<>*blanks
018300140930     c                   eval      *in16=*off
018400140930     c                   movel     ILRY02fgs     v1cfgs
018500140930     c                   else
018600140930     c                   eval      v1cfgs = dutpou
018700140930     c                   endif
018800140930     c
018900140930     c
019000070502     c     v1cfgs        chain     azorg01L
019100070502     c                   if        %found(azorg01l) and orgfva = *blanks
019200070502     c                   eval      v1dfgs = orgdes
019300070502     c                   endif
019400070502     c*
019500140930      * disabilito f18 se p.o. di secondo livello o richiamato
019600141014     c*****              eval      *in17 = (dutlpo = '2' or dutlpo = *blanks)
019700141014     c                   if        $pog(2)=*zeros
019800141014     c                   eval      *in17=*on
019900141014     c                   endif
020000070502     c                   eval      *in16=*off
020100141014     c                   exsr      sr_car£6
020200070502     c                   endsr
020300070316     c**********************************************************************
020400070316     c* Caricamento £6 della filiale in gestione
020500070316     c**********************************************************************
020600070316     C     SR_car£6      BEGSR
020700070316      * carico filiali £6
020800070316     C                   clear                   trul06ds
020900070316     C                   movel     '£6'          d06cod
021000070316     C                   movel     v1cfgs        d06key
021100070316     C                   movel     trul06ds      kpjbu
021200070316     C                   CALL      'TRUL06R'
021300070316     C                   parm                    kpjba
021400070316     C                   movel     kpjbu         trul06ds
021500070316     C                   movea     lin           l6
021600070316     c                   endsr
021700070315     c**********************************************************************
021800070315     c* Controllo filiale in gestione
021900070315     c**********************************************************************
022000070315     C     Ctrfgs        BEGSR
022100070315     c*
022200070315     c                   setoff                                       40
022300070315     c                   clear                   v1dfgs
022400070320      * Il p.o. gestione non può essere un p.o. gestito
022500070320     c     V1cFgs        Lookup    Nog                                    30
022600070320     c                   If        *In30
022700070320     c                   Eval      *In40 = *On
022800070320     c                   Eval      *In28 = *On
022900141027     c                   Eval      V1cMsg = Msg(1)
023000070320     c                   leavesr
023100070320     c                   EndIf
023200141014     c                   move      v1cfgs        w003a
023300141014     c     w003a         lookup    $pog                                   30
023400070315     c                   if        not *in30
023500070315     c                   seton                                        2840
023600141027     c                   eval      v1cmsg = msg(01)
023700070315     c                   leavesr
023800070315     c                   endif
023900070315
024000070315      * decodifica p.o. in gestione
024100070315     c     v1cfgs        chain     azorg01L
024200070315     c                   if        %found(azorg01l) and orgfva = *blanks
024300070315     c                   eval      v1dfgs = orgdes
024400141027     c                   endif
024500070315     c                   setoff                                       16
024600070417      *
024700070316     c                   exsr      sr_car£6
024800141030     c* Imposto il flag "Solo dirottamenti" in base alla nuova filiale gestione
024900141030     c                   exsr      sr_impodir
025000070417      *
025100070315     c                   endsr
025200140814     c**********************************************************************
025300141008     c* controllo il tipo disposizione in base alle richieste
025400140814     c**********************************************************************
025500140814     c     CtrDispo      BEGSR
025600140814     c                   setoff                                       90
025700140814     c                   exsr      ctrspecau
025800140814     c                   exsr      cerAbi
025900140814     c
026000140814    2c                   if        *in08
026100140814     c                   seton                                        90
026200140814   x2c                   else
026300140814    3c                   if        (wblk=*blanks and v1ccau=' ') or
026400140814     c                             (%subst(wblk:1:1)<>'E' and v1ccau='1') or
026500140814     c                             (%subst(wblk:2:1)<>'E' and v1ccau='2') or
026600140814     c                             (%subst(wblk:3:1)<>'E' and v1ccau='3') or
026700151112     c                             (%subst(wblk:4:1)<>'E' and v1ccau='4') or
026800151112     c                             (%subst(wblk:5:1)<>'E' and v1ccau='5')
026900140814     c                   seton                                        90
027000140814    3c                   endif
027100140814    2c                   endif
027200140814    2c                   ENDSR
027300070316     c**********************************************************************
027400140526     c* Controllo congruenza fra causale disposiz e spedizione
027500070316     c**********************************************************************
027600070316     C     Ctrspecau     BEGSR
027700151112     c                   clear                   wblk              5
027800070504     c                   setoff                                       08
027900070504     c
028000070402     c                   move      '3A'          tblcod
028100070402     c                   movel(p)  arbcbo        tblkey
028200070402     c                   exsr      ctabel
028300070402     c                   movel     tbluni        ds3a
028400070402     c
028500140527     c* Escludo bolle di recupero
028600140526    1c                   if        §3arbl='R'
028700070504     c                   seton                                        08
028800140526    1c                   endif
028900070402     c
029000140813     c                   clear                   savprg
029100140813     c                   clear                   savpiu
029200140523     c* chaino file variazioni
029300140909     c     kidc          setll     tiidc01l
029400140909     c     kidc          reade     tiidc01l
029500140909    1c                   dow       not %eof(tiidc01l)
029600070402     c*
029700140909     c                   if        idcelaflg='E'
029800140813     c
029900140813     c* mi salvo ilprimo progressivo che trovo
030000140813     c                   if        savprg=0
030100140813     c                   eval      savprg=idcprg
030200140813     c                   else
030300140813     c                   eval      savpiu ='S'
030400140813     c                   endif
030500140813     c
030600140814    2c**                 select
030700140814     c**                 when      v1ccau='1'  and idctipodis='1'
030800140814     c**                 eval      %subst(wblk:1:1)=idcelaflg
030900140814     c**                 when      v1ccau='2'  and idctipodis='2'
031000140814     c**                 eval      %subst(wblk:2:1)=idcelaflg
031100140814     c**                 when      v1ccau='3'  and idctipodis='3'
031200140814     c**                 eval      %subst(wblk:3:1)=idcelaflg
031300140814     c**                 when      v1ccau='4'  and idctipodis='4'
031400140814     c**                 eval      %subst(wblk:4:1)=idcelaflg
031500070504     c
031600070503     c* se non impostato metto se ha almeno una abilitazione
031700140814     c**                 when      v1ccau=' '
031800140526     c                   if        idctipodis='1'
031900140527     c                   eval      %subst(wblk:1:1)=idcelaflg
032000140526     c                   endif
032100140526     c                   if        idctipodis='2'
032200140527     c                   eval      %subst(wblk:2:1)=idcelaflg
032300140526     c                   endif
032400140526     c                   if        idctipodis='3'
032500140527     c                   eval      %subst(wblk:3:1)=idcelaflg
032600140526     c                   endif
032700140526     c                   if        idctipodis='4'
032800140527     c                   eval      %subst(wblk:4:1)=idcelaflg
032900140526     c                   endif
033000151112     c                   if        idctipodis='5'
033100151112     c                   eval      %subst(wblk:5:1)=idcelaflg
033200151112     c                   endif
033300070504     c
033400140814    2c**                 endsl
033500140812     c                   endif
033600140526     c
033700140909     c     kidc          reade     tiidc01l
033800140812    1c                   enddo
033900070402     c
034000070316     c                   endsr
034100070316     C*--- CHAIN TABEL00F --------------------------------------------*
034200070316     C     CTABEL        BEGSR
034300070316     C     KTAB          CHAIN     TABEL                              30
034400070316     c                   if        %found(tabel00f) and tblflg<> ' '
034500070316     C                   SETON                                        30
034600070316     C                   END
034700070316     C                   ENDSR
034800011227     c**********************************************************************
034900140814     C* gestione sfl
035000011227     c**********************************************************************
035100070316     C     ges_s01       BEGSR
035200000627      *
035300141027
035400070402     c                   eval      *inke='1'
035500070402     c* looppo per F5 - rivisulizza
035600070402     c                   dow       *inke='1'
035700070402     c
035800070402     c                   eval      *inke='0'
035900141028     c                   eval      w_ord=w_prvrsoc
036000070316     C                   EXSR      ries01
036100070316      * Imposto sottotitolo seconda videata
036200070316     c                   eval      v1ctit2=wtit2
036300070330     c
036400071204     c                   eval      v1tf11=ord_ragprv
036500141008     c                   seton                                        11
036600070316     c*
036700070316     C                   DO        *hival
036800140811     C                   WRITE     LRY02T01
036900140811     C                   WRITE     LRY02z01
037000141028     c                   if        *in20=*off
037100141028     c                   write     lry02n01
037200141028     c                   endif
037300140811     C                   EXFMT     LRY02c01
037400141027     c                   setoff                                       28
037500141027     c                   if        noaut='1'
037600141027     c                   leavesr
037700141027     c                   endif
037800011227     C                   SELECT
037900141027      * F18=cambio filiale - (Sproteggo cambio p.o gestione)
038000141027     c                   when      *inks
038100141027     c                   seton                                        16
038200141027     c                   iter
038300141027     c
038400070402     c* esco per F3-Fine  F12-Ritorno  F5-rivisualizza
038500070402     C                   WHEN      *INKL=*ON or *inkc  or *inke
038600070316     C                   leave
038700071204     c* f11=Cambio ordinamento sfl
038800071204     C                   WHEN      *INkk
038900071204     c                   select
039000071204     c                   when      w_ord=w_prvrsoc
039100071204     c                   eval      w_ord=w_rsocprv
039200071204     c                   eval      v1tf11=ord_prvrag
039300071204     c                   when      w_ord=w_rsocprv
039400071204     c                   eval      w_ord=w_prvrsoc
039500071204     c                   eval      v1tf11=ord_ragprv
039600071204     c                   endsl
039700141112     c* Solo se presenti record nel sfl
039800141112     c                   if        *in20
039900071204     C                   exsr      ordina
040000141112     c                   endif
040100071204     c                   iter
040200141027     c* Cambio fil. gestione
040300141027     c                   when      *in16
040400141027     c                   exsr      ctrfgs
040500141027     c                   if        not *in28
040600141027     c                   exsr      ries01
040700141027     c                   endif
040800141027     c                   iter
040900141027     c* cambio parzializzazione
041000141027     c                   when      v1cdir<>s_v1cdir
041100141027     c                   exsr      ries01
041200141027     c                   iter
041300141027
041400011227     C                   OTHER
041500011227
041600141027      * controllo filiale in gestione
041700141112     C   20              EXSR      GestioneSFL
041800011227     C                   ENDSL
041900011228
042000011227     C                   ENDDO
042100070402     c
042200070402     C                   ENDDO
042300070402     C                   ENDSR
042400070125     c**********************************************************************
042500070316     c* carica sfl
042600070125     c**********************************************************************
042700070316     C     Ries01        BEGSR
042800070330     c* Pulizia SFL
042900070327     c                   setoff                                       21
043000140811     C                   WRITE     LRY02c01
043100070316     c                   seton                                        21
043200070125     C                   EVAL      snrr1   = 0
043300070316     C                   EVAL      nrr1    = 0
043400141027     c                   eval      s_v1cdir=v1cdir
043500070330     c
043600070131      * gestione eventuali filiali in £6
043700070316    1c                   do        *hival        £6                3 0
043800070316    2c                   if        l6(£6)  = 0
043900070131     c                   leave
044000070316    2c                   endif
044100140526     c* Imposto il titolo
044200140526     c                   select
044300140526     c                   when      v1ccau='1'
044400140526     c                   eval      wtit2=Cosapp
044500140526     c                   when      v1ccau='2'
044600140526     c                   eval      wtit2=Cosffd
044700140526     c                   when      v1ccau='3'
044800140526     c                   eval      wtit2=Cosind
044900140526     c                   when      v1ccau='4'
045000140526     c                   eval      wtit2=Cosaltro
045100140526     c                   endsl
045200140813     c
045300140813     c                   clear                   arbaas
045400140813     c                   clear                   arblnp
045500140813     c                   clear                   arbnrs
045600140813     c                   clear                   arbnsp
045700070330     c
045800140812     c     l6(£6)        setll     tiidc05l
045900070316    2c                   do        *hival
046000140812     c     l6(£6)        reade     tiidc05l
046100070316      *
046200140812    3c                   if        %eof(tiidc05l)
046300070316     c                   leave
046400070316    3c                   endif
046500140813    c
046600140813     c* Se è la  stessa bolla che ho appena letto --> salto
046700140813     c                   if        idcaas=arbaas and idclnp=arblnp and
046800140814     c                             idcnrs=arbnrs and idcnsp=arbnsp
046900140813     c                   iter
047000140813     c                   endif
047100170111     c
047200170111     c                   clear                   wmaiaff           1
047300070131
047400160202      * se non trovo la bolla --> salto
047500140909     c     kidcA         chain     fnarb01l
047600160202    3c                   if        not %found(fnarb01l)
047700170111     c* verifico se bolla in partenza chiusa per merce mai aggidata
047800170111     c     kidcA         chain     fnblp01l
047900170111     c                   if        %found(fnblp01l) and arbcca<>' ' and
048000170111     c                             arbft2='N'
048100170111     c                   eval      wmaiaff='S'
048200170111     c                   endif
048300170111
048400170111     c                   if        wmaiaff=' '
048500170111     c                   iter
048600170111     c                   endif
048700170111     c                   endif
048800070131
048900070131      * bolle da escludere sempre
049000080418      * le spedizioni consegnate non le escludo perchè devo elaborare i
049100080418      *  ritorni all'incasso non abilitati dei C/ASS
049200140909    3c**                 if        arbfbc = '*' or arbfbc = 'B' or
049300140909     c**                           arbfbc = 'G'
049400140909     c**                 iter
049500140909    3c**                 endif
049600070330     c
049700070330     c
049800070330      * parzializzazione per RSD richiesto range a video
049900070330    3c                   if        v1crsd <> *blank and v1crsa <> *blank
050000070330     c                             and (arbrsd < v1crsd or arbrsd > v1crsa)
050100070330     c                   iter
050200070330    3c                   endif
050300070330     c
050400141027     c* Inclusione / esclusione dirottamenti
050500141028     c                   select
050600151112     c                   when      v1cdir='E' and idcfla<>' '
050700141027     c                   iter
050800151112     c                   when      v1cdir='I' and idcfla=' '
050900141028     c                   iter
051000141030     c                   when      v1cdir=' '
051100141030     c* nuova procedura: " " = Escludi
051200151119     c***                if         (%lookup('999':skFilOkdir)>0 or
051300151119     c***                           %lookup(%editc(v1cfgs:'X'):skFilOkdir)>0)
051400151119     c***                           and idcnewlna>0
051500151119     c
051600151119     c                   if        idcnewlna>0
051700141030     c                   iter
051800141030     c                   endif
051900151119     c
052000141028     c                   endsl
052100070330    c
052200070330     c* ********************
052300070330     c* Carico record in SFL
052400070330     c* ********************
052500070316     C                   move      *blank        V1sopz
052600070316     c                   movel     arbrsd        v1srsd
052700071203     c                   movel     arbprd        v1sprd
052800070316     c                   z-add     arbaas        v1saas
052900080908     c                   z-add     arblnp        v1hlnp
053000080908     c                   z-add     arbnrs        v1hnrs
053100080908     c                   z-add     arbnsp        v1hnsp
053200070330     c* Data spedizione
053300070330     c     *iso          movel     arbdsp        dataiso
053400070406     c                   movel     dataiso       datadmy
053500140812     c                   movel     datadmy       w1sdsp            8 0
053600140812     c                   movel     w1sdsp        v1sdsp
053700070330     c* Telefono destinatario
053800140919     c**                 if        idcteld<>*blanks
053900140919     c**                 Eval      V1sTeld = idcteld
054000140919     c**                 else
054100140919     c**                 Eval      V1sTeld = idctelavv
054200140919     c**                 endif
054300140813     c                   movel     idcprg        v1hprg
054400140813     c* data e ora richiesta
054500140813     c                   movel     idcinsdata    v1hinsdata
054600140812     c                   movel     idcinsdata    w0080             8 0
054700140527     c                   movel     3             g02err
054800140527     C                   Z-ADD     w0080         G02inv
054900140527     C                   MOVEL     *zeros        G02dat
055000140527     C                   CALL      'XSRDA8'
055100140527     C                   PARM                    WLBDA8
055200140919     c                   eval      v1sinsd=g02dat
055300140919     c                   eval      v1sinso=%int(%subst(%editc
055400140919     c                             (idcinsdata:'X'):9:4))
055500140919     c
055600140919     c**                 eval      v1sddt=%subst(%editc(g02dat:'X'):1:4)+
055700140919     c**                           %subst(%editc(g02dat:'X'):7:2)+' ' +
055800140919     c**                           %subst(%editc(idcinsdata:'X'):9:6)
055900140527     c
056000140527     c                   eval      v1sast=IDCTIPOcli
056100070330     c
056200140527     c                   clear                   v1snt1
056300140527     c* Per tipo "4" metto le disposiz nelle note
056400140527     c                   if        idctipodis='4'
056500140527     c                   eval      v1snt1=idcnote
056600140527     c                   endif
056700140814     c* Per tipo "2" riscrivo "Fermo Deposito"
056800140814     c                   if        idctipodis='2' and v1ccau<>' '
056900140814     c                   eval      v1snt1='Fermo Deposito'
057000140814     c                   endif
057100140814     c
057200140527     c* Per tipo "1" metto ConsRichiesta
057300140527     c                   if        idctipodis='1'
057400140527     c
057500140527     c                   movel     idcdcr        w0080             8 0
057600140527     c                   movel     3             g02err
057700140527     C                   Z-ADD     w0080         G02inv
057800140527     C                   MOVEL     *zeros        G02dat
057900140527     C                   CALL      'XSRDA8'
058000140527     C                   PARM                    WLBDA8
058100140527     c                   eval      v1snt1='IL '+%editw(g02dat:'  /  /    ')
058200140527     c                   if        idchcr>0
058300140527     c                   eval      v1snt1=%trim(v1snt1)+' '+
058400140527     c                                    %editw(idchcr: '  :  ')
058500140527     c                   endif
058600140527     c
058700140527     C                   IF        idcprcr='M'
058800140812     c                   eval      v1snt1=%trim(v1snt1) + '  '+
058900141008     c                                    'preferibilmente il MATTINO   '
059000140812     c                   endif
059100140812     C                   IF        idcprcr='P'
059200140812     c                   eval      v1snt1=%trim(v1snt1) + '  '+
059300141008     c                                    'preferibilmente il POMERIGGIO'
059400140527     c                   endif
059500140527     c                   endif
059600140812     c
059700151112     c* Per tipo "5" scrivo RESO
059800151112     c                   if        idctipodis='5'
059900151112     c                   eval      v1snt1= 'RESO'
060000170111     c                   if        wmaiaff='S'
060100170111     c                   eval      v1snt1=%trim(v1snt1)+
060200170111     C                             ' da CHIUDERE:' + REDUL +
060300170111     C                             ' sped.chiusa in partenza'+
060400170111     C                             ' per MERCE NON AFFIDATA' + UL
060500151112     c                   endif
060600170111     c                   endif
060700151112     c
060800151112     c* Per tipo "3" metto l'indirizzo
060900151112     c                   if        idctipodis='3'
061000141003     c* se dirottamento lo scrivo
061100151112     c                   if        idcfla<>' '
061200141003     c                   eval      v1snt1= 'Dirottamento a '+' '+idclod
061300141007     c                   eval      v1snt1=%trim(v1snt1)+ '('+idcprd+')' +
061400141007     c                             ' filiale : '+%editc(idcnewlna:'X')
061500151112     c
061600141003     c                   else
061700140812     c                   eval      v1snt1= idcIND+ ' ' +idclod
061800140812     c                   if        %subst(v1snt1:74:4)<>*blanks
061900141003     c                   eval      %subst(v1snt1:74:4)='('+idcprd+')'
062000140812     c                   else
062100141003     c                   eval      v1snt1=%trim(v1snt1)+ '('+idcprd+')'
062200140527     c                   endif
062300140812     c                   endif
062400141003     c                   endif
062500140527     c
062600140813      *
062700140813     c                   z-add     arbaas        k_aas
062800140813     c                   z-add     arblnp        k_lnp
062900140813     c                   z-add     arbnrs        k_nrs
063000140813     c                   z-add     arbnsp        k_nsp
063100140814     c                   exsr      CtrDispo
063200140814     c                   if        *in90
063300140814     c                   iter
063400140814     c                   endif
063500140813     c
063600070125      *esco per raggiunta massima capacità sfl
063700070316    3c                   if        snrr1 > 9990
063800070125     c                   leave
063900070316    3c                   endif
064000070125
064100070125     C                   EVAL      snrr1 = snrr1 + 1
064200140811     C                   WRITE     LRY02s01
064300070316    2C                   ENDDO
064400070316    1C                   enddo
064500070125
064600070316    1C                   IF        snrr1  > 0
064700070316     C                   EVAL      *IN20 = *ON
064800070125     C                   MOVE      1             OK                1 0
064900070125     C                   ELSE
065000141028     C                   EVAL      *IN20 = *Off
065100070125     C                   MOVE      0             OK                1 0
065200070316    1C                   ENDIF
065300070125     C                   EVAL      nrr1  = 1
065400070316     C                   EVAL      lasnrr1  = snrr1
065500070316     C                   EVAL      snrr1  = 1
065600070125     C                   EVAL      csrrrn = 1
065700070402     c
065800141028     c* ordino il subfile per provincia/ragione sociale dst.solo se caricati
065900141028     c* record
066000141028     c                   if        ok>0
066100141028     C                   EXSR      ordina
066200141028     c                   else
066300141028     c
066400141028     c* errore se non caricati record
066500141028     c                   if        ilry02fgs<>*blanks
066600141028     c                   seton                                        28
066700141028     c                   movel     msg(12)       v1cmsg
066800141028     c                   goto      fine
066900141028     c                   endif
067000141028     c                   endif
067100070125     C                   ENDSR
067200070504     C**************************************************************************
067300070507     c* Visuliazzo le abilitazioni della bolla
067400070507     c*  escludo quella richeista in prima videata, se richeista
067500070504     C**************************************************************************
067600070504     c     CerABI        BEGSR
067700140814     c                   clear                   wpos
067800140814     c                   clear                   v1sabi
067900140813     c                   clear                   v1hblk
068000070504     c                   z-add     1             x
068100070504     c                   clear                   sk8
068200070507     c*
068300070507    1c                   if        v1ccau=' '
068400140812     c                   eval      wpos=%scan('E':wblk)
068500140813    2c                   if        wpos>0
068600070504     c* Im wpos è contenuta la posizione della 1 abilitazione che
068700070504     c*  corrisponde al tipo spedizioni richiesto a video
068800140813     c*  poi di seguito visualizzo tutte le altre
068900070504     c                   if        wpos=1
069000140919     c                   movea     'ConsRic'     sk8(x)
069100070504     c                   endif
069200070504     c                   if        wpos=2
069300140919     c                   movea     'FerDep.'     sk8(x)
069400070504     c                   endif
069500070504     c                   if        wpos=3
069600140919     c                   movea     'IndirAl'     sk8(x)
069700070504     c                   endif
069800070504     c                   if        wpos=4
069900140919     c                   movea     'AltreIn'     sk8(x)
070000070504     c                   endif
070100151112     c                   if        wpos=5
070200151112     c                   movea     'Reso   '     sk8(x)
070300151112     c                   endif
070400070507     c
070500140813     c
070600140813    2c                   endif
070700140812    1c                   endif
070800070504     c                   clear                   wpos
070900070504     c
071000140527     c* Appuntamento
071100140813     c                   if        v1ccau<>' '
071200140813     c                   if        %subst(wblk:1:1)<>' 'and v1ccau<>'1'
071300141027     c                   movea     'consRic'     sk8(x)
071400140813     c                   endif
071500140527     c* Fermo Deposito
071600140813     c                   if        %subst(wblk:2:1)<>' 'and v1ccau<>'2'
071700140919     c                   movea     'FerDep.'     sk8(x)
071800140813     c                   endif
071900140527     c* cambio Indirizzo
072000140813     c                   if        %subst(wblk:3:1)<>' 'and v1ccau<>'3'
072100140919     c                   movea     'IndirAl'     sk8(x)
072200140813     c                   endif
072300140527     c* Altro
072400140813     c                   if        %subst(wblk:4:1)<>' 'and v1ccau<>'4'
072500140919     c                   movea     'Altro  '     sk8(x)
072600140813     c                   endif
072700151112     c* Reso
072800151112     c                   if        %subst(wblk:5:1)<>' 'and v1ccau<>'5'
072900151112     c                   movea     'Reso   '     sk8(x)
073000151112     c                   endif
073100140813     c                   endif
073200140814     c* se ne ho più di una imposto in "+" nel sfl
073300140814    3c                   if        savpiu='S'
073400140919     c                   eval      sk8(8)='+'
073500140814    3c                   endif
073600070504     c
073700070504     c                   movea     sk8           v1sabi
073800140813     c                   eval      v1hblk=wblk
073900070504     c                   ENDSR
074000070321     c**********************************************************************
074100070321     C     Gestionesfl   BEGSR
074200070321     c**********************************************************************
074300070321      * Elaborazione opzioni.
074400070321     c*
074500140811     c                   readc     LRY02s01
074600070402     c
074700070321     c                   dow       %eof=*oFF
074800070321     C                   IF        v1sopz <> *blanks
074900070321     c                   exsr      chiamo
075000070321     c                   move      *blank        v1sopz
075100140930     c                   z-add     snrr1         csrrrn
075200070402     c
075300070402     c* Se non premuto F12-proteggo opzione
075400140813     c                   if        wf6='S'
075500140813     c                   seton                                        10
075600140813     c                   endif
075700070402     c
075800140811     c                   update    LRY02s01
075900070402     c                   setoff                                       10
076000070321     C                   ENDIF
076100070402     c
076200140811     c                   readc(E)  LRY02s01
076300070321     C                   enddo
076400070321     C                   ENDSR
076500070321     c**********************************************************************
076600070321     C     chiamo        BEGSR
076700141028     c                   clear                   fnlry03ds2
076800140813     c                   clear                   wf6               1
076900140813     c                   if        v1cnsp>0
077000140813     c                   eval      ilry03aas=v1caas
077100140813     c                   eval      ilry03lnp=v1clnp
077200140813     c                   eval      ilry03nrs=v1cnrs
077300140813     c                   eval      ilry03nsp=v1cnsp
077400140813     c* imposto il primo progressivo da elaborare trovato
077500140813     c                   eval      ilry03prg=savprg
077600140813     c                   else
077700140813     c                   eval      ilry03aas=v1saas
077800140813     c                   eval      ilry03lnp=v1hlnp
077900140813     c                   eval      ilry03nrs=v1hnrs
078000140813     c                   eval      ilry03nsp=v1hnsp
078100140813     c                   eval      ilry03prg=v1hprg
078200140813     c                   endif
078300141006     c* imposto la filiale gestione
078400141006     c                   eval      ilry03fgs=v1cfgs
078500140813     c* Se richiesto causale contatto
078600140813    1c                   if        v1ccau<>' '
078700140813     c                   eval      ilry03tipd=v1ccau
078800140813     c                   else
078900140813     c                   eval      wpos=%scan('E':v1hblk)
079000140813     c                   movel     wpos          ilry03tipd
079100140813    1c                   endif
079200140813     c
079300140813     c                   clear                   wprima_v
079400140813     c                   clear                   xx
079500140813    1c                   do        *hival
079600140813    2c                   if        ilry03tipd=' '
079700140813     c                   leave
079800140813    2c                   endif
079900141028     c                   eval      ILRY03DF12='S'
080000141028     c                   eval      ILRY03DF21='N'
080100141030     c* se filiale non partita con nuova procedura abilito l'F21 per i dirott.
080200151119     c***                if        %lookup('999':skFilOkdir)=0 and
080300151119     c***                          %lookup(%editc(v1cfgs:'X'):skFilOkDir)=0
080400151119     c***                eval      ILRY03DF21='S'
080500151119     c***                endif
080600151119     c
080700141028     c                   eval      ILRY03DF06='S'
080800141028     c                   eval      ILRY03EF12='S'
080900141028     c                   eval      ILRY03EF21='S'
081000141028     c                   eval      ILRY03EF06='S'
081100140813     c
081200141028     c                   eval      kpjbu=fnlry03ds2
081300140813     c                   call      'FNLRY03R'
081400140813     c                   parm                    kpjba
081500141028     c                   eval      fnlry03ds2=kpjbu
081600140813    2c                   if        olry03f12=*blanks or olry03err<>' '
081700140813     c                   eval      wf6='S'
081800140813    2c                   endif
081900140813    c
082000140813     c* chaino file variazioni
082100140813     c                   z-add     ilry03aas     k_aas
082200140813     c                   z-add     ilry03lnp     k_lnp
082300140813     c                   z-add     ilry03nrs     k_nrs
082400140813     c                   z-add     ilry03nsp     k_nsp
082500140909     c     kidc          setll     tiidc01l
082600140909     c     kidc          reade     tiidc01l
082700140909    2c                   dow       not %eof(tiidc01l)
082800140813     c*
082900140909    3c                   if        idcelaflg='E'
083000140813     c                             and idcprg>ilry03prg
083100140813     c                   leave
083200140813    3c                   endif
083300140909     c     kidc          reade     tiidc01l
083400140813    2c                   enddo
083500140813     c
083600140909    2c                   if        not %eof(tiidc01l)
083700140813     c                   movel     idctipodis    ilry03tipd
083800140813     c                   eval      ilry03prg=idcprg
083900140813     c                   else
084000140813     c                   clear                   ilry03tipd
084100140813    2c                   endif
084200140813     c
084300140813    1c                   enddo
084400070321     C                   ENDSR
084500011227     c**********************************************************************
084600000607     C     *INZSR        BEGSR
084700011227     c**********************************************************************
084800000607      *
084900000607     C     *ENTRY        PLIST
085000000607     C                   PARM                    Kpjba
085100140930     c                   movel     kpjbu         fnlry02ds
085200141014
085300141014     c                   movel     kpjbu         savkpjbu
085400070123      *
085500070315     c     *dtaara       define    §azute        azuteds
085600070315     c     *dtaara       define    §datiute      ddatiute
085700070315     c                   in(e)     *dtaara
085800070315     c                   if        %error  or rsut = *blanks
085900070315     c                   clear                   tibs34ds
086000070315     c                   call      'TIBS34R'
086100070315     c                   parm                    tibs34ds
086200070315     c                   in        *dtaara
086300070315     c                   endif
086400141014     c                   Clear                   wabi
086500141027     c                   Clear                   v1cmsg
086600141014     c                   Clear                   dLat
086700141014
086800141028     c                   setoff                                       19
086900141030      * Aggancio tabella VPO record DECOFI816DIR
087000151119     c****               clear                   trulvpods
087100151119     c****               eval      ivpoke1 = 'DECOFI816DIR'
087200151119     c****               call      'TRULVPOR'
087300151119     c****               parm                    trulvpods
087400151119     c****               eval      skfilokdir=skfilok
087500151119     c
087600141028     c* PGM RICHIAMATO: è richiamato se presente fgs
087700141028     c                   if        ilry02fgs<>*blanks
087800141028     c* controllo correttezza della filiale gestione --> deve contenere solo numeri
087900141028     c                   if        %check(digits:ilry02fgs)>0
088000141028     c                   Seton                                        28
088100141028     C                   movel     msg(1)        v1cmsg
088200141028     c                   endif
088300141028     c* flag dirottamento; valori possibili:
088400141028     c*"I"=Includi
088500141028     C*"E"=Escludi
088600141028     c*" "=Escludi
088700141028     c                   if        ilry02dir<>'I' and
088800141028     c                             ilry02dir<>'E' and
088900141028     C                             ilry02dir<>' '
089000141028     c                   Seton                                        28
089100141028     C                   movel     msg(2)        v1cmsg
089200141028     c                   endif
089300141028
089400141028     c                   if        *in28
089500141028     c                   goto      fine
089600141028     c                   endif
089700141028     c                   seton                                        19
089800141028     c                   else
089900141014     c* PROGRAMMA NON RICHIAMATO:
090000141014      * Verifica errori e autorità profilo
090100141014s   1c                   Select
090200141014      * se ho errori nei dati utente esco dal pgm
090300141014w   1c                   When      DutErr = 'E'
090400141014     c                   GoTo      Fine
090500141014      * se non c'è l'abilitazione
090600141014      * --> se 1° livello, abilitazioni al terminal
090700141014      *     se 2° livello, abilitazioni al punto operativo
090800141014      *     se sede è impossibile ma se capita mando a fine il pgm
090900141014w   1c                   When      UteAut = *Blanks
091000141014if  2c                   If        DutLpo = '1'
091100141014     c                   Eval      wabi   = 'TP'
091200141014e   2c                   EndIf
091300141014if  2c                   If        DutLpo = '2'
091400141014     c                   Eval      wabi   = 'PO'
091500141014e   2c                   EndIf
091600141014if  2c                   If        DutLpo = 'S'
091700141014     c                   GoTo      Fine
091800141014e   2c                   EndIf
091900141014      * carica le abilitazioni del profilo
092000141014x   1c                   Other
092100141014     c                   Movel     UteFaf        Dute01
092200141014     c                   If        §Utembf <> *Blanks
092300141014     c                   Eval      wabi = §Utembf
092400141014     c                   Else
092500141014     c                   Eval      wabi = UteAut
092600141014     c                   EndIf
092700141014e   1c                   EndSl
092800141014
092900141014      * controllo se ok l'abilitazione dell'utente
093000141014     c                   Clear                   Tibs02ds
093100141014     c                   Eval      T02Mod = 'C'
093200141014     c                   Eval      T02Sif = knsif
093300141014     c                   Eval      T02Cod = 'LAT'
093400141014     c                   Movel(p)  wabi          T02Ke1
093500141014     c                   Call      'TIBS02R'
093600141014     c                   Parm                    kpjba
093700141014     c                   Parm                    Tibs02ds
093800141014if  1c                   If        T02Err = *Blanks
093900141014     c                   Eval      dLat = T02Uni
094000141014e   1c                   EndIf
094100141014      * errore o non abilitato
094200141014if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
094300141014     c                   Seton                                        28
094400141015     c                   eval      noaut='1'
094500141027     C                   movel     msg(16)       v1cmsg
094600141014   x1c                   Else
094700141014      * Reperimento dei P.O. gestibili dall'utente
094800141014     c                   clear                   TRUL31DS
094900141014     c                   clear                   TRUL31DS2
095000141014     c                   eval      I31abi = wabi
095100141014     c                   eval      I31cdi = DUTdis
095200141014     c                   eval      I31car = DUTare
095300141014     c                   eval      I31cpo = DUTpou
095400141014     c                   call      'TRUL31R'
095500141014     c                   parm                    KPJBA
095600141014     c                   parm                    TRUL31DS
095700141014     c                   parm                    TRUL31DS2
095800141014if  2c                   if        O31pog > *zeros
095900141014     c                   movea     O31pog        $pog
096000141014x   2c                   else
096100141014     c                   Seton                                        28
096200141027     c                   movel     MSG(16)       v1cmsg
096300141015     c                   eval      noaut='1'
096400141014e   2c                   endif
096500141014    1c                   endif
096600141030    1c                   endif
096700141014     c
096800141014     c                   movel     savkpjbu      kpjbu
096900030702      *reperimento data
097000070319     C                   TIME                    W0140            14 0
097100070319     C                   MOVE      W0140         WDAT              8 0
097200030702     C*
097300030702     C                   Z-ADD     WDAT          G02DAT
097400030702     C                   MOVEL     *BLANK        G02ERR
097500030702     C                   CALL      'XSRDA8'
097600030702     C                   PARM                    WLBDA8
097700030702     C* UDATE A 8 IN AAAA/MM/GG
097800030717     C                   Z-ADD     G02INV        WUDAte            8 0
097900000607      *
098000070123      *
098100070124     C     Karb          klist
098200070327     C                   kfld                    K_aas
098300070327     C                   kfld                    K_lnp
098400070327     C                   kfld                    K_nrs
098500070327     C                   kfld                    K_nsp
098600140909     C     KIDC          klist
098700140909     C                   kfld                    K_aas
098800140909     C                   kfld                    K_lnp
098900140909     C                   kfld                    K_nrs
099000140909     C                   kfld                    K_nsp
099100140909     C                   kfld                    K_ELAdata
099200070125      *
099300070201      *
099400140909     C     KidcA         klist
099500140812     C                   kfld                    idcaas
099600140812     C                   kfld                    idclnp
099700140812     C                   kfld                    idcnrs
099800140812     C                   kfld                    idcnsp
099900070201      *
100000070201     C     Ktab          klist
100100070201     C                   kfld                    tblkut
100200070201     C                   kfld                    tblcod
100300070201     C                   kfld                    tblkey
100400070522     c*
100500070522     C     Karg1         klist
100600070522     C                   kfld                    arbaas
100700070522     C                   kfld                    arblnp
100800070522     C                   kfld                    arbnrs
100900070522     C                   kfld                    arbnsp
101000070412     c
101100070201     C                   z-add     1             tblkut
101200070205      * carico filiali £1
101300070205     C                   clear                   trul06ds
101400070205     C                   movel     '£1'          d06cod
101500070320     C                   movel     simfel        d06key
101600070205     C                   movel     trul06ds      kpjbu
101700070205     C                   CALL      'TRUL06R'
101800070205     C                   parm                    kpjba
101900070315     c                   movel     kpjbu         trul06ds
102000070315     C                   movea     lin           l1
102100070320     C* CARICO TABELLA FILIALI GESTITE DA ALTRE  SU TUTTO L'AS
102200070320     C                   CLEAR                   trul06ds
102300070320     C                   MOVE      '£6'          D06COD
102400070320     c                   eval      d06esc = 'G'
102500070320     c                   movel     simfel        d06key
102600070320     C                   MOVEL     trul06ds      KPJBU
102700070320     C                   CALL      'TRUL06R'
102800070320     C                   PARM                    KPJBA
102900070320     C                   MOVEL     KPJBU         trul06ds
103000070320     C                   MOVEl     LIN           nog
103100030714
103200011227     C                   ENDSR
103300070125      *--------------------------------------------------------------------------------------------*
103400070125      * ordina - riscrive il sfl ordinato per località
103500070125      *--------------------------------------------------------------------------------------------*
103600070125     C     ordina        BEGSR
103700070125     C                   setoff                                       72
103800070125      * IMPOSTO ENTRATA AD 'A'
103900070125     C                   EVAL      snrr1 = 0
104000070125      *
104100070125     C                   CLEAR                   QLGSCB
104200070125     C                   CLEAR                   QLGSCB00
104300070125
104400071203     ** 2 campi chiave.
104500071203     C                   EVAL      QLGNBRK    = 2
104600071204     c                   select
104700140812     c* ordinamento per inserimento
104800071204     c                   when      w_ord=1
104900140812     ** inseriment dopo rag. sociale destinatario 2 byte,
105000140812     C                   EVAL      QLGSP      = 1 +%SIZE(v1srsd)+
105100140812     c                                          %SIZE(v1sprd)
105200140812     C                   EVAL      QLGSS      = %SIZE(v1hinsdata)
105300070125     C                   EVAL      QLGDT      = carattere
105400070125     C                   EVAL      QLGSO      = Ascendente
105500070125     C                   EVAL      QLGKL(1)   = QLGSKL
105600140812     ** ragione sociale
105700140812     C                   EVAL      QLGSP      = 1
105800140812     C                   EVAL      QLGSS      = %SIZE(v1srsd)
105900140812     C                   EVAL      QLGDT      = carattere
106000140812     C                   EVAL      QLGSO      = Ascendente
106100140812     C                   EVAL      QLGKL(2)   = QLGSKL
106200071204     c* ordinamento per rag.sociale/prv destinatario
106300071204     c                   when      w_ord=2
106400071204     C                   EVAL      QLGSP      = 1
106500071204     C                   EVAL      QLGSS      = %SIZE(v1srsd)
106600071204     C                   EVAL      QLGDT      = carattere
106700071204     C                   EVAL      QLGSO      = Ascendente
106800071204     C                   EVAL      QLGKL(1)   = QLGSKL
106900071204     ** ragione sociale a posizione 1, carattere, 24 byte,
107000071204     C                   EVAL      QLGSP      = 1 + %size(v1srsd)
107100071204     C                   EVAL      QLGSS      = %SIZE(v1sprd)
107200071204     C                   EVAL      QLGDT      = carattere
107300071204     C                   EVAL      QLGSO      = Ascendente
107400071204     C                   EVAL      QLGKL(2)   = QLGSKL
107500071204     c                   endsl
107600070125
107700070125      * Load other sort parameters.
107800070125     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
107900070125     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
108000070125     C                   EVAL      QLGRT     = 8
108100070125     C                   EVAL      QLGOKL    = 80
108200070125     C                   EVAL      QLGLKE    = 16
108300070125     C                   EVAL      QLGLSS    = 290
108400070125
108500070125      * Initialize Sort I/O API fields.
108600070125     C                   EVAL      QLGRL00  = QLGRL
108700070125     C                   EVAL      QLGRC00  = 1
108800070125     C                   CLEAR                   QUSEI
108900070125     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
109000070125
109100070125      * First step - Initialize the sort routine.
109200070125     C                   CALL      'QLGSORT'
109300070125     C                   PARM                    QLGSCB
109400070125     C                   PARM                    NotUsed
109500070125     C                   PARM                    NotUsed
109600070125     C                   PARM                    SizeList
109700070125     C                   PARM                    ReturnSize
109800070125     C                   PARM                    QUSEC
109900070125
110000070125      * Next step - Write records to I/O routine.
110100070125     C                   EVAL      QLGRT00 = Put
110200070125
110300070125     C                   DO        *hival        cx                4 0
110400070125
110500140811     C     cx            CHAIN     LRY02s01                           50
110600070125     c   50              leave
110700070125     ** Solo le righe con Selected = 'Y' sono riordinate,
110800070125     ** quindi per fare un ordinamento di tutte le righe
110900070125     ** metto 'Y' sempre.
111000070125     C                   EVAL      Selected = 'Y'
111100070125
111200070125     C                   CLEAR                   QUSEI
111300070125     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
111400070125     C                   CALL      'QLGSRTIO'
111500070125     C                   PARM                    QLGSCB00
111600070125     C                   PARM                    SflRcd
111700070125     C                   PARM                    NotUsed
111800070125     C                   PARM                    SizeList
111900070125     C                   PARM                    NotUsed
112000070125     C                   PARM                    QUSEC
112100070125
112200070125     C                   ENDDO
112300070125
112400070125      * Next step - Signal end of input, clear subfile for reload.
112500070125     C                   EVAL      QLGRT00 = EndPut
112600070125     C                   CLEAR                   QUSEI
112700070125     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
112800070125
112900070125     C                   CALL      'QLGSRTIO'
113000070125     C                   PARM                    QLGSCB00
113100070125     C                   PARM                    SflRcd
113200070125     C                   PARM                    NotUsed
113300070125     C                   PARM                    SizeList
113400070125     C                   PARM                    NotUsed
113500070125     C                   PARM                    QUSEC
113600070125
113700070316     C                   SETOff                                       21        *
113800140811     C                   WRITE     LRY02c01                                      *
113900070316     C                   SETOn                                        21        *
114000140811     C                   WRITE     LRY02z01                                      *
114100070125
114200070125      * Final step - Write the records back to the subfile.
114300070125     C                   EVAL      QLGRT00 = Get
114400070316     C                   DO        lasnrr1       x                 4 0
114500070125     C                   CLEAR                   QUSEI
114600070125     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
114700070125     C                   CALL      'QLGSRTIO'
114800070125     C                   PARM                    QLGSCB00
114900070125     C                   PARM                    NotUsed
115000070125     C                   PARM                    SflRcd
115100070125     C                   PARM                    QLGRL00
115200070125     C                   PARM                    NotUsed
115300070125     C                   PARM                    QUSEC
115400070125      *
115500070125     c                   z-add     x             snrr1
115600140811     C                   WRITE     LRY02s01
115700070125     C                   ENDDO
115800070125
115900070125      *
116000070125     C                   ENDSR
116100141030      *--------------------------------------------------------------------------------------------*
116200141030      * imposta a video il flag "Solo dirottamenti"
116300141030      *--------------------------------------------------------------------------------------------*
116400141030     C     sr_impodir    BEGSR
116500141030     c* PGM RICHIAMATO
116600141030     c* se fgs     partita con nuova procedura dir=' ' significa "Escludi"
116700141030     c* se fgs non partita con nuova procedura dir=' ' significa "Tutti"
116800141030     c                   if        ilry02fgs<>*blanks
116900141030     c                   eval      v1cdir=ilry02dir
117000141030     c                   if        ilry02dir=*blanks
117100151119     c***                if         %lookup('999':skFilOkdir)>0 or
117200151119     c***                           %lookup(%editc(v1cfgs:'X'):skFilOkdir)>0
117300151119     c*** SEMPRE "E"
117400141030     c                   movel     'E'           v1cdir
117500151119     c
117600151119     c***                endif
117700141030     c                   endif
117800141030     c                   else
117900141030     C* PGM NON RICHIAMATO
118000141030     c* default inclusione/esclusione dirottamenti
118100141030     c                   clear                   v1cdir
118200141030     c* se fgs     partita con nuova procedura dir=' ' significa "Escludi"
118300141030     c* se fgs non partita con nuova procedura dir=' ' significa "Tutti"
118400151119     c***                if         %lookup('999':skFilOkdir)>0 or
118500151119     c***                           %lookup(%editc(v1cfgs:'X'):skFilOkdir)>0
118600151119
118700151119     c*** SEMPRE "E"
118800141030     c                   movel     'E'           v1cdir
118900151119
119000151119     c****               endif
119100141030     c                   endif
119200141030     c                   endsr
119300070330**
119400070319Filiale in gestione errata
119500151116Richiesta errata. Valori possibilili: "I","E"," "
119600070402Selezionare per Numero Spedizione OPPURE altre parzializzazioni
119700070402Spedizione inesistente
119800070316Parzializzazione per ragione sociale errata: inserire correttamente
119900140527Tipo elaborazione obbligatorio
120000140527
120100140527La spedizione non prevede disposizioni da caricare in base al tipo immesso      8
120200140527
120300140527
120400140527
120500140527Nessuna disposizione da elaborare per la selezione fatta                       12
120600140527Spedizione non gestibile: bloccata con blocco " "                              13
120700140812Spedizione non gestibile: consegnata                                           14
120800070402Spedizione non gestibile: linea di arrivo non in gestione                      15
120900141015Utente non abilitato alla Gestione Disposizioni di Consegna                    16
