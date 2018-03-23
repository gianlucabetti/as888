000100030604     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200140811     H* FNLRY02R *---------------------------------------------------*
000300140523     H* - Gestione disposizioni di consegna
000400070403     H*--------------------------------------------------------------*
000500140811     FfnLRY02d  CF   E             WORKSTN
000600140811     f                                     sfile(LRY02s01:snrr1)
000700140812     Ffnarb01l  if   E           k disk
000800070316     Fazorg01l  if   E           k disk
000900070201     Ftabel00f  if   E           k disk
001000141027     F*fiarg01l  if   E           k disk
001100140909     Ftiidc01l  if   E           k disk
001200140812     Ftiidc05l  if   E           k disk    rename(tiidc000:tiidc005)
001300070504     d
001400070504     D sk8             S              1    DIM(8)
001500011228      *
001600011228     D Kpjba         E DS
001700140813     d
001800140930     D fnlry02ds     E DS
001900141028     D fnlry03ds2    E DS
002000011228      *
002100070522     D fidg09ds      E DS
002200070201     D fidn12ds      E DS
002300070315     d azuteds       e ds                  extname(azute00f)
002400070315     d ddatiute      e ds
002500141014     d dLat          e ds
002600141014     d dute01        e ds
002700141014     D tibs02ds      E DS
002800141014     D TRUL31DS      E DS
002900141014     D TRUL31DS2     E DS
003000070315     d tibs34ds      e ds                  inz
003100070131     D trul06ds      E DS
003200070131     D  lin                    1     90  0 dim(30)
003300151119     d*** TRULVPODS     e ds
003400151119     D***  skFilOk               16    765    dim(250)
003500141030
003600151119     D*** skFilOkdir      s                   like(skFilOk) dim(250) inz
003700070206     D* DATI X RICHIAMO PGM INTERR. BOLLE ARRIVI
003800070206     D PARAM1          DS                  INZ
003900070206     D  PA1AAS                14     17  0
004000070206     D  PA1LNP                18     20  0
004100070206     D  PA1NRS                21     22  0
004200070206     D  PA1NSP                23     29  0
004300070206     D  PA1F03                30     30
004400070206     D  PA1FLG                31     31
004500070206     D* SE =1 NON RICH. FNLG30R IN MODO RICORSIVO DA INTERR.BOLLE
004600070206     D  PA1GIA               144    144
004700070330     d
004800070330     D                 DS
004900070330     D  ARBAAS                 1      4  0
005000070330     D  ARBMGS                 5      8  0
005100070330     D  ARBDSP                 1      8  0
005200070330     d
005300070201     d ds3a          e ds
005400011228      *
005500140813     D savprg          S                   like(idcprg)
005600140813     D wtit2           S                   like(v1ctit2)
005700070327     d K_aas           s                   like(arbaas)
005800070327     d K_lnp           s                   like(arblnp)
005900070327     d K_nrs           s                   like(arbnrs)
006000070327     d K_nsp           s                   like(arbnsp)
006100140909     d K_ELADATA       s                   like(idceladata) inz
006200070430     D wprima_v        S              1
006300140813     D savpiu          S              1
006400030604     D snrr1           S              5i 0
006500070129     D lasnrr1         S              5i 0
006600030613     D WLBDA8          DS
006700030613     D  G02DAT                 1      8  0
006800030613     D  G02INV                 9     16  0
006900030613     D  G02ERR                17     17
007000030613     D  G02TGI                18     22  0
007100070319
007200070315     d  l6             s              3  0 dim(30)
007300070315     d  l1             s              3  0 dim(30)
007400070320     D nog             S              3  0 DIM(30)                              FIL.GESTITE
007500070330     d Dataiso         s               d   datfmt(*iso)
007600070406     d Datadmy         s               d   datfmt(*dmy)
007700140813     D wpos            S              1s 0
007800070504     D xx              S              1  0
007900141014     D savkpjbu        S                   LIKE(kpjbu)
008000141014     d wabi            s                   like(UteAut)
008100141014     D $pog            s              3    dim(250) inz(*zeros)
008200141014     D w003a           s              3
008300141015     D noaut           s              1    inz
008400141027     D s_v1cdir        s                   like(v1cdir)
008500070316      *---------------------------------------------------------------------
008600140930     D cosapp          C                   CONST('         Consegna Ri-
008700140919     D                                     chiesta       ')
008800070321     D cosffd          C                   CONST('         Fermo Depos-
008900070321     D                                     ito           ')
009000140919     D cosind          C                   CONST('       Indirizzo A-
009100140919     D                                     lternativo    ')
009200140919     D cosaltro        C                   CONST('         Altre Indic-
009300140919     D                                     azioni        ')
009400140527     D msg             S             78    DIM(16) CTDATA PERRCD(1)
009500071128     d weditora        c                   '  :  '
009600071128     d weditdta        c                   '  /  '
009700071204     d ord_ragprv      c                   'F11=Ord.xRagSoc/Prv'
009800140812     d ord_prvrag      c                   'F11=Ord.xDataInser.'
009900071204     d w_prvrsoc       C                   1
010000071204     d w_rsocprv       C                   2
010100071204     d W_ord           S              1  0
010200070125      *---------------------------------------------------------------------
010300070125      * specifiche per ordinamento forzato sfl
010400070125     d NotUsed         S             16A
010500070125     d ReturnSize      S              9B 0
010600070125     d SizeList        S              9B 0
010700070125     d*---
010800070125     d MaxKey          C                   2
010900070125     d Ascendente      C                   1
011000070125     d Discendente     C                   2
011100070125     d Carattere       C                   6
011200070125     d Put             C                   1
011300070125     d EndPut          C                   2
011400070125     d Get             C                   3
011500070125     d Numerico        C                   8
011600141028     D digits          c                   '0123456789'
011700070125     d*---
011800070125     d SflRcd          DS
011900070316     d  v1srsd
012000071203     d  v1sprd
012100140812     d  V1hinsdata
012200070316     d  V1sAAS
012300080908     d  V1hLNP
012400080908     d  V1hNRS
012500080908     d  V1hNSP
012600140919     d  V1sinsd
012700140919     d  V1sinso
012800070330     d  V1sdsp
012900070504     d  V1sabi
013000071031     d  V1sast
013100070402     d  V1snt1
013200140813     d  V1hprg
013300140813     d  V1hblk
013400070125     d  Selected                      1A
013500070125
013600070125      /COPY QSYSINC/QRPGLESRC,QLGSORT
013700070125     d QLGKL                         16    DIM(MaxKey)
013800070125     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
013900070125     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
014000070125     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
014100070125     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
014200070125
014300070125      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
014400070125      /COPY QSYSINC/QRPGLESRC,QUSEC
014500141027
014600070502     c                   exsr      inzfgs
014700141030     c                   exsr      sr_impodir
014800000608      *
014900141028     c* Gestione subfile
015000070316     C                   EXSR      ges_s01
015100070402     c
015200140930    c
015300070402     c
015400011227      *
015500141014
015600141014     c     fine          tag
015700141028
015800141028    4c                   if        ilry02fgs<>*blanks and *in28
015900141028     c                   eval      OLRY02MSG=v1cmsg
016000141028     c                   eval      OLRY02err='1'
016100141028     c                   endif
016200070321      * Chiusura con lr
016300140813     c                   eval      ilry03tla='C'
016400141028     c                   eval      kpjbu=fnlry03ds2
016500140813     c                   call      'FNLRY03R'
016600140813     c                   parm                    kpjba
016700140930     c
016800140930     c                   movel(p)  fnlry02ds     kpjbu
016900140813     c*
017000070315     C                   seton                                        lr
017100070502     c**********************************************************************
017200070502     c* inizializza p.o. in gestione
017300070502     c**********************************************************************
017400070502     c     inzfgs        begsr
017500140930     c
017600140930     c* se passata la filiale gestione la imposto, altrimenti prendo lo stesso dutpou
017700141028     c                   if        ILRY02fgs<>*blanks
017800140930     c                   eval      *in16=*off
017900140930     c                   movel     ILRY02fgs     v1cfgs
018000140930     c                   else
018100140930     c                   eval      v1cfgs = dutpou
018200140930     c                   endif
018300140930     c
018400140930     c
018500070502     c     v1cfgs        chain     azorg01L
018600070502     c                   if        %found(azorg01l) and orgfva = *blanks
018700070502     c                   eval      v1dfgs = orgdes
018800070502     c                   endif
018900070502     c*
019000140930      * disabilito f18 se p.o. di secondo livello o richiamato
019100141014     c*****              eval      *in17 = (dutlpo = '2' or dutlpo = *blanks)
019200141014     c                   if        $pog(2)=*zeros
019300141014     c                   eval      *in17=*on
019400141014     c                   endif
019500070502     c                   eval      *in16=*off
019600141014     c                   exsr      sr_car£6
019700070502     c                   endsr
019800070316     c**********************************************************************
019900070316     c* Caricamento £6 della filiale in gestione
020000070316     c**********************************************************************
020100070316     C     SR_car£6      BEGSR
020200070316      * carico filiali £6
020300070316     C                   clear                   trul06ds
020400070316     C                   movel     '£6'          d06cod
020500070316     C                   movel     v1cfgs        d06key
020600070316     C                   movel     trul06ds      kpjbu
020700070316     C                   CALL      'TRUL06R'
020800070316     C                   parm                    kpjba
020900070316     C                   movel     kpjbu         trul06ds
021000070316     C                   movea     lin           l6
021100070316     c                   endsr
021200070315     c**********************************************************************
021300070315     c* Controllo filiale in gestione
021400070315     c**********************************************************************
021500070315     C     Ctrfgs        BEGSR
021600070315     c*
021700070315     c                   setoff                                       40
021800070315     c                   clear                   v1dfgs
021900070320      * Il p.o. gestione non può essere un p.o. gestito
022000070320     c     V1cFgs        Lookup    Nog                                    30
022100070320     c                   If        *In30
022200070320     c                   Eval      *In40 = *On
022300070320     c                   Eval      *In28 = *On
022400141027     c                   Eval      V1cMsg = Msg(1)
022500070320     c                   leavesr
022600070320     c                   EndIf
022700141014     c                   move      v1cfgs        w003a
022800141014     c     w003a         lookup    $pog                                   30
022900070315     c                   if        not *in30
023000070315     c                   seton                                        2840
023100141027     c                   eval      v1cmsg = msg(01)
023200070315     c                   leavesr
023300070315     c                   endif
023400070315
023500070315      * decodifica p.o. in gestione
023600070315     c     v1cfgs        chain     azorg01L
023700070315     c                   if        %found(azorg01l) and orgfva = *blanks
023800070315     c                   eval      v1dfgs = orgdes
023900141027     c                   endif
024000070315     c                   setoff                                       16
024100070417      *
024200070316     c                   exsr      sr_car£6
024300141030     c* Imposto il flag "Solo dirottamenti" in base alla nuova filiale gestione
024400141030     c                   exsr      sr_impodir
024500070417      *
024600070315     c                   endsr
024700140814     c**********************************************************************
024800141008     c* controllo il tipo disposizione in base alle richieste
024900140814     c**********************************************************************
025000140814     c     CtrDispo      BEGSR
025100140814     c                   setoff                                       90
025200140814     c                   exsr      ctrspecau
025300140814     c                   exsr      cerAbi
025400140814     c
025500140814    2c                   if        *in08
025600140814     c                   seton                                        90
025700140814   x2c                   else
025800140814    3c                   if        (wblk=*blanks and v1ccau=' ') or
025900140814     c                             (%subst(wblk:1:1)<>'E' and v1ccau='1') or
026000140814     c                             (%subst(wblk:2:1)<>'E' and v1ccau='2') or
026100140814     c                             (%subst(wblk:3:1)<>'E' and v1ccau='3') or
026200151112     c                             (%subst(wblk:4:1)<>'E' and v1ccau='4') or
026300151112     c                             (%subst(wblk:5:1)<>'E' and v1ccau='5')
026400140814     c                   seton                                        90
026500140814    3c                   endif
026600140814    2c                   endif
026700140814    2c                   ENDSR
026800070316     c**********************************************************************
026900140526     c* Controllo congruenza fra causale disposiz e spedizione
027000070316     c**********************************************************************
027100070316     C     Ctrspecau     BEGSR
027200151112     c                   clear                   wblk              5
027300070504     c                   setoff                                       08
027400070504     c
027500070402     c                   move      '3A'          tblcod
027600070402     c                   movel(p)  arbcbo        tblkey
027700070402     c                   exsr      ctabel
027800070402     c                   movel     tbluni        ds3a
027900070402     c
028000140527     c* Escludo bolle di recupero
028100140526    1c                   if        §3arbl='R'
028200070504     c                   seton                                        08
028300140526    1c                   endif
028400070402     c
028500140813     c                   clear                   savprg
028600140813     c                   clear                   savpiu
028700140523     c* chaino file variazioni
028800140909     c     kidc          setll     tiidc01l
028900140909     c     kidc          reade     tiidc01l
029000140909    1c                   dow       not %eof(tiidc01l)
029100070402     c*
029200140909     c                   if        idcelaflg='E'
029300140813     c
029400140813     c* mi salvo ilprimo progressivo che trovo
029500140813     c                   if        savprg=0
029600140813     c                   eval      savprg=idcprg
029700140813     c                   else
029800140813     c                   eval      savpiu ='S'
029900140813     c                   endif
030000140813     c
030100140814    2c**                 select
030200140814     c**                 when      v1ccau='1'  and idctipodis='1'
030300140814     c**                 eval      %subst(wblk:1:1)=idcelaflg
030400140814     c**                 when      v1ccau='2'  and idctipodis='2'
030500140814     c**                 eval      %subst(wblk:2:1)=idcelaflg
030600140814     c**                 when      v1ccau='3'  and idctipodis='3'
030700140814     c**                 eval      %subst(wblk:3:1)=idcelaflg
030800140814     c**                 when      v1ccau='4'  and idctipodis='4'
030900140814     c**                 eval      %subst(wblk:4:1)=idcelaflg
031000070504     c
031100070503     c* se non impostato metto se ha almeno una abilitazione
031200140814     c**                 when      v1ccau=' '
031300140526     c                   if        idctipodis='1'
031400140527     c                   eval      %subst(wblk:1:1)=idcelaflg
031500140526     c                   endif
031600140526     c                   if        idctipodis='2'
031700140527     c                   eval      %subst(wblk:2:1)=idcelaflg
031800140526     c                   endif
031900140526     c                   if        idctipodis='3'
032000140527     c                   eval      %subst(wblk:3:1)=idcelaflg
032100140526     c                   endif
032200140526     c                   if        idctipodis='4'
032300140527     c                   eval      %subst(wblk:4:1)=idcelaflg
032400140526     c                   endif
032500151112     c                   if        idctipodis='5'
032600151112     c                   eval      %subst(wblk:5:1)=idcelaflg
032700151112     c                   endif
032800070504     c
032900140814    2c**                 endsl
033000140812     c                   endif
033100140526     c
033200140909     c     kidc          reade     tiidc01l
033300140812    1c                   enddo
033400070402     c
033500070316     c                   endsr
033600070316     C*--- CHAIN TABEL00F --------------------------------------------*
033700070316     C     CTABEL        BEGSR
033800070316     C     KTAB          CHAIN     TABEL                              30
033900070316     c                   if        %found(tabel00f) and tblflg<> ' '
034000070316     C                   SETON                                        30
034100070316     C                   END
034200070316     C                   ENDSR
034300011227     c**********************************************************************
034400140814     C* gestione sfl
034500011227     c**********************************************************************
034600070316     C     ges_s01       BEGSR
034700000627      *
034800141027
034900070402     c                   eval      *inke='1'
035000070402     c* looppo per F5 - rivisulizza
035100070402     c                   dow       *inke='1'
035200070402     c
035300070402     c                   eval      *inke='0'
035400141028     c                   eval      w_ord=w_prvrsoc
035500070316     C                   EXSR      ries01
035600070316      * Imposto sottotitolo seconda videata
035700070316     c                   eval      v1ctit2=wtit2
035800070330     c
035900071204     c                   eval      v1tf11=ord_ragprv
036000141008     c                   seton                                        11
036100070316     c*
036200070316     C                   DO        *hival
036300140811     C                   WRITE     LRY02T01
036400140811     C                   WRITE     LRY02z01
036500141028     c                   if        *in20=*off
036600141028     c                   write     lry02n01
036700141028     c                   endif
036800140811     C                   EXFMT     LRY02c01
036900141027     c                   setoff                                       28
037000141027     c                   if        noaut='1'
037100141027     c                   leavesr
037200141027     c                   endif
037300011227     C                   SELECT
037400141027      * F18=cambio filiale - (Sproteggo cambio p.o gestione)
037500141027     c                   when      *inks
037600141027     c                   seton                                        16
037700141027     c                   iter
037800141027     c
037900070402     c* esco per F3-Fine  F12-Ritorno  F5-rivisualizza
038000070402     C                   WHEN      *INKL=*ON or *inkc  or *inke
038100070316     C                   leave
038200071204     c* f11=Cambio ordinamento sfl
038300071204     C                   WHEN      *INkk
038400071204     c                   select
038500071204     c                   when      w_ord=w_prvrsoc
038600071204     c                   eval      w_ord=w_rsocprv
038700071204     c                   eval      v1tf11=ord_prvrag
038800071204     c                   when      w_ord=w_rsocprv
038900071204     c                   eval      w_ord=w_prvrsoc
039000071204     c                   eval      v1tf11=ord_ragprv
039100071204     c                   endsl
039200141112     c* Solo se presenti record nel sfl
039300141112     c                   if        *in20
039400071204     C                   exsr      ordina
039500141112     c                   endif
039600071204     c                   iter
039700141027     c* Cambio fil. gestione
039800141027     c                   when      *in16
039900141027     c                   exsr      ctrfgs
040000141027     c                   if        not *in28
040100141027     c                   exsr      ries01
040200141027     c                   endif
040300141027     c                   iter
040400141027     c* cambio parzializzazione
040500141027     c                   when      v1cdir<>s_v1cdir
040600141027     c                   exsr      ries01
040700141027     c                   iter
040800141027
040900011227     C                   OTHER
041000011227
041100141027      * controllo filiale in gestione
041200141112     C   20              EXSR      GestioneSFL
041300011227     C                   ENDSL
041400011228
041500011227     C                   ENDDO
041600070402     c
041700070402     C                   ENDDO
041800070402     C                   ENDSR
041900070125     c**********************************************************************
042000070316     c* carica sfl
042100070125     c**********************************************************************
042200070316     C     Ries01        BEGSR
042300070330     c* Pulizia SFL
042400070327     c                   setoff                                       21
042500140811     C                   WRITE     LRY02c01
042600070316     c                   seton                                        21
042700070125     C                   EVAL      snrr1   = 0
042800070316     C                   EVAL      nrr1    = 0
042900141027     c                   eval      s_v1cdir=v1cdir
043000070330     c
043100070131      * gestione eventuali filiali in £6
043200070316    1c                   do        *hival        £6                3 0
043300070316    2c                   if        l6(£6)  = 0
043400070131     c                   leave
043500070316    2c                   endif
043600140526     c* Imposto il titolo
043700140526     c                   select
043800140526     c                   when      v1ccau='1'
043900140526     c                   eval      wtit2=Cosapp
044000140526     c                   when      v1ccau='2'
044100140526     c                   eval      wtit2=Cosffd
044200140526     c                   when      v1ccau='3'
044300140526     c                   eval      wtit2=Cosind
044400140526     c                   when      v1ccau='4'
044500140526     c                   eval      wtit2=Cosaltro
044600140526     c                   endsl
044700140813     c
044800140813     c                   clear                   arbaas
044900140813     c                   clear                   arblnp
045000140813     c                   clear                   arbnrs
045100140813     c                   clear                   arbnsp
045200070330     c
045300140812     c     l6(£6)        setll     tiidc05l
045400070316    2c                   do        *hival
045500140812     c     l6(£6)        reade     tiidc05l
045600070316      *
045700140812    3c                   if        %eof(tiidc05l)
045800070316     c                   leave
045900070316    3c                   endif
046000140813    c
046100140813     c* Se è la  stessa bolla che ho appena letto --> salto
046200140813     c                   if        idcaas=arbaas and idclnp=arblnp and
046300140814     c                             idcnrs=arbnrs and idcnsp=arbnsp
046400140813     c                   iter
046500140813     c                   endif
046600070131
046700160202      * se non trovo la bolla --> salto
046800140909     c     kidcA         chain     fnarb01l
046900160202    3c                   if        not %found(fnarb01l)
046901160202     c                   iter
046902160202     c                   endif
047000070131
047100070131      * bolle da escludere sempre
047200080418      * le spedizioni consegnate non le escludo perchè devo elaborare i
047300080418      *  ritorni all'incasso non abilitati dei C/ASS
047400140909    3c**                 if        arbfbc = '*' or arbfbc = 'B' or
047500140909     c**                           arbfbc = 'G'
047600140909     c**                 iter
047700140909    3c**                 endif
047800070330     c
047900070330     c
048000070330      * parzializzazione per RSD richiesto range a video
048100070330    3c                   if        v1crsd <> *blank and v1crsa <> *blank
048200070330     c                             and (arbrsd < v1crsd or arbrsd > v1crsa)
048300070330     c                   iter
048400070330    3c                   endif
048500070330     c
048800141027     c* Inclusione / esclusione dirottamenti
048900141028     c                   select
049000151112     c                   when      v1cdir='E' and idcfla<>' '
049100141027     c                   iter
049200151112     c                   when      v1cdir='I' and idcfla=' '
049300141028     c                   iter
049400141030     c                   when      v1cdir=' '
049500141030     c* nuova procedura: " " = Escludi
049600151119     c***                if         (%lookup('999':skFilOkdir)>0 or
049700151119     c***                           %lookup(%editc(v1cfgs:'X'):skFilOkdir)>0)
049800151119     c***                           and idcnewlna>0
049900151119     c
050000151119     c                   if        idcnewlna>0
050100141030     c                   iter
050200141030     c                   endif
050300151119     c
050400141028     c                   endsl
050500070330    c
050600070330     c* ********************
050700070330     c* Carico record in SFL
050800070330     c* ********************
050900070316     C                   move      *blank        V1sopz
051000070316     c                   movel     arbrsd        v1srsd
051100071203     c                   movel     arbprd        v1sprd
051200070316     c                   z-add     arbaas        v1saas
051300080908     c                   z-add     arblnp        v1hlnp
051400080908     c                   z-add     arbnrs        v1hnrs
051500080908     c                   z-add     arbnsp        v1hnsp
051600070330     c* Data spedizione
051700070330     c     *iso          movel     arbdsp        dataiso
051800070406     c                   movel     dataiso       datadmy
051900140812     c                   movel     datadmy       w1sdsp            8 0
052000140812     c                   movel     w1sdsp        v1sdsp
052100070330     c* Telefono destinatario
052200140919     c**                 if        idcteld<>*blanks
052300140919     c**                 Eval      V1sTeld = idcteld
052400140919     c**                 else
052500140919     c**                 Eval      V1sTeld = idctelavv
052600140919     c**                 endif
052700140813     c                   movel     idcprg        v1hprg
052800140813     c* data e ora richiesta
052900140813     c                   movel     idcinsdata    v1hinsdata
053000140812     c                   movel     idcinsdata    w0080             8 0
053100140527     c                   movel     3             g02err
053200140527     C                   Z-ADD     w0080         G02inv
053300140527     C                   MOVEL     *zeros        G02dat
053400140527     C                   CALL      'XSRDA8'
053500140527     C                   PARM                    WLBDA8
053600140919     c                   eval      v1sinsd=g02dat
053700140919     c                   eval      v1sinso=%int(%subst(%editc
053800140919     c                             (idcinsdata:'X'):9:4))
053900140919     c
054000140919     c**                 eval      v1sddt=%subst(%editc(g02dat:'X'):1:4)+
054100140919     c**                           %subst(%editc(g02dat:'X'):7:2)+' ' +
054200140919     c**                           %subst(%editc(idcinsdata:'X'):9:6)
054300140527     c
054400140527     c                   eval      v1sast=IDCTIPOcli
054500070330     c
054600140527     c                   clear                   v1snt1
054700140527     c* Per tipo "4" metto le disposiz nelle note
054800140527     c                   if        idctipodis='4'
054900140527     c                   eval      v1snt1=idcnote
055000140527     c                   endif
055100140814     c* Per tipo "2" riscrivo "Fermo Deposito"
055200140814     c                   if        idctipodis='2' and v1ccau<>' '
055300140814     c                   eval      v1snt1='Fermo Deposito'
055400140814     c                   endif
055500140814     c
055600140527     c* Per tipo "1" metto ConsRichiesta
055700140527     c                   if        idctipodis='1'
055800140527     c
055900140527     c                   movel     idcdcr        w0080             8 0
056000140527     c                   movel     3             g02err
056100140527     C                   Z-ADD     w0080         G02inv
056200140527     C                   MOVEL     *zeros        G02dat
056300140527     C                   CALL      'XSRDA8'
056400140527     C                   PARM                    WLBDA8
056500140527     c                   eval      v1snt1='IL '+%editw(g02dat:'  /  /    ')
056600140527     c                   if        idchcr>0
056700140527     c                   eval      v1snt1=%trim(v1snt1)+' '+
056800140527     c                                    %editw(idchcr: '  :  ')
056900140527     c                   endif
057000140527     c
057100140527     C                   IF        idcprcr='M'
057200140812     c                   eval      v1snt1=%trim(v1snt1) + '  '+
057300141008     c                                    'preferibilmente il MATTINO   '
057400140812     c                   endif
057500140812     C                   IF        idcprcr='P'
057600140812     c                   eval      v1snt1=%trim(v1snt1) + '  '+
057700141008     c                                    'preferibilmente il POMERIGGIO'
057800140527     c                   endif
057900140527     c                   endif
058000140812     c
058100151112     c* Per tipo "5" scrivo RESO
058200151112     c                   if        idctipodis='5'
058300151112     c                   eval      v1snt1= 'RESO'
058400151112     c                   endif
058500151112     c
058600151112     c* Per tipo "3" metto l'indirizzo
058700151112     c                   if        idctipodis='3'
058800141003     c* se dirottamento lo scrivo
058900151112     c                   if        idcfla<>' '
059000141003     c                   eval      v1snt1= 'Dirottamento a '+' '+idclod
059100141007     c                   eval      v1snt1=%trim(v1snt1)+ '('+idcprd+')' +
059200141007     c                             ' filiale : '+%editc(idcnewlna:'X')
059300151112     c
059400141003     c                   else
059500140812     c                   eval      v1snt1= idcIND+ ' ' +idclod
059600140812     c                   if        %subst(v1snt1:74:4)<>*blanks
059700141003     c                   eval      %subst(v1snt1:74:4)='('+idcprd+')'
059800140812     c                   else
059900141003     c                   eval      v1snt1=%trim(v1snt1)+ '('+idcprd+')'
060000140527     c                   endif
060100140812     c                   endif
060200141003     c                   endif
060300140527     c
060400140813      *
060500140813     c                   z-add     arbaas        k_aas
060600140813     c                   z-add     arblnp        k_lnp
060700140813     c                   z-add     arbnrs        k_nrs
060800140813     c                   z-add     arbnsp        k_nsp
060900140814     c                   exsr      CtrDispo
061000140814     c                   if        *in90
061100140814     c                   iter
061200140814     c                   endif
061300140813     c
061400070125      *esco per raggiunta massima capacità sfl
061500070316    3c                   if        snrr1 > 9990
061600070125     c                   leave
061700070316    3c                   endif
061800070125
061900070125     C                   EVAL      snrr1 = snrr1 + 1
062000140811     C                   WRITE     LRY02s01
062100070316    2C                   ENDDO
062200070316    1C                   enddo
062300070125
062400070316    1C                   IF        snrr1  > 0
062500070316     C                   EVAL      *IN20 = *ON
062600070125     C                   MOVE      1             OK                1 0
062700070125     C                   ELSE
062800141028     C                   EVAL      *IN20 = *Off
062900070125     C                   MOVE      0             OK                1 0
063000070316    1C                   ENDIF
063100070125     C                   EVAL      nrr1  = 1
063200070316     C                   EVAL      lasnrr1  = snrr1
063300070316     C                   EVAL      snrr1  = 1
063400070125     C                   EVAL      csrrrn = 1
063500070402     c
063600141028     c* ordino il subfile per provincia/ragione sociale dst.solo se caricati
063700141028     c* record
063800141028     c                   if        ok>0
063900141028     C                   EXSR      ordina
064000141028     c                   else
064100141028     c
064200141028     c* errore se non caricati record
064300141028     c                   if        ilry02fgs<>*blanks
064400141028     c                   seton                                        28
064500141028     c                   movel     msg(12)       v1cmsg
064600141028     c                   goto      fine
064700141028     c                   endif
064800141028     c                   endif
064900070125     C                   ENDSR
065000070504     C**************************************************************************
065100070507     c* Visuliazzo le abilitazioni della bolla
065200070507     c*  escludo quella richeista in prima videata, se richeista
065300070504     C**************************************************************************
065400070504     c     CerABI        BEGSR
065500140814     c                   clear                   wpos
065600140814     c                   clear                   v1sabi
065700140813     c                   clear                   v1hblk
065800070504     c                   z-add     1             x
065900070504     c                   clear                   sk8
066000070507     c*
066100070507    1c                   if        v1ccau=' '
066200140812     c                   eval      wpos=%scan('E':wblk)
066300140813    2c                   if        wpos>0
066400070504     c* Im wpos è contenuta la posizione della 1 abilitazione che
066500070504     c*  corrisponde al tipo spedizioni richiesto a video
066600140813     c*  poi di seguito visualizzo tutte le altre
066700070504     c                   if        wpos=1
066800140919     c                   movea     'ConsRic'     sk8(x)
066900070504     c                   endif
067000070504     c                   if        wpos=2
067100140919     c                   movea     'FerDep.'     sk8(x)
067200070504     c                   endif
067300070504     c                   if        wpos=3
067400140919     c                   movea     'IndirAl'     sk8(x)
067500070504     c                   endif
067600070504     c                   if        wpos=4
067700140919     c                   movea     'AltreIn'     sk8(x)
067800070504     c                   endif
067900151112     c                   if        wpos=5
068000151112     c                   movea     'Reso   '     sk8(x)
068100151112     c                   endif
068200070507     c
068300140813     c
068400140813    2c                   endif
068500140812    1c                   endif
068600070504     c                   clear                   wpos
068700070504     c
068800140527     c* Appuntamento
068900140813     c                   if        v1ccau<>' '
069000140813     c                   if        %subst(wblk:1:1)<>' 'and v1ccau<>'1'
069100141027     c                   movea     'consRic'     sk8(x)
069200140813     c                   endif
069300140527     c* Fermo Deposito
069400140813     c                   if        %subst(wblk:2:1)<>' 'and v1ccau<>'2'
069500140919     c                   movea     'FerDep.'     sk8(x)
069600140813     c                   endif
069700140527     c* cambio Indirizzo
069800140813     c                   if        %subst(wblk:3:1)<>' 'and v1ccau<>'3'
069900140919     c                   movea     'IndirAl'     sk8(x)
070000140813     c                   endif
070100140527     c* Altro
070200140813     c                   if        %subst(wblk:4:1)<>' 'and v1ccau<>'4'
070300140919     c                   movea     'Altro  '     sk8(x)
070400140813     c                   endif
070500151112     c* Reso
070600151112     c                   if        %subst(wblk:5:1)<>' 'and v1ccau<>'5'
070700151112     c                   movea     'Reso   '     sk8(x)
070800151112     c                   endif
070900140813     c                   endif
071000140814     c* se ne ho più di una imposto in "+" nel sfl
071100140814    3c                   if        savpiu='S'
071200140919     c                   eval      sk8(8)='+'
071300140814    3c                   endif
071400070504     c
071500070504     c                   movea     sk8           v1sabi
071600140813     c                   eval      v1hblk=wblk
071700070504     c                   ENDSR
071800070321     c**********************************************************************
071900070321     C     Gestionesfl   BEGSR
072000070321     c**********************************************************************
072100070321      * Elaborazione opzioni.
072200070321     c*
072300140811     c                   readc     LRY02s01
072400070402     c
072500070321     c                   dow       %eof=*oFF
072600070321     C                   IF        v1sopz <> *blanks
072700070321     c                   exsr      chiamo
072800070321     c                   move      *blank        v1sopz
072900140930     c                   z-add     snrr1         csrrrn
073000070402     c
073100070402     c* Se non premuto F12-proteggo opzione
073200140813     c                   if        wf6='S'
073300140813     c                   seton                                        10
073400140813     c                   endif
073500070402     c
073600140811     c                   update    LRY02s01
073700070402     c                   setoff                                       10
073800070321     C                   ENDIF
073900070402     c
074000140811     c                   readc(E)  LRY02s01
074100070321     C                   enddo
074200070321     C                   ENDSR
074300070321     c**********************************************************************
074400070321     C     chiamo        BEGSR
074500141028     c                   clear                   fnlry03ds2
074600140813     c                   clear                   wf6               1
074700140813     c                   if        v1cnsp>0
074800140813     c                   eval      ilry03aas=v1caas
074900140813     c                   eval      ilry03lnp=v1clnp
075000140813     c                   eval      ilry03nrs=v1cnrs
075100140813     c                   eval      ilry03nsp=v1cnsp
075200140813     c* imposto il primo progressivo da elaborare trovato
075300140813     c                   eval      ilry03prg=savprg
075400140813     c                   else
075500140813     c                   eval      ilry03aas=v1saas
075600140813     c                   eval      ilry03lnp=v1hlnp
075700140813     c                   eval      ilry03nrs=v1hnrs
075800140813     c                   eval      ilry03nsp=v1hnsp
075900140813     c                   eval      ilry03prg=v1hprg
076000140813     c                   endif
076100141006     c* imposto la filiale gestione
076200141006     c                   eval      ilry03fgs=v1cfgs
076300140813     c* Se richiesto causale contatto
076400140813    1c                   if        v1ccau<>' '
076500140813     c                   eval      ilry03tipd=v1ccau
076600140813     c                   else
076700140813     c                   eval      wpos=%scan('E':v1hblk)
076800140813     c                   movel     wpos          ilry03tipd
076900140813    1c                   endif
077000140813     c
077100140813     c                   clear                   wprima_v
077200140813     c                   clear                   xx
077300140813    1c                   do        *hival
077400140813    2c                   if        ilry03tipd=' '
077500140813     c                   leave
077600140813    2c                   endif
077700141028     c                   eval      ILRY03DF12='S'
077800141028     c                   eval      ILRY03DF21='N'
077900141030     c* se filiale non partita con nuova procedura abilito l'F21 per i dirott.
078000151119     c***                if        %lookup('999':skFilOkdir)=0 and
078100151119     c***                          %lookup(%editc(v1cfgs:'X'):skFilOkDir)=0
078200151119     c***                eval      ILRY03DF21='S'
078300151119     c***                endif
078400151119     c
078500141028     c                   eval      ILRY03DF06='S'
078600141028     c                   eval      ILRY03EF12='S'
078700141028     c                   eval      ILRY03EF21='S'
078800141028     c                   eval      ILRY03EF06='S'
078900140813     c
079000141028     c                   eval      kpjbu=fnlry03ds2
079100140813     c                   call      'FNLRY03R'
079200140813     c                   parm                    kpjba
079300141028     c                   eval      fnlry03ds2=kpjbu
079400140813    2c                   if        olry03f12=*blanks or olry03err<>' '
079500140813     c                   eval      wf6='S'
079600140813    2c                   endif
079700140813    c
079800140813     c* chaino file variazioni
079900140813     c                   z-add     ilry03aas     k_aas
080000140813     c                   z-add     ilry03lnp     k_lnp
080100140813     c                   z-add     ilry03nrs     k_nrs
080200140813     c                   z-add     ilry03nsp     k_nsp
080300140909     c     kidc          setll     tiidc01l
080400140909     c     kidc          reade     tiidc01l
080500140909    2c                   dow       not %eof(tiidc01l)
080600140813     c*
080700140909    3c                   if        idcelaflg='E'
080800140813     c                             and idcprg>ilry03prg
080900140813     c                   leave
081000140813    3c                   endif
081100140909     c     kidc          reade     tiidc01l
081200140813    2c                   enddo
081300140813     c
081400140909    2c                   if        not %eof(tiidc01l)
081500140813     c                   movel     idctipodis    ilry03tipd
081600140813     c                   eval      ilry03prg=idcprg
081700140813     c                   else
081800140813     c                   clear                   ilry03tipd
081900140813    2c                   endif
082000140813     c
082100140813    1c                   enddo
082200070321     C                   ENDSR
082300011227     c**********************************************************************
082400000607     C     *INZSR        BEGSR
082500011227     c**********************************************************************
082600000607      *
082700000607     C     *ENTRY        PLIST
082800000607     C                   PARM                    Kpjba
082900140930     c                   movel     kpjbu         fnlry02ds
083000141014
083100141014     c                   movel     kpjbu         savkpjbu
083200070123      *
083300070315     c     *dtaara       define    §azute        azuteds
083400070315     c     *dtaara       define    §datiute      ddatiute
083500070315     c                   in(e)     *dtaara
083600070315     c                   if        %error  or rsut = *blanks
083700070315     c                   clear                   tibs34ds
083800070315     c                   call      'TIBS34R'
083900070315     c                   parm                    tibs34ds
084000070315     c                   in        *dtaara
084100070315     c                   endif
084200141014     c                   Clear                   wabi
084300141027     c                   Clear                   v1cmsg
084400141014     c                   Clear                   dLat
084500141014
084600141028     c                   setoff                                       19
084700141030      * Aggancio tabella VPO record DECOFI816DIR
084800151119     c****               clear                   trulvpods
084900151119     c****               eval      ivpoke1 = 'DECOFI816DIR'
085000151119     c****               call      'TRULVPOR'
085100151119     c****               parm                    trulvpods
085200151119     c****               eval      skfilokdir=skfilok
085300151119     c
085400141028     c* PGM RICHIAMATO: è richiamato se presente fgs
085500141028     c                   if        ilry02fgs<>*blanks
085600141028     c* controllo correttezza della filiale gestione --> deve contenere solo numeri
085700141028     c                   if        %check(digits:ilry02fgs)>0
085800141028     c                   Seton                                        28
085900141028     C                   movel     msg(1)        v1cmsg
086000141028     c                   endif
086100141028     c* flag dirottamento; valori possibili:
086200141028     c*"I"=Includi
086300141028     C*"E"=Escludi
086400141028     c*" "=Escludi
086500141028     c                   if        ilry02dir<>'I' and
086600141028     c                             ilry02dir<>'E' and
086700141028     C                             ilry02dir<>' '
086800141028     c                   Seton                                        28
086900141028     C                   movel     msg(2)        v1cmsg
087000141028     c                   endif
087100141028
087200141028     c                   if        *in28
087300141028     c                   goto      fine
087400141028     c                   endif
087500141028     c                   seton                                        19
087600141028     c                   else
087700141014     c* PROGRAMMA NON RICHIAMATO:
087800141014      * Verifica errori e autorità profilo
087900141014s   1c                   Select
088000141014      * se ho errori nei dati utente esco dal pgm
088100141014w   1c                   When      DutErr = 'E'
088200141014     c                   GoTo      Fine
088300141014      * se non c'è l'abilitazione
088400141014      * --> se 1° livello, abilitazioni al terminal
088500141014      *     se 2° livello, abilitazioni al punto operativo
088600141014      *     se sede è impossibile ma se capita mando a fine il pgm
088700141014w   1c                   When      UteAut = *Blanks
088800141014if  2c                   If        DutLpo = '1'
088900141014     c                   Eval      wabi   = 'TP'
089000141014e   2c                   EndIf
089100141014if  2c                   If        DutLpo = '2'
089200141014     c                   Eval      wabi   = 'PO'
089300141014e   2c                   EndIf
089400141014if  2c                   If        DutLpo = 'S'
089500141014     c                   GoTo      Fine
089600141014e   2c                   EndIf
089700141014      * carica le abilitazioni del profilo
089800141014x   1c                   Other
089900141014     c                   Movel     UteFaf        Dute01
090000141014     c                   If        §Utembf <> *Blanks
090100141014     c                   Eval      wabi = §Utembf
090200141014     c                   Else
090300141014     c                   Eval      wabi = UteAut
090400141014     c                   EndIf
090500141014e   1c                   EndSl
090600141014
090700141014      * controllo se ok l'abilitazione dell'utente
090800141014     c                   Clear                   Tibs02ds
090900141014     c                   Eval      T02Mod = 'C'
091000141014     c                   Eval      T02Sif = knsif
091100141014     c                   Eval      T02Cod = 'LAT'
091200141014     c                   Movel(p)  wabi          T02Ke1
091300141014     c                   Call      'TIBS02R'
091400141014     c                   Parm                    kpjba
091500141014     c                   Parm                    Tibs02ds
091600141014if  1c                   If        T02Err = *Blanks
091700141014     c                   Eval      dLat = T02Uni
091800141014e   1c                   EndIf
091900141014      * errore o non abilitato
092000141014if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
092100141014     c                   Seton                                        28
092200141015     c                   eval      noaut='1'
092300141027     C                   movel     msg(16)       v1cmsg
092400141014   x1c                   Else
092500141014      * Reperimento dei P.O. gestibili dall'utente
092600141014     c                   clear                   TRUL31DS
092700141014     c                   clear                   TRUL31DS2
092800141014     c                   eval      I31abi = wabi
092900141014     c                   eval      I31cdi = DUTdis
093000141014     c                   eval      I31car = DUTare
093100141014     c                   eval      I31cpo = DUTpou
093200141014     c                   call      'TRUL31R'
093300141014     c                   parm                    KPJBA
093400141014     c                   parm                    TRUL31DS
093500141014     c                   parm                    TRUL31DS2
093600141014if  2c                   if        O31pog > *zeros
093700141014     c                   movea     O31pog        $pog
093800141014x   2c                   else
093900141014     c                   Seton                                        28
094000141027     c                   movel     MSG(16)       v1cmsg
094100141015     c                   eval      noaut='1'
094200141014e   2c                   endif
094300141014    1c                   endif
094400141030    1c                   endif
094500141014     c
094600141014     c                   movel     savkpjbu      kpjbu
094700030702      *reperimento data
094800070319     C                   TIME                    W0140            14 0
094900070319     C                   MOVE      W0140         WDAT              8 0
095000030702     C*
095100030702     C                   Z-ADD     WDAT          G02DAT
095200030702     C                   MOVEL     *BLANK        G02ERR
095300030702     C                   CALL      'XSRDA8'
095400030702     C                   PARM                    WLBDA8
095500030702     C* UDATE A 8 IN AAAA/MM/GG
095600030717     C                   Z-ADD     G02INV        WUDAte            8 0
095700000607      *
095800070123      *
095900070124     C     Karb          klist
096000070327     C                   kfld                    K_aas
096100070327     C                   kfld                    K_lnp
096200070327     C                   kfld                    K_nrs
096300070327     C                   kfld                    K_nsp
096400140909     C     KIDC          klist
096500140909     C                   kfld                    K_aas
096600140909     C                   kfld                    K_lnp
096700140909     C                   kfld                    K_nrs
096800140909     C                   kfld                    K_nsp
096900140909     C                   kfld                    K_ELAdata
097000070125      *
097100070201      *
097200140909     C     KidcA         klist
097300140812     C                   kfld                    idcaas
097400140812     C                   kfld                    idclnp
097500140812     C                   kfld                    idcnrs
097600140812     C                   kfld                    idcnsp
097700070201      *
097800070201     C     Ktab          klist
097900070201     C                   kfld                    tblkut
098000070201     C                   kfld                    tblcod
098100070201     C                   kfld                    tblkey
098200070522     c*
098300070522     C     Karg1         klist
098400070522     C                   kfld                    arbaas
098500070522     C                   kfld                    arblnp
098600070522     C                   kfld                    arbnrs
098700070522     C                   kfld                    arbnsp
098800070412     c
098900070201     C                   z-add     1             tblkut
099000070205      * carico filiali £1
099100070205     C                   clear                   trul06ds
099200070205     C                   movel     '£1'          d06cod
099300070320     C                   movel     simfel        d06key
099400070205     C                   movel     trul06ds      kpjbu
099500070205     C                   CALL      'TRUL06R'
099600070205     C                   parm                    kpjba
099700070315     c                   movel     kpjbu         trul06ds
099800070315     C                   movea     lin           l1
099900070320     C* CARICO TABELLA FILIALI GESTITE DA ALTRE  SU TUTTO L'AS
100000070320     C                   CLEAR                   trul06ds
100100070320     C                   MOVE      '£6'          D06COD
100200070320     c                   eval      d06esc = 'G'
100300070320     c                   movel     simfel        d06key
100400070320     C                   MOVEL     trul06ds      KPJBU
100500070320     C                   CALL      'TRUL06R'
100600070320     C                   PARM                    KPJBA
100700070320     C                   MOVEL     KPJBU         trul06ds
100800070320     C                   MOVEl     LIN           nog
100900030714
101000011227     C                   ENDSR
101100070125      *--------------------------------------------------------------------------------------------*
101200070125      * ordina - riscrive il sfl ordinato per località
101300070125      *--------------------------------------------------------------------------------------------*
101400070125     C     ordina        BEGSR
101500070125     C                   setoff                                       72
101600070125      * IMPOSTO ENTRATA AD 'A'
101700070125     C                   EVAL      snrr1 = 0
101800070125      *
101900070125     C                   CLEAR                   QLGSCB
102000070125     C                   CLEAR                   QLGSCB00
102100070125
102200071203     ** 2 campi chiave.
102300071203     C                   EVAL      QLGNBRK    = 2
102400071204     c                   select
102500140812     c* ordinamento per inserimento
102600071204     c                   when      w_ord=1
102700140812     ** inseriment dopo rag. sociale destinatario 2 byte,
102800140812     C                   EVAL      QLGSP      = 1 +%SIZE(v1srsd)+
102900140812     c                                          %SIZE(v1sprd)
103000140812     C                   EVAL      QLGSS      = %SIZE(v1hinsdata)
103100070125     C                   EVAL      QLGDT      = carattere
103200070125     C                   EVAL      QLGSO      = Ascendente
103300070125     C                   EVAL      QLGKL(1)   = QLGSKL
103400140812     ** ragione sociale
103500140812     C                   EVAL      QLGSP      = 1
103600140812     C                   EVAL      QLGSS      = %SIZE(v1srsd)
103700140812     C                   EVAL      QLGDT      = carattere
103800140812     C                   EVAL      QLGSO      = Ascendente
103900140812     C                   EVAL      QLGKL(2)   = QLGSKL
104000071204     c* ordinamento per rag.sociale/prv destinatario
104100071204     c                   when      w_ord=2
104200071204     C                   EVAL      QLGSP      = 1
104300071204     C                   EVAL      QLGSS      = %SIZE(v1srsd)
104400071204     C                   EVAL      QLGDT      = carattere
104500071204     C                   EVAL      QLGSO      = Ascendente
104600071204     C                   EVAL      QLGKL(1)   = QLGSKL
104700071204     ** ragione sociale a posizione 1, carattere, 24 byte,
104800071204     C                   EVAL      QLGSP      = 1 + %size(v1srsd)
104900071204     C                   EVAL      QLGSS      = %SIZE(v1sprd)
105000071204     C                   EVAL      QLGDT      = carattere
105100071204     C                   EVAL      QLGSO      = Ascendente
105200071204     C                   EVAL      QLGKL(2)   = QLGSKL
105300071204     c                   endsl
105400070125
105500070125      * Load other sort parameters.
105600070125     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
105700070125     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
105800070125     C                   EVAL      QLGRT     = 8
105900070125     C                   EVAL      QLGOKL    = 80
106000070125     C                   EVAL      QLGLKE    = 16
106100070125     C                   EVAL      QLGLSS    = 290
106200070125
106300070125      * Initialize Sort I/O API fields.
106400070125     C                   EVAL      QLGRL00  = QLGRL
106500070125     C                   EVAL      QLGRC00  = 1
106600070125     C                   CLEAR                   QUSEI
106700070125     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
106800070125
106900070125      * First step - Initialize the sort routine.
107000070125     C                   CALL      'QLGSORT'
107100070125     C                   PARM                    QLGSCB
107200070125     C                   PARM                    NotUsed
107300070125     C                   PARM                    NotUsed
107400070125     C                   PARM                    SizeList
107500070125     C                   PARM                    ReturnSize
107600070125     C                   PARM                    QUSEC
107700070125
107800070125      * Next step - Write records to I/O routine.
107900070125     C                   EVAL      QLGRT00 = Put
108000070125
108100070125     C                   DO        *hival        cx                4 0
108200070125
108300140811     C     cx            CHAIN     LRY02s01                           50
108400070125     c   50              leave
108500070125     ** Solo le righe con Selected = 'Y' sono riordinate,
108600070125     ** quindi per fare un ordinamento di tutte le righe
108700070125     ** metto 'Y' sempre.
108800070125     C                   EVAL      Selected = 'Y'
108900070125
109000070125     C                   CLEAR                   QUSEI
109100070125     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
109200070125     C                   CALL      'QLGSRTIO'
109300070125     C                   PARM                    QLGSCB00
109400070125     C                   PARM                    SflRcd
109500070125     C                   PARM                    NotUsed
109600070125     C                   PARM                    SizeList
109700070125     C                   PARM                    NotUsed
109800070125     C                   PARM                    QUSEC
109900070125
110000070125     C                   ENDDO
110100070125
110200070125      * Next step - Signal end of input, clear subfile for reload.
110300070125     C                   EVAL      QLGRT00 = EndPut
110400070125     C                   CLEAR                   QUSEI
110500070125     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
110600070125
110700070125     C                   CALL      'QLGSRTIO'
110800070125     C                   PARM                    QLGSCB00
110900070125     C                   PARM                    SflRcd
111000070125     C                   PARM                    NotUsed
111100070125     C                   PARM                    SizeList
111200070125     C                   PARM                    NotUsed
111300070125     C                   PARM                    QUSEC
111400070125
111500070316     C                   SETOff                                       21        *
111600140811     C                   WRITE     LRY02c01                                      *
111700070316     C                   SETOn                                        21        *
111800140811     C                   WRITE     LRY02z01                                      *
111900070125
112000070125      * Final step - Write the records back to the subfile.
112100070125     C                   EVAL      QLGRT00 = Get
112200070316     C                   DO        lasnrr1       x                 4 0
112300070125     C                   CLEAR                   QUSEI
112400070125     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
112500070125     C                   CALL      'QLGSRTIO'
112600070125     C                   PARM                    QLGSCB00
112700070125     C                   PARM                    NotUsed
112800070125     C                   PARM                    SflRcd
112900070125     C                   PARM                    QLGRL00
113000070125     C                   PARM                    NotUsed
113100070125     C                   PARM                    QUSEC
113200070125      *
113300070125     c                   z-add     x             snrr1
113400140811     C                   WRITE     LRY02s01
113500070125     C                   ENDDO
113600070125
113700070125      *
113800070125     C                   ENDSR
113900141030      *--------------------------------------------------------------------------------------------*
114000141030      * imposta a video il flag "Solo dirottamenti"
114100141030      *--------------------------------------------------------------------------------------------*
114200141030     C     sr_impodir    BEGSR
114300141030     c* PGM RICHIAMATO
114400141030     c* se fgs     partita con nuova procedura dir=' ' significa "Escludi"
114500141030     c* se fgs non partita con nuova procedura dir=' ' significa "Tutti"
114600141030     c                   if        ilry02fgs<>*blanks
114700141030     c                   eval      v1cdir=ilry02dir
114800141030     c                   if        ilry02dir=*blanks
114900151119     c***                if         %lookup('999':skFilOkdir)>0 or
115000151119     c***                           %lookup(%editc(v1cfgs:'X'):skFilOkdir)>0
115100151119     c*** SEMPRE "E"
115200141030     c                   movel     'E'           v1cdir
115300151119     c
115400151119     c***                endif
115500141030     c                   endif
115600141030     c                   else
115700141030     C* PGM NON RICHIAMATO
115800141030     c* default inclusione/esclusione dirottamenti
115900141030     c                   clear                   v1cdir
116000141030     c* se fgs     partita con nuova procedura dir=' ' significa "Escludi"
116100141030     c* se fgs non partita con nuova procedura dir=' ' significa "Tutti"
116200151119     c***                if         %lookup('999':skFilOkdir)>0 or
116300151119     c***                           %lookup(%editc(v1cfgs:'X'):skFilOkdir)>0
116400151119
116500151119     c*** SEMPRE "E"
116600141030     c                   movel     'E'           v1cdir
116700151119
116800151119     c****               endif
116900141030     c                   endif
117000141030     c                   endsr
117100070330**
117200070319Filiale in gestione errata
117300151116Richiesta errata. Valori possibilili: "I","E"," "
117400070402Selezionare per Numero Spedizione OPPURE altre parzializzazioni
117500070402Spedizione inesistente
117600070316Parzializzazione per ragione sociale errata: inserire correttamente
117700140527Tipo elaborazione obbligatorio
117800140527
117900140527La spedizione non prevede disposizioni da caricare in base al tipo immesso      8
118000140527
118100140527
118200140527
118300140527Nessuna disposizione da elaborare per la selezione fatta                       12
118400140527Spedizione non gestibile: bloccata con blocco " "                              13
118500140812Spedizione non gestibile: consegnata                                           14
118600070402Spedizione non gestibile: linea di arrivo non in gestione                      15
118700141015Utente non abilitato alla Gestione Disposizioni di Consegna                    16
