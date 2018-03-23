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
002401141014     d dLat          e ds
002402141014     d dute01        e ds
002403141014     D tibs02ds      E DS
002404141014     D TRUL31DS      E DS
002405141014     D TRUL31DS2     E DS
002500070315     d tibs34ds      e ds                  inz
002600070131     D trul06ds      E DS
002700070131     D  lin                    1     90  0 dim(30)
002701141030     d TRULVPODS     e ds
002702141030     D  skFilOk               16    765    dim(250)
002703141030
002704141030     D skFilOkdir      s                   like(skFilOk) dim(250) inz
002800070206     D* DATI X RICHIAMO PGM INTERR. BOLLE ARRIVI
002900070206     D PARAM1          DS                  INZ
003000070206     D  PA1AAS                14     17  0
003100070206     D  PA1LNP                18     20  0
003200070206     D  PA1NRS                21     22  0
003300070206     D  PA1NSP                23     29  0
003400070206     D  PA1F03                30     30
003500070206     D  PA1FLG                31     31
003600070206     D* SE =1 NON RICH. FNLG30R IN MODO RICORSIVO DA INTERR.BOLLE
003700070206     D  PA1GIA               144    144
003800070330     d
003900070330     D                 DS
004000070330     D  ARBAAS                 1      4  0
004100070330     D  ARBMGS                 5      8  0
004200070330     D  ARBDSP                 1      8  0
004300070330     d
004400070201     d ds3a          e ds
004500011228      *
004600140813     D savprg          S                   like(idcprg)
004700140813     D wtit2           S                   like(v1ctit2)
004800070327     d K_aas           s                   like(arbaas)
004900070327     d K_lnp           s                   like(arblnp)
005000070327     d K_nrs           s                   like(arbnrs)
005100070327     d K_nsp           s                   like(arbnsp)
005200140909     d K_ELADATA       s                   like(idceladata) inz
005300070430     D wprima_v        S              1
005400140813     D savpiu          S              1
005500030604     D snrr1           S              5i 0
005600070129     D lasnrr1         S              5i 0
005700030613     D WLBDA8          DS
005800030613     D  G02DAT                 1      8  0
005900030613     D  G02INV                 9     16  0
006000030613     D  G02ERR                17     17
006100030613     D  G02TGI                18     22  0
006200070319
006300070315     d  l6             s              3  0 dim(30)
006400070315     d  l1             s              3  0 dim(30)
006500070320     D nog             S              3  0 DIM(30)                              FIL.GESTITE
006600070330     d Dataiso         s               d   datfmt(*iso)
006700070406     d Datadmy         s               d   datfmt(*dmy)
006800140813     D wpos            S              1s 0
006900070504     D xx              S              1  0
006901141014     D savkpjbu        S                   LIKE(kpjbu)
006902141014     d wabi            s                   like(UteAut)
006903141014     D $pog            s              3    dim(250) inz(*zeros)
006904141014     D w003a           s              3
006905141015     D noaut           s              1    inz
006906141027     D s_v1cdir        s                   like(v1cdir)
007000070316      *---------------------------------------------------------------------
007100140930     D cosapp          C                   CONST('         Consegna Ri-
007200140919     D                                     chiesta       ')
007300070321     D cosffd          C                   CONST('         Fermo Depos-
007400070321     D                                     ito           ')
007500140919     D cosind          C                   CONST('       Indirizzo A-
007600140919     D                                     lternativo    ')
007700140919     D cosaltro        C                   CONST('         Altre Indic-
007800140919     D                                     azioni        ')
007900140527     D msg             S             78    DIM(16) CTDATA PERRCD(1)
008000071128     d weditora        c                   '  :  '
008100071128     d weditdta        c                   '  /  '
008200071204     d ord_ragprv      c                   'F11=Ord.xRagSoc/Prv'
008300140812     d ord_prvrag      c                   'F11=Ord.xDataInser.'
008400071204     d w_prvrsoc       C                   1
008500071204     d w_rsocprv       C                   2
008600071204     d W_ord           S              1  0
008700070125      *---------------------------------------------------------------------
008800070125      * specifiche per ordinamento forzato sfl
008900070125     d NotUsed         S             16A
009000070125     d ReturnSize      S              9B 0
009100070125     d SizeList        S              9B 0
009200070125     d*---
009300070125     d MaxKey          C                   2
009400070125     d Ascendente      C                   1
009500070125     d Discendente     C                   2
009600070125     d Carattere       C                   6
009700070125     d Put             C                   1
009800070125     d EndPut          C                   2
009900070125     d Get             C                   3
010000070125     d Numerico        C                   8
010001141028     D digits          c                   '0123456789'
010100070125     d*---
010200070125     d SflRcd          DS
010300070316     d  v1srsd
010400071203     d  v1sprd
010500140812     d  V1hinsdata
010600070316     d  V1sAAS
010700080908     d  V1hLNP
010800080908     d  V1hNRS
010900080908     d  V1hNSP
011000140919     d  V1sinsd
011100140919     d  V1sinso
011200070330     d  V1sdsp
011300070504     d  V1sabi
011400071031     d  V1sast
011500070402     d  V1snt1
011600140813     d  V1hprg
011700140813     d  V1hblk
011800070125     d  Selected                      1A
011900070125
012000070125      /COPY QSYSINC/QRPGLESRC,QLGSORT
012100070125     d QLGKL                         16    DIM(MaxKey)
012200070125     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
012300070125     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
012400070125     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
012500070125     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
012600070125
012700070125      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
012800070125      /COPY QSYSINC/QRPGLESRC,QUSEC
012801141027
013000070502     c                   exsr      inzfgs
013001141030     c                   exsr      sr_impodir
013200000608      *
022300141028     c* Gestione subfile
022500070316     C                   EXSR      ges_s01
023300070402     c
024300140930    c
024500070402     c
025600011227      *
025701141014
025702141014     c     fine          tag
025703141028
025704141028    4c                   if        ilry02fgs<>*blanks and *in28
025705141028     c                   eval      OLRY02MSG=v1cmsg
025706141028     c                   eval      OLRY02err='1'
025707141028     c                   endif
025800070321      * Chiusura con lr
025900140813     c                   eval      ilry03tla='C'
025901141028     c                   eval      kpjbu=fnlry03ds2
026000140813     c                   call      'FNLRY03R'
026100140813     c                   parm                    kpjba
026300140930     c
026400140930     c                   movel(p)  fnlry02ds     kpjbu
026500140813     c*
026600070315     C                   seton                                        lr
028400070502     c**********************************************************************
028500070502     c* inizializza p.o. in gestione
028600070502     c**********************************************************************
028700070502     c     inzfgs        begsr
029000140930     c
029100140930     c* se passata la filiale gestione la imposto, altrimenti prendo lo stesso dutpou
029200141028     c                   if        ILRY02fgs<>*blanks
029300140930     c                   eval      *in16=*off
029400140930     c                   movel     ILRY02fgs     v1cfgs
029500140930     c                   else
029600140930     c                   eval      v1cfgs = dutpou
029700140930     c                   endif
029800140930     c
030200140930     c
030300070502     c     v1cfgs        chain     azorg01L
030400070502     c                   if        %found(azorg01l) and orgfva = *blanks
030500070502     c                   eval      v1dfgs = orgdes
030600070502     c                   endif
030700070502     c*
030800140930      * disabilito f18 se p.o. di secondo livello o richiamato
030900141014     c*****              eval      *in17 = (dutlpo = '2' or dutlpo = *blanks)
030901141014     c                   if        $pog(2)=*zeros
030902141014     c                   eval      *in17=*on
030903141014     c                   endif
031000070502     c                   eval      *in16=*off
031100141014     c                   exsr      sr_car£6
031200070502     c                   endsr
031300070316     c**********************************************************************
031400070316     c* Caricamento £6 della filiale in gestione
031500070316     c**********************************************************************
031600070316     C     SR_car£6      BEGSR
031700070316      * carico filiali £6
031800070316     C                   clear                   trul06ds
031900070316     C                   movel     '£6'          d06cod
032000070316     C                   movel     v1cfgs        d06key
032100070316     C                   movel     trul06ds      kpjbu
032200070316     C                   CALL      'TRUL06R'
032300070316     C                   parm                    kpjba
032400070316     C                   movel     kpjbu         trul06ds
032500070316     C                   movea     lin           l6
032600070316     c                   endsr
032700070315     c**********************************************************************
032800070315     c* Controllo filiale in gestione
032900070315     c**********************************************************************
033000070315     C     Ctrfgs        BEGSR
033100070315     c*
033200070315     c                   setoff                                       40
033300070315     c                   clear                   v1dfgs
033400070320      * Il p.o. gestione non può essere un p.o. gestito
033500070320     c     V1cFgs        Lookup    Nog                                    30
033600070320     c                   If        *In30
033700070320     c                   Eval      *In40 = *On
033800070320     c                   Eval      *In28 = *On
033900141027     c                   Eval      V1cMsg = Msg(1)
034000070320     c                   leavesr
034100070320     c                   EndIf
034101141014     c                   move      v1cfgs        w003a
034200141014     c     w003a         lookup    $pog                                   30
034300070315     c                   if        not *in30
034400070315     c                   seton                                        2840
034500141027     c                   eval      v1cmsg = msg(01)
034600070315     c                   leavesr
034700070315     c                   endif
034800070315
034900070315      * decodifica p.o. in gestione
035000070315     c     v1cfgs        chain     azorg01L
035100070315     c                   if        %found(azorg01l) and orgfva = *blanks
035200070315     c                   eval      v1dfgs = orgdes
035300141027     c                   endif
035400070315     c                   setoff                                       16
035500070417      *
035600070316     c                   exsr      sr_car£6
035601141030     c* Imposto il flag "Solo dirottamenti" in base alla nuova filiale gestione
035602141030     c                   exsr      sr_impodir
035700070417      *
035800070315     c                   endsr
044400140814     c**********************************************************************
044500141008     c* controllo il tipo disposizione in base alle richieste
044600140814     c**********************************************************************
044700140814     c     CtrDispo      BEGSR
044800140814     c                   setoff                                       90
044900140814     c                   exsr      ctrspecau
045000140814     c                   exsr      cerAbi
045100140814     c
045200140814    2c                   if        *in08
045300140814     c                   seton                                        90
045400140814   x2c                   else
045500140814    3c                   if        (wblk=*blanks and v1ccau=' ') or
045600140814     c                             (%subst(wblk:1:1)<>'E' and v1ccau='1') or
045700140814     c                             (%subst(wblk:2:1)<>'E' and v1ccau='2') or
045800140814     c                             (%subst(wblk:3:1)<>'E' and v1ccau='3') or
045900140814     c                             (%subst(wblk:4:1)<>'E' and v1ccau='4')
046000140814     c                   seton                                        90
046100140814    3c                   endif
046200140814    2c                   endif
046300140814    2c                   ENDSR
046400070316     c**********************************************************************
046500140526     c* Controllo congruenza fra causale disposiz e spedizione
046600070316     c**********************************************************************
046700070316     C     Ctrspecau     BEGSR
046800140526     c                   clear                   wblk              4
046900070504     c                   setoff                                       08
047000070504     c
047100070402     c                   move      '3A'          tblcod
047200070402     c                   movel(p)  arbcbo        tblkey
047300070402     c                   exsr      ctabel
047400070402     c                   movel     tbluni        ds3a
047500070402     c
047600140527     c* Escludo bolle di recupero
047700140526    1c                   if        §3arbl='R'
047800070504     c                   seton                                        08
047900140526    1c                   endif
048000070402     c
048100140813     c                   clear                   savprg
048200140813     c                   clear                   savpiu
048300140523     c* chaino file variazioni
048400140909     c     kidc          setll     tiidc01l
048500140909     c     kidc          reade     tiidc01l
048600140909    1c                   dow       not %eof(tiidc01l)
048700070402     c*
048800140909     c                   if        idcelaflg='E'
048900140813     c
049000140813     c* mi salvo ilprimo progressivo che trovo
049100140813     c                   if        savprg=0
049200140813     c                   eval      savprg=idcprg
049300140813     c                   else
049400140813     c                   eval      savpiu ='S'
049500140813     c                   endif
049600140813     c
049700140814    2c**                 select
049800140814     c**                 when      v1ccau='1'  and idctipodis='1'
049900140814     c**                 eval      %subst(wblk:1:1)=idcelaflg
050000140814     c**                 when      v1ccau='2'  and idctipodis='2'
050100140814     c**                 eval      %subst(wblk:2:1)=idcelaflg
050200140814     c**                 when      v1ccau='3'  and idctipodis='3'
050300140814     c**                 eval      %subst(wblk:3:1)=idcelaflg
050400140814     c**                 when      v1ccau='4'  and idctipodis='4'
050500140814     c**                 eval      %subst(wblk:4:1)=idcelaflg
050600070504     c
050700070503     c* se non impostato metto se ha almeno una abilitazione
050800140814     c**                 when      v1ccau=' '
050900140526     c                   if        idctipodis='1'
051000140527     c                   eval      %subst(wblk:1:1)=idcelaflg
051100140526     c                   endif
051200140526     c                   if        idctipodis='2'
051300140527     c                   eval      %subst(wblk:2:1)=idcelaflg
051400140526     c                   endif
051500140526     c                   if        idctipodis='3'
051600140527     c                   eval      %subst(wblk:3:1)=idcelaflg
051700140526     c                   endif
051800140526     c                   if        idctipodis='4'
051900140527     c                   eval      %subst(wblk:4:1)=idcelaflg
052000140526     c                   endif
052100070504     c
052200140814    2c**                 endsl
052300140812     c                   endif
052400140526     c
052500140909     c     kidc          reade     tiidc01l
052600140812    1c                   enddo
052700070402     c
052800070316     c                   endsr
052900070316     C*--- CHAIN TABEL00F --------------------------------------------*
053000070316     C     CTABEL        BEGSR
053100070316     C     KTAB          CHAIN     TABEL                              30
053200070316     c                   if        %found(tabel00f) and tblflg<> ' '
053300070316     C                   SETON                                        30
053400070316     C                   END
053500070316     C                   ENDSR
053600011227     c**********************************************************************
053700140814     C* gestione sfl
053800011227     c**********************************************************************
053900070316     C     ges_s01       BEGSR
054000000627      *
054002141027
054100070402     c                   eval      *inke='1'
054200070402     c* looppo per F5 - rivisulizza
054300070402     c                   dow       *inke='1'
054400070402     c
054500070402     c                   eval      *inke='0'
054501141028     c                   eval      w_ord=w_prvrsoc
054600070316     C                   EXSR      ries01
056000070316      * Imposto sottotitolo seconda videata
056100070316     c                   eval      v1ctit2=wtit2
057000070330     c
057100071204     c                   eval      v1tf11=ord_ragprv
057200141008     c                   seton                                        11
057300070316     c*
057400070316     C                   DO        *hival
057500140811     C                   WRITE     LRY02T01
057600140811     C                   WRITE     LRY02z01
057601141028     c                   if        *in20=*off
057602141028     c                   write     lry02n01
057603141028     c                   endif
057700140811     C                   EXFMT     LRY02c01
057701141027     c                   setoff                                       28
057702141027     c                   if        noaut='1'
057703141027     c                   leavesr
057704141027     c                   endif
057800011227     C                   SELECT
057801141027      * F18=cambio filiale - (Sproteggo cambio p.o gestione)
057802141027     c                   when      *inks
057803141027     c                   seton                                        16
057804141027     c                   iter
057806141027     c
057900070402     c* esco per F3-Fine  F12-Ritorno  F5-rivisualizza
058000070402     C                   WHEN      *INKL=*ON or *inkc  or *inke
058100070316     C                   leave
058200071204     c* f11=Cambio ordinamento sfl
058300071204     C                   WHEN      *INkk
058400071204     c                   select
058500071204     c                   when      w_ord=w_prvrsoc
058600071204     c                   eval      w_ord=w_rsocprv
058700071204     c                   eval      v1tf11=ord_prvrag
058800071204     c                   when      w_ord=w_rsocprv
058900071204     c                   eval      w_ord=w_prvrsoc
059000071204     c                   eval      v1tf11=ord_ragprv
059100071204     c                   endsl
059101141112     c* Solo se presenti record nel sfl
059102141112     c                   if        *in20
059200071204     C                   exsr      ordina
059201141112     c                   endif
059300071204     c                   iter
059400141027     c* Cambio fil. gestione
059401141027     c                   when      *in16
059403141027     c                   exsr      ctrfgs
059404141027     c                   if        not *in28
059405141027     c                   exsr      ries01
059406141027     c                   endif
059409141027     c                   iter
059410141027     c* cambio parzializzazione
059411141027     c                   when      v1cdir<>s_v1cdir
059412141027     c                   exsr      ries01
059413141027     c                   iter
059414141027
059500011227     C                   OTHER
059600011227
059601141027      * controllo filiale in gestione
059700141112     C   20              EXSR      GestioneSFL
059800011227     C                   ENDSL
059900011228
060000011227     C                   ENDDO
060100070402     c
060200070402     C                   ENDDO
060300070402     C                   ENDSR
060400070125     c**********************************************************************
060500070316     c* carica sfl
060600070125     c**********************************************************************
060700070316     C     Ries01        BEGSR
060800070330     c* Pulizia SFL
060900070327     c                   setoff                                       21
061000140811     C                   WRITE     LRY02c01
061100070316     c                   seton                                        21
061200070125     C                   EVAL      snrr1   = 0
061300070316     C                   EVAL      nrr1    = 0
061301141027     c                   eval      s_v1cdir=v1cdir
061400070330     c
061500070131      * gestione eventuali filiali in £6
061600070316    1c                   do        *hival        £6                3 0
061700070316    2c                   if        l6(£6)  = 0
061800070131     c                   leave
061900070316    2c                   endif
062000140526     c* Imposto il titolo
062100140526     c                   select
062200140526     c                   when      v1ccau='1'
062300140526     c                   eval      wtit2=Cosapp
062400140526     c                   when      v1ccau='2'
062500140526     c                   eval      wtit2=Cosffd
062600140526     c                   when      v1ccau='3'
062700140526     c                   eval      wtit2=Cosind
062800140526     c                   when      v1ccau='4'
062900140526     c                   eval      wtit2=Cosaltro
063000140526     c                   endsl
063100140813     c
063200140813     c                   clear                   arbaas
063300140813     c                   clear                   arblnp
063400140813     c                   clear                   arbnrs
063500140813     c                   clear                   arbnsp
063600070330     c
063700140812     c     l6(£6)        setll     tiidc05l
063800070316    2c                   do        *hival
063900140812     c     l6(£6)        reade     tiidc05l
064000070316      *
064100140812    3c                   if        %eof(tiidc05l)
064200070316     c                   leave
064300070316    3c                   endif
064400140813    c
064500140813     c* Se è la  stessa bolla che ho appena letto --> salto
064600140813     c                   if        idcaas=arbaas and idclnp=arblnp and
064700140814     c                             idcnrs=arbnrs and idcnsp=arbnsp
064800140813     c                   iter
064900140813     c                   endif
065000070131
065100070131      * filtri
065200140909     c     kidcA         chain     fnarb01l
065300140812   2ac                   if        %found(fnarb01l)
065400070131
065500070131      * bolle da escludere sempre
065600080418      * le spedizioni consegnate non le escludo perchè devo elaborare i
065700080418      *  ritorni all'incasso non abilitati dei C/ASS
065800140909    3c**                 if        arbfbc = '*' or arbfbc = 'B' or
065900140909     c**                           arbfbc = 'G'
066000140909     c**                 iter
066100140909    3c**                 endif
066200070330     c
067100070330     c
067200070330      * parzializzazione per RSD richiesto range a video
067300070330    3c                   if        v1crsd <> *blank and v1crsa <> *blank
067400070330     c                             and (arbrsd < v1crsd or arbrsd > v1crsa)
067500070330     c                   iter
067600070330    3c                   endif
067700070330     c
067800070330     c
071200070330    3c                   endif
071201141027     c* Inclusione / esclusione dirottamenti
071202141028     c                   select
071203141028     c                   when      v1cdir='E' and idcnewlna>0
071204141027     c                   iter
071205141028     c                   when      v1cdir='I' and idcnewlna=0
071206141028     c                   iter
071207141030     c                   when      v1cdir=' '
071208141030     c* nuova procedura: " " = Escludi
071209141030     c                   if         (%lookup('999':skFilOkdir)>0 or
071210141030     c                              %lookup(%editc(v1cfgs:'X'):skFilOkdir)>0)
071211141030     c                              and idcnewlna>0
071212141030     c                   iter
071213141030     c                   endif
071214141028     c                   endsl
071300070330    c
071400070330     c* ********************
071500070330     c* Carico record in SFL
071600070330     c* ********************
071700070316     C                   move      *blank        V1sopz
071800070316     c                   movel     arbrsd        v1srsd
071900071203     c                   movel     arbprd        v1sprd
072000070316     c                   z-add     arbaas        v1saas
072100080908     c                   z-add     arblnp        v1hlnp
072200080908     c                   z-add     arbnrs        v1hnrs
072300080908     c                   z-add     arbnsp        v1hnsp
072400070330     c* Data spedizione
072500070330     c     *iso          movel     arbdsp        dataiso
072600070406     c                   movel     dataiso       datadmy
072700140812     c                   movel     datadmy       w1sdsp            8 0
072800140812     c                   movel     w1sdsp        v1sdsp
072900070330     c* Telefono destinatario
073000140919     c**                 if        idcteld<>*blanks
073100140919     c**                 Eval      V1sTeld = idcteld
073200140919     c**                 else
073300140919     c**                 Eval      V1sTeld = idctelavv
073400140919     c**                 endif
073500140813     c                   movel     idcprg        v1hprg
073600140813     c* data e ora richiesta
073700140813     c                   movel     idcinsdata    v1hinsdata
073800140812     c                   movel     idcinsdata    w0080             8 0
073900140527     c                   movel     3             g02err
074000140527     C                   Z-ADD     w0080         G02inv
074100140527     C                   MOVEL     *zeros        G02dat
074200140527     C                   CALL      'XSRDA8'
074300140527     C                   PARM                    WLBDA8
074400140919     c                   eval      v1sinsd=g02dat
074500140919     c                   eval      v1sinso=%int(%subst(%editc
074600140919     c                             (idcinsdata:'X'):9:4))
074700140919     c
074800140919     c**                 eval      v1sddt=%subst(%editc(g02dat:'X'):1:4)+
074900140919     c**                           %subst(%editc(g02dat:'X'):7:2)+' ' +
075000140919     c**                           %subst(%editc(idcinsdata:'X'):9:6)
075100140527     c
075200140527     c                   eval      v1sast=IDCTIPOcli
075300070330     c
075400140527     c                   clear                   v1snt1
075500140527     c* Per tipo "4" metto le disposiz nelle note
075600140527     c                   if        idctipodis='4'
075700140527     c                   eval      v1snt1=idcnote
075800140527     c                   endif
075900140814     c* Per tipo "2" riscrivo "Fermo Deposito"
076000140814     c                   if        idctipodis='2' and v1ccau<>' '
076100140814     c                   eval      v1snt1='Fermo Deposito'
076200140814     c                   endif
076300140814     c
076400140527     c* Per tipo "1" metto ConsRichiesta
076500140527     c                   if        idctipodis='1'
076600140527     c
076700140527     c                   movel     idcdcr        w0080             8 0
076800140527     c                   movel     3             g02err
076900140527     C                   Z-ADD     w0080         G02inv
077000140527     C                   MOVEL     *zeros        G02dat
077100140527     C                   CALL      'XSRDA8'
077200140527     C                   PARM                    WLBDA8
077300140527     c                   eval      v1snt1='IL '+%editw(g02dat:'  /  /    ')
077400140527     c                   if        idchcr>0
077500140527     c                   eval      v1snt1=%trim(v1snt1)+' '+
077600140527     c                                    %editw(idchcr: '  :  ')
077700140527     c                   endif
077800140527     c
077900140527     C                   IF        idcprcr='M'
078000140812     c                   eval      v1snt1=%trim(v1snt1) + '  '+
078100141008     c                                    'preferibilmente il MATTINO   '
078200140812     c                   endif
078300140812     C                   IF        idcprcr='P'
078400140812     c                   eval      v1snt1=%trim(v1snt1) + '  '+
078500141008     c                                    'preferibilmente il POMERIGGIO'
078600140527     c                   endif
078700140527     c                   endif
078800140812     c
078900140814     c* Per tipo "3" metto l'indirizzo
079000140527     c                   if        idctipodis='3'
079100141003     c* se dirottamento lo scrivo
079200141003     c                   if        idcfla='S'
079300141003     c                   eval      v1snt1= 'Dirottamento a '+' '+idclod
079400141007     c                   eval      v1snt1=%trim(v1snt1)+ '('+idcprd+')' +
079500141007     c                             ' filiale : '+%editc(idcnewlna:'X')
079600141003     c                   else
079700140812     c                   eval      v1snt1= idcIND+ ' ' +idclod
079800140812     c                   if        %subst(v1snt1:74:4)<>*blanks
079900141003     c                   eval      %subst(v1snt1:74:4)='('+idcprd+')'
080000140812     c                   else
080100141003     c                   eval      v1snt1=%trim(v1snt1)+ '('+idcprd+')'
080200140527     c                   endif
080300140812     c                   endif
080400141003     c                   endif
080500140527     c
080600140813      *
080700140813     c                   z-add     arbaas        k_aas
080800140813     c                   z-add     arblnp        k_lnp
080900140813     c                   z-add     arbnrs        k_nrs
081000140813     c                   z-add     arbnsp        k_nsp
081100140814     c                   exsr      CtrDispo
081200140814     c                   if        *in90
081300140814     c                   iter
081400140814     c                   endif
081500140813     c
081600070125      *esco per raggiunta massima capacità sfl
081700070316    3c                   if        snrr1 > 9990
081800070125     c                   leave
081900070316    3c                   endif
082000070125
082100070125     C                   EVAL      snrr1 = snrr1 + 1
082200140811     C                   WRITE     LRY02s01
082300070316    2C                   ENDDO
082400070316    1C                   enddo
082500070125
082600070316    1C                   IF        snrr1  > 0
082700070316     C                   EVAL      *IN20 = *ON
082800070125     C                   MOVE      1             OK                1 0
082900070125     C                   ELSE
082901141028     C                   EVAL      *IN20 = *Off
083000070125     C                   MOVE      0             OK                1 0
083100070316    1C                   ENDIF
083200070125     C                   EVAL      nrr1  = 1
083300070316     C                   EVAL      lasnrr1  = snrr1
083400070316     C                   EVAL      snrr1  = 1
083500070125     C                   EVAL      csrrrn = 1
083600070402     c
083601141028     c* ordino il subfile per provincia/ragione sociale dst.solo se caricati
083602141028     c* record
083603141028     c                   if        ok>0
083605141028     C                   EXSR      ordina
083606141028     c                   else
083607141028     c
083608141028     c* errore se non caricati record
083612141028     c                   if        ilry02fgs<>*blanks
083613141028     c                   seton                                        28
083614141028     c                   movel     msg(12)       v1cmsg
083615141028     c                   goto      fine
083616141028     c                   endif
083617141028     c                   endif
083700070125     C                   ENDSR
083800070504     C**************************************************************************
083900070507     c* Visuliazzo le abilitazioni della bolla
084000070507     c*  escludo quella richeista in prima videata, se richeista
084100070504     C**************************************************************************
084200070504     c     CerABI        BEGSR
084300140814     c                   clear                   wpos
084400140814     c                   clear                   v1sabi
084500140813     c                   clear                   v1hblk
084600070504     c                   z-add     1             x
084700070504     c                   clear                   sk8
084800070507     c*
084900070507    1c                   if        v1ccau=' '
085000140812     c                   eval      wpos=%scan('E':wblk)
085100140813    2c                   if        wpos>0
085200070504     c* Im wpos è contenuta la posizione della 1 abilitazione che
085300070504     c*  corrisponde al tipo spedizioni richiesto a video
085400140813     c*  poi di seguito visualizzo tutte le altre
085500070504     c                   if        wpos=1
085600140919     c                   movea     'ConsRic'     sk8(x)
085700070504     c                   endif
085800070504     c                   if        wpos=2
085900140919     c                   movea     'FerDep.'     sk8(x)
086000070504     c                   endif
086100070504     c                   if        wpos=3
086200140919     c                   movea     'IndirAl'     sk8(x)
086300070504     c                   endif
086400070504     c                   if        wpos=4
086500140919     c                   movea     'AltreIn'     sk8(x)
086600070504     c                   endif
086700070507     c
086800140813     c
086900140813    2c                   endif
087000140812    1c                   endif
087100070504     c                   clear                   wpos
087200070504     c
087300140527     c* Appuntamento
087400140813     c                   if        v1ccau<>' '
087500140813     c                   if        %subst(wblk:1:1)<>' 'and v1ccau<>'1'
087600141027     c                   movea     'consRic'     sk8(x)
087700140813     c                   endif
087800140527     c* Fermo Deposito
087900140813     c                   if        %subst(wblk:2:1)<>' 'and v1ccau<>'2'
088000140919     c                   movea     'FerDep.'     sk8(x)
088100140813     c                   endif
088200140527     c* cambio Indirizzo
088300140813     c                   if        %subst(wblk:3:1)<>' 'and v1ccau<>'3'
088400140919     c                   movea     'IndirAl'     sk8(x)
088500140813     c                   endif
088600140527     c* Altro
088700140813     c                   if        %subst(wblk:4:1)<>' 'and v1ccau<>'4'
088800140919     c                   movea     'Altro  '     sk8(x)
088900140813     c                   endif
089000140813     c                   endif
089100140814     c* se ne ho più di una imposto in "+" nel sfl
089200140814    3c                   if        savpiu='S'
089300140919     c                   eval      sk8(8)='+'
089400140814    3c                   endif
089500070504     c
089600070504     c                   movea     sk8           v1sabi
089700140813     c                   eval      v1hblk=wblk
089800070504     c                   ENDSR
089900070321     c**********************************************************************
090000070321     C     Gestionesfl   BEGSR
090100070321     c**********************************************************************
090200070321      * Elaborazione opzioni.
090300070321     c*
090400140811     c                   readc     LRY02s01
090500070402     c
090600070321     c                   dow       %eof=*oFF
090700070321     C                   IF        v1sopz <> *blanks
090800070321     c                   exsr      chiamo
090900070321     c                   move      *blank        v1sopz
091000140930     c                   z-add     snrr1         csrrrn
091100070402     c
091200070402     c* Se non premuto F12-proteggo opzione
091300140813     c                   if        wf6='S'
091400140813     c                   seton                                        10
091500140813     c                   endif
091600070402     c
091700140811     c                   update    LRY02s01
091800070402     c                   setoff                                       10
091900070321     C                   ENDIF
092000070402     c
092100140811     c                   readc(E)  LRY02s01
092200070321     C                   enddo
092300070321     C                   ENDSR
092400070321     c**********************************************************************
092500070321     C     chiamo        BEGSR
092600141028     c                   clear                   fnlry03ds2
092700140813     c                   clear                   wf6               1
092800140813     c                   if        v1cnsp>0
092900140813     c                   eval      ilry03aas=v1caas
093000140813     c                   eval      ilry03lnp=v1clnp
093100140813     c                   eval      ilry03nrs=v1cnrs
093200140813     c                   eval      ilry03nsp=v1cnsp
093300140813     c* imposto il primo progressivo da elaborare trovato
093400140813     c                   eval      ilry03prg=savprg
093500140813     c                   else
093600140813     c                   eval      ilry03aas=v1saas
093700140813     c                   eval      ilry03lnp=v1hlnp
093800140813     c                   eval      ilry03nrs=v1hnrs
093900140813     c                   eval      ilry03nsp=v1hnsp
094000140813     c                   eval      ilry03prg=v1hprg
094100140813     c                   endif
094200141006     c* imposto la filiale gestione
094300141006     c                   eval      ilry03fgs=v1cfgs
094400140813     c* Se richiesto causale contatto
094500140813    1c                   if        v1ccau<>' '
094600140813     c                   eval      ilry03tipd=v1ccau
094700140813     c                   else
094800140813     c                   eval      wpos=%scan('E':v1hblk)
094900140813     c                   movel     wpos          ilry03tipd
095000140813    1c                   endif
095100140813     c
095200140813     c                   clear                   wprima_v
095300140813     c                   clear                   xx
095400140813    1c                   do        *hival
095500140813    2c                   if        ilry03tipd=' '
095600140813     c                   leave
095700140813    2c                   endif
095701141028     c                   eval      ILRY03DF12='S'
095702141028     c                   eval      ILRY03DF21='N'
095703141030     c* se filiale non partita con nuova procedura abilito l'F21 per i dirott.
095704141030     c                   if        %lookup('999':skFilOkdir)=0 and
095705141030     c                             %lookup(%editc(v1cfgs:'X'):skFilOkDir)=0
095706141030     c                   eval      ILRY03DF21='S'
095707141030     c                   endif
095708141028     c                   eval      ILRY03DF06='S'
095709141028     c                   eval      ILRY03EF12='S'
095710141028     c                   eval      ILRY03EF21='S'
095800141028     c                   eval      ILRY03EF06='S'
095900140813     c
095901141028     c                   eval      kpjbu=fnlry03ds2
096000140813     c                   call      'FNLRY03R'
096100140813     c                   parm                    kpjba
096101141028     c                   eval      fnlry03ds2=kpjbu
096300140813    2c                   if        olry03f12=*blanks or olry03err<>' '
096400140813     c                   eval      wf6='S'
096500140813    2c                   endif
096600140813    c
096700140813     c* chaino file variazioni
096800140813     c                   z-add     ilry03aas     k_aas
096900140813     c                   z-add     ilry03lnp     k_lnp
097000140813     c                   z-add     ilry03nrs     k_nrs
097100140813     c                   z-add     ilry03nsp     k_nsp
097200140909     c     kidc          setll     tiidc01l
097300140909     c     kidc          reade     tiidc01l
097400140909    2c                   dow       not %eof(tiidc01l)
097500140813     c*
097600140909    3c                   if        idcelaflg='E'
097700140813     c                             and idcprg>ilry03prg
097800140813     c                   leave
097900140813    3c                   endif
098000140909     c     kidc          reade     tiidc01l
098100140813    2c                   enddo
098200140813     c
098300140909    2c                   if        not %eof(tiidc01l)
098400140813     c                   movel     idctipodis    ilry03tipd
098500140813     c                   eval      ilry03prg=idcprg
098600140813     c                   else
098700140813     c                   clear                   ilry03tipd
098800140813    2c                   endif
098900140813     c
099000140813    1c                   enddo
099100070321     C                   ENDSR
099200011227     c**********************************************************************
099300000607     C     *INZSR        BEGSR
099400011227     c**********************************************************************
099500000607      *
099600000607     C     *ENTRY        PLIST
099700000607     C                   PARM                    Kpjba
099800140930     c                   movel     kpjbu         fnlry02ds
099801141014
099802141014     c                   movel     kpjbu         savkpjbu
099900070123      *
100000070315     c     *dtaara       define    §azute        azuteds
100100070315     c     *dtaara       define    §datiute      ddatiute
100200070315     c                   in(e)     *dtaara
100300070315     c                   if        %error  or rsut = *blanks
100400070315     c                   clear                   tibs34ds
100500070315     c                   call      'TIBS34R'
100600070315     c                   parm                    tibs34ds
100700070315     c                   in        *dtaara
100800070315     c                   endif
100801141014     c                   Clear                   wabi
100802141027     c                   Clear                   v1cmsg
100803141014     c                   Clear                   dLat
100804141014
100805141028     c                   setoff                                       19
100806141030      * Aggancio tabella VPO record DECOFI816DIR
100807141030     c                   clear                   trulvpods
100808141030     c                   eval      ivpoke1 = 'DECOFI816DIR'
100809141030     c                   call      'TRULVPOR'
100810141030     c                   parm                    trulvpods
100811141030     c                   eval      skfilokdir=skfilok
100812141028     c* PGM RICHIAMATO: è richiamato se presente fgs
100813141028     c                   if        ilry02fgs<>*blanks
100814141028     c* controllo correttezza della filiale gestione --> deve contenere solo numeri
100815141028     c                   if        %check(digits:ilry02fgs)>0
100816141028     c                   Seton                                        28
100817141028     C                   movel     msg(1)        v1cmsg
100818141028     c                   endif
100819141028     c* flag dirottamento; valori possibili:
100820141028     c*"I"=Includi
100821141028     C*"E"=Escludi
100822141028     c*" "=Escludi
100823141028     c                   if        ilry02dir<>'I' and
100824141028     c                             ilry02dir<>'E' and
100825141028     C                             ilry02dir<>' '
100826141028     c                   Seton                                        28
100827141028     C                   movel     msg(2)        v1cmsg
100828141028     c                   endif
100829141028
100830141028     c                   if        *in28
100831141028     c                   goto      fine
100832141028     c                   endif
100845141028     c                   seton                                        19
100848141028     c                   else
100849141014     c* PROGRAMMA NON RICHIAMATO:
100850141014      * Verifica errori e autorità profilo
100851141014s   1c                   Select
100852141014      * se ho errori nei dati utente esco dal pgm
100853141014w   1c                   When      DutErr = 'E'
100854141014     c                   GoTo      Fine
100855141014      * se non c'è l'abilitazione
100856141014      * --> se 1° livello, abilitazioni al terminal
100857141014      *     se 2° livello, abilitazioni al punto operativo
100858141014      *     se sede è impossibile ma se capita mando a fine il pgm
100859141014w   1c                   When      UteAut = *Blanks
100860141014if  2c                   If        DutLpo = '1'
100861141014     c                   Eval      wabi   = 'TP'
100862141014e   2c                   EndIf
100863141014if  2c                   If        DutLpo = '2'
100864141014     c                   Eval      wabi   = 'PO'
100865141014e   2c                   EndIf
100866141014if  2c                   If        DutLpo = 'S'
100867141014     c                   GoTo      Fine
100868141014e   2c                   EndIf
100869141014      * carica le abilitazioni del profilo
100870141014x   1c                   Other
100871141014     c                   Movel     UteFaf        Dute01
100872141014     c                   If        §Utembf <> *Blanks
100873141014     c                   Eval      wabi = §Utembf
100874141014     c                   Else
100875141014     c                   Eval      wabi = UteAut
100876141014     c                   EndIf
100877141014e   1c                   EndSl
100878141014
100879141014      * controllo se ok l'abilitazione dell'utente
100880141014     c                   Clear                   Tibs02ds
100881141014     c                   Eval      T02Mod = 'C'
100882141014     c                   Eval      T02Sif = knsif
100883141014     c                   Eval      T02Cod = 'LAT'
100884141014     c                   Movel(p)  wabi          T02Ke1
100885141014     c                   Call      'TIBS02R'
100886141014     c                   Parm                    kpjba
100887141014     c                   Parm                    Tibs02ds
100888141014if  1c                   If        T02Err = *Blanks
100889141014     c                   Eval      dLat = T02Uni
100890141014e   1c                   EndIf
100891141014      * errore o non abilitato
100892141014if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
100893141014     c                   Seton                                        28
100894141015     c                   eval      noaut='1'
100895141027     C                   movel     msg(16)       v1cmsg
100896141014   x1c                   Else
100897141014      * Reperimento dei P.O. gestibili dall'utente
100898141014     c                   clear                   TRUL31DS
100899141014     c                   clear                   TRUL31DS2
100900141014     c                   eval      I31abi = wabi
100901141014     c                   eval      I31cdi = DUTdis
100902141014     c                   eval      I31car = DUTare
100903141014     c                   eval      I31cpo = DUTpou
100904141014     c                   call      'TRUL31R'
100905141014     c                   parm                    KPJBA
100906141014     c                   parm                    TRUL31DS
100907141014     c                   parm                    TRUL31DS2
100908141014if  2c                   if        O31pog > *zeros
100909141014     c                   movea     O31pog        $pog
100910141014x   2c                   else
100911141014     c                   Seton                                        28
100912141027     c                   movel     MSG(16)       v1cmsg
100913141015     c                   eval      noaut='1'
100914141014e   2c                   endif
100915141014    1c                   endif
100916141030    1c                   endif
100928141014     c
100929141014     c                   movel     savkpjbu      kpjbu
100930030702      *reperimento data
101000070319     C                   TIME                    W0140            14 0
101100070319     C                   MOVE      W0140         WDAT              8 0
101200030702     C*
101300030702     C                   Z-ADD     WDAT          G02DAT
101400030702     C                   MOVEL     *BLANK        G02ERR
101500030702     C                   CALL      'XSRDA8'
101600030702     C                   PARM                    WLBDA8
101700030702     C* UDATE A 8 IN AAAA/MM/GG
101800030717     C                   Z-ADD     G02INV        WUDAte            8 0
101900000607      *
102000070123      *
102100070124     C     Karb          klist
102200070327     C                   kfld                    K_aas
102300070327     C                   kfld                    K_lnp
102400070327     C                   kfld                    K_nrs
102500070327     C                   kfld                    K_nsp
102600140909     C     KIDC          klist
102700140909     C                   kfld                    K_aas
102800140909     C                   kfld                    K_lnp
102900140909     C                   kfld                    K_nrs
103000140909     C                   kfld                    K_nsp
103100140909     C                   kfld                    K_ELAdata
103200070125      *
103300070201      *
103400140909     C     KidcA         klist
103500140812     C                   kfld                    idcaas
103600140812     C                   kfld                    idclnp
103700140812     C                   kfld                    idcnrs
103800140812     C                   kfld                    idcnsp
103900070201      *
104000070201     C     Ktab          klist
104100070201     C                   kfld                    tblkut
104200070201     C                   kfld                    tblcod
104300070201     C                   kfld                    tblkey
104400070522     c*
104500070522     C     Karg1         klist
104600070522     C                   kfld                    arbaas
104700070522     C                   kfld                    arblnp
104800070522     C                   kfld                    arbnrs
104900070522     C                   kfld                    arbnsp
105000070412     c
105100070201     C                   z-add     1             tblkut
105200070205      * carico filiali £1
105300070205     C                   clear                   trul06ds
105400070205     C                   movel     '£1'          d06cod
105500070320     C                   movel     simfel        d06key
105600070205     C                   movel     trul06ds      kpjbu
105700070205     C                   CALL      'TRUL06R'
105800070205     C                   parm                    kpjba
105900070315     c                   movel     kpjbu         trul06ds
106000070315     C                   movea     lin           l1
106100070320     C* CARICO TABELLA FILIALI GESTITE DA ALTRE  SU TUTTO L'AS
106200070320     C                   CLEAR                   trul06ds
106300070320     C                   MOVE      '£6'          D06COD
106400070320     c                   eval      d06esc = 'G'
106500070320     c                   movel     simfel        d06key
106600070320     C                   MOVEL     trul06ds      KPJBU
106700070320     C                   CALL      'TRUL06R'
106800070320     C                   PARM                    KPJBA
106900070320     C                   MOVEL     KPJBU         trul06ds
107000070320     C                   MOVEl     LIN           nog
107100030714
107200011227     C                   ENDSR
107300070125      *--------------------------------------------------------------------------------------------*
107400070125      * ordina - riscrive il sfl ordinato per località
107500070125      *--------------------------------------------------------------------------------------------*
107600070125     C     ordina        BEGSR
107700070125     C                   setoff                                       72
107800070125      * IMPOSTO ENTRATA AD 'A'
107900070125     C                   EVAL      snrr1 = 0
108000070125      *
108100070125     C                   CLEAR                   QLGSCB
108200070125     C                   CLEAR                   QLGSCB00
108300070125
108400071203     ** 2 campi chiave.
108500071203     C                   EVAL      QLGNBRK    = 2
108600071204     c                   select
108700140812     c* ordinamento per inserimento
108800071204     c                   when      w_ord=1
108900140812     ** inseriment dopo rag. sociale destinatario 2 byte,
109000140812     C                   EVAL      QLGSP      = 1 +%SIZE(v1srsd)+
109100140812     c                                          %SIZE(v1sprd)
109200140812     C                   EVAL      QLGSS      = %SIZE(v1hinsdata)
109300070125     C                   EVAL      QLGDT      = carattere
109400070125     C                   EVAL      QLGSO      = Ascendente
109500070125     C                   EVAL      QLGKL(1)   = QLGSKL
109600140812     ** ragione sociale
109700140812     C                   EVAL      QLGSP      = 1
109800140812     C                   EVAL      QLGSS      = %SIZE(v1srsd)
109900140812     C                   EVAL      QLGDT      = carattere
110000140812     C                   EVAL      QLGSO      = Ascendente
110100140812     C                   EVAL      QLGKL(2)   = QLGSKL
110200071204     c* ordinamento per rag.sociale/prv destinatario
110300071204     c                   when      w_ord=2
110400071204     C                   EVAL      QLGSP      = 1
110500071204     C                   EVAL      QLGSS      = %SIZE(v1srsd)
110600071204     C                   EVAL      QLGDT      = carattere
110700071204     C                   EVAL      QLGSO      = Ascendente
110800071204     C                   EVAL      QLGKL(1)   = QLGSKL
110900071204     ** ragione sociale a posizione 1, carattere, 24 byte,
111000071204     C                   EVAL      QLGSP      = 1 + %size(v1srsd)
111100071204     C                   EVAL      QLGSS      = %SIZE(v1sprd)
111200071204     C                   EVAL      QLGDT      = carattere
111300071204     C                   EVAL      QLGSO      = Ascendente
111400071204     C                   EVAL      QLGKL(2)   = QLGSKL
111500071204     c                   endsl
111600070125
111700070125      * Load other sort parameters.
111800070125     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
111900070125     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
112000070125     C                   EVAL      QLGRT     = 8
112100070125     C                   EVAL      QLGOKL    = 80
112200070125     C                   EVAL      QLGLKE    = 16
112300070125     C                   EVAL      QLGLSS    = 290
112400070125
112500070125      * Initialize Sort I/O API fields.
112600070125     C                   EVAL      QLGRL00  = QLGRL
112700070125     C                   EVAL      QLGRC00  = 1
112800070125     C                   CLEAR                   QUSEI
112900070125     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
113000070125
113100070125      * First step - Initialize the sort routine.
113200070125     C                   CALL      'QLGSORT'
113300070125     C                   PARM                    QLGSCB
113400070125     C                   PARM                    NotUsed
113500070125     C                   PARM                    NotUsed
113600070125     C                   PARM                    SizeList
113700070125     C                   PARM                    ReturnSize
113800070125     C                   PARM                    QUSEC
113900070125
114000070125      * Next step - Write records to I/O routine.
114100070125     C                   EVAL      QLGRT00 = Put
114200070125
114300070125     C                   DO        *hival        cx                4 0
114400070125
114500140811     C     cx            CHAIN     LRY02s01                           50
114600070125     c   50              leave
114700070125     ** Solo le righe con Selected = 'Y' sono riordinate,
114800070125     ** quindi per fare un ordinamento di tutte le righe
114900070125     ** metto 'Y' sempre.
115000070125     C                   EVAL      Selected = 'Y'
115100070125
115200070125     C                   CLEAR                   QUSEI
115300070125     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
115400070125     C                   CALL      'QLGSRTIO'
115500070125     C                   PARM                    QLGSCB00
115600070125     C                   PARM                    SflRcd
115700070125     C                   PARM                    NotUsed
115800070125     C                   PARM                    SizeList
115900070125     C                   PARM                    NotUsed
116000070125     C                   PARM                    QUSEC
116100070125
116200070125     C                   ENDDO
116300070125
116400070125      * Next step - Signal end of input, clear subfile for reload.
116500070125     C                   EVAL      QLGRT00 = EndPut
116600070125     C                   CLEAR                   QUSEI
116700070125     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
116800070125
116900070125     C                   CALL      'QLGSRTIO'
117000070125     C                   PARM                    QLGSCB00
117100070125     C                   PARM                    SflRcd
117200070125     C                   PARM                    NotUsed
117300070125     C                   PARM                    SizeList
117400070125     C                   PARM                    NotUsed
117500070125     C                   PARM                    QUSEC
117600070125
117700070316     C                   SETOff                                       21        *
117800140811     C                   WRITE     LRY02c01                                      *
117900070316     C                   SETOn                                        21        *
118000140811     C                   WRITE     LRY02z01                                      *
118100070125
118200070125      * Final step - Write the records back to the subfile.
118300070125     C                   EVAL      QLGRT00 = Get
118400070316     C                   DO        lasnrr1       x                 4 0
118500070125     C                   CLEAR                   QUSEI
118600070125     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
118700070125     C                   CALL      'QLGSRTIO'
118800070125     C                   PARM                    QLGSCB00
118900070125     C                   PARM                    NotUsed
119000070125     C                   PARM                    SflRcd
119100070125     C                   PARM                    QLGRL00
119200070125     C                   PARM                    NotUsed
119300070125     C                   PARM                    QUSEC
119400070125      *
119500070125     c                   z-add     x             snrr1
119600140811     C                   WRITE     LRY02s01
119700070125     C                   ENDDO
119800070125
119900070125      *
120000070125     C                   ENDSR
120001141030      *--------------------------------------------------------------------------------------------*
120002141030      * imposta a video il flag "Solo dirottamenti"
120003141030      *--------------------------------------------------------------------------------------------*
120004141030     C     sr_impodir    BEGSR
120005141030     c* PGM RICHIAMATO
120006141030     c* se fgs     partita con nuova procedura dir=' ' significa "Escludi"
120007141030     c* se fgs non partita con nuova procedura dir=' ' significa "Tutti"
120008141030     c                   if        ilry02fgs<>*blanks
120009141030     c                   eval      v1cdir=ilry02dir
120010141030     c                   if        ilry02dir=*blanks
120011141030     c                   if         %lookup('999':skFilOkdir)>0 or
120012141030     c                              %lookup(%editc(v1cfgs:'X'):skFilOkdir)>0
120013141030     c                   movel     'E'           v1cdir
120014141030     c                   endif
120015141030     c                   endif
120016141030     c                   else
120017141030     C* PGM NON RICHIAMATO
120018141030     c* default inclusione/esclusione dirottamenti
120019141030     c                   clear                   v1cdir
120020141030     c* se fgs     partita con nuova procedura dir=' ' significa "Escludi"
120021141030     c* se fgs non partita con nuova procedura dir=' ' significa "Tutti"
120022141030     c                   if         %lookup('999':skFilOkdir)>0 or
120023141030     c                              %lookup(%editc(v1cfgs:'X'):skFilOkdir)>0
120024141030     c                   movel     'E'           v1cdir
120025141030     c                   endif
120026141030     c                   endif
120027141030     c                   endsr
120100070330**
120200070319Filiale in gestione errata
120300141028Flag dirottamento errato. Valori possibilili: "I","E"," "
120400070402Selezionare per Numero Spedizione OPPURE altre parzializzazioni
120500070402Spedizione inesistente
120600070316Parzializzazione per ragione sociale errata: inserire correttamente
120700140527Tipo elaborazione obbligatorio
120800140527
120900140527La spedizione non prevede disposizioni da caricare in base al tipo immesso      8
121000140527
121100140527
121200140527
121300140527Nessuna disposizione da elaborare per la selezione fatta                       12
121400140527Spedizione non gestibile: bloccata con blocco " "                              13
121500140812Spedizione non gestibile: consegnata                                           14
121600070402Spedizione non gestibile: linea di arrivo non in gestione                      15
121700141015Utente non abilitato alla Gestione Disposizioni di Consegna                    16
