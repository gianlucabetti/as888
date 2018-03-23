000100001003     H DECEDIT('0,') DATEDIT(*YMD.)
000200000927
000300000927      *****************************************************************
000400000927      *                                                               *
000500041115      *       Lista tipi invio anagrafica clienti                     *
000600000927      *                                                               *
000700000927      *****************************************************************
000800000927
000900010118     FTABEL00F  IF   E           K DISK
001000041115     FAZORG01L  IF   E           K DISK
001100050803     FAZORG02L  IF   E           K DISK    rename(azorg:azorg002)
001200041115     FTNTA71D   CF   E             WORKSTN
001300000927
001400050803     D MSG             S             78    DIM(18) CTDATA PERRCD(1)
001500010118
001600050802     d wabi            s                   like(UteAut)
001700041115     d codut           s              1  0 Inz(1)
001800050803     D §KUT            s                   like(codut)
001900050803     D §COD            s                   like(TBLCOD)
002000050803     D §KEY            s                   like(TBLkey)
002100050803     D COD             s                   like(TBLCOD)
002200050803     D KEY             s                   like(TBLkey)
002300050803     D WARE            s                   like(ORGCAR)
002400050802     D $pog            s              3    dim(250)
002500050802     D $DIG            s              1    dim(20)
002600050802     D $ARG            s              3    dim(50)
002700010208
002800000927     D KPJBA         E DS
002900000927
003000000927      * Parametri Azienda e Tabelle Ricerca Conti
003100000927
003200041115     d Azuteds       e ds                  Extname(Azute00f)
003300041115     d dDatiute      e ds
003400041115     d Tibs34ds      e ds                  Inz
003500041115     d tnta71ds      e ds
003600050802     d dute01        e ds
003700050802     d trul31ds      e ds
003800050802     D TRUL31DS2     E DS
003900000927
004000000927      *---------------------------------------------------------------*
004100000927      *                                                               *
004200000927      *    UTILIZZO DEGLI INDICATORI                                  *
004300000927      *                                                               *
004400000927      *---------------------------------------------------------------*
004500000927      *                                                               *
004600041116      *    02 -                                                       *
004700000927      *                                                               *
004800000927      *---------------------------------------------------------------*
004900001011
005000050802     c                   Clear                   wabi
005100050802      * Verifica errori e autorità profilo
005200050802s   1c                   Select
005300050802      * se ho errori nei dati utente esco dal pgm
005400050802w   1c                   When      DutErr = 'E'
005500050802     c                   GoTo      Fine
005600050802      * se non c'è l'abilitazione
005700050802      * --> se 1° livello, abilitazioni al terminal
005800050802      *     se 2° livello, abilitazioni al punto operativo
005900050802w   1c                   When      UteAut = *Blanks
006000050802if  2c                   If        DutLpo = '1'
006100050802     c                   Eval      wabi   = 'TP'
006200050802e   2c                   EndIf
006300050802if  2c                   If        DutLpo = '2'
006400050802     c                   Eval      wabi   = 'PO'
006500050802e   2c                   EndIf
006600050802if  2c                   If        DutLpo = 'S'
006700050802     c                   Eval      wabi   = 'AZ'
006800050802e   2c                   EndIf
006900050802      * carica le abilitazioni del profilo
007000050802x   1c                   Other
007100050802     c                   Movel     UteFaf        Dute01
007200050802     c                   If        §UteCli <> *Blanks
007300050802     c                   Eval      wabi = §UteCli
007400050802     c                   Else
007500050802     c                   Eval      wabi = UteAut
007600050802     c                   EndIf
007700050802e   1c                   EndSl
007800050802
007900050802      * Reperimento dei P.O. gestibili dall'utente
008000050802     c                   clear                   TRUL31DS
008100050802     c                   clear                   TRUL31DS2
008200050802     c                   eval      I31abi = wabi
008300050802     c                   eval      I31cdi = DUTdis
008400050802     c                   eval      I31car = DUTare
008500050802     c                   eval      I31cpo = DUTpou
008600050802     c                   call      'TRUL31R'
008700050802     c                   parm                    KPJBA
008800050802     c                   parm                    TRUL31DS
008900050802     c                   parm                    TRUL31DS2
009000050802if  1c                   if        O31pog > *zeros
009100050802     c                   movea     O31pog        $pog
009200050802     c                   movea     O31arg        $arg
009300050802     c                   movea     O31dig        $dig
009400050802x   1c                   else
009500050802     c                   goto      fine
009600050802e   1c                   endif
009700041115     c                   exsr      sr_inz01
009800041115     c     emetti        tag
009900041115     c                   exfmt     ta71d01
010000041115     c                   setoff                                       9028
010100041115     c                   clear                   vidmsg
010200041115     c     *inkc         cabeq     *on           fine
010300041115     c                   exsr      ctrd01
010400041115     c                   if        *in90 = '1' or *inkf = '0'
010500041115     c                   goto      emetti
010600041115     c                   endif
010700041115     c* sottomissione pgm batch
010800041115     c                   clear                   tnta71ds
010900041115     c                   eval      d71pop = v1cpop
011000041115     c                   eval      d71dpo = v1dpop
011100050803     c                   eval      d71cdi = v1cdst
011200050803     c                   eval      d71car = v1ccar
011300041115     c                   eval      d71sl0 = v1csl0
011400041115     c                   eval      d71tag = v1ctag
011500041115     c                   eval      d71ec0 = v1cec0
011600041115     c                   eval      d71an1 = v1can1
011700041115     c                   eval      d71sl1 = v1csl1
011800041115     c                   eval      d71tcg = v1ctcg
011900041115     c                   eval      d71ec1 = v1cec1
012000041115     c                   eval      d71an2 = v1can2
012100041115     c                   eval      d71sl2 = v1csl2
012200041115     c                   eval      d71tca = v1ctca
012300041115     c                   eval      d71ec2 = v1cec2
012400041115     c                   eval      d71sl3 = v1csl3
012500041115     c                   eval      d71tpd = v1ctpd
012600041115     c                   eval      d71ec3 = v1cec3
012700041116     c                   eval      d71sl4 = v1csl4
012800041115     c                   eval      d71ttx = v1cttx
012900041209     c                   eval      d71abl = v1cabl
013000041123     c                   movel(p)  tnta71ds      kpjbu
013100041115     c                   movel     'TA72'        kcoaz
013200041115     c                   call      'BCH10'
013300041115     c                   parm                    kpjba
013400041115     c     fine          tag
013500000927     C                   eval      *inlr = *on
013600000927
013700000927
013800000927      *---------------------------------------------------------------*
013900000927      *    Inizializzazione Campi Video                               *
014000000927      *---------------------------------------------------------------*
014100000927
014200041115     C     SR_INZ01      begsr
014300000927
014400050803     c* se abilitazione a livello di p.o. propongo il p.o. a video
014500050803     c                   clear                   v1cpop
014600050803     c                   if        $pog(2) = *zeros
014700050803     c                   move      $pog(1)       v1cpop
014800050803     c                   endif
014900041115     c*
015000041115      *    Decodifico il P.O.
015100041115     c                   clear                   v1dpop
015200041115     c     v1cpop        ifgt      *zeros
015300041115     C     v1cpop        chain     azorg01l
015400000927     C                   if        %found(azorg01l)
015500041115     C                   movel     orgdes        v1dpop
015600050803     c                   movel     orgfl3        v1cdst
015700050803     c                   move      orgcar        v1ccar
015800050803     c                   exsr      decDISTR
015900050803     c                   exsr      decCAR
016000000927     C                   endif
016100041115     c                   endif
016200041209     c* clienti bloccati
016300041209     c                   eval      v1cabl = 'E'
016400000927
016500000927      *    Inizializzo i Campi Restanti
016600050803     c                   if        v1cpop = *zeros
016700050803     c                   clear                   v1cdst
016800050803     c                   clear                   v1ddst
016900050803     c                   clear                   v1ccar
017000050803     c                   clear                   v1dcar
017100050803     c                   endif
017200041115     c                   clear                   v1csl0
017300041115     c                   clear                   v1csl1
017400041115     c                   clear                   v1csl2
017500041115     c                   clear                   v1csl3
017600041115     c                   clear                   v1csl4
017700041115     c                   clear                   v1ctag
017800041115     c                   clear                   v1ctcg
017900041115     c                   clear                   v1ctca
018000041115     c                   clear                   v1ctpd
018100041115     c                   clear                   v1cttx
018200041115     c                   movel     'N'           v1cec0
018300041115     c                   movel     'N'           v1cec1
018400041115     c                   movel     'N'           v1cec2
018500041115     c                   movel     'N'           v1cec3
018600041115     c                   movel     'N'           v1can1
018700041115     c                   movel     'N'           v1can2
018800041115     c                   setoff                                       020304
018900041116     c                   setoff                                       0507
019000041115     c                   setoff                                       080910
019100041115     c                   setoff                                       111213
019200041116     c                   setoff                                       1416
019300050803     c                   setoff                                       1718
019400001012
019500000927
019600000927     C                   endsr
019700041115      *****************************************************************
019800041115      * Controllo videata
019900041115      *****************************************************************
020000041115     C     CTRD01        BEGSR
020100041115     c* spengo indicatori di errore
020200041115     c                   setoff                                       020304
020300041116     c                   setoff                                       0507
020400041115     c                   setoff                                       080910
020500041115     c                   setoff                                       111213
020600041116     c                   setoff                                       1416
020700050803     c                   setoff                                       1718
020800050803     c* DISTRETTO
020900050803     c                   exsr      CtrDISTR
021000050803     c   90              goto      EndC01
021100050803     c
021200050803     c* AREA
021300050803     c                   exsr      CtrAREA
021400050803     c   90              goto      EndC01
021500041115     c
021600041116     c* PUNTO OPERATIVO
021700050803     c                   exsr      CtrPO
021800050803     c   90              goto      EndC01
021900041116     c* TIPO COMUNICAZIONE: almeno uno deve essere stato scelto
022000041115    1c     v1csl0        ifeq      *blanks
022100041115     c     v1csl1        andeq     *blanks
022200041115     c     v1csl2        andeq     *blanks
022300041115     c     v1csl3        andeq     *blanks
022400041115     c     v1csl4        andeq     *blanks
022500041115     c                   seton                                        902809
022600041115     c                   movel     msg(3)        vidmsg
022700041115     c                   goto      endc01
022800041115    1c                   endif
022900041115     c* errore se scelto un tipo comunicazione o stampa solo eccez=S o stamp
023000041115     c* anomalie =S e non effettuata la scelta del tipo comunicazione
023100041125     c                   If        v1csl0 = *blanks and
023200041125     c                             (v1ctag <> *blanks or v1cec0 = 'S' or
023300041125     c                              v1can1 = 'S')
023400041115     c                   seton                                        902809
023500041115     c                   endif
023600041125     c                   If        v1csl1 = *blanks and
023700041125     c                             (v1ctcg <> *blanks or v1cec1 = 'S' or
023800041125     c                              V1can2 = 'S')
023900041115     c                   seton                                        902812
024000041115     c                   endif
024100041125     c                   If        v1csl2 = *blanks and
024200041125     c                             (v1ctca <> *blanks or v1cec2 = 'S')
024300041115     c                   seton                                        902813
024400041125     c                   endif
024500041125     c                   If        v1csl3 = *blanks and
024600041125     c                             (v1ctpd <> *blanks or v1cec3 = 'S')
024700041115     c                   seton                                        902814
024800041125     c                   endif
024900041125     c                   If        v1csl4 = *blanks and v1cttx <> *Blanks
025000041115     c                   seton                                        902816
025100041125     c                   endif
025200041115     c     *in90         ifeq      *on
025300041115     c                   movel     msg(7)        vidmsg
025400041115     c                   goto      endc01
025500041115     c                   endif
025600041122     c*  COMUNICAZIONE DI APERTURA GIACENZA
025700041122    1c     v1csl0        ifeq      'X'
025800041122     c     v1ctag        andeq     *blanks
025900041122     c                   seton                                        902802
026000041122     C                   movel     msg(6)        vidmsg
026100041122     C                   goto      endc01
026200041122     c                   endif
026300041124     c*  COMUNICAZIONE DI CESSATA GIACENZA
026400041122    1c     v1csl1        ifeq      'X'
026500041122     c     v1ctcg        andeq     *blanks
026600041122     c                   seton                                        902803
026700041122     C                   movel     msg(6)        vidmsg
026800041122     C                   goto      endc01
026900041122     c                   endif
027000041124     c*  AVVISO DI BONIFICO
027100041122    1c     v1csl2        ifeq      'X'
027200041122     c     v1ctca        andeq     *blanks
027300041122     c                   seton                                        902804
027400041122     C                   movel     msg(6)        vidmsg
027500041122     C                   goto      endc01
027600041122     c                   endif
027700041124    1c     v1csl2        ifeq      'X'
027800041124     c     v1ctca        andeq     'F'
027900041124     c                   seton                                        902804
028000041124     C                   movel     msg(4)        vidmsg
028100041124     C                   goto      endc01
028200041124     c                   endif
028300041124     c*  PREAVVISO DI DANNO
028400041122    1c     v1csl3        ifeq      'X'
028500041122     c     v1ctpd        andeq     *blanks
028600041122     c                   seton                                        902805
028700041122     C                   movel     msg(6)        vidmsg
028800041122     C                   goto      endc01
028900041122     c                   endif
029000041124     c*  TESTI LETTERA
029100041122    1c     v1csl4        ifeq      'X'
029200041123     c     v1cttx        andeq     *blanks
029300041122     c                   seton                                        902807
029400041122     C                   movel     msg(6)        vidmsg
029500041122     C                   goto      endc01
029600041122     c                   endif
029700041115     c     endc01        tag
029800041115     c                   endsr
029900050803      *--------------------------------------------------------------------*
030000050803     c* Controllo codice distretto
030100050803      *--------------------------------------------------------------------*
030200050803     c     CtrDISTR      BEGSR
030300050803     c                   clear                   v1ddst
030400050803     c     '?'           scan      v1cdst                                 90
030500050803    1c                   if        *in90
030600050803     c                   movel     codut         §kut
030700050803     c                   movel     '17'          §cod
030800050803     c                   clear                   §key
030900050803     c                   call      'X§TABER'
031000050803     c                   parm                    §kut
031100050803     c                   parm                    §cod
031200050803     c                   parm                    §key
031300050803     c                   parm                    §des             25
031400050803     c                   movel     §key          v1cdst
031500050803     c                   movel     §des          v1ddst
031600050803     c                   seton                                        17
031700050803     c                   goto      endctrDISTR
031800050803    1c                   endif
031900050803     c
032000050803    1c     v1cdst        ifne      *blanks
032100050803     c
032200050803     c                   movel     '17'          cod
032300050803     c                   movel(p)  v1cdst        key
032400050803     c     ktab          chain     tabel                              30
032500050803    2c                   if        *in30 OR tblflg<>' '
032600050803     c                   movel     msg(8)        vidmsg
032700050803     c                   seton                                        289017
032800050803     c                   goto      endctrDISTR
032900050803    2c                   endif
033000050803     c                   movel     tbluni        v1ddst
033100050803     C* Non è possibile richiedere il distretto '9' - SEDE
033200050803    2C                   if        v1cdst='9'
033300050803     c                   movel     msg(9)        vidmsg
033400050803     c                   seton                                        289017
033500050803     c                   goto      endctrDISTR
033600050803    2c                   endif
033700050803     c
033800050803     c
033900050803     c* controllo se esiste in organigramma
034000050803     c     v1cdst        chain     azorg02l                           30
034100050803    2c                   dow       not *in30 and orgfva<>' '
034200050803     c     v1cdst        reade     azorg02l                               30
034300050803    2c                   enddo
034400050803    2c                   if        *in30
034500050803     c                   movel     msg(10)       vidmsg
034600050803     c                   seton                                        289017
034700050803     c                   goto      endctrDISTR
034800050803    2c                   endif
034900050803     c* Controllo se abilitato al distretto,solo se non impostati area e p.o
035000050803    2c                   if        v1ccar<=*zeros and v1cpop = *zeros
035100050803     c     v1cdst        lookup    $dig                                   30
035200050803    3c                   if        not *in30
035300050803     c                   movel     msg(11)       vidmsg
035400050803     c                   seton                                        289017
035500050803     c                   goto      endctrDISTR
035600050803    3c                   endif
035700050803    2c                   endif
035800050803    1c                   endif
035900050803     c     EndctrDISTR   ENDSR
036000050803      *--------------------------------------------------------------------*
036100050803     c* controllo cod AREA
036200050803      *--------------------------------------------------------------------*
036300050803     c     CtrAREA       BEGSR
036400050803     c* Ricerca
036500050803     c                   clear                   v1dcar
036600050803     c     '?'           scan      v1ccar                                 90
036700050803    1c                   if        *in90
036800050803     c                   movel     codut         §kut
036900050803     c                   movel     '05'          §cod
037000050803     c                   clear                   §key
037100050803     c                   call      'X§TABER'
037200050803     c                   parm                    §kut
037300050803     c                   parm                    §cod
037400050803     c                   parm                    §key
037500050803     c                   parm                    §des
037600050803     c                   movel     §key          v1ccar
037700050803     c                   movel     §des          v1dcar
037800050803     c                   seton                                        18
037900050803     c                   goto      endctrAREA
038000050803    1c                   endif
038100050803     c     v1ccar        ifeq      *zeros
038200050803     c                   clear                   v1ccar
038300050803     c                   endif
038400050803     c*
038500050803    1c     v1ccar        ifne      *blanks
038600050803     c     v1ccar        andne     *zeros
038700050803     c*
038800050803     c* Deve esistere in tabella aree
038900050803     c                   movel     '05'          cod
039000050803     c                   movel(p)  v1ccar        key
039100050803     c     ktab          chain     tabel                              30
039200050803    2c                   if        *in30 OR tblflg<>' '
039300050803     c                   movel     msg(12)       vidmsg
039400050803     c                   seton                                        289018
039500050803     c                   goto      endctrAREA
039600050803    2c                   endif
039700050803     c                   movel     tbluni        v1dcar
039800050803     c
039900050803    2c     v1ccar        ifle      *zeros
040000050803     c                   clear                   ware
040100050803     c                   else
040200050803     c                   movel     v1ccar        ware
040300050803    2c                   endif
040400050803     c
040500050803     c* se immesso il distretto devono essere congruenti
040600050803    2c                   if        v1cdst<>' '
040700050803     c     KORG          chain     azorg02l                           30
040800050803    3c                   dow       not *in30 and orgfva<>' '
040900050803     c     KORG          reade     azorg02l                               30
041000050803    3c                   enddo
041100050803    3c                   if        *in30
041200050803     c                   movel     msg(13)       vidmsg
041300050803     c                   seton                                        289018
041400050803     c                   goto      endctrAREA
041500050803    3c                   endif
041600050803    2c                   endif
041700050803     c
041800050803     c* Controllo se utente abilitato  se non immesso il p.o.
041900050803     c                   if        v1cpop = *zeros
042000050803     c     v1ccar        lookup    $arg                                   30
042100050803     c                   if        not *in30
042200050803     c                   movel     msg(14)       vidmsg
042300050803     c                   seton                                        289018
042400050803     c                   goto      endctrAREA
042500050803     c                   endif
042600050803     c                   endif
042700050803     c
042800050803     c* valorizzo distretto se non immesso
042900050803     c     *loval        setll     azorg02l
043000050803     c                   read      azorg02l                               30
043100050803    2c                   dow       not *in30  and v1cdst=*blanks
043200050803    3c                   if        orgcar<>ware
043300050803     c     korg1         setgt     azorg02l
043400050803   x3c                   else
043500050803    4c     orgfva        ifeq      *blanks
043600050803     c                   movel     orgfl3        v1cdst
043700050803    4c                   endif
043800050803    3c                   endif
043900050803     c                   read      azorg02l                               30
044000050803    2c                   enddo
044100050803     c
044200050803     C* Non è possibile richiedere il distretto '9' - SEDE
044300050803     C                   if        v1cdst='9'
044400050803     c                   clear                   v1cdst
044500050803     c                   movel     msg(14)       vidmsg
044600050803     c                   seton                                        289018
044700050803     c                   goto      endctrAREA
044800050803     c                   endif
044900050803     c* non trovata area sull'organigramma
045000050803     C                   if        v1cdst=' '
045100050803     c                   clear                   v1cdst
045200050803     c                   movel     msg(16)       vidmsg
045300050803     c                   seton                                        289018
045400050803     c                   goto      endctrAREA
045500050803     c                   endif
045600050803     c
045700050803     c                   exsr      decDISTR
045800050803     c*
045900050803     c                   endif
046000050803     c     endctrAREA    ENDSR
046100050803      *--------------------------------------------------------------------*
046200050803     c* controllo cod p.o.
046300050803      *--------------------------------------------------------------------*
046400050803     c     CtrPO         BEGSR
046500050803     c* controllo p.o
046600050803     c                   clear                   v1dpop
046700050803    1c     v1cpop        ifgt      *zeros
046800050803     c     v1cpop        chain     azorg01l
046900050803    2c                   if        %found(azorg01l)
047000050803     c                   movel     orgdes        v1dpop
047100050803     c                   else
047200050803     c                   seton                                        289008
047300050803     c                   movel     msg(1)        vidmsg
047400050803     c                   goto      endctrPO
047500050803    2c                   endif
047600050803     c* controllo se utente abilitato al p.o.
047700050803     c                   move      v1cpop        w003a             3
047800050803     c     w003a         lookup    $pog                                   30
047900050803     c                   if        not *in30
048000050803     c                   movel     msg(2)        vidmsg
048100050803     c                   seton                                        289008
048200050803     c                   goto      endctrPO
048300050803     c                   endif
048400050803     c* se richiesto distretto errore se <>
048500050803     c                   if        v1cdst <> ' '   and orgfl3 <> v1cdst
048600050803     c                   seton                                        289008
048700050803     c                   movel     msg(17)       vidmsg
048800050803     c                   goto      endctrPO
048900050803     c                   endif
049000050803     c* se richiesta area      errore se <>
049100050803     c                   if        v1ccar > *zeros and ware <> orgcar
049200050803     c                   seton                                        289008
049300050803     c                   movel     msg(18)       vidmsg
049400050803     c                   goto      endctrPO
049500050803     c                   endif
049600050803     c*
049700050803     c                   if        v1cdst = *blanks
049800050803     c                   move      orgfl3        v1cdst
049900050803     c                   endif
050000050803     c                   if        v1ccar = *blanks or v1ccar = *zeros
050100050803     c                   move      orgcar        v1ccar
050200050803     c                   endif
050300050803     c* decodifico codice distretto
050400050803     c                   exsr      decDISTR
050500050803     c* decodifico area
050600050803     c                   exsr      decCAR
050700050803    1c                   endif
050800050803     c     endctrPO      ENDSR
050900050803      *****************************************************************
051000050803     c* decodifica distretto
051100050803      *****************************************************************
051200050803     c     decDISTR      begsr
051300050803     c* Decodifico codice distretto
051400050803     c                   movel     '17'          cod
051500050803     c                   movel(p)  v1cdst        key
051600050803     c     ktab          chain     tabel                              30
051700050803     c  N30              movel     tbluni        v1ddst
051800050803     c                   endsr
051900050803      *****************************************************************
052000050803     c* Decodifica area
052100050803      *****************************************************************
052200050803     c     decCAR        begsr
052300050803     c                   movel     '05'          cod
052400050803     c                   movel(p)  v1ccar        key
052500050803     c     ktab          chain     tabel                              30
052600050803     c  n30              movel     tbluni        v1dcar
052700050803     c                   endsr
052800040413      *****************************************************************
052900040413      * ROUTINE INIZIALE
053000040413      *****************************************************************
053100040413     C     *INZSR        BEGSR
053200040413
053300040413     C     *ENTRY        PLIST
053400040413     C                   PARM                    KPJBA
053500040413
053600040413
053700040413      *    Accesso a TABEL
053800040413
053900050803     C     Ktab          klist
054000040413     C                   kfld                    codut
054100050803     C                   kfld                    cod
054200050803     C                   kfld                    key
054300050803
054400050803     c     kORG          klist
054500050803     c                   kfld                    v1cdst
054600050803     c                   kfld                    ware
054700050803
054800050803     c     KORG1         klist
054900050803     c                   kfld                    orgfl3
055000050803     c                   kfld                    orgcar
055100040413
055200040413      *    Recupero Ragione Sociale Ditta da Emettere a Video
055300040413      *    e Punti Operativi Gestiti
055400040413
055500041115     c     *dtaara       define    §azute        azuteds
055600041115     c     *dtaara       define    §datiute      ddatiute
055700041115     c                   in(E)     *dtaara
055800041115     c                   If        %error  or RSUT = *blanks
055900041115     c                   CLEAR                   tibs34ds
056000041115     c                   CALL      'TIBS34R'
056100041115     c                   PARM                    tibs34ds
056200041115     c                   in        *dtaara
056300041115     c                   EndIf
056400041115     c*
056500040413
056600040413     C                   endsr
056700000929
056800041124** MSG                                                                       *
056900050803Punto operativo non presente in organigramma                                  01
057000050803Utente non abilitato a richiedere il p.o. indicato                            02
057100041124Scegliere almeno un tipo comunicazione                                        03
057200041124Per l'avviso di bonifico tipo invio "Fax" non previsto                        04
057300041124                                                                              05
057400041124Tipo invio obbligatorio                                                       06
057500041124Immettere 'X' per selezionare il tipo comunicazione richiesto                 07
057600050803Cod.Distretto errato                                                          08
057700050803Cod.Distretto 9 - SEDE  non selezinabile                                      09
057800050803Cod.Distretto non presente nell'oranigramma                                   10
057900050803Utente non abilitato a richiedere il distretto indicato                       11
058000050803Cod.Area errato                                                               12
058100050803Cod.Area non congruente col distretto selezionato                             13
058200050803Utente non abilitato a richiedere l'Area indicata                             14
058300050803Cod.Area di distretto 9 - SEDE  non selezionabile                             15
058400050803Cod.Area non presente nell'organigramma                                       16
058500050803Cod.Distretto non congruente col p.o. selezionato                             17
058600050803Cod.Area non congruente col p.o. selezionato                                  18
