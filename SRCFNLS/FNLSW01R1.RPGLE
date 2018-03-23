000100980521      ****************************************************************
000200980521      *  RIEPILOGO INDICATORI
000300981008      ****************************************************************
000400990603      * 01 -
000500990317      ****************************************************************
000600980521     H DECEDIT('0,') DATEDIT(*DMY.)
000700980623
000800990602      *------------------------------------------------------------------------*
000900150122     ffnlsw01d  cf   E             workstn
000901150122     Fazorg01l  IF   E           K DISK
001000980521      *------------------------------------------------------------------------*
001100980702
001200990602      * Definizione campi di work ------------------------------------------
001300990609     D XX              s              3  0
001301150122     D ww              s              3  0
001302150122     d Dataiso         s               d   datfmt(*iso)
001303150122     d Dataeur         s               d   datfmt(*eur)
001304150122     d Datasys         s               d   datfmt(*iso)  inz(*sys)
001400990603     d dini_eur        s               D   datfmt(*eur)
001500990603     d dini_iso        s               D   datfmt(*iso)
001600990603     d dfin_eur        s               D   datfmt(*eur)
001700990603     d dfin_iso        s               D   datfmt(*iso)
001800990603     D wtibs69r        s              1
001900990602      * Schiere ------------------------------------------------------------
002000150122     D MSG             S             78    DIM(7) CTDATA PERRCD(1)
002100150122     d vlnp            s              3  0 dim(6)
002200150122     d dlnp            s             20    dim(6)
002201150122     d vtfp            s              3  0 dim(6)
002202150122     d dtfp            s             20    dim(6)
002300990602      * DS esterne ---------------------------------------------------------
003000150122     D fnlsw01ds     E DS
003100150122     d lnp                            3  0 dim(6) overlay(fnlsw01ds:34)
003101150122     d tfp                            3  0 dim(6) overlay(fnlsw01ds:52)
003200990602
003700990602     D KPJBA         E DS
003701150122     d Azuteds       e ds                  extname(Azute00f)
003702150122     d dDatiute      e ds
003703150122     D TIBS34DS      E DS
003704150122     D fnlv55ds      E DS
003800990602      * DS interne ---------------------------------------------------------
004800990602
004900980525      *
005000990601
005100150122     Ilsw01d1
005300150122     I              v1cln1                      vlnp(1)
005400150122     I              v1cln2                      vlnp(2)
005500150122     I              v1cln3                      vlnp(3)
005600150122     I              v1cln4                      vlnp(4)
005700150122     I              v1cln5                      vlnp(5)
005800150122     I              v1cln6                      vlnp(6)
006300150122     I              v1dln1                      dlnp(1)
006400150122     I              v1dln2                      dlnp(2)
006500150122     I              v1dln3                      dlnp(3)
006600150122     I              v1dln4                      dlnp(4)
006700150122     I              v1dln5                      dlnp(5)
006800150122     I              v1dln6                      dlnp(6)
006801150122     I              v1ctf1                      vtfp(1)
006802150122     I              v1ctf2                      vtfp(2)
006803150122     I              v1ctf3                      vtfp(3)
006804150122     I              v1ctf4                      vtfp(4)
006805150122     I              v1ctf5                      vtfp(5)
006806150122     I              v1ctf6                      vtfp(6)
006807150122     I              v1dtf1                      dtfp(1)
006808150122     I              v1dtf2                      dtfp(2)
006809150122     I              v1dtf3                      dtfp(3)
006810150122     I              v1dtf4                      dtfp(4)
006811150122     I              v1dtf5                      dtfp(5)
006812150122     I              v1dtf6                      dtfp(6)
007200980525      *
007300990601
007400990603     c                   clear                   v1cdti
007500990603     c                   clear                   v1cdtf
007600150122     c                   clear                   vlnp
007700150122     c                   clear                   dlnp
007701150122     c                   clear                   vtfp
007702150122     c                   clear                   dtfp
007708150122     c* in data dal al propongo la data del giorno
007709150122     c                   eval      dataiso=datasys
007710150122     c                   eval      dataeur=dataiso
007712150122     c                   eval      v1cdti =%dec(dataeur)
007713150122     c                   eval      v1cdtf =%dec(dataeur)
007714150122     c                   eval      v1csts ='N'
007900990603     c     emetti        tag
008000150122     c                   exfmt     lsw01d1
008100990603     c                   setoff                                       2829
008200990603     c     *inkc         cabeq     '1'           fine
008300990603     c                   exsr      ctrd01
008400990603     c   28
008500990603     cornkf
008600990603     cor 29              goto      emetti
008700150122     c                   move      v1cdti        lswgmi
008800150122     c                   move      dini_iso      lswdti
008900150122     c                   move      v1cdtf        lswgmf
009000150122     c                   move      dfin_iso      lswdtf
009100990604     c     v1cdtf        ifeq      *zeros
009200150122     c                   move      v1cdti        lswgmf
009300150122     c                   move      dini_iso      lswdtf
009400990604     c                   end
010000150122     c                   movea     vlnp          lnp
010001150122     c                   movea     vtfp          tfp
010002150122     c                   move      v1csts        lswsts
010200150122     c                   movel     fnlsw01ds     kpjbu
010300150122     c                   movel     'LS1W'        kcoaz
010400150123     c                   call      'BCH10'
010401150123     c******             call      'FNLSW01R2'
010500990603     c                   parm                    kpjba
010600990603     c     fine          tag
010700990603     c*
011300990603
011400990603     c                   seton                                        lr
011500990602
011600990603     c**********************************************************************
011700990603     c     *inzsr        begsr
011800990603
011900990603     C     *entry        plist
012000990603     C                   parm                    kpjba
012100990603
012101150122     c                   z-add     1             codut             1 0
012102150122      *
012103150122     c     *dtaara       define    §azute        azuteds
012104150122     c     *dtaara       define    §datiute      ddatiute
012105150122     c                   in(E)     *dtaara
012106150122    1c                   If        %error  or RSUT = *blanks
012107150122     c                   Clear                   Tibs34ds
012108150122     c                   Call      'TIBS34R'
012109150122     c                   Parm                    Tibs34ds
012110150122     c                   In        *dtaara
012111150122    1c                   EndIf
016101081002      *
016169081002     c
016200990603
016300990603     c                   endsr
016400990603
016500990603     c**********************************************************************
016600990603     c     ctrd01        begsr
016700990603     c                   setoff                                       010203
016800990603     c                   setoff                                       040506
016900990603     c                   setoff                                       070809
017000990603     c                   setoff                                       101112
017001150122     c                   setoff                                       131415
017002150122     c                   setoff                                       161718
017100990603     c* controllo data dal
017200990603     c     *dmy          test(d)                 v1cdti                 30
017300990603     c                   if        *in30 = '0'
017400990603     c     *dmy          move      v1cdti        dini_eur
017500990603     c                   else
017600990603     c     *eur          test(d)                 v1cdti                 31
017700990603     c                   if        *in31 = '0'
017800990603     c                   move      v1cdti        dini_eur
017900990603     c                   else
018000150122     c                   seton                                        1728
018100990604     c                   movel     msg(1)        v1zmsg
018200990603     c                   goto      ectd01
018300990603     c                   endif
018400990603     c                   endif
018500990603     c                   move      dini_eur      dini_iso
018600990603     c                   move      dini_eur      v1cdti
018700990603
018800990604     c* controllo data al se immessa
018900990604     c                   clear                   dfin_eur
019000990604     c                   clear                   dfin_iso
019100990604     c     v1cdtf        ifgt      *zeros
019200990603     c     *dmy          test(d)                 v1cdtf                 30
019300990603     c                   if        *in30 = '0'
019400990603     c     *dmy          move      v1cdtf        dfin_eur
019500990603     c                   else
019600990603     c     *eur          test(d)                 v1cdtf                 31
019700990603     c                   if        *in31 = '0'
019800990603     c                   move      v1cdtf        dfin_eur
019900990603     c                   else
020000150122     c                   seton                                        1828
020100990604     c                   movel     msg(1)        v1zmsg
020200990603     c                   goto      ectd01
020300990603     c                   endif
020400990603     c                   endif
020500990603     c                   move      dfin_eur      dfin_iso
020600990603     c                   move      dfin_eur      v1cdtf
020700990609     c* errore se data dal > di data al
020800990604     c     dini_iso      ifgt      dfin_iso
020900150122     c                   seton                                        1728
021000990604     c                   movel     msg(2)        v1zmsg
021100990604     c                   goto      ectd01
021200990604     c                   endif
021300990604     c                   endif
021400990604     c
021500990603
021600150122     c* controllo linee di partenza
021700150122     c                   do        6             ii                2 0
021800150122     c     vlnp(ii)      ifgt      0
021801150122     c     vlnp(ii)      chain     azorg01l
021802150122     c                   if        not %found(azorg01l)
026100150122IF  2C                   eval      *IN28 = '1'
026300150122     c                   move      '1'           *in(ii)
026400150122     c                   movel     msg(3)        v1zmsg
026500150122     c                   goto      ectd01
026501150122     c                   endif
026600150122     c                   movel     orgdes        dlnp(ii)
026700150122     c* errore se lnp ripetuta
026800990609     c                   z-add     1             yy                2 0
026900150122     c     vlnp(ii)      lookup    vlnp(yy)                               30
027000990609     c   30ii            ifne      yy
027100990609     c                   seton                                        28
027200150122     c                   move      '1'           *in(ii)
027300150122     c                   movel     msg(4)        v1zmsg
027400150122     c                   goto      ectd01
027500990609     c                   endif
027600990603     c                   else
027700150122     c                   clear                   dlnp(ii)
027800990603     c                   end
027900990603     c                   enddo
027901150122     c* controllo i terminal di partenza
027902150122     c                   do        6             ii                2 0
027903150122     c     vtfp(ii)      ifgt      0
027906150122     C                   CLEAR                   fnlv55ds
027907150122     C                   MOVEL     'P'           D55TPT
027908150122     C                   MOVEL     vtfp(ii)      D55LIN
027909150122     C                   MOVE      dini_iso      D55DRF
027910150122     C                   CALL      'FNLV55R'
027911150122     C                   PARM                    fnlv55ds
027912150122     c                   if        d55tfp<>vtfp(ii)
027913150122     c                             or d55err<>*blanks
027917150122IF  2C                   eval      *IN28 = '1'
027918150122     c                   eval      ww=ii+10
027919150122     c                   move      '1'           *in(ww)
027920150122     c                   movel     msg(6)        v1zmsg
027921150122     c                   goto      ectd01
027922150122     c                   endif
027923150122     c     vtfp(ii)      chain     azorg01l
027925150122     c                   movel     orgdes        dtfp(ii)
027926150122     c* errore se tfp ripetuto
027927150122     c                   z-add     1             yy                2 0
027928150122     c     vtfp(ii)      lookup    vtfp(yy)                               30
027929150122     c   30ii            ifne      yy
027930150122     c                   seton                                        28
027931150122     c                   eval      ww=ii+10
027932150122     c                   move      '1'           *in(ww)
027933150122     c                   movel     msg(7)        v1zmsg
027934150122     c                   goto      ectd01
027935150122     c                   endif
027936150122     c                   else
027937150122     c                   clear                   dtfp(ii)
027938150122     c                   end
027939150122     c                   enddo
028000990603     c     ectd01        endsr
028100990604**
028200990604Data errata                                                                    1
028300990604Data dal > di Data al                                                          2
028400150122Filiale errata                                                                 3
028500150122Filiale ripetuta                                                               4
028600150122Data non elaborabile per mancanza di  dati
028700150122Terminal errato                                                                3
028800150122Terminal ripetuto                                                              4
