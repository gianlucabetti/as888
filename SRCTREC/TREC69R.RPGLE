000100130920     H DEBUG DECEDIT('0,') DATEDIT(*DMY/) dftactgrp(*no) actgrp(*new)
000200080912      *
000300130913     Ftrec69d   CF   E             WORKSTN
000400130515     f                                     sfile(video2:snrr1)
000500130913     Fecnot01l  uF a E           k disk
000600130913     Fecevt01l  iF   E           k disk
000700130913     Fecect02l  iF   E           k disk
001100080912      *
001200080912     D Psds           SDS
001300080912     D  PgmName          *PROC
001400080912     D Kpjba         E DS
001500130919     d trec69ds      e ds
002300080912      *
002301130916     D trec60ds      E DS
002400081216     D tibs02ds      E DS
002500080912     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
002600080912     D CNCR80        E DS
002700080912     d Tibs36Ds      e ds
003300080912      *
003400080912     D fatto           S              1
003500080912     D modalita        S              1
003600080912     D x               S              4  0
003700080912     D WrkEofS01       S              1
003800080912     D WrkCarS01       S              1
003900080912     D $VIDEO          S             10
004000080912     D snrr1           S              5i 0
004100080912     D Savkpjbu        S                   like(kpjbu)
004300080912     d dataiso         s               d   datfmt(*iso)
004400080912     d dataeur         s               d   datfmt(*eur)
004500080912     D WLBDA8          DS
004600080912     D  G02DAT                 1      8  0
004700080912     D  G02INV                 9     16  0
004800080912     D  G02ERR                17     17
004900080912     D  G02TGI                18     22  0
004901130913      *
005700081028     c                   if        richiamato = *blank
006000080912      *---------------- Gestione formato video video01----------------------
006100080912     C                   EXSR      Inzvideo1
006200080912     C                   DOU       $Video <> 'VIDEO1'
006300080912      *----------- Visualizzo dati di output in caso di errori.-------------
006400080912     C                   IF        *IN99
006500080912     C                   EVAL      *IN99 = *OFF
006600080912     C                   WRITE     video1
006700080912     C                   EVAL      *IN99 = *ON
006800080912     C                   ENDIF
006900080912      *
007000080912     c     emetti        tag
007001130913     c                   setoff                                       8499
007100080912     C                   EXFMT     video1
007200080912      *
007300080912     C                   SELECT
007400080912     C                   WHEN      *INKC
007500080912     C                             OR
007600080912     C                             *INKL
007700080912     C                   EVAL      $Video = *BLANKS
007800130916      *ricerca voci
007900130916     C                   WHEN      *INKg
007901130916     c                   clear                   trec60ds
007902130916     c                   movel     trec60ds      kpjbu
008000130916     C                   call      'TREC60R'
008001130916     c                   parm                    kpjba
008002130916     c                   goto      emetti
008003130916      *ricerca competenze
008100130916     C                   WHEN      *INKh
008101130916     c                   clear                   trec60ds
008102130916     c                   movel     trec60ds      kpjbu
008103130916     C                   call      'TREC72R'
008104130916     c                   parm                    kpjba
008105130916     c                   goto      emetti
008300080912     C                   OTHER
008400080912     C                   EXSR      Chkvideo1
008500130913     c   84              goto      emetti
008600080912     C  N99              EXSR      Wrkvideo2c
008700080912     C                   ENDSL
008800080912      *
008900080912     C                   ENDDO
009000081028      *richiamato da altri
009100081028     c                   else
009500081028     C                   EXSR      Wrkvideo2c
009600081028     c                   endif
009700080912      *
009800080912     C                   EXSR      Uscita
009900080912     c**********************************************************************
010000080912     c* uscita
010100080912     c**********************************************************************
010200080912     C     Uscita        BEGSR
010300080912      *
010800130422     c
010900080912     C                   EVAL      *INLR = *ON
011000080912     C                   RETURN
011100080912      *
011200080912     C                   ENDSR
011300080912     c**********************************************************************
011400080912     c* gestione sfl
011500080912     c**********************************************************************
011600080912     C     Wrkvideo2c    BEGSR
011700080912      *
011800080912     C                   EVAL      WrkCarS01 = *ON
011900080912     C                   EVAL      $Video = 'VIDEO2C'
012000080912      *
012100130515     c                   if        *in25
012200130515     C                   EXSR      Carvideo2
012300130515     c                   endif
012400130515     c
012500080912     C                   DOU       $Video <> 'VIDEO2C'
012600130515     c* in caso di scelta, una volta carico non ricarico
012700130515     c                   if        not *in25
012800080912     C                   EXSR      Carvideo2
012900130515     c                   endif
013000130515     c                   if        nrr1=0
013100130515     c                   z-add     1             nrr1
013200130515     c                   endif
013300130515     c
013400080912     c*Descrizione della ricerca
013500080912      *
013600080912     C                   WRITE     video2z
013700080912     C                   EXFMT     video2c
013800080912     C                   SELECT
013900080912     c* fine
014000080912     C                   WHEN      *INKC=*ON
014100080912     C                   EVAL      $VIDEO=*BLANKS
014200080912     c* guida
014300080912     C                   WHEN      *INKL=*ON
014400080912     C                   EVAL      $VIDEO='VIDEO1'
014500080912     c* ripristino
014600080912     C                   WHEN      *INKE=*ON
014700080912     C                   EVAL      WrkCarS01 = *ON
014800080912
014900080912     C                   OTHER
015000080912
015100080912     C                   EXSR      GestioneSFL
015200080912     C                   ENDSL
015300080912
015400080912     C                   ENDDO
015500080912     C                   ENDSR
015600080912     c**********************************************************************
015700080912     c* Controlli video R01.
015800080912     c**********************************************************************
015900080912     C     Chkvideo1     BEGSR
015901130913     c                   setoff                                       28
015902130913     c                   clear                   $msg
016300080912      *
016301130913     c                   if        vnotvoc = 0
016302130913     c                   seton                                        288499
016303130913     c                   eval      $msg = 'Codice voce o compe-
016304130913     c                             tenza obbligatorio'
016305130913     c                   leavesr
016306130913     c                   endif
016307130913     c                   if        vnottpana = 'V'
016308130919     c                   if        vnotprg <> 0
016309130913     c     kvoci         chain     ecevt01l
016310130913     c                   if        %found(ecevt01l)
016311130913     c                   move      evtvod        vdes
016312130913     c                   else
016313130913     c                   seton                                        288499
016314130913     c                   eval      $msg = 'Voce richiesta non -
016315130913     c                             presente in anagrafica'
016316130913     c                   leavesr
016318130913     c                   endif
016319130919     c                   else
016320130919     c     vnotvoc       setgt     ecevt01l
016321130919     c     vnotvoc       readpe    ecevt01l
016322130919     c                   if        %eof(ecevt01l)
016325130919     c                   seton                                        288499
016326130919     c                   eval      $msg = 'Voce richiesta non -
016327130919     c                             presente in anagrafica'
016328130919     c                   leavesr
016329130919     c                   else
016330130919     c                   move      evtvod        vdes
016331130919     c                   move      evtprg        vnotprg
016332130919     c                   endif
016333130919     c                   endif
016334130913     c                   else
016335130916     c                   z-add     vnotprg       wbloc
016336130916     c     kcomp         chain     ecect02l
016337130913     c                   if        %found(ecect02l)
016338130913     c                   move      ectcmd        vdes
016339130916     c                   else
016340130913     c                   seton                                        288499
016341130913     c                   eval      $msg = 'Competenza richiesta non -
016342130913     c                             presente in anagrafica'
016343130913     c                   leavesr
016344130913     c                   endif
016345130913     c                   endif
016400080912     C                   ENDSR
016500080912     c**********************************************************************
016600080912     c* inizializza r01
016700080912     c**********************************************************************
016800080912     C     Inzvideo1     BEGSR
016801130913     c                   if        richiamato = *blank
017001130913     c                   move      'V'           vnottpana
017002130913     c                   endif
017800080912     C                   ENDSR
045100080912     c**********************************************************************
045200080912     c* carica sfl
045300080912     c**********************************************************************
045400080912     C     Carvideo2     BEGSR
045500080912     C                   IF        WrkCarS01 = *ON
045600080912     C                   EVAL      snrr1   = 0
045700080912     C                   EVAL      *IN90 = *ON
045800080912     C                   EVAL      *IN91 = *OFF
045900080912     C                   WRITE     video2c
046000080912     C                   EVAL      *IN90 = *OFF
046300130913     c     knote         setll     ecnot01l
047000081029     c                   do        *hival
047300130913     c     knote         reade     ecnot01l
047301130913     c                   if        %eof(ecnot01l)
048000130913     c                   leave
048001130913     c                   endif
050700080912      *esco per raggiunta massima capacità sfl
050800080912     c                   if        snrr1 > 9990
050900080912     c                   leave
051000080912     c                   endif
051100080912
051200080912     C                   EVAL      snrr1 = snrr1 + 1
051201130913     c                   movel     notnob        Vnotnob
051202130913     c                   z-add     snrr1         vnotpno
051300080912     C                   WRITE     video2
051400080912     C                   ENDDO
051500080912
051501130913     c
051600080912     C                   IF        snrr1  > 0
051700080912     C                   EVAL      *IN91 = *ON
051800080912     C                   MOVE      1             OK                1 0
051801130913      * inserisce righe vuote fino a fine pagina
051802130916     c     snrr1         do        89
051808130913     C                   EVAL      snrr1 = snrr1 + 1
051809130913     c                   clear                   Vnotnob
051810130913     c                   z-add     snrr1         vnotpno
051811130913     C                   WRITE     video2
051813130913     c                   enddo
051900080912     C                   ELSE
051901130916     c                   do        90
051902130913     C                   EVAL      snrr1 = snrr1 + 1
051903130913     c                   clear                   Vnotnob
051904130913     c                   z-add     snrr1         vnotpno
051905130913     C                   WRITE     video2
051906130913     c                   enddo
052000130913     C                   MOVE      1             OK                1 0
052001130916     C                   EVAL      *IN91 = *ON
052100080912     C                   ENDIF
052200080912     C                   EVAL      nrr1  = 1
052300080912     C                   ENDIF
052400130916     C                   EVAL      WrkCarS01 = *Off
052500080912     C                   EVAL      snrr1  = 1
052600080912     C                   EVAL      csrrrn = 1
052800080912     C                   ENDSR
052900080912     c**********************************************************************
053000080912     c* gestione sfl
053100080912     c**********************************************************************
053200080912     C     GestioneSFL   BEGSR
053300080912      *
053400080912     C                   EVAL      WrkEofS01 = *OFF
055400080912      * Elaborazione opzioni.
055500080912     c*
055501130913     c                   if        *inkf
055600130515     c                   clear                   nrr1
055700080912     c                   do        *hival        X                 4 0
055800080912     c     X             CHAIN     video2                             50
055900080912     C   50              LEAVE
055901130916     c                   z-add     x             wriga
056001130916     c     knote2        chain     ecnot01l
056002130913     c                   if        %found(ecnot01l)
056003130916     c                   if        vnotnob <> *blank
056004130913     c                   movel     vnotpno       notpno
056005130916     c                   movel     vnotnob       notnob
056006130913     c                   update    ecnot000
056007130916     c                   else
056008130916     c                   delete    ecnot000
056009130916     c                   endif
056010130913     c                   else
056011130916     c                   if        vnotnob <> *blank
056012130913     c                   move      vNOTTPANA     NOTTPANA
056013130913     c                   z-add     vNOTVOC       NOTVOC
056014130913     c                   z-add     vNOTPRG       NOTPRG
056015130913     c                   z-add     vnotpno       notpno
056016130913     c                   movel     vnotnob       notnob
056101130913     c                   write     ecnot000
056102130916     C                   endif
058201130913     C                   endif
058300080912     C                   enddo
058301130913     C                   endif
059000130913     C                   ENDSR
059100080912     c**********************************************************************
059200080912     C     *INZSR        BEGSR
059300080912     c**********************************************************************
059400080912      *
059500080912     C     *ENTRY        PLIST
059600080912     C                   PARM                    Kpjba
059601130919     c                   clear                   trec69ds
059700081028      *richiamato da altri
060100081028     c                   if        kpjbu <> *blank
060101130913     c                   seton                                        25
060300130919     c                   movel     kpjbu         trec69ds
060400130919     C                   z-add     d69voc        vnotvoc
060500130919     C                   z-add     d69prg        vnotprg
060501130919     C                   move      d69tpana      vnottpana
060600081028     c                   movel     'X'           richiamato        1
060601130913     c                   if        vnottpana = 'V'
060700130913     c     kvoci         chain     ecevt01l
060800130913     c                   if        %found(ecevt01l)
061000130913     c                   move      evtvod        vdes
061100100104     c                   endif
061101130913     c                   else
061102130913     c     vnotvoc       chain     ecect02l
061103130913     c                   if        %found(ecect02l)
061104130913     c                   move      ectcmd        vdes
061105130913     c                   endif
061200100104     c                   endif
063200130422     c                   endif
063300130422     c
063400080912     C                   Z-ADD     1             CODUT
063500080912     C                   CALL      'X§PARUT'
063600080912     C                   PARM                    UTEDSE
063700080912     C                   MOVEL     REC80         CNCR80
063800080912     C                   MOVEL     Ragut         rsut
063900080912     C                   MOVEL     knsif         vknsif
064000080912     C                   MOVEL     knmus         vknmus
064100080912      *reperimento data
064200080912     C                   TIME                    W0120            14 0
064300080912     C                   MOVE      W0120         WDAT              8 0
064400080912     C*
064500080912     C                   Z-ADD     WDAT          G02DAT
064600080912     C                   MOVEL     *BLANK        G02ERR
064700080912     C                   CALL      'XSRDA8'
064800080912     C                   PARM                    WLBDA8
064900080912     C* UDATE A 8 IN AAAA/MM/GG
065000080912     C                   Z-ADD     G02INV        WUDA              8 0
065100080912      *profilo di filiale attivo indicatore di protezione campi non manutenziona
065200080912     C                   CLEAR                   Tibs36Ds
065300080912     C                   EVAL      I36ute = knmus
065400080912     C                   CALL      'TIBS36R'
065500080912     C                   PARM                    tibs36ds
065600081112      * profilo di filiale
065700130919     c                   if        %subst(knmus:1:3) <> 'EDP'
065800081112     c                   seton                                        30
065900081112     c                   endif
065901130913      *
068204130913     C     Kvoci         KLIST
068206130913     C                   KFLD                    vnotvoc
068207130913     C                   KFLD                    vnotprg
068208130916     C     Kcomp         KLIST
068209130916     C                   KFLD                    vnotvoc
068210130916     C                   KFLD                    wbloc             5 0
068211130913     C     Knote         KLIST
068212130913     C                   KFLD                    vNOTTPANA
068213130913     C                   KFLD                    wNOTTIP           1
068214130913     C                   KFLD                    vNOTVOC
068215130913     C                   KFLD                    vNOTPRG
068216130913     C                   clear                   wNOTTIP
068217130916     C     Knote2        KLIST
068218130916     C                   KFLD                    vNOTTPANA
068219130916     C                   KFLD                    wNOTTIP           1
068220130916     C                   KFLD                    vNOTVOC
068221130916     C                   KFLD                    vNOTPRG
068222130916     C                   kfld                    wriga             3 0
068300080912      *---------------------------------------------------------------------
068400080912     C                   ENDSR
