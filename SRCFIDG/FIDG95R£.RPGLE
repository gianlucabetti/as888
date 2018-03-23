000100070322     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200031017
000300031017      *---------------------------------------------------------------*
000400031017      *                                                               *
000500070605      *     Data inizio Distinta Consegna Automatica                  *
000600031017      *                                                               *
000700031017      *---------------------------------------------------------------*
000800031017
000900070605     ffidg95d   cf   e             workstn
001000070424     fazorg01l  uf   e           k disk    usropn
001700070424     ftntbe01l  uf a e           k disk
002600070308
002601070605     d azuteds       e ds                  extname(azute00f)
002602070605     d ddatiute      e ds
002603070605     d kpjba         e ds
002604070605     d og146         e ds
002605070611     d og148         e ds
002606070605     d tibs34ds      e ds
002607070611     D Trul52ds      E DS
002608070611     d Wdata8          DS
002609070611     d  dadata                 1      8  0
002610070611     d  adata                  9     16  0
002611070611     d  GioLav                17     21  0
004702070605     D WLBDAT          DS                  INZ
004703070605     D  G02DAT                 1      8  0
004704070605     D  G02INV                 9     16  0
004705070605     D  G02ERR                17     17
004706070605     D  G02TGI                18     22  0
004707070605     d dtaiso                          d
004708070605     d dtaeur                          d   datfmt(*EUR)
004800070323     d command         s             80
005400070323     d lung            s             15  5 inz(80)
007800070605     d cmd             s             80    dim(4)  ctdata perrcd(1)
011300070426     d password        c                   const('GIANNI    ')
011600070528
011601070605     c                   clear                   w1cpsw
011602070611     C                   SETOFF                                       03
011603070605
018900070424     c                   do        *hival
019000070605     c                   exfmt     dg95d01
019600070424      * f3=fine
019700070605     c   kc              leave
019701070611     C*  Sblocco data
019702070611     C     *INKA         IFEQ      '1'
019703070611     C                   SETON                                        03
019704070611     C                   iter
019705070611     C                   END
020100070424      * controllo
020200070424     c                   exsr      sr_ctrd01
020400070424     c                   if        *in28
020500070424     c                   iter
020600070424     c                   endif
020800070424      * f6=conferma
020900070424     c                   if        *inkf
020901070605     c                   exsr      aggio
021000070424     c                   leave
021100070424     c                   endif
021200070424
021300070424     c                   enddo
021400070424
021401070605     c                   eval      *inlr = *on
023800070424
024100070424      *------------------------------------------------------------------------*
024200070424     c     sr_ctrd01     begsr
024201070605      *------------------------------------------------------------------------*
024202070605     c                   setoff                                       284041
024203070605     c                   clear                   w1cmsg
024204070605     c                   clear                   h1colo
024205070605     c                   clear                   h1riga
024400070424      * la filiale inserita deve essere una filiale valida
024500070424     c                   clear                   w1dfil
024600070605     c                   clear                   og146
024601070611     c                   clear                   og148
024700070424     c     w1cfil        chain(n)  azorg01l
024800070424     c                   if        not %found(azorg01l) or orgfva <> *blanks or
024900070424     c                             (orgfag <> 'A' and orgfag <> 'F')
025000070424     c                   eval      w1cmsg = 'Filiale errata'
025200070605     c                   seton                                        4028
025300070424     c                   leavesr
025400070424     c                   endif
025501070611      * controllo se filiale già partita con distinte automatiche
025600070502     c                   eval      w1dfil= orgdes
025700070605     c                   eval      og146 = orgde6
025701070611     c                   eval      og148 = orgde8
025702070605     c*
025703070605     c                   if        §ogdda <> ' '
025704070605     c                   move      §ogdda        comdda            8 0
025705070605     c                   else
025706070605     c                   clear                   comdda
025707070605     c                   end
025708070605     c                   if        comdda <> 0 and comdda <= dateu8
025711070611     c                   eval      w1cmsg = 'Filiale già attivata x distinte -
025712070611     c                             automatiche'
025713070605     c                   seton                                        4028
025715070605     c                   leavesr
025716070605     c                   endif
025717070611      * controllo se filiale già attivata x GEO
025718070611     c                   if        §ogtlls <>'S' or §oggeot <>' '
025720070611     c                   eval      w1cmsg = 'Filiale con parametri GEO non -
025721070611     c                             congruenti'
025722070611     c                   seton                                        4028
025723070611     c                   leavesr
025724070611     c                   endif
025725070605     C* Data obbligatoria
025726070605    1C                   IF        w1cdda = 0
025727070605     c                   if        comdda <> 0
025728070605     c                   movel     comdda        dtaiso
025729070605     c                   movel     dtaiso        dtaeur
025730070605     c                   movel     dtaeur        w1cdda
025731070605     c                   else
025732070605     c                   eval      w1cmsg = 'Data errata'
025733070605     C                   SETON                                        4128
025734070605     C                   leavesr
025735070605    1C                   ENDIF
025736070605    1C                   ENDIF
025737070627     C* La data può essere solo > di oggi e minore di oggi + 4 gg lavorativi
025738070605     C                   Z-ADD     w1cdda        G02DAT
025739070605     C                   MOVEL     *BLANK        G02ERR
025740070605     C                   CALL      'XSRDA8'
025741070605     C                   PARM                    WLBDAT
025742070605    1C     G02ERR        IFEQ      '1'
025743070605     c                   eval      w1cmsg = 'Data errata'
025744070605     C                   SETON                                        4128
025745070605     C                   leavesr
025746070605    1C                   ENDIF
025747070605     C                   Z-ADD     G02DAT        w1cdda
025748070611    1C                   IF        g02inv <= DATEU8
025749070605     c                   eval      w1cmsg = 'Data errata'
025750070605     C                   SETON                                        4128
025751070605     C                   leavesr
025752070605    1C                   ENDIF
025753070627      *controllo differenza fra oggi e data immessa. Max 4 gg lavorativi
025754070611     c                   move      dateu8        dadata
025755070611     c                   move      g02inv        adata
025756070611     c                   CALL      'XSRLAV8'
025757070611     c                   PARM                    Wdata8
025758070627     c* controllo 5 gg perchè considera anche oggi
025759070627    1C                   IF        giolav > 5
025760070627     c                   eval      w1cmsg = 'Data superiore a 4 gg lavorativi'
025761070611     C                   SETON                                        4128
025762070611     C                   leavesr
025763070611    1C                   ENDIF
040300070502      * no utente EDP
040400070502     c                   if        not *in02
040500070424      * controllo la password
040600070424     c                   if        w1cpsw = *blanks
040700070424     c                   eval      w1cmsg = 'Immettere la password'
040800070424     c                   eval      *in28 = *on
040900070502     c                   eval      h1riga = 11
041000070525     c                   eval      h1colo = 11
041100070424     c                   leavesr
041200070424     c                   endif
041300070424      * controllo se esatta
041400070424     c                   if        w1cpsw <> password
041500070424     c                   clear                   w1cpsw
041600070424     c                   eval      w1cmsg = 'Password errata'
041700070424     c                   eval      *in28 = *on
041800070502     c                   eval      h1riga = 11
041900070525     c                   eval      h1colo = 11
042000070424     c                   leavesr
042100070424     c                   endif
042200070502     c                   endif
042300070424
042400070424     c                   endsr
055200070424      *------------------------------------------------------------------------*
055300070508      *  imposta flag in organigramma
055400070424      *------------------------------------------------------------------------*
055500070605     c     aggio         begsr
055600070424
055700070424      * chiudo il file aperto
055800070424     c                   if        %open(azorg01l)
055900070424     c                   close     azorg01l
056000070424     c                   endif
056100070424
056200070424      * ovrdbf azorg01l di filiale
056300070424     c                   clear                   command
056400070605     c  n01              movea     cmd(1)        command
056500070605     c   01              movea     cmd(3)        command
056600070424     c                   call      'QCMDEXC'
056700070424     c                   parm                    command
056800070424     c                   parm                    lung
056900070424     c                   open      azorg01l
057000070424
057100070424      * imposto subito il flag in organigramma di filiale
057200070605     c                   clear                   og146
057300070508     c     w1cfil        chain     azorg01l
057400070424     c                   if        %found(azorg01l)
057500070605     c                   eval      og146 = orgde6
057501070605     c                   movel     g02inv        §ogdda
058800070605     c                   eval      orgde6 = og146
058900070424     c                   update    azorg
059000070424     c                   endif
059100070424
059200070424      * chiudo il file aperto
059300070424     c                   if        %open(azorg01l)
059400070424     c                   close     azorg01l
059500070424     c                   endif
059600070424
059700070424      * ovrdbf azorg01l di sede
059800070424     c                   clear                   command
059900070605     c  n01              movea     cmd(2)        command
060000070605     c   01              movea     cmd(4)        command
060100070424     c                   call      'QCMDEXC'
060200070424     c                   parm                    command
060300070424     c                   parm                    lung
060400070424     c                   open      azorg01l
060500070424
060600070424      * imposto subito il flag in organigramma di filiale
060700070605     c                   clear                   og146
060800070508     c     w1cfil        chain     azorg01l
060900070424     c                   if        %found(azorg01l)
061000070605     c                   eval      og146 = orgde6
061002070605     c                   movel     g02inv        §ogdda
062300070605     c                   eval      orgde6 = og146
062400070424     c                   update    azorg
062500070424     c                   endif
062501070605     c* lancio pgm per aggiornare archivi FIARG/FIDST/FIAPD
062502070605     c                   movel(p)  w1cfil        kpjbu
062503070605     c                   call      'FIDG99C'
062504070605     c                   parm                    kpjba
062600070424
062700070424     c                   endsr
126400070308
126500070308      *------------------------------------------------------------------------*
126600070308      * routine iniziale
126700070308      *------------------------------------------------------------------------*
126800070308     c     *inzsr        begsr
126900070308
127000070308     c     *entry        plist
127100070424     c                   parm                    kpjba
127200070424
127300070424      * ambiente di prova
127400070424     c                   eval      *in01 = (%subst(knsif:7:1) = 'P')
127500070502
127600070502      * utente EDP
127700070502     c                   eval      *in02 = (%subst(knmus:1:3) = 'EDP')
127800070424
127900070424     c     *dtaara       define    §azute        azuteds
128000070424     c     *dtaara       define    §datiute      ddatiute
128100070424     c                   in(e)     *dtaara
128200070424     c                   if        %error or rsut = *blanks
128300070424     c                   clear                   tibs34ds
128400070424     c                   call      'TIBS34R'
128500070424     c                   parm                    tibs34ds
128600070424     c                   in        *dtaara
128700070424     c                   endif
128800070528
128900070528      * imposto la filiale utente a video
129000070528     c                   eval      w1cfil = dutpou
129100070528     c                   eval      w1dfil = dutdpo
133400070308
133500070424      * ovrdbf azorg01l di filiale
133600070424     c                   clear                   command
133700070605     c  n01              movea     cmd(1)        command
133800070605     c   01              movea     cmd(3)        command
133900070424     c                   call      'QCMDEXC'
134000070424     c                   parm                    command
134100070424     c                   parm                    lung
134200070424     c                   open      azorg01l
134300070424
134301070605     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
134302070605     C***
134303070605     C                   TIME                    W0140            14 0
134304070605     C                   MOVE      W0140         UDATE8            8 0
134305070605     C                   MOVEL     W0140         HHMM              4 0          *ORA/MINUTI
134306070605     C*
134307070605     C                   Z-ADD     UDATE8        G02DAT                         *GG/MM/AAAA
134309070605     C                   MOVEL     *BLANK        G02ERR
134310070605     C                   CALL      'XSRDA8'
134311070605     C                   PARM                    WLBDAT
134312070605     C                   Z-ADD     G02INV        DATEU8            8 0          *AAAA/MM/GG
134317070611     c* aggiungo 3 gg lavorativi alla data di oggi
134318070611     c                   clear                   trul52ds
134322070611     C                   Z-ADD     3             D52GIO
134327070611     C                   MOVE      g02inv        D52DIN
134329070611     C                   CALL      'TRUL52R'
134330070611     C                   PARM                    TRUL52DS
134331070611     C                   MOVE      D52DFI        dtaiso
134332070611     c                   move      dtaiso        dtaeur
134333070611     c                   move      dtaeur        w1cdda
134400070308     c                   endsr
134500070323
136300070323** CMD   (Lunga 80)                                                            *
136500070424OVRDBF FILE(AZORG01L) TOFILE(FILTRAGRU/AZORG01L)                                 2
136600070424OVRDBF FILE(AZORG01L) TOFILE(GAITRAGRU/AZORG01L)                                 3
136700070424OVRDBF FILE(AZORG01L) TOFILE(FILTRAGRPF/AZORG01L)                                4
136800070424OVRDBF FILE(AZORG01L) TOFILE(GAITRAGRPS/AZORG01L)                                5
