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
001100070424     ftntbe01l  uf a e           k disk
001200070308
001300070605     d azuteds       e ds                  extname(azute00f)
001400070605     d ddatiute      e ds
001500070605     d kpjba         e ds
001600070605     d og146         e ds
001700070611     d og148         e ds
001800070605     d tibs34ds      e ds
001900070611     D Trul52ds      E DS
002000070611     d Wdata8          DS
002100070611     d  dadata                 1      8  0
002200070611     d  adata                  9     16  0
002300070611     d  GioLav                17     21  0
002400070605     D WLBDAT          DS                  INZ
002500070605     D  G02DAT                 1      8  0
002600070605     D  G02INV                 9     16  0
002700070605     D  G02ERR                17     17
002800070605     D  G02TGI                18     22  0
002900070605     d dtaiso                          d
003000070605     d dtaeur                          d   datfmt(*EUR)
003100070323     d command         s             80
003200070323     d lung            s             15  5 inz(80)
003300070605     d cmd             s             80    dim(4)  ctdata perrcd(1)
003400070426     d password        c                   const('GIANNI    ')
003500070528
003600070605     c                   clear                   w1cpsw
003700070611     C                   SETOFF                                       03
003800070605
003900070424     c                   do        *hival
004000070605     c                   exfmt     dg95d01
004100070424      * f3=fine
004200070605     c   kc              leave
004300070611     C*  Sblocco data
004400070611     C     *INKA         IFEQ      '1'
004500070611     C                   SETON                                        03
004600070611     C                   iter
004700070611     C                   END
004800070424      * controllo
004900070424     c                   exsr      sr_ctrd01
005000070424     c                   if        *in28
005100070424     c                   iter
005200070424     c                   endif
005300070424      * f6=conferma
005400070424     c                   if        *inkf
005500070605     c                   exsr      aggio
005600070424     c                   leave
005700070424     c                   endif
005800070424
005900070424     c                   enddo
006000070424
006100070605     c                   eval      *inlr = *on
006200070424
006300070424      *------------------------------------------------------------------------*
006400070424     c     sr_ctrd01     begsr
006500070605      *------------------------------------------------------------------------*
006600070605     c                   setoff                                       284041
006700070605     c                   clear                   w1cmsg
006800070605     c                   clear                   h1colo
006900070605     c                   clear                   h1riga
007000070424      * la filiale inserita deve essere una filiale valida
007100070424     c                   clear                   w1dfil
007200070605     c                   clear                   og146
007300070611     c                   clear                   og148
007400070424     c     w1cfil        chain(n)  azorg01l
007500070424     c                   if        not %found(azorg01l) or orgfva <> *blanks or
007600070424     c                             (orgfag <> 'A' and orgfag <> 'F')
007700070424     c                   eval      w1cmsg = 'Filiale errata'
007800070605     c                   seton                                        4028
007900070424     c                   leavesr
008000070424     c                   endif
008100070611      * controllo se filiale già partita con distinte automatiche
008200070502     c                   eval      w1dfil= orgdes
008300070605     c                   eval      og146 = orgde6
008400070611     c                   eval      og148 = orgde8
008500070605     c*
008600070605     c                   if        §ogdda <> ' '
008700070605     c                   move      §ogdda        comdda            8 0
008800070605     c                   else
008900070605     c                   clear                   comdda
009000070605     c                   end
009100070605     c                   if        comdda <> 0 and comdda <= dateu8
009200070611     c                   eval      w1cmsg = 'Filiale già attivata x distinte -
009300070611     c                             automatiche'
009400070605     c                   seton                                        4028
009500070605     c                   leavesr
009600070605     c                   endif
009700070611      * controllo se filiale già attivata x GEO
009800070611     c                   if        §ogtlls <>'S' or §oggeot <>' '
009900070611     c                   eval      w1cmsg = 'Filiale con parametri GEO non -
010000070611     c                             congruenti'
010100070611     c                   seton                                        4028
010200070611     c                   leavesr
010300070611     c                   endif
010400070605     C* Data obbligatoria
010500070605    1C                   IF        w1cdda = 0
010600070605     c                   if        comdda <> 0
010700070605     c                   movel     comdda        dtaiso
010800070605     c                   movel     dtaiso        dtaeur
010900070605     c                   movel     dtaeur        w1cdda
011000070605     c                   else
011100070605     c                   eval      w1cmsg = 'Data errata'
011200070605     C                   SETON                                        4128
011300070605     C                   leavesr
011400070605    1C                   ENDIF
011500070605    1C                   ENDIF
011600070627     C* La data può essere solo > di oggi e minore di oggi + 4 gg lavorativi
011700070605     C                   Z-ADD     w1cdda        G02DAT
011800070605     C                   MOVEL     *BLANK        G02ERR
011900070605     C                   CALL      'XSRDA8'
012000070605     C                   PARM                    WLBDAT
012100070605    1C     G02ERR        IFEQ      '1'
012200070605     c                   eval      w1cmsg = 'Data errata'
012300070605     C                   SETON                                        4128
012400070605     C                   leavesr
012500070605    1C                   ENDIF
012600070605     C                   Z-ADD     G02DAT        w1cdda
012700070611    1C                   IF        g02inv <= DATEU8
012800070605     c                   eval      w1cmsg = 'Data errata'
012900070605     C                   SETON                                        4128
013000070605     C                   leavesr
013100070605    1C                   ENDIF
013200070627      *controllo differenza fra oggi e data immessa. Max 4 gg lavorativi
013300070611     c                   move      dateu8        dadata
013400070611     c                   move      g02inv        adata
013500070611     c                   CALL      'XSRLAV8'
013600070611     c                   PARM                    Wdata8
013700070627     c* controllo 5 gg perchè considera anche oggi
013800131206    1C*                  IF        giolav > 5
013900131206     c*                  eval      w1cmsg = 'Data superiore a 4 gg lavorativi'
014000131206     C*                  SETON                                        4128
014100131206     C*                  leavesr
014200131206    1C*                  ENDIF
014300070502      * no utente EDP
014400070502     c                   if        not *in02
014500070424      * controllo la password
014600070424     c                   if        w1cpsw = *blanks
014700070424     c                   eval      w1cmsg = 'Immettere la password'
014800070424     c                   eval      *in28 = *on
014900070502     c                   eval      h1riga = 11
015000070525     c                   eval      h1colo = 11
015100070424     c                   leavesr
015200070424     c                   endif
015300070424      * controllo se esatta
015400070424     c                   if        w1cpsw <> password
015500070424     c                   clear                   w1cpsw
015600070424     c                   eval      w1cmsg = 'Password errata'
015700070424     c                   eval      *in28 = *on
015800070502     c                   eval      h1riga = 11
015900070525     c                   eval      h1colo = 11
016000070424     c                   leavesr
016100070424     c                   endif
016200070502     c                   endif
016300070424
016400070424     c                   endsr
016500070424      *------------------------------------------------------------------------*
016600070508      *  imposta flag in organigramma
016700070424      *------------------------------------------------------------------------*
016800070605     c     aggio         begsr
016900070424
017000070424      * chiudo il file aperto
017100070424     c                   if        %open(azorg01l)
017200070424     c                   close     azorg01l
017300070424     c                   endif
017400070424
017500070424      * ovrdbf azorg01l di filiale
017600070424     c                   clear                   command
017700070605     c  n01              movea     cmd(1)        command
017800070605     c   01              movea     cmd(3)        command
017900070424     c                   call      'QCMDEXC'
018000070424     c                   parm                    command
018100070424     c                   parm                    lung
018200070424     c                   open      azorg01l
018300070424
018400070424      * imposto subito il flag in organigramma di filiale
018500070605     c                   clear                   og146
018600070508     c     w1cfil        chain     azorg01l
018700070424     c                   if        %found(azorg01l)
018800070605     c                   eval      og146 = orgde6
018900070605     c                   movel     g02inv        §ogdda
019000070605     c                   eval      orgde6 = og146
019100070424     c                   update    azorg
019200070424     c                   endif
019300070424
019400070424      * chiudo il file aperto
019500070424     c                   if        %open(azorg01l)
019600070424     c                   close     azorg01l
019700070424     c                   endif
019800070424
019900070424      * ovrdbf azorg01l di sede
020000070424     c                   clear                   command
020100070605     c  n01              movea     cmd(2)        command
020200070605     c   01              movea     cmd(4)        command
020300070424     c                   call      'QCMDEXC'
020400070424     c                   parm                    command
020500070424     c                   parm                    lung
020600070424     c                   open      azorg01l
020700070424
020800070424      * imposto subito il flag in organigramma di filiale
020900070605     c                   clear                   og146
021000070508     c     w1cfil        chain     azorg01l
021100070424     c                   if        %found(azorg01l)
021200070605     c                   eval      og146 = orgde6
021300070605     c                   movel     g02inv        §ogdda
021400070605     c                   eval      orgde6 = og146
021500070424     c                   update    azorg
021600070424     c                   endif
021700070605     c* lancio pgm per aggiornare archivi FIARG/FIDST/FIAPD
021800070605     c                   movel(p)  w1cfil        kpjbu
021900070605     c                   call      'FIDG99C'
022000070605     c                   parm                    kpjba
022100070424
022200070424     c                   endsr
022300070308
022400070308      *------------------------------------------------------------------------*
022500070308      * routine iniziale
022600070308      *------------------------------------------------------------------------*
022700070308     c     *inzsr        begsr
022800070308
022900070308     c     *entry        plist
023000070424     c                   parm                    kpjba
023100070424
023200070424      * ambiente di prova
023300070424     c                   eval      *in01 = (%subst(knsif:7:1) = 'P')
023400070502
023500070502      * utente EDP
023600070502     c                   eval      *in02 = (%subst(knmus:1:3) = 'EDP')
023700070424
023800070424     c     *dtaara       define    §azute        azuteds
023900070424     c     *dtaara       define    §datiute      ddatiute
024000070424     c                   in(e)     *dtaara
024100070424     c                   if        %error or rsut = *blanks
024200070424     c                   clear                   tibs34ds
024300070424     c                   call      'TIBS34R'
024400070424     c                   parm                    tibs34ds
024500070424     c                   in        *dtaara
024600070424     c                   endif
024700070528
024800070528      * imposto la filiale utente a video
024900070528     c                   eval      w1cfil = dutpou
025000070528     c                   eval      w1dfil = dutdpo
025100070308
025200070424      * ovrdbf azorg01l di filiale
025300070424     c                   clear                   command
025400070605     c  n01              movea     cmd(1)        command
025500070605     c   01              movea     cmd(3)        command
025600070424     c                   call      'QCMDEXC'
025700070424     c                   parm                    command
025800070424     c                   parm                    lung
025900070424     c                   open      azorg01l
026000070424
026100070605     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
026200070605     C***
026300070605     C                   TIME                    W0140            14 0
026400070605     C                   MOVE      W0140         UDATE8            8 0
026500070605     C                   MOVEL     W0140         HHMM              4 0          *ORA/MINUTI
026600070605     C*
026700070605     C                   Z-ADD     UDATE8        G02DAT                         *GG/MM/AAAA
026800070605     C                   MOVEL     *BLANK        G02ERR
026900070605     C                   CALL      'XSRDA8'
027000070605     C                   PARM                    WLBDAT
027100070605     C                   Z-ADD     G02INV        DATEU8            8 0          *AAAA/MM/GG
027200070611     c* aggiungo 3 gg lavorativi alla data di oggi
027300070611     c                   clear                   trul52ds
027400070611     C                   Z-ADD     3             D52GIO
027500070611     C                   MOVE      g02inv        D52DIN
027600070611     C                   CALL      'TRUL52R'
027700070611     C                   PARM                    TRUL52DS
027800070611     C                   MOVE      D52DFI        dtaiso
027900070611     c                   move      dtaiso        dtaeur
028000070611     c                   move      dtaeur        w1cdda
028100070308     c                   endsr
028200070323
028300070323** CMD   (Lunga 80)                                                            *
028400070424OVRDBF FILE(AZORG01L) TOFILE(FILTRAGRU/AZORG01L)                                 2
028500070424OVRDBF FILE(AZORG01L) TOFILE(GAITRAGRU/AZORG01L)                                 3
028600070424OVRDBF FILE(AZORG01L) TOFILE(FILTRAGRPF/AZORG01L)                                4
028700070424OVRDBF FILE(AZORG01L) TOFILE(GAITRAGRPS/AZORG01L)                                5
