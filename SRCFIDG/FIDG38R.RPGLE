000100070913     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200070913      *
000300070913     Ffidg38d   CF   E             workstn
000400070913     f                                     sfile(video2:snrr1)
000500071025     Ffnarb05l  if   e           k disk    rename(fnarb000:fnarb5)
000600070917     Ffnarb01l  if   E           k disk
000700070918     Ffidst02l  if   E           k disk
000800070913     fazorg01l  if   e           k disk
000900150720     Ffiarg01l  if   E           k disk
001000070913     Ftabel00f  if   e           k disk
001100070913      *
001200070913     D Psds           SDS
001300070913     D  PgmName          *PROC
001400070913     D Kpjba         E DS
001500070913     d kpjbus          s                   like(kpjbu)
001600070913     d §kpjbu          s                   like(kpjbu)
001700070913      *
001800070917     D OG146         E DS                  INZ
001900070917     D OG147         E DS                  INZ
002000070917     D og150         E DS
002100070917     D fnarbds       e ds                  extname(fnarb00f)
002200070918     D fiargds       e ds                  extname(fiarg00f)
002300070913     D fnlr48ds      E DS
002400070917     D fidg16ds      E DS
002500070913     d DSARBD        e ds
002600070913     d DSARBK        e ds
002700070913     d DARBT         e ds
002800070913     d DSARBG        e ds
002900070913     d trul90ds      e ds
003000070913     D trul06ds      E DS
003100070913     D  lin                    1     90  0 dim(30)
003200070913     D* DATI X RICHIAMO PGM INTERR. BOLLE ARRIVI
003300070913     D PARAM1          DS                  INZ
003400070913     D  PA1AAS                14     17  0
003500070913     D  PA1LNP                18     20  0
003600070913     D  PA1NRS                21     22  0
003700070913     D  PA1NSP                23     29  0
003800070913     D  PA1F03                30     30
003900070913     D  PA1FLG                31     31
004000070913     D* SE =1 NON RICH. FNLG30R IN MODO RICORSIVO DA INTERR.BOLLE
004100070913     D  PA1GIA               144    144
004200070913     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
004300070913     D CNCR80        E DS
004400070913     d Tibs36Ds      e ds
004500070913
004600070913     d data            ds            10
004700070913     d aa                      3      4
004800070913     d mm                      6      7
004900070913     d gg                      9     10
005000070913      *
005100070913     D fatto           S              1
005200070913     D riemetti        S              1
005300070913     D multi           S              1
005400070913     D modalita        S              1
005500070913     D abb             S              1
005600070913     D WrkEofS01       S              1
005700070913     D WrkCarS01       S              1
005800070913     D $VIDEO          S             10
005900070913     D snrr1           S              5i 0
006000070913     D lasnrr1         S              5i 0
006100070913     D SavOpzione      S                   like(v1csce)
006200070913     d dataiso         s               d   datfmt(*iso)
006300070913     d dataeur         s               d   datfmt(*eur)
006400070913     D WLBDA8          DS
006500070913     D  G02DAT                 1      8  0
006600070913     D  G02INV                 9     16  0
006700070913     D  G02ERR                17     17
006800070913     D  G02TGI                18     22  0
006900070913     d  fil£6          s              3  0 dim(30)
007000070913     d  fil£1          s              3  0 dim(30)
007100070918     D gio             s             10    dim(7) ctdata perrcd(1)              Giorni Settim.
007200070913      *---------------------------------------------------------------------
007300070913      * specifiche per ordinamento forzato sfl
007400070913     d NotUsed         S             16A
007500070913     d ReturnSize      S              9B 0
007600070913     d SizeList        S              9B 0
007700070913     d WrkSort         S              1
007800070913     d*---
007900070913     d MaxKey          C                   2
008000070913     d Ascendente      C                   1
008100070913     d Discendente     C                   2
008200070913     d Carattere       C                   6
008300070913     d Put             C                   1
008400070913     d EndPut          C                   2
008500070913     d Get             C                   3
008600070913     d Numerico        C                   8
008700070913     d*---
008800070913     d SflRcd          DS
008900070913     d  vlodind
009000070913     d  vlod
009100070913     d  vprov
009200070913     d  Vind
009300070913     d  Vpes
009400070913     d  Vrsd
009500070913     d  vgir
009600070913     d  Vvol
009700070913     d  Vtc1
009800070913     d  Vtc2
009900070913     d  VAAS
010000070913     d  VLNP
010100070913     d  VNRS
010200070913     d  VNSP
010300071012     d  in22h
010400070913     d  Selected                      1A
010500070913
010600070913      /COPY QSYSINC/QRPGLESRC,QLGSORT
010700070913     d QLGKL                         16    DIM(MaxKey)
010800070913     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
010900070913     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
011000070913     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
011100070913     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
011200070913
011300070913      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
011400070913      /COPY QSYSINC/QRPGLESRC,QUSEC
011500070913      *---------------- ----------------------------------------------------
011600070913      *da menu
011700070913      *---------------- Gestione formato video video01----------------------
011800070913     C                   EXSR      Inzvideo1
011900070918     C                   DOU       $Video <> 'VIDEO1'
012000070913      *----------- Visualizzo dati di output in caso di errori.-------------
012100070918     C                   IF        *IN99
012200070913     C                   EVAL      *IN99 = *OFF
012300070913     C                   WRITE     video1
012400070913     C                   EVAL      *IN99 = *ON
012500070918     C                   ENDIF
012600070913      *
012700070913     c     emetti        tag
012800070913     C                   EXFMT     video1
012900070913      *
013000070918     C                   SELECT
013100070913     C                   WHEN      *INKC
013200070913     C                             OR
013300070913     C                             *INKL
013400070913     C                   EVAL      $Video = *BLANKS
013500070913
013600070913     C                   WHEN      *INKE
013700070913     C                   EXSR      Inzvideo1
013800070913     C                   WHEN      *INKB
013900070913     C                   EXSR      Chkvideo1
014000070913     C                   OTHER
014100070913     C                   EXSR      Chkvideo1
014200070913     c   44              goto      emetti
014300070913     C  N99              EXSR      Wrkvideo2c
014400070913     C     OK            CABEQ     0             EMETTI
014500070913     C                   ENDSL
014600011227      *
014700070913     C                   ENDDO
014800070913      *
014900070913     C                   EXSR      Uscita
015000070913     c**********************************************************************
015100070913     c* uscita
015200070913     c**********************************************************************
015300070913     C     Uscita        BEGSR
015400070913      *
015500070913     C                   EVAL      *INLR = *ON
015600070913     C                   RETURN
015700070913      *
015800070913     C                   ENDSR
015900070913     c**********************************************************************
016000070913     c* gestione sfl
016100070913     c**********************************************************************
016200070913     C     Wrkvideo2c    BEGSR
016300070913      *
016400011227     C                   EVAL      WrkCarS01 = *ON
016500070913     C                   EVAL      $Video = 'VIDEO2C'
016600070913      *
016700070913     C                   DOU       $Video <> 'VIDEO2C'
016800070913     C                   EXSR      Carvideo20
016900070913      * ordina il sfl caricato per località solo se presenti rekord
017000070913      *
017100070913     C     OK            CABEQ     0             TORNA
017200070913     C                   EXSR      ordina
017300070913     C                   WRITE     video2z
017400070913     C                   EXFMT     video2c
017500070913     C                   SELECT
017600070913     c* fine
017700070913     C                   WHEN      *INKC=*ON
017800070913     C                   EVAL      $VIDEO=*BLANKS
017900070913     c* guida
018000070913     C                   WHEN      *INKL=*ON
018100070913     C                   EVAL      $VIDEO='VIDEO1'
018200070913
018300070913     C                   WHEN      *INKa=*ON
018400070913     C                   exsr      spalmaopz1
018500070913
018600070913     C                   OTHER
018700070913
018800070913     C                   EXSR      GestioneSFL
018900070913     C                   ENDSL
019000070913
019100070913     C                   ENDDO
019200070913     C     torna         ENDSR
019300070913     c**********************************************************************
019400070913     c* Controlli video R01.
019500070913     c**********************************************************************
019600070913     C     Chkvideo1     BEGSR
019700070913      *
019800071106     c                   setoff                                       949549
019900070918     c                   setoff                                       454647
020000070913     C                   EVAL      *IN99 = *OFF
020100070913     C                   EVAL      *IN44 = *OFF
020200070913      * profilo non edp verifico £1
020300070913     c                   if        %subst(knmus:1:3) <> 'EDP'
020400070913     c     vfil          lookup    fil£1                                  92
020500070913     c  n92              seton                                        44
020600070913     c                   endif
020700070913
020800070913      * carico filiali £6
020900070913     c                   eval      kpjbus = kpjbu
021000070913     C                   clear                   trul06ds
021100070913     C                   movel     '£6'          d06cod
021200070913     C                   movel     vfil          d06key
021300070913     C                   movel     trul06ds      kpjbu
021400070913     C                   CALL      'TRUL06R'
021500070913     C                   parm                    kpjba
021600070913     C                   movel     kpjbu         trul06ds
021700070913     C                   movea     lin           fil£6
021800070913     c                   eval      kpjbu = kpjbus
021900070913
022000070918     C                   clear                   og146
022100070918     C                   clear                   og147
022200070917     c* leggo organigramma x gestire ind 08 data/ora limite abbinamento
022300070917     c     vfil          chain     azorg01l
022400070917     c                   if        %found(azorg01l)
022500070918     C                   movel     orgde6        og146
022600070918     C                   movel     orgde7        og147
022700070918     c     §ogdoada      comp      'S'                                    08
022800070918     c                   endif
022900070917     c* se p.o. non partito esco
023000070917     c                   if        §ogdda = ' '
023100070917     c                   seton                                        4499
023200070918     c                   goto      endctr1
023300070918     c                   endif
023400070918      * data distinte
023500070918     c                   if        vddis = 0
023600070918     c                   seton                                        4599
023700070918     c                   goto      endctr1
023800070918     c                   else
023900070918     C                   z-add     vddis         g02dat
024000070918     C                   movel     *blank        g02err
024100070918     C                   call      'XSRDA8'
024200070918     C                   parm                    wlbda8
024300070918    1C                   if        g02err = '1'
024400070918     C                   seton                                        4599
024500070918     c                   goto      endctr1
024600070918    1C                   endif
024700070918     C                   Z-ADD     g02inv        vddisg            8 0
024800070918     C                   Z-ADD     g02dat        vddis             8 0
024900070918     c                   endif
025000070921      * data distinte non può essere minore di udate
025100070921     c                   if        vddisg < wuda
025200070921     c                   seton                                        4899
025300070921     c                   endif
025400070918      * verifica se esistono distinte aperte nella data
025500070918     c                   move      'X'           nodis             1
025600070918     c     kdst          setll     fidst02l
025700070918     c                   do        *hival
025800070918     c     kdst          reade     fidst02l
025900070918     c                   if        %eof(fidst02l)
026000070918     c                   leave
026100070918     c                   endif
026200070918     c                   if        dstfcf = *blank and dstatb = *blank
026300070918     c                   clear                   nodis
026400070918     c                   leave
026500070918     c                   endif
026600070918     c                   enddo
026700070918     c                   if        nodis <> *blank
026800070918     C                   seton                                        4799
026900070918     c                   goto      endctr1
027000070918     c                   endif
027100071106     c* data limite controlli attivi solo se filiale abilitata 08 *on
027200070918     c                   clear                   vdlimg
027300071106     c                   if        *in08
027400071106      * data limite obbligatoria
027500071106     c                   if        vdlim = 0
027600071106     c                   seton                                        4999
027700071106     c                   goto      endctr1
027800071106     c                   endif
027900071106      * data limite valida
028000070918     c                   if        vdlim > 0
028100070918     C                   z-add     vdlim         g02dat
028200070918     C                   movel     *blank        g02err
028300070918     C                   call      'XSRDA8'
028400070918     C                   parm                    wlbda8
028500070918    1C                   if        g02err = '1'
028600070918     C                   seton                                        4699
028700070918     c                   goto      endctr1
028800070918    1C                   endif
028900070918     C                   Z-ADD     g02inv        vdlimg            8 0
029000070918     C                   Z-ADD     g02dat        vdlim             8 0
029100070918      * data limite abbinamento non può essere maggiore di data distinte
029200070918     c                   if        vdlimg > vddisg
029300070918     C                   seton                                        4699
029400070918     c                   goto      endctr1
029500070918     c                   endif
029600071106     c                   endif
029700070918     c                   endif
029800070918      * cerca giorno della settimana da impostare in ultima videata
029900070918
030000070918     C                   clear                   vgiochiu
030100070918     C                   clear                   vdesgio
030200070918     C                   movel     vddis         wggmm             4
030300070918     C                   move      vddis         waa               2
030400070918     C                   movel     wggmm         wggmmaa           6
030500070918     C                   move      waa           wggmmaa
030600070918     C                   call      'XGIOSE1'
030700070918     C                   parm                    wggmmaa
030800070918     C                   parm                    vgiochiu
030900070918     C                   move      vgiochiu      y                 1 0
031000070918     C                   movel(p)  gio(y)        vdesgio
031100070918
031200070913      * se imposto località'DA'e località 'A'a blank la inserisco uguale
031300070913     c                   if        vdaloc <> *blank and valoc = *blank
031400070913     c                   eval      valoc = %trimr(vdaloc) + 'ZZZ'
031500070913     c                   endif
031600070913      * se imposto località'DA'e località 'A'con solo iniziale metto nel
031700070913      * secondo byte la 'Z' x la selezione
031800070913     c                   if        vdaloc <> *blank and valoc <> *blank and
031900070913     c                             %subst(valoc:2:1) = ' '
032000070913     c                   eval      valoc = %trimr(valoc) + 'ZZZ'
032100070913     c                   endif
032200070913      * vice versa località'A'e località 'DA'a blank la inserisco uguale
032300070913     c                   if        valoc <> *blank and vdaloc = *blank
032400070913     c                   movel     valoc         vdaloc
032500070913     c                   endif
032600070913      * verifico che lcalità'DA' sia congruente con località 'A'
032700070913     c                   if        vdaloc > valoc
032800070913     c                   seton                                        9499
032900070913     c                   endif
033000070913      *
033100070918     C     endctr1       ENDSR
033200070913     c**********************************************************************
033300070913     c* inizializza r01
033400070913     c**********************************************************************
033500070913     C     Inzvideo1     BEGSR
033600070913
033700070913     c                   move      o36pou        vfil
033800070913     c                   move      *blank        vdaloc
033900070913     c                   move      *blank        valoc
034000070918     c                   z-add     wdat          vddis
034100070913     C                   EXSR      Chkvideo1
034200070913
034300070913     C                   ENDSR
034400070913     c**********************************************************************
034500070913     C     opzione       BEGSR
034600070913     c**********************************************************************
034700070913     c                   move      v1csce        modalita
034800070918     c                   setoff                                       28
034900070913     c                   select
035000070913     c                   when      v1csce = '1'
035100070913     c                   exsr      multipla
035200070913     c                   clear                   $msg
035300070913     c                   setoff                                       28
035400070913     c                   when      v1csce = '5'
035500070913     c                   seton                                        11
035600070913     c                   exsr      richbl
035700070913     c                   setoff                                       11
035800070913     c                   endsl
035900070913     C                   ENDSR
036000070913     c**********************************************************************
036100070913     C     multipla      BEGSR
036200070913     c**********************************************************************
036300070913     c                   setoff                                       28
036400070913     c                   if        multi = *blanks
036500070913      *
036600070913     c                   exsr      Gesvideo3
036700070919     c                   move      'X'           multi
036800070913
036900070918      *imposta il giorno sulle ulteriori righe della scelta multipla
037000070913     c                   else
037100070919     c                   if        fatto <> *blank
037200070913     c                   exsr      aggiorna
037300070919     c                   endif
037400070919
037500070913     c                   endif
037600070913     C                   ENDSR
037700070913     c**********************************************************************
037800070913     C     gesvideo3     BEGSR
037900070913     c**********************************************************************
038000070913     c                   clear                   fatto
038100070913     c                   clear                   riemetti
038200070913     c                   do        *hival
038300070913     c                   exfmt     video3
038400070913     c                   if        *inkc
038500070913     C                   EVAL      $Video = *BLANKS
038600070913     c                   leave
038700070913     c                   endif
038800070913     c   kl              leave
038900070913     c                   exsr      controv3
039000070913     c   28
039100070918     cor 99              iter
039200070913     c   kf              exsr      aggiorna
039300070913     c                   if        fatto <> *blank or riemetti <> *blank
039400070913     c                   leave
039500070913     c                   endif
039600070913
039700070913     c                   enddo
039800070913     C                   ENDSR
039900070913     c**********************************************************************
040000070913     C     Controv3      BEGSR
040100070913     c**********************************************************************
040200070913      *
040300070913     c                   move      'X'           riemetti
040400070913
040500070917     c                   endsr
040600070913     c**********************************************************************
040700070913     C     aggiorna      BEGSR
040800070913     c**********************************************************************
040900070913     c                   move      'X'           fatto
041000070917      * RICHIAMO PGM DI VARIAZIONE SE IMPOSTATO IL CODICE
041100070917      *
041200070917     c     karb          chain     fnarb01l
041300070917     c                   if        %found(fnarb01l)
041400070917     c                   clear                   fnlr48ds
041500070918     c                   clear                   dsarbg
041600070917     c                   movel     arbaas        d48aas
041700070917     c                   movel     arblnp        d48lnp
041800070917     c                   movel     arbnrs        d48nrs
041900070917     c                   movel     arbnsp        d48nsp
042000070917     c                   movel     vfil          d48fgs
042100070917     c                   movel     arbcbo        d48cbo
042200071012     c                   movel     'CR'          d48cvb
042300070917     c                   movel     'A'           d48tbo
042400070917     c                   movel     'S'           d48f12
042500070918     c                   movel     'E'           d48ffr
042600070917     c                   movel     'G'           d48trc
042700070917     c                   movel     fnlr48ds      kpjbu
042800070917     c                   movel     arbtcr        §BGTCR
042900070917     c                   movel     arbhcr        §BGHCR
043000070917     c                   movel     arbdcr        §BGDCR
043100070917     c                   movel     arbtc1        §BGTC1
043200070917     c                   movel     arbtc2        §BGTC2
043300071012     c                   movel     arbgc1        §BGgC1
043400071012     c                   movel     arbgc2        §BGgC2
043500070917      * imposta giorno di chiusura
043600070917      * se primo campo libero lo imposta
043700070917     c                   if        arbgc1 = *blank
043800070918     c                   movel     vgiochiu      §bggc1
043900070917     c                   else
044000070917      * se primo campo occupato e secondo libero lo imposta
044100070917     c                   if        arbgc2 = *blank
044200070918     c                   movel     vgiochiu      §bggc2
044300070917     c                   else
044400070917      * se occupati entrambi forza nel primo campo
044500070918     c                   movel     vgiochiu      §bggc1
044600070917     c                   endif
044700070917     c                   endif
044800070917     c                   call      'FNLR48R'
044900070917     c                   parm                    KPJBA
045000070917     c                   parm                    DSARBD
045100070917     c                   parm                    DSARBK
045200070917     c                   parm                    DARBT
045300070917     c                   parm                    DSARBG
045400070917     c                   parm                    TRUL90DS
045500070917      *
045600070917     c                   movel     kpjbu         fnlr48ds
045700070917     C* errore
045800070920     C                   if        d48err <> ' '
045900070918     c                   eval      erro = 'X'
046000070917     c                   endif
046100070917
046200070917     c                   endif
046300070913     C                   ENDSR
046400070913     c**********************************************************************
046500070913     C     spalmaopz1    BEGSR
046600070913     c**********************************************************************
046700070913     c                   do        *hival        recor             4 0
046800070913     c     recor         chain     video2                             93
046900070913     c   93              leave
047000070913     c                   move      '1'           v1csce
047100070913     c                   update    video2
047200070913     c                   enddo
047300070913     C                   ENDSR
047400070913     c**********************************************************************
047500070913     c* carica sfl partendo da fnarb
047600070913     c**********************************************************************
047700070913     C     Carvideo20    BEGSR
047800070913     C                   IF        WrkCarS01 = *ON
047900070913     C                   EVAL      snrr1   = 0
048000070913     C                   EVAL      *IN90 = *ON
048100070913     C                   EVAL      *IN91 = *OFF
048200070913     C                   WRITE     video2c
048300070913     C                   EVAL      *IN90 = *OFF
048400070913      * gestione eventuali filiali in £6
048500070913     c                   do        *hival        £6                3 0
048600070913     c                   if        fil£6(£6) > 0
048700070913     c                   z-add     fil£6(£6)     vfil
048800070913     c                   else
048900070913     c                   z-add     fil£6(1)      vfil
049000070913     c                   leave
049100070913     c                   endif
049200071025     c     vfil          setll     fnarb05l
049300070913     c                   do        *hival
049400071025     c     vfil          reade     fnarb05l                               97
049500070913     c   97              leave
049600071025     c                   if        arbndc > 0
049700071025     c                   iter
049800071025     c                   endif
049900070913
050000070913      * filtri
050100070918     c                   clear                   fiargds
050200070917     c                   clear                   fidg16ds
050300070917     c                   eval      d16aasi = arbaas
050400070917     c                   eval      d16lnpi = arblnp
050500070917     c                   eval      d16nrsi = arbnrs
050600070917     c                   eval      d16nspi = arbnsp
050700070917     c                   eval      d16consi= 'S'
050800070917     c                   eval      d16dfvi = vddisg
050900070917     c                   eval      d16dtai = vdlimg
051000070917     c                   eval      d16orai = volim
051100070917     c                   eval      d16ndci = ' '
051200130221      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
051300130221      ***  quindi si deve sostituire il test per identificare il Perfect Order
051400130221     c************       if        arbxco <> ' '
051500130221     c                   if        arbxco > '0'
051600110728     c                   eval      d16prfoI= 'S'
051700110728     c                   end
051800070917
051900070917     c                   eval      kpjbu = fidg16ds
052000070917     c                   call      'FIDG16R'
052100070917     c                   parm                    kpjba
052200070917     c                   parm                    fnarbds
052300070917     c                   parm                    fiargds
052400070917     c                   eval      fidg16ds = kpjbu
052500070917      * validità
052600070917     c                   if        d16conso = 'N'
052700070917     c                   iter
052800070917     c                   endif
052900070913
053000070918      * parzializzazione per localita richiesto range a video
053100070918     c                   if        vdaloc <> *blank and valoc <> *blank
053200070918     c                             and (arblod < vdaloc or arblod > valoc)
053300070918     c                   iter
053400070918     c                   endif
053500070918      * parzializzazione per nome destinatario
053600070918     c                   if        vdesti <> *blank
053700070918     C                   clear                   pos               2 0
053800070918     C                   eval      pos = %scan(%trim(Vdesti):arbrsd)
053900070918     C                   if        pos = *zeros
054000070918     C                   iter                                                   *RECORD NON VALIDO
054100070918     C                   endif
054200070918     c                   endif
054300070918      *fine filtri
054400070913
054500070913     C                   move      *blank        V1csce
054600070913     c                   movel     arblod        vlod
054700070913     c                   movel     arbind        vind
054800070913     c                   eval      vlodind = vlod + arbcad + vind
054900070913     c*                  movel     vlod          vlodind
055000070913     c*                  move      vind          vlodind
055100070913     c                   movel     arbprd        vprov
055200070913     c                   movel     arbrsd        vrsd
055300070913     c                   movel     arbtc1        vtc1
055400070913     c                   movel     arbtc2        vtc2
055500070913     c                   z-add     arbpkb        vpes
055600070913     c                   z-add     arbvlb        vvol
055700070913     c                   z-add     arbaas        vaas
055800070913     c                   z-add     arblnp        vlnp
055900070913     c                   z-add     arbnrs        vnrs
056000070913     c                   z-add     arbnsp        vnsp
056100070913     c                   movel     arbcbo        vcbo
056200150720     c                   clear                   vgir
056300150720     c     karb          chain     fiarg01l
056400150720     c                   if        %found(fiarg01l)
056500150720     c                   movel     argcgi        vgir
056600150720     c                   endif
056700070913      *esco per raggiunta massima capacità sfl
056800070913     c                   if        snrr1 > 9990
056900070913     c                   leave
057000070913     c                   endif
057100070913
057200070913     C                   EVAL      snrr1 = snrr1 + 1
057300070913     C                   WRITE     video2
057400070913     C                   ENDDO
057500070913     C                   enddo
057600070913
057700070913     C                   IF        snrr1  > 0
057800070913     C                   EVAL      *IN91 = *ON
057900070913     C                   MOVE      1             OK                1 0
058000070913     C                   ELSE
058100070913     C                   MOVE      0             OK                1 0
058200070913     C                   ENDIF
058300070913     C                   EVAL      nrr1  = 1
058400070913     C                   ENDIF
058500070913     C                   EVAL      WrkCarS01 = *OFF
058600070913     C                   EVAL      lasnrr1  = snrr1
058700070913     C                   EVAL      snrr1  = 1
058800070913     C                   EVAL      csrrrn = 1
058900070913     C                   ENDSR
059000070913     c**********************************************************************
059100070913     c* gestione sfl
059200070913     c**********************************************************************
059300070913     C     GestioneSFL   BEGSR
059400070913      *
059500070918     c                   clear                   erro              1
059600070913     C                   EVAL      WrkEofS01 = *OFF
059700070913      * Elaborazione opzioni.
059800070913     c*
059900070913     c                   do        *hival        X                 4 0
060000070913     c     X             CHAIN     video2                             50
060100070913     C   50              LEAVE
060200070913     c   kc              goto      endrut
060300071012     C                   IF        v1csce <> *blanks
060400070913     c                   exsr      opzione
060500071001     c                   setoff                                       22
060600071106     c                   if        v1csce = '1' and not *inkl
060700070921     c                   seton                                        22
060800071012     c                   move      '1'           in22h
060900070921     c                   endif
061000070921     c                   move      *blank        v1csce
061100070921     c                   update    video2
061200071012     c                   move      *blank        in22h
061300070913
061400071012     C                   ENDIF
061500070913     C                   enddo
061600070913      * abblenca spia x gestione scelta multipla per prossima selezione
061700070913     c                   move      *blank        multi
061800070913
061900070918     c                   if        erro <> *blank
062000070918     c                   eval      $msg =
062100070918     c                              'Non tutte le bolle sono state aggiornate'
062200070918     c                   seton                                        28
062300070918     c                   endif
062400070921      * ricaricamento sfl
062500070921     c*                  if        modalita = '1'
062600070921     c*                  eval      wrkcars01 = *on
062700070921     c*                  endif
062800070913      *
062900070913     C     endrut        ENDSR
063000070913     c**********************************************************************
063100070913     C     *INZSR        BEGSR
063200070913     c**********************************************************************
063300070913      *
063400070913     C     *ENTRY        PLIST
063500070913     C                   PARM                    Kpjba
063600070913      *
063700070913     C                   Z-ADD     1             CODUT
063800070913     C                   CALL      'X§PARUT'
063900070913     C                   PARM                    UTEDSE
064000070913     C                   MOVEL     REC80         CNCR80
064100070913     C                   MOVEL     Ragut         rsut
064200070913     C                   MOVEL     knsif         vknsif
064300070913     C                   MOVEL     knmus         vknmus
064400070913      *reperimento data
064500070913     C                   TIME                    W0120            14 0
064600070913     C                   MOVE      W0120         WDAT              8 0
064700070913     C*
064800070913     C                   Z-ADD     WDAT          G02DAT
064900070913     C                   MOVEL     *BLANK        G02ERR
065000070913     C                   CALL      'XSRDA8'
065100070913     C                   PARM                    WLBDA8
065200070913     C* UDATE A 8 IN AAAA/MM/GG
065300070913     C                   Z-ADD     G02INV        WUDA              8 0
065400070913     C                   Z-ADD     G02INV        WUDAte            8 0
065500070913      *profilo di filiale attivo indicatore di protezione campi non manutenzionabili
065600070913     C                   CLEAR                   Tibs36Ds
065700070913     C                   EVAL      I36ute = knmus
065800070913     C                   CALL      'TIBS36R'
065900070913     C                   PARM                    tibs36ds
066000070913      *
066100070913     c                   movel     o36pou        vfil
066200070913      *
066300070917     c     vfil          chain     azorg01l
066400070917     c                   if        %found(azorg01l)
066500070917     C                   MOVEL     ORGDE6        og146
066600070917     C                   MOVEL     ORGDE7        og147
066700070917     c     §OGDOADA      comp      'S'                                    08
066800070917     c                   end
066900070917     c* se p.o. non partito esco
067000070917     c                   if        §ogdda = ' '
067100070917     c                   seton                                        lr
067200070917     c                   return
067300070917     c                   end
067400070917      *
067500070917     C     Karb          klist
067600070917     C                   kfld                    vaas
067700070917     C                   kfld                    vlnp
067800070917     C                   kfld                    vnrs
067900070917     C                   kfld                    vnsp
068000070913      *
068100070913     C     Ktab          klist
068200070913     C                   kfld                    tblkut
068300070913     C                   kfld                    tblcod
068400070913     C                   kfld                    tblkey
068500070913     C                   z-add     1             tblkut
068600070918     C     Kdst          klist
068700070918     C                   kfld                    categ             1 0
068800070918     C                   kfld                    vddisg
068900070918     C                   kfld                    vfil
069000070918     C                   z-add     4             categ
069100070913      * carico filiali £1
069200070913     c                   eval      kpjbus = kpjbu
069300070913     C                   clear                   trul06ds
069400070913     C                   movel     '£1'          d06cod
069500070913     C                   movel     vfil          d06key
069600070913     C                   movel     trul06ds      kpjbu
069700070913     C                   CALL      'TRUL06R'
069800070913     C                   parm                    kpjba
069900070913     C                   movel     kpjbu         trul06ds
070000070913     C                   movea     lin           fil£1
070100070913     c                   eval      kpjbu = kpjbus
070200070913
070300070913     C                   ENDSR
070400070913      *--------------------------------------------------------------------------------------------*
070500070913      * ordina - riscrive il sfl ordinato per località
070600070913      *--------------------------------------------------------------------------------------------*
070700070913     C     ordina        BEGSR
070800070913     C                   setoff                                       72
070900070913      * IMPOSTO ENTRATA AD 'A'
071000070913     C                   EVAL      snrr1 = 0
071100070913      *
071200070913     C                   CLEAR                   QLGSCB
071300070913     C                   CLEAR                   QLGSCB00
071400070913
071500070913     ** 1 campo chiave.
071600070913     C                   EVAL      QLGNBRK    = 1
071700070913     ** localita/indirizzoè a posizione 1, carattere, 71 byte,
071800070913     C                   EVAL      QLGSP      = 1
071900070913     C                   EVAL      QLGSS      = %SIZE(vlodind)
072000070913     C                   EVAL      QLGDT      = carattere
072100070913     C                   EVAL      QLGSO      = Ascendente
072200070913     C                   EVAL      QLGKL(1)   = QLGSKL
072300070913
072400070913      * Load other sort parameters.
072500070913     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
072600070913     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
072700070913     C                   EVAL      QLGRT     = 8
072800070913     C                   EVAL      QLGOKL    = 80
072900070913     C                   EVAL      QLGLKE    = 16
073000070913     C                   EVAL      QLGLSS    = 290
073100070913
073200070913      * Initialize Sort I/O API fields.
073300070913     C                   EVAL      QLGRL00  = QLGRL
073400070913     C                   EVAL      QLGRC00  = 1
073500070913     C                   CLEAR                   QUSEI
073600070913     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
073700070913
073800070913      * First step - Initialize the sort routine.
073900070913     C                   CALL      'QLGSORT'
074000070913     C                   PARM                    QLGSCB
074100070913     C                   PARM                    NotUsed
074200070913     C                   PARM                    NotUsed
074300070913     C                   PARM                    SizeList
074400070913     C                   PARM                    ReturnSize
074500070913     C                   PARM                    QUSEC
074600070913
074700070913      * Next step - Write records to I/O routine.
074800070913     C                   EVAL      QLGRT00 = Put
074900070913
075000070913     C                   DO        *hival        cx                4 0
075100070913
075200070913     C     cx            CHAIN     video2                             50
075300070913     c   50              leave
075400070913     ** Solo le righe con Selected = 'Y' sono riordinate,
075500070913     ** quindi per fare un ordinamento di tutte le righe
075600070913     ** metto 'Y' sempre.
075700070913     C                   EVAL      Selected = 'Y'
075800070913
075900070913     C                   CLEAR                   QUSEI
076000070913     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
076100070913     C                   CALL      'QLGSRTIO'
076200070913     C                   PARM                    QLGSCB00
076300070913     C                   PARM                    SflRcd
076400070913     C                   PARM                    NotUsed
076500070913     C                   PARM                    SizeList
076600070913     C                   PARM                    NotUsed
076700070913     C                   PARM                    QUSEC
076800070913
076900070913     C                   ENDDO
077000070913
077100070913      * Next step - Signal end of input, clear subfile for reload.
077200070913     C                   EVAL      QLGRT00 = EndPut
077300070913     C                   CLEAR                   QUSEI
077400070913     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
077500070913
077600070913     C                   CALL      'QLGSRTIO'
077700070913     C                   PARM                    QLGSCB00
077800070913     C                   PARM                    SflRcd
077900070913     C                   PARM                    NotUsed
078000070913     C                   PARM                    SizeList
078100070913     C                   PARM                    NotUsed
078200070913     C                   PARM                    QUSEC
078300070913
078400070913     C                   SETON                                        90        *
078500070913     C                   WRITE     video2c                                      *
078600070913     C                   SETOFF                                       90        *
078700070913     C                   WRITE     video2z                                      *
078800070913
078900070913      * Final step - Write the records back to the subfile.
079000070913     C                   EVAL      QLGRT00 = Get
079100070913     C                   DO        lasnrr1       x
079200070913     C                   CLEAR                   QUSEI
079300070913     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
079400070913     C                   CALL      'QLGSRTIO'
079500070913     C                   PARM                    QLGSCB00
079600070913     C                   PARM                    NotUsed
079700070913     C                   PARM                    SflRcd
079800070913     C                   PARM                    QLGRL00
079900070913     C                   PARM                    NotUsed
080000070913     C                   PARM                    QUSEC
080100070913      *
080200070913     c                   z-add     x             snrr1
080300071012     c                   setoff                                       22
080400071012     c                   if        in22h = '1'
080500071012     c                   seton                                        22
080600071012     c                   endif
080700070913     C                   WRITE     video2
080800070913     C                   ENDDO
080900070913     C                   SETOFF                                       20        *
081000070913
081100070913      *
081200070913     C                   ENDSR
081300070913      *---------------------------------------------------------------------
081400070913     C**************************************************************************
081500070913     C*  Routine per gestione richiamo interrogazione bolle
081600070913     C**************************************************************************
081700070913     C     richbl        begsr
081800070913      *
081900070913     c                   movel     kpjbu         §kpjbu
082000070913      *
082100070913     c                   z-add     vaas          pa1aas
082200070913     c                   z-add     vlnp          pa1lnp
082300070913     c                   z-add     vnrs          pa1nrs
082400070913     c                   z-add     vnsp          pa1nsp
082500070913     c                   movel     ' '           pa1f03
082600070913     c                   movel     '1'           pa1flg
082700070913     c                   movel     param1        kpjbu
082800070913     c                   call      'FNLR36R'
082900070913     c                   parm                    kpjba
083000070913     c                   movel     kpjbu         param1
083100070913      *
083200070913     c                   movel     §kpjbu        kpjbu
083300070913      *
083400070913     c                   endsr
083500070918      *____________________________________________________________________________
083600070918** GIO
083700070918Lunedì                                                                              1
083800070918Martedì                                                                             2
083900070918Mercoledì                                                                           3
084000070918Giovedì                                                                             4
084100070918Venerdì                                                                             5
084200070918Sabato                                                                              6
084300070918Domenica                                                                            7
