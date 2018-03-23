000100030604     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200070926     H* FIDG06R *-----------------------------------------------------*
000300070926     H*    ATTRIBUZIONE MANUALE GIRI DI CONSEGNA NELLE SPEDIZIONI
000400070926     H*---------------------------------------------------------------*
000500070123     Ffidg06d   CF   E             WORKSTN
000600030604     f                                     sfile(video2:snrr1)
000700070131     Ffnarb05l  if   E           k disk    rename(fnarb000:fnarb5)
000800070129     Ffiarg01l  uf a E           k disk
000900070403     Ffiarg52j  if   E           k disk    rename(fiarg000:fiarg2)
001000070125     Ffnarb01l  if   E           k disk
001100070123     Ffiar601l  if   E           k disk
001200070123     Ffiar901l  if   E           k disk
001300070124     Ffigrg03l  if   E           k disk
001400070201     Ffiar401l  if   E           k disk
001500070201     Ftabel00f  if   E           k disk
001600011228      *
001700020103     D Psds           SDS
001800020103     D  PgmName          *PROC
001900011228     D Kpjba         E DS
002000070131     d kpjbus          s                   like(kpjbu)
002100070206     d §kpjbu          s                   like(kpjbu)
002200011228      *
002300070123     D fidg06ds      E DS
002400070125     D fidg08ds      E DS
002500070125     D fidg09ds      E DS
002600070427     D fidg34ds      E DS
002700070201     D fidn12ds      E DS
002800030619     D trul33ds      E DS
002900070209     D fnlv53ds      E DS
003000070220     D fnlr66ds      E DS
003100070220     D fnlr48ds      E DS
003200070220     d DSARBD        e ds
003300070220     d DSARBK        e ds
003400070220     d DARBT         e ds
003500070220     d DSARBG        e ds
003600070220     d trul90ds      e ds
003700070131     D trul06ds      E DS
003800070131     D  lin                    1     90  0 dim(30)
003900070206     D* DATI X RICHIAMO PGM INTERR. BOLLE ARRIVI
004000070206     D PARAM1          DS                  INZ
004100070206     D  PA1AAS                14     17  0
004200070206     D  PA1LNP                18     20  0
004300070206     D  PA1NRS                21     22  0
004400070206     D  PA1NSP                23     29  0
004500070206     D  PA1F03                30     30
004600070206     D  PA1FLG                31     31
004700070206     D* SE =1 NON RICH. FNLG30R IN MODO RICORSIVO DA INTERR.BOLLE
004800070206     D  PA1GIA               144    144
004900030604     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
005000030604     D CNCR80        E DS
005100030617     d Tibs36Ds      e ds
005200070201     d ds3a          e ds
005300070201     d dsbl4a        e ds
005400070705     d dgrgflo       e ds
005500110701
005600110701     D fnlvstds      E DS
005700070705
005800011227     d data            ds            10
005900011228     d aa                      3      4
006000011228     d mm                      6      7
006100011228     d gg                      9     10
006200011228      *
006300070125     D caricada        S              1  0
006400070124     D fatto           S              1
006500070222     D riemetti        S              1
006600070124     D multi           S              1
006700070125     D modalita        S              1
006800070209     D abb             S              1
006900110701     D ChiamaST        S              1
007000070727     D argcgiw         S                   like(argcgi)
007100070125     D Savvarggir      S                   like(argcgi)
007200011227     D WrkEofS01       S              1
007300011227     D WrkCarS01       S              1
007400011228     D $VIDEO          S             10
007500030604     D snrr1           S              5i 0
007600070129     D lasnrr1         S              5i 0
007700030604     D SavOpzione      S                   like(v1csce)
007800030606     d dataiso         s               d   datfmt(*iso)
007900030606     d dataeur         s               d   datfmt(*eur)
008000030613     D WLBDA8          DS
008100030613     D  G02DAT                 1      8  0
008200030613     D  G02INV                 9     16  0
008300030613     D  G02ERR                17     17
008400030613     D  G02TGI                18     22  0
008500070131     d  fil£6          s              3  0 dim(30)
008600070131     d  fil£1          s              3  0 dim(30)
008700070125      *---------------------------------------------------------------------
008800070125      * specifiche per ordinamento forzato sfl
008900070125     d NotUsed         S             16A
009000070125     d ReturnSize      S              9B 0
009100070125     d SizeList        S              9B 0
009200070125     d WrkSort         S              1
009300070125     d*---
009400070125     d MaxKey          C                   2
009500070125     d Ascendente      C                   1
009600070125     d Discendente     C                   2
009700070125     d Carattere       C                   6
009800070125     d Put             C                   1
009900070125     d EndPut          C                   2
010000070125     d Get             C                   3
010100070125     d Numerico        C                   8
010200070125     d*---
010300070125     d SflRcd          DS
010400070206     d  vlodind
010500070125     d  vlod
010600070727     d  vprov
010700070125     d  Vind
010800070125     d  Vpes
010900070125     d  Vrsd
011000070727     d  vgir
011100070125     d  Vvol
011200070125     d  Vtc1
011300070125     d  Vtc2
011400070125     d  VAAS
011500070125     d  VLNP
011600070125     d  VNRS
011700070125     d  VNSP
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
012900030701      *---------------- ----------------------------------------------------
013000070124      * gestione singola spedizione chiamata esterna
013100070124     c                   if        d6nsp <> 0
013200070124     c                   move      d6aas         vaas
013300070124     c                   move      d6lnp         vlnp
013400070124     c                   move      d6nrs         vnrs
013500070124     c                   move      d6nsp         vnsp
013600070124     c                   exsr      singola
013700070124      *
013800070124     c                   else
013900070124      *da menu
014000030604      *---------------- Gestione formato video video01----------------------
014100030604     C                   EXSR      Inzvideo1
014200030604     C                   DOU       $Video <> 'VIDEO1'
014300000608      *----------- Visualizzo dati di output in caso di errori.-------------
014400000608     C                   IF        *IN99
014500000608     C                   EVAL      *IN99 = *OFF
014600030604     C                   WRITE     video1
014700000608     C                   EVAL      *IN99 = *ON
014800000608     C                   ENDIF
014900000608      *
015000030702     c     emetti        tag
015100030604     C                   EXFMT     video1
015200000608      *
015300011227     C                   SELECT
015400030612     C                   WHEN      *INKC
015500011227     C                             OR
015600011227     C                             *INKL
015700011227     C                   EVAL      $Video = *BLANKS
015800011227
015900070205      * ricerca interrogazione giri
016000070205     c                   when      *inkg
016100070205     c                   eval      kpjbus = kpjbu
016200070205     c                   clear                   fidg09ds
016300070205     c                   move      'P01'         d09iop0
016400070205     c                   movel     vfil          d09ifgs
016500070205     c                   z-add     wuda          d09idat
016600070705     c                   eval      d09itug = 'C'                                tipo util. consegna
016700070205     c                   movel     fidg09ds      kpjbu
016800070205     c                   call      'FIDG09R'
016900070205     c                   parm                    kpjba
017000070205     c  n11              movel     kpjbu         fidg09ds
017100070205     c  n11              movel     d09ocgi       vgiro
017200070205     c                   eval      kpjbu = kpjbus
017300070205     c                   goto      emetti
017400070205
017500011227     C                   WHEN      *INKE
017600030604     C                   EXSR      Inzvideo1
017700011227     C                   WHEN      *INKB
017800030604     C                   EXSR      Chkvideo1
017900011227     C                   OTHER
018000030604     C                   EXSR      Chkvideo1
018100030716     c   44              goto      emetti
018200030604     C  N99              EXSR      Wrkvideo2c
018300031020     C     OK            CABEQ     0             EMETTI
018400011227     C                   ENDSL
018500011227      *
018600011227     C                   ENDDO
018700070124      *
018800070124     c                   endif
018900011227      *
019000011227     C                   EXSR      Uscita
019100011227     c**********************************************************************
019200011227     c* uscita
019300011227     c**********************************************************************
019400011227     C     Uscita        BEGSR
019500110701     c                   clear                   fnlvstds
019600110701     c                   eval      ilvstTLA='C'
019700110701     c                   call      'FNLVSTR'
019800110701     c                   parm                    kpjba
019900110701     c                   parm                    fnlvstds
020000011227      *
020100011227     C                   EVAL      *INLR = *ON
020200011227     C                   RETURN
020300011227      *
020400011227     C                   ENDSR
020500011227     c**********************************************************************
020600011227     c* gestione sfl
020700011227     c**********************************************************************
020800030604     C     Wrkvideo2c    BEGSR
020900000627      *
021000011227     C                   EVAL      WrkCarS01 = *ON
021100030604     C                   EVAL      $Video = 'VIDEO2C'
021200011227      *
021300030604     C                   DOU       $Video <> 'VIDEO2C'
021400070125      * sbinamento routine di caricamento se il valore caricada = 0 il file
021500070125      * di partenza è FNARB se è uguale a 1 significa che ho impostato
021600070129      * a video il GIRO quindi miro solo le bolle con quel giro su ARG
021700070125     c                   if        caricada = 0
021800070125     C                   EXSR      Carvideo20
021900070129      * ordina il sfl caricato per località solo se presenti rekord
022000070206     C*    OK            CABEQ     0             TORNA
022100070206     C*                  EXSR      ordina
022200070129
022300070125     c                   else
022400070125     c                   exsr      carvideo21
022500070125     c                   endif
022600000627      *
022700031020     C     OK            CABEQ     0             TORNA
022800070206     C                   EXSR      ordina
022900030604     C                   WRITE     video2z
023000030604     C                   EXFMT     video2c
023100011227     C                   SELECT
023200011228     c* fine
023300011227     C                   WHEN      *INKC=*ON
023400011227     C                   EVAL      $VIDEO=*BLANKS
023500011228     c* guida
023600011227     C                   WHEN      *INKL=*ON
023700030604     C                   EVAL      $VIDEO='VIDEO1'
023800030715
023900030715     C                   WHEN      *INKa=*ON
024000070205     C                   exsr      spalmaopz1
024100011227
024200011227     C                   OTHER
024300011227
024400011227     C                   EXSR      GestioneSFL
024500011227     C                   ENDSL
024600011228
024700011227     C                   ENDDO
024800031020     C     torna         ENDSR
024900011227     c**********************************************************************
025000011227     c* Controlli video R01.
025100011227     c**********************************************************************
025200030604     C     Chkvideo1     BEGSR
025300000609      *
025400070129     c                   setoff                                       9495
025500000608     C                   EVAL      *IN99 = *OFF
025600030702     C                   EVAL      *IN44 = *OFF
025700070131      * profilo non edp verifico £1
025800070131     c                   if        %subst(knmus:1:3) <> 'EDP'
025900070131     c     vfil          lookup    fil£1                                  92
026000070205     c  n92              seton                                        44
026100070131     c                   endif
026200070131
026300070131      * carico filiali £6
026400070131     c                   eval      kpjbus = kpjbu
026500070131     C                   clear                   trul06ds
026600070131     C                   movel     '£6'          d06cod
026700070131     C                   movel     vfil          d06key
026800070131     C                   movel     trul06ds      kpjbu
026900070131     C                   CALL      'TRUL06R'
027000070131     C                   parm                    kpjba
027100070131     C                   movel     kpjbu         trul06ds
027200070131     C                   movea     lin           fil£6
027300070131     c                   eval      kpjbu = kpjbus
027400070131
027500070131     c                   if        vgiro <> *blank and vgiro <> '9999999999'
027600070125     c                   z-add     1             caricada
027700070125     c                   else
027800070125     c                   z-add     0             caricada
027900070125     c                   endif
028000070129      * se imposto località'DA'e località 'A'a blank la inserisco uguale
028100070129     c                   if        vdaloc <> *blank and valoc = *blank
028200070905     c                   eval      valoc = %trimr(vdaloc) + 'ZZZ'
028300070129     c                   endif
028400070205      * se imposto località'DA'e località 'A'con solo iniziale metto nel
028500070205      * secondo byte la 'Z' x la selezione
028600070205     c                   if        vdaloc <> *blank and valoc <> *blank and
028700070205     c                             %subst(valoc:2:1) = ' '
028800070905     c                   eval      valoc = %trimr(valoc) + 'ZZZ'
028900070205     c                   endif
029000070129      * vice versa località'A'e località 'DA'a blank la inserisco uguale
029100070129     c                   if        valoc <> *blank and vdaloc = *blank
029200070129     c                   movel     valoc         vdaloc
029300070129     c                   endif
029400070129      * verifico che lcalità'DA' sia congruente con località 'A'
029500070129     c                   if        vdaloc > valoc
029600070129     c                   seton                                        9499
029700070129     c                   endif
029800070129     c
029900000608      *
030000000608     C                   ENDSR
030100011227     c**********************************************************************
030200011227     c* inizializza r01
030300011227     c**********************************************************************
030400030604     C     Inzvideo1     BEGSR
030500000608
030600070123     c                   move      o36pou        vfil
030700070321     c*mm                move      *blank        vcons
030800070123     c                   move      *blank        vgiro
030900070123     c                   move      *blank        vdaloc
031000070123     c                   move      *blank        valoc
031100070131     c                   move      *blank        vrie
031200070131     c                   move      *blank        vcser
031300070321     c*mm                move      *blank        vinc
031400070131     c                   move      'E'           vferd
031500070220     c                   move      *blank        vddt
031600070209     c                   move      'E'           vabb
031700030604     C                   EXSR      Chkvideo1
031800000609
031900000608     C                   ENDSR
032000030604     c**********************************************************************
032100030605     C     opzione       BEGSR
032200030605     c**********************************************************************
032300030610     c                   move      v1csce        modalita
032400030605     c                   select
032500030710     c                   when      v1csce = '1'
032600070123     c                   exsr      singola
032700030717     c                   clear                   $msg
032800030717     c                   setoff                                       28
032900070206     c                   when      v1csce = '9'
033000070124     c                   seton                                        10
033100070123     c                   exsr      multipla
033200070124     c                   setoff                                       10
033300070123     c                   clear                   $msg
033400070124     c                   setoff                                       28
033500030605     c                   when      v1csce = '5'
033600070124     c                   seton                                        11
033700070206     c                   exsr      richbl
033800070124     c                   setoff                                       11
033900030605     c                   endsl
034000030604     C                   ENDSR
034100030717     c**********************************************************************
034200070123     C     singola       BEGSR
034300030717     c**********************************************************************
034400070222     c     sopra         tag
034500030717     c                   setoff                                       28
034600070124     c     karb          chain     fnarb01l                           98
034700070202     c                   clear                   varggira
034800070427     c                   clear                   vargerr
034900070427     c                   clear                   vargerrd
035000070427     c                   clear                   varglno
035100070427     c                   clear                   varglnod
035200070427      * richiamo pgm per valori attribuzione giro da tellus
035300070427     c                   clear                   fidg34ds
035400070427     c                   move      knsif         id34sif
035500070427     c                   move      *blank        id34tla
035600070427     c                   move      '2'           id34tip
035700070427     c                   move      arbaas        id34aas
035800070427     c                   move      arblnp        id34lnp
035900070427     c                   move      arbnrs        id34nrs
036000070427     c                   move      arbnsp        id34nsp
036100070427     c                   call      'FIDG34R'
036200070427     c                   parm                    fidg34ds
036300070427     c                   if        od34esito = *blank
036400070427     c                   movel     od34cgi       varggira
036500070427     c                   movel     od34err       vargerr
036600070427     c                   movel     od34errdes    vargerrd
036700070427     c                   movel     od34lno       varglno
036800070427     c                   movel     od34lnodes    varglnod
036900070202     c                   endif
037000030717     c                   if        not *in98
037100070123     c                   move      arbaas        varbaas
037200070123     c                   move      arblnp        varblnp
037300070123     c                   move      arbnrs        varbnrs
037400070123     c                   move      arbnsp        varbnsp
037500070125     c                   move      arbksc        varbccm
037600070220     c                   movel     arbcbo        varbcbo
037700070125     c                   movel     arbrsm        varbrsm
037800070125     c                   movel     arbrsd        varbrsd
037900070125     c                   movel     arbind        varbind
038000070125     c                   movel     arblod        varblod
038100070125     c                   movel     arbcad        varbcad
038200070427     c                   movel     arbprd        varbprd
038300070123     c                   move      arbtsp        varbtsp
038400070123     c                   move      arbpkb        varbpkb
038500070123     c                   move      arbncl        varbncl
038600070123     c                   move      arbvlb        varbvlb
038700070123     c                   move      arbgma        varbgma
038800070123     c                   move      arbgga        varbgga
038900070123     c                   move      arbgva        varbgva
039000070123     c                   move      arbtc1        varbtc1
039100070123     c                   move      arbtc2        varbtc2
039200070123     c                   move      arbtcr        varbtcr
039300070201     c                   clear                   varbdcr
039400070201     c                   if        arbdcr > 0
039500070201     c                   move      arbdcr        dataiso
039600070201     c                   move      dataiso       dataeur
039700070201     c                   move      dataeur       varbdcr
039800070201     c                   endif
039900070123     c                   move      arbhcr        varbhcr
040000070123     c                   move      arbfbr        varbfbr
040100070222     c                   clear                   varbffd
040200070222     c                   if        arbffd <> *blank
040300070222     c                   move      'SI'          varbffd
040400070222     c                   endif
040500070131     c                   clear                   var6ift
040600070131     c     karb          chain     fiar601l
040700070131     c                   if        %found(fiar601l)
040800070131     c                   z-add     ar6ift        var6ift
040900070131     c                   endif
041000070131      * c/assegno
041100070131     c                   clear                   var9cas
041200070131     c     karb          chain     fiar901l
041300070131     c                   if        %found(fiar901l)
041400070201     c                   z-add     ar9cas        var9cas
041500070131     c                   endif
041600070201      * seconda ragione sociale
041700070201     c                   clear                   varbrs2
041800070201     c                   movel     'D'           trck
041900070201     c     kar4          chain     fiar401l
042000070201     c                   if        %found(fiar401l)
042100070201     c                   movel     ar4not        varbrs2
042200070201     c                   endif
042300070201      * note
042400070201     c                   clear                   var4not
042500070201     c                   movel     '8'           trck
042600070201     c     kar4          chain     fiar401l
042700070201     c                   if        %found(fiar401l)
042800070201     c                   movel     ar4not        var4not
042900070201     c                   endif
043000070206     c                   clear                   var4not2
043100070206     c                   movel     '9'           trck
043200070206     c     kar4          chain     fiar401l
043300070206     c                   if        %found(fiar401l)
043400070206     c                   movel     ar4not        var4not2
043500070206     c                   endif
043600070123      *
043700070123     c                   clear                   varggir
043800070123     c                   clear                   vdesgir
043900070123     c                   clear                   vposint
044000070123      *
044100030717     c                   exsr      Gesvideo3
044200070222      *forza rilettura arb in caso aggiornamento bolla e conseguente
044300070222      * riemissione video
044400070222     c                   if        riemetti <> *blank
044500070222     c                   goto      sopra
044600070222     c                   endif
044700070222
044800030717     c                   endif
044900030717     C                   ENDSR
045000030605     c**********************************************************************
045100070123     C     multipla      BEGSR
045200030605     c**********************************************************************
045300070124     c                   setoff                                       28
045400070124     c     karb          chain     fnarb01l                           98
045500070124     c                   if        not *in98
045600070124     c                   if        multi = *blanks
045700070124     c                   move      arbind        varbind
045800070124     c                   move      arblod        varblod
045900070202     c                   movel     arbcad        varbcad
046000070206     c                   movel     arbprd        varbprd
046100070202      *
046200070202     c                   clear                   varggir
046300070202     c                   clear                   vdesgir
046400070202     c                   clear                   vposint
046500070202      *
046600030605     c                   exsr      Gesvideo3
046700070124      * se "fatto" <> blank ho aggiornato la prima riga multipla
046800070125      * quindi memorizzo i valori da aggiornare x eventuali righe con stessa
046900070125      * opzione e proseguo l'aggiornamento  senza più emettere il video per
047000070125      * queste ultime se le opzioni fossero mischiate le altre differenti
047100070125      * dalla multipla verranno elaborate normalmente nell'ambito dello stesso
047200070125      * ciclo di elaborazione. Le multiple verranno elaborate in maniera
047300070125      * occulta con il valore impostato inizialmente
047400070125
047500070124     c                   if        fatto <> *blank
047600070124     c                   movel     varggir       savvarggir
047700070124     c                   move      'X'           multi
047800070124     c                   endif
047900070124      *imposta il giro sulle ulteriori righe della scelta multipla
048000070124     c                   else
048100070124     c                   movel     savvarggir    varggir
048200070124     c                   exsr      aggiorna
048300030605     c                   endif
048400070124     c                   endif
048500030605     C                   ENDSR
048600030605     c**********************************************************************
048700030605     C     gesvideo3     BEGSR
048800030605     c**********************************************************************
048900030702     c                   clear                   fatto
049000070222     c                   clear                   riemetti
049100030605     c                   do        *hival
049200030605     c                   exfmt     video3
049300070205     c                   if        *inkc
049400070205     C                   EVAL      $Video = *BLANKS
049500070205     c                   leave
049600070205     c                   endif
049700030605     c   kl              leave
049800070124      * ricerca interrogazione giri
049900070124     c                   if        *inkg
050000070202     c                   eval      kpjbus = kpjbu
050100070124     c                   clear                   fidg09ds
050200070124     c                   move      'P01'         d09iop0
050300070124     c                   movel     vfil          d09ifgs
050400070124     c                   z-add     wuda          d09idat
050500070124     c                   movel     vposint       d09ides
050600070705     c                   eval      d09itug = 'C'                                tipo util. consegna
050700070124     c                   movel     fidg09ds      kpjbu
050800070124     c                   call      'FIDG09R'
050900070124     c                   parm                    kpjba
051000070124     c  n11              movel     kpjbu         fidg09ds
051100070124     c  n11              movel     d09ocgi       varggir
051200070202     c                   eval      kpjbu = kpjbus
051300070124     c                   endif
051400070124      * ricerca interrogazione autisti
051500070124     c                   if        *inkh
051600070202     c                   eval      kpjbus = kpjbu
051700070124     c                   clear                   fidg08ds
051800070926     c                   movel     'C'           d08tp
051900070124     c                   movel     vfil          d08fgs
052000070129     c                   movel     vfil          d08fgsgo
052100070124     c                   movel     vposint       d08rsc
052200070124     c                   movel     fidg08ds      kpjbu
052300070124     c                   call      'FIDG08R'
052400070124     c                   parm                    kpjba
052500070124     c  n11              movel     kpjbu         fidg08ds
052600070124     c  n11              movel     d08cgio       varggir
052700070202     c                   eval      kpjbu = kpjbus
052800070124     c                   endif
052900070124      *
053000030624     c                   exsr      controv3
053100030717     c   28
053200030717     cor 96              iter
053300030605     c   kf              exsr      aggiorna
053400070222     c                   if        fatto <> *blank or riemetti <> *blank
053500030702     c                   leave
053600030702     c                   endif
053700030605
053800030605     c                   enddo
053900030605     C                   ENDSR
054000030605     c**********************************************************************
054100030624     C     Controv3      BEGSR
054200030605     c**********************************************************************
054300030613      *
054400030702      * *in96 errore generico videata dettaglio
054500070131     c                   setoff                                       9695
054600070220     c                   setoff                                       8988
054700070220      * eventuale variazione bolla
054800070220     c                   clear                   fnlr66ds
054900070220    1c                   if        vvar <> *blanks
055000070220     c                   movel     Varbaas       d66aas
055100070220     c                   movel     Varblnp       d66lnp
055200070220     c                   movel     Varbnrs       d66nrs
055300070220     c                   movel     Varbnsp       d66nsp
055400070220      *
055500070220     c     '?'           scan      vvar                                   90
055600070220      * Ricerca causale
055700070220    2c                   if        *in90
055800070220     c                   movel     'V'           d66ges
055900070220     c                   movel     'A'           d66tbo
056000070220      *
056100070220     c                   movel     fnlr66ds      kpjbu
056200070220     c                   call      'FNLR66R'
056300070220     c                   parm                    kpjba
056400070220     c                   movel     kpjbu         fnlr66ds
056500070220     c                   movel     d66cvb        Vvar
056600070220     c                   movel     d66des        vvard
056700070220   X2c                   else
056800070220      * Controllo causale inserita
056900070220     c                   movel     'C'           d66ges
057000070220     c                   movel     'A'           d66tbo
057100070220     c                   movel     Vvar          d66cvb
057200070220      *
057300070220     c                   movel     fnlr66ds      kpjbu
057400070220     c                   call      'FNLR66R'
057500070220     c                   parm                    kpjba
057600070220     c                   movel     kpjbu         fnlr66ds
057700070220     c                   movel     d66cvb        vvar
057800070220     c                   MOVEL     d66des        vvard
057900070220      *
058000070220      * Errore
058100070220    3c                   if        d66msg <> *blank
058200070220     c                   seton                                        9689
058300070220     c                   goto      endcv3
058400070220    3c                   endif
058500070220
058600070220    2c                   endif
058700070220      *
058800070220      * RICHIAMO PGM DI VARIAZIONE SE IMPOSTATO IL CODICE
058900070220      *
059000070220     c                   clear                   fnlr48ds
059100070220     c                   movel     Varbaas       d48aas
059200070220     c                   movel     Varblnp       d48lnp
059300070220     c                   movel     Varbnrs       d48nrs
059400070220     c                   movel     Varbnsp       d48nsp
059500070220     c                   movel     Vfil          d48fgs
059600070220     c                   movel     varbcbo       d48cbo
059700070220     c                   movel     vvar          d48cvb
059800070220     c                   movel     'A'           d48tbo
059900070220     c                   movel     'S'           d48f12
060000070220     c                   movel     'V'           d48ffr
060100070220     c                   movel     fnlr48ds      kpjbu
060200070220     c                   call      'FNLR48R'
060300070220     c                   parm                    KPJBA
060400070220     c                   parm                    DSARBD
060500070220     c                   parm                    DSARBK
060600070220     c                   parm                    DARBT
060700070220     c                   parm                    DSARBG
060800070220     c                   parm                    TRUL90DS
060900070220      *
061000070220     c                   movel     kpjbu         fnlr48ds
061100070220     C* errore
061200070220     C                   if        d48err = '1'
061300070220     C                   seton                                        9688
061400070220     c                   goto      endcv3
061500070220     C                   else
061600070220     C                   clear                   vvar
061700070220     C                   clear                   vvard
061800070222     c                   move      'X'           riemetti
061900070220    1C                   endif
062000070220     C*
062100070220    1C                   endif
062200070124      * decodifica giro
062300070131     c                   if        varggir <> *blank
062400070124     c     kgrg          chain     figrg03l                           95
062500070705     c                   If        not *in95
062600070705     c                   eval      dgrgflo = grgflo
062700070705     c                   else
062800070705     c                   clear                   dgrgflo
062900070705     c                   endif
063000070613     c                   if        not *in95 and grgatb = *blank and
063100070613     c                             grgdde <= wuda and wuda < grgdscad
063200070705      * verifico il tipo utilizzo giro
063300070705     c                             and (§grgtug = 'C' or §grgtug = 'E')
063400070705
063500070613     c                   movel     grgdes        vdesgir
063600070613     c                   else
063700070613     c                   clear                   vdesgir
063800070613     c                   seton                                        9596
063900070131     c                   endif
064000070613     c                   endif
064100070124
064200070220     c     endcv3        endsr
064300030605     c**********************************************************************
064400030605     C     aggiorna      BEGSR
064500030605     c**********************************************************************
064600110701     c                   eval      ChiamaST=' '
064700110701     c
064800070129      * se non trova ARG dalla chain effettuata nelle routine singola o
064900070129      * multipla provvede alla scrittura impostando da ARB
065000070206     c     karg1         chain     fiarg01l
065100070206     c                   if        not %found(fiarg01l)
065200070206      * rek non trovato scrivo solo se impostato a video un giro
065300070206     C                   if        varggir <> *blank
065400070129     c                   clear                   fiarg000
065500070129     c                   exsr      imposta_arg
065600070129     c                   write     fiarg000
065700110701     c* Imposto flag di richiamo bolla centro storico
065800110701     c                   eval      ChiamaST='S'
065900110701     c
066000070206     c                   endif
066100070129      *con rek ARG trovato aggiorna solo giro/filiale consegna/flag manuale
066200070129     c                   else
066300070129     c                   z-add     vfil          argpoc
066400110707     c* memorizzo se cambiato giro per richiamare pgm di controllo flag centro storico
066500110701     c                   if        argcgi<>varggir
066600110701     c                   eval      ChiamaST='S'
066700110701     c                   endif
066800110701     c
066900070124     c                   movel     varggir       argcgi
067000070205     c                   if        varggir <> *blank
067100070129     c                   move      'M'           argtgi
067200070205     c                   else
067300070205     c                   move      *blank        argtgi
067400070205     c                   endif
067500070124     c                   update    fiarg000
067600070129     c                   endif
067700070124     c                   move      'X'           fatto
067800110701     c
067900110701     c* Richiamo pgm controllo centro storico
068000110701     c                   if        ChiamaST='S'
068100110701     c                             and olvstprop<>'N'
068200110701     c                   clear                   fnlvstds
068300110701     c                   eval      ilvstaas=arbaas
068400110701     c                   eval      ilvstlnp=arblnp
068500110701     c                   eval      ilvstnrs=arbnrs
068600110701     c                   eval      ilvstnsp=arbnsp
068700110701     c                   eval      ilvstmgs=arbmgs
068800110701     c                   eval      ilvstfin=arbfin
068900110701     c                   eval      ilvstcgi=argcgi
069000110701     c                   eval      ilvstpoc=Vfil
069100110701     c                   if        arbddc>0
069200110701     c                   eval      ilvstdat=arbddc
069300110701     c                   else
069400110701     c                   eval      ilvstdat=wuda
069500110701     c                   endif
069600110701     c                   call      'FNLVSTR'
069700110701     c                   parm                    kpjba
069800110701     c                   parm                    fnlvstds
069900110701     c                   clear                   chiamaST
070000110701     c                   endif
070100110701     c
070200030605     C                   ENDSR
070300070129     c**********************************************************************
070400070129     C     imposta_arg   BEGSR
070500070129     c**********************************************************************
070600070129     c                   z-add     arbaas        argaas
070700070129     c                   z-add     arblnp        arglnp
070800070129     c                   z-add     arbnrs        argnrs
070900070129     c                   z-add     arbnsp        argnsp
071000070129     c                   z-add     arblna        arglna
071100070129     c                   z-add     vfil          argpoc
071200070129     c                   move      varggir       argcgi
071300070205     c                   if        varggir <> *blank
071400070205     c                   move      'M'           argtgi
071500070205     c                   else
071600070205     c                   move      *blank        argtgi
071700070205     c                   endif
071800070129     C                   ENDSR
071900030715     c**********************************************************************
072000070205     C     spalmaopz1    BEGSR
072100030715     c**********************************************************************
072200030715     c                   do        *hival        recor             4 0
072300030715     c     recor         chain     video2                             93
072400030715     c   93              leave
072500030715     c                   move      '1'           v1csce
072600030715     c                   update    video2
072700030715     c                   enddo
072800030715     C                   ENDSR
072900070125     c**********************************************************************
073000070125     c* carica sfl partendo da fnarb
073100070125     c**********************************************************************
073200070125     C     Carvideo20    BEGSR
073300070125     C                   IF        WrkCarS01 = *ON
073400070125     C                   EVAL      snrr1   = 0
073500070125     C                   EVAL      *IN90 = *ON
073600070125     C                   EVAL      *IN91 = *OFF
073700070125     C                   WRITE     video2c
073800070125     C                   EVAL      *IN90 = *OFF
073900070131      * gestione eventuali filiali in £6
074000070131     c                   do        *hival        £6                3 0
074100070131     c                   if        fil£6(£6) > 0
074200070131     c                   z-add     fil£6(£6)     vfil
074300070131     c                   else
074400070131     c                   z-add     fil£6(1)      vfil
074500070131     c                   leave
074600070131     c                   endif
074700070131     c     vfil          setll     fnarb05l
074800070125     c                   do        *hival
074900070131     c     vfil          reade     fnarb05l                               97
075000070125     c   97              leave
075100070131
075200070131      * filtri
075300070131
075400070131      * bolle da escludere sempre
075500070131     c                   if        arbfbc = '*' or arbfbc = 'B' or
075600070201     c                             arbfbc = 'G' or arbndc > 0
075700070131     c                   iter
075800070131     c                   endif
075900070131
076000070201      * con C.A. aperta
076100070201     c                   clear                   fidn12ds
076200070201     c                   eval      i12aas = arbaas
076300070201     c                   eval      i12lnp = arblnp
076400070201     c                   eval      i12nrs = arbnrs
076500070201     c                   eval      i12nsp = arbnsp
076600070201     c                   eval      i12fel = 'E'
076700070201     c                   eval      i12fan = 'E'
076800070201     c                   eval      i12fch = 'E'
076900070201     c                   call      'FIDN12R'
077000070201     c                   parm                    fidn12ds
077100070201      **
077200070201      * se c'è almeno una C.A. aperta
077300070201      **
077400070201     c     o12nca        ifgt      0
077500070201     c                   iter
077600070201     c                   end
077700070201     c
077800070201      * parzializzazione per localita richiesto range a video
077900070201     c                   if        vdaloc <> *blank and valoc <> *blank
078000070201     c                             and (arblod < vdaloc or arblod > valoc)
078100070201     c                   iter
078200070201     c                   endif
078300070607      * parzializzazione per nome destinatario
078400070607     c                   if        vdesti <> *blank
078500070607     C                   clear                   pos               2 0
078600070607     C                   eval      pos = %scan(%trim(Vdesti):arbrsd)
078700070607     C                   if        pos = *zeros
078800070607     C                   iter                                                   *RECORD NON VALIDO
078900070607     C                   endif
079000070607     c                   endif
079100070607      *
079200070131      * abilitazione alla consegna
079300070321     c*mm                if        vcons = 'I' and arbama < 1 and arbdam = 0
079400070321     c*mm                iter
079500070321     c*mm                else
079600070321     c*mm                if        vcons = 'E' and (arbama > 0 or arbdam > 0)
079700070321     c*mm                iter
079800070321     c*mm                endif
079900070321     c*mm                endif
080000070131      *
080100070201      * abilitate incasso
080200070131      *
080300070321     c*mm                if        vinc = 'I' and (arbacc <>'S' or arbaca <>'S')
080400070321     c*mm                iter
080500070321     c*mm                else
080600070321     c*mm                if        vinc = 'E' and arbacc = 'S' and arbaca = 'S'
080700070321     c*mm                iter
080800070321     c*mm                endif
080900070321     c*mm                endif
081000070131
081100070131      *
081200070131      * non richieste bolle con giro impostato
081300070125      *verifico se esiste fiarg,e se ha già un giro impostato non considero
081400070131     c                   if        vgiro = *blank
081500070207     c     karg1         chain(n)  fiarg01l
081600070125     c                   if        %found(fiarg01l) and argcgi <> *blank
081700070125     c                   iter
081800070125     c                   endif
081900070131     c                   endif
082000070125
082100070727      * richieste tutte le bolle con o sensza giro
082200070727     c                   clear                   argcgiw
082300070727     c                   if        vgiro = '9999999999'
082400070727     c     karg1         chain(n)  fiarg01l
082500070727     c                   if        %found(fiarg01l)
082600070727     c                   movel     argcgi        argcgiw
082700070727     c                   endif
082800070727     c                   endif
082900070129      * rientri
083000070201     c                   if        vrie = 'I' and arbfbr = *blank
083100070201     c                   iter
083200070201     c                   else
083300070201     c                   if        vrie = 'E' and arbfbr <> *blank
083400070201     c                   iter
083500070201     c                   endif
083600070201     c                   endif
083700070129      * fermo deposito
083800070201     c                   if        vferd = 'I' and arbffd = *blank
083900070201     c                   iter
084000070201     c                   else
084100070201     c                   if        vferd = 'E' and arbffd <> *blank
084200070201     c                   iter
084300070201     c                   endif
084400070201     c                   endif
084500070129
084600070131      * conti servizio
084700070201     c                   if        vcser = 'E'
084800070201     c                   move      '3A'          tblcod
084900070201     c                   movel     arbcbo        tblkey
085000070201     c     ktab          chain     tabel00f
085100070201     c                   if        %found(tabel00f)
085200070201     c                   movel     tbluni        ds3a
085300070201     c                   if        §3arbl = 'C'
085400070201     c                   iter
085500070201     c                   endif
085600070201     c                   endif
085700070201     c                   else
085800070201     c                   if        vcser = 'I'
085900070201     c                   move      '3A'          tblcod
086000070201     c                   movel     arbcbo        tblkey
086100070201     c     ktab          chain     tabel00f
086200070201     c                   if        %found(tabel00f)
086300070201     c                   movel     tbluni        ds3a
086400070201     c                   if        §3arbl <> 'C'
086500070201     c                   iter
086600070201     c                   endif
086700070201     c                   endif
086800070201     c                   endif
086900070201
087000070201     c                   endif
087100070131
087200070209     c
087300070209     c* Bolle abbinate
087400070209     c                   clear                   abb
087500070209     c                   if        vabb <> *blank
087600070209     C* 1) DAL FLAG ABILITAZIONE ARRIVI(ARBAMA=3)
087700070209    2C                   select
087800070209     c                   when      arbama = 3
087900070209     C                   move      'A'           abb
088000070209     C*
088100070209     C* 2) DAL FLAG ABILITAZIONE PARTENZE (ARBAMP=1)
088200070209     c                   when      arbama = 1 and arbamp = 1
088300070209     C                   move      'A'           abb
088400070209     C*
088500070209     C* 3) SE LOCALE SEMPRE ABBINATO
088600070209     c                   when      arbtfp=arbtfa  or
088700070209     c                             arblnp=arblna
088800070209     C                   move      'A'           abb
088900070209     C*
089000070209   X2C                   other
089100070209     C* REPERISCO INFORMAZIONE SE FOGLIO E' ABBINATO O MENO
089200070209     C                   clear                   fnlv53ds
089300070209     C                   movel     'A'           d53tfo
089400070209     C                   movel     simfel        d53fel
089500070209     C                   movel     arbnfv        d53nfv
089600070209     C                   movel     arbfle        d53fgs
089700070209     C                   movel     arbfle        d53flf
089800070209     C                   movel     1             d53npg
089900070209     C                   movel     'B'           d53abb
090000070209     C                   movel     arblna        d53lna
090100070209     C                   movel     arbtfa        d53tfa
090200070209     C                   call      'FNLV53R'
090300070209     C                   parm                    fnlv53ds
090400070209     C                   if        d53err = ' ' and d53nfa > 0
090500070209     C                   move      'A'           abb
090600070209     C                   endif
090700070209     C*
090800070209    2C                   endsl
090900070209     C* E - escludi non abbinate
091000070209    2C                   if        vabb = 'E' and abb <> 'A'
091100070209    3C                   iter
091200070209    3C                   endif
091300070209     C* I - Includi non abbinate
091400070209    2C                   if        vabb = 'I' and abb = 'A'
091500070209    3C                   iter
091600070209    3C                   endif
091700070209
091800070209    3C                   endif
091900070209      *
092000070131      * ddt/si
092100070201     c                   if        vddt  = 'E'
092200070201     c                   move      'A'           trck
092300070201     c     kar4          chain     fiar401l
092400070201     c                   if        %found(fiar401l)
092500070201     c                   movel     ar4not        dsbl4a
092600070201     c                   if        §b4abm = 'P' or §b4abm = 'Q' or
092700070201     c                             §b4abm = 'C' or §b4abm = 'S'
092800070201     c                   iter
092900070201     c                   endif
093000070201     c                   endif
093100070201     c                   endif
093200070131
093300070201     C                   move      *blank        V1csce
093400070125     c                   movel     arblod        vlod
093500070125     c                   movel     arbind        vind
093600070423     c                   eval      vlodind = vlod + arbcad + vind
093700070423     c*                  movel     vlod          vlodind
093800070423     c*                  move      vind          vlodind
093900070206     c                   movel     arbprd        vprov
094000070125     c                   movel     arbrsd        vrsd
094100070125     c                   movel     arbtc1        vtc1
094200070125     c                   movel     arbtc2        vtc2
094300070125     c                   z-add     arbpkb        vpes
094400070125     c                   z-add     arbvlb        vvol
094500070125     c                   z-add     arbaas        vaas
094600070125     c                   z-add     arblnp        vlnp
094700070125     c                   z-add     arbnrs        vnrs
094800070125     c                   z-add     arbnsp        vnsp
094900070220     c                   movel     arbcbo        vcbo
095000070727     c                   movel     argcgiw       vgir
095100070125      *esco per raggiunta massima capacità sfl
095200070125     c                   if        snrr1 > 9990
095300070125     c                   leave
095400070125     c                   endif
095500070125
095600070125     C                   EVAL      snrr1 = snrr1 + 1
095700070125     C                   WRITE     video2
095800070125     C                   ENDDO
095900070131     C                   enddo
096000070125
096100070125     C                   IF        snrr1  > 0
096200070125     C                   EVAL      *IN91 = *ON
096300070125     C                   MOVE      1             OK                1 0
096400070125     C                   ELSE
096500070125     C                   MOVE      0             OK                1 0
096600070125     C                   ENDIF
096700070125     C                   EVAL      nrr1  = 1
096800070125     C                   ENDIF
096900070125     C                   EVAL      WrkCarS01 = *OFF
097000070129     C                   EVAL      lasnrr1  = snrr1
097100070125     C                   EVAL      snrr1  = 1
097200070125     C                   EVAL      csrrrn = 1
097300070125     C                   ENDSR
097400070125     c**********************************************************************
097500070125     c* carica sfl partendo da fiarg con giro impostato
097600070125     c**********************************************************************
097700070125     C     Carvideo21    BEGSR
097800070125     C                   IF        WrkCarS01 = *ON
097900070125     C                   EVAL      snrr1   = 0
098000070125     C                   EVAL      *IN90 = *ON
098100070125     C                   EVAL      *IN91 = *OFF
098200070125     C                   WRITE     video2c
098300070125     C                   EVAL      *IN90 = *OFF
098400070403     c     karg          setll     fiarg52j
098500070125     c                   do        *hival
098600070403     c     karg          reade     fiarg52j                               97
098700070125     c   97              leave
098800070205      * filtri
098900070205
099000070205      * bolle da escludere sempre
099100070205     c                   if        arbfbc = '*' or arbfbc = 'B' or
099200070205     c                             arbfbc = 'G' or arbndc > 0
099300070205     c                   iter
099400070205     c                   endif
099500070205
099600070205     c                   if        arbdcm  = 0 or
099700070205     c                             (arbicc = ' ' or arbicc = 'R') or
099800070205     c                             (arbica = ' ' or arbica = 'R')
099900070205     c                   else
100000070205     c                   iter
100100070205     c                   endif
100200070205
100300070205      * con C.A. aperta
100400070205     c                   clear                   fidn12ds
100500070205     c                   eval      i12aas = arbaas
100600070205     c                   eval      i12lnp = arblnp
100700070205     c                   eval      i12nrs = arbnrs
100800070205     c                   eval      i12nsp = arbnsp
100900070205     c                   eval      i12fel = 'E'
101000070205     c                   eval      i12fan = 'E'
101100070205     c                   eval      i12fch = 'E'
101200070205     c                   call      'FIDN12R'
101300070205     c                   parm                    fidn12ds
101400070205      **
101500070205      * se c'è almeno una C.A. aperta
101600070205      **
101700070205     c     o12nca        ifgt      0
101800070205     c                   iter
101900070205     c                   end
102000070205     c
102100070607      * parzializzazione per nome destinatario
102200070607     c                   if        vdesti <> *blank
102300070607     C                   clear                   pos               2 0
102400070607     C                   eval      pos = %scan(%trim(Vdesti):arbrsd)
102500070607     C                   if        pos = *zeros
102600070607     C                   iter                                                   *RECORD NON VALIDO
102700070607     C                   endif
102800070607     c                   endif
102900070607      *
103000070205      * abilitazione alla consegna
103100070321     c*mm                if        vcons = 'I' and arbama < 1 and arbdam = 0
103200070321     c*mm                iter
103300070321     c*mm                else
103400070321     c*mm                if        vcons = 'E' and (arbama > 0 or arbdam > 0)
103500070321     c*mm                iter
103600070321     c*mm                endif
103700070321     c*mm                endif
103800070205      *
103900070205      * abilitate incasso
104000070205      *
104100070321     c*mm                if        vinc = 'I' and (arbacc <>'S' or arbaca <>'S')
104200070321     c*mm                iter
104300070321     c*mm                else
104400070321     c*mm                if        vinc = 'E' and arbacc = 'S' and arbaca = 'S'
104500070321     c*mm                iter
104600070321     c*mm                endif
104700070321     c*mm                endif
104800070205
104900070205      * rientri
105000070205     c                   if        vrie = 'I' and arbfbr = *blank
105100070205     c                   iter
105200070205     c                   else
105300070205     c                   if        vrie = 'E' and arbfbr <> *blank
105400070205     c                   iter
105500070205     c                   endif
105600070205     c                   endif
105700070205      * fermo deposito
105800070205     c                   if        vferd = 'I' and arbffd = *blank
105900070205     c                   iter
106000070205     c                   else
106100070205     c                   if        vferd = 'E' and arbffd <> *blank
106200070205     c                   iter
106300070205     c                   endif
106400070205     c                   endif
106500070205
106600070205      * conti servizio
106700070205     c                   if        vcser = 'E'
106800070205     c                   move      '3A'          tblcod
106900070205     c                   movel     arbcbo        tblkey
107000070205     c     ktab          chain     tabel00f
107100070205     c                   if        %found(tabel00f)
107200070205     c                   movel     tbluni        ds3a
107300070205     c                   if        §3arbl = 'C'
107400070205     c                   iter
107500070205     c                   endif
107600070205     c                   endif
107700070205     c                   else
107800070205     c                   if        vcser = 'I'
107900070205     c                   move      '3A'          tblcod
108000070205     c                   movel     arbcbo        tblkey
108100070205     c     ktab          chain     tabel00f
108200070205     c                   if        %found(tabel00f)
108300070205     c                   movel     tbluni        ds3a
108400070205     c                   if        §3arbl <> 'C'
108500070205     c                   iter
108600070205     c                   endif
108700070205     c                   endif
108800070205     c                   endif
108900070205
109000070205     c                   endif
109100070205
109200070209     c* Bolle abbinate
109300070209     c                   clear                   abb
109400070209     c                   if        vabb <> *blank
109500070209     C* 1) DAL FLAG ABILITAZIONE ARRIVI(ARBAMA=3)
109600070209    2C                   select
109700070209     c                   when      arbama = 3
109800070209     C                   move      'A'           abb
109900070209     C*
110000070209     C* 2) DAL FLAG ABILITAZIONE PARTENZE (ARBAMP=1)
110100070209     c                   when      arbama = 1 and arbamp = 1
110200070209     C                   move      'A'           abb
110300070209     C*
110400070209     C* 3) SE LOCALE SEMPRE ABBINATO
110500070209     c                   when      arbtfp=arbtfa  or
110600070209     c                             arblnp=arblna
110700070209     C                   move      'A'           abb
110800070209     C*
110900070209   X2C                   other
111000070209     C* REPERISCO INFORMAZIONE SE FOGLIO E' ABBINATO O MENO
111100070209     C                   clear                   fnlv53ds
111200070209     C                   movel     'A'           d53tfo
111300070209     C                   movel     simfel        d53fel
111400070209     C                   movel     arbnfv        d53nfv
111500070209     C                   movel     arbfle        d53fgs
111600070209     C                   movel     arbfle        d53flf
111700070209     C                   movel     1             d53npg
111800070209     C                   movel     'B'           d53abb
111900070209     C                   movel     arblna        d53lna
112000070209     C                   movel     arbtfa        d53tfa
112100070209     C                   call      'FNLV53R'
112200070209     C                   parm                    fnlv53ds
112300070209     C                   if        d53err = ' ' and d53nfa > 0
112400070209     C                   move      'A'           abb
112500070209     C                   endif
112600070209     C*
112700070209    2C                   endsl
112800070209     C* E - escludi non abbinate
112900070209    2C                   if        vabb = 'E' and abb <> 'A'
113000070209    3C                   iter
113100070209    3C                   endif
113200070209     C* I - Includi non abbinate
113300070209    2C                   if        vabb = 'I' and abb = 'A'
113400070209    3C                   iter
113500070209    3C                   endif
113600070209
113700070209    3C                   endif
113800070209      *
113900070205      * ddt/si
114000070205     c                   if        vddt  = 'E'
114100070205     c                   move      'A'           trck
114200070205     c     kar4          chain     fiar401l
114300070205     c                   if        %found(fiar401l)
114400070205     c                   movel     ar4not        dsbl4a
114500070205     c                   if        §b4abm = 'P' or §b4abm = 'Q' or
114600070205     c                             §b4abm = 'C' or §b4abm = 'S'
114700070205     c                   iter
114800070205     c                   endif
114900070205     c                   endif
115000070205     c                   endif
115100070205
115200070125
115300070129     C                   MOVE      *BLANK        V1CSCE
115400070125     c                   movel     arblod        vlod
115500070125     c                   movel     arbind        vind
115600070423     c                   eval      vlodind = vlod + arbcad + vind
115700070423     c*                  movel     vlod          vlodind
115800070423     c*                  move      vind          vlodind
115900070206     c                   movel     arbprd        vprov
116000070125     c                   movel     arbrsd        vrsd
116100070125     c                   movel     arbtc1        vtc1
116200070125     c                   movel     arbtc2        vtc2
116300070125     c                   z-add     arbpkb        vpes
116400070125     c                   z-add     arbvlb        vvol
116500070125     c                   z-add     arbaas        vaas
116600070125     c                   z-add     arblnp        vlnp
116700070125     c                   z-add     arbnrs        vnrs
116800070125     c                   z-add     arbnsp        vnsp
116900070220     c                   movel     arbcbo        vcbo
117000070727     c                   movel     argcgi        vgir
117100070125      *esco per raggiunta massima capacità sfl
117200070125     c                   if        snrr1 > 9990
117300070125     c                   leave
117400070125     c                   endif
117500070125
117600070125     C                   EVAL      snrr1 = snrr1 + 1
117700070125     C                   WRITE     video2
117800070125     C                   ENDDO
117900070125
118000070125     C                   IF        snrr1  > 0
118100070125     C                   EVAL      *IN91 = *ON
118200070125     C                   MOVE      1             OK                1 0
118300070125     C                   ELSE
118400070125     C                   MOVE      0             OK                1 0
118500070125     C                   ENDIF
118600070125     C                   EVAL      nrr1  = 1
118700070125     C                   ENDIF
118800070125     C                   EVAL      WrkCarS01 = *OFF
118900070206     C                   EVAL      lasnrr1  = snrr1
119000070125     C                   EVAL      snrr1  = 1
119100070125     C                   EVAL      csrrrn = 1
119200070125     C                   ENDSR
119300011227     c**********************************************************************
119400011227     c* gestione sfl
119500011227     c**********************************************************************
119600000613     C     GestioneSFL   BEGSR
119700000627      *
119800000627     C                   EVAL      WrkEofS01 = *OFF
119900030605      * Elaborazione opzioni.
120000011228     c*
120100030605     c                   do        *hival        X                 4 0
120200030605     c     X             CHAIN     video2                             50
120300030605     C   50              LEAVE
120400070205     c   kc              goto      endrut
120500030604     C                   IF        v1csce <> *blanks
120600030605     c                   exsr      opzione
120700030611
120800030611     c                   move      *blank        v1csce
120900030605     c                   update    video2
121000030611
121100011228     C                   ENDIF
121200030605     C                   enddo
121300070124      * abblenca spia x gestione scelta multipla per prossima selezione
121400070124     c                   move      *blank        multi
121500070124
121600070206     c                   if        modalita = '1' or modalita = '9'
121700030710     c                   eval      wrkcars01 = *on
121800030710     c                   endif
121900000627      *
122000070205     C     endrut        ENDSR
122100011227     c**********************************************************************
122200000607     C     *INZSR        BEGSR
122300011227     c**********************************************************************
122400000607      *
122500000607     C     *ENTRY        PLIST
122600000607     C                   PARM                    Kpjba
122700070125     c                   clear                   fidg06ds
122800070123     C                   if        kpjbu <> *blank
122900070123     c                   movel     kpjbu         fidg06ds
123000070123     c                   endif
123100070123      *
123200030604     C                   Z-ADD     1             CODUT
123300030604     C                   CALL      'X§PARUT'
123400030604     C                   PARM                    UTEDSE
123500030604     C                   MOVEL     REC80         CNCR80
123600030703     C                   MOVEL     Ragut         rsut
123700030703     C                   MOVEL     knsif         vknsif
123800030703     C                   MOVEL     knmus         vknmus
123900030702      *reperimento data
124000030702     C                   TIME                    W0120            14 0
124100030702     C                   MOVE      W0120         WDAT              8 0
124200030702     C*
124300030702     C                   Z-ADD     WDAT          G02DAT
124400030702     C                   MOVEL     *BLANK        G02ERR
124500030702     C                   CALL      'XSRDA8'
124600030702     C                   PARM                    WLBDA8
124700030702     C* UDATE A 8 IN AAAA/MM/GG
124800030702     C                   Z-ADD     G02INV        WUDA              8 0
124900030717     C                   Z-ADD     G02INV        WUDAte            8 0
125000030606      *profilo di filiale attivo indicatore di protezione campi non manutenzionabili
125100030617     C                   CLEAR                   Tibs36Ds
125200030617     C                   EVAL      I36ute = knmus
125300030617     C                   CALL      'TIBS36R'
125400030617     C                   PARM                    tibs36ds
125500070123      *
125600070123     c                   movel     o36pou        vfil
125700000607      *
125800070125     C     Karg          klist
125900070124     C                   kfld                    vfil
126000070124     C                   kfld                    vgiro
126100070123      *
126200070124     C     Karb          klist
126300070124     C                   kfld                    vaas
126400070124     C                   kfld                    vlnp
126500070124     C                   kfld                    vnrs
126600070124     C                   kfld                    vnsp
126700070201      *
126800070201     C     Kar4          klist
126900070201     C                   kfld                    arbaas
127000070201     C                   kfld                    arblnp
127100070201     C                   kfld                    arbnrs
127200070201     C                   kfld                    arbnsp
127300070201     c                   kfld                    trck              1
127400070124      *
127500070125     C     Karg1         klist
127600070125     C                   kfld                    arbaas
127700070125     C                   kfld                    arblnp
127800070125     C                   kfld                    arbnrs
127900070125     C                   kfld                    arbnsp
128000070125      *
128100070124     C     Kgrg          klist
128200070124     C                   kfld                    vfil
128300070124     C                   kfld                    varggir
128400070201      *
128500070201     C     Ktab          klist
128600070201     C                   kfld                    tblkut
128700070201     C                   kfld                    tblcod
128800070201     C                   kfld                    tblkey
128900070201     C                   z-add     1             tblkut
129000070205      * carico filiali £1
129100070205     c                   eval      kpjbus = kpjbu
129200070205     C                   clear                   trul06ds
129300070205     C                   movel     '£1'          d06cod
129400070205     C                   movel     vfil          d06key
129500070205     C                   movel     trul06ds      kpjbu
129600070205     C                   CALL      'TRUL06R'
129700070205     C                   parm                    kpjba
129800070205     C                   movel     kpjbu         trul06ds
129900070205     C                   movea     lin           fil£1
130000070205     c                   eval      kpjbu = kpjbus
130100030714
130200011227     C                   ENDSR
130300070125      *--------------------------------------------------------------------------------------------*
130400070125      * ordina - riscrive il sfl ordinato per località
130500070125      *--------------------------------------------------------------------------------------------*
130600070125     C     ordina        BEGSR
130700070125     C                   setoff                                       72
130800070125      * IMPOSTO ENTRATA AD 'A'
130900070125     C                   EVAL      snrr1 = 0
131000070125      *
131100070125     C                   CLEAR                   QLGSCB
131200070125     C                   CLEAR                   QLGSCB00
131300070125
131400070125     ** 1 campo chiave.
131500070125     C                   EVAL      QLGNBRK    = 1
131600070423     ** localita/indirizzoè a posizione 1, carattere, 71 byte,
131700070125     C                   EVAL      QLGSP      = 1
131800070206     C                   EVAL      QLGSS      = %SIZE(vlodind)
131900070125     C                   EVAL      QLGDT      = carattere
132000070125     C                   EVAL      QLGSO      = Ascendente
132100070125     C                   EVAL      QLGKL(1)   = QLGSKL
132200070125
132300070125      * Load other sort parameters.
132400070125     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
132500070125     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
132600070125     C                   EVAL      QLGRT     = 8
132700070125     C                   EVAL      QLGOKL    = 80
132800070125     C                   EVAL      QLGLKE    = 16
132900070125     C                   EVAL      QLGLSS    = 290
133000070125
133100070125      * Initialize Sort I/O API fields.
133200070125     C                   EVAL      QLGRL00  = QLGRL
133300070125     C                   EVAL      QLGRC00  = 1
133400070125     C                   CLEAR                   QUSEI
133500070125     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
133600070125
133700070125      * First step - Initialize the sort routine.
133800070125     C                   CALL      'QLGSORT'
133900070125     C                   PARM                    QLGSCB
134000070125     C                   PARM                    NotUsed
134100070125     C                   PARM                    NotUsed
134200070125     C                   PARM                    SizeList
134300070125     C                   PARM                    ReturnSize
134400070125     C                   PARM                    QUSEC
134500070125
134600070125      * Next step - Write records to I/O routine.
134700070125     C                   EVAL      QLGRT00 = Put
134800070125
134900070125     C                   DO        *hival        cx                4 0
135000070125
135100070125     C     cx            CHAIN     video2                             50
135200070125     c   50              leave
135300070125     ** Solo le righe con Selected = 'Y' sono riordinate,
135400070125     ** quindi per fare un ordinamento di tutte le righe
135500070125     ** metto 'Y' sempre.
135600070125     C                   EVAL      Selected = 'Y'
135700070125
135800070125     C                   CLEAR                   QUSEI
135900070125     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
136000070125     C                   CALL      'QLGSRTIO'
136100070125     C                   PARM                    QLGSCB00
136200070125     C                   PARM                    SflRcd
136300070125     C                   PARM                    NotUsed
136400070125     C                   PARM                    SizeList
136500070125     C                   PARM                    NotUsed
136600070125     C                   PARM                    QUSEC
136700070125
136800070125     C                   ENDDO
136900070125
137000070125      * Next step - Signal end of input, clear subfile for reload.
137100070125     C                   EVAL      QLGRT00 = EndPut
137200070125     C                   CLEAR                   QUSEI
137300070125     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
137400070125
137500070125     C                   CALL      'QLGSRTIO'
137600070125     C                   PARM                    QLGSCB00
137700070125     C                   PARM                    SflRcd
137800070125     C                   PARM                    NotUsed
137900070125     C                   PARM                    SizeList
138000070125     C                   PARM                    NotUsed
138100070125     C                   PARM                    QUSEC
138200070125
138300070125     C                   SETON                                        90        *
138400070125     C                   WRITE     video2c                                      *
138500070125     C                   SETOFF                                       90        *
138600070125     C                   WRITE     video2z                                      *
138700070125
138800070125      * Final step - Write the records back to the subfile.
138900070125     C                   EVAL      QLGRT00 = Get
139000070129     C                   DO        lasnrr1       x
139100070125     C                   CLEAR                   QUSEI
139200070125     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
139300070125     C                   CALL      'QLGSRTIO'
139400070125     C                   PARM                    QLGSCB00
139500070125     C                   PARM                    NotUsed
139600070125     C                   PARM                    SflRcd
139700070125     C                   PARM                    QLGRL00
139800070125     C                   PARM                    NotUsed
139900070125     C                   PARM                    QUSEC
140000070125      *
140100070125     c                   z-add     x             snrr1
140200070125     C                   WRITE     video2
140300070125     C                   ENDDO
140400070125     C                   SETOFF                                       20        *
140500070125
140600070125      *
140700070125     C                   ENDSR
140800030714      *---------------------------------------------------------------------
140900070206     C**************************************************************************
141000070206     C*  Routine per gestione richiamo interrogazione bolle
141100070206     C**************************************************************************
141200070206     C     richbl        begsr
141300070206      *
141400070206     c                   movel     kpjbu         §kpjbu
141500070206      *
141600070206     c                   z-add     vaas          pa1aas
141700070206     c                   z-add     vlnp          pa1lnp
141800070206     c                   z-add     vnrs          pa1nrs
141900070206     c                   z-add     vnsp          pa1nsp
142000070206     c                   movel     ' '           pa1f03
142100070206     c                   movel     '1'           pa1flg
142200070206     c                   movel     param1        kpjbu
142300070206     c                   call      'FNLR36R'
142400070206     c                   parm                    kpjba
142500070206     c                   movel     kpjbu         param1
142600070206      *
142700070206     c                   movel     §kpjbu        kpjbu
142800070206      *
142900070206     c                   endsr
