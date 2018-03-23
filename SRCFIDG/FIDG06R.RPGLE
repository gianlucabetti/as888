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
005700141029     D fnlry09ds2    E DS
005800070705
005900011227     d data            ds            10
006000011228     d aa                      3      4
006100011228     d mm                      6      7
006200011228     d gg                      9     10
006300011228      *
006400070125     D caricada        S              1  0
006500070124     D fatto           S              1
006600070222     D riemetti        S              1
006700070124     D multi           S              1
006800070125     D modalita        S              1
006900070209     D abb             S              1
007000110701     D ChiamaST        S              1
007100070727     D argcgiw         S                   like(argcgi)
007200070125     D Savvarggir      S                   like(argcgi)
007300011227     D WrkEofS01       S              1
007400011227     D WrkCarS01       S              1
007500011228     D $VIDEO          S             10
007600030604     D snrr1           S              5i 0
007700070129     D lasnrr1         S              5i 0
007800030604     D SavOpzione      S                   like(v1csce)
007900030606     d dataiso         s               d   datfmt(*iso)
008000030606     d dataeur         s               d   datfmt(*eur)
008100030613     D WLBDA8          DS
008200030613     D  G02DAT                 1      8  0
008300030613     D  G02INV                 9     16  0
008400030613     D  G02ERR                17     17
008500030613     D  G02TGI                18     22  0
008600070131     d  fil£6          s              3  0 dim(30)
008700070131     d  fil£1          s              3  0 dim(30)
008800070125      *---------------------------------------------------------------------
008900070125      * specifiche per ordinamento forzato sfl
009000070125     d NotUsed         S             16A
009100070125     d ReturnSize      S              9B 0
009200070125     d SizeList        S              9B 0
009300070125     d WrkSort         S              1
009400070125     d*---
009500070125     d MaxKey          C                   2
009600070125     d Ascendente      C                   1
009700070125     d Discendente     C                   2
009800070125     d Carattere       C                   6
009900070125     d Put             C                   1
010000070125     d EndPut          C                   2
010100070125     d Get             C                   3
010200070125     d Numerico        C                   8
010300070125     d*---
010400070125     d SflRcd          DS
010500070206     d  vlodind
010600070125     d  vlod
010700070727     d  vprov
010800070125     d  Vind
010900070125     d  Vpes
011000070125     d  Vrsd
011100070727     d  vgir
011200070125     d  Vvol
011300070125     d  Vtc1
011400070125     d  Vtc2
011500070125     d  VAAS
011600070125     d  VLNP
011700070125     d  VNRS
011800070125     d  VNSP
011900070125     d  Selected                      1A
012000070125
012100070125      /COPY QSYSINC/QRPGLESRC,QLGSORT
012200070125     d QLGKL                         16    DIM(MaxKey)
012300070125     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
012400070125     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
012500070125     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
012600070125     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
012700070125
012800070125      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
012900070125      /COPY QSYSINC/QRPGLESRC,QUSEC
013000030701      *---------------- ----------------------------------------------------
013100070124      * gestione singola spedizione chiamata esterna
013200070124     c                   if        d6nsp <> 0
013300070124     c                   move      d6aas         vaas
013400070124     c                   move      d6lnp         vlnp
013500070124     c                   move      d6nrs         vnrs
013600070124     c                   move      d6nsp         vnsp
013700070124     c                   exsr      singola
013800070124      *
013900070124     c                   else
014000070124      *da menu
014100030604      *---------------- Gestione formato video video01----------------------
014200030604     C                   EXSR      Inzvideo1
014300030604     C                   DOU       $Video <> 'VIDEO1'
014400000608      *----------- Visualizzo dati di output in caso di errori.-------------
014500000608     C                   IF        *IN99
014600000608     C                   EVAL      *IN99 = *OFF
014700030604     C                   WRITE     video1
014800000608     C                   EVAL      *IN99 = *ON
014900000608     C                   ENDIF
015000000608      *
015100030702     c     emetti        tag
015200030604     C                   EXFMT     video1
015300000608      *
015400011227     C                   SELECT
015500030612     C                   WHEN      *INKC
015600011227     C                             OR
015700011227     C                             *INKL
015800011227     C                   EVAL      $Video = *BLANKS
015900011227
016000070205      * ricerca interrogazione giri
016100070205     c                   when      *inkg
016200070205     c                   eval      kpjbus = kpjbu
016300070205     c                   clear                   fidg09ds
016400070205     c                   move      'P01'         d09iop0
016500070205     c                   movel     vfil          d09ifgs
016600070205     c                   z-add     wuda          d09idat
016700070705     c                   eval      d09itug = 'C'                                tipo util. consegna
016800070205     c                   movel     fidg09ds      kpjbu
016900070205     c                   call      'FIDG09R'
017000070205     c                   parm                    kpjba
017100070205     c  n11              movel     kpjbu         fidg09ds
017200070205     c  n11              movel     d09ocgi       vgiro
017300070205     c                   eval      kpjbu = kpjbus
017400070205     c                   goto      emetti
017500070205
017600011227     C                   WHEN      *INKE
017700030604     C                   EXSR      Inzvideo1
017800011227     C                   WHEN      *INKB
017900030604     C                   EXSR      Chkvideo1
018000011227     C                   OTHER
018100030604     C                   EXSR      Chkvideo1
018200030716     c   44              goto      emetti
018300030604     C  N99              EXSR      Wrkvideo2c
018400031020     C     OK            CABEQ     0             EMETTI
018500011227     C                   ENDSL
018600011227      *
018700011227     C                   ENDDO
018800070124      *
018900070124     c                   endif
019000011227      *
019100011227     C                   EXSR      Uscita
019200011227     c**********************************************************************
019300011227     c* uscita
019400011227     c**********************************************************************
019500011227     C     Uscita        BEGSR
019600110701     c                   clear                   fnlvstds
019700110701     c                   eval      ilvstTLA='C'
019800110701     c                   call      'FNLVSTR'
019900110701     c                   parm                    kpjba
020000110701     c                   parm                    fnlvstds
020100011227      *
020200011227     C                   EVAL      *INLR = *ON
020300011227     C                   RETURN
020400011227      *
020500011227     C                   ENDSR
020600011227     c**********************************************************************
020700011227     c* gestione sfl
020800011227     c**********************************************************************
020900030604     C     Wrkvideo2c    BEGSR
021000000627      *
021100011227     C                   EVAL      WrkCarS01 = *ON
021200030604     C                   EVAL      $Video = 'VIDEO2C'
021300011227      *
021400030604     C                   DOU       $Video <> 'VIDEO2C'
021500070125      * sbinamento routine di caricamento se il valore caricada = 0 il file
021600070125      * di partenza è FNARB se è uguale a 1 significa che ho impostato
021700070129      * a video il GIRO quindi miro solo le bolle con quel giro su ARG
021800070125     c                   if        caricada = 0
021900070125     C                   EXSR      Carvideo20
022000070129      * ordina il sfl caricato per località solo se presenti rekord
022100070206     C*    OK            CABEQ     0             TORNA
022200070206     C*                  EXSR      ordina
022300070129
022400070125     c                   else
022500070125     c                   exsr      carvideo21
022600070125     c                   endif
022700000627      *
022800031020     C     OK            CABEQ     0             TORNA
022900070206     C                   EXSR      ordina
023000030604     C                   WRITE     video2z
023100030604     C                   EXFMT     video2c
023200011227     C                   SELECT
023300011228     c* fine
023400011227     C                   WHEN      *INKC=*ON
023500011227     C                   EVAL      $VIDEO=*BLANKS
023600011228     c* guida
023700011227     C                   WHEN      *INKL=*ON
023800030604     C                   EVAL      $VIDEO='VIDEO1'
023900030715
024000030715     C                   WHEN      *INKa=*ON
024100070205     C                   exsr      spalmaopz1
024200011227
024300011227     C                   OTHER
024400011227
024500011227     C                   EXSR      GestioneSFL
024600011227     C                   ENDSL
024700011228
024800011227     C                   ENDDO
024900031020     C     torna         ENDSR
025000011227     c**********************************************************************
025100011227     c* Controlli video R01.
025200011227     c**********************************************************************
025300030604     C     Chkvideo1     BEGSR
025400000609      *
025500070129     c                   setoff                                       9495
025600000608     C                   EVAL      *IN99 = *OFF
025700030702     C                   EVAL      *IN44 = *OFF
025800070131      * profilo non edp verifico £1
025900070131     c                   if        %subst(knmus:1:3) <> 'EDP'
026000070131     c     vfil          lookup    fil£1                                  92
026100070205     c  n92              seton                                        44
026200070131     c                   endif
026300070131
026400070131      * carico filiali £6
026500070131     c                   eval      kpjbus = kpjbu
026600070131     C                   clear                   trul06ds
026700070131     C                   movel     '£6'          d06cod
026800070131     C                   movel     vfil          d06key
026900070131     C                   movel     trul06ds      kpjbu
027000070131     C                   CALL      'TRUL06R'
027100070131     C                   parm                    kpjba
027200070131     C                   movel     kpjbu         trul06ds
027300070131     C                   movea     lin           fil£6
027400070131     c                   eval      kpjbu = kpjbus
027500070131
027600070131     c                   if        vgiro <> *blank and vgiro <> '9999999999'
027700070125     c                   z-add     1             caricada
027800070125     c                   else
027900070125     c                   z-add     0             caricada
028000070125     c                   endif
028100070129      * se imposto località'DA'e località 'A'a blank la inserisco uguale
028200070129     c                   if        vdaloc <> *blank and valoc = *blank
028300070905     c                   eval      valoc = %trimr(vdaloc) + 'ZZZ'
028400070129     c                   endif
028500070205      * se imposto località'DA'e località 'A'con solo iniziale metto nel
028600070205      * secondo byte la 'Z' x la selezione
028700070205     c                   if        vdaloc <> *blank and valoc <> *blank and
028800070205     c                             %subst(valoc:2:1) = ' '
028900070905     c                   eval      valoc = %trimr(valoc) + 'ZZZ'
029000070205     c                   endif
029100070129      * vice versa località'A'e località 'DA'a blank la inserisco uguale
029200070129     c                   if        valoc <> *blank and vdaloc = *blank
029300070129     c                   movel     valoc         vdaloc
029400070129     c                   endif
029500070129      * verifico che lcalità'DA' sia congruente con località 'A'
029600070129     c                   if        vdaloc > valoc
029700070129     c                   seton                                        9499
029800070129     c                   endif
029900070129     c
030000000608      *
030100000608     C                   ENDSR
030200011227     c**********************************************************************
030300011227     c* inizializza r01
030400011227     c**********************************************************************
030500030604     C     Inzvideo1     BEGSR
030600000608
030700070123     c                   move      o36pou        vfil
030800070321     c*mm                move      *blank        vcons
030900070123     c                   move      *blank        vgiro
031000070123     c                   move      *blank        vdaloc
031100070123     c                   move      *blank        valoc
031200070131     c                   move      *blank        vrie
031300070131     c                   move      *blank        vcser
031400070321     c*mm                move      *blank        vinc
031500070131     c                   move      'E'           vferd
031600070220     c                   move      *blank        vddt
031700070209     c                   move      'E'           vabb
031800030604     C                   EXSR      Chkvideo1
031900000609
032000000608     C                   ENDSR
032100030604     c**********************************************************************
032200030605     C     opzione       BEGSR
032300030605     c**********************************************************************
032400030610     c                   move      v1csce        modalita
032500030605     c                   select
032600030710     c                   when      v1csce = '1'
032700070123     c                   exsr      singola
032800030717     c                   clear                   $msg
032900030717     c                   setoff                                       28
033000070206     c                   when      v1csce = '9'
033100070124     c                   seton                                        10
033200070123     c                   exsr      multipla
033300070124     c                   setoff                                       10
033400070123     c                   clear                   $msg
033500070124     c                   setoff                                       28
033600030605     c                   when      v1csce = '5'
033700070124     c                   seton                                        11
033800070206     c                   exsr      richbl
033900070124     c                   setoff                                       11
034000030605     c                   endsl
034100030604     C                   ENDSR
034200030717     c**********************************************************************
034300070123     C     singola       BEGSR
034400030717     c**********************************************************************
034500070222     c     sopra         tag
034600030717     c                   setoff                                       28
034700070124     c     karb          chain     fnarb01l                           98
034800070202     c                   clear                   varggira
034900070427     c                   clear                   vargerr
035000070427     c                   clear                   vargerrd
035100070427     c                   clear                   varglno
035200070427     c                   clear                   varglnod
035300070427      * richiamo pgm per valori attribuzione giro da tellus
035400070427     c                   clear                   fidg34ds
035500070427     c                   move      knsif         id34sif
035600070427     c                   move      *blank        id34tla
035700070427     c                   move      '2'           id34tip
035800070427     c                   move      arbaas        id34aas
035900070427     c                   move      arblnp        id34lnp
036000070427     c                   move      arbnrs        id34nrs
036100070427     c                   move      arbnsp        id34nsp
036200070427     c                   call      'FIDG34R'
036300070427     c                   parm                    fidg34ds
036400070427     c                   if        od34esito = *blank
036500070427     c                   movel     od34cgi       varggira
036600070427     c                   movel     od34err       vargerr
036700070427     c                   movel     od34errdes    vargerrd
036800070427     c                   movel     od34lno       varglno
036900070427     c                   movel     od34lnodes    varglnod
037000070202     c                   endif
037100030717     c                   if        not *in98
037200070123     c                   move      arbaas        varbaas
037300070123     c                   move      arblnp        varblnp
037400070123     c                   move      arbnrs        varbnrs
037500070123     c                   move      arbnsp        varbnsp
037600070125     c                   move      arbksc        varbccm
037700070220     c                   movel     arbcbo        varbcbo
037800070125     c                   movel     arbrsm        varbrsm
037900070125     c                   movel     arbrsd        varbrsd
038000070125     c                   movel     arbind        varbind
038100070125     c                   movel     arblod        varblod
038200070125     c                   movel     arbcad        varbcad
038300070427     c                   movel     arbprd        varbprd
038400070123     c                   move      arbtsp        varbtsp
038500070123     c                   move      arbpkb        varbpkb
038600070123     c                   move      arbncl        varbncl
038700070123     c                   move      arbvlb        varbvlb
038800070123     c                   move      arbgma        varbgma
038900070123     c                   move      arbgga        varbgga
039000070123     c                   move      arbgva        varbgva
039100070123     c                   move      arbtc1        varbtc1
039200070123     c                   move      arbtc2        varbtc2
039300070123     c                   move      arbtcr        varbtcr
039400070201     c                   clear                   varbdcr
039500070201     c                   if        arbdcr > 0
039600070201     c                   move      arbdcr        dataiso
039700070201     c                   move      dataiso       dataeur
039800070201     c                   move      dataeur       varbdcr
039900070201     c                   endif
040000070123     c                   move      arbhcr        varbhcr
040100070123     c                   move      arbfbr        varbfbr
040200070222     c                   clear                   varbffd
040300070222     c                   if        arbffd <> *blank
040400070222     c                   move      'SI'          varbffd
040500070222     c                   endif
040600070131     c                   clear                   var6ift
040700070131     c     karb          chain     fiar601l
040800070131     c                   if        %found(fiar601l)
040900070131     c                   z-add     ar6ift        var6ift
041000070131     c                   endif
041100070131      * c/assegno
041200070131     c                   clear                   var9cas
041300070131     c     karb          chain     fiar901l
041400070131     c                   if        %found(fiar901l)
041500070201     c                   z-add     ar9cas        var9cas
041600070131     c                   endif
041700070201      * seconda ragione sociale
041800070201     c                   clear                   varbrs2
041900070201     c                   movel     'D'           trck
042000070201     c     kar4          chain     fiar401l
042100070201     c                   if        %found(fiar401l)
042200070201     c                   movel     ar4not        varbrs2
042300070201     c                   endif
042400070201      * note
042500070201     c                   clear                   var4not
042600070201     c                   movel     '8'           trck
042700070201     c     kar4          chain     fiar401l
042800070201     c                   if        %found(fiar401l)
042900070201     c                   movel     ar4not        var4not
043000070201     c                   endif
043100070206     c                   clear                   var4not2
043200070206     c                   movel     '9'           trck
043300070206     c     kar4          chain     fiar401l
043400070206     c                   if        %found(fiar401l)
043500070206     c                   movel     ar4not        var4not2
043600070206     c                   endif
043700070123      *
043800070123     c                   clear                   varggir
043900070123     c                   clear                   vdesgir
044000070123     c                   clear                   vposint
044100070123      *
044200030717     c                   exsr      Gesvideo3
044300070222      *forza rilettura arb in caso aggiornamento bolla e conseguente
044400070222      * riemissione video
044500070222     c                   if        riemetti <> *blank
044600070222     c                   goto      sopra
044700070222     c                   endif
044800070222
044900030717     c                   endif
045000030717     C                   ENDSR
045100030605     c**********************************************************************
045200070123     C     multipla      BEGSR
045300030605     c**********************************************************************
045400070124     c                   setoff                                       28
045500070124     c     karb          chain     fnarb01l                           98
045600070124     c                   if        not *in98
045700070124     c                   if        multi = *blanks
045800070124     c                   move      arbind        varbind
045900070124     c                   move      arblod        varblod
046000070202     c                   movel     arbcad        varbcad
046100070206     c                   movel     arbprd        varbprd
046200070202      *
046300070202     c                   clear                   varggir
046400070202     c                   clear                   vdesgir
046500070202     c                   clear                   vposint
046600070202      *
046700030605     c                   exsr      Gesvideo3
046800070124      * se "fatto" <> blank ho aggiornato la prima riga multipla
046900070125      * quindi memorizzo i valori da aggiornare x eventuali righe con stessa
047000070125      * opzione e proseguo l'aggiornamento  senza più emettere il video per
047100070125      * queste ultime se le opzioni fossero mischiate le altre differenti
047200070125      * dalla multipla verranno elaborate normalmente nell'ambito dello stesso
047300070125      * ciclo di elaborazione. Le multiple verranno elaborate in maniera
047400070125      * occulta con il valore impostato inizialmente
047500070125
047600070124     c                   if        fatto <> *blank
047700070124     c                   movel     varggir       savvarggir
047800070124     c                   move      'X'           multi
047900070124     c                   endif
048000070124      *imposta il giro sulle ulteriori righe della scelta multipla
048100070124     c                   else
048200070124     c                   movel     savvarggir    varggir
048300070124     c                   exsr      aggiorna
048400030605     c                   endif
048500070124     c                   endif
048600030605     C                   ENDSR
048700030605     c**********************************************************************
048800030605     C     gesvideo3     BEGSR
048900030605     c**********************************************************************
049000030702     c                   clear                   fatto
049100070222     c                   clear                   riemetti
049200030605     c                   do        *hival
049300030605     c                   exfmt     video3
049400070205     c                   if        *inkc
049500070205     C                   EVAL      $Video = *BLANKS
049600070205     c                   leave
049700070205     c                   endif
049800030605     c   kl              leave
049900070124      * ricerca interrogazione giri
050000070124     c                   if        *inkg
050100070202     c                   eval      kpjbus = kpjbu
050200070124     c                   clear                   fidg09ds
050300070124     c                   move      'P01'         d09iop0
050400070124     c                   movel     vfil          d09ifgs
050500070124     c                   z-add     wuda          d09idat
050600070124     c                   movel     vposint       d09ides
050700070705     c                   eval      d09itug = 'C'                                tipo util. consegna
050800070124     c                   movel     fidg09ds      kpjbu
050900070124     c                   call      'FIDG09R'
051000070124     c                   parm                    kpjba
051100070124     c  n11              movel     kpjbu         fidg09ds
051200070124     c  n11              movel     d09ocgi       varggir
051300070202     c                   eval      kpjbu = kpjbus
051400070124     c                   endif
051500070124      * ricerca interrogazione autisti
051600070124     c                   if        *inkh
051700070202     c                   eval      kpjbus = kpjbu
051800070124     c                   clear                   fidg08ds
051900070926     c                   movel     'C'           d08tp
052000070124     c                   movel     vfil          d08fgs
052100070129     c                   movel     vfil          d08fgsgo
052200070124     c                   movel     vposint       d08rsc
052300070124     c                   movel     fidg08ds      kpjbu
052400070124     c                   call      'FIDG08R'
052500070124     c                   parm                    kpjba
052600070124     c  n11              movel     kpjbu         fidg08ds
052700070124     c  n11              movel     d08cgio       varggir
052800070202     c                   eval      kpjbu = kpjbus
052900070124     c                   endif
053000070124      *
053100030624     c                   exsr      controv3
053200030717     c   28
053300030717     cor 96              iter
053400030605     c   kf              exsr      aggiorna
053500070222     c                   if        fatto <> *blank or riemetti <> *blank
053600030702     c                   leave
053700030702     c                   endif
053800030605
053900030605     c                   enddo
054000030605     C                   ENDSR
054100030605     c**********************************************************************
054200030624     C     Controv3      BEGSR
054300030605     c**********************************************************************
054400030613      *
054500030702      * *in96 errore generico videata dettaglio
054600070131     c                   setoff                                       9695
054700070220     c                   setoff                                       8988
054800070220      * eventuale variazione bolla
054900070220     c                   clear                   fnlr66ds
055000070220    1c                   if        vvar <> *blanks
055100070220     c                   movel     Varbaas       d66aas
055200070220     c                   movel     Varblnp       d66lnp
055300070220     c                   movel     Varbnrs       d66nrs
055400070220     c                   movel     Varbnsp       d66nsp
055500070220      *
055600070220     c     '?'           scan      vvar                                   90
055700070220      * Ricerca causale
055800070220    2c                   if        *in90
055900070220     c                   movel     'V'           d66ges
056000070220     c                   movel     'A'           d66tbo
056100070220      *
056200070220     c                   movel     fnlr66ds      kpjbu
056300070220     c                   call      'FNLR66R'
056400070220     c                   parm                    kpjba
056500070220     c                   movel     kpjbu         fnlr66ds
056600070220     c                   movel     d66cvb        Vvar
056700070220     c                   movel     d66des        vvard
056800070220   X2c                   else
056900070220      * Controllo causale inserita
057000070220     c                   movel     'C'           d66ges
057100070220     c                   movel     'A'           d66tbo
057200070220     c                   movel     Vvar          d66cvb
057300070220      *
057400070220     c                   movel     fnlr66ds      kpjbu
057500070220     c                   call      'FNLR66R'
057600070220     c                   parm                    kpjba
057700070220     c                   movel     kpjbu         fnlr66ds
057800070220     c                   movel     d66cvb        vvar
057900070220     c                   MOVEL     d66des        vvard
058000070220      *
058100070220      * Errore
058200070220    3c                   if        d66msg <> *blank
058300070220     c                   seton                                        9689
058400070220     c                   goto      endcv3
058500070220    3c                   endif
058600070220
058700070220    2c                   endif
058800070220      *
058900070220      * RICHIAMO PGM DI VARIAZIONE SE IMPOSTATO IL CODICE
059000070220      *
059100070220     c                   clear                   fnlr48ds
059200070220     c                   movel     Varbaas       d48aas
059300070220     c                   movel     Varblnp       d48lnp
059400070220     c                   movel     Varbnrs       d48nrs
059500070220     c                   movel     Varbnsp       d48nsp
059600070220     c                   movel     Vfil          d48fgs
059700070220     c                   movel     varbcbo       d48cbo
059800070220     c                   movel     vvar          d48cvb
059900070220     c                   movel     'A'           d48tbo
060000070220     c                   movel     'S'           d48f12
060100070220     c                   movel     'V'           d48ffr
060200070220     c                   movel     fnlr48ds      kpjbu
060300070220     c                   call      'FNLR48R'
060400070220     c                   parm                    KPJBA
060500070220     c                   parm                    DSARBD
060600070220     c                   parm                    DSARBK
060700070220     c                   parm                    DARBT
060800070220     c                   parm                    DSARBG
060900070220     c                   parm                    TRUL90DS
061000070220      *
061100070220     c                   movel     kpjbu         fnlr48ds
061200070220     C* errore
061300070220     C                   if        d48err = '1'
061400070220     C                   seton                                        9688
061500070220     c                   goto      endcv3
061600070220     C                   else
061700070220     C                   clear                   vvar
061800070220     C                   clear                   vvard
061900070222     c                   move      'X'           riemetti
062000070220    1C                   endif
062100070220     C*
062200070220    1C                   endif
062300070124      * decodifica giro
062400070131     c                   if        varggir <> *blank
062500070124     c     kgrg          chain     figrg03l                           95
062600070705     c                   If        not *in95
062700070705     c                   eval      dgrgflo = grgflo
062800070705     c                   else
062900070705     c                   clear                   dgrgflo
063000070705     c                   endif
063100070613     c                   if        not *in95 and grgatb = *blank and
063200070613     c                             grgdde <= wuda and wuda < grgdscad
063300070705      * verifico il tipo utilizzo giro
063400070705     c                             and (§grgtug = 'C' or §grgtug = 'E')
063500070705
063600070613     c                   movel     grgdes        vdesgir
063700070613     c                   else
063800070613     c                   clear                   vdesgir
063900070613     c                   seton                                        9596
064000070131     c                   endif
064100070613     c                   endif
064200070124
064300070220     c     endcv3        endsr
064400030605     c**********************************************************************
064500030605     C     aggiorna      BEGSR
064600030605     c**********************************************************************
064700110701     c                   eval      ChiamaST=' '
064800110701     c
064900070129      * se non trova ARG dalla chain effettuata nelle routine singola o
065000070129      * multipla provvede alla scrittura impostando da ARB
065100070206     c     karg1         chain     fiarg01l
065200070206     c                   if        not %found(fiarg01l)
065300070206      * rek non trovato scrivo solo se impostato a video un giro
065400070206     C                   if        varggir <> *blank
065500070129     c                   clear                   fiarg000
065600070129     c                   exsr      imposta_arg
065700070129     c                   write     fiarg000
065800110701     c* Imposto flag di richiamo bolla centro storico
065900110701     c                   eval      ChiamaST='S'
066000110701     c
066100070206     c                   endif
066200070129      *con rek ARG trovato aggiorna solo giro/filiale consegna/flag manuale
066300070129     c                   else
066400070129     c                   z-add     vfil          argpoc
066500110707     c* memorizzo se cambiato giro per richiamare pgm di controllo flag centro storico
066600110701     c                   if        argcgi<>varggir
066700110701     c                   eval      ChiamaST='S'
066800110701     c                   endif
066900110701     c
067000070124     c                   movel     varggir       argcgi
067100070205     c                   if        varggir <> *blank
067200070129     c                   move      'M'           argtgi
067300070205     c                   else
067400070205     c                   move      *blank        argtgi
067500070205     c                   endif
067600070124     c                   update    fiarg000
067700070129     c                   endif
067800070124     c                   move      'X'           fatto
067900110701     c
068000110701     c* Richiamo pgm controllo centro storico
068100110701     c                   if        ChiamaST='S'
068200110701     c                             and olvstprop<>'N'
068300110701     c                   clear                   fnlvstds
068400110701     c                   eval      ilvstaas=arbaas
068500110701     c                   eval      ilvstlnp=arblnp
068600110701     c                   eval      ilvstnrs=arbnrs
068700110701     c                   eval      ilvstnsp=arbnsp
068800110701     c                   eval      ilvstmgs=arbmgs
068900110701     c                   eval      ilvstfin=arbfin
069000110701     c                   eval      ilvstcgi=argcgi
069100110701     c                   eval      ilvstpoc=Vfil
069200110701     c                   if        arbddc>0
069300110701     c                   eval      ilvstdat=arbddc
069400110701     c                   else
069500110701     c                   eval      ilvstdat=wuda
069600110701     c                   endif
069700110701     c                   call      'FNLVSTR'
069800110701     c                   parm                    kpjba
069900110701     c                   parm                    fnlvstds
070000110701     c                   clear                   chiamaST
070100110701     c                   endif
070200110701     c
070300030605     C                   ENDSR
070400070129     c**********************************************************************
070500070129     C     imposta_arg   BEGSR
070600070129     c**********************************************************************
070700070129     c                   z-add     arbaas        argaas
070800070129     c                   z-add     arblnp        arglnp
070900070129     c                   z-add     arbnrs        argnrs
071000070129     c                   z-add     arbnsp        argnsp
071100070129     c                   z-add     arblna        arglna
071200070129     c                   z-add     vfil          argpoc
071300070129     c                   move      varggir       argcgi
071400070205     c                   if        varggir <> *blank
071500070205     c                   move      'M'           argtgi
071600070205     c                   else
071700070205     c                   move      *blank        argtgi
071800070205     c                   endif
071900070129     C                   ENDSR
072000030715     c**********************************************************************
072100070205     C     spalmaopz1    BEGSR
072200030715     c**********************************************************************
072300030715     c                   do        *hival        recor             4 0
072400030715     c     recor         chain     video2                             93
072500030715     c   93              leave
072600030715     c                   move      '1'           v1csce
072700030715     c                   update    video2
072800030715     c                   enddo
072900030715     C                   ENDSR
073000070125     c**********************************************************************
073100070125     c* carica sfl partendo da fnarb
073200070125     c**********************************************************************
073300070125     C     Carvideo20    BEGSR
073400070125     C                   IF        WrkCarS01 = *ON
073500070125     C                   EVAL      snrr1   = 0
073600070125     C                   EVAL      *IN90 = *ON
073700070125     C                   EVAL      *IN91 = *OFF
073800070125     C                   WRITE     video2c
073900070125     C                   EVAL      *IN90 = *OFF
074000070131      * gestione eventuali filiali in £6
074100070131     c                   do        *hival        £6                3 0
074200070131     c                   if        fil£6(£6) > 0
074300070131     c                   z-add     fil£6(£6)     vfil
074400070131     c                   else
074500070131     c                   z-add     fil£6(1)      vfil
074600070131     c                   leave
074700070131     c                   endif
074800070131     c     vfil          setll     fnarb05l
074900070125     c                   do        *hival
075000070131     c     vfil          reade     fnarb05l                               97
075100070125     c   97              leave
075200070131
075300070131      * filtri
075400070131
075500070131      * bolle da escludere sempre
075600070131     c                   if        arbfbc = '*' or arbfbc = 'B' or
075700070201     c                             arbfbc = 'G' or arbndc > 0
075800070131     c                   iter
075900070131     c                   endif
076000070131
076100070201      * con C.A. aperta
076200070201     c                   clear                   fidn12ds
076300070201     c                   eval      i12aas = arbaas
076400070201     c                   eval      i12lnp = arblnp
076500070201     c                   eval      i12nrs = arbnrs
076600070201     c                   eval      i12nsp = arbnsp
076700070201     c                   eval      i12fel = 'E'
076800070201     c                   eval      i12fan = 'E'
076900070201     c                   eval      i12fch = 'E'
077000070201     c                   call      'FIDN12R'
077100070201     c                   parm                    fidn12ds
077200070201      **
077300070201      * se c'è almeno una C.A. aperta
077400070201      **
077500070201     c     o12nca        ifgt      0
077600070201     c                   iter
077700070201     c                   end
077800070201     c
077900070201      * parzializzazione per localita richiesto range a video
078000070201     c                   if        vdaloc <> *blank and valoc <> *blank
078100070201     c                             and (arblod < vdaloc or arblod > valoc)
078200070201     c                   iter
078300070201     c                   endif
078400070607      * parzializzazione per nome destinatario
078500070607     c                   if        vdesti <> *blank
078600070607     C                   clear                   pos               2 0
078700070607     C                   eval      pos = %scan(%trim(Vdesti):arbrsd)
078800070607     C                   if        pos = *zeros
078900070607     C                   iter                                                   *RECORD NON VALIDO
079000070607     C                   endif
079100070607     c                   endif
079200070607      *
079300070131      * abilitazione alla consegna
079400070321     c*mm                if        vcons = 'I' and arbama < 1 and arbdam = 0
079500070321     c*mm                iter
079600070321     c*mm                else
079700070321     c*mm                if        vcons = 'E' and (arbama > 0 or arbdam > 0)
079800070321     c*mm                iter
079900070321     c*mm                endif
080000070321     c*mm                endif
080100070131      *
080200070201      * abilitate incasso
080300070131      *
080400070321     c*mm                if        vinc = 'I' and (arbacc <>'S' or arbaca <>'S')
080500070321     c*mm                iter
080600070321     c*mm                else
080700070321     c*mm                if        vinc = 'E' and arbacc = 'S' and arbaca = 'S'
080800070321     c*mm                iter
080900070321     c*mm                endif
081000070321     c*mm                endif
081100070131
081200070131      *
081300070131      * non richieste bolle con giro impostato
081400070125      *verifico se esiste fiarg,e se ha già un giro impostato non considero
081500070131     c                   if        vgiro = *blank
081600070207     c     karg1         chain(n)  fiarg01l
081700070125     c                   if        %found(fiarg01l) and argcgi <> *blank
081800070125     c                   iter
081900070125     c                   endif
082000070131     c                   endif
082100070125
082200070727      * richieste tutte le bolle con o sensza giro
082300070727     c                   clear                   argcgiw
082400070727     c                   if        vgiro = '9999999999'
082500070727     c     karg1         chain(n)  fiarg01l
082600070727     c                   if        %found(fiarg01l)
082700070727     c                   movel     argcgi        argcgiw
082800070727     c                   endif
082900070727     c                   endif
083000070129      * rientri
083100070201     c                   if        vrie = 'I' and arbfbr = *blank
083200070201     c                   iter
083300070201     c                   else
083400070201     c                   if        vrie = 'E' and arbfbr <> *blank
083500070201     c                   iter
083600070201     c                   endif
083700070201     c                   endif
083800070129      * fermo deposito
083900070201     c                   if        vferd = 'I' and arbffd = *blank
084000070201     c                   iter
084100070201     c                   else
084200070201     c                   if        vferd = 'E' and arbffd <> *blank
084300070201     c                   iter
084400070201     c                   endif
084500070201     c                   endif
084600070129
084700070131      * conti servizio
084800070201     c                   if        vcser = 'E'
084900070201     c                   move      '3A'          tblcod
085000070201     c                   movel     arbcbo        tblkey
085100070201     c     ktab          chain     tabel00f
085200070201     c                   if        %found(tabel00f)
085300070201     c                   movel     tbluni        ds3a
085400070201     c                   if        §3arbl = 'C'
085500070201     c                   iter
085600070201     c                   endif
085700070201     c                   endif
085800070201     c                   else
085900070201     c                   if        vcser = 'I'
086000070201     c                   move      '3A'          tblcod
086100070201     c                   movel     arbcbo        tblkey
086200070201     c     ktab          chain     tabel00f
086300070201     c                   if        %found(tabel00f)
086400070201     c                   movel     tbluni        ds3a
086500070201     c                   if        §3arbl <> 'C'
086600070201     c                   iter
086700070201     c                   endif
086800070201     c                   endif
086900070201     c                   endif
087000070201
087100070201     c                   endif
087200070131
087300070209     c
087400070209     c* Bolle abbinate
087500070209     c                   clear                   abb
087600070209     c                   if        vabb <> *blank
087700070209     C* 1) DAL FLAG ABILITAZIONE ARRIVI(ARBAMA=3)
087800070209    2C                   select
087900070209     c                   when      arbama = 3
088000070209     C                   move      'A'           abb
088100070209     C*
088200070209     C* 2) DAL FLAG ABILITAZIONE PARTENZE (ARBAMP=1)
088300070209     c                   when      arbama = 1 and arbamp = 1
088400070209     C                   move      'A'           abb
088500070209     C*
088600070209     C* 3) SE LOCALE SEMPRE ABBINATO
088700070209     c                   when      arbtfp=arbtfa  or
088800070209     c                             arblnp=arblna
088900070209     C                   move      'A'           abb
089000070209     C*
089100070209   X2C                   other
089200070209     C* REPERISCO INFORMAZIONE SE FOGLIO E' ABBINATO O MENO
089300070209     C                   clear                   fnlv53ds
089400070209     C                   movel     'A'           d53tfo
089500070209     C                   movel     arbnfv        d53nfv
089600160509     C                   movel     arbtfp        d53fgs
089700070209     C                   movel     1             d53npg
089800070209     C                   movel     'B'           d53abb
089900070209     C                   movel     arblna        d53lna
090000070209     C                   movel     arbtfa        d53tfa
090100070209     C                   call      'FNLV53R'
090200070209     C                   parm                    fnlv53ds
090300070209     C                   if        d53err = ' ' and d53nfa > 0
090400070209     C                   move      'A'           abb
090500070209     C                   endif
090600070209     C*
090700070209    2C                   endsl
090800070209     C* E - escludi non abbinate
090900070209    2C                   if        vabb = 'E' and abb <> 'A'
091000070209    3C                   iter
091100070209    3C                   endif
091200070209     C* I - Includi non abbinate
091300070209    2C                   if        vabb = 'I' and abb = 'A'
091400070209    3C                   iter
091500070209    3C                   endif
091600070209
091700070209    3C                   endif
091800070209      *
091900070131      * ddt/si
092000070201     c                   if        vddt  = 'E'
092100070201     c                   move      'A'           trck
092200070201     c     kar4          chain     fiar401l
092300070201     c                   if        %found(fiar401l)
092400070201     c                   movel     ar4not        dsbl4a
092500070201     c                   if        §b4abm = 'P' or §b4abm = 'Q' or
092600070201     c                             §b4abm = 'C' or §b4abm = 'S'
092700070201     c                   iter
092800070201     c                   endif
092900070201     c                   endif
093000070201     c                   endif
093100070131
093200070201     C                   move      *blank        V1csce
093300070125     c                   movel     arblod        vlod
093400070125     c                   movel     arbind        vind
093500070423     c                   eval      vlodind = vlod + arbcad + vind
093600070423     c*                  movel     vlod          vlodind
093700070423     c*                  move      vind          vlodind
093800070206     c                   movel     arbprd        vprov
093900070125     c                   movel     arbrsd        vrsd
094000070125     c                   movel     arbtc1        vtc1
094100070125     c                   movel     arbtc2        vtc2
094200070125     c                   z-add     arbpkb        vpes
094300070125     c                   z-add     arbvlb        vvol
094400070125     c                   z-add     arbaas        vaas
094500070125     c                   z-add     arblnp        vlnp
094600070125     c                   z-add     arbnrs        vnrs
094700070125     c                   z-add     arbnsp        vnsp
094800070220     c                   movel     arbcbo        vcbo
094900070727     c                   movel     argcgiw       vgir
095000141029     c                   if        vgir = 'DIR        '
095100141029     c                   clear                   fnlry09ds2
095200141029     c                   eval      ilry09tch = 'C'
095300141029     c                   eval      ilry09tdis= 'D'
095400141029     c                   eval      ilry09aas = vAAS
095500141029     c                   eval      ilry09lnp = vLNP
095600141029     c                   eval      ilry09nrs = vNRS
095700141029     c                   eval      ilry09nsp = vNSP
095800141029     c                   eval      ilry09fgs = vfil
095900141029     c                   movel     fnlry09ds2    kpjbu
096000141029     c                   call      'FNLRY09R'
096100141029     c                   parm                    kpjba
096200141029     c                   movel     kpjbu         fnlry09ds2
096300141029     c                   if        olry09err = *blank
096400141029     c                   if        olry09esdi = '1'
096500141029     c                   iter
096600141029     c                   endif
096700141029     c                   endif
096800141029     c                   endif
096900070125      *esco per raggiunta massima capacità sfl
097000070125     c                   if        snrr1 > 9990
097100070125     c                   leave
097200070125     c                   endif
097300070125
097400070125     C                   EVAL      snrr1 = snrr1 + 1
097500070125     C                   WRITE     video2
097600070125     C                   ENDDO
097700070131     C                   enddo
097800070125
097900070125     C                   IF        snrr1  > 0
098000070125     C                   EVAL      *IN91 = *ON
098100070125     C                   MOVE      1             OK                1 0
098200070125     C                   ELSE
098300070125     C                   MOVE      0             OK                1 0
098400070125     C                   ENDIF
098500070125     C                   EVAL      nrr1  = 1
098600070125     C                   ENDIF
098700070125     C                   EVAL      WrkCarS01 = *OFF
098800070129     C                   EVAL      lasnrr1  = snrr1
098900070125     C                   EVAL      snrr1  = 1
099000070125     C                   EVAL      csrrrn = 1
099100070125     C                   ENDSR
099200070125     c**********************************************************************
099300070125     c* carica sfl partendo da fiarg con giro impostato
099400070125     c**********************************************************************
099500070125     C     Carvideo21    BEGSR
099600070125     C                   IF        WrkCarS01 = *ON
099700070125     C                   EVAL      snrr1   = 0
099800070125     C                   EVAL      *IN90 = *ON
099900070125     C                   EVAL      *IN91 = *OFF
100000070125     C                   WRITE     video2c
100100070125     C                   EVAL      *IN90 = *OFF
100200070403     c     karg          setll     fiarg52j
100300070125     c                   do        *hival
100400070403     c     karg          reade     fiarg52j                               97
100500070125     c   97              leave
100600070205      * filtri
100700070205
100800070205      * bolle da escludere sempre
100900070205     c                   if        arbfbc = '*' or arbfbc = 'B' or
101000070205     c                             arbfbc = 'G' or arbndc > 0
101100070205     c                   iter
101200070205     c                   endif
101300070205
101400070205     c                   if        arbdcm  = 0 or
101500070205     c                             (arbicc = ' ' or arbicc = 'R') or
101600070205     c                             (arbica = ' ' or arbica = 'R')
101700070205     c                   else
101800070205     c                   iter
101900070205     c                   endif
102000070205
102100070205      * con C.A. aperta
102200070205     c                   clear                   fidn12ds
102300070205     c                   eval      i12aas = arbaas
102400070205     c                   eval      i12lnp = arblnp
102500070205     c                   eval      i12nrs = arbnrs
102600070205     c                   eval      i12nsp = arbnsp
102700070205     c                   eval      i12fel = 'E'
102800070205     c                   eval      i12fan = 'E'
102900070205     c                   eval      i12fch = 'E'
103000070205     c                   call      'FIDN12R'
103100070205     c                   parm                    fidn12ds
103200070205      **
103300070205      * se c'è almeno una C.A. aperta
103400070205      **
103500070205     c     o12nca        ifgt      0
103600070205     c                   iter
103700070205     c                   end
103800070205     c
103900070607      * parzializzazione per nome destinatario
104000070607     c                   if        vdesti <> *blank
104100070607     C                   clear                   pos               2 0
104200070607     C                   eval      pos = %scan(%trim(Vdesti):arbrsd)
104300070607     C                   if        pos = *zeros
104400070607     C                   iter                                                   *RECORD NON VALIDO
104500070607     C                   endif
104600070607     c                   endif
104700070607      *
104800070205      * abilitazione alla consegna
104900070321     c*mm                if        vcons = 'I' and arbama < 1 and arbdam = 0
105000070321     c*mm                iter
105100070321     c*mm                else
105200070321     c*mm                if        vcons = 'E' and (arbama > 0 or arbdam > 0)
105300070321     c*mm                iter
105400070321     c*mm                endif
105500070321     c*mm                endif
105600070205      *
105700070205      * abilitate incasso
105800070205      *
105900070321     c*mm                if        vinc = 'I' and (arbacc <>'S' or arbaca <>'S')
106000070321     c*mm                iter
106100070321     c*mm                else
106200070321     c*mm                if        vinc = 'E' and arbacc = 'S' and arbaca = 'S'
106300070321     c*mm                iter
106400070321     c*mm                endif
106500070321     c*mm                endif
106600070205
106700070205      * rientri
106800070205     c                   if        vrie = 'I' and arbfbr = *blank
106900070205     c                   iter
107000070205     c                   else
107100070205     c                   if        vrie = 'E' and arbfbr <> *blank
107200070205     c                   iter
107300070205     c                   endif
107400070205     c                   endif
107500070205      * fermo deposito
107600070205     c                   if        vferd = 'I' and arbffd = *blank
107700070205     c                   iter
107800070205     c                   else
107900070205     c                   if        vferd = 'E' and arbffd <> *blank
108000070205     c                   iter
108100070205     c                   endif
108200070205     c                   endif
108300070205
108400070205      * conti servizio
108500070205     c                   if        vcser = 'E'
108600070205     c                   move      '3A'          tblcod
108700070205     c                   movel     arbcbo        tblkey
108800070205     c     ktab          chain     tabel00f
108900070205     c                   if        %found(tabel00f)
109000070205     c                   movel     tbluni        ds3a
109100070205     c                   if        §3arbl = 'C'
109200070205     c                   iter
109300070205     c                   endif
109400070205     c                   endif
109500070205     c                   else
109600070205     c                   if        vcser = 'I'
109700070205     c                   move      '3A'          tblcod
109800070205     c                   movel     arbcbo        tblkey
109900070205     c     ktab          chain     tabel00f
110000070205     c                   if        %found(tabel00f)
110100070205     c                   movel     tbluni        ds3a
110200070205     c                   if        §3arbl <> 'C'
110300070205     c                   iter
110400070205     c                   endif
110500070205     c                   endif
110600070205     c                   endif
110700070205
110800070205     c                   endif
110900070205
111000070209     c* Bolle abbinate
111100070209     c                   clear                   abb
111200070209     c                   if        vabb <> *blank
111300070209     C* 1) DAL FLAG ABILITAZIONE ARRIVI(ARBAMA=3)
111400070209    2C                   select
111500070209     c                   when      arbama = 3
111600070209     C                   move      'A'           abb
111700070209     C*
111800070209     C* 2) DAL FLAG ABILITAZIONE PARTENZE (ARBAMP=1)
111900070209     c                   when      arbama = 1 and arbamp = 1
112000070209     C                   move      'A'           abb
112100070209     C*
112200070209     C* 3) SE LOCALE SEMPRE ABBINATO
112300070209     c                   when      arbtfp=arbtfa  or
112400070209     c                             arblnp=arblna
112500070209     C                   move      'A'           abb
112600070209     C*
112700070209   X2C                   other
112800070209     C* REPERISCO INFORMAZIONE SE FOGLIO E' ABBINATO O MENO
112900070209     C                   clear                   fnlv53ds
113000070209     C                   movel     'A'           d53tfo
113100070209     C                   movel     arbnfv        d53nfv
113200160509     C                   movel     arbtfp        d53fgs
113300070209     C                   movel     1             d53npg
113400070209     C                   movel     'B'           d53abb
113500070209     C                   movel     arblna        d53lna
113600070209     C                   movel     arbtfa        d53tfa
113700070209     C                   call      'FNLV53R'
113800070209     C                   parm                    fnlv53ds
113900070209     C                   if        d53err = ' ' and d53nfa > 0
114000070209     C                   move      'A'           abb
114100070209     C                   endif
114200070209     C*
114300070209    2C                   endsl
114400070209     C* E - escludi non abbinate
114500070209    2C                   if        vabb = 'E' and abb <> 'A'
114600070209    3C                   iter
114700070209    3C                   endif
114800070209     C* I - Includi non abbinate
114900070209    2C                   if        vabb = 'I' and abb = 'A'
115000070209    3C                   iter
115100070209    3C                   endif
115200070209
115300070209    3C                   endif
115400070209      *
115500070205      * ddt/si
115600070205     c                   if        vddt  = 'E'
115700070205     c                   move      'A'           trck
115800070205     c     kar4          chain     fiar401l
115900070205     c                   if        %found(fiar401l)
116000070205     c                   movel     ar4not        dsbl4a
116100070205     c                   if        §b4abm = 'P' or §b4abm = 'Q' or
116200070205     c                             §b4abm = 'C' or §b4abm = 'S'
116300070205     c                   iter
116400070205     c                   endif
116500070205     c                   endif
116600070205     c                   endif
116700070205
116800070125
116900070129     C                   MOVE      *BLANK        V1CSCE
117000070125     c                   movel     arblod        vlod
117100070125     c                   movel     arbind        vind
117200070423     c                   eval      vlodind = vlod + arbcad + vind
117300070423     c*                  movel     vlod          vlodind
117400070423     c*                  move      vind          vlodind
117500070206     c                   movel     arbprd        vprov
117600070125     c                   movel     arbrsd        vrsd
117700070125     c                   movel     arbtc1        vtc1
117800070125     c                   movel     arbtc2        vtc2
117900070125     c                   z-add     arbpkb        vpes
118000070125     c                   z-add     arbvlb        vvol
118100070125     c                   z-add     arbaas        vaas
118200070125     c                   z-add     arblnp        vlnp
118300070125     c                   z-add     arbnrs        vnrs
118400070125     c                   z-add     arbnsp        vnsp
118500070220     c                   movel     arbcbo        vcbo
118600070727     c                   movel     argcgi        vgir
118700141029     c                   if        vgir = 'DIR        '
118800141029     c                   clear                   fnlry09ds2
118900141029     c                   eval      ilry09tch = 'C'
119000141029     c                   eval      ilry09tdis= 'D'
119100141029     c                   eval      ilry09aas = vAAS
119200141029     c                   eval      ilry09lnp = vLNP
119300141029     c                   eval      ilry09nrs = vNRS
119400141029     c                   eval      ilry09nsp = vNSP
119500141029     c                   eval      ilry09fgs = vfil
119600141029     c                   movel     fnlry09ds2    kpjbu
119700141029     c                   call      'FNLRY09R'
119800141029     c                   parm                    kpjba
119900141029     c                   movel     kpjbu         fnlry09ds2
120000141029     c                   if        olry09err = *blank
120100141029     c                   if        olry09esdi = '1'
120200141029     c                   iter
120300141029     c                   endif
120400141029     c                   endif
120500141029     c                   endif
120600070125      *esco per raggiunta massima capacità sfl
120700070125     c                   if        snrr1 > 9990
120800070125     c                   leave
120900070125     c                   endif
121000070125
121100070125     C                   EVAL      snrr1 = snrr1 + 1
121200070125     C                   WRITE     video2
121300070125     C                   ENDDO
121400070125
121500070125     C                   IF        snrr1  > 0
121600070125     C                   EVAL      *IN91 = *ON
121700070125     C                   MOVE      1             OK                1 0
121800070125     C                   ELSE
121900070125     C                   MOVE      0             OK                1 0
122000070125     C                   ENDIF
122100070125     C                   EVAL      nrr1  = 1
122200070125     C                   ENDIF
122300070125     C                   EVAL      WrkCarS01 = *OFF
122400070206     C                   EVAL      lasnrr1  = snrr1
122500070125     C                   EVAL      snrr1  = 1
122600070125     C                   EVAL      csrrrn = 1
122700070125     C                   ENDSR
122800011227     c**********************************************************************
122900011227     c* gestione sfl
123000011227     c**********************************************************************
123100000613     C     GestioneSFL   BEGSR
123200000627      *
123300000627     C                   EVAL      WrkEofS01 = *OFF
123400030605      * Elaborazione opzioni.
123500011228     c*
123600030605     c                   do        *hival        X                 4 0
123700030605     c     X             CHAIN     video2                             50
123800030605     C   50              LEAVE
123900070205     c   kc              goto      endrut
124000030604     C                   IF        v1csce <> *blanks
124100030605     c                   exsr      opzione
124200030611
124300030611     c                   move      *blank        v1csce
124400030605     c                   update    video2
124500030611
124600011228     C                   ENDIF
124700030605     C                   enddo
124800070124      * abblenca spia x gestione scelta multipla per prossima selezione
124900070124     c                   move      *blank        multi
125000070124
125100070206     c                   if        modalita = '1' or modalita = '9'
125200030710     c                   eval      wrkcars01 = *on
125300030710     c                   endif
125400000627      *
125500070205     C     endrut        ENDSR
125600011227     c**********************************************************************
125700000607     C     *INZSR        BEGSR
125800011227     c**********************************************************************
125900000607      *
126000000607     C     *ENTRY        PLIST
126100000607     C                   PARM                    Kpjba
126200070125     c                   clear                   fidg06ds
126300070123     C                   if        kpjbu <> *blank
126400070123     c                   movel     kpjbu         fidg06ds
126500070123     c                   endif
126600070123      *
126700030604     C                   Z-ADD     1             CODUT
126800030604     C                   CALL      'X§PARUT'
126900030604     C                   PARM                    UTEDSE
127000030604     C                   MOVEL     REC80         CNCR80
127100030703     C                   MOVEL     Ragut         rsut
127200030703     C                   MOVEL     knsif         vknsif
127300030703     C                   MOVEL     knmus         vknmus
127400030702      *reperimento data
127500030702     C                   TIME                    W0120            14 0
127600030702     C                   MOVE      W0120         WDAT              8 0
127700030702     C*
127800030702     C                   Z-ADD     WDAT          G02DAT
127900030702     C                   MOVEL     *BLANK        G02ERR
128000030702     C                   CALL      'XSRDA8'
128100030702     C                   PARM                    WLBDA8
128200030702     C* UDATE A 8 IN AAAA/MM/GG
128300030702     C                   Z-ADD     G02INV        WUDA              8 0
128400030717     C                   Z-ADD     G02INV        WUDAte            8 0
128500030606      *profilo di filiale attivo indicatore di protezione campi non manutenzionabili
128600030617     C                   CLEAR                   Tibs36Ds
128700030617     C                   EVAL      I36ute = knmus
128800030617     C                   CALL      'TIBS36R'
128900030617     C                   PARM                    tibs36ds
129000070123      *
129100070123     c                   movel     o36pou        vfil
129200000607      *
129300070125     C     Karg          klist
129400070124     C                   kfld                    vfil
129500070124     C                   kfld                    vgiro
129600070123      *
129700070124     C     Karb          klist
129800070124     C                   kfld                    vaas
129900070124     C                   kfld                    vlnp
130000070124     C                   kfld                    vnrs
130100070124     C                   kfld                    vnsp
130200070201      *
130300070201     C     Kar4          klist
130400070201     C                   kfld                    arbaas
130500070201     C                   kfld                    arblnp
130600070201     C                   kfld                    arbnrs
130700070201     C                   kfld                    arbnsp
130800070201     c                   kfld                    trck              1
130900070124      *
131000070125     C     Karg1         klist
131100070125     C                   kfld                    arbaas
131200070125     C                   kfld                    arblnp
131300070125     C                   kfld                    arbnrs
131400070125     C                   kfld                    arbnsp
131500070125      *
131600070124     C     Kgrg          klist
131700070124     C                   kfld                    vfil
131800070124     C                   kfld                    varggir
131900070201      *
132000070201     C     Ktab          klist
132100070201     C                   kfld                    tblkut
132200070201     C                   kfld                    tblcod
132300070201     C                   kfld                    tblkey
132400070201     C                   z-add     1             tblkut
132500070205      * carico filiali £1
132600070205     c                   eval      kpjbus = kpjbu
132700070205     C                   clear                   trul06ds
132800070205     C                   movel     '£1'          d06cod
132900070205     C                   movel     vfil          d06key
133000070205     C                   movel     trul06ds      kpjbu
133100070205     C                   CALL      'TRUL06R'
133200070205     C                   parm                    kpjba
133300070205     C                   movel     kpjbu         trul06ds
133400070205     C                   movea     lin           fil£1
133500070205     c                   eval      kpjbu = kpjbus
133600030714
133700011227     C                   ENDSR
133800070125      *--------------------------------------------------------------------------------------------*
133900070125      * ordina - riscrive il sfl ordinato per località
134000070125      *--------------------------------------------------------------------------------------------*
134100070125     C     ordina        BEGSR
134200070125     C                   setoff                                       72
134300070125      * IMPOSTO ENTRATA AD 'A'
134400070125     C                   EVAL      snrr1 = 0
134500070125      *
134600070125     C                   CLEAR                   QLGSCB
134700070125     C                   CLEAR                   QLGSCB00
134800070125
134900070125     ** 1 campo chiave.
135000070125     C                   EVAL      QLGNBRK    = 1
135100070423     ** localita/indirizzoè a posizione 1, carattere, 71 byte,
135200070125     C                   EVAL      QLGSP      = 1
135300070206     C                   EVAL      QLGSS      = %SIZE(vlodind)
135400070125     C                   EVAL      QLGDT      = carattere
135500070125     C                   EVAL      QLGSO      = Ascendente
135600070125     C                   EVAL      QLGKL(1)   = QLGSKL
135700070125
135800070125      * Load other sort parameters.
135900070125     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
136000070125     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
136100070125     C                   EVAL      QLGRT     = 8
136200070125     C                   EVAL      QLGOKL    = 80
136300070125     C                   EVAL      QLGLKE    = 16
136400070125     C                   EVAL      QLGLSS    = 290
136500070125
136600070125      * Initialize Sort I/O API fields.
136700070125     C                   EVAL      QLGRL00  = QLGRL
136800070125     C                   EVAL      QLGRC00  = 1
136900070125     C                   CLEAR                   QUSEI
137000070125     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
137100070125
137200070125      * First step - Initialize the sort routine.
137300070125     C                   CALL      'QLGSORT'
137400070125     C                   PARM                    QLGSCB
137500070125     C                   PARM                    NotUsed
137600070125     C                   PARM                    NotUsed
137700070125     C                   PARM                    SizeList
137800070125     C                   PARM                    ReturnSize
137900070125     C                   PARM                    QUSEC
138000070125
138100070125      * Next step - Write records to I/O routine.
138200070125     C                   EVAL      QLGRT00 = Put
138300070125
138400070125     C                   DO        *hival        cx                4 0
138500070125
138600070125     C     cx            CHAIN     video2                             50
138700070125     c   50              leave
138800070125     ** Solo le righe con Selected = 'Y' sono riordinate,
138900070125     ** quindi per fare un ordinamento di tutte le righe
139000070125     ** metto 'Y' sempre.
139100070125     C                   EVAL      Selected = 'Y'
139200070125
139300070125     C                   CLEAR                   QUSEI
139400070125     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
139500070125     C                   CALL      'QLGSRTIO'
139600070125     C                   PARM                    QLGSCB00
139700070125     C                   PARM                    SflRcd
139800070125     C                   PARM                    NotUsed
139900070125     C                   PARM                    SizeList
140000070125     C                   PARM                    NotUsed
140100070125     C                   PARM                    QUSEC
140200070125
140300070125     C                   ENDDO
140400070125
140500070125      * Next step - Signal end of input, clear subfile for reload.
140600070125     C                   EVAL      QLGRT00 = EndPut
140700070125     C                   CLEAR                   QUSEI
140800070125     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
140900070125
141000070125     C                   CALL      'QLGSRTIO'
141100070125     C                   PARM                    QLGSCB00
141200070125     C                   PARM                    SflRcd
141300070125     C                   PARM                    NotUsed
141400070125     C                   PARM                    SizeList
141500070125     C                   PARM                    NotUsed
141600070125     C                   PARM                    QUSEC
141700070125
141800070125     C                   SETON                                        90        *
141900070125     C                   WRITE     video2c                                      *
142000070125     C                   SETOFF                                       90        *
142100070125     C                   WRITE     video2z                                      *
142200070125
142300070125      * Final step - Write the records back to the subfile.
142400070125     C                   EVAL      QLGRT00 = Get
142500070129     C                   DO        lasnrr1       x
142600070125     C                   CLEAR                   QUSEI
142700070125     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
142800070125     C                   CALL      'QLGSRTIO'
142900070125     C                   PARM                    QLGSCB00
143000070125     C                   PARM                    NotUsed
143100070125     C                   PARM                    SflRcd
143200070125     C                   PARM                    QLGRL00
143300070125     C                   PARM                    NotUsed
143400070125     C                   PARM                    QUSEC
143500070125      *
143600070125     c                   z-add     x             snrr1
143700070125     C                   WRITE     video2
143800070125     C                   ENDDO
143900070125     C                   SETOFF                                       20        *
144000070125
144100070125      *
144200070125     C                   ENDSR
144300030714      *---------------------------------------------------------------------
144400070206     C**************************************************************************
144500070206     C*  Routine per gestione richiamo interrogazione bolle
144600070206     C**************************************************************************
144700070206     C     richbl        begsr
144800070206      *
144900070206     c                   movel     kpjbu         §kpjbu
145000070206      *
145100070206     c                   z-add     vaas          pa1aas
145200070206     c                   z-add     vlnp          pa1lnp
145300070206     c                   z-add     vnrs          pa1nrs
145400070206     c                   z-add     vnsp          pa1nsp
145500070206     c                   movel     ' '           pa1f03
145600070206     c                   movel     '1'           pa1flg
145700070206     c                   movel     param1        kpjbu
145800070206     c                   call      'FNLR36R'
145900070206     c                   parm                    kpjba
146000070206     c                   movel     kpjbu         param1
146100070206      *
146200070206     c                   movel     §kpjbu        kpjbu
146300070206      *
146400070206     c                   endsr
