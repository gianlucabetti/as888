000100030604     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200130812      *
000300030701     Fficn75d   CF   E             WORKSTN
000400030604     f                                     sfile(video2:snrr1)
000500030701     Ffifli01l  uF a E           k disk
000600030701     Fazorg01l  iF   E           k disk
000700030718     Ffiapd01l  iF   E           k disk
000800011228      *
000900020103     D Psds           SDS
001000020103     D  PgmName          *PROC
001100011228     D Kpjba         E DS
001200011228      *
001300030702     D fnlv55ds      E DS
001400030721     D fnlv24ds      E DS
001500030619     D trul33ds      E DS
001501131007     D uteautds      E DS
001502131007     d tibs42ds      e ds
001503100617     d dvpocont      e ds
001504100617     d tibs02ds      e ds
001600030604     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
001700030604     D CNCR80        E DS
001800030617     d Tibs36Ds      e ds
001900011227     d data            ds            10
002000011228     d aa                      3      4
002100011228     d mm                      6      7
002200011228     d gg                      9     10
002300011228      *
002400030702     D fatto           S              1
002500030610     D modalita        S              1
002600030604     D x               S              4  0
002700011227     D WrkEofS01       S              1
002800011227     D WrkCarS01       S              1
002900011228     D $VIDEO          S             10
003000030604     D snrr1           S              5i 0
003100030721     D Savkpjbu        S                   like(kpjbu)
003200030604     D SavOpzione      S                   like(v1csce)
003300030702     D Savfliter       S                   like(vfliter)
003400030702     D Savfliterd      S                   like(vfliterd)
003500030606     d dataiso         s               d   datfmt(*iso)
003600030606     d dataeur         s               d   datfmt(*eur)
003700030613     D WLBDA8          DS
003800030613     D  G02DAT                 1      8  0
003900030613     D  G02INV                 9     16  0
004000030613     D  G02ERR                17     17
004100030613     D  G02TGI                18     22  0
004200030701      *---------------- ----------------------------------------------------
004300030604      *---------------- Gestione formato video video01----------------------
004400030604     C                   EXSR      Inzvideo1
004500030604     C                   DOU       $Video <> 'VIDEO1'
004600000608      *----------- Visualizzo dati di output in caso di errori.-------------
004700000608     C                   IF        *IN99
004800000608     C                   EVAL      *IN99 = *OFF
004900030604     C                   WRITE     video1
005000000608     C                   EVAL      *IN99 = *ON
005100000608     C                   ENDIF
005200000608      *
005300030702     c     emetti        tag
005400030604     C                   EXFMT     video1
005500000608      *
005600011227     C                   SELECT
005700030612     C                   WHEN      *INKC
005800011227     C                             OR
005900011227     C                             *INKL
006000011227     C                   EVAL      $Video = *BLANKS
006100011227
006200011227     C                   WHEN      *INKE
006300030604     C                   EXSR      Inzvideo1
006400011227     C                   WHEN      *INKB
006500030604     C                   EXSR      Chkvideo1
006600011227     C                   OTHER
006700030604     C                   EXSR      Chkvideo1
006800040728     c   84              goto      emetti
006900030604     C  N99              EXSR      Wrkvideo2c
007000011227     C                   ENDSL
007100011227      *
007200011227     C                   ENDDO
007300011227      *
007400011227     C                   EXSR      Uscita
007500011227     c**********************************************************************
007600011227     c* uscita
007700011227     c**********************************************************************
007800011227     C     Uscita        BEGSR
007900011227      *
008000011227     C                   EVAL      *INLR = *ON
008100011227     C                   RETURN
008200011227      *
008300011227     C                   ENDSR
008400011227     c**********************************************************************
008500011227     c* gestione sfl
008600011227     c**********************************************************************
008700030604     C     Wrkvideo2c    BEGSR
008800000627      *
008900011227     C                   EVAL      WrkCarS01 = *ON
009000030604     C                   EVAL      $Video = 'VIDEO2C'
009100011227      *
009200030604     C                   DOU       $Video <> 'VIDEO2C'
009300030604     C                   EXSR      Carvideo2
009400011228     c*Descrizione della ricerca
009500000627      *
009600030604     C                   WRITE     video2z
009700030604     C                   EXFMT     video2c
009800011227     C                   SELECT
009900011228     c* fine
010000011227     C                   WHEN      *INKC=*ON
010100011227     C                   EVAL      $VIDEO=*BLANKS
010200011228     c* guida
010300011227     C                   WHEN      *INKL=*ON
010400030604     C                   EVAL      $VIDEO='VIDEO1'
010500011228     c* ripristino
010600011227     C                   WHEN      *INKE=*ON
010700011227     C                   EVAL      WrkCarS01 = *ON
010800011227
010900011227     C                   OTHER
011000011227
011100011227     C                   EXSR      GestioneSFL
011200011227     C                   ENDSL
011300011228
011400011227     C                   ENDDO
011500031027     C                   ENDSR
011600011227     c**********************************************************************
011700011227     c* Controlli video R01.
011800011227     c**********************************************************************
011900030604     C     Chkvideo1     BEGSR
012000000609      *
012100000608     C                   EVAL      *IN99 = *OFF
012200040728     C                   EVAL      *IN84 = *OFF
012300060915      * profilo di filiale controllo che il terminal sia congruente
012400060915     c                   if        o36pou <> 046
012500060915     c                   if        vfliter <> terma and vfliter <> termp
012600060915     c                   seton                                        84
012700060915     c                   endif
012800060915     c                   else
012900060915
013000030702     c                   clear                   fnlv55ds
013100030702     c                   move      'L'           d55tla
013200030702     c                   move      vfliter       d55lin
013201100617     c                   if        wuda >= §vpoter
013300100617     c                   move      'E'           d55tpt
013301100617     c                   else
013302100617     c                   move      ' '           d55tpt
013303100617     c                   endif
013400030702     c                   z-add     wuda          d55drf
013500030702     c                   call      'FNLV55R'
013600030702     c                   parm                    fnlv55ds
013700030702     c                   if        vfliter <> d55tfp and vfliter <> d55tfa
013800030703     c                             or vfliter = *zeros
013900040728     c                   seton                                        84
014000030702     c                   endif
014100060915     c                   endif
014200030701     c     vfliter       chain     azorg01l
014300030701     c                   movel     orgdes        vfliterd
014301130812      * controllo autorizzazione del profilo
014302130812     c                   if        wuda    >= §vpoaut
014303131007     c                   clear                   tibs42ds
014304131007     c                   movel     vfliter       i42pge
014306131007     c                   call      'TIBS42R'
014307131007     c                   parm                    TIBS42ds
014308131007     c                   movel     o42uni        uteautds
014309130812     c                   if        §AFFGVSTD <> 'S'
014310130812     c                   eval      $msg = 'Profilo non autorizzato al terminal'
014311130812     c                   seton                                        8428
014312130812     c                   endif
014313130812     c                   endif
014400000608      *
014500000608     C                   ENDSR
014600011227     c**********************************************************************
014700011227     c* inizializza r01
014800011227     c**********************************************************************
014900030604     C     Inzvideo1     BEGSR
015000000608
015100030702     c                   if        o36pou <> 046
015200060915     c                   move      termp         vfliter
015300030702     c                   endif
015400030604     C                   EXSR      Chkvideo1
015500000609
015600000608     C                   ENDSR
015700030604     c**********************************************************************
015800030605     C     opzione       BEGSR
015900030605     c**********************************************************************
016000030610     c                   move      v1csce        modalita
016100040728     c                   movea     '000000000000'*in(40)
016200040728     c                   movea     '00000'       *in(52)
016300040728     c                   setoff                                       96
016400030605     c                   select
016500030605     c                   when      v1csce = '2'
016600030605     c                   exsr      modifica
016700030605     c                   when      v1csce = '3'
016800030605     c                   exsr      copia
016900030605     c                   when      v1csce = '5'
017000030606     c                   seton                                        21
017100030606     c                   exsr      modifica
017200030606     c                   setoff                                       21
017300040728     c                   when      v1csce = '4'
017400040728     c                   seton                                        21
017500040728     c                   exsr      modifica
017600040728     c                   setoff                                       2128
017700040728     c                   clear                   $msg
017800030605     c                   endsl
017900030604     C                   ENDSR
018000030605     c**********************************************************************
018100030605     C     modifica      BEGSR
018200030605     c**********************************************************************
018300030701     c     kfli2         chain     fifli01l                           98
018400030610     c                   if        not *in98
018500030701     c                   move      FLIVAD        vFLIVAD
018600030701     c                   move      FLITER        vFLITER
018700030701     c                   move      FLIFGP        vFLIFGP
018800030701     c                   move      FLIFGA        vFLIFGA
018900030701     c                   move      FLIDDE        vFLIDDE
019000030701     c                   move      FLIORP        vFLIORP
019100030701     c                   move      FLIORA        vFLIORA
019200030703     c                   move      FLIPRV        vFLIPRV
019300081119     c*                  move      FLIIMP        vFLIIMP
019400030721     c                   clear                   vflipdr
019500030721     c                   if        flipdr > 0
019600030701     c                   move      FLIPDR        vFLIPDR
019700030721     c                   endif
019800030721     c                   move      FLIPDR        vFLIPDRk
019900030701     c                   move      FLINOT        vFLINOT
020000030718      * decodifica autista
020100030718     c     kapd          chain     fiapd01l                           95
020200090507     c  n95              movel     apdrsf        vflipdrd
020300030718     c   95              clear                   vflipdrd
020400030701      * decodifica p.o.
020500030701     c     flifgp        chain     azorg01l                           95
020600030701     c  n95              movel     orgdes        vflifgpd
020700030701     c   95              clear                   vflifgpd
020800030701     c     flifga        chain     azorg01l                           95
020900030701     c  n95              movel     orgdes        vflifgad
021000030701     c   95              clear                   vflifgad
021100030701      *data decorrenza
021200030701     c                   if        flidde > 0
021300030701     c                   move      flidde        dataiso
021400030701     c                   move      dataiso       dataeur
021500030701     c                   move      dataeur       vflidde
021600030701     c                   else
021700030701     c                   clear                   vflidsc
021800030701     c                   endif
021900030701      *data scadenza
022000030701     c                   if        flidsc > 0
022100030701     c                   move      flidsc        dataiso
022200030606     c                   move      dataiso       dataeur
022300030701     c                   move      dataeur       vflidsc
022400030630     c                   else
022500030701     c                   clear                   vflidsc
022600030606     c                   endif
022700030605     c                   exsr      Gesvideo3
022800030605     c                   endif
022900030605     C                   ENDSR
023000030605     c**********************************************************************
023100030605     C     copia         BEGSR
023200030605     c**********************************************************************
023300030701     c     kfli2         chain     fifli01l                           98
023400030701     c                   if        not *in98
023500030701     c                   move      FLIVAD        vFLIVAD
023600030701     c                   move      FLITER        vFLITER
023700030701     c                   move      FLIFGP        vFLIFGP
023800030701     c                   move      FLIFGA        vFLIFGA
023900030701     c                   move      FLIDDE        vFLIDDE
024000030701     c                   move      FLIORP        vFLIORP
024100030701     c                   move      FLIORA        vFLIORA
024200030703     c                   move      FLIPRV        vFLIPRV
024300081119     c*                  move      FLIIMP        vFLIIMP
024400030721     c                   clear                   vflipdr
024500030721     c                   if        flipdr > 0
024600030701     c                   move      FLIPDR        vFLIPDR
024700030721     c                   endif
024800030721     c                   move      FLIPDR        vFLIPDRk
024900030701     c                   move      FLINOT        vFLINOT
025000030701      * decodifica p.o.
025100030701     c     flifgp        chain     azorg01l                           95
025200030701     c  n95              movel     orgdes        vflifgpd
025300030701     c   95              clear                   vflifgpd
025400030701     c     flifga        chain     azorg01l                           95
025500030701     c  n95              movel     orgdes        vflifgad
025600030701     c   95              clear                   vflifgad
025700030701      *data decorrenza
025800030701     c                   if        flidde > 0
025900030701     c                   move      flidde        dataiso
026000030701     c                   move      dataiso       dataeur
026100030701     c                   move      dataeur       vflidde
026200030701     c                   else
026300030701     c                   clear                   vflidsc
026400030701     c                   endif
026500030701      *data scadenza
026600030701     c                   if        flidsc > 0
026700030701     c                   move      flidsc        dataiso
026800030701     c                   move      dataiso       dataeur
026900030701     c                   move      dataeur       vflidsc
027000030701     c                   else
027100030701     c                   clear                   vflidsc
027200030701     c                   endif
027300030701     c                   clear                   fifli000
027400030610     c                   exsr      Gesvideo3
027500030701     c                   endif
027600030605     C                   ENDSR
027700030605     c**********************************************************************
027800030605     C     gesvideo3     BEGSR
027900030605     c**********************************************************************
028000030702     c                   clear                   fatto
028100030605     c                   do        *hival
028200030605     c                   exfmt     video3
028300030605     c   kl              leave
028400040728     c  n21              exsr      controv3
028500030624     c   96              iter
028600040728     c   kf              exsr      aggiorna
028700030702     c                   if        fatto <> *blank
028800030702     c                   leave
028900030702     c                   endif
029000030605
029100030605     c                   enddo
029200030605     C                   ENDSR
029300030605     c**********************************************************************
029400030624     C     Controv3      BEGSR
029500030605     c**********************************************************************
029600030613      *
029700030702      * *in96 errore generico videata dettaglio
029800040728     c                   setoff                                       9628
029900040728     c                   clear                   $msg
030000040728     c                   if        modalita ='4'
030100040728     c                   goto      endctr
030200040728     c                   endif
030300030702      *tipo viaggio obbligatorio
030400030702     c                   setoff                                       40
030500030702     c                   if        vflivad =  *zeros
030600030702     c                   seton                                        4096
030700030702     c                   endif
030800030701      *filiale partenza obbligatoria
030900030702     c                   setoff                                       41
031000030701     c                   if        vflifgp =  *zeros
031100030624     c                   seton                                        4196
031200030613     c                   endif
031300030702      *filiale partenza = terminal per defluenza
031400030702     c                   setoff                                       43
031500030702     c                   if        vflifgp <> vfliter and vflivad = 'D'
031600030702     c                   seton                                        4396
031700030702     c                   endif
031701111027      *data decorrenza obbligatoria
031702111027     c                   setoff                                       53
031703111027     c                   if        vflidde =  *zeros
031704111027     c                   seton                                        5396
031705111027     c                   endif
031706111027      *controlli data  decorrenza
031707111027     c                   setoff                                       50
031708111027     c                   if        vflidde > 0
031709111027     C                   MOVE      Vflidde       G02DAT
031710111027     C                   MOVEL     *BLANK        G02ERR
031711111027     C                   CALL      'XSRDA8'
031712111027     C                   PARM                    WLBDA8
031713111027     C     G02ERR        IFEQ      '1'
031714111027     C                   SETON                                        5096
031715111027     C                   END
031716111027     c                   move      g02dat        vflidde
031717111027     c                   move      g02inv        vfliddeg          8 0
031718111027     c                   endif
031800030702      *controllo congruenza terminal e filiale x defluenza
031900030702     c                   if        vflivad = 'D'
032000030702     c                   setoff                                       46
032100030702     c                   clear                   fnlv55ds
032200030702     c                   move      'L'           d55tla
032300030702     c                   move      vflifga       d55lin
032301100617     c                   if        wuda >= §vpoter
032302100617     c                   move      'D'           d55tpt
032303100617     c                   else
032400030702     c                   move      'A'           d55tpt
032401100617     c                   endif
032500111027     c                   z-add     vfliddeg      d55drf
032501130527     c                   move      o36pou        D55LNP
032600030702     c                   call      'FNLV55R'
032700030702     c                   parm                    fnlv55ds
032800030702     c                   if        d55tfa <> vfliter
032900030702     c                   seton                                        4696
033000030702     c                   endif
033100030702     c                   endif
033200030701      *filiale arrivo  obbligatoria
033300030702     c                   setoff                                       42
033400030701     c                   if        vflifga =  *zeros
033500030701     c                   seton                                        4296
033600030701     c                   endif
033700030702      *filiale partenza = terminal per defluenza
033800030702     c                   setoff                                       45
033900030702     c                   if        vflifga <> vfliter and vflivad = 'A'
034000030702     c                   seton                                        4596
034100030702     c                   endif
034200030702      *controllo congruenza terminal e filiale x Affluenza
034300030702     c                   if        vflivad = 'A'
034400030702     c                   setoff                                       46
034500030702     c                   clear                   fnlv55ds
034600030702     c                   move      'L'           d55tla
034700030702     c                   move      vflifgp       d55lin
034800030702     c                   move      'P'           d55tpt
034900111027     c                   z-add     vfliddeg      d55drf
035000030702     c                   call      'FNLV55R'
035100030702     c                   parm                    fnlv55ds
035200030702     c                   if        d55tfp <> vfliter
035300030702     c                   seton                                        4696
035400030702     c                   endif
035500030702     c                   endif
035600030721     C****  CODICE PADRONCINO  ****
035700030721     C     '?'           SCAN      vflipdr                                55
035800030721     C*
035900030721     C     *IN55         IFEQ      *ON
036000030721     C                   MOVEL     KPJBU         SAVKPJBU
036100030721     C                   Z-ADD     Vfliter       d24FIL
036200030721     C                   Z-ADD     0             d24pdr
036300030721     C                   MOVEL     'R'           d24FLG
036400030721     C                   MOVEL     'D'           d24tip
036500030721     C                   MOVEL     *BLANKS       d24rsc
036600030721     C                   MOVEL(p)  fnlv24ds      KPJBU
036700030721     C                   CALL      'FNLV24R'
036800030721     C                   PARM                    KPJBA
036900030721     C                   MOVEL     KPJBU         fnlv24ds
037000030721     C                   MOVEL     savKPJBU      KPJBU
037100030721     C*
037200030721     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
037300030721     C     d24pdr        IFNE      0
037400030721     C                   MOVEL     *ZEROS        Vflipdr
037500030721     C                   MOVEL     d24pdr        Vflipdr
037600030721     C                   MOVEL     vflipdr       Vflipdrk
037700030721     C                   MOVEL     d24rsc        vflipdrd
037800031027     c                   else
037900031027     C                   clear                   Vflipdr
038000031027     C                   clear                   Vflipdrk
038100031027     C                   clear                   vflipdrd
038200030721     C                   ENDIF
038300030721     C*
038400030721     C                   ENDIF
038500030718      * decodifica autista
038600030721     c                   setoff                                       48
038700030721     c                   if        vflipdr <> *zeros and vflipdr <> *blank
038800030721     c                   move      vflipdr       vflipdrk
038900030721     c     kapd          chain     fiapd01l                           95
039000090507     c  n95              movel     apdrsf        vflipdrd
039100030721     c   95              clear                   vflipdrd
039200030721     c   95              seton                                        4896
039300030721     c                   endif
039400030703      * decodifica p.o.
039500030703     c     vflifgp       chain     azorg01l                           95
039600030703     c  n95              movel     orgdes        vflifgpd
039700030703     c   95              clear                   vflifgpd
039800030703     c     vflifga       chain     azorg01l                           95
039900030703     c  n95              movel     orgdes        vflifgad
040000030703     c   95              clear                   vflifgad
040100030702      *data decorrenza obbligatoria
040200030702     c                   setoff                                       53
040300030702     c                   if        vflidde =  *zeros
040400030702     c                   seton                                        5396
040500030702     c                   endif
040600030701      *controlli data  decorrenza
040700030613     c                   setoff                                       50
040800030701     c                   if        vflidde > 0
040900030701     C                   MOVE      Vflidde       G02DAT
041000030613     C                   MOVEL     *BLANK        G02ERR
041100030613     C                   CALL      'XSRDA8'
041200030613     C                   PARM                    WLBDA8
041300030613     C     G02ERR        IFEQ      '1'
041400030701     C                   SETON                                        5096
041500030613     C                   END
041600030701     c                   move      g02dat        vflidde
041700030702     c                   move      g02inv        vfliddeg          8 0
041800030613     c                   endif
041900030702      *data scadenza obbligatoria
042000030702     c                   setoff                                       54
042100030702     c                   if        vflidsc =  *zeros
042200030702     c                   seton                                        5496
042300030702     c                   endif
042400030702      *controlli data  scadenza
042500030702     c                   setoff                                       51
042600030701     c                   if        vflidsc > 0
042700030701     C                   MOVE      Vflidsc       G02DAT
042800030613     C                   MOVEL     *BLANK        G02ERR
042900030613     C                   CALL      'XSRDA8'
043000030613     C                   PARM                    WLBDA8
043100030613     C     G02ERR        IFEQ      '1'
043200030702     C                   SETON                                        5196
043300030613     C                   END
043400030701     c                   move      g02dat        vflidsc
043500030702     c                   move      g02inv        vflidscg          8 0
043600030613     c                   endif
043700030702      *controllo data decorrenza maggiore data scadenza
043800030702     c                   setoff                                       47
043900030703     c                   if        vfliddeg > vflidscg
044000030702     c                   seton                                        4796
044100030702     c                   endif
044200030702      *Importo obbligatorio
044300081119     c*                  setoff                                       49
044400081119     c*                  if        vfliimp =  *zeros
044500081119     c*                  seton                                        4996
044600081119     c*                  endif
044700030702      *controllo ore
044800030909     c                   setoff                                       55
044900030909     c                   if        vfliorp = 0
045000030909     c                   seton                                        5596
045100030702     c                   endif
045200030909     c                   setoff                                       56
045300030909     c                   if        vfliora = 0
045400030909     c                   seton                                        5696
045500030909     c                   endif
045600030909     c                   setoff                                       52
045601130807     c                   if        vfliorp < 2200
045700030909     c                   if        vfliorp > vfliora
045800130806     c                   seton                                        5296
045801130806     c                   endif
045900030909     c                   endif
046000040728     C     endctr        ENDSR
046100030605     c**********************************************************************
046200030605     C     aggiorna      BEGSR
046300030605     c**********************************************************************
046400030701     c                   move      vFLIVAD       FLIVAD
046500030701     c                   move      vFLITER       FLITER
046600030701     c                   move      vFLIFGP       FLIFGP
046700030701     c                   move      vFLIFGA       FLIFGA
046800030701     c                   move      vFLIORP       FLIORP
046900030701     c                   move      vFLIORA       FLIORA
047000030703     c                   move      vFLIPRV       FLIPRV
047100081119     c*                  move      vFLIIMP       FLIIMP
047200030721     c                   if        vflipdr <> *zeros and vflipdr <> *blank
047300030701     c                   move      vFLIPDR       FLIPDR
047400030721     c                   else
047500030721     c                   clear                   FLIPDR
047600030721     c                   endif
047700030701     c                   move      vFLINOT       FLINOT
047800030701     c                   if        vflidde > 0
047900030701     c                   move      vfliDde       dataeur
048000030610     c                   move      dataeur       dataiso
048100030701     c                   move      dataiso       fliDde
048200030610     c                   endif
048300030701     c                   if        vflidsc > 0
048400030701     c                   move      vflidsc       dataeur
048500030610     c                   move      dataeur       dataiso
048600030701     c                   move      dataiso       flidsc
048700030610     c                   endif
048800030610      * inserimento e copia scrive altrimenti aggiorna il record esistente
048900040728     c                   select
049000040728     c                   when      modalita = '4'
049100040728     c                   delete    fifli000
049200040728     c                   when      modalita = *blank or modalita = '3'
049300030701     c                   exsr      repnum
049400030701     c                   write     fifli000
049500040728     c                   other
049600030701     c                   update    fifli000
049700040728     c                   endsl
049800030702     c                   move      'X'           fatto
049900030605     C                   ENDSR
050000030619     c**********************************************************************
050100030619     c* reperimento numero contratto
050200030619     c**********************************************************************
050300030619     C     repnum        BEGSR
050400030703     c                   clear                   trul33ds
050500030703     c                   move      'L'           i33tla
050600030703     c                   z-add     33            i33cnu
050700030703     c                   z-add     1             i33num
050800030703     c                   movel     trul33ds      kpjbu
050900030703     c                   call      'TRUL33R'
051000030703     c                   PARM                    kpjba
051100030703     c                   movel     kpjbu         trul33ds
051200030703     c                   z-add     o33nrf        fliprv
051300030619     C                   ENDSR
051400011228     c**********************************************************************
051500011228     c* carica sfl
051600011228     c**********************************************************************
051700030604     C     Carvideo2     BEGSR
051800011228     C                   IF        WrkCarS01 = *ON
051900030604     C                   EVAL      snrr1   = 0
052000011228     C                   EVAL      *IN90 = *ON
052100020219     C                   EVAL      *IN91 = *OFF
052200030604     C                   WRITE     video2c
052300011228     C                   EVAL      *IN90 = *OFF
052400030701     c     kfli          setll     fifli01l
052500030701     c                   do        *hival
052600030701     c     kfli          reade     fifli01l                               97
052700030701     c   97              leave
052800030701     c                   move      *blanks       v1csce
052900030701     c                   move      FLIVAD        vFLIVAD
053000030701     c                   move      FLITER        vFLITER
053100030701     c     fliter        chain     azorg01l                           95
053200030701     c  n95              movel     orgdes        vfliterd
053300030701     c   95              clear                   vfliterd
053400030701     c                   move      FLIFGP        vFLIFGP
053500030701     c     flifgp        chain     azorg01l                           95
053600030701     c  n95              movel     orgdes        vflifgpd2
053700030701     c   95              clear                   vflifgpd2
053800030701     c                   move      FLIFGA        vFLIFGA
053900030701     c     flifga        chain     azorg01l                           95
054000030701     c  n95              movel     orgdes        vflifgad2
054100030701     c   95              clear                   vflifgad2
054200030701     c                   move      FLIORP        vFLIORP
054300030701     c                   move      FLIORA        vFLIORA
054400030703     c                   move      FLIPRV        vFLIPRV
054500030721     c                   if        flipdr > 0
054600030701     c                   move      FLIPDR        vFLIPDR
054700030721     c                   else
054800030721     c                   clear                   vFLIPDR
054900030721     c                   endif
055000030701      *data decorrenza
055100030701     c                   if        flidde > 0
055200030701     c                   move      flidde        dataiso
055300030701     c                   move      dataiso       dataeur
055400030701     c                   move      dataeur       vflidde
055500030701     c                   else
055600030701     c                   clear                   vflidsc
055700030701     c                   endif
055800030701      *data scadenza
055900030701     c                   if        flidsc > 0
056000030701     c                   move      flidsc        dataiso
056100030701     c                   move      dataiso       dataeur
056200030701     c                   move      dataeur       vflidsc
056300030701     c                   else
056400030701     c                   clear                   vflidsc
056500030701     c                   endif
056600030701      *esco per raggiunta massima capacità sfl
056700030617     c                   if        snrr1 > 9990
056800030617     c                   leave
056900030617     c                   endif
057000030617
057100030604     C                   EVAL      snrr1 = snrr1 + 1
057200030604     C                   WRITE     video2
057300011228     C                   ENDDO
057400011228
057500030604     C                   IF        snrr1  > 0
057600020219     C                   EVAL      *IN91 = *ON
057700020219     C                   MOVE      1             OK                1 0
057800020219     C                   ELSE
057900020219     C                   MOVE      0             OK                1 0
058000030701     C                   ENDIF
058100030612     C                   EVAL      nrr1  = 1
058200011228     C                   ENDIF
058300020219     C                   EVAL      WrkCarS01 = *OFF
058400030604     C                   EVAL      snrr1  = 1
058500030604     C                   EVAL      csrrrn = 1
058600011228     C                   ENDSR
058700011227     c**********************************************************************
058800011227     c* gestione sfl
058900011227     c**********************************************************************
059000000613     C     GestioneSFL   BEGSR
059100000627      *
059200000627     C                   EVAL      WrkEofS01 = *OFF
059300000627      * F10 Immissione.
059400040728     C                   IF        *INKJ
059500040728     c                   move      vfliter       savfliter
059600040728     c                   move      vfliterd      savfliterd
059700040728     c                   clear                   modalita
059800040728     c                   clear                   video3
059900040728     c                   clear                   fifli000
060000040728     c                   move      knmus         vknmus
060100040728     c                   move      knsif         vknsif
060200040728     c                   movel     ragut         rsut
060300040728     c                   move      savfliter     vfliter
060400040728     c                   move      savfliterd    vfliterd
060500040728     c                   exsr      Gesvideo3
060600040728     c                   eval      wrkcars01 = *on
060700040728     c                   if        csrrrn = 0
060800040728     c                   goto      noriga
060900040728     c                   endif
061000040728     c                   endif
061100030605      * Elaborazione opzioni.
061200011228     c*
061300030605     c                   do        *hival        X                 4 0
061400030605     c     X             CHAIN     video2                             50
061500030605     C   50              LEAVE
061600030604     C                   IF        v1csce <> *blanks
061700030605     c                   exsr      opzione
061800030611
061900030611     c                   move      *blank        v1csce
062000030605     c                   update    video2
062100030611
062200011228     C                   ENDIF
062300030605     C                   enddo
062400040728      *
062500040728     c                   if        modalita = '1' or modalita = '3'
062600040728     c                             or modalita = '4'
062700040728     c                   eval      wrkcars01 = *on
062800040728     c                   endif
062900000627      *
063000031003     C     noriga        ENDSR
063100011227     c**********************************************************************
063200000607     C     *INZSR        BEGSR
063300011227     c**********************************************************************
063400000607      *
063500000607     C     *ENTRY        PLIST
063600000607     C                   PARM                    Kpjba
063700030604     C                   Z-ADD     1             CODUT
063800030604     C                   CALL      'X§PARUT'
063900030604     C                   PARM                    UTEDSE
064000030604     C                   MOVEL     REC80         CNCR80
064100030703     C                   MOVEL     Ragut         rsut
064200030703     C                   MOVEL     knsif         vknsif
064300030703     C                   MOVEL     knmus         vknmus
064400030702      *reperimento data
064500030702     C                   TIME                    W0120            14 0
064600030702     C                   MOVE      W0120         WDAT              8 0
064700030702     C*
064800030702     C                   Z-ADD     WDAT          G02DAT
064900030702     C                   MOVEL     *BLANK        G02ERR
065000030702     C                   CALL      'XSRDA8'
065100030702     C                   PARM                    WLBDA8
065200030702     C* UDATE A 8 IN AAAA/MM/GG
065300030702     C                   Z-ADD     G02INV        WUDA              8 0
065400030606      *profilo di filiale attivo indicatore di protezione campi non manutenzionabili
065500030617     C                   CLEAR                   Tibs36Ds
065600030617     C                   EVAL      I36ute = knmus
065700030617     C                   CALL      'TIBS36R'
065800030617     C                   PARM                    tibs36ds
065801100617      * controllo se attivare o no controllo autorizzazioni e gestione terminal
065802100617     c                   clear                   tibs02ds
065803100617     c                   clear                   dvpocont
065804100617     c                   eval      t02mod = 'C'
065805100617     c                   eval      t02sif = knsif
065806100617     c                   eval      t02cod = 'VPO'
065807100617     c                   eval      t02ke1 = 'CONT'
065808100617     c                   call      'TIBS02R'
065809100617     c                   parm                    kpjba
065810100617     c                   parm                    tibs02ds
065811100617    2c                   if        t02err = *blanks
065812100617     c                   eval      dvpocont  = t02uni
065813100617    2c                   endif
065900030617     c                   if        o36pou <> 046
066000060915     c                   clear                   fnlv55ds
066100060915     c                   move      'L'           d55tla
066200060915     c                   move      o36pou        d55lin
066201100617     c                   if        wuda >= §vpoter
066202100617     c                   move      'E'           d55tpt
066203100617     c                   else
066300060915     c                   move      ' '           d55tpt
066301100617     c                   endif
066400060915     c                   z-add     wuda          d55drf
066500060915     c                   call      'FNLV55R'
066600060915     c                   parm                    fnlv55ds
066700060915     c                   z-add     d55tfp        termp             3 0
066800060915     c                   z-add     d55tfa        terma             3 0
066900060915      *se profilo di filiale e i terminal P/A sono uguali proteggo il campo
067000060915     c                   if        terma = termp
067100060915     c                   seton                                        20
067200060915     c                   endif
067300030617     c                   endif
067400030718
067500030718     C     Kapd          KLIST
067600030718     C                   KFLD                    tipoa             1
067700030721     C                   KFLD                    vflipdrk          7 0
067800030718     C                   move      'D'           tipoa
067900000607      *
068000030701     C     Kfli2         KLIST
068100030701     C                   KFLD                    vfliter
068200030701     C                   KFLD                    vflivad
068300030701     C                   KFLD                    vflifgp
068400030701     C                   KFLD                    vflifga
068500030703     C                   KFLD                    vfliprv
068600030701     C     Kfli          KLIST
068700030701     C                   KFLD                    vfliter
068800000607      *---------------------------------------------------------------------
068900011227     C                   ENDSR
