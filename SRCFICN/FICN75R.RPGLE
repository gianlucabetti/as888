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
001600131007     D uteautds      E DS
001700131007     d tibs42ds      e ds
001800100617     d dvpocont      e ds
001900100617     d tibs02ds      e ds
002000030604     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
002100030604     D CNCR80        E DS
002200030617     d Tibs36Ds      e ds
002300011227     d data            ds            10
002400011228     d aa                      3      4
002500011228     d mm                      6      7
002600011228     d gg                      9     10
002700011228      *
002800030702     D fatto           S              1
002900030610     D modalita        S              1
003000030604     D x               S              4  0
003100011227     D WrkEofS01       S              1
003200011227     D WrkCarS01       S              1
003300011228     D $VIDEO          S             10
003400030604     D snrr1           S              5i 0
003500030721     D Savkpjbu        S                   like(kpjbu)
003600030604     D SavOpzione      S                   like(v1csce)
003700030702     D Savfliter       S                   like(vfliter)
003800030702     D Savfliterd      S                   like(vfliterd)
003900030606     d dataiso         s               d   datfmt(*iso)
004000030606     d dataeur         s               d   datfmt(*eur)
004100030613     D WLBDA8          DS
004200030613     D  G02DAT                 1      8  0
004300030613     D  G02INV                 9     16  0
004400030613     D  G02ERR                17     17
004500030613     D  G02TGI                18     22  0
004600030701      *---------------- ----------------------------------------------------
004700030604      *---------------- Gestione formato video video01----------------------
004800030604     C                   EXSR      Inzvideo1
004900030604     C                   DOU       $Video <> 'VIDEO1'
005000000608      *----------- Visualizzo dati di output in caso di errori.-------------
005100000608     C                   IF        *IN99
005200000608     C                   EVAL      *IN99 = *OFF
005300030604     C                   WRITE     video1
005400000608     C                   EVAL      *IN99 = *ON
005500000608     C                   ENDIF
005600000608      *
005700030702     c     emetti        tag
005800030604     C                   EXFMT     video1
005900160908     c                   clear                   $msg
006000000608      *
006100011227     C                   SELECT
006200030612     C                   WHEN      *INKC
006300011227     C                             OR
006400011227     C                             *INKL
006500011227     C                   EVAL      $Video = *BLANKS
006600011227
006700011227     C                   WHEN      *INKE
006800030604     C                   EXSR      Inzvideo1
006900011227     C                   WHEN      *INKB
007000030604     C                   EXSR      Chkvideo1
007100011227     C                   OTHER
007200030604     C                   EXSR      Chkvideo1
007300040728     c   84              goto      emetti
007400030604     C  N99              EXSR      Wrkvideo2c
007500011227     C                   ENDSL
007600011227      *
007700011227     C                   ENDDO
007800011227      *
007900011227     C                   EXSR      Uscita
008000011227     c**********************************************************************
008100011227     c* uscita
008200011227     c**********************************************************************
008300011227     C     Uscita        BEGSR
008400011227      *
008500011227     C                   EVAL      *INLR = *ON
008600011227     C                   RETURN
008700011227      *
008800011227     C                   ENDSR
008900011227     c**********************************************************************
009000011227     c* gestione sfl
009100011227     c**********************************************************************
009200030604     C     Wrkvideo2c    BEGSR
009300000627      *
009400011227     C                   EVAL      WrkCarS01 = *ON
009500030604     C                   EVAL      $Video = 'VIDEO2C'
009600011227      *
009700030604     C                   DOU       $Video <> 'VIDEO2C'
009800030604     C                   EXSR      Carvideo2
009900011228     c*Descrizione della ricerca
010000000627      *
010100030604     C                   WRITE     video2z
010200030604     C                   EXFMT     video2c
010300011227     C                   SELECT
010400011228     c* fine
010500011227     C                   WHEN      *INKC=*ON
010600011227     C                   EVAL      $VIDEO=*BLANKS
010700011228     c* guida
010800011227     C                   WHEN      *INKL=*ON
010900030604     C                   EVAL      $VIDEO='VIDEO1'
011000011228     c* ripristino
011100011227     C                   WHEN      *INKE=*ON
011200011227     C                   EVAL      WrkCarS01 = *ON
011300011227
011400011227     C                   OTHER
011500011227
011600011227     C                   EXSR      GestioneSFL
011700011227     C                   ENDSL
011800011228
011900011227     C                   ENDDO
012000031027     C                   ENDSR
012100011227     c**********************************************************************
012200011227     c* Controlli video R01.
012300011227     c**********************************************************************
012400030604     C     Chkvideo1     BEGSR
012500000609      *
012600000608     C                   EVAL      *IN99 = *OFF
012700040728     C                   EVAL      *IN84 = *OFF
012800160908     C                   EVAL      *IN28 = *OFF
012900060915      * profilo di filiale controllo che il terminal sia congruente
013000160908     c*m                 if        o36pou <> 046
013100160908     c*m                 if        vfliter <> terma and vfliter <> termp
013200160908     c*m                 seton                                        84
013300160908     c*m                 endif
013400160908     c*m                 else
013500060915
013600030702     c                   clear                   fnlv55ds
013700030702     c                   move      'L'           d55tla
013800030702     c                   move      vfliter       d55lin
013900100617     c                   if        wuda >= §vpoter
014000100617     c                   move      'E'           d55tpt
014100100617     c                   else
014200100617     c                   move      ' '           d55tpt
014300100617     c                   endif
014400030702     c                   z-add     wuda          d55drf
014500030702     c                   call      'FNLV55R'
014600030702     c                   parm                    fnlv55ds
014700030702     c                   if        vfliter <> d55tfp and vfliter <> d55tfa
014800030703     c                             or vfliter = *zeros
014900040728     c                   seton                                        84
015000030702     c                   endif
015100160908     c*m                 endif
015200030701     c     vfliter       chain     azorg01l
015300030701     c                   movel     orgdes        vfliterd
015400130812      * controllo autorizzazione del profilo
015500130812     c                   if        wuda    >= §vpoaut
015600131007     c                   clear                   tibs42ds
015700131007     c                   movel     vfliter       i42pge
015800131007     c                   call      'TIBS42R'
015900131007     c                   parm                    TIBS42ds
016000131007     c                   movel     o42uni        uteautds
016100130812     c                   if        §AFFGVSTD <> 'S'
016200130812     c                   eval      $msg = 'Profilo non autorizzato al terminal'
016300130812     c                   seton                                        8428
016400130812     c                   endif
016500130812     c                   endif
016600000608      *
016700000608     C                   ENDSR
016800011227     c**********************************************************************
016900011227     c* inizializza r01
017000011227     c**********************************************************************
017100030604     C     Inzvideo1     BEGSR
017200000608
017300030702     c                   if        o36pou <> 046
017400060915     c                   move      termp         vfliter
017500030702     c                   endif
017600030604     C                   EXSR      Chkvideo1
017700000609
017800000608     C                   ENDSR
017900030604     c**********************************************************************
018000030605     C     opzione       BEGSR
018100030605     c**********************************************************************
018200030610     c                   move      v1csce        modalita
018300040728     c                   movea     '000000000000'*in(40)
018400040728     c                   movea     '00000'       *in(52)
018500040728     c                   setoff                                       96
018600030605     c                   select
018700030605     c                   when      v1csce = '2'
018800030605     c                   exsr      modifica
018900030605     c                   when      v1csce = '3'
019000030605     c                   exsr      copia
019100030605     c                   when      v1csce = '5'
019200030606     c                   seton                                        21
019300030606     c                   exsr      modifica
019400030606     c                   setoff                                       21
019500040728     c                   when      v1csce = '4'
019600040728     c                   seton                                        21
019700040728     c                   exsr      modifica
019800040728     c                   setoff                                       2128
019900040728     c                   clear                   $msg
020000030605     c                   endsl
020100030604     C                   ENDSR
020200030605     c**********************************************************************
020300030605     C     modifica      BEGSR
020400030605     c**********************************************************************
020500030701     c     kfli2         chain     fifli01l                           98
020600030610     c                   if        not *in98
020700030701     c                   move      FLIVAD        vFLIVAD
020800030701     c                   move      FLITER        vFLITER
020900030701     c                   move      FLIFGP        vFLIFGP
021000030701     c                   move      FLIFGA        vFLIFGA
021100030701     c                   move      FLIDDE        vFLIDDE
021200030701     c                   move      FLIORP        vFLIORP
021300030701     c                   move      FLIORA        vFLIORA
021400030703     c                   move      FLIPRV        vFLIPRV
021500081119     c*                  move      FLIIMP        vFLIIMP
021600030721     c                   clear                   vflipdr
021700030721     c                   if        flipdr > 0
021800030701     c                   move      FLIPDR        vFLIPDR
021900030721     c                   endif
022000030721     c                   move      FLIPDR        vFLIPDRk
022100030701     c                   move      FLINOT        vFLINOT
022200030718      * decodifica autista
022300030718     c     kapd          chain     fiapd01l                           95
022400090507     c  n95              movel     apdrsf        vflipdrd
022500030718     c   95              clear                   vflipdrd
022600030701      * decodifica p.o.
022700030701     c     flifgp        chain     azorg01l                           95
022800030701     c  n95              movel     orgdes        vflifgpd
022900030701     c   95              clear                   vflifgpd
023000030701     c     flifga        chain     azorg01l                           95
023100030701     c  n95              movel     orgdes        vflifgad
023200030701     c   95              clear                   vflifgad
023300030701      *data decorrenza
023400030701     c                   if        flidde > 0
023500030701     c                   move      flidde        dataiso
023600030701     c                   move      dataiso       dataeur
023700030701     c                   move      dataeur       vflidde
023800030701     c                   else
023900030701     c                   clear                   vflidsc
024000030701     c                   endif
024100030701      *data scadenza
024200030701     c                   if        flidsc > 0
024300030701     c                   move      flidsc        dataiso
024400030606     c                   move      dataiso       dataeur
024500030701     c                   move      dataeur       vflidsc
024600030630     c                   else
024700030701     c                   clear                   vflidsc
024800030606     c                   endif
024900030605     c                   exsr      Gesvideo3
025000030605     c                   endif
025100030605     C                   ENDSR
025200030605     c**********************************************************************
025300030605     C     copia         BEGSR
025400030605     c**********************************************************************
025500030701     c     kfli2         chain     fifli01l                           98
025600030701     c                   if        not *in98
025700030701     c                   move      FLIVAD        vFLIVAD
025800030701     c                   move      FLITER        vFLITER
025900030701     c                   move      FLIFGP        vFLIFGP
026000030701     c                   move      FLIFGA        vFLIFGA
026100030701     c                   move      FLIDDE        vFLIDDE
026200030701     c                   move      FLIORP        vFLIORP
026300030701     c                   move      FLIORA        vFLIORA
026400030703     c                   move      FLIPRV        vFLIPRV
026500081119     c*                  move      FLIIMP        vFLIIMP
026600030721     c                   clear                   vflipdr
026700030721     c                   if        flipdr > 0
026800030701     c                   move      FLIPDR        vFLIPDR
026900030721     c                   endif
027000030721     c                   move      FLIPDR        vFLIPDRk
027100030701     c                   move      FLINOT        vFLINOT
027200030701      * decodifica p.o.
027300030701     c     flifgp        chain     azorg01l                           95
027400030701     c  n95              movel     orgdes        vflifgpd
027500030701     c   95              clear                   vflifgpd
027600030701     c     flifga        chain     azorg01l                           95
027700030701     c  n95              movel     orgdes        vflifgad
027800030701     c   95              clear                   vflifgad
027900030701      *data decorrenza
028000030701     c                   if        flidde > 0
028100030701     c                   move      flidde        dataiso
028200030701     c                   move      dataiso       dataeur
028300030701     c                   move      dataeur       vflidde
028400030701     c                   else
028500030701     c                   clear                   vflidsc
028600030701     c                   endif
028700030701      *data scadenza
028800030701     c                   if        flidsc > 0
028900030701     c                   move      flidsc        dataiso
029000030701     c                   move      dataiso       dataeur
029100030701     c                   move      dataeur       vflidsc
029200030701     c                   else
029300030701     c                   clear                   vflidsc
029400030701     c                   endif
029500030701     c                   clear                   fifli000
029600030610     c                   exsr      Gesvideo3
029700030701     c                   endif
029800030605     C                   ENDSR
029900030605     c**********************************************************************
030000030605     C     gesvideo3     BEGSR
030100030605     c**********************************************************************
030200030702     c                   clear                   fatto
030300030605     c                   do        *hival
030400030605     c                   exfmt     video3
030500030605     c   kl              leave
030600040728     c  n21              exsr      controv3
030700030624     c   96              iter
030800040728     c   kf              exsr      aggiorna
030900030702     c                   if        fatto <> *blank
031000030702     c                   leave
031100030702     c                   endif
031200030605
031300030605     c                   enddo
031400030605     C                   ENDSR
031500030605     c**********************************************************************
031600030624     C     Controv3      BEGSR
031700030605     c**********************************************************************
031800030613      *
031900030702      * *in96 errore generico videata dettaglio
032000040728     c                   setoff                                       9628
032100040728     c                   clear                   $msg
032200040728     c                   if        modalita ='4'
032300040728     c                   goto      endctr
032400040728     c                   endif
032500030702      *tipo viaggio obbligatorio
032600030702     c                   setoff                                       40
032700030702     c                   if        vflivad =  *zeros
032800030702     c                   seton                                        4096
032900030702     c                   endif
033000030701      *filiale partenza obbligatoria
033100030702     c                   setoff                                       41
033200030701     c                   if        vflifgp =  *zeros
033300030624     c                   seton                                        4196
033400030613     c                   endif
033500030702      *filiale partenza = terminal per defluenza
033600030702     c                   setoff                                       43
033700030702     c                   if        vflifgp <> vfliter and vflivad = 'D'
033800030702     c                   seton                                        4396
033900030702     c                   endif
034000111027      *data decorrenza obbligatoria
034100111027     c                   setoff                                       53
034200111027     c                   if        vflidde =  *zeros
034300111027     c                   seton                                        5396
034400111027     c                   endif
034500111027      *controlli data  decorrenza
034600111027     c                   setoff                                       50
034700111027     c                   if        vflidde > 0
034800111027     C                   MOVE      Vflidde       G02DAT
034900111027     C                   MOVEL     *BLANK        G02ERR
035000111027     C                   CALL      'XSRDA8'
035100111027     C                   PARM                    WLBDA8
035200111027     C     G02ERR        IFEQ      '1'
035300111027     C                   SETON                                        5096
035400111027     C                   END
035500111027     c                   move      g02dat        vflidde
035600111027     c                   move      g02inv        vfliddeg          8 0
035700111027     c                   endif
035800030702      *controllo congruenza terminal e filiale x defluenza
035900030702     c                   if        vflivad = 'D'
036000030702     c                   setoff                                       46
036100030702     c                   clear                   fnlv55ds
036200030702     c                   move      'L'           d55tla
036300030702     c                   move      vflifga       d55lin
036400100617     c                   if        wuda >= §vpoter
036500100617     c                   move      'D'           d55tpt
036600100617     c                   else
036700030702     c                   move      'A'           d55tpt
036800100617     c                   endif
036900111027     c                   z-add     vfliddeg      d55drf
037000130527     c                   move      o36pou        D55LNP
037100030702     c                   call      'FNLV55R'
037200030702     c                   parm                    fnlv55ds
037300030702     c                   if        d55tfa <> vfliter
037400030702     c                   seton                                        4696
037500030702     c                   endif
037600030702     c                   endif
037700030701      *filiale arrivo  obbligatoria
037800030702     c                   setoff                                       42
037900030701     c                   if        vflifga =  *zeros
038000030701     c                   seton                                        4296
038100030701     c                   endif
038200030702      *filiale partenza = terminal per defluenza
038300030702     c                   setoff                                       45
038400030702     c                   if        vflifga <> vfliter and vflivad = 'A'
038500030702     c                   seton                                        4596
038600030702     c                   endif
038700030702      *controllo congruenza terminal e filiale x Affluenza
038800030702     c                   if        vflivad = 'A'
038900030702     c                   setoff                                       46
039000030702     c                   clear                   fnlv55ds
039100030702     c                   move      'L'           d55tla
039200030702     c                   move      vflifgp       d55lin
039300030702     c                   move      'P'           d55tpt
039400111027     c                   z-add     vfliddeg      d55drf
039500030702     c                   call      'FNLV55R'
039600030702     c                   parm                    fnlv55ds
039700030702     c                   if        d55tfp <> vfliter
039800030702     c                   seton                                        4696
039900030702     c                   endif
040000030702     c                   endif
040100030721     C****  CODICE PADRONCINO  ****
040200030721     C     '?'           SCAN      vflipdr                                55
040300030721     C*
040400030721     C     *IN55         IFEQ      *ON
040500030721     C                   MOVEL     KPJBU         SAVKPJBU
040600030721     C                   Z-ADD     Vfliter       d24FIL
040700030721     C                   Z-ADD     0             d24pdr
040800030721     C                   MOVEL     'R'           d24FLG
040900030721     C                   MOVEL     'D'           d24tip
041000030721     C                   MOVEL     *BLANKS       d24rsc
041100030721     C                   MOVEL(p)  fnlv24ds      KPJBU
041200030721     C                   CALL      'FNLV24R'
041300030721     C                   PARM                    KPJBA
041400030721     C                   MOVEL     KPJBU         fnlv24ds
041500030721     C                   MOVEL     savKPJBU      KPJBU
041600030721     C*
041700030721     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
041800030721     C     d24pdr        IFNE      0
041900030721     C                   MOVEL     *ZEROS        Vflipdr
042000030721     C                   MOVEL     d24pdr        Vflipdr
042100030721     C                   MOVEL     vflipdr       Vflipdrk
042200030721     C                   MOVEL     d24rsc        vflipdrd
042300031027     c                   else
042400031027     C                   clear                   Vflipdr
042500031027     C                   clear                   Vflipdrk
042600031027     C                   clear                   vflipdrd
042700030721     C                   ENDIF
042800030721     C*
042900030721     C                   ENDIF
043000030718      * decodifica autista
043100030721     c                   setoff                                       48
043200030721     c                   if        vflipdr <> *zeros and vflipdr <> *blank
043300030721     c                   move      vflipdr       vflipdrk
043400030721     c     kapd          chain     fiapd01l                           95
043500090507     c  n95              movel     apdrsf        vflipdrd
043600030721     c   95              clear                   vflipdrd
043700030721     c   95              seton                                        4896
043800030721     c                   endif
043900030703      * decodifica p.o.
044000030703     c     vflifgp       chain     azorg01l                           95
044100030703     c  n95              movel     orgdes        vflifgpd
044200030703     c   95              clear                   vflifgpd
044300030703     c     vflifga       chain     azorg01l                           95
044400030703     c  n95              movel     orgdes        vflifgad
044500030703     c   95              clear                   vflifgad
044600030702      *data decorrenza obbligatoria
044700030702     c                   setoff                                       53
044800030702     c                   if        vflidde =  *zeros
044900030702     c                   seton                                        5396
045000030702     c                   endif
045100030701      *controlli data  decorrenza
045200030613     c                   setoff                                       50
045300030701     c                   if        vflidde > 0
045400030701     C                   MOVE      Vflidde       G02DAT
045500030613     C                   MOVEL     *BLANK        G02ERR
045600030613     C                   CALL      'XSRDA8'
045700030613     C                   PARM                    WLBDA8
045800030613     C     G02ERR        IFEQ      '1'
045900030701     C                   SETON                                        5096
046000030613     C                   END
046100030701     c                   move      g02dat        vflidde
046200030702     c                   move      g02inv        vfliddeg          8 0
046300030613     c                   endif
046400030702      *data scadenza obbligatoria
046500030702     c                   setoff                                       54
046600030702     c                   if        vflidsc =  *zeros
046700030702     c                   seton                                        5496
046800030702     c                   endif
046900030702      *controlli data  scadenza
047000030702     c                   setoff                                       51
047100030701     c                   if        vflidsc > 0
047200030701     C                   MOVE      Vflidsc       G02DAT
047300030613     C                   MOVEL     *BLANK        G02ERR
047400030613     C                   CALL      'XSRDA8'
047500030613     C                   PARM                    WLBDA8
047600030613     C     G02ERR        IFEQ      '1'
047700030702     C                   SETON                                        5196
047800030613     C                   END
047900030701     c                   move      g02dat        vflidsc
048000030702     c                   move      g02inv        vflidscg          8 0
048100030613     c                   endif
048200030702      *controllo data decorrenza maggiore data scadenza
048300030702     c                   setoff                                       47
048400030703     c                   if        vfliddeg > vflidscg
048500030702     c                   seton                                        4796
048600030702     c                   endif
048700030702      *Importo obbligatorio
048800081119     c*                  setoff                                       49
048900081119     c*                  if        vfliimp =  *zeros
049000081119     c*                  seton                                        4996
049100081119     c*                  endif
049200030702      *controllo ore
049300030909     c                   setoff                                       55
049400030909     c                   if        vfliorp = 0
049500030909     c                   seton                                        5596
049600030702     c                   endif
049700030909     c                   setoff                                       56
049800030909     c                   if        vfliora = 0
049900030909     c                   seton                                        5696
050000030909     c                   endif
050100030909     c                   setoff                                       52
050200130807     c                   if        vfliorp < 2200
050300030909     c                   if        vfliorp > vfliora
050400130806     c                   seton                                        5296
050500130806     c                   endif
050600030909     c                   endif
050700040728     C     endctr        ENDSR
050800030605     c**********************************************************************
050900030605     C     aggiorna      BEGSR
051000030605     c**********************************************************************
051100030701     c                   move      vFLIVAD       FLIVAD
051200030701     c                   move      vFLITER       FLITER
051300030701     c                   move      vFLIFGP       FLIFGP
051400030701     c                   move      vFLIFGA       FLIFGA
051500030701     c                   move      vFLIORP       FLIORP
051600030701     c                   move      vFLIORA       FLIORA
051700030703     c                   move      vFLIPRV       FLIPRV
051800081119     c*                  move      vFLIIMP       FLIIMP
051900030721     c                   if        vflipdr <> *zeros and vflipdr <> *blank
052000030701     c                   move      vFLIPDR       FLIPDR
052100030721     c                   else
052200030721     c                   clear                   FLIPDR
052300030721     c                   endif
052400030701     c                   move      vFLINOT       FLINOT
052500030701     c                   if        vflidde > 0
052600030701     c                   move      vfliDde       dataeur
052700030610     c                   move      dataeur       dataiso
052800030701     c                   move      dataiso       fliDde
052900030610     c                   endif
053000030701     c                   if        vflidsc > 0
053100030701     c                   move      vflidsc       dataeur
053200030610     c                   move      dataeur       dataiso
053300030701     c                   move      dataiso       flidsc
053400030610     c                   endif
053500030610      * inserimento e copia scrive altrimenti aggiorna il record esistente
053600040728     c                   select
053700040728     c                   when      modalita = '4'
053800040728     c                   delete    fifli000
053900040728     c                   when      modalita = *blank or modalita = '3'
054000030701     c                   exsr      repnum
054100030701     c                   write     fifli000
054200040728     c                   other
054300030701     c                   update    fifli000
054400040728     c                   endsl
054500030702     c                   move      'X'           fatto
054600030605     C                   ENDSR
054700030619     c**********************************************************************
054800030619     c* reperimento numero contratto
054900030619     c**********************************************************************
055000030619     C     repnum        BEGSR
055100030703     c                   clear                   trul33ds
055200030703     c                   move      'L'           i33tla
055300030703     c                   z-add     33            i33cnu
055400030703     c                   z-add     1             i33num
055500030703     c                   movel     trul33ds      kpjbu
055600030703     c                   call      'TRUL33R'
055700030703     c                   PARM                    kpjba
055800030703     c                   movel     kpjbu         trul33ds
055900030703     c                   z-add     o33nrf        fliprv
056000030619     C                   ENDSR
056100011228     c**********************************************************************
056200011228     c* carica sfl
056300011228     c**********************************************************************
056400030604     C     Carvideo2     BEGSR
056500011228     C                   IF        WrkCarS01 = *ON
056600030604     C                   EVAL      snrr1   = 0
056700011228     C                   EVAL      *IN90 = *ON
056800020219     C                   EVAL      *IN91 = *OFF
056900030604     C                   WRITE     video2c
057000011228     C                   EVAL      *IN90 = *OFF
057100030701     c     kfli          setll     fifli01l
057200030701     c                   do        *hival
057300030701     c     kfli          reade     fifli01l                               97
057400030701     c   97              leave
057500030701     c                   move      *blanks       v1csce
057600030701     c                   move      FLIVAD        vFLIVAD
057700030701     c                   move      FLITER        vFLITER
057800030701     c     fliter        chain     azorg01l                           95
057900030701     c  n95              movel     orgdes        vfliterd
058000030701     c   95              clear                   vfliterd
058100030701     c                   move      FLIFGP        vFLIFGP
058200030701     c     flifgp        chain     azorg01l                           95
058300030701     c  n95              movel     orgdes        vflifgpd2
058400030701     c   95              clear                   vflifgpd2
058500030701     c                   move      FLIFGA        vFLIFGA
058600030701     c     flifga        chain     azorg01l                           95
058700030701     c  n95              movel     orgdes        vflifgad2
058800030701     c   95              clear                   vflifgad2
058900030701     c                   move      FLIORP        vFLIORP
059000030701     c                   move      FLIORA        vFLIORA
059100030703     c                   move      FLIPRV        vFLIPRV
059200030721     c                   if        flipdr > 0
059300030701     c                   move      FLIPDR        vFLIPDR
059400030721     c                   else
059500030721     c                   clear                   vFLIPDR
059600030721     c                   endif
059700030701      *data decorrenza
059800030701     c                   if        flidde > 0
059900030701     c                   move      flidde        dataiso
060000030701     c                   move      dataiso       dataeur
060100030701     c                   move      dataeur       vflidde
060200030701     c                   else
060300030701     c                   clear                   vflidsc
060400030701     c                   endif
060500030701      *data scadenza
060600030701     c                   if        flidsc > 0
060700030701     c                   move      flidsc        dataiso
060800030701     c                   move      dataiso       dataeur
060900030701     c                   move      dataeur       vflidsc
061000030701     c                   else
061100030701     c                   clear                   vflidsc
061200030701     c                   endif
061300030701      *esco per raggiunta massima capacità sfl
061400030617     c                   if        snrr1 > 9990
061500030617     c                   leave
061600030617     c                   endif
061700030617
061800030604     C                   EVAL      snrr1 = snrr1 + 1
061900030604     C                   WRITE     video2
062000011228     C                   ENDDO
062100011228
062200030604     C                   IF        snrr1  > 0
062300020219     C                   EVAL      *IN91 = *ON
062400020219     C                   MOVE      1             OK                1 0
062500020219     C                   ELSE
062600020219     C                   MOVE      0             OK                1 0
062700030701     C                   ENDIF
062800030612     C                   EVAL      nrr1  = 1
062900011228     C                   ENDIF
063000020219     C                   EVAL      WrkCarS01 = *OFF
063100030604     C                   EVAL      snrr1  = 1
063200030604     C                   EVAL      csrrrn = 1
063300011228     C                   ENDSR
063400011227     c**********************************************************************
063500011227     c* gestione sfl
063600011227     c**********************************************************************
063700000613     C     GestioneSFL   BEGSR
063800000627      *
063900000627     C                   EVAL      WrkEofS01 = *OFF
064000000627      * F10 Immissione.
064100040728     C                   IF        *INKJ
064200040728     c                   move      vfliter       savfliter
064300040728     c                   move      vfliterd      savfliterd
064400040728     c                   clear                   modalita
064500040728     c                   clear                   video3
064600040728     c                   clear                   fifli000
064700040728     c                   move      knmus         vknmus
064800040728     c                   move      knsif         vknsif
064900040728     c                   movel     ragut         rsut
065000040728     c                   move      savfliter     vfliter
065100040728     c                   move      savfliterd    vfliterd
065200040728     c                   exsr      Gesvideo3
065300040728     c                   eval      wrkcars01 = *on
065400040728     c                   if        csrrrn = 0
065500040728     c                   goto      noriga
065600040728     c                   endif
065700040728     c                   endif
065800030605      * Elaborazione opzioni.
065900011228     c*
066000030605     c                   do        *hival        X                 4 0
066100030605     c     X             CHAIN     video2                             50
066200030605     C   50              LEAVE
066300030604     C                   IF        v1csce <> *blanks
066400030605     c                   exsr      opzione
066500030611
066600030611     c                   move      *blank        v1csce
066700030605     c                   update    video2
066800030611
066900011228     C                   ENDIF
067000030605     C                   enddo
067100040728      *
067200040728     c                   if        modalita = '1' or modalita = '3'
067300040728     c                             or modalita = '4'
067400040728     c                   eval      wrkcars01 = *on
067500040728     c                   endif
067600000627      *
067700031003     C     noriga        ENDSR
067800011227     c**********************************************************************
067900000607     C     *INZSR        BEGSR
068000011227     c**********************************************************************
068100000607      *
068200000607     C     *ENTRY        PLIST
068300000607     C                   PARM                    Kpjba
068400030604     C                   Z-ADD     1             CODUT
068500030604     C                   CALL      'X§PARUT'
068600030604     C                   PARM                    UTEDSE
068700030604     C                   MOVEL     REC80         CNCR80
068800030703     C                   MOVEL     Ragut         rsut
068900030703     C                   MOVEL     knsif         vknsif
069000030703     C                   MOVEL     knmus         vknmus
069100030702      *reperimento data
069200030702     C                   TIME                    W0120            14 0
069300030702     C                   MOVE      W0120         WDAT              8 0
069400030702     C*
069500030702     C                   Z-ADD     WDAT          G02DAT
069600030702     C                   MOVEL     *BLANK        G02ERR
069700030702     C                   CALL      'XSRDA8'
069800030702     C                   PARM                    WLBDA8
069900030702     C* UDATE A 8 IN AAAA/MM/GG
070000030702     C                   Z-ADD     G02INV        WUDA              8 0
070100030606      *profilo di filiale attivo indicatore di protezione campi non manutenzionabili
070200030617     C                   CLEAR                   Tibs36Ds
070300030617     C                   EVAL      I36ute = knmus
070400030617     C                   CALL      'TIBS36R'
070500030617     C                   PARM                    tibs36ds
070600100617      * controllo se attivare o no controllo autorizzazioni e gestione terminal
070700100617     c                   clear                   tibs02ds
070800100617     c                   clear                   dvpocont
070900100617     c                   eval      t02mod = 'C'
071000100617     c                   eval      t02sif = knsif
071100100617     c                   eval      t02cod = 'VPO'
071200100617     c                   eval      t02ke1 = 'CONT'
071300100617     c                   call      'TIBS02R'
071400100617     c                   parm                    kpjba
071500100617     c                   parm                    tibs02ds
071600100617    2c                   if        t02err = *blanks
071700100617     c                   eval      dvpocont  = t02uni
071800100617    2c                   endif
071900030617     c                   if        o36pou <> 046
072000060915     c                   clear                   fnlv55ds
072100060915     c                   move      'L'           d55tla
072200060915     c                   move      o36pou        d55lin
072300100617     c                   if        wuda >= §vpoter
072400100617     c                   move      'E'           d55tpt
072500100617     c                   else
072600060915     c                   move      ' '           d55tpt
072700100617     c                   endif
072800060915     c                   z-add     wuda          d55drf
072900060915     c                   call      'FNLV55R'
073000060915     c                   parm                    fnlv55ds
073100060915     c                   z-add     d55tfp        termp             3 0
073200060915     c                   z-add     d55tfa        terma             3 0
073300060915      *se profilo di filiale e i terminal P/A sono uguali proteggo il campo
073400160908     c*m                 if        terma = termp
073500160908     c*m                 seton                                        20
073600160908     c*m                 endif
073700030617     c                   endif
073800030718
073900030718     C     Kapd          KLIST
074000030718     C                   KFLD                    tipoa             1
074100030721     C                   KFLD                    vflipdrk          7 0
074200030718     C                   move      'D'           tipoa
074300000607      *
074400030701     C     Kfli2         KLIST
074500030701     C                   KFLD                    vfliter
074600030701     C                   KFLD                    vflivad
074700030701     C                   KFLD                    vflifgp
074800030701     C                   KFLD                    vflifga
074900030703     C                   KFLD                    vfliprv
075000030701     C     Kfli          KLIST
075100030701     C                   KFLD                    vfliter
075200000607      *---------------------------------------------------------------------
075300011227     C                   ENDSR
