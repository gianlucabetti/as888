000100030604     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200130813      *
000300030716     Fficn79d   CF   E             WORKSTN
000400030604     f                                     sfile(video2:snrr1)
000500030709     Ffiatt01l  uf a E           k disk
000600130524     Ffiatt11l  if   E           k disk
000700030709     Fazorg01l  if   E           k disk
000800030714     Ffiapd01l  if   E           k disk
000900011228      *
001000020103     D Psds           SDS
001100020103     D  PgmName          *PROC
001200011228     D Kpjba         E DS
001300011228      *
001400030702     D fnlv55ds      E DS
001500121031     D savpjbu         S                   like(KPJBU)
001600121031     D ficng5DS      E DS
001700080923     D ficn74ds      E DS
001800030619     D trul33ds      E DS
001900030604     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
002000030604     D CNCR80        E DS
002100131007     D uteautds      E DS
002200131007     d tibs42ds      e ds
002300100617     d dvpocont      e ds
002400100617     d tibs02ds      e ds
002500030617     d Tibs36Ds      e ds
002600011227     d data            ds            10
002700011228     d aa                      3      4
002800011228     d mm                      6      7
002900011228     d gg                      9     10
003000011228      *
003100030714     D errter          S              1
003200030702     D fatto           S              1
003300030610     D modalita        S              1
003400030711     D prime_time      S              1    inz('X')
003500030604     D x               S              4  0
003600011227     D WrkEofS01       S              1
003700011227     D WrkCarS01       S              1
003800011228     D $VIDEO          S             10
003900030604     D snrr1           S              5i 0
004000030604     D SavOpzione      S                   like(v1csce)
004100030709     D Savattter       S                   like(vattter)
004200030709     D Savattterd      S                   like(vattterd)
004300030606     d dataiso         s               d   datfmt(*iso)
004400030606     d dataeur         s               d   datfmt(*eur)
004500030613     D WLBDA8          DS
004600030613     D  G02DAT                 1      8  0
004700030613     D  G02INV                 9     16  0
004800030613     D  G02ERR                17     17
004900030613     D  G02TGI                18     22  0
005000030701      *---------------- ----------------------------------------------------
005100030604      *---------------- Gestione formato video video01----------------------
005200030604     C                   EXSR      Inzvideo1
005300030604     C                   DOU       $Video <> 'VIDEO1'
005400000608      *----------- Visualizzo dati di output in caso di errori.-------------
005500000608     C                   IF        *IN99
005600000608     C                   EVAL      *IN99 = *OFF
005700030604     C                   WRITE     video1
005800000608     C                   EVAL      *IN99 = *ON
005900000608     C                   ENDIF
006000000608      *
006100030702     c     emetti        tag
006200030604     C                   EXFMT     video1
006300000608      *
006400011227     C                   SELECT
006500030612     C                   WHEN      *INKC
006600011227     C                             OR
006700011227     C                             *INKL
006800011227     C                   EVAL      $Video = *BLANKS
006900011227
007000011227     C                   WHEN      *INKE
007100030604     C                   EXSR      Inzvideo1
007200011227     C                   WHEN      *INKB
007300030604     C                   EXSR      Chkvideo1
007400011227     C                   OTHER
007500030604     C                   EXSR      Chkvideo1
007600130813     c   45
007700130813     cor 44              goto      emetti
007800121031     C*
007900121031     C*  esegue controlli sulle FIRME
008000121031     C                   EXSR      Ctrl_FIRME
008100121031     C*  se deve essere bloccato può solo fare F3 e uscire da questo pgm
008200121031     c                   if          devo_bloccare = 'S'
008300121031     C                   GOTO      EMETTi
008400121031     c                   end
008500121031      *
008600030604     C  N99              EXSR      Wrkvideo2c
008700031020     C     OK            CABEQ     0             EMETTI
008800011227     C                   ENDSL
008900011227      *
009000011227     C                   ENDDO
009100011227      *
009200011227     C                   EXSR      Uscita
009300011227     c**********************************************************************
009400011227     c* uscita
009500011227     c**********************************************************************
009600011227     C     Uscita        BEGSR
009700011227      *
009800011227     C                   EVAL      *INLR = *ON
009900011227     C                   RETURN
010000011227      *
010100011227     C                   ENDSR
010200011227     c**********************************************************************
010300011227     c* gestione sfl
010400011227     c**********************************************************************
010500030604     C     Wrkvideo2c    BEGSR
010600000627      *
010700011227     C                   EVAL      WrkCarS01 = *ON
010800030604     C                   EVAL      $Video = 'VIDEO2C'
010900011227      *
011000030604     C                   DOU       $Video <> 'VIDEO2C'
011100030604     C                   EXSR      Carvideo2
011200011228     c*Descrizione della ricerca
011300000627      *
011400031020     C     OK            CABEQ     0             TORNA
011500030604     C                   WRITE     video2z
011600030604     C                   EXFMT     video2c
011700011227     C                   SELECT
011800011228     c* fine
011900011227     C                   WHEN      *INKC=*ON
012000011227     C                   EVAL      $VIDEO=*BLANKS
012100011228     c* guida
012200011227     C                   WHEN      *INKL=*ON
012300030604     C                   EVAL      $VIDEO='VIDEO1'
012400030715
012500030715     C                   WHEN      *INKa=*ON
012600030715     C                   exsr      spalmaopz
012700011227
012800011227     C                   OTHER
012900011227
013000011227     C                   EXSR      GestioneSFL
013100011227     C                   ENDSL
013200011228
013300011227     C                   ENDDO
013400031020     C     torna         ENDSR
013500011227     c**********************************************************************
013600011227     c* Controlli video R01.
013700011227     c**********************************************************************
013800030604     C     Chkvideo1     BEGSR
013900000609      *
014000000608     C                   EVAL      *IN99 = *OFF
014100030702     C                   EVAL      *IN44 = *OFF
014200130813     C                   EVAL      *IN45 = *OFF
014300060915      * profilo di filiale controllo che il terminal sia congruente
014400160909     c*m                 if        o36pou <> 046
014500160909     c*m                 if        vattter <> terma and vattter <> termp
014600160909     c*m                 seton                                        44
014700160909     c*m                 endif
014800160909     c*m                 else
014900030702     c
015000030702     c                   clear                   fnlv55ds
015100030702     c                   move      'L'           d55tla
015200030709     c                   move      vattter       d55lin
015300100617     c                   if        wuda >= §vpoter
015400100617     c                   move      'E'           d55tpt
015500100617     c                   else
015600100617     c                   move      ' '           d55tpt
015700100617     c                   endif
015800030702     c                   z-add     wuda          d55drf
015900030702     c                   call      'FNLV55R'
016000030702     c                   parm                    fnlv55ds
016100030709     c                   if        vattter <> d55tfp and vattter <> d55tfa
016200030709     c                             or vattter = *zeros
016300030702     c                   seton                                        44
016400030702     c                   endif
016500160909     c*m                 endif
016600030709     c     vattter       chain     azorg01l
016700030709     c                   movel     orgdes        vattterd
016800030709     c                   if        vdata1 > 0
016900030715     c                   move      vdata1        g02dat
017000030715     c                   move      *blank        g02err
017100030715     C                   CALL      'XSRDA8'
017200030715     C                   PARM                    WLBDA8
017300030715     C     G02ERR        IFEQ      '1'
017400030715     C                   SETON                                        43
017500030715     C                   END
017600030715     c                   move      g02dat        vdata1
017700030715     c                   move      g02inv        wuda
017800030715     c                   move      g02inv        vdata1g
017900030709     c                   endif
018000130813      * controllo autorizzazione del profilo ad accedere alle tariffe AFF/DEF
018100130813     c                   if        wuda    >= §vpoaut
018200131007     c                   clear                   tibs42ds
018300131007     c                   movel     vattter       i42pge
018400131007     c                   call      'TIBS42R'
018500131007     c                   parm                    tibs42ds
018600131007     c                   movel     o42uni        uteautds
018700130813      *specifiche asteriscate le autorizzazioni per ora non sono attive
018800130813      * in quanto la conferma è presente solo nel menu di rf1
018900130813     c                   if        §affcon <> 'S'
019000130813     c                   seton                                        45
019100130813     c                   endif
019200130813     c                   endif
019300000608      *
019400000608     C                   ENDSR
019500011227     c**********************************************************************
019600011227     c* inizializza r01
019700011227     c**********************************************************************
019800030604     C     Inzvideo1     BEGSR
019900000608
020000030702     c                   if        o36pou <> 046
020100060915     c                   move      termp         vattter
020200030702     c                   endif
020300030709     c                   move      wdat          vdata1
020400030709     c                   move      wuda          vdata1g
020500030604     C                   EXSR      Chkvideo1
020600000609
020700000608     C                   ENDSR
020800030604     c**********************************************************************
020900030605     C     opzione       BEGSR
021000030605     c**********************************************************************
021100030610     c                   move      v1csce        modalita
021200030605     c                   select
021300030710     c                   when      v1csce = '1'
021400030717     c                   exsr      conferma
021500030717     c                   clear                   $msg
021600100122     c                   setoff                                       2821
021700030605     c                   when      v1csce = '2'
021800030605     c                   exsr      modifica
021900030717     c   21              clear                   $msg
022000030717     c                   setoff                                       2128
022100030605     c                   when      v1csce = '5'
022200030606     c                   seton                                        21
022300030606     c                   exsr      modifica
022400030606     c                   setoff                                       21
022500030605     c                   endsl
022600030604     C                   ENDSR
022700030717     c**********************************************************************
022800030717     C     conferma      BEGSR
022900030717     c**********************************************************************
023000030717     c                   setoff                                       28
023100030717     c     katt1         chain     fiatt01l                           98
023200030717     c                   if        not *in98
023300030717     c                   if        attdco > 0
023400100122     c                   seton                                        2821
023500030717     c                   eval      $msg = 'Viaggio già confermato non è +
023600030717     c                             possibile modificare i dati'
023700030717     c                   endif
023800100122     c                   if        attdco = 0 and attorar = 0 or
023900100122     c                             attdco = 0 and attimpp = 0
024000030717     c                   seton                                        28
024100030717     c                   eval      $msg = 'Viaggio non confermabile mancano i +
024200030717     c                             dati di arrivo o l''importo '
024300030717     c                   endif
024400030717     c                   move      attVAD        vattVAD
024500030717     c                   move      attTER        vattTER
024600030717     c                   move      attFGP        vattFGP
024700030717     c                   move      attFGA        vattFGA
024800030717     c                   move      attDDc        vattDDc
024900030717     c                   move      attORPr       vattORPr
025000030717     c                   move      attORAr       vattORAr
025100030717     c                   move      attPRV        vattPRV
025200030728     c                   move      attPRg        vattPRg
025300030728     c                   move      attnot        vattnot
025400130524     c                   movel     attpmb        vattpmb
025500130524     c     katt1         chain     fiatt11l
025600130524     c                   if        %found(fiatt11l)
025700130524     c                   move      at1pmb        vattpmb
025800130524     c                   end
025900030717     c                   z-add     attIMP        vattIMP
026000030717     c                   z-add     attIMPp       vattIMPp
026100030717     c                   move      attPDR        vattPDR
026200030717     c     kapd          chain     fiapd01l                           95
026300090507     c  n95              movel     apdrsf        vattpdrd
026400030717     c   95              clear                   vattpdrd
026500030717      * decodifica p.o.
026600030717     c     attfgp        chain     azorg01l                           95
026700030717     c  n95              movel     orgdes        vattfgpd
026800030717     c   95              clear                   vattfgpd
026900030717     c     attfga        chain     azorg01l                           95
027000030717     c  n95              movel     orgdes        vattfgad
027100030717     c   95              clear                   vattfgad
027200030717      *data decorrenza
027300030717     c                   if        attddc > 0
027400030717     c                   move      attddc        dataiso
027500030717     c                   move      dataiso       dataeur
027600030717     c                   move      dataeur       vattddc
027700030717     c                   else
027800030717     c                   clear                   vattddc
027900030717     c                   endif
028000030717     c                   exsr      Gesvideo3
028100030717     c                   endif
028200030717     C                   ENDSR
028300030605     c**********************************************************************
028400030605     C     modifica      BEGSR
028500030605     c**********************************************************************
028600030710     c     katt1         chain     fiatt01l                           98
028700030610     c                   if        not *in98
028800030717     c                   if        attdco > 0 and modalita ='2'
028900030717     c                   seton                                        2128
029000030717     c                   eval      $msg = 'Viaggio già confermato non è +
029100030717     c                             possibile modificare i dati'
029200030717     c                   endif
029300030709     c                   move      attVAD        vattVAD
029400030709     c                   move      attTER        vattTER
029500030709     c                   move      attFGP        vattFGP
029600030709     c                   move      attFGA        vattFGA
029700030709     c                   move      attDDc        vattDDc
029800030716     c                   move      attORPr       vattORPr
029900030716     c                   move      attORAr       vattORAr
030000030709     c                   move      attPRV        vattPRV
030100030728     c                   move      attPRg        vattPRg
030200030709     c                   z-add     attIMP        vattIMP
030300030709     c                   z-add     attIMPp       vattIMPp
030400030709     c                   move      attPDR        vattPDR
030500030714     c     kapd          chain     fiapd01l                           95
030600090507     c  n95              movel     apdrsf        vattpdrd
030700030714     c   95              clear                   vattpdrd
030800030709     c                   move      attNOT        vattNOT
030900130524     c                   movel     attpmb        vattpmb
031000130524     c     katt1         chain     fiatt11l
031100130524     c                   if        %found(fiatt11l)
031200130524     c                   move      at1pmb        vattpmb
031300130524     c                   end
031400030701      * decodifica p.o.
031500030709     c     attfgp        chain     azorg01l                           95
031600030709     c  n95              movel     orgdes        vattfgpd
031700030709     c   95              clear                   vattfgpd
031800030709     c     attfga        chain     azorg01l                           95
031900030709     c  n95              movel     orgdes        vattfgad
032000030709     c   95              clear                   vattfgad
032100030701      *data decorrenza
032200030709     c                   if        attddc > 0
032300030709     c                   move      attddc        dataiso
032400030701     c                   move      dataiso       dataeur
032500030709     c                   move      dataeur       vattddc
032600030701     c                   else
032700030709     c                   clear                   vattddc
032800030701     c                   endif
032900030605     c                   exsr      Gesvideo3
033000030605     c                   endif
033100030605     C                   ENDSR
033200030605     c**********************************************************************
033300030605     C     gesvideo3     BEGSR
033400030605     c**********************************************************************
033500030702     c                   clear                   fatto
033600030605     c                   do        *hival
033700030605     c                   exfmt     video3
033800030605     c   kl              leave
033900030624     c                   exsr      controv3
034000030717     c   28
034100030717     cor 96              iter
034200030605     c   kf              exsr      aggiorna
034300030702     c                   if        fatto <> *blank
034400030702     c                   leave
034500030702     c                   endif
034600030605
034700030605     c                   enddo
034800030605     C                   ENDSR
034900030605     c**********************************************************************
035000030624     C     Controv3      BEGSR
035100030605     c**********************************************************************
035200030613      *
035300030702      * *in96 errore generico videata dettaglio
035400030703     c                   setoff                                       96
035500030703      * decodifica p.o.
035600030709     c     vattfgp       chain     azorg01l                           95
035700030709     c  n95              movel     orgdes        vattfgpd
035800030709     c   95              clear                   vattfgpd
035900030709     c     vattfga       chain     azorg01l                           95
036000030709     c  n95              movel     orgdes        vattfgad
036100030709     c   95              clear                   vattfgad
036200030702      *Importo obbligatorio
036300030702     c                   setoff                                       49
036400090630     c                   if        vattimpp =  *zeros and modalita <> '1'
036500030702     c                   seton                                        4996
036600030702     c                   endif
036700030716     c                   endsr
036800030605     c**********************************************************************
036900030605     C     aggiorna      BEGSR
037000030605     c**********************************************************************
037100030717     c                   if        modalita = '1'
037200080923      * richiamo routine per verifica se esiste sovrapprezzo carburante
037300080923     c                   exsr      caricofuel
037400080923     c                   clear                   attpcar
037500080923     c                   clear                   attmcp
037600080923     c                   if        o74err = *blank
037700080923     c                   z-add     o74impa       attpcar
037800080923     c                   z-add     o74mcp        attmcp
037900080923     c                   endif
038000080923      *
038100030828     c                   move      'C'           attflg
038200030717     c                   z-add     wudate        attdco
038300090630     c                   if        vattimpp = 0,001
038400090630     c                   z-add     0             attimpp
038500090630     c                   z-add     0             vattimpp2
038600090630     c                   endif
038700090630     c                   if        vattimp = 0,001
038800090630     c                   z-add     0             attimp
038900090630     c                   endif
039000090630     c                   else
039100030717     c                   z-add     vattIMPp      attIMPp
039200030717     c                   z-add     vattIMPp      vattIMPp2
039300090630     c                   endif
039400090630     c                   movel     vattnot       attnot
039500030709     c                   update    fiatt000
039600030702     c                   move      'X'           fatto
039700030605     C                   ENDSR
039800080923     c**********************************************************************
039900080923     C     caricofuel    BEGSR
040000080923     c**********************************************************************
040100080923     c                   clear                   ficn74ds
040200080923     c                   z-add     vattimpp      i74imp
040300080923     c                   z-add     vattpdr       i74pdr
040400080923     c                   z-add     attddc        i74dtpre
040500080923     c                   move      'D'           i74tip
040600080923     c                   call      'FICN74R'
040700080923     c                   parm                    ficn74ds
040800080923     c
040900080923      *
041000080923     c                   endsr
041100030715     c**********************************************************************
041200030715     C     spalmaopz     BEGSR
041300030715     c**********************************************************************
041400030715     c                   do        *hival        recor             4 0
041500030715     c     recor         chain     video2                             93
041600030715     c   93              leave
041700030715     c                   move      '1'           v1csce
041800030715     c                   update    video2
041900030715     c                   enddo
042000030715     C                   ENDSR
042100011228     c**********************************************************************
042200011228     c* carica sfl
042300011228     c**********************************************************************
042400030604     C     Carvideo2     BEGSR
042500011228     C                   IF        WrkCarS01 = *ON
042600030604     C                   EVAL      snrr1   = 0
042700011228     C                   EVAL      *IN90 = *ON
042800020219     C                   EVAL      *IN91 = *OFF
042900030604     C                   WRITE     video2c
043000011228     C                   EVAL      *IN90 = *OFF
043100030711     c     kattd         setll     fiatt01l
043200030701     c                   do        *hival
043300030711     c     kattd         reade     fiatt01l                               97
043400030701     c   97              leave
043500030701     c                   move      *blanks       v1csce
043600030709     c                   move      attVAD        vattVAD
043700030709     c                   move      attTER        vattTER
043800031110     c                   z-add     attprg        vattprg
043900030709     c     attter        chain     azorg01l                           95
044000030709     c  n95              movel     orgdes        vattterd
044100030709     c   95              clear                   vattterd
044200030709     c                   move      attFGP        vattFGP
044300030709     c     attfgp        chain     azorg01l                           95
044400030709     c  n95              movel     orgdes        vattfgpd2
044500030709     c   95              clear                   vattfgpd2
044600030709     c                   move      attFGA        vattFGA
044700030709     c     attfga        chain     azorg01l                           95
044800030709     c  n95              movel     orgdes        vattfgad2
044900030709     c   95              clear                   vattfgad2
045000030717     c                   move      attORAr       vattORAr
045100030709     c                   move      attPRV        vattPRV
045200030710     c                   move      attPRg        vattPRg
045300030709     c                   move      attPDR        vattPDR
045400030716     c     kapd          chain     fiapd01l                           95
045500090507     c  n95              movel     apdrsf        vattpdrd2
045600030716     c   95              clear                   vattpdrd2
045700030709     c                   z-add     attimpp       vattimpp2
045800030709      *data prestazione
045900030709     c                   if        attddc > 0
046000030709     c                   move      attddc        dataiso
046100030701     c                   move      dataiso       dataeur
046200030709     c                   move      dataeur       vattddc
046300030701     c                   else
046400030709     c                   clear                   vattddc
046500030701     c                   endif
046600031110      *data conferma
046700031110     c                   if        attdco > 0
046800031110     c                   move      attdco        dataiso
046900031110     c                   move      dataiso       dataeur
047000031110     c                   move      dataeur       vattdco
047100031110     c                   else
047200031110     c                   clear                   vattdco
047300031110     c                   endif
047400030701      *esco per raggiunta massima capacità sfl
047500030617     c                   if        snrr1 > 9990
047600030617     c                   leave
047700030617     c                   endif
047800030617
047900030604     C                   EVAL      snrr1 = snrr1 + 1
048000030604     C                   WRITE     video2
048100011228     C                   ENDDO
048200011228
048300030604     C                   IF        snrr1  > 0
048400020219     C                   EVAL      *IN91 = *ON
048500020219     C                   MOVE      1             OK                1 0
048600020219     C                   ELSE
048700020219     C                   MOVE      0             OK                1 0
048800030701     C                   ENDIF
048900030612     C                   EVAL      nrr1  = 1
049000011228     C                   ENDIF
049100020219     C                   EVAL      WrkCarS01 = *OFF
049200030604     C                   EVAL      snrr1  = 1
049300030604     C                   EVAL      csrrrn = 1
049400011228     C                   ENDSR
049500011227     c**********************************************************************
049600011227     c* gestione sfl
049700011227     c**********************************************************************
049800000613     C     GestioneSFL   BEGSR
049900000627      *
050000000627     C                   EVAL      WrkEofS01 = *OFF
050100030605      * Elaborazione opzioni.
050200011228     c*
050300030605     c                   do        *hival        X                 4 0
050400030605     c     X             CHAIN     video2                             50
050500030605     C   50              LEAVE
050600030604     C                   IF        v1csce <> *blanks
050700030605     c                   exsr      opzione
050800030611
050900030611     c                   move      *blank        v1csce
051000030605     c                   update    video2
051100030611
051200011228     C                   ENDIF
051300030605     C                   enddo
051400030710     c                   if        modalita = '1'
051500030710     c                   eval      wrkcars01 = *on
051600030710     c                   endif
051700000627      *
051800030703     C                   ENDSR
051900011227     c**********************************************************************
052000000607     C     *INZSR        BEGSR
052100011227     c**********************************************************************
052200000607      *
052300000607     C     *ENTRY        PLIST
052400000607     C                   PARM                    Kpjba
052500030604     C                   Z-ADD     1             CODUT
052600030604     C                   CALL      'X§PARUT'
052700030604     C                   PARM                    UTEDSE
052800030604     C                   MOVEL     REC80         CNCR80
052900030703     C                   MOVEL     Ragut         rsut
053000030703     C                   MOVEL     knsif         vknsif
053100030703     C                   MOVEL     knmus         vknmus
053200030702      *reperimento data
053300030702     C                   TIME                    W0120            14 0
053400030702     C                   MOVE      W0120         WDAT              8 0
053500030702     C*
053600030702     C                   Z-ADD     WDAT          G02DAT
053700030702     C                   MOVEL     *BLANK        G02ERR
053800030702     C                   CALL      'XSRDA8'
053900030702     C                   PARM                    WLBDA8
054000030702     C* UDATE A 8 IN AAAA/MM/GG
054100030702     C                   Z-ADD     G02INV        WUDA              8 0
054200030717     C                   Z-ADD     G02INV        WUDAte            8 0
054300030606      *profilo di filiale attivo indicatore di protezione campi non manutenzionabili
054400030617     C                   CLEAR                   Tibs36Ds
054500030617     C                   EVAL      I36ute = knmus
054600030617     C                   CALL      'TIBS36R'
054700030617     C                   PARM                    tibs36ds
054800100617      * controllo se attivare o no controllo autorizzazioni e gestione terminal
054900100617     c                   clear                   tibs02ds
055000100617     c                   clear                   dvpocont
055100100617     c                   eval      t02mod = 'C'
055200100617     c                   eval      t02sif = knsif
055300100617     c                   eval      t02cod = 'VPO'
055400100617     c                   eval      t02ke1 = 'CONT'
055500100617     c                   call      'TIBS02R'
055600100617     c                   parm                    kpjba
055700100617     c                   parm                    tibs02ds
055800100617    2c                   if        t02err = *blanks
055900100617     c                   eval      dvpocont  = t02uni
056000100617    2c                   endif
056100160909     c*m                 if        o36pou <> 046
056200060915     c                   clear                   fnlv55ds
056300060915     c                   move      'L'           d55tla
056400060915     c                   move      o36pou        d55lin
056500100617     c                   if        wuda >= §vpoter
056600100617     c                   move      'E'           d55tpt
056700100617     c                   else
056800100617     c                   move      ' '           d55tpt
056900100617     c                   endif
057000060915     c                   z-add     wuda          d55drf
057100060915     c                   call      'FNLV55R'
057200060915     c                   parm                    fnlv55ds
057300060915     c                   z-add     d55tfp        termp             3 0
057400060915     c                   z-add     d55tfa        terma             3 0
057500060915      *se profilo di filiale e i terminal P/A sono uguali proteggo il campo
057600160909     c*m                 if        terma = termp
057700160909     c*m                 seton                                        20
057800160909     c*m                 endif
057900160909     c*m                 endif
058000000607      *
058100030710     C     Katt1         KLIST
058200030709     C                   KFLD                    vattter
058300030709     C                   KFLD                    vdata1g           8 0
058400030709     C                   KFLD                    vattvad
058500030709     C                   KFLD                    vattfgp
058600030709     C                   KFLD                    vattfga
058700030710     C                   KFLD                    vattprg
058800030709     C     Katt          KLIST
058900030709     C                   KFLD                    vattter
059000030711     C     Kattd         KLIST
059100030711     C                   KFLD                    vattter
059200030711     C                   KFLD                    vdata1g
059300030714     C     Kapd          KLIST
059400030714     C                   KFLD                    tipoa             1
059500030714     C                   KFLD                    vattpdr
059600030714     C                   move      'D'           tipoa
059700030714
059800011227     C                   ENDSR
059900121031     C* --------------------------------------------------------------
060000121031     C*--- CONTROLLO sui DOCUMENTI Firmati della filiale -------------*
060100121031     C* --------------------------------------------------------------
060200121031     C     CTRL_FIRME    BEGSR
060300121031     C*
060400121031     C*  esegue un controllo sulle Valorizzazioni Firmate (NON solo x Autisti)
060500121031     C*   ma anche x COOP e AFFLUENZE/DEFLUENZE
060600121031     C*  Le firme devono essere a posto su tutta la filiale trattata altrimenti
060700121031     C*   viene emessa una Window di avvertimento e addirittura di BLOCCO.
060800121031     c                   move      'N'           devo_bloccare     1
060900121031     C                   MOVEL     kpjbu         savpjbu
061000121031     C*
061100121031      *  Inp-> §G5CNVL     1 A  C=CONTABILIZZAZIONE/V=VALORIZZAZIONI
061200121031      *  Inp-> §G5TIPO     1 A  BLANK=TUTTI/C=COOP/A=AUTCITTA/F=AFFDEF
061300121031      *  Inp-> §G5FIL      3 S0 FILIALE SOTTO CONTROLLO
061400121031      *  Out <-§G5FUN      2 A  TASTO FUNZIONALE PREMUTO
061500121031      *  Out <-§G5ESITO    1 A  F=FORZABILE/B=BLOCCANTE
061600121031     C*
061700121031     C                   clear                   kpjbu
061800121031     C                   clear                   ficng5ds
061900121031     c                   eval      §G5CNVL  = 'V'
062000121031     c                   eval      §G5FIL   = VattTER
062100121031     C                   MOVEL     ficng5ds      kpjbu
062200121031     c                   call      'FICNG5R'
062300121031     C                   parm                    kpjba
062400121031     C                   MOVEL     kpjbu         ficng5ds
062500121031     C                   clear                   kpjbu
062600121031     C                   MOVEL     savpjbu       kpjbu
062700121031     C*
062800121031     C*   se deve bloccare le valorizzazioni xchè la filiale è fuori tempo max
062900121031     C*    e devono firmare prima tutti i documenti in sospeso
063000121031     C*  è proprio il FICNG5R che ci dice questo e finchè non si sono messi a
063100121031     C*  posto con le firme non fa procedere con le valorizzazioni degli AUT
063200121031     c                   if        §G5ESITO ='B'
063300121031     c                   eval        devo_bloccare = 'S'
063400121031     c                   end
063500121031     C*
063600121031     C                   ENDSR
063700030714      *---------------------------------------------------------------------
