000100030604     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200000607      *
000300080930     Fficna4d   CF   E             WORKSTN
000400080930     f                                     sfile(video2:snrr1)
000500080930     Ffiadd01l  uF a E           k disk
000600030701     Fazorg01l  iF   E           k disk
000700081001     Ffiadt01l  iF   E           k disk
000800030718     Ffiapd01l  iF   E           k disk
000900090619     Fazcae05l  iF   E           k disk
001000011228      *
001100020103     D Psds           SDS
001200020103     D  PgmName          *PROC
001300011228     D Kpjba         E DS
001400011228      *
001500081001     D fnlv55ds      E DS
001600090108     D ficna2ds      E DS
001700030604     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
001800030604     D CNCR80        E DS
001900030617     d Tibs36Ds      e ds
002000011227     d data            ds            10
002100011228     d aa                      3      4
002200011228     d mm                      6      7
002300011228     d gg                      9     10
002400011228      *
002500030702     D fatto           S              1
002600030610     D modalita        S              1
002700030604     D x               S              4  0
002800011227     D WrkEofS01       S              1
002900011227     D WrkCarS01       S              1
003000011228     D $VIDEO          S             10
003100030604     D snrr1           S              5i 0
003200030721     D Savkpjbu        S                   like(kpjbu)
003300080930     D SavOpzione      S                   like(v1csce)
003400081001     d dataiso         s               d   datfmt(*iso)
003500081001     d dataeur         s               d   datfmt(*eur)
003600081001     D WLBDA8          DS
003700081001     D  G02DAT                 1      8  0
003800081001     D  G02INV                 9     16  0
003900081001     D  G02ERR                17     17
004000081001     D  G02TGI                18     22  0
004100080930     D param           ds
004200080930     D  parpdr                        7s 0
004300080930     D  parprg                        3s 0
004400080930     D  parmod                        1
004500030701      *---------------- ----------------------------------------------------
004600030604      *---------------- Gestione formato video video01----------------------
004700080930     C                   EXSR      Wrkvideo2c
004800011227      *
004900011227      *
005000011227     C                   EXSR      Uscita
005100011227     c**********************************************************************
005200011227     c* uscita
005300011227     c**********************************************************************
005400011227     C     Uscita        BEGSR
005500011227      *
005600011227     C                   EVAL      *INLR = *ON
005700011227     C                   RETURN
005800011227      *
005900011227     C                   ENDSR
006000011227     c**********************************************************************
006100011227     c* gestione sfl
006200011227     c**********************************************************************
006300030604     C     Wrkvideo2c    BEGSR
006400081013      *modalità interrogazione da programma richiamato
006500081013      *
006600011227     C                   EVAL      WrkCarS01 = *ON
006700030604     C                   EVAL      $Video = 'VIDEO2C'
006800011227      *
006900030604     C                   DOU       $Video <> 'VIDEO2C'
007000081013     c                   if        parmod = 'I'
007100081013     c                   seton                                        2125
007200081013     c                   endif
007300030604     C                   EXSR      Carvideo2
007400011228     c*Descrizione della ricerca
007500000627      *
007600030604     C                   WRITE     video2z
007700030604     C                   EXFMT     video2c
007800011227     C                   SELECT
007900011228     c* fine
008000081001     C                   WHEN      *INKL=*ON
008100011227     C                   EVAL      $VIDEO=*BLANKS
008200011228     c* guida
008300011228     c* ripristino
008400011227     C                   WHEN      *INKE=*ON
008500011227     C                   EVAL      WrkCarS01 = *ON
008600011227
008700011227     C                   OTHER
008800011227
008900011227     C                   EXSR      GestioneSFL
009000011227     C                   ENDSL
009100011228
009200011227     C                   ENDDO
009300031027     C                   ENDSR
009400030604     c**********************************************************************
009500030605     C     opzione       BEGSR
009600030605     c**********************************************************************
009700030610     c                   move      v1csce        modalita
009800040728     c                   movea     '000000000000'*in(40)
009900040728     c                   movea     '00000'       *in(52)
010000040728     c                   setoff                                       96
010100030605     c                   select
010200081013     c                   when      v1csce = '2' and not *in21
010300081001      * tariffa gia stampata non è possibile modificare i dati
010400081001     c                   if        adtdts > 0
010500081001     c                   seton                                        21
010600081001     c                   eval      $msg = 'Tariffa già stampata non è -
010700081001     c                             possibile modificare i dati'
010800081001     c                   endif
010900030605     c                   exsr      modifica
011000081001     c   21              setoff                                       2128
011100081013     c                   when      v1csce = '3' and not *in21
011200030605     c                   exsr      copia
011300030605     c                   when      v1csce = '5'
011400030606     c                   seton                                        21
011500030606     c                   exsr      modifica
011600030606     c                   setoff                                       21
011700081013     c                   when      v1csce = '4' and not *in21
011800081001      * tariffa gia stampata non è possibile modificare i dati
011900081001     c                   if        adtdts > 0
012000081001     c                   seton                                        21
012100081001     c                   eval      $msg = 'Tariffa già stampata non è -
012200081001     c                             possibile modificare i dati'
012300081001     c                   endif
012500140115     c                   exsr      modifica
012600140115     c   21              setoff                                       2128
012700040728     c                   clear                   $msg
012800030605     c                   endsl
012900030604     C                   ENDSR
013000030605     c**********************************************************************
013100030605     C     modifica      BEGSR
013200030605     c**********************************************************************
013300090108     c     kadd1         chain     fiadd01l                           98
013400030610     c                   if        not *in98
013500080930     c                   move      ADdPDR        vADdPDR
013600080930     c                   move      ADdPRG        vADdPRG
013700081001     c                   move      ADDTSR        vADDTSR
013800080930     c                   move      ADdlnp        vADdlnp
013900080930     c                   move      ADdlna        vADdlna
014000080930     c                   move      ADdimp        vADdimp
014100080930     c                   move      ADdnote       vADdnote
014200080930     c                   move      ADdkm         vADdkm
014300030718      * decodifica autista
014400030718     c     kapd          chain     fiapd01l                           95
014500090507     c  n95              movel     apdrsf        vaddpdrd
014600080930     c   95              clear                   vaddpdrd
014700080930     c     vaddlnp       chain     azorg01l                           95
014800080930     c  n95              movel     orgdes        vaddlnpd
014900080930     c   95              clear                   vaddlnpd
015000081001     c     vaddlna       chain     azorg01l                           95
015100081001     c  n95              movel     orgdes        vaddlnad
015200080930     c   95              clear                   vaddlnad
015300030605     c                   exsr      Gesvideo3
015400030605     c                   endif
015500030605     C                   ENDSR
015600030605     c**********************************************************************
015700030605     C     copia         BEGSR
015800030605     c**********************************************************************
015900030605     C                   ENDSR
016000030605     c**********************************************************************
016100030605     C     gesvideo3     BEGSR
016200030605     c**********************************************************************
016300030702     c                   clear                   fatto
016400030605     c                   do        *hival
016500030605     c                   exfmt     video3
016600030605     c   kl              leave
016700040728     c  n21              exsr      controv3
016800030624     c   96              iter
016900081001      * permetto aggiornamento solo se non ho forzato visualizzazione
017000081001     c                   if        not *in21
017100040728     c   kf              exsr      aggiorna
017200081001     c                   endif
017300030702     c                   if        fatto <> *blank
017400030702     c                   leave
017500030702     c                   endif
017600030605
017700030605     c                   enddo
017800030605     C                   ENDSR
017900030605     c**********************************************************************
018000030624     C     Controv3      BEGSR
018100030605     c**********************************************************************
018200030613      *
018300030702      * *in96 errore generico videata dettaglio
018400040728     c                   setoff                                       9628
018500040728     c                   clear                   $msg
018600140115     c*                  if        modalita ='4'
018700140115     c*                  goto      endctr
018800140115     c*                  endif
018900081003      * decodifico solo in immissione altrimenti già impostati in modifica
019000081003     c                   if        modalita = *blank
019100081003     c     vaddlnp       chain     azorg01l                           95
019200081003     c  n95              movel     orgdes        vaddlnpd
019300081003     c   95              clear                   vaddlnpd
019400081003     c     vaddlna       chain     azorg01l                           95
019500081003     c  n95              movel     orgdes        vaddlnad
019600081003     c   95              clear                   vaddlnad
019700081003     c                   endif
019800081001      *filiale partenza obbligatoria
019900081001     c                   setoff                                       41
020000081001     c                   if        vaddlnp = *zeros
020100081001     c                   seton                                        4196
020200081001     c                   endif
020300081001      *controllo congruenza terminal e filiale x defluenza
020400081001     c                   if        vaddtsr = 'D'
020500081001     c                   setoff                                       46
020600081001     c                   clear                   fnlv55ds
020700081001     c                   move      'L'           d55tla
020800081006     c                   move      vaddlna       d55lin
020900081001     c                   move      'A'           d55tpt
021000081001     c                   z-add     adtddt        d55drf
021100081001     c                   call      'FNLV55R'
021200081001     c                   parm                    fnlv55ds
021300081001     c                   if        d55tfa <> vaddlnp or
021400081001     c                             d55tfa <> terma
021500090619     c                   move      'A'           k1epa
021600090619     c                   move      vaddlnp       k2tfe
021700090619     c                   move      vaddlna       k3tfa
021800090619     c     kaz3          chain     azcae05l
021900090619     c                   if        not %found(azcae05l)
022000081001     c                   seton                                        4696
022100090619     c                   endif
022200081001     c                   endif
022300081001     c                   endif
022400081001      *filiale arrivo  obbligatoria
022500081001     c                   setoff                                       42
022600081001     c                   if        vaddlna =  *zeros
022700081001     c                   seton                                        4296
022800081001     c                   endif
022900081001      *controllo congruenza terminal e filiale x Affluenza
023000081001     c                   if        vaddtsr = 'A'
023100081001     c                   setoff                                       46
023200081001     c                   clear                   fnlv55ds
023300081001     c                   move      'L'           d55tla
023400081006     c                   move      vaddlnp       d55lin
023500081001     c                   move      'P'           d55tpt
023600081001     c                   z-add     wuda          d55drf
023700081001     c                   call      'FNLV55R'
023800081001     c                   parm                    fnlv55ds
023900081001     c                   if        d55tfp <> vaddlna or
024000081001     c                             d55tfp <> termp
024100090619     c                   move      'P'           k1epa
024200090619     c                   move      vaddlna       k2tfe
024300090619     c                   move      *zeros        k3tfa
024400090619     c                   move      vaddlnp       k4tfp
024500090619     c     kaz4          chain     azcae05l
024600090619     c                   if        not %found(azcae05l)
024700081001     c                   seton                                        4696
024800090619     c                   endif
024900081001     c                   endif
025000081001     c                   endif
025100081006     c                   setoff                                       43
025200081006     c                   if        vaddimp = 0
025300081006     c                   seton                                        4396
025400081006     c                   endif
025500090108      * solo in immissione verifico se esiste già un rekord uguale
025600090108     c                   if        *in23
025700081007     c                   setoff                                       44
025800081007     c     kaddchk       chain     fiadd01l
025900081007     c                   if        %found(fiadd01l)
026000081007     c                   seton                                        4496
026100090108     c                   clear                   video3
026200081007     c                   endif
026300090108     c                   endif
026400090108      * controllo esistenza/lunghezza tratta in anagrafica
026500090108     c                   setoff                                       45
026600090417     c*mm                if        vaddkm = 0
026700090417     c*mm                clear                   ficna2ds
026800090417     c*mm                z-add     vaddlnp       ia2lnp
026900090417     c*mm                z-add     vaddlna       ia2lna
027000090417     c*mm                call      'FICNA2R1'
027100090417     C*mm                parm                    ficna2ds
027200090417     c*mm                if        oa2error = *blank
027300090417     c*mm                z-add     oa2km         vaddkm
027400090417     c*mm                else
027500090107     c                   z-add     0             vaddkm
027600090417     c*mm                seton                                        4596
027700090417     c*mm                endif
027800090417     c*mm                endif
027900081007     C     endctr        ENDSR
028000030605     c**********************************************************************
028100030605     C     aggiorna      BEGSR
028200030605     c**********************************************************************
028300081001     c                   move      vADDPDR       ADDPDR
028400081001     c                   move      vADDPRG       ADDPRG
028500081001     c                   move      vADDTSR       ADDTSR
028600081001     c                   move      vADDPRG       ADDPRG
028700081001     c                   move      vADDLNP       ADDLNP
028800081001     c                   move      vADDLNA       ADDLNA
028900081001     c                   move      vADDIMP       ADDIMP
029000081001     c                   move      vADDNOTE      ADDNOTE
029100081001     c                   move      vADDKM        ADDKM
029200030610      * inserimento e copia scrive altrimenti aggiorna il record esistente
029300040728     c                   select
029400040728     c                   when      modalita = '4'
029500081001     c                   delete    fiadd000
029600040728     c                   when      modalita = *blank or modalita = '3'
029700081001     c                   write     fiadd000
029800040728     c                   other
029900081001     c                   update    fiadd000
030000040728     c                   endsl
030100030702     c                   move      'X'           fatto
030200030605     C                   ENDSR
030300011228     c**********************************************************************
030400011228     c* carica sfl
030500011228     c**********************************************************************
030600030604     C     Carvideo2     BEGSR
030700081001     C                   z-add     parpdr        vi2pdr
030800081001     C                   z-add     parprg        vi2prg
030900081001     c                   move      adtddt        dataiso
031000081001     c                   move      dataiso       dataeur
031100081001     c                   move      dataeur       vi2ddt
031200081001     c                   move      adtdst        dataiso
031300081001     c                   move      dataiso       dataeur
031400081001     c                   move      dataeur       vi2dst
031500081001     c                   clear                   vi2dts
031600081001     c                   if        adtdts > 0
031700081001     c                   move      adtdts        dataiso
031800081001     c                   move      dataiso       dataeur
031900081001     c                   move      dataeur       vi2dts
032000081001     c                   endif
032100081001     c     kapd          chain     fiapd01l                           95
032200090507     c  n95              movel     apdrsf        despdr
032300081001     c   95              clear                   despdr
032400011228     C                   IF        WrkCarS01 = *ON
032500030604     C                   EVAL      snrr1   = 0
032600011228     C                   EVAL      *IN90 = *ON
032700020219     C                   EVAL      *IN91 = *OFF
032800030604     C                   WRITE     video2c
032900011228     C                   EVAL      *IN90 = *OFF
033000081001     c     kadd          setll     fiadd01l
033100030701     c                   do        *hival
033200081001     c     kadd          reade     fiadd01l                               97
033300030701     c   97              leave
033400081001     c                   if        addatb <> *blank
033500080930     c                   iter
033600080930     c                   endif
033700030701     c                   move      *blanks       v1csce
033800081001     c                   move      addtsr        vi2tsr
033900081001     c                   z-add     addlnp        vi2lnp
034000081001     c                   z-add     addlna        vi2lna
034100081001     c                   z-add     addimp        vi2imp
034200081001     c     vi2lnp        chain     azorg01l
034300081001     c                   if        %found(azorg01l)
034400081001     c                   movel     orgdes        vi2lnpd
034500081001     c                   endif
034600081001     c     vi2lna        chain     azorg01l
034700081001     c                   if        %found(azorg01l)
034800081001     c                   movel     orgdes        vi2lnad
034900081001     c                   endif
035000030701      *esco per raggiunta massima capacità sfl
035100030617     c                   if        snrr1 > 9990
035200030617     c                   leave
035300030617     c                   endif
035400030617
035500030604     C                   EVAL      snrr1 = snrr1 + 1
035600030604     C                   WRITE     video2
035700011228     C                   ENDDO
035800011228
035900030604     C                   IF        snrr1  > 0
036000020219     C                   EVAL      *IN91 = *ON
036100020219     C                   MOVE      1             OK                1 0
036200020219     C                   ELSE
036300020219     C                   MOVE      0             OK                1 0
036400030701     C                   ENDIF
036500030612     C                   EVAL      nrr1  = 1
036600011228     C                   ENDIF
036700020219     C                   EVAL      WrkCarS01 = *OFF
036800030604     C                   EVAL      snrr1  = 1
036900030604     C                   EVAL      csrrrn = 1
037000011228     C                   ENDSR
037100011227     c**********************************************************************
037200011227     c* gestione sfl
037300011227     c**********************************************************************
037400000613     C     GestioneSFL   BEGSR
037500000627      *
037600000627     C                   EVAL      WrkEofS01 = *OFF
037700000627      * F10 Immissione.
037800081013     C                   IF        *INKJ and not *in21
037900081001      * tariffa gia stampata non è possibile modificare i dati
038000081001     c                   if        adtdts > 0
038100081003     c                   seton                                        2821
038200090429     c                   eval      $msg = 'Tariffa già convalidata non è -
038300081001     c                             possibile modificare i dati'
038400090429     c                   goto      noriga
038500081001     c                   endif
038600080930     c                   clear                   video3
038700040728     c                   clear                   modalita
038800081001     c                   clear                   fiadd000
038900040728     c                   move      knmus         vknmus
039000040728     c                   move      knsif         vknsif
039100040728     c                   movel     ragut         rsut
039200090108     c                   seton                                        23
039300081001      * decodifica autista
039400081001     C                   z-add     parpdr        vaddpdr
039500081001     C                   z-add     parprg        vaddprg
039600081001     c     kapd          chain     fiapd01l                           95
039700090507     c  n95              movel     apdrsf        vaddpdrd
039800081001     c   95              clear                   vaddpdrd
039900040728     c                   exsr      Gesvideo3
040000090108     c                   setoff                                       23
040100040728     c                   eval      wrkcars01 = *on
040200040728     c                   endif
040300090429     c                   if        csrrrn = 0
040400090429     c                   goto      noriga
040500090429     c                   endif
040600030605      * Elaborazione opzioni.
040700011228     c*
040800030605     c                   do        *hival        X                 4 0
040900030605     c     X             CHAIN     video2                             50
041000030605     C   50              LEAVE
041100030604     C                   IF        v1csce <> *blanks
041200030605     c                   exsr      opzione
041300030611
041400030611     c                   move      *blank        v1csce
041500030605     c                   update    video2
041600030611
041700011228     C                   ENDIF
041800030605     C                   enddo
041900040728      *
042000040728     c                   if        modalita = '1' or modalita = '3'
042100040728     c                             or modalita = '4'
042200090108     c                             or modalita = '2'
042300040728     c                   eval      wrkcars01 = *on
042400040728     c                   endif
042500000627      *
042600031003     C     noriga        ENDSR
042700011227     c**********************************************************************
042800000607     C     *INZSR        BEGSR
042900011227     c**********************************************************************
043000000607      *
043100000607     C     *ENTRY        PLIST
043200000607     C                   PARM                    Kpjba
043300080930     c                   movel     kpjbu         param
043400080930
043500030604     C                   Z-ADD     1             CODUT
043600030604     C                   CALL      'X§PARUT'
043700030604     C                   PARM                    UTEDSE
043800030604     C                   MOVEL     REC80         CNCR80
043900030703     C                   MOVEL     Ragut         rsut
044000030703     C                   MOVEL     knsif         vknsif
044100030703     C                   MOVEL     knmus         vknmus
044200030702      *reperimento data
044300030702     C                   TIME                    W0120            14 0
044400030702     C                   MOVE      W0120         WDAT              8 0
044500030702     C*
044600030702     C                   Z-ADD     WDAT          G02DAT
044700030702     C                   MOVEL     *BLANK        G02ERR
044800030702     C                   CALL      'XSRDA8'
044900030702     C                   PARM                    WLBDA8
045000030702     C* UDATE A 8 IN AAAA/MM/GG
045100030702     C                   Z-ADD     G02INV        WUDA              8 0
045200080930     C                   CLEAR                   Tibs36Ds
045300080930     C                   EVAL      I36ute = knmus
045400080930     C                   CALL      'TIBS36R'
045500080930     C                   PARM                    tibs36ds
045600030718
045700030718     C     Kapd          KLIST
045800030718     C                   KFLD                    tipoa             1
045900080930     C                   KFLD                    parpdr            7 0
046000030718     C                   move      'D'           tipoa
046100000607      *
046200080930     C     Kadd          KLIST
046300080930     C                   KFLD                    parpdr
046400080930     C                   KFLD                    parprg
046500090108     C     Kadd1         KLIST
046600090108     C                   KFLD                    vi2PDR
046700090108     C                   KFLD                    vi2PRG
046800090108     C                   KFLD                    vi2TSR
046900090108     C                   KFLD                    vi2LNP
047000090108     C                   KFLD                    vi2LNA
047100081007     C     Kaddchk       KLIST
047200081007     C                   KFLD                    vADDPDR
047300081007     C                   KFLD                    vADDPRG
047400081007     C                   KFLD                    vADDTSR
047500081007     C                   KFLD                    vADDLNP
047600081007     C                   KFLD                    vADDLNA
047700090619     C     Kaz3          KLIST
047800090619     C                   KFLD                    k1epa
047900090619     C                   KFLD                    k2tfe
048000090619     C                   KFLD                    k3tfa
048100090619     C     Kaz4          KLIST
048200090619     C                   KFLD                    k1epa             1
048300090619     C                   KFLD                    k2tfe             3 0
048400090619     C                   KFLD                    k3tfa             3 0
048500090619     C                   KFLD                    k4tfp             3 0
048600081001      * aggancia testata
048700081001     c     kadd          chain     fiadt01l
048800081001      * verifico terminal partenza e arrivo per impostare viaggi
048900081001     c                   if        o36pou <> 046
049000081001     c                   clear                   fnlv55ds
049100081001     c                   move      'L'           d55tla
049200081001     c                   move      o36pou        d55lin
049300081001     c                   move      ' '           d55tpt
049400081001     c                   z-add     wuda          d55drf
049500081001     c                   call      'FNLV55R'
049600081001     c                   parm                    fnlv55ds
049700081001     c                   z-add     d55tfp        termp             3 0
049800081001     c                   z-add     d55tfa        terma             3 0
049900081001     c                   endif
050000000607      *---------------------------------------------------------------------
050100011227     C                   ENDSR
