000100030604     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200000607      *
000300090105     Fficna2d   CF   E             WORKSTN
000400080930     f                                     sfile(video2:snrr1)
000500090105     Ffiadk01l  uF a E           k disk
000600030701     Fazorg01l  iF   E           k disk
000700011228      *
000800020103     D Psds           SDS
000900020103     D  PgmName          *PROC
001000011228     D Kpjba         E DS
001100011228      *
001200030604     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
001300030604     D CNCR80        E DS
001400030617     d Tibs36Ds      e ds
001500011227     d data            ds            10
001600011228     d aa                      3      4
001700011228     d mm                      6      7
001800011228     d gg                      9     10
001900011228      *
002000030702     D fatto           S              1
002100030610     D modalita        S              1
002200030604     D x               S              4  0
002300011227     D WrkEofS01       S              1
002400011227     D WrkCarS01       S              1
002500011228     D $VIDEO          S             10
002600030604     D snrr1           S              5i 0
002700030721     D Savkpjbu        S                   like(kpjbu)
002800080930     D SavOpzione      S                   like(v1csce)
002900090105     D WLBDA8          DS
003000090105     D  G02DAT                 1      8  0
003100090105     D  G02INV                 9     16  0
003200090105     D  G02ERR                17     17
003300090105     D  G02TGI                18     22  0
003400030604      *---------------- Gestione formato video video01----------------------
003500030604     C                   EXSR      Inzvideo1
003600030604     C                   DOU       $Video <> 'VIDEO1'
003700000608      *----------- Visualizzo dati di output in caso di errori.-------------
003800000608     C                   IF        *IN99
003900000608     C                   EVAL      *IN99 = *OFF
004000030604     C                   WRITE     video1
004100000608     C                   EVAL      *IN99 = *ON
004200000608     C                   ENDIF
004300000608      *
004400030702     c     emetti        tag
004500030604     C                   EXFMT     video1
004600000608      *
004700011227     C                   SELECT
004800030612     C                   WHEN      *INKC
004900011227     C                             OR
005000011227     C                             *INKL
005100011227     C                   EVAL      $Video = *BLANKS
005200011227
005300011227     C                   WHEN      *INKE
005400030604     C                   EXSR      Inzvideo1
005500011227     C                   WHEN      *INKB
005600030604     C                   EXSR      Chkvideo1
005700011227     C                   OTHER
005800030604     C                   EXSR      Chkvideo1
005900040728     c   84              goto      emetti
006000030604     C  N99              EXSR      Wrkvideo2c
006100011227     C                   ENDSL
006200011227      *
006300011227     C                   ENDDO
006400011227      *
006500011227     C                   EXSR      Uscita
006600011227     c**********************************************************************
006700011227     c* uscita
006800011227     c**********************************************************************
006900011227     C     Uscita        BEGSR
007000011227      *
007100011227     C                   EVAL      *INLR = *ON
007200011227     C                   RETURN
007300011227      *
007400011227     C                   ENDSR
007500011227     c**********************************************************************
007600011227     c* gestione sfl
007700011227     c**********************************************************************
007800030604     C     Wrkvideo2c    BEGSR
007900000627      *
008000011227     C                   EVAL      WrkCarS01 = *ON
008100030604     C                   EVAL      $Video = 'VIDEO2C'
008200011227      *
008300030604     C                   DOU       $Video <> 'VIDEO2C'
008400030604     C                   EXSR      Carvideo2
008500011228     c*Descrizione della ricerca
008600000627      *
008700030604     C                   WRITE     video2z
008800030604     C                   EXFMT     video2c
008900011227     C                   SELECT
009000011228     c* fine
009100011227     C                   WHEN      *INKC=*ON
009200011227     C                   EVAL      $VIDEO=*BLANKS
009300011228     c* guida
009400011227     C                   WHEN      *INKL=*ON
009500030604     C                   EVAL      $VIDEO='VIDEO1'
009600011228     c* ripristino
009700011227     C                   WHEN      *INKE=*ON
009800011227     C                   EVAL      WrkCarS01 = *ON
009900011227
010000011227     C                   OTHER
010100011227
010200011227     C                   EXSR      GestioneSFL
010300011227     C                   ENDSL
010400011228
010500011227     C                   ENDDO
010600031027     C                   ENDSR
010700011227     c**********************************************************************
010800011227     c* Controlli video R01.
010900011227     c**********************************************************************
011000030604     C     Chkvideo1     BEGSR
011100000609      *
011200000608     C                   EVAL      *IN99 = *OFF
011300040728     C                   EVAL      *IN84 = *OFF
011400081009     C                   EVAL      *IN40 = *OFF
011500090105     c                   clear                   vidfild
011600080930     c     vidfil        chain     azorg01l
011700090105     c                   if        %found(azorg01l)
011800080930     c                   movel     orgdes        vidfild
011900090105     c                   endif
012000090105      *
012100000608     C                   ENDSR
012200011227     c**********************************************************************
012300011227     c* inizializza r01
012400011227     c**********************************************************************
012500030604     C     Inzvideo1     BEGSR
012600000608
012700090105     c                   clear                   vidfil
012800030604     C                   EXSR      Chkvideo1
012900000609
013000000608     C                   ENDSR
013100030604     c**********************************************************************
013200030605     C     opzione       BEGSR
013300030605     c**********************************************************************
013400030610     c                   move      v1csce        modalita
013500040728     c                   movea     '000000000000'*in(40)
013600040728     c                   movea     '00000'       *in(52)
013700040728     c                   setoff                                       96
013800030605     c                   select
013900090105     c                   when      v1csce = '2' and not *in21
014000030605     c                   exsr      modifica
014100090105     c                   when      v1csce = '3' and not *in21
014200030605     c                   exsr      copia
014300030605     c                   when      v1csce = '5'
014400030606     c                   seton                                        21
014500030606     c                   exsr      modifica
014600081013     c                   when      v1csce = '4' and not *in21
014700040728     c                   seton                                        21
014800040728     c                   exsr      modifica
014900040728     c                   setoff                                       2128
015000040728     c                   clear                   $msg
015100030605     c                   endsl
015200030604     C                   ENDSR
015300030605     c**********************************************************************
015400030605     C     modifica      BEGSR
015500030605     c**********************************************************************
015600090105     c     kadk          chain     fiadk01l                           98
015700030610     c                   if        not *in98
015800090105     c                   move      ADklnp        vADklnp
015900090105     c                   move      ADklna        vADklna
016000090105     c                   move      ADkkm         vADkkm
016100090105      * decodifica linea partenza
016200090105     c                   clear                   vadklnpd
016300090105     c     vadklnp       chain     azorg01l
016400090105     c                   if        %found(azorg01l)
016500090105     c                   movel     orgdes        vadklnpd
016600081001     c                   endif
016700090105      * decodifica linea arrivo
016800090105     c                   clear                   vadklnad
016900090105     c     vadklna       chain     azorg01l
017000090105     c                   if        %found(azorg01l)
017100090105     c                   movel     orgdes        vadklnad
017200090105     c                   endif
017300030605     c                   exsr      Gesvideo3
017400030605     c                   endif
017500030605     C                   ENDSR
017600030605     c**********************************************************************
017700030605     C     copia         BEGSR
017800030605     c**********************************************************************
017900080930     c                   clear                   video3
018000030610     c                   exsr      Gesvideo3
018100030605     C                   ENDSR
018200030605     c**********************************************************************
018300030605     C     gesvideo3     BEGSR
018400030605     c**********************************************************************
018500030702     c                   clear                   fatto
018600030605     c                   do        *hival
018700030605     c                   exfmt     video3
018800081001     c                   setoff                                       2896
018900081001     c                   clear                   $msg
019000030605     c   kl              leave
019100040728     c  n21              exsr      controv3
019200030624     c   96              iter
019300040728     c   kf              exsr      aggiorna
019400030702     c                   if        fatto <> *blank
019500030702     c                   leave
019600030702     c                   endif
019700030605
019800030605     c                   enddo
019900030605     C                   ENDSR
020000030605     c**********************************************************************
020100030624     C     Controv3      BEGSR
020200030605     c**********************************************************************
020300030613      *
020400030702      * *in96 errore generico videata dettaglio
020500040728     c                   setoff                                       9628
020600090107     c                   setoff                                       404142
020700040728     c                   clear                   $msg
020800040728     c                   if        modalita ='4'
020900040728     c                   goto      endctr
021000040728     c                   endif
021100090105     c                   if        vadklnp = 0
021200090105     c                   seton                                        4096
021300090105     c                   endif
021400090105      * decodifica linea partenza
021500090105     c                   clear                   vadklnpd
021600090105     c     vadklnp       chain     azorg01l
021700090105     c                   if        %found(azorg01l)
021800090112     c                             and (orgfag ='A' or orgfag = 'F')
021900090105     c                   movel     orgdes        vadklnpd
022000090105     c                   else
022100090105     c                   seton                                        4096
022200090105     c                   endif
022300090105     c                   if        vadklna = 0
022400090105     c                   seton                                        4196
022500090105     c                   endif
022600090105      * decodifica linea arrivo
022700090105     c                   clear                   vadklnad
022800090105     c     vadklna       chain     azorg01l
022900090105     c                   if        %found(azorg01l)
023000090112     c                             and (orgfag ='A' or orgfag = 'F')
023100090105     c                   movel     orgdes        vadklnad
023200090105     c                   else
023300090105     c                   seton                                        4196
023400090105     c                   endif
023500090107     c                   if        modalita = *blank
023600090107     c     kadk1         chain     fiadk01l
023700090107     c                   if        %found(fiadk01l)
023800090107     c                   seton                                        4296
023900090107     c                   endif
024000090107     c                   endif
024100040728     C     endctr        ENDSR
024200030605     c**********************************************************************
024300030605     C     aggiorna      BEGSR
024400030605     c**********************************************************************
024500090105     c                   move      vADkkm        ADkkm
024600030610      * inserimento e copia scrive altrimenti aggiorna il record esistente
024700040728     c                   select
024800040728     c                   when      modalita = '4'
024900090105     c                   move      'A'           adkatb
025000090105     c                   update    fiadk000
025100090105     c                   clear                   adkatb
025200081009
025300090105     c                   when      modalita = *blank
025400090105     c                   move      vadklnp       adklnp
025500090105     c                   move      vadklna       adklna
025600090105     c                   write     fiadk000
025700081007      *
025800040728     c                   other
025900090105     c                   update    fiadk000
026000040728     c                   endsl
026100030702     c                   move      'X'           fatto
026200030605     C                   ENDSR
026300011228     c**********************************************************************
026400011228     c* carica sfl
026500011228     c**********************************************************************
026600030604     C     Carvideo2     BEGSR
026700011228     C                   IF        WrkCarS01 = *ON
026800030604     C                   EVAL      snrr1   = 0
026900011228     C                   EVAL      *IN90 = *ON
027000020219     C                   EVAL      *IN91 = *OFF
027100030604     C                   WRITE     video2c
027200011228     C                   EVAL      *IN90 = *OFF
027300090105     c     vidfil        setll     fiadk01l
027400030701     c                   do        *hival
027500090105     c                   read      fiadk01l                               97
027600030701     c   97              leave
027700090105     c                   if        adkatb <> *blank
027800080930     c                   iter
027900080930     c                   endif
028000030701     c                   move      *blanks       v1csce
028100090105     c                   z-add     adklnp        vi2lnp
028200090105     c                   z-add     adklna        vi2lna
028300090105     c                   z-add     adkkm         vi2km
028400090105     c                   clear                   vi2lnpd
028500090105     c     vi2lnp        chain     azorg01l
028600090105     c                   if        %found(azorg01l)
028700090105     c                   movel     orgdes        vi2lnpd
028800090105     c                   endif
028900090105     c                   clear                   vi2lnad
029000090105     c     vi2lna        chain     azorg01l
029100090105     c                   if        %found(azorg01l)
029200090105     c                   movel     orgdes        vi2lnad
029300090105     c                   endif
029400030701      *esco per raggiunta massima capacità sfl
029500030617     c                   if        snrr1 > 9990
029600030617     c                   leave
029700030617     c                   endif
029800030617
029900030604     C                   EVAL      snrr1 = snrr1 + 1
030000030604     C                   WRITE     video2
030100011228     C                   ENDDO
030200011228
030300030604     C                   IF        snrr1  > 0
030400020219     C                   EVAL      *IN91 = *ON
030500020219     C                   MOVE      1             OK                1 0
030600020219     C                   ELSE
030700020219     C                   MOVE      0             OK                1 0
030800030701     C                   ENDIF
030900030612     C                   EVAL      nrr1  = 1
031000011228     C                   ENDIF
031100020219     C                   EVAL      WrkCarS01 = *OFF
031200030604     C                   EVAL      snrr1  = 1
031300030604     C                   EVAL      csrrrn = 1
031400011228     C                   ENDSR
031500011227     c**********************************************************************
031600011227     c* gestione sfl
031700011227     c**********************************************************************
031800000613     C     GestioneSFL   BEGSR
031900000627      *
032000000627     C                   EVAL      WrkEofS01 = *OFF
032100000627      * F10 Immissione.
032200081013     C  n21              IF        *INKJ
032300080930     c                   clear                   video3
032400081010     c                   seton                                        23
032500040728     c                   clear                   modalita
032600090105     c                   clear                   fiadk000
032700040728     c                   move      knmus         vknmus
032800040728     c                   move      knsif         vknsif
032900040728     c                   movel     ragut         rsut
033000040728     c                   exsr      Gesvideo3
033100090105     c                   setoff                                       23
033200040728     c                   eval      wrkcars01 = *on
033300040728     c                   if        csrrrn = 0
033400040728     c                   goto      noriga
033500040728     c                   endif
033600040728     c                   endif
033700030605      * Elaborazione opzioni.
033800011228     c*
033900030605     c                   do        *hival        X                 4 0
034000030605     c     X             CHAIN     video2                             50
034100030605     C   50              LEAVE
034200030604     C                   IF        v1csce <> *blanks
034300030605     c                   exsr      opzione
034400030611
034500030611     c                   move      *blank        v1csce
034600030605     c                   update    video2
034700030611
034800011228     C                   ENDIF
034900030605     C                   enddo
035000040728      *
035100090105     c                   if        modalita = '4' or modalita = '2'
035200040728     c                   eval      wrkcars01 = *on
035300040728     c                   endif
035400000627      *
035500031003     C     noriga        ENDSR
035600011227     c**********************************************************************
035700000607     C     *INZSR        BEGSR
035800011227     c**********************************************************************
035900000607      *
036000000607     C     *ENTRY        PLIST
036100000607     C                   PARM                    Kpjba
036200030604     C                   Z-ADD     1             CODUT
036300030604     C                   CALL      'X§PARUT'
036400030604     C                   PARM                    UTEDSE
036500030604     C                   MOVEL     REC80         CNCR80
036600030703     C                   MOVEL     Ragut         rsut
036700030703     C                   MOVEL     knsif         vknsif
036800030703     C                   MOVEL     knmus         vknmus
036900030702      *reperimento data
037000030702     C                   TIME                    W0120            14 0
037100030702     C                   MOVE      W0120         WDAT              8 0
037200030702     C*
037300030702     C                   Z-ADD     WDAT          G02DAT
037400030702     C                   MOVEL     *BLANK        G02ERR
037500030702     C                   CALL      'XSRDA8'
037600030702     C                   PARM                    WLBDA8
037700030702     C* UDATE A 8 IN AAAA/MM/GG
037800030702     C                   Z-ADD     G02INV        WUDA              8 0
037900080930     C                   CLEAR                   Tibs36Ds
038000080930     C                   EVAL      I36ute = knmus
038100080930     C                   CALL      'TIBS36R'
038200080930     C                   PARM                    tibs36ds
038300030718
038400090105     C     Kadk          KLIST
038500090105     C                   KFLD                    vi2lnp
038600090105     C                   KFLD                    vi2lna
038700000607      *
038800090107     C     Kadk1         KLIST
038900090105     C                   KFLD                    vadklnp
039000090105     C                   KFLD                    vadklna
039100000607      *---------------------------------------------------------------------
039200011227     C                   ENDSR
