000100030604     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200130812      *
000300140430     Ftrtb90d   CF   E             WORKSTN
000400030604     f                                     sfile(video2:snrr1)
000500140507     Ftntbe01l  uF a E           k disk    usropn
000600030701     Fazorg01l  iF   E           k disk
000700011228      *
000800020103     D Psds           SDS
000900020103     D  PgmName          *PROC
001000011228     D Kpjba         E DS
001100011228      *
001200030604     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
001300030604     D CNCR80        E DS
001400030617     d Tibs36Ds      e ds
001500140512     d Tibs55Ds      e ds
001600140430     d dpspflag      e ds
001700011227     d data            ds            10
001800011228     d aa                      3      4
001900011228     d mm                      6      7
002000011228     d gg                      9     10
002100011228      *
002200030702     D fatto           S              1
002300030610     D modalita        S              1
002400030604     D x               S              4  0
002500011227     D WrkEofS01       S              1
002600011227     D WrkCarS01       S              1
002700011228     D $VIDEO          S             10
002800030604     D snrr1           S              5i 0
002900030721     D Savkpjbu        S                   like(kpjbu)
003000030604     D SavOpzione      S                   like(v1csce)
003100030606     d dataiso         s               d   datfmt(*iso)
003200030606     d dataeur         s               d   datfmt(*eur)
003300140430     D savfliter       S                   like(vfliter)
003400140430     D savfliterd      S                   like(vfliterd)
003500030613     D WLBDA8          DS
003600030613     D  G02DAT                 1      8  0
003700030613     D  G02INV                 9     16  0
003800030613     D  G02ERR                17     17
003900030613     D  G02TGI                18     22  0
004000140512     D Ctas            C                   CONST('/TNTBE01L)')
004100140512     D OVRTAb          S              1    DIM(39) CTDATA PERRCD(39)
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
006900140502     c   99              exsr      opzione
007000140502     C  N99              EXSR      Wrkvideo2c
007100011227     C                   ENDSL
007200011227      *
007300011227     C                   ENDDO
007400011227      *
007500011227     C                   EXSR      Uscita
007600011227     c**********************************************************************
007700011227     c* uscita
007800011227     c**********************************************************************
007900011227     C     Uscita        BEGSR
008000011227      *
008100011227     C                   EVAL      *INLR = *ON
008200011227     C                   RETURN
008300011227      *
008400011227     C                   ENDSR
008500011227     c**********************************************************************
008600011227     c* gestione sfl
008700011227     c**********************************************************************
008800030604     C     Wrkvideo2c    BEGSR
008900000627      *
009000011227     C                   EVAL      WrkCarS01 = *ON
009100030604     C                   EVAL      $Video = 'VIDEO2C'
009200011227      *
009300030604     C                   DOU       $Video <> 'VIDEO2C'
009400030604     C                   EXSR      Carvideo2
009500011228     c*Descrizione della ricerca
009600000627      *
009700030604     C                   WRITE     video2z
009800030604     C                   EXFMT     video2c
009900011227     C                   SELECT
010000011228     c* fine
010100011227     C                   WHEN      *INKC=*ON
010200011227     C                   EVAL      $VIDEO=*BLANKS
010300011228     c* guida
010400011227     C                   WHEN      *INKL=*ON
010500030604     C                   EVAL      $VIDEO='VIDEO1'
010600011228     c* ripristino
010700011227     C                   WHEN      *INKE=*ON
010800011227     C                   EVAL      WrkCarS01 = *ON
010900011227
011000011227     C                   OTHER
011100011227
011200011227     C                   EXSR      GestioneSFL
011300011227     C                   ENDSL
011400011228
011500011227     C                   ENDDO
011600031027     C                   ENDSR
011700011227     c**********************************************************************
011800011227     c* Controlli video R01.
011900011227     c**********************************************************************
012000030604     C     Chkvideo1     BEGSR
012100000609      *
012200000608     C                   EVAL      *IN99 = *OFF
012300040728     C                   EVAL      *IN84 = *OFF
012400140430     c                   if        vfliter =  999 or vfliter = 0
012500140430     c                   eval      vfliterd = 'Tutte le filiali'
012600140502     c                   setoff                                       99
012700140502     c                   clear                   v1csce
012800140502     c                   clear                   vfils
012900140430     c                   else
013000140502     c                   seton                                        99
013100140430     c     vfliter       chain     azorg01l
013200140502     c                   if        not %found(azorg01l)
013300140502     c                   seton                                        8428
013400140502     c                   eval      $msg = 'Filiale non trovata'
013500140502     c                   leavesr
013600140502     c                   endif
013700140505     c                   movel     vfliter       tbeke1
013800140505     c     ktbe2         chain     tntbe01l                           98
013900140505     c                   if        not *in98
014000140502     c                   move      vfliter       vfils
014100140502     c                   move      '2'           v1csce
014200140430     c                   movel     orgdes        vfliterd
014300140508     C                   EVAL      $Video = 'VIDEO1'
014400140505     c                   else
014500140508     c                   seton                                        16
014600140505     c                   move      vfliter       vfile
014700140505     c                   movel     orgdes        vfiled
014800140505     c                   clear                   vorae
014900140505     c                   clear                   vgpse
015000140506      * prende rekord di default
015100140507     c                   exsr      fildef
015200140507     c                   exsr      gesvideo3
015300140507     C                   EVAL      $Video = 'VIDEO1'
015400140507     c                   endif
015500140507     c                   endif
015600140508     c                   setoff                                       16
015700140507      *
015800140507     C                   ENDSR
015900140507      *______________________________________________________________
016000140507     c     fildef        begsr
016100140507      *______________________________________________________________
016200140506     c                   movel     999           tbeke1
016300140506     c     ktbe2         chain     tntbe01l                           98
016400140506     c                   if        not *in98
016500140506     c                   movel     tbeuni        dpspflag
016600140506     c                   move      §PSPCUS       voradft
016700140506     c                   move      §PSPGPS       vgpsdft
016800140506     c                   endif
016900140507     c                   endsr
017000011227     c**********************************************************************
017100011227     c* inizializza r01
017200011227     c**********************************************************************
017300030604     C     Inzvideo1     BEGSR
017400000608
017500140430     c                   z-add     999           vfliter
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
018900140430     c*                  when      v1csce = '3'
019000140430     c*                  exsr      copia
019100140430     c*                  when      v1csce = '5'
019200140430     c*                  seton                                        21
019300140430     c*                  exsr      modifica
019400140430     c*                  setoff                                       21
019500040728     c                   when      v1csce = '4'
019600140502     c*                  seton                                        21
019700040728     c                   exsr      modifica
019800140502     c*                  setoff                                       2128
019900140502     c                   setoff                                       28
020000040728     c                   clear                   $msg
020100030605     c                   endsl
020200030604     C                   ENDSR
020300030605     c**********************************************************************
020400030605     C     modifica      BEGSR
020500030605     c**********************************************************************
020600140430      * prende rekord di default
020700140507     c                   exsr      fildef
020800140430      * aggancia rekord in modifica
020900140430     c                   move      vfils         vfile
021000140430     c                   movel     vfile         tbeke1
021100140430     c     ktbe2         chain     tntbe01l                           98
021200030610     c                   if        not *in98
021300140502     c                   movel     tbeuni        dpspflag
021400140430     c                   move      §PSPCUS       vorae
021500140430     c                   move      §PSPGPS       vgpse
021600030701      * decodifica p.o.
021700140430     c     vfile         chain     azorg01l                           95
021800140430     c  n95              movel     orgdes        vfiled
021900140430     c   95              clear                   vfiled
022000030605     c                   exsr      Gesvideo3
022100030605     c                   endif
022200030605     C                   ENDSR
022300030605     c**********************************************************************
022400030605     C     copia         BEGSR
022500030605     c**********************************************************************
022600140430     c                   exsr      Gesvideo3
022700030605     C                   ENDSR
022800030605     c**********************************************************************
022900030605     C     gesvideo3     BEGSR
023000030605     c**********************************************************************
023100030702     c                   clear                   fatto
023200030605     c                   do        *hival
023300140505      *se valorizzato protegge campo filiale
023400140507     c                   if        vfile > 0 and modalita <> *blank
023500140505     c                   seton                                        23
023600140505     c                   else
023700140505     c                   setoff                                       23
023800140505     c                   endif
023900030605     c                   exfmt     video3
024000140507     c                   setoff                                       28
024100030605     c   kl              leave
024200040728     c  n21              exsr      controv3
024300030624     c   96              iter
024400040728     c   kf              exsr      aggiorna
024500140508     c                   if        *inkq
024600140508     c                   move      '4'           modalita
024700140508     c                   exsr      aggiorna
024800140508     c                   move      '2'           modalita
024900140508     c                   endif
025000030702     c                   if        fatto <> *blank
025100140507     c                   eval      wrkcars01 = *on
025200030702     c                   leave
025300030702     c                   endif
025400030605
025500030605     c                   enddo
025600030605     C                   ENDSR
025700030605     c**********************************************************************
025800030624     C     Controv3      BEGSR
025900030605     c**********************************************************************
026000030613      *
026100140502     c                   if        modalita = '4' and vfile = 999
026200140502     c                   eval      $msg  = 'Filiale 999 non cancella-
026300140502     c                             bile contiene valori default'
026400140502     c                   seton                                        9628
026500140502     c                   goto      endctr
026600140502     c                   endif
026700030702      * *in96 errore generico videata dettaglio
026800040728     c                   setoff                                       9628
026900040728     c                   clear                   $msg
027000140430     c                   if        modalita =' ' or modalita = '3'
027100140430     c                   movel     vfile         tbeke1
027200140430     c     ktbe2         chain     tntbe01l                           98
027300140430     c                   if        not *in98
027400140430     c                   eval      $msg  = 'Filiale già presente in -
027500140430     c                             tabella inserimento non possibile'
027600140430     c                   seton                                        9628
027700140430     c                   goto      endctr
027800140506     c
027900140506     c                   endif
028000140430     c                   endif
028100030703      * decodifica p.o.
028200140430     c     vfile         chain     azorg01l                           95
028300140430     c  n95              movel     orgdes        vfiled
028400140430     c   95              clear                   vfiled
028500140506      * controllo valori se immessi direttamente
028600140506     c                   if        vorae = *blank or vgpse = *blank
028700140506     c                   eval      $msg  = 'Valori a Blank non -
028800140506     c                             ammessi '
028900140506     c                   seton                                        9628
029000140506     c                   goto      endctr
029100140506     c                   endif
029200140506     C     endctr        ENDSR
029300030605     c**********************************************************************
029400030605     C     aggiorna      BEGSR
029500030605     c**********************************************************************
029600140430     c                   move      vorae         §PSPCUS
029700140430     c                   move      vgpse         §PSPGPS
029800140502     c                   movel(p)  dpspflag      tbeuni
029900030610      * inserimento e copia scrive altrimenti aggiorna il record esistente
030000040728     c                   select
030100140502      *
030200040728     c                   when      modalita = '4'
030300140430     c                   delete    tntbe000
030400140502      *
030500040728     c                   when      modalita = *blank or modalita = '3'
030600140430     c                   movel     'PD'          TBEAPL
030700140430     c                   move      'PSP'         tbecod
030800140430     c                   movel(p)  vfile         tbeke1
030900140430     c                   movel(p)  'FLAG'        tbeke2
031000140430     c                   clear                   TBEATB
031100140430     c                   clear                   TBESIF
031200140430     c                   clear                   TBELIN
031300140430     c                   clear                   TBEFTT
031400140430     c                   clear                   TBEFLT
031500140430     c                   clear                   TBEFTR
031600140430     c                   clear                   TBEDTR
031700140430     c                   write     tntbe000
031800040728     c                   other
031900140430     c                   update    tntbe000
032000040728     c                   endsl
032100030702     c                   move      'X'           fatto
032200030605     C                   ENDSR
032300011228     c**********************************************************************
032400011228     c* carica sfl
032500011228     c**********************************************************************
032600030604     C     Carvideo2     BEGSR
032700140507      * carica valori default filiale 999
032800140507     c                   exsr      fildef
032900140507      *
033000011228     C                   IF        WrkCarS01 = *ON
033100030604     C                   EVAL      snrr1   = 0
033200011228     C                   EVAL      *IN90 = *ON
033300020219     C                   EVAL      *IN91 = *OFF
033400030604     C                   WRITE     video2c
033500011228     C                   EVAL      *IN90 = *OFF
033600140430     c     'PSP'         setll     tntbe01l
033700030701     c                   do        *hival
033800140502     c     'PSP'         reade     tntbe01l                               97
033900030701     c   97              leave
034000140430     c                   if        tbeke2 <> 'FLAG'
034100140430     c                   iter
034200140430     c                   endif
034300140430     c                   movel     tbeke1        vfils
034400140430     c     vfils         chain     azorg01l                           95
034500140430     c  n95              movel     orgdes        vfilsd
034600140430     c   95              clear                   vfilsd
034700140502     c                   if        vfils = 999
034800140502     c                   eval      vfilsd = 'Filiale default'
034900140502     c                   endif
035000140502      *
035100140502     c                   movel     tbeuni        dpspflag
035200140430     c                   select
035300140430     c                   when      §PSPCUS = '0'
035400140430     c                   eval      voras = 'NO'
035500140430     c                   when      §PSPCUS = '1'
035600140430     c                   eval      voras = 'Alert'
035700140430     c                   when      §PSPCUS = '2'
035800140430     c                   eval      voras = 'SI'
035900140430     c                   endsl
036000140430     c                   select
036100140430     c                   when      §PSPGPS = '0'
036200140430     c                   eval      vgps  = 'Attivo'
036300140430     c                   when      §PSPGPS = '1'
036400140430     c                   eval      vgps  = 'Disattivo'
036500140430     c                   endsl
036600140507     c     §PSPCUS       comp      voradft                            2626
036700140507     c     §PSPGPS       comp      vgpsdft                            2525
036800030701      *esco per raggiunta massima capacità sfl
036900030617     c                   if        snrr1 > 9990
037000030617     c                   leave
037100030617     c                   endif
037200030617
037300030604     C                   EVAL      snrr1 = snrr1 + 1
037400030604     C                   WRITE     video2
037500011228     C                   ENDDO
037600011228
037700030604     C                   IF        snrr1  > 0
037800020219     C                   EVAL      *IN91 = *ON
037900020219     C                   MOVE      1             OK                1 0
038000020219     C                   ELSE
038100020219     C                   MOVE      0             OK                1 0
038200030701     C                   ENDIF
038300030612     C                   EVAL      nrr1  = 1
038400011228     C                   ENDIF
038500020219     C                   EVAL      WrkCarS01 = *OFF
038600030604     C                   EVAL      snrr1  = 1
038700030604     C                   EVAL      csrrrn = 1
038800011228     C                   ENDSR
038900011227     c**********************************************************************
039000011227     c* gestione sfl
039100011227     c**********************************************************************
039200000613     C     GestioneSFL   BEGSR
039300000627      *
039400000627     C                   EVAL      WrkEofS01 = *OFF
039500000627      * F10 Immissione.
039600040728     C                   IF        *INKJ
039700140502     c                   clear                   modalita
039800140502     c                   clear                   video3
039900140502      * prende rekord di default
040000140507     c                   exsr      fildef
040100140507      *
040200040728     c                   move      vfliter       savfliter
040300040728     c                   move      vfliterd      savfliterd
040400040728     c                   move      knmus         vknmus
040500040728     c                   move      knsif         vknsif
040600040728     c                   movel     ragut         rsut
040700040728     c                   move      savfliter     vfliter
040800040728     c                   move      savfliterd    vfliterd
040900040728     c                   exsr      Gesvideo3
041000040728     c                   eval      wrkcars01 = *on
041100040728     c                   if        csrrrn = 0
041200040728     c                   goto      noriga
041300040728     c                   endif
041400040728     c                   endif
041500030605      * Elaborazione opzioni.
041600011228     c*
041700030605     c                   do        *hival        X                 4 0
041800030605     c     X             CHAIN     video2                             50
041900030605     C   50              LEAVE
042000030604     C                   IF        v1csce <> *blanks
042100030605     c                   exsr      opzione
042200030611
042300030611     c                   move      *blank        v1csce
042400030605     c                   update    video2
042500030611
042600011228     C                   ENDIF
042700030605     C                   enddo
042800040728      *
042900040728     c                   if        modalita = '1' or modalita = '3'
043000040728     c                             or modalita = '4'
043100040728     c                   eval      wrkcars01 = *on
043200040728     c                   endif
043300000627      *
043400031003     C     noriga        ENDSR
043500011227     c**********************************************************************
043600000607     C     *INZSR        BEGSR
043700011227     c**********************************************************************
043800000607      *
043900000607     C     *ENTRY        PLIST
044000000607     C                   PARM                    Kpjba
044100030604     C                   Z-ADD     1             CODUT
044200030604     C                   CALL      'X§PARUT'
044300030604     C                   PARM                    UTEDSE
044400030604     C                   MOVEL     REC80         CNCR80
044500030703     C                   MOVEL     Ragut         rsut
044600030703     C                   MOVEL     knsif         vknsif
044700030703     C                   MOVEL     knmus         vknmus
044800030702      *reperimento data
044900030702     C                   TIME                    W0120            14 0
045000030702     C                   MOVE      W0120         WDAT              8 0
045100030702     C*
045200140430     C                   Z-ADD     WDAT          G02DAT
045300030702     C                   MOVEL     *BLANK        G02ERR
045400030702     C                   CALL      'XSRDA8'
045500030702     C                   PARM                    WLBDA8
045600030702     C* UDATE A 8 IN AAAA/MM/GG
045700030702     C                   Z-ADD     G02INV        WUDA              8 0
045800030606      *profilo di filiale attivo indicatore di protezione campi non manutenziona
045900030617     C                   CLEAR                   Tibs36Ds
046000030617     C                   EVAL      I36ute = knmus
046100030617     C                   CALL      'TIBS36R'
046200030617     C                   PARM                    tibs36ds
046300140507     c                   if        o36pou =  046
046400140512     C                   CLEAR                   tibs55ds
046500140512     c                   movel     'L'           i50tla
046600140512     c                   Movel     001           I50apo
046700140512     C                   CALL      'TIBS55R'
046800140512     C                   PARM                    tibs55ds
046900140512     C
047000140512     C* overdbf  TITAS30C
047100140512     c                   movea     o50alg        ovrtab(30)
047200140512     C                   SETOFF                                       01
047300140512     C                   CLEAR                   COMMAN           80
047400140512     C                   MOVEA     ovrtab        COMMAN
047500140512     c                   eval      comman=%trim(comman)+ctas
047600140512     C                   CALL      'QCMDEXC'
047700140512     C                   PARM                    COMMAN
047800140512     C                   PARM      50            LUNG             15 5
047900140507     c                   endif
048000140507     c                   open      tntbe01l
048100140507     C*
048200030718
048300000607      *
048400140430     C     Ktbe2         KLIST
048500140430     C                   KFLD                    TBECOD
048600140430     C                   KFLD                    TBEKE1
048700140430     C                   kfld                    TBEKE2
048800140502     C                   move      'PSP'         TBECOD
048900140502     C                   movel(p)  'FLAG'        TBEKE2
049000140430      *
049100000607      *---------------------------------------------------------------------
049200011227     C                   ENDSR
049300140512**         OVRTAB
049400140512OVRDBF FILE(Tntbe01l) TOFILE(XXXXXXXXXX
