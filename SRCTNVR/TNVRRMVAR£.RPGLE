000100011003     H DECEDIT('0,') DATEDIT(*DMY.)
000201161006     Fazute01l  if   E           k DISK
000202161006     Ftitasv1l  if   E           k DISK
000203161006     Ftita7v1l  if   E           k DISK
000204161006     Ftitas30c  if   E           k DISK    prefix(t_)
000205161007     Ftita730c  if   E           k DISK    prefix(t_)
000206161006     Ffiarbt0f  o  a E             DISK
000207161007     Ffiarbu0f  o  a E             DISK
000208161006     Ffnarbm0f  o  a E             DISK
000209161006     Ffnarbd0f  o  a E             DISK
000210161006     Ffnarbk0f  o  a E             DISK
001501161006
001502161007     d dst_titas       ds
001503161007     D  svt                           1    DIM(21)                              SIGLA  VARIE
001504161007     D  sit                          11  3 DIM(20)                              VALORE VARIE
001505161007     d dst_titasv      ds
001506161007     D  svv                           1    DIM(21)                              SIGLA  VARIE
001507161007     D  siv                          11  3 DIM(20)                              VALORE VARIE
001508161007     d x               s              2  0
001600011003     C*------------------------------------------------------------------------*
001700011003     C* MAIN LINE
001800011003     C*------------------------------------------------------------------------*
004500011003     C*
004501161006     C     KEYTAS        KLIST                                                  TITAS30C
004502161006     C                   KFLD                    tASAAS                         ..ANNO
004503161006     C                   KFLD                    tASLNP                         ..LNP
004504161006     C                   KFLD                    tASNRS                         ..SERIE
004505161006     C                   KFLD                    tASNSP                         ..SPEDIZIONE
004506161006     C                   KFLD                    tASTBL                         ..TIPO BOLLA
004507161007     C     KEYTA7v       KLIST                                                  TITAS30C
004508161007     C                   KFLD                    tASAAS                         ..ANNO
004509161007     C                   KFLD                    tASLNP                         ..LNP
004510161007     C                   KFLD                    tASNRS                         ..SERIE
004511161007     C                   KFLD                    tASNSP                         ..SPEDIZIONE
004512161007     C                   KFLD                    TASDTV
004513161007     C                   KFLD                    tASORV                         ..TIPO BOLLA
004514161007     C                   KFLD                    tASPRU                         ..TIPO BOLLA
004515161006      /free
004516161006         setll *loval titasv1l;
004517161006         read         titasv1l;
004518161006         dow not %eof(titasv1l);
004519161006         chain taspru azute01l;
004520161006            if %found(azute01l);
004521161006      // chaino titas per vedere cosa è variato
004522161006               chain (tasaas:taslnp:tasnrs:tasnsp:tastbl) titas30c ;
004523161007            if %found(titas30c);
004524161007               exsr sr_arbt00;
004525161007               exsr sr_arbm00;
004526161007               exsr sr_arbk00;
004527161007               if utefil=046;
004534161006      // variazione fatta da utente di sede
004537161007      // . . . Peso/volume
004538161007                  exsr sr_arbDI ;
004539161006               endif;
004540161007            endif;
004541161006            endif;
004542161006            read         titasv1l;
004543161006         enddo;
013900161006         *inlr=*on;
013901161006      /end-free
013902161006      *------------------------------------------------------------------------*
013903161006     C     sr_arbt00     BEGSR
013904161006     ù*------------------------------------------------------------------------*
013905161007     c* Estensione Tassazione
013906161007     c* Carico schiere varie per il confronto
013907161007     c                   clear                   svv
013908161007     c                   clear                   svt
013909161007     c                   clear                   siv
013910161007     c                   clear                   sit
013911161007     c* schiere da tita7V
013912161007     c                   z-add     1             x
013913161007     C     KEYTA7v       SETLL     TITA7v1l
013914161007     C     KEYTA7v       READE     TITA7v1l
013915161007     C                   DOW       not %eof(tita7v1l)
013916161007     c                   eval      svv(x)=ta7svn
013917161007     c                   eval      siv(x)=ta7van
013918161007     c                   add       1             x
013919161007     C     KEYTA7v       READE     TITA7v1l
013920161007     c                   enddo
013921161007     c* schiere da tita7
013922161007     c                   z-add     1             x
013923161007     C     KEYTAS        SETLL     TITA730c
013924161007     C     KEYTAS        READE     TITA730c
013925161007     C                   DOW       not %eof(tita730c)
013926161010     c                   eval      svt(x)=t_ta7svn
013927161010     c                   eval      sit(x)=t_ta7van
013928161007     c                   add       1             x
013929161007     C     KEYTAS        READE     TITA730c
013930161007     c                   enddo
014000161006     c* FIARBT0F - variazioni di tassazione:
014100161006     c* Codice tariffa
014200161006     c                   if        t_tasctr<>tasctr  or
014300161006     c* Importo da assicurare
014301161007     c                             (utefil=046 and  (
014400161006     c                             t_tasias<>tasias or
014500161007     c                             (t_tasvas<>tasvas and t_tasias>0))) or
014600161006     c* Importi Tassazione
014700161006     c                             t_tasdiv<>tasdiv or
014800161006     c                             t_taspor<>taspor or
014900161006     c                             t_tasimv<>tasimv or
015000161006     c                             t_tasfei<>tasfei or
015001161010     c                             t_TASSV1<>TASSV1 or
015002161010     c                             t_TASVA1<>TASVA1 or
015003161010     c                             t_TASSV2<>TASSV2 or
015004161010     c                             t_TASVA2<>TASVA2 or
015005161010     c                             t_TASSV3<>TASSV3 or
015006161010     c                             t_TASVA3<>TASVA3 or
015007161007     c***                          svv<>svt or siv<>sit  OR
015008161007     c                             dst_titas<>dst_titasv OR
015200161006     c* Quantità fatturabile
015300161006     c                             t_tasqft<>tasqft or
015400161006     c* Cliente se spedizione in assegnato
015500161006     c                             %subst(tastbl:1:1)='A' and t_tasksc<>tasksc
015501161007     c                   clear                   fiarbt00
015600161006     c                   exsr      sr_ImpoArbt
015700161006     c                   write     fiarbt00
015800161006     c                   endif
015900161006     c                   endsr
016000161006      *------------------------------------------------------------------------*
016100161006      * Valorizza campi specifici storico variazioni FIARBT0F + scrittura FIARBU
016200161006      *------------------------------------------------------------------------*
016300161006     C     sr_ImpoArbt   BEGSR
016400161006
016401161007     c                   eval      arbaas=tasaas
016402161007     c                   eval      arblnp=taslnp
016403161007     c                   eval      arbnrs=tasnrs
016404161007     c                   eval      arbnsp=tasnsp
016405161007     c                   eval      arbcbo=%subst(tastbl:1:1)
016406161007     c                   eval      arbdtv=tasdtv
016407161007     c                   eval      arborv=tasorv
016408161007     c                   eval      arbpru=taspru
016409161007     c                   eval      %subst(arbpru:10:1)= 'S'
016500161006     c* ARBTRC-I o II Bolla --> in sede non c'è questa distinzione
016600161006     c*                  metto il primo byte del tipo bolla che serve
016700161006     c*                  per collegare univocamente l'estensione tassazione alla sua
016800161006     c*                  bolla
016900161006     c                   eval      arbtrc=%subst(tastbl:1:1)
017000161006     c* Causale Variazione Bolla
017100161006     c                   select
017200161006     c* Bolla Franco --> "TP"
017300161006     c                   when      %subst(tastbl:1:1)='F'
017400161006     c                   eval      arbcvb='TP'
017500161006     c* Bolla in Assegnato S.F. --> "T0"
017600161007     c                   when      t_tasdft=0
017700161006     c                   eval      arbcvb='T0'
017800161006     c* Bolla in Assegnato F.I. --> "T2"
017900161006     c                   other
018000161006     c                   eval      arbcvb='T2'
018100161006     c                   endsl
018200161006
018300161006     c                   move      tasksc        arbksc
018400161006     c                   move      tasctr        arbctr
018500161006     c                   eval      ARBQFT=tasqft
018600161006     c***                eval      ARBVMD=tasvmd
018700161006     c***                eval      ARBVAD=tasvad
018800161006     c                   eval      ARBIAS=tasias
018900161006     c                   eval      ARBVAS=tasvas
019000161006     c                   eval      ARBDIV=tasdiv
019100161006     c                   eval      ARBPOR=taspor
019200161006     c                   eval      ARBSV1=tassv1
019300161006     c                   eval      ARBVA1=tasva1
019400161006     c                   eval      ARBSV2=tassv2
019500161006     c                   eval      ARBVA2=tasva2
019600161006     c                   eval      ARBSV3=tassv3
019700161006     c                   eval      ARBVA3=tasva3
019800161006     c                   eval      ARBIMV=tasIMV
019900161006     c***                eval      ARBFIM=tasFIM
020000161006     c***                eval      ARBALI=tasALI
020100161006     c                   eval      ARBCEI=tasFEI
020200161006     c****               eval      ARBIFT=tasIFT
020300161007     c                   eval      ARBDFT=t_tasDFT
020400161007     c                   eval      ARBNFT=t_tasNFT
020500161007     c                   eval      ARBFIV=t_tasFIV
020600161006      * LEGGO ANCHE TUTTE LE VARIE E LE TRASCRIVO
020700161007     C     KEYTA7V       SETLL     TITA7v1l
020800161007     C     KEYTA7V       READE     TITA7v1l
020900161006     C                   DOW       not %eof(tita7v1l)
020901161007     c                   eval      arbdtv=ta7dtv
020902161007     c                   eval      arborv=ta7orv
020903161007     c                   eval      arbpru=ta7pru
020904161007     c                   eval      %subst(arbpru:10:1)= 'S'
021000161006     c                   eval      ARBSVN=ta7SVN
021100161006     c                   eval      ARBVAN=ta7VAN
021200161006     C                   WRITE     fiarbu00
021300161007     C     KEYTA7V       READE     TITA7v1l
021400161006     C                   ENDDO
021500161006     c                   endsr
021501161007      *------------------------------------------------------------------------*
021502161007     C     sr_arbm00     BEGSR
021503161007     c*------------------------------------------------------------------------*
021600161007     c* FNARBM0F - variazioni di mittente:
021700161007     c                   if        t_tasccm<>tasccm
021701161007     c                   clear                   fnarbm00
021800161007     c                   exsr      sr_ImpoArbm
021900161007     c                   write     fnarbm00
022000161007     c                   endif
022100161007     c                   endsr
022200161007      *------------------------------------------------------------------------*
022300161007      * Valorizza campi specifici storico variazioni FNARBM0F
022400161007      *------------------------------------------------------------------------*
022500161007     C     sr_ImpoArbm   BEGSR
022600161007
022601161007     c                   eval      arbaas=tasaas
022602161007     c                   eval      arblnp=taslnp
022603161007     c                   eval      arbnrs=tasnrs
022604161007     c                   eval      arbnsp=tasnsp
022605161007     c                   eval      arbcbo=%subst(tastbl:1:1)
022606161007     c                   eval      arbdtv=tasdtv
022607161007     c                   eval      arborv=tasorv
022608161007     c                   eval      arbpru=taspru
022609161007     c                   eval      %subst(arbpru:10:1)= 'S'
022700161007     c* Causale Variazione Bolla
022800161007     c                   eval      arbcvb='MI'
022900161007     c                   eval      ARBCCM=tasccm
023000161007     c                   eval      ARBFAP=tasfap
023600161007     c                   eval      ARBrsm=tasrsc
023700161007     c                   eval      ARBINM=TASINM
023800161007     c                   eval      ARBCAM=TASCAP
023900161007     c                   eval      ARBLOM=TASLOC
024000161007     c                   eval      ARBPRM=TASPRV
024100161007     c                   eval      ARBNZM=TASNAZ
024200161007     c                   eval      ARBcpi=taspiv
025900161007     c                   endsr
026000161007      *------------------------------------------------------------------------*
026100161007     C     sr_arbDi      BEGSR
026200161007     C*------------------------------------------------------------------------*
026201161007     c                   if        t_taspkf<>taspkf
026202161007     c                   clear                   fnarbd00
026211161007     c                   eval      arbcvb='IS'
026212161007     c                   exsr      sr_ImpoArbD
026213161007     c                   write     fnarbd00
026214161007     c                   endif
026215161007     c                   if        t_tasvlf<>tasvlf
026216161007     c                   clear                   fnarbd00
026225161007     c                   eval      arbcvb='I2'
026226161007     c                   exsr      sr_ImpoArbD
026227161007     c                   write     fnarbd00
026228161007     c                   endif
026300161007     c                   endsr
026400161007      *------------------------------------------------------------------------*
026500161007      * Valorizza campi specifici storico variazioni FNARBD0F
026600161007      *------------------------------------------------------------------------*
026700161007     C     sr_ImpoArbd   BEGSR
026701161007     c                   eval      arbaas=tasaas
026702161007     c                   eval      arblnp=taslnp
026703161007     c                   eval      arbnrs=tasnrs
026704161007     c                   eval      arbnsp=tasnsp
026705161007     c                   eval      arbcbo=%subst(tastbl:1:1)
026706161007     c                   eval      arbdtv=tasdtv
026707161007     c                   eval      arborv=tasorv
026708161007     c                   eval      arbpru=taspru
026709161007     c                   eval      %subst(arbpru:10:1)= 'S'
026800161007     c                   eval      ARBRSD=t_tasrsd
026900161007     c                   eval      ARBIND=t_tasind
027000161007     c                   eval      ARBCAD=t_tascad
027100161007     c                   eval      ARBLOD=t_taslod
027200161007     c                   eval      ARBPRD=t_tasprd
027300161007     c                   eval      ARBNZD=t_tasnzd
027400161007     c                   eval      ARBFIN=tasfin
027500161007     c                   eval      ARBCPI=t_tascpd
027600161007     c                   eval      ARBPKB=taspkb
027700161007     c                   eval      ARBPKF=taspkf
027800161007     c                   eval      ARBVLB=tasvlb
027900161007     c                   eval      ARBFVB=tasfvb
028000161007     c                   eval      ARBVLF=tasvlf
028100161007     c                   eval      ARBFVF=tasfvf
028200161007     c                   eval      ARBFFD=tasffd
028300161007     c                   endsr
028400161007      *------------------------------------------------------------------------*
028500161007     C     sr_arbk00     BEGSR
028600161007     C*------------------------------------------------------------------------*
028700161007     c                   if        t_tastbl<>tastbl
028701161007     c                   eval      arbaas=tasaas
028702161007     c                   eval      arblnp=taslnp
028703161007     c                   eval      arbnrs=tasnrs
028704161007     c                   eval      arbnsp=tasnsp
028705161007     c                   eval      arbcbo=%subst(tastbl:1:1)
028706161007     c                   eval      arbdtv=tasdtv
028707161007     c                   eval      arborv=tasorv
028708161007     c                   eval      arbpru=taspru
028709161007     c                   eval      %subst(arbpru:10:1)= 'S'
028710161007     c                   eval      arbcvb='KT'
028711161007     c                   eval      ARBCBP=tastbl
028712161007     c                   eval      ARBCBN=t_tastbl
028713161007     c                   write     fnarbk00
028714161007     c                   endif
028800161007     c                   endsr
