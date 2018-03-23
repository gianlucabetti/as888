000100011003     H DECEDIT('0,') DATEDIT(*DMY.)
000201161006     Fazute01l  if   E           k DISK
000202161006     Ftitasv1l  if   E           k DISK
000203161006     Ftita7v1l  if   E           k DISK
000204161006     Ftitas30c  if   E           k DISK    prefix(t_)
000205161007     Ftita730c  if   E           k DISK    prefix(t_)
000206161020     Ftitai01l  if   E           k DISK
000207161006     Ffiarbt0f  o  a E             DISK
000208161007     Ffiarbu0f  o  a E             DISK
000209161006     Ffnarbm0f  o  a E             DISK
000210161006     Ffnarbd0f  o  a E             DISK
000211161006     Ffnarbk0f  o  a E             DISK
001501161006
001502161024     D TIBS69DS      E DS
001503161024     d DS_cnaco      E DS                  extname(CNACO00F)
001504161024     d DS_cnind      E DS                  extname(CNIND00F)
001505161024     d DS_cnclp      E DS                  extname(CNCLP00F)
001506161024     d DS_fncls      E DS                  extname(FNCLS00F)
001507161020     d titasv_ds     e ds                  extname(titasv0f)
001508161020     d dstasv        e ds                  extname(titasv0f) qualified
001509161007     d dst_titas       ds
001510161007     D  svt                           1    DIM(21)                              SIGLA  VARIE
001511161007     D  sit                          11  3 DIM(20)                              VALORE VARIE
001512161007     d dst_titasv      ds
001513161007     D  svv                           1    DIM(21)                              SIGLA  VARIE
001514161007     D  siv                          11  3 DIM(20)                              VALORE VARIE
001515161007     d x               s              2  0
001516161020     d wtbl            s                   like(tastbl)
001600011003     C*------------------------------------------------------------------------*
001700011003     C* MAIN LINE
001800011003     C*------------------------------------------------------------------------*
004500011003     C*
004501161006     C     KEYTAS        KLIST                                                  TITAS30C
004502161020     C                   KFLD                    dstasv.tASAAS
004503161020     C                   KFLD                    dstasv.tASLNP
004504161020     C                   KFLD                    dstasv.tASNRS
004505161020     C                   KFLD                    dstasv.tASNSP
004506161020     C                   KFLD                    dstasv.tASTBL
004507161007     C     KEYTA7v       KLIST                                                  TITAS30C
004508161020     C                   KFLD                    dstasv.tASAAS
004509161020     C                   KFLD                    dstasv.tASLNP
004510161020     C                   KFLD                    dstasv.tASNRS
004511161020     C                   KFLD                    dstasv.tASNSP
004512161020     C                   KFLD                    dstasv.TASDTV
004513161020     C                   KFLD                    dstasv.tASORV
004514161020     C                   KFLD                    dstasv.tASPRU
004515161020     C     KEYTA7s       KLIST                                                  TITAS30C
004516161020     C                   KFLD                    tASAAS
004517161020     C                   KFLD                    tASLNP
004518161020     C                   KFLD                    tASNRS
004519161020     C                   KFLD                    tASNSP
004520161020     C                   KFLD                    TASDTV
004521161020     C                   KFLD                    tASORV
004522161020     C                   KFLD                    tASPRU
004523161006      /free
004524161020         clear dstasv;
004525161020         setll *loval titasv1l;
004526161020         read         titasv1l;
004527161020         dow not %eof(titasv1l);
004529161020         chain taspru azute01l;
004530161020            if not %found(azute01l);
004531161020      // Se non trovato profilo su azute lo considero di filiale clearando utefil
004532161020               clear utefil;
004533161020            endif;
004534161020      // Rottura key sped.
004535161020             if  tasaas<>dstasv.tasaas or
004536161020                 taslnp<>dstasv.taslnp or
004537161020                 tasnrs<>dstasv.tasnrs or
004538161020                 tasnsp<>dstasv.tasnsp      ;
004539161020                    if  dstasv.tasnsp>0;
004540161020      // Se profilo "ASS"  --> ha cambiato solo importo da assicurare
004541161020                     if %subst(dstasv.taspru:1:3)= 'ASS' ;
004542161020                       exsr sr_arbtias;
004543161020                     else;
004544161020                       exsr sr_arbt00;
004546161020                       exsr sr_arbm00;
004547161020                       exsr sr_arbk00;
004548161020                       if utefil=046;
004549161020      // variazione fatta da utente di sede
004550161020      // . . . Peso/volume
004551161020                          exsr sr_arbDI ;
004552161020                       endif;
004553161020                     endif;
004554161020                    endif;
004555161020               eval  wtbl=tastbl;
004556161020               eval  %subst(wtbl:2:1)=' ';
004557161020               setll (tasaas:taslnp:tasnrs:tasnsp:wtbl) titas30c ;
004558161020               reade (tasaas:taslnp:tasnrs:tasnsp) titas30c ;
004559161020               if %eof(titas30c)   ;
004560161020                  setgt (tasaas:taslnp:tasnrs:tasnsp) titasv1l;
004561161020                  read  titasv1l;
004562161025
004563161025                  clear dstasv.tasaas;
004564161025                  clear dstasv.taslnp;
004565161025                  clear dstasv.tasnrs;
004566161025                  clear dstasv.tasnsp;
004567161025
004568161020                  iter;
004569161020               endif;
004570161020      // Se spedizione stornata e il tipo bolla sulla variazione è uguale
004571161020      // ignoro le variazioni di questa spedizione
004572161020               if (t_tastbl='AS' or t_tastbl='F7') and
004573161020                  t_tastbl=tastbl;
004574161020                  setgt (tasaas:taslnp:tasnrs:tasnsp) titasv1l;
004575161020                  read  titasv1l;
004576161025
004577161025                  clear dstasv.tasaas;
004578161025                  clear dstasv.taslnp;
004579161025                  clear dstasv.tasnrs;
004580161025                  clear dstasv.tasnsp;
004581161025
004582161020                  iter;
004583161020               endif;
004584161020             else;
004585161020               if  dstasv.tasnsp>0;
004586161020      // confronto la variazione precedente con quella successiva
004587161020      // Se profilo "ASS"  --> ha cambiato solo importo da assicurare
004588161020                    if %subst(dstasv.taspru:1:3)= 'ASS' ;
004589161020                       exsr sr_arbtiasv;
004590161020                    else;
004591161020                       exsr sr_arbt00v;
004592161020      // Se profilo "ASS" --> ha cambiato solo importo da assicurare quindi non verifico
004593161020      // le seguenti routine
004594161020                     if %subst(taspru:1:3)<>'ASS';
004595161020                       exsr sr_arbm00v;
004596161020                       exsr sr_arbk00v;
004597161020                       if utefil=046;
004598161020      // variazione fatta da utente di sede
004599161020      // . . . Peso/volume
004600161020                          exsr sr_arbDIv ;
004601161020                       endif;
004602161020                     endif ;
004603161020                    endif  ;
004604161020               endif;
004605161020             endif;
004606161020               eval dstasv=titasv_ds;
004607161020            read         titasv1l;
004608161020         enddo;
004609161020                    if  dstasv.tasnsp>0;
004610161020                       exsr sr_arbt00;
004611161020                       exsr sr_arbm00;
004612161020                       exsr sr_arbk00;
004613161020                       if utefil=046;
004614161020      // variazione fatta da utente di sede
004615161020      // . . . Peso/volume
004616161020                          exsr sr_arbDI ;
004617161020                       endif;
004618161020                    endif;
013900161006         *inlr=*on;
013901161006      /end-free
013902161024     C                   MOVEL     'C'           I69TLA
013903161024     C                   CALL      'TIBS69R'
013904161024     C                   PARM                    TIBS69DS
013905161020      *------------------------------------------------------------------------*
013906161020     C     sr_arbtias    BEGSR
013907161020     c*------------------------------------------------------------------------*
013908161020     c* FIARBT0F - variazione di importo da assicurare
013909161020     c* Importo da assicurare
013910161020     c                   if        t_tasias<>dstasv.tasias or
013911161020     c                             (t_tasvas<>dstasv.tasvas and t_tasias>0)
013912161020     c                   clear                   fiarbt00
013913161020     c                   exsr      sr_ImpoArbt
013914161020     c                   write     fiarbt00
013915161020     c                   endif
013916161020     c                   endsr
013917161006      *------------------------------------------------------------------------*
013918161006     C     sr_arbt00     BEGSR
013919161020     c*------------------------------------------------------------------------*
013920161007     c* Estensione Tassazione
013921161007     c* Carico schiere varie per il confronto
013922161007     c                   clear                   svv
013923161007     c                   clear                   svt
013924161007     c                   clear                   siv
013925161007     c                   clear                   sit
013926161007     c* schiere da tita7V
013927161007     c                   z-add     1             x
013928161021     c* prima metto eventuali varie dal record principale
013929161021     c                   if        dstasv.tassv1<>*blanks
013930161021     c                   eval      svv(x)=dstasv.tassv1
013931161021     c                   eval      siv(x)=dstasv.tasva1
013932161021     c                   add       1             x
013933161021     c                   endif
013934161021     c                   if        dstasv.tassv2<>*blanks
013935161021     c                   eval      svv(x)=dstasv.tassv2
013936161021     c                   eval      siv(x)=dstasv.tasva2
013937161021     c                   add       1             x
013938161021     c                   endif
013939161021     c                   if        dstasv.tassv3<>*blanks
013940161021     c                   eval      svv(x)=dstasv.tassv3
013941161021     c                   eval      siv(x)=dstasv.tasva3
013942161021     c                   add       1             x
013943161021     c                   endif
013944161007     C     KEYTA7v       SETLL     TITA7v1l
013945161007     C     KEYTA7v       READE     TITA7v1l
013946161007     C                   DOW       not %eof(tita7v1l)
013947161007     c                   eval      svv(x)=ta7svn
013948161007     c                   eval      siv(x)=ta7van
013949161007     c                   add       1             x
013950161007     C     KEYTA7v       READE     TITA7v1l
013951161007     c                   enddo
013952161020     c* schiere da tita7 se fiv <> 996 a 998
013953161020     c                   if        t_tasfiv<> 996 and t_tasfiv<>998
013954161007     c                   z-add     1             x
013955161021     c* prima metto eventuali varie dal record principale
013956161021     c                   if        t_tassv1<>*blanks
013957161021     c                   eval      svt(x)=t_tassv1
013958161021     c                   eval      sit(x)=t_tasva1
013959161021     c                   add       1             x
013960161021     c                   endif
013961161021     c                   if        t_tassv2<>*blanks
013962161021     c                   eval      svt(x)=t_tassv2
013963161021     c                   eval      sit(x)=t_tasva2
013964161021     c                   add       1             x
013965161021     c                   endif
013966161021     c                   if        t_tassv3<>*blanks
013967161021     c                   eval      svt(x)=t_tassv3
013968161021     c                   eval      sit(x)=t_tasva3
013969161021     c                   add       1             x
013970161021     c                   endif
013971161007     C     KEYTAS        SETLL     TITA730c
013972161007     C     KEYTAS        READE     TITA730c
013973161007     C                   DOW       not %eof(tita730c)
013974161010     c                   eval      svt(x)=t_ta7svn
013975161010     c                   eval      sit(x)=t_ta7van
013976161007     c                   add       1             x
013977161007     C     KEYTAS        READE     TITA730c
013978161007     c                   enddo
013979161020     c                   else
013980161020     c* se fiv  996 o 998  la tassazione la devo prendere da titai
013981161021     c                   clear                   t_taspor
013982161021     c                   clear                   t_tasimv
013983161021     c                   z-add     0             x
013984161020     c     KEYTAS        setll     titai01l
013985161021     c     KEYTAS        reade     titai01l
013986161021     c                   dow       not %eof(titai01l)
013987161021      * imponibile
013988161021     c                   if        taisvt = '£'
013989161021     c                   eval      t_tasimv = taivat
013990161021     c                   endif
013991161021      * porto
013992161021     c                   if        taisvt = ' '
013993161021     c                   eval      t_taspor = taivat
013994161021     c                   endif
013995161021      * varie
013996161021     c                   if        taisvt <> ' ' and taisvt <> '£'
014006161021     c                   add       1             x
014007161021     c                   eval      svt(x) = taisvt
014008161021     c                   eval      sit(x) = taivat
014010161021     c                   endif
014011161021     c     keytas        reade     titai01l
014021161021     c                   enddo
014022161020     c                   endif
014023161006     c* FIARBT0F - variazioni di tassazione:
014100161006     c* Codice tariffa
014200161020     c                   if        t_tasctr<>dstasv.tasctr  or
014300161006     c* Importo da assicurare
014301161020     c***                          (utefil=046 and  (
014400161020     c***                          t_tasias<>dstasv.tasias or
014500161020     c***                          (t_tasvas<>dstasv.tasvas and t_tasias>0))) or
014600161006     c* Importi Tassazione
014700161021     c***                          t_tasdiv<>dstasv.tasdiv or
014800161020     c                             t_taspor<>dstasv.taspor or
014900161020     c                             t_tasimv<>dstasv.tasimv or
015000161020     c                             t_tasfei<>dstasv.tasfei or
015007161007     c***                          svv<>svt or siv<>sit  OR
015008161021     c                             dst_titas<>dst_titasv or
015200161006     c* Quantità fatturabile
015300161020     c                             t_tasqft<>dstasv.tasqft or
015400161006     c* Cliente se spedizione in assegnato
015500161020     c                             %subst(tastbl:1:1)='A' and
015501161020     c                             t_tasksc<>dstasv.tasksc
015502161007     c                   clear                   fiarbt00
015600161006     c                   exsr      sr_ImpoArbt
015700161006     c                   write     fiarbt00
015800161006     c                   endif
015900161006     c                   endsr
016000161006      *------------------------------------------------------------------------*
016100161006      * Valorizza campi specifici storico variazioni FIARBT0F + scrittura FIARBU
016200161006      *------------------------------------------------------------------------*
016300161006     C     sr_ImpoArbt   BEGSR
016400161006
016401161020     c                   eval      arbaas=dstasv.tasaas
016402161020     c                   eval      arblnp=dstasv.taslnp
016403161020     c                   eval      arbnrs=dstasv.tasnrs
016404161020     c                   eval      arbnsp=dstasv.tasnsp
016405161020     c                   eval      arbcbo=%subst(dstasv.tastbl:1:1)
016406161020     c                   eval      arbdtv=dstasv.tasdtv
016407161020     c                   eval      arborv=dstasv.tasorv
016408161020     c                   eval      arbpru=dstasv.taspru
016409161007     c                   eval      %subst(arbpru:10:1)= 'S'
016500161006     c* ARBTRC-I o II Bolla --> in sede non c'è questa distinzione
016600161006     c*                  metto il primo byte del tipo bolla che serve
016700161006     c*                  per collegare univocamente l'estensione tassazione alla sua
016800161006     c*                  bolla
016900161020     c                   eval      arbtrc=%subst(dstasv.tastbl:1:1)
017000161006     c* Causale Variazione Bolla
017100161006     c                   select
017101161020     c* utente ASS --> importo da assicurare
017102161020     c                   when      %subst(dstasv.taspru:1:3)= 'ASS'
017103161020     c                   eval      arbcvb='TI'
017200161006     c* Bolla Franco --> "TP"
017300161020     c                   when      %subst(dstasv.tastbl:1:1)='F'
017400161006     c                   eval      arbcvb='TP'
017800161006     c* Bolla in Assegnato F.I. --> "T2"
017900161006     c                   other
018000161020     c                   eval      arbcvb='T0'
018100161006     c                   endsl
018200161006
018300161020     c                   move      dstasv.tasksc arbksc
018400161020     c                   move      dstasv.tasctr arbctr
018500161020     c                   eval      ARBQFT=dstasv.tasqft
018800161020     c                   eval      ARBIAS=dstasv.tasias
018900161020     c                   eval      ARBVAS=dstasv.tasvas
019000161020     c                   eval      ARBDIV=dstasv.tasdiv
019100161020     c                   eval      ARBPOR=dstasv.taspor
019200161020     c                   eval      ARBSV1=dstasv.tassv1
019300161020     c                   eval      ARBVA1=dstasv.tasva1
019400161020     c                   eval      ARBSV2=dstasv.tassv2
019500161020     c                   eval      ARBVA2=dstasv.tasva2
019600161020     c                   eval      ARBSV3=dstasv.tassv3
019700161020     c                   eval      ARBVA3=dstasv.tasva3
019800161020     c                   eval      ARBIMV=dstasv.tasIMV
019900161006     c***                eval      ARBFIM=tasFIM
020000161006     c***                eval      ARBALI=tasALI
020100161020     c                   eval      ARBCEI=dstasv.tasFEI
020200161006     c****               eval      ARBIFT=tasIFT
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
021700161020     c                   if        t_tasccm<>dstasv.tasccm
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
022601161020     c                   eval      arbaas=dstasv.tasaas
022602161020     c                   eval      arblnp=dstasv.taslnp
022603161020     c                   eval      arbnrs=dstasv.tasnrs
022604161020     c                   eval      arbnsp=dstasv.tasnsp
022605161020     c                   eval      arbcbo=%subst(dstasv.tastbl:1:1)
022606161020     c                   eval      arbdtv=dstasv.tasdtv
022607161020     c                   eval      arborv=dstasv.tasorv
022608161020     c                   eval      arbpru=dstasv.taspru
022609161020     c                   eval      %subst(arbpru:10:1)= 'S'
022700161007     c* Causale Variazione Bolla
022800161007     c                   eval      arbcvb='MI'
022900161020     c                   eval      ARBCCM=dstasv.tasccm
023000161020     c                   eval      ARBFAP=dstasv.tasfap
023600161020     c                   eval      ARBrsm=dstasv.tasrsc
023700161020     c                   eval      ARBINM=dstasv.TASINM
023800161025     c                   eval      ARBCAM=dstasv.TASCAM
023900161020     c                   eval      ARBLOM=dstasv.TASLOC
024000161020     c                   eval      ARBPRM=dstasv.TASPRV
024100161025     c                   eval      ARBNZM=dstasv.TASNZM
024200161020     c                   eval      ARBcpi=dstasv.taspiv
024201161024     c                   if        arbrsm=*blanks
024202161024      *    se codificato aggancio anagrafico
024203161024     C                   clear                   TIBS69DS
024204161024     c                   z-add     arbccm        I69kac
024205161024     c                   z-add     arbccm        I69kin
024206161024     c                   z-add     arbccm        I69kcs
024207161024     c                   exsr      CTR_anagra
024208161024     c                   eval      ARBrsm=acorag
024209161024     c                   eval      ARBINM=INDVIA
024210161025     c****               eval      ARBCAM=INDCae
024211161024     c                   eval      ARBLOM=INDCit
024212161024     c                   eval      ARBPRM=INDPRV
024213161025     c***                eval      ARBNZM=INDSTA
024214161024     c                   eval      ARBcpi=indiva
024215161024     c                   endif
025900161007     c                   endsr
026000161007      *------------------------------------------------------------------------*
026100161007     C     sr_arbDi      BEGSR
026200161007     C*------------------------------------------------------------------------*
026201161020     c                   if        t_taspkf<>dstasv.taspkf
026202161007     c                   clear                   fnarbd00
026211161007     c                   eval      arbcvb='IS'
026212161007     c                   exsr      sr_ImpoArbD
026213161007     c                   write     fnarbd00
026214161007     c                   endif
026215161020     c                   if        t_tasvlf<>dstasv.tasvlf
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
026701161020     c                   eval      arbaas=dstasv.tasaas
026702161020     c                   eval      arblnp=dstasv.taslnp
026703161020     c                   eval      arbnrs=dstasv.tasnrs
026704161020     c                   eval      arbnsp=dstasv.tasnsp
026705161020     c                   eval      arbcbo=%subst(dstasv.tastbl:1:1)
026706161020     c                   eval      arbdtv=dstasv.tasdtv
026707161020     c                   eval      arborv=dstasv.tasorv
026708161020     c                   eval      arbpru=dstasv.taspru
026709161007     c                   eval      %subst(arbpru:10:1)= 'S'
026800161007     c                   eval      ARBRSD=t_tasrsd
026900161007     c                   eval      ARBIND=t_tasind
027000161007     c                   eval      ARBCAD=t_tascad
027100161007     c                   eval      ARBLOD=t_taslod
027200161007     c                   eval      ARBPRD=t_tasprd
027300161007     c                   eval      ARBNZD=t_tasnzd
027400161020     c                   eval      ARBFIN=dstasv.tasfin
027500161007     c                   eval      ARBCPI=t_tascpd
027600161020     c                   eval      ARBPKB=dstasv.taspkb
027700161020     c                   eval      ARBPKF=dstasv.taspkf
027800161020     c                   eval      ARBVLB=dstasv.tasvlb
027900161020     c                   eval      ARBFVB=dstasv.tasfvb
028000161020     c                   eval      ARBVLF=dstasv.tasvlf
028100161020     c                   eval      ARBFVF=dstasv.tasfvf
028200161020     c                   eval      ARBFFD=dstasv.tasffd
028300161007     c                   endsr
028400161007      *------------------------------------------------------------------------*
028500161007     C     sr_arbk00     BEGSR
028600161007     C*------------------------------------------------------------------------*
028700161020     c                   if        t_tastbl<>dstasv.tastbl
028701161020     c                   eval      arbaas=dstasv.tasaas
028702161020     c                   eval      arblnp=dstasv.taslnp
028703161020     c                   eval      arbnrs=dstasv.tasnrs
028704161020     c                   eval      arbnsp=dstasv.tasnsp
028705161025     c                   eval      arbcbo=%subst(t_tastbl:1:1)
028706161020     c                   eval      arbdtv=dstasv.tasdtv
028707161020     c                   eval      arborv=dstasv.tasorv
028708161020     c                   eval      arbpru=dstasv.taspru
028709161020     c                   eval      %subst(arbpru:10:1)= 'S'
028710161007     c                   eval      arbcvb='KT'
028711161020     c                   eval      ARBCBP=dstasv.tastbl
028712161007     c                   eval      ARBCBN=t_tastbl
028713161007     c                   write     fnarbk00
028714161007     c                   endif
028800161007     c                   endsr
028801161020      *------------------------------------------------------------------------*
028802161020     C     sr_arbtiasv   BEGSR
028803161020     c*------------------------------------------------------------------------*
028804161020     c* FIARBT0F - variazione di importo da assicurare
028805161020     c* Importo da assicurare
028806161020     c                   if        tasias<>dstasv.tasias or
028807161020     c                             (tasvas<>dstasv.tasvas and tasias>0)
028808161020     c                   clear                   fiarbt00
028809161020     c                   exsr      sr_ImpoArbt
028810161020     c                   write     fiarbt00
028811161020     c                   endif
028812161020     c                   endsr
028923161020      *------------------------------------------------------------------------*
029000161020     C     sr_arbt00v    BEGSR
029100161020     ù*------------------------------------------------------------------------*
029200161020     c* Estensione Tassazione
029300161020     c* Carico schiere varie per il confronto
029400161020     c                   clear                   svv
029500161020     c                   clear                   svt
029600161020     c                   clear                   siv
029700161020     c                   clear                   sit
029800161020     c* schiere da tita7V precedente
029900161020     c                   z-add     1             x
030000161020     C     KEYTA7v       SETLL     TITA7v1l
030100161020     C     KEYTA7v       READE     TITA7v1l
030200161020     C                   DOW       not %eof(tita7v1l)
030300161020     c                   eval      svv(x)=ta7svn
030400161020     c                   eval      siv(x)=ta7van
030500161020     c                   add       1             x
030600161020     C     KEYTA7v       READE     TITA7v1l
030700161020     c                   enddo
030800161020     c* schiere da tita7v successivo
030900161020     c                   z-add     1             x
031000161020     C     KEYTA7s       SETLL     TITA7v1l
031100161020     C     KEYTA7s       READE     TITA7v1l
031200161020     C                   DOW       not %eof(tita7v1l)
031300161025     c                   eval      svt(x)=ta7svn
031400161025     c                   eval      sit(x)=ta7van
031500161020     c                   add       1             x
031600161020     C     KEYTA7s       READE     TITA7v1l
031700161020     c                   enddo
031800161020     c* FIARBT0F - variazioni di tassazione:
031900161020     c* Codice tariffa
032000161020     c                   if        tasctr<>dstasv.tasctr  or
032100161020     c* Importo da assicurare
032200161020     c***                          (utefil=046 and  (
032300161020     c***                          tasias<>dstasv.tasias or
032400161020     c***                          (tasvas<>dstasv.tasvas and tasias>0))) or
032500161020     c* Importi Tassazione
032600161021     c****                         tasdiv<>dstasv.tasdiv or
032700161020     c                             taspor<>dstasv.taspor or
032800161020     c                             tasimv<>dstasv.tasimv or
032900161020     c                             tasfei<>dstasv.tasfei or
033000161020     c                             TASSV1<>dstasv.TASSV1 or
033100161020     c                             TASVA1<>dstasv.TASVA1 or
033200161020     c                             TASSV2<>dstasv.TASSV2 or
033300161020     c                             TASVA2<>dstasv.TASVA2 or
033400161020     c                             TASSV3<>dstasv.TASSV3 or
033500161020     c                             TASVA3<>dstasv.TASVA3 or
033600161020     c***                          svv<>svt or siv<>sit  OR
033700161020     c                             dst_titas<>dst_titasv OR
033800161020     c* Quantità fatturabile
033900161020     c                             tasqft<>dstasv.tasqft or
034000161020     c* Cliente se spedizione in assegnato
034100161020     c                             %subst(tastbl:1:1)='A' and
034200161020     c                             tasksc<>dstasv.tasksc
034300161020     c                   clear                   fiarbt00
034400161020     c                   exsr      sr_ImpoArbt
034500161020     c                   write     fiarbt00
034600161020     c                   endif
034700161020     c                   endsr
041700161020      *------------------------------------------------------------------------*
041800161020     C     sr_arbm00v    BEGSR
041900161020     c*------------------------------------------------------------------------*
042000161020     c* FNARBM0F - variazioni di mittente:
042100161020     c                   if        tasccm<>dstasv.tasccm
042200161020     c                   clear                   fnarbm00
042300161020     c                   exsr      sr_ImpoArbm
042400161020     c                   write     fnarbm00
042500161020     c                   endif
042600161020     c                   endsr
045300161020      *------------------------------------------------------------------------*
045400161020     C     sr_arbDiv     BEGSR
045500161020     C*------------------------------------------------------------------------*
045600161020     c                   if        taspkf<>dstasv.taspkf
045700161020     c                   clear                   fnarbd00
045800161020     c                   eval      arbcvb='IS'
045900161020     c                   exsr      sr_ImpoArbD
046000161020     c                   write     fnarbd00
046100161020     c                   endif
046200161020     c                   if        tasvlf<>dstasv.tasvlf
046300161020     c                   clear                   fnarbd00
046400161020     c                   eval      arbcvb='I2'
046500161020     c                   exsr      sr_ImpoArbD
046600161020     c                   write     fnarbd00
046700161020     c                   endif
046800161020     c                   endsr
049800161020      *------------------------------------------------------------------------*
049900161020     C     sr_arbk00v    BEGSR
050000161020     C*------------------------------------------------------------------------*
050100161020     c                   if        tastbl<>dstasv.tastbl
050200161020     c                   eval      arbaas=dstasv.tasaas
050300161020     c                   eval      arblnp=dstasv.taslnp
050400161020     c                   eval      arbnrs=dstasv.tasnrs
050500161020     c                   eval      arbnsp=dstasv.tasnsp
050600161025     c                   eval      arbcbo=%subst(tastbl:1:1)
050700161020     c                   eval      arbdtv=dstasv.tasdtv
050800161020     c                   eval      arborv=dstasv.tasorv
050900161020     c                   eval      arbpru=dstasv.taspru
051000161020     c                   eval      %subst(arbpru:10:1)= 'S'
051100161020     c                   eval      arbcvb='KT'
051200161020     c                   eval      ARBCBP=dstasv.tastbl
051300161025     c                   eval      ARBCBN=tastbl
051400161020     c                   write     fnarbk00
051500161020     c                   endif
051600161020     c                   endsr
051700161024      *****************************************************************
051800161024      *  AGGANCIO ANAGRAFICHE
051900161024      *****************************************************************
052000161024     C     CTR_ANAGRA    BEGSR
052100161024      *
052200161024     C                   clear                   DS_cnaco
052300161024     C                   clear                   DS_cnind
052400161024     C                   clear                   DS_cnclp
052500161024     C                   clear                   DS_fncls
052600161024      *
052700161024     C                   CALL      'TIBS69R'
052800161024     C                   PARM                    tibs69DS
052900161024     C                   PARM                    DS_cnaco
053000161024     C                   PARM                    DS_cnind
053100161024     C                   PARM                    DS_cnclp
053200161024     C                   PARM                    DS_fncls
053500161024      *
053600161024     C                   ENDSR
