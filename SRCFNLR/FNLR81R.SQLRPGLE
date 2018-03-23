000100170201     H DECEDIT('0,') DATEDIT(*DMY.)
000200161205     H DFTACTGRP(*NO) ACTGRP(*NEW)
000300000000      *****************************************************************
000400000000      *
000500040331      *  Descrizione   :  Richiesta creazione file dati distinta
000600040331      *                   x scarico su Pc.
000700000000      *****************************************************************
000800040407     Ffnlr81d   cf   e             workstn
000900040331     Fazorg01l  if   E           k disk
001000040331     Ffnarb70l  if   E           k disk
001100040407     Ffnarb01l  if   E           k disk    rename(fnarb000:fnarb1)
001200040401     Ffnfvv01l  if   E           k disk
001300040401     Ffnfvv08l  if   E           k disk    rename(fnfvv000:fnfvv8)
001400040408     Ffnlbl01l  if   E           k disk
001500120607     Ffidst01l  if   E           k disk
001600051115     Ffiar901l  if   E           k disk
001700040408     Ffiar601l  if   E           k disk
001800040408     Ffiari01l  if   E           k disk
001900040402     Ffnart01l  if   E           k disk
002000170127     Fwfdsc01l  if   e           k disk
002100170127     Ftivgd00f  o    e             disk    commit
002200170201     F                                     usropn
002300060407     fTntbe01l  if   e           k disk
002400040402     D nsc             s              7  0 dim(10)
002500040331      *
002600060606     D err             s             70    dim(13) ctdata perrcd(1)
002700040331      *
002800161205     D psds           sds
002900161205     D  procname         *PROC
003000040331     D kpjba         e ds
003100060407     D WFDSCDS       e ds                  extname(wfdsc00f)
003200060410     D DDSC          e ds
003300170127     D TIS7VASDS     e ds
003400940926     D*
003500940926     D* DS PER TRUL06R - CARICAMENTO £1
003600040331     D trul06ds      e ds
003700040331     D  lin                    1     90  0
003800040331     D                                     dim(30)                              SKI COMODO
003900040407     d Tibs36Ds      e ds
004000040407     D DSLR01        E DS
004100040407     D*
004200060426     D wlbdat          ds
004300040331     D  g02dat                 1      8  0
004400040331     D  g02inv                 9     16  0
004500040331     D  g02err                17     17
004600040331     D  g02tgi                18     22  0
004700040407     d Wdata8          DS
004800040407     d  dadata                 1      8  0
004900040407     d  adata                  9     16  0
005000040407     d  GioLav                17     21  0
005100060426      *
005200060426      * Passaggio Parametri al pgm TIBS02R
005300060426     d TIBS02ds      e ds                  inz
005400060426     d  T02tla       e                     inz('L')
005500060426     d  T02mod       e                     inz('R')
005600060426     d  T02cod       e                     inz('DSC')
005700060504      *
005800060504      * DS REPERIMENTO DATI UTENTE
005900060504     D BS69DS        E DS                  EXTNAME(TIBS69DS)
006000060504     D ACODS         E DS                  EXTNAME(CNACO00F)
006100060504     D INDDS         E DS                  EXTNAME(CNIND00F)
006200060504     D CLPDS         E DS                  EXTNAME(CNCLP00F)
006300060504     D CLSDS         E DS                  EXTNAME(FNCLS00F)
006400060504      *
006500060504      * DS REPERIMENTO DATI UTENTE
006600060504     D TIBS34DS      E DS                                                       *Profili utente
006700060504     D DDATIUTE      E DS                                                       *Dati utente
006800060504     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006900040407      *
007000040402     d dataiso         s               d   datfmt(*iso)
007100040407     d dataisop        s               d   datfmt(*iso)
007200040407     D savkpjbu        s                   like(kpjbu)
007300060407     D wOGGI           s              8  0
007400120607     D knfv6           s              6  0
007500060407     D wFlgEXE         s              1    inz
007600000000      *
007700040331      *--------------------------------------------------------------*
007800040331      *             M A I N      L I N E
007900040331      *--------------------------------------------------------------*
008000000000      * Inizializzo i campi a video
008100040331     C                   z-add     0             vidpdc                         DATA FINALE
008200040331     C                   Z-ADD     0             viddat                         1' COD.PADRONC.
008300040407     C                   move      *blank        vidndc                         2' COD.PADRONC.
008400060508     C                   move      *blanks       vidksc                         CLIENTE UNIFIC.
008500000000      *
008600000000      * Emetto il formato video
008700040331     c                   do        *hival
008800040331     C     emetto        tag
008900040331     C                   setoff                                       90
009000040331     C                   exfmt     video1
009100040331     c                   clear                   $msg
009200040405     C                   setoff                                       504028
009300000000      *
009400040331     C   kc              goto      fine                                         F3=USCITA
009500040331      * Richiamo programma di stampa
009600040407     c                   if        o36pou = wo36tfp
009700040331     c                   if        *inks
009800040331     c                   seton                                        50
009900040331     c                   iter
010000040331     c                   endif
010100050325     c*                  else
010200050325     c*                  seton                                        409028
010300050325     c*                  movel     err(7)        $msg
010400050325     c*                  goto      emetto
010500040331     c                   endif
010600000000      *
010700000000      * Eseguo i  controlli sui campi immessi a video
010800040331     C                   exsr      contr
010900000000      *
011000020308      * Se non vi sono errori ma non e' premuto F6 emetto video
011100040331     C  n90
011200040331     Cannkf              seton                                          90      F6=CONFERMA
011300000000      *
011400000000      * Per errore generico riemetto il formato video
011500040331     C   90              goto      emetto
011600000000      *
011700020307      * Richiamo programma di stampa
011800020308     c                   if        *inkf
011900040331     c                   exsr      elabora
012000040331      * dopo elaborazione inizializzo campi a video e riemetto il video
012100060407     C                   if        wFlgEXE = 'S'
012200040331     C                   z-add     0             VIDpdc                         DATA FINALE
012300040331     C                   Z-ADD     0             viddat                         1' COD.PADRONC.
012400040407     C                   move      *blank        vidndc                         2' COD.PADRONC.
012500040407     C                   move      *zeros        vidndcn                        2' COD.PADRONC.
012600060508     C                   move      *blanks       vidksc                         CLIENTE UNIFIC.
012700060407     c                   endif
012800040331     c                   iter
012900020308     c                   endif
013000000000      *
013100040331     c                   enddo
013200040331     C     fine          tag
013300060407     C*
013400040331     C                   seton                                        lr
013500000000      *--------------------------------------------------------------*
013600000000      * Routine per i controlli sui campi del formato video          *
013700000000      *--------------------------------------------------------------*
013800040331     C     contr         begsr
013900000000      *
014000060606     c                   movea     '00000'       *in(40)
014100060504      *
014200000000      * Verifico il codice filiale
014300040331     C                   move      *blank        desfil
014400040331     C     vidfil        ifgt      0
014500040331     C     vidfil        chain     azorg01l                           89
014600040331     c                   if        *in89
014700040405     C                   SETON                                        904028
014800040331     c                   movel     err(1)        $msg
014900040331     C                   goto      endctl
015000040331     c                   else
015100040331     C                   movel     orgdes        desfil
015200040331     c                   endif
015300000000      *
015400000000      * IN SEDE QUALSIASI FILIALE, IN FIL SOLO LE MIE
015500091014     ***C     Wo36tfp       ifne      0
015600091014     c                   if             Wo36tfp <> *zeros
015700091014     c                             and  %subst(kNmUs:1:3) <> 'EDP'
015800040331     C     vidfil        lookup    Lin                                    99
015900040405     C  n99              seton                                        904028
016000040331     c  n99              movel     err(2)        $msg
016100040331     C  n99              goto      endctl
016200040331     C                   endif
016300020308     c                   else
016400040405     C                   seton                                        904028
016500040331     c                   movel     err(3)        $msg
016600040331     C                   goto      endctl
016700040331     C                   endif
016800000000      *
016900000000      * Controlli di congruenza
017000040331      * Se immesso il cd.autotras. non può essere inserito il numero dist.
017100040407     C     vidndc        ifne      *zero
017200040407     C     vidndc        andne     *blank
017300040407     C     vidpdc        andgt     *zero
017400040331     C                   seton                                        904142
017500040405     C                   seton                                        28
017600040331     c                   movel     err(4)        $msg
017700040331     C                   goto      endctl
017800040331     C                   endif
017900040331      * immettere o distinta o autotrasportatore
018000040331     C     vidndc        ifeq      *zero
018100040331     C     vidpdc        andeq     *zero
018200040407     C     vidndc        oreq      *blank
018300040407     C     vidpdc        andeq     *zero
018400040331     C                   seton                                        904142
018500040405     C                   seton                                        28
018600040331     c                   movel     err(4)        $msg
018700040331     C                   goto      endctl
018800040331     C                   endif
018900040331     C     vidpdc        ifgt      *zero
019000040331     C     viddat        andeq     *zero
019100040331     C                   seton                                        904243
019200040405     C                   seton                                        28
019300040331     c                   movel     err(5)        $msg
019400040331     C                   goto      endctl
019500040331     C                   endif
019600000000      *
019700000000      * Controllo data iniziale
019800040331     C     viddat        ifgt      0
019900040331     C                   z-add     viddat        g02dat
020000040331     C                   movel     *blank        g02err
020100040331     C                   call      'XSRDA8'
020200040331     C                   parm                    wlbdat
020300040331     C     g02err        ifeq      '1'
020400040405     C                   seton                                        439028
020500040331     c                   movel     err(6)        $msg
020600040331     C                   goto      endctl
020700040331     C                   endif
020800040331     C                   z-add     g02inv        viddatg           8 0
020900040331     C                   z-add     g02dat        viddat
021000020307     c                   endif
021100000000      *
021200040401      * Controlli di congruenza
021300040407     C*  Controllo il numero distinta
021400040407     C     vidndc        ifne      *zeros
021500040407     C     vidndc        andne     *blank
021600040407     C     '?'           scan      vidndc                                 86
021700040407     C     *in86         ifeq      '1'
021800040407     C                   clear                   dslr01
021900040407     C                   movel     'FNLR81R'     dlrpgm
022000040407     C                   movel     '2'           dlrtfv
022100040407     C                   z-add     4             dlrnpg
022200040407     C                   z-add     vidfil        dlrfgs
022300040407     C                   movel     'S'           dlrric
022400040407     C                   z-add     wdatg         dlrada
022500040407     C                   movel(p)  dslr01        kpjbu
022600040407     C                   call      'FNLR02R'
022700040407     C                   parm                    kpjba
022800040407     C                   movel     kpjbu         dslr01
022900040407     C                   move      dlrnfv        vidndc
023000040407     C                   endif
023100040407     C                   endif
023200040407     C*
023300040407     C                   testn                   vidndc               86
023400180110     C                   move      *zeros        vidndcn
023500040407     C   86              move      vidndc        vidndcn           7 0
023600180110     c                   if        vidndcn=0 and vidndc<>*blanks and
023700180110     c                             vidndc<>*zeros
023800180110     C                   seton                                        904128
023900180110     c                   movel     err(8)        $msg
024000180110     C                   goto      endctl
024100180110     c                   endif
024200180110     c
024300040401      * Se immesso il Nr distinta verifica che sia disponibile
024400040407     C     vidndcn       ifgt      *zero
024500040407     c                   z-add     vidndcn       dist5
024600040401     c     kfvv          chain     fnfvv01l                           88
024700140819     c***                if        *in88 or fvvfcf <> *blank or fvvatb <> *blank
024800140819     c                   if        *in88 or fvvatb <> *blank
024900040405     C                   seton                                        904128
025000060426     c                   movel     err(8)        $msg
025100040401     C                   goto      endctl
025200040401     C                   endif
025300040401     C                   endif
025400060426     C*
025500060426     C* Verifico se richiesto invio automatico a cliente
025600060606     C                   movel     *blanks       DESKSU
025700060504     C                   eval      T02mod = 'C'
025800060508     C     '?'           scan      VIDKSC                                 10
025900060426     C                   if        *in10
026000060504     C                   eval      T02mod = 'R'
026100060504     C                   endif
026200060504     C*
026300060508     C                   if        *IN10 or VIDKSC > *zeros
026400060508     C  N10              eval      T02KE1 = VIDKSC
026500060504     C   10              eval      T02KE1 = *blanks
026600060504     C                   call      'TIBS02R'
026700060426     C                   parm                    KPJBA
026800060426     C                   parm                    TIBS02ds
026900060426     C                   if        T02err  = *blanks
027000060426     C                   movel     T02uni        DDSC
027100060504     C* X utente nn EDP* verifico che il P.O. utente sia nella schiera d P.O. abilitati x il
027200060504     C* cliente scelto da tabella DSC
027300060504     C                   if        %subst(%trim(KNMUS):1:3) <> 'EDP'
027400060504     C                   if        DUTPOU = §DSCPO001 OR
027500060504     C                             DUTPOU = §DSCPO002 OR
027600060504     C                             DUTPOU = §DSCPO003 OR
027700060504     C                             DUTPOU = §DSCPO004 OR
027800060504     C                             DUTPOU = §DSCPO005 OR
027900060504     C                             DUTPOU = §DSCPO006 OR
028000060504     C                             DUTPOU = §DSCPO007 OR
028100060504     C                             DUTPOU = §DSCPO008 OR
028200060504     C                             DUTPOU = §DSCPO009 OR
028300060504     C                             DUTPOU = §DSCPO010
028400060504     C                   else
028500060504     C                   seton                                        449028
028600060504     C                   movel     err(11)       $msg
028700060606     C                   goto      endctl
028800060504     C                   endif
028900060504     C                   endif
029000060504     C*
029100060508     C                   movel     T02KE1        VIDKSC
029200060508     C                   movel     §DSCKSU       VIHKSU
029300060504     C                   eval      VIHTSC = §DSCTSC
029400060504     C* Decodifico la ragione sociale del cliente
029500060504     C                   exsr      DECTIB
029600120320     C***                if        O69ERR = '1'
029700120320     C***                movel     *all'*'       ACORAG
029800120320     C***                seton                                        449028
029900120320     C***                movel     err(12)       $msg
030000120320     C***                goto      endctl
030100120320     C***                endif
030200120320     C                   movel     ACORAG        DESKSU
030300060504     C*
030400060426     C                   else
030500060504     C                   seton                                        449028
030600060426     C                   movel     err(10)       $msg
030700060508     C                   movel     *all'0'       VIDKSC
030800060426     C                   goto      endctl
030900060426     C                   endif
031000060606     C                   else
031100060606     C                   seton                                        449028
031200060606     C                   movel     err(13)       $msg
031300060606     C                   goto      endctl
031400060504     C                   endif
031500040401      *
031600040331     C     endctl        endsr
031700060504     C*------------------------------------------------------------------------*
031800060504     C* DECTIB - CONTROLLO/DECODIFICA CLIENTE
031900060504     C*------------------------------------------------------------------------*
032000060504     C     DECTIB        BEGSR
032100060504     C*
032200060504     C                   CLEAR                   BS69DS
032300060504     C                   CLEAR                   ACODS
032400060504     C                   MOVEL     KNSIF         I69SIF
032500060508     C                   MOVE(P)   VIDKSC        I69KAC
032600060504     C                   CALL      'TIBS69R'
032700060504     C                   PARM                    BS69DS
032800060504     C                   PARM                    ACODS
032900060504     C                   PARM                    INDDS
033000060504     C                   PARM                    CLPDS
033100060504     C                   PARM                    CLSDS
033200060504     C*
033300060504     C                   ENDSR
033400040331      *---------------------------------------------------------------*
033500040331      * Elaborazione dati distinta e scrittura su file                *
033600040331      *---------------------------------------------------------------*
033700040331     C     elabora       begsr
033800161205     C*
033900161205     C* Inizializzo variabili d wrk
034000161205     C                   movel     'N'           wProcedi          1
034100060407     C*
034200060407     C* Inizializzo il flag che indica l'avvenuta effettiva elaborazione
034300060407     C                   movel     'S'           wFlgEXE
034400060407      *
034500040401      * esegue pulizia dati per il P.O da elaborare fino a 2 giorni
034600040401      * precedenti la data del giorno
034700040401     c                   exsr      pulisci
034800161205     C*
034900161205     C* Se scelto nel lancio invio automatico tramite BartVAS
035000161205     C                   if        VIDKSC  > *zeros
035100170201     C*
035200170201     C* Avvio il controllo sincronia
035300170201     C                   CLEAR                   TIS7VASDS
035400170201     C                   EVAL      i§7VASOPZ = 'SCM'
035500170201     C                   CALL(e)   'TIS7VASR1'
035600170201     C                   PARM                    TIS7VASDS
035700170201     C*
035800170201     C* Verifico esito chiamata
035900170201     C                   if        %error OR o§7VASOK = *off
036000170201     C                   exsr      EXEERR
036100170201     C                   endif
036200170201     C*
036300170201     C* Se tutto OK apro il file download
036400170201     C                   if        not %open(tivgd00f)
036500170201     C                   open      tivgd00f
036600170201     C                   endif
036700170201     C*
036800170201     C* Inizializzo la transazione
036900170201     C                   CLEAR                   TIS7VASDS
037000170127     C                   EVAL      i§7VASOPZ = 'PRG'
037100170201     C                   CALL(e)   'TIS7VASR1'
037200170127     C                   PARM                    TIS7VASDS
037300161205     C*
037400161205     C* Verifico esito chiamata
037500170127     C                   if        %error OR o§7VASOK = *off
037600161205     C                   exsr      EXEERR
037700161205     C                   endif
037800161205     C*
037900161205     C* Se OK => proseguo
038000170127     C                   if        o§7VASOK = *on AND o§7VASPRG <> *blanks
038100161205     C                   movel     'S'           wProcedi
038200161205     C                   endif
038300161205     C                   endif
038400161205     C*
038500161205     C* Se ok a procedere => elaboro
038600161205     C                   if        wProcedi = 'S'
038700040331      *
038800040401     c                   z-add     vidfil        kifp
038900040401      *elaborazione per singola distinta
039000040407     c                   if        vidndcn > 0
039100040407     c                   z-add     vidndcn       dist7
039200040402     c                   exsr      estrai
039300040401     c                   else
039400040401      *elaborazione per autotrasportatore / data
039500040401     c                   movel     vidfil        comodo7           7 0
039600040401     c                   move      vidpdc        comodo7
039700040401     c     kfvv8         setll     fnfvv08l
039800040401     c                   do        *hival
039900040401     c     kfvv8         reade     fnfvv08l                               87
040000040401     c   87              leave
040100120607     c* novo file FIDST con npg in key e foglio lungo 6
040200120607     c                   eval      knfv6=fvvnfv
040300120607     c     kdst          chain     fidst01l
040400120607     c
040500120607     c                   if        %found(fidst01l) and
040600120607     c                             not *in87 and dstpdr = comodo7 and
040700040401     c                             fvvfcf = *blank and fvvatb = *blank
040800040401     c                   z-add     fvvnfv        dist7
040900040401     c                   exsr      estrai
041000040401     c                   endif
041100040401     c                   enddo
041200040401     c                   endif
041300070305     C*
041400060407     C                   else
041500060407     C                   movel     'N'           wFlgEXE
041600060407     C                   seton                                        90
041700060407     c                   movel     err(9)        $msg
041800060407     C                   endif
041900161205     C*
042000170201     C* Se scelto nel lancio invio automatico tramite BartVAS
042100161205     C                   if        VIDKSC  > *zeros
042200170201     C*
042300170201     C* Chiudo il file download
042400170201     C                   if        %open(tivgd00f)
042500170201     C                   close     tivgd00f
042600170201     C*
042700170201     C* Finalizzo la transazione
042800170201     C                   EVAL      i§7VASOPZ  = 'RLS'
042900170201     C                   EVAL      i§7VASCMTE = '1'
043000170201     C                   EVAL      i§7VASTIP  = 'DC'
043100170201     C                   EVAL      i§7VASKSU  = '0'+VIHKSU
043200170201     C                   EVAL      i§7VASTSC  = VIHTSC
043300170201     C                   EVAL      i§7VASSTO  = '?'
043400170227     C                   EVAL      i§7VASSTTO = 'G'
043500170201     C                   EVAL      i§7VASPRG  = o§7VASPRG
043600170201     C                   CALL(e)   'TIS7VASR1'
043700170127     C                   PARM                    TIS7VASDS
043800161205     C*
043900161205     C* Verifico esito chiamata
044000170127     C                   if        %error OR o§7VASOK = *off
044100161205     C                   exsr      EXEERR
044200161205     C                   endif
044300170201     C*
044400170201     C* Fine controllo sincronia
044500170201     C                   EVAL      i§7VASOPZ = 'ECM'
044600170201     C                   CALL(e)   'TIS7VASR1'
044700170201     C                   PARM                    TIS7VASDS
044800170201     C*
044900170201     C* Verifico esito chiamata
045000170201     C                   if        %error OR o§7VASOK = *off
045100170201     C                   exsr      EXEERR
045200170201     C                   endif
045300170201     C                   endif
045400170201     C*
045500161205     C                   endif
045600170201     C*
045700040331     C                   endsr
045800040401      *---------------------------------------------------------------*
045900040401      * estrazione rekord da arb eventuale eliminazione rekord già esistenti
046000040401      *---------------------------------------------------------------*
046100040401     C     estrai        begsr
046200040401      *
046300040401      * verifica se distinta già presente la cancella per riscriverla
046400040401     c     kwfdsc        chain     wfdsc01l                           95
046500040401     c                   if        not *in95
046600040402     C/EXEC SQL
046700040402     C+ DELETE FROM WFDSC00F WHERE dscndc = :dist7 and dscfgs = :vidfil
046800040402     C/END-EXEC
046900040401      *
047000040401     c                   endif
047100040401
047200040401     c     karb          setll     fnarb70l
047300040401     c                   do        *hival
047400040401     c     karb          reade     fnarb70l                               94
047500040401     c   94              leave
047600040401      *
047700040402     c                   clear                   wfdsc000
047800060606     c                   clear                   WFDSCDS
047900040402
048000040408     c                   z-add     vidfil        dscfgs
048100040402     c                   z-add     arbaas        dscaas
048200040402     c                   z-add     arblnp        dsclnp
048300040402     c                   z-add     arbnrs        dscnrs
048400040402     c                   z-add     arbnsp        dscnsp
048500040402     c                   move      arbfns        dscfns
048600040402     c                   z-add     arblna        dsclna
048700040402     c                   move      arbznc        dscznc
048800040402     c                   move      arbrsm        dscrsm
048900040402     c                   move      arbinm        dscinm
049000040402     c                   move      arbcam        dsccam
049100040402     c                   move      arblom        dsclom
049200040402     c                   move      arbprm        dscprm
049300040402     c                   move      arbfap        dscfap
049400040402     c                   move      arbnzm        dscnzm
049500040402     c                   move      arbrsd        dscrsd
049600040402     c                   move      arbind        dscind
049700040402     c                   move      arbcad        dsccad
049800040402     c                   move      arblod        dsclod
049900040402     c                   move      arbprd        dscprd
050000040402     c                   move      arbnzd        dscnzd
050100040402     c                   move      arbfin        dscfin
050200040402     c                   move      arbtsp        dsctsp
050300040402     c                   move      arbfbr        dscfbr
050400040402     c                   z-add     arbncl        dscncl
050500081124     C* Peso: regola CML
050600081124     C     arbncp        ifeq      arbncl
050700081124     C                   z-add     arbpkc        dscpkf
050800081124     C                   else
050900081124     C     arbpkc        ifge      arbpkf
051000081124     C                   z-add     arbpkc        dscpkf
051100081124     C                   else
051200081124     C                   z-add     arbpkf        dscpkf
051300081124     C                   end
051400081124     C                   endif
051500081124     C* Volume: regola CML
051600081124     C     arbfvf        ifeq      'T'
051700081124     C                   z-add     arbvlf        dscvlf
051800081124     C                   else
051900081124     C     arbncr        ifeq      arbncl
052000081124     C                   z-add     arbvlc        dscvlf
052100081124     C                   else
052200081124     C     arbvlc        ifge      arbvlf
052300081124     C                   z-add     arbvlc        dscvlf
052400081124     C                   else
052500081124     C                   z-add     arbvlf        dscvlf
052600081124     C                   endif
052700081124     C                   endif
052800081124     C                   endif
052900081124     c*
053000040402     c                   z-add     arbrmn        dscrmn
053100040402     c                   move      arbrma        dscrma
053200040402     c                   move      arbtcr        dsctcr
053300040402     c                   z-add     arbdcr        dscdcr
053400040402     c                   z-add     arbhcr        dschcr
053500040402     c                   z-add     arbpdc        dscpdc
053600040402     c                   z-add     arbndc        dscndc
053700040402     c                   z-add     arbddc        dscddc
053800040402     c                   move      arbtc1        dsctc1
053900040402     c                   move      arbtc2        dsctc2
054000040402     c                   move      arbgga        dscgga
054100040402     c                   move      arbgma        dscgma
054200040402     c                   move      arbgva        dscgva
054300040402     c                   move      arbngd        dscngd
054400040402     c                   move      arbnss        dscnss
054500040407      * verifico se c'è assegnato da incassare
054600040407     c                   if        arbicc = 'R' or arbicc = ' '
054700040407     c     kar6          chain     fiar601l
054800040407     c                   if        %found(fiar601l)
054900040408     c                   z-add     ar6ift        importo          13 3
055000040408      * incasso parziale
055100040408     c                   if        arbfip = 'I'
055200040408     c     kari          chain     fiari01l
055300040408     c* Se esiste record determino importo residuo da incassare
055400040408     c                   if        %found(fiari01l)
055500040408     c                   sub       ariipp        importo
055600040408     c                   endif
055700040408     c                   endif
055800040407     c                   z-add     ar6fiv        dscfiv
055900040407     c                   z-add     ar6nft        dscnft
056000040407     c                   z-add     ar6dft        dscdft
056100040408     c                   z-add     importo       dscift
056200040407     c                   move      ar6div        dscdiv
056300040407     c                   endif
056400040408     c                   endif
056500040705      * salvo i campi chiave della figlia
056600040705     c                   z-add     arbaas        kaas
056700040705     c                   z-add     arblnp        klnp
056800040705     c                   z-add     arbnrs        knrs
056900040705     c                   z-add     arbnsp        knsp
057000040705      * pulizia data consegna bolla mamma
057100040705     c                   clear                   arbdcmmam
057200040407      * verifico se esiste  mamma
057300040408     c     kar           chain     fnlbl01l
057400040407      * se trovo il legame leggo arb con in chiave la mamma
057500040407      * per agganciare segnacolli e contrassegno
057600040408     c                   if        %found(fnlbl01l) and lbllan = lbllap
057700040407     c     karb1         chain     fnarb01l                           84
057800040705      * memorizzo data consegna mamma se presente
057900040705     c                   move      arbdcm        arbdcmmam         8 0
058000040407     c                   end
058100040407      * verifico se c'è contrassegno e se è da incassare
058200040407     c                   if        arbicc = 'R' or arbicc = ' '
058300051115     c     kar           chain     fiar901l
058400051115     c                   if        %found(fiar901l)
058500040407     c                   move      ar9tic        dsctic
058600040407     c                   z-add     ar9cas        dsccas
058700040407     c                   move      ar9vca        dscvca
058800040407     c                   endif
058900040408     c                   endif
059000040705      * se esiste un legame e la data consegna della mamma è maggiore di 0
059100040705      * mi riposiziono sulla figlia per il reperimento dei segnacolli
059200040705     c                   if        arbdcmmam > 0
059300040705     c     karb1f        chain     fnarb01l                           84
059400040705     c                   endif
059500040705
059600040705      * segnacolli
059700040408     c                   movea     *zeros        nsc
059800040408     c                   if        arbdcm = 0
059900040408     c                   if        arbdcp = 0
060000040402     c                   z-add     arbncd        dscncd
060100040402     c                   z-add     arbnca        dscnca
060200040408     c                   endif
060300040402     c                   z-add     arbfls        dscfls
060400040408     c                   if        arbncd = 0 and arbnca = 0 or arbdcp > 0
060500040408     c     kar           setll     fnart01l
060600040407     c                   z-add     0             i
060700040407     c                   do        *hival
060800040408     c     kar           reade     fnart01l                               91
060900040407      * fine file o massimo 10 colli
061000040407     c                   if        *in91 or i = 10
061100040407     c                   leave
061200040407     c                   endif
061300040407      * inserisco solo i colli non consegnati
061400040407     c                   if        artdcm = 0
061500040407     c                   add       1             i                 3 0
061600040402     c                   z-add     artnsc        nsc(i)
061700040407     c                   else
061800040407     c                   iter
061900040407     c                   endif
062000040402     c                   enddo
062100040408     c                   endif
062200040408     c                   endif
062300040402     c                   z-add     nsc(1)        dscs01
062400040402     c                   z-add     nsc(2)        dscs02
062500040402     c                   z-add     nsc(3)        dscs03
062600040402     c                   z-add     nsc(4)        dscs04
062700040402     c                   z-add     nsc(5)        dscs05
062800040402     c                   z-add     nsc(6)        dscs06
062900040402     c                   z-add     nsc(7)        dscs07
063000040402     c                   z-add     nsc(8)        dscs08
063100040402     c                   z-add     nsc(9)        dscs09
063200040402     c                   z-add     nsc(10)       dscs10
063300040406     c                   move      wdatg         dscdge
063400040402     c                   movel     wora          dschmc
063500060426     C*
063600060426     C* Se scelto nel lancio invio automatico tramite BartVAS => eseguo
063700060606     C                   if        VIDKSC  > *zeros
063800060407     C*
063900060407     C* Scrivo il dato sul TIVGD x lo scarico automatico dei dati tramite BartVAS
064000170127     C                   clear                   tivgd000
064100080404     C                   eval      vgdDTA = %subst(WFDSCDS:1:%size(WFDSCDS))
064200060407     C                   eval      vgdTIP = 'DC'
064300170127     C                   eval      vgdKSU = '0'+VIHKSU
064400060426     C                   eval      vgdTSC = VIHTSC
064500170127     C                   eval      vgdDAT = wOGGI
064600170127     C                   eval      vgdPRG = o§7VASPRG
064700170127     C                   eval      vgdPGM = procname
064800170127     C                   eval      vgdSTO = '?'
064900170127     C                   write     tivgd000
065000060407     C                   endif
065100060407     C*
065200040401     c                   enddo
065300040401      *
065400040401     C                   endsr
065500040401      *---------------------------------------------------------------*
065600040401      * Pulizia elimina rekord più vecchi di  3 giorni x il p.o da elaborare
065700040401      *---------------------------------------------------------------*
065800040401     C     pulisci       begsr
065900040401      *
066000040401     C/EXEC SQL
066100040406     C+ DELETE FROM WFDSC00F WHERE dscdge < :datapul and dscfgs = :vidfil
066200050825     C+ or dscdge < :datapulm
066300040401     C/END-EXEC
066400040401      *
066500040401     C                   endsr
066600060504     C*--------------------------------------------------------------------------------------------*
066700060504     C* REPERISCE I DATI UTENTE
066800060504     C*--------------------------------------------------------------------------------------------*
066900060504     C     REPDATIUTE    BEGSR
067000060504     C*
067100060504     C* INIZIALIZZA VARIABILI DI WRK
067200060504     C                   CLEAR                   TIBS34DS
067300060504     C                   CLEAR                   AZUTEDS
067400060504     C                   CLEAR                   DDATIUTE
067500060504     C*
067600060504     C     *DTAARA       DEFINE    §azute        azuteds
067700060504     C     *DTAARA       DEFINE    §datiute      ddatiute
067800060504     C                   IN(E)     *DTAARA
067900060504     C                   IF        %Error
068000060504     c                   EVAL      I34Tla = 'L'
068100060504     C                   CALL      'TIBS34R'
068200060504     C                   PARM                    Tibs34Ds
068300060504     C                   IN        *DTAARA
068400060504     C                   ENDIF
068500060504     C*
068600060504     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
068700060504     C                   MOVEL(P)  RSUT          DSFIRS           20
068800060504     C*
068900060504     C                   ENDSR
069000161205
069100161205
069200161205
069300161205     C*------------------------------------------------------------------------*
069400161205     C* EXEERR - Routine di esecuzione azioni su Errore
069500161205     C*------------------------------------------------------------------------*
069600161205     C     EXEERR        BEGSR
069700161205     C*
069800161205     C                   dump(A)
069900170322     C                   rolbk(e)
070000161205     C                   seton                                        lr
070100161205     C                   return
070200161205     C*
070300161205     C                   ENDSR
070400161205     C*------------------------------------------------------------------------*
070500161205
070600161205
070700161205
070800040331      *---------------------------------------------------------------*
070900040331      * Definizioni/dichiarazioni iniziali                            *
071000040331      *---------------------------------------------------------------*
071100040331     C     *inzsr        begsr
071200040331      *
071300040331     C     *entry        plist
071400040331     C                   parm                    kpjba
071500060504     C*
071600060504     C* Reperimento dati utente
071700060504     C                   exsr      REPDATIUTE
071800060407     C*
071900060407     C* Giro la data odierna
072000060426     C                   move      *date         g02dat
072100060426     C                   movel     *blanks       g02err
072200060407     C                   call      'XSRDA8'
072300060426     C                   parm                    wlbdat
072400060426     C                   if        g02err = '1'
072500060407     C                   z-add     0             wOGGI
072600060407     C                   else
072700060426     C                   z-add     g02inv        wOGGI
072800060407     C                   endif
072900040407      *profilo di filiale attivo indicatore di protezione campi non manutenzionabili
073000040407     C                   CLEAR                   Tibs36Ds
073100040407     C                   EVAL      I36ute = knmus
073200040407     C                   CALL      'TIBS36R'
073300040407     C                   PARM                    tibs36ds
073400040401     C                   time                    w0120            14 0
073500040401     C                   movel     w0120         wora              6 0
073600040401     C                   move      w0120         wdat              8 0
073700040401     C*
073800040407     C* UDATE A 8 IN AAAA/MM/GG
073900040401     C                   z-add     wdat          g02dat
074000040401     C                   movel     *blank        g02err
074100040401     C                   call      'XSRDA8'
074200040401     C                   parm                    wlbdat
074300040406     C                   move      g02inv        wdatg             8 0
074400040407     C                   move      g02inv        dataiso
074500040407     C                   move      g02inv        dataisop
074600040407      * ricava data pulizia archivio 2 giorni lavorativi precedenti
074700040407     c                   do        *hival
074800040407     c                   move      dataisop      dadata
074900040407     c                   move      dataiso       adata
075000040407     c                   CALL      'XSRLAV8'
075100040407     c                   PARM                    Wdata8
075200040407      * se tentano un estrazione di sabato o domenica forzo la data pulizia
075300040407     c                   if        giolav = 0
075400040407     c     dataisop      subdur    3:*d          dataisop
075500040407     c                   leave
075600040407     c                   endif
075700040407     c                   if        giolav > 3
075800040407     c                   leave
075900040407     c                   else
076000040407     c     dataisop      subdur    1:*d          dataisop
076100040407     c                   endif
076200040407     c                   enddo
076300040407     c                   move      dataisop      datapul           8 0
076400050825      * più vecchie di un mese
076500050825     c     dataisop      subdur    1:*m          dataisop
076600050825     c                   move      dataisop      datapulm          8 0
076700040401     C***
076800040401     c     karb          klist
076900040401     c                   kfld                    kifp              9 0
077000040401     c                   kfld                    dist7             7 0
077100040407     C***
077200040407     c     karb1         klist
077300040408     c                   kfld                    lblaap
077400040408     c                   kfld                    lbllpp
077500040408     c                   kfld                    lblnrp
077600040407     c                   kfld                    lblnsp
077700040705     C***
077800040705     c     karb1f        klist
077900040705     c                   kfld                    kaas              4 0
078000040705     c                   kfld                    klnp              3 0
078100040705     c                   kfld                    knrs              2 0
078200040705     c                   kfld                    knsp              7 0
078300040401     C***
078400040401     c     kwfdsc        klist
078500040401     c                   kfld                    vidfil
078600040401     c                   kfld                    dist7
078700040331     C***
078800040401     c     kfvv          klist
078900040401     c                   kfld                    categ             1 0
079000040401     c                   kfld                    dist5             5 0
079100040401     c                   kfld                    vidfil
079200040401     c                   z-add     4             categ
079300040401     C***
079400040401     c     kfvv8         klist
079500040401     c                   kfld                    vidfil
079600040401     c                   kfld                    categ
079700040401     c                   kfld                    viddatg
079800040401     C***
079900040401     c     kdst          klist
080000120607     c                   kfld                    fvvnpg
080100120607     c                   kfld                    knfv6
080200040405     c                   kfld                    fvvfgs
080300040402     C***
080400040408     c     kar           klist
080500040407     c                   kfld                    arbaas
080600040407     c                   kfld                    arblnp
080700040407     c                   kfld                    arbnrs
080800040407     c                   kfld                    arbnsp
080900040402     C***
081000040402     c     kar6          klist
081100040402     c                   kfld                    arbaas
081200040402     c                   kfld                    arblnp
081300040402     c                   kfld                    arbnrs
081400040402     c                   kfld                    arbnsp
081500040402     c                   kfld                    tirec             1
081600040402     c                   eval      tirec = '1'
081700040408     C***
081800040408     c     kari          klist
081900040408     c                   kfld                    arbaas
082000040408     c                   kfld                    arblnp
082100040408     c                   kfld                    arbnrs
082200040408     c                   kfld                    arbnsp
082300040408     c                   kfld                    ktip              1
082400040408     c                   eval      ktip  = 'A'
082500040408
082600040331     C* CARICO TABELLA FILIALI GESTITE £1
082700040331     C***
082800040407     C                   movel     kpjbu         savkpjbu
082900040405     C                   clear                   trul06ds
083000040331     C                   move      '£1'          d06cod
083100040407     C                   movel     o36tfp        d06key
083200040405     C                   movel     trul06ds      kpjbu
083300040331     C                   call      'TRUL06R'
083400040331     C                   parm                    kpjba
083500040405     C                   movel     kpjbu         trul06ds
083600040407     C                   movel     savkpjbu      kpjbu
083700040331      *
083800040407     C                   eval      vidfil = o36pou
083900040407     C                   move      o36tfp        wo36tfp           3 0
084000040331      *
084100040331     C                   endsr
084200040331      *----------------------------------------------------------------
084300040331**  ERRORI
084400040331Filiale inesistente !!!!                                              01
084500040331Filiale non in gestione !!!                                           02
084600040331Codice filiale obbligatorio !!!                                       03
084700040331Inserire il Nr. distinta oppure il codice autotrasportatore e la data 04
084800040331Se inserito il codice autotrasportatore inserire la data              05
084900040331Data errata reinserirla correttamente !!!                             06
085000040331Cambio P.O. ammesso solo a filiali di primo livello                   07
085100180110Numero distinta errato
085200060407Elaborazione al momento non disponibile riprovare tra qualche minuto. 09
085300060620Errore in reperimento clienti abilitati all'invio automatico. Tel CED 10
085400060504Non si è abilitati al cliente selezionato!                            11
085500060504Cliente inesistente, correggere.                                      12
085600060606Codice cliente obbligatorio !!!                                       13
