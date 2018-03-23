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
008400040407     C                   move      *zeros        vidndcn                        2' COD.PADRONC.
008500060508     C                   move      *blanks       vidksc                         CLIENTE UNIFIC.
008600000000      *
008700000000      * Emetto il formato video
008800040331     c                   do        *hival
008900040331     C     emetto        tag
009000040331     C                   setoff                                       90
009100040331     C                   exfmt     video1
009200040331     c                   clear                   $msg
009300040405     C                   setoff                                       504028
009400000000      *
009500040331     C   kc              goto      fine                                         F3=USCITA
009600040331      * Richiamo programma di stampa
009700040407     c                   if        o36pou = wo36tfp
009800040331     c                   if        *inks
009900040331     c                   seton                                        50
010000040331     c                   iter
010100040331     c                   endif
010200050325     c*                  else
010300050325     c*                  seton                                        409028
010400050325     c*                  movel     err(7)        $msg
010500050325     c*                  goto      emetto
010600040331     c                   endif
010700000000      *
010800000000      * Eseguo i  controlli sui campi immessi a video
010900040331     C                   exsr      contr
011000000000      *
011100020308      * Se non vi sono errori ma non e' premuto F6 emetto video
011200040331     C  n90
011300040331     Cannkf              seton                                          90      F6=CONFERMA
011400000000      *
011500000000      * Per errore generico riemetto il formato video
011600040331     C   90              goto      emetto
011700000000      *
011800020307      * Richiamo programma di stampa
011900020308     c                   if        *inkf
012000040331     c                   exsr      elabora
012100040331      * dopo elaborazione inizializzo campi a video e riemetto il video
012200060407     C                   if        wFlgEXE = 'S'
012300040331     C                   z-add     0             VIDpdc                         DATA FINALE
012400040331     C                   Z-ADD     0             viddat                         1' COD.PADRONC.
012500040407     C                   move      *blank        vidndc                         2' COD.PADRONC.
012600040407     C                   move      *zeros        vidndcn                        2' COD.PADRONC.
012700060508     C                   move      *blanks       vidksc                         CLIENTE UNIFIC.
012800060407     c                   endif
012900040331     c                   iter
013000020308     c                   endif
013100000000      *
013200040331     c                   enddo
013300040331     C     fine          tag
013400060407     C*
013500040331     C                   seton                                        lr
013600000000      *--------------------------------------------------------------*
013700000000      * Routine per i controlli sui campi del formato video          *
013800000000      *--------------------------------------------------------------*
013900040331     C     contr         begsr
014000000000      *
014100060606     c                   movea     '00000'       *in(40)
014200060504      *
014300000000      * Verifico il codice filiale
014400040331     C                   move      *blank        desfil
014500040331     C     vidfil        ifgt      0
014600040331     C     vidfil        chain     azorg01l                           89
014700040331     c                   if        *in89
014800040405     C                   SETON                                        904028
014900040331     c                   movel     err(1)        $msg
015000040331     C                   goto      endctl
015100040331     c                   else
015200040331     C                   movel     orgdes        desfil
015300040331     c                   endif
015400000000      *
015500000000      * IN SEDE QUALSIASI FILIALE, IN FIL SOLO LE MIE
015600091014     ***C     Wo36tfp       ifne      0
015700091014     c                   if             Wo36tfp <> *zeros
015800091014     c                             and  %subst(kNmUs:1:3) <> 'EDP'
015900040331     C     vidfil        lookup    Lin                                    99
016000040405     C  n99              seton                                        904028
016100040331     c  n99              movel     err(2)        $msg
016200040331     C  n99              goto      endctl
016300040331     C                   endif
016400020308     c                   else
016500040405     C                   seton                                        904028
016600040331     c                   movel     err(3)        $msg
016700040331     C                   goto      endctl
016800040331     C                   endif
016900000000      *
017000000000      * Controlli di congruenza
017100040331      * Se immesso il cd.autotras. non può essere inserito il numero dist.
017200040407     C     vidndc        ifne      *zero
017300040407     C     vidndc        andne     *blank
017400040407     C     vidpdc        andgt     *zero
017500040331     C                   seton                                        904142
017600040405     C                   seton                                        28
017700040331     c                   movel     err(4)        $msg
017800040331     C                   goto      endctl
017900040331     C                   endif
018000040331      * immettere o distinta o autotrasportatore
018100040331     C     vidndc        ifeq      *zero
018200040331     C     vidpdc        andeq     *zero
018300040407     C     vidndc        oreq      *blank
018400040407     C     vidpdc        andeq     *zero
018500040331     C                   seton                                        904142
018600040405     C                   seton                                        28
018700040331     c                   movel     err(4)        $msg
018800040331     C                   goto      endctl
018900040331     C                   endif
019000040331     C     vidpdc        ifgt      *zero
019100040331     C     viddat        andeq     *zero
019200040331     C                   seton                                        904243
019300040405     C                   seton                                        28
019400040331     c                   movel     err(5)        $msg
019500040331     C                   goto      endctl
019600040331     C                   endif
019700000000      *
019800000000      * Controllo data iniziale
019900040331     C     viddat        ifgt      0
020000040331     C                   z-add     viddat        g02dat
020100040331     C                   movel     *blank        g02err
020200040331     C                   call      'XSRDA8'
020300040331     C                   parm                    wlbdat
020400040331     C     g02err        ifeq      '1'
020500040405     C                   seton                                        439028
020600040331     c                   movel     err(6)        $msg
020700040331     C                   goto      endctl
020800040331     C                   endif
020900040331     C                   z-add     g02inv        viddatg           8 0
021000040331     C                   z-add     g02dat        viddat
021100020307     c                   endif
021200000000      *
021300040401      * Controlli di congruenza
021400040407     C*  Controllo il numero distinta
021500040407     C     vidndc        ifne      *zeros
021600040407     C     vidndc        andne     *blank
021700040407     C     '?'           scan      vidndc                                 86
021800040407     C     *in86         ifeq      '1'
021900040407     C                   clear                   dslr01
022000040407     C                   movel     'FNLR81R'     dlrpgm
022100040407     C                   movel     '2'           dlrtfv
022200040407     C                   z-add     4             dlrnpg
022300040407     C                   z-add     vidfil        dlrfgs
022400040407     C                   movel     'S'           dlrric
022500040407     C                   z-add     wdatg         dlrada
022600040407     C                   movel(p)  dslr01        kpjbu
022700040407     C                   call      'FNLR02R'
022800040407     C                   parm                    kpjba
022900040407     C                   movel     kpjbu         dslr01
023000040407     C                   move      dlrnfv        vidndc
023100040407     C                   endif
023200040407     C                   endif
023300040407     C*
023400040407     C                   testn                   vidndc               86
023500040407     C   86              move      vidndc        vidndcn           7 0
023600040401      * Se immesso il Nr distinta verifica che sia disponibile
023700040407     C     vidndcn       ifgt      *zero
023800040407     c                   z-add     vidndcn       dist5
023900040401     c     kfvv          chain     fnfvv01l                           88
024000140819     c***                if        *in88 or fvvfcf <> *blank or fvvatb <> *blank
024100140819     c                   if        *in88 or fvvatb <> *blank
024200040405     C                   seton                                        904128
024300060426     c                   movel     err(8)        $msg
024400040401     C                   goto      endctl
024500040401     C                   endif
024600040401     C                   endif
024700060426     C*
024800060426     C* Verifico se richiesto invio automatico a cliente
024900060606     C                   movel     *blanks       DESKSU
025000060504     C                   eval      T02mod = 'C'
025100060508     C     '?'           scan      VIDKSC                                 10
025200060426     C                   if        *in10
025300060504     C                   eval      T02mod = 'R'
025400060504     C                   endif
025500060504     C*
025600060508     C                   if        *IN10 or VIDKSC > *zeros
025700060508     C  N10              eval      T02KE1 = VIDKSC
025800060504     C   10              eval      T02KE1 = *blanks
025900060504     C                   call      'TIBS02R'
026000060426     C                   parm                    KPJBA
026100060426     C                   parm                    TIBS02ds
026200060426     C                   if        T02err  = *blanks
026300060426     C                   movel     T02uni        DDSC
026400060504     C* X utente nn EDP* verifico che il P.O. utente sia nella schiera d P.O. abilitati x il
026500060504     C* cliente scelto da tabella DSC
026600060504     C                   if        %subst(%trim(KNMUS):1:3) <> 'EDP'
026700060504     C                   if        DUTPOU = §DSCPO001 OR
026800060504     C                             DUTPOU = §DSCPO002 OR
026900060504     C                             DUTPOU = §DSCPO003 OR
027000060504     C                             DUTPOU = §DSCPO004 OR
027100060504     C                             DUTPOU = §DSCPO005 OR
027200060504     C                             DUTPOU = §DSCPO006 OR
027300060504     C                             DUTPOU = §DSCPO007 OR
027400060504     C                             DUTPOU = §DSCPO008 OR
027500060504     C                             DUTPOU = §DSCPO009 OR
027600060504     C                             DUTPOU = §DSCPO010
027700060504     C                   else
027800060504     C                   seton                                        449028
027900060504     C                   movel     err(11)       $msg
028000060606     C                   goto      endctl
028100060504     C                   endif
028200060504     C                   endif
028300060504     C*
028400060508     C                   movel     T02KE1        VIDKSC
028500060508     C                   movel     §DSCKSU       VIHKSU
028600060504     C                   eval      VIHTSC = §DSCTSC
028700060504     C* Decodifico la ragione sociale del cliente
028800060504     C                   exsr      DECTIB
028900120320     C***                if        O69ERR = '1'
029000120320     C***                movel     *all'*'       ACORAG
029100120320     C***                seton                                        449028
029200120320     C***                movel     err(12)       $msg
029300120320     C***                goto      endctl
029400120320     C***                endif
029500120320     C                   movel     ACORAG        DESKSU
029600060504     C*
029700060426     C                   else
029800060504     C                   seton                                        449028
029900060426     C                   movel     err(10)       $msg
030000060508     C                   movel     *all'0'       VIDKSC
030100060426     C                   goto      endctl
030200060426     C                   endif
030300060606     C                   else
030400060606     C                   seton                                        449028
030500060606     C                   movel     err(13)       $msg
030600060606     C                   goto      endctl
030700060504     C                   endif
030800040401      *
030900040331     C     endctl        endsr
031000060504     C*------------------------------------------------------------------------*
031100060504     C* DECTIB - CONTROLLO/DECODIFICA CLIENTE
031200060504     C*------------------------------------------------------------------------*
031300060504     C     DECTIB        BEGSR
031400060504     C*
031500060504     C                   CLEAR                   BS69DS
031600060504     C                   CLEAR                   ACODS
031700060504     C                   MOVEL     KNSIF         I69SIF
031800060508     C                   MOVE(P)   VIDKSC        I69KAC
031900060504     C                   CALL      'TIBS69R'
032000060504     C                   PARM                    BS69DS
032100060504     C                   PARM                    ACODS
032200060504     C                   PARM                    INDDS
032300060504     C                   PARM                    CLPDS
032400060504     C                   PARM                    CLSDS
032500060504     C*
032600060504     C                   ENDSR
032700040331      *---------------------------------------------------------------*
032800040331      * Elaborazione dati distinta e scrittura su file                *
032900040331      *---------------------------------------------------------------*
033000040331     C     elabora       begsr
033100161205     C*
033200161205     C* Inizializzo variabili d wrk
033300161205     C                   movel     'N'           wProcedi          1
033400060407     C*
033500060407     C* Inizializzo il flag che indica l'avvenuta effettiva elaborazione
033600060407     C                   movel     'S'           wFlgEXE
033700060407      *
033800040401      * esegue pulizia dati per il P.O da elaborare fino a 2 giorni
033900040401      * precedenti la data del giorno
034000040401     c                   exsr      pulisci
034100161205     C*
034200161205     C* Se scelto nel lancio invio automatico tramite BartVAS
034300161205     C                   if        VIDKSC  > *zeros
034400170201     C*
034500170201     C* Avvio il controllo sincronia
034600170201     C                   EVAL      i§7VASOPZ = 'SCM'
034700170201     C                   CALL(e)   'TIS7VASR1'
034800170201     C                   PARM                    TIS7VASDS
034900170201     C*
035000170201     C* Verifico esito chiamata
035100170201     C                   if        %error OR o§7VASOK = *off
035200170201     C                   exsr      EXEERR
035300170201     C                   endif
035400170201     C*
035500170201     C* Se tutto OK apro il file download
035600170201     C                   if        not %open(tivgd00f)
035700170201     C                   open      tivgd00f
035800170201     C                   endif
035900170201     C*
036000170201     C* Inizializzo la transazione
036100170127     C                   CLEAR                   TIS7VASDS
036200170127     C                   EVAL      i§7VASOPZ = 'PRG'
036300170201     C                   CALL(e)   'TIS7VASR1'
036400170127     C                   PARM                    TIS7VASDS
036500161205     C*
036600161205     C* Verifico esito chiamata
036700170127     C                   if        %error OR o§7VASOK = *off
036800161205     C                   exsr      EXEERR
036900161205     C                   endif
037000161205     C*
037100161205     C* Se OK => proseguo
037200170127     C                   if        o§7VASOK = *on AND o§7VASPRG <> *blanks
037300161205     C                   movel     'S'           wProcedi
037400161205     C                   endif
037500161205     C                   endif
037600161205     C*
037700161205     C* Se ok a procedere => elaboro
037800161205     C                   if        wProcedi = 'S'
037900040331      *
038000040401     c                   z-add     vidfil        kifp
038100040401      *elaborazione per singola distinta
038200040407     c                   if        vidndcn > 0
038300040407     c                   z-add     vidndcn       dist7
038400040402     c                   exsr      estrai
038500040401     c                   else
038600040401      *elaborazione per autotrasportatore / data
038700040401     c                   movel     vidfil        comodo7           7 0
038800040401     c                   move      vidpdc        comodo7
038900040401     c     kfvv8         setll     fnfvv08l
039000040401     c                   do        *hival
039100040401     c     kfvv8         reade     fnfvv08l                               87
039200040401     c   87              leave
039300120607     c* novo file FIDST con npg in key e foglio lungo 6
039400120607     c                   eval      knfv6=fvvnfv
039500120607     c     kdst          chain     fidst01l
039600120607     c
039700120607     c                   if        %found(fidst01l) and
039800120607     c                             not *in87 and dstpdr = comodo7 and
039900040401     c                             fvvfcf = *blank and fvvatb = *blank
040000040401     c                   z-add     fvvnfv        dist7
040100040401     c                   exsr      estrai
040200040401     c                   endif
040300040401     c                   enddo
040400040401     c                   endif
040500070305     C*
040600060407     C                   else
040700060407     C                   movel     'N'           wFlgEXE
040800060407     C                   seton                                        90
040900060407     c                   movel     err(9)        $msg
041000060407     C                   endif
041100161205     C*
041200170201     C* Se scelto nel lancio invio automatico tramite BartVAS
041300161205     C                   if        VIDKSC  > *zeros
041400170201     C*
041500170201     C* Chiudo il file download
041600170201     C                   if        %open(tivgd00f)
041700170201     C                   close     tivgd00f
041800170201     C*
041900170201     C* Finalizzo la transazione
042000170127     C                   EVAL      i§7VASOPZ = 'RLS'
042100170127     C                   EVAL      i§7VASTIP = 'DC'
042200170127     C                   EVAL      i§7VASKSU = '0'+VIHKSU
042300170127     C                   EVAL      i§7VASTSC = VIHTSC
042400170127     C                   EVAL      i§7VASSTO = '?'
042500170127     C                   EVAL      i§7VASPRG = o§7VASPRG
042600170201     C                   CALL(e)   'TIS7VASR1'
042700170127     C                   PARM                    TIS7VASDS
042800161205     C*
042900161205     C* Verifico esito chiamata
043000170127     C                   if        %error OR o§7VASOK = *off
043100161205     C                   exsr      EXEERR
043200161205     C                   endif
043300170201     C*
043400170201     C* Fine controllo sincronia
043500170201     C                   EVAL      i§7VASOPZ = 'ECM'
043600170201     C                   CALL(e)   'TIS7VASR1'
043700170201     C                   PARM                    TIS7VASDS
043800170201     C*
043900170201     C* Verifico esito chiamata
044000170201     C                   if        %error OR o§7VASOK = *off
044100170201     C                   exsr      EXEERR
044200170201     C                   endif
044300170201     C                   endif
044400170201     C*
044500161205     C                   endif
044600170201     C*
044700040331     C                   endsr
044800040401      *---------------------------------------------------------------*
044900040401      * estrazione rekord da arb eventuale eliminazione rekord già esistenti
045000040401      *---------------------------------------------------------------*
045100040401     C     estrai        begsr
045200040401      *
045300040401      * verifica se distinta già presente la cancella per riscriverla
045400040401     c     kwfdsc        chain     wfdsc01l                           95
045500040401     c                   if        not *in95
045600040402     C/EXEC SQL
045700040402     C+ DELETE FROM WFDSC00F WHERE dscndc = :dist7 and dscfgs = :vidfil
045800040402     C/END-EXEC
045900040401      *
046000040401     c                   endif
046100040401
046200040401     c     karb          setll     fnarb70l
046300040401     c                   do        *hival
046400040401     c     karb          reade     fnarb70l                               94
046500040401     c   94              leave
046600040401      *
046700040402     c                   clear                   wfdsc000
046800060606     c                   clear                   WFDSCDS
046900040402
047000040408     c                   z-add     vidfil        dscfgs
047100040402     c                   z-add     arbaas        dscaas
047200040402     c                   z-add     arblnp        dsclnp
047300040402     c                   z-add     arbnrs        dscnrs
047400040402     c                   z-add     arbnsp        dscnsp
047500040402     c                   move      arbfns        dscfns
047600040402     c                   z-add     arblna        dsclna
047700040402     c                   move      arbznc        dscznc
047800040402     c                   move      arbrsm        dscrsm
047900040402     c                   move      arbinm        dscinm
048000040402     c                   move      arbcam        dsccam
048100040402     c                   move      arblom        dsclom
048200040402     c                   move      arbprm        dscprm
048300040402     c                   move      arbfap        dscfap
048400040402     c                   move      arbnzm        dscnzm
048500040402     c                   move      arbrsd        dscrsd
048600040402     c                   move      arbind        dscind
048700040402     c                   move      arbcad        dsccad
048800040402     c                   move      arblod        dsclod
048900040402     c                   move      arbprd        dscprd
049000040402     c                   move      arbnzd        dscnzd
049100040402     c                   move      arbfin        dscfin
049200040402     c                   move      arbtsp        dsctsp
049300040402     c                   move      arbfbr        dscfbr
049400040402     c                   z-add     arbncl        dscncl
049500081124     C* Peso: regola CML
049600081124     C     arbncp        ifeq      arbncl
049700081124     C                   z-add     arbpkc        dscpkf
049800081124     C                   else
049900081124     C     arbpkc        ifge      arbpkf
050000081124     C                   z-add     arbpkc        dscpkf
050100081124     C                   else
050200081124     C                   z-add     arbpkf        dscpkf
050300081124     C                   end
050400081124     C                   endif
050500081124     C* Volume: regola CML
050600081124     C     arbfvf        ifeq      'T'
050700081124     C                   z-add     arbvlf        dscvlf
050800081124     C                   else
050900081124     C     arbncr        ifeq      arbncl
051000081124     C                   z-add     arbvlc        dscvlf
051100081124     C                   else
051200081124     C     arbvlc        ifge      arbvlf
051300081124     C                   z-add     arbvlc        dscvlf
051400081124     C                   else
051500081124     C                   z-add     arbvlf        dscvlf
051600081124     C                   endif
051700081124     C                   endif
051800081124     C                   endif
051900081124     c*
052000040402     c                   z-add     arbrmn        dscrmn
052100040402     c                   move      arbrma        dscrma
052200040402     c                   move      arbtcr        dsctcr
052300040402     c                   z-add     arbdcr        dscdcr
052400040402     c                   z-add     arbhcr        dschcr
052500040402     c                   z-add     arbpdc        dscpdc
052600040402     c                   z-add     arbndc        dscndc
052700040402     c                   z-add     arbddc        dscddc
052800040402     c                   move      arbtc1        dsctc1
052900040402     c                   move      arbtc2        dsctc2
053000040402     c                   move      arbgga        dscgga
053100040402     c                   move      arbgma        dscgma
053200040402     c                   move      arbgva        dscgva
053300040402     c                   move      arbngd        dscngd
053400040402     c                   move      arbnss        dscnss
053500040407      * verifico se c'è assegnato da incassare
053600040407     c                   if        arbicc = 'R' or arbicc = ' '
053700040407     c     kar6          chain     fiar601l
053800040407     c                   if        %found(fiar601l)
053900040408     c                   z-add     ar6ift        importo          13 3
054000040408      * incasso parziale
054100040408     c                   if        arbfip = 'I'
054200040408     c     kari          chain     fiari01l
054300040408     c* Se esiste record determino importo residuo da incassare
054400040408     c                   if        %found(fiari01l)
054500040408     c                   sub       ariipp        importo
054600040408     c                   endif
054700040408     c                   endif
054800040407     c                   z-add     ar6fiv        dscfiv
054900040407     c                   z-add     ar6nft        dscnft
055000040407     c                   z-add     ar6dft        dscdft
055100040408     c                   z-add     importo       dscift
055200040407     c                   move      ar6div        dscdiv
055300040407     c                   endif
055400040408     c                   endif
055500040705      * salvo i campi chiave della figlia
055600040705     c                   z-add     arbaas        kaas
055700040705     c                   z-add     arblnp        klnp
055800040705     c                   z-add     arbnrs        knrs
055900040705     c                   z-add     arbnsp        knsp
056000040705      * pulizia data consegna bolla mamma
056100040705     c                   clear                   arbdcmmam
056200040407      * verifico se esiste  mamma
056300040408     c     kar           chain     fnlbl01l
056400040407      * se trovo il legame leggo arb con in chiave la mamma
056500040407      * per agganciare segnacolli e contrassegno
056600040408     c                   if        %found(fnlbl01l) and lbllan = lbllap
056700040407     c     karb1         chain     fnarb01l                           84
056800040705      * memorizzo data consegna mamma se presente
056900040705     c                   move      arbdcm        arbdcmmam         8 0
057000040407     c                   end
057100040407      * verifico se c'è contrassegno e se è da incassare
057200040407     c                   if        arbicc = 'R' or arbicc = ' '
057300051115     c     kar           chain     fiar901l
057400051115     c                   if        %found(fiar901l)
057500040407     c                   move      ar9tic        dsctic
057600040407     c                   z-add     ar9cas        dsccas
057700040407     c                   move      ar9vca        dscvca
057800040407     c                   endif
057900040408     c                   endif
058000040705      * se esiste un legame e la data consegna della mamma è maggiore di 0
058100040705      * mi riposiziono sulla figlia per il reperimento dei segnacolli
058200040705     c                   if        arbdcmmam > 0
058300040705     c     karb1f        chain     fnarb01l                           84
058400040705     c                   endif
058500040705
058600040705      * segnacolli
058700040408     c                   movea     *zeros        nsc
058800040408     c                   if        arbdcm = 0
058900040408     c                   if        arbdcp = 0
059000040402     c                   z-add     arbncd        dscncd
059100040402     c                   z-add     arbnca        dscnca
059200040408     c                   endif
059300040402     c                   z-add     arbfls        dscfls
059400040408     c                   if        arbncd = 0 and arbnca = 0 or arbdcp > 0
059500040408     c     kar           setll     fnart01l
059600040407     c                   z-add     0             i
059700040407     c                   do        *hival
059800040408     c     kar           reade     fnart01l                               91
059900040407      * fine file o massimo 10 colli
060000040407     c                   if        *in91 or i = 10
060100040407     c                   leave
060200040407     c                   endif
060300040407      * inserisco solo i colli non consegnati
060400040407     c                   if        artdcm = 0
060500040407     c                   add       1             i                 3 0
060600040402     c                   z-add     artnsc        nsc(i)
060700040407     c                   else
060800040407     c                   iter
060900040407     c                   endif
061000040402     c                   enddo
061100040408     c                   endif
061200040408     c                   endif
061300040402     c                   z-add     nsc(1)        dscs01
061400040402     c                   z-add     nsc(2)        dscs02
061500040402     c                   z-add     nsc(3)        dscs03
061600040402     c                   z-add     nsc(4)        dscs04
061700040402     c                   z-add     nsc(5)        dscs05
061800040402     c                   z-add     nsc(6)        dscs06
061900040402     c                   z-add     nsc(7)        dscs07
062000040402     c                   z-add     nsc(8)        dscs08
062100040402     c                   z-add     nsc(9)        dscs09
062200040402     c                   z-add     nsc(10)       dscs10
062300040406     c                   move      wdatg         dscdge
062400040402     c                   movel     wora          dschmc
062500060426     C*
062600060426     C* Se scelto nel lancio invio automatico tramite BartVAS => eseguo
062700060606     C                   if        VIDKSC  > *zeros
062800060407     C*
062900060407     C* Scrivo il dato sul TIVGD x lo scarico automatico dei dati tramite BartVAS
063000170127     C                   clear                   tivgd000
063100080404     C                   eval      vgdDTA = %subst(WFDSCDS:1:%size(WFDSCDS))
063200060407     C                   eval      vgdTIP = 'DC'
063300170127     C                   eval      vgdKSU = '0'+VIHKSU
063400060426     C                   eval      vgdTSC = VIHTSC
063500170127     C                   eval      vgdDAT = wOGGI
063600170127     C                   eval      vgdPRG = o§7VASPRG
063700170127     C                   eval      vgdPGM = procname
063800170127     C                   eval      vgdSTO = '?'
063900170127     C                   write     tivgd000
064000060407     C                   endif
064100060407     C*
064200040401     c                   enddo
064300040401      *
064400040401     C                   endsr
064500040401      *---------------------------------------------------------------*
064600040401      * Pulizia elimina rekord più vecchi di  3 giorni x il p.o da elaborare
064700040401      *---------------------------------------------------------------*
064800040401     C     pulisci       begsr
064900040401      *
065000040401     C/EXEC SQL
065100040406     C+ DELETE FROM WFDSC00F WHERE dscdge < :datapul and dscfgs = :vidfil
065200050825     C+ or dscdge < :datapulm
065300040401     C/END-EXEC
065400040401      *
065500040401     C                   endsr
065600060504     C*--------------------------------------------------------------------------------------------*
065700060504     C* REPERISCE I DATI UTENTE
065800060504     C*--------------------------------------------------------------------------------------------*
065900060504     C     REPDATIUTE    BEGSR
066000060504     C*
066100060504     C* INIZIALIZZA VARIABILI DI WRK
066200060504     C                   CLEAR                   TIBS34DS
066300060504     C                   CLEAR                   AZUTEDS
066400060504     C                   CLEAR                   DDATIUTE
066500060504     C*
066600060504     C     *DTAARA       DEFINE    §azute        azuteds
066700060504     C     *DTAARA       DEFINE    §datiute      ddatiute
066800060504     C                   IN(E)     *DTAARA
066900060504     C                   IF        %Error
067000060504     c                   EVAL      I34Tla = 'L'
067100060504     C                   CALL      'TIBS34R'
067200060504     C                   PARM                    Tibs34Ds
067300060504     C                   IN        *DTAARA
067400060504     C                   ENDIF
067500060504     C*
067600060504     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
067700060504     C                   MOVEL(P)  RSUT          DSFIRS           20
067800060504     C*
067900060504     C                   ENDSR
068000161205
068100161205
068200161205
068300161205     C*------------------------------------------------------------------------*
068400161205     C* EXEERR - Routine di esecuzione azioni su Errore
068500161205     C*------------------------------------------------------------------------*
068600161205     C     EXEERR        BEGSR
068700161205     C*
068800161205     C                   dump(A)
068900161205     C                   rolbk
069000161205     C                   seton                                        lr
069100161205     C                   return
069200161205     C*
069300161205     C                   ENDSR
069400161205     C*------------------------------------------------------------------------*
069500161205
069600161205
069700161205
069800040331      *---------------------------------------------------------------*
069900040331      * Definizioni/dichiarazioni iniziali                            *
070000040331      *---------------------------------------------------------------*
070100040331     C     *inzsr        begsr
070200040331      *
070300040331     C     *entry        plist
070400040331     C                   parm                    kpjba
070500060504     C*
070600060504     C* Reperimento dati utente
070700060504     C                   exsr      REPDATIUTE
070800060407     C*
070900060407     C* Giro la data odierna
071000060426     C                   move      *date         g02dat
071100060426     C                   movel     *blanks       g02err
071200060407     C                   call      'XSRDA8'
071300060426     C                   parm                    wlbdat
071400060426     C                   if        g02err = '1'
071500060407     C                   z-add     0             wOGGI
071600060407     C                   else
071700060426     C                   z-add     g02inv        wOGGI
071800060407     C                   endif
071900040407      *profilo di filiale attivo indicatore di protezione campi non manutenzionabili
072000040407     C                   CLEAR                   Tibs36Ds
072100040407     C                   EVAL      I36ute = knmus
072200040407     C                   CALL      'TIBS36R'
072300040407     C                   PARM                    tibs36ds
072400040401     C                   time                    w0120            14 0
072500040401     C                   movel     w0120         wora              6 0
072600040401     C                   move      w0120         wdat              8 0
072700040401     C*
072800040407     C* UDATE A 8 IN AAAA/MM/GG
072900040401     C                   z-add     wdat          g02dat
073000040401     C                   movel     *blank        g02err
073100040401     C                   call      'XSRDA8'
073200040401     C                   parm                    wlbdat
073300040406     C                   move      g02inv        wdatg             8 0
073400040407     C                   move      g02inv        dataiso
073500040407     C                   move      g02inv        dataisop
073600040407      * ricava data pulizia archivio 2 giorni lavorativi precedenti
073700040407     c                   do        *hival
073800040407     c                   move      dataisop      dadata
073900040407     c                   move      dataiso       adata
074000040407     c                   CALL      'XSRLAV8'
074100040407     c                   PARM                    Wdata8
074200040407      * se tentano un estrazione di sabato o domenica forzo la data pulizia
074300040407     c                   if        giolav = 0
074400040407     c     dataisop      subdur    3:*d          dataisop
074500040407     c                   leave
074600040407     c                   endif
074700040407     c                   if        giolav > 3
074800040407     c                   leave
074900040407     c                   else
075000040407     c     dataisop      subdur    1:*d          dataisop
075100040407     c                   endif
075200040407     c                   enddo
075300040407     c                   move      dataisop      datapul           8 0
075400050825      * più vecchie di un mese
075500050825     c     dataisop      subdur    1:*m          dataisop
075600050825     c                   move      dataisop      datapulm          8 0
075700040401     C***
075800040401     c     karb          klist
075900040401     c                   kfld                    kifp              9 0
076000040401     c                   kfld                    dist7             7 0
076100040407     C***
076200040407     c     karb1         klist
076300040408     c                   kfld                    lblaap
076400040408     c                   kfld                    lbllpp
076500040408     c                   kfld                    lblnrp
076600040407     c                   kfld                    lblnsp
076700040705     C***
076800040705     c     karb1f        klist
076900040705     c                   kfld                    kaas              4 0
077000040705     c                   kfld                    klnp              3 0
077100040705     c                   kfld                    knrs              2 0
077200040705     c                   kfld                    knsp              7 0
077300040401     C***
077400040401     c     kwfdsc        klist
077500040401     c                   kfld                    vidfil
077600040401     c                   kfld                    dist7
077700040331     C***
077800040401     c     kfvv          klist
077900040401     c                   kfld                    categ             1 0
078000040401     c                   kfld                    dist5             5 0
078100040401     c                   kfld                    vidfil
078200040401     c                   z-add     4             categ
078300040401     C***
078400040401     c     kfvv8         klist
078500040401     c                   kfld                    vidfil
078600040401     c                   kfld                    categ
078700040401     c                   kfld                    viddatg
078800040401     C***
078900040401     c     kdst          klist
079000120607     c                   kfld                    fvvnpg
079100120607     c                   kfld                    knfv6
079200040405     c                   kfld                    fvvfgs
079300040402     C***
079400040408     c     kar           klist
079500040407     c                   kfld                    arbaas
079600040407     c                   kfld                    arblnp
079700040407     c                   kfld                    arbnrs
079800040407     c                   kfld                    arbnsp
079900040402     C***
080000040402     c     kar6          klist
080100040402     c                   kfld                    arbaas
080200040402     c                   kfld                    arblnp
080300040402     c                   kfld                    arbnrs
080400040402     c                   kfld                    arbnsp
080500040402     c                   kfld                    tirec             1
080600040402     c                   eval      tirec = '1'
080700040408     C***
080800040408     c     kari          klist
080900040408     c                   kfld                    arbaas
081000040408     c                   kfld                    arblnp
081100040408     c                   kfld                    arbnrs
081200040408     c                   kfld                    arbnsp
081300040408     c                   kfld                    ktip              1
081400040408     c                   eval      ktip  = 'A'
081500040408
081600040331     C* CARICO TABELLA FILIALI GESTITE £1
081700040331     C***
081800040407     C                   movel     kpjbu         savkpjbu
081900040405     C                   clear                   trul06ds
082000040331     C                   move      '£1'          d06cod
082100040407     C                   movel     o36tfp        d06key
082200040405     C                   movel     trul06ds      kpjbu
082300040331     C                   call      'TRUL06R'
082400040331     C                   parm                    kpjba
082500040405     C                   movel     kpjbu         trul06ds
082600040407     C                   movel     savkpjbu      kpjbu
082700040331      *
082800040407     C                   eval      vidfil = o36pou
082900040407     C                   move      o36tfp        wo36tfp           3 0
083000040331      *
083100040331     C                   endsr
083200040331      *----------------------------------------------------------------
083300040331**  ERRORI
083400040331Filiale inesistente !!!!                                              01
083500040331Filiale non in gestione !!!                                           02
083600040331Codice filiale obbligatorio !!!                                       03
083700040331Inserire il Nr. distinta oppure il codice autotrasportatore e la data 04
083800040331Se inserito il codice autotrasportatore inserire la data              05
083900040331Data errata reinserirla correttamente !!!                             06
084000040331Cambio P.O. ammesso solo a filiali di primo livello                   07
084100040401Distinta non disponibile già chiusa o impegnata da altra elaborazione 08
084200060407Elaborazione al momento non disponibile riprovare tra qualche minuto. 09
084300060620Errore in reperimento clienti abilitati all'invio automatico. Tel CED 10
084400060504Non si è abilitati al cliente selezionato!                            11
084500060504Cliente inesistente, correggere.                                      12
084600060606Codice cliente obbligatorio !!!                                       13
