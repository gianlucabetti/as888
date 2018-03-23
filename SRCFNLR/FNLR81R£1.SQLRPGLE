000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300000000      *
000400040331      *  Descrizione   :  Richiesta creazione file dati distinta
000500040331      *                   x scarico su Pc.
000600000000      *****************************************************************
000700040407     Ffnlr81d   cf   e             workstn
000800040331     Fazorg01l  if   E           k disk
000900040331     Ffnarb70l  if   E           k disk
001000040407     Ffnarb01l  if   E           k disk    rename(fnarb000:fnarb1)
001100040401     Ffnfvv01l  if   E           k disk
001200040401     Ffnfvv08l  if   E           k disk    rename(fnfvv000:fnfvv8)
001300040408     Ffnlbl01l  if   E           k disk
001400120607     Ffidst01l  if   E           k disk
001500051115     Ffiar901l  if   E           k disk
001600040408     Ffiar601l  if   E           k disk
001700040408     Ffiari01l  if   E           k disk
001800040402     Ffnart01l  if   E           k disk
001900060606     Fwfdsc01l  uf a e           k disk
002000060407     Ftivgd00f  o    e             disk
002100060407     fTntbe01l  if   e           k disk
002200040402     D nsc             s              7  0 dim(10)
002300040331      *
002400060606     D err             s             70    dim(13) ctdata perrcd(1)
002500040331      *
002600040331     D kpjba         e ds
002700060407     D WFDSCDS       e ds                  extname(wfdsc00f)
002800060410     D DDSC          e ds
002900060410     D TRUL47DS      e ds
003000060928     D TIS781DS      e ds
003100070207     D TIS781DFLO    e ds
003200070305     D DVGDFLO       e ds
003300940926     D*
003400940926     D* DS PER TRUL06R - CARICAMENTO £1
003500040331     D trul06ds      e ds
003600040331     D  lin                    1     90  0
003700040331     D                                     dim(30)                              SKI COMODO
003800040407     d Tibs36Ds      e ds
003900040407     D DSLR01        E DS
004000040407     D*
004100060426     D wlbdat          ds
004200040331     D  g02dat                 1      8  0
004300040331     D  g02inv                 9     16  0
004400040331     D  g02err                17     17
004500040331     D  g02tgi                18     22  0
004600040407     d Wdata8          DS
004700040407     d  dadata                 1      8  0
004800040407     d  adata                  9     16  0
004900040407     d  GioLav                17     21  0
005000060426      *
005100060426      * Passaggio Parametri al pgm TIBS02R
005200060426     d TIBS02ds      e ds                  inz
005300060426     d  T02tla       e                     inz('L')
005400060426     d  T02mod       e                     inz('R')
005500060426     d  T02cod       e                     inz('DSC')
005600060504      *
005700060504      * DS REPERIMENTO DATI UTENTE
005800060504     D BS69DS        E DS                  EXTNAME(TIBS69DS)
005900060504     D ACODS         E DS                  EXTNAME(CNACO00F)
006000060504     D INDDS         E DS                  EXTNAME(CNIND00F)
006100060504     D CLPDS         E DS                  EXTNAME(CNCLP00F)
006200060504     D CLSDS         E DS                  EXTNAME(FNCLS00F)
006300060504      *
006400060504      * DS REPERIMENTO DATI UTENTE
006500060504     D TIBS34DS      E DS                                                       *Profili utente
006600060504     D DDATIUTE      E DS                                                       *Dati utente
006700060504     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006800040407      *
006900040402     d dataiso         s               d   datfmt(*iso)
007000040407     d dataisop        s               d   datfmt(*iso)
007100040407     D savkpjbu        s                   like(kpjbu)
007200060407     D wOGGI           s              8  0
007300120607     D knfv6           s              6  0
007400060407     D wFlgEXE         s              1    inz
007500000000      *
007600040331      *--------------------------------------------------------------*
007700040331      *             M A I N      L I N E
007800040331      *--------------------------------------------------------------*
007900000000      * Inizializzo i campi a video
008000040331     C                   z-add     0             vidpdc                         DATA FINALE
008100040331     C                   Z-ADD     0             viddat                         1' COD.PADRONC.
008200040407     C                   move      *blank        vidndc                         2' COD.PADRONC.
008300040407     C                   move      *zeros        vidndcn                        2' COD.PADRONC.
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
023400040407     C   86              move      vidndc        vidndcn           7 0
023500040401      * Se immesso il Nr distinta verifica che sia disponibile
023600040407     C     vidndcn       ifgt      *zero
023700040407     c                   z-add     vidndcn       dist5
023800040401     c     kfvv          chain     fnfvv01l                           88
023900140819     c***                if        *in88 or fvvfcf <> *blank or fvvatb <> *blank
024000140819     c                   if        *in88 or fvvatb <> *blank
024100040405     C                   seton                                        904128
024200060426     c                   movel     err(8)        $msg
024300040401     C                   goto      endctl
024400040401     C                   endif
024500040401     C                   endif
024600060426     C*
024700060426     C* Verifico se richiesto invio automatico a cliente
024800060606     C                   movel     *blanks       DESKSU
024900060504     C                   eval      T02mod = 'C'
025000060508     C     '?'           scan      VIDKSC                                 10
025100060426     C                   if        *in10
025200060504     C                   eval      T02mod = 'R'
025300060504     C                   endif
025400060504     C*
025500060508     C                   if        *IN10 or VIDKSC > *zeros
025600060508     C  N10              eval      T02KE1 = VIDKSC
025700060504     C   10              eval      T02KE1 = *blanks
025800060504     C                   call      'TIBS02R'
025900060426     C                   parm                    KPJBA
026000060426     C                   parm                    TIBS02ds
026100060426     C                   if        T02err  = *blanks
026200060426     C                   movel     T02uni        DDSC
026300060504     C* X utente nn EDP* verifico che il P.O. utente sia nella schiera d P.O. abilitati x il
026400060504     C* cliente scelto da tabella DSC
026500060504     C                   if        %subst(%trim(KNMUS):1:3) <> 'EDP'
026600060504     C                   if        DUTPOU = §DSCPO001 OR
026700060504     C                             DUTPOU = §DSCPO002 OR
026800060504     C                             DUTPOU = §DSCPO003 OR
026900060504     C                             DUTPOU = §DSCPO004 OR
027000060504     C                             DUTPOU = §DSCPO005 OR
027100060504     C                             DUTPOU = §DSCPO006 OR
027200060504     C                             DUTPOU = §DSCPO007 OR
027300060504     C                             DUTPOU = §DSCPO008 OR
027400060504     C                             DUTPOU = §DSCPO009 OR
027500060504     C                             DUTPOU = §DSCPO010
027600060504     C                   else
027700060504     C                   seton                                        449028
027800060504     C                   movel     err(11)       $msg
027900060606     C                   goto      endctl
028000060504     C                   endif
028100060504     C                   endif
028200060504     C*
028300060508     C                   movel     T02KE1        VIDKSC
028400060508     C                   movel     §DSCKSU       VIHKSU
028500060504     C                   eval      VIHTSC = §DSCTSC
028600060504     C* Decodifico la ragione sociale del cliente
028700060504     C                   exsr      DECTIB
028800120320     C***                if        O69ERR = '1'
028900120320     C***                movel     *all'*'       ACORAG
029000120320     C***                seton                                        449028
029100120320     C***                movel     err(12)       $msg
029200120320     C***                goto      endctl
029300120320     C***                endif
029400120320     C                   movel     ACORAG        DESKSU
029500060504     C*
029600060426     C                   else
029700060504     C                   seton                                        449028
029800060426     C                   movel     err(10)       $msg
029900060508     C                   movel     *all'0'       VIDKSC
030000060426     C                   goto      endctl
030100060426     C                   endif
030200060606     C                   else
030300060606     C                   seton                                        449028
030400060606     C                   movel     err(13)       $msg
030500060606     C                   goto      endctl
030600060504     C                   endif
030700040401      *
030800040331     C     endctl        endsr
030900060504     C*------------------------------------------------------------------------*
031000060504     C* DECTIB - CONTROLLO/DECODIFICA CLIENTE
031100060504     C*------------------------------------------------------------------------*
031200060504     C     DECTIB        BEGSR
031300060504     C*
031400060504     C                   CLEAR                   BS69DS
031500060504     C                   CLEAR                   ACODS
031600060504     C                   MOVEL     KNSIF         I69SIF
031700060508     C                   MOVE(P)   VIDKSC        I69KAC
031800060504     C                   CALL      'TIBS69R'
031900060504     C                   PARM                    BS69DS
032000060504     C                   PARM                    ACODS
032100060504     C                   PARM                    INDDS
032200060504     C                   PARM                    CLPDS
032300060504     C                   PARM                    CLSDS
032400060504     C*
032500060504     C                   ENDSR
032600040331      *---------------------------------------------------------------*
032700040331      * Elaborazione dati distinta e scrittura su file                *
032800040331      *---------------------------------------------------------------*
032900040331     C     elabora       begsr
033000060407     C*
033100060407     C* Inizializzo il flag che indica l'avvenuta effettiva elaborazione
033200060407     C                   movel     'S'           wFlgEXE
033300060407      *
033400040401      * esegue pulizia dati per il P.O da elaborare fino a 2 giorni
033500040401      * precedenti la data del giorno
033600040401     c                   exsr      pulisci
033700060407     C*
033800060928     C* Verifco il blocco elaborazione TIVGD x tipo file corrente: 'DC'
033900060407     C                   clear                   trul47ds
034000070305     C***                eval      d47opz  = 'I'
034100070305     C                   eval      d47opz  = 'C'
034200060426     C                   eval      d47tip  = 'DC'
034300060407     C                   eval      d47lck  = 'N'
034400060407     C                   eval      d47chkj = 'N'
034500060426     C                   eval      d47pgm  = 'FNLR81R'
034600060407     C                   call      'TRUL47R'
034700060407     C                   parm                    trul47ds
034800060407     C*
034900060407     C* Se elaborazione consentita => proseguo
035000060410     C                   if        d47sts <> 'A'
035100040331      *
035200040401     c                   z-add     vidfil        kifp
035300040401      *elaborazione per singola distinta
035400040407     c                   if        vidndcn > 0
035500040407     c                   z-add     vidndcn       dist7
035600040402     c                   exsr      estrai
035700040401     c                   else
035800040401      *elaborazione per autotrasportatore / data
035900040401     c                   movel     vidfil        comodo7           7 0
036000040401     c                   move      vidpdc        comodo7
036100040401     c     kfvv8         setll     fnfvv08l
036200040401     c                   do        *hival
036300040401     c     kfvv8         reade     fnfvv08l                               87
036400040401     c   87              leave
036500120607     c* novo file FIDST con npg in key e foglio lungo 6
036600120607     c                   eval      knfv6=fvvnfv
036700120607     c     kdst          chain     fidst01l
036800120607     c
036900120607     c                   if        %found(fidst01l) and
037000120607     c                             not *in87 and dstpdr = comodo7 and
037100040401     c                             fvvfcf = *blank and fvvatb = *blank
037200040401     c                   z-add     fvvnfv        dist7
037300040401     c                   exsr      estrai
037400040401     c                   endif
037500040401     c                   enddo
037600040401     c                   endif
037700070305     C*
037800070305     C* Se scelto nel lancio invio automatico tramite BartVAS
037900070305     c* aL termine della scrittura dati forzo l'immediata esecuzione dello scarico dei dati
038000070305     C                   if        VIDKSC  > *zeros
038100070305     c                   clear                   tis781ds
038200070305     c                   eval      §781tip = vgdTIP
038300070305     c                   eval      §781ksu = vgdKSU
038400070305     c                   eval      §781tsc = vgdTSC
038500070305     c                   eval      §781dat = vgdDAT
038600070305     c                   eval      §781pgm = vgdPGM
038700070305     c* Imposto la "S" per il cambio del S.I.
038800070305     c                   clear                   TIS781DFLO
038900070305     c                   movel     'S'           §781flocsi
039000080905     c                   movel     'P'           §781floela
039100070305     c                   movel     TIS781DFLO    §781flo
039200070305     c*
039300070305     c                   CALL      'TIS781C1'
039400070305     c                   parm      *blanks       ESITO             1
039500070305     c                   parm                    tis781ds
039600070305     c                   parm      *blanks       vgdprg
039700070305     C*
039800070305     C                   endif
039900070305     C*
040000060407     C                   else
040100060407     C                   movel     'N'           wFlgEXE
040200060407     C                   seton                                        90
040300060407     c                   movel     err(9)        $msg
040400060407     C                   endif
040500060411     C*
040600070305     C* Infine elimino il blocco elaborazione TIVGD x tipo file corrente: 'DC'
040700060411     C                   clear                   trul47ds
040800060411     C                   eval      d47opz  = 'F'
040900060426     C                   eval      d47tip  = 'DC'
041000060411     C                   call      'TRUL47R'
041100060411     C                   parm                    trul47ds
041200040401      *
041300040331     C                   endsr
041400040401      *---------------------------------------------------------------*
041500040401      * estrazione rekord da arb eventuale eliminazione rekord già esistenti
041600040401      *---------------------------------------------------------------*
041700040401     C     estrai        begsr
041800040401      *
041900040401      * verifica se distinta già presente la cancella per riscriverla
042000040401     c     kwfdsc        chain     wfdsc01l                           95
042100040401     c                   if        not *in95
042200040402     C/EXEC SQL
042300040402     C+ DELETE FROM WFDSC00F WHERE dscndc = :dist7 and dscfgs = :vidfil
042400040402     C/END-EXEC
042500040401      *
042600040401     c                   endif
042700040401
042800040401     c     karb          setll     fnarb70l
042900040401     c                   do        *hival
043000040401     c     karb          reade     fnarb70l                               94
043100040401     c   94              leave
043200040401      *
043300040402     c                   clear                   wfdsc000
043400060606     c                   clear                   WFDSCDS
043500040402
043600040408     c                   z-add     vidfil        dscfgs
043700040402     c                   z-add     arbaas        dscaas
043800040402     c                   z-add     arblnp        dsclnp
043900040402     c                   z-add     arbnrs        dscnrs
044000040402     c                   z-add     arbnsp        dscnsp
044100040402     c                   move      arbfns        dscfns
044200040402     c                   z-add     arblna        dsclna
044300040402     c                   move      arbznc        dscznc
044400040402     c                   move      arbrsm        dscrsm
044500040402     c                   move      arbinm        dscinm
044600040402     c                   move      arbcam        dsccam
044700040402     c                   move      arblom        dsclom
044800040402     c                   move      arbprm        dscprm
044900040402     c                   move      arbfap        dscfap
045000040402     c                   move      arbnzm        dscnzm
045100040402     c                   move      arbrsd        dscrsd
045200040402     c                   move      arbind        dscind
045300040402     c                   move      arbcad        dsccad
045400040402     c                   move      arblod        dsclod
045500040402     c                   move      arbprd        dscprd
045600040402     c                   move      arbnzd        dscnzd
045700040402     c                   move      arbfin        dscfin
045800040402     c                   move      arbtsp        dsctsp
045900040402     c                   move      arbfbr        dscfbr
046000040402     c                   z-add     arbncl        dscncl
046100081124     C* Peso: regola CML
046200081124     C     arbncp        ifeq      arbncl
046300081124     C                   z-add     arbpkc        dscpkf
046400081124     C                   else
046500081124     C     arbpkc        ifge      arbpkf
046600081124     C                   z-add     arbpkc        dscpkf
046700081124     C                   else
046800081124     C                   z-add     arbpkf        dscpkf
046900081124     C                   end
047000081124     C                   endif
047100081124     C* Volume: regola CML
047200081124     C     arbfvf        ifeq      'T'
047300081124     C                   z-add     arbvlf        dscvlf
047400081124     C                   else
047500081124     C     arbncr        ifeq      arbncl
047600081124     C                   z-add     arbvlc        dscvlf
047700081124     C                   else
047800081124     C     arbvlc        ifge      arbvlf
047900081124     C                   z-add     arbvlc        dscvlf
048000081124     C                   else
048100081124     C                   z-add     arbvlf        dscvlf
048200081124     C                   endif
048300081124     C                   endif
048400081124     C                   endif
048500081124     c*
048600040402     c                   z-add     arbrmn        dscrmn
048700040402     c                   move      arbrma        dscrma
048800040402     c                   move      arbtcr        dsctcr
048900040402     c                   z-add     arbdcr        dscdcr
049000040402     c                   z-add     arbhcr        dschcr
049100040402     c                   z-add     arbpdc        dscpdc
049200040402     c                   z-add     arbndc        dscndc
049300040402     c                   z-add     arbddc        dscddc
049400040402     c                   move      arbtc1        dsctc1
049500040402     c                   move      arbtc2        dsctc2
049600040402     c                   move      arbgga        dscgga
049700040402     c                   move      arbgma        dscgma
049800040402     c                   move      arbgva        dscgva
049900040402     c                   move      arbngd        dscngd
050000040402     c                   move      arbnss        dscnss
050100040407      * verifico se c'è assegnato da incassare
050200040407     c                   if        arbicc = 'R' or arbicc = ' '
050300040407     c     kar6          chain     fiar601l
050400040407     c                   if        %found(fiar601l)
050500040408     c                   z-add     ar6ift        importo          13 3
050600040408      * incasso parziale
050700040408     c                   if        arbfip = 'I'
050800040408     c     kari          chain     fiari01l
050900040408     c* Se esiste record determino importo residuo da incassare
051000040408     c                   if        %found(fiari01l)
051100040408     c                   sub       ariipp        importo
051200040408     c                   endif
051300040408     c                   endif
051400040407     c                   z-add     ar6fiv        dscfiv
051500040407     c                   z-add     ar6nft        dscnft
051600040407     c                   z-add     ar6dft        dscdft
051700040408     c                   z-add     importo       dscift
051800040407     c                   move      ar6div        dscdiv
051900040407     c                   endif
052000040408     c                   endif
052100040705      * salvo i campi chiave della figlia
052200040705     c                   z-add     arbaas        kaas
052300040705     c                   z-add     arblnp        klnp
052400040705     c                   z-add     arbnrs        knrs
052500040705     c                   z-add     arbnsp        knsp
052600040705      * pulizia data consegna bolla mamma
052700040705     c                   clear                   arbdcmmam
052800040407      * verifico se esiste  mamma
052900040408     c     kar           chain     fnlbl01l
053000040407      * se trovo il legame leggo arb con in chiave la mamma
053100040407      * per agganciare segnacolli e contrassegno
053200040408     c                   if        %found(fnlbl01l) and lbllan = lbllap
053300040407     c     karb1         chain     fnarb01l                           84
053400040705      * memorizzo data consegna mamma se presente
053500040705     c                   move      arbdcm        arbdcmmam         8 0
053600040407     c                   end
053700040407      * verifico se c'è contrassegno e se è da incassare
053800040407     c                   if        arbicc = 'R' or arbicc = ' '
053900051115     c     kar           chain     fiar901l
054000051115     c                   if        %found(fiar901l)
054100040407     c                   move      ar9tic        dsctic
054200040407     c                   z-add     ar9cas        dsccas
054300040407     c                   move      ar9vca        dscvca
054400040407     c                   endif
054500040408     c                   endif
054600040705      * se esiste un legame e la data consegna della mamma è maggiore di 0
054700040705      * mi riposiziono sulla figlia per il reperimento dei segnacolli
054800040705     c                   if        arbdcmmam > 0
054900040705     c     karb1f        chain     fnarb01l                           84
055000040705     c                   endif
055100040705
055200040705      * segnacolli
055300040408     c                   movea     *zeros        nsc
055400040408     c                   if        arbdcm = 0
055500040408     c                   if        arbdcp = 0
055600040402     c                   z-add     arbncd        dscncd
055700040402     c                   z-add     arbnca        dscnca
055800040408     c                   endif
055900040402     c                   z-add     arbfls        dscfls
056000040408     c                   if        arbncd = 0 and arbnca = 0 or arbdcp > 0
056100040408     c     kar           setll     fnart01l
056200040407     c                   z-add     0             i
056300040407     c                   do        *hival
056400040408     c     kar           reade     fnart01l                               91
056500040407      * fine file o massimo 10 colli
056600040407     c                   if        *in91 or i = 10
056700040407     c                   leave
056800040407     c                   endif
056900040407      * inserisco solo i colli non consegnati
057000040407     c                   if        artdcm = 0
057100040407     c                   add       1             i                 3 0
057200040402     c                   z-add     artnsc        nsc(i)
057300040407     c                   else
057400040407     c                   iter
057500040407     c                   endif
057600040402     c                   enddo
057700040408     c                   endif
057800040408     c                   endif
057900040402     c                   z-add     nsc(1)        dscs01
058000040402     c                   z-add     nsc(2)        dscs02
058100040402     c                   z-add     nsc(3)        dscs03
058200040402     c                   z-add     nsc(4)        dscs04
058300040402     c                   z-add     nsc(5)        dscs05
058400040402     c                   z-add     nsc(6)        dscs06
058500040402     c                   z-add     nsc(7)        dscs07
058600040402     c                   z-add     nsc(8)        dscs08
058700040402     c                   z-add     nsc(9)        dscs09
058800040402     c                   z-add     nsc(10)       dscs10
058900040406     c                   move      wdatg         dscdge
059000040402     c                   movel     wora          dschmc
059100060605     c***                write     wfdsc000
059200060426     C*
059300060426     C* Se scelto nel lancio invio automatico tramite BartVAS => eseguo
059400060606     C                   if        VIDKSC  > *zeros
059500060407     C*
059600060407     C* Scrivo il dato sul TIVGD x lo scarico automatico dei dati tramite BartVAS
059700060407     C                   clear                   tivgd000
059800080404     C                   eval      vgdDTA = %subst(WFDSCDS:1:%size(WFDSCDS))
059900060407     C                   eval      vgdTIP = 'DC'
060000060426     C                   eval      vgdKSU = *all'0'
060100060508     C                   move      VIHKSU        vgdKSU
060200060426     C                   eval      vgdTSC = VIHTSC
060300060407     C                   eval      vgdDAT = wOGGI
060400060407     C                   eval      vgdPGM = 'FNLR81R'
060500070305     C                   clear                   DVGDFLO
060600070305     C                   movel     'P'           §VGDFELA
060700070305     C                   movel     DVGDFLO       vgdflo
060800060407     C                   write     tivgd000
060900060407     C                   endif
061000060407     C*
061100040401     c                   enddo
061200040401      *
061300040401     C                   endsr
061400040401      *---------------------------------------------------------------*
061500040401      * Pulizia elimina rekord più vecchi di  3 giorni x il p.o da elaborare
061600040401      *---------------------------------------------------------------*
061700040401     C     pulisci       begsr
061800040401      *
061900040401     C/EXEC SQL
062000040406     C+ DELETE FROM WFDSC00F WHERE dscdge < :datapul and dscfgs = :vidfil
062100050825     C+ or dscdge < :datapulm
062200040401     C/END-EXEC
062300040401      *
062400040401     C                   endsr
062500060504     C*--------------------------------------------------------------------------------------------*
062600060504     C* REPERISCE I DATI UTENTE
062700060504     C*--------------------------------------------------------------------------------------------*
062800060504     C     REPDATIUTE    BEGSR
062900060504     C*
063000060504     C* INIZIALIZZA VARIABILI DI WRK
063100060504     C                   CLEAR                   TIBS34DS
063200060504     C                   CLEAR                   AZUTEDS
063300060504     C                   CLEAR                   DDATIUTE
063400060504     C*
063500060504     C     *DTAARA       DEFINE    §azute        azuteds
063600060504     C     *DTAARA       DEFINE    §datiute      ddatiute
063700060504     C                   IN(E)     *DTAARA
063800060504     C                   IF        %Error
063900060504     c                   EVAL      I34Tla = 'L'
064000060504     C                   CALL      'TIBS34R'
064100060504     C                   PARM                    Tibs34Ds
064200060504     C                   IN        *DTAARA
064300060504     C                   ENDIF
064400060504     C*
064500060504     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
064600060504     C                   MOVEL(P)  RSUT          DSFIRS           20
064700060504     C*
064800060504     C                   ENDSR
064900040331      *---------------------------------------------------------------*
065000040331      * Definizioni/dichiarazioni iniziali                            *
065100040331      *---------------------------------------------------------------*
065200040331     C     *inzsr        begsr
065300040331      *
065400040331     C     *entry        plist
065500040331     C                   parm                    kpjba
065600060504     C*
065700060504     C* Reperimento dati utente
065800060504     C                   exsr      REPDATIUTE
065900060407     C*
066000060407     C* Giro la data odierna
066100060426     C                   move      *date         g02dat
066200060426     C                   movel     *blanks       g02err
066300060407     C                   call      'XSRDA8'
066400060426     C                   parm                    wlbdat
066500060426     C                   if        g02err = '1'
066600060407     C                   z-add     0             wOGGI
066700060407     C                   else
066800060426     C                   z-add     g02inv        wOGGI
066900060407     C                   endif
067000040407      *profilo di filiale attivo indicatore di protezione campi non manutenzionabili
067100040407     C                   CLEAR                   Tibs36Ds
067200040407     C                   EVAL      I36ute = knmus
067300040407     C                   CALL      'TIBS36R'
067400040407     C                   PARM                    tibs36ds
067500040401     C                   time                    w0120            14 0
067600040401     C                   movel     w0120         wora              6 0
067700040401     C                   move      w0120         wdat              8 0
067800040401     C*
067900040407     C* UDATE A 8 IN AAAA/MM/GG
068000040401     C                   z-add     wdat          g02dat
068100040401     C                   movel     *blank        g02err
068200040401     C                   call      'XSRDA8'
068300040401     C                   parm                    wlbdat
068400040406     C                   move      g02inv        wdatg             8 0
068500040407     C                   move      g02inv        dataiso
068600040407     C                   move      g02inv        dataisop
068700040407      * ricava data pulizia archivio 2 giorni lavorativi precedenti
068800040407     c                   do        *hival
068900040407     c                   move      dataisop      dadata
069000040407     c                   move      dataiso       adata
069100040407     c                   CALL      'XSRLAV8'
069200040407     c                   PARM                    Wdata8
069300040407      * se tentano un estrazione di sabato o domenica forzo la data pulizia
069400040407     c                   if        giolav = 0
069500040407     c     dataisop      subdur    3:*d          dataisop
069600040407     c                   leave
069700040407     c                   endif
069800040407     c                   if        giolav > 3
069900040407     c                   leave
070000040407     c                   else
070100040407     c     dataisop      subdur    1:*d          dataisop
070200040407     c                   endif
070300040407     c                   enddo
070400040407     c                   move      dataisop      datapul           8 0
070500050825      * più vecchie di un mese
070600050825     c     dataisop      subdur    1:*m          dataisop
070700050825     c                   move      dataisop      datapulm          8 0
070800040401     C***
070900040401     c     karb          klist
071000040401     c                   kfld                    kifp              9 0
071100040401     c                   kfld                    dist7             7 0
071200040407     C***
071300040407     c     karb1         klist
071400040408     c                   kfld                    lblaap
071500040408     c                   kfld                    lbllpp
071600040408     c                   kfld                    lblnrp
071700040407     c                   kfld                    lblnsp
071800040705     C***
071900040705     c     karb1f        klist
072000040705     c                   kfld                    kaas              4 0
072100040705     c                   kfld                    klnp              3 0
072200040705     c                   kfld                    knrs              2 0
072300040705     c                   kfld                    knsp              7 0
072400040401     C***
072500040401     c     kwfdsc        klist
072600040401     c                   kfld                    vidfil
072700040401     c                   kfld                    dist7
072800040331     C***
072900040401     c     kfvv          klist
073000040401     c                   kfld                    categ             1 0
073100040401     c                   kfld                    dist5             5 0
073200040401     c                   kfld                    vidfil
073300040401     c                   z-add     4             categ
073400040401     C***
073500040401     c     kfvv8         klist
073600040401     c                   kfld                    vidfil
073700040401     c                   kfld                    categ
073800040401     c                   kfld                    viddatg
073900040401     C***
074000040401     c     kdst          klist
074100120607     c                   kfld                    fvvnpg
074200120607     c                   kfld                    knfv6
074300040405     c                   kfld                    fvvfgs
074400040402     C***
074500040408     c     kar           klist
074600040407     c                   kfld                    arbaas
074700040407     c                   kfld                    arblnp
074800040407     c                   kfld                    arbnrs
074900040407     c                   kfld                    arbnsp
075000040402     C***
075100040402     c     kar6          klist
075200040402     c                   kfld                    arbaas
075300040402     c                   kfld                    arblnp
075400040402     c                   kfld                    arbnrs
075500040402     c                   kfld                    arbnsp
075600040402     c                   kfld                    tirec             1
075700040402     c                   eval      tirec = '1'
075800040408     C***
075900040408     c     kari          klist
076000040408     c                   kfld                    arbaas
076100040408     c                   kfld                    arblnp
076200040408     c                   kfld                    arbnrs
076300040408     c                   kfld                    arbnsp
076400040408     c                   kfld                    ktip              1
076500040408     c                   eval      ktip  = 'A'
076600040408
076700040331     C* CARICO TABELLA FILIALI GESTITE £1
076800040331     C***
076900040407     C                   movel     kpjbu         savkpjbu
077000040405     C                   clear                   trul06ds
077100040331     C                   move      '£1'          d06cod
077200040407     C                   movel     o36tfp        d06key
077300040405     C                   movel     trul06ds      kpjbu
077400040331     C                   call      'TRUL06R'
077500040331     C                   parm                    kpjba
077600040405     C                   movel     kpjbu         trul06ds
077700040407     C                   movel     savkpjbu      kpjbu
077800040331      *
077900040407     C                   eval      vidfil = o36pou
078000040407     C                   move      o36tfp        wo36tfp           3 0
078100040331      *
078200040331     C                   endsr
078300040331      *----------------------------------------------------------------
078400040331**  ERRORI
078500040331Filiale inesistente !!!!                                              01
078600040331Filiale non in gestione !!!                                           02
078700040331Codice filiale obbligatorio !!!                                       03
078800040331Inserire il Nr. distinta oppure il codice autotrasportatore e la data 04
078900040331Se inserito il codice autotrasportatore inserire la data              05
079000040331Data errata reinserirla correttamente !!!                             06
079100040331Cambio P.O. ammesso solo a filiali di primo livello                   07
079200040401Distinta non disponibile già chiusa o impegnata da altra elaborazione 08
079300060407Elaborazione al momento non disponibile riprovare tra qualche minuto. 09
079400060620Errore in reperimento clienti abilitati all'invio automatico. Tel CED 10
079500060504Non si è abilitati al cliente selezionato!                            11
079600060504Cliente inesistente, correggere.                                      12
079700060606Codice cliente obbligatorio !!!                                       13
