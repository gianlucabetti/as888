000100040406     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200000000     H DECEDIT('0,') DATEDIT(*DMY.)
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
001500040401     Ffndst01l  if   E           k disk
001600051115     Ffiar901l  if   E           k disk
001700040408     Ffiar601l  if   E           k disk
001800040408     Ffiari01l  if   E           k disk
001900090309     Ffiart01l  if   E           k disk
002000060606     Fwfdsc01l  uf a e           k disk
002100060407     Ftivgd00f  o    e             disk
002200060407     fTntbe01l  if   e           k disk
002300040402     D nsc             s              7  0 dim(10)
002400040331      *
002500060606     D err             s             70    dim(13) ctdata perrcd(1)
002600040331      *
002700040331     D kpjba         e ds
002800060407     D WFDSCDS       e ds                  extname(wfdsc00f)
002900060410     D DDSC          e ds
003000060410     D TRUL47DS      e ds
003100060928     D TIS781DS      e ds
003200070207     D TIS781DFLO    e ds
003300070305     D DVGDFLO       e ds
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
015500040407     C     Wo36tfp       ifne      0
015600040331     C     vidfil        lookup    Lin                                    99
015700040405     C  n99              seton                                        904028
015800040331     c  n99              movel     err(2)        $msg
015900040331     C  n99              goto      endctl
016000040331     C                   endif
016100020308     c                   else
016200040405     C                   seton                                        904028
016300040331     c                   movel     err(3)        $msg
016400040331     C                   goto      endctl
016500040331     C                   endif
016600000000      *
016700000000      * Controlli di congruenza
016800040331      * Se immesso il cd.autotras. non può essere inserito il numero dist.
016900040407     C     vidndc        ifne      *zero
017000040407     C     vidndc        andne     *blank
017100040407     C     vidpdc        andgt     *zero
017200040331     C                   seton                                        904142
017300040405     C                   seton                                        28
017400040331     c                   movel     err(4)        $msg
017500040331     C                   goto      endctl
017600040331     C                   endif
017700040331      * immettere o distinta o autotrasportatore
017800040331     C     vidndc        ifeq      *zero
017900040331     C     vidpdc        andeq     *zero
018000040407     C     vidndc        oreq      *blank
018100040407     C     vidpdc        andeq     *zero
018200040331     C                   seton                                        904142
018300040405     C                   seton                                        28
018400040331     c                   movel     err(4)        $msg
018500040331     C                   goto      endctl
018600040331     C                   endif
018700040331     C     vidpdc        ifgt      *zero
018800040331     C     viddat        andeq     *zero
018900040331     C                   seton                                        904243
019000040405     C                   seton                                        28
019100040331     c                   movel     err(5)        $msg
019200040331     C                   goto      endctl
019300040331     C                   endif
019400000000      *
019500000000      * Controllo data iniziale
019600040331     C     viddat        ifgt      0
019700040331     C                   z-add     viddat        g02dat
019800040331     C                   movel     *blank        g02err
019900040331     C                   call      'XSRDA8'
020000040331     C                   parm                    wlbdat
020100040331     C     g02err        ifeq      '1'
020200040405     C                   seton                                        439028
020300040331     c                   movel     err(6)        $msg
020400040331     C                   goto      endctl
020500040331     C                   endif
020600040331     C                   z-add     g02inv        viddatg           8 0
020700040331     C                   z-add     g02dat        viddat
020800020307     c                   endif
020900000000      *
021000040401      * Controlli di congruenza
021100040407     C*  Controllo il numero distinta
021200040407     C     vidndc        ifne      *zeros
021300040407     C     vidndc        andne     *blank
021400040407     C     '?'           scan      vidndc                                 86
021500040407     C     *in86         ifeq      '1'
021600040407     C                   clear                   dslr01
021700040407     C                   movel     'FNLR81R'     dlrpgm
021800040407     C                   movel     '2'           dlrtfv
021900040407     C                   z-add     4             dlrnpg
022000040407     C                   z-add     vidfil        dlrfgs
022100040407     C                   movel     'S'           dlrric
022200040407     C                   z-add     wdatg         dlrada
022300040407     C                   movel(p)  dslr01        kpjbu
022400040407     C                   call      'FNLR02R'
022500040407     C                   parm                    kpjba
022600040407     C                   movel     kpjbu         dslr01
022700040407     C                   move      dlrnfv        vidndc
022800040407     C                   endif
022900040407     C                   endif
023000040407     C*
023100040407     C                   testn                   vidndc               86
023200040407     C   86              move      vidndc        vidndcn           7 0
023300040401      * Se immesso il Nr distinta verifica che sia disponibile
023400040407     C     vidndcn       ifgt      *zero
023500040407     c                   z-add     vidndcn       dist5
023600040401     c     kfvv          chain     fnfvv01l                           88
023700040401     c                   if        *in88 or fvvfcf <> *blank or fvvatb <> *blank
023800040405     C                   seton                                        904128
023900060426     c                   movel     err(8)        $msg
024000040401     C                   goto      endctl
024100040401     C                   endif
024200040401     C                   endif
024300060426     C*
024400060426     C* Verifico se richiesto invio automatico a cliente
024500060606     C                   movel     *blanks       DESKSU
024600060504     C                   eval      T02mod = 'C'
024700060508     C     '?'           scan      VIDKSC                                 10
024800060426     C                   if        *in10
024900060504     C                   eval      T02mod = 'R'
025000060504     C                   endif
025100060504     C*
025200060508     C                   if        *IN10 or VIDKSC > *zeros
025300060508     C  N10              eval      T02KE1 = VIDKSC
025400060504     C   10              eval      T02KE1 = *blanks
025500060504     C                   call      'TIBS02R'
025600060426     C                   parm                    KPJBA
025700060426     C                   parm                    TIBS02ds
025800060426     C                   if        T02err  = *blanks
025900060426     C                   movel     T02uni        DDSC
026000060504     C* X utente nn EDP* verifico che il P.O. utente sia nella schiera d P.O. abilitati x il
026100060504     C* cliente scelto da tabella DSC
026200060504     C                   if        %subst(%trim(KNMUS):1:3) <> 'EDP'
026300060504     C                   if        DUTPOU = §DSCPO001 OR
026400060504     C                             DUTPOU = §DSCPO002 OR
026500060504     C                             DUTPOU = §DSCPO003 OR
026600060504     C                             DUTPOU = §DSCPO004 OR
026700060504     C                             DUTPOU = §DSCPO005 OR
026800060504     C                             DUTPOU = §DSCPO006 OR
026900060504     C                             DUTPOU = §DSCPO007 OR
027000060504     C                             DUTPOU = §DSCPO008 OR
027100060504     C                             DUTPOU = §DSCPO009 OR
027200060504     C                             DUTPOU = §DSCPO010
027300060504     C                   else
027400060504     C                   seton                                        449028
027500060504     C                   movel     err(11)       $msg
027600060606     C                   goto      endctl
027700060504     C                   endif
027800060504     C                   endif
027900060504     C*
028000060508     C                   movel     T02KE1        VIDKSC
028100060508     C                   movel     §DSCKSU       VIHKSU
028200060504     C                   eval      VIHTSC = §DSCTSC
028300060504     C* Decodifico la ragione sociale del cliente
028400060504     C                   exsr      DECTIB
028500060504     C                   if        O69ERR = '1'
028600060606     C                   movel     *all'*'       ACORAG
028700060606     C                   seton                                        449028
028800060504     C                   movel     err(12)       $msg
028900060606     C                   goto      endctl
029000060504     C                   endif
029100060504     C                   movel     ACORAG        DESKSU
029200060504     C*
029300060426     C                   else
029400060504     C                   seton                                        449028
029500060426     C                   movel     err(10)       $msg
029600060508     C                   movel     *all'0'       VIDKSC
029700060426     C                   goto      endctl
029800060426     C                   endif
029900060606     C                   else
030000060606     C                   seton                                        449028
030100060606     C                   movel     err(13)       $msg
030200060606     C                   goto      endctl
030300060504     C                   endif
030400040401      *
030500040331     C     endctl        endsr
030600060504     C*------------------------------------------------------------------------*
030700060504     C* DECTIB - CONTROLLO/DECODIFICA CLIENTE
030800060504     C*------------------------------------------------------------------------*
030900060504     C     DECTIB        BEGSR
031000060504     C*
031100060504     C                   CLEAR                   BS69DS
031200060504     C                   CLEAR                   ACODS
031300060504     C                   MOVEL     KNSIF         I69SIF
031400060508     C                   MOVE(P)   VIDKSC        I69KAC
031500060504     C                   CALL      'TIBS69R'
031600060504     C                   PARM                    BS69DS
031700060504     C                   PARM                    ACODS
031800060504     C                   PARM                    INDDS
031900060504     C                   PARM                    CLPDS
032000060504     C                   PARM                    CLSDS
032100060504     C*
032200060504     C                   ENDSR
032300040331      *---------------------------------------------------------------*
032400040331      * Elaborazione dati distinta e scrittura su file                *
032500040331      *---------------------------------------------------------------*
032600040331     C     elabora       begsr
032700060407     C*
032800060407     C* Inizializzo il flag che indica l'avvenuta effettiva elaborazione
032900060407     C                   movel     'S'           wFlgEXE
033000060407      *
033100040401      * esegue pulizia dati per il P.O da elaborare fino a 2 giorni
033200040401      * precedenti la data del giorno
033300040401     c                   exsr      pulisci
033400060407     C*
033500060928     C* Verifco il blocco elaborazione TIVGD x tipo file corrente: 'DC'
033600060407     C                   clear                   trul47ds
033700070305     C***                eval      d47opz  = 'I'
033800070305     C                   eval      d47opz  = 'C'
033900060426     C                   eval      d47tip  = 'DC'
034000060407     C                   eval      d47lck  = 'N'
034100060407     C                   eval      d47chkj = 'N'
034200060426     C                   eval      d47pgm  = 'FNLR81R'
034300060407     C                   call      'TRUL47R'
034400060407     C                   parm                    trul47ds
034500060407     C*
034600060407     C* Se elaborazione consentita => proseguo
034700060410     C                   if        d47sts <> 'A'
034800040331      *
034900040401     c                   z-add     vidfil        kifp
035000040401      *elaborazione per singola distinta
035100040407     c                   if        vidndcn > 0
035200040407     c                   z-add     vidndcn       dist7
035300040402     c                   exsr      estrai
035400040401     c                   else
035500040401      *elaborazione per autotrasportatore / data
035600040401     c                   movel     vidfil        comodo7           7 0
035700040401     c                   move      vidpdc        comodo7
035800040401     c     kfvv8         setll     fnfvv08l
035900040401     c                   do        *hival
036000040401     c     kfvv8         reade     fnfvv08l                               87
036100040401     c   87              leave
036200040401     c     kdst          chain     fndst01l
036300040405     c                   if        not *in87 and dstpdr = comodo7 and
036400040401     c                             fvvfcf = *blank and fvvatb = *blank
036500040401     c                   z-add     fvvnfv        dist7
036600040401     c                   exsr      estrai
036700040401     c                   endif
036800040401     c                   enddo
036900040401     c                   endif
037000070305     C*
037100070305     C* Se scelto nel lancio invio automatico tramite BartVAS
037200070305     c* aL termine della scrittura dati forzo l'immediata esecuzione dello scarico dei dati
037300070305     C                   if        VIDKSC  > *zeros
037400070305     c                   clear                   tis781ds
037500070305     c                   eval      §781tip = vgdTIP
037600070305     c                   eval      §781ksu = vgdKSU
037700070305     c                   eval      §781tsc = vgdTSC
037800070305     c                   eval      §781dat = vgdDAT
037900070305     c                   eval      §781pgm = vgdPGM
038000070305     c* Imposto la "S" per il cambio del S.I.
038100070305     c                   clear                   TIS781DFLO
038200070305     c                   movel     'S'           §781flocsi
038300080905     c                   movel     'P'           §781floela
038400070305     c                   movel     TIS781DFLO    §781flo
038500070305     c*
038600070305     c                   CALL      'TIS781C1'
038700070305     c                   parm      *blanks       ESITO             1
038800070305     c                   parm                    tis781ds
038900070305     c                   parm      *blanks       vgdprg
039000070305     C*
039100070305     C                   endif
039200070305     C*
039300060407     C                   else
039400060407     C                   movel     'N'           wFlgEXE
039500060407     C                   seton                                        90
039600060407     c                   movel     err(9)        $msg
039700060407     C                   endif
039800060411     C*
039900070305     C* Infine elimino il blocco elaborazione TIVGD x tipo file corrente: 'DC'
040000060411     C                   clear                   trul47ds
040100060411     C                   eval      d47opz  = 'F'
040200060426     C                   eval      d47tip  = 'DC'
040300060411     C                   call      'TRUL47R'
040400060411     C                   parm                    trul47ds
040500040401      *
040600040331     C                   endsr
040700040401      *---------------------------------------------------------------*
040800040401      * estrazione rekord da arb eventuale eliminazione rekord già esistenti
040900040401      *---------------------------------------------------------------*
041000040401     C     estrai        begsr
041100040401      *
041200040401      * verifica se distinta già presente la cancella per riscriverla
041300040401     c     kwfdsc        chain     wfdsc01l                           95
041400040401     c                   if        not *in95
041500040402     C/EXEC SQL
041600040402     C+ DELETE FROM WFDSC00F WHERE dscndc = :dist7 and dscfgs = :vidfil
041700040402     C/END-EXEC
041800040401      *
041900040401     c                   endif
042000040401
042100040401     c     karb          setll     fnarb70l
042200040401     c                   do        *hival
042300040401     c     karb          reade     fnarb70l                               94
042400040401     c   94              leave
042500040401      *
042600040402     c                   clear                   wfdsc000
042700060606     c                   clear                   WFDSCDS
042800040402
042900040408     c                   z-add     vidfil        dscfgs
043000040402     c                   z-add     arbaas        dscaas
043100040402     c                   z-add     arblnp        dsclnp
043200040402     c                   z-add     arbnrs        dscnrs
043300040402     c                   z-add     arbnsp        dscnsp
043400040402     c                   move      arbfns        dscfns
043500040402     c                   z-add     arblna        dsclna
043600040402     c                   move      arbznc        dscznc
043700040402     c                   move      arbrsm        dscrsm
043800040402     c                   move      arbinm        dscinm
043900040402     c                   move      arbcam        dsccam
044000040402     c                   move      arblom        dsclom
044100040402     c                   move      arbprm        dscprm
044200040402     c                   move      arbfap        dscfap
044300040402     c                   move      arbnzm        dscnzm
044400040402     c                   move      arbrsd        dscrsd
044500040402     c                   move      arbind        dscind
044600040402     c                   move      arbcad        dsccad
044700040402     c                   move      arblod        dsclod
044800040402     c                   move      arbprd        dscprd
044900040402     c                   move      arbnzd        dscnzd
045000040402     c                   move      arbfin        dscfin
045100040402     c                   move      arbtsp        dsctsp
045200040402     c                   move      arbfbr        dscfbr
045300040402     c                   z-add     arbncl        dscncl
045501081124     C* Peso: regola CML
045502081124     C     arbncp        ifeq      arbncl
045503081124     C                   z-add     arbpkc        dscpkf
045504081124     C                   else
045506081124     C     arbpkc        ifge      arbpkf
045507081124     C                   z-add     arbpkc        dscpkf
045508081124     C                   else
045509081124     C                   z-add     arbpkf        dscpkf
045510081124     C                   end
045512081124     C                   endif
045513081124     C* Volume: regola CML
045523081124     C     arbfvf        ifeq      'T'
045524081124     C                   z-add     arbvlf        dscvlf
045525081124     C                   else
045526081124     C     arbncr        ifeq      arbncl
045527081124     C                   z-add     arbvlc        dscvlf
045529081124     C                   else
045530081124     C     arbvlc        ifge      arbvlf
045531081124     C                   z-add     arbvlc        dscvlf
045532081124     C                   else
045533081124     C                   z-add     arbvlf        dscvlf
045534081124     C                   endif
045535081124     C                   endif
045536081124     C                   endif
045537081124     c*
045600040402     c                   z-add     arbrmn        dscrmn
045700040402     c                   move      arbrma        dscrma
045800040402     c                   move      arbtcr        dsctcr
045900040402     c                   z-add     arbdcr        dscdcr
046000040402     c                   z-add     arbhcr        dschcr
046100040402     c                   z-add     arbpdc        dscpdc
046200040402     c                   z-add     arbndc        dscndc
046300040402     c                   z-add     arbddc        dscddc
046400040402     c                   move      arbtc1        dsctc1
046500040402     c                   move      arbtc2        dsctc2
046600040402     c                   move      arbgga        dscgga
046700040402     c                   move      arbgma        dscgma
046800040402     c                   move      arbgva        dscgva
046900040402     c                   move      arbngd        dscngd
047000040402     c                   move      arbnss        dscnss
047100040407      * verifico se c'è assegnato da incassare
047200040407     c                   if        arbicc = 'R' or arbicc = ' '
047300040407     c     kar6          chain     fiar601l
047400040407     c                   if        %found(fiar601l)
047500040408     c                   z-add     ar6ift        importo          13 3
047600040408      * incasso parziale
047700040408     c                   if        arbfip = 'I'
047800040408     c     kari          chain     fiari01l
047900040408     c* Se esiste record determino importo residuo da incassare
048000040408     c                   if        %found(fiari01l)
048100040408     c                   sub       ariipp        importo
048200040408     c                   endif
048300040408     c                   endif
048400040407     c                   z-add     ar6fiv        dscfiv
048500040407     c                   z-add     ar6nft        dscnft
048600040407     c                   z-add     ar6dft        dscdft
048700040408     c                   z-add     importo       dscift
048800040407     c                   move      ar6div        dscdiv
048900040407     c                   endif
049000040408     c                   endif
049100040705      * salvo i campi chiave della figlia
049200040705     c                   z-add     arbaas        kaas
049300040705     c                   z-add     arblnp        klnp
049400040705     c                   z-add     arbnrs        knrs
049500040705     c                   z-add     arbnsp        knsp
049600040705      * pulizia data consegna bolla mamma
049700040705     c                   clear                   arbdcmmam
049800040407      * verifico se esiste  mamma
049900040408     c     kar           chain     fnlbl01l
050000040407      * se trovo il legame leggo arb con in chiave la mamma
050100040407      * per agganciare segnacolli e contrassegno
050200040408     c                   if        %found(fnlbl01l) and lbllan = lbllap
050300040407     c     karb1         chain     fnarb01l                           84
050400040705      * memorizzo data consegna mamma se presente
050500040705     c                   move      arbdcm        arbdcmmam         8 0
050600040407     c                   end
050700040407      * verifico se c'è contrassegno e se è da incassare
050800040407     c                   if        arbicc = 'R' or arbicc = ' '
050900051115     c     kar           chain     fiar901l
051000051115     c                   if        %found(fiar901l)
051100040407     c                   move      ar9tic        dsctic
051200040407     c                   z-add     ar9cas        dsccas
051300040407     c                   move      ar9vca        dscvca
051400040407     c                   endif
051500040408     c                   endif
051600040705      * se esiste un legame e la data consegna della mamma è maggiore di 0
051700040705      * mi riposiziono sulla figlia per il reperimento dei segnacolli
051800040705     c                   if        arbdcmmam > 0
051900040705     c     karb1f        chain     fnarb01l                           84
052000040705     c                   endif
052100040705
052200040705      * segnacolli
052300040408     c                   movea     *zeros        nsc
052400040408     c                   if        arbdcm = 0
052500040408     c                   if        arbdcp = 0
052600040402     c                   z-add     arbncd        dscncd
052700040402     c                   z-add     arbnca        dscnca
052800040408     c                   endif
052900040402     c                   z-add     arbfls        dscfls
053000040408     c                   if        arbncd = 0 and arbnca = 0 or arbdcp > 0
053100090309     c     kar           setll     fiart01l
053200040407     c                   z-add     0             i
053300040407     c                   do        *hival
053400090309     c     kar           reade     fiart01l                               91
053500040407      * fine file o massimo 10 colli
053600040407     c                   if        *in91 or i = 10
053700040407     c                   leave
053800040407     c                   endif
053900040407      * inserisco solo i colli non consegnati
054000040407     c                   if        artdcm = 0
054100040407     c                   add       1             i                 3 0
054200040402     c                   z-add     artnsc        nsc(i)
054300040407     c                   else
054400040407     c                   iter
054500040407     c                   endif
054600040402     c                   enddo
054700040408     c                   endif
054800040408     c                   endif
054900040402     c                   z-add     nsc(1)        dscs01
055000040402     c                   z-add     nsc(2)        dscs02
055100040402     c                   z-add     nsc(3)        dscs03
055200040402     c                   z-add     nsc(4)        dscs04
055300040402     c                   z-add     nsc(5)        dscs05
055400040402     c                   z-add     nsc(6)        dscs06
055500040402     c                   z-add     nsc(7)        dscs07
055600040402     c                   z-add     nsc(8)        dscs08
055700040402     c                   z-add     nsc(9)        dscs09
055800040402     c                   z-add     nsc(10)       dscs10
055900040406     c                   move      wdatg         dscdge
056000040402     c                   movel     wora          dschmc
056100060605     c***                write     wfdsc000
056200060426     C*
056300060426     C* Se scelto nel lancio invio automatico tramite BartVAS => eseguo
056400060606     C                   if        VIDKSC  > *zeros
056500060407     C*
056600060407     C* Scrivo il dato sul TIVGD x lo scarico automatico dei dati tramite BartVAS
056700060407     C                   clear                   tivgd000
056800080404     C                   eval      vgdDTA = %subst(WFDSCDS:1:%size(WFDSCDS))
056900060407     C                   eval      vgdTIP = 'DC'
057000060426     C                   eval      vgdKSU = *all'0'
057100060508     C                   move      VIHKSU        vgdKSU
057200060426     C                   eval      vgdTSC = VIHTSC
057300060407     C                   eval      vgdDAT = wOGGI
057400060407     C                   eval      vgdPGM = 'FNLR81R'
057500070305     C                   clear                   DVGDFLO
057600070305     C                   movel     'P'           §VGDFELA
057700070305     C                   movel     DVGDFLO       vgdflo
057800060407     C                   write     tivgd000
057900060407     C                   endif
058000060407     C*
058100040401     c                   enddo
058200040401      *
058300040401     C                   endsr
058400040401      *---------------------------------------------------------------*
058500040401      * Pulizia elimina rekord più vecchi di  3 giorni x il p.o da elaborare
058600040401      *---------------------------------------------------------------*
058700040401     C     pulisci       begsr
058800040401      *
058900040401     C/EXEC SQL
059000040406     C+ DELETE FROM WFDSC00F WHERE dscdge < :datapul and dscfgs = :vidfil
059100050825     C+ or dscdge < :datapulm
059200040401     C/END-EXEC
059300040401      *
059400040401     C                   endsr
059500060504     C*--------------------------------------------------------------------------------------------*
059600060504     C* REPERISCE I DATI UTENTE
059700060504     C*--------------------------------------------------------------------------------------------*
059800060504     C     REPDATIUTE    BEGSR
059900060504     C*
060000060504     C* INIZIALIZZA VARIABILI DI WRK
060100060504     C                   CLEAR                   TIBS34DS
060200060504     C                   CLEAR                   AZUTEDS
060300060504     C                   CLEAR                   DDATIUTE
060400060504     C*
060500060504     C     *DTAARA       DEFINE    §azute        azuteds
060600060504     C     *DTAARA       DEFINE    §datiute      ddatiute
060700060504     C                   IN(E)     *DTAARA
060800060504     C                   IF        %Error
060900060504     c                   EVAL      I34Tla = 'L'
061000060504     C                   CALL      'TIBS34R'
061100060504     C                   PARM                    Tibs34Ds
061200060504     C                   IN        *DTAARA
061300060504     C                   ENDIF
061400060504     C*
061500060504     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
061600060504     C                   MOVEL(P)  RSUT          DSFIRS           20
061700060504     C*
061800060504     C                   ENDSR
061900040331      *---------------------------------------------------------------*
062000040331      * Definizioni/dichiarazioni iniziali                            *
062100040331      *---------------------------------------------------------------*
062200040331     C     *inzsr        begsr
062300040331      *
062400040331     C     *entry        plist
062500040331     C                   parm                    kpjba
062600060504     C*
062700060504     C* Reperimento dati utente
062800060504     C                   exsr      REPDATIUTE
062900060407     C*
063000060407     C* Giro la data odierna
063100060426     C                   move      *date         g02dat
063200060426     C                   movel     *blanks       g02err
063300060407     C                   call      'XSRDA8'
063400060426     C                   parm                    wlbdat
063500060426     C                   if        g02err = '1'
063600060407     C                   z-add     0             wOGGI
063700060407     C                   else
063800060426     C                   z-add     g02inv        wOGGI
063900060407     C                   endif
064000040407      *profilo di filiale attivo indicatore di protezione campi non manutenzionabili
064100040407     C                   CLEAR                   Tibs36Ds
064200040407     C                   EVAL      I36ute = knmus
064300040407     C                   CALL      'TIBS36R'
064400040407     C                   PARM                    tibs36ds
064500040401     C                   time                    w0120            14 0
064600040401     C                   movel     w0120         wora              6 0
064700040401     C                   move      w0120         wdat              8 0
064800040401     C*
064900040407     C* UDATE A 8 IN AAAA/MM/GG
065000040401     C                   z-add     wdat          g02dat
065100040401     C                   movel     *blank        g02err
065200040401     C                   call      'XSRDA8'
065300040401     C                   parm                    wlbdat
065400040406     C                   move      g02inv        wdatg             8 0
065500040407     C                   move      g02inv        dataiso
065600040407     C                   move      g02inv        dataisop
065700040407      * ricava data pulizia archivio 2 giorni lavorativi precedenti
065800040407     c                   do        *hival
065900040407     c                   move      dataisop      dadata
066000040407     c                   move      dataiso       adata
066100040407     c                   CALL      'XSRLAV8'
066200040407     c                   PARM                    Wdata8
066300040407      * se tentano un estrazione di sabato o domenica forzo la data pulizia
066400040407     c                   if        giolav = 0
066500040407     c     dataisop      subdur    3:*d          dataisop
066600040407     c                   leave
066700040407     c                   endif
066800040407     c                   if        giolav > 3
066900040407     c                   leave
067000040407     c                   else
067100040407     c     dataisop      subdur    1:*d          dataisop
067200040407     c                   endif
067300040407     c                   enddo
067400040407     c                   move      dataisop      datapul           8 0
067500050825      * più vecchie di un mese
067600050825     c     dataisop      subdur    1:*m          dataisop
067700050825     c                   move      dataisop      datapulm          8 0
067800040401     C***
067900040401     c     karb          klist
068000040401     c                   kfld                    kifp              9 0
068100040401     c                   kfld                    dist7             7 0
068200040407     C***
068300040407     c     karb1         klist
068400040408     c                   kfld                    lblaap
068500040408     c                   kfld                    lbllpp
068600040408     c                   kfld                    lblnrp
068700040407     c                   kfld                    lblnsp
068800040705     C***
068900040705     c     karb1f        klist
069000040705     c                   kfld                    kaas              4 0
069100040705     c                   kfld                    klnp              3 0
069200040705     c                   kfld                    knrs              2 0
069300040705     c                   kfld                    knsp              7 0
069400040401     C***
069500040401     c     kwfdsc        klist
069600040401     c                   kfld                    vidfil
069700040401     c                   kfld                    dist7
069800040331     C***
069900040401     c     kfvv          klist
070000040401     c                   kfld                    categ             1 0
070100040401     c                   kfld                    dist5             5 0
070200040401     c                   kfld                    vidfil
070300040401     c                   z-add     4             categ
070400040401     C***
070500040401     c     kfvv8         klist
070600040401     c                   kfld                    vidfil
070700040401     c                   kfld                    categ
070800040401     c                   kfld                    viddatg
070900040401     C***
071000040401     c     kdst          klist
071100040401     c                   kfld                    fvvnfv
071200040405     c                   kfld                    fvvfgs
071300040402     C***
071400040408     c     kar           klist
071500040407     c                   kfld                    arbaas
071600040407     c                   kfld                    arblnp
071700040407     c                   kfld                    arbnrs
071800040407     c                   kfld                    arbnsp
071900040402     C***
072000040402     c     kar6          klist
072100040402     c                   kfld                    arbaas
072200040402     c                   kfld                    arblnp
072300040402     c                   kfld                    arbnrs
072400040402     c                   kfld                    arbnsp
072500040402     c                   kfld                    tirec             1
072600040402     c                   eval      tirec = '1'
072700040408     C***
072800040408     c     kari          klist
072900040408     c                   kfld                    arbaas
073000040408     c                   kfld                    arblnp
073100040408     c                   kfld                    arbnrs
073200040408     c                   kfld                    arbnsp
073300040408     c                   kfld                    ktip              1
073400040408     c                   eval      ktip  = 'A'
073500040408
073600040331     C* CARICO TABELLA FILIALI GESTITE £1
073700040331     C***
073800040407     C                   movel     kpjbu         savkpjbu
073900040405     C                   clear                   trul06ds
074000040331     C                   move      '£1'          d06cod
074100040407     C                   movel     o36tfp        d06key
074200040405     C                   movel     trul06ds      kpjbu
074300040331     C                   call      'TRUL06R'
074400040331     C                   parm                    kpjba
074500040405     C                   movel     kpjbu         trul06ds
074600040407     C                   movel     savkpjbu      kpjbu
074700040331      *
074800040407     C                   eval      vidfil = o36pou
074900040407     C                   move      o36tfp        wo36tfp           3 0
075000040331      *
075100040331     C                   endsr
075200040331      *----------------------------------------------------------------
075300040331**  ERRORI
075400040331Filiale inesistente !!!!                                              01
075500040331Filiale non in gestione !!!                                           02
075600040331Codice filiale obbligatorio !!!                                       03
075700040331Inserire il Nr. distinta oppure il codice autotrasportatore e la data 04
075800040331Se inserito il codice autotrasportatore inserire la data              05
075900040331Data errata reinserirla correttamente !!!                             06
076000040331Cambio P.O. ammesso solo a filiali di primo livello                   07
076100040401Distinta non disponibile già chiusa o impegnata da altra elaborazione 08
076200060407Elaborazione al momento non disponibile riprovare tra qualche minuto. 09
076300060620Errore in reperimento clienti abilitati all'invio automatico. Tel CED 10
076400060504Non si è abilitati al cliente selezionato!                            11
076500060504Cliente inesistente, correggere.                                      12
076600060606Codice cliente obbligatorio !!!                                       13
