000100030718     H DECEDIT('0,') DATEDIT(*DMY.)  OPTION(*NODEBUGIO)
000200140919     H* FNLRY09R *---------------------------------------------------*
000300140919     H* - Driver di controllo presenza / esecuzione indicazioni di cons.
000301110610     h*
000400000000     H*--------------------------------------------------------------*
000500140919     FTIIDC05l  IF   E           K DISK
000501140929     FTIIDC01l  IF   E           K DISK    rename(tiidc000:tiidc001)
001200030722     F**
001600070313     d
001800140919     D fnlry09ds     E DS
001801140919     D fnlry03ds     E DS
001802140919     D kpjba         E DS
002000070313     d
002001140929     d WELADATA        s                   like(IDCELADATA) INZ(0)
002004140206
003800900518     C***************************************************************
003900000000     C     *ENTRY        PLIST
004000140919     C                   PARM                    fnlry09ds
004300940223     C*---------------------------------------------------------------*
004400140922     C* TIPO LANCIO, ilry09TLA "C" -           CHIUSO CON LR
004500140922     C* TIPO LANCIO, ilry09TLA "L" - ELABORO E CHIUDO CON LR
004600140922     C* TIPO LANCIO, ilry09TLA " " - ELABORO E CHIUDO IN RETRN
004700941216     C*
004800140919    1C     ilry09tla     IFNE      'C'
004900140919     c                   clear                   OLry09esit
005000140919     c                   clear                   OLry09err
005001140919     c                   clear                   OLry09msg
005400070412     c
005500051123     c
005501140919     c* controlli formali dei parametri ricevuti
005502140919     c                   exsr      sr_chkform
005503140919
005504140919     c                   if        olry09err=' '
005505140919     c                   select
005900140919     c* Richiesto check
005901140922     c                   when      ilry09tch='C'
005902140919     c                   exsr      sr_Check
005903140919     c* Richiesta esecuzione indicazioni
005904140922     c                   when      ilry09tch='E'
005905140919     c                   exsr      sr_esegui
005906140919     c                   endsl
005907140919     c*
012001140205    1c                   endif
012002140919    1c                   endif
012100051123     c
012200140922    1C     ilry09tla     IFNE      ' '
012400051123     C                   SETON                                        LR
012500051123     C                   ELSE
012600051123     C                   SETON                                        RT
012700140205    1C                   ENDIF
012701140206     C**************************************************************************
012702140919     C     sr_chkform    BEGSR
012703140919     c* tipo chiamata obbligatorio "C" o "E"
012704140919     c                   if        ilry09tch<> 'C' and
012705140919     c                             ilry09tch<> 'E'
012706140919     c                   eval      olry09err='1'
012707140919     c                   eval      olry09msg='Errore tipo chiamata'
012708140922     c                   leavesr
012709140919     c                   endif
012710140929     c* Filiale gestione obbligatoria se non passato il numero di spedizione
012711140929     c                   if        ilry09fgs=0 and ilry09nsp=0
012712140919     c                   eval      olry09err='2'
012713140930     c                   eval      olry09msg='Filiale gestione o numero spedi+
012714140930     c                             zione obbligatorio'
012715140922     c                   leavesr
012716140919     c                   endif
012727140206     c                   endsr
012728140206     C**************************************************************************
012729140919     C     sr_check      BEGSR
012730140919     c                   if        ilry09nsp>0
012731140929     c                   eval      olry09esit='0'
012732140929     c     kidc1         setll     tiidc01l
012733140929     c     kidc1         reade     tiidc01l
012734140929     c                   dow       not %eof(tiidc01l) and olry09esit='0'
012735140929     c                   if        idcelaflg='E'and
012736140929     c                             (ilry09fgs=0 or ilry09fgs=idcfgs)
012737140929     c                   eval      olry09esit='1'
012738140929     c                   endif
012739140929     c     kidc1         reade     tiidc01l
012740140929     c                   enddo
012741140919     c                   else
012742140919     c     ilry09fgs     setll     tiidc05l
012743140929     c                   if        %equal(tiidc05l)
012744140929     c                   eval      olry09esit='1'
012745140929     c                   else
012746140929     c                   eval      olry09esit='0'
012747140929     c                   endif
012748140919     c                   endif
012749140206     c                   endsr
012750140919     C**************************************************************************
012751140919     C     sr_esegui     BEGSR
012752140919     c                   if        ilry09nsp>0
012753140929     c     kidc1         setll     tiidc01l
012754140929     c     kidc1         reade     tiidc01l
012755140929     c                   dow       not %eof(tiidc01l)
012756140919     c
012757140929     C                   IF        IDCELAFLG='E' and
012758140929     c                             (ilry09fgs=0 or ilry09fgs=idcfgs)
012759140919     c                   clear                   fnlry03ds
012760140919     c                   eval      ilry03aas=idcaas
012761140919     c                   eval      ilry03lnp=idclnp
012762140919     c                   eval      ilry03nrs=idcnrs
012763140919     c                   eval      ilry03nsp=idcnsp
012764140919     c                   eval      ilry03prg=idcprg
012765140919     c                   eval      ilry03tipd=idctipodis
012766140919     c                   call      'FNLRY03R'
012767140919     c                   parm                    kpjba
012768140919     c                   parm                    fnlry03ds
012769140929     c                   endif
012770140929
012771140929     c     kidc1         reade     tiidc01l
012772140919     c                   enddo
012773140919     c                   else
012774140919     c                   call      'FNLRY02R'
012775140919     c                   parm                    kpjba
012776140919     c                   endif
012777140919     c                   endsr
012800991206     C**************************************************************************
012900051123     C     *INZSR        BEGSR
013000051123     c
013100140919     c     kidc5         Klist
013200140919     c                   Kfld                    ilry09fgs
013300140919     c                   Kfld                    ilry09aas
013400140919     c                   Kfld                    ilry09lnp
013401140919     c                   Kfld                    ilry09nrs
013402140919     c                   Kfld                    ilry09nsp
013403140929     c     kidc1         Klist
013405140929     c                   Kfld                    ilry09aas
013406140929     c                   Kfld                    ilry09lnp
013407140929     c                   Kfld                    ilry09nrs
013408140929     c                   Kfld                    ilry09nsp
013409140929     c                   Kfld                    WELADATA
013410140206     c
015300070313     c                   endsr
