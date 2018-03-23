000100040107      *---------------------------------------------------------------*
000200090320      * Gestione note clienti/potenziali/trattative commerciali       *
000300040107      *---------------------------------------------------------------*
000400040107
000500040108     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600040107
000700040107      *---------------------------------------------------------------*
000800040107
001000040112      *
001100090414     ftrmk20d1  cf   e             workstn
001300040107
001400040107      *---------------------------------------------------------------*
001500040107
001600040107      *
003400040107     d KPJBA         e ds
003500040108      * - Parametri
003600090323     d trmk20ds      e ds
003900040108      *
004000040108      * - Parametri x Controllo profilo utenti
004100090414     d TIBS34DS      e ds                  inz
004501090402     d §azute        e ds                  extname(azute00f)
004502090402     d                                     dtaara
004503090402     d §datiute      e ds                  extname(ddatiute)
004504090402     d                                     dtaara
004505090414     d Status         sds
004506090414     d  SDSpgm           *proc
009300040107      *
012316081023
012317090402      // ? PROTOTIPI ?
012318090402      /copy gaitrasrc/srcprotopr,tibs34r
014600040107
014700040107      *===============================================================*
014800040107      *  RIEPILOGO INDICATORI                                         *
014900040107      *---------------------------------------------------------------*
016400040109      *  90    - Errore                                               *
016500040107      *===============================================================*
016600040107
016700040107     c     *Entry        plist
016800040107     c                   parm                    KPJBA
017503090403
017505090414     c                   dow       *inlr=*off
017506090414     C                   exfmt     mk20t01
017507090414     c                   if        *inkc=*on
017508090414     c                   eval      *inlr=*on
017509090414     c                   leave
017510090414     c                   endif
017511090414     c                   clear                   trmk20ds
017512090414     c                   eval      imk20tla=vtla
017513090414     c                   eval      imk20flm=vflm
017514090511     c                   eval      imk20fno=vfno
017515090423     c                   eval      imk20f10=vf10
017516090414     c                   eval      IMK20CPO=vcpo
017517090414     c                   eval      IMK20KSC=vksc
017518090518     c                   eval      IMK20tat=vtat
017519090518     c                   eval      IMK20atn=vatn
017520090518     c                   eval      IMK20atnP=vatnp
017521090414     c                   eval      IMK20NRV=vnrv
017522090414     c                   eval      IMK20RSC=vrsc
017523090414     c                   eval      EMK20NO1=vnot1
017524090414     c                   eval      EMK20NO2=vnot2
017527090414     c                   eval      EMK20DIM=vdim
017528090414     c                   eval      EMK20HIM=vhim
017529090414     c                   eval      EMK20PRU=vpru
017530090414     c                   eval      EMK20POS=vpos
017531090414     c                   eval      EMK20SNS=vsns
017532090414     c                   clear                   oMK20PIU
017533090414     c                   clear                   oMK20FXX
017534090414     c                   clear                   OMK20ERR
017535090414     c                   clear                   OMK20MSG
017537090414     c                   call      'TRMK20R'
017538090414     c                   parm                    kpjba
017539090414     c                   parm                    trmk20ds
017540090414     c                   eval      vnot1=EMK20NO1
017541090414     c                   eval      vnot2=EMK20NO2
017544090414     c                   eval      vdim=EMK20DIM
017545090414     c                   eval      vhim=EMK20HIM
017546090414     c                   eval      vpru=EMK20PRu
017547090414     c                   eval      vpos=EMK20POS
017548090414     c                   eval      vsns=EMK20SNS
017549090414     c                   eval      vpiu=oMK20PIU
017550090414     c                   eval      vfxx=oMK20FXX
017551090414     c                   eval      verr=OMK20ERR
017552090414     c                   eval      vmsg=OMK20MSG
017553090414     c                   enddo
024700040108
024701090414      /free
024800090402       //-------------------------------------------------------------*
024900090402       //Routine Iniziale                                             *
025000090402       //-------------------------------------------------------------*
025100090414       begsr *inzsr;
025200090402
025300090402       //Reperisco dati job
025400090402       exsr datijob;
025500090402       vtcpgm=SDSpgm;
025600090402
025700090402       EndSr;
025800040108
025900090402       //-------------------------------------------------------------*
026000090402       //Reperimento Dati del job (Utente/Operativi)                  *
026100090402       //-------------------------------------------------------------*
026200090402       Begsr DatiJob;
026201090403
026301090402          in(e) §azute;
026302090402          if not %error;
026303090402           in(e) §datiute;
026304090402          endif;
026305090402          if %error or rsut = *blanks;
026306090402           tibs34r(tibs34ds);
026307090402           in §azute;
026308090402           in §datiute;
026309090402          endif;
026310090402       endsr;
