000100000725     H DECEDIT('0,') DATEDIT(*DMY.)
000200060929     h dftactgrp(*no) actgrp(*caller)
000300061213      * Fidg02r *----------------------------------------------------*
000400070320      *      GESTIONE estensione ANAGRAFICA AUTOTRASP./distinte consegna
000500911204      *--------------------------------------------------------------*
000600071128     FFicn88D   CF   E             WORKSTN
001700061002     ddtaiso           s               d
001800061002     ddtaeur           s               d   datfmt(*eur)
001801071128     dkpjba          e ds
001802071128     d* Parametri x Controllo profilo utenti
001804071128     d DDatiUte      e ds
001805071128     d TIBS34ds      e ds
001806071128     d AZUTEds       e ds                  extname(AZUTE00F)
001807071128     D PARAM           DS
001808071128     d  data1                         8s 0
001809071128     d  data2                         8s 0
012300060926     C**********************************************************************
012400060926     c*
012600070115     c                   WRITE     dg02T01
016200060926     c                   do        *hival
016400070125     c                   WRITE     dg02T01
016500061213     C                   EXFMT     dg02D01
016600071128     C* FINE LAVORO
016700060926     C   KC              leave
016701071128     c* confronto le due date
016702071128     c                   move      vdta1         dtaiso
016703071128     c                   move      dtaiso        data1
016704071128     c                   move      vdta2         dtaiso
016705071128     c                   move      dtaiso        data2
016706071128     c                   setoff                                       99
016707071128     c                   if        data1 > data2
016708071128     c                   seton                                        99
016709071128     c                   iter
016710071128     c                   end
016711071128     c                   if        *inkf
016712071128     c                   movel(p)  param         kpjbu
016713071128     c                   call      'FICN87R'
016714071128     c                   parm                    kpjba
016715071128     c                   leave
016716071128     c                   end
016717071128     c                   enddo
016800071128     C                   SETON                                        LR
016900071128     c*-------------------------------------------------
017000071128     c     *inzsr        begsr
017100071128     c*-------------------------------------------------
017200071128     c     *entry        plist
017300071128     c                   parm                    kpjba
017400071128     c                   movel     kpjbu         param
017401071128      * Reperisco dati job
017402071128     c                   exsr      DatiJob
017403071128     C                   MOVEL     RSUT          VDSA
019400071128     c                   endsr
019500071128      *---------------------------------------------------------------*
019600071128      * Reperimento Dati del job (Utente/Operativi)                   *
019700071128      *---------------------------------------------------------------*
019800071128     c     DatiJob       BEGSR
019900071128      *
020000071128     c     *dtaara       define    §azute        azuteds
020100071128     c     *dtaara       define    §datiute      ddatiute
020200071128      *
020300071128     c                   in(E)     *dtaara
020400071128     c                   IF        %ERROR or RSUT = *blanks
020500071128     c                   clear                   Tibs34Ds
020600071128     c                   call      'TIBS34R'
020700071128     c                   parm                    Tibs34Ds
020800071128     c                   in        *dtaara
020900071128     c                   ENDIF
021000071128      *
021100071128     c                   ENDSR
