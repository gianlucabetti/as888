000100950524     H DECEDIT('0,') DATEDIT(*DMY.)
000102070205     h dftactgrp(*no) actgrp(*caller)
000103070410     H* FIDG29R *----------------------------------------------------*
000104070410     H*         Elimina riferimenti distinta da ARG se ARBNDC=0
000105070322     H*--------------------------------------------------------------*
000201070222     FFidg18p   O    E             PRINTER OFLIND(*IN80)
000601070410     FFiarg52j  iF   E           K DISK
000602070410     FFiarg01l  uF   E           K DISK    commit rename(fiarg000:fiarg001)
000603070523     F                                     infds(arginfds)
000604071107     FAZORG01L  IF   E           K DISK
000800950524      *--------------------------------------------------------------*
000900950524      *   DS
001000950524      *--------------------------------------------------------------*
001001070523     d arginfds        ds
001002070523     d  argnrr               397    400i 0
001003070523     D trul82ds      e ds
001102050520     D ddatiute      e ds
001200030910     D azuteds       e ds                  extname(AZUTE00F)
001300030910     D tibs34ds      E DS                  inz
001301100202     D dargflo       E DS                  inz
001400950524     D KPJBA         E DS
001500070205     D Fidg20ds      E DS
001501070410     ddtaiso           s               d
001502070410     ddtaeur           s               d   datfmt(*eur)
001503070523     D Errmsg          S             78    DIM(1) CTDATA PERRCD(1)              MSG DI ERRORE
001504071107     D OG147         E DS                  INZ
001505071107     D OG150         E DS                  INZ
002000950524     C*--------------------------------------------------------------*
002100950524     C*  Ciclo principale
002200950524     C*--------------------------------------------------------------*
004092070410     c* leggo le spedizioni senza numero distinta e non ancora consegnate
004093070410     c* per pulire riferimenti di ARG
004095070410     c     kargj         setgt     fiarg52j
004096070313     c                   do        *hival
004097070523     c     d20fgsi       reade     fiarg52j
004103070410     C                   if        %eof(fiarg52j)
004104070313     c                   leave
004105070313     c                   end
004106070410     c* controllo il numero distinta su ARB ... deve essere = 0
004107070410     c                   if        arbndc <> 0
004108070410     c                   iter
004109070410     c                   end
004110070410     c* aggiorno ARG
004111070410     c     karg          chain(e)  fiarg01l
004112070410     c                   if        %error
004113070523     c* reperisco il RRN del file
004114070523     c     karg          chain(n)  Fiarg01l
004115070523     c                   if        %found(Fiarg01l)
004116070523     c                   exsr      geslck
004117070523     c                   if        ul82§sts = 'A'
004118070410     c                   seton                                        83
004119070410     c                   exsr      srstampa
004120070410     c                   exsr      srfine
004121070528     c                   else
004122070528     c     karg          chain     Fiarg01l
004123070410     c                   end
004124070523     c                   end
004125070523     c                   end
004126070410     c                   if        %found(fiarg01l)
004127100202     c                   eval      dargflo = argflo
004128100202     c                   clear                   §argattesa
004129100202     c                   eval      argflo = dargflo
004130070410     c                   eval      argndc = 0
004131070410     c                   eval      argpdc = 0
004132070410     c                   eval      argfgs = 0
004133070410     c                   eval      argddc = 0
004134070410     c                   eval      argstp = 0
004135070410     c                   eval      argnftl= 0
004136070410     c                   eval      argslb = 0
004137070410     c                   eval      argdtvdis= 0
004138070410     c                   eval      arghvdis= 0
004139070410     c                   update    fiarg001
004140070410     c                   end
004141070313     c                   enddo
006400950525     C*
006401070206     c                   if        d20cmti= 'S'
006402070205     c                   commit
006403070205     c                   end
006404070323     c                   eval      d20pag = pages
006405070323     c                   eval      d20riga= riga
006406070205     c*
006407070205     c                   eval      kpjbu = fidg20ds
006500950524     C                   SETON                                        LR
010600950524     C*
013300950525     C*--------------------------------------------------------------*
013400070205     C*  Operazioni finali in caso di errore
013500950525     C*--------------------------------------------------------------*
013600070205     C     srfine        BEGSR
013601070206     c                   if        d20cmtI= 'S'
013602070205     c                   rolbk
013603070205     c                   end
013604070206     c                   eval      d20erro= '1'
013605070205     c                   eval      kpjbu = fidg20ds
013606070205     c                   seton                                        lr
013607070205     c                   return
013608070205     c                   endsr
013609070222     C*------------------------------------------------------------------------*
013610070222     C* stampa
013611070222     C*------------------------------------------------------------------------*
013612070222     C     srstampa      BEGSR
013613071107     c* stampo solo per assegnaz. automatica
013614071107     c                   if        kcdaz= 'DG17' or kcdaz='DG18'
013615070222     c                   move      d20dfvi       dtaiso
013616070222     c                   move      dtaiso        dtaeur
013617070222     c                   move      dtaeur        prtdfv
013618071107     c* decodifico parametri di lancio
013619071210     c                   if        d20ldcri <> 0
013620071210     c                   move      d20ldcri      dtaiso
013621071107     c                   move      dtaiso        dtaeur
013622071210     c                   move      dtaeur        decldcr
013623071210     c                   else
013624071210     c                   clear                   decldcr
013625071210     c                   end
013626071210     c                   if        *in71
013627071210     c                   move      d20dtai       dtaiso
013628071210     c                   move      dtaiso        dtaeur
013629071210     c                   move      dtaeur        decdta
013630071107     c                   eval      decora = d20orai
013631071107     c                   end
013632071107     c                   if        d20cleari = 'S'
013633071107     c                   eval      decclear='SI'
013634071107     c                   else
013635071107     c                   eval      decclear='NO'
013636071107     c                   end
013637150430     c                   if        d20p88i = 'N'
013638150515     c                   eval      decp88r='Spediz. pistola 88 NO'
013639150515     c                   else
013640150515     c                   clear                   decp88r
013642150430     c                   end
013643071107     c                   if        *in70
013644071107     c                   select
013645071107     c                   when      d20tpmi ='M'
013646071107     c                   eval      dectpm ='MESSAGGERIE'
013647071107     c                   when      d20tpmi ='P'
013648071107     c                   eval      dectpm ='PARCEL'
013649071107     c                   when      d20tpmi =' '
013650071107     c                   eval      dectpm ='TUTTE'
013651071107     c                   endsl
013652071107     c                   end
013653071107     c                   select
013654071107     c                   when      d20fppi ='M'
013655071107     c                   eval      decfpp ='MAT'
013656071107     c                   when      d20fppi ='P'
013657071107     c                   eval      decfpp ='POM'
013658071107     c                   when      d20fppi =' '
013659071107     c                   eval      decfpp ='TUTTE'
013660071107     c                   endsl
013661070323     C* PAGINA IN STAMPA
013662070323     C                   IF        *IN80 OR RIGA >=60
013663070323     C                   SETON                                        80
013664070323     C                   ADD       1             PAGES
013665071107     C                   Z-ADD     5             RIGA              3 0
013666070323     C                   END
013667070222     c*
013668070222     c   80              write     testa
013669071107     c   80              write     testap
013670070222     c                   setoff                                       80
013671070222     c   83              write     riga3
013672070323     C   83              ADD       1             RIGA
013673070410     c                   setoff                                       83
013674071107     c                   end
013675070222     c*
013676070222     c                   endsr
013677070205     C*--------------------------------------------------------------*
013678070205     C*  Operazioni iniziali
013679070205     C*--------------------------------------------------------------*
013680070205     C     *INZSR        BEGSR
013700950525     C*
013800950525     C     *ENTRY        PLIST
013900950525     C                   PARM                    KPJBA
014000070205     C                   MOVEL(P)  KPJBU         Fidg20ds
014001070206     c                   clear                   d20erro
014002070326     c                   eval      pages = d20pag
014003070326     c                   eval      riga = d20riga
014100030718     c*
014200030718     c     *dtaara       define    §azute        azuteds
014300030718     c     *dtaara       define    §datiute      ddatiute
014400030718     C                   in(E)     *dtaara
014500030718     C                   IF        %Error  or  RSUT = *blanks
014600030718     C                   call      'TIBS34R'
014700030718     C                   parm                    Tibs34Ds
014800030718     C                   in        *dtaara
014900030718     c                   ENDIF
015000950525     C*  Definisco chiavi di accesso
015401070410     C     Kargj         KLIST
015402070410     C                   KFLD                    d20fgsi
015404070410     C                   KFLD                    argcgi
015407070410     C     Karg          KLIST
015408070410     C                   KFLD                    arbaas
015409070410     C                   KFLD                    arblnp
015410070410     C                   KFLD                    arbnrs
015411070410     C                   KFLD                    arbnsp
015412071107     C     d20FGSi       CHAIN     AZORG01l
015413071107    1C                   IF        %found(azorg01l)
015414071107     c                   movel     orgdf0        og150
015415071107     C                   MOVEL     ORGDE7        og147
015416071107     c     §OGPKM        comp      'S'                                    70
015417071107     c     §OGdoada      comp      'S'                                    71
015418071107     c                   end
015419070410     c*
017100950525     C                   ENDSR
017200070523     C*----------------------------------------------------*
017300070523     C     geslck        begsr
017400070523     C*----------------------------------------------------*
017500070523     c                   clear                   trul82ds
017600070523     c                   Eval      ul82§rrn = argnrr
017700070523     c                   Eval      UL82§FIL = 'FIARG00F'
017800070523     c                   Eval      UL82§WIN = 'S'
018000070523     c                   Eval      UL82§num = 2
018100070523     c                   Eval      UL82§att = 2
018200070523     c                   eval      ul82§mss = ErrMsg(1)
018300070523     c                   Eval      UL82§msw = 'La Spedizione '
018400070523     c                             + %editc(arglnp:'Z') +  ' '
018500070523     c                             + %editc(argnrs:'Z') +  ' '
018600070523     c                             + %editc(argnsp:'Z') +  ' è già utilizza ta -
018700070523     c                             da un altro lavoro e non è manutenzionabile'
018800070523      * Effettuo la chiamata al *PGM d utilità
018900070523     c                   Call(e)   'TRUL82R'
019000070523     c                   Parm                    Trul82ds
019100070523
019200070523     C                   ENDSR
019300070523** ErrMsg
019400070523Si sta bloccando l'assegnazione DISTINTE: SI PREGA DI USCIRE dal lavoro!       27
