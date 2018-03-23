000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200070326     H*
000300070508     h* elimina blocco fbc per le bolle con dcr x renderle consegnabili
000400141126ab   H*   e per quelle che sono state dirottate in consegna
000500000000     H*--------------------------------------------------------------*
000600130225     Ffnarb54l  uf   e           k disk
000700141126     Fazorg01l  if   e           k disk
000800141126ab   Ffiarg01l  if   e           k disk
000900950125     D*
001000070302     D kpjba         E DS
001100070508     D fidg17ds      E DS
001200141126ab   D fnlry09ds2    E DS
001300070508     D og146         E DS
001400070508     D trul06ds      E DS
001500070508     D  lin                    1     90  0 dim(30)
001600070508
001700070508     D kpjbus          s                   like(kpjbu)
001800130225     D datak           s                   like(arbdcm)
001900130225     D lineak          s                   like(arblna)
002000070508     D x               s              3  0
002100070508      *___________________________________________________
002200070508     c* se in organigramma non è ancora impostata nessuna data esco
002300070508     c                   if        §ogdda = ' '
002400070508     c                   goto      fine
002500070508     c                   else
002600070508     c                   move      §ogdda        comdda            8 0
002700070508     c                   end
002800070508     c* se data passata in DS minore di data in organigramma esco il PO non
002900070508     c* è ancora partito con la gestione distinte automatiche
003000070508     c                   if        d17dfvi < comdda
003100070508     c                   goto      fine
003200070508     c                   endif
003300070508
003400070508      * posizionamento su ARB x aggiornamento per ogni filiale da gestire
003500070508     c                   do        30            x
003600070508     c                   if        lin(x) <> 0
003700070508     c                   exsr      elabARB
003800070508     c                   endif
003900070508     c                   enddo
004000070226
004100070226     c     fine          tag
004200070226     c                   seton                                        lr
004300070226      *___________________________________________________
004400070226     C     elabARB       begsr
004500070226      *___________________________________________________
004600130225     c                   z-add     lin(x)        lineak
004700130225     c     karb          setll     fnarb54l
004800070226     c                   do        *hival
004900130225     c     karb          reade     fnarb54l
005000141126      *-
005100130225     c                   if        %eof(fnarb54l)
005200070226     c                   leave
005300070226     c                   endif
005400141126ab    *-
005500141126ab    **  Sblocco eventuali bolle DIROTTATE
005600141126ab   c                   clear                   se_dirottata      1
005700141126ab    *-
005800141126ab    *-      che hanno una mancata consegna
005900141126ab   c                   if        arbCMC <> *blank and arbFBC <> *blank
006000141126ab    *-
006100141126ab    *-  solo se su giro DIR
006200141126ab   c     karg          chain     fiarg01l
006300141126ab   c                   if        %Found(fiarg01l) and argcgi = 'DIR        '
006400141126ab    *-
006500141126ab    *-  controlla se ci sono delle disposizioni
006600141126ab   c                   clear                   fnlry09ds2
006700141126ab   c                   eval      ilry09tch = 'C'
006800141126ab   c                   eval      ilry09tdis= 'D'
006900141126ab   c                   eval      ilry09fgs = arblna
007000141126ab   c                   eval      ilry09aas = arbaas
007100141126ab   c                   eval      ilry09lnp = arblnp
007200141126ab   c                   eval      ilry09nrs = arbnrs
007300141126ab   c                   eval      ilry09nsp = arbnsp
007400141126ab   c                   eval      kpjbus = kpjbu
007500141126ab   c                   eval      kpjbu  = fnlry09ds2
007600141126ab   c                   call      'FNLRY09R'
007700141126ab   c                   parm                    kpjba
007800141126ab   c                   eval      fnlry09ds2 = kpjbu
007900141126ab   c                   eval      kpjbu = kpjbus
008000141126ab    *-   DIROTTATA esdi = esito dirottamento 1=si 0=no
008100141126ab   c                   if        olry09err = ' ' and olry09esdi = '1'
008200141126ab   c                   eval       se_dirottata = 'S'
008300141126ab   c                   end
008400141126ab    *-
008500141126ab   c                   endif
008600141126ab    *-
008700141126ab   c                   end
008800141126ab    *----
008900141126ab    *    Solo se NON dirottata esegue altri controlli
009000141126ab   c                   IF         se_dirottata <> 'S'
009100070508      * considera solo bolle con dcr impostata e bloccate con la A
009200070508     c                   if        arbdcr = 0
009300070508     c                   iter
009400070508     c                   endif
009500070508     c                   if        arbfbc <> 'A'
009600070508     c                   iter
009700070508     c                   endif
009800070717     c                   if        arbcmc <> 'A16' and arbcmc <> 'A23'
009900070717     c                   iter
010000070717     c                   endif
010100141126ab   c                   end
010200141126ab    *-
010300070508      * bolle da trattare
010400141126ab    *-  aggiunto:(or)se chiesto il DIROTTAMENTO
010500070508     c                   if        arbtcr = *blank and arbdcr = d17dfvi or
010600070508     c                             arbtcr = 'P' and arbdcr > d17dfvi or
010700070508     c                             arbtcr = 'D' and arbdcr < d17dfvi
010800141126ab   c                              or se_dirottata ='S'
010900070508     c                   clear                   arbfbc
011000070508     c                   clear                   arbcmc
011100070508     c                   update    fnarb000
011200070508     c                   endif
011300141126      *-
011400070226     c                   enddo
011500070226      *
011600070226    1C                   endsr
011700070508      *___________________________________________________
011800070508     c     *inzsr        begsr
011900070508      *___________________________________________________
012000070508     C     *entry        plist
012100070508     C                   parm                    kpjba
012200070508     c                   movel     kpjbu         fidg17ds
012300130225     c     karb          klist
012400130225     c                   kfld                    lineak
012500130225     c                   kfld                    datak
012600070508      *
012700141126ab   c     karg          klist
012800141126ab   c                   kfld                    arbaas
012900141126ab   c                   kfld                    arblnp
013000141126ab   c                   kfld                    arbnrs
013100141126ab   c                   kfld                    arbnsp
013200141126      *
013300070508     C     d17FGSi       CHAIN     AZORG01l
013400070508    1C                   if        %found(azorg01l)
013500070508     C                   movel     orgde6        og146
013600070508     c                   endif
013700070508      * reperisco £6 per filiali in gestione
013800070508     c                   eval      kpjbus = kpjbu
013900070508     C                   clear                   trul06ds
014000070508     C                   movel     '£6'          d06cod
014100070508     C                   movel     d17FGSi       d06key
014200070508     C                   movel     trul06ds      kpjbu
014300070508     C                   CALL      'TRUL06R'
014400070508     C                   parm                    kpjba
014500070508     C                   movel     kpjbu         trul06ds
014600070508     c                   eval      kpjbu = kpjbus
014700070508      *
014800070226     c                   endsr
014900141126ab   c*-----------------------------------------------------------------
