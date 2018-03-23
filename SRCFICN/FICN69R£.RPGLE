000100030210     H DECEDIT('0,') DATEDIT(*YMD.)
000200030206      *--------------------------------------------------------------*
000300030206      *      Conferma valorizzazione per i servizi T e P
000400911204      *--------------------------------------------------------------*
000500030210     FFicn69D   CF   E             WORKSTN
000800030210     FTntbe01l  IF   E           K DISK
000801030210     FTabel00f  IF   E           K DISK
001100030206     Ffiapd01L  if   E           K DISK
001101030210     Ffictt03l  uf   E           k DISK    usropn
001102030210     Ffiftt02l  uf   E           k DISK    usropn
001200940809     D*
001400950314RM*  D L1              S              3  0 DIM(30) ASCEND
001500950314RM*  D L6              S              3  0 DIM(30) ASCEND
001501030206     D fnlv24ds      E DS
001502030210     D dccg          E DS
002701021129     D KPJBA         E DS
002702030206     D CNCR80        E DS
002703030206     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003500940909     D* DS PER TRUL06R - CARICAMENTO £X
003600940909     D DSUL06        E DS                  EXTNAME(TRUL06DS)
003700950314RM*  D  LIN                    1     90  0
003800950314     D                                     DIM(30)
003900950314RM*  D                 DS
004000030206RM*  D  V1CPDR                 1      7
004100950314RM*  D  V1FPAD                 1      3  0
004200030206RM*  D  V1CPAD                 4      7
004201030206     D DSFMT           DS
004202030206     D  $TASTO               369    369
004203030206     D  NRG                  370    370
004204030206     D  NCL                  371    371
004205030206     D* posizione cursore
004206030206     D CURSOR          DS
004207030206     D  RIRI                   1      2B 0 INZ
004208030206     D  R$$                    2      2
004209030206     D  COCO                   3      4B 0 INZ
004210030206     D  C$$                    4      4
004220030207     d ds1z          e ds
004221030206     d com03           s              3
004222030210     d dataiso         s               d   inz
004223030210     d dataeur         s               d   inz datfmt(*eur)
004225030210     d data            s                   like(fttddc)
004226030210     d com07           s                   like(fttpdr)
004227030210     d oggi            s               d   inz
006900920921     C*****************************************************************
012300030206RM*  C                   exsr      azzera
012301030206     c*
012302030206     c                   do        *hival
012303030206     c                   if        *in99
012304030206     c                   setoff                                       99
012305030206     c                   write     y64d01
012306030206     c                   seton                                        99
012307030206     c                   end
012800030206     C                   exfmt     y64d01
013500920918     C* CMD3 - FINE LAVORO
013600030206     C   KC              leave
013601030206     c* ricerca
013602030206     c   kd              exsr      srric
013603030206     c* controlli
013604030206     c                   exsr      srctr
013605030206     c   99              iter
013606030206     c* scrivi
013607030210     c                   if        *inkf
013608030206     c                   exsr      scrivi
013609030206     c                   leave
013610030206     c                   end
013611030206     c                   enddo
013612030206      *
013613030206     C                   SETON                                        LR
013619030206     C*****************************************************************
013620030206     c     srctr         begsr
013621030206     C*****************************************************************
013622030206     c                   setoff                                       99
013623030206     c                   do
015300030206RM*  C* Filiale diversa da 0:
015301030129     c                   select
015700030129     c                   when      simtpp = '2' or simtpp = ' '
015900950314RM*  C* deve essere in £6 se secondo livello
016000950314RM*  C     V1FPAD        LOOKUP    L6                                     45
016100030129RM*  C                   when      simtpp = '1'
016200950314RM*  C* deve essere in £1 se primo livello
016300950314RM*  C     V1FPAD        LOOKUP    L1                                     45
016400030129RM* 3C                   ENDsl
016600950314RM*  C* Filiale non presente in tabella
016700950314RM* 3C     *IN45         IFEQ      *OFF
016900030206RM*  C                   SETON                                        9901
016901030206     c                   leave
017100950314RM* 3C                   END
017400030206     C* CODICE COOP  SEMPRE > 0
017500950314RM*  C     V1CPAD        IFEQ      *BLANKS
017600950314RM*  C     V1CPAD        OREQ      *ZERO
017800030206     C                   SETON                                        9902
017801030206     c                   leave
018000920918     C                   END
024900021129     C* CONTROLLO VALIDITA' CODICE AUTO/COOP
025000940809     C                   TESTN                   V1CPDR               50
025100950314RM*  C                   MOVE      V1CPDR        W001A             1
025200950314RM*  C     *IN50         IFEQ      *OFF
025300950314RM*  C     W001A         ORLT      '0'
025500030206     C                   SETON                                        9902
025501030206     c                   leave
025700950314RM*  C                   END
044000920921     C*
044001030206     c                   clear                   descop
044002030206     C                   MOVEL     V1CPDR        KPDR
044200030206     C     Kapd          CHAIN     fiapd01L
044300030206    1C                   IF        not %found(fiapd01l)
044301030206    1C                             or (%found(fiapd01l) and apdatb <> ' ')
044400030206     C                   SETON                                        9902
044401030206     c                   leave
044402030210     c                   else
044403030210     c                   if        orepv = 0 and oreav = 0
044404030210     c                   z-add     apdhpa        orepv
044406030210     c                   z-add     apdhar        oreav
045100000726     C                   ENDIF
045101030210     C                   ENDIF
045200030206     C                   MOVEL     APDRSC        descop
045201030210     c* data
045202030210     c                   if        datav = 0
045203030206     C                   SETON                                        9905
045204030206     c                   leave
045205030210     c                   else
045206030210     c     *eur          test(d)                 datav                  41
045207030210     c                   if        *in41
045208030210     c     *dmy          test(d)                 datav                  41
045210030210     c  n41*dmy          move      datav         dataeur
045211030210     c  n41              move      dataeur       datav
045212030210     c                   else
045213030210     c                   move      datav         dataeur
045214030210     c                   end
045215030210     c  n41              move      dataeur       dataiso
045216030210     c  n41              move      dataiso       data
045217030210     c                   end
045218030206     c* tipo servizio
045219030206     c                   clear                   destsr
045220030210     c                   if        tsrv <> ' '
045221030206     c                   eval      tblkey = tsrv
045222030206     c     ktab          chain     tabel00f
045223030206     c                   if        %found(tabel00f)
045224030207     c                   movel     tbluni        ds1z
045225030207     c                   movel     §1zdes        destsr
045226030206     c                   end
045227030206     c                   end
045228030210     c* nastro lavorativo
045229030210     c                   if        orepv = 0
045230030210     C                   SETON                                        9903
045231030210     c                   leave
045233030210     c                   end
045234030210     c                   if        oreav = 0 and tipo = 'C'
045235030210     C                   SETON                                        9904
045236030210     c                   leave
045237030210     c                   end
045238030210     c* controllo se già confermato
045240030210     c* autotrasportatori
045241030210     c                   if        tipo = 'A'
045242030210     c                   move      v1cpdr        com07
045243030210     c     kftt          chain     fiftt02l
045244030210     c                   if        not %found(fiftt02l)
045245030210     C                   SETON                                        9908
045246030210     c                   leave
045247030210     c                   else
045248030210     c* controllo limite gg
045249030210     c                   move      fttddc        dataiso
045250030210     c                   adddur    §cggga:*d     dataiso
045252030210     c                   if        dataiso < oggi
045253030210     C                   SETON                                        9909
045254030210     c                   leave
045255030210     C                   END
045256030210     C                   END
045257030210     c                   else
045258030210     c* cooperative
045259030210     c     kctt          chain     fictt03l
045260030210     c                   if        not %found(fictt03l)
045261030210     C                   SETON                                        9908
045262030210     c                   leave
045263030210     c                   else
045264030210     c* controllo limite gg
045265030210     c                   move      cttddc        dataiso
045266030210     c                   adddur    §cgggc:*d     dataiso
045267030210     c                   if        dataiso < oggi
045269030210     C                   SETON                                        9909
045270030210     c                   leave
045271030210     C                   END
045272030210     c                   end
045273030210     c                   end
045274030206     c                   enddo
045275030206     c*
058400030206     C                   ENDSR
093416940809     C**************************************************************************
093419920921     C     SCRIVI        BEGSR
093420030206     C**************************************************************************
093421030210     c                   if        tipo = 'A'
093422030210     c                   eval      ftthlv = orepv
093424030210     c                   except    fiftt
093425030210     c                   else
093430030210     c                   eval      ctthpa = orepv
093431030210     c                   eval      ctthar = oreav
093433030210     c                   except    fictt
093434030210     c                   end
097100920921     C                   ENDSR
097101030206     C**************************************************************************
097102030206     C     srric         BEGSR
097103030206     C**************************************************************************
097104030206     C* determino Riga/Colonna del cursore
097105030206     C                   MOVE      nrg           R$$
097106030206     C                   MOVE      ncl           C$$
097107030206     C                   Z-ADD     RIRI          CURR
097108030206     C                   Z-ADD     COCO          CURC
097112030206     C*
0971130302061    C                   SELECT
097114030206     C* ricerca cooperativa
097115030206    >C     H1NMFL        WHEneq    'V1CPAD'
097116030206     C                   MOVEL     *BLANKS       d24rsc
097117030206     C                   Z-ADD     0             d24pdr
097118030206     C                   Z-ADD     V1fpad        d24FIL
097119030206     C                   MOVEL     'R'           d24FLG
097121030210     C                   MOVEL     tipo          d24tip
097122030206     C                   MOVEL(p)  fnlv24ds      KPJBU
097123030206     C                   CALL      'FNLV24R'
097124030206     C                   PARM                    KPJBA
097125030206     C                   MOVEL     KPJBU         fnlv24ds
097126030206     C                   MOVEl     d24pdr        V1fpad
097127030206     C                   MOVE      d24pdr        V1cpad
097128030206     C                   MOVEL     d24rsc        descop
097129030206     c                   endsl
097130030206     C* imposto pos. del cursore
097131030206     C                   Z-ADD     CURR          H1RIGA
097132030206     C                   Z-ADD     CURC          H1COLO
097133030206     C                   ENDSR
097134030206     C*****************************************************************
097135030206     c     azzera        begsr
097136030206     C*****************************************************************
097137030206     c                   move      simpou        v1fpad
097138030206     c                   clear                   v1cpad
097139030206     c                   clear                   descop
097140030210     c                   clear                   datav
097141030210     c                   if        tipo = 'A'
097142030206     c                   clear                   tsrv
097143030210     c                   else
097144030210     c                   eval      tsrv = 'X'
097145030210     c                   end
097146030206     c                   clear                   destsr
097147030210     c                   clear                   orepv
097148030210     c                   clear                   oreav
097149030206     C                   Z-ADD     0             CURR              2 0
097150030206     C                   Z-ADD     0             CURC              2 0
097151030206     c                   endsr
097200940809     C**************************************************************************
097500940809     C     *INZSR        BEGSR
097501030206     C**************************************************************************
097502030206     C*
097503030206     C     *ENTRY        PLIST
097504030206     C                   PARM                    KPJBA
097505030206     C                   Z-ADD     1             CODUT
097506030206     C                   CALL      'X§PARUT'
097507030206     C                   PARM                    UT§DSE
097508030206     C                   MOVEL     RAGUT         rsut
097509030206     C                   MOVEL     REC80         CNCR80
097510030210     c                   movel     kpjbu         tipo              1
097511030210     c                   move      udate         oggi
097512030210     C                   IF        tipo = 'A'
097513030210RM*  C                   eval      destip = 'AUTOTRASPORTATORE'
097514030210     c                   eval      apdtip = 'A'
097515030210     c                   open      fiftt02l
097516030210     C                   ELSE
097517030210     c                   seton                                        60
097519030210RM*  C                   eval      destip = '   COOPERATIVA'
097520030210     c                   eval      apdtip = 'C'
097521030210     c                   open      fictt03l
097522030210     c* se  profilo COPxxx trucco il SIMFEL
097523030210     c                   if        %subst(knmus: 1: 3) = 'COP'
097524030210     c                   eval      com03 = %subst(knmus: 4: 3)
097525030210     c                   move      com03         simfel
097526030210     c                   end
097527030210     C                   END
098000940909     C*
098001030206     C     *LIKE         DEFINE    APDPDR        KPDR
098100021129     C     Kapd          KLIST
098200030206     C                   KFLD                    apdtip
098300030206     C                   KFLD                    kpdr
098700010207     C*
098800030206     C     KTAB          KLIST
098900030206     C                   KFLD                    tblkut
099000030206     C                   KFLD                    tblcod
099100030206     C                   KFLD                    tblkey
099300030206     C                   Z-ADD     1             tblkut
099301030206     C                   eval      tblcod = '1Z'
099302030210     C*
099303030210     C     KTbe          KLIST
099304030210     C                   KFLD                    tbecod
099305030210     C                   KFLD                    tbeke1
099307030206     C*
099308030206     C     Kctt          KLIST
099309030206     C                   KFLD                    v1fpad
099310030206     C                   KFLD                    kpdr
099311030206     C                   KFLD                    tsrv
099312030206     C                   KFLD                    data
099313030210     C*
099314030210     C     Kftt          KLIST
099315030210     C                   KFLD                    com07
099316030210     C                   KFLD                    fttfvl
099318030210     C                   KFLD                    data
099319030210     C                   KFLD                    tsrv
099320030210     c                   eval      fttfvl = 'C'
099326030206     C* CARICO LA TABELLA '£1'
099327030206     C                   MOVE      '£1'          D06COD
099328030206     C                   MOVEL     SIMFEL        D06KEY
099329030206     C                   MOVEL     DSUL06        KPJBU
099330030206     C                   CALL      'TRUL06R'
099331030206     C                   PARM                    KPJBA
099332030206     C                   MOVEL     KPJBU         DSUL06
099333030206     C                   MOVEA     LIN           L1
099334030206RM*  C* CARICO LA TABELLA '£6' SE FILIALE DI SECONDO LIVELLO
099335030206     c     simtpp        ifeq      '2'
099336030206     c     simtpp        oreq      *blanks
099337030206RM*  C                   MOVE      '£6'          D06COD
099338030206RM*  C                   MOVEL     simpou        D06KEY
099339030206RM*  C                   MOVEL     DSUL06        KPJBU
099340030206RM*  C                   CALL      'TRUL06R'
099341030206RM*  C                   PARM                    KPJBA
099342030206RM*  C                   MOVEL     KPJBU         DSUL06
099343030206RM*  C                   MOVEA     LIN           L6
099344030206RM*  C                   END
099345030210     c* tabella TNTBE00F CCG
099346030210     c                   eval      tbecod = 'CCG'
099347030210     c                   eval      tbeke1 = '1'
099348030210     c     ktbe          chain     tntbe01l
099349030210     c                   if        not %found(tntbe01l)
099350030210     c                   z-add     99            §cggga
099351030210     c                   z-add     99            §cgggc
099352030210     c                   else
099353030210     c                   eval      dccg = tbeuni
099354030210     c                   end
099500000808     C*
099600940809     C                   ENDSR
099700030210     ofiftt000  e            fiftt
099701030210     o                       ftthlv
099800030210     ofictt000  e            fictt
099900030210     o                       ctthpa
100000030210     o                       ctthar
