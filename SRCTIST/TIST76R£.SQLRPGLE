000001100215     H DECEDIT('0,') DATEDIT(*DMY.)
000100030117      *****************************************************************
000200100215      * Statistica clienti x spedizioni con attesa alla cons.
000300030117      *****************************************************************
000400100215     Ftist76p   o    E             printer oflind(*in01)                        x Data Prestazione
000401100215     Fazorg01l  if   E           k disk
000402100215     Fcnaco00f  IF   E           K DISK                                         x Data Prestazione
000700030117      *****************************************************************
000800030131      *
000900030129      *****************************************************************
001000030117     d KPJBA         E DS
001300030825      *
001400100215     D tist75ds      e DS
001401091112     D ddatiute      e ds
001402091112     D azuteds       e ds                  extname(AZUTE00F)
001403100215     D wftist75      e ds                  extname(wftist75F)
001404091112     D tibs34ds      E DS                  inz
002200100114     D  dtaiso         S               d   inz
002300100215     D  dtaeur         S               d   inz datfmt(*eur)
002301100215     D  stpsav         S                   like(stp)
002302100215     D  stp            S                   like(st75stp)
002303100215     D  nrspe          S                   like(ST75NSF)
002304100215     D  nrferma        S                   like(ST75NSF)
002305100215     D  speatt         S              6  0
002306100215     D  spenoatt       S              6  0
003100030826      *
005100030117      *****************************************************************
005200030117     C     *ENTRY        PLIST
005300030117     C                   PARM                    KPJBA
005400100215     C                   movel     KPJBU         tist75ds
005407091113     c*
005408091112     c     *dtaara       define    §azute        azuteds
005409091112     c     *dtaara       define    §datiute      ddatiute
005410091112     C                   in(E)     *dtaara
005411091112     C                   IF        %Error  or  RSUT = *blanks
005412091112     C                   call      'TIBS34R'
005413091112     C                   parm                    Tibs34Ds
005414091112     C                   in        *dtaara
005415100215     c                   end
005416100215     c*
005417100215     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
005418100215     C                   MOVE      WN14          Wdate             8 0          *DATA (8) IN G/M/AA
005426100215     c                   movel     wn14          utime             6 0
005427100215     c*
005428100215     c                   seton                                        01
005429100215     c     kaco          klist
005430100215     c                   kfld                    acokut
005431100215     c                   kfld                    acokcc
005432100215     c                   kfld                    t75ksc
005433100215     c                   eval      acokut = 1
005434100215     c                   eval      acokcc = 0151
005435100215     c     kaco          chain     cnaco00f
005436100215     c                   if        %found(cnaco00f)
005437100215     c                   eval      desrag = acorag
005438100215     c                   else
005439100215     c                   clear                   desrag           35
005440100215     c                   end
005441100215     c                   clear                   compdi            7 0
005442100215     c                   if        t75fgs <> 999
005443100215     c     t75fgs        chain     azorg01l
005444100215     c                   if        %found(azorg01l)
005449100215     c                   movel     orgdes        desfgs
005450100215     c                   end
005451100215     c                   else
005452100215     c                   eval      desfgs = 'Tutte'
005454100215     c                   end
005455100215     c                   if        t75fgs <> 999
005456100215     c                   movel     t75fgs        compdi
005457100215     c                   end
005458100215     c                   movel     *all'9'       compdf            7 0
005459100215     c                   if        t75fgs <> 999
005460100215     c                   movel     t75fgs        compdf
005461100215     c                   end
005462100215     c*
005463100215     c                   movel     t75dti        dtaiso
005464100215     c                   movel     dtaiso        dtaeur
005465100215     c                   movel     dtaeur        comdti            8 0
005466100215     c                   movel     t75dtf        dtaiso
005467100215     c                   movel     dtaiso        dtaeur
005468100215     c                   movel     dtaeur        comdtf            8 0
005469100215     c*
006100030117      *--------------------------------------------------------------*
006200030117      *             M A I N      L I N E
006300030117      *--------------------------------------------------------------*
007719100114     C/EXEC SQL
007720100215     C+ INSERT INTO GAITRAFIL/WFTIST75F (ST75FGS, ST75NDC, ST75DCM,
007721100215     C+ ST75RSD, ST75IND, ST75STP, ST75NSF) SELECT substr(digits(taspdc),
007722100215     C+ 1, 3), tasndc, tasdcm, TASRSD, TASIND, TASSTP, count(*) FROM
007723100215     C+ titas10f WHERE tasdcm between :t75dti and :t75dtf and taspdc
007724100215     C+ between :compdi and :compdf and tasgma = :t75gma and tasksc =
007725100215     C+ :t75ksc and tasstp not in (0, 99999) GROUP BY
007726100215     C+ substr(digits(taspdc), 1, 3), tasndc, tasdcm, TASRSD, TASIND,
007727100215     C+ TASSTP
007728100215     C/END-EXEC
011900030416     c*
011901100215     C/EXEC SQL
011902100215     C+ DECLARE A1 CURSOR FOR SELECT ST75STP, ST75NSF, count(*) FROM
011903100215     C+ wftist75f GROUP BY ST75STP, ST75NSF ORDER BY ST75STP, ST75NSF
011906100215     C/END-EXEC
011907100215
011908100215     C/EXEC SQL
011909100215     C+ OPEN a1
011910100215     C/END-EXEC
011911100215
011912100215     C                   do        *hival
011913100215
011914100215     C/EXEC SQL
011915100215     C+ FETCH NEXT FROM a1 INTO :stp, :nrspe, :nrferma
011916100215     C/END-EXEC
011917100215
011918100215     C                   select
011919100215     C                   when      sqlcod = 100
011920100215     c                   leave
011921100215     C                   WHEN      SqlCod < 0
011922100215     c* record allocato
011923100215     c                   if        sqlcod <> -913
011924100215     C                   seton                                        H1
011925100215     c                   end
011929100215     c                   other
011930100215     c* a rottura di stop
011931100215     c                   if        stp <> savstp
011932100215     c                   if        savstp <> 0
011933100215     c                   seton                                        02
011934100215     c                   exsr      stampa
011935100215     c                   add       stpferma      genferma
011936100215     c                   add       stpatt        genatt
011937100215     c                   add       stpnoatt      gennoatt
011938100215     c                   clear                   stpferma
011939100215     c                   clear                   stpatt
011940100215     c                   clear                   stpnoatt
011941100215     c                   end
011942100215     c                   eval      savstp = stp
011943100215     c                   end
011944100215     c                   eval      speatt = nrspe * nrferma
011945100215     c                   eval      spenoatt = stp * nrferma
011946100215     c                   add       nrferma       stpferma
011947100215     c                   add       speatt        stpatt
011948100215     c                   add       spenoatt      stpnoatt
011975100215     C                   endsl
011976100215     C*
011977100215     C                   enddo
011978100215
011979100215     C/EXEC SQL
011980100215     C+ CLOSE a1
011981100215     C/END-EXEC
011982100215     c                   if        savstp <> 0
011983100215     c                   seton                                        02
011984100215     c                   exsr      stampa
011985100215     c                   add       stpferma      genferma
011986100215     c                   add       stpatt        genatt
011987100215     c                   add       stpnoatt      gennoatt
011988100215     c                   eval      medspe = genatt / genferma
011989100215     c                   seton                                        03
011990100215     c                   exsr      stampa
011991100215     c                   end
011992100215     c                   seton                                        04
011993100215     c                   exsr      stampa
011994100215     c
012000030415     C                   SETON                                        LR
012001100215     c**********************************************************************
012100100215     C     stampa        begsr
012200100215     c**********************************************************************
012201100215     c*
012202100215     c                   if        *in01
012203100215     c                   write     testa
012204100215     c                   setoff                                       01
012205100215     c                   end
012206100215     c   02              write     riga
012207100215     c   03              write     totali
012208100215     c   04              write     fine
012209100215     c                   setoff                                       020304
012300100215     c                   endsr
