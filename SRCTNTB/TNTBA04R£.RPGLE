000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300000000      *
000400160930      *  Descrizione   :
000500000000      *****************************************************************
000600161028     Ftntba04d  CF   E             WORKSTN
000700160928     fazorg01l  if   e           k disk
000800161026     ftntbe01l  if   e           k disk
000900161027      *****************************************************************
001000160411     D KPJBA         E DS
001100160411     D CNCR80        E DS
001200160928     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001300161026     D tibs02ds      E DS
001400161026     D dTAI          E DS
001500161026     D dATS          E DS
001600161026     D dMAS          E DS
001700161026     D dAAT          E DS
001800161027     D dALL          E DS
001900161027     D tntba04ds     E DS
001901161108      **
001902161108      * Parametri in ricezione
001903161108     D  TABDS          DS
001904161108     D  XTAOPZ                 1      2
001905161108     D  XTARET                 3      3
001906161108     D  XTAOPR                 4      4
001907161108     D  XTAERR                 5      5
001908161108     D  XTAKE1                 6     20
001909161108     D  XTAKE2                21     35
001910161108      *-------------
002000940926     D*
002100081008      *__________________________________________________________________
002200000000     C     *ENTRY        PLIST
002300000000     C                   PARM                    KPJBA
002400161028     C                   SETON                                        LR
002500161027      *-----------------------------------------*
002600000000      *             M A I N      L I N E
002700161027      *-----------------------------------------*
002800000000     C                   Z-ADD     1             CODUT
002900000000     C                   CALL      'X§PARUT'
003000000000     C                   PARM                    UT§DSE
003100000000     C                   MOVEL     RAGUT         RSUT             20
003200000000     C                   MOVEL     RAGUT         RSUT2            20
003300000000     C                   MOVEL     REC80         CNCR80
003400161025      * inzializza video
003500161025     c                   exsr      inzD1
003600000000      *
003700160411     c                   do        *hival
003800161028      *
003900081008     C                   EXFMT     cna7D01
004000161028     c                   clear                   vmessag
004100161028      *
004200160411     C   KC              LEAVE
004201161108     C   KJ              exsr      nuovoREC
004202161108      **
004300161026     c                   setoff                                       90
004400161026     c                   exsr      ctrD1
004500161026      * errore
004600161026     C   90              iter
004700000000      *
004800081008      * Richiamo programma di conferma
004900161027     c                   clear                   tntba04ds
005000161027     c                   move      vfil          a04FIL
005100161027     c                   eval      a04TAI = vtasp
005200161027     c                   eval      a04ATS = vterr
005300161027     c                   eval      a04MAS = vmass
005400161027     c                   eval      a04AAT = vtaat
005500161027     c                   eval      a04ALL = vtall
005600161027      **
005700161027     c                   movel     tntba04ds     kpjbu
005800160930     c                   call      'TNTBA04R1'
005900160411     c                   parm                    kpjba
006000161028     c                   eval      tntba04ds = kpjbu
006100161028     c                   if        a04fun ='03'
006200161028     c                   leave
006300161028     c                   end
006400161028     c                   eval        vmessag = a04MSG
006500161028     c                   clear                   a04MSG
006600161027      **
006700160411     c                   enddo
006800000000      *
006900161028     C                   RETURN
007000161028      *
007100161025      *------------------------------------------------------------------
007200161025     c     inzD1         begsr
007300161025      *
007400161027     c                   clear                   vfil
007500161027     c                   movel(p)  'Tutte'       vfild
007600161025     c                   move      *blank        vtasp
007700161025     c                   movel(p)  'Tutto'       vdtasp
007800161025     c                   move      *blank        vterr
007900161025     c                   movel(p)  'Tutto'       vdterr
008000161025     c                   move      *blank        vmass
008100161025     c                   movel(p)  'Tutto'       vdmass
008200161026     c                   move      *blank        vtaat
008300161026     c                   movel(p)  'Tutto'       vdtaat
008400161027     c                   move      *blank        vtall
008500161027     c                   movel(p)  'Tutto'       vdtall
008600161025      *
008700161025     c                   endsr
008800161025      *------------------------------------------------------------------
008900161026     c     ctrD1         begsr
009000161026      *
009100161026      **  filiale
009200161027     c                   if        vfil  = 0
009300161027     c                   movel(p)  'Tutte'       vfild
009400161028     c                   elseif    vfil  = 999
009500161028     c                   movel(p)  'Azienda'     vfild
009600161028     c                   elseif    vfil <> 999
009700161026     c                   clear                   vfild
009800161026     c     vfil          chain     azorg01l
009900161026     c                   if        %found(azorg01l)
010000161026     c                   movel     orgdes        vfild
010100161026     c                   elseif    vfil  = 999
010200161027     c                   movel(p)  'Azienda'     vfild
010300161026     c                   else
010400161026     c                   seton                                        9011
010500161026     c                   LEAVESR
010600161026     c                   endif
010700161026     c                   endif
010800161026      *
010900161026      **  Tipo aspettativa
011000161026     C                   CLEAR                   tibs02ds
011100161026     C                   MOVEL     KNSIF         T02SIF
011200161026     C                   MOVEL     'TAI'         T02COD
011300161026     c                   if        vTasp  = '?'
011400161026     C                   MOVEL     'R'           T02MOD
011500161026     c                   seton                                        90
011600161026     c                   elseif    vTasp  = *blank
011700161026     c                   movel(p)  'Tutto'       vdtasp
011800161026     c                   else
011900161026     C                   MOVEL     'C'           T02MOD
012000161026     C                   movel(p)  vTasp         T02KE1
012100161026     c                   endif
012200161026      *
012300161026     c                   if        vTasp  <> *blank
012400161026     C                   CALL      'TIBS02R'
012500161026     C                   PARM                    KPJBA
012600161026     C                   PARM                    tibs02ds
012700161026     C     T02ERR        IFEQ      *BLANKS
012800161026     C                   MOVEL     T02UNI        dtai
012900161026     c                   movel     d§taides      vDTasp
013000161026     c                   movel     t02ke1        vTasp
013100161026     C                   ELSE
013200161026     c                   clear                   vDTasp
013300161026     C                   CLEAR                   Dtai
013400161026     c                   seton                                        9012
013500161026     c                   LEAVESR
013600161026     C                   ENDIF
013700161026     c                   end
013800161026      *
013900161026      **  Territorio
014000161026     C                   CLEAR                   tibs02ds
014100161026     C                   MOVEL     KNSIF         T02SIF
014200161026     C                   MOVEL     'ATS'         T02COD
014300161026     c                   if        vTerr  = *blank
014400161026     c                   movel(p)  'Tutto'       vdterr
014500161026     c                   else
014600161026      **
014700161026     c                   if        vTerr  = '?'
014800161026     C                   MOVEL     'R'           T02MOD
014900161026     c                   seton                                        90
015000161026     c                   else
015100161026     C                   MOVEL     'C'           T02MOD
015200161026     C                   movel(p)  vTerr         T02KE1
015300161026     c                   endif
015400161026      *
015500161026     C                   CALL      'TIBS02R'
015600161026     C                   PARM                    KPJBA
015700161026     C                   PARM                    tibs02ds
015800161026     C     T02ERR        IFEQ      *BLANKS
015900161026     C                   MOVEL     T02UNI        dats
016000161027     c                   movel     d§atsdes      vDTerr
016100161026     c                   movel     t02ke1        vTerr
016200161026     C                   ELSE
016300161026     c                   clear                   vDTerr
016400161026     C                   CLEAR                   Dats
016500161026     c                   seton                                        9013
016600161026     c                   LEAVESR
016700161026     C                   ENDIF
016800161026     c                   end
016900161026      *
017000161026      **  Massa
017100161026     C                   CLEAR                   tibs02ds
017200161026     C                   MOVEL     KNSIF         T02SIF
017300161026     C                   MOVEL     'MAS'         T02COD
017400161026     c                   if        vMass  = *blank
017500161026     c                   movel(p)  'Tutto'       vdMass
017600161026     c                   else
017700161026      **
017800161026     c                   if        vMass  = '?'
017900161026     C                   MOVEL     'R'           T02MOD
018000161026     c                   seton                                        90
018100161026     C                   CALL      'TIBS02R'
018200161026     C                   PARM                    KPJBA
018300161026     C                   PARM                    tibs02ds
018400161026     C     T02ERR        IFEQ      *BLANKS
018500161026     C                   MOVEL     T02UNI        Dmas
018600161026     c                   movel     d§masdes      vDMass
018700161026     c                   movel     t02ke2        vMass
018800161026     C                   ELSE
018900161026     c                   clear                   vDMass
019000161026     C                   CLEAR                   Dmas
019100161026     c                   seton                                        9014
019200161026     c                   LEAVESR
019300161026     C                   ENDIF
019400161026     c                   else
019500161026     c                   seton                                        9014
019600161026     c     'MAS'         setll     tntbe01l
019700161026     c     'MAS'         reade     tntbe01l
019800161026     c                   dow       not %EoF(tntbe01l)
019900161026     c                   if        vMass = %subst(tbeKE2:1:1)
020000161026     c                   setoff                                       9014
020100161026     c                   leave
020200161026     c                   end
020300161026     c     'MAS'         reade     tntbe01l
020400161026     C                   ENDdo
020500161026     c   90              LEAVESR
020600161026     c                   endif
020700161026     c                   end
020800161026      *
020900161026      **  Tipo Attività
021000161026     C                   CLEAR                   tibs02ds
021100161026     C                   MOVEL     KNSIF         T02SIF
021200161026     C                   MOVEL     'AAT'         T02COD
021300161026     c                   if        vTaat  = *blank
021400161026     c                   movel(p)  'Tutto'       vdTaat
021500161026     c                   else
021600161026      **
021700161026     c                   if        vTaat  = '?'
021800161026     C                   MOVEL     'R'           T02MOD
021900161026     c                   seton                                        90
022000161026     c                   else
022100161026     C                   MOVEL     'C'           T02MOD
022200161026     C                   movel(p)  vTaat         T02KE1
022300161026     c                   endif
022400161026      *
022500161026     C                   CALL      'TIBS02R'
022600161026     C                   PARM                    KPJBA
022700161026     C                   PARM                    tibs02ds
022800161026     C     T02ERR        IFEQ      *BLANKS
022900161026     C                   MOVEL     T02UNI        Daat
023000161026     c                   movel     d§aatdes      vDTaat
023100161026     c                   movel     t02ke1        vTaat
023200161026     C                   ELSE
023300161026     c                   clear                   vDTaat
023400161026     C                   CLEAR                   Daat
023500161026     c                   seton                                        9015
023600161026     c                   LEAVESR
023700161026     C                   ENDIF
023800161026     c                   end
023900161027      *
024000161027      **  Tipo Allestimento
024100161027     C                   CLEAR                   tibs02ds
024200161027     C                   MOVEL     KNSIF         T02SIF
024300161027     C                   MOVEL     'ALL'         T02COD
024400161027     c                   if        vTall  = *blank
024500161027     c                   movel(p)  'Tutto'       vdTall
024600161027     c                   else
024700161027      **
024800161027     c                   if        vTall  = '?'
024900161027     C                   MOVEL     'R'           T02MOD
025000161027     c                   seton                                        90
025100161027     c                   else
025200161027     C                   MOVEL     'C'           T02MOD
025300161027     C                   movel(p)  vTall         T02KE1
025400161027     c                   endif
025500161027      *
025600161027     C                   CALL      'TIBS02R'
025700161027     C                   PARM                    KPJBA
025800161027     C                   PARM                    tibs02ds
025900161027     C     T02ERR        IFEQ      *BLANKS
026000161027     C                   MOVEL     T02UNI        Dall
026100161027     c                   movel     d§alldes      vDTall
026200161027     c                   movel     t02ke1        vTall
026300161027     C                   ELSE
026400161027     c                   clear                   vDTall
026500161027     C                   CLEAR                   Dall
026600161027     c                   seton                                        9016
026700161027     c                   LEAVESR
026800161027     C                   ENDIF
026900161027     c                   end
027000161027      **
027100161026      *
027200161026     c                   endsr
027300161026      *------------------------------------------------------------------
027302161108      *  - Tasto funzionale F10 -> Inserimento nuovo record     *
027303161108      *------------------------------------------------------------------
027304161108     c     NUOVOrec      begsr
027400161108      *
027405161108     C                   RESET                   tabds
027406161108     C                   MOVEL     '01'          xtaopz
027407161108     C                   MOVE      *ZERO         xtaret
027408161108     C                   MOVE      *ZERO         xtaopr
027409161108     C                   clear                   xtake1
027410161108     C                   clear                   xtake2
027411161108     C                   MOVE      *BLANKS       KPJBU
027412161108     C                   MOVEL     tabds         KPJBU
027413161108$004 C                   CALL      'TNTBA04R3'
027414161108     C                   PARM                    KPJBA
027415161108     C                   MOVEL     KPJBU         tabds
027417161108      *
027500161108     c                   endsr
027600161108      *------------------------------------------------------------------
