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
002000161108      **
002100161108      * Parametri in ricezione
002200161108     D  TABDS          DS
002300161108     D  XTAOPZ                 1      2
002400161108     D  XTARET                 3      3
002500161108     D  XTAOPR                 4      4
002600161108     D  XTAERR                 5      5
002700161108     D  XTAKE1                 6     20
002800161108     D  XTAKE2                21     35
002900161108      *-------------
003000940926     D*
003100081008      *__________________________________________________________________
003200000000     C     *ENTRY        PLIST
003300000000     C                   PARM                    KPJBA
003400161028     C                   SETON                                        LR
003500161027      *-----------------------------------------*
003600000000      *             M A I N      L I N E
003700161027      *-----------------------------------------*
003800000000     C                   Z-ADD     1             CODUT
003900000000     C                   CALL      'X§PARUT'
004000000000     C                   PARM                    UT§DSE
004100000000     C                   MOVEL     RAGUT         RSUT             20
004200000000     C                   MOVEL     RAGUT         RSUT2            20
004300000000     C                   MOVEL     REC80         CNCR80
004400161025      * inzializza video
004500161025     c                   exsr      inzD1
004600000000      *
004700160411     c                   do        *hival
004800161028      *
004900081008     C                   EXFMT     cna7D01
005000161028     c                   clear                   vmessag
005100161028      *
005200160411     C   KC              LEAVE
005300161108     C   KJ              exsr      nuovoREC
005400161108      **
005500161026     c                   setoff                                       90
005600161026     c                   exsr      ctrD1
005700161026      * errore
005800161026     C   90              iter
005900000000      *
006000081008      * Richiamo programma di conferma
006100161027     c                   clear                   tntba04ds
006200161027     c                   move      vfil          a04FIL
006300161027     c                   eval      a04TAI = vtasp
006400161027     c                   eval      a04ATS = vterr
006500161027     c                   eval      a04MAS = vmass
006600161027     c                   eval      a04AAT = vtaat
006700161027     c                   eval      a04ALL = vtall
006800161027      **
006900161027     c                   movel     tntba04ds     kpjbu
007000160930     c                   call      'TNTBA04R1'
007100160411     c                   parm                    kpjba
007200161028     c                   eval      tntba04ds = kpjbu
007300161028     c                   if        a04fun ='03'
007400161028     c                   leave
007500161028     c                   end
007600161028     c                   eval        vmessag = a04MSG
007700161028     c                   clear                   a04MSG
007800161027      **
007900160411     c                   enddo
008000000000      *
008100161028     C                   RETURN
008200161028      *
008300161025      *------------------------------------------------------------------
008400161025     c     inzD1         begsr
008500161025      *
008600161027     c                   clear                   vfil
008700161027     c                   movel(p)  'Tutte'       vfild
008800161025     c                   move      *blank        vtasp
008900161025     c                   movel(p)  'Tutto'       vdtasp
009000161025     c                   move      *blank        vterr
009100161025     c                   movel(p)  'Tutto'       vdterr
009200161025     c                   move      *blank        vmass
009300161025     c                   movel(p)  'Tutto'       vdmass
009400161026     c                   move      *blank        vtaat
009500161026     c                   movel(p)  'Tutto'       vdtaat
009600161027     c                   move      *blank        vtall
009700161027     c                   movel(p)  'Tutto'       vdtall
009800161025      *
009900161025     c                   endsr
010000161025      *------------------------------------------------------------------
010100161026     c     ctrD1         begsr
010200161026      *
010300161026      **  filiale
010400161027     c                   if        vfil  = 0
010500161027     c                   movel(p)  'Tutte'       vfild
010600161028     c                   elseif    vfil  = 999
010700161028     c                   movel(p)  'Azienda'     vfild
010800161028     c                   elseif    vfil <> 999
010900161026     c                   clear                   vfild
011000161026     c     vfil          chain     azorg01l
011100161026     c                   if        %found(azorg01l)
011200161026     c                   movel     orgdes        vfild
011300161026     c                   elseif    vfil  = 999
011400161027     c                   movel(p)  'Azienda'     vfild
011500161026     c                   else
011600161026     c                   seton                                        9011
011700161026     c                   LEAVESR
011800161026     c                   endif
011900161026     c                   endif
012000161026      *
012100161026      **  Tipo aspettativa
012200161026     C                   CLEAR                   tibs02ds
012300161026     C                   MOVEL     KNSIF         T02SIF
012400161026     C                   MOVEL     'TAI'         T02COD
012500161026     c                   if        vTasp  = '?'
012600161026     C                   MOVEL     'R'           T02MOD
012700161026     c                   seton                                        90
012800161026     c                   elseif    vTasp  = *blank
012900161026     c                   movel(p)  'Tutto'       vdtasp
013000161026     c                   else
013100161026     C                   MOVEL     'C'           T02MOD
013200161026     C                   movel(p)  vTasp         T02KE1
013300161026     c                   endif
013400161026      *
013500161026     c                   if        vTasp  <> *blank
013600161026     C                   CALL      'TIBS02R'
013700161026     C                   PARM                    KPJBA
013800161026     C                   PARM                    tibs02ds
013900161026     C     T02ERR        IFEQ      *BLANKS
014000161026     C                   MOVEL     T02UNI        dtai
014100161026     c                   movel     d§taides      vDTasp
014200161026     c                   movel     t02ke1        vTasp
014300161026     C                   ELSE
014400161026     c                   clear                   vDTasp
014500161026     C                   CLEAR                   Dtai
014600161026     c                   seton                                        9012
014700161026     c                   LEAVESR
014800161026     C                   ENDIF
014900161026     c                   end
015000161026      *
015100161026      **  Territorio
015200161026     C                   CLEAR                   tibs02ds
015300161026     C                   MOVEL     KNSIF         T02SIF
015400161026     C                   MOVEL     'ATS'         T02COD
015500161026     c                   if        vTerr  = *blank
015600161026     c                   movel(p)  'Tutto'       vdterr
015700161026     c                   else
015800161026      **
015900161026     c                   if        vTerr  = '?'
016000161026     C                   MOVEL     'R'           T02MOD
016100161026     c                   seton                                        90
016200161026     c                   else
016300161026     C                   MOVEL     'C'           T02MOD
016400161026     C                   movel(p)  vTerr         T02KE1
016500161026     c                   endif
016600161026      *
016700161026     C                   CALL      'TIBS02R'
016800161026     C                   PARM                    KPJBA
016900161026     C                   PARM                    tibs02ds
017000161026     C     T02ERR        IFEQ      *BLANKS
017100161026     C                   MOVEL     T02UNI        dats
017200161027     c                   movel     d§atsdes      vDTerr
017300161026     c                   movel     t02ke1        vTerr
017400161026     C                   ELSE
017500161026     c                   clear                   vDTerr
017600161026     C                   CLEAR                   Dats
017700161026     c                   seton                                        9013
017800161026     c                   LEAVESR
017900161026     C                   ENDIF
018000161026     c                   end
018100161026      *
018200161026      **  Massa
018300161026     C                   CLEAR                   tibs02ds
018400161026     C                   MOVEL     KNSIF         T02SIF
018500161026     C                   MOVEL     'MAS'         T02COD
018600161026     c                   if        vMass  = *blank
018700161026     c                   movel(p)  'Tutto'       vdMass
018800161026     c                   else
018900161026      **
019000161026     c                   if        vMass  = '?'
019100161026     C                   MOVEL     'R'           T02MOD
019200161026     c                   seton                                        90
019300161026     C                   CALL      'TIBS02R'
019400161026     C                   PARM                    KPJBA
019500161026     C                   PARM                    tibs02ds
019600161026     C     T02ERR        IFEQ      *BLANKS
019700161026     C                   MOVEL     T02UNI        Dmas
019800161026     c                   movel     d§masdes      vDMass
019900161026     c                   movel     t02ke2        vMass
020000161026     C                   ELSE
020100161026     c                   clear                   vDMass
020200161026     C                   CLEAR                   Dmas
020300161026     c                   seton                                        9014
020400161026     c                   LEAVESR
020500161026     C                   ENDIF
020600161026     c                   else
020700161026     c                   seton                                        9014
020800161026     c     'MAS'         setll     tntbe01l
020900161026     c     'MAS'         reade     tntbe01l
021000161026     c                   dow       not %EoF(tntbe01l)
021100161026     c                   if        vMass = %subst(tbeKE2:1:1)
021200170821     C                   MOVEL     TbeUNI        Dmas
021300170821     c                   movel     d§masdes      vDMass
021400161026     c                   setoff                                       9014
021500161026     c                   leave
021600161026     c                   end
021700161026     c     'MAS'         reade     tntbe01l
021800161026     C                   ENDdo
021900161026     c   90              LEAVESR
022000161026     c                   endif
022100161026     c                   end
022200161026      *
022300161026      **  Tipo Attività
022400161026     C                   CLEAR                   tibs02ds
022500161026     C                   MOVEL     KNSIF         T02SIF
022600161026     C                   MOVEL     'AAT'         T02COD
022700161026     c                   if        vTaat  = *blank
022800161026     c                   movel(p)  'Tutto'       vdTaat
022900161026     c                   else
023000161026      **
023100161026     c                   if        vTaat  = '?'
023200161026     C                   MOVEL     'R'           T02MOD
023300161026     c                   seton                                        90
023400161026     c                   else
023500161026     C                   MOVEL     'C'           T02MOD
023600161026     C                   movel(p)  vTaat         T02KE1
023700161026     c                   endif
023800161026      *
023900161026     C                   CALL      'TIBS02R'
024000161026     C                   PARM                    KPJBA
024100161026     C                   PARM                    tibs02ds
024200161026     C     T02ERR        IFEQ      *BLANKS
024300161026     C                   MOVEL     T02UNI        Daat
024400161026     c                   movel     d§aatdes      vDTaat
024500161026     c                   movel     t02ke1        vTaat
024600161026     C                   ELSE
024700161026     c                   clear                   vDTaat
024800161026     C                   CLEAR                   Daat
024900161026     c                   seton                                        9015
025000161026     c                   LEAVESR
025100161026     C                   ENDIF
025200161026     c                   end
025300161027      *
025400161027      **  Tipo Allestimento
025500161027     C                   CLEAR                   tibs02ds
025600161027     C                   MOVEL     KNSIF         T02SIF
025700161027     C                   MOVEL     'ALL'         T02COD
025800161027     c                   if        vTall  = *blank
025900161027     c                   movel(p)  'Tutto'       vdTall
026000161027     c                   else
026100161027      **
026200161027     c                   if        vTall  = '?'
026300161027     C                   MOVEL     'R'           T02MOD
026400161027     c                   seton                                        90
026500161027     c                   else
026600161027     C                   MOVEL     'C'           T02MOD
026700161027     C                   movel(p)  vTall         T02KE1
026800161027     c                   endif
026900161027      *
027000161027     C                   CALL      'TIBS02R'
027100161027     C                   PARM                    KPJBA
027200161027     C                   PARM                    tibs02ds
027300161027     C     T02ERR        IFEQ      *BLANKS
027400161027     C                   MOVEL     T02UNI        Dall
027500161027     c                   movel     d§alldes      vDTall
027600161027     c                   movel     t02ke1        vTall
027700161027     C                   ELSE
027800161027     c                   clear                   vDTall
027900161027     C                   CLEAR                   Dall
028000161027     c                   seton                                        9016
028100161027     c                   LEAVESR
028200161027     C                   ENDIF
028300161027     c                   end
028400161027      **
028500161026      *
028600161026     c                   endsr
028700161026      *------------------------------------------------------------------
028800161108      *  - Tasto funzionale F10 -> Inserimento nuovo record     *
028900161108      *------------------------------------------------------------------
029000161108     c     NUOVOrec      begsr
029100161108      *
029200161108     C                   RESET                   tabds
029300161108     C                   MOVEL     '01'          xtaopz
029400161108     C                   MOVE      *ZERO         xtaret
029500161108     C                   MOVE      *ZERO         xtaopr
029600161108     C                   clear                   xtake1
029700161108     C                   clear                   xtake2
029800161108     C                   MOVE      *BLANKS       KPJBU
029900161108     C                   MOVEL     tabds         KPJBU
030000161108$004 C                   CALL      'TNTBA04R3'
030100161108     C                   PARM                    KPJBA
030200161108     C                   MOVEL     KPJBU         tabds
030300161108      *
030400161108     c                   endsr
030500161108      *------------------------------------------------------------------
