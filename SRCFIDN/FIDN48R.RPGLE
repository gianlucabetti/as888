000100990127      *----------------------------------------------------*
000200010907      *  Cerco se cliente con mandato
000300990127      *----------------------------------------------------*
000400990127     H DECEDIT('0,') DATEDIT(*DMY.)
000500990127
000600990407     fTABEL00F  IF   E           K DISK
000700010316     fTITPT01L  IF   E           K DISK
000800010316     fTNTAM01L  IF   E           K DISK
000900070606     fAZORG01L  IF   E           K DISK
001000070606     fTNTBE01L  IF   E           K DISK
001100010316
001200990127      *------------------------------------------------------------------------*
001300160819     d fnlv59ds      E DS                  inz
001400990407     D FIDN48DS      E DS
001500991105     D TIBS02DS      E DS
001600010910     D DGEDDN        E DS
001700010907     D DSCV          E DS
001800991105     D DSTA01        E DS
001900991105     D YEUR          E DS                  extname(YEURCODS)
002000070607     D OG143         E DS
002100070607     D DSTDDPD       E DS
002200990202      *
002300990209     D CNCR80        E DS
002400990209     D UT§DSE0F      E DS
002500990202     D KPJBA         E DS
002600990407     d  tcu                  398    697    DIM(50)
002700990407     d  kcu                  698    847P 0 DIM(50)  PACKEVEN
002800990407     d
002900990407     d TCUDS           DS
003000990407     d  f1                     1      1
003100990407     d  f3                     3      3
003200990407     d  f2                     2      2
003300990407     d  f4                     4      4
003400990407     d  f56                    5      6
003500990407
003600990407     D wftc            s                   LIKE(TPTftc)
003700990407     D Kcod            S                   LIKE(TBLcod)
003800990407     D Kkey            S                   LIKE(TBLkey)
003900991105
004000991105     D divisa          S                   LIKE(§TAdiv)
004100991105     D comodo          S                   LIKE(TPTvlm)
004200070606
004300070606     D Ktbecod         S                   LIKE(TBEcod)
004400070606     D Ktbeke1         S                   LIKE(TBEke1)
004500990218      *
004600990218      ***************************************************************************
004700990407      * Richiamo pgm per ricerca tariffa
004800990408     c                   if        i48tla<>'C'
004900990408     c                   eval      o48mand='N'
005000990407     c
005100160818     c                   clear                   FNLV59ds
005200160819     c                   eval      iLV59tla = i48tla
005300160818     c                   z-add     kci           iLV59kci
005400160818     c                   z-add     i48ksc        iLV59ksc
005500160818     c                   z-add     i48dsp        iLV59dsp                       Dta Sped x Rif. Tar.
005600160818     c                   movel     i48fie        iLV59fie
005700160818     c                   eval      iLV59ta2 = 'B'
005800160818     c                   eval      iLV59blo = 'S'
005900160818     c                   movel     i48tsp        iLV59tsp
006000160818     c                   movel     'S'           iLV59con
006100160818     c                   move      i48ctr        iLV59ctr
006200990407      *
006300160818     c                   call      'FNLV59R'
006400160818     c                   parm                    FNLV59DS
006500990407
006600990407      * Se trovato progressivo aggancio le tariffe
006700160818    2c                   IF        oLV59ERR = *BLANKS  and  oLV59TKS = 'S'
006800070606
006900070606      * aggancio Tntam per recupero divisa , flag rinuncia ac base
007000070606     c                   clear                   TAMflo
007100070606      *
007200070606     c     ktam          chain     tntam01l                           30
007300070606      *
007400070606     c                   movel     tamflo        dsta01
007500070606      * recupero la divisa della tariffa
007600070606    2c                   IF        *IN30 = *ON  or  §tadiv = *Blanks
007700070606     c                   eval      divisa  = 'ITL'
007800070606     c                   else
007900070606     c                   eval      divisa  = §tadiv
008000070606     c                   ENDIF
008100990407      *
008200070606     c                   movel     I48ksc        Kfil              3 0
008300070606      *
008400070606     c     Kfil          chain     azorg
008500070606      *
008600070606     c                   IF        %found(AZORG01L) and ORGFVA = *blanks
008700070606     C                   movel     ORGDE3        OG143
008800070606     c                   IF        §OGntw = 'DPD'
008900070606      * se cliente dpd aggancio la tabella STD DPD per recuperare il mandato a valore fisso per i
009000070606      * clienti DPD
009100070606     c                   eval      Ktbecod = 'STD'
009200070606     c                   eval      Ktbeke1 = 'DPD'
009300070606     c     Ktbe          chain     tntbe01l
009400070606     c                   If        %found(tntbe01l) and tbeatb = *blank
009500070606     c                   eval      o48mand  = 'S'
009600070606     c                   movel     tbeuni        dstddpd
009700070607      *
009800070607     c                   eval      o48vlm=§stddpdvlm
009900070607     c                   eval      o48vvm=§stddpdvvm
010000070607     c                   eval      o48fvm=§stddpdfvm
010100070607     c                   eval      o48ftc=§stddpdftc
010200070607     c                   eval      o48prg=0
010300070607
010400070606     c                   endif
010500070606     c                   endif
010600070606     c                   endif
010700070606     c                   if        i48mns='S' and o48mand = 'N'
010800060307      *   Mandato Per Conto "Nascosto" AC PLUS REALE
010900990407     c                   movel     '8'           wftc
011000990407     c                   exsr      AGGTAR
011100990407      *   Elevazione Limite Risarcibile "Nascosto" (se ho il valore in bolla non ricerco)
011200990407     c                   IF         o48MAND = 'N'
011300990407     c                   movel     '9'           wftc
011400990407     c                   exsr      AGGTAR
011500990407     c                   ENDIF
011600990407     c                   ENDIF
011700060307      *   Mandato Per Conto AC PULS
011800990407     c                   IF         o48MAND = 'N'
011900990407     c                   movel     'C'           wftc
012000990407     c                   exsr      AGGTAR
012100990407     c                   ENDIF
012200990407      *   Elevazione Limite Risarcibile (se ho il valore in bolla non ricerco)
012300990407     c                   IF         o48MAND = 'N'
012400990407     c                   movel     'R'           wftc
012500990407     c                   exsr      AGGTAR
012600990407     c                   ENDIF
012700060307      *   Mandato Per Conto base AC BASE se data spedizione > = 01032006
012800060307     c                   IF         o48MAND = 'N' and i48dsp > 20060228
012900060307     c                   movel     'd'           wftc
013000060307     c                   exsr      AGGTAR
013100060307     c                   ENDIF
013200990407      *
013300990407     c                   ENDIF
013400990407      *
013500990407     c                   if        o48mand='N'
013600990407     c                   clear                   o48vlm
013700010907     c                   clear                   o48vvm
013800990407     c                   clear                   o48fvm
013900990407     c                   clear                   o48ftc
014000990407     c                   clear                   o48prg
014100990407     c                   endif
014200990408     c                   endif
014300990407     c
014400990408     c                   if        i48tla<>' '
014500990407     c                   eval      *inlr = *on
014600990408     c                   else
014700990408     c                   eval      *inrt = *on
014800990408     c                   endif
014900990407      **********************************************************************
015000990407      * AGGANCIO TARIFFE
015100990407      **********************************************************************
015200990407     C     AGGtar        BEGSR
015300991105      *
015400060307      * verifico se ha rifiutato in testata la tariffa AC BASE e vado a fine
015500060307     c                   If        tamfmp = 'S' and Wftc = 'd'
015600060307     c                   movel     'N'           o48mand                        MANDATO
015700060307     c                   goto      endaggtar
015800060307     c                   endif
015900060307
016000990407      *
016100990818     c     ktpt          chain     titpt01l                           30
016200990407      *
016300990407      * Controllo se tariffa nell'ambito corretto
016400990407    2c                   IF        *IN30 = *off  AND  TPTatb = *blanks
016500990407     c                             AND  (tptapl = *blanks  or
016600990407     c                                   (tptapl = 'A'  and  i48por = 'A')    or
016700990407     c                                   (tptapl = 'P'  and  i48por = 'F')  )
016800990407      *
016900990407     c                   movel     'S'           o48mand                        MANDATO
017000010907      * se la divisa non è quella della moneta corrente converto
017100010907      * l'importo TPTVLM
017200991108     c                   Z-ADD     tptvlm        comodo
017300010910     c                   if        divisa <> §gedDBA
017400991105     c                   clear                   yeur
017500991105     c                   movel     divisa        YECDVI
017600010910     c                   movel     §gedDBA       YECDVO
017700991108     c                   Z-ADD     comodo        YECIMI
017800010907     c                   MOVE      §cvDEC        YECDCO
017900991105     C*
018000991105     C                   CALL      'YEURCO'
018100991105     C                   PARM                    YEUR
018200991105     C*
018300991105    5C                   if        yecesi  = ' '
018400991105     C                   Z-ADD     YECIMO        COMODO
018500991105     C                   ELSE
018600991105     C                   Z-ADD     tptvlm        COMODO
018700991105    5C                   END
018800991108      *
018900991108     c                   end
019000010907      *
019100070620     C*                  IF        comodo = 0
019200070620     C*                  clear                   divisa
019300070620     C*                  ENDIF
019400991105      *
019500991108     c                   eval      o48vlm=comodo
019600010912     c                   eval      o48vvm=§geddba
019700990407     c                   eval      o48fvm=tptfvm
019800990407     c                   eval      o48ftc=tptftc
019900160818     c                   eval      o48prg=oLV59prg
020000990407
020100991005      * Se Mandato Per Conto controllo se è un mandato fittizio
020200990407     c                   IF        WFTC = 'C'
020300990407      *
020400991005    2c                   IF        tpttma <> *Blanks
020500991005     c                   move      'N'           o48mand
020600991005     c                   ENDIF
020700990407      *
020800990407     c                   ELSE
020900990407      *
021000990407      * Per il Limite Risarcibile il valore deve essere > di quello stabilito per legge
021100990408      * Anche se per le tariffe estere il confronto dovrebbe essere con
021200990408      * 8,33 ecu perche' assicurato su base cmr lasciamo il confronto con
021300990408      * le 12.000 dal momento che in ogni parte e' cosi'
021400991105      *
021500060307     c****               IF         TPTvlm <= §GElrp   AND
021600060307     c                   IF         TPTvlm <= 6,2      AND
021700991105     c                              (WFTC = 'R'  or  WFTC = '9')
021800990407     c                   move      'N'           o48mand
021900990407     c                   ENDIF
022000990407     c                   ENDIF
022100990407
022200990407     c                   ENDIF
022300990202      *
022400060307     C     endaggtar     ENDSR
022500010907      **********************************************************************
022600010907      * CONTROLLO VALIDITA' CODICE VALUTA
022700010907      **********************************************************************
022800010907     C     CTRVLT        BEGSR
022900010907      *
023000010907     C                   clear                   DSCV
023100010907      *
023200010907     C                   Z-ADD     1             CODUT
023300010907     C                   MOVEL     'CV'          KCOD
023400010912     C                   MOVEL(P)  §geddba       KKEY
023500010907     C     KTAB          CHAIN     TABEL00F                           30
023600010907      * ERRORE
023700010907     C                   if        *IN30 = *ON    OR
023800010907     C                              ( *IN30 = *OFF  and  TBLFLG <> *blanks)
023900010907     C***                   clear                   divisa
024000010907     C                   else
024100010907      * OK
024200010907     C                   MOVEL     TBLUNI        DSCV
024300010907     C                   endif
024400010907      *
024500010907     C                   ENDSR
024600990127      *****************************************************************
024700990127      *   ROUTINE INIZIALE
024800990127      *****************************************************************
024900990127     C     *INZSR        BEGSR
025000990127      *
025100990127     C     *ENTRY        PLIST
025200990127     C                   PARM                    KPJBA
025300990407     C                   PARM                    FIDN48DS
025400990209      *
025500990209     C                   Z-ADD     1             CODUT
025600990209     C                   CALL      'X§PARUT'
025700990209     C                   PARM                    UT§DSE0F
025800990209     C                   MOVEL     RAGUT         RSUT             20
025900990209     C                   MOVEL     REC80         CNCR80
026000990407      *
026100990407      *--- RICERCA CAPOCONTI
026200990407     C                   DO        50            XX                3 0
026300990407     C                   MOVE      TCU(XX)       TCUDS
026400990407     C     f56           CABNE     'CG'          END1
026500990407     C     f4            COMP      '1'                                    21
026600990407     C     f4            COMP      '2'                                    22
026700990407     C     f4            COMP      '3'                                    23
026800990407     C     f4            COMP      '6'                                    27
026900990407     C** 1 CLIENTI   21
027000990407     C** 2 FORNITORI 22
027100990407     C** 3 AGENTI    23
027200990407     C     f3            comp      '0'                                242425
027300990407     C     f3            comp      'I'                                    26
027400990407     C** 0 ITALIA   25
027500990407     C** 1 ESTERO   24
027600990407     ** I CAPO CONTO IVA
027700990407     c                   if        *in21 = *on
027800990407     C   24              Z-ADD     KCU(XX)       KCE               4 0
027900990407     C   25              Z-ADD     KCU(XX)       KCI               4 0
028000990407     c                   end
028100990407     c                   if        *in22 = *on
028200990407     C   24              Z-ADD     KCU(XX)       KFE               4 0
028300990407     C   25              Z-ADD     KCU(XX)       KFI               4 0
028400990407     c                   end
028500990407     c                   if        *in23 = *on
028600990407     C   24              Z-ADD     KCU(XX)       KAE               4 0
028700990407     C   25              Z-ADD     KCU(XX)       KAI               4 0
028800990407     c                   end
028900990407     C   26              Z-ADD     KCU(XX)       KIVA              4 0
029000990407     C   27              Z-ADD     KCU(XX)       KBNA              4 0
029100990407     C     end1          tag
029200990407     C                   end
029300990407     C                   SETOFF                                       212223
029400990407     C                   SETOFF                                       242526
029500990407     C                   SETOFF                                       27
029600010907      *
029700010907      * recupero la moneta corrente
029800010907     C                   CLEAR                   TIBS02DS
029900010907      *
030000010907     C                   MOVEL     'L'           T02TLA
030100010907     C                   MOVEL     'C'           T02MOD
030200010907     C                   MOVEL     KNSIF         T02SIF
030300010907     C                   MOVEL     'GED'         T02COD
030400010907     C                   MOVEL     'DANNI'       T02KE1
030500010907      *
030600010907     C                   CALL      'TIBS02R'
030700010907     C                   PARM                    KPJBA
030800010907     C                   PARM                    TIBS02DS
030900010907      *
031000010910     C                   MOVEL     T02UNI        DGEDDN
031100010912      *
031200010912     c                   exsr      CTRVLT
031300010316      *
031400990407     c
031500990407     C     KTPT          KLIST
031600990407     C                   KFLD                    i48KSC
031700990407     C                   KFLD                    i48CTR
031800160818     C                   KFLD                    oLV59PRG
031900990407     C                   KFLD                    WFTC
032000990407     C     KTAM          KLIST
032100990407     C                   KFLD                    i48KSC
032200990407     C                   KFLD                    i48CTR
032300160818     C                   KFLD                    oLV59PRG
032400990407     C     KTAB          KLIST
032500010907     C                   KFLD                    CODUT
032600990407     C                   KFLD                    KCOD
032700990407     C                   KFLD                    KKEY
032800070606     C     KTbe          KLIST
032900070606     C                   KFLD                    Ktbecod
033000070606     C                   KFLD                    Ktbeke1
033100990407      *
033200990407     c                   endsr
