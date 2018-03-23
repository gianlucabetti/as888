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
001300990407     D FNLV51DS      E DS
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
005100990407     c                   clear                   FNLV51DS
005200990407     c                   z-add     i48dsp        d51dsp                         Dta Sped x Rif. Tar.
005300990407     c                   z-add     kci           d51kci
005400990407     c                   z-add     i48ksc        d51ksc
005500990408     c                   movel     'Y'           d51ric
005600990407     c                   movel     i48tsp        d51tsp
005700990407     c                   movel     'S'           d51con
005800990407     c                   move      i48ctr        d51ctr
005900990407     c                   movel     i48fie        d51fie
006000990407      *
006100990407     c                   call      'FNLV51R'
006200990407     c                   parm                    FNLV51DS
006300990407
006400990407      * Se trovato progressivo aggancio le tariffe
006500990407    2c                   IF        D51ERR = *BLANKS  and  D51TKS = 'S'
006600070606
006700070606      * aggancio Tntam per recupero divisa , flag rinuncia ac base
006800070606     c                   clear                   TAMflo
006900070606      *
007000070606     c     ktam          chain     tntam01l                           30
007100070606      *
007200070606     c                   movel     tamflo        dsta01
007300070606      * recupero la divisa della tariffa
007400070606    2c                   IF        *IN30 = *ON  or  §tadiv = *Blanks
007500070606     c                   eval      divisa  = 'ITL'
007600070606     c                   else
007700070606     c                   eval      divisa  = §tadiv
007800070606     c                   ENDIF
007900990407      *
008000070606     c                   movel     I48ksc        Kfil              3 0
008100070606      *
008200070606     c     Kfil          chain     azorg
008300070606      *
008400070606     c                   IF        %found(AZORG01L) and ORGFVA = *blanks
008500070606     C                   movel     ORGDE3        OG143
008600070606     c                   IF        §OGntw = 'DPD'
008700070606      * se cliente dpd aggancio la tabella STD DPD per recuperare il mandato a valore fisso per i
008800070606      * clienti DPD
008900070606     c                   eval      Ktbecod = 'STD'
009000070606     c                   eval      Ktbeke1 = 'DPD'
009100070606     c     Ktbe          chain     tntbe01l
009200070606     c                   If        %found(tntbe01l) and tbeatb = *blank
009300070606     c                   eval      o48mand  = 'S'
009400070606     c                   movel     tbeuni        dstddpd
009500070607      *
009600070607     c                   eval      o48vlm=§stddpdvlm
009700070607     c                   eval      o48vvm=§stddpdvvm
009800070607     c                   eval      o48fvm=§stddpdfvm
009900070607     c                   eval      o48ftc=§stddpdftc
010000070607     c                   eval      o48prg=0
010100070607
010200070606     c                   endif
010300070606     c                   endif
010400070606     c                   endif
010500070606     c                   if        i48mns='S' and o48mand = 'N'
010600060307      *   Mandato Per Conto "Nascosto" AC PLUS REALE
010700990407     c                   movel     '8'           wftc
010800990407     c                   exsr      AGGTAR
010900990407      *   Elevazione Limite Risarcibile "Nascosto" (se ho il valore in bolla non ricerco)
011000990407     c                   IF         o48MAND = 'N'
011100990407     c                   movel     '9'           wftc
011200990407     c                   exsr      AGGTAR
011300990407     c                   ENDIF
011400990407     c                   ENDIF
011500060307      *   Mandato Per Conto AC PULS
011600990407     c                   IF         o48MAND = 'N'
011700990407     c                   movel     'C'           wftc
011800990407     c                   exsr      AGGTAR
011900990407     c                   ENDIF
012000990407      *   Elevazione Limite Risarcibile (se ho il valore in bolla non ricerco)
012100990407     c                   IF         o48MAND = 'N'
012200990407     c                   movel     'R'           wftc
012300990407     c                   exsr      AGGTAR
012400990407     c                   ENDIF
012500060307      *   Mandato Per Conto base AC BASE se data spedizione > = 01032006
012600060307     c                   IF         o48MAND = 'N' and i48dsp > 20060228
012700060307     c                   movel     'd'           wftc
012800060307     c                   exsr      AGGTAR
012900060307     c                   ENDIF
013000990407      *
013100990407     c                   ENDIF
013200990407      *
013300990407     c                   if        o48mand='N'
013400990407     c                   clear                   o48vlm
013500010907     c                   clear                   o48vvm
013600990407     c                   clear                   o48fvm
013700990407     c                   clear                   o48ftc
013800990407     c                   clear                   o48prg
013900990407     c                   endif
014000990408     c                   endif
014100990407     c
014200990408     c                   if        i48tla<>' '
014300990407     c                   eval      *inlr = *on
014400990408     c                   else
014500990408     c                   eval      *inrt = *on
014600990408     c                   endif
014700990407      **********************************************************************
014800990407      * AGGANCIO TARIFFE
014900990407      **********************************************************************
015000990407     C     AGGtar        BEGSR
015100991105      *
015200060307      * verifico se ha rifiutato in testata la tariffa AC BASE e vado a fine
015300060307     c                   If        tamfmp = 'S' and Wftc = 'd'
015400060307     c                   movel     'N'           o48mand                        MANDATO
015500060307     c                   goto      endaggtar
015600060307     c                   endif
015700060307
015800990407      *
015900990818     c     ktpt          chain     titpt01l                           30
016000990407      *
016100990407      * Controllo se tariffa nell'ambito corretto
016200990407    2c                   IF        *IN30 = *off  AND  TPTatb = *blanks
016300990407     c                             AND  (tptapl = *blanks  or
016400990407     c                                   (tptapl = 'A'  and  i48por = 'A')    or
016500990407     c                                   (tptapl = 'P'  and  i48por = 'F')  )
016600990407      *
016700990407     c                   movel     'S'           o48mand                        MANDATO
016800010907      * se la divisa non è quella della moneta corrente converto
016900010907      * l'importo TPTVLM
017000991108     c                   Z-ADD     tptvlm        comodo
017100010910     c                   if        divisa <> §gedDBA
017200991105     c                   clear                   yeur
017300991105     c                   movel     divisa        YECDVI
017400010910     c                   movel     §gedDBA       YECDVO
017500991108     c                   Z-ADD     comodo        YECIMI
017600010907     c                   MOVE      §cvDEC        YECDCO
017700991105     C*
017800991105     C                   CALL      'YEURCO'
017900991105     C                   PARM                    YEUR
018000991105     C*
018100991105    5C                   if        yecesi  = ' '
018200991105     C                   Z-ADD     YECIMO        COMODO
018300991105     C                   ELSE
018400991105     C                   Z-ADD     tptvlm        COMODO
018500991105    5C                   END
018600991108      *
018700991108     c                   end
018800010907      *
018900070620     C*                  IF        comodo = 0
019000070620     C*                  clear                   divisa
019100070620     C*                  ENDIF
019200991105      *
019300991108     c                   eval      o48vlm=comodo
019400010912     c                   eval      o48vvm=§geddba
019500990407     c                   eval      o48fvm=tptfvm
019600990407     c                   eval      o48ftc=tptftc
019700990909     c                   eval      o48prg=D51prg
019800990407
019900991005      * Se Mandato Per Conto controllo se è un mandato fittizio
020000990407     c                   IF        WFTC = 'C'
020100990407      *
020200991005    2c                   IF        tpttma <> *Blanks
020300991005     c                   move      'N'           o48mand
020400991005     c                   ENDIF
020500990407      *
020600990407     c                   ELSE
020700990407      *
020800990407      * Per il Limite Risarcibile il valore deve essere > di quello stabilito per legge
020900990408      * Anche se per le tariffe estere il confronto dovrebbe essere con
021000990408      * 8,33 ecu perche' assicurato su base cmr lasciamo il confronto con
021100990408      * le 12.000 dal momento che in ogni parte e' cosi'
021200991105      *
021300060307     c****               IF         TPTvlm <= §GElrp   AND
021400060307     c                   IF         TPTvlm <= 6,2      AND
021500991105     c                              (WFTC = 'R'  or  WFTC = '9')
021600990407     c                   move      'N'           o48mand
021700990407     c                   ENDIF
021800990407     c                   ENDIF
021900990407
022000990407     c                   ENDIF
022100990202      *
022200060307     C     endaggtar     ENDSR
022300010907      **********************************************************************
022400010907      * CONTROLLO VALIDITA' CODICE VALUTA
022500010907      **********************************************************************
022600010907     C     CTRVLT        BEGSR
022700010907      *
022800010907     C                   clear                   DSCV
022900010907      *
023000010907     C                   Z-ADD     1             CODUT
023100010907     C                   MOVEL     'CV'          KCOD
023200010912     C                   MOVEL(P)  §geddba       KKEY
023300010907     C     KTAB          CHAIN     TABEL00F                           30
023400010907      * ERRORE
023500010907     C                   if        *IN30 = *ON    OR
023600010907     C                              ( *IN30 = *OFF  and  TBLFLG <> *blanks)
023700010907     C***                   clear                   divisa
023800010907     C                   else
023900010907      * OK
024000010907     C                   MOVEL     TBLUNI        DSCV
024100010907     C                   endif
024200010907      *
024300010907     C                   ENDSR
024400990127      *****************************************************************
024500990127      *   ROUTINE INIZIALE
024600990127      *****************************************************************
024700990127     C     *INZSR        BEGSR
024800990127      *
024900990127     C     *ENTRY        PLIST
025000990127     C                   PARM                    KPJBA
025100990407     C                   PARM                    FIDN48DS
025200990209      *
025300990209     C                   Z-ADD     1             CODUT
025400990209     C                   CALL      'X§PARUT'
025500990209     C                   PARM                    UT§DSE0F
025600990209     C                   MOVEL     RAGUT         RSUT             20
025700990209     C                   MOVEL     REC80         CNCR80
025800990407      *
025900990407      *--- RICERCA CAPOCONTI
026000990407     C                   DO        50            XX                3 0
026100990407     C                   MOVE      TCU(XX)       TCUDS
026200990407     C     f56           CABNE     'CG'          END1
026300990407     C     f4            COMP      '1'                                    21
026400990407     C     f4            COMP      '2'                                    22
026500990407     C     f4            COMP      '3'                                    23
026600990407     C     f4            COMP      '6'                                    27
026700990407     C** 1 CLIENTI   21
026800990407     C** 2 FORNITORI 22
026900990407     C** 3 AGENTI    23
027000990407     C     f3            comp      '0'                                242425
027100990407     C     f3            comp      'I'                                    26
027200990407     C** 0 ITALIA   25
027300990407     C** 1 ESTERO   24
027400990407     ** I CAPO CONTO IVA
027500990407     c                   if        *in21 = *on
027600990407     C   24              Z-ADD     KCU(XX)       KCE               4 0
027700990407     C   25              Z-ADD     KCU(XX)       KCI               4 0
027800990407     c                   end
027900990407     c                   if        *in22 = *on
028000990407     C   24              Z-ADD     KCU(XX)       KFE               4 0
028100990407     C   25              Z-ADD     KCU(XX)       KFI               4 0
028200990407     c                   end
028300990407     c                   if        *in23 = *on
028400990407     C   24              Z-ADD     KCU(XX)       KAE               4 0
028500990407     C   25              Z-ADD     KCU(XX)       KAI               4 0
028600990407     c                   end
028700990407     C   26              Z-ADD     KCU(XX)       KIVA              4 0
028800990407     C   27              Z-ADD     KCU(XX)       KBNA              4 0
028900990407     C     end1          tag
029000990407     C                   end
029100990407     C                   SETOFF                                       212223
029200990407     C                   SETOFF                                       242526
029300990407     C                   SETOFF                                       27
029400010907      *
029500010907      * recupero la moneta corrente
029600010907     C                   CLEAR                   TIBS02DS
029700010907      *
029800010907     C                   MOVEL     'L'           T02TLA
029900010907     C                   MOVEL     'C'           T02MOD
030000010907     C                   MOVEL     KNSIF         T02SIF
030100010907     C                   MOVEL     'GED'         T02COD
030200010907     C                   MOVEL     'DANNI'       T02KE1
030300010907      *
030400010907     C                   CALL      'TIBS02R'
030500010907     C                   PARM                    KPJBA
030600010907     C                   PARM                    TIBS02DS
030700010907      *
030800010910     C                   MOVEL     T02UNI        DGEDDN
030900010912      *
031000010912     c                   exsr      CTRVLT
031100010316      *
031200990407     c
031300990407     C     KTPT          KLIST
031400990407     C                   KFLD                    i48KSC
031500990407     C                   KFLD                    i48CTR
031600990407     C                   KFLD                    D51PRG
031700990407     C                   KFLD                    WFTC
031800990407     C     KTAM          KLIST
031900990407     C                   KFLD                    i48KSC
032000990407     C                   KFLD                    i48CTR
032100990407     C                   KFLD                    D51PRG
032200990407     C     KTAB          KLIST
032300010907     C                   KFLD                    CODUT
032400990407     C                   KFLD                    KCOD
032500990407     C                   KFLD                    KKEY
032600070606     C     KTbe          KLIST
032700070606     C                   KFLD                    Ktbecod
032800070606     C                   KFLD                    Ktbeke1
032900990407      *
033000990407     c                   endsr
