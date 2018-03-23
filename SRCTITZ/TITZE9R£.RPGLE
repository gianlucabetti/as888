000100060221      /TITLE Upload via Internet: traduzione in EDIVATWR. (x STEFANEL/NORDICA Cod. 1330192/0592101)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500060221     FEDIVATwr  O    E             DISK    usropn
000600000313     D*
000700010330     D*----------------------------------------------------
000800010330     D* DICHIARAZIOINE VARIABILI DI WRK
000900010330     D*----------------------------------------------------
001000010330     D dscmz         e ds                  inz
001100010330     D psds           sds
001200010330     D  procname         *PROC
001300010330     D tivlrds       e ds                  extname(tivlr00f)
001400010330     D esito           s              1
001500010330     D prmlit          s             10
001600010330     D prmfir          s             10
001700010330     D wrkesito        s                   like(esito)
001800010330     D rrnum           s              6  0 INZ(*zeros)
001900060221     D wNomeFile       s             30    INZ(*blanks)
002000060221     D wCMR            s             35    INZ(*blanks)
002100140328     D wNOT_E          s             35    INZ(*blanks)
002200140328     D wNOT_S          s             35    INZ(*blanks)
002300140328     D wNOT_I          s             35    INZ(*blanks)
002400140328     D wNOT_J          s             35    INZ(*blanks)
002500140328     D w70             s             70    INZ(*blanks)
002600010330     D*------------------
002700010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002800010330     D*------------------
002900010330     D WLBDA8          DS                  INZ
003000010330     D  G08DAT                 1      8  0
003100010330     D  G08INV                 9     16  0
003200010330     D  G08ERR                17     17
003300010330     D  G08TGI                18     22  0
003400000313     D*
003500010330
003600010330
003700010330
003800000913     C                   reset                   rrnum
003900990921     C                   reset                   esito
004000990921     C                   reset                   wrkesito
004100010601     C*
004200010601     C                   exsr      opeini
004300020321     C                   exsr      rwvat
004400010601     C*
004500010601     C                   seton                                        lr
004600010601
004700010601
004800010601
004900010601
005000010601     C*--------------------------------------------------------
005100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005200010601     C*--------------------------------------------------------
005300010601     C     PREELA        BEGSR
005400010601     C*
005500010601     C* SE OCCORRE SPEDIRE IN FILIALE
005600010601     C                   if        invfil <> *zeros and
005700010601     C                             flgGiro = '0'
005800010601     C*
005900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006000010601     C                   eval      flgGiro = '1'
006100010601     C*
006200010601     C                   endif
006300010601     C*
006400010601     C                   ENDSR
006500010601     C***
006600010601
006700010601
006800010601
006900010601
007000010601     C*--------------------------------------------------------
007100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007200010601     C*--------------------------------------------------------
007300010601     C     ENDELA        BEGSR
007400000616     C*
007500010601     C                   ENDSR
007600010601     C***
007700000613
007800010330
007900010601
008000010601
008100010330     C*--------------------------------------------------------
008200060221     C* RWVAT   LEGGE TIVIN00R E SCRIVE EDIVATWF              *
008300010330     C*--------------------------------------------------------
008400020321     C     RWVAT         BEGSR
008500010330     C*
008600010330     C                   if        not %open(tivin00r)
008700010330     C                   open      tivin00r
008800010330     C                   endif
008900060221     C                   if        not %open(edivatwr)
009000060221     C                   open      edivatwr
009100010330     C                   endif
009200010330     C*
009300010604     C                   clear                   §CTROK
009400010604     C                   clear                   §CTRMO
009500010604     C                   clear                   §CTRNO
009600010330     C*
009700010330     C                   DO        *HIVAL
009800010330     C*
009900010330     C                   READ      tivin00r                               70
010000010618     C*
010100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
010200010618     C                   if        vinflg = '1'
010300010618     C                   eval      flgOk = '1'
010400010618     C                   endif
010500010618     C*
010600010330     C                   if        vindta > *blanks
010700010330     C                   add       1             rrnum
010800010330     C*
010900010601     C                   if        *in70 = *off and
011000010330     C                             (vinflg = *blanks
011100010330     C                              or vinflg = '0'
011200010330     C                              or vinflg = '2')
011300010330     C*
011400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011500010711     C                   if        vinflg = *blanks or vinflg = '0'
011600010711     C                   clear                   vinmsg
011700010711     C                   endif
011800010601     C*
011900010330     C                   exsr      inzvar
012000010330     C                   exsr      defcam
012100020321     C                   exsr      impvat
012200010601     C*
012300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012400020321     C                   eval      depfil = VATLNP
012500010601     C                   exsr      repfil
012600140328     C                   if        depfil = invfil
012700021025     C                   if        vlrpoi = 999
012800021025     C                   MOVE(P)   invfil        VATFGS
012900021025     C                   else
013000021025     C                   MOVE(P)   vlrpoi        VATFGS
013100021025     C                   endif
013200010601     C*
013300010601     C                   exsr      PREELA
013400060221     C*
013500060221     C* VALORIZZO CAMPI RELATIVI AL "CMR"
013600060221     C                   EVAL      VATCMR = wCMR
013700060221     C                   EVAL      VATCNT = 1
013800010601     C*
013900010604     C* Ebbene...
014000010604     C*
014100010604     C  N31              ADD       1             §CTROK            7 0
014200010604     C   32              ADD       1             §CTRMO            7 0
014300010604     C   31              ADD       1             §CTRNO            7 0
014400020205     C*
014500020205     C* Effettuo test particolare solo x DA ROLD x condizoinare la scrittura
014600140328     C                   IF        *in31 = *off
014700140328     C                   IF        wNOT_I <> *blank
014800140328     C                   EVAL      VATTRC = 'I'
014900140328     C                   EVAL      VATNOT = wNOT_I
015000140328     C                   WRITE     EDIVAT00
015100140328     C                   ENDIF
015200140328     C                   IF        wNOT_J <> *blank
015300140328     C                   EVAL      VATTRC = 'J'
015400140328     C                   EVAL      VATNOT = wNOT_J
015500140328     C                   WRITE     EDIVAT00
015600140328     C                   ENDIF
015700140328     C                   IF        wNOT_S <> *blank
015800140328     C                   EVAL      VATTRC = 'S'
015900140328     C                   EVAL      VATNOT = wNOT_S
016000140328     C                   WRITE     EDIVAT00
016100140328     C                   ENDIF
016200140328     C                   IF        wNOT_E <> *blank
016300140328     C                   EVAL      VATTRC = 'E'
016400140328     C                   EVAL      VATNOT = wNOT_E
016500140328     C                   WRITE     EDIVAT00
016600140328     C                   ENDIF
016700140328     C                   ENDIF
016800010604     C*
016900010604     C                   if        *in31 = *off and
017000010604     C                             *in32 = *off
017100010604     C                   eval      vinflg = '1'
017200010604     C                   else
017300010604     C                   eval      vinflg = '2'
017400010604     C                   endif
017500010604     C                   endif
017600010601     C*
017700010604     C                   endif
017800010604     C*
017900010330     C                   else
018000010330     C                   eval      vinflg = '1'
018100010330     C                   endif
018200010601     C*
018300010601     C  N70              update    tivin000
018400010330     C*
018500010330     C  N70              ENDdo
018600010601     C
018700010601     C*
018800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
018900010601     C                   if        cntNonEl = *zeros or
019000010601     C                             flgMulti = '0'
019100010330     C* Se non ci sono record con errori ...
019200010601     C                   if        §ctrno = 0 and
019300010604     C                             §ctrmo = 0 and
019400010601     C                             flgStato <> '2'
019500010330     C* ... restituisco esito OK.
019600010330     C                   eval      wrkesito = '0'
019700010330     C                   else
019800010330     C                   if        §ctrok > 0
019900010330     C                   eval      wrkesito = '1'
020000010330     C                   else
020100010615     C                   if        flgOk = '0'
020200010615     C                   eval      wrkesito = '2'
020300010615     C                   else
020400010615     C                   eval      wrkesito = '6'
020500010615     C                   endif
020600010330     C                   endif
020700010330     C                   endif
020800010601     C                   else
020900010601     C                   eval      wrkesito = '9'
021000010601     C                   endif
021100010330     C*
021200010330     C                   if        %open(tivin00r)
021300010330     C                   close     tivin00r
021400010330     C                   endif
021500060221     C                   if        %open(edivatwr)
021600060221     C                   close     edivatwr
021700010330     C                   endif
021800010601     C*
021900010601     C                   if        vlrpoi <> 999
022000010601     C                   eval      invfil = vlrpoi
022100010601     C                   endif
022200010330     C*
022300010330     C                   if        §ctrok > 0
022400010601     C                             and invfil > *zeros
022500010330     C                   exsr      invio
022600010330     C                   endif
022700010601     C*
022800010618     C                   if        flgGiro = '1'
022900010601     C                   exsr      endela
023000010618     C                   endif
023100010330     C*
023200010330     C                   ENDSR
023300010330     C***
023400010330
023500010601
023600010601
023700010601
023800010330     C*----------------------------------------------------*
023900010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
024000010330     C*----------------------------------------------------*
024100040218     C     INZVAR        BEGSR
024200010330     C*
024300010330     C*
024400010330     C                   ENDSR
024500010330     C*----------------------------------------------------*
024600010330     C*  IMPOSTAZIONE CAMPI COSTANTI
024700010330     C*----------------------------------------------------*
024800010330     C     DEFCAM        BEGSR
024900010330     C*
025000020204     C* Inizializzo il buffer del record da scrivere
025100060221     C                   CLEAR                   EDIVAT00
025200020204     C* Imposto i valori di default...
025300040218     C                   EVAL      VATCCM = 1330192
025400040218     C                   EVAL      VATLNP = 133
025500140328     C***                EVAL      VATTRC = 'E'
025600020204     C* ... e poi verifico se sono stati passati come parametri
025700020204     C                   IF        vlrppt > *blanks
025800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
025900020204     C                   EXSR      CHKNUM
026000020204     C                   IF        PiInt=*on
026100020321     C                   Z-ADD     PiVal         VATCCM
026200020204     C                   ENDIF
026300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
026400020204     C                   EXSR      CHKNUM
026500020204     C                   IF        PiInt=*on
026600020321     C                   Z-ADD     PiVal         VATLNP
026700020204     C                   ENDIF
026800020321     C                   ENDIF
026900020204     C*
027000010330     C                   ENDSR
027100010607     C*----------------------------------------------------*
027200060221     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
027300010607     C*----------------------------------------------------*
027400020321     C     IMPVAT        BEGSR
027500010607     C*
027600010607     C                   SETOFF                                       3132
027700040218     C*
027800040218     C                   MOVEL     datcor        VATAAS
027900010607     C*
028000010607     C* Reperimento campi ALFA
028100140328     C* _E
028200140328     C                   EVAL      wNOT_E=%trim(%subst(vindta:42:18))
028300140328     C* _I/J
028400140328     C                   eval      wNOT_I=*blank
028500140328     C                   eval      wNOT_J=*blank
028600140328     C                   eval      w70=*blank
028700140328     C                   eval      w70 = %trim(%subst(vindta:271:50))
028800140328     C                   eval      wNOT_I = %subst(w70:1:35)
028900140328     C                   eval      wNOT_J = %subst(w70:36:35)
029000140328     C* _S
029100140328     C                   eval      wNOT_S=*blank
029200140328     C                   eval      %subst(wNOT_S:1:16) =
029300140328     C                               %trim(%subst(vindta:241:30))
029400140328     C                   eval      %subst(wNOT_S:17:2) = *blank
029500020304     C*
029600040218     C* VATNSP
029700040218     C                   EVAL      PiStr=%trim(%subst(vindta:121:5))
029800020304     C                   EXSR      CHKNUM
029900020304     C                   IF        PiInt=*on
030000020321     C                   Z-ADD     PiVal         VATNSP
030100020304     C                   ELSE
030200020304     C                   SETON                                        32
030300020321     C                   Z-ADD     *zeros        VATNSP
030400020304     C                   EVAL      vinmsg = %trimr(vinmsg)
030500020321     C                             + ' ' + 'VATNSP'
030600020304     C                   ENDIF
030700020204     C*
030800010607     C                   ENDSR
030900010607     C*----------------------------------------------------*
031000010601
031100010330
031200010330     C*----------------------------------------------------*
031300010330     C*  CONTROLLO NUMERICITA' CAMPI
031400010330     C*----------------------------------------------------*
031500010330     C     CHKNUM        BEGSR
031600010330     C*
031700010606     C                   IF        PiDecChr = *blanks
031800010606     C                   EVAL      PiDecChr = ','
031900010606     C                   ENDIF
032000010606     C*
032100010606     C                   CALL(e)   'ISNUMERIC'
032200010330     C                   PARM                    PiStr            30
032300010606     C                   PARM                    PiDecChr          1
032400010330     C                   PARM      *ZEROS        PiVal            30 9
032500010330     C                   PARM      '0'           PiInt             1
032600010330     C                   PARM      '0'           PiNum             1
032700010330     C                   IF        %error
032800010606     C                   EVAL      PiNum=*off
032900010330     C                   ENDIF
033000010330     C*
033100010330     C                   ENDSR
033200010330     C***
033300020403
033400020403
033500010601
033600010601
033700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
033800010601     C     repfil        BEGSR
033900010601     C*
034000010601     C                   if        invfil = *zeros and
034100010601     C                             depfil > *zeros and
034200010629     C                             (vinflg = *blanks or
034300010629     C                              vinflg = *zeros)
034400010601     C
034500010601     C                   eval      invfil = depfil
034600010601     C                   endif
034700010601     C*
034800010601     C                   if        depfil <> invfil and
034900010601     C                             invfil > *zeros
035000010601     C                   eval      flgMulti = '1'
035100010601     C                   if        vinflg = *blanks
035200010601     C                   add       1             cntNonEl
035300010601     C                   endif
035400010601     C                   endif
035500010601     C*
035600010601     C                   if        vinflg = '2'
035700010601     C                   eval      flgStato = '2'
035800010601     C                   endif
035900010601     C*
036000010601     C                   ENDSR
036100010601     C***
036200010601
036300010601
036400010601
036500010330
036600010330
036700010330
036800990920      /TITLE Invio dei dati al punto operativo.
036900000613     C     invio         BEGSR
037000990920     C*
037100990920     C                   reset                   dscmz
037200010601     C                   move      invfil        cmzdst
037300990920     C                   eval      cmzfld = vlrfou
037400990920     C                   eval      cmzmbd = vlrhdl
037500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
037600000710     C                   if        prmfir = *blanks
037700060221     C                   eval      cmzfla = 'EDIVAT0F'
037800060221     C                   eval      cmzmba = 'EDIVAT0F'
037900000710     C                   else
038000000710     C                   eval      cmzfla = prmfir
038100000710     C                   eval      cmzmba = prmfir
038200000710     C                   endif
038300990920     C                   eval      cmznrr = *zeros
038400990920     C                   move      §ctrok        cmznrr
038500021018     C                   eval      cmzlba = vlrfl1
038600990920     C                   call(e)   'TIS711C'
038700990920     C                   parm                    dscmz
038800990921     C                   parm      *blanks       esito
038900990920     C                   if        %error
039000990920     C                             or cmzerr = '1'
039100990921     C                             or esito  = '1'
039200000710     C                   eval      wrkesito = '3'
039300990920     C                   endif
039400990920     C*
039500000613     C                   ENDSR
039600990910
039700010601
039800010601
039900010601
040000010601
040100010601      /TITLE Invio dei dati al punto operativo.
040200010601     C     opeini        BEGSR
040300010601     C*
040400010601     C* Inizializzo flag e contatori operativi
040500010601     C                   movel     '0'           flgGiro           1
040600010601     C                   movel     '0'           flgMulti          1
040700010601     C                   movel     '1'           flgStato          1
040800010615     C                   movel     '0'           flgOk             1
040900010601     C                   z-add     *zeros        cntNonEl         10 0
041000010601     C                   z-add     *zeros        depfil            3 0
041100010601     C                   z-add     *zeros        invfil            3 0
041200010601     C*
041300010601     C                   ENDSR
041400010601     C***
041500010601
041600010601
041700010601
041800010330
041900010330
042000000613     C     *inzsr        BEGSR
042100990910     C*
042200990910     C     *entry        plist
042300990920     C                   parm                    tivlrds
042400990921     C                   parm      wrkesito      esito
042500000724     C                   parm                    prmlit
042600000710     C                   parm                    prmfir
042700010330     C*
042800010330     C* CALCOLA LA DATA CORRENTE
042900010330     C                   time                    wn14             14 0
043000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
043100010330     C                   z-add     wn8           g08dat
043200010330     C                   z-add     *zeros        g08inv
043300010330     C                   movel     '0'           g08err
043400010330     C                   call      'XSRDA8'
043500010330     C                   parm                    wlbda8
043600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
043700060221     C*
043800060221     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
043900060221     C                   move(p)   vlrMSG        wNomeFile
044000060221     C*
044100060221     C* Reperisco il CMR da attribuire alla transazione
044200060221     C                   eval      wCMR = %subst(wNomeFile:7:6)
044300000613     C*
044400000613     C                   ENDSR
044500000613     C***
