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
002300160603     D wNOT_P          s             35    INZ(*blanks)
002400140328     D wNOT_I          s             35    INZ(*blanks)
002500140328     D wNOT_J          s             35    INZ(*blanks)
002600140328     D w70             s             70    INZ(*blanks)
002700010330     D*------------------
002800010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002900010330     D*------------------
003000010330     D WLBDA8          DS                  INZ
003100010330     D  G08DAT                 1      8  0
003200010330     D  G08INV                 9     16  0
003300010330     D  G08ERR                17     17
003400010330     D  G08TGI                18     22  0
003500000313     D*
003600010330
003700010330
003800010330
003900000913     C                   reset                   rrnum
004000990921     C                   reset                   esito
004100990921     C                   reset                   wrkesito
004200010601     C*
004300010601     C                   exsr      opeini
004400020321     C                   exsr      rwvat
004500010601     C*
004600010601     C                   seton                                        lr
004700010601
004800010601
004900010601
005000010601
005100010601     C*--------------------------------------------------------
005200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005300010601     C*--------------------------------------------------------
005400010601     C     PREELA        BEGSR
005500010601     C*
005600010601     C* SE OCCORRE SPEDIRE IN FILIALE
005700010601     C                   if        invfil <> *zeros and
005800010601     C                             flgGiro = '0'
005900010601     C*
006000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006100010601     C                   eval      flgGiro = '1'
006200010601     C*
006300010601     C                   endif
006400010601     C*
006500010601     C                   ENDSR
006600010601     C***
006700010601
006800010601
006900010601
007000010601
007100010601     C*--------------------------------------------------------
007200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007300010601     C*--------------------------------------------------------
007400010601     C     ENDELA        BEGSR
007500000616     C*
007600010601     C                   ENDSR
007700010601     C***
007800000613
007900010330
008000010601
008100010601
008200010330     C*--------------------------------------------------------
008300060221     C* RWVAT   LEGGE TIVIN00R E SCRIVE EDIVATWF              *
008400010330     C*--------------------------------------------------------
008500020321     C     RWVAT         BEGSR
008600010330     C*
008700010330     C                   if        not %open(tivin00r)
008800010330     C                   open      tivin00r
008900010330     C                   endif
009000060221     C                   if        not %open(edivatwr)
009100060221     C                   open      edivatwr
009200010330     C                   endif
009300010330     C*
009400010604     C                   clear                   §CTROK
009500010604     C                   clear                   §CTRMO
009600010604     C                   clear                   §CTRNO
009700010330     C*
009800010330     C                   DO        *HIVAL
009900010330     C*
010000010330     C                   READ      tivin00r                               70
010100010618     C*
010200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
010300010618     C                   if        vinflg = '1'
010400010618     C                   eval      flgOk = '1'
010500010618     C                   endif
010600010618     C*
010700010330     C                   if        vindta > *blanks
010800010330     C                   add       1             rrnum
010900010330     C*
011000010601     C                   if        *in70 = *off and
011100010330     C                             (vinflg = *blanks
011200010330     C                              or vinflg = '0'
011300010330     C                              or vinflg = '2')
011400010330     C*
011500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011600010711     C                   if        vinflg = *blanks or vinflg = '0'
011700010711     C                   clear                   vinmsg
011800010711     C                   endif
011900010601     C*
012000010330     C                   exsr      inzvar
012100010330     C                   exsr      defcam
012200020321     C                   exsr      impvat
012300010601     C*
012400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012500020321     C                   eval      depfil = VATLNP
012600010601     C                   exsr      repfil
012700140328     C                   if        depfil = invfil
012800021025     C                   if        vlrpoi = 999
012900021025     C                   MOVE(P)   invfil        VATFGS
013000021025     C                   else
013100021025     C                   MOVE(P)   vlrpoi        VATFGS
013200021025     C                   endif
013300010601     C*
013400010601     C                   exsr      PREELA
013500060221     C*
013600060221     C* VALORIZZO CAMPI RELATIVI AL "CMR"
013700060221     C                   EVAL      VATCMR = wCMR
013800060221     C                   EVAL      VATCNT = 1
013900010601     C*
014000010604     C* Ebbene...
014100010604     C*
014200010604     C  N31              ADD       1             §CTROK            7 0
014300010604     C   32              ADD       1             §CTRMO            7 0
014400010604     C   31              ADD       1             §CTRNO            7 0
014500020205     C*
014600020205     C* Effettuo test particolare solo x DA ROLD x condizoinare la scrittura
014700140328     C                   IF        *in31 = *off
014800140328     C                   IF        wNOT_I <> *blank
014900140328     C                   EVAL      VATTRC = 'I'
015000140328     C                   EVAL      VATNOT = wNOT_I
015100140328     C                   WRITE     EDIVAT00
015200140328     C                   ENDIF
015300140328     C                   IF        wNOT_J <> *blank
015400140328     C                   EVAL      VATTRC = 'J'
015500140328     C                   EVAL      VATNOT = wNOT_J
015600140328     C                   WRITE     EDIVAT00
015700140328     C                   ENDIF
015800140328     C                   IF        wNOT_S <> *blank
015900140328     C                   EVAL      VATTRC = 'S'
016000140328     C                   EVAL      VATNOT = wNOT_S
016100140328     C                   WRITE     EDIVAT00
016200140328     C                   ENDIF
016300160603     C                   IF        wNOT_P <> *blank
016400160603     C                   EVAL      VATTRC = 'P'
016500160603     C                   EVAL      VATNOT = wNOT_P
016600160603     C                   WRITE     EDIVAT00
016700160603     C                   ENDIF
016800140328     C                   IF        wNOT_E <> *blank
016900140328     C                   EVAL      VATTRC = 'E'
017000140328     C                   EVAL      VATNOT = wNOT_E
017100140328     C                   WRITE     EDIVAT00
017200140328     C                   ENDIF
017300140328     C                   ENDIF
017400010604     C*
017500010604     C                   if        *in31 = *off and
017600010604     C                             *in32 = *off
017700010604     C                   eval      vinflg = '1'
017800010604     C                   else
017900010604     C                   eval      vinflg = '2'
018000010604     C                   endif
018100010604     C                   endif
018200010601     C*
018300010604     C                   endif
018400010604     C*
018500010330     C                   else
018600010330     C                   eval      vinflg = '1'
018700010330     C                   endif
018800010601     C*
018900010601     C  N70              update    tivin000
019000010330     C*
019100010330     C  N70              ENDdo
019200010601     C
019300010601     C*
019400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
019500010601     C                   if        cntNonEl = *zeros or
019600010601     C                             flgMulti = '0'
019700010330     C* Se non ci sono record con errori ...
019800010601     C                   if        §ctrno = 0 and
019900010604     C                             §ctrmo = 0 and
020000010601     C                             flgStato <> '2'
020100010330     C* ... restituisco esito OK.
020200010330     C                   eval      wrkesito = '0'
020300010330     C                   else
020400010330     C                   if        §ctrok > 0
020500010330     C                   eval      wrkesito = '1'
020600010330     C                   else
020700010615     C                   if        flgOk = '0'
020800010615     C                   eval      wrkesito = '2'
020900010615     C                   else
021000010615     C                   eval      wrkesito = '6'
021100010615     C                   endif
021200010330     C                   endif
021300010330     C                   endif
021400010601     C                   else
021500010601     C                   eval      wrkesito = '9'
021600010601     C                   endif
021700010330     C*
021800010330     C                   if        %open(tivin00r)
021900010330     C                   close     tivin00r
022000010330     C                   endif
022100060221     C                   if        %open(edivatwr)
022200060221     C                   close     edivatwr
022300010330     C                   endif
022400010601     C*
022500010601     C                   if        vlrpoi <> 999
022600010601     C                   eval      invfil = vlrpoi
022700010601     C                   endif
022800010330     C*
022900010330     C                   if        §ctrok > 0
023000010601     C                             and invfil > *zeros
023100010330     C                   exsr      invio
023200010330     C                   endif
023300010601     C*
023400010618     C                   if        flgGiro = '1'
023500010601     C                   exsr      endela
023600010618     C                   endif
023700010330     C*
023800010330     C                   ENDSR
023900010330     C***
024000010330
024100010601
024200010601
024300010601
024400010330     C*----------------------------------------------------*
024500010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
024600010330     C*----------------------------------------------------*
024700040218     C     INZVAR        BEGSR
024800010330     C*
024900010330     C*
025000010330     C                   ENDSR
025100010330     C*----------------------------------------------------*
025200010330     C*  IMPOSTAZIONE CAMPI COSTANTI
025300010330     C*----------------------------------------------------*
025400010330     C     DEFCAM        BEGSR
025500010330     C*
025600020204     C* Inizializzo il buffer del record da scrivere
025700060221     C                   CLEAR                   EDIVAT00
025800020204     C* Imposto i valori di default...
025900040218     C                   EVAL      VATCCM = 1330192
026000040218     C                   EVAL      VATLNP = 133
026100140328     C***                EVAL      VATTRC = 'E'
026200020204     C* ... e poi verifico se sono stati passati come parametri
026300020204     C                   IF        vlrppt > *blanks
026400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
026500020204     C                   EXSR      CHKNUM
026600020204     C                   IF        PiInt=*on
026700020321     C                   Z-ADD     PiVal         VATCCM
026800020204     C                   ENDIF
026900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
027000020204     C                   EXSR      CHKNUM
027100020204     C                   IF        PiInt=*on
027200020321     C                   Z-ADD     PiVal         VATLNP
027300020204     C                   ENDIF
027400020321     C                   ENDIF
027500020204     C*
027600010330     C                   ENDSR
027700010607     C*----------------------------------------------------*
027800060221     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
027900010607     C*----------------------------------------------------*
028000020321     C     IMPVAT        BEGSR
028100010607     C*
028200010607     C                   SETOFF                                       3132
028300040218     C*
028400040218     C                   MOVEL     datcor        VATAAS
028500010607     C*
028600010607     C* Reperimento campi ALFA
028700140328     C* _E
028800140328     C                   EVAL      wNOT_E=%trim(%subst(vindta:42:18))
028900140328     C* _I/J
029000140328     C                   eval      wNOT_I=*blank
029100140328     C                   eval      wNOT_J=*blank
029200140328     C                   eval      w70=*blank
029300140328     C                   eval      w70 = %trim(%subst(vindta:271:50))
029400140328     C                   eval      wNOT_I = %subst(w70:1:35)
029500140328     C                   eval      wNOT_J = %subst(w70:36:35)
029600140328     C* _S
029700140328     C                   eval      wNOT_S=*blank
029800140328     C                   eval      %subst(wNOT_S:1:16) =
029900140328     C                               %trim(%subst(vindta:241:30))
030000140328     C                   eval      %subst(wNOT_S:17:2) = *blank
030100160603     C* _P
030200160603     C                   eval      wNOT_P=*blank
030300160603     C                   eval      wNOT_P =
030400160603     C                               %trim(%subst(vindta:394:60))
030500160627     C                   eval      %subst(wNOT_P:34:2) = '10'                   * stampa in arrivo
030600020304     C*
030700040218     C* VATNSP
030800040218     C                   EVAL      PiStr=%trim(%subst(vindta:121:5))
030900020304     C                   EXSR      CHKNUM
031000020304     C                   IF        PiInt=*on
031100020321     C                   Z-ADD     PiVal         VATNSP
031200020304     C                   ELSE
031300020304     C                   SETON                                        32
031400020321     C                   Z-ADD     *zeros        VATNSP
031500020304     C                   EVAL      vinmsg = %trimr(vinmsg)
031600020321     C                             + ' ' + 'VATNSP'
031700020304     C                   ENDIF
031800020204     C*
031900010607     C                   ENDSR
032000010607     C*----------------------------------------------------*
032100010601
032200010330
032300010330     C*----------------------------------------------------*
032400010330     C*  CONTROLLO NUMERICITA' CAMPI
032500010330     C*----------------------------------------------------*
032600010330     C     CHKNUM        BEGSR
032700010330     C*
032800010606     C                   IF        PiDecChr = *blanks
032900010606     C                   EVAL      PiDecChr = ','
033000010606     C                   ENDIF
033100010606     C*
033200010606     C                   CALL(e)   'ISNUMERIC'
033300010330     C                   PARM                    PiStr            30
033400010606     C                   PARM                    PiDecChr          1
033500010330     C                   PARM      *ZEROS        PiVal            30 9
033600010330     C                   PARM      '0'           PiInt             1
033700010330     C                   PARM      '0'           PiNum             1
033800010330     C                   IF        %error
033900010606     C                   EVAL      PiNum=*off
034000010330     C                   ENDIF
034100010330     C*
034200010330     C                   ENDSR
034300010330     C***
034400020403
034500020403
034600010601
034700010601
034800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
034900010601     C     repfil        BEGSR
035000010601     C*
035100010601     C                   if        invfil = *zeros and
035200010601     C                             depfil > *zeros and
035300010629     C                             (vinflg = *blanks or
035400010629     C                              vinflg = *zeros)
035500010601     C
035600010601     C                   eval      invfil = depfil
035700010601     C                   endif
035800010601     C*
035900010601     C                   if        depfil <> invfil and
036000010601     C                             invfil > *zeros
036100010601     C                   eval      flgMulti = '1'
036200010601     C                   if        vinflg = *blanks
036300010601     C                   add       1             cntNonEl
036400010601     C                   endif
036500010601     C                   endif
036600010601     C*
036700010601     C                   if        vinflg = '2'
036800010601     C                   eval      flgStato = '2'
036900010601     C                   endif
037000010601     C*
037100010601     C                   ENDSR
037200010601     C***
037300010601
037400010601
037500010601
037600010330
037700010330
037800010330
037900990920      /TITLE Invio dei dati al punto operativo.
038000000613     C     invio         BEGSR
038100990920     C*
038200990920     C                   reset                   dscmz
038300010601     C                   move      invfil        cmzdst
038400990920     C                   eval      cmzfld = vlrfou
038500990920     C                   eval      cmzmbd = vlrhdl
038600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
038700000710     C                   if        prmfir = *blanks
038800060221     C                   eval      cmzfla = 'EDIVAT0F'
038900060221     C                   eval      cmzmba = 'EDIVAT0F'
039000000710     C                   else
039100000710     C                   eval      cmzfla = prmfir
039200000710     C                   eval      cmzmba = prmfir
039300000710     C                   endif
039400990920     C                   eval      cmznrr = *zeros
039500990920     C                   move      §ctrok        cmznrr
039600021018     C                   eval      cmzlba = vlrfl1
039700990920     C                   call(e)   'TIS711C'
039800990920     C                   parm                    dscmz
039900990921     C                   parm      *blanks       esito
040000990920     C                   if        %error
040100990920     C                             or cmzerr = '1'
040200990921     C                             or esito  = '1'
040300000710     C                   eval      wrkesito = '3'
040400990920     C                   endif
040500990920     C*
040600000613     C                   ENDSR
040700990910
040800010601
040900010601
041000010601
041100010601
041200010601      /TITLE Invio dei dati al punto operativo.
041300010601     C     opeini        BEGSR
041400010601     C*
041500010601     C* Inizializzo flag e contatori operativi
041600010601     C                   movel     '0'           flgGiro           1
041700010601     C                   movel     '0'           flgMulti          1
041800010601     C                   movel     '1'           flgStato          1
041900010615     C                   movel     '0'           flgOk             1
042000010601     C                   z-add     *zeros        cntNonEl         10 0
042100010601     C                   z-add     *zeros        depfil            3 0
042200010601     C                   z-add     *zeros        invfil            3 0
042300010601     C*
042400010601     C                   ENDSR
042500010601     C***
042600010601
042700010601
042800010601
042900010330
043000010330
043100000613     C     *inzsr        BEGSR
043200990910     C*
043300990910     C     *entry        plist
043400990920     C                   parm                    tivlrds
043500990921     C                   parm      wrkesito      esito
043600000724     C                   parm                    prmlit
043700000710     C                   parm                    prmfir
043800010330     C*
043900010330     C* CALCOLA LA DATA CORRENTE
044000010330     C                   time                    wn14             14 0
044100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
044200010330     C                   z-add     wn8           g08dat
044300010330     C                   z-add     *zeros        g08inv
044400010330     C                   movel     '0'           g08err
044500010330     C                   call      'XSRDA8'
044600010330     C                   parm                    wlbda8
044700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
044800060221     C*
044900060221     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
045000060221     C                   move(p)   vlrMSG        wNomeFile
045100060221     C*
045200060221     C* Reperisco il CMR da attribuire alla transazione
045300060221     C                   eval      wCMR = %subst(wNomeFile:7:6)
045400000613     C*
045500000613     C                   ENDSR
045600000613     C***
