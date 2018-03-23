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
002100010330     D*------------------
002200010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002300010330     D*------------------
002400010330     D WLBDA8          DS                  INZ
002500010330     D  G08DAT                 1      8  0
002600010330     D  G08INV                 9     16  0
002700010330     D  G08ERR                17     17
002800010330     D  G08TGI                18     22  0
002900000313     D*
003000010330
003100010330
003200010330
003300000913     C                   reset                   rrnum
003400990921     C                   reset                   esito
003500990921     C                   reset                   wrkesito
003600010601     C*
003700010601     C                   exsr      opeini
003800020321     C                   exsr      rwvat
003900010601     C*
004000010601     C                   seton                                        lr
004100010601
004200010601
004300010601
004400010601
004500010601     C*--------------------------------------------------------
004600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
004700010601     C*--------------------------------------------------------
004800010601     C     PREELA        BEGSR
004900010601     C*
005000010601     C* SE OCCORRE SPEDIRE IN FILIALE
005100010601     C                   if        invfil <> *zeros and
005200010601     C                             flgGiro = '0'
005300010601     C*
005400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005500010601     C                   eval      flgGiro = '1'
005600010601     C*
005700010601     C                   endif
005800010601     C*
005900010601     C                   ENDSR
006000010601     C***
006100010601
006200010601
006300010601
006400010601
006500010601     C*--------------------------------------------------------
006600010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
006700010601     C*--------------------------------------------------------
006800010601     C     ENDELA        BEGSR
006900000616     C*
007000010601     C                   ENDSR
007100010601     C***
007200000613
007300010330
007400010601
007500010601
007600010330     C*--------------------------------------------------------
007700060221     C* RWVAT   LEGGE TIVIN00R E SCRIVE EDIVATWF              *
007800010330     C*--------------------------------------------------------
007900020321     C     RWVAT         BEGSR
008000010330     C*
008100010330     C                   if        not %open(tivin00r)
008200010330     C                   open      tivin00r
008300010330     C                   endif
008400060221     C                   if        not %open(edivatwr)
008500060221     C                   open      edivatwr
008600010330     C                   endif
008700010330     C*
008800010604     C                   clear                   §CTROK
008900010604     C                   clear                   §CTRMO
009000010604     C                   clear                   §CTRNO
009100010330     C*
009200010330     C                   DO        *HIVAL
009300010330     C*
009400010330     C                   READ      tivin00r                               70
009500010618     C*
009600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
009700010618     C                   if        vinflg = '1'
009800010618     C                   eval      flgOk = '1'
009900010618     C                   endif
010000010618     C*
010100010330     C                   if        vindta > *blanks
010200010330     C                   add       1             rrnum
010300010330     C*
010400010601     C                   if        *in70 = *off and
010500010330     C                             (vinflg = *blanks
010600010330     C                              or vinflg = '0'
010700010330     C                              or vinflg = '2')
010800010330     C*
010900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011000010711     C                   if        vinflg = *blanks or vinflg = '0'
011100010711     C                   clear                   vinmsg
011200010711     C                   endif
011300010601     C*
011400010330     C                   exsr      inzvar
011500010330     C                   exsr      defcam
011600020321     C                   exsr      impvat
011700010601     C*
011800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
011900020321     C                   eval      depfil = VATLNP
012000010601     C                   exsr      repfil
012100010601     C                   if        depfil = invfil
012200021025     C                   if        vlrpoi = 999
012300021025     C                   MOVE(P)   invfil        VATFGS
012400021025     C                   else
012500021025     C                   MOVE(P)   vlrpoi        VATFGS
012600021025     C                   endif
012700010601     C*
012800010601     C                   exsr      PREELA
012900060221     C*
013000060221     C* VALORIZZO CAMPI RELATIVI AL "CMR"
013100060221     C                   EVAL      VATCMR = wCMR
013200060221     C                   EVAL      VATCNT = 1
013300010601     C*
013400010604     C* Ebbene...
013500010604     C*
013600010604     C  N31              ADD       1             §CTROK            7 0
013700010604     C   32              ADD       1             §CTRMO            7 0
013800010604     C   31              ADD       1             §CTRNO            7 0
013900020205     C*
014000020205     C* Effettuo test particolare solo x DA ROLD x condizoinare la scrittura
014100060221     C  N31              WRITE     EDIVAT00
014200010604     C*
014300010604     C                   if        *in31 = *off and
014400010604     C                             *in32 = *off
014500010604     C                   eval      vinflg = '1'
014600010604     C                   else
014700010604     C                   eval      vinflg = '2'
014800010604     C                   endif
014900010604     C                   endif
015000010601     C*
015100010604     C                   endif
015200010604     C*
015300010330     C                   else
015400010330     C                   eval      vinflg = '1'
015500010330     C                   endif
015600010601     C*
015700010601     C  N70              update    tivin000
015800010330     C*
015900010330     C  N70              ENDdo
016000010601     C
016100010601     C*
016200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
016300010601     C                   if        cntNonEl = *zeros or
016400010601     C                             flgMulti = '0'
016500010330     C* Se non ci sono record con errori ...
016600010601     C                   if        §ctrno = 0 and
016700010604     C                             §ctrmo = 0 and
016800010601     C                             flgStato <> '2'
016900010330     C* ... restituisco esito OK.
017000010330     C                   eval      wrkesito = '0'
017100010330     C                   else
017200010330     C                   if        §ctrok > 0
017300010330     C                   eval      wrkesito = '1'
017400010330     C                   else
017500010615     C                   if        flgOk = '0'
017600010615     C                   eval      wrkesito = '2'
017700010615     C                   else
017800010615     C                   eval      wrkesito = '6'
017900010615     C                   endif
018000010330     C                   endif
018100010330     C                   endif
018200010601     C                   else
018300010601     C                   eval      wrkesito = '9'
018400010601     C                   endif
018500010330     C*
018600010330     C                   if        %open(tivin00r)
018700010330     C                   close     tivin00r
018800010330     C                   endif
018900060221     C                   if        %open(edivatwr)
019000060221     C                   close     edivatwr
019100010330     C                   endif
019200010601     C*
019300010601     C                   if        vlrpoi <> 999
019400010601     C                   eval      invfil = vlrpoi
019500010601     C                   endif
019600010330     C*
019700010330     C                   if        §ctrok > 0
019800010601     C                             and invfil > *zeros
019900010330     C                   exsr      invio
020000010330     C                   endif
020100010601     C*
020200010618     C                   if        flgGiro = '1'
020300010601     C                   exsr      endela
020400010618     C                   endif
020500010330     C*
020600010330     C                   ENDSR
020700010330     C***
020800010330
020900010601
021000010601
021100010601
021200010330     C*----------------------------------------------------*
021300010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021400010330     C*----------------------------------------------------*
021500040218     C     INZVAR        BEGSR
021600010330     C*
021700010330     C*
021800010330     C                   ENDSR
021900010330     C*----------------------------------------------------*
022000010330     C*  IMPOSTAZIONE CAMPI COSTANTI
022100010330     C*----------------------------------------------------*
022200010330     C     DEFCAM        BEGSR
022300010330     C*
022400020204     C* Inizializzo il buffer del record da scrivere
022500060221     C                   CLEAR                   EDIVAT00
022600020204     C* Imposto i valori di default...
022700040218     C                   EVAL      VATCCM = 1330192
022800040218     C                   EVAL      VATLNP = 133
022900020321     C                   EVAL      VATTRC = 'E'
023000020204     C* ... e poi verifico se sono stati passati come parametri
023100020204     C                   IF        vlrppt > *blanks
023200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023300020204     C                   EXSR      CHKNUM
023400020204     C                   IF        PiInt=*on
023500020321     C                   Z-ADD     PiVal         VATCCM
023600020204     C                   ENDIF
023700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023800020204     C                   EXSR      CHKNUM
023900020204     C                   IF        PiInt=*on
024000020321     C                   Z-ADD     PiVal         VATLNP
024100020204     C                   ENDIF
024200020321     C                   ENDIF
024300020204     C*
024400010330     C                   ENDSR
024500010607     C*----------------------------------------------------*
024600060221     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
024700010607     C*----------------------------------------------------*
024800020321     C     IMPVAT        BEGSR
024900010607     C*
025000010607     C                   SETOFF                                       3132
025100040218     C*
025200040218     C                   MOVEL     datcor        VATAAS
025300010607     C*
025400010607     C* Reperimento campi ALFA
025500040218     C                   EVAL      VATNOT=%trim(%subst(vindta:42:18))
025600020304     C*
025700040218     C* VATNSP
025800040218     C                   EVAL      PiStr=%trim(%subst(vindta:121:5))
025900020304     C                   EXSR      CHKNUM
026000020304     C                   IF        PiInt=*on
026100020321     C                   Z-ADD     PiVal         VATNSP
026200020304     C                   ELSE
026300020304     C                   SETON                                        32
026400020321     C                   Z-ADD     *zeros        VATNSP
026500020304     C                   EVAL      vinmsg = %trimr(vinmsg)
026600020321     C                             + ' ' + 'VATNSP'
026700020304     C                   ENDIF
026800020204     C*
026900010607     C                   ENDSR
027000010607     C*----------------------------------------------------*
027100010601
027200010330
027300010330     C*----------------------------------------------------*
027400010330     C*  CONTROLLO NUMERICITA' CAMPI
027500010330     C*----------------------------------------------------*
027600010330     C     CHKNUM        BEGSR
027700010330     C*
027800010606     C                   IF        PiDecChr = *blanks
027900010606     C                   EVAL      PiDecChr = ','
028000010606     C                   ENDIF
028100010606     C*
028200010606     C                   CALL(e)   'ISNUMERIC'
028300010330     C                   PARM                    PiStr            30
028400010606     C                   PARM                    PiDecChr          1
028500010330     C                   PARM      *ZEROS        PiVal            30 9
028600010330     C                   PARM      '0'           PiInt             1
028700010330     C                   PARM      '0'           PiNum             1
028800010330     C                   IF        %error
028900010606     C                   EVAL      PiNum=*off
029000010330     C                   ENDIF
029100010330     C*
029200010330     C                   ENDSR
029300010330     C***
029400020403
029500020403
029600010601
029700010601
029800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
029900010601     C     repfil        BEGSR
030000010601     C*
030100010601     C                   if        invfil = *zeros and
030200010601     C                             depfil > *zeros and
030300010629     C                             (vinflg = *blanks or
030400010629     C                              vinflg = *zeros)
030500010601     C
030600010601     C                   eval      invfil = depfil
030700010601     C                   endif
030800010601     C*
030900010601     C                   if        depfil <> invfil and
031000010601     C                             invfil > *zeros
031100010601     C                   eval      flgMulti = '1'
031200010601     C                   if        vinflg = *blanks
031300010601     C                   add       1             cntNonEl
031400010601     C                   endif
031500010601     C                   endif
031600010601     C*
031700010601     C                   if        vinflg = '2'
031800010601     C                   eval      flgStato = '2'
031900010601     C                   endif
032000010601     C*
032100010601     C                   ENDSR
032200010601     C***
032300010601
032400010601
032500010601
032600010330
032700010330
032800010330
032900990920      /TITLE Invio dei dati al punto operativo.
033000000613     C     invio         BEGSR
033100990920     C*
033200990920     C                   reset                   dscmz
033300010601     C                   move      invfil        cmzdst
033400990920     C                   eval      cmzfld = vlrfou
033500990920     C                   eval      cmzmbd = vlrhdl
033600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
033700000710     C                   if        prmfir = *blanks
033800060221     C                   eval      cmzfla = 'EDIVAT0F'
033900060221     C                   eval      cmzmba = 'EDIVAT0F'
034000000710     C                   else
034100000710     C                   eval      cmzfla = prmfir
034200000710     C                   eval      cmzmba = prmfir
034300000710     C                   endif
034400990920     C                   eval      cmznrr = *zeros
034500990920     C                   move      §ctrok        cmznrr
034600021018     C                   eval      cmzlba = vlrfl1
034700990920     C                   call(e)   'TIS711C'
034800990920     C                   parm                    dscmz
034900990921     C                   parm      *blanks       esito
035000990920     C                   if        %error
035100990920     C                             or cmzerr = '1'
035200990921     C                             or esito  = '1'
035300000710     C                   eval      wrkesito = '3'
035400990920     C                   endif
035500990920     C*
035600000613     C                   ENDSR
035700990910
035800010601
035900010601
036000010601
036100010601
036200010601      /TITLE Invio dei dati al punto operativo.
036300010601     C     opeini        BEGSR
036400010601     C*
036500010601     C* Inizializzo flag e contatori operativi
036600010601     C                   movel     '0'           flgGiro           1
036700010601     C                   movel     '0'           flgMulti          1
036800010601     C                   movel     '1'           flgStato          1
036900010615     C                   movel     '0'           flgOk             1
037000010601     C                   z-add     *zeros        cntNonEl         10 0
037100010601     C                   z-add     *zeros        depfil            3 0
037200010601     C                   z-add     *zeros        invfil            3 0
037300010601     C*
037400010601     C                   ENDSR
037500010601     C***
037600010601
037700010601
037800010601
037900010330
038000010330
038100000613     C     *inzsr        BEGSR
038200990910     C*
038300990910     C     *entry        plist
038400990920     C                   parm                    tivlrds
038500990921     C                   parm      wrkesito      esito
038600000724     C                   parm                    prmlit
038700000710     C                   parm                    prmfir
038800010330     C*
038900010330     C* CALCOLA LA DATA CORRENTE
039000010330     C                   time                    wn14             14 0
039100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
039200010330     C                   z-add     wn8           g08dat
039300010330     C                   z-add     *zeros        g08inv
039400010330     C                   movel     '0'           g08err
039500010330     C                   call      'XSRDA8'
039600010330     C                   parm                    wlbda8
039700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
039800060221     C*
039900060221     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
040000060221     C                   move(p)   vlrMSG        wNomeFile
040100060221     C*
040200060221     C* Reperisco il CMR da attribuire alla transazione
040300060221     C                   eval      wCMR = %subst(wNomeFile:7:6)
040400000613     C*
040500000613     C                   ENDSR
040600000613     C***
