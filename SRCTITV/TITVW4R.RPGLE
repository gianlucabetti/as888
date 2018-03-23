000100020916      /TITLE Upload via Internet: traduzione in FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500020916     FFIVATwwr  O    E             DISK    usropn
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
001900010330     D*------------------
002000010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002100010330     D*------------------
002200010330     D WLBDA8          DS                  INZ
002300010330     D  G08DAT                 1      8  0
002400010330     D  G08INV                 9     16  0
002500010330     D  G08ERR                17     17
002600010330     D  G08TGI                18     22  0
002700010612     D*
002800010612     D fnvataar        ds                  inz
002900010705     D  a_vatatb               1      1
003000010705     D  a_vatccm               2      8
003100010705     D  a_vatlnp               9     11
003200010705     D  a_vataas              12     15
003300010705     D  a_vatnrs              16     17
003400010705     D  a_vatnsp              18     24
003500010705     D  a_vattrc              25     25
003600010705     D  a_vatnot              26     60
003700010330
003800010330
003900010330
004000000913     C                   reset                   rrnum
004100990921     C                   reset                   esito
004200990921     C                   reset                   wrkesito
004300010601     C*
004400010601     C                   exsr      opeini
004500010612     C                   exsr      rwvat
004600010601     C*
004700010601     C                   seton                                        lr
004800010601
004900010601
005000010601
005100010601
005200010601     C*--------------------------------------------------------
005300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005400010601     C*--------------------------------------------------------
005500010601     C     PREELA        BEGSR
005600010601     C*
005700010601     C* SE OCCORRE SPEDIRE IN FILIALE
005800010601     C                   if        invfil <> *zeros and
005900010601     C                             flgGiro = '0'
006000010601     C*
006100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006200010601     C                   eval      flgGiro = '1'
006300010601     C*
006400010601     C                   endif
006500010601     C*
006600010601     C                   ENDSR
006700010601     C***
006800010601
006900010601
007000010601
007100010601
007200010601
007300010601     C*--------------------------------------------------------
007400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007500010601     C*--------------------------------------------------------
007600010601     C     ENDELA        BEGSR
007700000616     C*
007800010601     C                   ENDSR
007900010601     C***
008000000613
008100010330
008200010601
008300010601
008400010601
008500010330     C*--------------------------------------------------------
008600020916     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
008700010330     C*--------------------------------------------------------
008800010612     C     RWVAT         BEGSR
008900010330     C*
009000010330     C                   if        not %open(tivin00r)
009100010330     C                   open      tivin00r
009200010330     C                   endif
009300020916     C                   if        not %open(fivatwwr)
009400020916     C                   open      fivatwwr
009500010330     C                   endif
009600010330     C*
009700010604     C                   clear                   §CTROK
009800010604     C                   clear                   §CTRMO
009900010604     C                   clear                   §CTRNO
010000010330     C*
010100010330     C                   DO        *HIVAL
010200010330     C*
010300010330     C                   READ      tivin00r                               70
010400010618     C*
010500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
010600010618     C                   if        vinflg = '1'
010700010618     C                   eval      flgOk = '1'
010800010618     C                   endif
010900010618     C*
011000010330     C                   if        vindta > *blanks
011100010330     C                   add       1             rrnum
011200010330     C*
011300010601     C                   if        *in70 = *off and
011400010330     C                             (vinflg = *blanks
011500010330     C                              or vinflg = '0'
011600010330     C                              or vinflg = '2')
011700010330     C*
011800010330     C                   clear                   vinmsg
011900010613     C                   eval      fnvataar = vindta
012000010601     C*
012100010330     C                   exsr      inzvar
012200010330     C                   exsr      defcam
012300010612     C                   exsr      impvat
012400010601     C*
012500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012600010612     C                   eval      depfil = VATLNP
012700010601     C                   exsr      repfil
012800010601     C                   if        depfil = invfil
012900021014     C                   if        vlrpoi = 999
013000021010     C                   MOVE(P)   invfil        VATFGS
013100021014     C                   else
013200021014     C                   MOVE(P)   vlrpoi        VATFGS
013300021014     C                   endif
013400010601     C*
013500010601     C                   exsr      PREELA
013600010601     C*
013700010604     C* Ebbene...
013800010604     C*
013900010604     C  N31              ADD       1             §CTROK            7 0
014000010604     C   32              ADD       1             §CTRMO            7 0
014100010604     C   31              ADD       1             §CTRNO            7 0
014200020916     C  N31              WRITE     FIVAT000
014300010604     C*
014400010604     C                   if        *in31 = *off and
014500010604     C                             *in32 = *off
014600010604     C                   eval      vinflg = '1'
014700010604     C                   else
014800010604     C                   eval      vinflg = '2'
014900010604     C                   endif
015000010604     C                   endif
015100010601     C*
015200010604     C                   endif
015300010604     C*
015400010330     C                   else
015500010330     C                   eval      vinflg = '1'
015600010330     C                   endif
015700010601     C*
015800010601     C  N70              update    tivin000
015900010330     C*
016000010330     C  N70              ENDdo
016100010601     C
016200010601     C*
016300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
016400010601     C                   if        cntNonEl = *zeros or
016500010601     C                             flgMulti = '0'
016600010330     C* Se non ci sono record con errori ...
016700010601     C                   if        §ctrno = 0 and
016800010604     C                             §ctrmo = 0 and
016900010601     C                             flgStato <> '2'
017000010330     C* ... restituisco esito OK.
017100010330     C                   eval      wrkesito = '0'
017200010330     C                   else
017300010330     C                   if        §ctrok > 0
017400010330     C                   eval      wrkesito = '1'
017500010330     C                   else
017600010615     C                   if        flgOk = '0'
017700010615     C                   eval      wrkesito = '2'
017800010615     C                   else
017900010615     C                   eval      wrkesito = '6'
018000010615     C                   endif
018100010330     C                   endif
018200010330     C                   endif
018300010601     C                   else
018400010601     C                   eval      wrkesito = '9'
018500010601     C                   endif
018600010330     C*
018700010330     C                   if        %open(tivin00r)
018800010330     C                   close     tivin00r
018900010330     C                   endif
019000020916     C                   if        %open(fivatwwr)
019100020916     C                   close     fivatwwr
019200010330     C                   endif
019300010601     C*
019400010601     C                   if        vlrpoi <> 999
019500010601     C                   eval      invfil = vlrpoi
019600010601     C                   endif
019700010330     C*
019800010330     C                   if        §ctrok > 0
019900010601     C                             and invfil > *zeros
020000010330     C                   exsr      invio
020100010330     C                   endif
020200010601     C*
020300010612     C                   if        flgGiro = '1'
020400010601     C                   exsr      endela
020500010612     C                   endif
020600010330     C*
020700010330     C                   ENDSR
020800010330     C***
020900010330
021000010601
021100010601
021200010601
021300010601
021400010330     C*----------------------------------------------------*
021500010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021600010330     C*----------------------------------------------------*
021700010330     C     INZVAR        BEGSR
021800010330     C*
021900010330     C*
022000010330     C                   ENDSR
022100010330     C*----------------------------------------------------*
022200010330     C*  IMPOSTAZIONE CAMPI COSTANTI
022300010330     C*----------------------------------------------------*
022400010330     C     DEFCAM        BEGSR
022500010330     C*
022600020916     C                   CLEAR                   FIVAT000
022700070125     C* Imposto i valori di default...
022800070125     C                   EVAL      VATCCM = 1160951
022900070125     C                   EVAL      VATLNP = 116
023000070125     C* ... e poi verifico se sono stati passati come parametri
023100070125     C                   IF        vlrppt > *blanks
023200070125     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
023300070125     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023400070125     C                   EXSR      CHKNUM
023500070125     C                   IF        PiInt=*on
023600070125     C                   Z-ADD     PiVal         VATCCM
023700070125     C                   ENDIF
023800070125     C                   ENDIF
023900070125     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
024000070125     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024100070125     C                   EXSR      CHKNUM
024200070125     C                   IF        PiInt=*on
024300070125     C                   Z-ADD     PiVal         VATLNP
024400070125     C                   ENDIF
024500070125     C                   ENDIF
024600070125     C                   ENDIF
024700010330     C*
024800010330     C                   ENDSR
024900010705     C*----------------------------------------------------*
025000010705
025100010705
025200010705
025300010705
025400010705     C*----------------------------------------------------*
025500020916     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
025600010705     C*----------------------------------------------------*
025700010705     C     IMPVAT        BEGSR
025800010705     C*
025900010705     C                   SETOFF                                       3132
026000010705     C*
026100010705     C* Reperimento campi ALFA
026200010705     C                   EVAL      VATATB=a_vatatb
026300010705     C                   EVAL      VATTRC=a_vattrc
026400020725     C                   EVAL      VATNOT=%trim(a_vatnot)
026500010705     C*
026600070125     C*                  EVAL      PiStr=%trim(a_vatccm)
026700070125     C*                  EXSR      CHKNUM
026800070125     C*                  IF        PiInt=*on
026900070125     C*                  Z-ADD     PiVal         VATCCM
027000070125     C*                  ELSE
027100070125     C*                  SETON                                        31
027200070125     C*                  EVAL      vinmsg = %trimr(vinmsg)
027300070125     C*                            + ' ' + 'VATCCM'
027400070125     C*                  ENDIF
027500070125     C*
027600070125     C*                  EVAL      PiStr=%trim(a_vatlnp)
027700070125     C*                  EXSR      CHKNUM
027800070125     C*                  IF        PiInt=*on
027900070125     C*                  Z-ADD     PiVal         VATLNP
028000070125     C*                  ELSE
028100070125     C*                  SETON                                        31
028200070125     C*                  EVAL      vinmsg = %trimr(vinmsg)
028300070125     C*                            + ' ' + 'VATLNP'
028400070125     C*                  ENDIF
028500010705     C*
028600010705     C                   EVAL      PiStr=%trim(a_vataas)
028700010705     C                   EXSR      CHKNUM
028800010705     C                   IF        PiInt=*on
028900010705     C                   Z-ADD     PiVal         VATAAS
029000010705     C                   ELSE
029100010705     C                   SETON                                        31
029200010705     C                   EVAL      vinmsg = %trimr(vinmsg)
029300010705     C                             + ' ' + 'VATAAS'
029400010705     C                   ENDIF
029500010705     C*
029600010705     C                   EVAL      PiStr=%trim(a_vatnrs)
029700010705     C                   IF        PiStr = *blanks
029800010705     C                   EVAL      PiStr = *zeros
029900010705     C                   ENDIF
030000010705     C                   EXSR      CHKNUM
030100010705     C                   IF        PiInt=*on
030200010705     C                   Z-ADD     PiVal         VATNRS
030300010705     C                   ELSE
030400010705     C                   SETON                                        31
030500010705     C                   EVAL      vinmsg = %trimr(vinmsg)
030600010705     C                             + ' ' + 'VATNRS'
030700010705     C                   ENDIF
030800010705     C*
030900010705     C                   EVAL      PiStr=%trim(a_vatnsp)
031000010705     C                   EXSR      CHKNUM
031100010705     C                   IF        PiInt=*on
031200010705     C                   Z-ADD     PiVal         VATNSP
031300010705     C                   ELSE
031400010705     C                   SETON                                        31
031500010705     C                   EVAL      vinmsg = %trimr(vinmsg)
031600010705     C                             + ' ' + 'VATNSP'
031700010705     C                   ENDIF
031800010705     C*
031900010705     C                   ENDSR
032000010705     C*----------------------------------------------------*
032100010705
032200010705
032300010705
032400010705
032500010705
032600010705     C*----------------------------------------------------*
032700010705     C*  CONTROLLO NUMERICITA' CAMPI
032800010705     C*----------------------------------------------------*
032900010705     C     CHKNUM        BEGSR
033000010705     C*
033100010705     C                   call(e)   'ISNUMERIC'
033200010705     C                   PARM                    PiStr            30
033300010705     C                   PARM      ','           PiDecChr          1
033400010705     C                   PARM      *ZEROS        PiVal            30 9
033500010705     C                   PARM      '0'           PiInt             1
033600010705     C                   PARM      '0'           PiNum             1
033700010705     C                   IF        %error
033800010705     C                   EVAL      PiInt=*off
033900010705     C                   ENDIF
034000010705     C*
034100010705     C                   ENDSR
034200010705     C*----------------------------------------------------*
034300010705
034400010705
034500010705
034600010601
034700010601
034800010601
034900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
035000010601     C     repfil        BEGSR
035100010601     C*
035200010601     C                   if        invfil = *zeros and
035300010601     C                             depfil > *zeros and
035400010613     C                             (vinflg = *blanks or
035500010613     C                              vinflg = *zeros)
035600010601     C
035700010601     C                   eval      invfil = depfil
035800010601     C                   endif
035900010601     C*
036000010601     C                   if        depfil <> invfil and
036100010601     C                             invfil > *zeros
036200010601     C                   eval      flgMulti = '1'
036300010601     C                   if        vinflg = *blanks
036400010601     C                   add       1             cntNonEl
036500010601     C                   endif
036600010601     C                   endif
036700010601     C*
036800010601     C                   if        vinflg = '2'
036900010601     C                   eval      flgStato = '2'
037000010601     C                   endif
037100010601     C*
037200010601     C                   ENDSR
037300010601     C***
037400010601
037500010601
037600010601
037700010330
037800010330
037900010330
038000990920      /TITLE Invio dei dati al punto operativo.
038100000613     C     invio         BEGSR
038200990920     C*
038300990920     C                   reset                   dscmz
038400010601     C                   move      invfil        cmzdst
038500990920     C                   eval      cmzfld = vlrfou
038600990920     C                   eval      cmzmbd = vlrhdl
038700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
038800000710     C                   if        prmfir = *blanks
038900020916     C                   eval      cmzfla = 'FIVAT00F'
039000020916     C                   eval      cmzmba = 'FIVAT00F'
039100000710     C                   else
039200000710     C                   eval      cmzfla = prmfir
039300000710     C                   eval      cmzmba = prmfir
039400000710     C                   endif
039500990920     C                   eval      cmznrr = *zeros
039600990920     C                   move      §ctrok        cmznrr
039700021003     C                   eval      cmzlba = vlrfl1
039800990920     C                   call(e)   'TIS711C'
039900990920     C                   parm                    dscmz
040000990921     C                   parm      *blanks       esito
040100990920     C                   if        %error
040200990920     C                             or cmzerr = '1'
040300990921     C                             or esito  = '1'
040400000710     C                   eval      wrkesito = '3'
040500990920     C                   endif
040600990920     C*
040700000613     C                   ENDSR
040800990910
040900010601
041000010601
041100010601
041200010601
041300010601      /TITLE Invio dei dati al punto operativo.
041400010601     C     opeini        BEGSR
041500010601     C*
041600010601     C* Inizializzo flag e contatori operativi
041700010601     C                   movel     '0'           flgGiro           1
041800010601     C                   movel     '0'           flgMulti          1
041900010601     C                   movel     '1'           flgStato          1
042000010615     C                   movel     '0'           flgOk             1
042100010601     C                   z-add     *zeros        cntNonEl         10 0
042200010601     C                   z-add     *zeros        depfil            3 0
042300010601     C                   z-add     *zeros        invfil            3 0
042400030407     C*
042500030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
042600030407     C                   if        %subst(vlrppt:1:1) = 'S'
042700030407     C                   seton                                        50
042800030407     C                   else
042900030407     C                   setoff                                       50
043000030407     C                   endif
043100010601     C*
043200010601     C                   ENDSR
043300010601     C***
043400010601
043500010601
043600010601
043700010330
043800010330
043900000613     C     *inzsr        BEGSR
044000990910     C*
044100990910     C     *entry        plist
044200990920     C                   parm                    tivlrds
044300990921     C                   parm      wrkesito      esito
044400000724     C                   parm                    prmlit
044500000710     C                   parm                    prmfir
044600010330     C*
044700010330     C* CALCOLA LA DATA CORRENTE
044800010330     C                   time                    wn14             14 0
044900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
045000010330     C                   z-add     wn8           g08dat
045100010330     C                   z-add     *zeros        g08inv
045200010330     C                   movel     '0'           g08err
045300010330     C                   call      'XSRDA8'
045400010330     C                   parm                    wlbda8
045500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
045600000613     C*
045700000613     C                   ENDSR
045800000613     C***
