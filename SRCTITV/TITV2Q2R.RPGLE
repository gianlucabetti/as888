000100020916      /TITLE Upload via Internet: traduzione in FIVATWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
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
013600130604     C***
013700130604     C*
013800130604     C* Personalizzazione => Forzatura per tipi record 'P'
013900130604     C                   if        VATTRC = 'P'
014000130604     C                   eval      VATNOT = %subst(VATNOT:1:23)+'*'+
014100130604     C                                      %subst(VATNOT:28)
014200130604     C                   move      '10'          VATNOT
014300130604     C                   endif
014400130604     C***
014500010601     C*
014600010604     C* Ebbene...
014700010604     C*
014800010604     C  N31              ADD       1             §CTROK            7 0
014900010604     C   32              ADD       1             §CTRMO            7 0
015000010604     C   31              ADD       1             §CTRNO            7 0
015100020916     C  N31              WRITE     FIVAT000
015200010604     C*
015300010604     C                   if        *in31 = *off and
015400010604     C                             *in32 = *off
015500010604     C                   eval      vinflg = '1'
015600010604     C                   else
015700010604     C                   eval      vinflg = '2'
015800010604     C                   endif
015900010604     C                   endif
016000010601     C*
016100010604     C                   endif
016200010604     C*
016300010330     C                   else
016400010330     C                   eval      vinflg = '1'
016500010330     C                   endif
016600010601     C*
016700010601     C  N70              update    tivin000
016800010330     C*
016900010330     C  N70              ENDdo
017000010601     C
017100010601     C*
017200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
017300010601     C                   if        cntNonEl = *zeros or
017400010601     C                             flgMulti = '0'
017500010330     C* Se non ci sono record con errori ...
017600010601     C                   if        §ctrno = 0 and
017700010604     C                             §ctrmo = 0 and
017800010601     C                             flgStato <> '2'
017900010330     C* ... restituisco esito OK.
018000010330     C                   eval      wrkesito = '0'
018100010330     C                   else
018200010330     C                   if        §ctrok > 0
018300010330     C                   eval      wrkesito = '1'
018400010330     C                   else
018500010615     C                   if        flgOk = '0'
018600010615     C                   eval      wrkesito = '2'
018700010615     C                   else
018800010615     C                   eval      wrkesito = '6'
018900010615     C                   endif
019000010330     C                   endif
019100010330     C                   endif
019200010601     C                   else
019300010601     C                   eval      wrkesito = '9'
019400010601     C                   endif
019500010330     C*
019600010330     C                   if        %open(tivin00r)
019700010330     C                   close     tivin00r
019800010330     C                   endif
019900020916     C                   if        %open(fivatwwr)
020000020916     C                   close     fivatwwr
020100010330     C                   endif
020200010601     C*
020300010601     C                   if        vlrpoi <> 999
020400010601     C                   eval      invfil = vlrpoi
020500010601     C                   endif
020600010330     C*
020700010330     C                   if        §ctrok > 0
020800010601     C                             and invfil > *zeros
020900010330     C                   exsr      invio
021000010330     C                   endif
021100010601     C*
021200010612     C                   if        flgGiro = '1'
021300010601     C                   exsr      endela
021400010612     C                   endif
021500010330     C*
021600010330     C                   ENDSR
021700010330     C***
021800010330
021900010601
022000010601
022100010601
022200010601
022300010330     C*----------------------------------------------------*
022400010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022500010330     C*----------------------------------------------------*
022600010330     C     INZVAR        BEGSR
022700010330     C*
022800010330     C*
022900010330     C                   ENDSR
023000010330     C*----------------------------------------------------*
023100010330     C*  IMPOSTAZIONE CAMPI COSTANTI
023200010330     C*----------------------------------------------------*
023300010330     C     DEFCAM        BEGSR
023400010330     C*
023500020916     C                   CLEAR                   FIVAT000
023600010330     C*
023700010330     C                   ENDSR
023800010705     C*----------------------------------------------------*
023900010705
024000010705
024100010705
024200010705
024300010705     C*----------------------------------------------------*
024400020916     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
024500010705     C*----------------------------------------------------*
024600010705     C     IMPVAT        BEGSR
024700010705     C*
024800010705     C                   SETOFF                                       3132
024900010705     C*
025000010705     C* Reperimento campi ALFA
025100010705     C                   EVAL      VATATB=a_vatatb
025200010705     C                   EVAL      VATTRC=a_vattrc
025300020725     C                   EVAL      VATNOT=%trim(a_vatnot)
025400010705     C*
025500010705     C                   EVAL      PiStr=%trim(a_vatccm)
025600010705     C                   EXSR      CHKNUM
025700010705     C                   IF        PiInt=*on
025800010705     C                   Z-ADD     PiVal         VATCCM
025900010705     C                   ELSE
026000010705     C                   SETON                                        31
026100010705     C                   EVAL      vinmsg = %trimr(vinmsg)
026200010705     C                             + ' ' + 'VATCCM'
026300010705     C                   ENDIF
026400010705     C*
026500010705     C                   EVAL      PiStr=%trim(a_vatlnp)
026600010705     C                   EXSR      CHKNUM
026700010705     C                   IF        PiInt=*on
026800010705     C                   Z-ADD     PiVal         VATLNP
026900010705     C                   ELSE
027000010705     C                   SETON                                        31
027100010705     C                   EVAL      vinmsg = %trimr(vinmsg)
027200021010     C                             + ' ' + 'VATLNP'
027300010705     C                   ENDIF
027400010705     C*
027500010705     C                   EVAL      PiStr=%trim(a_vataas)
027600010705     C                   EXSR      CHKNUM
027700010705     C                   IF        PiInt=*on
027800010705     C                   Z-ADD     PiVal         VATAAS
027900010705     C                   ELSE
028000010705     C                   SETON                                        31
028100010705     C                   EVAL      vinmsg = %trimr(vinmsg)
028200010705     C                             + ' ' + 'VATAAS'
028300010705     C                   ENDIF
028400010705     C*
028500010705     C                   EVAL      PiStr=%trim(a_vatnrs)
028600010705     C                   IF        PiStr = *blanks
028700010705     C                   EVAL      PiStr = *zeros
028800100309     C                   ELSE
028900010705     C                   EXSR      CHKNUM
029000010705     C                   IF        PiInt=*on
029100010705     C                   Z-ADD     PiVal         VATNRS
029200010705     C                   ELSE
029300010705     C                   SETON                                        31
029400010705     C                   EVAL      vinmsg = %trimr(vinmsg)
029500010705     C                             + ' ' + 'VATNRS'
029600010705     C                   ENDIF
029700100309     C                   ENDIF
029800010705     C*
029900010705     C                   EVAL      PiStr=%trim(a_vatnsp)
030000010705     C                   EXSR      CHKNUM
030100010705     C                   IF        PiInt=*on
030200010705     C                   Z-ADD     PiVal         VATNSP
030300010705     C                   ELSE
030400010705     C                   SETON                                        31
030500010705     C                   EVAL      vinmsg = %trimr(vinmsg)
030600010705     C                             + ' ' + 'VATNSP'
030700010705     C                   ENDIF
030800010705     C*
030900010705     C                   ENDSR
031000010705     C*----------------------------------------------------*
031100010705
031200010705
031300010705
031400010705
031500010705
031600010705     C*----------------------------------------------------*
031700010705     C*  CONTROLLO NUMERICITA' CAMPI
031800010705     C*----------------------------------------------------*
031900010705     C     CHKNUM        BEGSR
032000010705     C*
032100010705     C                   call(e)   'ISNUMERIC'
032200010705     C                   PARM                    PiStr            30
032300010705     C                   PARM      ','           PiDecChr          1
032400010705     C                   PARM      *ZEROS        PiVal            30 9
032500010705     C                   PARM      '0'           PiInt             1
032600010705     C                   PARM      '0'           PiNum             1
032700010705     C                   IF        %error
032800010705     C                   EVAL      PiInt=*off
032900010705     C                   ENDIF
033000010705     C*
033100010705     C                   ENDSR
033200010705     C*----------------------------------------------------*
033300010705
033400010705
033500010705
033600010601
033700010601
033800010601
033900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
034000010601     C     repfil        BEGSR
034100010601     C*
034200010601     C                   if        invfil = *zeros and
034300010601     C                             depfil > *zeros and
034400010613     C                             (vinflg = *blanks or
034500010613     C                              vinflg = *zeros)
034600010601     C
034700010601     C                   eval      invfil = depfil
034800010601     C                   endif
034900010601     C*
035000010601     C                   if        depfil <> invfil and
035100010601     C                             invfil > *zeros
035200010601     C                   eval      flgMulti = '1'
035300010601     C                   if        vinflg = *blanks
035400010601     C                   add       1             cntNonEl
035500010601     C                   endif
035600010601     C                   endif
035700010601     C*
035800010601     C                   if        vinflg = '2'
035900010601     C                   eval      flgStato = '2'
036000010601     C                   endif
036100010601     C*
036200010601     C                   ENDSR
036300010601     C***
036400010601
036500010601
036600010601
036700010330
036800010330
036900010330
037000990920      /TITLE Invio dei dati al punto operativo.
037100000613     C     invio         BEGSR
037200990920     C*
037300990920     C                   reset                   dscmz
037400010601     C                   move      invfil        cmzdst
037500990920     C                   eval      cmzfld = vlrfou
037600990920     C                   eval      cmzmbd = vlrhdl
037700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
037800000710     C                   if        prmfir = *blanks
037900020916     C                   eval      cmzfla = 'FIVAT00F'
038000020916     C                   eval      cmzmba = 'FIVAT00F'
038100000710     C                   else
038200000710     C                   eval      cmzfla = prmfir
038300000710     C                   eval      cmzmba = prmfir
038400000710     C                   endif
038500990920     C                   eval      cmznrr = *zeros
038600990920     C                   move      §ctrok        cmznrr
038700021003     C                   eval      cmzlba = vlrfl1
038800990920     C                   call(e)   'TIS711C'
038900990920     C                   parm                    dscmz
039000990921     C                   parm      *blanks       esito
039100990920     C                   if        %error
039200990920     C                             or cmzerr = '1'
039300990921     C                             or esito  = '1'
039400000710     C                   eval      wrkesito = '3'
039500990920     C                   endif
039600990920     C*
039700000613     C                   ENDSR
039800990910
039900010601
040000010601
040100010601
040200010601
040300010601      /TITLE Invio dei dati al punto operativo.
040400010601     C     opeini        BEGSR
040500010601     C*
040600010601     C* Inizializzo flag e contatori operativi
040700010601     C                   movel     '0'           flgGiro           1
040800010601     C                   movel     '0'           flgMulti          1
040900010601     C                   movel     '1'           flgStato          1
041000010615     C                   movel     '0'           flgOk             1
041100010601     C                   z-add     *zeros        cntNonEl         10 0
041200010601     C                   z-add     *zeros        depfil            3 0
041300010601     C                   z-add     *zeros        invfil            3 0
041400030407     C*
041500030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
041600030407     C                   if        %subst(vlrppt:1:1) = 'S'
041700030407     C                   seton                                        50
041800030407     C                   else
041900030407     C                   setoff                                       50
042000030407     C                   endif
042100010601     C*
042200010601     C                   ENDSR
042300010601     C***
042400010601
042500010601
042600010601
042700010330
042800010330
042900000613     C     *inzsr        BEGSR
043000990910     C*
043100990910     C     *entry        plist
043200990920     C                   parm                    tivlrds
043300990921     C                   parm      wrkesito      esito
043400000724     C                   parm                    prmlit
043500000710     C                   parm                    prmfir
043600010330     C*
043700010330     C* CALCOLA LA DATA CORRENTE
043800010330     C                   time                    wn14             14 0
043900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
044000010330     C                   z-add     wn8           g08dat
044100010330     C                   z-add     *zeros        g08inv
044200010330     C                   movel     '0'           g08err
044300010330     C                   call      'XSRDA8'
044400010330     C                   parm                    wlbda8
044500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
044600000613     C*
044700000613     C                   ENDSR
044800000613     C***
