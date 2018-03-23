000100080410      /TITLE Upload via Internet: traduzione in FIVATWWR.
000200081118     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500040301     FFIVATwwr  O    E             DISK    usropn
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
002700000313     D*
002800081118
002900081118     D*------------------
003000081118     D* LINKING A DEFINIZIONI ESTERNE
003100081118     D*------------------
003200081118     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
003300081218     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
003400081118
003500010330
003600010330
003700010330
003800000913     C                   reset                   rrnum
003900990921     C                   reset                   esito
004000990921     C                   reset                   wrkesito
004100010601     C*
004200010601     C                   exsr      opeini
004300040301     C                   exsr      rwvat
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
006900010601     C*--------------------------------------------------------
007000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007100010601     C*--------------------------------------------------------
007200010601     C     ENDELA        BEGSR
007300990915     C*
007400000616     C*
007500010601     C                   ENDSR
007600010601     C***
007700000613
007800010601
007900010601
008000010330     C*--------------------------------------------------------
008100040301     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
008200010330     C*--------------------------------------------------------
008300040301     C     RWVAT         BEGSR
008400010330     C*
008500010330     C                   if        not %open(tivin00r)
008600010330     C                   open      tivin00r
008700010330     C                   endif
008800040301     C                   if        not %open(fivatwwr)
008900040301     C                   open      fivatwwr
009000010330     C                   endif
009100010330     C*
009200010604     C                   clear                   低TROK
009300010604     C                   clear                   低TRMO
009400010604     C                   clear                   低TRNO
009500010330     C*
009600010330     C                   DO        *HIVAL
009700010330     C*
009800010330     C                   READ      tivin00r                               70
009900010618     C*
010000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
010100010618     C                   if        vinflg = '1'
010200010618     C                   eval      flgOk = '1'
010300010618     C                   endif
010400010618     C*
010500010330     C                   if        vindta > *blanks
010600010330     C                   add       1             rrnum
010700010330     C*
010800010601     C                   if        *in70 = *off and
010900010330     C                             (vinflg = *blanks
011000010330     C                              or vinflg = '0'
011100010330     C                              or vinflg = '2')
011200010330     C*
011300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011400010711     C                   if        vinflg = *blanks or vinflg = '0'
011500010711     C                   clear                   vinmsg
011600010711     C                   endif
011700010601     C*
011800040301     C* Inizializzo campi d wrk e imposto i valori d default
011900010330     C                   exsr      inzvar
012000010330     C                   exsr      defcam
012100010601     C*
012200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012300040301     C                   eval      depfil = VATLNP
012400010601     C                   exsr      repfil
012500010601     C                   if        depfil = invfil
012600021025     C                   if        vlrpoi = 999
012700040301     C                   MOVE(P)   invfil        VATFGS
012800021025     C                   else
012900040301     C                   MOVE(P)   vlrpoi        VATFGS
013000021025     C                   endif
013100050114     C                   endif
013200040301     C*
013300040301     C* Imposto e scarico il buffer del FIVAT
013400040301     C                   exsr      impvat
013500010601     C*
013600010601     C                   exsr      PREELA
013700010601     C*
013800010604     C* Ebbene...
013900010604     C*
014000010604     C  N31              ADD       1             低TROK            7 0
014100010604     C   32              ADD       1             低TRMO            7 0
014200010604     C   31              ADD       1             低TRNO            7 0
014300010604     C*
014400020722     C                   endif
014500020722     C*
014600010604     C                   if        *in31 = *off and
014700010604     C                             *in32 = *off
014800010604     C                   eval      vinflg = '1'
014900010604     C                   else
015000010604     C                   eval      vinflg = '2'
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
016600010601     C                   if        呃trno = 0 and
016700010604     C                             呃trmo = 0 and
016800010601     C                             flgStato <> '2'
016900010330     C* ... restituisco esito OK.
017000010330     C                   eval      wrkesito = '0'
017100010330     C                   else
017200010330     C                   if        呃trok > 0
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
018900040301     C                   if        %open(fivatwwr)
019000040301     C                   close     fivatwwr
019100010330     C                   endif
019200010601     C*
019300010601     C                   if        vlrpoi <> 999
019400010601     C                   eval      invfil = vlrpoi
019500010601     C                   endif
019600010330     C*
019700010330     C                   if        呃trok > 0
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
021200010601
021300010330     C*----------------------------------------------------*
021400020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021500010330     C*----------------------------------------------------*
021600010330     C     INZVAR        BEGSR
021700051219     C*
021800010330     C*
021900010330     C                   ENDSR
022000010330     C*----------------------------------------------------*
022100020722     C*  IMPOSTAZIONE CAMPI COSTANTI
022200010330     C*----------------------------------------------------*
022300010330     C     DEFCAM        BEGSR
022400010330     C*
022500020204     C* Inizializzo il buffer del record da scrivere
022600040301     C                   CLEAR                   FIVAT000
022700020204     C* Imposto i valori di default...
022800090714     C                   EVAL      VATCCM = 0061248
022900090714     C                   EVAL      VATLNP = 006
023000040301     C                   EVAL      VATTRC = 'E'
023100020204     C* ... e poi verifico se sono stati passati come parametri
023200020204     C                   IF        vlrppt > *blanks
023300040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
023400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023500020204     C                   EXSR      CHKNUM
023600020204     C                   IF        PiInt=*on
023700040301     C                   Z-ADD     PiVal         VATCCM
023800020204     C                   ENDIF
023900040301     C                   ENDIF
024000040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
024100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024200020204     C                   EXSR      CHKNUM
024300020204     C                   IF        PiInt=*on
024400040301     C                   Z-ADD     PiVal         VATLNP
024500020204     C                   ENDIF
024600040301     C                   ENDIF
024700020204     C                   ENDIF
024800020204     C*
024900010330     C                   ENDSR
025000010607     C*----------------------------------------------------*
025100040301     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
025200010607     C*----------------------------------------------------*
025300040301     C     IMPVAT        BEGSR
025400010607     C*
025500010607     C                   SETOFF                                       3132
025600040316     C*
025700040316     C* Campi "calcolati"
025800040316     C                   MOVEL     datcor        VATAAS
025900010607     C*
026000010607     C* Reperimento campi ALFA
026100090605     C*
026200091028     C                   EVAL      VATNOT = %trim(%subst(vindta:31:18))
026300040301     C*
026400040301     C* Reperimento campi NUMERICI
026500090609     C*
026600140312     C* AAS
026700140312     C                   EVAL      PiStr=%trim(%subst(vindta:51:4))
026800111213     C                   EXSR      CHKNUM
026900111213     C                   IF        PiInt=*on
027000140312     C                   Z-ADD     PiVal         VATAAS
027100111213     C                   ELSE
027200111213     C                   EVAL      vinmsg = %trimr(vinmsg)
027300140312     C                             + ' ' + 'VATAAS'
027400111213     C                   ENDIF
027500140312     C*
027600140312     C* CCM
027700140312     C                   EVAL      PiStr=%trim(%subst(vindta:1:7))
027800140312     C                   EXSR      CHKNUM
027900140312     C                   IF        PiInt=*on
028000140312     C                   Z-ADD     PiVal         VATCCM
028100140312     C                   ELSE
028200140312     C                   EVAL      vinmsg = %trimr(vinmsg)
028300140312     C                             + ' ' + 'VATCCM'
028400140312     C                   ENDIF
028500140311     C*
028600140311     C* Forzatura su codice / linea di partenza
028700140319     C                   IF        VATCCM = 0066123 OR
028800140319     C                             VATCCM = 0066266
028900140311     C                   EVAL      VATLNP = 059
029000140311     C                   EVAL      VATFGS = 059
029100140311     C                   ENDIF
029200140717     C* LNP/FGS
029300140717     C                   IF        %subst(vindta:101:3) <> *blank
029400140717     C                   EVAL      PiStr=%trim(%subst(vindta:101:3))
029500140717     C                   EXSR      CHKNUM
029600140717     C                   IF        PiInt=*on
029700140717     C                   Z-ADD     PiVal         VATLNP
029800140717     C                   Z-ADD     PiVal         VATFGS
029900140717     C                   ELSE
030000140717     C                   EVAL      vinmsg = %trimr(vinmsg)
030100140717     C                             + ' ' + 'VATLNP VATFGS'
030200140717     C                   ENDIF
030300140717     C                   ENDIF
030400090609     C* NSP
030500090714     C                   EVAL      PiStr=%trim(%subst(vindta:16:15))
030600040301     C                   EXSR      CHKNUM
030700040301     C                   IF        PiInt=*on
030800040301     C                   Z-ADD     PiVal         VATNSP
030900040301     C                   ELSE
031000040301     C                   SETON                                        31
031100040301     C                   Z-ADD     *zeros        VATNSP
031200040301     C                   EVAL      vinmsg = %trimr(vinmsg)
031300040301     C                             + ' ' + 'VATNSP'
031400040301     C                   ENDIF
031500040301     C*
031600040301     C* Scarico il buffer del FIVAT
031700040301     C  N31              WRITE     FIVAT000
031800020204     C*
031900010607     C                   ENDSR
032000010607     C*----------------------------------------------------*
032100020204
032200010330
032300010330
032400010330     C*----------------------------------------------------*
032500010330     C*  CONTROLLO NUMERICITA' CAMPI
032600010330     C*----------------------------------------------------*
032700010330     C     CHKNUM        BEGSR
032800081118     C*
032900081118     C                   IF        PiDecChr = *blanks
033000081118     C                   EVAL      PiDecChr = ','
033100081118     C                   ENDIF
033200081118     C*
033300081118     C                   callp(e)  UBISNUM_Check(PiStr
033400081118     C                                          :PiDecChr
033500081118     C                                          :PiVal
033600081118     C                                          :PiNum
033700081118     C                                          :PiInt)
033800081118     C*
033900081118     C                   IF        %error
034000081118     C                   EVAL      PiInt=*off
034100081118     C                   ENDIF
034200081118     C*
034300010330     C                   ENDSR
034400010330     C***
034500010330
034600010601
034700010601
034800010601
034900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
035000010601     C     repfil        BEGSR
035100010601     C*
035200010601     C                   if        invfil = *zeros and
035300010601     C                             depfil > *zeros and
035400010629     C                             (vinflg = *blanks or
035500010629     C                              vinflg = *zeros)
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
038900040301     C                   eval      cmzfla = 'FIVAT00F'
039000040301     C                   eval      cmzmba = 'FIVAT00F'
039100000710     C                   else
039200000710     C                   eval      cmzfla = prmfir
039300000710     C                   eval      cmzmba = prmfir
039400000710     C                   endif
039500990920     C                   eval      cmznrr = *zeros
039600990920     C                   move      呃trok        cmznrr
039700021018     C                   eval      cmzlba = vlrfl1
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
042400010601     C*
042500010601     C                   ENDSR
042600010601     C***
042700010601
042800010601
042900010601
043000010330
043100010330
043200000613     C     *inzsr        BEGSR
043300990910     C*
043400990910     C     *entry        plist
043500990920     C                   parm                    tivlrds
043600990921     C                   parm      wrkesito      esito
043700000724     C                   parm                    prmlit
043800000710     C                   parm                    prmfir
043900010330     C*
044000010330     C* CALCOLA LA DATA CORRENTE
044100010330     C                   time                    wn14             14 0
044200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
044300010330     C                   z-add     wn8           g08dat
044400010330     C                   z-add     *zeros        g08inv
044500010330     C                   movel     '0'           g08err
044600010330     C                   call      'XSRDA8'
044700010330     C                   parm                    wlbda8
044800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
044900000613     C*
045000000613     C                   ENDSR
045100000613     C***
