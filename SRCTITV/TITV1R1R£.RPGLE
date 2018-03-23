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
029200090609     C* NSP
029300090714     C                   EVAL      PiStr=%trim(%subst(vindta:16:15))
029400040301     C                   EXSR      CHKNUM
029500040301     C                   IF        PiInt=*on
029600040301     C                   Z-ADD     PiVal         VATNSP
029700040301     C                   ELSE
029800040301     C                   SETON                                        31
029900040301     C                   Z-ADD     *zeros        VATNSP
030000040301     C                   EVAL      vinmsg = %trimr(vinmsg)
030100040301     C                             + ' ' + 'VATNSP'
030200040301     C                   ENDIF
030300040301     C*
030400040301     C* Scarico il buffer del FIVAT
030500040301     C  N31              WRITE     FIVAT000
030600020204     C*
030700010607     C                   ENDSR
030800010607     C*----------------------------------------------------*
030900020204
031000010330
031100010330
031200010330     C*----------------------------------------------------*
031300010330     C*  CONTROLLO NUMERICITA' CAMPI
031400010330     C*----------------------------------------------------*
031500010330     C     CHKNUM        BEGSR
031600081118     C*
031700081118     C                   IF        PiDecChr = *blanks
031800081118     C                   EVAL      PiDecChr = ','
031900081118     C                   ENDIF
032000081118     C*
032100081118     C                   callp(e)  UBISNUM_Check(PiStr
032200081118     C                                          :PiDecChr
032300081118     C                                          :PiVal
032400081118     C                                          :PiNum
032500081118     C                                          :PiInt)
032600081118     C*
032700081118     C                   IF        %error
032800081118     C                   EVAL      PiInt=*off
032900081118     C                   ENDIF
033000081118     C*
033100010330     C                   ENDSR
033200010330     C***
033300010330
033400010601
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
037700040301     C                   eval      cmzfla = 'FIVAT00F'
037800040301     C                   eval      cmzmba = 'FIVAT00F'
037900000710     C                   else
038000000710     C                   eval      cmzfla = prmfir
038100000710     C                   eval      cmzmba = prmfir
038200000710     C                   endif
038300990920     C                   eval      cmznrr = *zeros
038400990920     C                   move      呃trok        cmznrr
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
043700000613     C*
043800000613     C                   ENDSR
043900000613     C***
