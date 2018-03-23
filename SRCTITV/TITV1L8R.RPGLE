000100080410      /TITLE Upload via Internet: traduzione in FIVATWWR.
000200130125     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500040301     FFIVATwwr  O    E             DISK    usropn
000600010330     D*----------------------------------------------------
000700010330     D* DICHIARAZIOINE VARIABILI DI WRK
000800010330     D*----------------------------------------------------
000900010330     D dscmz         e ds                  inz
001000010330     D psds           sds
001100010330     D  procname         *PROC
001200010330     D tivlrds       e ds                  extname(tivlr00f)
001300010330     D esito           s              1
001400010330     D prmlit          s             10
001500010330     D prmfir          s             10
001600010330     D wrkesito        s                   like(esito)
001700010330     D rrnum           s              6  0 INZ(*zeros)
001800010330     D*------------------
001900010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002000010330     D*------------------
002100010330     D WLBDA8          DS                  INZ
002200010330     D  G08DAT                 1      8  0
002300010330     D  G08INV                 9     16  0
002400010330     D  G08ERR                17     17
002500010330     D  G08TGI                18     22  0
002600000313     D*
002700081118
002800081118     D*------------------
002900081118     D* LINKING A DEFINIZIONI ESTERNE
003000081118     D*------------------
003100081118     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
003200081218     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
003300081118
003400010330
003500010330
003600010330
003700000913     C                   reset                   rrnum
003800990921     C                   reset                   esito
003900990921     C                   reset                   wrkesito
004000010601     C*
004100010601     C                   exsr      opeini
004200040301     C                   exsr      rwvat
004300010601     C*
004400010601     C                   seton                                        lr
004500010601
004600010601
004700010601
004800010601
004900010601     C*--------------------------------------------------------
005000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005100010601     C*--------------------------------------------------------
005200010601     C     PREELA        BEGSR
005300010601     C*
005400010601     C* SE OCCORRE SPEDIRE IN FILIALE
005500010601     C                   if        invfil <> *zeros and
005600010601     C                             flgGiro = '0'
005700010601     C*
005800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005900010601     C                   eval      flgGiro = '1'
006000010601     C*
006100010601     C                   endif
006200010601     C*
006300010601     C                   ENDSR
006400010601     C***
006500010601
006600010601
006700010601
006800010601     C*--------------------------------------------------------
006900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007000010601     C*--------------------------------------------------------
007100010601     C     ENDELA        BEGSR
007200000616     C*
007300010601     C                   ENDSR
007400010601     C***
007500000613
007600010601
007700010601
007800010330     C*--------------------------------------------------------
007900040301     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
008000010330     C*--------------------------------------------------------
008100040301     C     RWVAT         BEGSR
008200010330     C*
008300010330     C                   if        not %open(tivin00r)
008400010330     C                   open      tivin00r
008500010330     C                   endif
008600040301     C                   if        not %open(fivatwwr)
008700040301     C                   open      fivatwwr
008800010330     C                   endif
008900010330     C*
009000010604     C                   clear                   低TROK
009100010604     C                   clear                   低TRMO
009200010604     C                   clear                   低TRNO
009300010330     C*
009400010330     C                   DO        *HIVAL
009500010330     C*
009600010330     C                   READ      tivin00r                               70
009700010618     C*
009800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
009900010618     C                   if        vinflg = '1'
010000010618     C                   eval      flgOk = '1'
010100010618     C                   endif
010200010618     C*
010300010330     C                   if        vindta > *blanks
010400010330     C                   add       1             rrnum
010500010330     C*
010600010601     C                   if        *in70 = *off and
010700010330     C                             (vinflg = *blanks
010800010330     C                              or vinflg = '0'
010900010330     C                              or vinflg = '2')
011000010330     C*
011100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011200010711     C                   if        vinflg = *blanks or vinflg = '0'
011300010711     C                   clear                   vinmsg
011400010711     C                   endif
011500010601     C*
011600040301     C* Inizializzo campi d wrk e imposto i valori d default
011700010330     C                   exsr      inzvar
011800010330     C                   exsr      defcam
011900010601     C*
012000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012100040301     C                   eval      depfil = VATLNP
012200010601     C                   exsr      repfil
012300010601     C                   if        depfil = invfil
012400021025     C                   if        vlrpoi = 999
012500040301     C                   MOVE(P)   invfil        VATFGS
012600021025     C                   else
012700040301     C                   MOVE(P)   vlrpoi        VATFGS
012800021025     C                   endif
012900050114     C                   endif
013000040301     C*
013100040301     C* Imposto e scarico il buffer del FIVAT
013200040301     C                   exsr      impvat
013300010601     C*
013400010601     C                   exsr      PREELA
013500010601     C*
013600010604     C* Ebbene...
013700010604     C*
013800010604     C  N31              ADD       1             低TROK            7 0
013900010604     C   32              ADD       1             低TRMO            7 0
014000010604     C   31              ADD       1             低TRNO            7 0
014100010604     C*
014200020722     C                   endif
014300020722     C*
014400010604     C                   if        *in31 = *off and
014500010604     C                             *in32 = *off
014600010604     C                   eval      vinflg = '1'
014700010604     C                   else
014800010604     C                   eval      vinflg = '2'
014900010604     C                   endif
015000010604     C*
015100010330     C                   else
015200010330     C                   eval      vinflg = '1'
015300010330     C                   endif
015400010601     C*
015500010601     C  N70              update    tivin000
015600010330     C*
015700010330     C  N70              ENDdo
015800010601     C
015900010601     C*
016000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
016100010601     C                   if        cntNonEl = *zeros or
016200010601     C                             flgMulti = '0'
016300010330     C* Se non ci sono record con errori ...
016400010601     C                   if        呃trno = 0 and
016500010604     C                             呃trmo = 0 and
016600010601     C                             flgStato <> '2'
016700010330     C* ... restituisco esito OK.
016800010330     C                   eval      wrkesito = '0'
016900010330     C                   else
017000010330     C                   if        呃trok > 0
017100010330     C                   eval      wrkesito = '1'
017200010330     C                   else
017300010615     C                   if        flgOk = '0'
017400010615     C                   eval      wrkesito = '2'
017500010615     C                   else
017600010615     C                   eval      wrkesito = '6'
017700010615     C                   endif
017800010330     C                   endif
017900010330     C                   endif
018000010601     C                   else
018100010601     C                   eval      wrkesito = '9'
018200010601     C                   endif
018300010330     C*
018400010330     C                   if        %open(tivin00r)
018500010330     C                   close     tivin00r
018600010330     C                   endif
018700040301     C                   if        %open(fivatwwr)
018800040301     C                   close     fivatwwr
018900010330     C                   endif
019000010601     C*
019100010601     C                   if        vlrpoi <> 999
019200010601     C                   eval      invfil = vlrpoi
019300010601     C                   endif
019400010330     C*
019500010330     C                   if        呃trok > 0
019600010601     C                             and invfil > *zeros
019700010330     C                   exsr      invio
019800010330     C                   endif
019900010601     C*
020000010618     C                   if        flgGiro = '1'
020100010601     C                   exsr      endela
020200010618     C                   endif
020300010330     C*
020400010330     C                   ENDSR
020500010330     C***
020600010330
020700010601
020800010601
020900010601
021000010601
021100010330     C*----------------------------------------------------*
021200020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021300010330     C*----------------------------------------------------*
021400010330     C     INZVAR        BEGSR
021500051219     C*
021600010330     C*
021700010330     C                   ENDSR
021800010330     C*----------------------------------------------------*
021900020722     C*  IMPOSTAZIONE CAMPI COSTANTI
022000010330     C*----------------------------------------------------*
022100010330     C     DEFCAM        BEGSR
022200010330     C*
022300020204     C* Inizializzo il buffer del record da scrivere
022400040301     C                   CLEAR                   FIVAT000
022500020204     C* Imposto i valori di default...
022600090107     C                   EVAL      VATCCM = 0895008
022700090107     C                   EVAL      VATLNP = 089
022800040301     C                   EVAL      VATTRC = 'E'
022900020204     C* ... e poi verifico se sono stati passati come parametri
023000020204     C                   IF        vlrppt > *blanks
023100040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
023200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023300020204     C                   EXSR      CHKNUM
023400020204     C                   IF        PiInt=*on
023500040301     C                   Z-ADD     PiVal         VATCCM
023600020204     C                   ENDIF
023700040301     C                   ENDIF
023800040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
023900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024000020204     C                   EXSR      CHKNUM
024100020204     C                   IF        PiInt=*on
024200040301     C                   Z-ADD     PiVal         VATLNP
024300020204     C                   ENDIF
024400040301     C                   ENDIF
024500020204     C                   ENDIF
024600020204     C*
024700010330     C                   ENDSR
024800010607     C*----------------------------------------------------*
024900040301     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
025000010607     C*----------------------------------------------------*
025100040301     C     IMPVAT        BEGSR
025200010607     C*
025300010607     C                   SETOFF                                       3132
025400040316     C*
025500040316     C* Campi "calcolati"
025600040316     C                   MOVEL     datcor        VATAAS
025700010607     C*
025800010607     C* Reperimento campi ALFA
025900090107     C                   EVAL      VATNOT = %trim(%subst(vindta:23:12))
026000040301     C*
026100040301     C* Reperimento campi NUMERICI
026200090107     C                   EVAL      PiStr=%trim(%subst(vindta:7:15))
026300040301     C                   EXSR      CHKNUM
026400040301     C                   IF        PiInt=*on
026500040301     C                   Z-ADD     PiVal         VATNSP
026600040301     C                   ELSE
026700040301     C                   SETON                                        31
026800040301     C                   Z-ADD     *zeros        VATNSP
026900040301     C                   EVAL      vinmsg = %trimr(vinmsg)
027000040301     C                             + ' ' + 'VATNSP'
027100040301     C                   ENDIF
027200040301     C*
027300040301     C* Scarico il buffer del FIVAT
027400040301     C  N31              WRITE     FIVAT000
027500020204     C*
027600010607     C                   ENDSR
027700010607     C*----------------------------------------------------*
027800020204
027900010330
028000010330
028100010330     C*----------------------------------------------------*
028200010330     C*  CONTROLLO NUMERICITA' CAMPI
028300010330     C*----------------------------------------------------*
028400010330     C     CHKNUM        BEGSR
028500081118     C*
028600081118     C                   IF        PiDecChr = *blanks
028700081118     C                   EVAL      PiDecChr = ','
028800081118     C                   ENDIF
028900081118     C*
029000081118     C                   callp(e)  UBISNUM_Check(PiStr
029100081118     C                                          :PiDecChr
029200081118     C                                          :PiVal
029300081118     C                                          :PiNum
029400081118     C                                          :PiInt)
029500081118     C*
029600081118     C                   IF        %error
029700081118     C                   EVAL      PiInt=*off
029800081118     C                   ENDIF
029900081118     C*
030000010330     C                   ENDSR
030100010330     C***
030200010330
030300010601
030400010601
030500010601
030600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
030700010601     C     repfil        BEGSR
030800010601     C*
030900010601     C                   if        invfil = *zeros and
031000010601     C                             depfil > *zeros and
031100010629     C                             (vinflg = *blanks or
031200010629     C                              vinflg = *zeros)
031300010601     C
031400010601     C                   eval      invfil = depfil
031500010601     C                   endif
031600010601     C*
031700010601     C                   if        depfil <> invfil and
031800010601     C                             invfil > *zeros
031900010601     C                   eval      flgMulti = '1'
032000010601     C                   if        vinflg = *blanks
032100010601     C                   add       1             cntNonEl
032200010601     C                   endif
032300010601     C                   endif
032400010601     C*
032500010601     C                   if        vinflg = '2'
032600010601     C                   eval      flgStato = '2'
032700010601     C                   endif
032800010601     C*
032900010601     C                   ENDSR
033000010601     C***
033100010601
033200010601
033300010601
033400010330
033500010330
033600010330
033700990920      /TITLE Invio dei dati al punto operativo.
033800000613     C     invio         BEGSR
033900990920     C*
034000990920     C                   reset                   dscmz
034100010601     C                   move      invfil        cmzdst
034200990920     C                   eval      cmzfld = vlrfou
034300990920     C                   eval      cmzmbd = vlrhdl
034400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
034500000710     C                   if        prmfir = *blanks
034600040301     C                   eval      cmzfla = 'FIVAT00F'
034700040301     C                   eval      cmzmba = 'FIVAT00F'
034800000710     C                   else
034900000710     C                   eval      cmzfla = prmfir
035000000710     C                   eval      cmzmba = prmfir
035100000710     C                   endif
035200990920     C                   eval      cmznrr = *zeros
035300990920     C                   move      呃trok        cmznrr
035400021018     C                   eval      cmzlba = vlrfl1
035500990920     C                   call(e)   'TIS711C'
035600990920     C                   parm                    dscmz
035700990921     C                   parm      *blanks       esito
035800990920     C                   if        %error
035900990920     C                             or cmzerr = '1'
036000990921     C                             or esito  = '1'
036100000710     C                   eval      wrkesito = '3'
036200990920     C                   endif
036300990920     C*
036400000613     C                   ENDSR
036500990910
036600010601
036700010601
036800010601
036900010601
037000010601      /TITLE Invio dei dati al punto operativo.
037100010601     C     opeini        BEGSR
037200010601     C*
037300010601     C* Inizializzo flag e contatori operativi
037400010601     C                   movel     '0'           flgGiro           1
037500010601     C                   movel     '0'           flgMulti          1
037600010601     C                   movel     '1'           flgStato          1
037700010615     C                   movel     '0'           flgOk             1
037800010601     C                   z-add     *zeros        cntNonEl         10 0
037900010601     C                   z-add     *zeros        depfil            3 0
038000010601     C                   z-add     *zeros        invfil            3 0
038100010601     C*
038200010601     C                   ENDSR
038300010601     C***
038400010601
038500010601
038600010601
038700010330
038800010330
038900000613     C     *inzsr        BEGSR
039000990910     C*
039100990910     C     *entry        plist
039200990920     C                   parm                    tivlrds
039300990921     C                   parm      wrkesito      esito
039400000724     C                   parm                    prmlit
039500000710     C                   parm                    prmfir
039600010330     C*
039700010330     C* CALCOLA LA DATA CORRENTE
039800010330     C                   time                    wn14             14 0
039900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
040000010330     C                   z-add     wn8           g08dat
040100010330     C                   z-add     *zeros        g08inv
040200010330     C                   movel     '0'           g08err
040300010330     C                   call      'XSRDA8'
040400010330     C                   parm                    wlbda8
040500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
040600000613     C*
040700000613     C                   ENDSR
040800000613     C***
