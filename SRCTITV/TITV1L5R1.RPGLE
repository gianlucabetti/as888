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
007300000616     C*
007400010601     C                   ENDSR
007500010601     C***
007600000613
007700010601
007800010601
007900010330     C*--------------------------------------------------------
008000040301     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
008100010330     C*--------------------------------------------------------
008200040301     C     RWVAT         BEGSR
008300010330     C*
008400010330     C                   if        not %open(tivin00r)
008500010330     C                   open      tivin00r
008600010330     C                   endif
008700040301     C                   if        not %open(fivatwwr)
008800040301     C                   open      fivatwwr
008900010330     C                   endif
009000010330     C*
009100010604     C                   clear                   低TROK
009200010604     C                   clear                   低TRMO
009300010604     C                   clear                   低TRNO
009400010330     C*
009500010330     C                   DO        *HIVAL
009600010330     C*
009700010330     C                   READ      tivin00r                               70
009800010618     C*
009900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
010000010618     C                   if        vinflg = '1'
010100010618     C                   eval      flgOk = '1'
010200010618     C                   endif
010300010618     C*
010400010330     C                   if        vindta > *blanks
010500010330     C                   add       1             rrnum
010600010330     C*
010700010601     C                   if        *in70 = *off and
010800010330     C                             (vinflg = *blanks
010900010330     C                              or vinflg = '0'
011000010330     C                              or vinflg = '2')
011100010330     C*
011200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011300010711     C                   if        vinflg = *blanks or vinflg = '0'
011400010711     C                   clear                   vinmsg
011500010711     C                   endif
011600010601     C*
011700040301     C* Inizializzo campi d wrk e imposto i valori d default
011800010330     C                   exsr      inzvar
011900010330     C                   exsr      defcam
012000010601     C*
012100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012200040301     C                   eval      depfil = VATLNP
012300010601     C                   exsr      repfil
012400010601     C                   if        depfil = invfil
012500021025     C                   if        vlrpoi = 999
012600040301     C                   MOVE(P)   invfil        VATFGS
012700021025     C                   else
012800040301     C                   MOVE(P)   vlrpoi        VATFGS
012900021025     C                   endif
013000050114     C                   endif
013100040301     C*
013200040301     C* Imposto e scarico il buffer del FIVAT
013300040301     C                   exsr      impvat
013400010601     C*
013500010601     C                   exsr      PREELA
013600010601     C*
013700010604     C* Ebbene...
013800010604     C*
013900010604     C  N31              ADD       1             低TROK            7 0
014000010604     C   32              ADD       1             低TRMO            7 0
014100010604     C   31              ADD       1             低TRNO            7 0
014200010604     C*
014300020722     C                   endif
014400020722     C*
014500010604     C                   if        *in31 = *off and
014600010604     C                             *in32 = *off
014700010604     C                   eval      vinflg = '1'
014800010604     C                   else
014900010604     C                   eval      vinflg = '2'
015000010604     C                   endif
015100010604     C*
015200010330     C                   else
015300010330     C                   eval      vinflg = '1'
015400010330     C                   endif
015500010601     C*
015600010601     C  N70              update    tivin000
015700010330     C*
015800010330     C  N70              ENDdo
015900010601     C
016000010601     C*
016100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
016200010601     C                   if        cntNonEl = *zeros or
016300010601     C                             flgMulti = '0'
016400010330     C* Se non ci sono record con errori ...
016500010601     C                   if        呃trno = 0 and
016600010604     C                             呃trmo = 0 and
016700010601     C                             flgStato <> '2'
016800010330     C* ... restituisco esito OK.
016900010330     C                   eval      wrkesito = '0'
017000010330     C                   else
017100010330     C                   if        呃trok > 0
017200010330     C                   eval      wrkesito = '1'
017300010330     C                   else
017400010615     C                   if        flgOk = '0'
017500010615     C                   eval      wrkesito = '2'
017600010615     C                   else
017700010615     C                   eval      wrkesito = '6'
017800010615     C                   endif
017900010330     C                   endif
018000010330     C                   endif
018100010601     C                   else
018200010601     C                   eval      wrkesito = '9'
018300010601     C                   endif
018400010330     C*
018500010330     C                   if        %open(tivin00r)
018600010330     C                   close     tivin00r
018700010330     C                   endif
018800040301     C                   if        %open(fivatwwr)
018900040301     C                   close     fivatwwr
019000010330     C                   endif
019100010601     C*
019200010601     C                   if        vlrpoi <> 999
019300010601     C                   eval      invfil = vlrpoi
019400010601     C                   endif
019500010330     C*
019600010330     C                   if        呃trok > 0
019700010601     C                             and invfil > *zeros
019800010330     C                   exsr      invio
019900010330     C                   endif
020000010601     C*
020100010618     C                   if        flgGiro = '1'
020200010601     C                   exsr      endela
020300010618     C                   endif
020400010330     C*
020500010330     C                   ENDSR
020600010330     C***
020700010330
020800010601
020900010601
021000010601
021100010601
021200010330     C*----------------------------------------------------*
021300020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021400010330     C*----------------------------------------------------*
021500010330     C     INZVAR        BEGSR
021600051219     C*
021700010330     C*
021800010330     C                   ENDSR
021900010330     C*----------------------------------------------------*
022000020722     C*  IMPOSTAZIONE CAMPI COSTANTI
022100010330     C*----------------------------------------------------*
022200010330     C     DEFCAM        BEGSR
022300010330     C*
022400020204     C* Inizializzo il buffer del record da scrivere
022500040301     C                   CLEAR                   FIVAT000
022600020204     C* Imposto i valori di default...
022700081218     C                   EVAL      VATCCM = 0064052
022800081218     C                   EVAL      VATLNP = 006
022900040301     C                   EVAL      VATTRC = 'E'
023000020204     C* ... e poi verifico se sono stati passati come parametri
023100020204     C                   IF        vlrppt > *blanks
023200040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
023300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023400020204     C                   EXSR      CHKNUM
023500020204     C                   IF        PiInt=*on
023600040301     C                   Z-ADD     PiVal         VATCCM
023700020204     C                   ENDIF
023800040301     C                   ENDIF
023900040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
024000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024100020204     C                   EXSR      CHKNUM
024200020204     C                   IF        PiInt=*on
024300040301     C                   Z-ADD     PiVal         VATLNP
024400020204     C                   ENDIF
024500040301     C                   ENDIF
024600020204     C                   ENDIF
024700020204     C*
024800010330     C                   ENDSR
024900010607     C*----------------------------------------------------*
025000040301     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
025100010607     C*----------------------------------------------------*
025200040301     C     IMPVAT        BEGSR
025300010607     C*
025400010607     C                   SETOFF                                       3132
025500040316     C*
025600040316     C* Campi "calcolati"
025700040316     C                   MOVEL     datcor        VATAAS
025800010607     C*
025900010607     C* Reperimento campi ALFA
026000160314     C                   EVAL      VATNOT = %trim(%subst(vindta:86:16))
026100040301     C*
026200040301     C* Reperimento campi NUMERICI
026300160314     C                   EVAL      PiStr=%trim(%subst(vindta:14:2)) +
026400160314     C                                   %trim(%subst(vindta:17:6))
026500040301     C                   EXSR      CHKNUM
026600040301     C                   IF        PiInt=*on
026700040301     C                   Z-ADD     PiVal         VATNSP
026800040301     C                   ELSE
026900040301     C                   SETON                                        31
027000040301     C                   Z-ADD     *zeros        VATNSP
027100040301     C                   EVAL      vinmsg = %trimr(vinmsg)
027200040301     C                             + ' ' + 'VATNSP'
027300040301     C                   ENDIF
027400040301     C*
027500040301     C* Scarico il buffer del FIVAT
027600040301     C  N31              WRITE     FIVAT000
027700020204     C*
027800010607     C                   ENDSR
027900010607     C*----------------------------------------------------*
028000020204
028100010330
028200010330
028300010330     C*----------------------------------------------------*
028400010330     C*  CONTROLLO NUMERICITA' CAMPI
028500010330     C*----------------------------------------------------*
028600010330     C     CHKNUM        BEGSR
028700081118     C*
028800081118     C                   IF        PiDecChr = *blanks
028900081118     C                   EVAL      PiDecChr = ','
029000081118     C                   ENDIF
029100081118     C*
029200081118     C                   callp(e)  UBISNUM_Check(PiStr
029300081118     C                                          :PiDecChr
029400081118     C                                          :PiVal
029500081118     C                                          :PiNum
029600081118     C                                          :PiInt)
029700081118     C*
029800081118     C                   IF        %error
029900081118     C                   EVAL      PiInt=*off
030000081118     C                   ENDIF
030100081118     C*
030200010330     C                   ENDSR
030300010330     C***
030400010330
030500010601
030600010601
030700010601
030800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
030900010601     C     repfil        BEGSR
031000010601     C*
031100010601     C                   if        invfil = *zeros and
031200010601     C                             depfil > *zeros and
031300010629     C                             (vinflg = *blanks or
031400010629     C                              vinflg = *zeros)
031500010601     C
031600010601     C                   eval      invfil = depfil
031700010601     C                   endif
031800010601     C*
031900010601     C                   if        depfil <> invfil and
032000010601     C                             invfil > *zeros
032100010601     C                   eval      flgMulti = '1'
032200010601     C                   if        vinflg = *blanks
032300010601     C                   add       1             cntNonEl
032400010601     C                   endif
032500010601     C                   endif
032600010601     C*
032700010601     C                   if        vinflg = '2'
032800010601     C                   eval      flgStato = '2'
032900010601     C                   endif
033000010601     C*
033100010601     C                   ENDSR
033200010601     C***
033300010601
033400010601
033500010601
033600010330
033700010330
033800010330
033900990920      /TITLE Invio dei dati al punto operativo.
034000000613     C     invio         BEGSR
034100990920     C*
034200990920     C                   reset                   dscmz
034300010601     C                   move      invfil        cmzdst
034400990920     C                   eval      cmzfld = vlrfou
034500990920     C                   eval      cmzmbd = vlrhdl
034600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
034700000710     C                   if        prmfir = *blanks
034800040301     C                   eval      cmzfla = 'FIVAT00F'
034900040301     C                   eval      cmzmba = 'FIVAT00F'
035000000710     C                   else
035100000710     C                   eval      cmzfla = prmfir
035200000710     C                   eval      cmzmba = prmfir
035300000710     C                   endif
035400990920     C                   eval      cmznrr = *zeros
035500990920     C                   move      呃trok        cmznrr
035600021018     C                   eval      cmzlba = vlrfl1
035700990920     C                   call(e)   'TIS711C'
035800990920     C                   parm                    dscmz
035900990921     C                   parm      *blanks       esito
036000990920     C                   if        %error
036100990920     C                             or cmzerr = '1'
036200990921     C                             or esito  = '1'
036300000710     C                   eval      wrkesito = '3'
036400990920     C                   endif
036500990920     C*
036600000613     C                   ENDSR
036700990910
036800010601
036900010601
037000010601
037100010601
037200010601      /TITLE Invio dei dati al punto operativo.
037300010601     C     opeini        BEGSR
037400010601     C*
037500010601     C* Inizializzo flag e contatori operativi
037600010601     C                   movel     '0'           flgGiro           1
037700010601     C                   movel     '0'           flgMulti          1
037800010601     C                   movel     '1'           flgStato          1
037900010615     C                   movel     '0'           flgOk             1
038000010601     C                   z-add     *zeros        cntNonEl         10 0
038100010601     C                   z-add     *zeros        depfil            3 0
038200010601     C                   z-add     *zeros        invfil            3 0
038300010601     C*
038400010601     C                   ENDSR
038500010601     C***
038600010601
038700010601
038800010601
038900010330
039000010330
039100000613     C     *inzsr        BEGSR
039200990910     C*
039300990910     C     *entry        plist
039400990920     C                   parm                    tivlrds
039500990921     C                   parm      wrkesito      esito
039600000724     C                   parm                    prmlit
039700000710     C                   parm                    prmfir
039800010330     C*
039900010330     C* CALCOLA LA DATA CORRENTE
040000010330     C                   time                    wn14             14 0
040100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
040200010330     C                   z-add     wn8           g08dat
040300010330     C                   z-add     *zeros        g08inv
040400010330     C                   movel     '0'           g08err
040500010330     C                   call      'XSRDA8'
040600010330     C                   parm                    wlbda8
040700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
040800000613     C*
040900000613     C                   ENDSR
041000000613     C***
