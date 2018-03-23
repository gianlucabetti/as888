000100080410      /TITLE Upload via Internet: traduzione in FIVATWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
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
001800010330
001900010330
002000161219
002100000913     C                   reset                   rrnum
002200990921     C                   reset                   esito
002300990921     C                   reset                   wrkesito
002400010601     C*
002500010601     C                   exsr      opeini
002600040301     C                   exsr      rwvat
002700010601     C*
002800010601     C                   seton                                        lr
002900010601
003000010601
003100010601
003200010601
003300010601     C*--------------------------------------------------------
003400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
003500010601     C*--------------------------------------------------------
003600010601     C     PREELA        BEGSR
003700010601     C*
003800010601     C* SE OCCORRE SPEDIRE IN FILIALE
003900010601     C                   if        invfil <> *zeros and
004000010601     C                             flgGiro = '0'
004100010601     C*
004200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
004300010601     C                   eval      flgGiro = '1'
004400010601     C*
004500010601     C                   endif
004600010601     C*
004700010601     C                   ENDSR
004800010601     C***
004900010601
005000010601
005100010601
005200010601     C*--------------------------------------------------------
005300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
005400010601     C*--------------------------------------------------------
005500010601     C     ENDELA        BEGSR
005600990915     C*
005700000616     C*
005800010601     C                   ENDSR
005900010601     C***
006000010601
006100010330     C*--------------------------------------------------------
006200040301     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
006300010330     C*--------------------------------------------------------
006400040301     C     RWVAT         BEGSR
006500010330     C*
006600010330     C                   if        not %open(tivin00r)
006700010330     C                   open      tivin00r
006800010330     C                   endif
006900040301     C                   if        not %open(fivatwwr)
007000040301     C                   open      fivatwwr
007100010330     C                   endif
007200010330     C*
007300010604     C                   clear                   低TROK
007400010604     C                   clear                   低TRMO
007500010604     C                   clear                   低TRNO
007600010330     C*
007700010330     C                   DO        *HIVAL
007800010330     C*
007900010330     C                   READ      tivin00r                               70
008000010618     C*
008100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
008200010618     C                   if        vinflg = '1'
008300010618     C                   eval      flgOk = '1'
008400010618     C                   endif
008500010618     C*
008600010330     C                   if        vindta > *blanks
008700010330     C                   add       1             rrnum
008800010330     C*
008900010601     C                   if        *in70 = *off and
009000010330     C                             (vinflg = *blanks
009100010330     C                              or vinflg = '0'
009200010330     C                              or vinflg = '2')
009300010330     C*
009400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
009500010711     C                   if        vinflg = *blanks or vinflg = '0'
009600010711     C                   clear                   vinmsg
009700010711     C                   endif
009800010601     C*
009900040301     C* Inizializzo campi d wrk e imposto i valori d default
010000010330     C                   exsr      inzvar
010100010330     C                   exsr      defcam
010200010601     C*
010300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
010400040301     C                   eval      depfil = VATLNP
010500010601     C                   exsr      repfil
010600010601     C                   if        depfil = invfil
010700021025     C                   if        vlrpoi = 999
010800040301     C                   MOVE(P)   invfil        VATFGS
010900021025     C                   else
011000040301     C                   MOVE(P)   vlrpoi        VATFGS
011100021025     C                   endif
011200050114     C                   endif
011300040301     C*
011400040301     C* Imposto e scarico il buffer del FIVAT
011500040301     C                   exsr      impvat
011600010601     C*
011700010601     C                   exsr      PREELA
011800010601     C*
011900010604     C* Ebbene...
012000010604     C*
012100010604     C  N31              ADD       1             低TROK            7 0
012200010604     C   32              ADD       1             低TRMO            7 0
012300010604     C   31              ADD       1             低TRNO            7 0
012400010604     C*
012500020722     C                   endif
012600020722     C*
012700010604     C                   if        *in31 = *off and
012800010604     C                             *in32 = *off
012900010604     C                   eval      vinflg = '1'
013000010604     C                   else
013100010604     C                   eval      vinflg = '2'
013200010604     C                   endif
013300010604     C*
013400010330     C                   else
013500010330     C                   eval      vinflg = '1'
013600010330     C                   endif
013700010601     C*
013800010601     C  N70              update    tivin000
013900010330     C*
014000010330     C  N70              ENDdo
014100010601     C
014200010601     C*
014300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
014400010601     C                   if        cntNonEl = *zeros or
014500010601     C                             flgMulti = '0'
014600010330     C* Se non ci sono record con errori ...
014700010601     C                   if        呃trno = 0 and
014800010604     C                             呃trmo = 0 and
014900010601     C                             flgStato <> '2'
015000010330     C* ... restituisco esito OK.
015100010330     C                   eval      wrkesito = '0'
015200010330     C                   else
015300010330     C                   if        呃trok > 0
015400010330     C                   eval      wrkesito = '1'
015500010330     C                   else
015600010615     C                   if        flgOk = '0'
015700010615     C                   eval      wrkesito = '2'
015800010615     C                   else
015900010615     C                   eval      wrkesito = '6'
016000010615     C                   endif
016100010330     C                   endif
016200010330     C                   endif
016300010601     C                   else
016400010601     C                   eval      wrkesito = '9'
016500010601     C                   endif
016600010330     C*
016700010330     C                   if        %open(tivin00r)
016800010330     C                   close     tivin00r
016900010330     C                   endif
017000040301     C                   if        %open(fivatwwr)
017100040301     C                   close     fivatwwr
017200010330     C                   endif
017300010601     C*
017400010601     C                   if        vlrpoi <> 999
017500010601     C                   eval      invfil = vlrpoi
017600010601     C                   endif
017700010330     C*
017800010330     C                   if        呃trok > 0
017900010601     C                             and invfil > *zeros
018000010330     C                   exsr      invio
018100010330     C                   endif
018200010601     C*
018300010618     C                   if        flgGiro = '1'
018400010601     C                   exsr      endela
018500010618     C                   endif
018600010330     C*
018700010330     C                   ENDSR
018800010330     C***
018900010330
019000010601
019100010601
019200010601
019300010601
019400010330     C*----------------------------------------------------*
019500020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019600010330     C*----------------------------------------------------*
019700010330     C     INZVAR        BEGSR
019800051219     C*
019900010330     C*
020000010330     C                   ENDSR
020100010330     C*----------------------------------------------------*
020200020722     C*  IMPOSTAZIONE CAMPI COSTANTI
020300010330     C*----------------------------------------------------*
020400010330     C     DEFCAM        BEGSR
020500010330     C*
020600020204     C* Inizializzo il buffer del record da scrivere
020700040301     C                   CLEAR                   FIVAT000
020800020204     C* Imposto i valori di default...
020900161219     C                   EVAL      VATCCM = 0012811
021000161219     C                   EVAL      VATLNP = 001
021100040301     C                   EVAL      VATTRC = 'E'
021200020204     C* ... e poi verifico se sono stati passati come parametri
021300020204     C                   IF        vlrppt > *blanks
021400040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
021500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
021600020204     C                   EXSR      CHKNUM
021700020204     C                   IF        PiInt=*on
021800040301     C                   Z-ADD     PiVal         VATCCM
021900020204     C                   ENDIF
022000040301     C                   ENDIF
022100040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
022200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022300020204     C                   EXSR      CHKNUM
022400020204     C                   IF        PiInt=*on
022500040301     C                   Z-ADD     PiVal         VATLNP
022600020204     C                   ENDIF
022700040301     C                   ENDIF
022800020204     C                   ENDIF
022900020204     C*
023000010330     C                   ENDSR
023100010607     C*----------------------------------------------------*
023200040301     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
023300010607     C*----------------------------------------------------*
023400040301     C     IMPVAT        BEGSR
023500010607     C*
023600010607     C                   SETOFF                                       3132
023700040316     C*
023800040316     C* Campi "calcolati"
023900040316     C                   MOVEL     datcor        VATAAS
024000040301     C*
024100040301     C* Reperimento campi NUMERICI
024200080410     C                   EVAL      PiStr=%trim(%subst(vindta:16:7))
024300040301     C                   EXSR      CHKNUM
024400040301     C                   IF        PiInt=*on
024500040301     C                   Z-ADD     PiVal         VATNSP
024600040301     C                   ELSE
024700040301     C                   SETON                                        31
024800040301     C                   Z-ADD     *zeros        VATNSP
024900040301     C                   EVAL      vinmsg = %trimr(vinmsg)
025000040301     C                             + ' ' + 'VATNSP'
025100040301     C                   ENDIF
025200130117     C*
025300130117     C* - tipo record 'E'
025400130117     C                   EVAL      VATTRC = 'E'
025500130117     C                   EVAL      VATNOT = %trim(%subst(vindta:117:35))
025600040301     C  N31              WRITE     FIVAT000
025700020204     C*
025800010607     C                   ENDSR
025900010607     C*----------------------------------------------------*
026000020204
026100010330
026200010330
026300010330     C*----------------------------------------------------*
026400010330     C*  CONTROLLO NUMERICITA' CAMPI
026500010330     C*----------------------------------------------------*
026600010330     C     CHKNUM        BEGSR
026700010330     C*
026800010606     C                   IF        PiDecChr = *blanks
026900010606     C                   EVAL      PiDecChr = ','
027000010606     C                   ENDIF
027100010606     C*
027200010606     C                   CALL(e)   'ISNUMERIC'
027300010330     C                   PARM                    PiStr            30
027400010606     C                   PARM                    PiDecChr          1
027500010330     C                   PARM      *ZEROS        PiVal            30 9
027600010330     C                   PARM      '0'           PiInt             1
027700010330     C                   PARM      '0'           PiNum             1
027800010330     C                   IF        %error
027900010606     C                   EVAL      PiNum=*off
028000010330     C                   ENDIF
028100010330     C*
028200010330     C                   ENDSR
028300010330     C***
028400010330
028500010601
028600010601
028700010601
028800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
028900010601     C     repfil        BEGSR
029000010601     C*
029100010601     C                   if        invfil = *zeros and
029200010601     C                             depfil > *zeros and
029300010629     C                             (vinflg = *blanks or
029400010629     C                              vinflg = *zeros)
029500010601     C
029600010601     C                   eval      invfil = depfil
029700010601     C                   endif
029800010601     C*
029900010601     C                   if        depfil <> invfil and
030000010601     C                             invfil > *zeros
030100010601     C                   eval      flgMulti = '1'
030200010601     C                   if        vinflg = *blanks
030300010601     C                   add       1             cntNonEl
030400010601     C                   endif
030500010601     C                   endif
030600010601     C*
030700010601     C                   if        vinflg = '2'
030800010601     C                   eval      flgStato = '2'
030900010601     C                   endif
031000010601     C*
031100010601     C                   ENDSR
031200010601     C***
031300010601
031400010601
031500010601
031600010330
031700010330
031800010330
031900990920      /TITLE Invio dei dati al punto operativo.
032000000613     C     invio         BEGSR
032100990920     C*
032200990920     C                   reset                   dscmz
032300010601     C                   move      invfil        cmzdst
032400990920     C                   eval      cmzfld = vlrfou
032500990920     C                   eval      cmzmbd = vlrhdl
032600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
032700000710     C                   if        prmfir = *blanks
032800040301     C                   eval      cmzfla = 'FIVAT00F'
032900040301     C                   eval      cmzmba = 'FIVAT00F'
033000000710     C                   else
033100000710     C                   eval      cmzfla = prmfir
033200000710     C                   eval      cmzmba = prmfir
033300000710     C                   endif
033400990920     C                   eval      cmznrr = *zeros
033500990920     C                   move      呃trok        cmznrr
033600021018     C                   eval      cmzlba = vlrfl1
033700990920     C                   call(e)   'TIS711C'
033800990920     C                   parm                    dscmz
033900990921     C                   parm      *blanks       esito
034000990920     C                   if        %error
034100990920     C                             or cmzerr = '1'
034200990921     C                             or esito  = '1'
034300000710     C                   eval      wrkesito = '3'
034400990920     C                   endif
034500990920     C*
034600000613     C                   ENDSR
034700990910
034800010601
034900010601
035000010601
035100010601
035200010601      /TITLE Invio dei dati al punto operativo.
035300010601     C     opeini        BEGSR
035400010601     C*
035500010601     C* Inizializzo flag e contatori operativi
035600010601     C                   movel     '0'           flgGiro           1
035700010601     C                   movel     '0'           flgMulti          1
035800010601     C                   movel     '1'           flgStato          1
035900010615     C                   movel     '0'           flgOk             1
036000010601     C                   z-add     *zeros        cntNonEl         10 0
036100010601     C                   z-add     *zeros        depfil            3 0
036200010601     C                   z-add     *zeros        invfil            3 0
036300010601     C*
036400010601     C                   ENDSR
036500010601     C***
036600010601
036700010601
036800010601
036900010330
037000010330
037100000613     C     *inzsr        BEGSR
037200990910     C*
037300990910     C     *entry        plist
037400990920     C                   parm                    tivlrds
037500990921     C                   parm      wrkesito      esito
037600000724     C                   parm                    prmlit
037700000710     C                   parm                    prmfir
037800010330     C*
037900010330     C* CALCOLA LA DATA CORRENTE
038000161219     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
038100161219     C                   eval      datcor = %dec(%date() : *iso)
038200000613     C*
038300000613     C                   ENDSR
038400000613     C***
