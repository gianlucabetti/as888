000100071029      /TITLE Upload via Internet: traduzione in FIVATWWR.
000200990908     H dftactgrp(*yes)
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
001900010330
002000010330
002100010330
002200000913     C                   reset                   rrnum
002300990921     C                   reset                   esito
002400990921     C                   reset                   wrkesito
002500010601     C*
002600010601     C                   exsr      opeini
002700040301     C                   exsr      rwvat
002800010601     C*
002900010601     C                   seton                                        lr
003000010601
003100010601
003200010601
003300010601
003400010601     C*--------------------------------------------------------
003500010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
003600010601     C*--------------------------------------------------------
003700010601     C     PREELA        BEGSR
003800010601     C*
003900010601     C* SE OCCORRE SPEDIRE IN FILIALE
004000010601     C                   if        invfil <> *zeros and
004100010601     C                             flgGiro = '0'
004200010601     C*
004300010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
004400010601     C                   eval      flgGiro = '1'
004500010601     C*
004600010601     C                   endif
004700010601     C*
004800010601     C                   ENDSR
004900010601     C***
005000010601
005100010601
005200010601
005300010601     C*--------------------------------------------------------
005400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
005500010601     C*--------------------------------------------------------
005600010601     C     ENDELA        BEGSR
005700000616     C*
005800010601     C                   ENDSR
005900010601     C***
006000000613
006100010601
006200010601
006300010330     C*--------------------------------------------------------
006400040301     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
006500010330     C*--------------------------------------------------------
006600040301     C     RWVAT         BEGSR
006700010330     C*
006800010330     C                   if        not %open(tivin00r)
006900010330     C                   open      tivin00r
007000010330     C                   endif
007100040301     C                   if        not %open(fivatwwr)
007200040301     C                   open      fivatwwr
007300010330     C                   endif
007400010330     C*
007500010604     C                   clear                   低TROK
007600010604     C                   clear                   低TRMO
007700010604     C                   clear                   低TRNO
007800010330     C*
007900010330     C                   DO        *HIVAL
008000010330     C*
008100010330     C                   READ      tivin00r                               70
008200010618     C*
008300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
008400010618     C                   if        vinflg = '1'
008500010618     C                   eval      flgOk = '1'
008600010618     C                   endif
008700010618     C*
008800010330     C                   if        vindta > *blanks
008900010330     C                   add       1             rrnum
009000010330     C*
009100010601     C                   if        *in70 = *off and
009200010330     C                             (vinflg = *blanks
009300010330     C                              or vinflg = '0'
009400010330     C                              or vinflg = '2')
009500010330     C*
009600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
009700010711     C                   if        vinflg = *blanks or vinflg = '0'
009800010711     C                   clear                   vinmsg
009900010711     C                   endif
010000010601     C*
010100040301     C* Inizializzo campi d wrk e imposto i valori d default
010200010330     C                   exsr      inzvar
010300010330     C                   exsr      defcam
010400010601     C*
010500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
010600040301     C                   eval      depfil = VATLNP
010700010601     C                   exsr      repfil
010800010601     C                   if        depfil = invfil
010900021025     C                   if        vlrpoi = 999
011000040301     C                   MOVE(P)   invfil        VATFGS
011100021025     C                   else
011200040301     C                   MOVE(P)   vlrpoi        VATFGS
011300021025     C                   endif
011400050114     C                   endif
011500040301     C*
011600040301     C* Imposto e scarico il buffer del FIVAT
011700040301     C                   exsr      impvat
011800010601     C*
011900010601     C                   exsr      PREELA
012000010601     C*
012100010604     C* Ebbene...
012200010604     C*
012300010604     C  N31              ADD       1             低TROK            7 0
012400010604     C   32              ADD       1             低TRMO            7 0
012500010604     C   31              ADD       1             低TRNO            7 0
012600010604     C*
012700020722     C                   endif
012800020722     C*
012900010604     C                   if        *in31 = *off and
013000010604     C                             *in32 = *off
013100010604     C                   eval      vinflg = '1'
013200010604     C                   else
013300010604     C                   eval      vinflg = '2'
013400010604     C                   endif
013500010604     C*
013600010330     C                   else
013700010330     C                   eval      vinflg = '1'
013800010330     C                   endif
013900010601     C*
014000010601     C  N70              update    tivin000
014100010330     C*
014200010330     C  N70              ENDdo
014300010601     C
014400010601     C*
014500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
014600010601     C                   if        cntNonEl = *zeros or
014700010601     C                             flgMulti = '0'
014800010330     C* Se non ci sono record con errori ...
014900010601     C                   if        呃trno = 0 and
015000010604     C                             呃trmo = 0 and
015100010601     C                             flgStato <> '2'
015200010330     C* ... restituisco esito OK.
015300010330     C                   eval      wrkesito = '0'
015400010330     C                   else
015500010330     C                   if        呃trok > 0
015600010330     C                   eval      wrkesito = '1'
015700010330     C                   else
015800010615     C                   if        flgOk = '0'
015900010615     C                   eval      wrkesito = '2'
016000010615     C                   else
016100010615     C                   eval      wrkesito = '6'
016200010615     C                   endif
016300010330     C                   endif
016400010330     C                   endif
016500010601     C                   else
016600010601     C                   eval      wrkesito = '9'
016700010601     C                   endif
016800010330     C*
016900010330     C                   if        %open(tivin00r)
017000010330     C                   close     tivin00r
017100010330     C                   endif
017200040301     C                   if        %open(fivatwwr)
017300040301     C                   close     fivatwwr
017400010330     C                   endif
017500010601     C*
017600010601     C                   if        vlrpoi <> 999
017700010601     C                   eval      invfil = vlrpoi
017800010601     C                   endif
017900010330     C*
018000010330     C                   if        呃trok > 0
018100010601     C                             and invfil > *zeros
018200010330     C                   exsr      invio
018300010330     C                   endif
018400010601     C*
018500010618     C                   if        flgGiro = '1'
018600010601     C                   exsr      endela
018700010618     C                   endif
018800010330     C*
018900010330     C                   ENDSR
019000010330     C***
019100010330
019200010601
019300010601
019400010601
019500010601
019600010330     C*----------------------------------------------------*
019700020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019800010330     C*----------------------------------------------------*
019900010330     C     INZVAR        BEGSR
020000051219     C*
020100010330     C*
020200010330     C                   ENDSR
020300010330     C*----------------------------------------------------*
020400020722     C*  IMPOSTAZIONE CAMPI COSTANTI
020500010330     C*----------------------------------------------------*
020600010330     C     DEFCAM        BEGSR
020700010330     C*
020800020204     C* Inizializzo il buffer del record da scrivere
020900040301     C                   CLEAR                   FIVAT000
021000020204     C* Imposto i valori di default...
021100100225     C                   EVAL      VATCCM = 0860249
021200100225     C                   EVAL      VATLNP = 020
021300040301     C                   EVAL      VATTRC = 'E'
021400020204     C* ... e poi verifico se sono stati passati come parametri
021500020204     C                   IF        vlrppt > *blanks
021600040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
021700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
021800020204     C                   EXSR      CHKNUM
021900020204     C                   IF        PiInt=*on
022000040301     C                   Z-ADD     PiVal         VATCCM
022100020204     C                   ENDIF
022200040301     C                   ENDIF
022300040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
022400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022500020204     C                   EXSR      CHKNUM
022600020204     C                   IF        PiInt=*on
022700040301     C                   Z-ADD     PiVal         VATLNP
022800020204     C                   ENDIF
022900040301     C                   ENDIF
023000020204     C                   ENDIF
023100020204     C*
023200010330     C                   ENDSR
023300010607     C*----------------------------------------------------*
023400040301     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
023500010607     C*----------------------------------------------------*
023600040301     C     IMPVAT        BEGSR
023700010607     C*
023800010607     C                   SETOFF                                       3132
023900040316     C*
024000040316     C* Campi "calcolati"
024100040316     C                   MOVEL     datcor        VATAAS
024200010607     C*
024300010607     C* Reperimento campi ALFA
024400080311     C                   IF        %trim(%subst(vindta:97:15)) = *blanks
024500080311     C                   EVAL      VATNOT = %trim(%subst(vindta:21:15))
024600080311     C                   ELSE
024700080311     C                   EVAL      VATNOT = %trim(%subst(vindta:21:15))+
024800080311     C                                      %trim(%subst(vindta:97:15))
024900080311     C                   ENDIF
025000040301     C*
025100040301     C* Reperimento campi NUMERICI
025200080318     C* CCM
025300080318     C                   IF        %trim(%subst(vindta:66:6)) = 'MC300'
025400100225     C***                EVAL      VATCCM=
025500080318     C                   ENDIF
025600080318     C                   IF        %trim(%subst(vindta:66:6)) = 'MC3000'
025700100225     C***                EVAL      VATCCM=
025800080318     C                   ENDIF
025900080318     C* NSP
026000080311     C                   EVAL      PiStr=%trim(%subst(vindta:1:15))
026100040301     C                   EXSR      CHKNUM
026200040301     C                   IF        PiInt=*on
026300040301     C                   Z-ADD     PiVal         VATNSP
026400040301     C                   ELSE
026500040301     C                   SETON                                        31
026600040301     C                   Z-ADD     *zeros        VATNSP
026700040301     C                   EVAL      vinmsg = %trimr(vinmsg)
026800040301     C                             + ' ' + 'VATNSP'
026900040301     C                   ENDIF
027000040301     C*
027100040301     C* Scarico il buffer del FIVAT
027200040301     C  N31              WRITE     FIVAT000
027300020204     C*
027400010607     C                   ENDSR
027500010607     C*----------------------------------------------------*
027600020204
027700010330
027800010330
027900010330     C*----------------------------------------------------*
028000010330     C*  CONTROLLO NUMERICITA' CAMPI
028100010330     C*----------------------------------------------------*
028200010330     C     CHKNUM        BEGSR
028300010330     C*
028400010606     C                   IF        PiDecChr = *blanks
028500080311     C                   EVAL      PiDecChr = '.'
028600010606     C                   ENDIF
028700010606     C*
028800010606     C                   CALL(e)   'ISNUMERIC'
028900010330     C                   PARM                    PiStr            30
029000010606     C                   PARM                    PiDecChr          1
029100010330     C                   PARM      *ZEROS        PiVal            30 9
029200010330     C                   PARM      '0'           PiInt             1
029300010330     C                   PARM      '0'           PiNum             1
029400010330     C                   IF        %error
029500010606     C                   EVAL      PiNum=*off
029600010330     C                   ENDIF
029700010330     C*
029800010330     C                   ENDSR
029900010330     C***
030000010330
030100010601
030200010601
030300010601
030400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
030500010601     C     repfil        BEGSR
030600010601     C*
030700010601     C                   if        invfil = *zeros and
030800010601     C                             depfil > *zeros and
030900010629     C                             (vinflg = *blanks or
031000010629     C                              vinflg = *zeros)
031100010601     C
031200010601     C                   eval      invfil = depfil
031300010601     C                   endif
031400010601     C*
031500010601     C                   if        depfil <> invfil and
031600010601     C                             invfil > *zeros
031700010601     C                   eval      flgMulti = '1'
031800010601     C                   if        vinflg = *blanks
031900010601     C                   add       1             cntNonEl
032000010601     C                   endif
032100010601     C                   endif
032200010601     C*
032300010601     C                   if        vinflg = '2'
032400010601     C                   eval      flgStato = '2'
032500010601     C                   endif
032600010601     C*
032700010601     C                   ENDSR
032800010601     C***
032900010601
033000010601
033100010601
033200010330
033300010330
033400010330
033500990920      /TITLE Invio dei dati al punto operativo.
033600000613     C     invio         BEGSR
033700990920     C*
033800990920     C                   reset                   dscmz
033900010601     C                   move      invfil        cmzdst
034000990920     C                   eval      cmzfld = vlrfou
034100990920     C                   eval      cmzmbd = vlrhdl
034200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
034300000710     C                   if        prmfir = *blanks
034400040301     C                   eval      cmzfla = 'FIVAT00F'
034500040301     C                   eval      cmzmba = 'FIVAT00F'
034600000710     C                   else
034700000710     C                   eval      cmzfla = prmfir
034800000710     C                   eval      cmzmba = prmfir
034900000710     C                   endif
035000990920     C                   eval      cmznrr = *zeros
035100990920     C                   move      呃trok        cmznrr
035200021018     C                   eval      cmzlba = vlrfl1
035300990920     C                   call(e)   'TIS711C'
035400990920     C                   parm                    dscmz
035500990921     C                   parm      *blanks       esito
035600990920     C                   if        %error
035700990920     C                             or cmzerr = '1'
035800990921     C                             or esito  = '1'
035900000710     C                   eval      wrkesito = '3'
036000990920     C                   endif
036100990920     C*
036200000613     C                   ENDSR
036300990910
036400010601
036500010601
036600010601
036700010601
036800010601      /TITLE Invio dei dati al punto operativo.
036900010601     C     opeini        BEGSR
037000010601     C*
037100010601     C* Inizializzo flag e contatori operativi
037200010601     C                   movel     '0'           flgGiro           1
037300010601     C                   movel     '0'           flgMulti          1
037400010601     C                   movel     '1'           flgStato          1
037500010615     C                   movel     '0'           flgOk             1
037600010601     C                   z-add     *zeros        cntNonEl         10 0
037700010601     C                   z-add     *zeros        depfil            3 0
037800010601     C                   z-add     *zeros        invfil            3 0
037900010601     C*
038000010601     C                   ENDSR
038100010601     C***
038200010601
038300010601
038400010601
038500010330
038600010330
038700000613     C     *inzsr        BEGSR
038800990910     C*
038900990910     C     *entry        plist
039000990920     C                   parm                    tivlrds
039100990921     C                   parm      wrkesito      esito
039200000724     C                   parm                    prmlit
039300000710     C                   parm                    prmfir
039400010330     C*
039500010330     C* CALCOLA LA DATA CORRENTE
039600100225     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
039700100225     C                   eval      datcor = %dec(%date() : *ISO)
039800000613     C*
039900000613     C                   ENDSR
040000000613     C***
