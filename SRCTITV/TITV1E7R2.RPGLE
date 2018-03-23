000100071029      /TITLE Upload via Internet: traduzione in FIVATWWR.
000200121218     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
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
002800010330
002900010330
003000010330
003100000913     C                   reset                   rrnum
003200990921     C                   reset                   esito
003300990921     C                   reset                   wrkesito
003400010601     C*
003500010601     C                   exsr      opeini
003600040301     C                   exsr      rwvat
003700010601     C*
003800010601     C                   seton                                        lr
003900010601
004000010601
004100010601
004200010601
004300010601     C*--------------------------------------------------------
004400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
004500010601     C*--------------------------------------------------------
004600010601     C     PREELA        BEGSR
004700010601     C*
004800010601     C* SE OCCORRE SPEDIRE IN FILIALE
004900010601     C                   if        invfil <> *zeros and
005000010601     C                             flgGiro = '0'
005100010601     C*
005200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005300010601     C                   eval      flgGiro = '1'
005400010601     C*
005500010601     C                   endif
005600010601     C*
005700010601     C                   ENDSR
005800010601     C***
005900010601
006000010601
006100010601
006200010601     C*--------------------------------------------------------
006300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
006400010601     C*--------------------------------------------------------
006500010601     C     ENDELA        BEGSR
006600000616     C*
006700010601     C                   ENDSR
006800010601     C***
006900000613
007000010601
007100010601
007200010330     C*--------------------------------------------------------
007300040301     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
007400010330     C*--------------------------------------------------------
007500040301     C     RWVAT         BEGSR
007600010330     C*
007700010330     C                   if        not %open(tivin00r)
007800010330     C                   open      tivin00r
007900010330     C                   endif
008000040301     C                   if        not %open(fivatwwr)
008100040301     C                   open      fivatwwr
008200010330     C                   endif
008300010330     C*
008400010604     C                   clear                   低TROK
008500010604     C                   clear                   低TRMO
008600010604     C                   clear                   低TRNO
008700010330     C*
008800010330     C                   DO        *HIVAL
008900010330     C*
009000010330     C                   READ      tivin00r                               70
009100010618     C*
009200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
009300010618     C                   if        vinflg = '1'
009400010618     C                   eval      flgOk = '1'
009500010618     C                   endif
009600010618     C*
009700010330     C                   if        vindta > *blanks
009800010330     C                   add       1             rrnum
009900010330     C*
010000010601     C                   if        *in70 = *off and
010100010330     C                             (vinflg = *blanks
010200010330     C                              or vinflg = '0'
010300010330     C                              or vinflg = '2')
010400010330     C*
010500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
010600010711     C                   if        vinflg = *blanks or vinflg = '0'
010700010711     C                   clear                   vinmsg
010800010711     C                   endif
010900010601     C*
011000040301     C* Inizializzo campi d wrk e imposto i valori d default
011100010330     C                   exsr      inzvar
011200010330     C                   exsr      defcam
011300010601     C*
011400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
011500040301     C                   eval      depfil = VATLNP
011600010601     C                   exsr      repfil
011700010601     C                   if        depfil = invfil
011800021025     C                   if        vlrpoi = 999
011900040301     C                   MOVE(P)   invfil        VATFGS
012000021025     C                   else
012100040301     C                   MOVE(P)   vlrpoi        VATFGS
012200021025     C                   endif
012300050114     C                   endif
012400040301     C*
012500040301     C* Imposto e scarico il buffer del FIVAT
012600040301     C                   exsr      impvat
012700010601     C*
012800010601     C                   exsr      PREELA
012900010601     C*
013000010604     C* Ebbene...
013100010604     C*
013200010604     C  N31              ADD       1             低TROK            7 0
013300010604     C   32              ADD       1             低TRMO            7 0
013400010604     C   31              ADD       1             低TRNO            7 0
013500010604     C*
013600020722     C                   endif
013700020722     C*
013800010604     C                   if        *in31 = *off and
013900010604     C                             *in32 = *off
014000010604     C                   eval      vinflg = '1'
014100010604     C                   else
014200010604     C                   eval      vinflg = '2'
014300010604     C                   endif
014400010604     C*
014500010330     C                   else
014600010330     C                   eval      vinflg = '1'
014700010330     C                   endif
014800010601     C*
014900010601     C  N70              update    tivin000
015000010330     C*
015100010330     C  N70              ENDdo
015200010601     C
015300010601     C*
015400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
015500010601     C                   if        cntNonEl = *zeros or
015600010601     C                             flgMulti = '0'
015700010330     C* Se non ci sono record con errori ...
015800010601     C                   if        呃trno = 0 and
015900010604     C                             呃trmo = 0 and
016000010601     C                             flgStato <> '2'
016100010330     C* ... restituisco esito OK.
016200010330     C                   eval      wrkesito = '0'
016300010330     C                   else
016400010330     C                   if        呃trok > 0
016500010330     C                   eval      wrkesito = '1'
016600010330     C                   else
016700010615     C                   if        flgOk = '0'
016800010615     C                   eval      wrkesito = '2'
016900010615     C                   else
017000010615     C                   eval      wrkesito = '6'
017100010615     C                   endif
017200010330     C                   endif
017300010330     C                   endif
017400010601     C                   else
017500010601     C                   eval      wrkesito = '9'
017600010601     C                   endif
017700010330     C*
017800010330     C                   if        %open(tivin00r)
017900010330     C                   close     tivin00r
018000010330     C                   endif
018100040301     C                   if        %open(fivatwwr)
018200040301     C                   close     fivatwwr
018300010330     C                   endif
018400010601     C*
018500010601     C                   if        vlrpoi <> 999
018600010601     C                   eval      invfil = vlrpoi
018700010601     C                   endif
018800010330     C*
018900010330     C                   if        呃trok > 0
019000010601     C                             and invfil > *zeros
019100010330     C                   exsr      invio
019200010330     C                   endif
019300010601     C*
019400010618     C                   if        flgGiro = '1'
019500010601     C                   exsr      endela
019600010618     C                   endif
019700010330     C*
019800010330     C                   ENDSR
019900010330     C***
020000010330
020100010601
020200010601
020300010601
020400010601
020500010330     C*----------------------------------------------------*
020600020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020700010330     C*----------------------------------------------------*
020800010330     C     INZVAR        BEGSR
020900051219     C*
021000010330     C*
021100010330     C                   ENDSR
021200010330     C*----------------------------------------------------*
021300020722     C*  IMPOSTAZIONE CAMPI COSTANTI
021400010330     C*----------------------------------------------------*
021500010330     C     DEFCAM        BEGSR
021600010330     C*
021700020204     C* Inizializzo il buffer del record da scrivere
021800040301     C                   CLEAR                   FIVAT000
021900020204     C* Imposto i valori di default...
022000130118     C                   EVAL      VATCCM = 0661908
022100071030     C                   EVAL      VATLNP = 001
022200040301     C                   EVAL      VATTRC = 'E'
022300020204     C* ... e poi verifico se sono stati passati come parametri
022400020204     C                   IF        vlrppt > *blanks
022500040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
022600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
022700020204     C                   EXSR      CHKNUM
022800020204     C                   IF        PiInt=*on
022900040301     C                   Z-ADD     PiVal         VATCCM
023000020204     C                   ENDIF
023100040301     C                   ENDIF
023200040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
023300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023400020204     C                   EXSR      CHKNUM
023500020204     C                   IF        PiInt=*on
023600040301     C                   Z-ADD     PiVal         VATLNP
023700020204     C                   ENDIF
023800040301     C                   ENDIF
023900020204     C                   ENDIF
024000020204     C*
024100010330     C                   ENDSR
024200010607     C*----------------------------------------------------*
024300040301     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
024400010607     C*----------------------------------------------------*
024500040301     C     IMPVAT        BEGSR
024600010607     C*
024700010607     C                   SETOFF                                       3132
024800040316     C*
024900040316     C* Campi "calcolati"
025000040316     C                   MOVEL     datcor        VATAAS
025100010607     C*
025200010607     C* Reperimento campi ALFA
025300080311     C                   IF        %trim(%subst(vindta:97:15)) = *blanks
025400080311     C                   EVAL      VATNOT = %trim(%subst(vindta:21:15))
025500080311     C                   ELSE
025600080311     C                   EVAL      VATNOT = %trim(%subst(vindta:21:15))+
025700080311     C                                      %trim(%subst(vindta:97:15))
025800080311     C                   ENDIF
025900040301     C*
026000040301     C* Reperimento campi NUMERICI
026100080318     C* CCM
026200080318     C                   IF        %trim(%subst(vindta:66:6)) = 'MC300'
026300130118     C                   EVAL      VATCCM=0661908
026400080318     C                   ENDIF
026500080318     C                   IF        %trim(%subst(vindta:66:6)) = 'MC3000'
026600130118     C                   EVAL      VATCCM=0661909
026700080318     C                   ENDIF
026800121207     C                   IF        %trim(%subst(vindta:66:6)) = 'RE5775'
026900121207     C                   EVAL      VATCCM=2494836
027000121207     C                   ENDIF
027100121218     C                   IF        %trim(%subst(vindta:66:6)) = 'AD000'
027200130118     C                   EVAL      VATCCM=0661912
027300121218     C                   ENDIF
027400121218     C                   IF        %trim(%subst(vindta:66:6)) = 'AD0000'
027500130118     C                   EVAL      VATCCM=0661913
027600121218     C                   ENDIF
027700121218     C                   IF        %trim(%subst(vindta:66:6)) = 'ES070'
027800130118     C                   EVAL      VATCCM=0661914
027900121218     C                   ENDIF
028000121218     C                   IF        %trim(%subst(vindta:66:6)) = 'ES0700'
028100130118     C                   EVAL      VATCCM=0661915
028200121218     C                   ENDIF
028300121218     C                   IF        %trim(%subst(vindta:66:6)) = 'ES380'
028400130118     C                   EVAL      VATCCM=0661916
028500121218     C                   ENDIF
028600121218     C                   IF        %trim(%subst(vindta:66:6)) = 'ES3800'
028700130118     C                   EVAL      VATCCM=0661917
028800121218     C                   ENDIF
028900121218     C                   IF        %trim(%subst(vindta:66:6)) = 'ES160'
029000130118     C                   EVAL      VATCCM=0661918
029100121218     C                   ENDIF
029200121218     C                   IF        %trim(%subst(vindta:66:6)) = 'ES1600'
029300130118     C                   EVAL      VATCCM=0661919
029400121218     C                   ENDIF
029500121218     C                   IF        %trim(%subst(vindta:66:6)) = 'PT000'
029600130118     C                   EVAL      VATCCM=0661920
029700121218     C                   ENDIF
029800121218     C                   IF        %trim(%subst(vindta:66:6)) = 'PT0000'
029900130118     C                   EVAL      VATCCM=0661921
030000121218     C                   ENDIF
030100121218     C                   IF        %trim(%subst(vindta:66:6)) = 'ES000'
030200130118     C                   EVAL      VATCCM=0661922
030300121218     C                   ENDIF
030400121218     C                   IF        %trim(%subst(vindta:66:6)) = 'ES0000'
030500130118     C                   EVAL      VATCCM=0661923
030600121218     C                   ENDIF
030700080318     C* NSP
030800080311     C                   EVAL      PiStr=%trim(%subst(vindta:1:15))
030900040301     C                   EXSR      CHKNUM
031000040301     C                   IF        PiInt=*on
031100040301     C                   Z-ADD     PiVal         VATNSP
031200040301     C                   ELSE
031300040301     C                   SETON                                        31
031400040301     C                   Z-ADD     *zeros        VATNSP
031500040301     C                   EVAL      vinmsg = %trimr(vinmsg)
031600040301     C                             + ' ' + 'VATNSP'
031700040301     C                   ENDIF
031800040301     C*
031900040301     C* Scarico il buffer del FIVAT
032000040301     C  N31              WRITE     FIVAT000
032100020204     C*
032200010607     C                   ENDSR
032300010607     C*----------------------------------------------------*
032400020204
032500010330
032600010330
032700010330     C*----------------------------------------------------*
032800010330     C*  CONTROLLO NUMERICITA' CAMPI
032900010330     C*----------------------------------------------------*
033000010330     C     CHKNUM        BEGSR
033100010330     C*
033200010606     C                   IF        PiDecChr = *blanks
033300080311     C                   EVAL      PiDecChr = '.'
033400010606     C                   ENDIF
033500010606     C*
033600010606     C                   CALL(e)   'ISNUMERIC'
033700010330     C                   PARM                    PiStr            30
033800010606     C                   PARM                    PiDecChr          1
033900010330     C                   PARM      *ZEROS        PiVal            30 9
034000010330     C                   PARM      '0'           PiInt             1
034100010330     C                   PARM      '0'           PiNum             1
034200010330     C                   IF        %error
034300010606     C                   EVAL      PiNum=*off
034400010330     C                   ENDIF
034500010330     C*
034600010330     C                   ENDSR
034700010330     C***
034800010330
034900010601
035000010601
035100010601
035200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
035300010601     C     repfil        BEGSR
035400010601     C*
035500010601     C                   if        invfil = *zeros and
035600010601     C                             depfil > *zeros and
035700010629     C                             (vinflg = *blanks or
035800010629     C                              vinflg = *zeros)
035900010601     C
036000010601     C                   eval      invfil = depfil
036100010601     C                   endif
036200010601     C*
036300010601     C                   if        depfil <> invfil and
036400010601     C                             invfil > *zeros
036500010601     C                   eval      flgMulti = '1'
036600010601     C                   if        vinflg = *blanks
036700010601     C                   add       1             cntNonEl
036800010601     C                   endif
036900010601     C                   endif
037000010601     C*
037100010601     C                   if        vinflg = '2'
037200010601     C                   eval      flgStato = '2'
037300010601     C                   endif
037400010601     C*
037500010601     C                   ENDSR
037600010601     C***
037700010601
037800010601
037900010601
038000010330
038100010330
038200010330
038300990920      /TITLE Invio dei dati al punto operativo.
038400000613     C     invio         BEGSR
038500990920     C*
038600990920     C                   reset                   dscmz
038700010601     C                   move      invfil        cmzdst
038800990920     C                   eval      cmzfld = vlrfou
038900990920     C                   eval      cmzmbd = vlrhdl
039000990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
039100000710     C                   if        prmfir = *blanks
039200040301     C                   eval      cmzfla = 'FIVAT00F'
039300040301     C                   eval      cmzmba = 'FIVAT00F'
039400000710     C                   else
039500000710     C                   eval      cmzfla = prmfir
039600000710     C                   eval      cmzmba = prmfir
039700000710     C                   endif
039800990920     C                   eval      cmznrr = *zeros
039900990920     C                   move      呃trok        cmznrr
040000021018     C                   eval      cmzlba = vlrfl1
040100990920     C                   call(e)   'TIS711C'
040200990920     C                   parm                    dscmz
040300990921     C                   parm      *blanks       esito
040400990920     C                   if        %error
040500990920     C                             or cmzerr = '1'
040600990921     C                             or esito  = '1'
040700000710     C                   eval      wrkesito = '3'
040800990920     C                   endif
040900990920     C*
041000000613     C                   ENDSR
041100990910
041200010601
041300010601
041400010601
041500010601
041600010601      /TITLE Invio dei dati al punto operativo.
041700010601     C     opeini        BEGSR
041800010601     C*
041900010601     C* Inizializzo flag e contatori operativi
042000010601     C                   movel     '0'           flgGiro           1
042100010601     C                   movel     '0'           flgMulti          1
042200010601     C                   movel     '1'           flgStato          1
042300010615     C                   movel     '0'           flgOk             1
042400010601     C                   z-add     *zeros        cntNonEl         10 0
042500010601     C                   z-add     *zeros        depfil            3 0
042600010601     C                   z-add     *zeros        invfil            3 0
042700010601     C*
042800010601     C                   ENDSR
042900010601     C***
043000010601
043100010601
043200010601
043300010330
043400010330
043500000613     C     *inzsr        BEGSR
043600990910     C*
043700990910     C     *entry        plist
043800990920     C                   parm                    tivlrds
043900990921     C                   parm      wrkesito      esito
044000000724     C                   parm                    prmlit
044100000710     C                   parm                    prmfir
044200010330     C*
044300010330     C* CALCOLA LA DATA CORRENTE
044400010330     C                   time                    wn14             14 0
044500010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
044600010330     C                   z-add     wn8           g08dat
044700010330     C                   z-add     *zeros        g08inv
044800010330     C                   movel     '0'           g08err
044900010330     C                   call      'XSRDA8'
045000010330     C                   parm                    wlbda8
045100010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
045200000613     C*
045300000613     C                   ENDSR
045400000613     C***
