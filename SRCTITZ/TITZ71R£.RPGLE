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
001800010330     D*------------------
001900010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002000010330     D*------------------
002100010330     D WLBDA8          DS                  INZ
002200010330     D  G08DAT                 1      8  0
002300010330     D  G08INV                 9     16  0
002400010330     D  G08ERR                17     17
002500010330     D  G08TGI                18     22  0
002600000313     D*
002700010330
002800010330
002900010330
003000000913     C                   reset                   rrnum
003100990921     C                   reset                   esito
003200990921     C                   reset                   wrkesito
003300010601     C*
003400010601     C                   exsr      opeini
003500040301     C                   exsr      rwvat
003600010601     C*
003700010601     C                   seton                                        lr
003800010601
003900010601
004000010601
004100010601
004200010601     C*--------------------------------------------------------
004300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
004400010601     C*--------------------------------------------------------
004500010601     C     PREELA        BEGSR
004600010601     C*
004700010601     C* SE OCCORRE SPEDIRE IN FILIALE
004800010601     C                   if        invfil <> *zeros and
004900010601     C                             flgGiro = '0'
005000010601     C*
005100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005200010601     C                   eval      flgGiro = '1'
005300010601     C*
005400010601     C                   endif
005500010601     C*
005600010601     C                   ENDSR
005700010601     C***
005800010601
005900010601
006000010601
006100010601     C*--------------------------------------------------------
006200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
006300010601     C*--------------------------------------------------------
006400010601     C     ENDELA        BEGSR
006500990915     C*
006600000616     C*
006700010601     C                   ENDSR
006800010601     C***
006900010601
007000010330     C*--------------------------------------------------------
007100040301     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
007200010330     C*--------------------------------------------------------
007300040301     C     RWVAT         BEGSR
007400010330     C*
007500010330     C                   if        not %open(tivin00r)
007600010330     C                   open      tivin00r
007700010330     C                   endif
007800040301     C                   if        not %open(fivatwwr)
007900040301     C                   open      fivatwwr
008000010330     C                   endif
008100010330     C*
008200010604     C                   clear                   低TROK
008300010604     C                   clear                   低TRMO
008400010604     C                   clear                   低TRNO
008500010330     C*
008600010330     C                   DO        *HIVAL
008700010330     C*
008800010330     C                   READ      tivin00r                               70
008900010618     C*
009000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
009100010618     C                   if        vinflg = '1'
009200010618     C                   eval      flgOk = '1'
009300010618     C                   endif
009400010618     C*
009500010330     C                   if        vindta > *blanks
009600010330     C                   add       1             rrnum
009700010330     C*
009800010601     C                   if        *in70 = *off and
009900010330     C                             (vinflg = *blanks
010000010330     C                              or vinflg = '0'
010100010330     C                              or vinflg = '2')
010200010330     C*
010300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
010400010711     C                   if        vinflg = *blanks or vinflg = '0'
010500010711     C                   clear                   vinmsg
010600010711     C                   endif
010700010601     C*
010800040301     C* Inizializzo campi d wrk e imposto i valori d default
010900010330     C                   exsr      inzvar
011000010330     C                   exsr      defcam
011100010601     C*
011200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
011300040301     C                   eval      depfil = VATLNP
011400010601     C                   exsr      repfil
011500010601     C                   if        depfil = invfil
011600021025     C                   if        vlrpoi = 999
011700040301     C                   MOVE(P)   invfil        VATFGS
011800021025     C                   else
011900040301     C                   MOVE(P)   vlrpoi        VATFGS
012000021025     C                   endif
012100050114     C                   endif
012200040301     C*
012300040301     C* Imposto e scarico il buffer del FIVAT
012400040301     C                   exsr      impvat
012500010601     C*
012600010601     C                   exsr      PREELA
012700010601     C*
012800010604     C* Ebbene...
012900010604     C*
013000010604     C  N31              ADD       1             低TROK            7 0
013100010604     C   32              ADD       1             低TRMO            7 0
013200010604     C   31              ADD       1             低TRNO            7 0
013300010604     C*
013400020722     C                   endif
013500020722     C*
013600010604     C                   if        *in31 = *off and
013700010604     C                             *in32 = *off
013800010604     C                   eval      vinflg = '1'
013900010604     C                   else
014000010604     C                   eval      vinflg = '2'
014100010604     C                   endif
014200010604     C*
014300010330     C                   else
014400010330     C                   eval      vinflg = '1'
014500010330     C                   endif
014600010601     C*
014700010601     C  N70              update    tivin000
014800010330     C*
014900010330     C  N70              ENDdo
015000010601     C
015100010601     C*
015200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
015300010601     C                   if        cntNonEl = *zeros or
015400010601     C                             flgMulti = '0'
015500010330     C* Se non ci sono record con errori ...
015600010601     C                   if        呃trno = 0 and
015700010604     C                             呃trmo = 0 and
015800010601     C                             flgStato <> '2'
015900010330     C* ... restituisco esito OK.
016000010330     C                   eval      wrkesito = '0'
016100010330     C                   else
016200010330     C                   if        呃trok > 0
016300010330     C                   eval      wrkesito = '1'
016400010330     C                   else
016500010615     C                   if        flgOk = '0'
016600010615     C                   eval      wrkesito = '2'
016700010615     C                   else
016800010615     C                   eval      wrkesito = '6'
016900010615     C                   endif
017000010330     C                   endif
017100010330     C                   endif
017200010601     C                   else
017300010601     C                   eval      wrkesito = '9'
017400010601     C                   endif
017500010330     C*
017600010330     C                   if        %open(tivin00r)
017700010330     C                   close     tivin00r
017800010330     C                   endif
017900040301     C                   if        %open(fivatwwr)
018000040301     C                   close     fivatwwr
018100010330     C                   endif
018200010601     C*
018300010601     C                   if        vlrpoi <> 999
018400010601     C                   eval      invfil = vlrpoi
018500010601     C                   endif
018600010330     C*
018700010330     C                   if        呃trok > 0
018800010601     C                             and invfil > *zeros
018900010330     C                   exsr      invio
019000010330     C                   endif
019100010601     C*
019200010618     C                   if        flgGiro = '1'
019300010601     C                   exsr      endela
019400010618     C                   endif
019500010330     C*
019600010330     C                   ENDSR
019700010330     C***
019800010330
019900010601
020000010601
020100010601
020200010601
020300010330     C*----------------------------------------------------*
020400020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020500010330     C*----------------------------------------------------*
020600010330     C     INZVAR        BEGSR
020700051219     C*
020800010330     C*
020900010330     C                   ENDSR
021000010330     C*----------------------------------------------------*
021100020722     C*  IMPOSTAZIONE CAMPI COSTANTI
021200010330     C*----------------------------------------------------*
021300010330     C     DEFCAM        BEGSR
021400010330     C*
021500020204     C* Inizializzo il buffer del record da scrivere
021600040301     C                   CLEAR                   FIVAT000
021700020204     C* Imposto i valori di default...
021800121024     C                   EVAL      VATCCM = 0941197
021900121024     C                   EVAL      VATLNP = 094
022000040301     C                   EVAL      VATTRC = 'E'
022100020204     C* ... e poi verifico se sono stati passati come parametri
022200020204     C                   IF        vlrppt > *blanks
022300040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
022400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
022500020204     C                   EXSR      CHKNUM
022600020204     C                   IF        PiInt=*on
022700040301     C                   Z-ADD     PiVal         VATCCM
022800020204     C                   ENDIF
022900040301     C                   ENDIF
023000040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
023100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023200020204     C                   EXSR      CHKNUM
023300020204     C                   IF        PiInt=*on
023400040301     C                   Z-ADD     PiVal         VATLNP
023500020204     C                   ENDIF
023600040301     C                   ENDIF
023700020204     C                   ENDIF
023800020204     C*
023900010330     C                   ENDSR
024000010607     C*----------------------------------------------------*
024100040301     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
024200010607     C*----------------------------------------------------*
024300040301     C     IMPVAT        BEGSR
024400010607     C*
024500010607     C                   SETOFF                                       3132
024600040316     C*
024700040316     C* Campi "calcolati"
024800121024     C*
024900121024     C* AAS (deve essere preso da loro)
025000121024     C                   EVAL      PiStr=%trim(%subst(vindta:11:4))
025100121024     C                   EXSR      CHKNUM
025200121024     C                   IF        PiInt=*on
025300121024     C                   Z-ADD     PiVal         VATAAS
025400121024     C                   ELSE
025500121024     C                   SETON                                        31
025600121024     C                   Z-ADD     *zeros        VATAAS
025700121024     C                   EVAL      vinmsg = %trimr(vinmsg)
025800121024     C                             + ' ' + 'VATAAS'
025900121024     C                   ENDIF
026000010607     C*
026100010607     C* Reperimento campi ALFA
026200121024     C                   EVAL      VATNOT =
026300121024     C                                      %trim(%subst(vindta:19:19))
026400040301     C*
026500040301     C* Reperimento campi NUMERICI
026600121024     C                   EVAL      PiStr=%trim(%subst(vindta:1:10))
026700040301     C                   EXSR      CHKNUM
026800040301     C                   IF        PiInt=*on
026900040301     C                   Z-ADD     PiVal         VATNSP
027000040301     C                   ELSE
027100040301     C                   SETON                                        31
027200040301     C                   Z-ADD     *zeros        VATNSP
027300040301     C                   EVAL      vinmsg = %trimr(vinmsg)
027400040301     C                             + ' ' + 'VATNSP'
027500040301     C                   ENDIF
027600040301     C*
027700040301     C* Scarico il buffer del FIVAT
027800040301     C  N31              WRITE     FIVAT000
027900020204     C*
028000010607     C                   ENDSR
028100010607     C*----------------------------------------------------*
028200020204
028300010330
028400010330
028500010330     C*----------------------------------------------------*
028600010330     C*  CONTROLLO NUMERICITA' CAMPI
028700010330     C*----------------------------------------------------*
028800010330     C     CHKNUM        BEGSR
028900010330     C*
029000010606     C                   IF        PiDecChr = *blanks
029100010606     C                   EVAL      PiDecChr = ','
029200010606     C                   ENDIF
029300010606     C*
029400010606     C                   CALL(e)   'ISNUMERIC'
029500010330     C                   PARM                    PiStr            30
029600010606     C                   PARM                    PiDecChr          1
029700010330     C                   PARM      *ZEROS        PiVal            30 9
029800010330     C                   PARM      '0'           PiInt             1
029900010330     C                   PARM      '0'           PiNum             1
030000010330     C                   IF        %error
030100010606     C                   EVAL      PiNum=*off
030200010330     C                   ENDIF
030300010330     C*
030400010330     C                   ENDSR
030500010330     C***
030600010330
030700010601
030800010601
030900010601
031000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
031100010601     C     repfil        BEGSR
031200010601     C*
031300010601     C                   if        invfil = *zeros and
031400010601     C                             depfil > *zeros and
031500010629     C                             (vinflg = *blanks or
031600010629     C                              vinflg = *zeros)
031700010601     C
031800010601     C                   eval      invfil = depfil
031900010601     C                   endif
032000010601     C*
032100010601     C                   if        depfil <> invfil and
032200010601     C                             invfil > *zeros
032300010601     C                   eval      flgMulti = '1'
032400010601     C                   if        vinflg = *blanks
032500010601     C                   add       1             cntNonEl
032600010601     C                   endif
032700010601     C                   endif
032800010601     C*
032900010601     C                   if        vinflg = '2'
033000010601     C                   eval      flgStato = '2'
033100010601     C                   endif
033200010601     C*
033300010601     C                   ENDSR
033400010601     C***
033500010601
033600010601
033700010601
033800010330
033900010330
034000010330
034100990920      /TITLE Invio dei dati al punto operativo.
034200000613     C     invio         BEGSR
034300990920     C*
034400990920     C                   reset                   dscmz
034500010601     C                   move      invfil        cmzdst
034600990920     C                   eval      cmzfld = vlrfou
034700990920     C                   eval      cmzmbd = vlrhdl
034800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
034900000710     C                   if        prmfir = *blanks
035000040301     C                   eval      cmzfla = 'FIVAT00F'
035100040301     C                   eval      cmzmba = 'FIVAT00F'
035200000710     C                   else
035300000710     C                   eval      cmzfla = prmfir
035400000710     C                   eval      cmzmba = prmfir
035500000710     C                   endif
035600990920     C                   eval      cmznrr = *zeros
035700990920     C                   move      呃trok        cmznrr
035800021018     C                   eval      cmzlba = vlrfl1
035900990920     C                   call(e)   'TIS711C'
036000990920     C                   parm                    dscmz
036100990921     C                   parm      *blanks       esito
036200990920     C                   if        %error
036300990920     C                             or cmzerr = '1'
036400990921     C                             or esito  = '1'
036500000710     C                   eval      wrkesito = '3'
036600990920     C                   endif
036700990920     C*
036800000613     C                   ENDSR
036900990910
037000010601
037100010601
037200010601
037300010601
037400010601      /TITLE Invio dei dati al punto operativo.
037500010601     C     opeini        BEGSR
037600010601     C*
037700010601     C* Inizializzo flag e contatori operativi
037800010601     C                   movel     '0'           flgGiro           1
037900010601     C                   movel     '0'           flgMulti          1
038000010601     C                   movel     '1'           flgStato          1
038100010615     C                   movel     '0'           flgOk             1
038200010601     C                   z-add     *zeros        cntNonEl         10 0
038300010601     C                   z-add     *zeros        depfil            3 0
038400010601     C                   z-add     *zeros        invfil            3 0
038500010601     C*
038600010601     C                   ENDSR
038700010601     C***
038800010601
038900010601
039000010601
039100010330
039200010330
039300000613     C     *inzsr        BEGSR
039400990910     C*
039500990910     C     *entry        plist
039600990920     C                   parm                    tivlrds
039700990921     C                   parm      wrkesito      esito
039800000724     C                   parm                    prmlit
039900000710     C                   parm                    prmfir
040000010330     C*
040100010330     C* CALCOLA LA DATA CORRENTE
040200010330     C                   time                    wn14             14 0
040300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
040400010330     C                   z-add     wn8           g08dat
040500010330     C                   z-add     *zeros        g08inv
040600010330     C                   movel     '0'           g08err
040700010330     C                   call      'XSRDA8'
040800010330     C                   parm                    wlbda8
040900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
041000000613     C*
041100000613     C                   ENDSR
041200000613     C***
