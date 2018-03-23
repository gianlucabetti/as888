000100080410      /TITLE Upload via Internet: traduzione in FIVATWWR.
000200140203      *
000300140203      * PARTICOLARITA':
000400140203      *
000500140203     H dftactgrp(*no) actgrp(*caller)
000600000313     F*
000700990910     Ftivin00r  uF   E             DISK    usropn
000800040301     FFIVATwwr  O    E             DISK    usropn
000900000313     D*
001000010330     D*----------------------------------------------------
001100010330     D* DICHIARAZIOINE VARIABILI DI WRK
001200010330     D*----------------------------------------------------
001300010330     D dscmz         e ds                  inz
001400010330     D psds           sds
001500010330     D  procname         *PROC
001600010330     D tivlrds       e ds                  extname(tivlr00f)
001700010330     D esito           s              1
001800010330     D prmlit          s             10
001900010330     D prmfir          s             10
002000010330     D wrkesito        s                   like(esito)
002100010330     D rrnum           s              6  0 INZ(*zeros)
002200010330     D*------------------
002300010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002400010330     D*------------------
002500010330     D WLBDA8          DS                  INZ
002600010330     D  G08DAT                 1      8  0
002700010330     D  G08INV                 9     16  0
002800010330     D  G08ERR                17     17
002900010330     D  G08TGI                18     22  0
003000000313     D*
003100010330
003200010330
003300010330
003400000913     C                   reset                   rrnum
003500990921     C                   reset                   esito
003600990921     C                   reset                   wrkesito
003700010601     C*
003800010601     C                   exsr      opeini
003900040301     C                   exsr      rwvat
004000010601     C*
004100010601     C                   seton                                        lr
004200010601
004300010601
004400010601
004500010601
004600010601     C*--------------------------------------------------------
004700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
004800010601     C*--------------------------------------------------------
004900010601     C     PREELA        BEGSR
005000010601     C*
005100010601     C* SE OCCORRE SPEDIRE IN FILIALE
005200010601     C                   if        invfil <> *zeros and
005300010601     C                             flgGiro = '0'
005400010601     C*
005500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005600010601     C                   eval      flgGiro = '1'
005700010601     C*
005800010601     C                   endif
005900010601     C*
006000010601     C                   ENDSR
006100010601     C***
006200010601
006300010601
006400010601
006500010601     C*--------------------------------------------------------
006600010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
006700010601     C*--------------------------------------------------------
006800010601     C     ENDELA        BEGSR
006900000616     C*
007000010601     C                   ENDSR
007100010601     C***
007200000613
007300010601
007400010601
007500010330     C*--------------------------------------------------------
007600040301     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
007700010330     C*--------------------------------------------------------
007800040301     C     RWVAT         BEGSR
007900010330     C*
008000010330     C                   if        not %open(tivin00r)
008100010330     C                   open      tivin00r
008200010330     C                   endif
008300040301     C                   if        not %open(fivatwwr)
008400040301     C                   open      fivatwwr
008500010330     C                   endif
008600010330     C*
008700010604     C                   clear                   低TROK
008800010604     C                   clear                   低TRMO
008900010604     C                   clear                   低TRNO
009000010330     C*
009100010330     C                   DO        *HIVAL
009200010330     C*
009300010330     C                   READ      tivin00r                               70
009400010618     C*
009500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
009600010618     C                   if        vinflg = '1'
009700010618     C                   eval      flgOk = '1'
009800010618     C                   endif
009900010618     C*
010000010330     C                   if        vindta > *blanks
010100010330     C                   add       1             rrnum
010200010330     C*
010300010601     C                   if        *in70 = *off and
010400010330     C                             (vinflg = *blanks
010500010330     C                              or vinflg = '0'
010600010330     C                              or vinflg = '2')
010700010330     C*
010800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
010900010711     C                   if        vinflg = *blanks or vinflg = '0'
011000010711     C                   clear                   vinmsg
011100010711     C                   endif
011200010601     C*
011300040301     C* Inizializzo campi d wrk e imposto i valori d default
011400010330     C                   exsr      inzvar
011500010330     C                   exsr      defcam
011600010601     C*
011700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
011800040301     C                   eval      depfil = VATLNP
011900010601     C                   exsr      repfil
012000010601     C                   if        depfil = invfil
012100021025     C                   if        vlrpoi = 999
012200040301     C                   MOVE(P)   invfil        VATFGS
012300021025     C                   else
012400040301     C                   MOVE(P)   vlrpoi        VATFGS
012500021025     C                   endif
012600050114     C                   endif
012700040301     C*
012800040301     C* Imposto e scarico il buffer del FIVAT
012900040301     C                   exsr      impvat
013000010601     C*
013100010601     C                   exsr      PREELA
013200010601     C*
013300010604     C* Ebbene...
013400010604     C*
013500010604     C  N31              ADD       1             低TROK            7 0
013600010604     C   32              ADD       1             低TRMO            7 0
013700010604     C   31              ADD       1             低TRNO            7 0
013800010604     C*
013900020722     C                   endif
014000020722     C*
014100010604     C                   if        *in31 = *off and
014200010604     C                             *in32 = *off
014300010604     C                   eval      vinflg = '1'
014400010604     C                   else
014500010604     C                   eval      vinflg = '2'
014600010604     C                   endif
014700010604     C*
014800010330     C                   else
014900010330     C                   eval      vinflg = '1'
015000010330     C                   endif
015100010601     C*
015200010601     C  N70              update    tivin000
015300010330     C*
015400010330     C  N70              ENDdo
015500010601     C
015600010601     C*
015700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
015800010601     C                   if        cntNonEl = *zeros or
015900010601     C                             flgMulti = '0'
016000010330     C* Se non ci sono record con errori ...
016100010601     C                   if        呃trno = 0 and
016200010604     C                             呃trmo = 0 and
016300010601     C                             flgStato <> '2'
016400010330     C* ... restituisco esito OK.
016500010330     C                   eval      wrkesito = '0'
016600010330     C                   else
016700010330     C                   if        呃trok > 0
016800010330     C                   eval      wrkesito = '1'
016900010330     C                   else
017000010615     C                   if        flgOk = '0'
017100010615     C                   eval      wrkesito = '2'
017200010615     C                   else
017300010615     C                   eval      wrkesito = '6'
017400010615     C                   endif
017500010330     C                   endif
017600010330     C                   endif
017700010601     C                   else
017800010601     C                   eval      wrkesito = '9'
017900010601     C                   endif
018000010330     C*
018100010330     C                   if        %open(tivin00r)
018200010330     C                   close     tivin00r
018300010330     C                   endif
018400040301     C                   if        %open(fivatwwr)
018500040301     C                   close     fivatwwr
018600010330     C                   endif
018700010601     C*
018800010601     C                   if        vlrpoi <> 999
018900010601     C                   eval      invfil = vlrpoi
019000010601     C                   endif
019100010330     C*
019200010330     C                   if        呃trok > 0
019300010601     C                             and invfil > *zeros
019400010330     C                   exsr      invio
019500010330     C                   endif
019600010601     C*
019700010618     C                   if        flgGiro = '1'
019800010601     C                   exsr      endela
019900010618     C                   endif
020000010330     C*
020100010330     C                   ENDSR
020200010330     C***
020300010330
020400010601
020500010601
020600010601
020700010601
020800010330     C*----------------------------------------------------*
020900020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021000010330     C*----------------------------------------------------*
021100010330     C     INZVAR        BEGSR
021200051219     C*
021300010330     C*
021400010330     C                   ENDSR
021500010330     C*----------------------------------------------------*
021600020722     C*  IMPOSTAZIONE CAMPI COSTANTI
021700010330     C*----------------------------------------------------*
021800010330     C     DEFCAM        BEGSR
021900010330     C*
022000020204     C* Inizializzo il buffer del record da scrivere
022100040301     C                   CLEAR                   FIVAT000
022200020204     C* Imposto i valori di default...
022300140203     C                   EVAL      VATCCM = 0897599
022400140203     C                   EVAL      VATLNP = 089
022500040301     C                   EVAL      VATTRC = 'E'
022600020204     C* ... e poi verifico se sono stati passati come parametri
022700020204     C                   IF        vlrppt > *blanks
022800040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
022900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023000020204     C                   EXSR      CHKNUM
023100020204     C                   IF        PiInt=*on
023200040301     C                   Z-ADD     PiVal         VATCCM
023300020204     C                   ENDIF
023400040301     C                   ENDIF
023500040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
023600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023700020204     C                   EXSR      CHKNUM
023800020204     C                   IF        PiInt=*on
023900040301     C                   Z-ADD     PiVal         VATLNP
024000020204     C                   ENDIF
024100040301     C                   ENDIF
024200020204     C                   ENDIF
024300020204     C*
024400010330     C                   ENDSR
024500010607     C*----------------------------------------------------*
024600040301     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
024700010607     C*----------------------------------------------------*
024800040301     C     IMPVAT        BEGSR
024900010607     C*
025000010607     C                   SETOFF                                       3132
025100040316     C*
025200040316     C* Campi "calcolati"
025300040316     C                   MOVEL     datcor        VATAAS
025400010607     C*
025500010607     C* Reperimento campi ALFA
025600140203     C                   EVAL      VATNOT = %trim(%subst(vindta:117:18))
025700040301     C*
025800040301     C* Reperimento campi NUMERICI
025900080410     C                   EVAL      PiStr=%trim(%subst(vindta:16:7))
026000040301     C                   EXSR      CHKNUM
026100040301     C                   IF        PiInt=*on
026200040301     C                   Z-ADD     PiVal         VATNSP
026300040301     C                   ELSE
026400040301     C                   SETON                                        31
026500040301     C                   Z-ADD     *zeros        VATNSP
026600040301     C                   EVAL      vinmsg = %trimr(vinmsg)
026700040301     C                             + ' ' + 'VATNSP'
026800040301     C                   ENDIF
026900040301     C*
027000040301     C* Scarico il buffer del FIVAT
027100040301     C  N31              WRITE     FIVAT000
027200020204     C*
027300010607     C                   ENDSR
027400010607     C*----------------------------------------------------*
027500020204
027600010330
027700010330
027800010330     C*----------------------------------------------------*
027900010330     C*  CONTROLLO NUMERICITA' CAMPI
028000010330     C*----------------------------------------------------*
028100010330     C     CHKNUM        BEGSR
028200010330     C*
028300010606     C                   IF        PiDecChr = *blanks
028400010606     C                   EVAL      PiDecChr = ','
028500010606     C                   ENDIF
028600010606     C*
028700010606     C                   CALL(e)   'ISNUMERIC'
028800010330     C                   PARM                    PiStr            30
028900010606     C                   PARM                    PiDecChr          1
029000010330     C                   PARM      *ZEROS        PiVal            30 9
029100010330     C                   PARM      '0'           PiInt             1
029200010330     C                   PARM      '0'           PiNum             1
029300010330     C                   IF        %error
029400010606     C                   EVAL      PiNum=*off
029500010330     C                   ENDIF
029600010330     C*
029700010330     C                   ENDSR
029800010330     C***
029900010330
030000010601
030100010601
030200010601
030300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
030400010601     C     repfil        BEGSR
030500010601     C*
030600010601     C                   if        invfil = *zeros and
030700010601     C                             depfil > *zeros and
030800010629     C                             (vinflg = *blanks or
030900010629     C                              vinflg = *zeros)
031000010601     C
031100010601     C                   eval      invfil = depfil
031200010601     C                   endif
031300010601     C*
031400010601     C                   if        depfil <> invfil and
031500010601     C                             invfil > *zeros
031600010601     C                   eval      flgMulti = '1'
031700010601     C                   if        vinflg = *blanks
031800010601     C                   add       1             cntNonEl
031900010601     C                   endif
032000010601     C                   endif
032100010601     C*
032200010601     C                   if        vinflg = '2'
032300010601     C                   eval      flgStato = '2'
032400010601     C                   endif
032500010601     C*
032600010601     C                   ENDSR
032700010601     C***
032800010601
032900010601
033000010601
033100010330
033200010330
033300010330
033400990920      /TITLE Invio dei dati al punto operativo.
033500000613     C     invio         BEGSR
033600990920     C*
033700990920     C                   reset                   dscmz
033800010601     C                   move      invfil        cmzdst
033900990920     C                   eval      cmzfld = vlrfou
034000990920     C                   eval      cmzmbd = vlrhdl
034100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
034200000710     C                   if        prmfir = *blanks
034300040301     C                   eval      cmzfla = 'FIVAT00F'
034400040301     C                   eval      cmzmba = 'FIVAT00F'
034500000710     C                   else
034600000710     C                   eval      cmzfla = prmfir
034700000710     C                   eval      cmzmba = prmfir
034800000710     C                   endif
034900990920     C                   eval      cmznrr = *zeros
035000990920     C                   move      呃trok        cmznrr
035100021018     C                   eval      cmzlba = vlrfl1
035200990920     C                   call(e)   'TIS711C'
035300990920     C                   parm                    dscmz
035400990921     C                   parm      *blanks       esito
035500990920     C                   if        %error
035600990920     C                             or cmzerr = '1'
035700990921     C                             or esito  = '1'
035800000710     C                   eval      wrkesito = '3'
035900990920     C                   endif
036000990920     C*
036100000613     C                   ENDSR
036200990910
036300010601
036400010601
036500010601
036600010601
036700010601      /TITLE Invio dei dati al punto operativo.
036800010601     C     opeini        BEGSR
036900010601     C*
037000010601     C* Inizializzo flag e contatori operativi
037100010601     C                   movel     '0'           flgGiro           1
037200010601     C                   movel     '0'           flgMulti          1
037300010601     C                   movel     '1'           flgStato          1
037400010615     C                   movel     '0'           flgOk             1
037500010601     C                   z-add     *zeros        cntNonEl         10 0
037600010601     C                   z-add     *zeros        depfil            3 0
037700010601     C                   z-add     *zeros        invfil            3 0
037800010601     C*
037900010601     C                   ENDSR
038000010601     C***
038100010601
038200010601
038300010601
038400010330
038500010330
038600000613     C     *inzsr        BEGSR
038700990910     C*
038800990910     C     *entry        plist
038900990920     C                   parm                    tivlrds
039000990921     C                   parm      wrkesito      esito
039100000724     C                   parm                    prmlit
039200000710     C                   parm                    prmfir
039300010330     C*
039400010330     C* CALCOLA LA DATA CORRENTE
039500010330     C                   time                    wn14             14 0
039600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
039700010330     C                   z-add     wn8           g08dat
039800010330     C                   z-add     *zeros        g08inv
039900010330     C                   movel     '0'           g08err
040000010330     C                   call      'XSRDA8'
040100010330     C                   parm                    wlbda8
040200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
040300000613     C*
040400000613     C                   ENDSR
040500000613     C***
