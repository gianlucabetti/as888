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
022000071029     C                   EVAL      VATCCM = 0894032
022100071029     C                   EVAL      VATLNP = 089
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
025300071029     C                   EVAL      VATNOT = %trim(%subst(vindta:21:14))
025400040301     C*
025500040301     C* Reperimento campi NUMERICI
025600071029     C                   EVAL      PiStr=%trim(%subst(vindta:6:7))
025700040301     C                   EXSR      CHKNUM
025800040301     C                   IF        PiInt=*on
025900040301     C                   Z-ADD     PiVal         VATNSP
026000040301     C                   ELSE
026100040301     C                   SETON                                        31
026200040301     C                   Z-ADD     *zeros        VATNSP
026300040301     C                   EVAL      vinmsg = %trimr(vinmsg)
026400040301     C                             + ' ' + 'VATNSP'
026500040301     C                   ENDIF
026600040301     C*
026700040301     C* Scarico il buffer del FIVAT
026800040301     C  N31              WRITE     FIVAT000
026900020204     C*
027000010607     C                   ENDSR
027100010607     C*----------------------------------------------------*
027200020204
027300010330
027400010330
027500010330     C*----------------------------------------------------*
027600010330     C*  CONTROLLO NUMERICITA' CAMPI
027700010330     C*----------------------------------------------------*
027800010330     C     CHKNUM        BEGSR
027900010330     C*
028000010606     C                   IF        PiDecChr = *blanks
028100010606     C                   EVAL      PiDecChr = ','
028200010606     C                   ENDIF
028300010606     C*
028400010606     C                   CALL(e)   'ISNUMERIC'
028500010330     C                   PARM                    PiStr            30
028600010606     C                   PARM                    PiDecChr          1
028700010330     C                   PARM      *ZEROS        PiVal            30 9
028800010330     C                   PARM      '0'           PiInt             1
028900010330     C                   PARM      '0'           PiNum             1
029000010330     C                   IF        %error
029100010606     C                   EVAL      PiNum=*off
029200010330     C                   ENDIF
029300010330     C*
029400010330     C                   ENDSR
029500010330     C***
029600010330
029700010601
029800010601
029900010601
030000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
030100010601     C     repfil        BEGSR
030200010601     C*
030300010601     C                   if        invfil = *zeros and
030400010601     C                             depfil > *zeros and
030500010629     C                             (vinflg = *blanks or
030600010629     C                              vinflg = *zeros)
030700010601     C
030800010601     C                   eval      invfil = depfil
030900010601     C                   endif
031000010601     C*
031100010601     C                   if        depfil <> invfil and
031200010601     C                             invfil > *zeros
031300010601     C                   eval      flgMulti = '1'
031400010601     C                   if        vinflg = *blanks
031500010601     C                   add       1             cntNonEl
031600010601     C                   endif
031700010601     C                   endif
031800010601     C*
031900010601     C                   if        vinflg = '2'
032000010601     C                   eval      flgStato = '2'
032100010601     C                   endif
032200010601     C*
032300010601     C                   ENDSR
032400010601     C***
032500010601
032600010601
032700010601
032800010330
032900010330
033000010330
033100990920      /TITLE Invio dei dati al punto operativo.
033200000613     C     invio         BEGSR
033300990920     C*
033400990920     C                   reset                   dscmz
033500010601     C                   move      invfil        cmzdst
033600990920     C                   eval      cmzfld = vlrfou
033700990920     C                   eval      cmzmbd = vlrhdl
033800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
033900000710     C                   if        prmfir = *blanks
034000040301     C                   eval      cmzfla = 'FIVAT00F'
034100040301     C                   eval      cmzmba = 'FIVAT00F'
034200000710     C                   else
034300000710     C                   eval      cmzfla = prmfir
034400000710     C                   eval      cmzmba = prmfir
034500000710     C                   endif
034600990920     C                   eval      cmznrr = *zeros
034700990920     C                   move      呃trok        cmznrr
034800021018     C                   eval      cmzlba = vlrfl1
034900990920     C                   call(e)   'TIS711C'
035000990920     C                   parm                    dscmz
035100990921     C                   parm      *blanks       esito
035200990920     C                   if        %error
035300990920     C                             or cmzerr = '1'
035400990921     C                             or esito  = '1'
035500000710     C                   eval      wrkesito = '3'
035600990920     C                   endif
035700990920     C*
035800000613     C                   ENDSR
035900990910
036000010601
036100010601
036200010601
036300010601
036400010601      /TITLE Invio dei dati al punto operativo.
036500010601     C     opeini        BEGSR
036600010601     C*
036700010601     C* Inizializzo flag e contatori operativi
036800010601     C                   movel     '0'           flgGiro           1
036900010601     C                   movel     '0'           flgMulti          1
037000010601     C                   movel     '1'           flgStato          1
037100010615     C                   movel     '0'           flgOk             1
037200010601     C                   z-add     *zeros        cntNonEl         10 0
037300010601     C                   z-add     *zeros        depfil            3 0
037400010601     C                   z-add     *zeros        invfil            3 0
037500010601     C*
037600010601     C                   ENDSR
037700010601     C***
037800010601
037900010601
038000010601
038100010330
038200010330
038300000613     C     *inzsr        BEGSR
038400990910     C*
038500990910     C     *entry        plist
038600990920     C                   parm                    tivlrds
038700990921     C                   parm      wrkesito      esito
038800000724     C                   parm                    prmlit
038900000710     C                   parm                    prmfir
039000010330     C*
039100010330     C* CALCOLA LA DATA CORRENTE
039200010330     C                   time                    wn14             14 0
039300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
039400010330     C                   z-add     wn8           g08dat
039500010330     C                   z-add     *zeros        g08inv
039600010330     C                   movel     '0'           g08err
039700010330     C                   call      'XSRDA8'
039800010330     C                   parm                    wlbda8
039900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
040000000613     C*
040100000613     C                   ENDSR
040200000613     C***
