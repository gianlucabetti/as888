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
021800120601     C                   EVAL      VATCCM = 1661574
021900120601     C                   EVAL      VATLNP = 166
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
024800040316     C                   MOVEL     datcor        VATAAS
024900010607     C*
025000010607     C* Reperimento campi ALFA
025100120601     C                   EVAL      VATNOT = '0000000' +
025200120601     C                                      %trim(%subst(vindta:19:18))
025300040301     C*
025400040301     C* Reperimento campi NUMERICI
025500120601     C                   EVAL      PiStr=%trim(%subst(vindta:11:8))
025600040301     C                   EXSR      CHKNUM
025700040301     C                   IF        PiInt=*on
025800040301     C                   Z-ADD     PiVal         VATNSP
025900040301     C                   ELSE
026000040301     C                   SETON                                        31
026100040301     C                   Z-ADD     *zeros        VATNSP
026200040301     C                   EVAL      vinmsg = %trimr(vinmsg)
026300040301     C                             + ' ' + 'VATNSP'
026400040301     C                   ENDIF
026500040301     C*
026600040301     C* Scarico il buffer del FIVAT
026700040301     C  N31              WRITE     FIVAT000
026800020204     C*
026900010607     C                   ENDSR
027000010607     C*----------------------------------------------------*
027100020204
027200010330
027300010330
027400010330     C*----------------------------------------------------*
027500010330     C*  CONTROLLO NUMERICITA' CAMPI
027600010330     C*----------------------------------------------------*
027700010330     C     CHKNUM        BEGSR
027800010330     C*
027900010606     C                   IF        PiDecChr = *blanks
028000010606     C                   EVAL      PiDecChr = ','
028100010606     C                   ENDIF
028200010606     C*
028300010606     C                   CALL(e)   'ISNUMERIC'
028400010330     C                   PARM                    PiStr            30
028500010606     C                   PARM                    PiDecChr          1
028600010330     C                   PARM      *ZEROS        PiVal            30 9
028700010330     C                   PARM      '0'           PiInt             1
028800010330     C                   PARM      '0'           PiNum             1
028900010330     C                   IF        %error
029000010606     C                   EVAL      PiNum=*off
029100010330     C                   ENDIF
029200010330     C*
029300010330     C                   ENDSR
029400010330     C***
029500010330
029600010601
029700010601
029800010601
029900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
030000010601     C     repfil        BEGSR
030100010601     C*
030200010601     C                   if        invfil = *zeros and
030300010601     C                             depfil > *zeros and
030400010629     C                             (vinflg = *blanks or
030500010629     C                              vinflg = *zeros)
030600010601     C
030700010601     C                   eval      invfil = depfil
030800010601     C                   endif
030900010601     C*
031000010601     C                   if        depfil <> invfil and
031100010601     C                             invfil > *zeros
031200010601     C                   eval      flgMulti = '1'
031300010601     C                   if        vinflg = *blanks
031400010601     C                   add       1             cntNonEl
031500010601     C                   endif
031600010601     C                   endif
031700010601     C*
031800010601     C                   if        vinflg = '2'
031900010601     C                   eval      flgStato = '2'
032000010601     C                   endif
032100010601     C*
032200010601     C                   ENDSR
032300010601     C***
032400010601
032500010601
032600010601
032700010330
032800010330
032900010330
033000990920      /TITLE Invio dei dati al punto operativo.
033100000613     C     invio         BEGSR
033200990920     C*
033300990920     C                   reset                   dscmz
033400010601     C                   move      invfil        cmzdst
033500990920     C                   eval      cmzfld = vlrfou
033600990920     C                   eval      cmzmbd = vlrhdl
033700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
033800000710     C                   if        prmfir = *blanks
033900040301     C                   eval      cmzfla = 'FIVAT00F'
034000040301     C                   eval      cmzmba = 'FIVAT00F'
034100000710     C                   else
034200000710     C                   eval      cmzfla = prmfir
034300000710     C                   eval      cmzmba = prmfir
034400000710     C                   endif
034500990920     C                   eval      cmznrr = *zeros
034600990920     C                   move      呃trok        cmznrr
034700021018     C                   eval      cmzlba = vlrfl1
034800990920     C                   call(e)   'TIS711C'
034900990920     C                   parm                    dscmz
035000990921     C                   parm      *blanks       esito
035100990920     C                   if        %error
035200990920     C                             or cmzerr = '1'
035300990921     C                             or esito  = '1'
035400000710     C                   eval      wrkesito = '3'
035500990920     C                   endif
035600990920     C*
035700000613     C                   ENDSR
035800990910
035900010601
036000010601
036100010601
036200010601
036300010601      /TITLE Invio dei dati al punto operativo.
036400010601     C     opeini        BEGSR
036500010601     C*
036600010601     C* Inizializzo flag e contatori operativi
036700010601     C                   movel     '0'           flgGiro           1
036800010601     C                   movel     '0'           flgMulti          1
036900010601     C                   movel     '1'           flgStato          1
037000010615     C                   movel     '0'           flgOk             1
037100010601     C                   z-add     *zeros        cntNonEl         10 0
037200010601     C                   z-add     *zeros        depfil            3 0
037300010601     C                   z-add     *zeros        invfil            3 0
037400010601     C*
037500010601     C                   ENDSR
037600010601     C***
037700010601
037800010601
037900010601
038000010330
038100010330
038200000613     C     *inzsr        BEGSR
038300990910     C*
038400990910     C     *entry        plist
038500990920     C                   parm                    tivlrds
038600990921     C                   parm      wrkesito      esito
038700000724     C                   parm                    prmlit
038800000710     C                   parm                    prmfir
038900010330     C*
039000010330     C* CALCOLA LA DATA CORRENTE
039100010330     C                   time                    wn14             14 0
039200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
039300010330     C                   z-add     wn8           g08dat
039400010330     C                   z-add     *zeros        g08inv
039500010330     C                   movel     '0'           g08err
039600010330     C                   call      'XSRDA8'
039700010330     C                   parm                    wlbda8
039800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
039900000613     C*
040000000613     C                   ENDSR
040100000613     C***
