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
001800160610     D $X              s              5  0
001900160610     D $Y              s              5  0
002000010330     D*------------------
002100010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002200010330     D*------------------
002300010330     D WLBDA8          DS                  INZ
002400010330     D  G08DAT                 1      8  0
002500010330     D  G08INV                 9     16  0
002600010330     D  G08ERR                17     17
002700010330     D  G08TGI                18     22  0
002800000313     D*
002900160610     D cifre           c                   const('0123456789')
003000010330
003100010330
003200010330
003300000913     C                   reset                   rrnum
003400990921     C                   reset                   esito
003500990921     C                   reset                   wrkesito
003600010601     C*
003700010601     C                   exsr      opeini
003800040301     C                   exsr      rwvat
003900010601     C*
004000010601     C                   seton                                        lr
004100010601
004200010601
004300010601
004400010601
004500010601     C*--------------------------------------------------------
004600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
004700010601     C*--------------------------------------------------------
004800010601     C     PREELA        BEGSR
004900010601     C*
005000010601     C* SE OCCORRE SPEDIRE IN FILIALE
005100010601     C                   if        invfil <> *zeros and
005200010601     C                             flgGiro = '0'
005300010601     C*
005400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005500010601     C                   eval      flgGiro = '1'
005600010601     C*
005700010601     C                   endif
005800010601     C*
005900010601     C                   ENDSR
006000010601     C***
006100010601
006200010601
006300010601
006400010601     C*--------------------------------------------------------
006500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
006600010601     C*--------------------------------------------------------
006700010601     C     ENDELA        BEGSR
006800990915     C*
006900000616     C*
007000010601     C                   ENDSR
007100010601     C***
007200010601
007300010330     C*--------------------------------------------------------
007400040301     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
007500010330     C*--------------------------------------------------------
007600040301     C     RWVAT         BEGSR
007700010330     C*
007800010330     C                   if        not %open(tivin00r)
007900010330     C                   open      tivin00r
008000010330     C                   endif
008100040301     C                   if        not %open(fivatwwr)
008200040301     C                   open      fivatwwr
008300010330     C                   endif
008400010330     C*
008500010604     C                   clear                   §CTROK
008600010604     C                   clear                   §CTRMO
008700010604     C                   clear                   §CTRNO
008800010330     C*
008900010330     C                   DO        *HIVAL
009000010330     C*
009100010330     C                   READ      tivin00r                               70
009200010618     C*
009300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
009400010618     C                   if        vinflg = '1'
009500010618     C                   eval      flgOk = '1'
009600010618     C                   endif
009700010618     C*
009800010330     C                   if        vindta > *blanks
009900010330     C                   add       1             rrnum
010000010330     C*
010100010601     C                   if        *in70 = *off and
010200010330     C                             (vinflg = *blanks
010300010330     C                              or vinflg = '0'
010400010330     C                              or vinflg = '2')
010500010330     C*
010600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
010700010711     C                   if        vinflg = *blanks or vinflg = '0'
010800010711     C                   clear                   vinmsg
010900010711     C                   endif
011000010601     C*
011100040301     C* Inizializzo campi d wrk e imposto i valori d default
011200010330     C                   exsr      inzvar
011300010330     C                   exsr      defcam
011400010601     C*
011500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
011600040301     C                   eval      depfil = VATLNP
011700010601     C                   exsr      repfil
011800010601     C                   if        depfil = invfil
011900021025     C                   if        vlrpoi = 999
012000040301     C                   MOVE(P)   invfil        VATFGS
012100021025     C                   else
012200040301     C                   MOVE(P)   vlrpoi        VATFGS
012300021025     C                   endif
012400050114     C                   endif
012500040301     C*
012600040301     C* Imposto e scarico il buffer del FIVAT
012700040301     C                   exsr      impvat
012800010601     C*
012900010601     C                   exsr      PREELA
013000010601     C*
013100010604     C* Ebbene...
013200010604     C*
013300010604     C  N31              ADD       1             §CTROK            7 0
013400010604     C   32              ADD       1             §CTRMO            7 0
013500010604     C   31              ADD       1             §CTRNO            7 0
013600010604     C*
013700020722     C                   endif
013800020722     C*
013900010604     C                   if        *in31 = *off and
014000010604     C                             *in32 = *off
014100010604     C                   eval      vinflg = '1'
014200010604     C                   else
014300010604     C                   eval      vinflg = '2'
014400010604     C                   endif
014500010604     C*
014600010330     C                   else
014700010330     C                   eval      vinflg = '1'
014800010330     C                   endif
014900010601     C*
015000010601     C  N70              update    tivin000
015100010330     C*
015200010330     C  N70              ENDdo
015300010601     C
015400010601     C*
015500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
015600010601     C                   if        cntNonEl = *zeros or
015700010601     C                             flgMulti = '0'
015800010330     C* Se non ci sono record con errori ...
015900010601     C                   if        §ctrno = 0 and
016000010604     C                             §ctrmo = 0 and
016100010601     C                             flgStato <> '2'
016200010330     C* ... restituisco esito OK.
016300010330     C                   eval      wrkesito = '0'
016400010330     C                   else
016500010330     C                   if        §ctrok > 0
016600010330     C                   eval      wrkesito = '1'
016700010330     C                   else
016800010615     C                   if        flgOk = '0'
016900010615     C                   eval      wrkesito = '2'
017000010615     C                   else
017100010615     C                   eval      wrkesito = '6'
017200010615     C                   endif
017300010330     C                   endif
017400010330     C                   endif
017500010601     C                   else
017600010601     C                   eval      wrkesito = '9'
017700010601     C                   endif
017800010330     C*
017900010330     C                   if        %open(tivin00r)
018000010330     C                   close     tivin00r
018100010330     C                   endif
018200040301     C                   if        %open(fivatwwr)
018300040301     C                   close     fivatwwr
018400010330     C                   endif
018500010601     C*
018600010601     C                   if        vlrpoi <> 999
018700010601     C                   eval      invfil = vlrpoi
018800010601     C                   endif
018900010330     C*
019000010330     C                   if        §ctrok > 0
019100010601     C                             and invfil > *zeros
019200010330     C                   exsr      invio
019300010330     C                   endif
019400010601     C*
019500010618     C                   if        flgGiro = '1'
019600010601     C                   exsr      endela
019700010618     C                   endif
019800010330     C*
019900010330     C                   ENDSR
020000010330     C***
020100010330
020200010601
020300010601
020400010601
020500010601
020600010330     C*----------------------------------------------------*
020700020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020800010330     C*----------------------------------------------------*
020900010330     C     INZVAR        BEGSR
021000051219     C*
021100010330     C*
021200010330     C                   ENDSR
021300010330     C*----------------------------------------------------*
021400020722     C*  IMPOSTAZIONE CAMPI COSTANTI
021500010330     C*----------------------------------------------------*
021600010330     C     DEFCAM        BEGSR
021700010330     C*
021800020204     C* Inizializzo il buffer del record da scrivere
021900040301     C                   CLEAR                   FIVAT000
022000020204     C* Imposto i valori di default...
022100160610     C                   EVAL      VATCCM = 0058437
022200160610     C                   EVAL      VATLNP = 005
022300040301     C                   EVAL      VATTRC = 'E'
022400020204     C* ... e poi verifico se sono stati passati come parametri
022500020204     C                   IF        vlrppt > *blanks
022600040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
022700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
022800020204     C                   EXSR      CHKNUM
022900020204     C                   IF        PiInt=*on
023000040301     C                   Z-ADD     PiVal         VATCCM
023100020204     C                   ENDIF
023200040301     C                   ENDIF
023300040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
023400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023500020204     C                   EXSR      CHKNUM
023600020204     C                   IF        PiInt=*on
023700040301     C                   Z-ADD     PiVal         VATLNP
023800020204     C                   ENDIF
023900040301     C                   ENDIF
024000020204     C                   ENDIF
024100020204     C*
024200010330     C                   ENDSR
024300010607     C*----------------------------------------------------*
024400040301     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
024500010607     C*----------------------------------------------------*
024600040301     C     IMPVAT        BEGSR
024700010607     C*
024800010607     C                   SETOFF                                       3132
024900040316     C*
025000040316     C* Campi "calcolati"
025100040316     C                   MOVEL     datcor        VATAAS
025200040301     C*
025300040301     C* Reperimento campi NUMERICI
025400160610
025500160624     C* --------------------------------------------------------------------------------------------
025600160624     C* INVECE CHE LA PARTE NUMERICA FINALE DEVO CONSIDERARE TUTTA LA PARTE NUMERICA E CHE LA
025700160624     C* PARTE NON NUMERICA E' L'UNDERSCORE (_)
025800160610     C* devo estrarre la parte numerica finale di un pezzo di stringa della vindta
025900160610     C* per cui trovo la prima pos. <> da blank partendo dal fondo della sottostringa
026000160624     C***                EVAL      $X    =
026100160624     C***                           %checkr(' ':%subst(vindta:11:20))
026200160610     C* poi da lì devo trovare la prima pos non numerica
026300160624     C***                EVAL      $Y    =
026400160624     C***                           %checkr(cifre:%subst(vindta:11:20):$X)
026500160624     C***                EVAL      PiStr=%subst(vindta:$Y+1+10:$X-$Y)
026600160624     C* --------------------------------------------------------------------------------------------
026700160624
026800160624     C                   EVAL      PiStr=
026900160624     C                              %scanrpl('_':'':
027000160624     C                              %trim(%subst(vindta:11:20)))
027100160610
027200040301     C                   EXSR      CHKNUM
027300040301     C                   IF        PiInt=*on
027400040301     C                   Z-ADD     PiVal         VATNSP
027500040301     C                   ELSE
027600040301     C                   SETON                                        31
027700040301     C                   Z-ADD     *zeros        VATNSP
027800040301     C                   EVAL      vinmsg = %trimr(vinmsg)
027900040301     C                             + ' ' + 'VATNSP'
028000040301     C                   ENDIF
028100130117     C*
028200130117     C* Reperimento campi ALFA
028300130117     C*
028400130117     C* - tipo record 'E'
028500130117     C                   EVAL      VATTRC = 'E'
028600160610     C                   EVAL      VATNOT = %trim(%subst(vindta:31:35))
028700040301     C  N31              WRITE     FIVAT000
028800020204     C*
028900010607     C                   ENDSR
029000010607     C*----------------------------------------------------*
029100020204
029200010330
029300010330
029400010330     C*----------------------------------------------------*
029500010330     C*  CONTROLLO NUMERICITA' CAMPI
029600010330     C*----------------------------------------------------*
029700010330     C     CHKNUM        BEGSR
029800010330     C*
029900010606     C                   IF        PiDecChr = *blanks
030000010606     C                   EVAL      PiDecChr = ','
030100010606     C                   ENDIF
030200010606     C*
030300010606     C                   CALL(e)   'ISNUMERIC'
030400010330     C                   PARM                    PiStr            30
030500010606     C                   PARM                    PiDecChr          1
030600010330     C                   PARM      *ZEROS        PiVal            30 9
030700010330     C                   PARM      '0'           PiInt             1
030800010330     C                   PARM      '0'           PiNum             1
030900010330     C                   IF        %error
031000010606     C                   EVAL      PiNum=*off
031100010330     C                   ENDIF
031200010330     C*
031300010330     C                   ENDSR
031400010330     C***
031500010330
031600010601
031700010601
031800010601
031900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
032000010601     C     repfil        BEGSR
032100010601     C*
032200010601     C                   if        invfil = *zeros and
032300010601     C                             depfil > *zeros and
032400010629     C                             (vinflg = *blanks or
032500010629     C                              vinflg = *zeros)
032600010601     C
032700010601     C                   eval      invfil = depfil
032800010601     C                   endif
032900010601     C*
033000010601     C                   if        depfil <> invfil and
033100010601     C                             invfil > *zeros
033200010601     C                   eval      flgMulti = '1'
033300010601     C                   if        vinflg = *blanks
033400010601     C                   add       1             cntNonEl
033500010601     C                   endif
033600010601     C                   endif
033700010601     C*
033800010601     C                   if        vinflg = '2'
033900010601     C                   eval      flgStato = '2'
034000010601     C                   endif
034100010601     C*
034200010601     C                   ENDSR
034300010601     C***
034400010601
034500010601
034600010601
034700010330
034800010330
034900010330
035000990920      /TITLE Invio dei dati al punto operativo.
035100000613     C     invio         BEGSR
035200990920     C*
035300990920     C                   reset                   dscmz
035400010601     C                   move      invfil        cmzdst
035500990920     C                   eval      cmzfld = vlrfou
035600990920     C                   eval      cmzmbd = vlrhdl
035700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
035800000710     C                   if        prmfir = *blanks
035900040301     C                   eval      cmzfla = 'FIVAT00F'
036000040301     C                   eval      cmzmba = 'FIVAT00F'
036100000710     C                   else
036200000710     C                   eval      cmzfla = prmfir
036300000710     C                   eval      cmzmba = prmfir
036400000710     C                   endif
036500990920     C                   eval      cmznrr = *zeros
036600990920     C                   move      §ctrok        cmznrr
036700021018     C                   eval      cmzlba = vlrfl1
036800990920     C                   call(e)   'TIS711C'
036900990920     C                   parm                    dscmz
037000990921     C                   parm      *blanks       esito
037100990920     C                   if        %error
037200990920     C                             or cmzerr = '1'
037300990921     C                             or esito  = '1'
037400000710     C                   eval      wrkesito = '3'
037500990920     C                   endif
037600990920     C*
037700000613     C                   ENDSR
037800990910
037900010601
038000010601
038100010601
038200010601
038300010601      /TITLE Invio dei dati al punto operativo.
038400010601     C     opeini        BEGSR
038500010601     C*
038600010601     C* Inizializzo flag e contatori operativi
038700010601     C                   movel     '0'           flgGiro           1
038800010601     C                   movel     '0'           flgMulti          1
038900010601     C                   movel     '1'           flgStato          1
039000010615     C                   movel     '0'           flgOk             1
039100010601     C                   z-add     *zeros        cntNonEl         10 0
039200010601     C                   z-add     *zeros        depfil            3 0
039300010601     C                   z-add     *zeros        invfil            3 0
039400010601     C*
039500010601     C                   ENDSR
039600010601     C***
039700010601
039800010601
039900010601
040000010330
040100010330
040200000613     C     *inzsr        BEGSR
040300990910     C*
040400990910     C     *entry        plist
040500990920     C                   parm                    tivlrds
040600990921     C                   parm      wrkesito      esito
040700000724     C                   parm                    prmlit
040800000710     C                   parm                    prmfir
040900010330     C*
041000010330     C* CALCOLA LA DATA CORRENTE
041100010330     C                   time                    wn14             14 0
041200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
041300010330     C                   z-add     wn8           g08dat
041400010330     C                   z-add     *zeros        g08inv
041500010330     C                   movel     '0'           g08err
041600010330     C                   call      'XSRDA8'
041700010330     C                   parm                    wlbda8
041800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
041900000613     C*
042000000613     C                   ENDSR
042100000613     C***
