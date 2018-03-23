000100080410      /TITLE Upload via Internet: traduzione in FIVATWWR.
000200130306      *
000300130306      * PARTICOLARITA':
000400130306      * barcode = prendere la numerazione di 18 CARATTERI dal byte 41 x 18 bytes
000500130306      *
000600121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000700000313     F*
000800990910     Ftivin00r  uF   E             DISK    usropn
000900040301     FFIVATwwr  O    E             DISK    usropn
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
006900990915     C*
007000000616     C*
007100010601     C                   ENDSR
007200010601     C***
007300010601
007400010330     C*--------------------------------------------------------
007500040301     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
007600010330     C*--------------------------------------------------------
007700040301     C     RWVAT         BEGSR
007800010330     C*
007900010330     C                   if        not %open(tivin00r)
008000010330     C                   open      tivin00r
008100010330     C                   endif
008200040301     C                   if        not %open(fivatwwr)
008300040301     C                   open      fivatwwr
008400010330     C                   endif
008500010330     C*
008600010604     C                   clear                   低TROK
008700010604     C                   clear                   低TRMO
008800010604     C                   clear                   低TRNO
008900010330     C*
009000010330     C                   DO        *HIVAL
009100010330     C*
009200010330     C                   READ      tivin00r                               70
009300010618     C*
009400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
009500010618     C                   if        vinflg = '1'
009600010618     C                   eval      flgOk = '1'
009700010618     C                   endif
009800010618     C*
009900010330     C                   if        vindta > *blanks
010000010330     C                   add       1             rrnum
010100010330     C*
010200010601     C                   if        *in70 = *off and
010300010330     C                             (vinflg = *blanks
010400010330     C                              or vinflg = '0'
010500010330     C                              or vinflg = '2')
010600010330     C*
010700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
010800010711     C                   if        vinflg = *blanks or vinflg = '0'
010900010711     C                   clear                   vinmsg
011000010711     C                   endif
011100010601     C*
011200040301     C* Inizializzo campi d wrk e imposto i valori d default
011300010330     C                   exsr      inzvar
011400010330     C                   exsr      defcam
011500010601     C*
011600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
011700040301     C                   eval      depfil = VATLNP
011800010601     C                   exsr      repfil
011900010601     C                   if        depfil = invfil
012000021025     C                   if        vlrpoi = 999
012100040301     C                   MOVE(P)   invfil        VATFGS
012200021025     C                   else
012300040301     C                   MOVE(P)   vlrpoi        VATFGS
012400021025     C                   endif
012500050114     C                   endif
012600040301     C*
012700040301     C* Imposto e scarico il buffer del FIVAT
012800040301     C                   exsr      impvat
012900010601     C*
013000010601     C                   exsr      PREELA
013100010601     C*
013200010604     C* Ebbene...
013300010604     C*
013400010604     C  N31              ADD       1             低TROK            7 0
013500010604     C   32              ADD       1             低TRMO            7 0
013600010604     C   31              ADD       1             低TRNO            7 0
013700010604     C*
013800020722     C                   endif
013900020722     C*
014000010604     C                   if        *in31 = *off and
014100010604     C                             *in32 = *off
014200010604     C                   eval      vinflg = '1'
014300010604     C                   else
014400010604     C                   eval      vinflg = '2'
014500010604     C                   endif
014600010604     C*
014700010330     C                   else
014800010330     C                   eval      vinflg = '1'
014900010330     C                   endif
015000010601     C*
015100010601     C  N70              update    tivin000
015200010330     C*
015300010330     C  N70              ENDdo
015400010601     C
015500010601     C*
015600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
015700010601     C                   if        cntNonEl = *zeros or
015800010601     C                             flgMulti = '0'
015900010330     C* Se non ci sono record con errori ...
016000010601     C                   if        呃trno = 0 and
016100010604     C                             呃trmo = 0 and
016200010601     C                             flgStato <> '2'
016300010330     C* ... restituisco esito OK.
016400010330     C                   eval      wrkesito = '0'
016500010330     C                   else
016600010330     C                   if        呃trok > 0
016700010330     C                   eval      wrkesito = '1'
016800010330     C                   else
016900010615     C                   if        flgOk = '0'
017000010615     C                   eval      wrkesito = '2'
017100010615     C                   else
017200010615     C                   eval      wrkesito = '6'
017300010615     C                   endif
017400010330     C                   endif
017500010330     C                   endif
017600010601     C                   else
017700010601     C                   eval      wrkesito = '9'
017800010601     C                   endif
017900010330     C*
018000010330     C                   if        %open(tivin00r)
018100010330     C                   close     tivin00r
018200010330     C                   endif
018300040301     C                   if        %open(fivatwwr)
018400040301     C                   close     fivatwwr
018500010330     C                   endif
018600010601     C*
018700010601     C                   if        vlrpoi <> 999
018800010601     C                   eval      invfil = vlrpoi
018900010601     C                   endif
019000010330     C*
019100010330     C                   if        呃trok > 0
019200010601     C                             and invfil > *zeros
019300010330     C                   exsr      invio
019400010330     C                   endif
019500010601     C*
019600010618     C                   if        flgGiro = '1'
019700010601     C                   exsr      endela
019800010618     C                   endif
019900010330     C*
020000010330     C                   ENDSR
020100010330     C***
020200010330
020300010601
020400010601
020500010601
020600010601
020700010330     C*----------------------------------------------------*
020800020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020900010330     C*----------------------------------------------------*
021000010330     C     INZVAR        BEGSR
021100051219     C*
021200010330     C*
021300010330     C                   ENDSR
021400010330     C*----------------------------------------------------*
021500020722     C*  IMPOSTAZIONE CAMPI COSTANTI
021600010330     C*----------------------------------------------------*
021700010330     C     DEFCAM        BEGSR
021800010330     C*
021900020204     C* Inizializzo il buffer del record da scrivere
022000040301     C                   CLEAR                   FIVAT000
022100020204     C* Imposto i valori di default...
022200130306     C                   EVAL      VATCCM = 0505772
022300130306     C                   EVAL      VATLNP = 050
022400040301     C                   EVAL      VATTRC = 'E'
022500020204     C* ... e poi verifico se sono stati passati come parametri
022600020204     C                   IF        vlrppt > *blanks
022700040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
022800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
022900020204     C                   EXSR      CHKNUM
023000020204     C                   IF        PiInt=*on
023100040301     C                   Z-ADD     PiVal         VATCCM
023200020204     C                   ENDIF
023300040301     C                   ENDIF
023400040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
023500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023600020204     C                   EXSR      CHKNUM
023700020204     C                   IF        PiInt=*on
023800040301     C                   Z-ADD     PiVal         VATLNP
023900020204     C                   ENDIF
024000040301     C                   ENDIF
024100020204     C                   ENDIF
024200020204     C*
024300010330     C                   ENDSR
024400010607     C*----------------------------------------------------*
024500040301     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
024600010607     C*----------------------------------------------------*
024700040301     C     IMPVAT        BEGSR
024800010607     C*
024900010607     C                   SETOFF                                       3132
025000040316     C*
025100040316     C* Campi "calcolati"
025200121024     C*
025300121024     C* AAS (deve essere preso da loro)
025400121024     C                   EVAL      PiStr=%trim(%subst(vindta:11:4))
025500121024     C                   EXSR      CHKNUM
025600121024     C                   IF        PiInt=*on
025700121024     C                   Z-ADD     PiVal         VATAAS
025800121024     C                   ELSE
025900121024     C                   SETON                                        31
026000121024     C                   Z-ADD     *zeros        VATAAS
026100121024     C                   EVAL      vinmsg = %trimr(vinmsg)
026200121024     C                             + ' ' + 'VATAAS'
026300121024     C                   ENDIF
026400010607     C*
026500010607     C* Reperimento campi ALFA
026600121024     C                   EVAL      VATNOT =
026700130306     C                                      %trim(%subst(vindta:41:18))
026800040301     C*
026900040301     C* Reperimento campi NUMERICI
027000130306     C                   EVAL      PiStr=%trim(%subst(vindta:3:8))
027100040301     C                   EXSR      CHKNUM
027200040301     C                   IF        PiInt=*on
027300040301     C                   Z-ADD     PiVal         VATNSP
027400040301     C                   ELSE
027500040301     C                   SETON                                        31
027600040301     C                   Z-ADD     *zeros        VATNSP
027700040301     C                   EVAL      vinmsg = %trimr(vinmsg)
027800040301     C                             + ' ' + 'VATNSP'
027900040301     C                   ENDIF
028000040301     C*
028100040301     C* Scarico il buffer del FIVAT
028200040301     C  N31              WRITE     FIVAT000
028300020204     C*
028400010607     C                   ENDSR
028500010607     C*----------------------------------------------------*
028600020204
028700010330
028800010330
028900010330     C*----------------------------------------------------*
029000010330     C*  CONTROLLO NUMERICITA' CAMPI
029100010330     C*----------------------------------------------------*
029200010330     C     CHKNUM        BEGSR
029300010330     C*
029400010606     C                   IF        PiDecChr = *blanks
029500010606     C                   EVAL      PiDecChr = ','
029600010606     C                   ENDIF
029700010606     C*
029800010606     C                   CALL(e)   'ISNUMERIC'
029900010330     C                   PARM                    PiStr            30
030000010606     C                   PARM                    PiDecChr          1
030100010330     C                   PARM      *ZEROS        PiVal            30 9
030200010330     C                   PARM      '0'           PiInt             1
030300010330     C                   PARM      '0'           PiNum             1
030400010330     C                   IF        %error
030500010606     C                   EVAL      PiNum=*off
030600010330     C                   ENDIF
030700010330     C*
030800010330     C                   ENDSR
030900010330     C***
031000010330
031100010601
031200010601
031300010601
031400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
031500010601     C     repfil        BEGSR
031600010601     C*
031700010601     C                   if        invfil = *zeros and
031800010601     C                             depfil > *zeros and
031900010629     C                             (vinflg = *blanks or
032000010629     C                              vinflg = *zeros)
032100010601     C
032200010601     C                   eval      invfil = depfil
032300010601     C                   endif
032400010601     C*
032500010601     C                   if        depfil <> invfil and
032600010601     C                             invfil > *zeros
032700010601     C                   eval      flgMulti = '1'
032800010601     C                   if        vinflg = *blanks
032900010601     C                   add       1             cntNonEl
033000010601     C                   endif
033100010601     C                   endif
033200010601     C*
033300010601     C                   if        vinflg = '2'
033400010601     C                   eval      flgStato = '2'
033500010601     C                   endif
033600010601     C*
033700010601     C                   ENDSR
033800010601     C***
033900010601
034000010601
034100010601
034200010330
034300010330
034400010330
034500990920      /TITLE Invio dei dati al punto operativo.
034600000613     C     invio         BEGSR
034700990920     C*
034800990920     C                   reset                   dscmz
034900010601     C                   move      invfil        cmzdst
035000990920     C                   eval      cmzfld = vlrfou
035100990920     C                   eval      cmzmbd = vlrhdl
035200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
035300000710     C                   if        prmfir = *blanks
035400040301     C                   eval      cmzfla = 'FIVAT00F'
035500040301     C                   eval      cmzmba = 'FIVAT00F'
035600000710     C                   else
035700000710     C                   eval      cmzfla = prmfir
035800000710     C                   eval      cmzmba = prmfir
035900000710     C                   endif
036000990920     C                   eval      cmznrr = *zeros
036100990920     C                   move      呃trok        cmznrr
036200021018     C                   eval      cmzlba = vlrfl1
036300990920     C                   call(e)   'TIS711C'
036400990920     C                   parm                    dscmz
036500990921     C                   parm      *blanks       esito
036600990920     C                   if        %error
036700990920     C                             or cmzerr = '1'
036800990921     C                             or esito  = '1'
036900000710     C                   eval      wrkesito = '3'
037000990920     C                   endif
037100990920     C*
037200000613     C                   ENDSR
037300990910
037400010601
037500010601
037600010601
037700010601
037800010601      /TITLE Invio dei dati al punto operativo.
037900010601     C     opeini        BEGSR
038000010601     C*
038100010601     C* Inizializzo flag e contatori operativi
038200010601     C                   movel     '0'           flgGiro           1
038300010601     C                   movel     '0'           flgMulti          1
038400010601     C                   movel     '1'           flgStato          1
038500010615     C                   movel     '0'           flgOk             1
038600010601     C                   z-add     *zeros        cntNonEl         10 0
038700010601     C                   z-add     *zeros        depfil            3 0
038800010601     C                   z-add     *zeros        invfil            3 0
038900010601     C*
039000010601     C                   ENDSR
039100010601     C***
039200010601
039300010601
039400010601
039500010330
039600010330
039700000613     C     *inzsr        BEGSR
039800990910     C*
039900990910     C     *entry        plist
040000990920     C                   parm                    tivlrds
040100990921     C                   parm      wrkesito      esito
040200000724     C                   parm                    prmlit
040300000710     C                   parm                    prmfir
040400010330     C*
040500010330     C* CALCOLA LA DATA CORRENTE
040600010330     C                   time                    wn14             14 0
040700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
040800010330     C                   z-add     wn8           g08dat
040900010330     C                   z-add     *zeros        g08inv
041000010330     C                   movel     '0'           g08err
041100010330     C                   call      'XSRDA8'
041200010330     C                   parm                    wlbda8
041300010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
041400000613     C*
041500000613     C                   ENDSR
041600000613     C***
