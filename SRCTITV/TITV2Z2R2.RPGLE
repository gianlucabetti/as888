000100121005      /TITLE Upload via Internet: traduzione in EDIVATWR.
000200121218     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500121005     FEDIVATwr  O    E             DISK    usropn
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
001900170224     D wNomeFile       s             30    INZ(*blanks)
002000000313     D*
002100010330
002200010330
002300010330
002400000913     C                   reset                   rrnum
002500990921     C                   reset                   esito
002600990921     C                   reset                   wrkesito
002700010601     C*
002800010601     C                   exsr      opeini
002900040301     C                   exsr      rwvat
003000010601     C*
003100010601     C                   seton                                        lr
003200010601
003300010601
003400010601
003500010601     C*--------------------------------------------------------
003600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
003700010601     C*--------------------------------------------------------
003800010601     C     PREELA        BEGSR
003900010601     C*
004000010601     C* SE OCCORRE SPEDIRE IN FILIALE
004100010601     C                   if        invfil <> *zeros and
004200010601     C                             flgGiro = '0'
004300010601     C*
004400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
004500010601     C                   eval      flgGiro = '1'
004600010601     C*
004700010601     C                   endif
004800010601     C*
004900010601     C                   ENDSR
005000010601     C***
005100010601
005200010601
005300010601
005400010601     C*--------------------------------------------------------
005500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
005600010601     C*--------------------------------------------------------
005700010601     C     ENDELA        BEGSR
005800000616     C*
005900010601     C                   ENDSR
006000010601     C***
006100000613
006200010601
006300010601
006400010330     C*--------------------------------------------------------
006500121005     C* RWVAT   LEGGE TIVIN00R E SCRIVE EDIVATWF              *
006600010330     C*--------------------------------------------------------
006700040301     C     RWVAT         BEGSR
006800010330     C*
006900010330     C                   if        not %open(tivin00r)
007000010330     C                   open      tivin00r
007100010330     C                   endif
007200121005     C                   if        not %open(edivatwr)
007300121005     C                   open      edivatwr
007400010330     C                   endif
007500010330     C*
007600010604     C                   clear                   §CTROK
007700010604     C                   clear                   §CTRMO
007800010604     C                   clear                   §CTRNO
007900010330     C*
008000010330     C                   DO        *HIVAL
008100010330     C*
008200010330     C                   READ      tivin00r                               70
008300010618     C*
008400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
008500010618     C                   if        vinflg = '1'
008600010618     C                   eval      flgOk = '1'
008700010618     C                   endif
008800010618     C*
008900010330     C                   if        vindta > *blanks
009000010330     C                   add       1             rrnum
009100010330     C*
009200010601     C                   if        *in70 = *off and
009300010330     C                             (vinflg = *blanks
009400010330     C                              or vinflg = '0'
009500010330     C                              or vinflg = '2')
009600010330     C*
009700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
009800010711     C                   if        vinflg = *blanks or vinflg = '0'
009900010711     C                   clear                   vinmsg
010000010711     C                   endif
010100010601     C*
010200040301     C* Inizializzo campi d wrk e imposto i valori d default
010300010330     C                   exsr      inzvar
010400010330     C                   exsr      defcam
010500010601     C*
010600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
010700040301     C                   eval      depfil = VATLNP
010800010601     C                   exsr      repfil
010900010601     C                   if        depfil = invfil
011000021025     C                   if        vlrpoi = 999
011100040301     C                   MOVE(P)   invfil        VATFGS
011200021025     C                   else
011300040301     C                   MOVE(P)   vlrpoi        VATFGS
011400021025     C                   endif
011500050114     C                   endif
011600040301     C*
011700121005     C* Imposto e scarico il buffer del EDIVAT
011800040301     C                   exsr      impvat
011900010601     C*
012000010601     C                   exsr      PREELA
012100010601     C*
012200010604     C* Ebbene...
012300010604     C*
012400010604     C  N31              ADD       1             §CTROK            7 0
012500010604     C   32              ADD       1             §CTRMO            7 0
012600010604     C   31              ADD       1             §CTRNO            7 0
012700010604     C*
012800020722     C                   endif
012900020722     C*
013000010604     C                   if        *in31 = *off and
013100010604     C                             *in32 = *off
013200010604     C                   eval      vinflg = '1'
013300010604     C                   else
013400010604     C                   eval      vinflg = '2'
013500010604     C                   endif
013600010604     C*
013700010330     C                   else
013800010330     C                   eval      vinflg = '1'
013900010330     C                   endif
014000010601     C*
014100010601     C  N70              update    tivin000
014200010330     C*
014300010330     C  N70              ENDdo
014400010601     C
014500010601     C*
014600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
014700010601     C                   if        cntNonEl = *zeros or
014800010601     C                             flgMulti = '0'
014900010330     C* Se non ci sono record con errori ...
015000010601     C                   if        §ctrno = 0 and
015100010604     C                             §ctrmo = 0 and
015200010601     C                             flgStato <> '2'
015300010330     C* ... restituisco esito OK.
015400010330     C                   eval      wrkesito = '0'
015500010330     C                   else
015600010330     C                   if        §ctrok > 0
015700010330     C                   eval      wrkesito = '1'
015800010330     C                   else
015900010615     C                   if        flgOk = '0'
016000010615     C                   eval      wrkesito = '2'
016100010615     C                   else
016200010615     C                   eval      wrkesito = '6'
016300010615     C                   endif
016400010330     C                   endif
016500010330     C                   endif
016600010601     C                   else
016700010601     C                   eval      wrkesito = '9'
016800010601     C                   endif
016900010330     C*
017000010330     C                   if        %open(tivin00r)
017100010330     C                   close     tivin00r
017200010330     C                   endif
017300121005     C                   if        %open(edivatwr)
017400121005     C                   close     edivatwr
017500010330     C                   endif
017600010601     C*
017700010601     C                   if        vlrpoi <> 999
017800010601     C                   eval      invfil = vlrpoi
017900010601     C                   endif
018000010330     C*
018100010330     C                   if        §ctrok > 0
018200010601     C                             and invfil > *zeros
018300010330     C                   exsr      invio
018400010330     C                   endif
018500010601     C*
018600010618     C                   if        flgGiro = '1'
018700010601     C                   exsr      endela
018800010618     C                   endif
018900010330     C*
019000010330     C                   ENDSR
019100010330     C***
019200010601
019300010601
019400010601
019500010330     C*----------------------------------------------------*
019600020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019700010330     C*----------------------------------------------------*
019800010330     C     INZVAR        BEGSR
019900051219     C*
020000010330     C*
020100010330     C                   ENDSR
020200170224
020300170224
020400170224
020500010330     C*----------------------------------------------------*
020600020722     C*  IMPOSTAZIONE CAMPI COSTANTI
020700010330     C*----------------------------------------------------*
020800010330     C     DEFCAM        BEGSR
020900010330     C*
021000020204     C* Inizializzo il buffer del record da scrivere
021100121005     C                   CLEAR                   EDIVAT00
021200020204     C* Imposto i valori di default...
021300170224     C                   EVAL      VATCCM = 2201097
021400170224     C                   EVAL      VATLNP = 220
021500040301     C                   EVAL      VATTRC = 'E'
021600020204     C* ... e poi verifico se sono stati passati come parametri
021700020204     C                   IF        vlrppt > *blanks
021800040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
021900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
022000020204     C                   EXSR      CHKNUM
022100020204     C                   IF        PiInt=*on
022200040301     C                   Z-ADD     PiVal         VATCCM
022300020204     C                   ENDIF
022400040301     C                   ENDIF
022500040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
022600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022700020204     C                   EXSR      CHKNUM
022800020204     C                   IF        PiInt=*on
022900040301     C                   Z-ADD     PiVal         VATLNP
023000020204     C                   ENDIF
023100040301     C                   ENDIF
023200020204     C                   ENDIF
023300020204     C*
023400010330     C                   ENDSR
023500170224
023600170224
023700170224
023800010607     C*----------------------------------------------------*
023900121005     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
024000010607     C*----------------------------------------------------*
024100040301     C     IMPVAT        BEGSR
024200010607     C*
024300010607     C                   SETOFF                                       3132
024400040316     C*
024500040316     C* Campi "calcolati"
024600040316     C                   MOVEL     datcor        VATAAS
024700010607     C*
024800010607     C* Reperimento campi ALFA
024900080311     C                   IF        %trim(%subst(vindta:97:15)) = *blanks
025000080311     C                   EVAL      VATNOT = %trim(%subst(vindta:21:15))
025100080311     C                   ELSE
025200080311     C                   EVAL      VATNOT = %trim(%subst(vindta:21:15))+
025300080311     C                                      %trim(%subst(vindta:97:15))
025400080311     C                   ENDIF
025500040301     C*
025600040301     C* Reperimento campi NUMERICI
025700080318     C* NSP
025800080311     C                   EVAL      PiStr=%trim(%subst(vindta:1:15))
025900040301     C                   EXSR      CHKNUM
026000040301     C                   IF        PiInt=*on
026100040301     C                   Z-ADD     PiVal         VATNSP
026200040301     C                   ELSE
026300040301     C                   SETON                                        31
026400040301     C                   Z-ADD     *zeros        VATNSP
026500040301     C                   EVAL      vinmsg = %trimr(vinmsg)
026600040301     C                             + ' ' + 'VATNSP'
026700040301     C                   ENDIF
026800121005     C*
026900121005     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
027000170224     C                   EVAL      VATCMR = %subst(wNomeFile:1:12) + ' ' +
027100170224     C                                      %char(datcor)
027200121005     C                   EVAL      VATCNT = 1
027300040301     C*
027400121005     C* Scarico il buffer del EDIVAT
027500121005     C  N31              WRITE     EDIVAT00
027600020204     C*
027700010607     C                   ENDSR
027800010607     C*----------------------------------------------------*
027900020204
028000010330
028100010330
028200010330     C*----------------------------------------------------*
028300010330     C*  CONTROLLO NUMERICITA' CAMPI
028400010330     C*----------------------------------------------------*
028500010330     C     CHKNUM        BEGSR
028600010330     C*
028700010606     C                   IF        PiDecChr = *blanks
028800080311     C                   EVAL      PiDecChr = '.'
028900010606     C                   ENDIF
029000010606     C*
029100010606     C                   CALL(e)   'ISNUMERIC'
029200010330     C                   PARM                    PiStr            30
029300010606     C                   PARM                    PiDecChr          1
029400010330     C                   PARM      *ZEROS        PiVal            30 9
029500010330     C                   PARM      '0'           PiInt             1
029600010330     C                   PARM      '0'           PiNum             1
029700010330     C                   IF        %error
029800010606     C                   EVAL      PiNum=*off
029900010330     C                   ENDIF
030000010330     C*
030100010330     C                   ENDSR
030200010330     C***
030300010330
030400010601
030500010601
030600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
030700010601     C     repfil        BEGSR
030800010601     C*
030900010601     C                   if        invfil = *zeros and
031000010601     C                             depfil > *zeros and
031100010629     C                             (vinflg = *blanks or
031200010629     C                              vinflg = *zeros)
031300010601     C
031400010601     C                   eval      invfil = depfil
031500010601     C                   endif
031600010601     C*
031700010601     C                   if        depfil <> invfil and
031800010601     C                             invfil > *zeros
031900010601     C                   eval      flgMulti = '1'
032000010601     C                   if        vinflg = *blanks
032100010601     C                   add       1             cntNonEl
032200010601     C                   endif
032300010601     C                   endif
032400010601     C*
032500010601     C                   if        vinflg = '2'
032600010601     C                   eval      flgStato = '2'
032700010601     C                   endif
032800010601     C*
032900010601     C                   ENDSR
033000010601     C***
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
034300121005     C                   eval      cmzfla = 'EDIVAT0F'
034400121005     C                   eval      cmzmba = 'EDIVAT0F'
034500000710     C                   else
034600000710     C                   eval      cmzfla = prmfir
034700000710     C                   eval      cmzmba = prmfir
034800000710     C                   endif
034900990920     C                   eval      cmznrr = *zeros
035000990920     C                   move      §ctrok        cmznrr
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
036500010601      /TITLE Invio dei dati al punto operativo.
036600010601     C     opeini        BEGSR
036700010601     C*
036800010601     C* Inizializzo flag e contatori operativi
036900010601     C                   movel     '0'           flgGiro           1
037000010601     C                   movel     '0'           flgMulti          1
037100010601     C                   movel     '1'           flgStato          1
037200010615     C                   movel     '0'           flgOk             1
037300010601     C                   z-add     *zeros        cntNonEl         10 0
037400010601     C                   z-add     *zeros        depfil            3 0
037500010601     C                   z-add     *zeros        invfil            3 0
037600010601     C*
037700010601     C                   ENDSR
037800010601     C***
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
038900170224     C*
039000170224     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
039100170224     C                   move(p)   vlrMSG        wNomeFile
039200010330     C*
039300010330     C* CALCOLA LA DATA CORRENTE
039400170224     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
039500170224     C                   eval      datcor = %dec(%date() : *iso)
039600000613     C*
039700000613     C                   ENDSR
039800000613     C***
