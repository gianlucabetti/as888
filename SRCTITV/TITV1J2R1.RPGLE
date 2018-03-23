000100081016      /TITLE Upload via Internet: traduzione in EDIVATWR.
000200130114     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500081016     FEDIVATwr  O    E             DISK    usropn
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
001900081029     D wNomeFile       s             30    INZ(*blanks)
002000010330     D*------------------
002100010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002200010330     D*------------------
002300010330     D WLBDA8          DS                  INZ
002400010330     D  G08DAT                 1      8  0
002500010330     D  G08INV                 9     16  0
002600010330     D  G08ERR                17     17
002700010330     D  G08TGI                18     22  0
002800081007     D*------------------
002900081007     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
003000081007     D*------------------
003100081007     D PiStr           S            100A   INZ
003200081007     D PiDecChr        S              1A   INZ
003300081007     D PiVal           S             63S30 INZ
003400081007     D PiInt           S               N   INZ
003500081007     D PiNum           S               N   INZ
003600081007
003700081007     D*------------------
003800081007     D* LINKING A DEFINIZIONI ESTERNE
003900081007     D*------------------
004000081007     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
004100081007
004200081007
004300010330
004400000913     C                   reset                   rrnum
004500990921     C                   reset                   esito
004600990921     C                   reset                   wrkesito
004700010601     C*
004800010601     C                   exsr      opeini
004900040301     C                   exsr      rwvat
005000010601     C*
005100010601     C                   seton                                        lr
005200010601
005300010601
005400010601
005500010601
005600010601     C*--------------------------------------------------------
005700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005800010601     C*--------------------------------------------------------
005900010601     C     PREELA        BEGSR
006000010601     C*
006100010601     C* SE OCCORRE SPEDIRE IN FILIALE
006200010601     C                   if        invfil <> *zeros and
006300010601     C                             flgGiro = '0'
006400010601     C*
006500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006600010601     C                   eval      flgGiro = '1'
006700010601     C*
006800010601     C                   endif
006900010601     C*
007000010601     C                   ENDSR
007100010601     C***
007200010601
007300010601
007400010601
007500010601     C*--------------------------------------------------------
007600010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007700010601     C*--------------------------------------------------------
007800010601     C     ENDELA        BEGSR
007900000616     C*
008000010601     C                   ENDSR
008100010601     C***
008200000613
008300010601
008400010601
008500010330     C*--------------------------------------------------------
008600081016     C* RWVAT   LEGGE TIVIN00R E SCRIVE EDIVATWF              *
008700010330     C*--------------------------------------------------------
008800040301     C     RWVAT         BEGSR
008900010330     C*
009000010330     C                   if        not %open(tivin00r)
009100010330     C                   open      tivin00r
009200010330     C                   endif
009300081016     C                   if        not %open(edivatwr)
009400081016     C                   open      edivatwr
009500010330     C                   endif
009600010330     C*
009700010604     C                   clear                   §CTROK
009800010604     C                   clear                   §CTRMO
009900010604     C                   clear                   §CTRNO
010000010330     C*
010100010330     C                   DO        *HIVAL
010200010330     C*
010300010330     C                   READ      tivin00r                               70
010400010618     C*
010500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
010600010618     C                   if        vinflg = '1'
010700010618     C                   eval      flgOk = '1'
010800010618     C                   endif
010900010618     C*
011000010330     C                   if        vindta > *blanks
011100010330     C                   add       1             rrnum
011200010330     C*
011300010601     C                   if        *in70 = *off and
011400010330     C                             (vinflg = *blanks
011500010330     C                              or vinflg = '0'
011600010330     C                              or vinflg = '2')
011700010330     C*
011800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011900010711     C                   if        vinflg = *blanks or vinflg = '0'
012000010711     C                   clear                   vinmsg
012100010711     C                   endif
012200010601     C*
012300040301     C* Inizializzo campi d wrk e imposto i valori d default
012400010330     C                   exsr      inzvar
012500010330     C                   exsr      defcam
012600010601     C*
012700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012800040301     C                   eval      depfil = VATLNP
012900010601     C                   exsr      repfil
013000010601     C                   if        depfil = invfil
013100021025     C                   if        vlrpoi = 999
013200040301     C                   MOVE(P)   invfil        VATFGS
013300021025     C                   else
013400040301     C                   MOVE(P)   vlrpoi        VATFGS
013500021025     C                   endif
013600050114     C                   endif
013700040301     C*
013800081016     C* Imposto e scarico il buffer del EDIVAT
013900040301     C                   exsr      impvat
014000010601     C*
014100010601     C                   exsr      PREELA
014200010601     C*
014300010604     C* Ebbene...
014400010604     C*
014500010604     C  N31              ADD       1             §CTROK            7 0
014600010604     C   32              ADD       1             §CTRMO            7 0
014700010604     C   31              ADD       1             §CTRNO            7 0
014800010604     C*
014900020722     C                   endif
015000020722     C*
015100010604     C                   if        *in31 = *off and
015200010604     C                             *in32 = *off
015300010604     C                   eval      vinflg = '1'
015400010604     C                   else
015500010604     C                   eval      vinflg = '2'
015600010604     C                   endif
015700010604     C*
015800010330     C                   else
015900010330     C                   eval      vinflg = '1'
016000010330     C                   endif
016100010601     C*
016200010601     C  N70              update    tivin000
016300010330     C*
016400010330     C  N70              ENDdo
016500010601     C
016600010601     C*
016700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
016800010601     C                   if        cntNonEl = *zeros or
016900010601     C                             flgMulti = '0'
017000010330     C* Se non ci sono record con errori ...
017100010601     C                   if        §ctrno = 0 and
017200010604     C                             §ctrmo = 0 and
017300010601     C                             flgStato <> '2'
017400010330     C* ... restituisco esito OK.
017500010330     C                   eval      wrkesito = '0'
017600010330     C                   else
017700010330     C                   if        §ctrok > 0
017800010330     C                   eval      wrkesito = '1'
017900010330     C                   else
018000010615     C                   if        flgOk = '0'
018100010615     C                   eval      wrkesito = '2'
018200010615     C                   else
018300010615     C                   eval      wrkesito = '6'
018400010615     C                   endif
018500010330     C                   endif
018600010330     C                   endif
018700010601     C                   else
018800010601     C                   eval      wrkesito = '9'
018900010601     C                   endif
019000010330     C*
019100010330     C                   if        %open(tivin00r)
019200010330     C                   close     tivin00r
019300010330     C                   endif
019400081016     C                   if        %open(edivatwr)
019500081016     C                   close     edivatwr
019600010330     C                   endif
019700010601     C*
019800010601     C                   if        vlrpoi <> 999
019900010601     C                   eval      invfil = vlrpoi
020000010601     C                   endif
020100010330     C*
020200010330     C                   if        §ctrok > 0
020300010601     C                             and invfil > *zeros
020400010330     C                   exsr      invio
020500010330     C                   endif
020600010601     C*
020700010618     C                   if        flgGiro = '1'
020800010601     C                   exsr      endela
020900010618     C                   endif
021000010330     C*
021100010330     C                   ENDSR
021200010330     C***
021300010330
021400010601
021500010601
021600010601
021700010601
021800010330     C*----------------------------------------------------*
021900020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022000010330     C*----------------------------------------------------*
022100010330     C     INZVAR        BEGSR
022200051219     C*
022300010330     C*
022400010330     C                   ENDSR
022500010330     C*----------------------------------------------------*
022600020722     C*  IMPOSTAZIONE CAMPI COSTANTI
022700010330     C*----------------------------------------------------*
022800010330     C     DEFCAM        BEGSR
022900010330     C*
023000020204     C* Inizializzo il buffer del record da scrivere
023100081016     C                   CLEAR                   EDIVAT00
023200020204     C* Imposto i valori di default...
023300081007     C                   EVAL      VATCCM = 2220929
023400081007     C                   EVAL      VATLNP = 222
023500040301     C                   EVAL      VATTRC = 'E'
023600020204     C* ... e poi verifico se sono stati passati come parametri
023700020204     C                   IF        vlrppt > *blanks
023800040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
023900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
024000020204     C                   EXSR      CHKNUM
024100020204     C                   IF        PiInt=*on
024200040301     C                   Z-ADD     PiVal         VATCCM
024300020204     C                   ENDIF
024400040301     C                   ENDIF
024500040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
024600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024700020204     C                   EXSR      CHKNUM
024800020204     C                   IF        PiInt=*on
024900040301     C                   Z-ADD     PiVal         VATLNP
025000020204     C                   ENDIF
025100040301     C                   ENDIF
025200020204     C                   ENDIF
025300020204     C*
025400010330     C                   ENDSR
025500010607     C*----------------------------------------------------*
025600081016     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
025700010607     C*----------------------------------------------------*
025800040301     C     IMPVAT        BEGSR
025900010607     C*
026000010607     C                   SETOFF                                       3132
026100040316     C*
026200040316     C* Campi "calcolati"
026300040316     C                   MOVEL     datcor        VATAAS
026400010607     C*
026500010607     C* Reperimento campi ALFA
026600081013     C                   EVAL      VATNOT=%trim(%subst(vindta:38:10))
026700040301     C*
026800040301     C* Reperimento campi NUMERICI
026900081007     C                   EVAL      PiStr=%trim(%subst(vindta:11:10))
027000040301     C                   EXSR      CHKNUM
027100040301     C                   IF        PiInt=*on
027200040301     C                   Z-ADD     PiVal         VATNSP
027300040301     C                   ELSE
027400040301     C                   SETON                                        31
027500040301     C                   Z-ADD     *zeros        VATNSP
027600040301     C                   EVAL      vinmsg = %trimr(vinmsg)
027700040301     C                             + ' ' + 'VATNSP'
027800040301     C                   ENDIF
027900081016     C*
028000081016     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
028100090310     C                   EVAL      VATCMR = %subst(wNomeFile:10:05)
028200081016     C                   EVAL      VATCNT = 1
028300040301     C*
028400081016     C* Scarico il buffer del EDIVAT
028500081016     C  N31              WRITE     EDIVAT00
028600020204     C*
028700010607     C                   ENDSR
028800010607     C*----------------------------------------------------*
028900020204
029000010330
029100010330
029200010330     C*----------------------------------------------------*
029300010330     C*  CONTROLLO NUMERICITA' CAMPI
029400010330     C*----------------------------------------------------*
029500010330     C     CHKNUM        BEGSR
029600081007     C*
029700081007     C                   IF        PiDecChr = *blanks
029800081007     C                   EVAL      PiDecChr = ','
029900081007     C                   ENDIF
030000081007     C*
030100081007     C                   callp     UBISNUM_Check(PiStr
030200081007     C                                          :PiDecChr
030300081007     C                                          :PiVal
030400081007     C                                          :PiNum
030500081007     C                                          :PiInt)
030600010330     C*
030700010330     C                   ENDSR
030800010330     C***
030900010330
031000010601
031100010601
031200010601
031300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
031400010601     C     repfil        BEGSR
031500010601     C*
031600010601     C                   if        invfil = *zeros and
031700010601     C                             depfil > *zeros and
031800010629     C                             (vinflg = *blanks or
031900010629     C                              vinflg = *zeros)
032000010601     C
032100010601     C                   eval      invfil = depfil
032200010601     C                   endif
032300010601     C*
032400010601     C                   if        depfil <> invfil and
032500010601     C                             invfil > *zeros
032600010601     C                   eval      flgMulti = '1'
032700010601     C                   if        vinflg = *blanks
032800010601     C                   add       1             cntNonEl
032900010601     C                   endif
033000010601     C                   endif
033100010601     C*
033200010601     C                   if        vinflg = '2'
033300010601     C                   eval      flgStato = '2'
033400010601     C                   endif
033500010601     C*
033600010601     C                   ENDSR
033700010601     C***
033800010601
033900010601
034000010601
034100010330
034200010330
034300010330
034400990920      /TITLE Invio dei dati al punto operativo.
034500000613     C     invio         BEGSR
034600990920     C*
034700990920     C                   reset                   dscmz
034800010601     C                   move      invfil        cmzdst
034900990920     C                   eval      cmzfld = vlrfou
035000990920     C                   eval      cmzmbd = vlrhdl
035100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
035200000710     C                   if        prmfir = *blanks
035300081016     C                   eval      cmzfla = 'EDIVAT0F'
035400081016     C                   eval      cmzmba = 'EDIVAT0F'
035500000710     C                   else
035600000710     C                   eval      cmzfla = prmfir
035700000710     C                   eval      cmzmba = prmfir
035800000710     C                   endif
035900990920     C                   eval      cmznrr = *zeros
036000990920     C                   move      §ctrok        cmznrr
036100021018     C                   eval      cmzlba = vlrfl1
036200990920     C                   call(e)   'TIS711C'
036300990920     C                   parm                    dscmz
036400990921     C                   parm      *blanks       esito
036500990920     C                   if        %error
036600990920     C                             or cmzerr = '1'
036700990921     C                             or esito  = '1'
036800000710     C                   eval      wrkesito = '3'
036900990920     C                   endif
037000990920     C*
037100000613     C                   ENDSR
037200990910
037300010601
037400010601
037500010601
037600010601
037700010601      /TITLE Invio dei dati al punto operativo.
037800010601     C     opeini        BEGSR
037900010601     C*
038000010601     C* Inizializzo flag e contatori operativi
038100010601     C                   movel     '0'           flgGiro           1
038200010601     C                   movel     '0'           flgMulti          1
038300010601     C                   movel     '1'           flgStato          1
038400010615     C                   movel     '0'           flgOk             1
038500010601     C                   z-add     *zeros        cntNonEl         10 0
038600010601     C                   z-add     *zeros        depfil            3 0
038700010601     C                   z-add     *zeros        invfil            3 0
038800010601     C*
038900010601     C                   ENDSR
039000010601     C***
039100010601
039200010601
039300010601
039400010330
039500010330
039600000613     C     *inzsr        BEGSR
039700990910     C*
039800990910     C     *entry        plist
039900990920     C                   parm                    tivlrds
040000990921     C                   parm      wrkesito      esito
040100000724     C                   parm                    prmlit
040200000710     C                   parm                    prmfir
040300081029     C*
040400081029     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
040500081029     C                   move(p)   vlrMSG        wNomeFile
040600010330     C*
040700010330     C* CALCOLA LA DATA CORRENTE
040800010330     C                   time                    wn14             14 0
040900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
041000010330     C                   z-add     wn8           g08dat
041100010330     C                   z-add     *zeros        g08inv
041200010330     C                   movel     '0'           g08err
041300010330     C                   call      'XSRDA8'
041400010330     C                   parm                    wlbda8
041500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
041600000613     C*
041700000613     C                   ENDSR
041800000613     C***
