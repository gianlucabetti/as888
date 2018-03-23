000100021025      /TITLE Upload via Internet: traduzione in FIVATWWR. (x STEFANEL Cod. 0591072)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021025     FFIVATwwr  O    E             DISK    usropn
000600000313     D*
000700050314     D*------------
000800050314     D* SCHIERE A RUN-TIME
000900050314     D*------------
001000050314     D* - "SERIE" CLIENTE ALFA
001100050314     D SERALF          S              1    DIM(10) CTDATA PERRCD(10)
001200050314     D* - "SERIE" CLIENTE NUMERICA
001300050314     D SERNUM          S              1    DIM(10) CTDATA PERRCD(10)
001400010330     D*----------------------------------------------------
001500010330     D* DICHIARAZIOINE VARIABILI DI WRK
001600010330     D*----------------------------------------------------
001700010330     D dscmz         e ds                  inz
001800010330     D psds           sds
001900010330     D  procname         *PROC
002000010330     D tivlrds       e ds                  extname(tivlr00f)
002100010330     D esito           s              1
002200010330     D prmlit          s             10
002300010330     D prmfir          s             10
002400010330     D wrkesito        s                   like(esito)
002500010330     D rrnum           s              6  0 INZ(*zeros)
002600010330     D*------------------
002700010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002800010330     D*------------------
002900010330     D WLBDA8          DS                  INZ
003000010330     D  G08DAT                 1      8  0
003100010330     D  G08INV                 9     16  0
003200010330     D  G08ERR                17     17
003300010330     D  G08TGI                18     22  0
003400000313     D*
003500010330
003600010330
003700010330
003800000913     C                   reset                   rrnum
003900990921     C                   reset                   esito
004000990921     C                   reset                   wrkesito
004100010601     C*
004200010601     C                   exsr      opeini
004300020321     C                   exsr      rwvat
004400010601     C*
004500010601     C                   seton                                        lr
004600010601
004700010601
004800010601
004900010601
005000010601     C*--------------------------------------------------------
005100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005200010601     C*--------------------------------------------------------
005300010601     C     PREELA        BEGSR
005400010601     C*
005500010601     C* SE OCCORRE SPEDIRE IN FILIALE
005600010601     C                   if        invfil <> *zeros and
005700010601     C                             flgGiro = '0'
005800010601     C*
005900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006000010601     C                   eval      flgGiro = '1'
006100010601     C*
006200010601     C                   endif
006300010601     C*
006400010601     C                   ENDSR
006500010601     C***
006600010601
006700010601
006800010601
006900010601
007000010601
007100010601
007200010601
007300010601     C*--------------------------------------------------------
007400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007500010601     C*--------------------------------------------------------
007600010601     C     ENDELA        BEGSR
007700000616     C*
007800010601     C                   ENDSR
007900010601     C***
008000000613
008100010330
008200010601
008300010601
008400010601
008500010330     C*--------------------------------------------------------
008600021025     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
008700010330     C*--------------------------------------------------------
008800020321     C     RWVAT         BEGSR
008900010330     C*
009000010330     C                   if        not %open(tivin00r)
009100010330     C                   open      tivin00r
009200010330     C                   endif
009300021025     C                   if        not %open(fivatwwr)
009400021025     C                   open      fivatwwr
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
012300010330     C                   exsr      inzvar
012400010330     C                   exsr      defcam
012500020321     C                   exsr      impvat
012600010601     C*
012700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012800020321     C                   eval      depfil = VATLNP
012900010601     C                   exsr      repfil
013000010601     C                   if        depfil = invfil
013100021025     C                   if        vlrpoi = 999
013200021025     C                   MOVE(P)   invfil        VATFGS
013300021025     C                   else
013400021025     C                   MOVE(P)   vlrpoi        VATFGS
013500021025     C                   endif
013600010601     C*
013700010601     C                   exsr      PREELA
013800010601     C*
013900010604     C* Ebbene...
014000010604     C*
014100010604     C  N31              ADD       1             §CTROK            7 0
014200010604     C   32              ADD       1             §CTRMO            7 0
014300010604     C   31              ADD       1             §CTRNO            7 0
014400020205     C*
014500020205     C* Effettuo test particolare solo x DA ROLD x condizoinare la scrittura
014600021025     C  N31              WRITE     FIVAT000
014700010604     C*
014800010604     C                   if        *in31 = *off and
014900010604     C                             *in32 = *off
015000010604     C                   eval      vinflg = '1'
015100010604     C                   else
015200010604     C                   eval      vinflg = '2'
015300010604     C                   endif
015400010604     C                   endif
015500010601     C*
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
019400021025     C                   if        %open(fivatwwr)
019500021025     C                   close     fivatwwr
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
021900010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022000010330     C*----------------------------------------------------*
022100060331     C     INZVAR        BEGSR
022200010330     C*
022300010330     C*
022400010330     C                   ENDSR
022500010330     C*----------------------------------------------------*
022600010330     C*  IMPOSTAZIONE CAMPI COSTANTI
022700010330     C*----------------------------------------------------*
022800010330     C     DEFCAM        BEGSR
022900010330     C*
023000020204     C* Inizializzo il buffer del record da scrivere
023100021025     C                   CLEAR                   FIVAT000
023200020204     C* Imposto i valori di default...
023300020321     C                   EVAL      VATCCM = 0591072
023400020321     C                   EVAL      VATLNP = 059
023500020321     C                   EVAL      VATTRC = 'E'
023600020204     C* ... e poi verifico se sono stati passati come parametri
023700020204     C                   IF        vlrppt > *blanks
023800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023900020204     C                   EXSR      CHKNUM
024000020204     C                   IF        PiInt=*on
024100020321     C                   Z-ADD     PiVal         VATCCM
024200020204     C                   ENDIF
024300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024400020204     C                   EXSR      CHKNUM
024500020204     C                   IF        PiInt=*on
024600020321     C                   Z-ADD     PiVal         VATLNP
024700020204     C                   ENDIF
024800020321     C                   ENDIF
024900020204     C*
025000010330     C                   ENDSR
025100010607     C*----------------------------------------------------*
025200060331     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
025300010607     C*----------------------------------------------------*
025400020321     C     IMPVAT        BEGSR
025500010607     C*
025600010607     C                   SETOFF                                       3132
025700010607     C*
025800010607     C* Reperimento campi ALFA
025900060331     C***                EVAL      VATNOT='20'+%trim(%subst(vindta:17:2))+
026000060331     C***                                 %trim(%subst(vindta:21:8))
026100060331     C                   EVAL      VATNOT=%trim(%subst(vindta:17:12))
026200020403     C*
026300020403     C* Calcolo il check-digits sul barcode fin qui composto
026400020403     C                   EXSR      CHKDGT
026500010607     C*
026600010607     C* Reperimento campi NUMERICI
026700020321     C                   EVAL      PiStr=%trim(%subst(vindta:4:4))
026800020304     C                   EXSR      CHKNUM
026900020304     C                   IF        PiInt=*on
027000020321     C                   Z-ADD     PiVal         VATAAS
027100020304     C                   ELSE
027200020304     C                   SETON                                        32
027300020321     C                   EVAL      VATAAS = *zeros
027400020304     C                   EVAL      vinmsg = %trimr(vinmsg)
027500020321     C                             + ' ' + 'VATAAS'
027600020304     C                   ENDIF
027700020304     C*
027800050314     C                   MOVEL     *blanks       wSerieAlfa        1
027900050314     C                   MOVEL     *zeros        wSerieNum         1
028000050314     C                   EVAL      wSerieAlfa=%subst(vindta:8:1)
028100050314     C                   Z-ADD     1             wInd              2 0
028200050314     C     wSerieAlfa    LOOKUP    SERALF(wInd)                           55
028300050314     C                   IF        %found
028400050314     C                   EVAL      wSerieNum = SERNUM(wInd)
028500050314     C                   ENDIF
028600050314     C*
028700050314     C                   EVAL      PiStr=%trim(%subst(vindta:10:7))
028800020304     C                   EXSR      CHKNUM
028900020304     C                   IF        PiInt=*on
029000020321     C                   Z-ADD     PiVal         VATNSP
029100050314     C                   MOVEL     wSerieNum     VATNSP
029200020304     C                   ELSE
029300020304     C                   SETON                                        32
029400020321     C                   Z-ADD     *zeros        VATNSP
029500020304     C                   EVAL      vinmsg = %trimr(vinmsg)
029600020321     C                             + ' ' + 'VATNSP'
029700020304     C                   ENDIF
029800020204     C*
029900010607     C                   ENDSR
030000010607     C*----------------------------------------------------*
030100010601
030200010330
030300010330     C*----------------------------------------------------*
030400010330     C*  CONTROLLO NUMERICITA' CAMPI
030500010330     C*----------------------------------------------------*
030600010330     C     CHKNUM        BEGSR
030700010330     C*
030800010606     C                   IF        PiDecChr = *blanks
030900010606     C                   EVAL      PiDecChr = ','
031000010606     C                   ENDIF
031100010606     C*
031200010606     C                   CALL(e)   'ISNUMERIC'
031300010330     C                   PARM                    PiStr            30
031400010606     C                   PARM                    PiDecChr          1
031500010330     C                   PARM      *ZEROS        PiVal            30 9
031600010330     C                   PARM      '0'           PiInt             1
031700010330     C                   PARM      '0'           PiNum             1
031800010330     C                   IF        %error
031900010606     C                   EVAL      PiNum=*off
032000010330     C                   ENDIF
032100010330     C*
032200010330     C                   ENDSR
032300010330     C***
032400010330
032500010601
032600020403
032700020403     C*----------------------------------------------------*
032800020403     C*  CALCOLO IL CHECK-DIGITS (EAN/UCC-13)
032900020403     C*----------------------------------------------------*
033000020403     C     CHKDGT        BEGSR
033100020403     C*
033200020403     C* Innanzitutto verifico che sia numerico
033300020403     C                   EVAL      PiStr=%trim(VATNOT)
033400020403     C                   EXSR      CHKNUM
033500020403     C                   IF        PiInt=*on
033600020403     C* Se è numerico... procedo con il calcolo del check-digits
033700020403     C                   Z-ADD     *ZEROS        SumPARI           3 0
033800020403     C                   Z-ADD     *ZEROS        SumDISPARI        3 0
033900020403     C                   Z-ADD     *ZEROS        SumTOT            3 0
034000020403     C                   MOVEL     *ZEROS        AddendoA          1
034100020403     C                   Z-ADD     *ZEROS        AddendoN          1 0
034200020403     C                   Z-ADD     *ZEROS        RestoA10          2 0
034300020403     C                   MOVEL     *BLANKS       ChkDigits         1
034400020403     C* Sommo le cifre in posizioni pari
034500020403     C                   EVAL      AddendoA = %subst(VATNOT:2:1)
034600020403     C                   MOVEL     AddendoA      AddendoN
034700020403     C                   ADD       AddendoN      SumPARI
034800020403     C                   EVAL      AddendoA = %subst(VATNOT:4:1)
034900020403     C                   MOVEL     AddendoA      AddendoN
035000020403     C                   ADD       AddendoN      SumPARI
035100020403     C                   EVAL      AddendoA = %subst(VATNOT:6:1)
035200020403     C                   MOVEL     AddendoA      AddendoN
035300020403     C                   ADD       AddendoN      SumPARI
035400020403     C                   EVAL      AddendoA = %subst(VATNOT:8:1)
035500020403     C                   MOVEL     AddendoA      AddendoN
035600020403     C                   ADD       AddendoN      SumPARI
035700020403     C                   EVAL      AddendoA = %subst(VATNOT:10:1)
035800020403     C                   MOVEL     AddendoA      AddendoN
035900020403     C                   ADD       AddendoN      SumPARI
036000020403     C                   EVAL      AddendoA = %subst(VATNOT:12:1)
036100020403     C                   MOVEL     AddendoA      AddendoN
036200020403     C                   ADD       AddendoN      SumPARI
036300020403     C* Moltiplico la somma ottenuta per 3
036400020403     C                   EVAL      SumPARI = SumPARI*3
036500020403     C* Sommo le cifre in posizioni dispari
036600020403     C                   EVAL      AddendoA = %subst(VATNOT:1:1)
036700020403     C                   MOVEL     AddendoA      AddendoN
036800020403     C                   ADD       AddendoN      SumDISPARI
036900020403     C                   EVAL      AddendoA = %subst(VATNOT:3:1)
037000020403     C                   MOVEL     AddendoA      AddendoN
037100020403     C                   ADD       AddendoN      SumDISPARI
037200020403     C                   EVAL      AddendoA = %subst(VATNOT:5:1)
037300020403     C                   MOVEL     AddendoA      AddendoN
037400020403     C                   ADD       AddendoN      SumDISPARI
037500020403     C                   EVAL      AddendoA = %subst(VATNOT:7:1)
037600020403     C                   MOVEL     AddendoA      AddendoN
037700020403     C                   ADD       AddendoN      SumDISPARI
037800020403     C                   EVAL      AddendoA = %subst(VATNOT:9:1)
037900020403     C                   MOVEL     AddendoA      AddendoN
038000020403     C                   ADD       AddendoN      SumDISPARI
038100020403     C                   EVAL      AddendoA = %subst(VATNOT:11:1)
038200020403     C                   MOVEL     AddendoA      AddendoN
038300020403     C                   ADD       AddendoN      SumDISPARI
038400020403     C* Aggiungo alla somma delle cifre pari moltiplicate per 3 alla somma di quelle dispari
038500020403     C                   EVAL      SumTOT = SumPARI + SumDISPARI
038600020403     C                   MOVE      SumTOT        AddendoN
038700020403     C                   EVAL      RestoA10 = 10 - AddendoN
038800020403     C                   MOVE      RestoA10      ChkDigits
038900020403     C* Infine compongo il barcode finale
039000020403     C                   EVAL      VATNOT = %trim(VATNOT) + ChkDigits
039100020403     C                   ENDIF
039200020403     C*
039300020403     C                   ENDSR
039400020403     C***
039500020403
039600020403
039700010601
039800010601
039900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
040000010601     C     repfil        BEGSR
040100010601     C*
040200010601     C                   if        invfil = *zeros and
040300010601     C                             depfil > *zeros and
040400010629     C                             (vinflg = *blanks or
040500010629     C                              vinflg = *zeros)
040600010601     C
040700010601     C                   eval      invfil = depfil
040800010601     C                   endif
040900010601     C*
041000010601     C                   if        depfil <> invfil and
041100010601     C                             invfil > *zeros
041200010601     C                   eval      flgMulti = '1'
041300010601     C                   if        vinflg = *blanks
041400010601     C                   add       1             cntNonEl
041500010601     C                   endif
041600010601     C                   endif
041700010601     C*
041800010601     C                   if        vinflg = '2'
041900010601     C                   eval      flgStato = '2'
042000010601     C                   endif
042100010601     C*
042200010601     C                   ENDSR
042300010601     C***
042400010601
042500010601
042600010601
042700010330
042800010330
042900010330
043000990920      /TITLE Invio dei dati al punto operativo.
043100000613     C     invio         BEGSR
043200990920     C*
043300990920     C                   reset                   dscmz
043400010601     C                   move      invfil        cmzdst
043500990920     C                   eval      cmzfld = vlrfou
043600990920     C                   eval      cmzmbd = vlrhdl
043700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
043800000710     C                   if        prmfir = *blanks
043900021025     C                   eval      cmzfla = 'FIVAT00F'
044000021025     C                   eval      cmzmba = 'FIVAT00F'
044100000710     C                   else
044200000710     C                   eval      cmzfla = prmfir
044300000710     C                   eval      cmzmba = prmfir
044400000710     C                   endif
044500990920     C                   eval      cmznrr = *zeros
044600990920     C                   move      §ctrok        cmznrr
044700021018     C                   eval      cmzlba = vlrfl1
044800990920     C                   call(e)   'TIS711C'
044900990920     C                   parm                    dscmz
045000990921     C                   parm      *blanks       esito
045100990920     C                   if        %error
045200990920     C                             or cmzerr = '1'
045300990921     C                             or esito  = '1'
045400000710     C                   eval      wrkesito = '3'
045500990920     C                   endif
045600990920     C*
045700000613     C                   ENDSR
045800990910
045900010601
046000010601
046100010601
046200010601
046300010601      /TITLE Invio dei dati al punto operativo.
046400010601     C     opeini        BEGSR
046500010601     C*
046600010601     C* Inizializzo flag e contatori operativi
046700010601     C                   movel     '0'           flgGiro           1
046800010601     C                   movel     '0'           flgMulti          1
046900010601     C                   movel     '1'           flgStato          1
047000010615     C                   movel     '0'           flgOk             1
047100010601     C                   z-add     *zeros        cntNonEl         10 0
047200010601     C                   z-add     *zeros        depfil            3 0
047300010601     C                   z-add     *zeros        invfil            3 0
047400010601     C*
047500010601     C                   ENDSR
047600010601     C***
047700010601
047800010601
047900010601
048000010330
048100010330
048200000613     C     *inzsr        BEGSR
048300990910     C*
048400990910     C     *entry        plist
048500990920     C                   parm                    tivlrds
048600990921     C                   parm      wrkesito      esito
048700000724     C                   parm                    prmlit
048800000710     C                   parm                    prmfir
048900010330     C*
049000010330     C* CALCOLA LA DATA CORRENTE
049100010330     C                   time                    wn14             14 0
049200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
049300010330     C                   z-add     wn8           g08dat
049400010330     C                   z-add     *zeros        g08inv
049500010330     C                   movel     '0'           g08err
049600010330     C                   call      'XSRDA8'
049700010330     C                   parm                    wlbda8
049800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
049900000613     C*
050000000613     C                   ENDSR
050100000613     C***
050200050314** "SERIE" CLIENTE ALFA **
050300050314CF
050400050314** "SERIE" CLIENTE NUMERICA **
05050005031436
