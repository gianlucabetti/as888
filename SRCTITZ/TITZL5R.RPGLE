000100171023      /TITLE Upload via Internet: traduzione in EDIVATWR.
000200171023      *
000300171023      * PARTICOLARITA':
000400171023      *
000500171023      * copiato dal TITZG4R ma con gestione EDIVAB/VAT
000600171023      *
000700171023     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000800000313     F*
000900990910     Ftivin00r  uF   E             DISK    usropn
001000060221     FEDIVATwr  O    E             DISK    usropn
001100000313     D*
001200010330     D*----------------------------------------------------
001300010330     D* DICHIARAZIOINE VARIABILI DI WRK
001400010330     D*----------------------------------------------------
001500010330     D dscmz         e ds                  inz
001600010330     D psds           sds
001700010330     D  procname         *PROC
001800010330     D tivlrds       e ds                  extname(tivlr00f)
001900010330     D esito           s              1
002000010330     D prmlit          s             10
002100010330     D prmfir          s             10
002200010330     D wrkesito        s                   like(esito)
002300010330     D rrnum           s              6  0 INZ(*zeros)
002400060221     D wNomeFile       s             30    INZ(*blanks)
002500140328     D wNOT_E          s             35    INZ(*blanks)
002600140328     D wNOT_S          s             35    INZ(*blanks)
002700140328     D wNOT_I          s             35    INZ(*blanks)
002800140328     D wNOT_J          s             35    INZ(*blanks)
002900140328     D w70             s             70    INZ(*blanks)
003000010330     D*------------------
003100010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003200010330     D*------------------
003300010330     D WLBDA8          DS                  INZ
003400010330     D  G08DAT                 1      8  0
003500010330     D  G08INV                 9     16  0
003600010330     D  G08ERR                17     17
003700010330     D  G08TGI                18     22  0
003800000313     D*
003900010330
004000010330
004100010330
004200000913     C                   reset                   rrnum
004300990921     C                   reset                   esito
004400990921     C                   reset                   wrkesito
004500010601     C*
004600010601     C                   exsr      opeini
004700020321     C                   exsr      rwvat
004800010601     C*
004900010601     C                   seton                                        lr
005000010330
005100010601
005200010601
005300010330     C*--------------------------------------------------------
005400060221     C* RWVAT   LEGGE TIVIN00R E SCRIVE EDIVATWF              *
005500010330     C*--------------------------------------------------------
005600020321     C     RWVAT         BEGSR
005700010330     C*
005800010330     C                   if        not %open(tivin00r)
005900010330     C                   open      tivin00r
006000010330     C                   endif
006100060221     C                   if        not %open(edivatwr)
006200060221     C                   open      edivatwr
006300010330     C                   endif
006400010330     C*
006500010604     C                   clear                   §CTROK
006600010604     C                   clear                   §CTRMO
006700010604     C                   clear                   §CTRNO
006800010330     C*
006900010330     C                   DO        *HIVAL
007000010330     C*
007100010330     C                   READ      tivin00r                               70
007200010618     C*
007300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
007400010618     C                   if        vinflg = '1'
007500010618     C                   eval      flgOk = '1'
007600010618     C                   endif
007700010618     C*
007800010330     C                   if        vindta > *blanks
007900010330     C                   add       1             rrnum
008000010330     C*
008100010601     C                   if        *in70 = *off and
008200010330     C                             (vinflg = *blanks
008300010330     C                              or vinflg = '0'
008400010330     C                              or vinflg = '2')
008500010330     C*
008600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
008700010711     C                   if        vinflg = *blanks or vinflg = '0'
008800010711     C                   clear                   vinmsg
008900010711     C                   endif
009000010601     C*
009100010330     C                   exsr      inzvar
009200010330     C                   exsr      defcam
009300020321     C                   exsr      impvat
009400060221     C*
009500060221     C* VALORIZZO CAMPI RELATIVI AL "CMR"
009600170907     C***                EVAL      VATCMR =%char(datcor)+'-'+%char(oracor)
009700171023     C                   SELECT
009800171023     C                   WHEN      VATCCM = 0015517
009900171023     C                   EVAL      VATCMR ='LIMONI'
010000171023     C                   WHEN      VATCCM = 0018402
010100171023     C                   EVAL      VATCMR ='GARDENIA'
010200171023     C                   OTHER
010300171023     C                   EVAL      VATCMR ='???'
010400171023     C                   ENDSL
010500060221     C                   EVAL      VATCNT = 1
010600010601     C*
010700010604     C* Ebbene...
010800010604     C*
010900010604     C  N31              ADD       1             §CTROK            7 0
011000010604     C   32              ADD       1             §CTRMO            7 0
011100010604     C   31              ADD       1             §CTRNO            7 0
011200020205     C*
011300140328     C                   IF        *in31 = *off
011400140328     C                   IF        wNOT_I <> *blank
011500140328     C                   EVAL      VATTRC = 'I'
011600140328     C                   EVAL      VATNOT = wNOT_I
011700140328     C                   WRITE     EDIVAT00
011800140328     C                   ENDIF
011900140328     C                   IF        wNOT_J <> *blank
012000140328     C                   EVAL      VATTRC = 'J'
012100140328     C                   EVAL      VATNOT = wNOT_J
012200140328     C                   WRITE     EDIVAT00
012300140328     C                   ENDIF
012400140328     C                   IF        wNOT_S <> *blank
012500140328     C                   EVAL      VATTRC = 'S'
012600140328     C                   EVAL      VATNOT = wNOT_S
012700140328     C                   WRITE     EDIVAT00
012800140328     C                   ENDIF
012900140328     C                   IF        wNOT_E <> *blank
013000140328     C                   EVAL      VATTRC = 'E'
013100170822     C                   EVAL      VATNOT = %trimr(wNOT_E)
013200140328     C                   WRITE     EDIVAT00
013300140328     C                   ENDIF
013400140328     C                   ENDIF
013500010604     C*
013600010604     C                   if        *in31 = *off and
013700010604     C                             *in32 = *off
013800010604     C                   eval      vinflg = '1'
013900010604     C                   else
014000010604     C                   eval      vinflg = '2'
014100010604     C                   endif
014200010604     C                   endif
014300010604     C*
014400010330     C                   else
014500010330     C                   eval      vinflg = '1'
014600010330     C                   endif
014700010601     C*
014800010601     C  N70              update    tivin000
014900010330     C*
015000010330     C  N70              ENDdo
015100010601     C
015200010601     C*
015300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
015400010601     C                   if        cntNonEl = *zeros or
015500010601     C                             flgMulti = '0'
015600010330     C* Se non ci sono record con errori ...
015700010601     C                   if        §ctrno = 0 and
015800010604     C                             §ctrmo = 0 and
015900010601     C                             flgStato <> '2'
016000010330     C* ... restituisco esito OK.
016100010330     C                   eval      wrkesito = '0'
016200010330     C                   else
016300010330     C                   if        §ctrok > 0
016400010330     C                   eval      wrkesito = '1'
016500010330     C                   else
016600010615     C                   if        flgOk = '0'
016700010615     C                   eval      wrkesito = '2'
016800010615     C                   else
016900010615     C                   eval      wrkesito = '6'
017000010615     C                   endif
017100010330     C                   endif
017200010330     C                   endif
017300010601     C                   else
017400010601     C                   eval      wrkesito = '9'
017500010601     C                   endif
017600010330     C*
017700010330     C                   if        %open(tivin00r)
017800010330     C                   close     tivin00r
017900010330     C                   endif
018000060221     C                   if        %open(edivatwr)
018100060221     C                   close     edivatwr
018200010330     C                   endif
018300010330     C*
018400010330     C                   if        §ctrok > 0
018500171009     C                             and vlrpoi <> *zeros
018600010330     C                   exsr      invio
018700010330     C                   endif
018800010330     C*
018900010330     C                   ENDSR
019000010330     C***
019100010330
019200010601
019300010601
019400010601
019500010330     C*----------------------------------------------------*
019600010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019700010330     C*----------------------------------------------------*
019800040218     C     INZVAR        BEGSR
019900010330     C*
020000010330     C*
020100010330     C                   ENDSR
020200010330     C*----------------------------------------------------*
020300010330     C*  IMPOSTAZIONE CAMPI COSTANTI
020400010330     C*----------------------------------------------------*
020500010330     C     DEFCAM        BEGSR
020600010330     C*
020700020204     C* Inizializzo il buffer del record da scrivere
020800060221     C                   CLEAR                   EDIVAT00
020900020204     C* Imposto i valori di default...
021000171023     C                   Z-ADD     0015517       VATCCM
021100171023     C                   Z-ADD     275           VATLNP
021200020204     C* ... e poi verifico se sono stati passati come parametri
021300020204     C                   IF        vlrppt > *blanks
021400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
021500020204     C                   EXSR      CHKNUM
021600020204     C                   IF        PiInt=*on
021700020321     C                   Z-ADD     PiVal         VATCCM
021800020204     C                   ENDIF
021900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022000020204     C                   EXSR      CHKNUM
022100020204     C                   IF        PiInt=*on
022200020321     C                   Z-ADD     PiVal         VATLNP
022300020204     C                   ENDIF
022400020321     C                   ENDIF
022500020204     C*
022600010330     C                   ENDSR
022700010607     C*----------------------------------------------------*
022800060221     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
022900010607     C*----------------------------------------------------*
023000020321     C     IMPVAT        BEGSR
023100010607     C*
023200010607     C                   SETOFF                                       3132
023300040218     C*
023400040218     C                   MOVEL     datcor        VATAAS
023500170822     C*
023600170822     C* _E
023700171023     C                   EVAL      wNOT_E=%trim(%subst(vindta:31:18))
023800171006     C*
023900171030     C                   MOVE(P)   vlrpoi        VATFGS
024000171023     C*
024100171023     C* CCM
024200171023     C                   EVAL      PiStr=%trim(%subst(vindta:95:7))
024300171023     C                   EXSR      CHKNUM
024400171023     C                   IF        PiInt=*on
024500171023     C                   Z-ADD     PiVal         VATCCM
024600171023     C                   ELSE
024700171023     C                   Z-ADD     *zeros        VATCCM
024800171023     C                   SETON                                        32
024900171023     C                   EVAL      vinmsg = %trimr(vinmsg)
025000171023     C                             + ' ' + 'VATCCM'
025100171023     C                   ENDIF
025200020304     C*
025300040218     C* VATNSP
025400171023     C                   EVAL      PiStr=%trim(%subst(vindta:21:7))
025500020304     C                   EXSR      CHKNUM
025600020304     C                   IF        PiInt=*on
025700020321     C                   Z-ADD     PiVal         VATNSP
025800020304     C                   ELSE
025900020304     C                   SETON                                        32
026000020321     C                   Z-ADD     *zeros        VATNSP
026100020304     C                   EVAL      vinmsg = %trimr(vinmsg)
026200020321     C                             + ' ' + 'VATNSP'
026300020304     C                   ENDIF
026400020204     C*
026500010607     C                   ENDSR
026600010607     C*----------------------------------------------------*
026700010601
026800010330
026900010330     C*----------------------------------------------------*
027000010330     C*  CONTROLLO NUMERICITA' CAMPI
027100010330     C*----------------------------------------------------*
027200010330     C     CHKNUM        BEGSR
027300010330     C*
027400010606     C                   IF        PiDecChr = *blanks
027500010606     C                   EVAL      PiDecChr = ','
027600010606     C                   ENDIF
027700010606     C*
027800010606     C                   CALL(e)   'ISNUMERIC'
027900010330     C                   PARM                    PiStr            30
028000010606     C                   PARM                    PiDecChr          1
028100010330     C                   PARM      *ZEROS        PiVal            30 9
028200010330     C                   PARM      '0'           PiInt             1
028300010330     C                   PARM      '0'           PiNum             1
028400010330     C                   IF        %error
028500010606     C                   EVAL      PiNum=*off
028600010330     C                   ENDIF
028700010330     C*
028800010330     C                   ENDSR
028900010330     C***
029000010330
029100010330
029200010330
029300990920      /TITLE Invio dei dati al punto operativo.
029400000613     C     invio         BEGSR
029500990920     C*
029600990920     C                   reset                   dscmz
029700171009     C                   move      vlrpoi        cmzdst
029800990920     C                   eval      cmzfld = vlrfou
029900990920     C                   eval      cmzmbd = vlrhdl
030000990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
030100000710     C                   if        prmfir = *blanks
030200060221     C                   eval      cmzfla = 'EDIVAT0F'
030300060221     C                   eval      cmzmba = 'EDIVAT0F'
030400000710     C                   else
030500000710     C                   eval      cmzfla = prmfir
030600000710     C                   eval      cmzmba = prmfir
030700000710     C                   endif
030800990920     C                   eval      cmznrr = *zeros
030900990920     C                   move      §ctrok        cmznrr
031000021018     C                   eval      cmzlba = vlrfl1
031100990920     C                   call(e)   'TIS711C'
031200990920     C                   parm                    dscmz
031300990921     C                   parm      *blanks       esito
031400990920     C                   if        %error
031500990920     C                             or cmzerr = '1'
031600990921     C                             or esito  = '1'
031700000710     C                   eval      wrkesito = '3'
031800990920     C                   endif
031900990920     C*
032000000613     C                   ENDSR
032100990910
032200010601
032300010601
032400010601
032500010601
032600010601      /TITLE Invio dei dati al punto operativo.
032700010601     C     opeini        BEGSR
032800010601     C*
032900010601     C* Inizializzo flag e contatori operativi
033000010601     C                   movel     '0'           flgGiro           1
033100010601     C                   movel     '0'           flgMulti          1
033200010601     C                   movel     '1'           flgStato          1
033300010615     C                   movel     '0'           flgOk             1
033400010601     C                   z-add     *zeros        cntNonEl         10 0
033500010601     C*
033600010601     C                   ENDSR
033700010601     C***
033800010601
033900010601
034000010601
034100010330
034200010330
034300000613     C     *inzsr        BEGSR
034400990910     C*
034500990910     C     *entry        plist
034600990920     C                   parm                    tivlrds
034700990921     C                   parm      wrkesito      esito
034800000724     C                   parm                    prmlit
034900000710     C                   parm                    prmfir
035000010330     C*
035100010330     C* CALCOLA LA DATA CORRENTE
035200010330     C                   time                    wn14             14 0
035300170711     C                   movel     wn14          oracor            6 0          *ORA
035400010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
035500010330     C                   z-add     wn8           g08dat
035600010330     C                   z-add     *zeros        g08inv
035700010330     C                   movel     '0'           g08err
035800010330     C                   call      'XSRDA8'
035900010330     C                   parm                    wlbda8
036000010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
036100060221     C*
036200060221     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
036300060221     C                   move(p)   vlrMSG        wNomeFile
036400000613     C*
036500000613     C                   ENDSR
036600000613     C***
