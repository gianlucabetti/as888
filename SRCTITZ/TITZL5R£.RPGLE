000100990908     H dftactgrp(*yes)
000200000313     F*
000300990910     Ftivin00r  uF   E             DISK    usropn
000400060221     FEDIVATwr  O    E             DISK    usropn
000500000313     D*
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
001800060221     D wNomeFile       s             30    INZ(*blanks)
001900140328     D wNOT_E          s             35    INZ(*blanks)
002000140328     D wNOT_S          s             35    INZ(*blanks)
002100140328     D wNOT_I          s             35    INZ(*blanks)
002200140328     D wNOT_J          s             35    INZ(*blanks)
002300140328     D w70             s             70    INZ(*blanks)
002400010330     D*------------------
002500010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002600010330     D*------------------
002700010330     D WLBDA8          DS                  INZ
002800010330     D  G08DAT                 1      8  0
002900010330     D  G08INV                 9     16  0
003000010330     D  G08ERR                17     17
003100010330     D  G08TGI                18     22  0
003200000313     D*
003300010330
003400010330
003500010330
003600000913     C                   reset                   rrnum
003700990921     C                   reset                   esito
003800990921     C                   reset                   wrkesito
003900010601     C*
004000010601     C                   exsr      opeini
004100020321     C                   exsr      rwvat
004200010601     C*
004300010601     C                   seton                                        lr
004400010330
004500010601
004600010601
004700010330     C*--------------------------------------------------------
004800060221     C* RWVAT   LEGGE TIVIN00R E SCRIVE EDIVATWF              *
004900010330     C*--------------------------------------------------------
005000020321     C     RWVAT         BEGSR
005100010330     C*
005200010330     C                   if        not %open(tivin00r)
005300010330     C                   open      tivin00r
005400010330     C                   endif
005500060221     C                   if        not %open(edivatwr)
005600060221     C                   open      edivatwr
005700010330     C                   endif
005800010330     C*
005900010604     C                   clear                   §CTROK
006000010604     C                   clear                   §CTRMO
006100010604     C                   clear                   §CTRNO
006200010330     C*
006300010330     C                   DO        *HIVAL
006400010330     C*
006500010330     C                   READ      tivin00r                               70
006600010618     C*
006700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
006800010618     C                   if        vinflg = '1'
006900010618     C                   eval      flgOk = '1'
007000010618     C                   endif
007100010618     C*
007200010330     C                   if        vindta > *blanks
007300010330     C                   add       1             rrnum
007400010330     C*
007500010601     C                   if        *in70 = *off and
007600010330     C                             (vinflg = *blanks
007700010330     C                              or vinflg = '0'
007800010330     C                              or vinflg = '2')
007900010330     C*
008000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
008100010711     C                   if        vinflg = *blanks or vinflg = '0'
008200010711     C                   clear                   vinmsg
008300010711     C                   endif
008400010601     C*
008500010330     C                   exsr      inzvar
008600010330     C                   exsr      defcam
008700020321     C                   exsr      impvat
008800060221     C*
008900060221     C* VALORIZZO CAMPI RELATIVI AL "CMR"
009000170907     C***                EVAL      VATCMR =%char(datcor)+'-'+%char(oracor)
009100170907     C                   EVAL      VATCMR ='KASANOVA'
009200060221     C                   EVAL      VATCNT = 1
009300010601     C*
009400010604     C* Ebbene...
009500010604     C*
009600010604     C  N31              ADD       1             §CTROK            7 0
009700010604     C   32              ADD       1             §CTRMO            7 0
009800010604     C   31              ADD       1             §CTRNO            7 0
009900020205     C*
010000140328     C                   IF        *in31 = *off
010100140328     C                   IF        wNOT_I <> *blank
010200140328     C                   EVAL      VATTRC = 'I'
010300140328     C                   EVAL      VATNOT = wNOT_I
010400140328     C                   WRITE     EDIVAT00
010500140328     C                   ENDIF
010600140328     C                   IF        wNOT_J <> *blank
010700140328     C                   EVAL      VATTRC = 'J'
010800140328     C                   EVAL      VATNOT = wNOT_J
010900140328     C                   WRITE     EDIVAT00
011000140328     C                   ENDIF
011100140328     C                   IF        wNOT_S <> *blank
011200140328     C                   EVAL      VATTRC = 'S'
011300140328     C                   EVAL      VATNOT = wNOT_S
011400140328     C                   WRITE     EDIVAT00
011500140328     C                   ENDIF
011600140328     C                   IF        wNOT_E <> *blank
011700140328     C                   EVAL      VATTRC = 'E'
011800170822     C                   EVAL      VATNOT = %trimr(wNOT_E)
011900140328     C                   WRITE     EDIVAT00
012000140328     C                   ENDIF
012100140328     C                   ENDIF
012200010604     C*
012300010604     C                   if        *in31 = *off and
012400010604     C                             *in32 = *off
012500010604     C                   eval      vinflg = '1'
012600010604     C                   else
012700010604     C                   eval      vinflg = '2'
012800010604     C                   endif
012900010604     C                   endif
013000010604     C*
013100010330     C                   else
013200010330     C                   eval      vinflg = '1'
013300010330     C                   endif
013400010601     C*
013500010601     C  N70              update    tivin000
013600010330     C*
013700010330     C  N70              ENDdo
013800010601     C
013900010601     C*
014000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
014100010601     C                   if        cntNonEl = *zeros or
014200010601     C                             flgMulti = '0'
014300010330     C* Se non ci sono record con errori ...
014400010601     C                   if        §ctrno = 0 and
014500010604     C                             §ctrmo = 0 and
014600010601     C                             flgStato <> '2'
014700010330     C* ... restituisco esito OK.
014800010330     C                   eval      wrkesito = '0'
014900010330     C                   else
015000010330     C                   if        §ctrok > 0
015100010330     C                   eval      wrkesito = '1'
015200010330     C                   else
015300010615     C                   if        flgOk = '0'
015400010615     C                   eval      wrkesito = '2'
015500010615     C                   else
015600010615     C                   eval      wrkesito = '6'
015700010615     C                   endif
015800010330     C                   endif
015900010330     C                   endif
016000010601     C                   else
016100010601     C                   eval      wrkesito = '9'
016200010601     C                   endif
016300010330     C*
016400010330     C                   if        %open(tivin00r)
016500010330     C                   close     tivin00r
016600010330     C                   endif
016700060221     C                   if        %open(edivatwr)
016800060221     C                   close     edivatwr
016900010330     C                   endif
017000010330     C*
017100010330     C                   if        §ctrok > 0
017200171009     C                             and vlrpoi <> *zeros
017300010330     C                   exsr      invio
017400010330     C                   endif
017500010330     C*
017600010330     C                   ENDSR
017700010330     C***
017800010330
017900010601
018000010601
018100010601
018200010330     C*----------------------------------------------------*
018300010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
018400010330     C*----------------------------------------------------*
018500040218     C     INZVAR        BEGSR
018600010330     C*
018700010330     C*
018800010330     C                   ENDSR
018900010330     C*----------------------------------------------------*
019000010330     C*  IMPOSTAZIONE CAMPI COSTANTI
019100010330     C*----------------------------------------------------*
019200010330     C     DEFCAM        BEGSR
019300010330     C*
019400020204     C* Inizializzo il buffer del record da scrivere
019500060221     C                   CLEAR                   EDIVAT00
019600020204     C* Imposto i valori di default...
019700170822     C                   EVAL      VATCCM = 2495207
019800170822     C                   EVAL      VATLNP = 249
019900020204     C* ... e poi verifico se sono stati passati come parametri
020000020204     C                   IF        vlrppt > *blanks
020100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
020200020204     C                   EXSR      CHKNUM
020300020204     C                   IF        PiInt=*on
020400020321     C                   Z-ADD     PiVal         VATCCM
020500020204     C                   ENDIF
020600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
020700020204     C                   EXSR      CHKNUM
020800020204     C                   IF        PiInt=*on
020900020321     C                   Z-ADD     PiVal         VATLNP
021000020204     C                   ENDIF
021100020321     C                   ENDIF
021200020204     C*
021300010330     C                   ENDSR
021400010607     C*----------------------------------------------------*
021500060221     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
021600010607     C*----------------------------------------------------*
021700020321     C     IMPVAT        BEGSR
021800010607     C*
021900010607     C                   SETOFF                                       3132
022000040218     C*
022100040218     C                   MOVEL     datcor        VATAAS
022200170822     C*
022300170822     C* _E
022400170822     C                   EVAL      wNOT_E=%trim(%subst(vindta:117:16))
022500170822     C*
022600170711     C* CCM
022700170822     C                   SELECT
022800170907     C                   WHEN      %trim(%subst(vindta:45:3)) = 'DDT'
022900170822     C* uso il valore di default
023000170907     C                   WHEN      %trim(%subst(vindta:45:3)) = 'TSP'
023100170822     C                   EVAL      VATCCM=2495829
023200171006     C                   EVAL      VATLNP=249
023300170926     C                   WHEN      %trim(%subst(vindta:45:3)) = 'MGT'
023400170926     C                   EVAL      VATCCM=2495848
023500171006     C                   EVAL      VATLNP=249
023600171009     C                   WHEN      %trim(%subst(vindta:45:3)) = 'MGD'
023700171009     C                   EVAL      VATCCM=2495853
023800171009     C                   EVAL      VATLNP=249
023900171006     C                   WHEN      %trim(%subst(vindta:45:3)) = 'COT'
024000171006     C                   EVAL      VATCCM=0899623
024100171006     C                   EVAL      VATLNP=089
024200171006     C                   WHEN      %trim(%subst(vindta:45:3)) = 'COD'
024300171006     C                   EVAL      VATCCM=0899068
024400171006     C                   EVAL      VATLNP=089
024500171009     C                   WHEN      %trim(%subst(vindta:43:3)) = 'TUV'
024600171006     C                   EVAL      VATCCM=0898676
024700171009     C                   EVAL      VATLNP=202
024800170822     C                   OTHER
024900170822     C* altrimenti ignoro il rcd non valorizzando il chi sono
025000170822     C                   EVAL      wNOT_E=*blank
025100170822     C                   ENDSL
025200171006     C*
025300171006     C* Forzatura su FGS
025400171006     C                   EVAL      VATFGS = VATLNP
025500020304     C*
025600040218     C* VATNSP
025700170907     C                   EVAL      PiStr=%trim(%subst(vindta:48:6))
025800020304     C                   EXSR      CHKNUM
025900020304     C                   IF        PiInt=*on
026000020321     C                   Z-ADD     PiVal         VATNSP
026100020304     C                   ELSE
026200020304     C                   SETON                                        32
026300020321     C                   Z-ADD     *zeros        VATNSP
026400020304     C                   EVAL      vinmsg = %trimr(vinmsg)
026500020321     C                             + ' ' + 'VATNSP'
026600020304     C                   ENDIF
026700020204     C*
026800010607     C                   ENDSR
026900010607     C*----------------------------------------------------*
027000010601
027100010330
027200010330     C*----------------------------------------------------*
027300010330     C*  CONTROLLO NUMERICITA' CAMPI
027400010330     C*----------------------------------------------------*
027500010330     C     CHKNUM        BEGSR
027600010330     C*
027700010606     C                   IF        PiDecChr = *blanks
027800010606     C                   EVAL      PiDecChr = ','
027900010606     C                   ENDIF
028000010606     C*
028100010606     C                   CALL(e)   'ISNUMERIC'
028200010330     C                   PARM                    PiStr            30
028300010606     C                   PARM                    PiDecChr          1
028400010330     C                   PARM      *ZEROS        PiVal            30 9
028500010330     C                   PARM      '0'           PiInt             1
028600010330     C                   PARM      '0'           PiNum             1
028700010330     C                   IF        %error
028800010606     C                   EVAL      PiNum=*off
028900010330     C                   ENDIF
029000010330     C*
029100010330     C                   ENDSR
029200010330     C***
029300010330
029400010330
029500010330
029600990920      /TITLE Invio dei dati al punto operativo.
029700000613     C     invio         BEGSR
029800990920     C*
029900990920     C                   reset                   dscmz
030000171009     C                   move      vlrpoi        cmzdst
030100990920     C                   eval      cmzfld = vlrfou
030200990920     C                   eval      cmzmbd = vlrhdl
030300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
030400000710     C                   if        prmfir = *blanks
030500060221     C                   eval      cmzfla = 'EDIVAT0F'
030600060221     C                   eval      cmzmba = 'EDIVAT0F'
030700000710     C                   else
030800000710     C                   eval      cmzfla = prmfir
030900000710     C                   eval      cmzmba = prmfir
031000000710     C                   endif
031100990920     C                   eval      cmznrr = *zeros
031200990920     C                   move      §ctrok        cmznrr
031300021018     C                   eval      cmzlba = vlrfl1
031400990920     C                   call(e)   'TIS711C'
031500990920     C                   parm                    dscmz
031600990921     C                   parm      *blanks       esito
031700990920     C                   if        %error
031800990920     C                             or cmzerr = '1'
031900990921     C                             or esito  = '1'
032000000710     C                   eval      wrkesito = '3'
032100990920     C                   endif
032200990920     C*
032300000613     C                   ENDSR
032400990910
032500010601
032600010601
032700010601
032800010601
032900010601      /TITLE Invio dei dati al punto operativo.
033000010601     C     opeini        BEGSR
033100010601     C*
033200010601     C* Inizializzo flag e contatori operativi
033300010601     C                   movel     '0'           flgGiro           1
033400010601     C                   movel     '0'           flgMulti          1
033500010601     C                   movel     '1'           flgStato          1
033600010615     C                   movel     '0'           flgOk             1
033700010601     C                   z-add     *zeros        cntNonEl         10 0
033800010601     C*
033900010601     C                   ENDSR
034000010601     C***
034100010601
034200010601
034300010601
034400010330
034500010330
034600000613     C     *inzsr        BEGSR
034700990910     C*
034800990910     C     *entry        plist
034900990920     C                   parm                    tivlrds
035000990921     C                   parm      wrkesito      esito
035100000724     C                   parm                    prmlit
035200000710     C                   parm                    prmfir
035300010330     C*
035400010330     C* CALCOLA LA DATA CORRENTE
035500010330     C                   time                    wn14             14 0
035600170711     C                   movel     wn14          oracor            6 0          *ORA
035700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
035800010330     C                   z-add     wn8           g08dat
035900010330     C                   z-add     *zeros        g08inv
036000010330     C                   movel     '0'           g08err
036100010330     C                   call      'XSRDA8'
036200010330     C                   parm                    wlbda8
036300010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
036400060221     C*
036500060221     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
036600060221     C                   move(p)   vlrMSG        wNomeFile
036700000613     C*
036800000613     C                   ENDSR
036900000613     C***
