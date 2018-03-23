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
001900060221     D wCMR            s             35    INZ(*blanks)
002000140328     D wNOT_E          s             35    INZ(*blanks)
002100140328     D wNOT_S          s             35    INZ(*blanks)
002200140328     D wNOT_I          s             35    INZ(*blanks)
002300140328     D wNOT_J          s             35    INZ(*blanks)
002400140328     D w70             s             70    INZ(*blanks)
002500010330     D*------------------
002600010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002700010330     D*------------------
002800010330     D WLBDA8          DS                  INZ
002900010330     D  G08DAT                 1      8  0
003000010330     D  G08INV                 9     16  0
003100010330     D  G08ERR                17     17
003200010330     D  G08TGI                18     22  0
003300000313     D*
003400010330
003500010330
003600010330
003700000913     C                   reset                   rrnum
003800990921     C                   reset                   esito
003900990921     C                   reset                   wrkesito
004000010601     C*
004100010601     C                   exsr      opeini
004200020321     C                   exsr      rwvat
004300010601     C*
004400010601     C                   seton                                        lr
004500010601
004600010601
004700010601
004800010601
004900010601     C*--------------------------------------------------------
005000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005100010601     C*--------------------------------------------------------
005200010601     C     PREELA        BEGSR
005300010601     C*
005400010601     C* SE OCCORRE SPEDIRE IN FILIALE
005500010601     C                   if        invfil <> *zeros and
005600010601     C                             flgGiro = '0'
005700010601     C*
005800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005900010601     C                   eval      flgGiro = '1'
006000010601     C*
006100010601     C                   endif
006200010601     C*
006300010601     C                   ENDSR
006400010601     C***
006500010601
006600010601
006700010601
006800010601
006900010601     C*--------------------------------------------------------
007000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007100010601     C*--------------------------------------------------------
007200010601     C     ENDELA        BEGSR
007300000616     C*
007400010601     C                   ENDSR
007500010601     C***
007600000613
007700010330
007800010601
007900010601
008000010330     C*--------------------------------------------------------
008100060221     C* RWVAT   LEGGE TIVIN00R E SCRIVE EDIVATWF              *
008200010330     C*--------------------------------------------------------
008300020321     C     RWVAT         BEGSR
008400010330     C*
008500010330     C                   if        not %open(tivin00r)
008600010330     C                   open      tivin00r
008700010330     C                   endif
008800060221     C                   if        not %open(edivatwr)
008900060221     C                   open      edivatwr
009000010330     C                   endif
009100010330     C*
009200010604     C                   clear                   §CTROK
009300010604     C                   clear                   §CTRMO
009400010604     C                   clear                   §CTRNO
009500010330     C*
009600010330     C                   DO        *HIVAL
009700010330     C*
009800010330     C                   READ      tivin00r                               70
009900010618     C*
010000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
010100010618     C                   if        vinflg = '1'
010200010618     C                   eval      flgOk = '1'
010300010618     C                   endif
010400010618     C*
010500010330     C                   if        vindta > *blanks
010600010330     C                   add       1             rrnum
010700010330     C*
010800010601     C                   if        *in70 = *off and
010900010330     C                             (vinflg = *blanks
011000010330     C                              or vinflg = '0'
011100010330     C                              or vinflg = '2')
011200010330     C*
011300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011400010711     C                   if        vinflg = *blanks or vinflg = '0'
011500010711     C                   clear                   vinmsg
011600010711     C                   endif
011700010601     C*
011800010330     C                   exsr      inzvar
011900010330     C                   exsr      defcam
012000020321     C                   exsr      impvat
012100010601     C*
012200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012300020321     C                   eval      depfil = VATLNP
012400010601     C                   exsr      repfil
012500140328     C                   if        depfil = invfil
012600021025     C                   if        vlrpoi = 999
012700021025     C                   MOVE(P)   invfil        VATFGS
012800021025     C                   else
012900021025     C                   MOVE(P)   vlrpoi        VATFGS
013000021025     C                   endif
013100010601     C*
013200010601     C                   exsr      PREELA
013300060221     C*
013400060221     C* VALORIZZO CAMPI RELATIVI AL "CMR"
013500060221     C                   EVAL      VATCMR = wCMR
013600060221     C                   EVAL      VATCNT = 1
013700010601     C*
013800010604     C* Ebbene...
013900010604     C*
014000010604     C  N31              ADD       1             §CTROK            7 0
014100010604     C   32              ADD       1             §CTRMO            7 0
014200010604     C   31              ADD       1             §CTRNO            7 0
014300020205     C*
014400140328     C                   IF        *in31 = *off
014500140328     C                   IF        wNOT_I <> *blank
014600140328     C                   EVAL      VATTRC = 'I'
014700140328     C                   EVAL      VATNOT = wNOT_I
014800140328     C                   WRITE     EDIVAT00
014900140328     C                   ENDIF
015000140328     C                   IF        wNOT_J <> *blank
015100140328     C                   EVAL      VATTRC = 'J'
015200140328     C                   EVAL      VATNOT = wNOT_J
015300140328     C                   WRITE     EDIVAT00
015400140328     C                   ENDIF
015500140328     C                   IF        wNOT_S <> *blank
015600140328     C                   EVAL      VATTRC = 'S'
015700140328     C                   EVAL      VATNOT = wNOT_S
015800140328     C                   WRITE     EDIVAT00
015900140328     C                   ENDIF
016000140328     C                   IF        wNOT_E <> *blank
016100140328     C                   EVAL      VATTRC = 'E'
016200170906     C                   EVAL      VATNOT = wNOT_E
016300140328     C                   WRITE     EDIVAT00
016400140328     C                   ENDIF
016500140328     C                   ENDIF
016600010604     C*
016700010604     C                   if        *in31 = *off and
016800010604     C                             *in32 = *off
016900010604     C                   eval      vinflg = '1'
017000010604     C                   else
017100010604     C                   eval      vinflg = '2'
017200010604     C                   endif
017300010604     C                   endif
017400010601     C*
017500010604     C                   endif
017600010604     C*
017700010330     C                   else
017800010330     C                   eval      vinflg = '1'
017900010330     C                   endif
018000010601     C*
018100010601     C  N70              update    tivin000
018200010330     C*
018300010330     C  N70              ENDdo
018400010601     C
018500010601     C*
018600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
018700010601     C                   if        cntNonEl = *zeros or
018800010601     C                             flgMulti = '0'
018900010330     C* Se non ci sono record con errori ...
019000010601     C                   if        §ctrno = 0 and
019100010604     C                             §ctrmo = 0 and
019200010601     C                             flgStato <> '2'
019300010330     C* ... restituisco esito OK.
019400010330     C                   eval      wrkesito = '0'
019500010330     C                   else
019600010330     C                   if        §ctrok > 0
019700010330     C                   eval      wrkesito = '1'
019800010330     C                   else
019900010615     C                   if        flgOk = '0'
020000010615     C                   eval      wrkesito = '2'
020100010615     C                   else
020200010615     C                   eval      wrkesito = '6'
020300010615     C                   endif
020400010330     C                   endif
020500010330     C                   endif
020600010601     C                   else
020700010601     C                   eval      wrkesito = '9'
020800010601     C                   endif
020900010330     C*
021000010330     C                   if        %open(tivin00r)
021100010330     C                   close     tivin00r
021200010330     C                   endif
021300060221     C                   if        %open(edivatwr)
021400060221     C                   close     edivatwr
021500010330     C                   endif
021600010601     C*
021700010601     C                   if        vlrpoi <> 999
021800010601     C                   eval      invfil = vlrpoi
021900010601     C                   endif
022000010330     C*
022100010330     C                   if        §ctrok > 0
022200010601     C                             and invfil > *zeros
022300010330     C                   exsr      invio
022400010330     C                   endif
022500010601     C*
022600010618     C                   if        flgGiro = '1'
022700010601     C                   exsr      endela
022800010618     C                   endif
022900010330     C*
023000010330     C                   ENDSR
023100010330     C***
023200010330
023300010601
023400010601
023500010601
023600010330     C*----------------------------------------------------*
023700010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
023800010330     C*----------------------------------------------------*
023900040218     C     INZVAR        BEGSR
024000010330     C*
024100010330     C*
024200010330     C                   ENDSR
024300010330     C*----------------------------------------------------*
024400010330     C*  IMPOSTAZIONE CAMPI COSTANTI
024500010330     C*----------------------------------------------------*
024600010330     C     DEFCAM        BEGSR
024700010330     C*
024800020204     C* Inizializzo il buffer del record da scrivere
024900060221     C                   CLEAR                   EDIVAT00
025000020204     C* Imposto i valori di default...
025100170906     C                   EVAL      VATCCM = 1663914
025200170906     C                   EVAL      VATLNP = 166
025300170711     C***                EVAL      VATTRC = 'E'
025400020204     C* ... e poi verifico se sono stati passati come parametri
025500020204     C                   IF        vlrppt > *blanks
025600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
025700020204     C                   EXSR      CHKNUM
025800020204     C                   IF        PiInt=*on
025900020321     C                   Z-ADD     PiVal         VATCCM
026000020204     C                   ENDIF
026100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
026200020204     C                   EXSR      CHKNUM
026300020204     C                   IF        PiInt=*on
026400020321     C                   Z-ADD     PiVal         VATLNP
026500020204     C                   ENDIF
026600020321     C                   ENDIF
026700020204     C*
026800010330     C                   ENDSR
026900010607     C*----------------------------------------------------*
027000060221     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
027100010607     C*----------------------------------------------------*
027200020321     C     IMPVAT        BEGSR
027300010607     C*
027400010607     C                   SETOFF                                       3132
027500040218     C*
027600040218     C                   MOVEL     datcor        VATAAS
027700010607     C*
027800010607     C* Reperimento campi ALFA
027900140328     C* _E
028000170906     C                   EVAL      wNOT_E=%trim(%subst(vindta:21:15))
028100020304     C*
028200040218     C* VATNSP
028300170906     C                   EVAL      PiStr=%trim(%subst(vindta:1:15))
028400020304     C                   EXSR      CHKNUM
028500020304     C                   IF        PiInt=*on
028600020321     C                   Z-ADD     PiVal         VATNSP
028700020304     C                   ELSE
028800020304     C                   SETON                                        32
028900020321     C                   Z-ADD     *zeros        VATNSP
029000020304     C                   EVAL      vinmsg = %trimr(vinmsg)
029100020321     C                             + ' ' + 'VATNSP'
029200020304     C                   ENDIF
029300020204     C*
029400010607     C                   ENDSR
029500010607     C*----------------------------------------------------*
029600010601
029700010330
029800010330     C*----------------------------------------------------*
029900010330     C*  CONTROLLO NUMERICITA' CAMPI
030000010330     C*----------------------------------------------------*
030100010330     C     CHKNUM        BEGSR
030200010330     C*
030300010606     C                   IF        PiDecChr = *blanks
030400010606     C                   EVAL      PiDecChr = ','
030500010606     C                   ENDIF
030600010606     C*
030700010606     C                   CALL(e)   'ISNUMERIC'
030800010330     C                   PARM                    PiStr            30
030900010606     C                   PARM                    PiDecChr          1
031000010330     C                   PARM      *ZEROS        PiVal            30 9
031100010330     C                   PARM      '0'           PiInt             1
031200010330     C                   PARM      '0'           PiNum             1
031300010330     C                   IF        %error
031400010606     C                   EVAL      PiNum=*off
031500010330     C                   ENDIF
031600010330     C*
031700010330     C                   ENDSR
031800010330     C***
031900020403
032000020403
032100010601
032200010601
032300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
032400010601     C     repfil        BEGSR
032500010601     C*
032600010601     C                   if        invfil = *zeros and
032700010601     C                             depfil > *zeros and
032800010629     C                             (vinflg = *blanks or
032900010629     C                              vinflg = *zeros)
033000010601     C
033100010601     C                   eval      invfil = depfil
033200010601     C                   endif
033300010601     C*
033400010601     C                   if        depfil <> invfil and
033500010601     C                             invfil > *zeros
033600010601     C                   eval      flgMulti = '1'
033700010601     C                   if        vinflg = *blanks
033800010601     C                   add       1             cntNonEl
033900010601     C                   endif
034000010601     C                   endif
034100010601     C*
034200010601     C                   if        vinflg = '2'
034300010601     C                   eval      flgStato = '2'
034400010601     C                   endif
034500010601     C*
034600010601     C                   ENDSR
034700010601     C***
034800010601
034900010601
035000010601
035100010330
035200010330
035300010330
035400990920      /TITLE Invio dei dati al punto operativo.
035500000613     C     invio         BEGSR
035600990920     C*
035700990920     C                   reset                   dscmz
035800010601     C                   move      invfil        cmzdst
035900990920     C                   eval      cmzfld = vlrfou
036000990920     C                   eval      cmzmbd = vlrhdl
036100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
036200000710     C                   if        prmfir = *blanks
036300060221     C                   eval      cmzfla = 'EDIVAT0F'
036400060221     C                   eval      cmzmba = 'EDIVAT0F'
036500000710     C                   else
036600000710     C                   eval      cmzfla = prmfir
036700000710     C                   eval      cmzmba = prmfir
036800000710     C                   endif
036900990920     C                   eval      cmznrr = *zeros
037000990920     C                   move      §ctrok        cmznrr
037100021018     C                   eval      cmzlba = vlrfl1
037200990920     C                   call(e)   'TIS711C'
037300990920     C                   parm                    dscmz
037400990921     C                   parm      *blanks       esito
037500990920     C                   if        %error
037600990920     C                             or cmzerr = '1'
037700990921     C                             or esito  = '1'
037800000710     C                   eval      wrkesito = '3'
037900990920     C                   endif
038000990920     C*
038100000613     C                   ENDSR
038200990910
038300010601
038400010601
038500010601
038600010601
038700010601      /TITLE Invio dei dati al punto operativo.
038800010601     C     opeini        BEGSR
038900010601     C*
039000010601     C* Inizializzo flag e contatori operativi
039100010601     C                   movel     '0'           flgGiro           1
039200010601     C                   movel     '0'           flgMulti          1
039300010601     C                   movel     '1'           flgStato          1
039400010615     C                   movel     '0'           flgOk             1
039500010601     C                   z-add     *zeros        cntNonEl         10 0
039600010601     C                   z-add     *zeros        depfil            3 0
039700010601     C                   z-add     *zeros        invfil            3 0
039800010601     C*
039900010601     C                   ENDSR
040000010601     C***
040100010601
040200010601
040300010601
040400010330
040500010330
040600000613     C     *inzsr        BEGSR
040700990910     C*
040800990910     C     *entry        plist
040900990920     C                   parm                    tivlrds
041000990921     C                   parm      wrkesito      esito
041100000724     C                   parm                    prmlit
041200000710     C                   parm                    prmfir
041300010330     C*
041400010330     C* CALCOLA LA DATA CORRENTE
041500010330     C                   time                    wn14             14 0
041600170711     C                   movel     wn14          oracor            6 0          *ORA
041700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
041800010330     C                   z-add     wn8           g08dat
041900010330     C                   z-add     *zeros        g08inv
042000010330     C                   movel     '0'           g08err
042100010330     C                   call      'XSRDA8'
042200010330     C                   parm                    wlbda8
042300010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
042400060221     C*
042500060221     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
042600060221     C                   move(p)   vlrMSG        wNomeFile
042700060221     C*
042800170711     C* ATTRIBUISCO UN CMR FISSO
042900170906     C                   EVAL      WCMR ='HASBRO'
043000000613     C*
043100000613     C                   ENDSR
043200000613     C***
