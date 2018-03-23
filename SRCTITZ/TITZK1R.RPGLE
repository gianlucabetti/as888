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
002500170711     D wNCL            s              3s 0 INZ(0)
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
007000010601     C*--------------------------------------------------------
007100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007200010601     C*--------------------------------------------------------
007300010601     C     ENDELA        BEGSR
007400000616     C*
007500010601     C                   ENDSR
007600010601     C***
007700000613
007800010330
007900010601
008000010601
008100010330     C*--------------------------------------------------------
008200060221     C* RWVAT   LEGGE TIVIN00R E SCRIVE EDIVATWF              *
008300010330     C*--------------------------------------------------------
008400020321     C     RWVAT         BEGSR
008500010330     C*
008600010330     C                   if        not %open(tivin00r)
008700010330     C                   open      tivin00r
008800010330     C                   endif
008900060221     C                   if        not %open(edivatwr)
009000060221     C                   open      edivatwr
009100010330     C                   endif
009200010330     C*
009300010604     C                   clear                   §CTROK
009400010604     C                   clear                   §CTRMO
009500010604     C                   clear                   §CTRNO
009600010330     C*
009700010330     C                   DO        *HIVAL
009800010330     C*
009900010330     C                   READ      tivin00r                               70
010000010618     C*
010100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
010200010618     C                   if        vinflg = '1'
010300010618     C                   eval      flgOk = '1'
010400010618     C                   endif
010500010618     C*
010600010330     C                   if        vindta > *blanks
010700010330     C                   add       1             rrnum
010800010330     C*
010900010601     C                   if        *in70 = *off and
011000010330     C                             (vinflg = *blanks
011100010330     C                              or vinflg = '0'
011200010330     C                              or vinflg = '2')
011300010330     C*
011400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011500010711     C                   if        vinflg = *blanks or vinflg = '0'
011600010711     C                   clear                   vinmsg
011700010711     C                   endif
011800010601     C*
011900010330     C                   exsr      inzvar
012000010330     C                   exsr      defcam
012100020321     C                   exsr      impvat
012200010601     C*
012300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012400020321     C                   eval      depfil = VATLNP
012500010601     C                   exsr      repfil
012600140328     C                   if        depfil = invfil
012700021025     C                   if        vlrpoi = 999
012800021025     C                   MOVE(P)   invfil        VATFGS
012900021025     C                   else
013000021025     C                   MOVE(P)   vlrpoi        VATFGS
013100021025     C                   endif
013200010601     C*
013300010601     C                   exsr      PREELA
013400060221     C*
013500060221     C* VALORIZZO CAMPI RELATIVI AL "CMR"
013600060221     C                   EVAL      VATCMR = wCMR
013700060221     C                   EVAL      VATCNT = 1
013800010601     C*
013900010604     C* Ebbene...
014000010604     C*
014100010604     C  N31              ADD       1             §CTROK            7 0
014200010604     C   32              ADD       1             §CTRMO            7 0
014300010604     C   31              ADD       1             §CTRNO            7 0
014400020205     C*
014500140328     C                   IF        *in31 = *off
014600140328     C                   IF        wNOT_I <> *blank
014700140328     C                   EVAL      VATTRC = 'I'
014800140328     C                   EVAL      VATNOT = wNOT_I
014900140328     C                   WRITE     EDIVAT00
015000140328     C                   ENDIF
015100140328     C                   IF        wNOT_J <> *blank
015200140328     C                   EVAL      VATTRC = 'J'
015300140328     C                   EVAL      VATNOT = wNOT_J
015400140328     C                   WRITE     EDIVAT00
015500140328     C                   ENDIF
015600140328     C                   IF        wNOT_S <> *blank
015700140328     C                   EVAL      VATTRC = 'S'
015800140328     C                   EVAL      VATNOT = wNOT_S
015900140328     C                   WRITE     EDIVAT00
016000140328     C                   ENDIF
016100140328     C                   IF        wNOT_E <> *blank
016200140328     C                   EVAL      VATTRC = 'E'
016300170711     C*
016400170711     C* Sviluppati i "CHI SONO" in relazione al numero colli
016500170711     C*
016600170711     C     1             DO        wNCL          wNumCollo         3 0
016700170711     C*** da analisi del POC il progressivo colli è max 3 char e deve contenere gli zeri non
016800170711     C*** significativi
016900170711     C                   EVAL      VATNOT = %trimr(wNOT_E) +
017000170711     C                                %editc(wNumCollo:'X')
017100140328     C                   WRITE     EDIVAT00
017200170711     C                   ENDDO
017300140328     C                   ENDIF
017400140328     C                   ENDIF
017500010604     C*
017600010604     C                   if        *in31 = *off and
017700010604     C                             *in32 = *off
017800010604     C                   eval      vinflg = '1'
017900010604     C                   else
018000010604     C                   eval      vinflg = '2'
018100010604     C                   endif
018200010604     C                   endif
018300010601     C*
018400010604     C                   endif
018500010604     C*
018600010330     C                   else
018700010330     C                   eval      vinflg = '1'
018800010330     C                   endif
018900010601     C*
019000010601     C  N70              update    tivin000
019100010330     C*
019200010330     C  N70              ENDdo
019300010601     C
019400010601     C*
019500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
019600010601     C                   if        cntNonEl = *zeros or
019700010601     C                             flgMulti = '0'
019800010330     C* Se non ci sono record con errori ...
019900010601     C                   if        §ctrno = 0 and
020000010604     C                             §ctrmo = 0 and
020100010601     C                             flgStato <> '2'
020200010330     C* ... restituisco esito OK.
020300010330     C                   eval      wrkesito = '0'
020400010330     C                   else
020500010330     C                   if        §ctrok > 0
020600010330     C                   eval      wrkesito = '1'
020700010330     C                   else
020800010615     C                   if        flgOk = '0'
020900010615     C                   eval      wrkesito = '2'
021000010615     C                   else
021100010615     C                   eval      wrkesito = '6'
021200010615     C                   endif
021300010330     C                   endif
021400010330     C                   endif
021500010601     C                   else
021600010601     C                   eval      wrkesito = '9'
021700010601     C                   endif
021800010330     C*
021900010330     C                   if        %open(tivin00r)
022000010330     C                   close     tivin00r
022100010330     C                   endif
022200060221     C                   if        %open(edivatwr)
022300060221     C                   close     edivatwr
022400010330     C                   endif
022500010601     C*
022600010601     C                   if        vlrpoi <> 999
022700010601     C                   eval      invfil = vlrpoi
022800010601     C                   endif
022900010330     C*
023000010330     C                   if        §ctrok > 0
023100010601     C                             and invfil > *zeros
023200010330     C                   exsr      invio
023300010330     C                   endif
023400010601     C*
023500010618     C                   if        flgGiro = '1'
023600010601     C                   exsr      endela
023700010618     C                   endif
023800010330     C*
023900010330     C                   ENDSR
024000010330     C***
024100010330
024200010601
024300010601
024400010601
024500010330     C*----------------------------------------------------*
024600010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
024700010330     C*----------------------------------------------------*
024800040218     C     INZVAR        BEGSR
024900010330     C*
025000010330     C*
025100010330     C                   ENDSR
025200010330     C*----------------------------------------------------*
025300010330     C*  IMPOSTAZIONE CAMPI COSTANTI
025400010330     C*----------------------------------------------------*
025500010330     C     DEFCAM        BEGSR
025600010330     C*
025700020204     C* Inizializzo il buffer del record da scrivere
025800060221     C                   CLEAR                   EDIVAT00
025900020204     C* Imposto i valori di default...
026000170711     C                   EVAL      VATCCM = 0057811
026100150212     C                   EVAL      VATLNP = 005
026200170711     C***                EVAL      VATTRC = 'E'
026300020204     C* ... e poi verifico se sono stati passati come parametri
026400020204     C                   IF        vlrppt > *blanks
026500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
026600020204     C                   EXSR      CHKNUM
026700020204     C                   IF        PiInt=*on
026800020321     C                   Z-ADD     PiVal         VATCCM
026900020204     C                   ENDIF
027000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
027100020204     C                   EXSR      CHKNUM
027200020204     C                   IF        PiInt=*on
027300020321     C                   Z-ADD     PiVal         VATLNP
027400020204     C                   ENDIF
027500020321     C                   ENDIF
027600020204     C*
027700010330     C                   ENDSR
027800010607     C*----------------------------------------------------*
027900060221     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
028000010607     C*----------------------------------------------------*
028100020321     C     IMPVAT        BEGSR
028200010607     C*
028300010607     C                   SETOFF                                       3132
028400040218     C*
028500040218     C                   MOVEL     datcor        VATAAS
028600170711     C* CCM
028700170711     C                   EVAL      PiStr=%trim(%subst(vindta:3:7))
028800170711     C                   EXSR      CHKNUM
028900170711     C                   IF        PiInt=*on
029000170711     C                   Z-ADD     PiVal         VATCCM
029100170711     C                   ELSE
029200170711     C                   SETON                                        32
029300170711     C                   EVAL      vinmsg = %trimr(vinmsg)
029400170711     C                             + ' ' + 'VATCCM'
029500170711     C                   ENDIF
029600010607     C*
029700010607     C* Reperimento campi ALFA
029800140328     C* _E
029900170711     C                   EVAL      wNOT_E=%trim(%subst(vindta:31:11))
030000020304     C*
030100040218     C* VATNSP
030200170711     C                   EVAL      PiStr=%trim(%subst(vindta:67:15))
030300020304     C                   EXSR      CHKNUM
030400020304     C                   IF        PiInt=*on
030500020321     C                   Z-ADD     PiVal         VATNSP
030600020304     C                   ELSE
030700020304     C                   SETON                                        32
030800020321     C                   Z-ADD     *zeros        VATNSP
030900020304     C                   EVAL      vinmsg = %trimr(vinmsg)
031000020321     C                             + ' ' + 'VATNSP'
031100020304     C                   ENDIF
031200170711     C*
031300170711     C* numero colli
031400170711     C                   EVAL      PiStr=%trim(%subst(vindta:82:3))
031500170711     C                   EXSR      CHKNUM
031600170711     C                   IF        PiInt=*on
031700170711     C                   Z-ADD     PiVal         wNCL
031800170711     C                   ELSE
031900170711     C                   SETON                                        32
032000170711     C                   Z-ADD     *zeros        wNCL
032100170711     C                   EVAL      vinmsg = %trimr(vinmsg)
032200170711     C                             + ' ' + 'VATNOT'
032300170711     C                   ENDIF
032400020204     C*
032500010607     C                   ENDSR
032600010607     C*----------------------------------------------------*
032700010601
032800010330
032900010330     C*----------------------------------------------------*
033000010330     C*  CONTROLLO NUMERICITA' CAMPI
033100010330     C*----------------------------------------------------*
033200010330     C     CHKNUM        BEGSR
033300010330     C*
033400010606     C                   IF        PiDecChr = *blanks
033500010606     C                   EVAL      PiDecChr = ','
033600010606     C                   ENDIF
033700010606     C*
033800010606     C                   CALL(e)   'ISNUMERIC'
033900010330     C                   PARM                    PiStr            30
034000010606     C                   PARM                    PiDecChr          1
034100010330     C                   PARM      *ZEROS        PiVal            30 9
034200010330     C                   PARM      '0'           PiInt             1
034300010330     C                   PARM      '0'           PiNum             1
034400010330     C                   IF        %error
034500010606     C                   EVAL      PiNum=*off
034600010330     C                   ENDIF
034700010330     C*
034800010330     C                   ENDSR
034900010330     C***
035000020403
035100020403
035200010601
035300010601
035400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
035500010601     C     repfil        BEGSR
035600010601     C*
035700010601     C                   if        invfil = *zeros and
035800010601     C                             depfil > *zeros and
035900010629     C                             (vinflg = *blanks or
036000010629     C                              vinflg = *zeros)
036100010601     C
036200010601     C                   eval      invfil = depfil
036300010601     C                   endif
036400010601     C*
036500010601     C                   if        depfil <> invfil and
036600010601     C                             invfil > *zeros
036700010601     C                   eval      flgMulti = '1'
036800010601     C                   if        vinflg = *blanks
036900010601     C                   add       1             cntNonEl
037000010601     C                   endif
037100010601     C                   endif
037200010601     C*
037300010601     C                   if        vinflg = '2'
037400010601     C                   eval      flgStato = '2'
037500010601     C                   endif
037600010601     C*
037700010601     C                   ENDSR
037800010601     C***
037900010601
038000010601
038100010601
038200010330
038300010330
038400010330
038500990920      /TITLE Invio dei dati al punto operativo.
038600000613     C     invio         BEGSR
038700990920     C*
038800990920     C                   reset                   dscmz
038900010601     C                   move      invfil        cmzdst
039000990920     C                   eval      cmzfld = vlrfou
039100990920     C                   eval      cmzmbd = vlrhdl
039200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
039300000710     C                   if        prmfir = *blanks
039400060221     C                   eval      cmzfla = 'EDIVAT0F'
039500060221     C                   eval      cmzmba = 'EDIVAT0F'
039600000710     C                   else
039700000710     C                   eval      cmzfla = prmfir
039800000710     C                   eval      cmzmba = prmfir
039900000710     C                   endif
040000990920     C                   eval      cmznrr = *zeros
040100990920     C                   move      §ctrok        cmznrr
040200021018     C                   eval      cmzlba = vlrfl1
040300990920     C                   call(e)   'TIS711C'
040400990920     C                   parm                    dscmz
040500990921     C                   parm      *blanks       esito
040600990920     C                   if        %error
040700990920     C                             or cmzerr = '1'
040800990921     C                             or esito  = '1'
040900000710     C                   eval      wrkesito = '3'
041000990920     C                   endif
041100990920     C*
041200000613     C                   ENDSR
041300990910
041400010601
041500010601
041600010601
041700010601
041800010601      /TITLE Invio dei dati al punto operativo.
041900010601     C     opeini        BEGSR
042000010601     C*
042100010601     C* Inizializzo flag e contatori operativi
042200010601     C                   movel     '0'           flgGiro           1
042300010601     C                   movel     '0'           flgMulti          1
042400010601     C                   movel     '1'           flgStato          1
042500010615     C                   movel     '0'           flgOk             1
042600010601     C                   z-add     *zeros        cntNonEl         10 0
042700010601     C                   z-add     *zeros        depfil            3 0
042800010601     C                   z-add     *zeros        invfil            3 0
042900010601     C*
043000010601     C                   ENDSR
043100010601     C***
043200010601
043300010601
043400010601
043500010330
043600010330
043700000613     C     *inzsr        BEGSR
043800990910     C*
043900990910     C     *entry        plist
044000990920     C                   parm                    tivlrds
044100990921     C                   parm      wrkesito      esito
044200000724     C                   parm                    prmlit
044300000710     C                   parm                    prmfir
044400010330     C*
044500010330     C* CALCOLA LA DATA CORRENTE
044600010330     C                   time                    wn14             14 0
044700170711     C                   movel     wn14          oracor            6 0          *ORA
044800010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
044900010330     C                   z-add     wn8           g08dat
045000010330     C                   z-add     *zeros        g08inv
045100010330     C                   movel     '0'           g08err
045200010330     C                   call      'XSRDA8'
045300010330     C                   parm                    wlbda8
045400010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
045500060221     C*
045600060221     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
045700060221     C                   move(p)   vlrMSG        wNomeFile
045800060221     C*
045900170711     C* ATTRIBUISCO UN CMR FISSO
046000170711     C                   EVAL      WCMR ='SIFARMA'
046100000613     C*
046200000613     C                   ENDSR
046300000613     C***
