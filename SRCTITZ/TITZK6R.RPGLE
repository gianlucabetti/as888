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
009200010604     C                   clear                   低TROK
009300010604     C                   clear                   低TRMO
009400010604     C                   clear                   低TRNO
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
014000010604     C  N31              ADD       1             低TROK            7 0
014100010604     C   32              ADD       1             低TRMO            7 0
014200010604     C   31              ADD       1             低TRNO            7 0
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
019000010601     C                   if        呃trno = 0 and
019100010604     C                             呃trmo = 0 and
019200010601     C                             flgStato <> '2'
019300010330     C* ... restituisco esito OK.
019400010330     C                   eval      wrkesito = '0'
019500010330     C                   else
019600010330     C                   if        呃trok > 0
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
022100010330     C                   if        呃trok > 0
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
028000170921     C                   EVAL      wNOT_E=%trim(
028100170921     C                                          %subst(vindta:21:15) +
028200170921     C                                          %subst(vindta:97:15))
028300020304     C*
028400040218     C* VATNSP
028500170906     C                   EVAL      PiStr=%trim(%subst(vindta:1:15))
028600020304     C                   EXSR      CHKNUM
028700020304     C                   IF        PiInt=*on
028800020321     C                   Z-ADD     PiVal         VATNSP
028900020304     C                   ELSE
029000020304     C                   SETON                                        32
029100020321     C                   Z-ADD     *zeros        VATNSP
029200020304     C                   EVAL      vinmsg = %trimr(vinmsg)
029300020321     C                             + ' ' + 'VATNSP'
029400020304     C                   ENDIF
029500020204     C*
029600010607     C                   ENDSR
029700010607     C*----------------------------------------------------*
029800010601
029900010330
030000010330     C*----------------------------------------------------*
030100010330     C*  CONTROLLO NUMERICITA' CAMPI
030200010330     C*----------------------------------------------------*
030300010330     C     CHKNUM        BEGSR
030400010330     C*
030500010606     C                   IF        PiDecChr = *blanks
030600010606     C                   EVAL      PiDecChr = ','
030700010606     C                   ENDIF
030800010606     C*
030900010606     C                   CALL(e)   'ISNUMERIC'
031000010330     C                   PARM                    PiStr            30
031100010606     C                   PARM                    PiDecChr          1
031200010330     C                   PARM      *ZEROS        PiVal            30 9
031300010330     C                   PARM      '0'           PiInt             1
031400010330     C                   PARM      '0'           PiNum             1
031500010330     C                   IF        %error
031600010606     C                   EVAL      PiNum=*off
031700010330     C                   ENDIF
031800010330     C*
031900010330     C                   ENDSR
032000010330     C***
032100020403
032200020403
032300010601
032400010601
032500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
032600010601     C     repfil        BEGSR
032700010601     C*
032800010601     C                   if        invfil = *zeros and
032900010601     C                             depfil > *zeros and
033000010629     C                             (vinflg = *blanks or
033100010629     C                              vinflg = *zeros)
033200010601     C
033300010601     C                   eval      invfil = depfil
033400010601     C                   endif
033500010601     C*
033600010601     C                   if        depfil <> invfil and
033700010601     C                             invfil > *zeros
033800010601     C                   eval      flgMulti = '1'
033900010601     C                   if        vinflg = *blanks
034000010601     C                   add       1             cntNonEl
034100010601     C                   endif
034200010601     C                   endif
034300010601     C*
034400010601     C                   if        vinflg = '2'
034500010601     C                   eval      flgStato = '2'
034600010601     C                   endif
034700010601     C*
034800010601     C                   ENDSR
034900010601     C***
035000010601
035100010601
035200010601
035300010330
035400010330
035500010330
035600990920      /TITLE Invio dei dati al punto operativo.
035700000613     C     invio         BEGSR
035800990920     C*
035900990920     C                   reset                   dscmz
036000010601     C                   move      invfil        cmzdst
036100990920     C                   eval      cmzfld = vlrfou
036200990920     C                   eval      cmzmbd = vlrhdl
036300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
036400000710     C                   if        prmfir = *blanks
036500060221     C                   eval      cmzfla = 'EDIVAT0F'
036600060221     C                   eval      cmzmba = 'EDIVAT0F'
036700000710     C                   else
036800000710     C                   eval      cmzfla = prmfir
036900000710     C                   eval      cmzmba = prmfir
037000000710     C                   endif
037100990920     C                   eval      cmznrr = *zeros
037200990920     C                   move      呃trok        cmznrr
037300021018     C                   eval      cmzlba = vlrfl1
037400990920     C                   call(e)   'TIS711C'
037500990920     C                   parm                    dscmz
037600990921     C                   parm      *blanks       esito
037700990920     C                   if        %error
037800990920     C                             or cmzerr = '1'
037900990921     C                             or esito  = '1'
038000000710     C                   eval      wrkesito = '3'
038100990920     C                   endif
038200990920     C*
038300000613     C                   ENDSR
038400990910
038500010601
038600010601
038700010601
038800010601
038900010601      /TITLE Invio dei dati al punto operativo.
039000010601     C     opeini        BEGSR
039100010601     C*
039200010601     C* Inizializzo flag e contatori operativi
039300010601     C                   movel     '0'           flgGiro           1
039400010601     C                   movel     '0'           flgMulti          1
039500010601     C                   movel     '1'           flgStato          1
039600010615     C                   movel     '0'           flgOk             1
039700010601     C                   z-add     *zeros        cntNonEl         10 0
039800010601     C                   z-add     *zeros        depfil            3 0
039900010601     C                   z-add     *zeros        invfil            3 0
040000010601     C*
040100010601     C                   ENDSR
040200010601     C***
040300010601
040400010601
040500010601
040600010330
040700010330
040800000613     C     *inzsr        BEGSR
040900990910     C*
041000990910     C     *entry        plist
041100990920     C                   parm                    tivlrds
041200990921     C                   parm      wrkesito      esito
041300000724     C                   parm                    prmlit
041400000710     C                   parm                    prmfir
041500010330     C*
041600010330     C* CALCOLA LA DATA CORRENTE
041700010330     C                   time                    wn14             14 0
041800170711     C                   movel     wn14          oracor            6 0          *ORA
041900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
042000010330     C                   z-add     wn8           g08dat
042100010330     C                   z-add     *zeros        g08inv
042200010330     C                   movel     '0'           g08err
042300010330     C                   call      'XSRDA8'
042400010330     C                   parm                    wlbda8
042500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
042600060221     C*
042700060221     C* Reperisco subito il nome del file "UPLOADATO" (se c'�')
042800060221     C                   move(p)   vlrMSG        wNomeFile
042900060221     C*
043000170711     C* ATTRIBUISCO UN CMR FISSO
043100170906     C                   EVAL      WCMR ='HASBRO'
043200000613     C*
043300000613     C                   ENDSR
043400000613     C***
