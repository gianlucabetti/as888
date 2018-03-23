000100031121      /TITLE Upload via Internet: traduzione in EDIVATWR. (x NOTTINTON Cod. 0591254)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500031121     FEDIVATwr  O    E             DISK    usropn
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
001900030326     D savBol          s             15    INZ(*blanks)
002000030326     D savNsp          s              7    INZ(*blanks)
002100031121     D wNomeFile       s             30    INZ(*blanks)
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
003900020321     C                   exsr      rwvat
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
008000010601
008100010330     C*--------------------------------------------------------
008200031121     C* RWVAT   LEGGE TIVIN00R E SCRIVE EDIVATWF              *
008300010330     C*--------------------------------------------------------
008400020321     C     RWVAT         BEGSR
008500010330     C*
008600010330     C                   if        not %open(tivin00r)
008700010330     C                   open      tivin00r
008800010330     C                   endif
008900031121     C                   if        not %open(edivatwr)
009000031121     C                   open      edivatwr
009100010330     C                   endif
009200010330     C*
009300010604     C                   clear                   低TROK
009400010604     C                   clear                   低TRMO
009500010604     C                   clear                   低TRNO
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
012200030326     C*
012300030326     C* Effettuo considerazioni sull'identificazione della bolla e del suo numero spedizione
012400160408     C***                if        %subst(vindta:1:15) = savBol
012500160408     C***                move(p)   savNsp        vatNSP
012600160408     C***                else
012700160408     C***                eval      savBol = %subst(vindta:1:15)
012800160408     C***                eval      savNsp = %subst(vindta:8:6)+
012900160408     C***                                   %subst(vindta:15:1)
013000160408     C***                if        %subst(savNsp:7:1) = *blank
013100160408     C***                eval      %subst(savNsp:7:1) = '0'
013200160408     C***                endif
013300160408     C***                endif
013400010601     C*
013500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
013600020321     C                   eval      depfil = VATLNP
013700010601     C                   exsr      repfil
013800010601     C                   if        depfil = invfil
013900021025     C                   if        vlrpoi = 999
014000021025     C                   MOVE(P)   invfil        VATFGS
014100021025     C                   else
014200021025     C                   MOVE(P)   vlrpoi        VATFGS
014300021025     C                   endif
014400010601     C*
014500010601     C                   exsr      PREELA
014600010601     C*
014700010604     C* Ebbene...
014800010604     C*
014900010604     C  N31              ADD       1             低TROK            7 0
015000010604     C   32              ADD       1             低TRMO            7 0
015100010604     C   31              ADD       1             低TRNO            7 0
015200020205     C*
015300020205     C* Effettuo test particolare solo x DA ROLD x condizoinare la scrittura
015400031121     C  N31              WRITE     EDIVAT00
015500010604     C*
015600010604     C                   if        *in31 = *off and
015700010604     C                             *in32 = *off
015800010604     C                   eval      vinflg = '1'
015900010604     C                   else
016000010604     C                   eval      vinflg = '2'
016100010604     C                   endif
016200010604     C                   endif
016300010601     C*
016400010604     C                   endif
016500010604     C*
016600010330     C                   else
016700010330     C                   eval      vinflg = '1'
016800010330     C                   endif
016900010601     C*
017000010601     C  N70              update    tivin000
017100010330     C*
017200010330     C  N70              ENDdo
017300010601     C
017400010601     C*
017500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
017600010601     C                   if        cntNonEl = *zeros or
017700010601     C                             flgMulti = '0'
017800010330     C* Se non ci sono record con errori ...
017900010601     C                   if        呃trno = 0 and
018000010604     C                             呃trmo = 0 and
018100010601     C                             flgStato <> '2'
018200010330     C* ... restituisco esito OK.
018300010330     C                   eval      wrkesito = '0'
018400010330     C                   else
018500010330     C                   if        呃trok > 0
018600010330     C                   eval      wrkesito = '1'
018700010330     C                   else
018800010615     C                   if        flgOk = '0'
018900010615     C                   eval      wrkesito = '2'
019000010615     C                   else
019100010615     C                   eval      wrkesito = '6'
019200010615     C                   endif
019300010330     C                   endif
019400010330     C                   endif
019500010601     C                   else
019600010601     C                   eval      wrkesito = '9'
019700010601     C                   endif
019800010330     C*
019900010330     C                   if        %open(tivin00r)
020000010330     C                   close     tivin00r
020100010330     C                   endif
020200031121     C                   if        %open(edivatwr)
020300031121     C                   close     edivatwr
020400010330     C                   endif
020500010601     C*
020600010601     C                   if        vlrpoi <> 999
020700010601     C                   eval      invfil = vlrpoi
020800010601     C                   endif
020900010330     C*
021000010330     C                   if        呃trok > 0
021100010601     C                             and invfil > *zeros
021200010330     C                   exsr      invio
021300010330     C                   endif
021400010601     C*
021500010618     C                   if        flgGiro = '1'
021600010601     C                   exsr      endela
021700010618     C                   endif
021800010330     C*
021900010330     C                   ENDSR
022000010330     C***
022100010330
022200010601
022300010601
022400010601
022500010601
022600010330     C*----------------------------------------------------*
022700010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022800010330     C*----------------------------------------------------*
022900020321 xxx C     INZVAR        BEGSR
023000010330     C*
023100010330     C*
023200010330     C                   ENDSR
023300010330     C*----------------------------------------------------*
023400010330     C*  IMPOSTAZIONE CAMPI COSTANTI
023500010330     C*----------------------------------------------------*
023600010330     C     DEFCAM        BEGSR
023700010330     C*
023800020204     C* Inizializzo il buffer del record da scrivere
023900031121     C                   CLEAR                   EDIVAT00
024000020204     C* Imposto i valori di default...
024100020329     C                   EVAL      VATCCM = 0591254
024200020321     C                   EVAL      VATLNP = 059
024300020321     C                   EVAL      VATTRC = 'E'
024400020204     C* ... e poi verifico se sono stati passati come parametri
024500020204     C                   IF        vlrppt > *blanks
024600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
024700020204     C                   EXSR      CHKNUM
024800020204     C                   IF        PiInt=*on
024900020321     C                   Z-ADD     PiVal         VATCCM
025000020204     C                   ENDIF
025100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
025200020204     C                   EXSR      CHKNUM
025300020204     C                   IF        PiInt=*on
025400020321     C                   Z-ADD     PiVal         VATLNP
025500020204     C                   ENDIF
025600020321     C                   ENDIF
025700020204     C*
025800010330     C                   ENDSR
025900010607     C*----------------------------------------------------*
026000031121 xxx C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
026100010607     C*----------------------------------------------------*
026200020321     C     IMPVAT        BEGSR
026300010607     C*
026400010607     C                   SETOFF                                       3132
026500010607     C*
026600010607     C* Reperimento campi ALFA
026700020404     C                   EVAL      VATNOT='1'+%trim(%subst(vindta:34:4))+
026800060201     C                                        %trim(%subst(vindta:39:7))+
026900020404     C                                        %trim(%subst(vindta:51:4))
027000020329     C*
027100020329     C                   MOVEL     DATCOR        VATAAS
027200010607     C*
027300010607     C* Reperimento campi NUMERICI
027400160408     C***                EVAL      PiStr=%subst(vindta:8:6)+
027500160408     C***                                %subst(vindta:15:1)
027600160408     C***                if        %subst(PiStr:7:1) = *blank
027700160408     C***                eval      %subst(PiStr:7:1) = '0'
027800160408     C***                endif
027900160408     C                   EVAL      PiStr=%subst(vindta:155:7)
028000020304     C                   EXSR      CHKNUM
028100020304     C                   IF        PiInt=*on
028200020321     C                   Z-ADD     PiVal         VATNSP
028300020304     C                   ELSE
028400020304     C                   SETON                                        32
028500020321     C                   Z-ADD     *zeros        VATNSP
028600020304     C                   EVAL      vinmsg = %trimr(vinmsg)
028700020321     C                             + ' ' + 'VATNSP'
028800020304     C                   ENDIF
028900031121     C*
029000031121     C* VALORIZZO CAMPI RELATIVI AL "CMR"
029100031121     C                   MOVE(P)   vlrMSG        wNomeFile
029200031121     C                   EVAL      VATCMR = %subst(wNomeFile:2:7)
029300031121     C                   EVAL      VATCNT = 1
029400020204     C*
029500010607     C                   ENDSR
029600010607     C*----------------------------------------------------*
029700010601
029800010330
029900010330     C*----------------------------------------------------*
030000010330     C*  CONTROLLO NUMERICITA' CAMPI
030100010330     C*----------------------------------------------------*
030200010330     C     CHKNUM        BEGSR
030300010330     C*
030400010606     C                   IF        PiDecChr = *blanks
030500010606     C                   EVAL      PiDecChr = ','
030600010606     C                   ENDIF
030700010606     C*
030800010606     C                   CALL(e)   'ISNUMERIC'
030900010330     C                   PARM                    PiStr            30
031000010606     C                   PARM                    PiDecChr          1
031100010330     C                   PARM      *ZEROS        PiVal            30 9
031200010330     C                   PARM      '0'           PiInt             1
031300010330     C                   PARM      '0'           PiNum             1
031400010330     C                   IF        %error
031500010606     C                   EVAL      PiNum=*off
031600010330     C                   ENDIF
031700010330     C*
031800010330     C                   ENDSR
031900010330     C***
032000010330
032100010601
032200010601
032300010601
032400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
032500010601     C     repfil        BEGSR
032600010601     C*
032700010601     C                   if        invfil = *zeros and
032800010601     C                             depfil > *zeros and
032900010629     C                             (vinflg = *blanks or
033000010629     C                              vinflg = *zeros)
033100010601     C
033200010601     C                   eval      invfil = depfil
033300010601     C                   endif
033400010601     C*
033500010601     C                   if        depfil <> invfil and
033600010601     C                             invfil > *zeros
033700010601     C                   eval      flgMulti = '1'
033800010601     C                   if        vinflg = *blanks
033900010601     C                   add       1             cntNonEl
034000010601     C                   endif
034100010601     C                   endif
034200010601     C*
034300010601     C                   if        vinflg = '2'
034400010601     C                   eval      flgStato = '2'
034500010601     C                   endif
034600010601     C*
034700010601     C                   ENDSR
034800010601     C***
034900010601
035000010601
035100010601
035200010330
035300010330
035400010330
035500990920      /TITLE Invio dei dati al punto operativo.
035600000613     C     invio         BEGSR
035700990920     C*
035800990920     C                   reset                   dscmz
035900010601     C                   move      invfil        cmzdst
036000990920     C                   eval      cmzfld = vlrfou
036100990920     C                   eval      cmzmbd = vlrhdl
036200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
036300000710     C                   if        prmfir = *blanks
036400031121     C                   eval      cmzfla = 'EDIVAT0F'
036500031121     C                   eval      cmzmba = 'EDIVAT0F'
036600000710     C                   else
036700000710     C                   eval      cmzfla = prmfir
036800000710     C                   eval      cmzmba = prmfir
036900000710     C                   endif
037000990920     C                   eval      cmznrr = *zeros
037100990920     C                   move      呃trok        cmznrr
037200021018     C                   eval      cmzlba = vlrfl1
037300990920     C                   call(e)   'TIS711C'
037400990920     C                   parm                    dscmz
037500990921     C                   parm      *blanks       esito
037600990920     C                   if        %error
037700990920     C                             or cmzerr = '1'
037800990921     C                             or esito  = '1'
037900000710     C                   eval      wrkesito = '3'
038000990920     C                   endif
038100990920     C*
038200000613     C                   ENDSR
038300990910
038400010601
038500010601
038600010601
038700010601
038800010601      /TITLE Invio dei dati al punto operativo.
038900010601     C     opeini        BEGSR
039000010601     C*
039100010601     C* Inizializzo flag e contatori operativi
039200010601     C                   movel     '0'           flgGiro           1
039300010601     C                   movel     '0'           flgMulti          1
039400010601     C                   movel     '1'           flgStato          1
039500010615     C                   movel     '0'           flgOk             1
039600010601     C                   z-add     *zeros        cntNonEl         10 0
039700010601     C                   z-add     *zeros        depfil            3 0
039800010601     C                   z-add     *zeros        invfil            3 0
039900010601     C*
040000010601     C                   ENDSR
040100010601     C***
040200010601
040300010601
040400010601
040500010330
040600010330
040700000613     C     *inzsr        BEGSR
040800990910     C*
040900990910     C     *entry        plist
041000990920     C                   parm                    tivlrds
041100990921     C                   parm      wrkesito      esito
041200000724     C                   parm                    prmlit
041300000710     C                   parm                    prmfir
041400010330     C*
041500010330     C* CALCOLA LA DATA CORRENTE
041600010330     C                   time                    wn14             14 0
041700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
041800010330     C                   z-add     wn8           g08dat
041900010330     C                   z-add     *zeros        g08inv
042000010330     C                   movel     '0'           g08err
042100010330     C                   call      'XSRDA8'
042200010330     C                   parm                    wlbda8
042300010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
042400000613     C*
042500000613     C                   ENDSR
042600000613     C***
