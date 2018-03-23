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
012400030326     C                   if        %subst(vindta:1:15) = savBol
012500030326     C                   move(p)   savNsp        vatNSP
012600030326     C                   else
012700030326     C                   eval      savBol = %subst(vindta:1:15)
012800030326     C                   eval      savNsp = %subst(vindta:8:6)+
012900030326     C                                      %subst(vindta:15:1)
013000030326     C                   endif
013100010601     C*
013200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
013300020321     C                   eval      depfil = VATLNP
013400010601     C                   exsr      repfil
013500010601     C                   if        depfil = invfil
013600021025     C                   if        vlrpoi = 999
013700021025     C                   MOVE(P)   invfil        VATFGS
013800021025     C                   else
013900021025     C                   MOVE(P)   vlrpoi        VATFGS
014000021025     C                   endif
014100010601     C*
014200010601     C                   exsr      PREELA
014300010601     C*
014400010604     C* Ebbene...
014500010604     C*
014600010604     C  N31              ADD       1             低TROK            7 0
014700010604     C   32              ADD       1             低TRMO            7 0
014800010604     C   31              ADD       1             低TRNO            7 0
014900020205     C*
015000020205     C* Effettuo test particolare solo x DA ROLD x condizoinare la scrittura
015100031121     C  N31              WRITE     EDIVAT00
015200010604     C*
015300010604     C                   if        *in31 = *off and
015400010604     C                             *in32 = *off
015500010604     C                   eval      vinflg = '1'
015600010604     C                   else
015700010604     C                   eval      vinflg = '2'
015800010604     C                   endif
015900010604     C                   endif
016000010601     C*
016100010604     C                   endif
016200010604     C*
016300010330     C                   else
016400010330     C                   eval      vinflg = '1'
016500010330     C                   endif
016600010601     C*
016700010601     C  N70              update    tivin000
016800010330     C*
016900010330     C  N70              ENDdo
017000010601     C
017100010601     C*
017200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
017300010601     C                   if        cntNonEl = *zeros or
017400010601     C                             flgMulti = '0'
017500010330     C* Se non ci sono record con errori ...
017600010601     C                   if        呃trno = 0 and
017700010604     C                             呃trmo = 0 and
017800010601     C                             flgStato <> '2'
017900010330     C* ... restituisco esito OK.
018000010330     C                   eval      wrkesito = '0'
018100010330     C                   else
018200010330     C                   if        呃trok > 0
018300010330     C                   eval      wrkesito = '1'
018400010330     C                   else
018500010615     C                   if        flgOk = '0'
018600010615     C                   eval      wrkesito = '2'
018700010615     C                   else
018800010615     C                   eval      wrkesito = '6'
018900010615     C                   endif
019000010330     C                   endif
019100010330     C                   endif
019200010601     C                   else
019300010601     C                   eval      wrkesito = '9'
019400010601     C                   endif
019500010330     C*
019600010330     C                   if        %open(tivin00r)
019700010330     C                   close     tivin00r
019800010330     C                   endif
019900031121     C                   if        %open(edivatwr)
020000031121     C                   close     edivatwr
020100010330     C                   endif
020200010601     C*
020300010601     C                   if        vlrpoi <> 999
020400010601     C                   eval      invfil = vlrpoi
020500010601     C                   endif
020600010330     C*
020700010330     C                   if        呃trok > 0
020800010601     C                             and invfil > *zeros
020900010330     C                   exsr      invio
021000010330     C                   endif
021100010601     C*
021200010618     C                   if        flgGiro = '1'
021300010601     C                   exsr      endela
021400010618     C                   endif
021500010330     C*
021600010330     C                   ENDSR
021700010330     C***
021800010330
021900010601
022000010601
022100010601
022200010601
022300010330     C*----------------------------------------------------*
022400010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022500010330     C*----------------------------------------------------*
022600020321 xxx C     INZVAR        BEGSR
022700010330     C*
022800010330     C*
022900010330     C                   ENDSR
023000010330     C*----------------------------------------------------*
023100010330     C*  IMPOSTAZIONE CAMPI COSTANTI
023200010330     C*----------------------------------------------------*
023300010330     C     DEFCAM        BEGSR
023400010330     C*
023500020204     C* Inizializzo il buffer del record da scrivere
023600031121     C                   CLEAR                   EDIVAT00
023700020204     C* Imposto i valori di default...
023800020329     C                   EVAL      VATCCM = 0591254
023900020321     C                   EVAL      VATLNP = 059
024000020321     C                   EVAL      VATTRC = 'E'
024100020204     C* ... e poi verifico se sono stati passati come parametri
024200020204     C                   IF        vlrppt > *blanks
024300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
024400020204     C                   EXSR      CHKNUM
024500020204     C                   IF        PiInt=*on
024600020321     C                   Z-ADD     PiVal         VATCCM
024700020204     C                   ENDIF
024800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024900020204     C                   EXSR      CHKNUM
025000020204     C                   IF        PiInt=*on
025100020321     C                   Z-ADD     PiVal         VATLNP
025200020204     C                   ENDIF
025300020321     C                   ENDIF
025400020204     C*
025500010330     C                   ENDSR
025600010607     C*----------------------------------------------------*
025700031121 xxx C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
025800010607     C*----------------------------------------------------*
025900020321     C     IMPVAT        BEGSR
026000010607     C*
026100010607     C                   SETOFF                                       3132
026200010607     C*
026300010607     C* Reperimento campi ALFA
026400020404     C                   EVAL      VATNOT='1'+%trim(%subst(vindta:34:4))+
026500060201     C                                        %trim(%subst(vindta:39:7))+
026600020404     C                                        %trim(%subst(vindta:51:4))
026700020329     C*
026800020329     C                   MOVEL     DATCOR        VATAAS
026900010607     C*
027000010607     C* Reperimento campi NUMERICI
027100030326     C                   EVAL      PiStr=%trim(%subst(vindta:8:6)+
027200030326     C                                         %subst(vindta:15:1))
027300020304     C                   EXSR      CHKNUM
027400020304     C                   IF        PiInt=*on
027500020321     C                   Z-ADD     PiVal         VATNSP
027600020304     C                   ELSE
027700020304     C                   SETON                                        32
027800020321     C                   Z-ADD     *zeros        VATNSP
027900020304     C                   EVAL      vinmsg = %trimr(vinmsg)
028000020321     C                             + ' ' + 'VATNSP'
028100020304     C                   ENDIF
028200031121     C*
028300031121     C* VALORIZZO CAMPI RELATIVI AL "CMR"
028400031121     C                   MOVE(P)   vlrMSG        wNomeFile
028500031121     C                   EVAL      VATCMR = %subst(wNomeFile:2:7)
028600031121     C                   EVAL      VATCNT = 1
028700020204     C*
028800010607     C                   ENDSR
028900010607     C*----------------------------------------------------*
029000010601
029100010330
029200010330     C*----------------------------------------------------*
029300010330     C*  CONTROLLO NUMERICITA' CAMPI
029400010330     C*----------------------------------------------------*
029500010330     C     CHKNUM        BEGSR
029600010330     C*
029700010606     C                   IF        PiDecChr = *blanks
029800010606     C                   EVAL      PiDecChr = ','
029900010606     C                   ENDIF
030000010606     C*
030100010606     C                   CALL(e)   'ISNUMERIC'
030200010330     C                   PARM                    PiStr            30
030300010606     C                   PARM                    PiDecChr          1
030400010330     C                   PARM      *ZEROS        PiVal            30 9
030500010330     C                   PARM      '0'           PiInt             1
030600010330     C                   PARM      '0'           PiNum             1
030700010330     C                   IF        %error
030800010606     C                   EVAL      PiNum=*off
030900010330     C                   ENDIF
031000010330     C*
031100010330     C                   ENDSR
031200010330     C***
031300010330
031400010601
031500010601
031600010601
031700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
031800010601     C     repfil        BEGSR
031900010601     C*
032000010601     C                   if        invfil = *zeros and
032100010601     C                             depfil > *zeros and
032200010629     C                             (vinflg = *blanks or
032300010629     C                              vinflg = *zeros)
032400010601     C
032500010601     C                   eval      invfil = depfil
032600010601     C                   endif
032700010601     C*
032800010601     C                   if        depfil <> invfil and
032900010601     C                             invfil > *zeros
033000010601     C                   eval      flgMulti = '1'
033100010601     C                   if        vinflg = *blanks
033200010601     C                   add       1             cntNonEl
033300010601     C                   endif
033400010601     C                   endif
033500010601     C*
033600010601     C                   if        vinflg = '2'
033700010601     C                   eval      flgStato = '2'
033800010601     C                   endif
033900010601     C*
034000010601     C                   ENDSR
034100010601     C***
034200010601
034300010601
034400010601
034500010330
034600010330
034700010330
034800990920      /TITLE Invio dei dati al punto operativo.
034900000613     C     invio         BEGSR
035000990920     C*
035100990920     C                   reset                   dscmz
035200010601     C                   move      invfil        cmzdst
035300990920     C                   eval      cmzfld = vlrfou
035400990920     C                   eval      cmzmbd = vlrhdl
035500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
035600000710     C                   if        prmfir = *blanks
035700031121     C                   eval      cmzfla = 'EDIVAT0F'
035800031121     C                   eval      cmzmba = 'EDIVAT0F'
035900000710     C                   else
036000000710     C                   eval      cmzfla = prmfir
036100000710     C                   eval      cmzmba = prmfir
036200000710     C                   endif
036300990920     C                   eval      cmznrr = *zeros
036400990920     C                   move      呃trok        cmznrr
036500021018     C                   eval      cmzlba = vlrfl1
036600990920     C                   call(e)   'TIS711C'
036700990920     C                   parm                    dscmz
036800990921     C                   parm      *blanks       esito
036900990920     C                   if        %error
037000990920     C                             or cmzerr = '1'
037100990921     C                             or esito  = '1'
037200000710     C                   eval      wrkesito = '3'
037300990920     C                   endif
037400990920     C*
037500000613     C                   ENDSR
037600990910
037700010601
037800010601
037900010601
038000010601
038100010601      /TITLE Invio dei dati al punto operativo.
038200010601     C     opeini        BEGSR
038300010601     C*
038400010601     C* Inizializzo flag e contatori operativi
038500010601     C                   movel     '0'           flgGiro           1
038600010601     C                   movel     '0'           flgMulti          1
038700010601     C                   movel     '1'           flgStato          1
038800010615     C                   movel     '0'           flgOk             1
038900010601     C                   z-add     *zeros        cntNonEl         10 0
039000010601     C                   z-add     *zeros        depfil            3 0
039100010601     C                   z-add     *zeros        invfil            3 0
039200010601     C*
039300010601     C                   ENDSR
039400010601     C***
039500010601
039600010601
039700010601
039800010330
039900010330
040000000613     C     *inzsr        BEGSR
040100990910     C*
040200990910     C     *entry        plist
040300990920     C                   parm                    tivlrds
040400990921     C                   parm      wrkesito      esito
040500000724     C                   parm                    prmlit
040600000710     C                   parm                    prmfir
040700010330     C*
040800010330     C* CALCOLA LA DATA CORRENTE
040900010330     C                   time                    wn14             14 0
041000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
041100010330     C                   z-add     wn8           g08dat
041200010330     C                   z-add     *zeros        g08inv
041300010330     C                   movel     '0'           g08err
041400010330     C                   call      'XSRDA8'
041500010330     C                   parm                    wlbda8
041600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
041700000613     C*
041800000613     C                   ENDSR
041900000613     C***
