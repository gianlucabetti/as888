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
012800140606     C                   eval      savNsp = %subst(vindta:8:6)+
012900140606     C                                      %subst(vindta:15:1)
013000140606     C                   evalr     savNsp = %trim(savNsp)
013100030326     C                   endif
013200010601     C*
013300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
013400020321     C                   eval      depfil = VATLNP
013500010601     C                   exsr      repfil
013600010601     C                   if        depfil = invfil
013700021025     C                   if        vlrpoi = 999
013800021025     C                   MOVE(P)   invfil        VATFGS
013900021025     C                   else
014000021025     C                   MOVE(P)   vlrpoi        VATFGS
014100021025     C                   endif
014200010601     C*
014300010601     C                   exsr      PREELA
014400010601     C*
014500010604     C* Ebbene...
014600010604     C*
014700010604     C  N31              ADD       1             低TROK            7 0
014800010604     C   32              ADD       1             低TRMO            7 0
014900010604     C   31              ADD       1             低TRNO            7 0
015000020205     C*
015100020205     C* Effettuo test particolare solo x DA ROLD x condizoinare la scrittura
015200031121     C  N31              WRITE     EDIVAT00
015300010604     C*
015400010604     C                   if        *in31 = *off and
015500010604     C                             *in32 = *off
015600010604     C                   eval      vinflg = '1'
015700010604     C                   else
015800010604     C                   eval      vinflg = '2'
015900010604     C                   endif
016000010604     C                   endif
016100010601     C*
016200010604     C                   endif
016300010604     C*
016400010330     C                   else
016500010330     C                   eval      vinflg = '1'
016600010330     C                   endif
016700010601     C*
016800010601     C  N70              update    tivin000
016900010330     C*
017000010330     C  N70              ENDdo
017100010601     C
017200010601     C*
017300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
017400010601     C                   if        cntNonEl = *zeros or
017500010601     C                             flgMulti = '0'
017600010330     C* Se non ci sono record con errori ...
017700010601     C                   if        呃trno = 0 and
017800010604     C                             呃trmo = 0 and
017900010601     C                             flgStato <> '2'
018000010330     C* ... restituisco esito OK.
018100010330     C                   eval      wrkesito = '0'
018200010330     C                   else
018300010330     C                   if        呃trok > 0
018400010330     C                   eval      wrkesito = '1'
018500010330     C                   else
018600010615     C                   if        flgOk = '0'
018700010615     C                   eval      wrkesito = '2'
018800010615     C                   else
018900010615     C                   eval      wrkesito = '6'
019000010615     C                   endif
019100010330     C                   endif
019200010330     C                   endif
019300010601     C                   else
019400010601     C                   eval      wrkesito = '9'
019500010601     C                   endif
019600010330     C*
019700010330     C                   if        %open(tivin00r)
019800010330     C                   close     tivin00r
019900010330     C                   endif
020000031121     C                   if        %open(edivatwr)
020100031121     C                   close     edivatwr
020200010330     C                   endif
020300010601     C*
020400010601     C                   if        vlrpoi <> 999
020500010601     C                   eval      invfil = vlrpoi
020600010601     C                   endif
020700010330     C*
020800010330     C                   if        呃trok > 0
020900010601     C                             and invfil > *zeros
021000010330     C                   exsr      invio
021100010330     C                   endif
021200010601     C*
021300010618     C                   if        flgGiro = '1'
021400010601     C                   exsr      endela
021500010618     C                   endif
021600010330     C*
021700010330     C                   ENDSR
021800010330     C***
021900010330
022000010601
022100010601
022200010601
022300010601
022400010330     C*----------------------------------------------------*
022500010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022600010330     C*----------------------------------------------------*
022700020321 xxx C     INZVAR        BEGSR
022800010330     C*
022900010330     C*
023000010330     C                   ENDSR
023100010330     C*----------------------------------------------------*
023200010330     C*  IMPOSTAZIONE CAMPI COSTANTI
023300010330     C*----------------------------------------------------*
023400010330     C     DEFCAM        BEGSR
023500010330     C*
023600020204     C* Inizializzo il buffer del record da scrivere
023700031121     C                   CLEAR                   EDIVAT00
023800020204     C* Imposto i valori di default...
023900020329     C                   EVAL      VATCCM = 0591254
024000020321     C                   EVAL      VATLNP = 059
024100020321     C                   EVAL      VATTRC = 'E'
024200020204     C* ... e poi verifico se sono stati passati come parametri
024300020204     C                   IF        vlrppt > *blanks
024400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
024500020204     C                   EXSR      CHKNUM
024600020204     C                   IF        PiInt=*on
024700020321     C                   Z-ADD     PiVal         VATCCM
024800020204     C                   ENDIF
024900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
025000020204     C                   EXSR      CHKNUM
025100020204     C                   IF        PiInt=*on
025200020321     C                   Z-ADD     PiVal         VATLNP
025300020204     C                   ENDIF
025400020321     C                   ENDIF
025500020204     C*
025600010330     C                   ENDSR
025700010607     C*----------------------------------------------------*
025800031121 xxx C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
025900010607     C*----------------------------------------------------*
026000020321     C     IMPVAT        BEGSR
026100010607     C*
026200010607     C                   SETOFF                                       3132
026300010607     C*
026400010607     C* Reperimento campi ALFA
026500020404     C                   EVAL      VATNOT='1'+%trim(%subst(vindta:34:4))+
026600060201     C                                        %trim(%subst(vindta:39:7))+
026700020404     C                                        %trim(%subst(vindta:51:4))
026800020329     C*
026900020329     C                   MOVEL     DATCOR        VATAAS
027000010607     C*
027100010607     C* Reperimento campi NUMERICI
027200030326     C                   EVAL      PiStr=%trim(%subst(vindta:8:6)+
027300030326     C                                         %subst(vindta:15:1))
027400020304     C                   EXSR      CHKNUM
027500020304     C                   IF        PiInt=*on
027600020321     C                   Z-ADD     PiVal         VATNSP
027700020304     C                   ELSE
027800020304     C                   SETON                                        32
027900020321     C                   Z-ADD     *zeros        VATNSP
028000020304     C                   EVAL      vinmsg = %trimr(vinmsg)
028100020321     C                             + ' ' + 'VATNSP'
028200020304     C                   ENDIF
028300031121     C*
028400031121     C* VALORIZZO CAMPI RELATIVI AL "CMR"
028500031121     C                   MOVE(P)   vlrMSG        wNomeFile
028600031121     C                   EVAL      VATCMR = %subst(wNomeFile:2:7)
028700031121     C                   EVAL      VATCNT = 1
028800020204     C*
028900010607     C                   ENDSR
029000010607     C*----------------------------------------------------*
029100010601
029200010330
029300010330     C*----------------------------------------------------*
029400010330     C*  CONTROLLO NUMERICITA' CAMPI
029500010330     C*----------------------------------------------------*
029600010330     C     CHKNUM        BEGSR
029700010330     C*
029800010606     C                   IF        PiDecChr = *blanks
029900010606     C                   EVAL      PiDecChr = ','
030000010606     C                   ENDIF
030100010606     C*
030200010606     C                   CALL(e)   'ISNUMERIC'
030300010330     C                   PARM                    PiStr            30
030400010606     C                   PARM                    PiDecChr          1
030500010330     C                   PARM      *ZEROS        PiVal            30 9
030600010330     C                   PARM      '0'           PiInt             1
030700010330     C                   PARM      '0'           PiNum             1
030800010330     C                   IF        %error
030900010606     C                   EVAL      PiNum=*off
031000010330     C                   ENDIF
031100010330     C*
031200010330     C                   ENDSR
031300010330     C***
031400010330
031500010601
031600010601
031700010601
031800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
031900010601     C     repfil        BEGSR
032000010601     C*
032100010601     C                   if        invfil = *zeros and
032200010601     C                             depfil > *zeros and
032300010629     C                             (vinflg = *blanks or
032400010629     C                              vinflg = *zeros)
032500010601     C
032600010601     C                   eval      invfil = depfil
032700010601     C                   endif
032800010601     C*
032900010601     C                   if        depfil <> invfil and
033000010601     C                             invfil > *zeros
033100010601     C                   eval      flgMulti = '1'
033200010601     C                   if        vinflg = *blanks
033300010601     C                   add       1             cntNonEl
033400010601     C                   endif
033500010601     C                   endif
033600010601     C*
033700010601     C                   if        vinflg = '2'
033800010601     C                   eval      flgStato = '2'
033900010601     C                   endif
034000010601     C*
034100010601     C                   ENDSR
034200010601     C***
034300010601
034400010601
034500010601
034600010330
034700010330
034800010330
034900990920      /TITLE Invio dei dati al punto operativo.
035000000613     C     invio         BEGSR
035100990920     C*
035200990920     C                   reset                   dscmz
035300010601     C                   move      invfil        cmzdst
035400990920     C                   eval      cmzfld = vlrfou
035500990920     C                   eval      cmzmbd = vlrhdl
035600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
035700000710     C                   if        prmfir = *blanks
035800031121     C                   eval      cmzfla = 'EDIVAT0F'
035900031121     C                   eval      cmzmba = 'EDIVAT0F'
036000000710     C                   else
036100000710     C                   eval      cmzfla = prmfir
036200000710     C                   eval      cmzmba = prmfir
036300000710     C                   endif
036400990920     C                   eval      cmznrr = *zeros
036500990920     C                   move      呃trok        cmznrr
036600021018     C                   eval      cmzlba = vlrfl1
036700990920     C                   call(e)   'TIS711C'
036800990920     C                   parm                    dscmz
036900990921     C                   parm      *blanks       esito
037000990920     C                   if        %error
037100990920     C                             or cmzerr = '1'
037200990921     C                             or esito  = '1'
037300000710     C                   eval      wrkesito = '3'
037400990920     C                   endif
037500990920     C*
037600000613     C                   ENDSR
037700990910
037800010601
037900010601
038000010601
038100010601
038200010601      /TITLE Invio dei dati al punto operativo.
038300010601     C     opeini        BEGSR
038400010601     C*
038500010601     C* Inizializzo flag e contatori operativi
038600010601     C                   movel     '0'           flgGiro           1
038700010601     C                   movel     '0'           flgMulti          1
038800010601     C                   movel     '1'           flgStato          1
038900010615     C                   movel     '0'           flgOk             1
039000010601     C                   z-add     *zeros        cntNonEl         10 0
039100010601     C                   z-add     *zeros        depfil            3 0
039200010601     C                   z-add     *zeros        invfil            3 0
039300010601     C*
039400010601     C                   ENDSR
039500010601     C***
039600010601
039700010601
039800010601
039900010330
040000010330
040100000613     C     *inzsr        BEGSR
040200990910     C*
040300990910     C     *entry        plist
040400990920     C                   parm                    tivlrds
040500990921     C                   parm      wrkesito      esito
040600000724     C                   parm                    prmlit
040700000710     C                   parm                    prmfir
040800010330     C*
040900010330     C* CALCOLA LA DATA CORRENTE
041000010330     C                   time                    wn14             14 0
041100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
041200010330     C                   z-add     wn8           g08dat
041300010330     C                   z-add     *zeros        g08inv
041400010330     C                   movel     '0'           g08err
041500010330     C                   call      'XSRDA8'
041600010330     C                   parm                    wlbda8
041700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
041800000613     C*
041900000613     C                   ENDSR
042000000613     C***
