000100060221      /TITLE Upload via Internet: traduzione in EDIVABWR. (x TECNICA - Cod. 1330192)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500060221     FEDIVABwr  O    E             DISK    usropn
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
001900060221     D wNomeFile       s             30    INZ(*blanks)
002000060221     D wCMR            s             35    INZ(*blanks)
002100010330     D*------------------
002200010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002300010330     D*------------------
002400010330     D WLBDA8          DS                  INZ
002500010330     D  G08DAT                 1      8  0
002600010330     D  G08INV                 9     16  0
002700010330     D  G08ERR                17     17
002800010330     D  G08TGI                18     22  0
002900000313     D*
003000010330
003100010330
003200010330
003300000913     C                   reset                   rrnum
003400990921     C                   reset                   esito
003500990921     C                   reset                   wrkesito
003600010601     C*
003700010601     C                   exsr      opeini
003800010605     C                   exsr      rwvab
003900010601     C*
004000010601     C                   seton                                        lr
004100010601
004200010601
004300010601
004400010601
004500010601     C*--------------------------------------------------------
004600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
004700010601     C*--------------------------------------------------------
004800010601     C     PREELA        BEGSR
004900010601     C*
005000010601     C* SE OCCORRE SPEDIRE IN FILIALE
005100010601     C                   if        invfil <> *zeros and
005200010601     C                             flgGiro = '0'
005300010601     C*
005400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005500010601     C                   eval      flgGiro = '1'
005600010601     C*
005700010601     C                   endif
005800010601     C*
005900010601     C                   ENDSR
006000010601     C***
006100010601
006200010601
006300010601
006400010601
006500010601
006600010601
006700010601
006800010601     C*--------------------------------------------------------
006900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007000010601     C*--------------------------------------------------------
007100010601     C     ENDELA        BEGSR
007200000616     C*
007300010601     C                   ENDSR
007400010601     C***
007500000613
007600010330
007700010601
007800010601
007900010601
008000010330     C*--------------------------------------------------------
008100060221     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
008200010330     C*--------------------------------------------------------
008300010605     C     RWVAB         BEGSR
008400010330     C*
008500010330     C                   if        not %open(tivin00r)
008600010330     C                   open      tivin00r
008700010330     C                   endif
008800060221     C                   if        not %open(edivabwr)
008900060221     C                   open      edivabwr
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
012000010605     C                   exsr      impvab
012100010601     C*
012200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012300010605     C                   eval      depfil = VABLNP
012400010601     C                   exsr      repfil
012500010601     C                   if        depfil = invfil
012600021025     C                   if        vlrpoi = 999
012700021025     C                   MOVE(P)   invfil        VABFGS
012800021025     C                   else
012900021025     C                   MOVE(P)   vlrpoi        VABFGS
013000021025     C                   endif
013100010601     C*
013200010601     C                   exsr      PREELA
013300060221     C*
013400060221     C* VALORIZZO CAMPI RELATIVI AL "CMR"
013500060221     C                   EVAL      VABCMR = wCMR
013600060221     C                   EVAL      VABDCM = DATCOR
013700060221     C                   EVAL      VABDTS = DATCOR
013800060221     C                   EVAL      VABHMS = ORACOR
013900060221     C                   EVAL      VABCNT = 1
014000010601     C*
014100010604     C* Ebbene...
014200010604     C*
014300010604     C  N31              ADD       1             §CTROK            7 0
014400010604     C   32              ADD       1             §CTRMO            7 0
014500010604     C   31              ADD       1             §CTRNO            7 0
014600020205     C*
014700020205     C* Effettuo test particolare solo x DA ROLD x condizoinare la scrittura
014800060221     C  N31              WRITE     EDIVAB00
014900010604     C*
015000010604     C                   if        *in31 = *off and
015100010604     C                             *in32 = *off
015200010604     C                   eval      vinflg = '1'
015300010604     C                   else
015400010604     C                   eval      vinflg = '2'
015500010604     C                   endif
015600010604     C                   endif
015700010601     C*
015800010604     C                   endif
015900010604     C*
016000010330     C                   else
016100010330     C                   eval      vinflg = '1'
016200010330     C                   endif
016300010601     C*
016400010601     C  N70              update    tivin000
016500010330     C*
016600010330     C  N70              ENDdo
016700010601     C
016800010601     C*
016900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
017000010601     C                   if        cntNonEl = *zeros or
017100010601     C                             flgMulti = '0'
017200010330     C* Se non ci sono record con errori ...
017300010601     C                   if        §ctrno = 0 and
017400010604     C                             §ctrmo = 0 and
017500010601     C                             flgStato <> '2'
017600010330     C* ... restituisco esito OK.
017700010330     C                   eval      wrkesito = '0'
017800010330     C                   else
017900010330     C                   if        §ctrok > 0
018000010330     C                   eval      wrkesito = '1'
018100010330     C                   else
018200010615     C                   if        flgOk = '0'
018300010615     C                   eval      wrkesito = '2'
018400010615     C                   else
018500010615     C                   eval      wrkesito = '6'
018600010615     C                   endif
018700010330     C                   endif
018800010330     C                   endif
018900010601     C                   else
019000010601     C                   eval      wrkesito = '9'
019100010601     C                   endif
019200010330     C*
019300010330     C                   if        %open(tivin00r)
019400010330     C                   close     tivin00r
019500010330     C                   endif
019600060221     C                   if        %open(edivabwr)
019700060221     C                   close     edivabwr
019800010330     C                   endif
019900010601     C*
020000010601     C                   if        vlrpoi <> 999
020100010601     C                   eval      invfil = vlrpoi
020200010601     C                   endif
020300010330     C*
020400010330     C                   if        §ctrok > 0
020500010601     C                             and invfil > *zeros
020600010330     C                   exsr      invio
020700010330     C                   endif
020800010601     C*
020900010618     C                   if        flgGiro = '1'
021000010601     C                   exsr      endela
021100010618     C                   endif
021200010330     C*
021300010330     C                   ENDSR
021400010330     C***
021500010330
021600010601
021700010601
021800010601
021900010601
022000010330     C*----------------------------------------------------*
022100010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022200010330     C*----------------------------------------------------*
022300010330     C     INZVAR        BEGSR
022400010330     C*
022500020204     C                   Z-ADD     *zeros        Num5_0            5 0
022600020213     C                   Z-ADD     *zeros        Num15_3          15 3
022700020204     C                   MOVEL     *blanks       WrkNote          70
022800010330     C*
022900010330     C                   ENDSR
023000010330     C*----------------------------------------------------*
023100010330     C*  IMPOSTAZIONE CAMPI COSTANTI
023200010330     C*----------------------------------------------------*
023300010330     C     DEFCAM        BEGSR
023400010330     C*
023500020204     C* Inizializzo il buffer del record da scrivere
023600060221     C                   CLEAR                   EDIVAB00
023700020204     C* Imposto i valori di default...
023800020305     C                   EVAL      VABCCM = 1330192
023900020305     C                   EVAL      VABLNP = 133
024000020213     C                   EVAL      VABCTR = 000
024100020204     C* ... e poi verifico se sono stati passati come parametri
024200020204     C                   IF        vlrppt > *blanks
024300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
024400020204     C                   EXSR      CHKNUM
024500020204     C                   IF        PiInt=*on
024600020204     C                   Z-ADD     PiVal         VABCCM
024700020204     C                   ENDIF
024800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024900020204     C                   EXSR      CHKNUM
025000020204     C                   IF        PiInt=*on
025100020204     C                   Z-ADD     PiVal         VABLNP
025200020204     C                   ENDIF
025300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
025400020204     C                   EXSR      CHKNUM
025500020204     C                   IF        PiInt=*on
025600020204     C                   Z-ADD     PiVal         VABCTR
025700020204     C                   ENDIF
025800040218     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
025900020204     C                   ENDIF
026000020204     C*
026100010330     C                   ENDSR
026200010607     C*----------------------------------------------------*
026300060221     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
026400010607     C*----------------------------------------------------*
026500010607     C     IMPVAB        BEGSR
026600010607     C*
026700010607     C                   SETOFF                                       3132
026800010607     C                   MOVEL     datcor        VABAAS
026900010607     C                   MOVE      datcor        VABMGS
027000010607     C*
027100010607     C* Reperimento campi ALFA
027200010607     C*
027300010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
027400030915     C                   EVAL      VABRSD=%trim(%subst(vindta:31:30))
027500020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
027600020117     C     '@':'A'       XLATE     VABRSD        VABRSD
027700020117     C* ==
027800030915     C                   EVAL      VABIND=%trim(%subst(vindta:61:30))
027900030915     C                   EVAL      VABLOD=%trim(%subst(vindta:91:30))
028000030915     C                   EVAL      VABPRD=%trim(%subst(vindta:121:2))
028100030922     C***                EVAL      VABNZD=%trim(%subst(vindta:141:3))
028200090513     C***                EVAL      VABRMA=%trim(%subst(vindta:185:11))
028300090513     C                   EVAL      VABRMA=%trim(%subst(vindta:181:14))
028400010607     C*
028500010607     C* Reperimento campi NUMERICI
028600030304     C* CAD
028700030915     C                   EVAL      PiStr=%trim(%subst(vindta:123:12))
028800010607     C                   EXSR      CHKNUM
028900010607     C                   IF        PiInt=*on
029000020204     C                   Z-ADD     PiVal         Num5_0
029100020204     C                   MOVEL(p)  Num5_0        VABCAD
029200010607     C                   ELSE
029300010607     C                   SETON                                        32
029400020204     C                   EVAL      VABCAD = *zeros
029500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
029600020204     C                             + ' ' + 'VABCAD'
029700010607     C                   ENDIF
029800090317     C* NSP
029900090317     C                   EVAL      PiStr=%trim(%subst(vindta:190:5))
030000011023     C                   EXSR      CHKNUM
030100011023     C                   IF        PiInt=*on
030200040218     C                   Z-ADD     PiVal         VABNSP
030300011023     C                   ELSE
030400090317     C                   SETON                                        31
030500020213     C                   EVAL      vinmsg = %trimr(vinmsg)
030600090317     C                             + ' ' + 'VABNSP'
030700011023     C                   ENDIF
030800090317     C* RMN
030900090513     C***                EVAL      PiStr=%trim(%subst(vindta:190:2))+
031000090513     C***                                %trim(%subst(vindta:190+2+2:4))+
031100090513     C***                                %trim(%subst(vindta:190+2+2+4+1:5))
031200090513     C                   EVAL      PiStr=%trim(%subst(vindta:181:2))+
031300090513     C                                   %trim(%subst(vindta:181+2+2:4))+
031400090513     C                                   %trim(%subst(vindta:181+2+2+4+1:5))
031500090317     C                   EXSR      CHKNUM
031600090317     C                   IF        PiInt=*on
031700090317     C                   Z-ADD     PiVal         VABRMN
031800090317     C                   ELSE
031900090317     C                   SETON                                        32
032000090317     C                   EVAL      VABRMN = *zeros
032100090317     C                   EVAL      vinmsg = %trimr(vinmsg)
032200090317     C                             + ' ' + 'VABRMN'
032300090317     C                   ENDIF
032400030304     C* NCL
032500030915     C                   EVAL      PiStr=%trim(%subst(vindta:271:5))
032600010607     C                   EXSR      CHKNUM
032700010607     C                   IF        PiInt=*on
032800010607     C                   Z-ADD     PiVal         VABNCL
032900010607     C                   ELSE
033000010607     C                   SETON                                        32
033100010607     C                   Z-ADD     *zeros        VABNCL
033200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
033300010607     C                             + ' ' + 'VABNCL'
033400010607     C                   ENDIF
033500030915     C* PKB
033600030915     C                   EVAL      PiStr=%trim(%subst(vindta:203:10))
033700010607     C                   EXSR      CHKNUM
033800010607     C                   IF        PiNum=*on
033900010607     C                   Z-ADD(H)  PiVal         VABPKB
034000010607     C                   ELSE
034100010607     C                   SETON                                        32
034200010607     C                   Z-ADD     *zeros        VABPKB
034300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
034400010607     C                             + ' ' + 'VABPKB'
034500010607     C                   ENDIF
034600030915     C* VLB
034700030915     C                   EVAL      PiStr=%trim(%subst(vindta:223:16))
034800030915     C                   EXSR      CHKNUM
034900030915     C                   IF        PiNum=*on
035000030915     C                   Z-ADD(H)  PiVal         VABVLB
035100030915     C                   ELSE
035200030915     C                   SETON                                        32
035300030915     C                   Z-ADD     *zeros        VABVLB
035400030915     C                   EVAL      vinmsg = %trimr(vinmsg)
035500030915     C                             + ' ' + 'VABVLB'
035600030915     C                   ENDIF
035700070118     C* QFT
035800070118     C                   EVAL      PiStr=%trim(%subst(vindta:223:16))
035900070118     C                   EXSR      CHKNUM
036000070118     C                   IF        PiNum=*on
036100070118     C                   Z-ADD(H)  PiVal         VABQFT
036200070118     C                   ELSE
036300070118     C                   SETON                                        32
036400070118     C                   Z-ADD     *zeros        VABQFT
036500070118     C                   EVAL      vinmsg = %trimr(vinmsg)
036600070118     C                             + ' ' + 'VABQFT'
036700070118     C                   ENDIF
036800030304     C* CAS
036900030915     C                   IF        %subst(vindta:255:16) <> *blanks AND
037000030915     C                             %subst(vindta:255:16) <> '0000000000000,00'
037100030915     C                   EVAL      PiStr=%trim(%subst(vindta:255:16))
037200030304     C                   EXSR      CHKNUM
037300030304     C                   IF        PiNum=*on
037400030304     C                   Z-ADD     PiVal         VABCAS
037500030304     C                   ELSE
037600030304     C                   SETON                                        32
037700030304     C                   Z-ADD     *zeros        VABCAS
037800030304     C                   EVAL      vinmsg = %trimr(vinmsg)
037900030304     C                             + ' ' + 'VABCAS'
038000030304     C                   ENDIF
038100030304     C                   ENDIF
038200020204     C*
038300020204     C* Considerazioni finali sui valori sopra ricavati
038400030915     C                   IF        VABCAS = *zeros
038500020204     C                   EVAL      VABCBO = '1 '
038600020204     C                   ELSE
038700020204     C                   EVAL      VABCBO = '4 '
038800030304     C                   EVAL      VABVCA = 'EUR'
038900020204     C                   ENDIF
039000020204     C*
039100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
039200020204     C                   EXSR      CHKIMPDIV
039300020204     C*
039400010607     C                   ENDSR
039500010607     C*----------------------------------------------------*
039600010601
039700020204
039800020204     C*----------------------------------------------------*
039900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
040000020204     C*----------------------------------------------------*
040100020204     C     CHKIMPDIV     BEGSR
040200020204     C*
040300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
040400020204     C                   Z-ADD     *zeros        wrkDec            9 9
040500020204     C*
040600020204     C* Come prima cosa effettuo considerazioni sulla divisa
040700020204     C                   IF        vabIAS > *zeros
040800020204     C                   IF        vabVAS <> 'EUR'
040900020204     C                   EVAL      vabVAS =  'ITL'
041000020204     C                   ENDIF
041100020204     C                   ENDIF
041200020204     C*
041300020204     C                   IF        vabCAS > *zeros
041400020204     C                   IF        vabVCA <> 'EUR'
041500020204     C                   EVAL      vabVCA =  'ITL'
041600020204     C                   ENDIF
041700020204     C                   ENDIF
041800020204     C*
041900020204     C                   IF        vabVMD > *zeros
042000020204     C                   IF        vabVAD <> 'EUR'
042100020204     C                   EVAL      vabVAD =  'ITL'
042200020204     C                   ENDIF
042300020204     C                   ENDIF
042400020204     C*
042500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
042600020204     C                   Z-ADD     vabIAS        wrkDec
042700020204     C                   IF        wrkDec > *zeros
042800020204     C                   IF        vabVAS = 'ITL'
042900020204     C                   EVAL      vabIAS = *zeros
043000020204     C                   ENDIF
043100020204     C                   ENDIF
043200020204     C*
043300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
043400020204     C                   Z-ADD     vabCAS        wrkDec
043500020204     C                   IF        wrkDec > *zeros
043600020204     C                   IF        vabVCA = 'ITL'
043700020204     C                   EVAL      vabCAS = *zeros
043800020204     C                   ENDIF
043900020204     C                   ENDIF
044000020204     C*
044100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
044200020204     C                   Z-ADD     vabVMD        wrkDec
044300020204     C                   IF        wrkDec > *zeros
044400020204     C                   IF        vabVAD = 'ITL'
044500020204     C                   EVAL      vabVMD = *zeros
044600020204     C                   ENDIF
044700020204     C                   ENDIF
044800020204     C*
044900020204     C                   ENDSR
045000020204     C***
045100020204
045200010330
045300010330
045400010330     C*----------------------------------------------------*
045500010330     C*  CONTROLLO NUMERICITA' CAMPI
045600010330     C*----------------------------------------------------*
045700010330     C     CHKNUM        BEGSR
045800010330     C*
045900010606     C                   IF        PiDecChr = *blanks
046000010606     C                   EVAL      PiDecChr = ','
046100010606     C                   ENDIF
046200010606     C*
046300010606     C                   CALL(e)   'ISNUMERIC'
046400010330     C                   PARM                    PiStr            30
046500010606     C                   PARM                    PiDecChr          1
046600010330     C                   PARM      *ZEROS        PiVal            30 9
046700010330     C                   PARM      '0'           PiInt             1
046800010330     C                   PARM      '0'           PiNum             1
046900010330     C                   IF        %error
047000010606     C                   EVAL      PiNum=*off
047100010330     C                   ENDIF
047200010330     C*
047300010330     C                   ENDSR
047400010330     C***
047500010330
047600010601
047700010601
047800010601
047900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
048000010601     C     repfil        BEGSR
048100010601     C*
048200010601     C                   if        invfil = *zeros and
048300010601     C                             depfil > *zeros and
048400010629     C                             (vinflg = *blanks or
048500010629     C                              vinflg = *zeros)
048600010601     C
048700010601     C                   eval      invfil = depfil
048800010601     C                   endif
048900010601     C*
049000010601     C                   if        depfil <> invfil and
049100010601     C                             invfil > *zeros
049200010601     C                   eval      flgMulti = '1'
049300010601     C                   if        vinflg = *blanks
049400010601     C                   add       1             cntNonEl
049500010601     C                   endif
049600010601     C                   endif
049700010601     C*
049800010601     C                   if        vinflg = '2'
049900010601     C                   eval      flgStato = '2'
050000010601     C                   endif
050100010601     C*
050200010601     C                   ENDSR
050300010601     C***
050400010601
050500010601
050600010601
050700010330
050800010330
050900010330
051000990920      /TITLE Invio dei dati al punto operativo.
051100000613     C     invio         BEGSR
051200990920     C*
051300990920     C                   reset                   dscmz
051400010601     C                   move      invfil        cmzdst
051500990920     C                   eval      cmzfld = vlrfou
051600990920     C                   eval      cmzmbd = vlrhdl
051700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
051800000710     C                   if        prmfir = *blanks
051900060221     C                   eval      cmzfla = 'EDIVAB0F'
052000060221     C                   eval      cmzmba = 'EDIVAB0F'
052100000710     C                   else
052200000710     C                   eval      cmzfla = prmfir
052300000710     C                   eval      cmzmba = prmfir
052400000710     C                   endif
052500990920     C                   eval      cmznrr = *zeros
052600990920     C                   move      §ctrok        cmznrr
052700021018     C                   eval      cmzlba = vlrfl1
052800990920     C                   call(e)   'TIS711C'
052900990920     C                   parm                    dscmz
053000990921     C                   parm      *blanks       esito
053100990920     C                   if        %error
053200990920     C                             or cmzerr = '1'
053300990921     C                             or esito  = '1'
053400000710     C                   eval      wrkesito = '3'
053500990920     C                   endif
053600990920     C*
053700000613     C                   ENDSR
053800990910
053900010601
054000010601
054100010601
054200010601
054300010601      /TITLE Invio dei dati al punto operativo.
054400010601     C     opeini        BEGSR
054500010601     C*
054600010601     C* Inizializzo flag e contatori operativi
054700010601     C                   movel     '0'           flgGiro           1
054800010601     C                   movel     '0'           flgMulti          1
054900010601     C                   movel     '1'           flgStato          1
055000010615     C                   movel     '0'           flgOk             1
055100010601     C                   z-add     *zeros        cntNonEl         10 0
055200010601     C                   z-add     *zeros        depfil            3 0
055300010601     C                   z-add     *zeros        invfil            3 0
055400010601     C*
055500010601     C                   ENDSR
055600010601     C***
055700010601
055800010601
055900010601
056000010330
056100010330
056200000613     C     *inzsr        BEGSR
056300990910     C*
056400990910     C     *entry        plist
056500990920     C                   parm                    tivlrds
056600990921     C                   parm      wrkesito      esito
056700000724     C                   parm                    prmlit
056800000710     C                   parm                    prmfir
056900010330     C*
057000010330     C* CALCOLA LA DATA CORRENTE
057100010330     C                   time                    wn14             14 0
057200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
057300060221     C                   movel     wn14          oracor            6 0          *ORA
057400010330     C                   z-add     wn8           g08dat
057500010330     C                   z-add     *zeros        g08inv
057600010330     C                   movel     '0'           g08err
057700010330     C                   call      'XSRDA8'
057800010330     C                   parm                    wlbda8
057900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
058000060221     C*
058100060221     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
058200060221     C                   move(p)   vlrMSG        wNomeFile
058300060221     C*
058400060221     C* Reperisco il CMR da attribuire alla transazione
058500170426     C                   eval      wCMR = %subst(wNomeFile:6:11)
058600000613     C*
058700000613     C                   ENDSR
058800000613     C***
