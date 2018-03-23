000100090219      /TITLE Upload via Internet: traduzione in EDIVABWR. (x BENLOG/BENETTON - Cod. 0590586/0590803)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600090219     FEDIVABwr  O    E             DISK    usropn
000700010607     Ftis7blp   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900010607     Ftis7blps  O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100000313     D*
001200000613     D*------------
001300000613     D* COMANDI
001400000613     D*------------
001500011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600010330     D*----------------------------------------------------
001700010330     D* DICHIARAZIOINE VARIABILI DI WRK
001800010330     D*----------------------------------------------------
001900010330     D dscmz         e ds                  inz
002000010330     D psds           sds
002100010330     D  procname         *PROC
002200010330     D tivlrds       e ds                  extname(tivlr00f)
002300010330     D esito           s              1
002400010330     D prmlit          s             10
002500010330     D prmfir          s             10
002600010330     D wrkesito        s                   like(esito)
002700010330     D wrkdata         s               d
002800010330     D wrkora          s               t
002900010330     D rrnum           s              6  0 INZ(*zeros)
003000010330     D recko           s            150    INZ(*blanks)
003100011113     D depcmd          s            150    INZ(*blanks)
003200010330     D*------------------
003300010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003400010330     D*------------------
003500010330     D WLBDA8          DS                  INZ
003600010330     D  G08DAT                 1      8  0
003700010330     D  G08INV                 9     16  0
003800010330     D  G08ERR                17     17
003900010330     D  G08TGI                18     22  0
004000000313     D*
004100010330
004200010330
004300010330
004400990915     C                   time                    wrkdata
004500990915     C                   time                    wrkora
004600000913     C                   reset                   rrnum
004700990921     C                   reset                   esito
004800990921     C                   reset                   wrkesito
004900010601     C*
005000010601     C                   exsr      opeini
005100010605     C                   exsr      rwvab
005200010601     C*
005300010601     C                   seton                                        lr
005400010601
005500010601
005600010601
005700010601
005800010601     C*--------------------------------------------------------
005900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006000010601     C*--------------------------------------------------------
006100010601     C     PREELA        BEGSR
006200010601     C*
006300010601     C* SE OCCORRE SPEDIRE IN FILIALE
006400010601     C                   if        invfil <> *zeros and
006500010601     C                             flgGiro = '0'
006600010601     C*
006700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006800010601     C                   eval      flgGiro = '1'
006900000724     C*
007000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007200010601     C     invfil        chain     azorg01l
007300000724     C                   if        %found
007400000724     C                   movel(p)  CMD(1)        depcmd
007500020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007600000724     C*
007700000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007800011113     C                   Z-ADD     150           LENGH            15 5
007900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008000000724     C                   PARM                    depcmd
008100000724     C                   PARM                    LENGH
008200000724     C*
008300000724     C                   endif
008400000616     C*
008500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008600000616     C                   movel(p)  CMD(2)        depcmd
008700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008800000616     C*
008900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009000011113     C                   Z-ADD     150           LENGH            15 5
009100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009200000616     C                   PARM                    depcmd
009300000616     C                   PARM                    LENGH
009400000613     C*
009500010607     C                   if        not %open(tis7blps)
009600010607     C                   open      tis7blps
009700000616     C                   except    testdett
009800000613     C                   endif
009900010601     C*
010000010601     C                   endif
010100010601     C*
010200010601     C                   ENDSR
010300010601     C***
010400010601
010500010601
010600010601
010700010601
010800010601
010900010601
011000010601
011100010601     C*--------------------------------------------------------
011200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011300010601     C*--------------------------------------------------------
011400010601     C     ENDELA        BEGSR
011500990915     C*
011600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011700000613     C*
011800010607     C                   if        %open(tis7blps)
011900000616     C                   except    findett
012000010607     C                   close     tis7blps
012100000613     C                   endif
012200000616     C*
012300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012400011113     C                   Z-ADD     150           LENGH            15 5
012500010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012600000616     C                   PARM                    CMD(3)
012700000616     C                   PARM                    LENGH
012800000616     C*
012900010601     C                   ENDSR
013000010601     C***
013100010601
013200010601
013300010601
013400010601
013500000613
013600000613     C*--------------------------------------------------------
013700000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013800000613     C*--------------------------------------------------------
013900000621     C     STPR          BEGSR
014000000613     C*
014100010607     C                   if        not %open(tis7blp)
014200010607     C                   open      tis7blp
014300990915     C                   endif
014400990915     C*
014500990915     C                   except    riepilogo
014600990915     C*
014700010607     C                   if        %open(tis7blp)
014800010607     C                   close     tis7blp
014900990914     C                   endif
015000000613     C*
015100000613     C                   ENDSR
015200000613     C***
015300000613
015400010330
015500010601
015600010601
015700010601
015800010330     C*--------------------------------------------------------
015900090219     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
016000010330     C*--------------------------------------------------------
016100010605     C     RWVAB         BEGSR
016200010330     C*
016300010330     C                   if        not %open(tivin00r)
016400010330     C                   open      tivin00r
016500010330     C                   endif
016600090219     C                   if        not %open(edivabwr)
016700090219     C                   open      edivabwr
016800010330     C                   endif
016900010330     C*
017000010604     C                   clear                   §CTROK
017100010604     C                   clear                   §CTRMO
017200010604     C                   clear                   §CTRNO
017300010330     C*
017400010330     C                   DO        *HIVAL
017500010330     C*
017600010330     C                   READ      tivin00r                               70
017700010618     C*
017800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017900010618     C                   if        vinflg = '1'
018000010618     C                   eval      flgOk = '1'
018100010618     C                   endif
018200010618     C*
018300010330     C                   if        vindta > *blanks
018400010330     C                   add       1             rrnum
018500010330     C*
018600010601     C                   if        *in70 = *off and
018700010330     C                             (vinflg = *blanks
018800010330     C                              or vinflg = '0'
018900010330     C                              or vinflg = '2')
019000010330     C*
019100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019200010711     C                   if        vinflg = *blanks or vinflg = '0'
019300010711     C                   clear                   vinmsg
019400010711     C                   endif
019500010601     C*
019600010330     C                   exsr      inzvar
019700010330     C                   exsr      defcam
019800010605     C                   exsr      impvab
019900010601     C*
020000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020100010605     C                   eval      depfil = VABLNP
020200010601     C                   exsr      repfil
020300010601     C                   if        depfil = invfil
020400021113     C                   if        vlrpoi = 999
020500021113     C                   MOVE(P)   invfil        VABFGS
020600021113     C                   else
020700021113     C                   MOVE(P)   vlrpoi        VABFGS
020800021113     C                   endif
020900010601     C*
021000010601     C                   exsr      PREELA
021100090219     C*
021200090219     C* VALORIZZO CAMPI RELATIVI AL "CMR"
021300090219     C                   move(p)   vlrMSG        wNomeFile        30
021400090219     C                   EVAL      VABCMR = %subst(wNomeFile:1:7)
021500090219     C                   EVAL      VABDCM = DATCOR
021600090219     C                   EVAL      VABDTS = DATCOR
021700090219     C                   EVAL      VABHMS = ORACOR
021800090219     C                   EVAL      VABCNT = 1
021900010601     C*
022000010604     C* Ebbene...
022100010604     C*
022200010604     C  N31              ADD       1             §CTROK            7 0
022300010604     C   32              ADD       1             §CTRMO            7 0
022400010604     C   31              ADD       1             §CTRNO            7 0
022500090219     C  N31              WRITE     EDIVAB00
022600010604     C*
022700010604     C                   if        *in31 = *off and
022800010604     C                             *in32 = *off
022900010604     C                   eval      vinflg = '1'
023000010604     C                   else
023100010604     C                   eval      recko = vindta
023200010604     C                   except    rigadett
023300010604     C                   eval      vinflg = '2'
023400010604     C                   endif
023500010604     C                   endif
023600010601     C*
023700010604     C                   endif
023800010604     C*
023900010330     C                   else
024000010330     C                   eval      vinflg = '1'
024100010330     C                   endif
024200010601     C*
024300010601     C  N70              update    tivin000
024400010330     C*
024500010330     C  N70              ENDdo
024600010601     C
024700010601     C*
024800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024900010601     C                   if        cntNonEl = *zeros or
025000010601     C                             flgMulti = '0'
025100010330     C* Se non ci sono record con errori ...
025200010601     C                   if        §ctrno = 0 and
025300010604     C                             §ctrmo = 0 and
025400010601     C                             flgStato <> '2'
025500010330     C* ... restituisco esito OK.
025600010330     C                   eval      wrkesito = '0'
025700010330     C                   else
025800010330     C                   if        §ctrok > 0
025900010330     C                   eval      wrkesito = '1'
026000010330     C                   else
026100010615     C                   if        flgOk = '0'
026200010615     C                   eval      wrkesito = '2'
026300010615     C                   else
026400010615     C                   eval      wrkesito = '6'
026500010615     C                   endif
026600010330     C                   endif
026700010330     C                   endif
026800010601     C                   else
026900010601     C                   eval      wrkesito = '9'
027000010601     C                   endif
027100010330     C*
027200010330     C                   if        %open(tivin00r)
027300010330     C                   close     tivin00r
027400010330     C                   endif
027500090219     C                   if        %open(edivabwr)
027600090219     C                   close     edivabwr
027700010330     C                   endif
027800010601     C*
027900010601     C                   if        vlrpoi <> 999
028000010601     C                   eval      invfil = vlrpoi
028100010601     C                   endif
028200010330     C*
028300010330     C                   if        §ctrok > 0
028400010601     C                             and invfil > *zeros
028500010330     C                   exsr      invio
028600010330     C                   endif
028700010601     C*
028800010618     C                   if        flgGiro = '1'
028900010601     C                   exsr      endela
029000010618     C                   endif
029100010330     C*
029200010330     C                   ENDSR
029300010330     C***
029400010330
029500010601
029600010601
029700010601
029800010601
029900010330     C*----------------------------------------------------*
030000010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030100010330     C*----------------------------------------------------*
030200010330     C     INZVAR        BEGSR
030300010330     C*
030400071220     C                   MOVEL     *blanks       wFlgCCM           1
030500010330     C*
030600010330     C                   ENDSR
030700010330     C*----------------------------------------------------*
030800010330     C*  IMPOSTAZIONE CAMPI COSTANTI
030900010330     C*----------------------------------------------------*
031000010330     C     DEFCAM        BEGSR
031100010330     C*
031200090219     C                   CLEAR                   EDIVAB00
031300010607     C                   EVAL      VABLNP = 059
031400010607     C                   EVAL      VABCTR = 500
031500010607     C                   EVAL      VABCBO = '1 '
031600010608     C                   EVAL      VABCTM = '7Q'
031700071220     C*
031800071220     C* ...verifico se sono stati passati come parametri
031900071220     C                   IF        vlrppt > *blanks
032000071220     C                   EVAL      wFlgCCM=%trim(%subst(vlrppt:1:1))
032100071220     C                   IF        %subst(vlrppt:2:7) <> *blanks
032200071220     C                   EVAL      PiStr=%trim(%subst(vlrppt:2:7))
032300071220     C                   EXSR      CHKNUM
032400071220     C                   IF        PiInt=*on
032500071220     C                   Z-ADD     PiVal         VABCCM
032600071220     C                   ENDIF
032700071220     C                   ENDIF
032800071220     C                   IF        %subst(vlrppt:9:3) <> *blanks
032900071220     C                   EVAL      PiStr=%trim(%subst(vlrppt:9:3))
033000071220     C                   EXSR      CHKNUM
033100071220     C                   IF        PiInt=*on
033200071220     C                   Z-ADD     PiVal         VABLNP
033300071220     C                   ENDIF
033400071220     C                   ENDIF
033500071220     C                   ENDIF
033600071220     C*
033700010330     C                   ENDSR
033800010607     C*----------------------------------------------------*
033900090219     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
034000010607     C*----------------------------------------------------*
034100010607     C     IMPVAB        BEGSR
034200010607     C*
034300010607     C                   SETOFF                                       3132
034400010607     C                   MOVEL     datcor        VABAAS
034500010607     C                   MOVE      datcor        VABMGS
034600010607     C*
034700010607     C* Reperimento campi ALFA
034800010607     C*
034900010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
035000010607     C                   EVAL      VABRSD=%trim(%subst(vindta:36:35))
035100020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035200020117     C     '@':'A'       XLATE     VABRSD        VABRSD
035300020117     C* ==
035400010607     C                   EVAL      VABIND=%trim(%subst(vindta:86:35))
035500010607     C                   EVAL      VABCAD=%trim(%subst(vindta:136:5))
035600010607     C                   EVAL      VABLOD=%trim(%subst(vindta:141:26))
035700010607     C                   EVAL      VABPRD=%trim(%subst(vindta:171:2))
035800010607     C*
035900010607     C* Compongo il riferimento alfanumerico
036000010607     C                   EVAL      VABRMA = %trim(%subst(vindta:16:2)) +
036100010607     C                                      '         '                +
036200010607     C                                      %trim(%subst(vindta:8:4))
036300010607     C*
036400010607     C* Reperimento campi NUMERICI
036500010607     C*
036600071220     C* Considerazioni sul cliente mittente (solo se richiesto)
036700071220     C                   IF        wFlgCCM = 'N'
036800011210     C                   IF        %trim(%subst(vindta:19:2)) = 'BA'
036900031201     C                   EVAL      VABCCM = 0590803
037000010607     C                   ELSE
037100011210     C                   IF        %trim(%subst(vindta:19:2)) = 'N '
037200011210     C                   EVAL      VABCCM = 0590586
037300011210     C                   ELSE
037400011210     C                   EVAL      VABCCM = 0590586
037500011210     C                   EVAL      VABCTM = *blanks
037600011210     C                   ENDIF
037700011210     C*                  SETON                                        31
037800011210     C*                  Z-ADD     *zeros        VABCCM
037900011210     C*                  EVAL      vinmsg = %trimr(vinmsg)
038000011210     C*                            + ' ' + 'VABCCM'
038100011210     C*                  ENDIF
038200010607     C                   ENDIF
038300071220     C                   ENDIF
038400010607     C*
038500040531     C***                EVAL      PiStr=%trim(%subst(vindta:29:6)) +
038600040531     C***                                %trim(%subst(vindta:11:1))
038700040531     C                   EVAL      PiStr=%trim(%subst(vindta:122:7))
038800010607     C                   EXSR      CHKNUM
038900010607     C                   IF        PiInt=*on
039000010607     C                   Z-ADD     PiVal         VABNSP
039100040531     C                   Z-ADD     PiVal         VABRMN
039200010607     C                   ELSE
039300010607     C                   SETON                                        32
039400010607     C                   Z-ADD     *zeros        VABNSP
039500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039600040531     C                             + ' ' + 'VABNSP VABRMN'
039700010607     C                   ENDIF
039800011023     C*
039900040531     C***                EVAL      PiStr=%trim(%subst(vindta:2:5))+
040000040531     C***                                %trim(%subst(vindta:29:6))
040100040531     C***                EXSR      CHKNUM
040200040531     C***                IF        PiInt=*on
040300040531     C***                Z-ADD     PiVal         VABRMN
040400040531     C***                ELSE
040500040531     C***                SETON                                        32
040600040531     C***                Z-ADD     *zeros        VABRMN
040700040531     C***                EVAL      vinmsg = %trimr(vinmsg)
040800040531     C***                          + ' ' + 'VABRMN'
040900040531     C***                ENDIF
041000010607     C*
041100010607     C                   EVAL      PiStr=%trim(%subst(vindta:173:4))
041200010607     C                   EXSR      CHKNUM
041300010607     C                   IF        PiInt=*on
041400010607     C                   Z-ADD     PiVal         VABNCL
041500010607     C                   ELSE
041600010607     C                   SETON                                        32
041700010607     C                   Z-ADD     *zeros        VABNCL
041800010607     C                   EVAL      vinmsg = %trimr(vinmsg)
041900010607     C                             + ' ' + 'VABNCL'
042000010607     C                   ENDIF
042100010607     C*
042200010607     C                   EVAL      PiStr=%trim(%subst(vindta:177:6))
042300010607     C                   EVAL      PiDecChr = '.'
042400010607     C                   EXSR      CHKNUM
042500010607     C                   IF        PiNum=*on
042600010607     C                   Z-ADD(H)  PiVal         VABPKB
042700010607     C                   ELSE
042800010607     C                   SETON                                        32
042900010607     C                   Z-ADD     *zeros        VABPKB
043000010607     C                   EVAL      vinmsg = %trimr(vinmsg)
043100010607     C                             + ' ' + 'VABPKB'
043200010607     C                   ENDIF
043300010607     C*
043400010607     C                   EVAL      PiStr=%trim(%subst(vindta:183:6))
043500010607     C                   EVAL      PiDecChr = '.'
043600010607     C                   EXSR      CHKNUM
043700010607     C                   IF        PiNum=*on
043800010607     C                   Z-ADD(H)  PiVal         VABVLB
043900010607     C                   Z-ADD(H)  PiVal         VABQFT
044000010607     C                   ELSE
044100010607     C                   SETON                                        32
044200010607     C                   Z-ADD     *zeros        VABVLB
044300010607     C                   Z-ADD     *zeros        VABQFT
044400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
044500010607     C                             + ' ' + 'VABVLB VABQFT'
044600010607     C                   ENDIF
044700010607     C*
044800010607     C                   ENDSR
044900010607     C*----------------------------------------------------*
045000010601
045100010330
045200010330
045300010330     C*----------------------------------------------------*
045400010330     C*  CONTROLLO NUMERICITA' CAMPI
045500010330     C*----------------------------------------------------*
045600010330     C     CHKNUM        BEGSR
045700010330     C*
045800010606     C                   IF        PiDecChr = *blanks
045900010606     C                   EVAL      PiDecChr = ','
046000010606     C                   ENDIF
046100010606     C*
046200010606     C                   CALL(e)   'ISNUMERIC'
046300010330     C                   PARM                    PiStr            30
046400010606     C                   PARM                    PiDecChr          1
046500010330     C                   PARM      *ZEROS        PiVal            30 9
046600010330     C                   PARM      '0'           PiInt             1
046700010330     C                   PARM      '0'           PiNum             1
046800010330     C                   IF        %error
046900010606     C                   EVAL      PiNum=*off
047000010330     C                   ENDIF
047100010330     C*
047200010330     C                   ENDSR
047300010330     C***
047400010330
047500010601
047600010601
047700010601
047800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
047900010601     C     repfil        BEGSR
048000010601     C*
048100010601     C                   if        invfil = *zeros and
048200010601     C                             depfil > *zeros and
048300010629     C                             (vinflg = *blanks or
048400010629     C                              vinflg = *zeros)
048500010601     C
048600010601     C                   eval      invfil = depfil
048700010601     C                   endif
048800010601     C*
048900010601     C                   if        depfil <> invfil and
049000010601     C                             invfil > *zeros
049100010601     C                   eval      flgMulti = '1'
049200010601     C                   if        vinflg = *blanks
049300010601     C                   add       1             cntNonEl
049400010601     C                   endif
049500010601     C                   endif
049600010601     C*
049700010601     C                   if        vinflg = '2'
049800010601     C                   eval      flgStato = '2'
049900010601     C                   endif
050000010601     C*
050100010601     C                   ENDSR
050200010601     C***
050300010601
050400010601
050500010601
050600010330
050700010330
050800010330
050900990920      /TITLE Invio dei dati al punto operativo.
051000000613     C     invio         BEGSR
051100990920     C*
051200990920     C                   reset                   dscmz
051300010601     C                   move      invfil        cmzdst
051400990920     C                   eval      cmzfld = vlrfou
051500990920     C                   eval      cmzmbd = vlrhdl
051600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
051700000710     C                   if        prmfir = *blanks
051800090219     C                   eval      cmzfla = 'EDIVAB0F'
051900090219     C                   eval      cmzmba = 'EDIVAB0F'
052000000710     C                   else
052100000710     C                   eval      cmzfla = prmfir
052200000710     C                   eval      cmzmba = prmfir
052300000710     C                   endif
052400990920     C                   eval      cmznrr = *zeros
052500990920     C                   move      §ctrok        cmznrr
052600021111     C                   eval      cmzlba = vlrfl1
052700990920     C                   call(e)   'TIS711C'
052800990920     C                   parm                    dscmz
052900990921     C                   parm      *blanks       esito
053000990920     C                   if        %error
053100990920     C                             or cmzerr = '1'
053200990921     C                             or esito  = '1'
053300000710     C                   eval      wrkesito = '3'
053400990920     C                   endif
053500990920     C*
053600000613     C                   ENDSR
053700990910
053800010601
053900010601
054000010601
054100010601
054200010601      /TITLE Invio dei dati al punto operativo.
054300010601     C     opeini        BEGSR
054400010601     C*
054500010601     C* Inizializzo flag e contatori operativi
054600010601     C                   movel     '0'           flgGiro           1
054700010601     C                   movel     '0'           flgMulti          1
054800010601     C                   movel     '1'           flgStato          1
054900010615     C                   movel     '0'           flgOk             1
055000010601     C                   z-add     *zeros        cntNonEl         10 0
055100010601     C                   z-add     *zeros        depfil            3 0
055200010601     C                   z-add     *zeros        invfil            3 0
055300010601     C*
055400010601     C                   ENDSR
055500010601     C***
055600010601
055700010601
055800010601
055900010330
056000010330
056100000613     C     *inzsr        BEGSR
056200990910     C*
056300990910     C     *entry        plist
056400990920     C                   parm                    tivlrds
056500990921     C                   parm      wrkesito      esito
056600000724     C                   parm                    prmlit
056700000710     C                   parm                    prmfir
056800010330     C*
056900010330     C* CALCOLA LA DATA CORRENTE
057000010330     C                   time                    wn14             14 0
057100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
057200090219     C                   movel     wn14          oracor            6 0          *ORA
057300010330     C                   z-add     wn8           g08dat
057400010330     C                   z-add     *zeros        g08inv
057500010330     C                   movel     '0'           g08err
057600010330     C                   call      'XSRDA8'
057700010330     C                   parm                    wlbda8
057800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
057900000613     C*
058000000613     C                   ENDSR
058100000613     C***
058200990908
058300010607     Otis7blp   E            riepilogo         2
058400990915     O                                              'Upload via Internet'
058500990915     O                                           +1 'Traduzione TIVIN00R -'
058600090219     O                                           +1 'EDIVABWR'
058700990915     O                                           +1 'Testate bolle'
058800990915     O          E            riepilogo   2
058900990915     O                       wrkdata
059000990915     O                       wrkora              +1
059100990915     O                       procname            +1
059200990915     O          E            riepilogo   2
059300990915     O                                              'Cliente..................:'
059400010605     O                       VABCCM        z     +1
059500990915     O          E            riepilogo   2
059600990920     O                                              'Riferimento Strategi.....:'
059700990920     O                       vlrhdl              +1
059800990915     O          E            riepilogo   2
059900990915     O                                              'Giusti...................:'
060000971022     O                       §CTROK        2   +  1
060100990915     O          E            riepilogo   2
060200990915     O                                              'Sbagliati e corretti.....:'
060300971022     O                       §CTRMO        2   +  1
060400990915     O          E            riepilogo   2
060500990915     O                                              'Sbagliati e scartati.....:'
060600971022     O                       §CTRNO        2   +  1
060700000613
060800010607     Otis7blps  E            testdett          2
060900000613     O                                              'Upload via Internet'
061000000613     O                                           +1 'Traduzione TIVIN00R -'
061100090219     O                                           +1 'EDIVABWR'
061200000613     O                                           +1 'Testate bolle'
061300000616     O          E            testdett    3
061400000613     O                                           +2 'N° rec'
061500000613     O                                           +3 'Anteprima contenuto'
061600000616     O          E            rigadett    2
061700000613     O                       rrnum               +2
061800000621     O                       recko               +3
061900000616     O          E            findett     2
062000000613     O                       wrkdata
062100000613     O                       wrkora              +1
062200000613     O                       procname            +1
062300000616     O          E            findett     2
062400000613     O                                              'Cliente..................:'
062500010605     O                       VABCCM        z     +1
062600000616     O          E            findett     2
062700000613     O                                              'Riferimento Strategi.....:'
062800000613     O                       vlrhdl              +1
062900000616     O          E            findett     2
063000000613     O                                              'Giusti...................:'
063100000613     O                       §CTROK        2   +  1
063200000616     O          E            findett     2
063300000613     O                                              'Sbagliati e corretti.....:'
063400000613     O                       §CTRMO        2   +  1
063500000616     O          E            findett     2
063600000613     O                                              'Sbagliati e scartati.....:'
063700000613     O                       §CTRNO        2   +  1
063800000613** CMD - COMANDI CL
063900011113OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7BLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
064000011113OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7BLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
064100010607DLTOVR FILE(TIS7BLP TIS7BLPS) LVL(*)
064200010606
064300000613
