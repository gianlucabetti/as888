000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR. (x BENLOG/BENETTON - Cod. 0590586/0590803)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
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
015900021113     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
016000010330     C*--------------------------------------------------------
016100010605     C     RWVAB         BEGSR
016200010330     C*
016300010330     C                   if        not %open(tivin00r)
016400010330     C                   open      tivin00r
016500010330     C                   endif
016600021113     C                   if        not %open(fivabwwr)
016700021113     C                   open      fivabwwr
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
021100010601     C*
021200010604     C* Ebbene...
021300010604     C*
021400010604     C  N31              ADD       1             §CTROK            7 0
021500010604     C   32              ADD       1             §CTRMO            7 0
021600010604     C   31              ADD       1             §CTRNO            7 0
021700021113     C  N31              WRITE     FIVAB000
021800010604     C*
021900010604     C                   if        *in31 = *off and
022000010604     C                             *in32 = *off
022100010604     C                   eval      vinflg = '1'
022200010604     C                   else
022300010604     C                   eval      recko = vindta
022400010604     C                   except    rigadett
022500010604     C                   eval      vinflg = '2'
022600010604     C                   endif
022700010604     C                   endif
022800010601     C*
022900010604     C                   endif
023000010604     C*
023100010330     C                   else
023200010330     C                   eval      vinflg = '1'
023300010330     C                   endif
023400010601     C*
023500010601     C  N70              update    tivin000
023600010330     C*
023700010330     C  N70              ENDdo
023800010601     C
023900010601     C*
024000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024100010601     C                   if        cntNonEl = *zeros or
024200010601     C                             flgMulti = '0'
024300010330     C* Se non ci sono record con errori ...
024400010601     C                   if        §ctrno = 0 and
024500010604     C                             §ctrmo = 0 and
024600010601     C                             flgStato <> '2'
024700010330     C* ... restituisco esito OK.
024800010330     C                   eval      wrkesito = '0'
024900010330     C                   else
025000010330     C                   if        §ctrok > 0
025100010330     C                   eval      wrkesito = '1'
025200010330     C                   else
025300010615     C                   if        flgOk = '0'
025400010615     C                   eval      wrkesito = '2'
025500010615     C                   else
025600010615     C                   eval      wrkesito = '6'
025700010615     C                   endif
025800010330     C                   endif
025900010330     C                   endif
026000010601     C                   else
026100010601     C                   eval      wrkesito = '9'
026200010601     C                   endif
026300010330     C*
026400010330     C                   if        %open(tivin00r)
026500010330     C                   close     tivin00r
026600010330     C                   endif
026700021113     C                   if        %open(fivabwwr)
026800021113     C                   close     fivabwwr
026900010330     C                   endif
027000010601     C*
027100010601     C                   if        vlrpoi <> 999
027200010601     C                   eval      invfil = vlrpoi
027300010601     C                   endif
027400010330     C*
027500010330     C                   if        §ctrok > 0
027600010601     C                             and invfil > *zeros
027700010330     C                   exsr      invio
027800010330     C                   endif
027900010601     C*
028000010618     C                   if        flgGiro = '1'
028100010601     C                   exsr      endela
028200010618     C                   endif
028300010330     C*
028400010330     C                   ENDSR
028500010330     C***
028600010330
028700010601
028800010601
028900010601
029000010601
029100010330     C*----------------------------------------------------*
029200010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029300010330     C*----------------------------------------------------*
029400010330     C     INZVAR        BEGSR
029500010330     C*
029600071220     C                   MOVEL     *blanks       wFlgCCM           1
029700010330     C*
029800010330     C                   ENDSR
029900010330     C*----------------------------------------------------*
030000010330     C*  IMPOSTAZIONE CAMPI COSTANTI
030100010330     C*----------------------------------------------------*
030200010330     C     DEFCAM        BEGSR
030300010330     C*
030400021113     C                   CLEAR                   FIVAB000
030500010607     C                   EVAL      VABLNP = 059
030600010607     C                   EVAL      VABCTR = 500
030700010607     C                   EVAL      VABCBO = '1 '
030800010608     C                   EVAL      VABCTM = '7Q'
030900071220     C*
031000071220     C* ...verifico se sono stati passati come parametri
031100071220     C                   IF        vlrppt > *blanks
031200071220     C                   EVAL      wFlgCCM=%trim(%subst(vlrppt:1:1))
031300071220     C                   IF        %subst(vlrppt:2:7) <> *blanks
031400071220     C                   EVAL      PiStr=%trim(%subst(vlrppt:2:7))
031500071220     C                   EXSR      CHKNUM
031600071220     C                   IF        PiInt=*on
031700071220     C                   Z-ADD     PiVal         VABCCM
031800071220     C                   ENDIF
031900071220     C                   ENDIF
032000071220     C                   IF        %subst(vlrppt:9:3) <> *blanks
032100071220     C                   EVAL      PiStr=%trim(%subst(vlrppt:9:3))
032200071220     C                   EXSR      CHKNUM
032300071220     C                   IF        PiInt=*on
032400071220     C                   Z-ADD     PiVal         VABLNP
032500071220     C                   ENDIF
032600071220     C                   ENDIF
032700071220     C                   ENDIF
032800071220     C*
032900010330     C                   ENDSR
033000010607     C*----------------------------------------------------*
033100021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033200010607     C*----------------------------------------------------*
033300010607     C     IMPVAB        BEGSR
033400010607     C*
033500010607     C                   SETOFF                                       3132
033600010607     C                   MOVEL     datcor        VABAAS
033700010607     C                   MOVE      datcor        VABMGS
033800010607     C*
033900010607     C* Reperimento campi ALFA
034000010607     C*
034100010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034200010607     C                   EVAL      VABRSD=%trim(%subst(vindta:36:35))
034300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034500020117     C* ==
034600010607     C                   EVAL      VABIND=%trim(%subst(vindta:86:35))
034700010607     C                   EVAL      VABCAD=%trim(%subst(vindta:136:5))
034800010607     C                   EVAL      VABLOD=%trim(%subst(vindta:141:26))
034900010607     C                   EVAL      VABPRD=%trim(%subst(vindta:171:2))
035000010607     C*
035100010607     C* Compongo il riferimento alfanumerico
035200010607     C                   EVAL      VABRMA = %trim(%subst(vindta:16:2)) +
035300010607     C                                      '         '                +
035400010607     C                                      %trim(%subst(vindta:8:4))
035500010607     C*
035600010607     C* Reperimento campi NUMERICI
035700010607     C*
035800071220     C* Considerazioni sul cliente mittente (solo se richiesto)
035900071220     C                   IF        wFlgCCM = 'N'
036000011210     C                   IF        %trim(%subst(vindta:19:2)) = 'BA'
036100031201     C                   EVAL      VABCCM = 0590803
036200010607     C                   ELSE
036300011210     C                   IF        %trim(%subst(vindta:19:2)) = 'N '
036400011210     C                   EVAL      VABCCM = 0590586
036500011210     C                   ELSE
036600011210     C                   EVAL      VABCCM = 0590586
036700011210     C                   EVAL      VABCTM = *blanks
036800011210     C                   ENDIF
036900011210     C*                  SETON                                        31
037000011210     C*                  Z-ADD     *zeros        VABCCM
037100011210     C*                  EVAL      vinmsg = %trimr(vinmsg)
037200011210     C*                            + ' ' + 'VABCCM'
037300011210     C*                  ENDIF
037400010607     C                   ENDIF
037500071220     C                   ENDIF
037600010607     C*
037700040531     C***                EVAL      PiStr=%trim(%subst(vindta:29:6)) +
037800040531     C***                                %trim(%subst(vindta:11:1))
037900040531     C                   EVAL      PiStr=%trim(%subst(vindta:122:7))
038000010607     C                   EXSR      CHKNUM
038100010607     C                   IF        PiInt=*on
038200010607     C                   Z-ADD     PiVal         VABNSP
038300040531     C                   Z-ADD     PiVal         VABRMN
038400010607     C                   ELSE
038500010607     C                   SETON                                        32
038600010607     C                   Z-ADD     *zeros        VABNSP
038700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
038800040531     C                             + ' ' + 'VABNSP VABRMN'
038900010607     C                   ENDIF
039000011023     C*
039100040531     C***                EVAL      PiStr=%trim(%subst(vindta:2:5))+
039200040531     C***                                %trim(%subst(vindta:29:6))
039300040531     C***                EXSR      CHKNUM
039400040531     C***                IF        PiInt=*on
039500040531     C***                Z-ADD     PiVal         VABRMN
039600040531     C***                ELSE
039700040531     C***                SETON                                        32
039800040531     C***                Z-ADD     *zeros        VABRMN
039900040531     C***                EVAL      vinmsg = %trimr(vinmsg)
040000040531     C***                          + ' ' + 'VABRMN'
040100040531     C***                ENDIF
040200010607     C*
040300010607     C                   EVAL      PiStr=%trim(%subst(vindta:173:4))
040400010607     C                   EXSR      CHKNUM
040500010607     C                   IF        PiInt=*on
040600010607     C                   Z-ADD     PiVal         VABNCL
040700010607     C                   ELSE
040800010607     C                   SETON                                        32
040900010607     C                   Z-ADD     *zeros        VABNCL
041000010607     C                   EVAL      vinmsg = %trimr(vinmsg)
041100010607     C                             + ' ' + 'VABNCL'
041200010607     C                   ENDIF
041300010607     C*
041400010607     C                   EVAL      PiStr=%trim(%subst(vindta:177:6))
041500010607     C                   EVAL      PiDecChr = '.'
041600010607     C                   EXSR      CHKNUM
041700010607     C                   IF        PiNum=*on
041800010607     C                   Z-ADD(H)  PiVal         VABPKB
041900010607     C                   ELSE
042000010607     C                   SETON                                        32
042100010607     C                   Z-ADD     *zeros        VABPKB
042200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
042300010607     C                             + ' ' + 'VABPKB'
042400010607     C                   ENDIF
042500010607     C*
042600010607     C                   EVAL      PiStr=%trim(%subst(vindta:183:6))
042700010607     C                   EVAL      PiDecChr = '.'
042800010607     C                   EXSR      CHKNUM
042900010607     C                   IF        PiNum=*on
043000010607     C                   Z-ADD(H)  PiVal         VABVLB
043100010607     C                   Z-ADD(H)  PiVal         VABQFT
043200010607     C                   ELSE
043300010607     C                   SETON                                        32
043400010607     C                   Z-ADD     *zeros        VABVLB
043500010607     C                   Z-ADD     *zeros        VABQFT
043600010607     C                   EVAL      vinmsg = %trimr(vinmsg)
043700010607     C                             + ' ' + 'VABVLB VABQFT'
043800010607     C                   ENDIF
043900010607     C*
044000010607     C                   ENDSR
044100010607     C*----------------------------------------------------*
044200010601
044300010330
044400010330
044500010330     C*----------------------------------------------------*
044600010330     C*  CONTROLLO NUMERICITA' CAMPI
044700010330     C*----------------------------------------------------*
044800010330     C     CHKNUM        BEGSR
044900010330     C*
045000010606     C                   IF        PiDecChr = *blanks
045100010606     C                   EVAL      PiDecChr = ','
045200010606     C                   ENDIF
045300010606     C*
045400010606     C                   CALL(e)   'ISNUMERIC'
045500010330     C                   PARM                    PiStr            30
045600010606     C                   PARM                    PiDecChr          1
045700010330     C                   PARM      *ZEROS        PiVal            30 9
045800010330     C                   PARM      '0'           PiInt             1
045900010330     C                   PARM      '0'           PiNum             1
046000010330     C                   IF        %error
046100010606     C                   EVAL      PiNum=*off
046200010330     C                   ENDIF
046300010330     C*
046400010330     C                   ENDSR
046500010330     C***
046600010330
046700010601
046800010601
046900010601
047000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
047100010601     C     repfil        BEGSR
047200010601     C*
047300010601     C                   if        invfil = *zeros and
047400010601     C                             depfil > *zeros and
047500010629     C                             (vinflg = *blanks or
047600010629     C                              vinflg = *zeros)
047700010601     C
047800010601     C                   eval      invfil = depfil
047900010601     C                   endif
048000010601     C*
048100010601     C                   if        depfil <> invfil and
048200010601     C                             invfil > *zeros
048300010601     C                   eval      flgMulti = '1'
048400010601     C                   if        vinflg = *blanks
048500010601     C                   add       1             cntNonEl
048600010601     C                   endif
048700010601     C                   endif
048800010601     C*
048900010601     C                   if        vinflg = '2'
049000010601     C                   eval      flgStato = '2'
049100010601     C                   endif
049200010601     C*
049300010601     C                   ENDSR
049400010601     C***
049500010601
049600010601
049700010601
049800010330
049900010330
050000010330
050100990920      /TITLE Invio dei dati al punto operativo.
050200000613     C     invio         BEGSR
050300990920     C*
050400990920     C                   reset                   dscmz
050500010601     C                   move      invfil        cmzdst
050600990920     C                   eval      cmzfld = vlrfou
050700990920     C                   eval      cmzmbd = vlrhdl
050800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
050900000710     C                   if        prmfir = *blanks
051000021113     C                   eval      cmzfla = 'FIVAB00F'
051100021113     C                   eval      cmzmba = 'FIVAB00F'
051200000710     C                   else
051300000710     C                   eval      cmzfla = prmfir
051400000710     C                   eval      cmzmba = prmfir
051500000710     C                   endif
051600990920     C                   eval      cmznrr = *zeros
051700990920     C                   move      §ctrok        cmznrr
051800021111     C                   eval      cmzlba = vlrfl1
051900990920     C                   call(e)   'TIS711C'
052000990920     C                   parm                    dscmz
052100990921     C                   parm      *blanks       esito
052200990920     C                   if        %error
052300990920     C                             or cmzerr = '1'
052400990921     C                             or esito  = '1'
052500000710     C                   eval      wrkesito = '3'
052600990920     C                   endif
052700990920     C*
052800000613     C                   ENDSR
052900990910
053000010601
053100010601
053200010601
053300010601
053400010601      /TITLE Invio dei dati al punto operativo.
053500010601     C     opeini        BEGSR
053600010601     C*
053700010601     C* Inizializzo flag e contatori operativi
053800010601     C                   movel     '0'           flgGiro           1
053900010601     C                   movel     '0'           flgMulti          1
054000010601     C                   movel     '1'           flgStato          1
054100010615     C                   movel     '0'           flgOk             1
054200010601     C                   z-add     *zeros        cntNonEl         10 0
054300010601     C                   z-add     *zeros        depfil            3 0
054400010601     C                   z-add     *zeros        invfil            3 0
054500010601     C*
054600010601     C                   ENDSR
054700010601     C***
054800010601
054900010601
055000010601
055100010330
055200010330
055300000613     C     *inzsr        BEGSR
055400990910     C*
055500990910     C     *entry        plist
055600990920     C                   parm                    tivlrds
055700990921     C                   parm      wrkesito      esito
055800000724     C                   parm                    prmlit
055900000710     C                   parm                    prmfir
056000010330     C*
056100010330     C* CALCOLA LA DATA CORRENTE
056200010330     C                   time                    wn14             14 0
056300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
056400010330     C                   z-add     wn8           g08dat
056500010330     C                   z-add     *zeros        g08inv
056600010330     C                   movel     '0'           g08err
056700010330     C                   call      'XSRDA8'
056800010330     C                   parm                    wlbda8
056900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
057000000613     C*
057100000613     C                   ENDSR
057200000613     C***
057300990908
057400010607     Otis7blp   E            riepilogo         2
057500990915     O                                              'Upload via Internet'
057600990915     O                                           +1 'Traduzione TIVIN00R -'
057700021113     O                                           +1 'FIVABWWR'
057800990915     O                                           +1 'Testate bolle'
057900990915     O          E            riepilogo   2
058000990915     O                       wrkdata
058100990915     O                       wrkora              +1
058200990915     O                       procname            +1
058300990915     O          E            riepilogo   2
058400990915     O                                              'Cliente..................:'
058500010605     O                       VABCCM        z     +1
058600990915     O          E            riepilogo   2
058700990920     O                                              'Riferimento Strategi.....:'
058800990920     O                       vlrhdl              +1
058900990915     O          E            riepilogo   2
059000990915     O                                              'Giusti...................:'
059100971022     O                       §CTROK        2   +  1
059200990915     O          E            riepilogo   2
059300990915     O                                              'Sbagliati e corretti.....:'
059400971022     O                       §CTRMO        2   +  1
059500990915     O          E            riepilogo   2
059600990915     O                                              'Sbagliati e scartati.....:'
059700971022     O                       §CTRNO        2   +  1
059800000613
059900010607     Otis7blps  E            testdett          2
060000000613     O                                              'Upload via Internet'
060100000613     O                                           +1 'Traduzione TIVIN00R -'
060200021113     O                                           +1 'FIVABWWR'
060300000613     O                                           +1 'Testate bolle'
060400000616     O          E            testdett    3
060500000613     O                                           +2 'N° rec'
060600000613     O                                           +3 'Anteprima contenuto'
060700000616     O          E            rigadett    2
060800000613     O                       rrnum               +2
060900000621     O                       recko               +3
061000000616     O          E            findett     2
061100000613     O                       wrkdata
061200000613     O                       wrkora              +1
061300000613     O                       procname            +1
061400000616     O          E            findett     2
061500000613     O                                              'Cliente..................:'
061600010605     O                       VABCCM        z     +1
061700000616     O          E            findett     2
061800000613     O                                              'Riferimento Strategi.....:'
061900000613     O                       vlrhdl              +1
062000000616     O          E            findett     2
062100000613     O                                              'Giusti...................:'
062200000613     O                       §CTROK        2   +  1
062300000616     O          E            findett     2
062400000613     O                                              'Sbagliati e corretti.....:'
062500000613     O                       §CTRMO        2   +  1
062600000616     O          E            findett     2
062700000613     O                                              'Sbagliati e scartati.....:'
062800000613     O                       §CTRNO        2   +  1
062900000613** CMD - COMANDI CL
063000011113OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7BLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
063100011113OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7BLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
063200010607DLTOVR FILE(TIS7BLP TIS7BLPS) LVL(*)
063300010606
063400000613
