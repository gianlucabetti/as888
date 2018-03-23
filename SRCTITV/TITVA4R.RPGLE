000100040309      /TITLE Upload via Internet: traduzione in FIVABWWR. (x COVEL Cod. 0450189)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600030130     FFIVABwwr  O    E             DISK    usropn
000700040309     Ftitva4p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900040309     Ftitva4ps  O    f  198        PRINTER usropn
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
007500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
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
009500040309     C                   if        not %open(titva4ps)
009600040309     C                   open      titva4ps
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
011800040309     C                   if        %open(titva4ps)
011900000616     C                   except    findett
012000040309     C                   close     titva4ps
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
014100040309     C                   if        not %open(titva4p)
014200040309     C                   open      titva4p
014300990915     C                   endif
014400990915     C*
014500990915     C                   except    riepilogo
014600990915     C*
014700040309     C                   if        %open(titva4p)
014800040309     C                   close     titva4p
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
015900030130     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
016000010330     C*--------------------------------------------------------
016100010605     C     RWVAB         BEGSR
016200010330     C*
016300010330     C                   if        not %open(tivin00r)
016400010330     C                   open      tivin00r
016500010330     C                   endif
016600030130     C                   if        not %open(fivabwwr)
016700030130     C                   open      fivabwwr
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
020400030130     C                   if        vlrpoi = 999
020500030130     C                   MOVE(P)   invfil        VABFGS
020600030130     C                   else
020700030130     C                   MOVE(P)   vlrpoi        VABFGS
020800030130     C                   endif
020900010601     C*
021000010601     C                   exsr      PREELA
021100010601     C*
021200010604     C* Ebbene...
021300010604     C*
021400010604     C  N31              ADD       1             §CTROK            7 0
021500010604     C   32              ADD       1             §CTRMO            7 0
021600010604     C   31              ADD       1             §CTRNO            7 0
021900030130     C  N31              WRITE     FIVAB000
022000020722     C*
022100010604     C                   if        *in31 = *off and
022200010604     C                             *in32 = *off
022300010604     C                   eval      vinflg = '1'
022400010604     C                   else
022500010604     C                   eval      recko = vindta
022600010604     C                   except    rigadett
022700010604     C                   eval      vinflg = '2'
022800010604     C                   endif
022900010604     C                   endif
023000010601     C*
023100010604     C                   endif
023200010604     C*
023300010330     C                   else
023400010330     C                   eval      vinflg = '1'
023500010330     C                   endif
023600010601     C*
023700010601     C  N70              update    tivin000
023800010330     C*
023900010330     C  N70              ENDdo
024000010601     C
024100010601     C*
024200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024300010601     C                   if        cntNonEl = *zeros or
024400010601     C                             flgMulti = '0'
024500010330     C* Se non ci sono record con errori ...
024600010601     C                   if        §ctrno = 0 and
024700010604     C                             §ctrmo = 0 and
024800010601     C                             flgStato <> '2'
024900010330     C* ... restituisco esito OK.
025000010330     C                   eval      wrkesito = '0'
025100010330     C                   else
025200010330     C                   if        §ctrok > 0
025300010330     C                   eval      wrkesito = '1'
025400010330     C                   else
025500010615     C                   if        flgOk = '0'
025600010615     C                   eval      wrkesito = '2'
025700010615     C                   else
025800010615     C                   eval      wrkesito = '6'
025900010615     C                   endif
026000010330     C                   endif
026100010330     C                   endif
026200010601     C                   else
026300010601     C                   eval      wrkesito = '9'
026400010601     C                   endif
026500010330     C*
026600010330     C                   if        %open(tivin00r)
026700010330     C                   close     tivin00r
026800010330     C                   endif
026900030130     C                   if        %open(fivabwwr)
027000030130     C                   close     fivabwwr
027100010330     C                   endif
027200010601     C*
027300010601     C                   if        vlrpoi <> 999
027400010601     C                   eval      invfil = vlrpoi
027500010601     C                   endif
027600010330     C*
027700010330     C                   if        §ctrok > 0
027800010601     C                             and invfil > *zeros
027900010330     C                   exsr      invio
028000010330     C                   endif
028100010601     C*
028200010618     C                   if        flgGiro = '1'
028300010601     C                   exsr      endela
028400010618     C                   endif
028500010330     C*
028600010330     C                   ENDSR
028700010330     C***
028800010330
028900010601
029000010601
029100010601
029200010601
029300010330     C*----------------------------------------------------*
029400020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029500010330     C*----------------------------------------------------*
029600010330     C     INZVAR        BEGSR
029700010330     C*
029800020204     C                   Z-ADD     *zeros        Num5_0            5 0
029900030327     C                   Z-ADD     *zeros        Num7_0            7 0
030000010330     C*
030100010330     C                   ENDSR
030200010330     C*----------------------------------------------------*
030300020722     C*  IMPOSTAZIONE CAMPI COSTANTI
030400010330     C*----------------------------------------------------*
030500010330     C     DEFCAM        BEGSR
030600010330     C*
030700020204     C* Inizializzo il buffer del record da scrivere
030800030130     C                   CLEAR                   FIVAB000
030900020204     C* Imposto i valori di default...
031000040309     C                   EVAL      VABCCM = 0450189
031100040309     C                   EVAL      VABLNP = 045
031200040309     C                   EVAL      VABCTR = 000
031300020204     C* ... e poi verifico se sono stati passati come parametri
031400040309     C                   IF        %subst(vlrppt:1:7) <> *blanks
031500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031600020204     C                   EXSR      CHKNUM
031700020204     C                   IF        PiInt=*on
031800020204     C                   Z-ADD     PiVal         VABCCM
031900020204     C                   ENDIF
031901040309     C                   ENDIF
031902040309     C                   IF        %subst(vlrppt:8:3) <> *blanks
032000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032100020204     C                   EXSR      CHKNUM
032200020204     C                   IF        PiInt=*on
032300020204     C                   Z-ADD     PiVal         VABLNP
032400020204     C                   ENDIF
032401040309     C                   ENDIF
032402040309     C                   IF        %subst(vlrppt:11:3) <> *blanks
032500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032600020204     C                   EXSR      CHKNUM
032700020204     C                   IF        PiInt=*on
032800020204     C                   Z-ADD     PiVal         VABCTR
032900020204     C                   ENDIF
032901040309     C                   ENDIF
033100020204     C*
033200010330     C                   ENDSR
033300010607     C*----------------------------------------------------*
033400030130     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033500010607     C*----------------------------------------------------*
033600010607     C     IMPVAB        BEGSR
033700010607     C*
033800010607     C                   SETOFF                                       3132
033900030327     C*
034000030327     C                   MOVEL     datcor        VABAAS
034100030327     C                   MOVE      datcor        VABMGS
034200030327     C*
034400040309     C                   EVAL      VABRSD=%trim(%subst(vindta:17:35))
034800030327     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034900030327     C     '@':'A'       XLATE     VABRSD        VABRSD
035000030327     C* ==
035200040309     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
035700040309     C                   EVAL      VABLOD=%trim(%subst(vindta:112:25))
036200040309     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
036800040309     C                   EVAL      VABRMA=%trim(%subst(vindta:258:6))
036801040309     C                   EVAL      VABNOT=%trim(%subst(vindta:196:35))
036802040309     C                   EVAL      VABNT2=%trim(%subst(vindta:196+35:51-35))
037200030327     C* CAP
037500040309     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
037900030327     C                   EXSR      CHKNUM
038000030327     C                   IF        PiInt=*on
038100030327     C                   Z-ADD     PiVal         Num5_0
038200030327     C                   MOVEL     Num5_0        VABCAD
038300030327     C                   ELSE
038400030327     C                   SETON                                        32
038500030327     C                   EVAL      vinmsg = %trimr(vinmsg)
038600030327     C                             + ' ' + 'VABCAD'
038700030327     C                   ENDIF
038800030327     C* NSP/RMN
039101040309     C                   EVAL      PiStr=%trim(%subst(vindta:258:6))
039500030327     C                   EXSR      CHKNUM
039600030327     C                   IF        PiInt=*on
039700030327     C                   Z-ADD     PiVal         VABRMN
039800030327     C                   Z-ADD     PiVal         Num7_0
039900030327     C                   MOVE(P)   Num7_0        VABNSP
040000030327     C                   ELSE
040100030327     C                   SETON                                        31
040200030327     C                   EVAL      vinmsg = %trimr(vinmsg)
040300030327     C                             + ' ' + 'VABNSP VABRMN'
040400030327     C                   ENDIF
040500030327     C* NCL
040600040309     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
040700030327     C                   EXSR      CHKNUM
040800030327     C                   IF        PiInt=*on
040900030327     C                   Z-ADD     PiVal         VABNCL
041000030327     C                   ELSE
041100030327     C                   SETON                                        32
041200030327     C                   EVAL      vinmsg = %trimr(vinmsg)
041300030327     C                             + ' ' + 'VABNCL'
041400030327     C                   ENDIF
041500030327     C* PKB
041600040309     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
041700030327     C                   EXSR      CHKNUM
041800030327     C                   IF        PiNum=*on
041901040309     C                   Z-ADD     PiVal         VABPKB
042000030327     C                   ELSE
042100030327     C                   SETON                                        32
042200030327     C                   EVAL      vinmsg = %trimr(vinmsg)
042300030327     C                             + ' ' + 'VABPKB'
042400030327     C                   ENDIF
042401040309     C* CAS
042402040315     C***                IF        %subst(vindta:307:1) = '1'
042403040316     C                   IF        %subst(vindta:280:12) <> *blanks
042404040309     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
042405040309     C                   EXSR      CHKNUM
042406040309     C                   IF        PiNum=*on
042407040309     C                   Z-ADD     PiVal         VABCAS
042408040309     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
042409040309     C                   ELSE
042410040309     C                   SETON                                        32
042411040309     C                   EVAL      vinmsg = %trimr(vinmsg)
042412040309     C                             + ' ' + 'VABCAS'
042413040309     C                   ENDIF
042414040316     C                   ENDIF
042415040315     C***                ENDIF
042500030327     C*
042600030327     C* Considerazioni sul contenuto di campi precedentemente valorizzati
042701040315     C***                IF        %subst(vindta:307:1) = '1'
042702040315     C                   IF        %subst(vindta:280:12) <> *blanks
042800030327     C                   EVAL      VABCBO = '4'
042900030327     C                   ELSE
043000030327     C                   EVAL      VABCBO = '1'
043100030327     C                   ENDIF
043200030327     C*
043300030327     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043400030327     C                   EXSR      CHKIMPDIV
043500020204     C*
043600010607     C                   ENDSR
043700010607     C*----------------------------------------------------*
043800010601
043900020204
044000020204     C*----------------------------------------------------*
044100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
044200020204     C*----------------------------------------------------*
044300020204     C     CHKIMPDIV     BEGSR
044400020204     C*
044500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
044600020204     C                   Z-ADD     *zeros        wrkDec            9 9
044700020204     C*
044800020204     C* Come prima cosa effettuo considerazioni sulla divisa
044900020204     C                   IF        vabIAS > *zeros
045000020204     C                   IF        vabVAS <> 'EUR'
045100020204     C                   EVAL      vabVAS =  'ITL'
045200020204     C                   ENDIF
045300020204     C                   ENDIF
045400020204     C*
045500020204     C                   IF        vabCAS > *zeros
045600020204     C                   IF        vabVCA <> 'EUR'
045700020204     C                   EVAL      vabVCA =  'ITL'
045800020204     C                   ENDIF
045900020204     C                   ENDIF
046000020204     C*
046100020204     C                   IF        vabVMD > *zeros
046200020321     C                   IF        vabVAD <> 'EUR'
046300020204     C                   EVAL      vabVAD =  'ITL'
046400020204     C                   ENDIF
046500020204     C                   ENDIF
046600020204     C*
046700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
046800020204     C                   Z-ADD     vabIAS        wrkDec
046900020204     C                   IF        wrkDec > *zeros
047000020204     C                   IF        vabVAS = 'ITL'
047100020204     C                   EVAL      vabIAS = *zeros
047200020204     C                   ENDIF
047300020204     C                   ENDIF
047400020204     C*
047500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
047600020204     C                   Z-ADD     vabCAS        wrkDec
047700020204     C                   IF        wrkDec > *zeros
047800020204     C                   IF        vabVCA = 'ITL'
047900020204     C                   EVAL      vabCAS = *zeros
048000020204     C                   ENDIF
048100020204     C                   ENDIF
048200020204     C*
048300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
048400020204     C                   Z-ADD     vabVMD        wrkDec
048500020204     C                   IF        wrkDec > *zeros
048600020204     C                   IF        vabVAD = 'ITL'
048700020204     C                   EVAL      vabVMD = *zeros
048800020204     C                   ENDIF
048900020204     C                   ENDIF
049000020204     C*
049100020204     C                   ENDSR
049200020204     C***
049300020204
049400010330
049500010330
049600010330     C*----------------------------------------------------*
049700010330     C*  CONTROLLO NUMERICITA' CAMPI
049800010330     C*----------------------------------------------------*
049900010330     C     CHKNUM        BEGSR
050000010330     C*
050100010606     C                   IF        PiDecChr = *blanks
050200040309     C                   EVAL      PiDecChr = '.'
050300010606     C                   ENDIF
050400010606     C*
050500010606     C                   CALL(e)   'ISNUMERIC'
050600010330     C                   PARM                    PiStr            30
050700010606     C                   PARM                    PiDecChr          1
050800010330     C                   PARM      *ZEROS        PiVal            30 9
050900010330     C                   PARM      '0'           PiInt             1
051000010330     C                   PARM      '0'           PiNum             1
051100010330     C                   IF        %error
051200030327     C                   EVAL      PiInt=*off
051300010606     C                   EVAL      PiNum=*off
051400010330     C                   ENDIF
051500010330     C*
051600010330     C                   ENDSR
051700010330     C***
051800010330
051900010601
052000010601
052100010601
052200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
052300010601     C     repfil        BEGSR
052400010601     C*
052500010601     C                   if        invfil = *zeros and
052600010601     C                             depfil > *zeros and
052700010629     C                             (vinflg = *blanks or
052800010629     C                              vinflg = *zeros)
052900010601     C
053000010601     C                   eval      invfil = depfil
053100010601     C                   endif
053200010601     C*
053300010601     C                   if        depfil <> invfil and
053400010601     C                             invfil > *zeros
053500010601     C                   eval      flgMulti = '1'
053600010601     C                   if        vinflg = *blanks
053700010601     C                   add       1             cntNonEl
053800010601     C                   endif
053900010601     C                   endif
054000010601     C*
054100010601     C                   if        vinflg = '2'
054200010601     C                   eval      flgStato = '2'
054300010601     C                   endif
054400010601     C*
054500010601     C                   ENDSR
054600010601     C***
054700010601
054800010601
054900010601
055000010330
055100010330
055200010330
055300990920      /TITLE Invio dei dati al punto operativo.
055400000613     C     invio         BEGSR
055500990920     C*
055600990920     C                   reset                   dscmz
055700010601     C                   move      invfil        cmzdst
055800990920     C                   eval      cmzfld = vlrfou
055900990920     C                   eval      cmzmbd = vlrhdl
056000990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
056100000710     C                   if        prmfir = *blanks
056200030130     C                   eval      cmzfla = 'FIVAB00F'
056300030130     C                   eval      cmzmba = 'FIVAB00F'
056400000710     C                   else
056500000710     C                   eval      cmzfla = prmfir
056600000710     C                   eval      cmzmba = prmfir
056700000710     C                   endif
056800990920     C                   eval      cmznrr = *zeros
056900990920     C                   move      §ctrok        cmznrr
057000021018     C                   eval      cmzlba = vlrfl1
057100990920     C                   call(e)   'TIS711C'
057200990920     C                   parm                    dscmz
057300990921     C                   parm      *blanks       esito
057400990920     C                   if        %error
057500990920     C                             or cmzerr = '1'
057600990921     C                             or esito  = '1'
057700000710     C                   eval      wrkesito = '3'
057800990920     C                   endif
057900990920     C*
058000000613     C                   ENDSR
058100990910
058200010601
058300010601
058400010601
058500010601
058600010601      /TITLE Invio dei dati al punto operativo.
058700010601     C     opeini        BEGSR
058800010601     C*
058900010601     C* Inizializzo flag e contatori operativi
059000010601     C                   movel     '0'           flgGiro           1
059100010601     C                   movel     '0'           flgMulti          1
059200010601     C                   movel     '1'           flgStato          1
059300010615     C                   movel     '0'           flgOk             1
059400010601     C                   z-add     *zeros        cntNonEl         10 0
059500010601     C                   z-add     *zeros        depfil            3 0
059600010601     C                   z-add     *zeros        invfil            3 0
059700010601     C*
059800010601     C                   ENDSR
059900010601     C***
060000010601
060100010601
060200010601
060300010330
060400010330
060500000613     C     *inzsr        BEGSR
060600990910     C*
060700990910     C     *entry        plist
060800990920     C                   parm                    tivlrds
060900990921     C                   parm      wrkesito      esito
061000000724     C                   parm                    prmlit
061100000710     C                   parm                    prmfir
061200010330     C*
061300010330     C* CALCOLA LA DATA CORRENTE
061400010330     C                   time                    wn14             14 0
061500010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
061600010330     C                   z-add     wn8           g08dat
061700010330     C                   z-add     *zeros        g08inv
061800010330     C                   movel     '0'           g08err
061900010330     C                   call      'XSRDA8'
062000010330     C                   parm                    wlbda8
062100010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
062200000613     C*
062300000613     C                   ENDSR
062400000613     C***
062500990908
062600040309     Otitva4p   E            riepilogo         2
062700990915     O                                              'Upload via Internet'
062800990915     O                                           +1 'Traduzione TIVIN00R -'
062900030130     O                                           +1 'FIVABWWR'
063000990915     O                                           +1 'Testate bolle'
063100990915     O          E            riepilogo   2
063200990915     O                       wrkdata
063300990915     O                       wrkora              +1
063400990915     O                       procname            +1
063500990915     O          E            riepilogo   2
063600990915     O                                              'Cliente..................:'
063700010605     O                       VABCCM        z     +1
063800990915     O          E            riepilogo   2
063900990920     O                                              'Riferimento Strategi.....:'
064000990920     O                       vlrhdl              +1
064100990915     O          E            riepilogo   2
064200990915     O                                              'Giusti...................:'
064300971022     O                       §CTROK        2   +  1
064400990915     O          E            riepilogo   2
064500990915     O                                              'Sbagliati e corretti.....:'
064600971022     O                       §CTRMO        2   +  1
064700990915     O          E            riepilogo   2
064800990915     O                                              'Sbagliati e scartati.....:'
064900971022     O                       §CTRNO        2   +  1
065000000613
065100040309     Otitva4ps  E            testdett          2
065200000613     O                                              'Upload via Internet'
065300000613     O                                           +1 'Traduzione TIVIN00R -'
065400030130     O                                           +1 'FIVABWWR'
065500000613     O                                           +1 'Testate bolle'
065600000616     O          E            testdett    3
065700000613     O                                           +2 'N° rec'
065800000613     O                                           +3 'Anteprima contenuto'
065900000616     O          E            rigadett    2
066000000613     O                       rrnum               +2
066100000621     O                       recko               +3
066200000616     O          E            findett     2
066300000613     O                       wrkdata
066400000613     O                       wrkora              +1
066500000613     O                       procname            +1
066600000616     O          E            findett     2
066700000613     O                                              'Cliente..................:'
066800010605     O                       VABCCM        z     +1
066900000616     O          E            findett     2
067000000613     O                                              'Riferimento Strategi.....:'
067100000613     O                       vlrhdl              +1
067200000616     O          E            findett     2
067300000613     O                                              'Giusti...................:'
067400000613     O                       §CTROK        2   +  1
067500000616     O          E            findett     2
067600000613     O                                              'Sbagliati e corretti.....:'
067700000613     O                       §CTRMO        2   +  1
067800000616     O          E            findett     2
067900000613     O                                              'Sbagliati e scartati.....:'
068000000613     O                       §CTRNO        2   +  1
068100000613** CMD - COMANDI CL
068200040309OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVA4P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
068300040309OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVA4PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
068400040309DLTOVR FILE(TITVA4P TITVA4PS) LVL(*)
068500010606
068600000613
