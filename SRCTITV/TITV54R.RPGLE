000100030508      /TITLE Upload via Internet: traduzione in FIVABWWR. (x AVANZI Cod. 0011085)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600030130     FFIVABwwr  O    E             DISK    usropn
000700030508     Ftitv54p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900030508     Ftitv54ps  O    f  198        PRINTER usropn
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
009500030508     C                   if        not %open(titv54ps)
009600030508     C                   open      titv54ps
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
011800030508     C                   if        %open(titv54ps)
011900000616     C                   except    findett
012000030508     C                   close     titv54ps
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
014100030508     C                   if        not %open(titv54p)
014200030508     C                   open      titv54p
014300990915     C                   endif
014400990915     C*
014500990915     C                   except    riepilogo
014600990915     C*
014700030508     C                   if        %open(titv54p)
014800030508     C                   close     titv54p
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
021700020205     C*
021800020205     C* Effettuo test particolare solo x DA ROLD x condizoinare la scrittura
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
029900030508     C                   Z-ADD     *zeros        Num7_0            7 0
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
031000030508     C                   EVAL      VABCCM = 0011085
031100030508     C                   EVAL      VABLNP = 001
031200020322     C                   EVAL      VABCTR = 000
031300020204     C* ... e poi verifico se sono stati passati come parametri
031400020204     C                   IF        vlrppt > *blanks
031500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031600020204     C                   EXSR      CHKNUM
031700020204     C                   IF        PiInt=*on
031800020204     C                   Z-ADD     PiVal         VABCCM
031900020204     C                   ENDIF
032000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032100020204     C                   EXSR      CHKNUM
032200020204     C                   IF        PiInt=*on
032300020204     C                   Z-ADD     PiVal         VABLNP
032400020204     C                   ENDIF
032500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032600020204     C                   EXSR      CHKNUM
032700020204     C                   IF        PiInt=*on
032800020204     C                   Z-ADD     PiVal         VABCTR
032900020204     C                   ENDIF
033000020204     C                   ENDIF
033100020204     C*
033200010330     C                   ENDSR
033300010607     C*----------------------------------------------------*
033400030130     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033500010607     C*----------------------------------------------------*
033600010607     C     IMPVAB        BEGSR
033700010607     C*
033800010607     C                   SETOFF                                       3132
033900010607     C*
034000010607     C* Reperimento campi ALFA
034100010607     C*
034200010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034300050413     C                   EVAL      VABRSD=%trim(%subst(vindta:17:39))
034400020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034500020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034600020117     C* ==
034700030530     C                   EVAL      VABIND=%trim(%subst(vindta:56:40))
034800030530     C                   EVAL      VABLOD=%trim(%subst(vindta:101:38))
034900030530     C                   EVAL      VABPRD=%trim(%subst(vindta:139:2))
035000030509     C                   EVAL      VABRMA=%trim(%subst(vindta:1:6))
035100050413     C                   EVAL      VABCBO=%trim(%subst(vindta:141:1))
035200010607     C*
035300010607     C* Reperimento campi NUMERICI
035400030130     C                   MOVEL     datcor        VABAAS
035500030130     C                   MOVE      datcor        VABMGS
035600030130     C* NSP/RMN
035700030509     C                   EVAL      PiStr=%trim(%subst(vindta:1:6))
035800030130     C                   EXSR      CHKNUM
035900030130     C                   IF        PiInt=*on
036000030508     C                   Z-ADD     PiVal         Num7_0
036100030508     C                   MOVE(P)   Num7_0        VABNSP
036200030130     C                   Z-ADD     PiVal         VABRMN
036300030130     C                   ELSE
036400030130     C                   SETON                                        31
036500030130     C                   Z-ADD     *zeros        VABNSP
036600030130     C                   Z-ADD     *zeros        VABRMN
036700030130     C                   EVAL      vinmsg = %trimr(vinmsg)
036800030130     C                             + ' ' + 'VABNSP VABRMN'
036900030130     C                   ENDIF
037000030130     C* CAD
037100030530     C                   EVAL      PiStr=%trim(%subst(vindta:96:5))
037200010607     C                   EXSR      CHKNUM
037300010607     C                   IF        PiInt=*on
037400020204     C                   Z-ADD     PiVal         Num5_0
037500020204     C                   MOVEL(p)  Num5_0        VABCAD
037600010607     C                   ELSE
037700010607     C                   SETON                                        32
037800020204     C                   EVAL      VABCAD = *zeros
037900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
038000020204     C                             + ' ' + 'VABCAD'
038100010607     C                   ENDIF
038200030130     C* NCL
038300050413     C                   EVAL      PiStr=%trim(%subst(vindta:142:2))
038400010607     C                   EXSR      CHKNUM
038500010607     C                   IF        PiInt=*on
038600010607     C                   Z-ADD     PiVal         VABNCL
038700010607     C                   ELSE
038800010607     C                   SETON                                        32
038900010607     C                   Z-ADD     *zeros        VABNCL
039000010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039100010607     C                             + ' ' + 'VABNCL'
039200010607     C                   ENDIF
039300030130     C* PKB
039400050413     C                   EVAL      PiStr=%trim(%subst(vindta:144:5))
039500030130     C                   EXSR      CHKNUM
039600030210     C                   IF        PiNum=*on
039700030130     C                   Z-ADD     PiVal         VABPKB
039800030130     C                   ELSE
039900030130     C                   SETON                                        32
040000030130     C                   Z-ADD     *zeros        VABPKB
040100030130     C                   EVAL      vinmsg = %trimr(vinmsg)
040200030130     C                             + ' ' + 'VABPKB'
040300030130     C                   ENDIF
040400020204     C*
040500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
040600020204     C                   EXSR      CHKIMPDIV
040700020204     C*
040800010607     C                   ENDSR
040900010607     C*----------------------------------------------------*
041000010601
041100020204
041200020204     C*----------------------------------------------------*
041300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
041400020204     C*----------------------------------------------------*
041500020204     C     CHKIMPDIV     BEGSR
041600020204     C*
041700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
041800020204     C                   Z-ADD     *zeros        wrkDec            9 9
041900020204     C*
042000020204     C* Come prima cosa effettuo considerazioni sulla divisa
042100020204     C                   IF        vabIAS > *zeros
042200020204     C                   IF        vabVAS <> 'EUR'
042300020204     C                   EVAL      vabVAS =  'ITL'
042400020204     C                   ENDIF
042500020204     C                   ENDIF
042600020204     C*
042700020204     C                   IF        vabCAS > *zeros
042800020204     C                   IF        vabVCA <> 'EUR'
042900020204     C                   EVAL      vabVCA =  'ITL'
043000020204     C                   ENDIF
043100020204     C                   ENDIF
043200020204     C*
043300020204     C                   IF        vabVMD > *zeros
043400020321     C                   IF        vabVAD <> 'EUR'
043500020204     C                   EVAL      vabVAD =  'ITL'
043600020204     C                   ENDIF
043700020204     C                   ENDIF
043800020204     C*
043900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
044000020204     C                   Z-ADD     vabIAS        wrkDec
044100020204     C                   IF        wrkDec > *zeros
044200020204     C                   IF        vabVAS = 'ITL'
044300020204     C                   EVAL      vabIAS = *zeros
044400020204     C                   ENDIF
044500020204     C                   ENDIF
044600020204     C*
044700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
044800020204     C                   Z-ADD     vabCAS        wrkDec
044900020204     C                   IF        wrkDec > *zeros
045000020204     C                   IF        vabVCA = 'ITL'
045100020204     C                   EVAL      vabCAS = *zeros
045200020204     C                   ENDIF
045300020204     C                   ENDIF
045400020204     C*
045500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
045600020204     C                   Z-ADD     vabVMD        wrkDec
045700020204     C                   IF        wrkDec > *zeros
045800020204     C                   IF        vabVAD = 'ITL'
045900020204     C                   EVAL      vabVMD = *zeros
046000020204     C                   ENDIF
046100020204     C                   ENDIF
046200020204     C*
046300020204     C                   ENDSR
046400020204     C***
046500020204
046600010330
046700010330
046800010330     C*----------------------------------------------------*
046900010330     C*  CONTROLLO NUMERICITA' CAMPI
047000010330     C*----------------------------------------------------*
047100010330     C     CHKNUM        BEGSR
047200010330     C*
047300010606     C                   IF        PiDecChr = *blanks
047400030509     C                   EVAL      PiDecChr = ','
047500010606     C                   ENDIF
047600010606     C*
047700010606     C                   CALL(e)   'ISNUMERIC'
047800010330     C                   PARM                    PiStr            30
047900010606     C                   PARM                    PiDecChr          1
048000010330     C                   PARM      *ZEROS        PiVal            30 9
048100010330     C                   PARM      '0'           PiInt             1
048200010330     C                   PARM      '0'           PiNum             1
048300010330     C                   IF        %error
048400010606     C                   EVAL      PiNum=*off
048500010330     C                   ENDIF
048600010330     C*
048700010330     C                   ENDSR
048800010330     C***
048900010330
049000010601
049100010601
049200010601
049300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
049400010601     C     repfil        BEGSR
049500010601     C*
049600010601     C                   if        invfil = *zeros and
049700010601     C                             depfil > *zeros and
049800010629     C                             (vinflg = *blanks or
049900010629     C                              vinflg = *zeros)
050000010601     C
050100010601     C                   eval      invfil = depfil
050200010601     C                   endif
050300010601     C*
050400010601     C                   if        depfil <> invfil and
050500010601     C                             invfil > *zeros
050600010601     C                   eval      flgMulti = '1'
050700010601     C                   if        vinflg = *blanks
050800010601     C                   add       1             cntNonEl
050900010601     C                   endif
051000010601     C                   endif
051100010601     C*
051200010601     C                   if        vinflg = '2'
051300010601     C                   eval      flgStato = '2'
051400010601     C                   endif
051500010601     C*
051600010601     C                   ENDSR
051700010601     C***
051800010601
051900010601
052000010601
052100010330
052200010330
052300010330
052400990920      /TITLE Invio dei dati al punto operativo.
052500000613     C     invio         BEGSR
052600990920     C*
052700990920     C                   reset                   dscmz
052800010601     C                   move      invfil        cmzdst
052900990920     C                   eval      cmzfld = vlrfou
053000990920     C                   eval      cmzmbd = vlrhdl
053100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
053200000710     C                   if        prmfir = *blanks
053300030130     C                   eval      cmzfla = 'FIVAB00F'
053400030130     C                   eval      cmzmba = 'FIVAB00F'
053500000710     C                   else
053600000710     C                   eval      cmzfla = prmfir
053700000710     C                   eval      cmzmba = prmfir
053800000710     C                   endif
053900990920     C                   eval      cmznrr = *zeros
054000990920     C                   move      §ctrok        cmznrr
054100021018     C                   eval      cmzlba = vlrfl1
054200990920     C                   call(e)   'TIS711C'
054300990920     C                   parm                    dscmz
054400990921     C                   parm      *blanks       esito
054500990920     C                   if        %error
054600990920     C                             or cmzerr = '1'
054700990921     C                             or esito  = '1'
054800000710     C                   eval      wrkesito = '3'
054900990920     C                   endif
055000990920     C*
055100000613     C                   ENDSR
055200990910
055300010601
055400010601
055500010601
055600010601
055700010601      /TITLE Invio dei dati al punto operativo.
055800010601     C     opeini        BEGSR
055900010601     C*
056000010601     C* Inizializzo flag e contatori operativi
056100010601     C                   movel     '0'           flgGiro           1
056200010601     C                   movel     '0'           flgMulti          1
056300010601     C                   movel     '1'           flgStato          1
056400010615     C                   movel     '0'           flgOk             1
056500010601     C                   z-add     *zeros        cntNonEl         10 0
056600010601     C                   z-add     *zeros        depfil            3 0
056700010601     C                   z-add     *zeros        invfil            3 0
056800010601     C*
056900010601     C                   ENDSR
057000010601     C***
057100010601
057200010601
057300010601
057400010330
057500010330
057600000613     C     *inzsr        BEGSR
057700990910     C*
057800990910     C     *entry        plist
057900990920     C                   parm                    tivlrds
058000990921     C                   parm      wrkesito      esito
058100000724     C                   parm                    prmlit
058200000710     C                   parm                    prmfir
058300010330     C*
058400010330     C* CALCOLA LA DATA CORRENTE
058500010330     C                   time                    wn14             14 0
058600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
058700010330     C                   z-add     wn8           g08dat
058800010330     C                   z-add     *zeros        g08inv
058900010330     C                   movel     '0'           g08err
059000010330     C                   call      'XSRDA8'
059100010330     C                   parm                    wlbda8
059200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
059300000613     C*
059400000613     C                   ENDSR
059500000613     C***
059600990908
059700030508     Otitv54p   E            riepilogo         2
059800990915     O                                              'Upload via Internet'
059900990915     O                                           +1 'Traduzione TIVIN00R -'
060000030130     O                                           +1 'FIVABWWR'
060100990915     O                                           +1 'Testate bolle'
060200990915     O          E            riepilogo   2
060300990915     O                       wrkdata
060400990915     O                       wrkora              +1
060500990915     O                       procname            +1
060600990915     O          E            riepilogo   2
060700990915     O                                              'Cliente..................:'
060800010605     O                       VABCCM        z     +1
060900990915     O          E            riepilogo   2
061000990920     O                                              'Riferimento Strategi.....:'
061100990920     O                       vlrhdl              +1
061200990915     O          E            riepilogo   2
061300990915     O                                              'Giusti...................:'
061400971022     O                       §CTROK        2   +  1
061500990915     O          E            riepilogo   2
061600990915     O                                              'Sbagliati e corretti.....:'
061700971022     O                       §CTRMO        2   +  1
061800990915     O          E            riepilogo   2
061900990915     O                                              'Sbagliati e scartati.....:'
062000971022     O                       §CTRNO        2   +  1
062100000613
062200030508     Otitv54ps  E            testdett          2
062300000613     O                                              'Upload via Internet'
062400000613     O                                           +1 'Traduzione TIVIN00R -'
062500030130     O                                           +1 'FIVABWWR'
062600000613     O                                           +1 'Testate bolle'
062700000616     O          E            testdett    3
062800000613     O                                           +2 'N° rec'
062900000613     O                                           +3 'Anteprima contenuto'
063000000616     O          E            rigadett    2
063100000613     O                       rrnum               +2
063200000621     O                       recko               +3
063300000616     O          E            findett     2
063400000613     O                       wrkdata
063500000613     O                       wrkora              +1
063600000613     O                       procname            +1
063700000616     O          E            findett     2
063800000613     O                                              'Cliente..................:'
063900010605     O                       VABCCM        z     +1
064000000616     O          E            findett     2
064100000613     O                                              'Riferimento Strategi.....:'
064200000613     O                       vlrhdl              +1
064300000616     O          E            findett     2
064400000613     O                                              'Giusti...................:'
064500000613     O                       §CTROK        2   +  1
064600000616     O          E            findett     2
064700000613     O                                              'Sbagliati e corretti.....:'
064800000613     O                       §CTRMO        2   +  1
064900000616     O          E            findett     2
065000000613     O                                              'Sbagliati e scartati.....:'
065100000613     O                       §CTRNO        2   +  1
065200000613** CMD - COMANDI CL
065300030508OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV54P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
065400030508OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV54PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
065500030508DLTOVR FILE(TITV54P TITV54PS) LVL(*)
065600010606
065700000613
