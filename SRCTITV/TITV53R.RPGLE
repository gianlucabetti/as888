000100030507      /TITLE Upload via Internet: traduzione in FIVABWWR. (x BRUGI Cod. 2770668)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600030130     FFIVABwwr  O    E             DISK    usropn
000700030507     Ftitv53p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900030507     Ftitv53ps  O    f  198        PRINTER usropn
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
003200030507     D conta           s              1  0 inz(*zeros)
003300010330     D*------------------
003400010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003500010330     D*------------------
003600010330     D WLBDA8          DS                  INZ
003700010330     D  G08DAT                 1      8  0
003800010330     D  G08INV                 9     16  0
003900010330     D  G08ERR                17     17
004000010330     D  G08TGI                18     22  0
004100000313     D*
004200010330
004300010330
004400010330
004500990915     C                   time                    wrkdata
004600990915     C                   time                    wrkora
004700000913     C                   reset                   rrnum
004800990921     C                   reset                   esito
004900990921     C                   reset                   wrkesito
005000010601     C*
005100010601     C                   exsr      opeini
005200010605     C                   exsr      rwvab
005300010601     C*
005400010601     C                   seton                                        lr
005500010601
005600010601
005700010601
005800010601
005900010601     C*--------------------------------------------------------
006000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006100010601     C*--------------------------------------------------------
006200010601     C     PREELA        BEGSR
006300010601     C*
006400010601     C* SE OCCORRE SPEDIRE IN FILIALE
006500010601     C                   if        invfil <> *zeros and
006600010601     C                             flgGiro = '0'
006700010601     C*
006800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006900010601     C                   eval      flgGiro = '1'
007000000724     C*
007100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007300010601     C     invfil        chain     azorg01l
007400000724     C                   if        %found
007500000724     C                   movel(p)  CMD(1)        depcmd
007600020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007700000724     C*
007800000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007900011113     C                   Z-ADD     150           LENGH            15 5
008000000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008100000724     C                   PARM                    depcmd
008200000724     C                   PARM                    LENGH
008300000724     C*
008400000724     C                   endif
008500000616     C*
008600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008700000616     C                   movel(p)  CMD(2)        depcmd
008800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008900000616     C*
009000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009100011113     C                   Z-ADD     150           LENGH            15 5
009200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009300000616     C                   PARM                    depcmd
009400000616     C                   PARM                    LENGH
009500000613     C*
009600030507     C                   if        not %open(titv53ps)
009700030507     C                   open      titv53ps
009800000616     C                   except    testdett
009900000613     C                   endif
010000010601     C*
010100010601     C                   endif
010200010601     C*
010300010601     C                   ENDSR
010400010601     C***
010500010601
010600010601
010700010601
010800010601
010900010601
011000010601
011100010601
011200010601     C*--------------------------------------------------------
011300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011400010601     C*--------------------------------------------------------
011500010601     C     ENDELA        BEGSR
011600990915     C*
011700000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011800000613     C*
011900030507     C                   if        %open(titv53ps)
012000000616     C                   except    findett
012100030507     C                   close     titv53ps
012200000613     C                   endif
012300000616     C*
012400000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012500011113     C                   Z-ADD     150           LENGH            15 5
012600010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012700000616     C                   PARM                    CMD(3)
012800000616     C                   PARM                    LENGH
012900000616     C*
013000010601     C                   ENDSR
013100010601     C***
013200010601
013300010601
013400010601
013500010601
013600000613
013700000613     C*--------------------------------------------------------
013800000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013900000613     C*--------------------------------------------------------
014000000621     C     STPR          BEGSR
014100000613     C*
014200030507     C                   if        not %open(titv53p)
014300030507     C                   open      titv53p
014400990915     C                   endif
014500990915     C*
014600990915     C                   except    riepilogo
014700990915     C*
014800030507     C                   if        %open(titv53p)
014900030507     C                   close     titv53p
015000990914     C                   endif
015100000613     C*
015200000613     C                   ENDSR
015300000613     C***
015400000613
015500010330
015600010601
015700010601
015800010601
015900010330     C*--------------------------------------------------------
016000030130     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
016100010330     C*--------------------------------------------------------
016200010605     C     RWVAB         BEGSR
016300010330     C*
016400010330     C                   if        not %open(tivin00r)
016500010330     C                   open      tivin00r
016600010330     C                   endif
016700030130     C                   if        not %open(fivabwwr)
016800030130     C                   open      fivabwwr
016900010330     C                   endif
017000010330     C*
017100010604     C                   clear                   §CTROK
017200010604     C                   clear                   §CTRMO
017300010604     C                   clear                   §CTRNO
017400010330     C*
017500010330     C                   DO        *HIVAL
017600010330     C*
017700010330     C                   READ      tivin00r                               70
017800010618     C*
017900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018000010618     C                   if        vinflg = '1'
018100010618     C                   eval      flgOk = '1'
018200010618     C                   endif
018300010618     C*
018400010330     C                   if        vindta > *blanks
018500010330     C                   add       1             rrnum
018600010330     C*
018700010601     C                   if        *in70 = *off and
018800010330     C                             (vinflg = *blanks
018900010330     C                              or vinflg = '0'
019000010330     C                              or vinflg = '2')
019100010330     C*
019200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019300010711     C                   if        vinflg = *blanks or vinflg = '0'
019400010711     C                   clear                   vinmsg
019500010711     C                   endif
019600010601     C*
019700030507     C* Verifico il contatore x riconosciere il binomio: tipo record "DES" + tipo record "BOL"
019800030507     C                   if        conta = 0
019900030507     C                   exsr      inzvar
020000030507     C                   exsr      defcam
020100030507     C                   endif
020200030507     C                   exsr      impvab
020300010601     C*
020400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020500010605     C                   eval      depfil = VABLNP
020600010601     C                   exsr      repfil
020700010601     C                   if        depfil = invfil
020800030130     C                   if        vlrpoi = 999
020900030130     C                   MOVE(P)   invfil        VABFGS
021000030130     C                   else
021100030130     C                   MOVE(P)   vlrpoi        VABFGS
021200030130     C                   endif
021300010601     C*
021400010601     C                   exsr      PREELA
021500010601     C*
021600010604     C* Ebbene...
021700010604     C*
021800010604     C  N31              ADD       1             §CTROK            7 0
021900010604     C   32              ADD       1             §CTRMO            7 0
022000010604     C   31              ADD       1             §CTRNO            7 0
022100020205     C*
022200020205     C* Effettuo test particolare solo x DA ROLD x condizoinare la scrittura
022300030507     C                   if        conta = 2
022400030130     C  N31              WRITE     FIVAB000
022500030507     C                   eval      conta = 0
022600030507     C                   endif
022700020722     C*
022800010604     C                   if        *in31 = *off and
022900010604     C                             *in32 = *off
023000010604     C                   eval      vinflg = '1'
023100010604     C                   else
023200010604     C                   eval      recko = vindta
023300010604     C                   except    rigadett
023400010604     C                   eval      vinflg = '2'
023500010604     C                   endif
023600010604     C                   endif
023700010601     C*
023800010604     C                   endif
023900010604     C*
024000010330     C                   else
024100010330     C                   eval      vinflg = '1'
024200010330     C                   endif
024300010601     C*
024400010601     C  N70              update    tivin000
024500010330     C*
024600010330     C  N70              ENDdo
024700010601     C
024800010601     C*
024900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025000010601     C                   if        cntNonEl = *zeros or
025100010601     C                             flgMulti = '0'
025200010330     C* Se non ci sono record con errori ...
025300010601     C                   if        §ctrno = 0 and
025400010604     C                             §ctrmo = 0 and
025500010601     C                             flgStato <> '2'
025600010330     C* ... restituisco esito OK.
025700010330     C                   eval      wrkesito = '0'
025800010330     C                   else
025900010330     C                   if        §ctrok > 0
026000010330     C                   eval      wrkesito = '1'
026100010330     C                   else
026200010615     C                   if        flgOk = '0'
026300010615     C                   eval      wrkesito = '2'
026400010615     C                   else
026500010615     C                   eval      wrkesito = '6'
026600010615     C                   endif
026700010330     C                   endif
026800010330     C                   endif
026900010601     C                   else
027000010601     C                   eval      wrkesito = '9'
027100010601     C                   endif
027200010330     C*
027300010330     C                   if        %open(tivin00r)
027400010330     C                   close     tivin00r
027500010330     C                   endif
027600030130     C                   if        %open(fivabwwr)
027700030130     C                   close     fivabwwr
027800010330     C                   endif
027900010601     C*
028000010601     C                   if        vlrpoi <> 999
028100010601     C                   eval      invfil = vlrpoi
028200010601     C                   endif
028300010330     C*
028400010330     C                   if        §ctrok > 0
028500010601     C                             and invfil > *zeros
028600010330     C                   exsr      invio
028700010330     C                   endif
028800010601     C*
028900010618     C                   if        flgGiro = '1'
029000010601     C                   exsr      endela
029100010618     C                   endif
029200010330     C*
029300010330     C                   ENDSR
029400010330     C***
029500010330
029600010601
029700010601
029800010601
029900010601
030000010330     C*----------------------------------------------------*
030100020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030200010330     C*----------------------------------------------------*
030300010330     C     INZVAR        BEGSR
030400010330     C*
030500020204     C                   Z-ADD     *zeros        Num5_0            5 0
030600030327     C                   Z-ADD     *zeros        Num7_0            7 0
030700010330     C*
030800010330     C                   ENDSR
030900010330     C*----------------------------------------------------*
031000020722     C*  IMPOSTAZIONE CAMPI COSTANTI
031100010330     C*----------------------------------------------------*
031200010330     C     DEFCAM        BEGSR
031300010330     C*
031400020204     C* Inizializzo il buffer del record da scrivere
031500030130     C                   CLEAR                   FIVAB000
031600020204     C* Imposto i valori di default...
031700030507     C                   EVAL      VABCCM = 2770668
031800030507     C                   EVAL      VABLNP = 277
031900020322     C                   EVAL      VABCTR = 000
032000020204     C* ... e poi verifico se sono stati passati come parametri
032100020204     C                   IF        vlrppt > *blanks
032200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032300020204     C                   EXSR      CHKNUM
032400020204     C                   IF        PiInt=*on
032500020204     C                   Z-ADD     PiVal         VABCCM
032600020204     C                   ENDIF
032700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032800020204     C                   EXSR      CHKNUM
032900020204     C                   IF        PiInt=*on
033000020204     C                   Z-ADD     PiVal         VABLNP
033100020204     C                   ENDIF
033200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033300020204     C                   EXSR      CHKNUM
033400020204     C                   IF        PiInt=*on
033500020204     C                   Z-ADD     PiVal         VABCTR
033600020204     C                   ENDIF
033700020204     C                   ENDIF
033800020204     C*
033900010330     C                   ENDSR
034000010607     C*----------------------------------------------------*
034100030130     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034200010607     C*----------------------------------------------------*
034300010607     C     IMPVAB        BEGSR
034400010607     C*
034500010607     C                   SETOFF                                       3132
034600030507     C*
034700030507     C* Verifico il tipo record corrente
034800030507     C                   if        %subst(vindta:1:3) = 'DES'
034900030507     C                   add       1             conta
035000030327     C*
035100030327     C                   MOVEL     datcor        VABAAS
035200030327     C                   MOVE      datcor        VABMGS
035300030327     C*
035400030507     C                   EVAL      VABRSD=%trim(%subst(vindta:14:40))
035500030327     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035600030327     C     '@':'A'       XLATE     VABRSD        VABRSD
035700030327     C* ==
035800030507     C                   EVAL      VABIND=%trim(%subst(vindta:54:40))
035900030507     C                   EVAL      VABLOD=%trim(%subst(vindta:94:40))
036000030507     C                   EVAL      VABPRD=%trim(%subst(vindta:134:2))
036100030507     C                   EVAL      VABNOT='Tel.'+%trim(%subst(vindta:141:20))
036200030507     C                   EVAL      VABNT2='Fax.'+%trim(%subst(vindta:161:20))
036300030327     C* CAP
036400030507     C                   EVAL      PiStr=%trim(%subst(vindta:136:5))
036500030327     C                   EXSR      CHKNUM
036600030327     C                   IF        PiInt=*on
036700030327     C                   Z-ADD     PiVal         Num5_0
036800030327     C                   MOVEL     Num5_0        VABCAD
036900030327     C                   ELSE
037000030327     C                   SETON                                        32
037100030327     C                   EVAL      vinmsg = %trimr(vinmsg)
037200030327     C                             + ' ' + 'VABCAD'
037300030327     C                   ENDIF
037400030507     C*
037500030507     C                   endif
037600030507     C***
037700030507     C                   if        %subst(vindta:1:3) = 'BOL'
037800030507     C                   add       1             conta
037900030507     C*
038000030507     C                   EVAL      VABNAS=%trim(%subst(vindta:105:30))
038100030507     C                   EVAL      VABRMA=%trim(%subst(vindta:12:6))
038200030508     C                   EVAL      VABVCA='EUR'
038300030327     C* NSP/RMN
038400030507     C                   EVAL      PiStr=%trim(%subst(vindta:12:6))
038500030327     C                   EXSR      CHKNUM
038600030327     C                   IF        PiInt=*on
038700030327     C                   Z-ADD     PiVal         VABRMN
038800030327     C                   Z-ADD     PiVal         Num7_0
038900030327     C                   MOVE(P)   Num7_0        VABNSP
039000030327     C                   ELSE
039100030327     C                   SETON                                        31
039200030327     C                   EVAL      vinmsg = %trimr(vinmsg)
039300030327     C                             + ' ' + 'VABNSP VABRMN'
039400030327     C                   ENDIF
039500030327     C* NCL
039600030507     C                   EVAL      PiStr=%trim(%subst(vindta:80:5))
039700030327     C                   EXSR      CHKNUM
039800030327     C                   IF        PiInt=*on
039900030327     C                   Z-ADD     PiVal         VABNCL
040000030327     C                   ELSE
040100030327     C                   SETON                                        32
040200030327     C                   EVAL      vinmsg = %trimr(vinmsg)
040300030327     C                             + ' ' + 'VABNCL'
040400030327     C                   ENDIF
040500030327     C* PKB
040600030507     C                   EVAL      PiStr=%trim(%subst(vindta:85:5))
040700030327     C                   EXSR      CHKNUM
040800030327     C                   IF        PiNum=*on
040900030327     C                   Z-ADD(H)  PiVal         VABPKB
041000030327     C                   ELSE
041100030327     C                   SETON                                        32
041200030327     C                   EVAL      vinmsg = %trimr(vinmsg)
041300030327     C                             + ' ' + 'VABPKB'
041400030327     C                   ENDIF
041500030508     C* CAS
041600030508     C                   EVAL      PiStr=%trim(%subst(vindta:199:9))
041700030508     C                   EXSR      CHKNUM
041800030508     C                   IF        PiNum=*on
041900030508     C                   Z-ADD     PiVal         VABCAS
042000030508     C                   ELSE
042100030508     C                   SETON                                        32
042200030508     C                   EVAL      vinmsg = %trimr(vinmsg)
042300030508     C                             + ' ' + 'VABCAS'
042400030508     C                   ENDIF
042500030507     C*
042600030507     C                   endif
042700030507     C***
042800030327     C*
042900030327     C* Considerazioni sul contenuto di campi precedentemente valorizzati
043000030327     C                   IF        VABCAS > *zeros
043100030327     C                   EVAL      VABCBO = '4'
043200030327     C                   ELSE
043300030327     C                   EVAL      VABCBO = '1'
043400030327     C                   ENDIF
043500030327     C*
043600030327     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043700030327     C                   EXSR      CHKIMPDIV
043800020204     C*
043900010607     C                   ENDSR
044000010607     C*----------------------------------------------------*
044100010601
044200020204
044300020204     C*----------------------------------------------------*
044400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
044500020204     C*----------------------------------------------------*
044600020204     C     CHKIMPDIV     BEGSR
044700020204     C*
044800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
044900020204     C                   Z-ADD     *zeros        wrkDec            9 9
045000020204     C*
045100020204     C* Come prima cosa effettuo considerazioni sulla divisa
045200020204     C                   IF        vabIAS > *zeros
045300020204     C                   IF        vabVAS <> 'EUR'
045400020204     C                   EVAL      vabVAS =  'ITL'
045500020204     C                   ENDIF
045600020204     C                   ENDIF
045700020204     C*
045800020204     C                   IF        vabCAS > *zeros
045900020204     C                   IF        vabVCA <> 'EUR'
046000020204     C                   EVAL      vabVCA =  'ITL'
046100020204     C                   ENDIF
046200020204     C                   ENDIF
046300020204     C*
046400020204     C                   IF        vabVMD > *zeros
046500020321     C                   IF        vabVAD <> 'EUR'
046600020204     C                   EVAL      vabVAD =  'ITL'
046700020204     C                   ENDIF
046800020204     C                   ENDIF
046900020204     C*
047000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
047100020204     C                   Z-ADD     vabIAS        wrkDec
047200020204     C                   IF        wrkDec > *zeros
047300020204     C                   IF        vabVAS = 'ITL'
047400020204     C                   EVAL      vabIAS = *zeros
047500020204     C                   ENDIF
047600020204     C                   ENDIF
047700020204     C*
047800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
047900020204     C                   Z-ADD     vabCAS        wrkDec
048000020204     C                   IF        wrkDec > *zeros
048100020204     C                   IF        vabVCA = 'ITL'
048200020204     C                   EVAL      vabCAS = *zeros
048300020204     C                   ENDIF
048400020204     C                   ENDIF
048500020204     C*
048600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
048700020204     C                   Z-ADD     vabVMD        wrkDec
048800020204     C                   IF        wrkDec > *zeros
048900020204     C                   IF        vabVAD = 'ITL'
049000020204     C                   EVAL      vabVMD = *zeros
049100020204     C                   ENDIF
049200020204     C                   ENDIF
049300020204     C*
049400020204     C                   ENDSR
049500020204     C***
049600020204
049700010330
049800010330
049900010330     C*----------------------------------------------------*
050000010330     C*  CONTROLLO NUMERICITA' CAMPI
050100010330     C*----------------------------------------------------*
050200010330     C     CHKNUM        BEGSR
050300010330     C*
050400010606     C                   IF        PiDecChr = *blanks
050500030327     C                   EVAL      PiDecChr = ','
050600010606     C                   ENDIF
050700010606     C*
050800010606     C                   CALL(e)   'ISNUMERIC'
050900010330     C                   PARM                    PiStr            30
051000010606     C                   PARM                    PiDecChr          1
051100010330     C                   PARM      *ZEROS        PiVal            30 9
051200010330     C                   PARM      '0'           PiInt             1
051300010330     C                   PARM      '0'           PiNum             1
051400010330     C                   IF        %error
051500030327     C                   EVAL      PiInt=*off
051600010606     C                   EVAL      PiNum=*off
051700010330     C                   ENDIF
051800010330     C*
051900010330     C                   ENDSR
052000010330     C***
052100010330
052200010601
052300010601
052400010601
052500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
052600010601     C     repfil        BEGSR
052700010601     C*
052800010601     C                   if        invfil = *zeros and
052900010601     C                             depfil > *zeros and
053000010629     C                             (vinflg = *blanks or
053100010629     C                              vinflg = *zeros)
053200010601     C
053300010601     C                   eval      invfil = depfil
053400010601     C                   endif
053500010601     C*
053600010601     C                   if        depfil <> invfil and
053700010601     C                             invfil > *zeros
053800010601     C                   eval      flgMulti = '1'
053900010601     C                   if        vinflg = *blanks
054000010601     C                   add       1             cntNonEl
054100010601     C                   endif
054200010601     C                   endif
054300010601     C*
054400010601     C                   if        vinflg = '2'
054500010601     C                   eval      flgStato = '2'
054600010601     C                   endif
054700010601     C*
054800010601     C                   ENDSR
054900010601     C***
055000010601
055100010601
055200010601
055300010330
055400010330
055500010330
055600990920      /TITLE Invio dei dati al punto operativo.
055700000613     C     invio         BEGSR
055800990920     C*
055900990920     C                   reset                   dscmz
056000010601     C                   move      invfil        cmzdst
056100990920     C                   eval      cmzfld = vlrfou
056200990920     C                   eval      cmzmbd = vlrhdl
056300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
056400000710     C                   if        prmfir = *blanks
056500030130     C                   eval      cmzfla = 'FIVAB00F'
056600030130     C                   eval      cmzmba = 'FIVAB00F'
056700000710     C                   else
056800000710     C                   eval      cmzfla = prmfir
056900000710     C                   eval      cmzmba = prmfir
057000000710     C                   endif
057100990920     C                   eval      cmznrr = *zeros
057200990920     C                   move      §ctrok        cmznrr
057300021018     C                   eval      cmzlba = vlrfl1
057400990920     C                   call(e)   'TIS711C'
057500990920     C                   parm                    dscmz
057600990921     C                   parm      *blanks       esito
057700990920     C                   if        %error
057800990920     C                             or cmzerr = '1'
057900990921     C                             or esito  = '1'
058000000710     C                   eval      wrkesito = '3'
058100990920     C                   endif
058200990920     C*
058300000613     C                   ENDSR
058400990910
058500010601
058600010601
058700010601
058800010601
058900010601      /TITLE Invio dei dati al punto operativo.
059000010601     C     opeini        BEGSR
059100010601     C*
059200010601     C* Inizializzo flag e contatori operativi
059300010601     C                   movel     '0'           flgGiro           1
059400010601     C                   movel     '0'           flgMulti          1
059500010601     C                   movel     '1'           flgStato          1
059600010615     C                   movel     '0'           flgOk             1
059700010601     C                   z-add     *zeros        cntNonEl         10 0
059800010601     C                   z-add     *zeros        depfil            3 0
059900010601     C                   z-add     *zeros        invfil            3 0
060000010601     C*
060100010601     C                   ENDSR
060200010601     C***
060300010601
060400010601
060500010601
060600010330
060700010330
060800000613     C     *inzsr        BEGSR
060900990910     C*
061000990910     C     *entry        plist
061100990920     C                   parm                    tivlrds
061200990921     C                   parm      wrkesito      esito
061300000724     C                   parm                    prmlit
061400000710     C                   parm                    prmfir
061500010330     C*
061600010330     C* CALCOLA LA DATA CORRENTE
061700010330     C                   time                    wn14             14 0
061800010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
061900010330     C                   z-add     wn8           g08dat
062000010330     C                   z-add     *zeros        g08inv
062100010330     C                   movel     '0'           g08err
062200010330     C                   call      'XSRDA8'
062300010330     C                   parm                    wlbda8
062400010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
062500000613     C*
062600000613     C                   ENDSR
062700000613     C***
062800990908
062900030507     Otitv53p   E            riepilogo         2
063000990915     O                                              'Upload via Internet'
063100990915     O                                           +1 'Traduzione TIVIN00R -'
063200030130     O                                           +1 'FIVABWWR'
063300990915     O                                           +1 'Testate bolle'
063400990915     O          E            riepilogo   2
063500990915     O                       wrkdata
063600990915     O                       wrkora              +1
063700990915     O                       procname            +1
063800990915     O          E            riepilogo   2
063900990915     O                                              'Cliente..................:'
064000010605     O                       VABCCM        z     +1
064100990915     O          E            riepilogo   2
064200990920     O                                              'Riferimento Strategi.....:'
064300990920     O                       vlrhdl              +1
064400990915     O          E            riepilogo   2
064500990915     O                                              'Giusti...................:'
064600971022     O                       §CTROK        2   +  1
064700990915     O          E            riepilogo   2
064800990915     O                                              'Sbagliati e corretti.....:'
064900971022     O                       §CTRMO        2   +  1
065000990915     O          E            riepilogo   2
065100990915     O                                              'Sbagliati e scartati.....:'
065200971022     O                       §CTRNO        2   +  1
065300000613
065400030507     Otitv53ps  E            testdett          2
065500000613     O                                              'Upload via Internet'
065600000613     O                                           +1 'Traduzione TIVIN00R -'
065700030130     O                                           +1 'FIVABWWR'
065800000613     O                                           +1 'Testate bolle'
065900000616     O          E            testdett    3
066000000613     O                                           +2 'N° rec'
066100000613     O                                           +3 'Anteprima contenuto'
066200000616     O          E            rigadett    2
066300000613     O                       rrnum               +2
066400000621     O                       recko               +3
066500000616     O          E            findett     2
066600000613     O                       wrkdata
066700000613     O                       wrkora              +1
066800000613     O                       procname            +1
066900000616     O          E            findett     2
067000000613     O                                              'Cliente..................:'
067100010605     O                       VABCCM        z     +1
067200000616     O          E            findett     2
067300000613     O                                              'Riferimento Strategi.....:'
067400000613     O                       vlrhdl              +1
067500000616     O          E            findett     2
067600000613     O                                              'Giusti...................:'
067700000613     O                       §CTROK        2   +  1
067800000616     O          E            findett     2
067900000613     O                                              'Sbagliati e corretti.....:'
068000000613     O                       §CTRMO        2   +  1
068100000616     O          E            findett     2
068200000613     O                                              'Sbagliati e scartati.....:'
068300000613     O                       §CTRNO        2   +  1
068400000613** CMD - COMANDI CL
068500030507OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV53P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
068600030507OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV53PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
068700030507DLTOVR FILE(TITV53P TITV53PS) LVL(*)
068800010606
068900000613
