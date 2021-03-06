000100030513      /TITLE Upload via Internet: traduzione in FIVABWWR. (x ACERBIS Cod. 0940009)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600030130     FFIVABwwr  O    E             DISK    usropn
000700030513     Ftitv55p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900030513     Ftitv55ps  O    f  198        PRINTER usropn
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
009500030513     C                   if        not %open(titv55ps)
009600030513     C                   open      titv55ps
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
011800030513     C                   if        %open(titv55ps)
011900000616     C                   except    findett
012000030513     C                   close     titv55ps
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
014100030513     C                   if        not %open(titv55p)
014200030513     C                   open      titv55p
014300990915     C                   endif
014400990915     C*
014500990915     C                   except    riepilogo
014600990915     C*
014700030513     C                   if        %open(titv55p)
014800030513     C                   close     titv55p
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
017000010604     C                   clear                   低TROK
017100010604     C                   clear                   低TRMO
017200010604     C                   clear                   低TRNO
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
021000030513 xxx C                   exsr      PREELA
021100010601     C*
021200010604     C* Ebbene...
021300010604     C*
021400010604     C  N31              ADD       1             低TROK            7 0
021500010604     C   32              ADD       1             低TRMO            7 0
021600010604     C   31              ADD       1             低TRNO            7 0
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
024000010601     C*
024100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024200010601     C                   if        cntNonEl = *zeros or
024300010601     C                             flgMulti = '0'
024400010330     C* Se non ci sono record con errori ...
024500010601     C                   if        呃trno = 0 and
024600010604     C                             呃trmo = 0 and
024700010601     C                             flgStato <> '2'
024800010330     C* ... restituisco esito OK.
024900010330     C                   eval      wrkesito = '0'
025000010330     C                   else
025100010330     C                   if        呃trok > 0
025200010330     C                   eval      wrkesito = '1'
025300010330     C                   else
025400010615     C                   if        flgOk = '0'
025500010615     C                   eval      wrkesito = '2'
025600010615     C                   else
025700010615     C                   eval      wrkesito = '6'
025800010615     C                   endif
025900010330     C                   endif
026000010330     C                   endif
026100010601     C                   else
026200010601     C                   eval      wrkesito = '9'
026300010601     C                   endif
026400010330     C*
026500010330     C                   if        %open(tivin00r)
026600010330     C                   close     tivin00r
026700010330     C                   endif
026800030130     C                   if        %open(fivabwwr)
026900030130     C                   close     fivabwwr
027000010330     C                   endif
027100010601     C*
027200010601     C                   if        vlrpoi <> 999
027300010601     C                   eval      invfil = vlrpoi
027400010601     C                   endif
027500010330     C*
027600010330     C                   if        呃trok > 0
027700010601     C                             and invfil > *zeros
027800010330     C                   exsr      invio
027900010330     C                   endif
028000010601     C*
028100010618     C                   if        flgGiro = '1'
028200010601     C                   exsr      endela
028300010618     C                   endif
028400010330     C*
028500010330     C                   ENDSR
028600010330     C***
028700010330
028800010601
028900010601
029000010601
029100010601
029200010330     C*----------------------------------------------------*
029300020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029400010330     C*----------------------------------------------------*
029500010330     C     INZVAR        BEGSR
029600010330     C*
029700020204     C                   Z-ADD     *zeros        Num5_0            5 0
029800020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029900020322     C                   MOVEL     '0'           FlgCAS            1
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
031000030513     C                   EVAL      VABCCM = 0940009
031100030513     C                   EVAL      VABLNP = 094
031200020322     C                   EVAL      VABCTR = 000
031300030513     C                   EVAL      VABTSP = 'C'
031400020204     C* ... e poi verifico se sono stati passati come parametri
031500020204     C                   IF        vlrppt > *blanks
031600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031700020204     C                   EXSR      CHKNUM
031800020204     C                   IF        PiInt=*on
031900020204     C                   Z-ADD     PiVal         VABCCM
032000020204     C                   ENDIF
032100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032200020204     C                   EXSR      CHKNUM
032300020204     C                   IF        PiInt=*on
032400020204     C                   Z-ADD     PiVal         VABLNP
032500020204     C                   ENDIF
032600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032700020204     C                   EXSR      CHKNUM
032800020204     C                   IF        PiInt=*on
032900020204     C                   Z-ADD     PiVal         VABCTR
033000020204     C                   ENDIF
033100020204     C                   ENDIF
033200020204     C*
033300010330     C                   ENDSR
033400010607     C*----------------------------------------------------*
033500030130     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033600010607     C*----------------------------------------------------*
033700010607     C     IMPVAB        BEGSR
033800010607     C*
033900010607     C                   SETOFF                                       3132
034000010607     C*
034100010607     C* Reperimento campi ALFA
034200010607     C*
034300010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034400030514     C                   EVAL      VABRSD=%trim(%subst(vindta:13:35))
034500020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'� diventa A ==
034600020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034700020117     C* ==
034800030514     C                   EVAL      VABIND=%trim(%subst(vindta:48:30))
034900030514     C                   EVAL      VABLOD=%trim(%subst(vindta:83:30))
035000030514     C                   EVAL      VABPRD=%trim(%subst(vindta:113:2))
035100030514     C                   EVAL      VABRMA=%trim(%subst(vindta:1:6))
035200010607     C*
035300010607     C* Reperimento campi NUMERICI
035400030130     C                   MOVEL     datcor        VABAAS
035500030130     C                   MOVE      datcor        VABMGS
035600030130     C* NSP/RMN
035700030514     C                   EVAL      PiStr=%trim(%subst(vindta:1:6))
035800030130     C                   EXSR      CHKNUM
035900030130     C                   IF        PiInt=*on
036000030130     C                   Z-ADD     PiVal         VABNSP
036100030130     C                   Z-ADD     PiVal         VABRMN
036200030130     C                   ELSE
036300030130     C                   SETON                                        31
036400030130     C                   Z-ADD     *zeros        VABNSP
036500030130     C                   Z-ADD     *zeros        VABRMN
036600030130     C                   EVAL      vinmsg = %trimr(vinmsg)
036700030130     C                             + ' ' + 'VABNSP VABRMN'
036800030130     C                   ENDIF
036900030130     C* CAD
037000030514     C                   EVAL      PiStr=%trim(%subst(vindta:78:5))
037100010607     C                   EXSR      CHKNUM
037200010607     C                   IF        PiInt=*on
037300020204     C                   Z-ADD     PiVal         Num5_0
037400020204     C                   MOVEL(p)  Num5_0        VABCAD
037500010607     C                   ELSE
037600010607     C                   SETON                                        32
037700020204     C                   EVAL      VABCAD = *zeros
037800010607     C                   EVAL      vinmsg = %trimr(vinmsg)
037900020204     C                             + ' ' + 'VABCAD'
038000010607     C                   ENDIF
038100030130     C* NCL
038200030514     C                   EVAL      PiStr=%trim(%subst(vindta:115:5))
038300010607     C                   EXSR      CHKNUM
038400010607     C                   IF        PiInt=*on
038500010607     C                   Z-ADD     PiVal         VABNCL
038600010607     C                   ELSE
038700010607     C                   SETON                                        32
038800010607     C                   Z-ADD     *zeros        VABNCL
038900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039000010607     C                             + ' ' + 'VABNCL'
039100010607     C                   ENDIF
039200030130     C* PKB
039300030514     C                   EVAL      PiStr=%trim(%subst(vindta:120:7))
039400030130     C                   EXSR      CHKNUM
039500030210     C                   IF        PiNum=*on
039600030514     C                   EVAL(H)   VABPKB = PiVal/100                           * gestisco i 2 dec.
039700030130     C                   ELSE
039800030130     C                   SETON                                        32
039900030130     C                   Z-ADD     *zeros        VABPKB
040000030130     C                   EVAL      vinmsg = %trimr(vinmsg)
040100030130     C                             + ' ' + 'VABPKB'
040200030130     C                   ENDIF
040300030513     C* CAS
040400030514     C                   EVAL      PiStr=%trim(%subst(vindta:127:10))
040500030513     C                   EXSR      CHKNUM
040600030513     C                   IF        PiNum=*on
040700030514     C                   EVAL      VABCAS = PiVal/100                           * gestisco i 2 dec.
040800030513     C                   IF        VABCAS > *zeros
040900030513     C                   EVAL      VABVCA = 'EUR'
041000030513     C                   ENDIF
041100030513     C                   ELSE
041200030513     C                   SETON                                        32
041300030513     C                   Z-ADD     *zeros        VABCAS
041400030513     C                   EVAL      vinmsg = %trimr(vinmsg)
041500030513     C                             + ' ' + 'VABCAS'
041600030513     C                   ENDIF
041700020322     C*
041800020322     C* Considerazioni finali su CBO/CAS
041900030513     C                   IF        VABCAS > *zeros
042000020322     C                   EVAL      VABCBO = '4'
042100020322     C                   ELSE
042200020322     C                   EVAL      VABCBO = '1'
042300020322     C                   ENDIF
042400020204     C*
042500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
042600020204     C                   EXSR      CHKIMPDIV
042700020204     C*
042800010607     C                   ENDSR
042900010607     C*----------------------------------------------------*
043000010601
043100020204
043200020204     C*----------------------------------------------------*
043300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
043400020204     C*----------------------------------------------------*
043500020204     C     CHKIMPDIV     BEGSR
043600020204     C*
043700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilit�
043800020204     C                   Z-ADD     *zeros        wrkDec            9 9
043900020204     C*
044000020204     C* Come prima cosa effettuo considerazioni sulla divisa
044100020204     C                   IF        vabIAS > *zeros
044200020204     C                   IF        vabVAS <> 'EUR'
044300020204     C                   EVAL      vabVAS =  'ITL'
044400020204     C                   ENDIF
044500020204     C                   ENDIF
044600020204     C*
044700020204     C                   IF        vabCAS > *zeros
044800020204     C                   IF        vabVCA <> 'EUR'
044900020204     C                   EVAL      vabVCA =  'ITL'
045000020204     C                   ENDIF
045100020204     C                   ENDIF
045200020204     C*
045300020204     C                   IF        vabVMD > *zeros
045400020321     C                   IF        vabVAD <> 'EUR'
045500020204     C                   EVAL      vabVAD =  'ITL'
045600020204     C                   ENDIF
045700020204     C                   ENDIF
045800020204     C*
045900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
046000020204     C                   Z-ADD     vabIAS        wrkDec
046100020204     C                   IF        wrkDec > *zeros
046200020204     C                   IF        vabVAS = 'ITL'
046300020204     C                   EVAL      vabIAS = *zeros
046400020204     C                   ENDIF
046500020204     C                   ENDIF
046600020204     C*
046700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
046800020204     C                   Z-ADD     vabCAS        wrkDec
046900020204     C                   IF        wrkDec > *zeros
047000020204     C                   IF        vabVCA = 'ITL'
047100020204     C                   EVAL      vabCAS = *zeros
047200020204     C                   ENDIF
047300020204     C                   ENDIF
047400020204     C*
047500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
047600020204     C                   Z-ADD     vabVMD        wrkDec
047700020204     C                   IF        wrkDec > *zeros
047800020204     C                   IF        vabVAD = 'ITL'
047900020204     C                   EVAL      vabVMD = *zeros
048000020204     C                   ENDIF
048100020204     C                   ENDIF
048200020204     C*
048300020204     C                   ENDSR
048400020204     C***
048500020204
048600010330
048700010330
048800010330     C*----------------------------------------------------*
048900010330     C*  CONTROLLO NUMERICITA' CAMPI
049000010330     C*----------------------------------------------------*
049100010330     C     CHKNUM        BEGSR
049200010330     C*
049300010606     C                   IF        PiDecChr = *blanks
049400030513     C                   EVAL      PiDecChr = ','
049500010606     C                   ENDIF
049600010606     C*
049700010606     C                   CALL(e)   'ISNUMERIC'
049800010330     C                   PARM                    PiStr            30
049900010606     C                   PARM                    PiDecChr          1
050000010330     C                   PARM      *ZEROS        PiVal            30 9
050100010330     C                   PARM      '0'           PiInt             1
050200010330     C                   PARM      '0'           PiNum             1
050300010330     C                   IF        %error
050400010606     C                   EVAL      PiNum=*off
050500010330     C                   ENDIF
050600010330     C*
050700010330     C                   ENDSR
050800010330     C***
050900010330
051000010601
051100010601
051200010601
051300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
051400010601     C     repfil        BEGSR
051500010601     C*
051600010601     C                   if        invfil = *zeros and
051700010601     C                             depfil > *zeros and
051800010629     C                             (vinflg = *blanks or
051900010629     C                              vinflg = *zeros)
052000010601     C
052100010601     C                   eval      invfil = depfil
052200010601     C                   endif
052300010601     C*
052400010601     C                   if        depfil <> invfil and
052500010601     C                             invfil > *zeros
052600010601     C                   eval      flgMulti = '1'
052700010601     C                   if        vinflg = *blanks
052800010601     C                   add       1             cntNonEl
052900010601     C                   endif
053000010601     C                   endif
053100010601     C*
053200010601     C                   if        vinflg = '2'
053300010601     C                   eval      flgStato = '2'
053400010601     C                   endif
053500010601     C*
053600010601     C                   ENDSR
053700010601     C***
053800010601
053900010601
054000010601
054100010330
054200010330
054300010330
054400990920      /TITLE Invio dei dati al punto operativo.
054500000613     C     invio         BEGSR
054600990920     C*
054700990920     C                   reset                   dscmz
054800010601     C                   move      invfil        cmzdst
054900990920     C                   eval      cmzfld = vlrfou
055000990920     C                   eval      cmzmbd = vlrhdl
055100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
055200000710     C                   if        prmfir = *blanks
055300030130     C                   eval      cmzfla = 'FIVAB00F'
055400030130     C                   eval      cmzmba = 'FIVAB00F'
055500000710     C                   else
055600000710     C                   eval      cmzfla = prmfir
055700000710     C                   eval      cmzmba = prmfir
055800000710     C                   endif
055900990920     C                   eval      cmznrr = *zeros
056000990920     C                   move      呃trok        cmznrr
056100021018     C                   eval      cmzlba = vlrfl1
056200990920     C                   call(e)   'TIS711C'
056300990920     C                   parm                    dscmz
056400990921     C                   parm      *blanks       esito
056500990920     C                   if        %error
056600990920     C                             or cmzerr = '1'
056700990921     C                             or esito  = '1'
056800000710     C                   eval      wrkesito = '3'
056900990920     C                   endif
057000990920     C*
057100000613     C                   ENDSR
057200990910
057300010601
057400010601
057500010601
057600010601
057700010601      /TITLE Invio dei dati al punto operativo.
057800010601     C     opeini        BEGSR
057900010601     C*
058000010601     C* Inizializzo flag e contatori operativi
058100010601     C                   movel     '0'           flgGiro           1
058200010601     C                   movel     '0'           flgMulti          1
058300010601     C                   movel     '1'           flgStato          1
058400010615     C                   movel     '0'           flgOk             1
058500010601     C                   z-add     *zeros        cntNonEl         10 0
058600010601     C                   z-add     *zeros        depfil            3 0
058700010601     C                   z-add     *zeros        invfil            3 0
058800010601     C*
058900010601     C                   ENDSR
059000010601     C***
059100010601
059200010601
059300010601
059400010330
059500010330
059600000613     C     *inzsr        BEGSR
059700990910     C*
059800990910     C     *entry        plist
059900990920     C                   parm                    tivlrds
060000990921     C                   parm      wrkesito      esito
060100000724     C                   parm                    prmlit
060200000710     C                   parm                    prmfir
060300010330     C*
060400010330     C* CALCOLA LA DATA CORRENTE
060500010330     C                   time                    wn14             14 0
060600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
060700010330     C                   z-add     wn8           g08dat
060800010330     C                   z-add     *zeros        g08inv
060900010330     C                   movel     '0'           g08err
061000010330     C                   call      'XSRDA8'
061100010330     C                   parm                    wlbda8
061200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
061300000613     C*
061400000613     C                   ENDSR
061500000613     C***
061600990908
061700030513     Otitv55p   E            riepilogo         2
061800990915     O                                              'Upload via Internet'
061900990915     O                                           +1 'Traduzione TIVIN00R -'
062000030130     O                                           +1 'FIVABWWR'
062100990915     O                                           +1 'Testate bolle'
062200990915     O          E            riepilogo   2
062300990915     O                       wrkdata
062400990915     O                       wrkora              +1
062500990915     O                       procname            +1
062600990915     O          E            riepilogo   2
062700990915     O                                              'Cliente..................:'
062800010605     O                       VABCCM        z     +1
062900990915     O          E            riepilogo   2
063000990920     O                                              'Riferimento Strategi.....:'
063100990920     O                       vlrhdl              +1
063200990915     O          E            riepilogo   2
063300990915     O                                              'Giusti...................:'
063400971022     O                       低TROK        2   +  1
063500990915     O          E            riepilogo   2
063600990915     O                                              'Sbagliati e corretti.....:'
063700971022     O                       低TRMO        2   +  1
063800990915     O          E            riepilogo   2
063900990915     O                                              'Sbagliati e scartati.....:'
064000971022     O                       低TRNO        2   +  1
064100000613
064200030513     Otitv55ps  E            testdett          2
064300000613     O                                              'Upload via Internet'
064400000613     O                                           +1 'Traduzione TIVIN00R -'
064500030130     O                                           +1 'FIVABWWR'
064600000613     O                                           +1 'Testate bolle'
064700000616     O          E            testdett    3
064800000613     O                                           +2 'N� rec'
064900000613     O                                           +3 'Anteprima contenuto'
065000000616     O          E            rigadett    2
065100000613     O                       rrnum               +2
065200000621     O                       recko               +3
065300000616     O          E            findett     2
065400000613     O                       wrkdata
065500000613     O                       wrkora              +1
065600000613     O                       procname            +1
065700000616     O          E            findett     2
065800000613     O                                              'Cliente..................:'
065900010605     O                       VABCCM        z     +1
066000000616     O          E            findett     2
066100000613     O                                              'Riferimento Strategi.....:'
066200000613     O                       vlrhdl              +1
066300000616     O          E            findett     2
066400000613     O                                              'Giusti...................:'
066500000613     O                       低TROK        2   +  1
066600000616     O          E            findett     2
066700000613     O                                              'Sbagliati e corretti.....:'
066800000613     O                       低TRMO        2   +  1
066900000616     O          E            findett     2
067000000613     O                                              'Sbagliati e scartati.....:'
067100000613     O                       低TRNO        2   +  1
067200000613** CMD - COMANDI CL
067300030513OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV55P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
067400030513OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV55PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
067500030513DLTOVR FILE(TITV55P TITV55PS) LVL(*)
067600010606
067700000613
