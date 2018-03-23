000100040420      /TITLE Upload via Internet: traduzione in FIVABWWR. (x CELL SERVICE Cod. 0960274)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021014     FFIVABwwr  O    E             DISK    usropn
000700040420     Ftitvb2p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900040420     Ftitvb2ps  O    f  198        PRINTER usropn
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
009500040420     C                   if        not %open(titvb2ps)
009600040420     C                   open      titvb2ps
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
010700010601     C*--------------------------------------------------------
010800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010900010601     C*--------------------------------------------------------
011000010601     C     ENDELA        BEGSR
011100990915     C*
011200000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011300000613     C*
011400040420     C                   if        %open(titvb2ps)
011500000616     C                   except    findett
011600040420     C                   close     titvb2ps
011700000613     C                   endif
011800000616     C*
011900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012000011113     C                   Z-ADD     150           LENGH            15 5
012100010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012200000616     C                   PARM                    CMD(3)
012300000616     C                   PARM                    LENGH
012400000616     C*
012500010601     C                   ENDSR
012600010601     C***
012700010601
012800010601
012900000613
013000000613     C*--------------------------------------------------------
013100000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013200000613     C*--------------------------------------------------------
013300000621     C     STPR          BEGSR
013400000613     C*
013500040420     C                   if        not %open(titvb2p)
013600040420     C                   open      titvb2p
013700990915     C                   endif
013800990915     C*
013900990915     C                   except    riepilogo
014000990915     C*
014100040420     C                   if        %open(titvb2p)
014200040420     C                   close     titvb2p
014300990914     C                   endif
014400000613     C*
014500000613     C                   ENDSR
014600000613     C***
014700000613
014800010601
014900010601
015000010330     C*--------------------------------------------------------
015100040301     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015200010330     C*--------------------------------------------------------
015300010605     C     RWVAB         BEGSR
015400010330     C*
015500010330     C                   if        not %open(tivin00r)
015600010330     C                   open      tivin00r
015700010330     C                   endif
015800021014     C                   if        not %open(fivabwwr)
015900021014     C                   open      fivabwwr
016000010330     C                   endif
016100010330     C*
016200010604     C                   clear                   §CTROK
016300010604     C                   clear                   §CTRMO
016400010604     C                   clear                   §CTRNO
016500010330     C*
016600010330     C                   DO        *HIVAL
016700010330     C*
016800010330     C                   READ      tivin00r                               70
016900010618     C*
017000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017100010618     C                   if        vinflg = '1'
017200010618     C                   eval      flgOk = '1'
017300010618     C                   endif
017400010618     C*
017500010330     C                   if        vindta > *blanks
017600010330     C                   add       1             rrnum
017700010330     C*
017800010601     C                   if        *in70 = *off and
017900010330     C                             (vinflg = *blanks
018000010330     C                              or vinflg = '0'
018100010330     C                              or vinflg = '2')
018200010330     C*
018300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018400010711     C                   if        vinflg = *blanks or vinflg = '0'
018500010711     C                   clear                   vinmsg
018600010711     C                   endif
018700010601     C*
018800010330     C                   exsr      inzvar
018900010330     C                   exsr      defcam
019000010605     C                   exsr      impvab
019100010601     C*
019200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019300010605     C                   eval      depfil = VABLNP
019400010601     C                   exsr      repfil
019500010601     C                   if        depfil = invfil
019600021025     C                   if        vlrpoi = 999
019700021025     C                   MOVE(P)   invfil        VABFGS
019800021025     C                   else
019900021025     C                   MOVE(P)   vlrpoi        VABFGS
020000021025     C                   endif
020100010601     C*
020200010601     C                   exsr      PREELA
020300010601     C*
020400010604     C* Ebbene...
020500010604     C*
020600010604     C  N31              ADD       1             §CTROK            7 0
020700010604     C   32              ADD       1             §CTRMO            7 0
020800010604     C   31              ADD       1             §CTRNO            7 0
020900020205     C*
021000040301     C* Scarico il buffer del FIVAB
021100021014     C  N31              WRITE     FIVAB000
021200010604     C*
021300020722     C                   endif
021400020722     C*
021500010604     C                   if        *in31 = *off and
021600010604     C                             *in32 = *off
021700010604     C                   eval      vinflg = '1'
021800010604     C                   else
021900010604     C                   eval      recko = vindta
022000010604     C                   except    rigadett
022100010604     C                   eval      vinflg = '2'
022200010604     C                   endif
022300010604     C                   endif
022400010604     C*
022500010330     C                   else
022600010330     C                   eval      vinflg = '1'
022700010330     C                   endif
022800010601     C*
022900010601     C  N70              update    tivin000
023000010330     C*
023100010330     C  N70              ENDdo
023200010601     C
023300010601     C*
023400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023500010601     C                   if        cntNonEl = *zeros or
023600010601     C                             flgMulti = '0'
023700010330     C* Se non ci sono record con errori ...
023800010601     C                   if        §ctrno = 0 and
023900010604     C                             §ctrmo = 0 and
024000010601     C                             flgStato <> '2'
024100010330     C* ... restituisco esito OK.
024200010330     C                   eval      wrkesito = '0'
024300010330     C                   else
024400010330     C                   if        §ctrok > 0
024500010330     C                   eval      wrkesito = '1'
024600010330     C                   else
024700010615     C                   if        flgOk = '0'
024800010615     C                   eval      wrkesito = '2'
024900010615     C                   else
025000010615     C                   eval      wrkesito = '6'
025100010615     C                   endif
025200010330     C                   endif
025300010330     C                   endif
025400010601     C                   else
025500010601     C                   eval      wrkesito = '9'
025600010601     C                   endif
025700010330     C*
025800010330     C                   if        %open(tivin00r)
025900010330     C                   close     tivin00r
026000010330     C                   endif
026100021014     C                   if        %open(fivabwwr)
026200021014     C                   close     fivabwwr
026300010330     C                   endif
026400010601     C*
026500010601     C                   if        vlrpoi <> 999
026600010601     C                   eval      invfil = vlrpoi
026700010601     C                   endif
026800010330     C*
026900010330     C                   if        §ctrok > 0
027000010601     C                             and invfil > *zeros
027100010330     C                   exsr      invio
027200010330     C                   endif
027300010601     C*
027400010618     C                   if        flgGiro = '1'
027500010601     C                   exsr      endela
027600010618     C                   endif
027700010330     C*
027800010330     C                   ENDSR
027900010330     C***
028000010330
028100010601
028200010601
028300010601
028400010601
028500010330     C*----------------------------------------------------*
028600020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028700010330     C*----------------------------------------------------*
028800010330     C     INZVAR        BEGSR
028900010330     C*
029000020204     C                   Z-ADD     *zeros        Num5_0            5 0
029100020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029200020322     C                   MOVEL     '0'           FlgCAS            1
029300010330     C*
029400010330     C                   ENDSR
029500010330     C*----------------------------------------------------*
029600020722     C*  IMPOSTAZIONE CAMPI COSTANTI
029700010330     C*----------------------------------------------------*
029800010330     C     DEFCAM        BEGSR
029900010330     C*
030000020204     C* Inizializzo il buffer del record da scrivere
030100021014     C                   CLEAR                   FIVAB000
030200020204     C* Imposto i valori di default...
030300040420     C                   EVAL      VABCCM = 0960274
030400040420     C                   EVAL      VABLNP = 096
030500040420     C                   EVAL      VABCTR = 000
030600040301     C                   EVAL      VABTSP = 'C'
030700040301     C                   EVAL      VABCTM = '7Q'
030800020204     C* ... e poi verifico se sono stati passati come parametri
030900020204     C                   IF        vlrppt > *blanks
031000040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
031100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031200020204     C                   EXSR      CHKNUM
031300020204     C                   IF        PiInt=*on
031400020204     C                   Z-ADD     PiVal         VABCCM
031500020204     C                   ENDIF
031600040301     C                   ENDIF
031700040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
031800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031900020204     C                   EXSR      CHKNUM
032000020204     C                   IF        PiInt=*on
032100020204     C                   Z-ADD     PiVal         VABLNP
032200020204     C                   ENDIF
032300040301     C                   ENDIF
032400040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
032500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032600020204     C                   EXSR      CHKNUM
032700020204     C                   IF        PiInt=*on
032800020204     C                   Z-ADD     PiVal         VABCTR
032900040301     C                   ENDIF
033000020204     C                   ENDIF
033100020204     C                   ENDIF
033200020204     C*
033300010330     C                   ENDSR
033400010607     C*----------------------------------------------------*
033500021014     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033600010607     C*----------------------------------------------------*
033700010607     C     IMPVAB        BEGSR
033800010607     C*
033900010607     C                   SETOFF                                       3132
034000010607     C*
034100010607     C* Reperimento campi ALFA
034200010607     C*
034300010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034400040420     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
034500020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034600020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034700020117     C* ==
034800040420     C                   EVAL      VABRD2=%trim(%subst(vindta:36:35))
034900040420     C                   EVAL      VABIND=%trim(%subst(vindta:71:35))
035000040420     C                   EVAL      VABLOD=%trim(%subst(vindta:115:35))
035100040420     C                   EVAL      VABPRD=%trim(%subst(vindta:150:2))
035200040420     C                   EVAL      VABRMA=%trim(%subst(vindta:164:15))
035300040420     C                   EVAL      VABNOT=%trim(%subst(vindta:199:35))
035400040420     C                   EVAL      VABNT2=%trim(%subst(vindta:234:35))
035500040420     C                   IF        %trim(%subst(vindta:179:10)) = 'FRANCO'
035600040420     C                   EVAL      VABCBO = '1'
035700040420     C                   ELSE
035800040420     C                   EVAL      VABCBO = '2'
035900040301     C                   ENDIF
036000010607     C*
036100010607     C* Reperimento campi NUMERICI
036200020722     C                   MOVEL     DATCOR        VABAAS
036300020722     C                   MOVE      DATCOR        VABMGS
036400040420     C* NSP/RMN
036500040420     C                   EVAL      PiStr=%trim(%subst(vindta:164:15))
036600020304     C                   EXSR      CHKNUM
036700020304     C                   IF        PiInt=*on
036800040301     C                   Z-ADD     PiVal         VABNSP
036900040301     C                   Z-ADD     PiVal         VABRMN
037000020304     C                   ELSE
037100020722     C                   SETON                                        31
037200020304     C                   Z-ADD     *zeros        VABNSP
037300040301     C                   Z-ADD     *zeros        VABRMN
037400020304     C                   EVAL      vinmsg = %trimr(vinmsg)
037500040301     C                             + ' ' + 'VABNSP VABRMN'
037600020304     C                   ENDIF
037700040420     C* CAD
037800040420     C                   EVAL      PiStr=%trim(%subst(vindta:106:9))
037900010607     C                   EXSR      CHKNUM
038000010607     C                   IF        PiInt=*on
038100020204     C                   Z-ADD     PiVal         Num5_0
038200020204     C                   MOVEL(p)  Num5_0        VABCAD
038300010607     C                   ELSE
038400010607     C                   SETON                                        32
038500020204     C                   EVAL      VABCAD = *zeros
038600010607     C                   EVAL      vinmsg = %trimr(vinmsg)
038700020204     C                             + ' ' + 'VABCAD'
038800010607     C                   ENDIF
038900040420     C* NCL
039000040420     C                   EVAL      PiStr=%trim(%subst(vindta:152:5))
039100010607     C                   EXSR      CHKNUM
039200010607     C                   IF        PiInt=*on
039300010607     C                   Z-ADD     PiVal         VABNCL
039400010607     C                   ELSE
039500010607     C                   SETON                                        32
039600010607     C                   Z-ADD     *zeros        VABNCL
039700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039800010607     C                             + ' ' + 'VABNCL'
039900010607     C                   ENDIF
040000040420     C* PKB
040100040420     C                   EVAL      PiStr=%trim(%subst(vindta:157:7))
040200020204     C                   EVAL      PiDecChr = ','
040300010607     C                   EXSR      CHKNUM
040400010607     C                   IF        PiNum=*on
040500040420     C                   Z-ADD     PiVal         VABPKB
040600010607     C                   ELSE
040700010607     C                   SETON                                        32
040800010607     C                   Z-ADD     *zeros        VABPKB
040900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
041000010607     C                             + ' ' + 'VABPKB'
041100010607     C                   ENDIF
041200040420     C* CAS
041300040420     C                   IF        %trim(%subst(vindta:189:10)) <> '0,00'
041400040420     C                   EVAL      FlgCAS = '1'
041500040420     C                   EVAL      VABVCA = 'EUR'
041600040430     C                   EVAL      VABTIC = 'BM'
041700040420     C                   EVAL      PiStr=%trim(%subst(vindta:189:10))
041800040420     C                   EVAL      PiDecChr = ','
041900040420     C                   EXSR      CHKNUM
042000040420     C                   IF        PiNum=*on
042100040420     C                   Z-ADD     PiVal         VABCAS
042200040420     C                   ELSE
042300040420     C                   SETON                                        32
042400040420     C                   Z-ADD     *zeros        VABCAS
042500040420     C                   EVAL      vinmsg = %trimr(vinmsg)
042600040420     C                             + ' ' + 'VABCAS'
042700040420     C                   ENDIF
042800040420     C                   ENDIF
042900040420     C* IAS
043000040420     C                   EVAL      PiStr=%trim(%subst(vindta:269:10))
043100040420     C                   EVAL      PiDecChr = ','
043200040420     C                   EXSR      CHKNUM
043300040420     C                   IF        PiNum=*on
043400040420     C                   Z-ADD     PiVal         VABIAS
043500040420     C                   EVAL      VABVAS = 'EUR'
043600040420     C                   ELSE
043700040420     C                   SETON                                        32
043800040420     C                   Z-ADD     *zeros        VABIAS
043900040420     C                   EVAL      vinmsg = %trimr(vinmsg)
044000040420     C                             + ' ' + 'VABIAS'
044100040420     C                   ENDIF
044200020322     C*
044300020322     C* Considerazioni finali su CBO/CAS
044400020322     C                   IF        FlgCAS = '1'
044500040420     C                   IF        VABCBO = '1'
044600020322     C                   EVAL      VABCBO = '4'
044700040420     C                   ELSE
044800040420     C                   EVAL      VABCBO = '6'
044900040420     C                   ENDIF
045000020322     C                   ENDIF
045100020204     C*
045200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
045300020204     C                   EXSR      CHKIMPDIV
045400020204     C*
045500010607     C                   ENDSR
045600010607     C*----------------------------------------------------*
045700010601
045800020204
045900020204     C*----------------------------------------------------*
046000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
046100020204     C*----------------------------------------------------*
046200020204     C     CHKIMPDIV     BEGSR
046300020204     C*
046400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
046500020204     C                   Z-ADD     *zeros        wrkDec            9 9
046600020204     C*
046700020204     C* Come prima cosa effettuo considerazioni sulla divisa
046800020204     C                   IF        vabIAS > *zeros
046900020204     C                   IF        vabVAS <> 'EUR'
047000020204     C                   EVAL      vabVAS =  'ITL'
047100020204     C                   ENDIF
047200020204     C                   ENDIF
047300020204     C*
047400020204     C                   IF        vabCAS > *zeros
047500020204     C                   IF        vabVCA <> 'EUR'
047600020204     C                   EVAL      vabVCA =  'ITL'
047700020204     C                   ENDIF
047800020204     C                   ENDIF
047900020204     C*
048000020204     C                   IF        vabVMD > *zeros
048100020321     C                   IF        vabVAD <> 'EUR'
048200020204     C                   EVAL      vabVAD =  'ITL'
048300020204     C                   ENDIF
048400020204     C                   ENDIF
048500020204     C*
048600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
048700020204     C                   Z-ADD     vabIAS        wrkDec
048800020204     C                   IF        wrkDec > *zeros
048900020204     C                   IF        vabVAS = 'ITL'
049000020204     C                   EVAL      vabIAS = *zeros
049100020204     C                   ENDIF
049200020204     C                   ENDIF
049300020204     C*
049400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
049500020204     C                   Z-ADD     vabCAS        wrkDec
049600020204     C                   IF        wrkDec > *zeros
049700020204     C                   IF        vabVCA = 'ITL'
049800020204     C                   EVAL      vabCAS = *zeros
049900020204     C                   ENDIF
050000020204     C                   ENDIF
050100020204     C*
050200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
050300020204     C                   Z-ADD     vabVMD        wrkDec
050400020204     C                   IF        wrkDec > *zeros
050500020204     C                   IF        vabVAD = 'ITL'
050600020204     C                   EVAL      vabVMD = *zeros
050700020204     C                   ENDIF
050800020204     C                   ENDIF
050900020204     C*
051000020204     C                   ENDSR
051100020204     C***
051200020204
051300010330
051400010330
051500010330     C*----------------------------------------------------*
051600010330     C*  CONTROLLO NUMERICITA' CAMPI
051700010330     C*----------------------------------------------------*
051800010330     C     CHKNUM        BEGSR
051900010330     C*
052000010606     C                   IF        PiDecChr = *blanks
052100010606     C                   EVAL      PiDecChr = ','
052200010606     C                   ENDIF
052300010606     C*
052400010606     C                   CALL(e)   'ISNUMERIC'
052500010330     C                   PARM                    PiStr            30
052600010606     C                   PARM                    PiDecChr          1
052700010330     C                   PARM      *ZEROS        PiVal            30 9
052800010330     C                   PARM      '0'           PiInt             1
052900010330     C                   PARM      '0'           PiNum             1
053000010330     C                   IF        %error
053100010606     C                   EVAL      PiNum=*off
053200010330     C                   ENDIF
053300010330     C*
053400010330     C                   ENDSR
053500010330     C***
053600010330
053700010601
053800010601
053900010601
054000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
054100010601     C     repfil        BEGSR
054200010601     C*
054300010601     C                   if        invfil = *zeros and
054400010601     C                             depfil > *zeros and
054500010629     C                             (vinflg = *blanks or
054600010629     C                              vinflg = *zeros)
054700010601     C
054800010601     C                   eval      invfil = depfil
054900010601     C                   endif
055000010601     C*
055100010601     C                   if        depfil <> invfil and
055200010601     C                             invfil > *zeros
055300010601     C                   eval      flgMulti = '1'
055400010601     C                   if        vinflg = *blanks
055500010601     C                   add       1             cntNonEl
055600010601     C                   endif
055700010601     C                   endif
055800010601     C*
055900010601     C                   if        vinflg = '2'
056000010601     C                   eval      flgStato = '2'
056100010601     C                   endif
056200010601     C*
056300010601     C                   ENDSR
056400010601     C***
056500010601
056600010601
056700010601
056800010330
056900010330
057000010330
057100990920      /TITLE Invio dei dati al punto operativo.
057200000613     C     invio         BEGSR
057300990920     C*
057400990920     C                   reset                   dscmz
057500010601     C                   move      invfil        cmzdst
057600990920     C                   eval      cmzfld = vlrfou
057700990920     C                   eval      cmzmbd = vlrhdl
057800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
057900000710     C                   if        prmfir = *blanks
058000021014     C                   eval      cmzfla = 'FIVAB00F'
058100021014     C                   eval      cmzmba = 'FIVAB00F'
058200000710     C                   else
058300000710     C                   eval      cmzfla = prmfir
058400000710     C                   eval      cmzmba = prmfir
058500000710     C                   endif
058600990920     C                   eval      cmznrr = *zeros
058700990920     C                   move      §ctrok        cmznrr
058800021018     C                   eval      cmzlba = vlrfl1
058900990920     C                   call(e)   'TIS711C'
059000990920     C                   parm                    dscmz
059100990921     C                   parm      *blanks       esito
059200990920     C                   if        %error
059300990920     C                             or cmzerr = '1'
059400990921     C                             or esito  = '1'
059500000710     C                   eval      wrkesito = '3'
059600990920     C                   endif
059700990920     C*
059800000613     C                   ENDSR
059900990910
060000010601
060100010601
060200010601
060300010601
060400010601      /TITLE Invio dei dati al punto operativo.
060500010601     C     opeini        BEGSR
060600010601     C*
060700010601     C* Inizializzo flag e contatori operativi
060800010601     C                   movel     '0'           flgGiro           1
060900010601     C                   movel     '0'           flgMulti          1
061000010601     C                   movel     '1'           flgStato          1
061100010615     C                   movel     '0'           flgOk             1
061200010601     C                   z-add     *zeros        cntNonEl         10 0
061300010601     C                   z-add     *zeros        depfil            3 0
061400010601     C                   z-add     *zeros        invfil            3 0
061500010601     C*
061600010601     C                   ENDSR
061700010601     C***
061800010601
061900010601
062000010601
062100010330
062200010330
062300000613     C     *inzsr        BEGSR
062400990910     C*
062500990910     C     *entry        plist
062600990920     C                   parm                    tivlrds
062700990921     C                   parm      wrkesito      esito
062800000724     C                   parm                    prmlit
062900000710     C                   parm                    prmfir
063000010330     C*
063100010330     C* CALCOLA LA DATA CORRENTE
063200010330     C                   time                    wn14             14 0
063300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
063400010330     C                   z-add     wn8           g08dat
063500010330     C                   z-add     *zeros        g08inv
063600010330     C                   movel     '0'           g08err
063700010330     C                   call      'XSRDA8'
063800010330     C                   parm                    wlbda8
063900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
064000000613     C*
064100000613     C                   ENDSR
064200000613     C***
064300990908
064400040420     Otitvb2p   E            riepilogo         2
064500990915     O                                              'Upload via Internet'
064600990915     O                                           +1 'Traduzione TIVIN00R -'
064700021014     O                                           +1 'FIVABWWR'
064800990915     O                                           +1 'Testate bolle'
064900990915     O          E            riepilogo   2
065000990915     O                       wrkdata
065100990915     O                       wrkora              +1
065200990915     O                       procname            +1
065300990915     O          E            riepilogo   2
065400990915     O                                              'Cliente..................:'
065500010605     O                       VABCCM        z     +1
065600990915     O          E            riepilogo   2
065700990920     O                                              'Riferimento Strategi.....:'
065800990920     O                       vlrhdl              +1
065900990915     O          E            riepilogo   2
066000990915     O                                              'Giusti...................:'
066100971022     O                       §CTROK        2   +  1
066200990915     O          E            riepilogo   2
066300990915     O                                              'Sbagliati e corretti.....:'
066400971022     O                       §CTRMO        2   +  1
066500990915     O          E            riepilogo   2
066600990915     O                                              'Sbagliati e scartati.....:'
066700971022     O                       §CTRNO        2   +  1
066800000613
066900040420     Otitvb2ps  E            testdett          2
067000000613     O                                              'Upload via Internet'
067100000613     O                                           +1 'Traduzione TIVIN00R -'
067200021014     O                                           +1 'FIVABWWR'
067300000613     O                                           +1 'Testate bolle'
067400000616     O          E            testdett    3
067500000613     O                                           +2 'N° rec'
067600000613     O                                           +3 'Anteprima contenuto'
067700000616     O          E            rigadett    2
067800000613     O                       rrnum               +2
067900000621     O                       recko               +3
068000000616     O          E            findett     2
068100000613     O                       wrkdata
068200000613     O                       wrkora              +1
068300000613     O                       procname            +1
068400000616     O          E            findett     2
068500000613     O                                              'Cliente..................:'
068600010605     O                       VABCCM        z     +1
068700000616     O          E            findett     2
068800000613     O                                              'Riferimento Strategi.....:'
068900000613     O                       vlrhdl              +1
069000000616     O          E            findett     2
069100000613     O                                              'Giusti...................:'
069200000613     O                       §CTROK        2   +  1
069300000616     O          E            findett     2
069400000613     O                                              'Sbagliati e corretti.....:'
069500000613     O                       §CTRMO        2   +  1
069600000616     O          E            findett     2
069700000613     O                                              'Sbagliati e scartati.....:'
069800000613     O                       §CTRNO        2   +  1
069900000613** CMD - COMANDI CL
070000040420OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVB2P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
070100040420OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVB2PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
070200040420DLTOVR FILE(TITVB2P TITVB2PS) LVL(*)
070300010606
070400000613
