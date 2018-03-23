000100051221      /TITLE Upload via Internet: traduzione in FIVABWWR. (x THELMA   Cod. 1050011)
000200051221      /TITLE Upload via Internet: traduzione in FIVABWWR. (x ARTHE EL Cod. 1050213)
000300990908     H dftactgrp(*yes)
000400051221     F*
000500000724     Fazorg01l  if   e           k disk
000600990910     Ftivin00r  uF   E             DISK    usropn
000700021014     FFIVABwwr  O    E             DISK    usropn
000800050218     Ftitvi3p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000050218     Ftitvi3ps  O    f  198        PRINTER usropn
001100000621     F                                     oflind(*inob)
001200000313     D*
001300000613     D*------------
001400000613     D* COMANDI
001500000613     D*------------
001600011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001700010330     D*----------------------------------------------------
001800010330     D* DICHIARAZIOINE VARIABILI DI WRK
001900010330     D*----------------------------------------------------
002000010330     D dscmz         e ds                  inz
002100010330     D psds           sds
002200010330     D  procname         *PROC
002300010330     D tivlrds       e ds                  extname(tivlr00f)
002400050223     D tisi95ds      e ds
002500010330     D esito           s              1
002600010330     D prmlit          s             10
002700010330     D prmfir          s             10
002800010330     D wrkesito        s                   like(esito)
002900010330     D wrkdata         s               d
003000010330     D wrkora          s               t
003100010330     D rrnum           s              6  0 INZ(*zeros)
003200010330     D recko           s            150    INZ(*blanks)
003300011113     D depcmd          s            150    INZ(*blanks)
003400010330     D*------------------
003500010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003600010330     D*------------------
003700010330     D WLBDA8          DS                  INZ
003800010330     D  G08DAT                 1      8  0
003900010330     D  G08INV                 9     16  0
004000010330     D  G08ERR                17     17
004100010330     D  G08TGI                18     22  0
004200000313     D*
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
005300050223     C*
005400050223     C* Effettuo lancio TISI95 solo x chiusura
005500050223     C                   CLEAR                   TISI95DS
005600050223     C                   EVAL      I95TLA = 'C'
005700050223     C                   CALL      'TISI95R'
005800050223     C                   PARM                    TISI95DS
005900010601     C*
006000010601     C                   seton                                        lr
006100010601
006200010601
006300010601
006400010601
006500010601     C*--------------------------------------------------------
006600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006700010601     C*--------------------------------------------------------
006800010601     C     PREELA        BEGSR
006900010601     C*
007000010601     C* SE OCCORRE SPEDIRE IN FILIALE
007100010601     C                   if        invfil <> *zeros and
007200010601     C                             flgGiro = '0'
007300010601     C*
007400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007500010601     C                   eval      flgGiro = '1'
007600000724     C*
007700000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007800000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007900010601     C     invfil        chain     azorg01l
008000000724     C                   if        %found
008100000724     C                   movel(p)  CMD(1)        depcmd
008200020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008300000724     C*
008400000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008500011113     C                   Z-ADD     150           LENGH            15 5
008600000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008700000724     C                   PARM                    depcmd
008800000724     C                   PARM                    LENGH
008900000724     C*
009000000724     C                   endif
009100000616     C*
009200000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009300000616     C                   movel(p)  CMD(2)        depcmd
009400000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009500000616     C*
009600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009700011113     C                   Z-ADD     150           LENGH            15 5
009800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009900000616     C                   PARM                    depcmd
010000000616     C                   PARM                    LENGH
010100000613     C*
010200050218     C                   if        not %open(titvi3ps)
010300050218     C                   open      titvi3ps
010400000616     C                   except    testdett
010500000613     C                   endif
010600010601     C*
010700010601     C                   endif
010800010601     C*
010900010601     C                   ENDSR
011000010601     C***
011100010601
011200010601
011300010601
011400010601     C*--------------------------------------------------------
011500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011600010601     C*--------------------------------------------------------
011700010601     C     ENDELA        BEGSR
011800990915     C*
011900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012000000613     C*
012100050218     C                   if        %open(titvi3ps)
012200000616     C                   except    findett
012300050218     C                   close     titvi3ps
012400000613     C                   endif
012500000616     C*
012600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012700011113     C                   Z-ADD     150           LENGH            15 5
012800010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012900000616     C                   PARM                    CMD(3)
013000000616     C                   PARM                    LENGH
013100000616     C*
013200010601     C                   ENDSR
013300010601     C***
013400010601
013500010601
013600000613
013700000613     C*--------------------------------------------------------
013800000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013900000613     C*--------------------------------------------------------
014000000621     C     STPR          BEGSR
014100000613     C*
014200050218     C                   if        not %open(titvi3p)
014300050218     C                   open      titvi3p
014400990915     C                   endif
014500990915     C*
014600990915     C                   except    riepilogo
014700990915     C*
014800050218     C                   if        %open(titvi3p)
014900050218     C                   close     titvi3p
015000990914     C                   endif
015100000613     C*
015200000613     C                   ENDSR
015300000613     C***
015400000613
015500010601
015600010601
015700010330     C*--------------------------------------------------------
015800040301     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015900010330     C*--------------------------------------------------------
016000010605     C     RWVAB         BEGSR
016100010330     C*
016200010330     C                   if        not %open(tivin00r)
016300010330     C                   open      tivin00r
016400010330     C                   endif
016500021014     C                   if        not %open(fivabwwr)
016600021014     C                   open      fivabwwr
016700010330     C                   endif
016800010330     C*
016900010604     C                   clear                   §CTROK
017000010604     C                   clear                   §CTRMO
017100010604     C                   clear                   §CTRNO
017200010330     C*
017300010330     C                   DO        *HIVAL
017400010330     C*
017500010330     C                   READ      tivin00r                               70
017600010618     C*
017700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017800010618     C                   if        vinflg = '1'
017900010618     C                   eval      flgOk = '1'
018000010618     C                   endif
018100010618     C*
018200010330     C                   if        vindta > *blanks
018300010330     C                   add       1             rrnum
018400010330     C*
018500010601     C                   if        *in70 = *off and
018600010330     C                             (vinflg = *blanks
018700010330     C                              or vinflg = '0'
018800010330     C                              or vinflg = '2')
018900050217     C*
019000050217     C                   clear                   FIVAB000
019100010330     C*
019200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019300010711     C                   if        vinflg = *blanks or vinflg = '0'
019400010711     C                   clear                   vinmsg
019500010711     C                   endif
019600010601     C*
019700010330     C                   exsr      inzvar
019800010330     C                   exsr      defcam
019900010605     C                   exsr      impvab
020000010601     C*
020100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020200010605     C                   eval      depfil = VABLNP
020300010601     C                   exsr      repfil
020400010601     C                   if        depfil = invfil
020500021025     C                   if        vlrpoi = 999
020600021025     C                   MOVE(P)   invfil        VABFGS
020700021025     C                   else
020800021025     C                   MOVE(P)   vlrpoi        VABFGS
020900021025     C                   endif
021000050114     C                   endif
021100010601     C*
021200010601     C                   exsr      PREELA
021300010601     C*
021400010604     C* Ebbene...
021500010604     C*
021600010604     C  N31              ADD       1             §CTROK            7 0
021700010604     C   32              ADD       1             §CTRMO            7 0
021800010604     C   31              ADD       1             §CTRNO            7 0
021900020205     C*
022000040301     C* Scarico il buffer del FIVAB
022100021014     C  N31              WRITE     FIVAB000
022200010604     C*
022300020722     C                   endif
022400020722     C*
022500010604     C                   if        *in31 = *off and
022600010604     C                             *in32 = *off
022700010604     C                   eval      vinflg = '1'
022800010604     C                   else
022900010604     C                   eval      recko = vindta
023000010604     C                   except    rigadett
023100010604     C                   eval      vinflg = '2'
023200010604     C                   endif
023300010604     C*
023400010330     C                   else
023500010330     C                   eval      vinflg = '1'
023600010330     C                   endif
023700010601     C*
023800010601     C  N70              update    tivin000
023900010330     C*
024000010330     C  N70              ENDdo
024100010601     C
024200010601     C*
024300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024400010601     C                   if        cntNonEl = *zeros or
024500010601     C                             flgMulti = '0'
024600010330     C* Se non ci sono record con errori ...
024700010601     C                   if        §ctrno = 0 and
024800010604     C                             §ctrmo = 0 and
024900010601     C                             flgStato <> '2'
025000010330     C* ... restituisco esito OK.
025100010330     C                   eval      wrkesito = '0'
025200010330     C                   else
025300010330     C                   if        §ctrok > 0
025400010330     C                   eval      wrkesito = '1'
025500010330     C                   else
025600010615     C                   if        flgOk = '0'
025700010615     C                   eval      wrkesito = '2'
025800010615     C                   else
025900010615     C                   eval      wrkesito = '6'
026000010615     C                   endif
026100010330     C                   endif
026200010330     C                   endif
026300010601     C                   else
026400010601     C                   eval      wrkesito = '9'
026500010601     C                   endif
026600010330     C*
026700010330     C                   if        %open(tivin00r)
026800010330     C                   close     tivin00r
026900010330     C                   endif
027000021014     C                   if        %open(fivabwwr)
027100021014     C                   close     fivabwwr
027200010330     C                   endif
027300010601     C*
027400010601     C                   if        vlrpoi <> 999
027500010601     C                   eval      invfil = vlrpoi
027600010601     C                   endif
027700010330     C*
027800010330     C                   if        §ctrok > 0
027900010601     C                             and invfil > *zeros
028000010330     C                   exsr      invio
028100010330     C                   endif
028200010601     C*
028300010618     C                   if        flgGiro = '1'
028400010601     C                   exsr      endela
028500010618     C                   endif
028600010330     C*
028700010330     C                   ENDSR
028800010330     C***
028900010330
029000010601
029100010601
029200010601
029300010601
029400010330     C*----------------------------------------------------*
029500020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029600010330     C*----------------------------------------------------*
029700010330     C     INZVAR        BEGSR
029800010330     C*
029900020204     C                   Z-ADD     *zeros        Num5_0            5 0
030000020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
030100020322     C                   MOVEL     '0'           FlgCAS            1
030200010330     C*
030300010330     C                   ENDSR
030400010330     C*----------------------------------------------------*
030500020722     C*  IMPOSTAZIONE CAMPI COSTANTI
030600010330     C*----------------------------------------------------*
030700010330     C     DEFCAM        BEGSR
030800010330     C*
030900020204     C* Inizializzo il buffer del record da scrivere
031000021014     C                   CLEAR                   FIVAB000
031100020204     C* Imposto i valori di default...
031200060109     C                   EVAL      VABCCM = 1050011
031300060109     C                   EVAL      VABLNP = 105
031400050218     C                   EVAL      VABCTR = 300
031500041108     C                   EVAL      VABCBO = '1'
031600050218     C                   EVAL      VABTSP = 'C'
031700020204     C* ... e poi verifico se sono stati passati come parametri
031800020204     C                   IF        vlrppt > *blanks
031900040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
032000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032100020204     C                   EXSR      CHKNUM
032200020204     C                   IF        PiInt=*on
032300020204     C                   Z-ADD     PiVal         VABCCM
032400020204     C                   ENDIF
032500040301     C                   ENDIF
032600040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
032700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032800020204     C                   EXSR      CHKNUM
032900020204     C                   IF        PiInt=*on
033000020204     C                   Z-ADD     PiVal         VABLNP
033100020204     C                   ENDIF
033200040301     C                   ENDIF
033300040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
033400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033500020204     C                   EXSR      CHKNUM
033600020204     C                   IF        PiInt=*on
033700020204     C                   Z-ADD     PiVal         VABCTR
033800040301     C                   ENDIF
033900020204     C                   ENDIF
034000020204     C                   ENDIF
034100020204     C*
034200010330     C                   ENDSR
034300010607     C*----------------------------------------------------*
034400021014     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034500010607     C*----------------------------------------------------*
034600010607     C     IMPVAB        BEGSR
034700010607     C*
034800010607     C                   SETOFF                                       3132
034900010607     C*
035000010607     C* Reperimento campi ALFA
035100010607     C*
035200010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
035300050218     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
035400020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035500020117     C     '@':'A'       XLATE     VABRSD        VABRSD
035600020117     C* ==
035700050218     C                   EVAL      VABRD2=%trim(%subst(vindta:1+35:35))
035800050218     C                   EVAL      VABIND=%trim(%subst(vindta:71:35))
035900050221     C                   EVAL      VABLOD=%trim(%subst(vindta:115:35))
036000111130     C                   EVAL      VABRMA=%trim(%subst(vindta:173:6))
036100050221     C                   EVAL      VABNOT=%trim(%subst(vindta:203:35))
036200050221     C                   EVAL      VABNT2=%trim(%subst(vindta:203+35:35))
036300010607     C*
036400010607     C* Reperimento campi NUMERICI
036500020722     C                   MOVEL     DATCOR        VABAAS
036600020722     C                   MOVE      DATCOR        VABMGS
036700040420     C* NSP/RMN
036800111130     C                   EVAL      PiStr=%trim(%subst(vindta:173:6))
036900020304     C                   EXSR      CHKNUM
037000020304     C                   IF        PiInt=*on
037100040301     C                   Z-ADD     PiVal         VABNSP
037200040301     C                   Z-ADD     PiVal         VABRMN
037300020304     C                   ELSE
037400050217     C                   SETON                                        31
037500050217     C                   Z-ADD     *zeros        VABNSP
037600050217     C                   Z-ADD     *zeros        VABRMN
037700020304     C                   EVAL      vinmsg = %trimr(vinmsg)
037800040301     C                             + ' ' + 'VABNSP VABRMN'
037900020304     C                   ENDIF
038000050128     C* CAD
038100050218     C                   EVAL      PiStr=%trim(%subst(vindta:106:5))
038200050128     C                   EXSR      CHKNUM
038300050128     C                   IF        PiInt=*on
038400050128     C                   Z-ADD     PiVal         Num5_0
038500050128     C                   MOVEL(p)  Num5_0        VABCAD
038600050128     C                   ELSE
038700050128     C                   SETON                                        32
038800050128     C                   EVAL      VABCAD = *zeros
038900050128     C                   EVAL      vinmsg = %trimr(vinmsg)
039000050128     C                             + ' ' + 'VABCAD'
039100050128     C                   ENDIF
039200050223     C* PRD (reperita mediante TISI95)
039300050223     C                   IF        VABCAD <> *blanks
039400050223     C                   CLEAR                   TISI95DS
039500050223     C                   EVAL      I95TCN = '3'
039600050223     C                   Z-ADD     datcor        I95DAT
039700050223     C                   EVAL      I95CAP = VABCAD
039800050223     C                   EVAL      I95LOC = VABLOD
039900050223     C                   CALL      'TISI95R'
040000050223     C                   PARM                    TISI95DS
040100050223     C                   EVAL      VABPRD = O95PRV
040200050223     C                   ENDIF
040300040420     C* NCL
040400050221     C                   EVAL      PiStr=%trim(%subst(vindta:150:5))
040500010607     C                   EXSR      CHKNUM
040600010607     C                   IF        PiInt=*on
040700010607     C                   Z-ADD     PiVal         VABNCL
040800010607     C                   ELSE
040900010607     C                   SETON                                        32
041000010607     C                   Z-ADD     *zeros        VABNCL
041100010607     C                   EVAL      vinmsg = %trimr(vinmsg)
041200010607     C                             + ' ' + 'VABNCL'
041300010607     C                   ENDIF
041400040420     C* PKB
041500050221     C                   EVAL      PiStr=%trim(%subst(vindta:155:9))
041600010607     C                   EXSR      CHKNUM
041700010607     C                   IF        PiNum=*on
041800050114     C                   Z-ADD     PiVal         VABPKB
041900010607     C                   ELSE
042000010607     C                   SETON                                        32
042100010607     C                   Z-ADD     *zeros        VABPKB
042200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
042300010607     C                             + ' ' + 'VABPKB'
042400010607     C                   ENDIF
042500050216     C* CAS
042600050221     C                   IF        %trim(%subst(vindta:189:11)) <> *blanks
042700050216     C                   MOVEL     '1'           FlgCAS
042800050221     C                   EVAL      PiStr=%trim(%subst(vindta:189:11))
042900050217     C                   EVAL      VABVCA='EUR'
043000050221     C                   EVAL      VABTIC='BM'
043100050216     C                   EXSR      CHKNUM
043200050216     C                   IF        PiNum=*on
043300050216     C                   Z-ADD     PiVal         VABCAS
043400050216     C                   ELSE
043500050216     C                   SETON                                        32
043600050216     C                   EVAL      vinmsg = %trimr(vinmsg)
043700050216     C                             + ' ' + 'VABCAS'
043800050216     C                   ENDIF
043900050216     C                   ENDIF
044000020322     C*
044100020322     C* Considerazioni finali su CBO/CAS
044200020322     C                   IF        FlgCAS = '1'
044300040713     C                   IF        VABCBO = '1'
044400020322     C                   EVAL      VABCBO = '4'
044500040713     C                   ENDIF
044600040713     C                   IF        VABCBO = '2'
044700040713     C                   EVAL      VABCBO = '6'
044800040713     C                   ENDIF
044900020322     C                   ENDIF
045000020204     C*
045100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
045200020204     C                   EXSR      CHKIMPDIV
045300020204     C*
045400010607     C                   ENDSR
045500010607     C*----------------------------------------------------*
045600010601
045700020204
045800020204     C*----------------------------------------------------*
045900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
046000020204     C*----------------------------------------------------*
046100020204     C     CHKIMPDIV     BEGSR
046200020204     C*
046300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
046400020204     C                   Z-ADD     *zeros        wrkDec            9 9
046500020204     C*
046600020204     C* Come prima cosa effettuo considerazioni sulla divisa
046700020204     C                   IF        vabIAS > *zeros
046800020204     C                   IF        vabVAS <> 'EUR'
046900020204     C                   EVAL      vabVAS =  'ITL'
047000020204     C                   ENDIF
047100020204     C                   ENDIF
047200020204     C*
047300020204     C                   IF        vabCAS > *zeros
047400020204     C                   IF        vabVCA <> 'EUR'
047500020204     C                   EVAL      vabVCA =  'ITL'
047600020204     C                   ENDIF
047700020204     C                   ENDIF
047800020204     C*
047900020204     C                   IF        vabVMD > *zeros
048000020321     C                   IF        vabVAD <> 'EUR'
048100020204     C                   EVAL      vabVAD =  'ITL'
048200020204     C                   ENDIF
048300020204     C                   ENDIF
048400020204     C*
048500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
048600020204     C                   Z-ADD     vabIAS        wrkDec
048700020204     C                   IF        wrkDec > *zeros
048800020204     C                   IF        vabVAS = 'ITL'
048900020204     C                   EVAL      vabIAS = *zeros
049000020204     C                   ENDIF
049100020204     C                   ENDIF
049200020204     C*
049300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
049400020204     C                   Z-ADD     vabCAS        wrkDec
049500020204     C                   IF        wrkDec > *zeros
049600020204     C                   IF        vabVCA = 'ITL'
049700020204     C                   EVAL      vabCAS = *zeros
049800020204     C                   ENDIF
049900020204     C                   ENDIF
050000020204     C*
050100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
050200020204     C                   Z-ADD     vabVMD        wrkDec
050300020204     C                   IF        wrkDec > *zeros
050400020204     C                   IF        vabVAD = 'ITL'
050500020204     C                   EVAL      vabVMD = *zeros
050600020204     C                   ENDIF
050700020204     C                   ENDIF
050800020204     C*
050900020204     C                   ENDSR
051000020204     C***
051100020204
051200010330
051300010330
051400010330     C*----------------------------------------------------*
051500010330     C*  CONTROLLO NUMERICITA' CAMPI
051600010330     C*----------------------------------------------------*
051700010330     C     CHKNUM        BEGSR
051800010330     C*
051900010606     C                   IF        PiDecChr = *blanks
052000050217     C                   EVAL      PiDecChr = ','
052100010606     C                   ENDIF
052200010606     C*
052300010606     C                   CALL(e)   'ISNUMERIC'
052400010330     C                   PARM                    PiStr            30
052500010606     C                   PARM                    PiDecChr          1
052600010330     C                   PARM      *ZEROS        PiVal            30 9
052700010330     C                   PARM      '0'           PiInt             1
052800010330     C                   PARM      '0'           PiNum             1
052900010330     C                   IF        %error
053000010606     C                   EVAL      PiNum=*off
053100010330     C                   ENDIF
053200010330     C*
053300010330     C                   ENDSR
053400010330     C***
053500010330
053600010601
053700010601
053800010601
053900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
054000010601     C     repfil        BEGSR
054100010601     C*
054200010601     C                   if        invfil = *zeros and
054300010601     C                             depfil > *zeros and
054400010629     C                             (vinflg = *blanks or
054500010629     C                              vinflg = *zeros)
054600010601     C
054700010601     C                   eval      invfil = depfil
054800010601     C                   endif
054900010601     C*
055000010601     C                   if        depfil <> invfil and
055100010601     C                             invfil > *zeros
055200010601     C                   eval      flgMulti = '1'
055300010601     C                   if        vinflg = *blanks
055400010601     C                   add       1             cntNonEl
055500010601     C                   endif
055600010601     C                   endif
055700010601     C*
055800010601     C                   if        vinflg = '2'
055900010601     C                   eval      flgStato = '2'
056000010601     C                   endif
056100010601     C*
056200010601     C                   ENDSR
056300010601     C***
056400050217
056500050217
056600050217
056700050217
056800050217      /TITLE Invio dei dati al punto operativo.
056900050217     C     invio         BEGSR
057000050217     C*
057100050217     C                   reset                   dscmz
057200050217     C                   move      vlrpoi        cmzdst
057300050217     C                   eval      cmzfld = vlrfou
057400050217     C                   eval      cmzmbd = vlrhdl
057500050217     C                   eval      %subst(cmzmbd:1:1) = 'M'
057600050217     C***                if        prmfir = *blanks
057700050217     C                   eval      cmzfla = 'FIVAB00F'
057800050217     C                   eval      cmzmba = 'FIVAB00F'
057900050217     C***                else
058000050217     C***                eval      cmzfla = prmfir
058100050217     C***                eval      cmzmba = prmfir
058200050217     C***                endif
058300050217     C                   eval      cmznrr = *zeros
058400050217     C                   move      §ctrok        cmznrr
058500050217     C                   eval      cmzlba = vlrfl1
058600050217     C                   call(e)   'TIS711C'
058700050217     C                   parm                    dscmz
058800050217     C                   parm      *blanks       esito
058900050217     C                   if        %error
059000050217     C                             or cmzerr = '1'
059100050217     C                             or esito  = '1'
059200050217     C                   eval      wrkesito = '3'
059300050217     C                   endif
059400050217     C*
059500050217     C                   ENDSR
059600050217     C***
059700050217
059800050217
059900050217
060000010601
060100010601
060200010601      /TITLE Invio dei dati al punto operativo.
060300010601     C     opeini        BEGSR
060400010601     C*
060500010601     C* Inizializzo flag e contatori operativi
060600010601     C                   movel     '0'           flgGiro           1
060700010601     C                   movel     '0'           flgMulti          1
060800010601     C                   movel     '1'           flgStato          1
060900010615     C                   movel     '0'           flgOk             1
061000010601     C                   z-add     *zeros        cntNonEl         10 0
061100010601     C                   z-add     *zeros        depfil            3 0
061200010601     C                   z-add     *zeros        invfil            3 0
061300010601     C*
061400010601     C                   ENDSR
061500010601     C***
061600010601
061700010601
061800010601
061900010330
062000010330
062100000613     C     *inzsr        BEGSR
062200990910     C*
062300990910     C     *entry        plist
062400990920     C                   parm                    tivlrds
062500990921     C                   parm      wrkesito      esito
062600000724     C                   parm                    prmlit
062700000710     C                   parm                    prmfir
062800010330     C*
062900010330     C* CALCOLA LA DATA CORRENTE
063000010330     C                   time                    wn14             14 0
063100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
063200010330     C                   z-add     wn8           g08dat
063300010330     C                   z-add     *zeros        g08inv
063400010330     C                   movel     '0'           g08err
063500010330     C                   call      'XSRDA8'
063600010330     C                   parm                    wlbda8
063700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
063800000613     C*
063900000613     C                   ENDSR
064000000613     C***
064100990908
064200050218     Otitvi3p   E            riepilogo         2
064300990915     O                                              'Upload via Internet'
064400990915     O                                           +1 'Traduzione TIVIN00R -'
064500021014     O                                           +1 'FIVABWWR'
064600990915     O                                           +1 'Testate bolle'
064700990915     O          E            riepilogo   2
064800990915     O                       wrkdata
064900990915     O                       wrkora              +1
065000990915     O                       procname            +1
065100990915     O          E            riepilogo   2
065200990915     O                                              'Cliente..................:'
065300010605     O                       VABCCM        z     +1
065400990915     O          E            riepilogo   2
065500990920     O                                              'Riferimento Strategi.....:'
065600990920     O                       vlrhdl              +1
065700990915     O          E            riepilogo   2
065800990915     O                                              'Giusti...................:'
065900971022     O                       §CTROK        2   +  1
066000990915     O          E            riepilogo   2
066100990915     O                                              'Sbagliati e corretti.....:'
066200971022     O                       §CTRMO        2   +  1
066300990915     O          E            riepilogo   2
066400990915     O                                              'Sbagliati e scartati.....:'
066500971022     O                       §CTRNO        2   +  1
066600000613
066700050218     Otitvi3ps  E            testdett          2
066800000613     O                                              'Upload via Internet'
066900000613     O                                           +1 'Traduzione TIVIN00R -'
067000021014     O                                           +1 'FIVABWWR'
067100000613     O                                           +1 'Testate bolle'
067200000616     O          E            testdett    3
067300000613     O                                           +2 'N° rec'
067400000613     O                                           +3 'Anteprima contenuto'
067500000616     O          E            rigadett    2
067600000613     O                       rrnum               +2
067700000621     O                       recko               +3
067800000616     O          E            findett     2
067900000613     O                       wrkdata
068000000613     O                       wrkora              +1
068100000613     O                       procname            +1
068200000616     O          E            findett     2
068300000613     O                                              'Cliente..................:'
068400010605     O                       VABCCM        z     +1
068500000616     O          E            findett     2
068600000613     O                                              'Riferimento Strategi.....:'
068700000613     O                       vlrhdl              +1
068800000616     O          E            findett     2
068900000613     O                                              'Giusti...................:'
069000000613     O                       §CTROK        2   +  1
069100000616     O          E            findett     2
069200000613     O                                              'Sbagliati e corretti.....:'
069300000613     O                       §CTRMO        2   +  1
069400000616     O          E            findett     2
069500000613     O                                              'Sbagliati e scartati.....:'
069600000613     O                       §CTRNO        2   +  1
069700000613** CMD - COMANDI CL
069800050218OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVI3P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
069900050218OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVI3PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
070000050218DLTOVR FILE(TITVI3P TITVI3PS) LVL(*)
070100010606
070200000613
