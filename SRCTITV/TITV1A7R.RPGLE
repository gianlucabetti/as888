000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FFIVABWWR  O    E             DISK    usropn
000700070926     Ftitv1a7p  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900070926     Ftitv1a7ps O    f  198        PRINTER usropn
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
002300070502     D tisi95ds      e ds
002400010330     D esito           s              1
002500010330     D prmlit          s             10
002600010330     D prmfir          s             10
002700010330     D wrkesito        s                   like(esito)
002800010330     D wrkdata         s               d
002900010330     D wrkora          s               t
003000010330     D rrnum           s              6  0 INZ(*zeros)
003100010330     D recko           s            150    INZ(*blanks)
003200011113     D depcmd          s            150    INZ(*blanks)
003300010330     D*------------------
003400010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003500010330     D*------------------
003600010330     D WLBDA8          DS                  INZ
003700010330     D  G08DAT                 1      8  0
003800010330     D  G08INV                 9     16  0
003900010330     D  G08ERR                17     17
004000010330     D  G08TGI                18     22  0
004100010330
004200010330
004300990915     C                   time                    wrkdata
004400990915     C                   time                    wrkora
004500000913     C                   reset                   rrnum
004600990921     C                   reset                   esito
004700990921     C                   reset                   wrkesito
004800010601     C*
004900010601     C                   exsr      opeini
005000010605     C                   exsr      rwvab
005100070502     C*
005200070502     C* Effettuo lancio TISI95 solo x chiusura
005300070502     C                   CLEAR                   TISI95DS
005400070502     C                   EVAL      I95TLA = 'C'
005500070502     C                   CALL      'TISI95R'
005600070502     C                   PARM                    TISI95DS
005700010601     C*
005800010601     C                   seton                                        lr
005900010601
006000010601
006100010601
006200010601
006300010601     C*--------------------------------------------------------
006400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006500010601     C*--------------------------------------------------------
006600010601     C     PREELA        BEGSR
006700010601     C*
006800010601     C* SE OCCORRE SPEDIRE IN FILIALE
006900010601     C                   if        invfil <> *zeros and
007000010601     C                             flgGiro = '0'
007100010601     C*
007200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007300010601     C                   eval      flgGiro = '1'
007400000724     C*
007500000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007600000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007700010601     C     invfil        chain     azorg01l
007800000724     C                   if        %found
007900000724     C                   movel(p)  CMD(1)        depcmd
008000020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008100000724     C*
008200000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008300011113     C                   Z-ADD     150           LENGH            15 5
008400000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008500000724     C                   PARM                    depcmd
008600000724     C                   PARM                    LENGH
008700000724     C*
008800000724     C                   endif
008900000616     C*
009000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009100000616     C                   movel(p)  CMD(2)        depcmd
009200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009300000616     C*
009400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009500011113     C                   Z-ADD     150           LENGH            15 5
009600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009700000616     C                   PARM                    depcmd
009800000616     C                   PARM                    LENGH
009900000613     C*
010000070926     C                   if        not %open(titv1a7ps)
010100070926     C                   open      titv1a7ps
010200000616     C                   except    testdett
010300000613     C                   endif
010400010601     C*
010500010601     C                   endif
010600010601     C*
010700010601     C                   ENDSR
010800010601     C***
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
011900070926     C                   if        %open(titv1a7ps)
012000000616     C                   except    findett
012100070926     C                   close     titv1a7ps
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
013400000613
013500000613     C*--------------------------------------------------------
013600000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013700000613     C*--------------------------------------------------------
013800000621     C     STPR          BEGSR
013900000613     C*
014000070926     C                   if        not %open(titv1a7p)
014100070926     C                   open      titv1a7p
014200990915     C                   endif
014300990915     C*
014400990915     C                   except    riepilogo
014500990915     C*
014600070926     C                   if        %open(titv1a7p)
014700070926     C                   close     titv1a7p
014800990914     C                   endif
014900000613     C*
015000000613     C                   ENDSR
015100000613     C***
015200000613
015300010601
015400010601
015500010330     C*--------------------------------------------------------
015600050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
015700010330     C*--------------------------------------------------------
015800010605     C     RWVAB         BEGSR
015900050408     C*
016000010330     C                   if        not %open(tivin00r)
016100010330     C                   open      tivin00r
016200010330     C                   endif
016300050408     C                   if        not %open(fivabwwr)
016400050408     C                   open      fivabwwr
016500010330     C                   endif
016600010330     C*
016700010604     C                   clear                   §CTROK
016800010604     C                   clear                   §CTRMO
016900010604     C                   clear                   §CTRNO
017000010330     C*
017100010330     C                   DO        *HIVAL
017200010330     C*
017300010330     C                   READ      tivin00r                               70
017400010618     C*
017500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017600010618     C                   if        vinflg = '1'
017700010618     C                   eval      flgOk = '1'
017800010618     C                   endif
017900010618     C*
018000010330     C                   if        vindta > *blanks
018100010330     C                   add       1             rrnum
018200010330     C*
018300010601     C                   if        *in70 = *off and
018400010330     C                             (vinflg = *blanks
018500010330     C                              or vinflg = '0'
018600010330     C                              or vinflg = '2')
018700010330     C*
018800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018900010711     C                   if        vinflg = *blanks or vinflg = '0'
019000010711     C                   clear                   vinmsg
019100010711     C                   endif
019200070601     C*
019300070601     C                   exsr      inzvar
019400070601     C                   exsr      defcam
019500070601     C                   exsr      impvab
019600010601     C*
019700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019800010605     C                   eval      depfil = VABLNP
019900010601     C                   exsr      repfil
020000010601     C                   if        depfil = invfil
020100021025     C                   if        vlrpoi = 999
020200021025     C                   MOVE(P)   invfil        VABFGS
020300021025     C                   else
020400021025     C                   MOVE(P)   vlrpoi        VABFGS
020500021025     C                   endif
020600010601     C*
020700010601     C                   exsr      PREELA
020800010601     C*
020900010604     C* Ebbene...
021000010604     C*
021100010604     C  N31              ADD       1             §CTROK            7 0
021200010604     C   32              ADD       1             §CTRMO            7 0
021300010604     C   31              ADD       1             §CTRNO            7 0
021400070927     C*
021500070927     C                   exsr      wrivab                                       => scrivo VAB
021600010604     C*
021700020722     C                   endif
021800020722     C*
021900010604     C                   if        *in31 = *off and
022000010604     C                             *in32 = *off
022100010604     C                   eval      vinflg = '1'
022200010604     C                   else
022300010604     C                   eval      recko = vindta
022400010604     C                   except    rigadett
022500010604     C                   eval      vinflg = '2'
022600010604     C                   endif
022700010604     C                   endif
022800010604     C*
022900010330     C                   else
023000010330     C                   eval      vinflg = '1'
023100010330     C                   endif
023200010601     C*
023300010601     C  N70              update    tivin000
023400010330     C*
023500010330     C  N70              ENDdo
023600010601     C*
023700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023800010601     C                   if        cntNonEl = *zeros or
023900010601     C                             flgMulti = '0'
024000010330     C* Se non ci sono record con errori ...
024100010601     C                   if        §ctrno = 0 and
024200010604     C                             §ctrmo = 0 and
024300010601     C                             flgStato <> '2'
024400010330     C* ... restituisco esito OK.
024500010330     C                   eval      wrkesito = '0'
024600010330     C                   else
024700010330     C                   if        §ctrok > 0
024800010330     C                   eval      wrkesito = '1'
024900010330     C                   else
025000010615     C                   if        flgOk = '0'
025100010615     C                   eval      wrkesito = '2'
025200010615     C                   else
025300010615     C                   eval      wrkesito = '6'
025400010615     C                   endif
025500010330     C                   endif
025600010330     C                   endif
025700010601     C                   else
025800010601     C                   eval      wrkesito = '9'
025900010601     C                   endif
026000010330     C*
026100010330     C                   if        %open(tivin00r)
026200010330     C                   close     tivin00r
026300010330     C                   endif
026400050408     C                   if        %open(fivabwwr)
026500050408     C                   close     fivabwwr
026600010330     C                   endif
026700010601     C*
026800010601     C                   if        vlrpoi <> 999
026900010601     C                   eval      invfil = vlrpoi
027000010601     C                   endif
027100010330     C*
027200010330     C                   if        §ctrok > 0
027300010601     C                             and invfil > *zeros
027400010330     C                   exsr      invio
027500010330     C                   endif
027600010601     C*
027700010618     C                   if        flgGiro = '1'
027800010601     C                   exsr      endela
027900010618     C                   endif
028000010330     C*
028100010330     C                   ENDSR
028200010330     C***
028300070601
028400070601     C*----------------------------------------------------*
028500070601     C*  SCARICAMENTO BUFFER RECORDS VAB
028600070601     C*----------------------------------------------------*
028700070601     C     WRIVAB        BEGSR
028800070601     C*
028900070601     C  N31              WRITE     FIVAB000
029000070601     C*
029100070601     C                   ENDSR
029200010601
029300010601
029400010601
029500010330     C*----------------------------------------------------*
029600020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029700010330     C*----------------------------------------------------*
029800010330     C     INZVAR        BEGSR
029900010330     C*
030000020204     C                   Z-ADD     *zeros        Num5_0            5 0
030100020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
030200020322     C                   MOVEL     '0'           FlgCAS            1
030300010330     C*
030400010330     C                   ENDSR
030500010330     C*----------------------------------------------------*
030600020722     C*  IMPOSTAZIONE CAMPI COSTANTI
030700010330     C*----------------------------------------------------*
030800010330     C     DEFCAM        BEGSR
030900010330     C*
031000020204     C* Inizializzo il buffer del record da scrivere
031100050408     C                   CLEAR                   FIVAB000
031200020204     C* Imposto i valori di default...
031300070926     C                   EVAL      VABCCM = 2493851
031400070926     C                   EVAL      VABLNP = 249
031500070925     C                   EVAL      VABCTR = 000
031600070926     C                   EVAL      VABCTM = '7Q'
031700070208     C                   EVAL      VABCBO = '1'
031800071029     C                   EVAL      VABRMO = 'PHILIPS SPA'
031900071029     C                   EVAL      VABCMO = '20052'
032000020204     C* ... e poi verifico se sono stati passati come parametri
032100020204     C                   IF        vlrppt > *blanks
032200040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
032300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032400020204     C                   EXSR      CHKNUM
032500020204     C                   IF        PiInt=*on
032600020204     C                   Z-ADD     PiVal         VABCCM
032700020204     C                   ENDIF
032800040301     C                   ENDIF
032900040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
033000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
033100020204     C                   EXSR      CHKNUM
033200020204     C                   IF        PiInt=*on
033300020204     C                   Z-ADD     PiVal         VABLNP
033400020204     C                   ENDIF
033500040301     C                   ENDIF
033600040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
033700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033800020204     C                   EXSR      CHKNUM
033900020204     C                   IF        PiInt=*on
034000020204     C                   Z-ADD     PiVal         VABCTR
034100040301     C                   ENDIF
034200020204     C                   ENDIF
034300020204     C                   ENDIF
034400020204     C*
034500010330     C                   ENDSR
034600010607     C*----------------------------------------------------*
034700050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034800010607     C*----------------------------------------------------*
034900010607     C     IMPVAB        BEGSR
035000010607     C*
035100010607     C                   SETOFF                                       3132
035200010607     C*
035300010607     C* Reperimento campi ALFA
035400010607     C*
035500010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
035600070926     C                   EVAL      VABRSD=%trim(%subst(vindta:29:30))
035700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
035900020117     C* ==
036000070926     C                   EVAL      VABIND=%trim(%subst(vindta:59:30))
036100070926     C                   EVAL      VABLOD=%trim(%subst(vindta:89:30))
036200070926     C                   EVAL      VABPRD=%trim(%subst(vindta:119:2))
036300070926     C                   EVAL      VABRMA=%trim(%subst(vindta:243:20))
036400060510     C*
036500060510     C* Reperimento campi NUMERICI
036600060510     C                   MOVEL     DATCOR        VABAAS
036700060510     C                   MOVE      DATCOR        VABMGS
036800060710     C* NSP/RMN
036900070926     C                   EVAL      PiStr=%trim(%subst(vindta:2:10))
037000060510     C                   EXSR      CHKNUM
037100060510     C                   IF        PiInt=*on
037200060510     C                   Z-ADD     PiVal         VABNSP
037300060510     C                   Z-ADD     PiVal         VABRMN
037400060404     C                   ELSE
037500070502     C                   SETON                                        31
037600060510     C                   Z-ADD     *zeros        VABNSP
037700060510     C                   Z-ADD     1             VABRMN
037800060404     C                   EVAL      vinmsg = %trimr(vinmsg)
037900060510     C                             + ' ' + 'VABNSP VABRMN'
038000060404     C                   ENDIF
038100040420     C* CAD
038200070926     C                   EVAL      PiStr=%trim(%subst(vindta:121:5))
038300010607     C                   EXSR      CHKNUM
038400010607     C                   IF        PiInt=*on
038500020204     C                   Z-ADD     PiVal         Num5_0
038600020204     C                   MOVEL(p)  Num5_0        VABCAD
038700010607     C                   ELSE
038800010607     C                   SETON                                        32
038900020204     C                   EVAL      VABCAD = *zeros
039000010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039100020204     C                             + ' ' + 'VABCAD'
039200060316     C                   ENDIF
039300070502     C* Reperisco la provincia dal CAP e dalla località
039400070502     C                   IF        VABCAD <> *blanks AND
039500070502     C                             VABPRD  = *blanks
039600070502     C                   CLEAR                   TISI95DS
039700070502     C                   EVAL      I95TCN = '3'
039800070502     C                   Z-ADD     datcor        I95DAT
039900070502     C                   EVAL      I95CAP = VABCAD
040000070502     C                   EVAL      I95LOC = VABLOD
040100070502     C                   EVAL      I95NAR = VABNZD
040200070502     C                   CALL      'TISI95R'
040300070502     C                   PARM                    TISI95DS
040400070502     C                   EVAL      VABPRD = O95PRV
040500070502     C                   ENDIF
040600040420     C* NCL
040700070926     C                   EVAL      PiStr=%trim(%subst(vindta:136:5))
040800010607     C                   EXSR      CHKNUM
040900010607     C                   IF        PiInt=*on
041000070606     C                   Z-ADD     PiVal         VABNCL
041100010607     C                   ELSE
041200010607     C                   SETON                                        32
041300010607     C                   Z-ADD     *zeros        VABNCL
041400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
041500010607     C                             + ' ' + 'VABNCL'
041600010607     C                   ENDIF
041700040420     C* PKB
041800070926     C                   EVAL      PiStr=%trim(%subst(vindta:129:7))
041900070208     C                   EVAL      PiDecChr = '.'
042000010607     C                   EXSR      CHKNUM
042100010607     C                   IF        PiNum=*on
042200070926     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
042300070925     C                   Z-ADD(H)  PiVal         VABPKB
042400010607     C                   ELSE
042500010607     C                   SETON                                        32
042600010607     C                   Z-ADD     *zeros        VABPKB
042700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
042800010607     C                             + ' ' + 'VABPKB'
042900070926     C                   ENDIF
043000070926     C* CAS
043100070926     C                   IF        %subst(vindta:161:9) <> *zeros
043200070926     C                   EVAL      FlgCAS = '1'
043300070926     C                   EVAL      PiStr=%trim(%subst(vindta:161:9))
043400070926     C                   EXSR      CHKNUM
043500070926     C                   IF        PiNum=*on
043600070926     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
043700070926     C                   Z-ADD(H)  PiVal         VABCAS
043800071029     C                   EVAL      VABVCA = 'EUR'
043900071029     C                   EVAL      VABTIC = 'OM'
044000070926     C                   ELSE
044100070926     C                   SETON                                        32
044200070926     C                   Z-ADD     *zeros        VABCAS
044300070926     C                   EVAL      vinmsg = %trimr(vinmsg)
044400070926     C                             + ' ' + 'VABCAS'
044500070926     C                   ENDIF
044600070926     C                   ENDIF
044700020322     C*
044800020322     C* Considerazioni finali su CBO/CAS
044900050908     C                   IF        FlgCAS = '1'
045000040713     C                   IF        VABCBO = '1'
045100020322     C                   EVAL      VABCBO = '4'
045200040713     C                   ENDIF
045300040713     C                   IF        VABCBO = '2'
045400040713     C                   EVAL      VABCBO = '6'
045500040713     C                   ENDIF
045600020322     C                   ENDIF
045700020204     C*
045800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
045900020204     C                   EXSR      CHKIMPDIV
046000020204     C*
046100010607     C                   ENDSR
046200010607     C*----------------------------------------------------*
046300010601
046400020204
046500020204     C*----------------------------------------------------*
046600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
046700020204     C*----------------------------------------------------*
046800020204     C     CHKIMPDIV     BEGSR
046900020204     C*
047000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
047100020204     C                   Z-ADD     *zeros        wrkDec            9 9
047200020204     C*
047300020204     C* Come prima cosa effettuo considerazioni sulla divisa
047400020204     C                   IF        vabIAS > *zeros
047500020204     C                   IF        vabVAS <> 'EUR'
047600020204     C                   EVAL      vabVAS =  'ITL'
047700020204     C                   ENDIF
047800020204     C                   ENDIF
047900020204     C*
048000020204     C                   IF        vabCAS > *zeros
048100020204     C                   IF        vabVCA <> 'EUR'
048200020204     C                   EVAL      vabVCA =  'ITL'
048300020204     C                   ENDIF
048400020204     C                   ENDIF
048500020204     C*
048600020204     C                   IF        vabVMD > *zeros
048700020321     C                   IF        vabVAD <> 'EUR'
048800020204     C                   EVAL      vabVAD =  'ITL'
048900020204     C                   ENDIF
049000020204     C                   ENDIF
049100020204     C*
049200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
049300020204     C                   Z-ADD     vabIAS        wrkDec
049400020204     C                   IF        wrkDec > *zeros
049500020204     C                   IF        vabVAS = 'ITL'
049600020204     C                   EVAL      vabIAS = *zeros
049700020204     C                   ENDIF
049800020204     C                   ENDIF
049900020204     C*
050000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
050100020204     C                   Z-ADD     vabCAS        wrkDec
050200020204     C                   IF        wrkDec > *zeros
050300020204     C                   IF        vabVCA = 'ITL'
050400020204     C                   EVAL      vabCAS = *zeros
050500020204     C                   ENDIF
050600020204     C                   ENDIF
050700020204     C*
050800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
050900020204     C                   Z-ADD     vabVMD        wrkDec
051000020204     C                   IF        wrkDec > *zeros
051100020204     C                   IF        vabVAD = 'ITL'
051200020204     C                   EVAL      vabVMD = *zeros
051300020204     C                   ENDIF
051400020204     C                   ENDIF
051500020204     C*
051600020204     C                   ENDSR
051700020204     C***
051800020204
051900010330
052000010330
052100010330     C*----------------------------------------------------*
052200010330     C*  CONTROLLO NUMERICITA' CAMPI
052300010330     C*----------------------------------------------------*
052400010330     C     CHKNUM        BEGSR
052500010330     C*
052600010606     C                   IF        PiDecChr = *blanks
052700070502     C                   EVAL      PiDecChr = '.'
052800010606     C                   ENDIF
052900010606     C*
053000010606     C                   CALL(e)   'ISNUMERIC'
053100010330     C                   PARM                    PiStr            30
053200010606     C                   PARM                    PiDecChr          1
053300010330     C                   PARM      *ZEROS        PiVal            30 9
053400010330     C                   PARM      '0'           PiInt             1
053500010330     C                   PARM      '0'           PiNum             1
053600010330     C                   IF        %error
053700010606     C                   EVAL      PiNum=*off
053800010330     C                   ENDIF
053900010330     C*
054000010330     C                   ENDSR
054100010330     C***
054200010330
054300010601
054400010601
054500010601
054600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
054700010601     C     repfil        BEGSR
054800010601     C*
054900010601     C                   if        invfil = *zeros and
055000010601     C                             depfil > *zeros and
055100010629     C                             (vinflg = *blanks or
055200010629     C                              vinflg = *zeros)
055300010601     C
055400010601     C                   eval      invfil = depfil
055500010601     C                   endif
055600010601     C*
055700010601     C                   if        depfil <> invfil and
055800010601     C                             invfil > *zeros
055900010601     C                   eval      flgMulti = '1'
056000010601     C                   if        vinflg = *blanks
056100010601     C                   add       1             cntNonEl
056200010601     C                   endif
056300010601     C                   endif
056400010601     C*
056500010601     C                   if        vinflg = '2'
056600010601     C                   eval      flgStato = '2'
056700010601     C                   endif
056800010601     C*
056900010601     C                   ENDSR
057000010601     C***
057100010601
057200010601
057300010601
057400010330
057500010330
057600010330
057700990920      /TITLE Invio dei dati al punto operativo.
057800000613     C     invio         BEGSR
057900990920     C*
058000990920     C                   reset                   dscmz
058100010601     C                   move      invfil        cmzdst
058200990920     C                   eval      cmzfld = vlrfou
058300990920     C                   eval      cmzmbd = vlrhdl
058400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
058500000710     C                   if        prmfir = *blanks
058600050408     C                   eval      cmzfla = 'FIVAB00F'
058700050408     C                   eval      cmzmba = 'FIVAB00F'
058800000710     C                   else
058900000710     C                   eval      cmzfla = prmfir
059000000710     C                   eval      cmzmba = prmfir
059100000710     C                   endif
059200990920     C                   eval      cmznrr = *zeros
059300990920     C                   move      §ctrok        cmznrr
059400021018     C                   eval      cmzlba = vlrfl1
059500990920     C                   call(e)   'TIS711C'
059600990920     C                   parm                    dscmz
059700990921     C                   parm      *blanks       esito
059800990920     C                   if        %error
059900990920     C                             or cmzerr = '1'
060000990921     C                             or esito  = '1'
060100000710     C                   eval      wrkesito = '3'
060200990920     C                   endif
060300990920     C*
060400000613     C                   ENDSR
060500990910
060600010601
060700010601
060800010601
060900010601
061000010601      /TITLE Invio dei dati al punto operativo.
061100010601     C     opeini        BEGSR
061200010601     C*
061300010601     C* Inizializzo flag e contatori operativi
061400010601     C                   movel     '0'           flgGiro           1
061500010601     C                   movel     '0'           flgMulti          1
061600010601     C                   movel     '1'           flgStato          1
061700010615     C                   movel     '0'           flgOk             1
061800010601     C                   z-add     *zeros        cntNonEl         10 0
061900010601     C                   z-add     *zeros        depfil            3 0
062000010601     C                   z-add     *zeros        invfil            3 0
062100010601     C*
062200010601     C                   ENDSR
062300010601     C***
062400010601
062500010601
062600010601
062700010330
062800010330
062900000613     C     *inzsr        BEGSR
063000990910     C*
063100990910     C     *entry        plist
063200990920     C                   parm                    tivlrds
063300990921     C                   parm      wrkesito      esito
063400000724     C                   parm                    prmlit
063500000710     C                   parm                    prmfir
063600010330     C*
063700010330     C* CALCOLA LA DATA CORRENTE
063800010330     C                   time                    wn14             14 0
063900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
064000010330     C                   z-add     wn8           g08dat
064100010330     C                   z-add     *zeros        g08inv
064200010330     C                   movel     '0'           g08err
064300010330     C                   call      'XSRDA8'
064400010330     C                   parm                    wlbda8
064500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
064600000613     C*
064700000613     C                   ENDSR
064800000613     C***
064900990908
065000070926     Otitv1a7p  E            riepilogo         2
065100990915     O                                              'Upload via Internet'
065200990915     O                                           +1 'Traduzione TIVIN00R -'
065300050408     O                                           +1 'FIVABWWR'
065400990915     O                                           +1 'Testate bolle'
065500990915     O          E            riepilogo   2
065600990915     O                       wrkdata
065700990915     O                       wrkora              +1
065800990915     O                       procname            +1
065900990915     O          E            riepilogo   2
066000990915     O                                              'Cliente..................:'
066100010605     O                       VABCCM        z     +1
066200990915     O          E            riepilogo   2
066300990920     O                                              'Riferimento Strategi.....:'
066400990920     O                       vlrhdl              +1
066500990915     O          E            riepilogo   2
066600990915     O                                              'Giusti...................:'
066700971022     O                       §CTROK        2   +  1
066800990915     O          E            riepilogo   2
066900990915     O                                              'Sbagliati e corretti.....:'
067000971022     O                       §CTRMO        2   +  1
067100990915     O          E            riepilogo   2
067200990915     O                                              'Sbagliati e scartati.....:'
067300971022     O                       §CTRNO        2   +  1
067400000613
067500070926     Otitv1a7ps E            testdett          2
067600000613     O                                              'Upload via Internet'
067700000613     O                                           +1 'Traduzione TIVIN00R -'
067800050408     O                                           +1 'FIVABWWR'
067900000613     O                                           +1 'Testate bolle'
068000000616     O          E            testdett    3
068100000613     O                                           +2 'N° rec'
068200000613     O                                           +3 'Anteprima contenuto'
068300000616     O          E            rigadett    2
068400000613     O                       rrnum               +2
068500000621     O                       recko               +3
068600000616     O          E            findett     2
068700000613     O                       wrkdata
068800000613     O                       wrkora              +1
068900000613     O                       procname            +1
069000000616     O          E            findett     2
069100000613     O                                              'Cliente..................:'
069200010605     O                       VABCCM        z     +1
069300000616     O          E            findett     2
069400000613     O                                              'Riferimento Strategi.....:'
069500000613     O                       vlrhdl              +1
069600000616     O          E            findett     2
069700000613     O                                              'Giusti...................:'
069800000613     O                       §CTROK        2   +  1
069900000616     O          E            findett     2
070000000613     O                                              'Sbagliati e corretti.....:'
070100000613     O                       §CTRMO        2   +  1
070200000616     O          E            findett     2
070300000613     O                                              'Sbagliati e scartati.....:'
070400000613     O                       §CTRNO        2   +  1
070500000613** CMD - COMANDI CL
070600070926OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1A7P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
070700070926OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1A7PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
070800070926DLTOVR FILE(TITV1A7P TITV1A7PS) LVL(*)
070900010606
071000000613
