000100031013      /TITLE Upload via Internet: traduzione in FIVABWWR. (x D.E.A. - Cod. 2021741)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700031013     Ftitv70p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900031013     Ftitv70ps  O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100000313     D*
001200000613     D*------------
001300000613     D* COMANDI
001400000613     D*------------
001500011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600010330     D*----------------------------------------------------
001700010330     D* DICHIARAZIOINE VARIABILI DI WRK
001800010330     D*----------------------------------------------------
001900031013     D tisi95ds      e ds
002000010330     D dscmz         e ds                  inz
002100010330     D psds           sds
002200010330     D  procname         *PROC
002300010330     D tivlrds       e ds                  extname(tivlr00f)
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
009600031013     C                   if        not %open(titv70ps)
009700031013     C                   open      titv70ps
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
011800050201     C*
011900050201     C* Effettuo lancio TISI95 solo x chiusura
012000050201     C                   CLEAR                   TISI95DS
012100050201     C                   EVAL      I95TLA = 'C'
012200050201     C                   CALL      'TISI95R'
012300050201     C                   PARM                    TISI95DS
012400000613     C*
012500031013     C                   if        %open(titv70ps)
012600000616     C                   except    findett
012700031013     C                   close     titv70ps
012800000613     C                   endif
012900000616     C*
013000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013100011113     C                   Z-ADD     150           LENGH            15 5
013200010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
013300000616     C                   PARM                    CMD(3)
013400000616     C                   PARM                    LENGH
013500000616     C*
013600010601     C                   ENDSR
013700010601     C***
013800010601
013900010601
014000010601
014100010601
014200000613
014300000613     C*--------------------------------------------------------
014400000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014500000613     C*--------------------------------------------------------
014600000621     C     STPR          BEGSR
014700000613     C*
014800031013     C                   if        not %open(titv70p)
014900031013     C                   open      titv70p
015000990915     C                   endif
015100990915     C*
015200990915     C                   except    riepilogo
015300990915     C*
015400031013     C                   if        %open(titv70p)
015500031013     C                   close     titv70p
015600990914     C                   endif
015700000613     C*
015800000613     C                   ENDSR
015900000613     C***
016000000613
016100010330
016200010601
016300010601
016400010601
016500010330     C*--------------------------------------------------------
016600021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
016700010330     C*--------------------------------------------------------
016800010605     C     RWVAB         BEGSR
016900010330     C*
017000010330     C                   if        not %open(tivin00r)
017100010330     C                   open      tivin00r
017200010330     C                   endif
017300021025     C                   if        not %open(fivabwwr)
017400021025     C                   open      fivabwwr
017500010330     C                   endif
017600010330     C*
017700010604     C                   clear                   §CTROK
017800010604     C                   clear                   §CTRMO
017900010604     C                   clear                   §CTRNO
018000010330     C*
018100010330     C                   DO        *HIVAL
018200010330     C*
018300010330     C                   READ      tivin00r                               70
018400010618     C*
018500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018600010618     C                   if        vinflg = '1'
018700010618     C                   eval      flgOk = '1'
018800010618     C                   endif
018900010618     C*
019000010330     C                   if        vindta > *blanks
019100010330     C                   add       1             rrnum
019200010330     C*
019300010601     C                   if        *in70 = *off and
019400010330     C                             (vinflg = *blanks
019500010330     C                              or vinflg = '0'
019600010330     C                              or vinflg = '2')
019700010330     C*
019800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019900010711     C                   if        vinflg = *blanks or vinflg = '0'
020000010711     C                   clear                   vinmsg
020100010711     C                   endif
020200010601     C*
020300010330     C                   exsr      inzvar
020400010330     C                   exsr      defcam
020500010605     C                   exsr      impvab
020600010601     C*
020700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020800010605     C                   eval      depfil = VABLNP
020900010601     C                   exsr      repfil
021000010601     C                   if        depfil = invfil
021100021025     C                   if        vlrpoi = 999
021200021025     C                   MOVE(P)   invfil        VABFGS
021300021025     C                   else
021400021025     C                   MOVE(P)   vlrpoi        VABFGS
021500021025     C                   endif
021600010601     C*
021700010601     C                   exsr      PREELA
021800010601     C*
021900010604     C* Ebbene...
022000010604     C*
022100010604     C  N31              ADD       1             §CTROK            7 0
022200010604     C   32              ADD       1             §CTRMO            7 0
022300010604     C   31              ADD       1             §CTRNO            7 0
022400021025     C  N31              WRITE     FIVAB000
022500010604     C*
022600010604     C                   if        *in31 = *off and
022700010604     C                             *in32 = *off
022800010604     C                   eval      vinflg = '1'
022900010604     C                   else
023000010604     C                   eval      recko = vindta
023100010604     C                   except    rigadett
023200010604     C                   eval      vinflg = '2'
023300010604     C                   endif
023400010604     C                   endif
023500010601     C*
023600010604     C                   endif
023700010604     C*
023800010330     C                   else
023900010330     C                   eval      vinflg = '1'
024000010330     C                   endif
024100010601     C*
024200010601     C  N70              update    tivin000
024300010330     C*
024400010330     C  N70              ENDdo
024500010601     C
024600010601     C*
024700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024800010601     C                   if        cntNonEl = *zeros or
024900010601     C                             flgMulti = '0'
025000010330     C* Se non ci sono record con errori ...
025100010601     C                   if        §ctrno = 0 and
025200010604     C                             §ctrmo = 0 and
025300010601     C                             flgStato <> '2'
025400010330     C* ... restituisco esito OK.
025500010330     C                   eval      wrkesito = '0'
025600010330     C                   else
025700010330     C                   if        §ctrok > 0
025800010330     C                   eval      wrkesito = '1'
025900010330     C                   else
026000010615     C                   if        flgOk = '0'
026100010615     C                   eval      wrkesito = '2'
026200010615     C                   else
026300010615     C                   eval      wrkesito = '6'
026400010615     C                   endif
026500010330     C                   endif
026600010330     C                   endif
026700010601     C                   else
026800010601     C                   eval      wrkesito = '9'
026900010601     C                   endif
027000010330     C*
027100010330     C                   if        %open(tivin00r)
027200010330     C                   close     tivin00r
027300010330     C                   endif
027400021025     C                   if        %open(fivabwwr)
027500021025     C                   close     fivabwwr
027600010330     C                   endif
027700010601     C*
027800010601     C                   if        vlrpoi <> 999
027900010601     C                   eval      invfil = vlrpoi
028000010601     C                   endif
028100010330     C*
028200010330     C                   if        §ctrok > 0
028300010601     C                             and invfil > *zeros
028400010330     C                   exsr      invio
028500010330     C                   endif
028600010601     C*
028700010618     C                   if        flgGiro = '1'
028800010601     C                   exsr      endela
028900010618     C                   endif
029000010330     C*
029100010330     C                   ENDSR
029200010330     C***
029300010601
029400010601
029500010601
029600010330     C*----------------------------------------------------*
029700010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029800010330     C*----------------------------------------------------*
029900010330     C     INZVAR        BEGSR
030000010330     C*
030100020204     C                   Z-ADD     *zeros        Num5_0            5 0
030200030606     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
030300030606     C                   MOVEL     '0'           FlgCAS            1
030400010330     C*
030500010330     C                   ENDSR
030600010330     C*----------------------------------------------------*
030700010330     C*  IMPOSTAZIONE CAMPI COSTANTI
030800010330     C*----------------------------------------------------*
030900010330     C     DEFCAM        BEGSR
031000010330     C*
031100020204     C* Inizializzo il buffer del record da scrivere
031200021025     C                   CLEAR                   FIVAB000
031300020204     C* Imposto i valori di default...
031400031013     C                   EVAL      VABCCM = 2021741
031500031013     C                   EVAL      VABLNP = 202
031600031013     C                   EVAL      VABCTR = 000
031700031013     C                   EVAL      VABTSP = 'C'
031800020204     C* ... e poi verifico se sono stati passati come parametri
031900020204     C                   IF        vlrppt > *blanks
032000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032100020204     C                   EXSR      CHKNUM
032200020204     C                   IF        PiInt=*on
032300020204     C                   Z-ADD     PiVal         VABCCM
032400020204     C                   ENDIF
032500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032600020204     C                   EXSR      CHKNUM
032700020204     C                   IF        PiInt=*on
032800020204     C                   Z-ADD     PiVal         VABLNP
032900020204     C                   ENDIF
033000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033100020204     C                   EXSR      CHKNUM
033200020204     C                   IF        PiInt=*on
033300020204     C                   Z-ADD     PiVal         VABCTR
033400020204     C                   ENDIF
033500020204     C                   ENDIF
033600020204     C*
033700010330     C                   ENDSR
033800010607     C*----------------------------------------------------*
033900021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034000010607     C*----------------------------------------------------*
034100010607     C     IMPVAB        BEGSR
034200010607     C*
034300010607     C                   SETOFF                                       3132
034400030515     C*
034500010607     C                   MOVEL     datcor        VABAAS
034600010607     C                   MOVE      datcor        VABMGS
034700010607     C*
034800010607     C* Reperimento campi ALFA
034900010607     C*
035000010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
035100031006     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
035200020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035300020117     C     '@':'A'       XLATE     VABRSD        VABRSD
035400020117     C* ==
035500031013     C                   EVAL      VABRD2=%trim(%subst(vindta:35:35))
035600031013     C                   EVAL      VABIND=%trim(%subst(vindta:71:35))
035700031013     C                   EVAL      VABLOD=%trim(%subst(vindta:116:35))
035800031013     C                   EVAL      VABNOT=%trim(%subst(vindta:200:35))
035900031013     C                   EVAL      VABNT2=%trim(%subst(vindta:239:35))
036000031013     C                   EVAL      VABRMA=%trim(%subst(vindta:163:15))
036100010607     C*
036200010607     C* Reperimento campi NUMERICI
036300020604     C*
036400030515     C* CAD
036500031013     C                   EVAL      PiStr=%trim(%subst(vindta:111:5))
036600010607     C                   EXSR      CHKNUM
036700010607     C                   IF        PiInt=*on
036800020204     C                   Z-ADD     PiVal         Num5_0
036900020204     C                   MOVEL(p)  Num5_0        VABCAD
037000010607     C                   ELSE
037100010607     C                   SETON                                        32
037200020204     C                   EVAL      VABCAD = *zeros
037300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
037400020204     C                             + ' ' + 'VABCAD'
037500010607     C                   ENDIF
037600031013     C* Reperisco la provincia dal CAP e dalla località
037700031013     C                   IF        VABCAD <> *blanks
037800031013     C                   CLEAR                   TISI95DS
037900031013     C                   EVAL      I95TCN = '3'
038000031013     C                   Z-ADD     datcor        I95DAT
038100031013     C                   EVAL      I95CAP = VABCAD
038200031013     C                   EVAL      I95LOC = VABLOD
038300031013     C                   CALL      'TISI95R'
038400031013     C                   PARM                    TISI95DS
038500031013     C                   EVAL      VABPRD = O95PRV
038600031013     C                   ENDIF
038700030515     C* RMN/NSP
038800031013     C                   EVAL      PiStr=%trim(%subst(vindta:163:15))
038900011023     C                   EXSR      CHKNUM
039000011023     C                   IF        PiInt=*on
039100030225     C                   Z-ADD     PiVal         VABRMN
039200031013     C                   MOVE(P)   VABRMN        VABNSP
039300011023     C                   ELSE
039400020205     C                   EVAL      VABRMN = 1
039500011023     C                   ENDIF
039600030515     C* NCL
039700031013     C                   EVAL      PiStr=%trim(%subst(vindta:151:5))
039800010607     C                   EXSR      CHKNUM
039900010607     C                   IF        PiInt=*on
040000010607     C                   Z-ADD     PiVal         VABNCL
040100010607     C                   ELSE
040200010607     C                   SETON                                        32
040300010607     C                   Z-ADD     *zeros        VABNCL
040400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040500010607     C                             + ' ' + 'VABNCL'
040600010607     C                   ENDIF
040700031013     C* PKB
040800031013     C                   EVAL      PiStr=%trim(%subst(vindta:156:7))
040900031013     C                   EXSR      CHKNUM
041000031017     C                   IF        PiNum=*on
041100031013     C                   Z-ADD     PiVal         VABPKB
041200031013     C                   ELSE
041300031013     C                   SETON                                        32
041400031013     C                   Z-ADD     *zeros        VABPKB
041500031013     C                   EVAL      vinmsg = %trimr(vinmsg)
041600031013     C                             + ' ' + 'VABPKB'
041700031013     C                   ENDIF
041800030929     C*
041900030929     C* Considerazioni finali su CBO/CAS
042000030929     C                   IF        FlgCAS = '1'
042100030929     C                   EVAL      VABCBO = '4'
042200030929     C                   ELSE
042300030929     C                   EVAL      VABCBO = '1'
042400030929     C                   ENDIF
042500020204     C*
042600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
042700020204     C                   EXSR      CHKIMPDIV
042800020204     C*
042900010607     C                   ENDSR
043000010607     C*----------------------------------------------------*
043100010601
043200020204
043300020204     C*----------------------------------------------------*
043400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
043500020204     C*----------------------------------------------------*
043600020204     C     CHKIMPDIV     BEGSR
043700020204     C*
043800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
043900020204     C                   Z-ADD     *zeros        wrkDec            9 9
044000020204     C*
044100020204     C* Come prima cosa effettuo considerazioni sulla divisa
044200020204     C                   IF        vabIAS > *zeros
044300020204     C                   IF        vabVAS <> 'EUR'
044400020204     C                   EVAL      vabVAS =  'ITL'
044500020204     C                   ENDIF
044600020204     C                   ENDIF
044700020204     C*
044800020204     C                   IF        vabCAS > *zeros
044900020204     C                   IF        vabVCA <> 'EUR'
045000020204     C                   EVAL      vabVCA =  'ITL'
045100020204     C                   ENDIF
045200020204     C                   ENDIF
045300020204     C*
045400020204     C                   IF        vabVMD > *zeros
045500020204     C                   IF        vabVAD <> 'EUR'
045600020204     C                   EVAL      vabVAD =  'ITL'
045700020204     C                   ENDIF
045800020204     C                   ENDIF
045900020204     C*
046000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
046100020204     C                   Z-ADD     vabIAS        wrkDec
046200020204     C                   IF        wrkDec > *zeros
046300020204     C                   IF        vabVAS = 'ITL'
046400020204     C                   EVAL      vabIAS = *zeros
046500020204     C                   ENDIF
046600020204     C                   ENDIF
046700020204     C*
046800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
046900020204     C                   Z-ADD     vabCAS        wrkDec
047000020204     C                   IF        wrkDec > *zeros
047100020204     C                   IF        vabVCA = 'ITL'
047200020204     C                   EVAL      vabCAS = *zeros
047300020204     C                   ENDIF
047400020204     C                   ENDIF
047500020204     C*
047600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
047700020204     C                   Z-ADD     vabVMD        wrkDec
047800020204     C                   IF        wrkDec > *zeros
047900020204     C                   IF        vabVAD = 'ITL'
048000020204     C                   EVAL      vabVMD = *zeros
048100020204     C                   ENDIF
048200020204     C                   ENDIF
048300020204     C*
048400020204     C                   ENDSR
048500020204     C***
048600020204
048700010330
048800010330
048900010330     C*----------------------------------------------------*
049000010330     C*  CONTROLLO NUMERICITA' CAMPI
049100010330     C*----------------------------------------------------*
049200010330     C     CHKNUM        BEGSR
049300010330     C*
049400010606     C                   IF        PiDecChr = *blanks
049500030606     C                   EVAL      PiDecChr = ','
049600010606     C                   ENDIF
049700010606     C*
049800010606     C                   CALL(e)   'ISNUMERIC'
049900010330     C                   PARM                    PiStr            30
050000010606     C                   PARM                    PiDecChr          1
050100010330     C                   PARM      *ZEROS        PiVal            30 9
050200010330     C                   PARM      '0'           PiInt             1
050300010330     C                   PARM      '0'           PiNum             1
050400010330     C                   IF        %error
050500010606     C                   EVAL      PiNum=*off
050600010330     C                   ENDIF
050700010330     C*
050800010330     C                   ENDSR
050900010330     C***
051000010330
051100010601
051200010601
051300010601
051400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
051500010601     C     repfil        BEGSR
051600010601     C*
051700010601     C                   if        invfil = *zeros and
051800010601     C                             depfil > *zeros and
051900010629     C                             (vinflg = *blanks or
052000010629     C                              vinflg = *zeros)
052100010601     C
052200010601     C                   eval      invfil = depfil
052300010601     C                   endif
052400010601     C*
052500010601     C                   if        depfil <> invfil and
052600010601     C                             invfil > *zeros
052700010601     C                   eval      flgMulti = '1'
052800010601     C                   if        vinflg = *blanks
052900010601     C                   add       1             cntNonEl
053000010601     C                   endif
053100010601     C                   endif
053200010601     C*
053300010601     C                   if        vinflg = '2'
053400010601     C                   eval      flgStato = '2'
053500010601     C                   endif
053600010601     C*
053700010601     C                   ENDSR
053800010601     C***
053900010601
054000010601
054100010601
054200010330
054300010330
054400010330
054500990920      /TITLE Invio dei dati al punto operativo.
054600000613     C     invio         BEGSR
054700990920     C*
054800990920     C                   reset                   dscmz
054900010601     C                   move      invfil        cmzdst
055000990920     C                   eval      cmzfld = vlrfou
055100990920     C                   eval      cmzmbd = vlrhdl
055200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
055300000710     C                   if        prmfir = *blanks
055400021025     C                   eval      cmzfla = 'FIVAB00F'
055500021025     C                   eval      cmzmba = 'FIVAB00F'
055600000710     C                   else
055700000710     C                   eval      cmzfla = prmfir
055800000710     C                   eval      cmzmba = prmfir
055900000710     C                   endif
056000990920     C                   eval      cmznrr = *zeros
056100990920     C                   move      §ctrok        cmznrr
056200021018     C                   eval      cmzlba = vlrfl1
056300990920     C                   call(e)   'TIS711C'
056400990920     C                   parm                    dscmz
056500990921     C                   parm      *blanks       esito
056600990920     C                   if        %error
056700990920     C                             or cmzerr = '1'
056800990921     C                             or esito  = '1'
056900000710     C                   eval      wrkesito = '3'
057000990920     C                   endif
057100990920     C*
057200000613     C                   ENDSR
057300990910
057400010601
057500010601
057600010601
057700010601
057800010601      /TITLE Invio dei dati al punto operativo.
057900010601     C     opeini        BEGSR
058000010601     C*
058100010601     C* Inizializzo flag e contatori operativi
058200010601     C                   movel     '0'           flgGiro           1
058300010601     C                   movel     '0'           flgMulti          1
058400010601     C                   movel     '1'           flgStato          1
058500010615     C                   movel     '0'           flgOk             1
058600010601     C                   z-add     *zeros        cntNonEl         10 0
058700010601     C                   z-add     *zeros        depfil            3 0
058800010601     C                   z-add     *zeros        invfil            3 0
058900010601     C*
059000010601     C                   ENDSR
059100010601     C***
059200010601
059300010601
059400010601
059500010330
059600010330
059700000613     C     *inzsr        BEGSR
059800990910     C*
059900990910     C     *entry        plist
060000990920     C                   parm                    tivlrds
060100990921     C                   parm      wrkesito      esito
060200000724     C                   parm                    prmlit
060300000710     C                   parm                    prmfir
060400010330     C*
060500010330     C* CALCOLA LA DATA CORRENTE
060600010330     C                   time                    wn14             14 0
060700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
060800010330     C                   z-add     wn8           g08dat
060900010330     C                   z-add     *zeros        g08inv
061000010330     C                   movel     '0'           g08err
061100010330     C                   call      'XSRDA8'
061200010330     C                   parm                    wlbda8
061300010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
061400000613     C*
061500000613     C                   ENDSR
061600000613     C***
061700990908
061800031013     Otitv70p   E            riepilogo         2
061900990915     O                                              'Upload via Internet'
062000990915     O                                           +1 'Traduzione TIVIN00R -'
062100021025     O                                           +1 'FIVABWWR'
062200990915     O                                           +1 'Testate bolle'
062300990915     O          E            riepilogo   2
062400990915     O                       wrkdata
062500990915     O                       wrkora              +1
062600990915     O                       procname            +1
062700990915     O          E            riepilogo   2
062800990915     O                                              'Cliente..................:'
062900010605     O                       VABCCM        z     +1
063000990915     O          E            riepilogo   2
063100990920     O                                              'Riferimento Strategi.....:'
063200990920     O                       vlrhdl              +1
063300990915     O          E            riepilogo   2
063400990915     O                                              'Giusti...................:'
063500971022     O                       §CTROK        2   +  1
063600990915     O          E            riepilogo   2
063700990915     O                                              'Sbagliati e corretti.....:'
063800971022     O                       §CTRMO        2   +  1
063900990915     O          E            riepilogo   2
064000990915     O                                              'Sbagliati e scartati.....:'
064100971022     O                       §CTRNO        2   +  1
064200000613
064300031013     Otitv70ps  E            testdett          2
064400000613     O                                              'Upload via Internet'
064500000613     O                                           +1 'Traduzione TIVIN00R -'
064600021025     O                                           +1 'FIVABWWR'
064700000613     O                                           +1 'Testate bolle'
064800000616     O          E            testdett    3
064900000613     O                                           +2 'N° rec'
065000000613     O                                           +3 'Anteprima contenuto'
065100000616     O          E            rigadett    2
065200000613     O                       rrnum               +2
065300000621     O                       recko               +3
065400000616     O          E            findett     2
065500000613     O                       wrkdata
065600000613     O                       wrkora              +1
065700000613     O                       procname            +1
065800000616     O          E            findett     2
065900000613     O                                              'Cliente..................:'
066000010605     O                       VABCCM        z     +1
066100000616     O          E            findett     2
066200000613     O                                              'Riferimento Strategi.....:'
066300000613     O                       vlrhdl              +1
066400000616     O          E            findett     2
066500000613     O                                              'Giusti...................:'
066600000613     O                       §CTROK        2   +  1
066700000616     O          E            findett     2
066800000613     O                                              'Sbagliati e corretti.....:'
066900000613     O                       §CTRMO        2   +  1
067000000616     O          E            findett     2
067100000613     O                                              'Sbagliati e scartati.....:'
067200000613     O                       §CTRNO        2   +  1
067300000613** CMD - COMANDI CL
067400031013OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV70P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
067500031013OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV70PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
067600031013DLTOVR FILE(TITV70P TITV70PS) LVL(*)
067700010606
067800000613
