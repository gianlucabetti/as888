000100040316      /TITLE Upload via Internet: traduzione in FIVABWWR. (ADIDAS OLANDA - Cod. 0892729)
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600020913     FFIVABwwr  O    E             DISK    usropn
000700040316     Ftitva6p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900040316     Ftitva6ps  O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100990908
001200000512     D*------------
001300000512     D* COMANDI
001400000512     D*------------
001500011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600000512     D*------------
001700990920     D dscmz         e ds                  inz
001800990910     D psds           sds
001900990910     D  procname         *PROC
002000990920     D tivlrds       e ds                  extname(tivlr00f)
002100040318     D tisi95ds      e ds
002200990910     D esito           s              1
002300000724     D prmlit          s             10
002400000710     D prmfir          s             10
002500990921     D wrkesito        s                   like(esito)
002600990915     D wrkdata         s               d
002700990915     D wrkora          s               t
002800000613     D rrnum           s              6  0 INZ(*zeros)
002900000621     D recko           s            150    INZ(*blanks)
003000011123     D depcmd          s            150    INZ(*blanks)
003100010606     D*------------------
003200010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
003300010606     D*------------------
003400010606     D WLBDA8          DS                  INZ
003500010606     D  G08DAT                 1      8  0
003600010606     D  G08INV                 9     16  0
003700010606     D  G08ERR                17     17
003800010606     D  G08TGI                18     22  0
003900010606     D*
004000990908
004100990915     C                   time                    wrkdata
004200990915     C                   time                    wrkora
004300000913     C                   reset                   rrnum
004400990921     C                   reset                   esito
004500990921     C                   reset                   wrkesito
004600010530     C*
004700010530     C                   exsr      opeini
004800010530     C                   exsr      rwvab
004900010530     C*
005000010530     C                   seton                                        lr
005100010530
005200010530
005300010530
005400010530
005500010530     C*--------------------------------------------------------
005600010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005700010530     C*--------------------------------------------------------
005800010530     C     PREELA        BEGSR
005900000724     C*
006000000724     C* SE OCCORRE SPEDIRE IN FILIALE
006100010530     C                   if        invfil <> *zeros and
006200010530     C                             flgGiro = '0'
006300010530     C*
006400010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006500010530     C                   eval      flgGiro = '1'
006600000724     C*
006700000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006800000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006900010530     C     invfil        chain     azorg01l
007000000724     C                   if        %found
007100000616     C                   movel(p)  CMD(1)        depcmd
007200020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007300000616     C*
007400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007500011123     C                   Z-ADD     150           LENGH            15 5
007600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007700000616     C                   PARM                    depcmd
007800000616     C                   PARM                    LENGH
007900000724     C*
008000000724     C                   endif
008100000616     C*
008200000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008300000616     C                   movel(p)  CMD(2)        depcmd
008400000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008500000616     C*
008600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008700011123     C                   Z-ADD     150           LENGH            15 5
008800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008900000616     C                   PARM                    depcmd
009000000616     C                   PARM                    LENGH
009100000616     C*
009200040316     C                   if        not %open(titva6ps)
009300040316     C                   open      titva6ps
009400030407     C   50              except    testdett
009500000613     C                   endif
009600010530     C*
009700010530     C                   endif
009800010530     C*
009900010530     C                   ENDSR
010000010530     C***
010100010530
010200010530
010300010530
010400010530
010500010530
010600010530
010700010530     C*--------------------------------------------------------
010800010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010900010530     C*--------------------------------------------------------
011000010530     C     ENDELA        BEGSR
011100010530     C*
011200010530     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011300050201     C*
011400050201     C* Effettuo lancio TISI95 solo x chiusura
011500050201     C                   CLEAR                   TISI95DS
011600050201     C                   EVAL      I95TLA = 'C'
011700050201     C                   CALL      'TISI95R'
011800050201     C                   PARM                    TISI95DS
011900000613     C*
012000040316     C                   if        %open(titva6ps)
012100030407     C   50              except    findett
012200040316     C                   close     titva6ps
012300000613     C                   endif
012400000616     C*
012500000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012600011123     C                   Z-ADD     150           LENGH            15 5
012700010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012800000616     C                   PARM                    CMD(3)
012900000616     C                   PARM                    LENGH
013000010530     C*
013100010530     C                   ENDSR
013200010530     C***
013300000613
013400010530
013500010530
013600000613
013700000613     C*--------------------------------------------------------
013800000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013900000613     C*--------------------------------------------------------
014000000621     C     STPR          BEGSR
014100000613     C*
014200040316     C                   if        not %open(titva6p)
014300040316     C                   open      titva6p
014400990915     C                   endif
014500990915     C*
014600030407     C   50              except    riepilogo
014700990915     C*
014800040316     C                   if        %open(titva6p)
014900040316     C                   close     titva6p
015000990914     C                   endif
015100990910     C*
015200000613     C                   ENDSR
015300000613     C***
015400990908
015500010530
015600010530
015700010530
015800910830     C*--------------------------------------------------------
015900020913     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
016000910830     C*--------------------------------------------------------
016100941229     C     RWVAB         BEGSR
016200990910     C*
016300990914     C                   if        not %open(tivin00r)
016400990908     C                   open      tivin00r
016500990914     C                   endif
016600020913     C                   if        not %open(fivabwwr)
016700020913     C                   open      fivabwwr
016800990914     C                   endif
016900990910     C*
017000990910     C                   clear                   §CTROK
017100990910     C                   clear                   §CTRMO
017200990910     C                   clear                   §CTRNO
017300990910     C*
017400921023     C                   DO        *HIVAL
017500990913     C*
017600990915     C                   READ      tivin00r                               70
017700010618     C*
017800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017900010618     C                   if        vinflg = '1'
018000010618     C                   eval      flgOk = '1'
018100010618     C                   endif
018200010705     C*
018300010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
018400010705     C                   if        %subst(vindta:1:1) = '='
018500010705     C                   eval      %subst(vindta:1:1) = *blanks
018600010705     C                   endif
018700010705     C*
018800000905     C                   if        vindta > *blanks
018900000613     C                   add       1             rrnum
019000010601     C* ...elimino eventuali segni
019100010601     C     '+':' '       xlate     vindta        vindta
019200990913     C*
019300010601     C                   if        *in70 = *off and
019400990913     C                             (vinflg = *blanks
019500010711     C                              or vinflg = '0'
019600010711     C                              or vinflg = '2')
019700990913     C*
019800020913     C                   clear                   FIVAB000
019900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020000010711     C                   if        vinflg = *blanks or vinflg = '0'
020100010711     C                   clear                   vinmsg
020200010711     C                   endif
020300040316     C*
020400040316     C                   exsr      inzvar
020500040316     C                   exsr      defcam
020600040316     C                   exsr      impvab
020700010530     C*
020800010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020900010530     C                   eval      depfil = VABLNP
021000010530     C                   exsr      repfil
021100010601     C                   if        depfil = invfil
021200021014     C                   if        vlrpoi = 999
021300021010     C                   MOVE(P)   invfil        VABFGS
021400021014     C                   else
021500021014     C                   MOVE(P)   vlrpoi        VABFGS
021600021014     C                   endif
021700010530     C*
021800010530     C                   exsr      PREELA
021900930409     C*
022000010604     C  N31              ADD       1             §CTROK            7 0
022100010604     C   32              ADD       1             §CTRMO            7 0
022200010604     C   31              ADD       1             §CTRNO            7 0
022300020913     C  N31              WRITE     FIVAB000
022400990910     C*
022500010601     C                   if        *in31 = *off and
022600010601     C                             *in32 = *off
022700990910     C                   eval      vinflg = '1'
022800990910     C                   else
022900000621     C                   eval      recko = vindta
023000040316     C                   if        %open(titva6ps)
023100030407     C   50              except    rigadett
023200010612     C                   endif
023300990910     C                   eval      vinflg = '2'
023400990910     C                   endif
023500991022     C                   endif
023600010601     C*
023700010601     C                   endif
023800000905     C*
023900000905     C                   else
024000000905     C                   eval      vinflg = '1'
024100000905     C                   endif
024200000905     C*
024300000905     C  N70              update    tivin000
024400991022     C*
024500991022     C  N70              ENDdo
024600010530     C
024700010530     C*
024800010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024900010530     C                   if        cntNonEl = *zeros or
025000010530     C                             flgMulti = '0'
025100990910     C* Se non ci sono record con errori ...
025200010530     C                   if        §ctrno = 0 and
025300010601     C                             §ctrmo = 0 and
025400010530     C                             flgStato <> '2'
025500990910     C* ... restituisco esito OK.
025600990921     C                   eval      wrkesito = '0'
025700990910     C                   else
025800010601     C                   if        §ctrok > 0
025900990921     C                   eval      wrkesito = '1'
026000000710     C                   else
026100010615     C                   if        flgOk = '0'
026200010615     C                   eval      wrkesito = '2'
026300010615     C                   else
026400010615     C                   eval      wrkesito = '6'
026500010615     C                   endif
026600990910     C                   endif
026700000710     C                   endif
026800010530     C                   else
026900010530     C                   eval      wrkesito = '9'
027000010530     C                   endif
027100990910     C*
027200990914     C                   if        %open(tivin00r)
027300990908     C                   close     tivin00r
027400990914     C                   endif
027500020913     C                   if        %open(fivabwwr)
027600020913     C                   close     fivabwwr
027700990914     C                   endif
027800010530     C*
027900010530     C                   if        vlrpoi <> 999
028000010530     C                   eval      invfil = vlrpoi
028100010530     C                   endif
028200990910     C*
028300990920     C                   if        §ctrok > 0
028400010530     C                             and invfil > *zeros
028500000613     C                   exsr      invio
028600990920     C                   endif
028700010612     C*
028800010612     C                   if        flgGiro = '1'
028900010612     C                   exsr      endela
029000010612     C                   endif
029100990920     C*
029200910830     C                   ENDSR
029300000613     C***
029400040316
029500040316
029600040316
029700040316
029800040316
029900040316     C*----------------------------------------------------*
030000040316     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030100040316     C*----------------------------------------------------*
030200040316     C     INZVAR        BEGSR
030300040316     C*
030400040316     C                   Z-ADD     *zeros        Num5_0            5 0
030500040316     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
030600040316     C                   MOVEL     '0'           FlgCAS            1
030700040316     C*
030800040316     C                   ENDSR
030900040316     C*----------------------------------------------------*
031000040316     C*  IMPOSTAZIONE CAMPI COSTANTI
031100040316     C*----------------------------------------------------*
031200040316     C     DEFCAM        BEGSR
031300040316     C*
031400040316     C* Inizializzo il buffer del record da scrivere
031500040316     C                   CLEAR                   FIVAB000
031600040316     C*
031700040316     C* Valorizzo i campi costanti
031800040316     C                   eval      VABCCM = 0892729
031900040316     C                   eval      VABLNP = 089
032000040316     C                   eval      VABCTR = 000
032100040316     C                   eval      VABTSP = 'D'
032200040316     C                   eval      VABCTM = '7Q'
032300040316     C* ... e poi verifico se sono stati passati come parametri
032400040316     C                   IF        vlrppt > *blanks
032500040324     C                   IF        %trim(%subst(vlrppt:2:7)) <> *blanks
032600040324     C                   EVAL      PiStr=%trim(%subst(vlrppt:2:7))
032700040316     C                   EXSR      CHKNUM
032800040316     C                   IF        PiInt=*on
032900040316     C                   Z-ADD     PiVal         VABCCM
033000040316     C                   ENDIF
033100040316     C                   ENDIF
033200040324     C                   IF        %trim(%subst(vlrppt:9:3)) <> *blanks
033300040324     C                   EVAL      PiStr=%trim(%subst(vlrppt:9:3))
033400040316     C                   EXSR      CHKNUM
033500040316     C                   IF        PiInt=*on
033600040316     C                   Z-ADD     PiVal         VABLNP
033700040316     C                   ENDIF
033800040316     C                   ENDIF
033900040324     C                   IF        %trim(%subst(vlrppt:12:3)) <> *blanks
034000040324     C                   EVAL      PiStr=%trim(%subst(vlrppt:12:3))
034100040316     C                   EXSR      CHKNUM
034200040316     C                   IF        PiInt=*on
034300040316     C                   Z-ADD     PiVal         VABCTR
034400040316     C                   ENDIF
034500040316     C                   ENDIF
034600040316     C                   ENDIF
034700040316     C*
034800040316     C                   ENDSR
034900040316     C*----------------------------------------------------*
035000040316     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
035100040316     C*----------------------------------------------------*
035200040316     C     IMPVAB        BEGSR
035300040316     C*
035400040316     C                   SETOFF                                       3132
035500040316     C*
035600040316     C* Reperimento campi ALFA
035700040316     C*
035800040316     C* Considerazioni sulla ragione sociale del destinatario da indicare
035900040316     C                   EVAL      VABRSD=%trim(%subst(vindta:41:35))
036000040316     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
036100040316     C     '@':'A'       XLATE     VABRSD        VABRSD
036200040316     C* ==
036300040316     C                   EVAL      VABRD2=%trim(%subst(vindta:76:35))
036400040316     C                   EVAL      VABIND=%trim(%subst(vindta:111:35))
036500040316     C                   EVAL      VABLOD=%trim(%subst(vindta:155:35))
036600040316     C                   EVAL      VABPRD=%trim(%subst(vindta:190:2))
036700040316     C                   EVAL      VABRMA=%trim(%subst(vindta:296:16))
036800040316     C                   EVAL      VABCBO=%trim(%subst(vindta:35:2))
036900040316     C*
037000040316     C* Reperimento campi NUMERICI
037100040316     C                   MOVEL     DATCOR        VABAAS
037200040316     C                   MOVE      DATCOR        VABMGS
037300040316     C* NSP/RMN
037400040316     C                   EVAL      PiStr=%trim(%subst(vindta:296:16))
037500040316     C                   EXSR      CHKNUM
037600040316     C                   IF        PiInt=*on
037700040316     C                   Z-ADD     PiVal         VABNSP
037800040316     C                   Z-ADD     PiVal         VABRMN
037900040316     C                   ELSE
038000040316     C                   SETON                                        31
038100040316     C                   Z-ADD     *zeros        VABNSP
038200040316     C                   Z-ADD     *zeros        VABRMN
038300040316     C                   EVAL      vinmsg = %trimr(vinmsg)
038400040316     C                             + ' ' + 'VABNSP VABRMN'
038500040316     C                   ENDIF
038600040316     C* CAD
038700040316     C                   EVAL      PiStr=%trim(%subst(vindta:146:9))
038800040316     C                   EXSR      CHKNUM
038900040316     C                   IF        PiInt=*on
039000040316     C                   Z-ADD     PiVal         Num5_0
039100040316     C                   MOVEL(p)  Num5_0        VABCAD
039200040316     C                   ELSE
039300040316     C                   SETON                                        32
039400040316     C                   EVAL      vinmsg = %trimr(vinmsg)
039500040316     C                             + ' ' + 'VABCAD'
039600040316     C                   ENDIF
039700040318     C* Reperisco la provincia dal CAP e dalla località
039800040318     C                   IF        VABCAD <> *blanks
039900040318     C                   CLEAR                   TISI95DS
040000040318     C                   EVAL      I95TCN = '3'
040100040318     C                   Z-ADD     datcor        I95DAT
040200040318     C                   EVAL      I95CAP = VABCAD
040300040318     C                   EVAL      I95LOC = VABLOD
040400040318     C                   CALL      'TISI95R'
040500040318     C                   PARM                    TISI95DS
040600040318     C                   EVAL      VABPRD = O95PRV
040700040318     C                   ENDIF
040800040316     C* NCL
040900040316     C                   EVAL      PiStr=%trim(%subst(vindta:237:6))
041000040316     C                   EXSR      CHKNUM
041100040316     C                   IF        PiInt=*on
041200040316     C                   Z-ADD     PiVal         VABNCL
041300040316     C                   ELSE
041400040316     C                   SETON                                        32
041500040316     C                   Z-ADD     *zeros        VABNCL
041600040316     C                   EVAL      vinmsg = %trimr(vinmsg)
041700040316     C                             + ' ' + 'VABNCL'
041800040316     C                   ENDIF
041900040316     C* PKB
042000040316     C                   EVAL      PiStr=%trim(%subst(vindta:243:9))
042100040316     C                   EXSR      CHKNUM
042200040316     C                   IF        PiNum=*on
042300040317     C                   EVAL      VABPKB = PiVal/1000                          * gestisco il peso
042400040317     C                   ELSE                                                   * espresso in grammi
042500040316     C                   SETON                                        32
042600040316     C                   Z-ADD     *zeros        VABPKB
042700040316     C                   EVAL      vinmsg = %trimr(vinmsg)
042800040316     C                             + ' ' + 'VABPKB'
042900040316     C                   ENDIF
043000040316     C*
043100040316     C* Considerazioni finali su CBO/CAS
043200040316     C                   IF        FlgCAS = '1'
043300040316     C                   EVAL      VABCBO = '4'
043400040316     C                   ELSE
043500040316     C                   EVAL      VABCBO = '1'
043600040316     C                   ENDIF
043700040316     C*
043800040316     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043900040316     C                   EXSR      CHKIMPDIV
044000040316     C*
044100040316     C                   ENDSR
044200040316     C*----------------------------------------------------*
044300040316
044400040316
044500040316     C*----------------------------------------------------*
044600040316     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
044700040316     C*----------------------------------------------------*
044800040316     C     CHKIMPDIV     BEGSR
044900040316     C*
045000040316     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
045100040316     C                   Z-ADD     *zeros        wrkDec            9 9
045200040316     C*
045300040316     C* Come prima cosa effettuo considerazioni sulla divisa
045400040316     C                   IF        vabIAS > *zeros
045500040316     C                   IF        vabVAS <> 'EUR'
045600040316     C                   EVAL      vabVAS =  'ITL'
045700040316     C                   ENDIF
045800040316     C                   ENDIF
045900040316     C*
046000040316     C                   IF        vabCAS > *zeros
046100040316     C                   IF        vabVCA <> 'EUR'
046200040316     C                   EVAL      vabVCA =  'ITL'
046300040316     C                   ENDIF
046400040316     C                   ENDIF
046500040316     C*
046600040316     C                   IF        vabVMD > *zeros
046700040316     C                   IF        vabVAD <> 'EUR'
046800040316     C                   EVAL      vabVAD =  'ITL'
046900040316     C                   ENDIF
047000040316     C                   ENDIF
047100040316     C*
047200040316     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
047300040316     C                   Z-ADD     vabIAS        wrkDec
047400040316     C                   IF        wrkDec > *zeros
047500040316     C                   IF        vabVAS = 'ITL'
047600040316     C                   EVAL      vabIAS = *zeros
047700040316     C                   ENDIF
047800040316     C                   ENDIF
047900040316     C*
048000040316     C* Stabilisco se il contrasegno ha decimali valorizzati
048100040316     C                   Z-ADD     vabCAS        wrkDec
048200040316     C                   IF        wrkDec > *zeros
048300040316     C                   IF        vabVCA = 'ITL'
048400040316     C                   EVAL      vabCAS = *zeros
048500040316     C                   ENDIF
048600040316     C                   ENDIF
048700040316     C*
048800040316     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
048900040316     C                   Z-ADD     vabVMD        wrkDec
049000040316     C                   IF        wrkDec > *zeros
049100040316     C                   IF        vabVAD = 'ITL'
049200040316     C                   EVAL      vabVMD = *zeros
049300040316     C                   ENDIF
049400040316     C                   ENDIF
049500040316     C*
049600040316     C                   ENDSR
049700040316     C***
049800040316
049900040316
050000040316
050100040316     C*----------------------------------------------------*
050200040316     C*  CONTROLLO NUMERICITA' CAMPI
050300040316     C*----------------------------------------------------*
050400040316     C     CHKNUM        BEGSR
050500040316     C*
050600040316     C                   IF        PiDecChr = *blanks
050700040316     C                   EVAL      PiDecChr = ','
050800040316     C                   ENDIF
050900040316     C*
051000040316     C                   CALL(e)   'ISNUMERIC'
051100040316     C                   PARM                    PiStr            30
051200040316     C                   PARM                    PiDecChr          1
051300040316     C                   PARM      *ZEROS        PiVal            30 9
051400040316     C                   PARM      '0'           PiInt             1
051500040316     C                   PARM      '0'           PiNum             1
051600040316     C                   IF        %error
051700040316     C                   EVAL      PiNum=*off
051800040316     C                   ENDIF
051900040316     C*
052000040316     C                   ENDSR
052100040316     C***
052200990920
052300010530
052400010530
052500010530
052600010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
052700010530     C     repfil        BEGSR
052800010530     C*
052900010530     C                   if        invfil = *zeros and
053000010530     C                             depfil > *zeros and
053100010613     C                             (vinflg = *blanks or
053200010613     C                              vinflg = *zeros)
053300010530     C
053400010530     C                   eval      invfil = depfil
053500010530     C                   endif
053600010530     C*
053700010530     C                   if        depfil <> invfil and
053800010530     C                             invfil > *zeros
053900010530     C                   eval      flgMulti = '1'
054000010530     C                   if        vinflg = *blanks
054100010530     C                   add       1             cntNonEl
054200010530     C                   endif
054300010530     C                   endif
054400010530     C*
054500010530     C                   if        vinflg = '2'
054600010530     C                   eval      flgStato = '2'
054700010530     C                   endif
054800010530     C*
054900010530     C                   ENDSR
055000010530     C***
055100010530
055200010530
055300010530
055400010530
055500990920      /TITLE Invio dei dati al punto operativo.
055600000613     C     invio         BEGSR
055700990920     C*
055800990920     C                   reset                   dscmz
055900010530     C                   move      invfil        cmzdst
056000990920     C                   eval      cmzfld = vlrfou
056100990920     C                   eval      cmzmbd = vlrhdl
056200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
056300000710     C                   if        prmfir = *blanks
056400020913     C                   eval      cmzfla = 'FIVAB00F'
056500020913     C                   eval      cmzmba = 'FIVAB00F'
056600000710     C                   else
056700000710     C                   eval      cmzfla = prmfir
056800000710     C                   eval      cmzmba = prmfir
056900000710     C                   endif
057000990920     C                   eval      cmznrr = *zeros
057100990920     C                   move      §ctrok        cmznrr
057200021003     C                   eval      cmzlba = vlrfl1
057300990920     C                   call(e)   'TIS711C'
057400990920     C                   parm                    dscmz
057500990921     C                   parm      *blanks       esito
057600990920     C                   if        %error
057700990920     C                             or cmzerr = '1'
057800990921     C                             or esito  = '1'
057900000710     C                   eval      wrkesito = '3'
058000990920     C                   endif
058100990920     C*
058200000613     C                   ENDSR
058300000613     C***
058400010530
058500010530
058600010530
058700010530
058800010530      /TITLE Invio dei dati al punto operativo.
058900010530     C     opeini        BEGSR
059000010530     C*
059100010530     C* Inizializzo flag e contatori operativi
059200010530     C                   movel     '0'           flgGiro           1
059300010530     C                   movel     '0'           flgMulti          1
059400010530     C                   movel     '1'           flgStato          1
059500010615     C                   movel     '0'           flgOk             1
059600010530     C                   z-add     *zeros        cntNonEl         10 0
059700010530     C                   z-add     *zeros        depfil            3 0
059800010530     C                   z-add     *zeros        invfil            3 0
059900030407     C*
060000030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
060100030407     C                   if        %subst(vlrppt:1:1) = 'S'
060200030407     C                   seton                                        50
060300030407     C                   else
060400030407     C                   setoff                                       50
060500030407     C                   endif
060600010530     C*
060700010530     C                   ENDSR
060800010530     C***
060900010530
061000010530
061100010530
061200990910
061300000613     C     *inzsr        BEGSR
061400990910     C*
061500990910     C     *entry        plist
061600990920     C                   parm                    tivlrds
061700990921     C                   parm      wrkesito      esito
061800000724     C                   parm                    prmlit
061900000710     C                   parm                    prmfir
062000010606     C*
062100010606     C* CALCOLA LA DATA CORRENTE
062200010606     C                   time                    wn14             14 0
062300010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
062400010606     C                   z-add     wn8           g08dat
062500010606     C                   z-add     *zeros        g08inv
062600010606     C                   movel     '0'           g08err
062700010606     C                   call      'XSRDA8'
062800010606     C                   parm                    wlbda8
062900010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
063000000613     C*
063100000613     C                   ENDSR
063200000613     C***
063300990908
063400040316     Otitva6p   E            riepilogo         2
063500990915     O                                              'Upload via Internet'
063600990915     O                                           +1 'Traduzione TIVIN00R -'
063700020913     O                                           +1 'FIVABWWR'
063800990915     O                                           +1 'Testate bolle'
063900990915     O          E            riepilogo   2
064000990915     O                       wrkdata
064100990915     O                       wrkora              +1
064200990915     O                       procname            +1
064300990915     O          E            riepilogo   2
064400990915     O                                              'Cliente..................:'
064500990915     O                       VABCCM        z     +1
064600990915     O          E            riepilogo   2
064700990920     O                                              'Riferimento Strategi.....:'
064800990920     O                       vlrhdl              +1
064900990915     O          E            riepilogo   2
065000990915     O                                              'Giusti...................:'
065100971022     O                       §CTROK        2   +  1
065200990915     O          E            riepilogo   2
065300990915     O                                              'Sbagliati e corretti.....:'
065400971022     O                       §CTRMO        2   +  1
065500990915     O          E            riepilogo   2
065600990915     O                                              'Sbagliati e scartati.....:'
065700971022     O                       §CTRNO        2   +  1
065800000613
065900040316     Otitva6ps  E            testdett          2
066000000613     O                                              'Upload via Internet'
066100000613     O                                           +1 'Traduzione TIVIN00R -'
066200020913     O                                           +1 'FIVABWWR'
066300000613     O                                           +1 'Testate bolle'
066400000616     O          E            testdett    3
066500000613     O                                           +2 'N° rec'
066600000613     O                                           +3 'Anteprima contenuto'
066700000616     O          E            rigadett    2
066800000613     O                       rrnum               +2
066900000621     O                       recko               +3
067000000616     O          E            findett     2
067100000613     O                       wrkdata
067200000613     O                       wrkora              +1
067300000613     O                       procname            +1
067400000616     O          E            findett     2
067500000613     O                                              'Cliente..................:'
067600000613     O                       VABCCM        z     +1
067700000616     O          E            findett     2
067800000613     O                                              'Riferimento Strategi.....:'
067900000613     O                       vlrhdl              +1
068000000616     O          E            findett     2
068100000613     O                                              'Giusti...................:'
068200000613     O                       §CTROK        2   +  1
068300000616     O          E            findett     2
068400000613     O                                              'Sbagliati e corretti.....:'
068500000613     O                       §CTRMO        2   +  1
068600000616     O          E            findett     2
068700000613     O                                              'Sbagliati e scartati.....:'
068800000613     O                       §CTRNO        2   +  1
068900000512** CMD - COMANDI CL
069000040316OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVA6P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
069100040316OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVA6PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
069200040316DLTOVR FILE(TITVA6P TITVA6PS) LVL(*)
069300000512
069400000512
