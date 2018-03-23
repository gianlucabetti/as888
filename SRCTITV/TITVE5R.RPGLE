000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800040823     Ftitve5p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100040823     Ftitve5ps  O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
001400990908
001500000512     D*------------
001600000512     D* COMANDI
001700000512     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900000801     D*----------------------------------------------------
002000000801     D* DICHIARAZIOINE VARIABILI DI WRK
002100000801     D*----------------------------------------------------
002200990920     D dscmz         e ds                  inz
002300990910     D psds           sds
002400990910     D  procname         *PROC
002500990920     D tivlrds       e ds                  extname(tivlr00f)
002600040506     D tisi95ds      e ds
002700990910     D esito           s              1
002800000724     D prmlit          s             10
002900000710     D prmfir          s             10
003000990921     D wrkesito        s                   like(esito)
003100990915     D wrkdata         s               d
003200990915     D wrkora          s               t
003300000613     D rrnum           s              6  0 INZ(*zeros)
003400000621     D recko           s            150    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600010202     D parccm          s              8    INZ(*blanks)
003700010202     D parmbr          s             10    INZ(*blanks)
003800010202     D paropz          s              1    INZ(*blanks)
003900010202     D chkcall         s              1    INZ(*blanks)
004000040823     D depspe          s             15    INZ(*blanks)
004100040823     D curspe          s             15    INZ(*blanks)
004200000830
004300000830     D*------------------
004400000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004500000830     D*------------------
004600000830     D WLBDA8          DS                  INZ
004700000830     D  G08DAT                 1      8  0
004800000830     D  G08INV                 9     16  0
004900000830     D  G08ERR                17     17
005000000830     D  G08TGI                18     22  0
005100000830     D*
005200990908
005300010201
005400010201
005500990915     C                   time                    wrkdata
005600990915     C                   time                    wrkora
005700000913     C                   reset                   rrnum
005800990921     C                   reset                   esito
005900990921     C                   reset                   wrkesito
006000000724     C*
006100000724     C* SE OCCORRE SPEDIRE IN FILIALE
006200000724     C                   if        vlrpoi <> *zeros
006300000724     C*
006400000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006500000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006600000724     C     vlrpoi        chain     azorg01l
006700000724     C                   if        %found
006800000616     C                   movel(p)  CMD(1)        depcmd
006900020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007000000616     C*
007100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007200011113     C                   Z-ADD     150           LENGH            15 5
007300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007400000616     C                   PARM                    depcmd
007500000616     C                   PARM                    LENGH
007600000724     C*
007700000724     C                   endif
007800000724     C                   endif
007900000616     C*
008000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008100000616     C                   movel(p)  CMD(2)        depcmd
008200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008300000616     C*
008400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008500011113     C                   Z-ADD     150           LENGH            15 5
008600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008700000616     C                   PARM                    depcmd
008800000616     C                   PARM                    LENGH
008900000616     C*
009000040823     C                   if        not %open(titve5ps)
009100040823     C                   open      titve5ps
009200000616     C                   except    testdett
009300000613     C                   endif
009400000613     C*
009500040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
009600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009700000613     C*
009800010202     C* Effettuo la chiamata al CLLE preposto
009900040506     C                   call(e)   'TITVVTC'
010000010202     C                   parm                    parccm
010100010202     C                   parm                    parmbr
010200010202     C                   parm      '2'           paropz
010300050201     C*
010400050201     C* Effettuo lancio TISI95 solo x chiusura
010500050201     C                   CLEAR                   TISI95DS
010600050201     C                   EVAL      I95TLA = 'C'
010700050201     C                   CALL      'TISI95R'
010800050201     C                   PARM                    TISI95DS
010900010202     C*
011000040823     C                   if        %open(titve5ps)
011100000616     C                   except    findett
011200040823     C                   close     titve5ps
011300000613     C                   endif
011400000616     C*
011500000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011600011113     C                   Z-ADD     150           LENGH            15 5
011700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011800000616     C                   PARM                    CMD(3)
011900000616     C                   PARM                    LENGH
012000000616     C*
012100000801     C
012200010201     C                   seton                                        LR
012300000613
012400000613
012500000613     C*--------------------------------------------------------
012600000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
012700000613     C*--------------------------------------------------------
012800000621     C     STPR          BEGSR
012900000613     C*
013000040823     C                   if        not %open(titve5p)
013100040823     C                   open      titve5p
013200990915     C                   endif
013300990915     C*
013400990915     C                   except    riepilogo
013500990915     C*
013600040823     C                   if        %open(titve5p)
013700040823     C                   close     titve5p
013800990914     C                   endif
013900990910     C*
014000000613     C                   ENDSR
014100000613     C***
014200990908
014300000801
014400910830     C*--------------------------------------------------------
014500040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
014600910830     C*--------------------------------------------------------
014700040526     C     RWFILE        BEGSR
014800990910     C*
014900990914     C                   if        not %open(tivin00r)
015000990908     C                   open      tivin00r
015100990914     C                   endif
015200021113     C                   if        not %open(fivabwwr)
015300021113     C                   open      fivabwwr
015400990914     C                   endif
015500021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015600020305     C                   exsr      prevat
015700010201     C*
015800010202     C                   if        chkcall = '0'
015900010202     C*
016000021113     C                   if        not %open(fivatwwr)
016100021113     C                   open      fivatwwr
016200010201     C                   endif
016300990910     C*
016400010201     C                   clear                   §CTROKVB          5 0
016500020305     C                   clear                   §CTROKVT          5 0
016600000801     C                   clear                   §CTRMO            5 0
016700000801     C                   clear                   §CTRNO            5 0
016800990910     C*
016900921023     C                   DO        *HIVAL
017000990913     C*
017100990915     C                   READ      tivin00r                               70
017200040823     C                   if        vindta > *blanks                AND
017300040823     C                             %subst(vindta:1:7) <> 'barcode' AND
017400040823     C                             %subst(vindta:1:7) <> 'BARCODE'
017500000613     C                   add       1             rrnum
017600000801     C*
017700000801     C                   if        *in70 = *off
017800000801     C                             and
017900000801     C                             (vinflg = *blanks
018000000801     C                              or vinflg = '0'
018100000801     C                              or vinflg = '2')
018200000801     C*
018300000801     C                   clear                   vinmsg
018400000801     C                   eval      vinflg = '1'
018500010305     C*
018600040823     C* Verifico la rottura d spedizione
018700040823     C                   eval      curspe = %subst(vindta:19:15)
018800040823     C                   if        depspe <> curspe
018900040823     C* Se NN trattasi d 1° giro scarico il buffer del "VAB" precedente
019000040823     C                   if        depspe <> *blanks
019100040802     C                   exsr      wrivab
019200040823     C                   endif
019300040823     C* Imposto il "VAB" corrente
019400040823     C                   exsr      impvab
019500040823     C* Salvo la spedizione corrente
019600040823     C                   eval      depspe = curspe
019700040823     C                   endif
019800040823     C*
019900040823     C* Ad ogni record scrivo sempre e cmq il "VAT"
020000040823     C                   exsr      wrivat
020100040802     C*
020200010305     C                   endif
020300000905     C*
020400000905     C                   else
020500000905     C                   eval      vinflg = '1'
020600000905     C                   endif
020700000905     C*
020800000905     C  N70              update    tivin000
020900000905     C*
021000991022     C  N70              ENDdo
021100040823     C*
021200040823     C* Al termine della lettura del file scarico il buffer del "VAB" rimasto "in canna"
021300040823     C                   exsr      wrivab
021400010202     C*
021500010202     C                   endif
021600990910
021700990910     C* Se non ci sono record con errori ...
021800000710     C                   if        §ctrno = 0
021900990910     C* ... restituisco esito OK.
022000990921     C                   eval      wrkesito = '0'
022100990910     C                   else
022200010201     C                   if        §ctrokvb > 0
022300990921     C                   eval      wrkesito = '1'
022400000710     C                   else
022500000710     C                   eval      wrkesito = '2'
022600990910     C                   endif
022700000710     C                   endif
022800990910     C*
022900990914     C                   if        %open(tivin00r)
023000990908     C                   close     tivin00r
023100990914     C                   endif
023200021113     C                   if        %open(fivabwwr)
023300021113     C                   close     fivabwwr
023400990914     C                   endif
023500021113     C                   if        %open(fivatwwr)
023600021113     C                   close     fivatwwr
023700010201     C                   endif
023800990910     C*
023900010201     C                   if        §ctrokvb > 0
024000000724     C                             and vlrpoi <> *zeros
024100010202     C                   exsr      invio
024200990920     C                   endif
024300990920     C*
024400910830     C                   ENDSR
024500000613     C***
024600010305
024700010305     C*----------------------------------------------------*
024800020305     C*  SCARICAMENTO BUFFER RECORDS VAB
024900010305     C*----------------------------------------------------*
025000020305     C     WRIVAB        BEGSR
025100010305     C*
025200021113     C                   write     fivab000                                     => scarico il VAB
025300010305     C*
025400010305     C                   ENDSR
025500990920
025600000801     C*----------------------------------------------------*
025700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
025800000801     C*----------------------------------------------------*
025900010201     C     INZVAR        BEGSR
026000000801     C*
026100040802     C                   Z-ADD     *zeros        Num5_0            5 0
026200040802     C                   MOVEL     '0'           FlgCAS            1
026300000801     C*
026400000801     C                   ENDSR
026500000801     C*----------------------------------------------------*
026600000801     C*  IMPOSTAZIONE CAMPI COSTANTI
026700000801     C*----------------------------------------------------*
026800000801     C     DEFCAM        BEGSR
026900000801     C*
027000021113     C                   CLEAR                   FIVAB000
027100040802     C                   CLEAR                   FIVAT000
027200020619     C* Imposto i valori di default...
027300040823     C                   Z-ADD     0932292       VABCCM
027400040823     C                   Z-ADD     0932292       VATCCM
027500040823     C                   Z-ADD     093           VABLNP
027600040823     C                   Z-ADD     093           VATLNP
027700040714     C                   Z-ADD     000           VABCTR
027800040714     C                   MOVEL     '7Q'          VABCTM
027900040823     C                   MOVEL     '1'           VABCBO
028000020619     C* ... e poi verifico se sono stati passati come parametri
028100020619     C                   IF        vlrppt > *blanks
028200040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
028300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
028400020619     C                   EXSR      CHKNUM
028500020619     C                   IF        PiInt=*on
028600020619     C                   Z-ADD     PiVal         VABCCM
028700020619     C                   Z-ADD     PiVal         VATCCM
028800020619     C                   ENDIF
028900040506     C                   ENDIF
029000040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
029100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
029200020619     C                   EXSR      CHKNUM
029300020619     C                   IF        PiInt=*on
029400020619     C                   Z-ADD     PiVal         VABLNP
029500020619     C                   Z-ADD     PiVal         VATLNP
029600040506     C                   ENDIF
029700020619     C                   ENDIF
029800040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
029900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
030000020619     C                   EXSR      CHKNUM
030100020619     C                   IF        PiInt=*on
030200020619     C                   Z-ADD     PiVal         VABCTR
030300040506     C                   ENDIF
030400020619     C                   ENDIF
030500020619     C                   ENDIF
030600000801     C*
030700000801     C                   ENDSR
030800000801     C*----------------------------------------------------*
030900021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
031000000801     C*----------------------------------------------------*
031100040823     C     IMPVAB        BEGSR
031200040823     C*
031300020305     C                   EXSR      INZVAR
031400020305     C                   EXSR      DEFCAM
031500010305     C*
031600000801     C                   Z-ADD     *zeros        errore            1 0
031700000830     C                   MOVEL     datcor        VABAAS
031800020305     C                   MOVEL     datcor        VATAAS
031900040526     C                   MOVE      datcor        VABMGS
032000040823     C                   MOVE(P)   vlrpoi        VABFGS
032100040823     C                   MOVE(P)   vlrpoi        VATFGS
032200040823     C                   EVAL      VABRSD=%trim(%subst(vindta:46:35))
032300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
032400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
032500020117     C* ==
032600040823     C                   EVAL      VABIND=%trim(%subst(vindta:121:35))
032700040823     C                   EVAL      VABLOD=%trim(%subst(vindta:86:35))
032800040823     C                   EVAL      VABRMA=%trim(%subst(vindta:19:15))
032900040823     C                   EVAL      VABNOT=%trim(%subst(vindta:156:35))
033000040506     C* CAD
033100040823     C                   EVAL      PiStr=%trim(%subst(vindta:81:5))
033200010201     C                   EXSR      CHKNUM
033300010201     C                   IF        PiInt=*on
033400010201     C                   Z-ADD     PiVal         Num5_0
033500040506     C                   MOVEL(P)  Num5_0        VABCAD
033600010201     C                   ELSE
033700040506     C                   ADD       1             errore
033800010201     C                   EVAL      vinmsg = %trimr(vinmsg)
033900040506     C                             + ' ' + 'VABCAD'
034000010201     C                   ENDIF
034100040506     C* Reperisco la provincia dal CAP e dalla località
034200040526     C                   IF        VABCAD <> *blanks AND
034300040526     C                             VABPRD  = *blanks
034400040506     C                   CLEAR                   TISI95DS
034500040506     C                   EVAL      I95TCN = '3'
034600040506     C                   Z-ADD     datcor        I95DAT
034700040506     C                   EVAL      I95CAP = VABCAD
034800040506     C                   EVAL      I95LOC = VABLOD
034900040506     C                   CALL      'TISI95R'
035000040506     C                   PARM                    TISI95DS
035100040506     C                   EVAL      VABPRD = O95PRV
035200040506     C                   ENDIF
035300040506     C* NCL
035400040823     C                   EVAL      PiStr=%trim(%subst(vindta:34:5))
035500040506     C                   EXSR      CHKNUM
035600040506     C                   IF        PiInt=*on
035700040506     C                   Z-ADD     PiVal         VABNCL
035800040506     C                   ELSE
035900040506     C                   ADD       1             errore
036000040506     C                   EVAL      vinmsg = %trimr(vinmsg)
036100040506     C                             + ' ' + 'VABNCL'
036200040506     C                   ENDIF
036300040506     C* PKB
036400040823     C                   EVAL      PiStr=%trim(%subst(vindta:39:7))
036500010201     C                   EXSR      CHKNUM
036600010201     C                   IF        PiNum=*on
036700011115     C                   Z-ADD     PiVal         VABPKB
036800010201     C                   ELSE
036900010201     C                   ADD       1             errore
037000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
037100010201     C                             + ' ' + 'VABPKB'
037200010201     C                   ENDIF
037300040506     C* NSP/RMN
037400040823     C                   EVAL      PiStr=%trim(%subst(vindta:19:15))
037500010201     C                   EXSR      CHKNUM
037600010201     C                   IF        PiInt=*on
037700040506     C                   Z-ADD     PiVal         VABNSP
037800040506     C                   Z-ADD     PiVal         VATNSP
037900040506     C                   Z-ADD     PiVal         VABRMN
038000010201     C                   ELSE
038100010201     C                   ADD       1             errore
038200010201     C                   EVAL      vinmsg = %trimr(vinmsg)
038300040506     C                             + ' ' + 'VABNSP VATNSP VABRMN'
038400040526     C                   ENDIF
038500010205     C*
038600010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
038700040802     C                   IF        FlgCAS <> '0'
038800010205     C                   EVAL      VABCBO = '4'
038900010205     C                   ELSE
039000010205     C                   EVAL      VABCBO = '1'
039100010205     C                   ENDIF
039200020305     C*
039300011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
039400011113     C                   EXSR      CHKIMPDIV
039500010202     C*
039600000801     C* Ebbene...
039700000801     C                   ADD       1             §CTRMO
039800010201     C                   IF        errore <> *zeros
039900000801     C                   ADD       1             §CTRNO
040000000801     C                   EVAL      recko = vindta
040100000801     C                   EXCEPT    rigadett
040200000801     C                   EVAL      vinflg = '2'
040300000801     C                   ELSE
040400010201     C                   ADD       1             §CTROKVB
040500000801     C                   ENDIF
040600000801     C*
040700000801     C                   ENDSR
040800010201     C*----------------------------------------------------*
040900040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
041000010201     C*----------------------------------------------------*
041100020305     C     WRIVAT        BEGSR
041200040802     C*
041300040802     C* Valorizzo i campi chiave
041400040802     C                   EVAL      VATNSP = VABNSP
041500040802     C                   EVAL      VATTRC = 'E'
041600040802     C*
041700040823     C* Valorizzo il barcode
041800040823     C                   EVAL      VATNOT = %subst(vindta:1:8)
041900040802     C                   WRITE     FIVAT000
042000010201     C*
042100010201     C                   ENDSR
042200010202     C*----------------------------------------------------*
042300021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
042400010202     C*----------------------------------------------------*
042500020305     C     PREVAT        BEGSR
042600010202     C*
042700021113     C* Compongo il nome del membro da dare al FIVATWWR
042800010202     C                   eval      parmbr = vlrhdl
042900010202     C                   movel     'M'           parmbr
043000040823     C                   eval      parccm = '00932292'
043100010202     C                   eval      paropz = '1'
043200010202     C* Effettuo la chiamata al CLLE preposto
043300040506     C                   call(e)   'TITVVTC'
043400010202     C                   parm                    parccm
043500010202     C                   parm                    parmbr
043600010202     C                   parm                    paropz
043700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
043800010202     C                   if        %error
043900010202     C                   movel     '1'           chkcall
044000010202     C                   else
044100010202     C                   movel     '0'           chkcall
044200010202     C                   endif
044300010202     C*
044400010202     C                   ENDSR
044500000801     C*----------------------------------------------------*
044600000801     C*  CONTROLLO NUMERICITA' CAMPI
044700000801     C*----------------------------------------------------*
044800000801     C     CHKNUM        BEGSR
044900000801     C*
045000000801     C                   call(e)   'ISNUMERIC'
045100000801     C                   PARM                    PiStr            30
045200040714     C                   PARM      ','           PiDecChr          1
045300000801     C                   PARM      *ZEROS        PiVal            30 9
045400000801     C                   PARM      '0'           PiInt             1
045500000801     C                   PARM      '0'           PiNum             1
045600000801     C                   IF        %error
045700000801     C                   EVAL      PiInt=*off
045800000801     C                   ENDIF
045900000801     C*
046000000801     C                   ENDSR
046100000801     C***
046200000801
046300011113
046400011113     C*----------------------------------------------------*
046500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
046600011113     C*----------------------------------------------------*
046700011113     C     CHKIMPDIV     BEGSR
046800011113     C*
046900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
047000011113     C                   Z-ADD     *zeros        wrkDec            9 9
047100011113     C*
047200011113     C* Come prima cosa effettuo considerazioni sulla divisa
047300011113     C                   IF        vabIAS > *zeros
047400011113     C                   IF        vabVAS <> 'EUR'
047500011113     C                   EVAL      vabVAS =  'ITL'
047600011113     C                   ENDIF
047700011113     C                   ENDIF
047800011113     C*
047900011113     C                   IF        vabCAS > *zeros
048000011113     C                   IF        vabVCA <> 'EUR'
048100011113     C                   EVAL      vabVCA =  'ITL'
048200011113     C                   ENDIF
048300011113     C                   ENDIF
048400011113     C*
048500011113     C                   IF        vabVMD > *zeros
048600020305     C                   IF        vabVAD <> 'EUR'
048700011113     C                   EVAL      vabVAD =  'ITL'
048800011113     C                   ENDIF
048900011113     C                   ENDIF
049000011113     C*
049100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
049200011113     C                   Z-ADD     vabIAS        wrkDec
049300011113     C                   IF        wrkDec > *zeros
049400011113     C                   IF        vabVAS = 'ITL'
049500011113     C                   EVAL      vabIAS = *zeros
049600011113     C                   ENDIF
049700011113     C                   ENDIF
049800011113     C*
049900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
050000011113     C                   Z-ADD     vabCAS        wrkDec
050100011113     C                   IF        wrkDec > *zeros
050200011113     C                   IF        vabVCA = 'ITL'
050300011113     C                   EVAL      vabCAS = *zeros
050400011113     C                   ENDIF
050500011113     C                   ENDIF
050600011113     C*
050700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
050800011113     C                   Z-ADD     vabVMD        wrkDec
050900011113     C                   IF        wrkDec > *zeros
051000011113     C                   IF        vabVAD = 'ITL'
051100011113     C                   EVAL      vabVMD = *zeros
051200011113     C                   ENDIF
051300011113     C                   ENDIF
051400011113     C*
051500011113     C                   ENDSR
051600011113     C***
051700011113
051800011113
051900000801
052000000801
052100990920      /TITLE Invio dei dati al punto operativo.
052200010202     C     invio         BEGSR
052300990920     C*
052400021113     C* 1° invio FIVAT
052500010201     C                   reset                   dscmz
052600010201     C                   move      vlrpoi        cmzdst
052700021113     C                   eval      cmzfld = 'FIVATWWR'
052800010201     C                   eval      cmzmbd = vlrhdl
052900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
053000021009     C***                if        prmfir = *blanks
053100021113     C                   eval      cmzfla = 'FIVAT00F'
053200021113     C                   eval      cmzmba = 'FIVAT00F'
053300021009     C***                else
053400021009     C***                eval      cmzfla = prmfir
053500021009     C***                eval      cmzmba = prmfir
053600021009     C***                endif
053700010201     C                   eval      cmznrr = *zeros
053800020305     C                   move      §ctrokvt      cmznrr
053900021018     C                   eval      cmzlba = vlrfl1
054000010201     C                   call(e)   'TIS711C'
054100010201     C                   parm                    dscmz
054200010201     C                   parm      *blanks       esito
054300010205     C                   if        %error
054400010205     C                             or cmzerr = '1'
054500010205     C                             or esito  = '1'
054600010205     C                   eval      wrkesito = '3'
054700010205     C                   else
054800010201     C*
054900021113     C* 2° invio FIVAB
055000010201     C                   reset                   dscmz
055100010201     C                   move      vlrpoi        cmzdst
055200010201     C                   eval      cmzfld = vlrfou
055300010201     C                   eval      cmzmbd = vlrhdl
055400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055500021009     C***                if        prmfir = *blanks
055600021113     C                   eval      cmzfla = 'FIVAB00F'
055700021113     C                   eval      cmzmba = 'FIVAB00F'
055800021009     C***                else
055900021009     C***                eval      cmzfla = prmfir
056000021009     C***                eval      cmzmba = prmfir
056100021009     C***                endif
056200010201     C                   eval      cmznrr = *zeros
056300010201     C                   move      §ctrokvb      cmznrr
056400021018     C                   eval      cmzlba = vlrfl1
056500010201     C                   call(e)   'TIS711C'
056600010201     C                   parm                    dscmz
056700010201     C                   parm      *blanks       esito
056800010201     C                   if        %error
056900010201     C                             or cmzerr = '1'
057000010201     C                             or esito  = '1'
057100010201     C                   eval      wrkesito = '3'
057200010201     C                   endif
057300010205     C                   endif
057400990920     C*
057500000613     C                   ENDSR
057600000613     C***
057700070411
057800070411     C     *pssr         BEGSR
057900070411     C*
058000070411     C                   if        %open(tivin00r)
058100070411     C                   close     tivin00r
058200070411     C                   endif
058300070411     C                   if        %open(fivabwwr)
058400070411     C                   close     fivabwwr
058500070411     C                   endif
058600070411     C                   if        %open(fivatwwr)
058700070411     C                   close     fivatwwr
058800070411     C                   endif
058900070411     C*
059000070411     C* Effettuo la chiamata al CLLE preposto
059100070411     C                   call(e)   'TITVVTC'
059200070411     C                   parm                    parccm
059300070411     C                   parm                    parmbr
059400070411     C                   parm      '2'           paropz
059500070411     C*
059600070411     C                   eval      wrkesito = '2'
059700070411     C*
059800070411     C                   seton                                        LR
059900070411     C*
060000070411     C                   ENDSR     '*CANCL'
060100070411     C***
060200070411
060300990910
060400000613     C     *inzsr        BEGSR
060500990910     C*
060600990910     C     *entry        plist
060700990920     C                   parm                    tivlrds
060800990921     C                   parm      wrkesito      esito
060900000724     C                   parm                    prmlit
061000000710     C                   parm                    prmfir
061100000613     C*
061200000830     C* CALCOLA LA DATA CORRENTE
061300000830     C                   time                    wn14             14 0
061400000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
061500000830     C                   z-add     wn8           g08dat
061600000830     C                   z-add     *zeros        g08inv
061700000830     C                   movel     '0'           g08err
061800000830     C                   call      'XSRDA8'
061900000830     C                   parm                    wlbda8
062000000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
062100000830     C*
062200000613     C                   ENDSR
062300000613     C***
062400990908
062500040823     Otitve5p   E            riepilogo         2
062600990915     O                                              'Upload via Internet'
062700990915     O                                           +1 'Traduzione TIVIN00R -'
062800021113     O                                           +1 'FIVABWWR/FIVATWWR'
062900010201     O                                           +1 'Report testate bolle'
063000990915     O          E            riepilogo   2
063100990915     O                       wrkdata
063200990915     O                       wrkora              +1
063300990915     O                       procname            +1
063400990915     O          E            riepilogo   2
063500990915     O                                              'Cliente..................:'
063600990915     O                       VABCCM        z     +1
063700990915     O          E            riepilogo   2
063800990920     O                                              'Riferimento Strategi.....:'
063900990920     O                       vlrhdl              +1
064000990915     O          E            riepilogo   2
064100990915     O                                              'Giusti...................:'
064200010201     O                       §CTROKVB      2   +  1
064300990915     O          E            riepilogo   2
064400990915     O                                              'Sbagliati e corretti.....:'
064500971022     O                       §CTRMO        2   +  1
064600990915     O          E            riepilogo   2
064700990915     O                                              'Sbagliati e scartati.....:'
064800971022     O                       §CTRNO        2   +  1
064900000613
065000040823     Otitve5ps  E            testdett          2
065100000613     O                                              'Upload via Internet'
065200000613     O                                           +1 'Traduzione TIVIN00R -'
065300021113     O                                           +1 'FIVABWWR/FIVATWWR'
065400010201     O                                           +1 'Report testate bolle'
065500000616     O          E            testdett    3
065600000613     O                                           +2 'N° rec'
065700000613     O                                           +3 'Anteprima contenuto'
065800000616     O          E            rigadett    2
065900000613     O                       rrnum               +2
066000000621     O                       recko               +3
066100000616     O          E            findett     2
066200000613     O                       wrkdata
066300000613     O                       wrkora              +1
066400000613     O                       procname            +1
066500000616     O          E            findett     2
066600000613     O                                              'Cliente..................:'
066700000613     O                       VABCCM        z     +1
066800000616     O          E            findett     2
066900000613     O                                              'Riferimento Strategi.....:'
067000000613     O                       vlrhdl              +1
067100000616     O          E            findett     2
067200000613     O                                              'Giusti...................:'
067300010201     O                       §CTROKVB      2   +  1
067400000616     O          E            findett     2
067500000613     O                                              'Sbagliati e corretti.....:'
067600000613     O                       §CTRMO        2   +  1
067700000616     O          E            findett     2
067800000613     O                                              'Sbagliati e scartati.....:'
067900000613     O                       §CTRNO        2   +  1
068000000512** CMD - COMANDI CL
068100040823OVRPRTF FILE(TITVE5P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
068200040823OVRPRTF FILE(TITVE5PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
068300040823DLTOVR FILE(TITVE5P TITVE5PS) LVL(*)
068400000512
068500000512
