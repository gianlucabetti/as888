000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021113     FFIVABwwr  O    E             DISK    usropn
000600021113     FFIVATwwr  O    E             DISK    usropn
000700990908
000800000801     D*----------------------------------------------------
000900000801     D* DICHIARAZIOINE VARIABILI DI WRK
001000000801     D*----------------------------------------------------
001100990920     D dscmz         e ds                  inz
001200990910     D psds           sds
001300990910     D  procname         *PROC
001400990920     D tivlrds       e ds                  extname(tivlr00f)
001500060223     D tisi95ds      e ds
001600990910     D esito           s              1
001700000724     D prmlit          s             10
001800000710     D prmfir          s             10
001900990921     D wrkesito        s                   like(esito)
002000000613     D rrnum           s              6  0 INZ(*zeros)
002100010202     D parccm          s              8    INZ(*blanks)
002200010202     D parmbr          s             10    INZ(*blanks)
002300010202     D paropz          s              1    INZ(*blanks)
002400010202     D chkcall         s              1    INZ(*blanks)
002500060223     D depspe          s              6    INZ(*blanks)
002600060223     D curspe          s              6    INZ(*blanks)
002700060223     D cntspe          s              7  0 INZ(*zeros)
002800000830
002900000830     D*------------------
003000000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003100000830     D*------------------
003200000830     D WLBDA8          DS                  INZ
003300000830     D  G08DAT                 1      8  0
003400000830     D  G08INV                 9     16  0
003500000830     D  G08ERR                17     17
003600000830     D  G08TGI                18     22  0
003700000830     D*
003800990908
003900010201
004000010201
004100000913     C                   reset                   rrnum
004200990921     C                   reset                   esito
004300990921     C                   reset                   wrkesito
004400000613     C*
004500040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004600000613     C*
004700010202     C* Effettuo la chiamata al CLLE preposto
004800040506     C                   call(e)   'TITVVTC'
004900010202     C                   parm                    parccm
005000010202     C                   parm                    parmbr
005100010202     C                   parm      '2'           paropz
005200050201     C*
005300050201     C* Effettuo lancio TISI95 solo x chiusura
005400050201     C                   CLEAR                   TISI95DS
005500050201     C                   EVAL      I95TLA = 'C'
005600050201     C                   CALL      'TISI95R'
005700050201     C                   PARM                    TISI95DS
005800000616     C*
005900000801     C
006000010201     C                   seton                                        LR
006100990908
006200000801
006300910830     C*--------------------------------------------------------
006400040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
006500910830     C*--------------------------------------------------------
006600040526     C     RWFILE        BEGSR
006700990910     C*
006800990914     C                   if        not %open(tivin00r)
006900990908     C                   open      tivin00r
007000990914     C                   endif
007100021113     C                   if        not %open(fivabwwr)
007200021113     C                   open      fivabwwr
007300990914     C                   endif
007400021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007500020305     C                   exsr      prevat
007600010201     C*
007700010202     C                   if        chkcall = '0'
007800010202     C*
007900021113     C                   if        not %open(fivatwwr)
008000021113     C                   open      fivatwwr
008100010201     C                   endif
008200990910     C*
008300010201     C                   clear                   §CTROKVB          5 0
008400020305     C                   clear                   §CTROKVT          5 0
008500000801     C                   clear                   §CTRMO            5 0
008600000801     C                   clear                   §CTRNO            5 0
008700990910     C*
008800921023     C                   DO        *HIVAL
008900990913     C*
009000990915     C                   READ      tivin00r                               70
009100050627     C                   if        vindta > *blanks
009200000613     C                   add       1             rrnum
009300000801     C*
009400000801     C                   if        *in70 = *off
009500000801     C                             and
009600000801     C                             (vinflg = *blanks
009700000801     C                              or vinflg = '0'
009800000801     C                              or vinflg = '2')
009900000801     C*
010000000801     C                   clear                   vinmsg
010100000801     C                   eval      vinflg = '1'
010200010305     C*
010300050628     C* Determino il numero di Spedizione e a rottura eseguo operazioni
010400060223     C                   eval      PiStr=%trim(%subst(vindta:33:6))
010500050628     C                   movel(p)  PiStr         curspe
010600050628     C                   add       1             cntspe
010700050628     C*
010800050628     C                   if        cntspe = 1
010900050628     C* A rottura inizializzo campi di totalizzazione
011000050628     C                   clear                   fivab000
011100050628     C*
011200050628     C                   exsr      impvab                                       => carico VAB
011300050628     C                   exsr      exevat                                       => carico VAT
011400050628     C                   eval      depspe = curspe
011500050628     C                   else
011600060330     C                   if        curspe <> depspe
011700040802     C                   exsr      wrivab
011800060330     C                   EXSR      INZVAR
011900050628     C* A rottura inizializzo campi di totalizzazione
012000050628     C                   clear                   fivab000
012100050628     C*
012200050628     C                   exsr      impvab                                       => carico VAB
012300050628     C                   exsr      exevat                                       => carico VAT
012400050628     C                   eval      depspe = curspe
012500050628     C                   else
012600050628     C                   exsr      impvab                                       => carico VAB
012700050628     C                   exsr      exevat                                       => carico VAT
012800050628     C                   endif
012900050628     C                   endif
013000000905     C*
013100000905     C                   else
013200000905     C                   eval      vinflg = '1'
013300050628     C                   endif
013400000905     C                   endif
013500000905     C*
013600000905     C  N70              update    tivin000
013700000905     C*
013800991022     C  N70              ENDdo
013900040823     C*
014000040823     C* Al termine della lettura del file scarico il buffer del "VAB" rimasto "in canna"
014100040823     C                   exsr      wrivab
014200010202     C*
014300010202     C                   endif
014400990910
014500990910     C* Se non ci sono record con errori ...
014600000710     C                   if        §ctrno = 0
014700990910     C* ... restituisco esito OK.
014800990921     C                   eval      wrkesito = '0'
014900990910     C                   else
015000010201     C                   if        §ctrokvb > 0
015100990921     C                   eval      wrkesito = '1'
015200000710     C                   else
015300000710     C                   eval      wrkesito = '2'
015400990910     C                   endif
015500000710     C                   endif
015600990910     C*
015700990914     C                   if        %open(tivin00r)
015800990908     C                   close     tivin00r
015900990914     C                   endif
016000021113     C                   if        %open(fivabwwr)
016100021113     C                   close     fivabwwr
016200990914     C                   endif
016300021113     C                   if        %open(fivatwwr)
016400021113     C                   close     fivatwwr
016500010201     C                   endif
016600990910     C*
016700010201     C                   if        §ctrokvb > 0
016800000724     C                             and vlrpoi <> *zeros
016900010202     C                   exsr      invio
017000990920     C                   endif
017100990920     C*
017200910830     C                   ENDSR
017300000613     C***
017400010305
017500010305     C*----------------------------------------------------*
017600020305     C*  SCARICAMENTO BUFFER RECORDS VAB
017700010305     C*----------------------------------------------------*
017800020305     C     WRIVAB        BEGSR
017900060225     C*
018000060225     C* Imposto il peso totale della spedizione così come da sommatoria peso singoli colli
018100060225     C                   EVAL(H)   vabpkb = wPKB
018200060225     C*
018300060225     C* Se trattasi d spedizione multi-collo inizializzo il riferimento mittente alfa
018400060225     C                   IF        VABNCL > 1
018500060225     C                   CLEAR                   VABRMA
018600060225     C                   ENDIF
018700010305     C*
018800060225     C* Quindi scarico il buffer del file d testata
018900021113     C                   write     fivab000                                     => scarico il VAB
019000010305     C*
019100010305     C                   ENDSR
019200990920
019300000801     C*----------------------------------------------------*
019400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019500000801     C*----------------------------------------------------*
019600010201     C     INZVAR        BEGSR
019700000801     C*
019800040802     C                   Z-ADD     *zeros        Num5_0            5 0
019900060225     C                   Z-ADD     *zeros        wPKB              8 2
020000040802     C                   MOVEL     '0'           FlgCAS            1
020100000801     C*
020200000801     C                   ENDSR
020300000801     C*----------------------------------------------------*
020400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
020500000801     C*----------------------------------------------------*
020600000801     C     DEFCAM        BEGSR
020700000801     C*
020800020619     C* Imposto i valori di default...
020900060223     C                   Z-ADD     0053666       VABCCM
021000060223     C                   Z-ADD     0053666       VATCCM
021100060223     C                   Z-ADD     005           VABLNP
021200060223     C                   Z-ADD     005           VATLNP
021300040714     C                   Z-ADD     000           VABCTR
021400040714     C                   MOVEL     '7Q'          VABCTM
021500040823     C                   MOVEL     '1'           VABCBO
021600020619     C* ... e poi verifico se sono stati passati come parametri
021700020619     C                   IF        vlrppt > *blanks
021800040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
021900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
022000020619     C                   EXSR      CHKNUM
022100020619     C                   IF        PiInt=*on
022200020619     C                   Z-ADD     PiVal         VABCCM
022300020619     C                   Z-ADD     PiVal         VATCCM
022400020619     C                   ENDIF
022500040506     C                   ENDIF
022600040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
022700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022800020619     C                   EXSR      CHKNUM
022900020619     C                   IF        PiInt=*on
023000020619     C                   Z-ADD     PiVal         VABLNP
023100020619     C                   Z-ADD     PiVal         VATLNP
023200040506     C                   ENDIF
023300020619     C                   ENDIF
023400040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
023500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
023600020619     C                   EXSR      CHKNUM
023700020619     C                   IF        PiInt=*on
023800020619     C                   Z-ADD     PiVal         VABCTR
023900040506     C                   ENDIF
024000020619     C                   ENDIF
024100020619     C                   ENDIF
024200000801     C*
024300000801     C                   ENDSR
024400000801     C*----------------------------------------------------*
024500021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
024600000801     C*----------------------------------------------------*
024700040823     C     IMPVAB        BEGSR
024800040823     C*
024900020305     C                   EXSR      DEFCAM
025000010305     C*
025100000801     C                   Z-ADD     *zeros        errore            1 0
025200000830     C                   MOVEL     datcor        VABAAS
025300020305     C                   MOVEL     datcor        VATAAS
025400040526     C                   MOVE      datcor        VABMGS
025500040823     C                   MOVE(P)   vlrpoi        VABFGS
025600040823     C                   MOVE(P)   vlrpoi        VATFGS
025700050628     C*
025800060223     C                   EVAL      VABRSD=%trim(%subst(vindta:165:35))
025900020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026000020117     C     '@':'A'       XLATE     VABRSD        VABRSD
026100020117     C* ==
026200060223     C                   EVAL      VABRD2=%trim(%subst(vindta:165+35:61-35))
026300060223     C                   EVAL      VABIND=%trim(%subst(vindta:226:61))
026400060223     C                   EVAL      VABLOD=%trim(%subst(vindta:287:25))
026500060223     C                   EVAL      VABRMA=%trim(%subst(vindta:48:12))
026600060223     C                   EVAL      VABRMO=%trim(%subst(vindta:3:25))
026700170927     C*
026800170927     C* Reperisco l'email del destinatario
026900170928     C***                MOVEL     *blanks       wEMAIL           70
027000170928     C***                EVAL      wEMAIL = %trim(%subst(vindta:629:70))
027100170928     C***                EVAL      VATTRC='I'
027200170928     C***                EVAL      VATNOT = %subst(wEMAIL:1:35)
027300170928     C***                exsr      wrivat                                       => scarico VAT
027400170928     C***                EVAL      VATTRC='J'
027500170928     C***                EVAL      VATNOT = %subst(wEMAIL:36:35)
027600170928     C***                exsr      wrivat                                       => scarico VAT
027700170927     C*
027800170927     C* Reperisco il telefono del destinatario
027900170928     C***                EVAL      VATTRC='S'
028000170928     C***                EVAL      VATNOT = %trim(%subst(vindta:699:20))
028100170928     C***                exsr      wrivat                                       => scarico VAT
028200090114     C*
028300090114     C* Forzature particolari x codice cliente mittente e linea partenza
028400090114     C                   IF        %subst(vindta:485:3) = '115'
028500090114     C                   EVAL      VABCCM=0053666
028600090114     C                   EVAL      VATCCM=0053666
028700090114     C                   EVAL      VABLNP=005
028800090114     C                   EVAL      VATLNP=005
028900090114     C                   ENDIF
029000090114     C                   IF        %subst(vindta:485:3) = '119'
029100090114     C                   EVAL      VABCCM=2494149
029200090114     C                   EVAL      VATCCM=2494149
029300090114     C                   EVAL      VABLNP=249
029400090114     C                   EVAL      VATLNP=249
029500090114     C                   ENDIF
029600050628     C* CAP
029700060223     C                   EVAL      PiStr=%trim(%subst(vindta:312:5))
029800010201     C                   EXSR      CHKNUM
029900010201     C                   IF        PiInt=*on
030000010201     C                   Z-ADD     PiVal         Num5_0
030100040506     C                   MOVEL(P)  Num5_0        VABCAD
030200010201     C                   ELSE
030300040506     C                   ADD       1             errore
030400010201     C                   EVAL      vinmsg = %trimr(vinmsg)
030500040506     C                             + ' ' + 'VABCAD'
030600010201     C                   ENDIF
030700040506     C* Reperisco la provincia dal CAP e dalla località
030800040526     C                   IF        VABCAD <> *blanks AND
030900040526     C                             VABPRD  = *blanks
031000040506     C                   CLEAR                   TISI95DS
031100040506     C                   EVAL      I95TCN = '3'
031200040506     C                   Z-ADD     datcor        I95DAT
031300040506     C                   EVAL      I95CAP = VABCAD
031400040506     C                   EVAL      I95LOC = VABLOD
031500050627     C                   EVAL      I95NAR = VABNZD
031600040506     C                   CALL      'TISI95R'
031700040506     C                   PARM                    TISI95DS
031800040506     C                   EVAL      VABPRD = O95PRV
031900040506     C                   ENDIF
032000040506     C* NCL
032100050627     C                   EVAL      VABNCL = VABNCL + 1
032200040506     C* PKB
032300060223     C                   EVAL      PiStr=%trim(%subst(vindta:60:10))
032400010201     C                   EXSR      CHKNUM
032500010201     C                   IF        PiNum=*on
032600050628     C                   EVAL(h)   Pival = Pival/100                            * gestisco 2 decim.
032700060225     C                   ADD       PiVal         wPKB
032800010201     C                   ELSE
032900010201     C                   ADD       1             errore
033000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
033100010201     C                             + ' ' + 'VABPKB'
033200010201     C                   ENDIF
033300040506     C* NSP/RMN
033400060223     C                   EVAL      PiStr=%trim(%subst(vindta:33:6))
033500050928     C                   EXSR      CHKNUM
033600050928     C                   IF        PiInt=*on
033700050928     C                   Z-ADD     PiVal         VABNSP
033800050928     C                   Z-ADD     PiVal         VATNSP
033900050928     C                   Z-ADD     PiVal         VABRMN
034000050928     C                   ELSE
034100050928     C                   ADD       1             errore
034200050928     C                   EVAL      vinmsg = %trimr(vinmsg)
034300050928     C                             + ' ' + 'VABNSP VATNSP VABRMN'
034400050928     C                   ENDIF
034500010205     C*
034600010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
034700040802     C                   IF        FlgCAS <> '0'
034800010205     C                   EVAL      VABCBO = '4'
034900010205     C                   ELSE
035000010205     C                   EVAL      VABCBO = '1'
035100010205     C                   ENDIF
035200020305     C*
035300011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
035400011113     C                   EXSR      CHKIMPDIV
035500010202     C*
035600000801     C* Ebbene...
035700000801     C                   ADD       1             §CTRMO
035800010201     C                   IF        errore <> *zeros
035900000801     C                   ADD       1             §CTRNO
036000000801     C                   EVAL      vinflg = '2'
036100000801     C                   ELSE
036200010201     C                   ADD       1             §CTROKVB
036300000801     C                   ENDIF
036400000801     C*
036500000801     C                   ENDSR
036600050628     C*----------------------------------------------------*
036700050628     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
036800050628     C*----------------------------------------------------*
036900050628     C     EXEVAT        BEGSR
037000050628     C*
037100050628     C                   EXSR      DEFCAM
037200050628     C*
037300050628     C* Valorizzo il barcode
037400050628     C                   EVAL      VATTRC='E'
037500060223     C                   EVAL      VATNOT = %trim(%subst(vindta:48:12))
037600050628     C                   exsr      wrivat                                       => scarico VAT
037700050628     C*
037800050628     C                   ENDSR
037900010201     C*----------------------------------------------------*
038000040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
038100010201     C*----------------------------------------------------*
038200020305     C     WRIVAT        BEGSR
038300050628     C*
038400060223     C* Scrivo solo se valorizzato qualcosa
038500060223     C                   IF        VATNOT <> *blanks
038600040802     C                   WRITE     FIVAT000
038700060223     C                   ENDIF
038800010201     C*
038900010201     C                   ENDSR
039000010202     C*----------------------------------------------------*
039100021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
039200010202     C*----------------------------------------------------*
039300020305     C     PREVAT        BEGSR
039400010202     C*
039500021113     C* Compongo il nome del membro da dare al FIVATWWR
039600010202     C                   eval      parmbr = vlrhdl
039700010202     C                   movel     'M'           parmbr
039800050627     C                   eval      parccm = %subst(vlrKSC:2:7)
039900010202     C                   eval      paropz = '1'
040000010202     C* Effettuo la chiamata al CLLE preposto
040100040506     C                   call(e)   'TITVVTC'
040200010202     C                   parm                    parccm
040300010202     C                   parm                    parmbr
040400010202     C                   parm                    paropz
040500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
040600010202     C                   if        %error
040700010202     C                   movel     '1'           chkcall
040800010202     C                   else
040900010202     C                   movel     '0'           chkcall
041000010202     C                   endif
041100010202     C*
041200010202     C                   ENDSR
041300000801     C*----------------------------------------------------*
041400000801     C*  CONTROLLO NUMERICITA' CAMPI
041500000801     C*----------------------------------------------------*
041600000801     C     CHKNUM        BEGSR
041700000801     C*
041800000801     C                   call(e)   'ISNUMERIC'
041900000801     C                   PARM                    PiStr            30
042000050627     C                   PARM      ','           PiDecChr          1
042100000801     C                   PARM      *ZEROS        PiVal            30 9
042200000801     C                   PARM      '0'           PiInt             1
042300000801     C                   PARM      '0'           PiNum             1
042400000801     C                   IF        %error
042500000801     C                   EVAL      PiInt=*off
042600000801     C                   ENDIF
042700000801     C*
042800000801     C                   ENDSR
042900000801     C***
043000000801
043100011113
043200011113     C*----------------------------------------------------*
043300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
043400011113     C*----------------------------------------------------*
043500011113     C     CHKIMPDIV     BEGSR
043600011113     C*
043700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
043800011113     C                   Z-ADD     *zeros        wrkDec            9 9
043900011113     C*
044000011113     C* Come prima cosa effettuo considerazioni sulla divisa
044100011113     C                   IF        vabIAS > *zeros
044200011113     C                   IF        vabVAS <> 'EUR'
044300011113     C                   EVAL      vabVAS =  'ITL'
044400011113     C                   ENDIF
044500011113     C                   ENDIF
044600011113     C*
044700011113     C                   IF        vabCAS > *zeros
044800011113     C                   IF        vabVCA <> 'EUR'
044900011113     C                   EVAL      vabVCA =  'ITL'
045000011113     C                   ENDIF
045100011113     C                   ENDIF
045200011113     C*
045300011113     C                   IF        vabVMD > *zeros
045400020305     C                   IF        vabVAD <> 'EUR'
045500011113     C                   EVAL      vabVAD =  'ITL'
045600011113     C                   ENDIF
045700011113     C                   ENDIF
045800011113     C*
045900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
046000011113     C                   Z-ADD     vabIAS        wrkDec
046100011113     C                   IF        wrkDec > *zeros
046200011113     C                   IF        vabVAS = 'ITL'
046300011113     C                   EVAL      vabIAS = *zeros
046400011113     C                   ENDIF
046500011113     C                   ENDIF
046600011113     C*
046700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
046800011113     C                   Z-ADD     vabCAS        wrkDec
046900011113     C                   IF        wrkDec > *zeros
047000011113     C                   IF        vabVCA = 'ITL'
047100011113     C                   EVAL      vabCAS = *zeros
047200011113     C                   ENDIF
047300011113     C                   ENDIF
047400011113     C*
047500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
047600011113     C                   Z-ADD     vabVMD        wrkDec
047700011113     C                   IF        wrkDec > *zeros
047800011113     C                   IF        vabVAD = 'ITL'
047900011113     C                   EVAL      vabVMD = *zeros
048000011113     C                   ENDIF
048100011113     C                   ENDIF
048200011113     C*
048300011113     C                   ENDSR
048400011113     C***
048500011113
048600011113
048700000801
048800000801
048900990920      /TITLE Invio dei dati al punto operativo.
049000010202     C     invio         BEGSR
049100990920     C*
049200021113     C* 1° invio FIVAT
049300010201     C                   reset                   dscmz
049400010201     C                   move      vlrpoi        cmzdst
049500021113     C                   eval      cmzfld = 'FIVATWWR'
049600010201     C                   eval      cmzmbd = vlrhdl
049700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
049800021009     C***                if        prmfir = *blanks
049900021113     C                   eval      cmzfla = 'FIVAT00F'
050000021113     C                   eval      cmzmba = 'FIVAT00F'
050100021009     C***                else
050200021009     C***                eval      cmzfla = prmfir
050300021009     C***                eval      cmzmba = prmfir
050400021009     C***                endif
050500010201     C                   eval      cmznrr = *zeros
050600020305     C                   move      §ctrokvt      cmznrr
050700021018     C                   eval      cmzlba = vlrfl1
050800010201     C                   call(e)   'TIS711C'
050900010201     C                   parm                    dscmz
051000010201     C                   parm      *blanks       esito
051100010205     C                   if        %error
051200010205     C                             or cmzerr = '1'
051300010205     C                             or esito  = '1'
051400010205     C                   eval      wrkesito = '3'
051500010205     C                   else
051600010201     C*
051700021113     C* 2° invio FIVAB
051800010201     C                   reset                   dscmz
051900010201     C                   move      vlrpoi        cmzdst
052000010201     C                   eval      cmzfld = vlrfou
052100010201     C                   eval      cmzmbd = vlrhdl
052200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
052300021009     C***                if        prmfir = *blanks
052400021113     C                   eval      cmzfla = 'FIVAB00F'
052500021113     C                   eval      cmzmba = 'FIVAB00F'
052600021009     C***                else
052700021009     C***                eval      cmzfla = prmfir
052800021009     C***                eval      cmzmba = prmfir
052900021009     C***                endif
053000010201     C                   eval      cmznrr = *zeros
053100010201     C                   move      §ctrokvb      cmznrr
053200021018     C                   eval      cmzlba = vlrfl1
053300010201     C                   call(e)   'TIS711C'
053400010201     C                   parm                    dscmz
053500010201     C                   parm      *blanks       esito
053600010201     C                   if        %error
053700010201     C                             or cmzerr = '1'
053800010201     C                             or esito  = '1'
053900010201     C                   eval      wrkesito = '3'
054000010201     C                   endif
054100010205     C                   endif
054200990920     C*
054300000613     C                   ENDSR
054400000613     C***
054500070411
054600070411     C     *pssr         BEGSR
054700070411     C*
054800070411     C                   if        %open(tivin00r)
054900070411     C                   close     tivin00r
055000070411     C                   endif
055100070411     C                   if        %open(fivabwwr)
055200070411     C                   close     fivabwwr
055300070411     C                   endif
055400070411     C                   if        %open(fivatwwr)
055500070411     C                   close     fivatwwr
055600070411     C                   endif
055700070411     C*
055800070411     C* Effettuo la chiamata al CLLE preposto
055900070411     C                   call(e)   'TITVVTC'
056000070411     C                   parm                    parccm
056100070411     C                   parm                    parmbr
056200070411     C                   parm      '2'           paropz
056300070411     C*
056400070411     C                   eval      wrkesito = '2'
056500070411     C*
056600070411     C                   seton                                        LR
056700070411     C*
056800070411     C                   ENDSR     '*CANCL'
056900070411     C***
057000070411
057100990910
057200000613     C     *inzsr        BEGSR
057300990910     C*
057400990910     C     *entry        plist
057500990920     C                   parm                    tivlrds
057600990921     C                   parm      wrkesito      esito
057700000724     C                   parm                    prmlit
057800000710     C                   parm                    prmfir
057900000613     C*
058000000830     C* CALCOLA LA DATA CORRENTE
058100000830     C                   time                    wn14             14 0
058200000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
058300000830     C                   z-add     wn8           g08dat
058400000830     C                   z-add     *zeros        g08inv
058500000830     C                   movel     '0'           g08err
058600000830     C                   call      'XSRDA8'
058700000830     C                   parm                    wlbda8
058800000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
058900000830     C*
059000000613     C                   ENDSR
059100000613     C***
