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
001500990910     D esito           s              1
001600000724     D prmlit          s             10
001700000710     D prmfir          s             10
001800990921     D wrkesito        s                   like(esito)
001900000613     D rrnum           s              6  0 INZ(*zeros)
002000010202     D parccm          s              8    INZ(*blanks)
002100010202     D parmbr          s             10    INZ(*blanks)
002200010202     D paropz          s              1    INZ(*blanks)
002300010202     D chkcall         s              1    INZ(*blanks)
002400000830
002500000830     D*------------------
002600000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
002700000830     D*------------------
002800000830     D WLBDA8          DS                  INZ
002900000830     D  G08DAT                 1      8  0
003000000830     D  G08INV                 9     16  0
003100000830     D  G08ERR                17     17
003200000830     D  G08TGI                18     22  0
003300000830     D*
003400990908
003500010201
003600010201
003700000913     C                   reset                   rrnum
003800990921     C                   reset                   esito
003900990921     C                   reset                   wrkesito
004000000613     C*
004100040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004200000613     C*
004300010202     C* Effettuo la chiamata al CLLE preposto
004400040506     C                   call(e)   'TITVVTC'
004500010202     C                   parm                    parccm
004600010202     C                   parm                    parmbr
004700010202     C                   parm      '2'           paropz
004800000616     C*
004900000801     C
005000010201     C                   seton                                        LR
005100990908
005200000801
005300910830     C*--------------------------------------------------------
005400040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
005500910830     C*--------------------------------------------------------
005600040526     C     RWFILE        BEGSR
005700990910     C*
005800990914     C                   if        not %open(tivin00r)
005900990908     C                   open      tivin00r
006000990914     C                   endif
006100021113     C                   if        not %open(fivabwwr)
006200021113     C                   open      fivabwwr
006300990914     C                   endif
006400021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
006500020305     C                   exsr      prevat
006600010201     C*
006700010202     C                   if        chkcall = '0'
006800010202     C*
006900021113     C                   if        not %open(fivatwwr)
007000021113     C                   open      fivatwwr
007100010201     C                   endif
007200990910     C*
007300010201     C                   clear                   §CTROKVB          5 0
007400020305     C                   clear                   §CTROKVT          5 0
007500000801     C                   clear                   §CTRMO            5 0
007600000801     C                   clear                   §CTRNO            5 0
007700990910     C*
007800921023     C                   DO        *HIVAL
007900990913     C*
008000990915     C                   READ      tivin00r                               70
008100000905     C                   if        vindta > *blanks
008200000613     C                   add       1             rrnum
008300000801     C*
008400000801     C                   if        *in70 = *off
008500000801     C                             and
008600000801     C                             (vinflg = *blanks
008700000801     C                              or vinflg = '0'
008800000801     C                              or vinflg = '2')
008900000801     C*
009000000801     C                   clear                   vinmsg
009100000801     C                   eval      vinflg = '1'
009200010305     C*
009300040802     C                   exsr      impvab
009400040802     C                   exsr      wrivab
009500020305     C                   exsr      wrivat                                       => carico VAT
009600040802     C*
009700010305     C                   endif
009800000905     C*
009900000905     C                   else
010000000905     C                   eval      vinflg = '1'
010100000905     C                   endif
010200000905     C*
010300000905     C  N70              update    tivin000
010400000905     C*
010500991022     C  N70              ENDdo
010600010202     C*
010700010202     C                   endif
010800990910
010900990910     C* Se non ci sono record con errori ...
011000000710     C                   if        §ctrno = 0
011100990910     C* ... restituisco esito OK.
011200990921     C                   eval      wrkesito = '0'
011300990910     C                   else
011400010201     C                   if        §ctrokvb > 0
011500990921     C                   eval      wrkesito = '1'
011600000710     C                   else
011700000710     C                   eval      wrkesito = '2'
011800990910     C                   endif
011900000710     C                   endif
012000990910     C*
012100990914     C                   if        %open(tivin00r)
012200990908     C                   close     tivin00r
012300990914     C                   endif
012400021113     C                   if        %open(fivabwwr)
012500021113     C                   close     fivabwwr
012600990914     C                   endif
012700021113     C                   if        %open(fivatwwr)
012800021113     C                   close     fivatwwr
012900010201     C                   endif
013000990910     C*
013100010201     C                   if        §ctrokvb > 0
013200000724     C                             and vlrpoi <> *zeros
013300010202     C                   exsr      invio
013400990920     C                   endif
013500990920     C*
013600910830     C                   ENDSR
013700000613     C***
013800010305
013900010305     C*----------------------------------------------------*
014000020305     C*  SCARICAMENTO BUFFER RECORDS VAB
014100010305     C*----------------------------------------------------*
014200020305     C     WRIVAB        BEGSR
014300010305     C*
014400021113     C                   write     fivab000                                     => scarico il VAB
014500010305     C*
014600010305     C                   ENDSR
014700990920
014800000801     C*----------------------------------------------------*
014900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
015000000801     C*----------------------------------------------------*
015100010201     C     INZVAR        BEGSR
015200000801     C*
015300040802     C                   Z-ADD     *zeros        Num5_0            5 0
015400040802     C                   MOVEL     '0'           FlgCAS            1
015500000801     C*
015600000801     C                   ENDSR
015700000801     C*----------------------------------------------------*
015800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
015900000801     C*----------------------------------------------------*
016000000801     C     DEFCAM        BEGSR
016100000801     C*
016200021113     C                   CLEAR                   FIVAB000
016300040802     C                   CLEAR                   FIVAT000
016400020619     C* Imposto i valori di default...
016500050216     C                   Z-ADD     0502599       VABCCM
016600050216     C                   Z-ADD     0502599       VATCCM
016700050216     C                   Z-ADD     050           VABLNP
016800050216     C                   Z-ADD     050           VATLNP
016900040714     C                   Z-ADD     000           VABCTR
017000020619     C* ... e poi verifico se sono stati passati come parametri
017100020619     C                   IF        vlrppt > *blanks
017200040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
017300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
017400020619     C                   EXSR      CHKNUM
017500020619     C                   IF        PiInt=*on
017600020619     C                   Z-ADD     PiVal         VABCCM
017700020619     C                   Z-ADD     PiVal         VATCCM
017800020619     C                   ENDIF
017900040506     C                   ENDIF
018000040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
018100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
018200020619     C                   EXSR      CHKNUM
018300020619     C                   IF        PiInt=*on
018400020619     C                   Z-ADD     PiVal         VABLNP
018500020619     C                   Z-ADD     PiVal         VATLNP
018600040506     C                   ENDIF
018700020619     C                   ENDIF
018800040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
018900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
019000020619     C                   EXSR      CHKNUM
019100020619     C                   IF        PiInt=*on
019200020619     C                   Z-ADD     PiVal         VABCTR
019300040506     C                   ENDIF
019400020619     C                   ENDIF
019500020619     C                   ENDIF
019600000801     C*
019700000801     C                   ENDSR
019800000801     C*----------------------------------------------------*
019900021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
020000000801     C*----------------------------------------------------*
020100010201     C     IMPVAB        BEGSR
020200000801     C*
020300020305     C                   EXSR      INZVAR
020400020305     C                   EXSR      DEFCAM
020500010305     C*
020600000801     C                   Z-ADD     *zeros        errore            1 0
020700000830     C                   MOVEL     datcor        VABAAS
020800020305     C                   MOVEL     datcor        VATAAS
020900040526     C                   MOVE      datcor        VABMGS
021000021113     C                   MOVE(P)   vlrpoi        VABFGS
021100021113     C                   MOVE(P)   vlrpoi        VATFGS
021200050216     C*
021300050216     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
021400020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
021500020117     C     '@':'A'       XLATE     VABRSD        VABRSD
021600020117     C* ==
021700050216     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
021800041201     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
021900041201     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
022000050216     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
022100050216     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
022200050216     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
022300050216     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:35))
022400050520     C                   EVAL      VABRMO=%trim(%subst(vindta:362:4))
022500110218     C* TC1
022600110218     C                   IF        %subst(vindta:13:1) = 'A'
022700110218     C                   EVAL      VABTC1='A'
022800110218     C                   ENDIF
022900110218     C* TC2
023000110218     C                   IF        %subst(vindta:15:1) = 'P'
023100110218     C                   EVAL      VABTC2='P'
023200110218     C                   ENDIF
023300040506     C* CAD
023400041201     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
023500010201     C                   EXSR      CHKNUM
023600010201     C                   IF        PiInt=*on
023700010201     C                   Z-ADD     PiVal         Num5_0
023800040506     C                   MOVEL(P)  Num5_0        VABCAD
023900010201     C                   ELSE
024000040506     C                   ADD       1             errore
024100010201     C                   EVAL      vinmsg = %trimr(vinmsg)
024200040506     C                             + ' ' + 'VABCAD'
024300010201     C                   ENDIF
024400050926     C* CCM
024500061106     C*                  EVAL      PiStr=%trim(%subst(vindta:1:7))
024600061106     C*                  EXSR      CHKNUM
024700061106     C*                  IF        PiInt=*on
024800061106     C*                  Z-ADD     PiVal         VABCCM
024900061106     C*                  ELSE
025000061106     C*                  ADD       1             errore
025100061106     C*                  EVAL      vinmsg = %trimr(vinmsg)
025200061106     C*                            + ' ' + 'VABCCM'
025300061106     C*                  ENDIF
025400041201     C* NCL
025500041201     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
025600040506     C                   EXSR      CHKNUM
025700040506     C                   IF        PiInt=*on
025800040506     C                   Z-ADD     PiVal         VABNCL
025900040506     C                   ELSE
026000040506     C                   ADD       1             errore
026100040506     C                   EVAL      vinmsg = %trimr(vinmsg)
026200040506     C                             + ' ' + 'VABNCL'
026300040506     C                   ENDIF
026400040506     C* PKB
026500050216     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
026600010201     C                   EXSR      CHKNUM
026700010201     C                   IF        PiNum=*on
026800011115     C                   Z-ADD     PiVal         VABPKB
026900010201     C                   ELSE
027000010201     C                   ADD       1             errore
027100010201     C                   EVAL      vinmsg = %trimr(vinmsg)
027200010201     C                             + ' ' + 'VABPKB'
027300010201     C                   ENDIF
027400041201     C* VLB
027500050216     C                   IF        %trim(%subst(vindta:308:7)) <> *blanks
027600050216     C                   EVAL      PiStr=%trim(%subst(vindta:308:7))
027700041201     C                   EXSR      CHKNUM
027800041201     C                   IF        PiNum=*on
027900090122     C                   EVAL      PiVal = PiVal / 1000000                      * da cm3 a m3
028000090122     C                   Z-ADD(H)  PiVal         VABVLB
028100041201     C                   ELSE
028200041201     C                   ADD       1             errore
028300041201     C                   EVAL      vinmsg = %trimr(vinmsg)
028400041201     C                             + ' ' + 'VABVLB'
028500041201     C                   ENDIF
028600050216     C                   ENDIF
028700040506     C* NSP/RMN
028800050216     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
028900010201     C                   EXSR      CHKNUM
029000010201     C                   IF        PiInt=*on
029100040506     C                   Z-ADD     PiVal         VABNSP
029200040506     C                   Z-ADD     PiVal         VATNSP
029300040506     C                   Z-ADD     PiVal         VABRMN
029400010201     C                   ELSE
029500010201     C                   ADD       1             errore
029600010201     C                   EVAL      vinmsg = %trimr(vinmsg)
029700040506     C                             + ' ' + 'VABNSP VATNSP VABRMN'
029800040526     C                   ENDIF
029900041201     C* CAS
030000050216     C                   IF        %trim(%subst(vindta:280:12)) <> *blanks
030100040802     C                   EVAL      FlgCAS = '1'
030200050216     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
030300050216     C                   EVAL      VABVCA = %trim(%subst(vindta:277:3))
030400040714     C                   EXSR      CHKNUM
030500040714     C                   IF        PiNum=*on
030600050216     C                   Z-ADD     PiVal         VABCAS
030700040714     C                   ELSE
030800040714     C                   ADD       1             errore
030900040714     C                   EVAL      vinmsg = %trimr(vinmsg)
031000040802     C                             + ' ' + 'VABCAS'
031100040714     C                   ENDIF
031200040802     C                   ENDIF
031300050216     C* IAS
031400050216     C                   IF        %trim(%subst(vindta:295:12)) <> *blanks
031500050216     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
031600050216     C                   EVAL      VABVAS = %trim(%subst(vindta:292:3))
031700050216     C                   EXSR      CHKNUM
031800050216     C                   IF        PiNum=*on
031900050216     C                   Z-ADD     PiVal         VABIAS
032000050216     C                   ELSE
032100050216     C                   ADD       1             errore
032200050216     C                   EVAL      vinmsg = %trimr(vinmsg)
032300050216     C                             + ' ' + 'VABIAS'
032400050216     C                   ENDIF
032500050216     C                   ENDIF
032600010205     C*
032700010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
032800040802     C                   IF        FlgCAS <> '0'
032900010205     C                   EVAL      VABCBO = '4'
033000010205     C                   ELSE
033100010205     C                   EVAL      VABCBO = '1'
033200010205     C                   ENDIF
033300020305     C*
033400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
033500011113     C                   EXSR      CHKIMPDIV
033600010202     C*
033700000801     C* Ebbene...
033800000801     C                   ADD       1             §CTRMO
033900010201     C                   IF        errore <> *zeros
034000000801     C                   ADD       1             §CTRNO
034100000801     C                   EVAL      vinflg = '2'
034200000801     C                   ELSE
034300010201     C                   ADD       1             §CTROKVB
034400000801     C                   ENDIF
034500000801     C*
034600000801     C                   ENDSR
034700010201     C*----------------------------------------------------*
034800040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
034900010201     C*----------------------------------------------------*
035000020305     C     WRIVAT        BEGSR
035100040802     C*
035200040802     C* Valorizzo i campi chiave
035300041201     C                   EVAL      VATTRC = 'B'
035400040802     C*
035500041201     C* Compongo i barcode e scarico il buffer del FIVAT
035600050216     C                   EVAL      VATNOT = %subst(vindta:92:15)
035700050216     C                   IF        VATNOT <> *blanks
035800040802     C                   WRITE     FIVAT000
035900050216     C                   ENDIF
036000010201     C*
036100010201     C                   ENDSR
036200010202     C*----------------------------------------------------*
036300021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
036400010202     C*----------------------------------------------------*
036500020305     C     PREVAT        BEGSR
036600010202     C*
036700021113     C* Compongo il nome del membro da dare al FIVATWWR
036800010202     C                   eval      parmbr = vlrhdl
036900010202     C                   movel     'M'           parmbr
037000041201     C                   eval      parccm = vlrksc
037100010202     C                   eval      paropz = '1'
037200010202     C* Effettuo la chiamata al CLLE preposto
037300040506     C                   call(e)   'TITVVTC'
037400010202     C                   parm                    parccm
037500010202     C                   parm                    parmbr
037600010202     C                   parm                    paropz
037700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
037800010202     C                   if        %error
037900010202     C                   movel     '1'           chkcall
038000010202     C                   else
038100010202     C                   movel     '0'           chkcall
038200010202     C                   endif
038300010202     C*
038400010202     C                   ENDSR
038500000801     C*----------------------------------------------------*
038600000801     C*  CONTROLLO NUMERICITA' CAMPI
038700000801     C*----------------------------------------------------*
038800000801     C     CHKNUM        BEGSR
038900000801     C*
039000000801     C                   call(e)   'ISNUMERIC'
039100000801     C                   PARM                    PiStr            30
039200050216     C                   PARM      '.'           PiDecChr          1
039300000801     C                   PARM      *ZEROS        PiVal            30 9
039400000801     C                   PARM      '0'           PiInt             1
039500000801     C                   PARM      '0'           PiNum             1
039600000801     C                   IF        %error
039700000801     C                   EVAL      PiInt=*off
039800000801     C                   ENDIF
039900000801     C*
040000000801     C                   ENDSR
040100000801     C***
040200000801
040300011113
040400011113     C*----------------------------------------------------*
040500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
040600011113     C*----------------------------------------------------*
040700011113     C     CHKIMPDIV     BEGSR
040800011113     C*
040900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
041000011113     C                   Z-ADD     *zeros        wrkDec            9 9
041100011113     C*
041200011113     C* Come prima cosa effettuo considerazioni sulla divisa
041300011113     C                   IF        vabIAS > *zeros
041400011113     C                   IF        vabVAS <> 'EUR'
041500011113     C                   EVAL      vabVAS =  'ITL'
041600011113     C                   ENDIF
041700011113     C                   ENDIF
041800011113     C*
041900011113     C                   IF        vabCAS > *zeros
042000011113     C                   IF        vabVCA <> 'EUR'
042100011113     C                   EVAL      vabVCA =  'ITL'
042200011113     C                   ENDIF
042300011113     C                   ENDIF
042400011113     C*
042500011113     C                   IF        vabVMD > *zeros
042600020305     C                   IF        vabVAD <> 'EUR'
042700011113     C                   EVAL      vabVAD =  'ITL'
042800011113     C                   ENDIF
042900011113     C                   ENDIF
043000011113     C*
043100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
043200011113     C                   Z-ADD     vabIAS        wrkDec
043300011113     C                   IF        wrkDec > *zeros
043400011113     C                   IF        vabVAS = 'ITL'
043500011113     C                   EVAL      vabIAS = *zeros
043600011113     C                   ENDIF
043700011113     C                   ENDIF
043800011113     C*
043900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
044000011113     C                   Z-ADD     vabCAS        wrkDec
044100011113     C                   IF        wrkDec > *zeros
044200011113     C                   IF        vabVCA = 'ITL'
044300011113     C                   EVAL      vabCAS = *zeros
044400011113     C                   ENDIF
044500011113     C                   ENDIF
044600011113     C*
044700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
044800011113     C                   Z-ADD     vabVMD        wrkDec
044900011113     C                   IF        wrkDec > *zeros
045000011113     C                   IF        vabVAD = 'ITL'
045100011113     C                   EVAL      vabVMD = *zeros
045200011113     C                   ENDIF
045300011113     C                   ENDIF
045400011113     C*
045500011113     C                   ENDSR
045600011113     C***
045700011113
045800011113
045900000801
046000000801
046100990920      /TITLE Invio dei dati al punto operativo.
046200010202     C     invio         BEGSR
046300990920     C*
046400021113     C* 1° invio FIVAT
046500010201     C                   reset                   dscmz
046600010201     C                   move      vlrpoi        cmzdst
046700021113     C                   eval      cmzfld = 'FIVATWWR'
046800010201     C                   eval      cmzmbd = vlrhdl
046900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
047000021009     C***                if        prmfir = *blanks
047100021113     C                   eval      cmzfla = 'FIVAT00F'
047200021113     C                   eval      cmzmba = 'FIVAT00F'
047300021009     C***                else
047400021009     C***                eval      cmzfla = prmfir
047500021009     C***                eval      cmzmba = prmfir
047600021009     C***                endif
047700010201     C                   eval      cmznrr = *zeros
047800020305     C                   move      §ctrokvt      cmznrr
047900021018     C                   eval      cmzlba = vlrfl1
048000010201     C                   call(e)   'TIS711C'
048100010201     C                   parm                    dscmz
048200010201     C                   parm      *blanks       esito
048300010205     C                   if        %error
048400010205     C                             or cmzerr = '1'
048500010205     C                             or esito  = '1'
048600010205     C                   eval      wrkesito = '3'
048700010205     C                   else
048800010201     C*
048900021113     C* 2° invio FIVAB
049000010201     C                   reset                   dscmz
049100010201     C                   move      vlrpoi        cmzdst
049200010201     C                   eval      cmzfld = vlrfou
049300010201     C                   eval      cmzmbd = vlrhdl
049400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
049500021009     C***                if        prmfir = *blanks
049600021113     C                   eval      cmzfla = 'FIVAB00F'
049700021113     C                   eval      cmzmba = 'FIVAB00F'
049800021009     C***                else
049900021009     C***                eval      cmzfla = prmfir
050000021009     C***                eval      cmzmba = prmfir
050100021009     C***                endif
050200010201     C                   eval      cmznrr = *zeros
050300010201     C                   move      §ctrokvb      cmznrr
050400021018     C                   eval      cmzlba = vlrfl1
050500010201     C                   call(e)   'TIS711C'
050600010201     C                   parm                    dscmz
050700010201     C                   parm      *blanks       esito
050800010201     C                   if        %error
050900010201     C                             or cmzerr = '1'
051000010201     C                             or esito  = '1'
051100010201     C                   eval      wrkesito = '3'
051200010201     C                   endif
051300010205     C                   endif
051400990920     C*
051500000613     C                   ENDSR
051600000613     C***
051700070411
051800070411     C     *pssr         BEGSR
051900070411     C*
052000070411     C                   if        %open(tivin00r)
052100070411     C                   close     tivin00r
052200070411     C                   endif
052300070411     C                   if        %open(fivabwwr)
052400070411     C                   close     fivabwwr
052500070411     C                   endif
052600070411     C                   if        %open(fivatwwr)
052700070411     C                   close     fivatwwr
052800070411     C                   endif
052900070411     C*
053000070411     C* Effettuo la chiamata al CLLE preposto
053100070411     C                   call(e)   'TITVVTC'
053200070411     C                   parm                    parccm
053300070411     C                   parm                    parmbr
053400070411     C                   parm      '2'           paropz
053500070411     C*
053600070411     C                   eval      wrkesito = '2'
053700070411     C*
053800070411     C                   seton                                        LR
053900070411     C*
054000070411     C                   ENDSR     '*CANCL'
054100070411     C***
054200070411
054300990910
054400000613     C     *inzsr        BEGSR
054500990910     C*
054600990910     C     *entry        plist
054700990920     C                   parm                    tivlrds
054800990921     C                   parm      wrkesito      esito
054900000724     C                   parm                    prmlit
055000000710     C                   parm                    prmfir
055100000613     C*
055200000830     C* CALCOLA LA DATA CORRENTE
055300000830     C                   time                    wn14             14 0
055400000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
055500000830     C                   z-add     wn8           g08dat
055600000830     C                   z-add     *zeros        g08inv
055700000830     C                   movel     '0'           g08err
055800000830     C                   call      'XSRDA8'
055900000830     C                   parm                    wlbda8
056000000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
056100000830     C*
056200000613     C                   ENDSR
056300000613     C***
