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
001500040506     D tisi95ds      e ds
001600990910     D esito           s              1
001700000724     D prmlit          s             10
001800000710     D prmfir          s             10
001900990921     D wrkesito        s                   like(esito)
002000000613     D rrnum           s              6  0 INZ(*zeros)
002100010202     D parccm          s              8    INZ(*blanks)
002200010202     D parmbr          s             10    INZ(*blanks)
002300010202     D paropz          s              1    INZ(*blanks)
002400010202     D chkcall         s              1    INZ(*blanks)
002500000830
002600000830     D*------------------
002700000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
002800000830     D*------------------
002900000830     D WLBDA8          DS                  INZ
003000000830     D  G08DAT                 1      8  0
003100000830     D  G08INV                 9     16  0
003200000830     D  G08ERR                17     17
003300000830     D  G08TGI                18     22  0
003400000830     D*
003500990908
003600010201
003700010201
003800000913     C                   reset                   rrnum
003900990921     C                   reset                   esito
004000990921     C                   reset                   wrkesito
004100000613     C*
004200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004300000613     C*
004400010202     C* Effettuo la chiamata al CLLE preposto
004500040506     C                   call(e)   'TITVVTC'
004600010202     C                   parm                    parccm
004700010202     C                   parm                    parmbr
004800010202     C                   parm      '2'           paropz
004900050201     C*
005000050201     C* Effettuo lancio TISI95 solo x chiusura
005100050201     C                   CLEAR                   TISI95DS
005200050201     C                   EVAL      I95TLA = 'C'
005300050201     C                   CALL      'TISI95R'
005400050201     C                   PARM                    TISI95DS
005500000616     C*
005600000801     C
005700010201     C                   seton                                        LR
005800990908
005900000801
006000910830     C*--------------------------------------------------------
006100040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
006200910830     C*--------------------------------------------------------
006300040526     C     RWFILE        BEGSR
006400990910     C*
006500990914     C                   if        not %open(tivin00r)
006600990908     C                   open      tivin00r
006700990914     C                   endif
006800021113     C                   if        not %open(fivabwwr)
006900021113     C                   open      fivabwwr
007000990914     C                   endif
007100021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007200020305     C                   exsr      prevat
007300010201     C*
007400010202     C                   if        chkcall = '0'
007500010202     C*
007600021113     C                   if        not %open(fivatwwr)
007700021113     C                   open      fivatwwr
007800010201     C                   endif
007900990910     C*
008000010201     C                   clear                   §CTROKVB          5 0
008100020305     C                   clear                   §CTROKVT          5 0
008200000801     C                   clear                   §CTRMO            5 0
008300000801     C                   clear                   §CTRNO            5 0
008400990910     C*
008500921023     C                   DO        *HIVAL
008600990913     C*
008700990915     C                   READ      tivin00r                               70
008800000905     C                   if        vindta > *blanks
008900000613     C                   add       1             rrnum
009000000801     C*
009100000801     C                   if        *in70 = *off
009200000801     C                             and
009300000801     C                             (vinflg = *blanks
009400000801     C                              or vinflg = '0'
009500000801     C                              or vinflg = '2')
009600000801     C*
009700000801     C                   clear                   vinmsg
009800000801     C                   eval      vinflg = '1'
009900010305     C*
010000040802     C                   exsr      impvab
010100040802     C                   exsr      wrivab
010200020305     C                   exsr      wrivat                                       => carico VAT
010300040802     C*
010400010305     C                   endif
010500000905     C*
010600000905     C                   else
010700000905     C                   eval      vinflg = '1'
010800000905     C                   endif
010900000905     C*
011000000905     C  N70              update    tivin000
011100000905     C*
011200991022     C  N70              ENDdo
011300010202     C*
011400010202     C                   endif
011500990910
011600990910     C* Se non ci sono record con errori ...
011700000710     C                   if        §ctrno = 0
011800990910     C* ... restituisco esito OK.
011900990921     C                   eval      wrkesito = '0'
012000990910     C                   else
012100010201     C                   if        §ctrokvb > 0
012200990921     C                   eval      wrkesito = '1'
012300000710     C                   else
012400000710     C                   eval      wrkesito = '2'
012500990910     C                   endif
012600000710     C                   endif
012700990910     C*
012800990914     C                   if        %open(tivin00r)
012900990908     C                   close     tivin00r
013000990914     C                   endif
013100021113     C                   if        %open(fivabwwr)
013200021113     C                   close     fivabwwr
013300990914     C                   endif
013400021113     C                   if        %open(fivatwwr)
013500021113     C                   close     fivatwwr
013600010201     C                   endif
013700990910     C*
013800010201     C                   if        §ctrokvb > 0
013900000724     C                             and vlrpoi <> *zeros
014000010202     C                   exsr      invio
014100990920     C                   endif
014200990920     C*
014300910830     C                   ENDSR
014400000613     C***
014500010305
014600010305     C*----------------------------------------------------*
014700020305     C*  SCARICAMENTO BUFFER RECORDS VAB
014800010305     C*----------------------------------------------------*
014900020305     C     WRIVAB        BEGSR
015000010305     C*
015100021113     C                   write     fivab000                                     => scarico il VAB
015200010305     C*
015300010305     C                   ENDSR
015400990920
015500000801     C*----------------------------------------------------*
015600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
015700000801     C*----------------------------------------------------*
015800010201     C     INZVAR        BEGSR
015900000801     C*
016000040802     C                   Z-ADD     *zeros        Num5_0            5 0
016100040802     C                   MOVEL     '0'           FlgCAS            1
016200000801     C*
016300000801     C                   ENDSR
016400000801     C*----------------------------------------------------*
016500000801     C*  IMPOSTAZIONE CAMPI COSTANTI
016600000801     C*----------------------------------------------------*
016700000801     C     DEFCAM        BEGSR
016800000801     C*
016900021113     C                   CLEAR                   FIVAB000
017000040802     C                   CLEAR                   FIVAT000
017100020619     C* Imposto i valori di default...
017200040802     C                   Z-ADD     1200433       VABCCM
017300040802     C                   Z-ADD     1200433       VATCCM
017400040802     C                   Z-ADD     120           VABLNP
017500040802     C                   Z-ADD     120           VATLNP
017600040714     C                   Z-ADD     000           VABCTR
017700040714     C                   MOVEL     '7Q'          VABCTM
017800020619     C* ... e poi verifico se sono stati passati come parametri
017900020619     C                   IF        vlrppt > *blanks
018000040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
018100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
018200020619     C                   EXSR      CHKNUM
018300020619     C                   IF        PiInt=*on
018400020619     C                   Z-ADD     PiVal         VABCCM
018500020619     C                   Z-ADD     PiVal         VATCCM
018600020619     C                   ENDIF
018700040506     C                   ENDIF
018800040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
018900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
019000020619     C                   EXSR      CHKNUM
019100020619     C                   IF        PiInt=*on
019200020619     C                   Z-ADD     PiVal         VABLNP
019300020619     C                   Z-ADD     PiVal         VATLNP
019400040506     C                   ENDIF
019500020619     C                   ENDIF
019600040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
019700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
019800020619     C                   EXSR      CHKNUM
019900020619     C                   IF        PiInt=*on
020000020619     C                   Z-ADD     PiVal         VABCTR
020100040506     C                   ENDIF
020200020619     C                   ENDIF
020300020619     C                   ENDIF
020400000801     C*
020500000801     C                   ENDSR
020600000801     C*----------------------------------------------------*
020700021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
020800000801     C*----------------------------------------------------*
020900010201     C     IMPVAB        BEGSR
021000000801     C*
021100020305     C                   EXSR      INZVAR
021200020305     C                   EXSR      DEFCAM
021300010305     C*
021400000801     C                   Z-ADD     *zeros        errore            1 0
021500000830     C                   MOVEL     datcor        VABAAS
021600020305     C                   MOVEL     datcor        VATAAS
021700040526     C                   MOVE      datcor        VABMGS
021800021113     C                   MOVE(P)   vlrpoi        VABFGS
021900021113     C                   MOVE(P)   vlrpoi        VATFGS
022000040802     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
022100020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
022200020117     C     '@':'A'       XLATE     VABRSD        VABRSD
022300020117     C* ==
022400040802     C                   EVAL      VABRD2=%trim(%subst(vindta:1+35:60-35))
022500040802     C                   EVAL      VABIND=%trim(%subst(vindta:61:35))
022600040802     C                   EVAL      VABLOD=%trim(%subst(vindta:101:25))
022700040802     C                   EVAL      VABPRD=%trim(%subst(vindta:126:2))
022800040802     C                   EVAL      VABRMA=%trim(%subst(vindta:169:6))
022900040802     C                   EVAL      VABNOT=%trim(%subst(vindta:175:35))
023000040802     C                   EVAL      VABNT2=%trim(%subst(vindta:175+35:56-35))
023100040506     C* CAD
023200040802     C                   EVAL      PiStr=%trim(%subst(vindta:96:5))
023300010201     C                   EXSR      CHKNUM
023400010201     C                   IF        PiInt=*on
023500010201     C                   Z-ADD     PiVal         Num5_0
023600040506     C                   MOVEL(P)  Num5_0        VABCAD
023700010201     C                   ELSE
023800040506     C                   ADD       1             errore
023900010201     C                   EVAL      vinmsg = %trimr(vinmsg)
024000040506     C                             + ' ' + 'VABCAD'
024100010201     C                   ENDIF
024200040506     C* Reperisco la provincia dal CAP e dalla località
024300040526     C                   IF        VABCAD <> *blanks AND
024400040526     C                             VABPRD  = *blanks
024500040506     C                   CLEAR                   TISI95DS
024600040506     C                   EVAL      I95TCN = '3'
024700040506     C                   Z-ADD     datcor        I95DAT
024800040506     C                   EVAL      I95CAP = VABCAD
024900040506     C                   EVAL      I95LOC = VABLOD
025000040506     C                   CALL      'TISI95R'
025100040506     C                   PARM                    TISI95DS
025200040506     C                   EVAL      VABPRD = O95PRV
025300040506     C                   ENDIF
025400040506     C* NCL
025500040802     C                   EVAL      PiStr=%trim(%subst(vindta:128:4))
025600040506     C                   EXSR      CHKNUM
025700040506     C                   IF        PiInt=*on
025800040506     C                   Z-ADD     PiVal         VABNCL
025900040506     C                   ELSE
026000040506     C                   ADD       1             errore
026100040506     C                   EVAL      vinmsg = %trimr(vinmsg)
026200040506     C                             + ' ' + 'VABNCL'
026300040506     C                   ENDIF
026400040506     C* PKB
026500040802     C                   EVAL      PiStr=%trim(%subst(vindta:132:7))
026600010201     C                   EXSR      CHKNUM
026700010201     C                   IF        PiNum=*on
026800011115     C                   Z-ADD     PiVal         VABPKB
026900010201     C                   ELSE
027000010201     C                   ADD       1             errore
027100010201     C                   EVAL      vinmsg = %trimr(vinmsg)
027200010201     C                             + ' ' + 'VABPKB'
027300010201     C                   ENDIF
027400040506     C* NSP/RMN
027500040802     C                   EVAL      PiStr=%trim(%subst(vindta:169:6))
027600010201     C                   EXSR      CHKNUM
027700010201     C                   IF        PiInt=*on
027800040506     C                   Z-ADD     PiVal         VABNSP
027900040506     C                   Z-ADD     PiVal         VATNSP
028000040506     C                   Z-ADD     PiVal         VABRMN
028100010201     C                   ELSE
028200010201     C                   ADD       1             errore
028300010201     C                   EVAL      vinmsg = %trimr(vinmsg)
028400040506     C                             + ' ' + 'VABNSP VATNSP VABRMN'
028500040526     C                   ENDIF
028600040802     C* CAS
028700040802     C                   IF        %subst(vindta:144:25) <> *blanks
028800040802     C                   EVAL      FlgCAS = '1'
028900040802     C                   EVAL      PiStr=%trim(%subst(vindta:144:25))
029000040714     C                   EXSR      CHKNUM
029100040714     C                   IF        PiNum=*on
029200040802     C                   Z-ADD     PiVal         VABCAS
029300040802     C                   EVAL      VABVCA = 'EUR'
029400040714     C                   ELSE
029500040714     C                   ADD       1             errore
029600040714     C                   EVAL      vinmsg = %trimr(vinmsg)
029700040802     C                             + ' ' + 'VABCAS'
029800040714     C                   ENDIF
029900040802     C                   ENDIF
030000010205     C*
030100010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
030200040802     C                   IF        FlgCAS <> '0'
030300010205     C                   EVAL      VABCBO = '4'
030400010205     C                   ELSE
030500010205     C                   EVAL      VABCBO = '1'
030600010205     C                   ENDIF
030700020305     C*
030800011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
030900011113     C                   EXSR      CHKIMPDIV
031000010202     C*
031100000801     C* Ebbene...
031200000801     C                   ADD       1             §CTRMO
031300010201     C                   IF        errore <> *zeros
031400000801     C                   ADD       1             §CTRNO
031500000801     C                   EVAL      vinflg = '2'
031600000801     C                   ELSE
031700010201     C                   ADD       1             §CTROKVB
031800000801     C                   ENDIF
031900000801     C*
032000000801     C                   ENDSR
032100010201     C*----------------------------------------------------*
032200040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
032300010201     C*----------------------------------------------------*
032400020305     C     WRIVAT        BEGSR
032500040802     C*
032600040802     C* Inizializzo campi d wrk
032700040802     C                   Z-ADD     *zeros        wColloNum         4 0
032800040802     C                   MOVEL     *blanks       wColloAlfaDa      4
032900040802     C                   MOVE(P)   VABNCL        wColloAlfaA       4
033000040802     C*
033100040802     C* Valorizzo i campi chiave
033200040802     C                   EVAL      VATNSP = VABNSP
033300040802     C                   EVAL      VATTRC = 'E'
033400040802     C*
033500040802     C* Compongo i barcode
033600040802     C                   FOR       wColloNum = 1 to VABNCL
033700040802     C                   MOVE(P)   wColloNum     wColloAlfaDa
033800040802     C                   EVAL      VATNOT = '1200433' + %subst(vindta:169:6) +
033900040802     C                                      wColloAlfaDa + wColloAlfaA
034000040802     C                   WRITE     FIVAT000
034100040802     C                   ENDFOR
034200010201     C*
034300010201     C                   ENDSR
034400010202     C*----------------------------------------------------*
034500021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
034600010202     C*----------------------------------------------------*
034700020305     C     PREVAT        BEGSR
034800010202     C*
034900021113     C* Compongo il nome del membro da dare al FIVATWWR
035000010202     C                   eval      parmbr = vlrhdl
035100010202     C                   movel     'M'           parmbr
035200040802     C                   eval      parccm = '01200433'
035300010202     C                   eval      paropz = '1'
035400010202     C* Effettuo la chiamata al CLLE preposto
035500040506     C                   call(e)   'TITVVTC'
035600010202     C                   parm                    parccm
035700010202     C                   parm                    parmbr
035800010202     C                   parm                    paropz
035900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
036000010202     C                   if        %error
036100010202     C                   movel     '1'           chkcall
036200010202     C                   else
036300010202     C                   movel     '0'           chkcall
036400010202     C                   endif
036500010202     C*
036600010202     C                   ENDSR
036700000801     C*----------------------------------------------------*
036800000801     C*  CONTROLLO NUMERICITA' CAMPI
036900000801     C*----------------------------------------------------*
037000000801     C     CHKNUM        BEGSR
037100000801     C*
037200000801     C                   call(e)   'ISNUMERIC'
037300000801     C                   PARM                    PiStr            30
037400040714     C                   PARM      ','           PiDecChr          1
037500000801     C                   PARM      *ZEROS        PiVal            30 9
037600000801     C                   PARM      '0'           PiInt             1
037700000801     C                   PARM      '0'           PiNum             1
037800000801     C                   IF        %error
037900000801     C                   EVAL      PiInt=*off
038000000801     C                   ENDIF
038100000801     C*
038200000801     C                   ENDSR
038300000801     C***
038400000801
038500011113
038600011113     C*----------------------------------------------------*
038700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
038800011113     C*----------------------------------------------------*
038900011113     C     CHKIMPDIV     BEGSR
039000011113     C*
039100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
039200011113     C                   Z-ADD     *zeros        wrkDec            9 9
039300011113     C*
039400011113     C* Come prima cosa effettuo considerazioni sulla divisa
039500011113     C                   IF        vabIAS > *zeros
039600011113     C                   IF        vabVAS <> 'EUR'
039700011113     C                   EVAL      vabVAS =  'ITL'
039800011113     C                   ENDIF
039900011113     C                   ENDIF
040000011113     C*
040100011113     C                   IF        vabCAS > *zeros
040200011113     C                   IF        vabVCA <> 'EUR'
040300011113     C                   EVAL      vabVCA =  'ITL'
040400011113     C                   ENDIF
040500011113     C                   ENDIF
040600011113     C*
040700011113     C                   IF        vabVMD > *zeros
040800020305     C                   IF        vabVAD <> 'EUR'
040900011113     C                   EVAL      vabVAD =  'ITL'
041000011113     C                   ENDIF
041100011113     C                   ENDIF
041200011113     C*
041300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
041400011113     C                   Z-ADD     vabIAS        wrkDec
041500011113     C                   IF        wrkDec > *zeros
041600011113     C                   IF        vabVAS = 'ITL'
041700011113     C                   EVAL      vabIAS = *zeros
041800011113     C                   ENDIF
041900011113     C                   ENDIF
042000011113     C*
042100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
042200011113     C                   Z-ADD     vabCAS        wrkDec
042300011113     C                   IF        wrkDec > *zeros
042400011113     C                   IF        vabVCA = 'ITL'
042500011113     C                   EVAL      vabCAS = *zeros
042600011113     C                   ENDIF
042700011113     C                   ENDIF
042800011113     C*
042900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
043000011113     C                   Z-ADD     vabVMD        wrkDec
043100011113     C                   IF        wrkDec > *zeros
043200011113     C                   IF        vabVAD = 'ITL'
043300011113     C                   EVAL      vabVMD = *zeros
043400011113     C                   ENDIF
043500011113     C                   ENDIF
043600011113     C*
043700011113     C                   ENDSR
043800011113     C***
043900011113
044000011113
044100000801
044200000801
044300990920      /TITLE Invio dei dati al punto operativo.
044400010202     C     invio         BEGSR
044500990920     C*
044600021113     C* 1° invio FIVAT
044700010201     C                   reset                   dscmz
044800010201     C                   move      vlrpoi        cmzdst
044900021113     C                   eval      cmzfld = 'FIVATWWR'
045000010201     C                   eval      cmzmbd = vlrhdl
045100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
045200021009     C***                if        prmfir = *blanks
045300021113     C                   eval      cmzfla = 'FIVAT00F'
045400021113     C                   eval      cmzmba = 'FIVAT00F'
045500021009     C***                else
045600021009     C***                eval      cmzfla = prmfir
045700021009     C***                eval      cmzmba = prmfir
045800021009     C***                endif
045900010201     C                   eval      cmznrr = *zeros
046000020305     C                   move      §ctrokvt      cmznrr
046100021018     C                   eval      cmzlba = vlrfl1
046200010201     C                   call(e)   'TIS711C'
046300010201     C                   parm                    dscmz
046400010201     C                   parm      *blanks       esito
046500010205     C                   if        %error
046600010205     C                             or cmzerr = '1'
046700010205     C                             or esito  = '1'
046800010205     C                   eval      wrkesito = '3'
046900010205     C                   else
047000010201     C*
047100021113     C* 2° invio FIVAB
047200010201     C                   reset                   dscmz
047300010201     C                   move      vlrpoi        cmzdst
047400010201     C                   eval      cmzfld = vlrfou
047500010201     C                   eval      cmzmbd = vlrhdl
047600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
047700021009     C***                if        prmfir = *blanks
047800021113     C                   eval      cmzfla = 'FIVAB00F'
047900021113     C                   eval      cmzmba = 'FIVAB00F'
048000021009     C***                else
048100021009     C***                eval      cmzfla = prmfir
048200021009     C***                eval      cmzmba = prmfir
048300021009     C***                endif
048400010201     C                   eval      cmznrr = *zeros
048500010201     C                   move      §ctrokvb      cmznrr
048600021018     C                   eval      cmzlba = vlrfl1
048700010201     C                   call(e)   'TIS711C'
048800010201     C                   parm                    dscmz
048900010201     C                   parm      *blanks       esito
049000010201     C                   if        %error
049100010201     C                             or cmzerr = '1'
049200010201     C                             or esito  = '1'
049300010201     C                   eval      wrkesito = '3'
049400010201     C                   endif
049500010205     C                   endif
049600990920     C*
049700000613     C                   ENDSR
049800000613     C***
049900070411
050000070411     C     *pssr         BEGSR
050100070411     C*
050200070411     C                   if        %open(tivin00r)
050300070411     C                   close     tivin00r
050400070411     C                   endif
050500070411     C                   if        %open(fivabwwr)
050600070411     C                   close     fivabwwr
050700070411     C                   endif
050800070411     C                   if        %open(fivatwwr)
050900070411     C                   close     fivatwwr
051000070411     C                   endif
051100070411     C*
051200070411     C* Effettuo la chiamata al CLLE preposto
051300070411     C                   call(e)   'TITVVTC'
051400070411     C                   parm                    parccm
051500070411     C                   parm                    parmbr
051600070411     C                   parm      '2'           paropz
051700070411     C*
051800070411     C                   eval      wrkesito = '2'
051900070411     C*
052000070411     C                   seton                                        LR
052100070411     C*
052200070411     C                   ENDSR     '*CANCL'
052300070411     C***
052400070411
052500990910
052600000613     C     *inzsr        BEGSR
052700990910     C*
052800990910     C     *entry        plist
052900990920     C                   parm                    tivlrds
053000990921     C                   parm      wrkesito      esito
053100000724     C                   parm                    prmlit
053200000710     C                   parm                    prmfir
053300000613     C*
053400000830     C* CALCOLA LA DATA CORRENTE
053500000830     C                   time                    wn14             14 0
053600000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
053700000830     C                   z-add     wn8           g08dat
053800000830     C                   z-add     *zeros        g08inv
053900000830     C                   movel     '0'           g08err
054000000830     C                   call      'XSRDA8'
054100000830     C                   parm                    wlbda8
054200000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
054300000830     C*
054400000613     C                   ENDSR
054500000613     C***
