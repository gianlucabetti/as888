000100021017      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021017     FFIVABwwr  O    E             DISK    usropn
000600021017     FFIVATwwr  O    E             DISK    usropn
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
002000110615     D depspe          s             10    INZ(*blanks)
002100110615     D curspe          s             10    INZ(*blanks)
002200010202     D parccm          s              8    INZ(*blanks)
002300010202     D parmbr          s             10    INZ(*blanks)
002400010202     D paropz          s              1    INZ(*blanks)
002500010202     D chkcall         s              1    INZ(*blanks)
002600010213
002700000801     D  Num5_0         s              5  0
002800030107     D  Num7_0         s              7  0
002900000830
003000000830     D*------------------
003100000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003200000830     D*------------------
003300000830     D WLBDA8          DS                  INZ
003400000830     D  G08DAT                 1      8  0
003500000830     D  G08INV                 9     16  0
003600000830     D  G08ERR                17     17
003700000830     D  G08TGI                18     22  0
003800000830     D*
003900990908
004000010201
004100010201
004200000913     C                   reset                   rrnum
004300990921     C                   reset                   esito
004400990921     C                   reset                   wrkesito
004500000613     C*
004600040428     C                   EXSR      RWBSH                                        LETT/SCR. VAB/VAT
004700000613     C*
004800010202     C* Effettuo la chiamata al CLLE preposto
004900030711     C                   call(e)   'TITVVTC'
005000010202     C                   parm                    parccm
005100010202     C                   parm                    parmbr
005200010202     C                   parm      '2'           paropz
005300000616     C*
005400000801     C
005500010201     C                   seton                                        LR
005600990908
005700000801
005800910830     C*--------------------------------------------------------
005900040428     C* RWBSH     LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR *
006000910830     C*--------------------------------------------------------
006100040428     C     RWBSH         BEGSR
006200990910     C*
006300990914     C                   if        not %open(tivin00r)
006400990908     C                   open      tivin00r
006500990914     C                   endif
006600021017     C                   if        not %open(fivabwwr)
006700021017     C                   open      fivabwwr
006800990914     C                   endif
006900021017     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007000020305     C                   exsr      prevat
007100010201     C*
007200040428     C                   if        chkcall = '0'
007300010202     C*
007400021017     C                   if        not %open(fivatwwr)
007500021017     C                   open      fivatwwr
007600010201     C                   endif
007700990910     C*
007800010201     C                   clear                   §CTROKVB          5 0
007900020305     C                   clear                   §CTROKVT          5 0
008000000801     C                   clear                   §CTRMO            5 0
008100000801     C                   clear                   §CTRNO            5 0
008200990910     C*
008300921023     C                   DO        *HIVAL
008400990913     C*
008500990915     C                   READ      tivin00r                               70
008600000905     C                   if        vindta > *blanks
008700000613     C                   add       1             rrnum
008800000801     C*
008900000801     C                   if        *in70 = *off
009000000801     C                             and
009100000801     C                             (vinflg = *blanks
009200000801     C                              or vinflg = '0'
009300000801     C                              or vinflg = '2')
009400000801     C*
009500000801     C                   clear                   vinmsg
009600010305     C*
009700010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
009800110615     C                   EVAL      PiStr=%trim(%subst(vindta:298:10))
009900020305     C                   MOVEL(p)  PiStr         curspe
010000010305     C*
010100010305     C                   if        depspe = *blanks                             => 1° giro
010200010305     C                   eval      depspe = curspe                              => memorizz. spediz
010300040428     C                   exsr      impvab                                       => carico VAB
010400040428     C                   exsr      wrivat                                       => carico/scrivo VAT
010500010305     C                   else
010600010305     C                   if        curspe <> depspe                             => rottura di spediz
010700010305     C                   eval      depspe = curspe                              => memorizz. spediz
010800040428     C                   exsr      wrivab                                       => scrivo VAB
010900040428     C                   exsr      impvab                                       => carico VAB
011000040428     C                   exsr      wrivat                                       => carico/scrivo VAT
011100020305     C                   else                                                   => x stessa spediz
011200040428     C                   exsr      impvab                                       => carico VAB
011300040428     C                   exsr      wrivat                                       => carico/scrivo VAT
011400010305     C                   endif
011500010305     C                   endif
011600010305     C                   endif
011700000905     C*
011800000905     C                   else
011900000905     C                   eval      vinflg = '1'
012000000905     C                   endif
012100000905     C*
012200000905     C  N70              update    tivin000
012300000905     C*
012400991022     C  N70              ENDdo
012500010202     C*
012600020305     C* Scarico i VAB rimasti "in sospeso"
012700020305     C                   exsr      wrivab
012800010202     C*
012900010202     C                   endif
013000990910
013100990910     C* Se non ci sono record con errori ...
013200000710     C                   if        §ctrno = 0
013300990910     C* ... restituisco esito OK.
013400990921     C                   eval      wrkesito = '0'
013500990910     C                   else
013600010201     C                   if        §ctrokvb > 0
013700990921     C                   eval      wrkesito = '1'
013800000710     C                   else
013900000710     C                   eval      wrkesito = '2'
014000990910     C                   endif
014100000710     C                   endif
014200990910     C*
014300990914     C                   if        %open(tivin00r)
014400990908     C                   close     tivin00r
014500990914     C                   endif
014600021017     C                   if        %open(fivabwwr)
014700021017     C                   close     fivabwwr
014800990914     C                   endif
014900021017     C                   if        %open(fivatwwr)
015000021017     C                   close     fivatwwr
015100010201     C                   endif
015200021017     C*
015300021017     C                   if        vlrpoi <> 999
015400021017     C                   eval      vlrpoi = vlrpoi
015500021017     C                   endif
015600990910     C*
015700010201     C                   if        §ctrokvb > 0
015800021017     C                             and vlrpoi > *zeros
015900010202     C                   exsr      invio
016000990920     C                   endif
016100990920     C*
016200910830     C                   ENDSR
016300000613     C***
016400010305
016500010305     C*----------------------------------------------------*
016600020305     C*  SCARICAMENTO BUFFER RECORDS VAB
016700010305     C*----------------------------------------------------*
016800020305     C     WRIVAB        BEGSR
016900010305     C*
017000021017     C                   write     fivab000                                     => scarico il VAB
017100010305     C*
017200010305     C                   ENDSR
017300990920
017400000801     C*----------------------------------------------------*
017500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
017600000801     C*----------------------------------------------------*
017700010201     C     INZVAR        BEGSR
017800000801     C*
017900010201     C                   Z-ADD     *zeros        Num5_0
018000030107     C                   Z-ADD     *zeros        Num7_0
018100030711     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
018200030711     C                   MOVEL     '0'           FlgCAS            1
018300000801     C*
018400000801     C                   ENDSR
018500000801     C*----------------------------------------------------*
018600000801     C*  IMPOSTAZIONE CAMPI COSTANTI
018700000801     C*----------------------------------------------------*
018800000801     C     DEFCAM        BEGSR
018900040428     C*
019000040428     C                   CLEAR                   FIVAB000
019100000801     C*
019200020619     C* Imposto i valori di default...
019300110610     C                   Z-ADD     1660381       VABCCM
019400110610     C                   Z-ADD     1660381       VATCCM
019500021017     C                   Z-ADD     vlrpoi        VABFGS
019600021017     C                   Z-ADD     vlrpoi        VATFGS
019700110610     C                   Z-ADD     166           VABLNP
019800110610     C                   Z-ADD     166           VATLNP
019900040428     C                   Z-ADD     001           VABCTR
020000021017     C                   MOVEL     '7Q'          VABCTM
020100020305     C                   MOVEL     'E'           VATTRC
020200020619     C* ... e poi verifico se sono stati passati come parametri
020300020619     C                   IF        vlrppt > *blanks
020400040428     C                   IF        %subst(vlrppt:1:7) <> *blanks
020500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
020600020619     C                   EXSR      CHKNUM
020700020619     C                   IF        PiInt=*on
020800020619     C                   Z-ADD     PiVal         VABCCM
020900020619     C                   Z-ADD     PiVal         VATCCM
021000020619     C                   ENDIF
021100040428     C                   ENDIF
021200040428     C                   IF        %subst(vlrppt:8:3) <> *blanks
021300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
021400020619     C                   EXSR      CHKNUM
021500020619     C                   IF        PiInt=*on
021600020619     C                   Z-ADD     PiVal         VABLNP
021700020619     C                   Z-ADD     PiVal         VATLNP
021800020619     C                   ENDIF
021900040428     C                   ENDIF
022000040428     C                   IF        %subst(vlrppt:11:3) <> *blanks
022100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
022200020619     C                   EXSR      CHKNUM
022300020619     C                   IF        PiInt=*on
022400020619     C                   Z-ADD     PiVal         VABCTR
022500020619     C                   ENDIF
022600040428     C                   ENDIF
022700020619     C                   ENDIF
022800000801     C*
022900000801     C                   ENDSR
023000000801     C*----------------------------------------------------*
023100021017     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
023200000801     C*----------------------------------------------------*
023300010201     C     IMPVAB        BEGSR
023400000801     C*
023500020305     C                   EXSR      INZVAR
023600020305     C                   EXSR      DEFCAM
023700010305     C*
023800000801     C                   Z-ADD     *zeros        errore            1 0
023900000830     C                   MOVEL     datcor        VABAAS
024000020305     C                   MOVEL     datcor        VATAAS
024100000830     C                   MOVE      datcor        VABMGS
024200031117     C*
024300040428     C                   EVAL      VABRSD=%trim(%subst(vindta:145:35))
024400020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
024500020117     C     '@':'A'       XLATE     VABRSD        VABRSD
024600020117     C* ==
024700040428     C                   EVAL      VABRD2=%trim(%subst(vindta:180:15))
024800040428     C                   EVAL      VABIND=%trim(%subst(vindta:195:25))
024900040428     C                   EVAL      VABLOD=%trim(%subst(vindta:229:25))
025000110912     C                   EVAL      VABPRD=%trim(%subst(vindta:505:2))
025100110615     C                   EVAL      VABRMA=%trim(%subst(vindta:298:10))
025200030711     C* CAD
025300040428     C                   EVAL      PiStr=%trim(%subst(vindta:223:5))
025400010201     C                   EXSR      CHKNUM
025500010201     C                   IF        PiInt=*on
025600010201     C                   Z-ADD     PiVal         Num5_0
025700021017     C                   MOVEL     Num5_0        VABCAD
025800010201     C                   ELSE
025900010201     C                   ADD       1             errore
026000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
026100021017     C                             + ' ' + 'VABCAD'
026200010201     C                   ENDIF
026300030711     C* RMN
026400110615     C                   EVAL      PiStr=%trim(%subst(vindta:298:10))
026500010201     C                   EXSR      CHKNUM
026600010201     C                   IF        PiInt=*on
026700021017     C                   Z-ADD     PiVal         VABRMN
026800040428     C                   Z-ADD     PiVal         Num7_0
026900040428     C                   MOVE(P)   Num7_0        VABNSP
027000040428     C                   MOVE(P)   Num7_0        VATNSP
027100010201     C                   ELSE
027200010201     C                   ADD       1             errore
027300010201     C                   EVAL      vinmsg = %trimr(vinmsg)
027400040428     C                             + ' ' + 'VABRMN VABNSP VATNSP'
027500010201     C                   ENDIF
027600040428     C* NCL 1
027700040428     C                   EVAL      PiStr=%trim(%subst(vindta:338:4))
027800030107     C                   EXSR      CHKNUM
027900030107     C                   IF        PiInt=*on
028000040428     C                   ADD       PiVal         VABNCL
028100030107     C                   ELSE
028200030107     C                   ADD       1             errore
028300030107     C                   EVAL      vinmsg = %trimr(vinmsg)
028400030107     C                             + ' ' + 'VABNCL'
028500030107     C                   ENDIF
028600040428     C* NCL 2
028700040428     C                   EVAL      PiStr=%trim(%subst(vindta:369:4))
028800040428     C                   EXSR      CHKNUM
028900040428     C                   IF        PiInt=*on
029000040428     C                   ADD       PiVal         VABNCL
029100040428     C                   ELSE
029200040428     C                   ADD       1             errore
029300040428     C                   EVAL      vinmsg = %trimr(vinmsg)
029400040428     C                             + ' ' + 'VABNCL'
029500040428     C                   ENDIF
029600040428     C* NCL 3
029700040428     C                   EVAL      PiStr=%trim(%subst(vindta:400:4))
029800040428     C                   EXSR      CHKNUM
029900040428     C                   IF        PiInt=*on
030000040428     C                   ADD       PiVal         VABNCL
030100040428     C                   ELSE
030200040428     C                   ADD       1             errore
030300040428     C                   EVAL      vinmsg = %trimr(vinmsg)
030400040428     C                             + ' ' + 'VABNCL'
030500040428     C                   ENDIF
030600040428     C* NCL 4
030700040428     C                   EVAL      PiStr=%trim(%subst(vindta:431:4))
030800040428     C                   EXSR      CHKNUM
030900040428     C                   IF        PiInt=*on
031000040428     C                   ADD       PiVal         VABNCL
031100040428     C                   ELSE
031200040428     C                   ADD       1             errore
031300040428     C                   EVAL      vinmsg = %trimr(vinmsg)
031400040428     C                             + ' ' + 'VABNCL'
031500040428     C                   ENDIF
031600040428     C* PKB 1
031700040428     C                   EVAL      PiStr=%trim(%subst(vindta:362:5))
031800030107     C                   EXSR      CHKNUM
031900030107     C                   IF        PiNum=*on
032000040428     C                   EVAL      VABPKB = VABPKB + PiVal/10                   * gestisco 1 decim.
032100030107     C                   ELSE
032200030107     C                   ADD       1             errore
032300030107     C                   EVAL      vinmsg = %trimr(vinmsg)
032400030107     C                             + ' ' + 'VABPKB'
032500030107     C                   ENDIF
032600040428     C* PKB 2
032700040428     C                   EVAL      PiStr=%trim(%subst(vindta:393:5))
032800040428     C                   EXSR      CHKNUM
032900040428     C                   IF        PiNum=*on
033000040428     C                   EVAL      VABPKB = VABPKB + PiVal/10                   * gestisco 1 decim.
033100040428     C                   ELSE
033200040428     C                   ADD       1             errore
033300040428     C                   EVAL      vinmsg = %trimr(vinmsg)
033400040428     C                             + ' ' + 'VABPKB'
033500040428     C                   ENDIF
033600040428     C* PKB 3
033700040428     C                   EVAL      PiStr=%trim(%subst(vindta:424:5))
033800040428     C                   EXSR      CHKNUM
033900040428     C                   IF        PiNum=*on
034000040428     C                   EVAL      VABPKB = VABPKB + PiVal/10                   * gestisco 1 decim.
034100040428     C                   ELSE
034200040428     C                   ADD       1             errore
034300040428     C                   EVAL      vinmsg = %trimr(vinmsg)
034400040428     C                             + ' ' + 'VABPKB'
034500040428     C                   ENDIF
034600040428     C* PKB 4
034700040428     C                   EVAL      PiStr=%trim(%subst(vindta:455:5))
034800040428     C                   EXSR      CHKNUM
034900040428     C                   IF        PiNum=*on
035000040428     C                   EVAL      VABPKB = VABPKB + PiVal/10                   * gestisco 1 decim.
035100040428     C                   ELSE
035200040428     C                   ADD       1             errore
035300040428     C                   EVAL      vinmsg = %trimr(vinmsg)
035400040428     C                             + ' ' + 'VABPKB'
035500040428     C                   ENDIF
035600010205     C*
035700010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
035800040428     C                   IF        FlgCAS = '0'
035900031117     C                   EVAL      VABCBO = '1'                                 * franco  NO contr.
036000040428     C                   ELSE
036100040428     C                   EVAL      VABCBO = '4'                                 * franco  SI contr.
036200040428     C                   EVAL      VABVCA = 'EUR'
036300031117     C                   ENDIF
036400020305     C*
036500011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
036600011113     C                   EXSR      CHKIMPDIV
036700010202     C*
036800000801     C* Ebbene...
036900000801     C                   ADD       1             §CTRMO
037000010201     C                   IF        errore <> *zeros
037100000801     C                   ADD       1             §CTRNO
037200000801     C                   EVAL      vinflg = '2'
037300000801     C                   ELSE
037400010201     C                   ADD       1             §CTROKVB
037500000801     C                   ENDIF
037600000801     C*
037700000801     C                   ENDSR
037800010201     C*----------------------------------------------------*
037900021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
038000010201     C*----------------------------------------------------*
038100020305     C     WRIVAT        BEGSR
038200040428     C*
038300040428     C* X ogni collo indicato in testata
038400040428     C                   z-add     1             wColli            5 0
038500040428     C                   dow       wColli <= VABNCL
038600010201     C*
038700031117     C* Valorizzo il buffer di scrittura del FIVAT
038800050112     C                   move(p)   wColli        wColliAlfa        2
038900110615     C***                eval      VATNOT = '01'+%subst(vindta:298:10)+
039000110615     C***                                   wColliAlfa
039100110615     C                   eval      VATNOT = %subst(vindta:298:10)+
039200110615     C                                      wColliAlfa
039300021017     C                   write     FIVAT000
039400040428     C*
039500040428     C                   add       1             wColli
039600040428     C                   enddo
039700010201     C*
039800010201     C                   ENDSR
039900010202     C*----------------------------------------------------*
040000021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
040100010202     C*----------------------------------------------------*
040200020305     C     PREVAT        BEGSR
040300010202     C*
040400021017     C* Compongo il nome del membro da dare al FIVATWWR
040500010202     C                   eval      parmbr = vlrhdl
040600010202     C                   movel     'M'           parmbr
040700040428     C                   eval      parccm = '00052323'
040800010202     C                   eval      paropz = '1'
040900010202     C* Effettuo la chiamata al CLLE preposto
041000030711     C                   call(e)   'TITVVTC'
041100010202     C                   parm                    parccm
041200010202     C                   parm                    parmbr
041300010202     C                   parm                    paropz
041400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
041500010202     C                   if        %error
041600010202     C                   movel     '1'           chkcall
041700010202     C                   else
041800010202     C                   movel     '0'           chkcall
041900010202     C                   endif
042000010202     C*
042100010202     C                   ENDSR
042200000801     C*----------------------------------------------------*
042300000801     C*  CONTROLLO NUMERICITA' CAMPI
042400000801     C*----------------------------------------------------*
042500000801     C     CHKNUM        BEGSR
042600000801     C*
042700000801     C                   call(e)   'ISNUMERIC'
042800000801     C                   PARM                    PiStr            30
042900030711     C                   PARM      '.'           PiDecChr          1
043000000801     C                   PARM      *ZEROS        PiVal            30 9
043100000801     C                   PARM      '0'           PiInt             1
043200000801     C                   PARM      '0'           PiNum             1
043300000801     C                   IF        %error
043400000801     C                   EVAL      PiInt=*off
043500030107     C                   EVAL      PiNum=*off
043600000801     C                   ENDIF
043700000801     C*
043800000801     C                   ENDSR
043900000801     C***
044000000801
044100011113
044200011113     C*----------------------------------------------------*
044300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
044400011113     C*----------------------------------------------------*
044500011113     C     CHKIMPDIV     BEGSR
044600011113     C*
044700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
044800011113     C                   Z-ADD     *zeros        wrkDec            9 9
044900011113     C*
045000011113     C* Come prima cosa effettuo considerazioni sulla divisa
045100011113     C                   IF        vabIAS > *zeros
045200011113     C                   IF        vabVAS <> 'EUR'
045300011113     C                   EVAL      vabVAS =  'ITL'
045400011113     C                   ENDIF
045500011113     C                   ENDIF
045600011113     C*
045700011113     C                   IF        vabCAS > *zeros
045800011113     C                   IF        vabVCA <> 'EUR'
045900011113     C                   EVAL      vabVCA =  'ITL'
046000011113     C                   ENDIF
046100011113     C                   ENDIF
046200011113     C*
046300011113     C                   IF        vabVMD > *zeros
046400020305     C                   IF        vabVAD <> 'EUR'
046500011113     C                   EVAL      vabVAD =  'ITL'
046600011113     C                   ENDIF
046700011113     C                   ENDIF
046800011113     C*
046900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
047000011113     C                   Z-ADD     vabIAS        wrkDec
047100011113     C                   IF        wrkDec > *zeros
047200011113     C                   IF        vabVAS = 'ITL'
047300011113     C                   EVAL      vabIAS = *zeros
047400011113     C                   ENDIF
047500011113     C                   ENDIF
047600011113     C*
047700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
047800011113     C                   Z-ADD     vabCAS        wrkDec
047900011113     C                   IF        wrkDec > *zeros
048000011113     C                   IF        vabVCA = 'ITL'
048100011113     C                   EVAL      vabCAS = *zeros
048200011113     C                   ENDIF
048300011113     C                   ENDIF
048400011113     C*
048500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
048600011113     C                   Z-ADD     vabVMD        wrkDec
048700011113     C                   IF        wrkDec > *zeros
048800011113     C                   IF        vabVAD = 'ITL'
048900011113     C                   EVAL      vabVMD = *zeros
049000011113     C                   ENDIF
049100011113     C                   ENDIF
049200011113     C*
049300011113     C                   ENDSR
049400011113     C***
049500021017
049600000801
049700000801
049800990920      /TITLE Invio dei dati al punto operativo.
049900010202     C     invio         BEGSR
050000990920     C*
050100021017     C* 1° invio FIVAT
050200010201     C                   reset                   dscmz
050300021017     C                   move      vlrpoi        cmzdst
050400021017     C                   eval      cmzfld = 'FIVATWWR'
050500010201     C                   eval      cmzmbd = vlrhdl
050600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
050700021009     C***                if        prmfir = *blanks
050800021017     C                   eval      cmzfla = 'FIVAT00F'
050900021017     C                   eval      cmzmba = 'FIVAT00F'
051000021009     C***                else
051100021009     C***                eval      cmzfla = prmfir
051200021009     C***                eval      cmzmba = prmfir
051300021009     C***                endif
051400010201     C                   eval      cmznrr = *zeros
051500020305     C                   move      §ctrokvt      cmznrr
051600021018     C                   eval      cmzlba = vlrfl1
051700010201     C                   call(e)   'TIS711C'
051800010201     C                   parm                    dscmz
051900010201     C                   parm      *blanks       esito
052000010205     C                   if        %error
052100010205     C                             or cmzerr = '1'
052200010205     C                             or esito  = '1'
052300010205     C                   eval      wrkesito = '3'
052400010205     C                   else
052500010201     C*
052600021017     C* 2° invio FIVAB
052700010201     C                   reset                   dscmz
052800021017     C                   move      vlrpoi        cmzdst
052900010201     C                   eval      cmzfld = vlrfou
053000010201     C                   eval      cmzmbd = vlrhdl
053100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
053200021009     C***                if        prmfir = *blanks
053300021017     C                   eval      cmzfla = 'FIVAB00F'
053400021017     C                   eval      cmzmba = 'FIVAB00F'
053500021009     C***                else
053600021009     C***                eval      cmzfla = prmfir
053700021009     C***                eval      cmzmba = prmfir
053800021009     C***                endif
053900010201     C                   eval      cmznrr = *zeros
054000010201     C                   move      §ctrokvb      cmznrr
054100021018     C                   eval      cmzlba = vlrfl1
054200010201     C                   call(e)   'TIS711C'
054300010201     C                   parm                    dscmz
054400010201     C                   parm      *blanks       esito
054500010201     C                   if        %error
054600010201     C                             or cmzerr = '1'
054700010201     C                             or esito  = '1'
054800010201     C                   eval      wrkesito = '3'
054900010201     C                   endif
055000010205     C                   endif
055100990920     C*
055200000613     C                   ENDSR
055300000613     C***
055400070411
055500070411     C     *pssr         BEGSR
055600070411     C*
055700070411     C                   if        %open(tivin00r)
055800070411     C                   close     tivin00r
055900070411     C                   endif
056000070411     C                   if        %open(fivabwwr)
056100070411     C                   close     fivabwwr
056200070411     C                   endif
056300070411     C                   if        %open(fivatwwr)
056400070411     C                   close     fivatwwr
056500070411     C                   endif
056600070411     C*
056700070411     C* Effettuo la chiamata al CLLE preposto
056800070411     C                   call(e)   'TITVVTC'
056900070411     C                   parm                    parccm
057000070411     C                   parm                    parmbr
057100070411     C                   parm      '2'           paropz
057200070411     C*
057300070411     C                   eval      wrkesito = '2'
057400070411     C*
057500070411     C                   seton                                        LR
057600070411     C*
057700070411     C                   ENDSR     '*CANCL'
057800070411     C***
057900990910
058000000613     C     *inzsr        BEGSR
058100990910     C*
058200990910     C     *entry        plist
058300990920     C                   parm                    tivlrds
058400990921     C                   parm      wrkesito      esito
058500000724     C                   parm                    prmlit
058600000710     C                   parm                    prmfir
058700000613     C*
058800000830     C* CALCOLA LA DATA CORRENTE
058900000830     C                   time                    wn14             14 0
059000000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
059100000830     C                   z-add     wn8           g08dat
059200000830     C                   z-add     *zeros        g08inv
059300000830     C                   movel     '0'           g08err
059400000830     C                   call      'XSRDA8'
059500000830     C                   parm                    wlbda8
059600000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
059700000830     C*
059800000613     C                   ENDSR
059900000613     C***
