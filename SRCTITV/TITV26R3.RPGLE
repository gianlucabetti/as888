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
002000020305     D depspe          s             14    INZ(*blanks)
002100010213     D depdat          s              8  0 INZ(*zeros)
002200020305     D curspe          s             14    INZ(*blanks)
002300010202     D parccm          s              8    INZ(*blanks)
002400010202     D parmbr          s             10    INZ(*blanks)
002500010202     D paropz          s              1    INZ(*blanks)
002600010202     D chkcall         s              1    INZ(*blanks)
002700010213
002800000801     D  Num5_0         s              5  0
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
004600021017     C                   EXSR      RWMEDIOL                                     LETT/SCR. VAB
004700000613     C*
004800010202     C* Effettuo la chiamata al CLLE preposto
004900050128     C                   call(e)   'TITVVTC'
005000010202     C                   parm                    parccm
005100010202     C                   parm                    parmbr
005200010202     C                   parm      '2'           paropz
005300000616     C*
005400000801     C
005500010201     C                   seton                                        LR
005600990908
005700000801
005800910830     C*--------------------------------------------------------
005900021017     C* RWMEDIOL LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
006000910830     C*--------------------------------------------------------
006100021017     C     RWMEDIOL      BEGSR
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
007200010202     C                   if        chkcall = '0'
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
009800021017     C                   EVAL      PiStr=%trim(%subst(vindta:1:7))
009900020305     C                   MOVEL(p)  PiStr         curspe
010000010305     C*
010100010305     C                   if        depspe = *blanks                             => 1° giro
010200010305     C                   eval      depspe = curspe                              => memorizz. spediz
010300020305     C                   exsr      impvab
010400020305     C                   exsr      wrivat                                       => carico VAT
010500010305     C                   else
010600010305     C                   if        curspe <> depspe                             => rottura di spediz
010700010305     C                   eval      depspe = curspe                              => memorizz. spediz
010800020305     C                   exsr      wrivab
010900020305     C                   exsr      impvab
011000020305     C                   exsr      wrivat                                       => carico VAT
011100020305     C                   else                                                   => x stessa spediz
011200020305     C                   exsr      impvab
011300020305     C                   exsr      wrivat                                       => carico VAT
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
018000000801     C*
018100000801     C                   ENDSR
018200000801     C*----------------------------------------------------*
018300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
018400000801     C*----------------------------------------------------*
018500000801     C     DEFCAM        BEGSR
018600000801     C*
018700021017     C                   CLEAR                   FIVAB000
018800020619     C* Imposto i valori di default...
018900021017     C                   Z-ADD     0020950       VABCCM
019000021017     C                   Z-ADD     0020950       VATCCM
019100021017     C                   Z-ADD     vlrpoi        VABFGS
019200021017     C                   Z-ADD     vlrpoi        VATFGS
019300021017     C                   Z-ADD     002           VABLNP
019400021017     C                   Z-ADD     002           VATLNP
019500021017     C                   Z-ADD     300           VABCTR
019600021017     C                   Z-ADD     1             VABNCL
019700021017     C                   Z-ADD     1             VABPKB
019800021017     C                   MOVEL     '7Q'          VABCTM
019900021120     C***                MOVEL     'A'           VABTC1
020000020305     C                   MOVEL     'E'           VATTRC
020100020619     C* ... e poi verifico se sono stati passati come parametri
020200020619     C                   IF        vlrppt > *blanks
020300101124     C                   IF        %subst(vlrppt:1:7) <> *blanks
020400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
020500020619     C                   EXSR      CHKNUM
020600020619     C                   IF        PiInt=*on
020700020619     C                   Z-ADD     PiVal         VABCCM
020800020619     C                   Z-ADD     PiVal         VATCCM
020900020619     C                   ENDIF
021000101124     C                   ENDIF
021100101124     C                   IF        %subst(vlrppt:8:3) <> *blanks
021200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
021300020619     C                   EXSR      CHKNUM
021400020619     C                   IF        PiInt=*on
021500020619     C                   Z-ADD     PiVal         VABLNP
021600020619     C                   Z-ADD     PiVal         VATLNP
021700020619     C                   ENDIF
021800101124     C                   ENDIF
021900101124     C                   IF        %subst(vlrppt:11:3) <> *blanks
022000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
022100020619     C                   EXSR      CHKNUM
022200020619     C                   IF        PiInt=*on
022300020619     C                   Z-ADD     PiVal         VABCTR
022400020619     C                   ENDIF
022500101124     C                   ENDIF
022600101124     C                   IF        %subst(vlrppt:14:1) <> *blanks
022700101124     C                   EVAL      VABTC1 = %subst(vlrppt:14:1)
022800101124     C                   ENDIF
022900020619     C                   ENDIF
023000000801     C*
023100000801     C                   ENDSR
023200000801     C*----------------------------------------------------*
023300021017     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
023400000801     C*----------------------------------------------------*
023500010201     C     IMPVAB        BEGSR
023600000801     C*
023700020305     C                   EXSR      INZVAR
023800020305     C                   EXSR      DEFCAM
023900010305     C*
024000000801     C                   Z-ADD     *zeros        errore            1 0
024100000830     C                   MOVEL     datcor        VABAAS
024200020305     C                   MOVEL     datcor        VATAAS
024300000830     C                   MOVE      datcor        VABMGS
024400021017     C                   EVAL      VABRSD=%trim(%subst(vindta:43:35))
024500020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
024600020117     C     '@':'A'       XLATE     VABRSD        VABRSD
024700020117     C* ==
024800081017     C                   EVAL      VABRD2='Fraz. '+%trim(%subst(vindta:335:35))
024900081017     C                   EVAL      VABIND=%trim(%subst(vindta:113:35))+' '+
025000081017     C                                    %trim(%subst(vindta:370:40))
025100050525     C                   EVAL      VABLOD=%trim(%subst(vindta:148:35))
025200050525     C                   EVAL      VABPRD=%trim(%subst(vindta:193:2))
025300050915     C                   EVALR     VABRMA=%trim(%subst(vindta:8:35))
025400070626     C                   EVAL      VABNOT='Ag. ' +
025500070626     C                                    %trim(%subst(vindta:245:70)) + ' ' +
025600070626     C                                    %trim(%subst(vindta:315:20))
025700111114     C* TC1
025800111114     C                   SELECT
025900111114     C                   WHEN      %subst(vindta:1141:1) = '0'
026000111114     C                   EVAL      VABTC1 = 'A'
026100111114     C                   ENDSL
026200020305     C*
026300050525     C                   EVAL      PiStr=%trim(%subst(vindta:183:10))
026400010201     C                   EXSR      CHKNUM
026500010201     C                   IF        PiInt=*on
026600010201     C                   Z-ADD     PiVal         Num5_0
026700021017     C                   MOVEL     Num5_0        VABCAD
026800010201     C                   ELSE
026900010201     C                   ADD       1             errore
027000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
027100021017     C                             + ' ' + 'VABCAD'
027200010201     C                   ENDIF
027300101202     C*
027400101202     C                   EVAL      PiStr=%trim(%subst(vindta:1:7))
027500101202     C                   EXSR      CHKNUM
027600101202     C                   IF        PiInt=*on
027700101202     C                   Z-ADD     PiVal         VABNSP
027800101202     C                   Z-ADD     PiVal         VATNSP
027900101202     C                   ELSE
028000101202     C                   ADD       1             errore
028100101202     C                   EVAL      vinmsg = %trimr(vinmsg)
028200101202     C                             + ' ' + 'VABNSP VATNSP'
028300101202     C                   ENDIF
028400020619     C*
028500101202     C                   EVAL      PiStr=%trim(%subst(vindta:411:10))
028600010201     C                   EXSR      CHKNUM
028700010201     C                   IF        PiInt=*on
028800021017     C                   Z-ADD     PiVal         VABRMN
028900010201     C                   ELSE
029000010201     C                   ADD       1             errore
029100010201     C                   EVAL      vinmsg = %trimr(vinmsg)
029200101202     C                             + ' ' + 'VABRMN'
029300010201     C                   ENDIF
029400081105     C*
029500110124     C* Forzature particolari x cod. 0022284 - 0023078
029600110124     C                   IF        VABCCM = 0022284 or VABCCM = 0023078
029700081111     C                   EVAL      VABNOT='RICHIESTA VARIAZIONE INDIRIZZO E'
029800081111     C                   EVAL      VABNT2='FERMO DEP AUTORIZZATO - NO GIACENZA'
029900081105     C                   ENDIF
030000010205     C*
030100010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
030200010205     C                   IF        VABCAS > *zeros
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
032200021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
032300010201     C*----------------------------------------------------*
032400020305     C     WRIVAT        BEGSR
032500010201     C*
032600021017     C* Valorizzo l buffer di scrittura del FIVAT
032700070626     C                   EVAL      VATTRC = 'E'
032800021017     C                   EVAL      VATNOT = %subst(vindta:8:35)
032900021017     C                   write     FIVAT000
033000070717     C                   exsr      wrivat_a
033100070717     C                   exsr      wrivat_b
033200010201     C*
033300010201     C                   ENDSR
033400070626     C*----------------------------------------------------*
033500070626     C*  SCRITTURA ESTENSIONE FNVAT X INFO AGGUNTIVE DESTINATARIO
033600070626     C*----------------------------------------------------*
033700070626     C     WRIVAT_A      BEGSR
033800070626     C*
033900070626     C* Valorizzo l buffer di scrittura del FIVAT
034000070626     C                   EVAL      VATTRC = 'A'
034100070626     C                   EVAL      VATNOT = %subst(vindta:205:20)
034200080730     C                   IF        VATNOT = *blanks
034300080730     C                   EVAL      VATNOT = '.'
034400070717     C                   ENDIF
034500080730     C                   write     FIVAT000
034600070626     C*
034700070626     C                   ENDSR
034800070626     C*----------------------------------------------------*
034900070626     C*  SCRITTURA ESTENSIONE FNVAT X INFO AGGUNTIVE DESTINATARIO
035000070626     C*----------------------------------------------------*
035100070626     C     WRIVAT_B      BEGSR
035200070626     C*
035300070626     C* Valorizzo l buffer di scrittura del FIVAT
035400070626     C                   EVAL      VATTRC = 'B'
035500070626     C                   EVAL      VATNOT = %subst(vindta:225:20)
035600080730     C                   IF        VATNOT = *blanks
035700080730     C                   EVAL      VATNOT = '.'
035800080730     C                   ENDIF
035900080730     C                   write     FIVAT000
036000070626     C*
036100070626     C                   ENDSR
036200010202     C*----------------------------------------------------*
036300021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
036400010202     C*----------------------------------------------------*
036500020305     C     PREVAT        BEGSR
036600010202     C*
036700021017     C* Compongo il nome del membro da dare al FIVATWWR
036800010202     C                   eval      parmbr = vlrhdl
036900010202     C                   movel     'M'           parmbr
037000021017     C                   eval      parccm = '00020950'
037100010202     C                   eval      paropz = '1'
037200010202     C* Effettuo la chiamata al CLLE preposto
037300050128     C                   call(e)   'TITVVTC'
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
039200020619     C                   PARM      '.'           PiDecChr          1
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
045700021017
045800000801
045900000801
046000990920      /TITLE Invio dei dati al punto operativo.
046100010202     C     invio         BEGSR
046200990920     C*
046300021017     C* 1° invio FIVAT
046400010201     C                   reset                   dscmz
046500021017     C                   move      vlrpoi        cmzdst
046600021017     C                   eval      cmzfld = 'FIVATWWR'
046700010201     C                   eval      cmzmbd = vlrhdl
046800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
046900021009     C***                if        prmfir = *blanks
047000021017     C                   eval      cmzfla = 'FIVAT00F'
047100021017     C                   eval      cmzmba = 'FIVAT00F'
047200021009     C***                else
047300021009     C***                eval      cmzfla = prmfir
047400021009     C***                eval      cmzmba = prmfir
047500021009     C***                endif
047600010201     C                   eval      cmznrr = *zeros
047700020305     C                   move      §ctrokvt      cmznrr
047800021018     C                   eval      cmzlba = vlrfl1
047900010201     C                   call(e)   'TIS711C'
048000010201     C                   parm                    dscmz
048100010201     C                   parm      *blanks       esito
048200010205     C                   if        %error
048300010205     C                             or cmzerr = '1'
048400010205     C                             or esito  = '1'
048500010205     C                   eval      wrkesito = '3'
048600010205     C                   else
048700010201     C*
048800021017     C* 2° invio FIVAB
048900010201     C                   reset                   dscmz
049000021017     C                   move      vlrpoi        cmzdst
049100010201     C                   eval      cmzfld = vlrfou
049200010201     C                   eval      cmzmbd = vlrhdl
049300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
049400021009     C***                if        prmfir = *blanks
049500021017     C                   eval      cmzfla = 'FIVAB00F'
049600021017     C                   eval      cmzmba = 'FIVAB00F'
049700021009     C***                else
049800021009     C***                eval      cmzfla = prmfir
049900021009     C***                eval      cmzmba = prmfir
050000021009     C***                endif
050100010201     C                   eval      cmznrr = *zeros
050200010201     C                   move      §ctrokvb      cmznrr
050300021018     C                   eval      cmzlba = vlrfl1
050400010201     C                   call(e)   'TIS711C'
050500010201     C                   parm                    dscmz
050600010201     C                   parm      *blanks       esito
050700010201     C                   if        %error
050800010201     C                             or cmzerr = '1'
050900010201     C                             or esito  = '1'
051000010201     C                   eval      wrkesito = '3'
051100010201     C                   endif
051200010205     C                   endif
051300990920     C*
051400000613     C                   ENDSR
051500000613     C***
051600070411
051700070411     C     *pssr         BEGSR
051800070411     C*
051900070411     C                   if        %open(tivin00r)
052000070411     C                   close     tivin00r
052100070411     C                   endif
052200070411     C                   if        %open(fivabwwr)
052300070411     C                   close     fivabwwr
052400070411     C                   endif
052500070411     C                   if        %open(fivatwwr)
052600070411     C                   close     fivatwwr
052700070411     C                   endif
052800070411     C*
052900070411     C* Effettuo la chiamata al CLLE preposto
053000070411     C                   call(e)   'TITVVTC'
053100070411     C                   parm                    parccm
053200070411     C                   parm                    parmbr
053300070411     C                   parm      '2'           paropz
053400070411     C*
053500070411     C                   eval      wrkesito = '2'
053600070411     C*
053700070411     C                   seton                                        LR
053800070411     C*
053900070411     C                   ENDSR     '*CANCL'
054000070411     C***
054100070411
054200990910
054300000613     C     *inzsr        BEGSR
054400990910     C*
054500990910     C     *entry        plist
054600990920     C                   parm                    tivlrds
054700990921     C                   parm      wrkesito      esito
054800000724     C                   parm                    prmlit
054900000710     C                   parm                    prmfir
055000000613     C*
055100000830     C* CALCOLA LA DATA CORRENTE
055200000830     C                   time                    wn14             14 0
055300000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
055400000830     C                   z-add     wn8           g08dat
055500000830     C                   z-add     *zeros        g08inv
055600000830     C                   movel     '0'           g08err
055700000830     C                   call      'XSRDA8'
055800000830     C                   parm                    wlbda8
055900000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
056000000830     C*
056100000613     C                   ENDSR
056200000613     C***
