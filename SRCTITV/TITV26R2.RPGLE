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
020300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
020400020619     C                   EXSR      CHKNUM
020500020619     C                   IF        PiInt=*on
020600020619     C                   Z-ADD     PiVal         VABCCM
020700020619     C                   Z-ADD     PiVal         VATCCM
020800020619     C                   ENDIF
020900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
021000020619     C                   EXSR      CHKNUM
021100020619     C                   IF        PiInt=*on
021200020619     C                   Z-ADD     PiVal         VABLNP
021300020619     C                   Z-ADD     PiVal         VATLNP
021400020619     C                   ENDIF
021500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
021600020619     C                   EXSR      CHKNUM
021700020619     C                   IF        PiInt=*on
021800020619     C                   Z-ADD     PiVal         VABCTR
021900020619     C                   ENDIF
022000020619     C                   ENDIF
022100000801     C*
022200000801     C                   ENDSR
022300000801     C*----------------------------------------------------*
022400021017     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
022500000801     C*----------------------------------------------------*
022600010201     C     IMPVAB        BEGSR
022700000801     C*
022800020305     C                   EXSR      INZVAR
022900020305     C                   EXSR      DEFCAM
023000010305     C*
023100000801     C                   Z-ADD     *zeros        errore            1 0
023200000830     C                   MOVEL     datcor        VABAAS
023300020305     C                   MOVEL     datcor        VATAAS
023400000830     C                   MOVE      datcor        VABMGS
023500021017     C                   EVAL      VABRSD=%trim(%subst(vindta:43:35))
023600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
023700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
023800020117     C* ==
023900090122     C                   IF        VABCCM = 0391021
024000090122     C                   EVAL      VABRD2=%trim(%subst(vindta:43+35:70-35))
024100090122     C                   ELSE
024200081017     C                   EVAL      VABRD2='Fraz. '+%trim(%subst(vindta:335:35))
024300090122     C                   ENDIF
024400081017     C                   EVAL      VABIND=%trim(%subst(vindta:113:35))+' '+
024500081017     C                                    %trim(%subst(vindta:370:40))
024600050525     C                   EVAL      VABLOD=%trim(%subst(vindta:148:35))
024700050525     C                   EVAL      VABPRD=%trim(%subst(vindta:193:2))
024800050915     C                   EVALR     VABRMA=%trim(%subst(vindta:8:35))
024900070626     C                   EVAL      VABNOT='Ag. ' +
025000070626     C                                    %trim(%subst(vindta:245:70)) + ' ' +
025100070626     C                                    %trim(%subst(vindta:315:20))
025200020305     C*
025300050525     C                   EVAL      PiStr=%trim(%subst(vindta:183:10))
025400010201     C                   EXSR      CHKNUM
025500010201     C                   IF        PiInt=*on
025600010201     C                   Z-ADD     PiVal         Num5_0
025700021017     C                   MOVEL     Num5_0        VABCAD
025800010201     C                   ELSE
025900010201     C                   ADD       1             errore
026000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
026100021017     C                             + ' ' + 'VABCAD'
026200010201     C                   ENDIF
026300020619     C*
026400021017     C                   EVAL      PiStr=%trim(%subst(vindta:1:7))
026500010201     C                   EXSR      CHKNUM
026600010201     C                   IF        PiInt=*on
026700011115     C                   Z-ADD     PiVal         VABNSP
026800020305     C                   Z-ADD     PiVal         VATNSP
026900021017     C                   Z-ADD     PiVal         VABRMN
027000010201     C                   ELSE
027100010201     C                   ADD       1             errore
027200010201     C                   EVAL      vinmsg = %trimr(vinmsg)
027300021017     C                             + ' ' + 'VABNSP VATNSP VABRMN'
027400010201     C                   ENDIF
027500081105     C*
027600081105     C* Forzature particolari x cod. 0022284
027700081105     C                   IF        VABCCM = 0022284
027800081111     C                   EVAL      VABNOT='RICHIESTA VARIAZIONE INDIRIZZO E'
027900081111     C                   EVAL      VABNT2='FERMO DEP AUTORIZZATO - NO GIACENZA'
028000081105     C                   ENDIF
028100010205     C*
028200010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
028300010205     C                   IF        VABCAS > *zeros
028400010205     C                   EVAL      VABCBO = '4'
028500010205     C                   ELSE
028600010205     C                   EVAL      VABCBO = '1'
028700010205     C                   ENDIF
028800020305     C*
028900011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
029000011113     C                   EXSR      CHKIMPDIV
029100010202     C*
029200000801     C* Ebbene...
029300000801     C                   ADD       1             §CTRMO
029400010201     C                   IF        errore <> *zeros
029500000801     C                   ADD       1             §CTRNO
029600000801     C                   EVAL      vinflg = '2'
029700000801     C                   ELSE
029800010201     C                   ADD       1             §CTROKVB
029900000801     C                   ENDIF
030000000801     C*
030100000801     C                   ENDSR
030200010201     C*----------------------------------------------------*
030300021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
030400010201     C*----------------------------------------------------*
030500020305     C     WRIVAT        BEGSR
030600010201     C*
030700021017     C* Valorizzo l buffer di scrittura del FIVAT
030800070626     C                   EVAL      VATTRC = 'E'
030900021017     C                   EVAL      VATNOT = %subst(vindta:8:35)
031000021017     C                   write     FIVAT000
031100070717     C                   exsr      wrivat_a
031200070717     C                   exsr      wrivat_b
031300010201     C*
031400010201     C                   ENDSR
031500070626     C*----------------------------------------------------*
031600070626     C*  SCRITTURA ESTENSIONE FNVAT X INFO AGGUNTIVE DESTINATARIO
031700070626     C*----------------------------------------------------*
031800070626     C     WRIVAT_A      BEGSR
031900070626     C*
032000070626     C* Valorizzo l buffer di scrittura del FIVAT
032100070626     C                   EVAL      VATTRC = 'A'
032200070626     C                   EVAL      VATNOT = %subst(vindta:205:20)
032300080730     C                   IF        VATNOT = *blanks
032400080730     C                   EVAL      VATNOT = '.'
032500070717     C                   ENDIF
032600080730     C                   write     FIVAT000
032700070626     C*
032800070626     C                   ENDSR
032900070626     C*----------------------------------------------------*
033000070626     C*  SCRITTURA ESTENSIONE FNVAT X INFO AGGUNTIVE DESTINATARIO
033100070626     C*----------------------------------------------------*
033200070626     C     WRIVAT_B      BEGSR
033300070626     C*
033400070626     C* Valorizzo l buffer di scrittura del FIVAT
033500070626     C                   EVAL      VATTRC = 'B'
033600070626     C                   EVAL      VATNOT = %subst(vindta:225:20)
033700080730     C                   IF        VATNOT = *blanks
033800080730     C                   EVAL      VATNOT = '.'
033900080730     C                   ENDIF
034000080730     C                   write     FIVAT000
034100070626     C*
034200070626     C                   ENDSR
034300010202     C*----------------------------------------------------*
034400021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
034500010202     C*----------------------------------------------------*
034600020305     C     PREVAT        BEGSR
034700010202     C*
034800021017     C* Compongo il nome del membro da dare al FIVATWWR
034900010202     C                   eval      parmbr = vlrhdl
035000010202     C                   movel     'M'           parmbr
035100021017     C                   eval      parccm = '00020950'
035200010202     C                   eval      paropz = '1'
035300010202     C* Effettuo la chiamata al CLLE preposto
035400050128     C                   call(e)   'TITVVTC'
035500010202     C                   parm                    parccm
035600010202     C                   parm                    parmbr
035700010202     C                   parm                    paropz
035800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
035900010202     C                   if        %error
036000010202     C                   movel     '1'           chkcall
036100010202     C                   else
036200010202     C                   movel     '0'           chkcall
036300010202     C                   endif
036400010202     C*
036500010202     C                   ENDSR
036600000801     C*----------------------------------------------------*
036700000801     C*  CONTROLLO NUMERICITA' CAMPI
036800000801     C*----------------------------------------------------*
036900000801     C     CHKNUM        BEGSR
037000000801     C*
037100000801     C                   call(e)   'ISNUMERIC'
037200000801     C                   PARM                    PiStr            30
037300020619     C                   PARM      '.'           PiDecChr          1
037400000801     C                   PARM      *ZEROS        PiVal            30 9
037500000801     C                   PARM      '0'           PiInt             1
037600000801     C                   PARM      '0'           PiNum             1
037700000801     C                   IF        %error
037800000801     C                   EVAL      PiInt=*off
037900000801     C                   ENDIF
038000000801     C*
038100000801     C                   ENDSR
038200000801     C***
038300000801
038400011113
038500011113     C*----------------------------------------------------*
038600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
038700011113     C*----------------------------------------------------*
038800011113     C     CHKIMPDIV     BEGSR
038900011113     C*
039000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
039100011113     C                   Z-ADD     *zeros        wrkDec            9 9
039200011113     C*
039300011113     C* Come prima cosa effettuo considerazioni sulla divisa
039400011113     C                   IF        vabIAS > *zeros
039500011113     C                   IF        vabVAS <> 'EUR'
039600011113     C                   EVAL      vabVAS =  'ITL'
039700011113     C                   ENDIF
039800011113     C                   ENDIF
039900011113     C*
040000011113     C                   IF        vabCAS > *zeros
040100011113     C                   IF        vabVCA <> 'EUR'
040200011113     C                   EVAL      vabVCA =  'ITL'
040300011113     C                   ENDIF
040400011113     C                   ENDIF
040500011113     C*
040600011113     C                   IF        vabVMD > *zeros
040700020305     C                   IF        vabVAD <> 'EUR'
040800011113     C                   EVAL      vabVAD =  'ITL'
040900011113     C                   ENDIF
041000011113     C                   ENDIF
041100011113     C*
041200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
041300011113     C                   Z-ADD     vabIAS        wrkDec
041400011113     C                   IF        wrkDec > *zeros
041500011113     C                   IF        vabVAS = 'ITL'
041600011113     C                   EVAL      vabIAS = *zeros
041700011113     C                   ENDIF
041800011113     C                   ENDIF
041900011113     C*
042000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
042100011113     C                   Z-ADD     vabCAS        wrkDec
042200011113     C                   IF        wrkDec > *zeros
042300011113     C                   IF        vabVCA = 'ITL'
042400011113     C                   EVAL      vabCAS = *zeros
042500011113     C                   ENDIF
042600011113     C                   ENDIF
042700011113     C*
042800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
042900011113     C                   Z-ADD     vabVMD        wrkDec
043000011113     C                   IF        wrkDec > *zeros
043100011113     C                   IF        vabVAD = 'ITL'
043200011113     C                   EVAL      vabVMD = *zeros
043300011113     C                   ENDIF
043400011113     C                   ENDIF
043500011113     C*
043600011113     C                   ENDSR
043700011113     C***
043800021017
043900000801
044000000801
044100990920      /TITLE Invio dei dati al punto operativo.
044200010202     C     invio         BEGSR
044300990920     C*
044400021017     C* 1° invio FIVAT
044500010201     C                   reset                   dscmz
044600021017     C                   move      vlrpoi        cmzdst
044700021017     C                   eval      cmzfld = 'FIVATWWR'
044800010201     C                   eval      cmzmbd = vlrhdl
044900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
045000021009     C***                if        prmfir = *blanks
045100021017     C                   eval      cmzfla = 'FIVAT00F'
045200021017     C                   eval      cmzmba = 'FIVAT00F'
045300021009     C***                else
045400021009     C***                eval      cmzfla = prmfir
045500021009     C***                eval      cmzmba = prmfir
045600021009     C***                endif
045700010201     C                   eval      cmznrr = *zeros
045800020305     C                   move      §ctrokvt      cmznrr
045900021018     C                   eval      cmzlba = vlrfl1
046000010201     C                   call(e)   'TIS711C'
046100010201     C                   parm                    dscmz
046200010201     C                   parm      *blanks       esito
046300010205     C                   if        %error
046400010205     C                             or cmzerr = '1'
046500010205     C                             or esito  = '1'
046600010205     C                   eval      wrkesito = '3'
046700010205     C                   else
046800010201     C*
046900021017     C* 2° invio FIVAB
047000010201     C                   reset                   dscmz
047100021017     C                   move      vlrpoi        cmzdst
047200010201     C                   eval      cmzfld = vlrfou
047300010201     C                   eval      cmzmbd = vlrhdl
047400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
047500021009     C***                if        prmfir = *blanks
047600021017     C                   eval      cmzfla = 'FIVAB00F'
047700021017     C                   eval      cmzmba = 'FIVAB00F'
047800021009     C***                else
047900021009     C***                eval      cmzfla = prmfir
048000021009     C***                eval      cmzmba = prmfir
048100021009     C***                endif
048200010201     C                   eval      cmznrr = *zeros
048300010201     C                   move      §ctrokvb      cmznrr
048400021018     C                   eval      cmzlba = vlrfl1
048500010201     C                   call(e)   'TIS711C'
048600010201     C                   parm                    dscmz
048700010201     C                   parm      *blanks       esito
048800010201     C                   if        %error
048900010201     C                             or cmzerr = '1'
049000010201     C                             or esito  = '1'
049100010201     C                   eval      wrkesito = '3'
049200010201     C                   endif
049300010205     C                   endif
049400990920     C*
049500000613     C                   ENDSR
049600000613     C***
049700070411
049800070411     C     *pssr         BEGSR
049900070411     C*
050000070411     C                   if        %open(tivin00r)
050100070411     C                   close     tivin00r
050200070411     C                   endif
050300070411     C                   if        %open(fivabwwr)
050400070411     C                   close     fivabwwr
050500070411     C                   endif
050600070411     C                   if        %open(fivatwwr)
050700070411     C                   close     fivatwwr
050800070411     C                   endif
050900070411     C*
051000070411     C* Effettuo la chiamata al CLLE preposto
051100070411     C                   call(e)   'TITVVTC'
051200070411     C                   parm                    parccm
051300070411     C                   parm                    parmbr
051400070411     C                   parm      '2'           paropz
051500070411     C*
051600070411     C                   eval      wrkesito = '2'
051700070411     C*
051800070411     C                   seton                                        LR
051900070411     C*
052000070411     C                   ENDSR     '*CANCL'
052100070411     C***
052200070411
052300990910
052400000613     C     *inzsr        BEGSR
052500990910     C*
052600990910     C     *entry        plist
052700990920     C                   parm                    tivlrds
052800990921     C                   parm      wrkesito      esito
052900000724     C                   parm                    prmlit
053000000710     C                   parm                    prmfir
053100000613     C*
053200000830     C* CALCOLA LA DATA CORRENTE
053300000830     C                   time                    wn14             14 0
053400000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
053500000830     C                   z-add     wn8           g08dat
053600000830     C                   z-add     *zeros        g08inv
053700000830     C                   movel     '0'           g08err
053800000830     C                   call      'XSRDA8'
053900000830     C                   parm                    wlbda8
054000000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
054100000830     C*
054200000613     C                   ENDSR
054300000613     C***
