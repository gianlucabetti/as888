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
025700020305     C*
025800050525     C                   EVAL      PiStr=%trim(%subst(vindta:183:10))
025900010201     C                   EXSR      CHKNUM
026000010201     C                   IF        PiInt=*on
026100010201     C                   Z-ADD     PiVal         Num5_0
026200021017     C                   MOVEL     Num5_0        VABCAD
026300010201     C                   ELSE
026400010201     C                   ADD       1             errore
026500010201     C                   EVAL      vinmsg = %trimr(vinmsg)
026600021017     C                             + ' ' + 'VABCAD'
026700010201     C                   ENDIF
026800020619     C*
026900021017     C                   EVAL      PiStr=%trim(%subst(vindta:1:7))
027000010201     C                   EXSR      CHKNUM
027100010201     C                   IF        PiInt=*on
027200011115     C                   Z-ADD     PiVal         VABNSP
027300020305     C                   Z-ADD     PiVal         VATNSP
027400021017     C                   Z-ADD     PiVal         VABRMN
027500010201     C                   ELSE
027600010201     C                   ADD       1             errore
027700010201     C                   EVAL      vinmsg = %trimr(vinmsg)
027800021017     C                             + ' ' + 'VABNSP VATNSP VABRMN'
027900010201     C                   ENDIF
028000081105     C*
028100081105     C* Forzature particolari x cod. 0022284
028200081105     C                   IF        VABCCM = 0022284
028300081111     C                   EVAL      VABNOT='RICHIESTA VARIAZIONE INDIRIZZO E'
028400081111     C                   EVAL      VABNT2='FERMO DEP AUTORIZZATO - NO GIACENZA'
028500081105     C                   ENDIF
028600010205     C*
028700010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
028800010205     C                   IF        VABCAS > *zeros
028900010205     C                   EVAL      VABCBO = '4'
029000010205     C                   ELSE
029100010205     C                   EVAL      VABCBO = '1'
029200010205     C                   ENDIF
029300020305     C*
029400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
029500011113     C                   EXSR      CHKIMPDIV
029600010202     C*
029700000801     C* Ebbene...
029800000801     C                   ADD       1             §CTRMO
029900010201     C                   IF        errore <> *zeros
030000000801     C                   ADD       1             §CTRNO
030100000801     C                   EVAL      vinflg = '2'
030200000801     C                   ELSE
030300010201     C                   ADD       1             §CTROKVB
030400000801     C                   ENDIF
030500000801     C*
030600000801     C                   ENDSR
030700010201     C*----------------------------------------------------*
030800021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
030900010201     C*----------------------------------------------------*
031000020305     C     WRIVAT        BEGSR
031100010201     C*
031200021017     C* Valorizzo l buffer di scrittura del FIVAT
031300070626     C                   EVAL      VATTRC = 'E'
031400021017     C                   EVAL      VATNOT = %subst(vindta:8:35)
031500021017     C                   write     FIVAT000
031600070717     C                   exsr      wrivat_a
031700070717     C                   exsr      wrivat_b
031800141002     C                   exsr      wrivat_s
031900010201     C*
032000010201     C                   ENDSR
032100070626     C*----------------------------------------------------*
032200070626     C*  SCRITTURA ESTENSIONE FNVAT X INFO AGGUNTIVE DESTINATARIO
032300070626     C*----------------------------------------------------*
032400070626     C     WRIVAT_A      BEGSR
032500070626     C*
032600070626     C* Valorizzo l buffer di scrittura del FIVAT
032700070626     C                   EVAL      VATTRC = 'A'
032800070626     C                   EVAL      VATNOT = %subst(vindta:205:20)
032900080730     C                   IF        VATNOT = *blanks
033000080730     C                   EVAL      VATNOT = '.'
033100070717     C                   ENDIF
033200080730     C                   write     FIVAT000
033300070626     C*
033400070626     C                   ENDSR
033500070626     C*----------------------------------------------------*
033600070626     C*  SCRITTURA ESTENSIONE FNVAT X INFO AGGUNTIVE DESTINATARIO
033700070626     C*----------------------------------------------------*
033800070626     C     WRIVAT_B      BEGSR
033900070626     C*
034000070626     C* Valorizzo l buffer di scrittura del FIVAT
034100070626     C                   EVAL      VATTRC = 'B'
034200070626     C                   EVAL      VATNOT = %subst(vindta:225:20)
034300080730     C                   IF        VATNOT = *blanks
034400080730     C                   EVAL      VATNOT = '.'
034500080730     C                   ENDIF
034600080730     C                   write     FIVAT000
034700070626     C*
034800070626     C                   ENDSR
034900141002     C*----------------------------------------------------*
035000141002     C*  SCRITTURA ESTENSIONE FNVAT X alert SMS
035100141002     C*----------------------------------------------------*
035200141002     C     WRIVAT_S      BEGSR
035300141002     C*
035400141002     C* Valorizzo l buffer di scrittura del FIVAT
035500141002     C                   EVAL      VATTRC = 'S'
035600170405     C                   EVAL      VATNOT = %subst(vindta:225:20)
035700141002     C                   EVAL      %subst(VATNOT:17:2) = *blank
035800141002     C                   IF        VATNOT <> *blanks
035900141002     C                   write     FIVAT000
036000141002     C                   ENDIF
036100141002     C*
036200141002     C                   ENDSR
036300010202     C*----------------------------------------------------*
036400021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
036500010202     C*----------------------------------------------------*
036600020305     C     PREVAT        BEGSR
036700010202     C*
036800021017     C* Compongo il nome del membro da dare al FIVATWWR
036900010202     C                   eval      parmbr = vlrhdl
037000010202     C                   movel     'M'           parmbr
037100021017     C                   eval      parccm = '00020950'
037200010202     C                   eval      paropz = '1'
037300010202     C* Effettuo la chiamata al CLLE preposto
037400050128     C                   call(e)   'TITVVTC'
037500010202     C                   parm                    parccm
037600010202     C                   parm                    parmbr
037700010202     C                   parm                    paropz
037800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
037900010202     C                   if        %error
038000010202     C                   movel     '1'           chkcall
038100010202     C                   else
038200010202     C                   movel     '0'           chkcall
038300010202     C                   endif
038400010202     C*
038500010202     C                   ENDSR
038600000801     C*----------------------------------------------------*
038700000801     C*  CONTROLLO NUMERICITA' CAMPI
038800000801     C*----------------------------------------------------*
038900000801     C     CHKNUM        BEGSR
039000000801     C*
039100000801     C                   call(e)   'ISNUMERIC'
039200000801     C                   PARM                    PiStr            30
039300020619     C                   PARM      '.'           PiDecChr          1
039400000801     C                   PARM      *ZEROS        PiVal            30 9
039500000801     C                   PARM      '0'           PiInt             1
039600000801     C                   PARM      '0'           PiNum             1
039700000801     C                   IF        %error
039800000801     C                   EVAL      PiInt=*off
039900000801     C                   ENDIF
040000000801     C*
040100000801     C                   ENDSR
040200000801     C***
040300000801
040400011113
040500011113     C*----------------------------------------------------*
040600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
040700011113     C*----------------------------------------------------*
040800011113     C     CHKIMPDIV     BEGSR
040900011113     C*
041000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
041100011113     C                   Z-ADD     *zeros        wrkDec            9 9
041200011113     C*
041300011113     C* Come prima cosa effettuo considerazioni sulla divisa
041400011113     C                   IF        vabIAS > *zeros
041500011113     C                   IF        vabVAS <> 'EUR'
041600011113     C                   EVAL      vabVAS =  'ITL'
041700011113     C                   ENDIF
041800011113     C                   ENDIF
041900011113     C*
042000011113     C                   IF        vabCAS > *zeros
042100011113     C                   IF        vabVCA <> 'EUR'
042200011113     C                   EVAL      vabVCA =  'ITL'
042300011113     C                   ENDIF
042400011113     C                   ENDIF
042500011113     C*
042600011113     C                   IF        vabVMD > *zeros
042700020305     C                   IF        vabVAD <> 'EUR'
042800011113     C                   EVAL      vabVAD =  'ITL'
042900011113     C                   ENDIF
043000011113     C                   ENDIF
043100011113     C*
043200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
043300011113     C                   Z-ADD     vabIAS        wrkDec
043400011113     C                   IF        wrkDec > *zeros
043500011113     C                   IF        vabVAS = 'ITL'
043600011113     C                   EVAL      vabIAS = *zeros
043700011113     C                   ENDIF
043800011113     C                   ENDIF
043900011113     C*
044000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
044100011113     C                   Z-ADD     vabCAS        wrkDec
044200011113     C                   IF        wrkDec > *zeros
044300011113     C                   IF        vabVCA = 'ITL'
044400011113     C                   EVAL      vabCAS = *zeros
044500011113     C                   ENDIF
044600011113     C                   ENDIF
044700011113     C*
044800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
044900011113     C                   Z-ADD     vabVMD        wrkDec
045000011113     C                   IF        wrkDec > *zeros
045100011113     C                   IF        vabVAD = 'ITL'
045200011113     C                   EVAL      vabVMD = *zeros
045300011113     C                   ENDIF
045400011113     C                   ENDIF
045500011113     C*
045600011113     C                   ENDSR
045700011113     C***
045800021017
045900000801
046000000801
046100990920      /TITLE Invio dei dati al punto operativo.
046200010202     C     invio         BEGSR
046300990920     C*
046400021017     C* 1° invio FIVAT
046500010201     C                   reset                   dscmz
046600021017     C                   move      vlrpoi        cmzdst
046700021017     C                   eval      cmzfld = 'FIVATWWR'
046800010201     C                   eval      cmzmbd = vlrhdl
046900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
047000021009     C***                if        prmfir = *blanks
047100021017     C                   eval      cmzfla = 'FIVAT00F'
047200021017     C                   eval      cmzmba = 'FIVAT00F'
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
048900021017     C* 2° invio FIVAB
049000010201     C                   reset                   dscmz
049100021017     C                   move      vlrpoi        cmzdst
049200010201     C                   eval      cmzfld = vlrfou
049300010201     C                   eval      cmzmbd = vlrhdl
049400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
049500021009     C***                if        prmfir = *blanks
049600021017     C                   eval      cmzfla = 'FIVAB00F'
049700021017     C                   eval      cmzmba = 'FIVAB00F'
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
