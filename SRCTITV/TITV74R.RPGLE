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
002000031117     D depspe          s              6    INZ(*blanks)
002100031117     D curspe          s              6    INZ(*blanks)
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
004600031117     C                   EXSR      RWLFSPA                                      LETT/SCR. VAB/VAT
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
005900031117     C* RWLFSPA   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR *
006000910830     C*--------------------------------------------------------
006100031117     C     RWLFSPA       BEGSR
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
009800031117     C                   EVAL      PiStr=%trim(%subst(vindta:384:6))
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
017000111220     C* Forzatura codice tariffa x sped. no Italia
017100111221     C                   if        %subst(vindta:785:1) <> '1' AND
017200111220     C                             vabpkb <= 30
017300111220     C                   eval      vabctr = 100
017400111220     C                   endif
017500111220     C*
017600021017     C                   write     fivab000                                     => scarico il VAB
017700010305     C*
017800010305     C                   ENDSR
017900990920
018000000801     C*----------------------------------------------------*
018100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
018200000801     C*----------------------------------------------------*
018300010201     C     INZVAR        BEGSR
018400000801     C*
018500010201     C                   Z-ADD     *zeros        Num5_0
018600030107     C                   Z-ADD     *zeros        Num7_0
018700030711     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
018800030711     C                   MOVEL     '0'           FlgCAS            1
018900000801     C*
019000000801     C                   ENDSR
019100000801     C*----------------------------------------------------*
019200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
019300000801     C*----------------------------------------------------*
019400000801     C     DEFCAM        BEGSR
019500000801     C*
019600021017     C                   CLEAR                   FIVAB000
019700020619     C* Imposto i valori di default...
019800031117     C                   Z-ADD     1310015       VABCCM
019900031117     C                   Z-ADD     1310015       VATCCM
020000021017     C                   Z-ADD     vlrpoi        VABFGS
020100021017     C                   Z-ADD     vlrpoi        VATFGS
020200031117     C                   Z-ADD     131           VABLNP
020300031117     C                   Z-ADD     131           VATLNP
020400030711     C                   Z-ADD     000           VABCTR
020500021017     C                   MOVEL     '7Q'          VABCTM
020600020305     C                   MOVEL     'E'           VATTRC
020700020619     C* ... e poi verifico se sono stati passati come parametri
020800020619     C                   IF        vlrppt > *blanks
020900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
021000020619     C                   EXSR      CHKNUM
021100020619     C                   IF        PiInt=*on
021200020619     C                   Z-ADD     PiVal         VABCCM
021300020619     C                   Z-ADD     PiVal         VATCCM
021400020619     C                   ENDIF
021500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
021600020619     C                   EXSR      CHKNUM
021700020619     C                   IF        PiInt=*on
021800020619     C                   Z-ADD     PiVal         VABLNP
021900020619     C                   Z-ADD     PiVal         VATLNP
022000020619     C                   ENDIF
022100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
022200020619     C                   EXSR      CHKNUM
022300020619     C                   IF        PiInt=*on
022400020619     C                   Z-ADD     PiVal         VABCTR
022500020619     C                   ENDIF
022600020619     C                   ENDIF
022700000801     C*
022800000801     C                   ENDSR
022900000801     C*----------------------------------------------------*
023000021017     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
023100000801     C*----------------------------------------------------*
023200010201     C     IMPVAB        BEGSR
023300000801     C*
023400020305     C                   EXSR      INZVAR
023500020305     C                   EXSR      DEFCAM
023600010305     C*
023700000801     C                   Z-ADD     *zeros        errore            1 0
023800000830     C                   MOVEL     datcor        VABAAS
023900020305     C                   MOVEL     datcor        VATAAS
024000000830     C                   MOVE      datcor        VABMGS
024100031117     C*
024200031117     C* Verifico quali campi considerare x l'instradamento al destinatario
024300031117     C                   IF        %subst(vindta:259:40) <> *blanks
024400031117     C                   EVAL      VABRSD=%trim(%subst(vindta:259:40))
024500031117     C                   ELSE
024600031117     C                   EVAL      VABRSD=%trim(%subst(vindta:142:40))
024700031117     C                   ENDIF
024800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
024900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
025000020117     C* ==
025100031117     C                   IF        %subst(vindta:299:40) <> *blanks
025200031117     C                   EVAL      VABIND=%trim(%subst(vindta:299:40))
025300031117     C                   ELSE
025400031117     C                   EVAL      VABIND=%trim(%subst(vindta:182:40))
025500031117     C                   ENDIF
025600031117     C                   IF        %subst(vindta:344:30) <> *blanks
025700031117     C                   EVAL      VABLOD=%trim(%subst(vindta:344:30))
025800031117     C                   ELSE
025900031117     C                   EVAL      VABLOD=%trim(%subst(vindta:227:30))
026000031117     C                   ENDIF
026100031117     C                   IF        %subst(vindta:374:2) <> *blanks
026200031121     C                   EVAL      VABPRD=%trim(%subst(vindta:374:2))
026300031117     C                   ELSE
026400031121     C                   EVAL      VABPRD=%trim(%subst(vindta:257:2))
026500031117     C                   ENDIF
026600031117     C*
026700060306     C* VABTIC
026800060306     C                   IF        %subst(vindta:583:50) <> *blanks
026900060306     C                   EVAL      VABTIC = 'BM'
027000060306     C                   ENDIF
027100060306     C*
027200041129     C                   EVAL      VABNZD=%trim(%subst(vindta:782:3))
027300031117     C                   EVAL      VABRMA=%trim(%subst(vindta:376:14))
027400031117     C                   EVAL      VABNAS=%trim(%subst(vindta:411:15))
027500031117     C                   EVAL      VABNOT=%trim(%subst(vindta:463:40))
027600031117     C                   EVAL      VABNT2=%trim(%subst(vindta:503:40))
027700041129     C*
027800041129     C* Verifico il "tipo circuito" indicato dal cliente: 0=Bartolini 1=DPD 2=EuroExpress
027900041129     C                   IF        %subst(vindta:785:1) = '1'
028000041129     C                   EVAL      VABCTR=353
028100041129     C                   EVAL      VABCTM='7R'
028200041129     C                   ENDIF
028300041129     C*
028400030711     C* CAD
028500031117     C                   IF        %subst(vindta:339:5) <> *blanks
028600031117     C                   EVAL      PiStr=%trim(%subst(vindta:339:5))
028700031117     C                   ELSE
028800031117     C                   EVAL      PiStr=%trim(%subst(vindta:222:5))
028900031117     C                   ENDIF
029000010201     C                   EXSR      CHKNUM
029100010201     C                   IF        PiInt=*on
029200010201     C                   Z-ADD     PiVal         Num5_0
029300021017     C                   MOVEL     Num5_0        VABCAD
029400010201     C                   ELSE
029500010201     C                   ADD       1             errore
029600010201     C                   EVAL      vinmsg = %trimr(vinmsg)
029700021017     C                             + ' ' + 'VABCAD'
029800010201     C                   ENDIF
029900030711     C* RMN
030000050119     C***                EVAL      PiStr=%trim(%subst(vindta:384:6))
030100050119     C                   EVAL      PiStr=%trim(%subst(vindta:376:14))
030200010201     C                   EXSR      CHKNUM
030300010201     C                   IF        PiInt=*on
030400021017     C                   Z-ADD     PiVal         VABRMN
030500010201     C                   ELSE
030600010201     C                   ADD       1             errore
030700010201     C                   EVAL      vinmsg = %trimr(vinmsg)
030800030711     C                             + ' ' + 'VABRMN'
030900010201     C                   ENDIF
031000030711     C* NSP
031100031117     C                   EVAL      PiStr=%trim(%subst(vindta:384:6))
031200030711     C                   EXSR      CHKNUM
031300030711     C                   IF        PiInt=*on
031400030711     C                   Z-ADD     PiVal         Num7_0
031500030711     C                   MOVE(P)   Num7_0        VABNSP
031600030711     C                   MOVE(P)   Num7_0        VATNSP
031700030711     C                   ELSE
031800030711     C                   ADD       1             errore
031900030711     C                   EVAL      vinmsg = %trimr(vinmsg)
032000030711     C                             + ' ' + 'VABNSP VATNSP'
032100030711     C                   ENDIF
032200030711     C* NCL
032300031117     C                   EVAL      PiStr=%trim(%subst(vindta:426:5))
032400030107     C                   EXSR      CHKNUM
032500030107     C                   IF        PiInt=*on
032600030107     C                   Z-ADD     PiVal         VABNCL
032700030107     C                   ELSE
032800030107     C                   ADD       1             errore
032900030107     C                   EVAL      vinmsg = %trimr(vinmsg)
033000030107     C                             + ' ' + 'VABNCL'
033100030107     C                   ENDIF
033200030711     C* PKB
033300031117     C                   EVAL      PiStr=%trim(%subst(vindta:431:10))
033400030107     C                   EXSR      CHKNUM
033500030107     C                   IF        PiNum=*on
033600031117     C                   EVAL      VABPKB = PiVal/10000                         * gestisco 4 decim.
033700030107     C                   ELSE
033800030107     C                   ADD       1             errore
033900030107     C                   EVAL      vinmsg = %trimr(vinmsg)
034000030107     C                             + ' ' + 'VABPKB'
034100030107     C                   ENDIF
034200031117     C* VLB
034300031117     C                   EVAL      PiStr=%trim(%subst(vindta:441:9))
034400031117     C                   EXSR      CHKNUM
034500031117     C                   IF        PiNum=*on
034600050111     C                   EVAL      PiVal = PiVal/10000                          * gestisco 4 dec.
034700050111     C                   Z-ADD     PiVal         VABVLB
034800031117     C                   ELSE
034900031117     C                   ADD       1             errore
035000031117     C                   EVAL      vinmsg = %trimr(vinmsg)
035100031117     C                             + ' ' + 'VABVLB'
035200031117     C                   ENDIF
035300030711     C* CAS
035400031117     C                   IF        %subst(vindta:450:13) <> *zeros
035500030711     C                   MOVEL     '1'           FlgCAS
035600031117     C                   EVAL      PiStr=%trim(%subst(vindta:450:13))
035700030107     C                   EXSR      CHKNUM
035800030107     C                   IF        PiNum=*on
035900031117     C                   EVAL      VABCAS = PiVal/100                           * gestisco 2 decim.
036000030107     C                   ELSE
036100030107     C                   ADD       1             errore
036200030107     C                   EVAL      vinmsg = %trimr(vinmsg)
036300030107     C                             + ' ' + 'VABCAS'
036400030107     C                   ENDIF
036500030107     C                   ENDIF
036600010205     C*
036700010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
036800031117     C                   EVAL      VABCBO = '1'                                 * default
036900031117     C                   IF        FlgCAS = '0' AND %subst(vindta:410:1) = 'F'
037000031117     C                   EVAL      VABCBO = '1'                                 * franco  NO contr.
037100031117     C                   ENDIF
037200031117     C                   IF        FlgCAS = '1' AND %subst(vindta:410:1) = 'F'
037300031117     C                   EVAL      VABCBO = '4'                                 * franco  + contras.
037400031117     C                   EVAL      VABVCA='EUR'
037500031117     C                   ENDIF
037600031117     C                   IF        FlgCAS = '0' AND %subst(vindta:410:1) = 'A'
037700031117     C                   EVAL      VABCBO = '2'                                 * assegn. NO contr.
037800031117     C                   ENDIF
037900031117     C                   IF        FlgCAS = '1' AND %subst(vindta:410:1) = 'A'
038000031117     C                   EVAL      VABCBO = '6'                                 * assegn. + contras.
038100031117     C                   EVAL      VABVCA='EUR'
038200031117     C                   ENDIF
038300020305     C*
038400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
038500011113     C                   EXSR      CHKIMPDIV
038600010202     C*
038700000801     C* Ebbene...
038800000801     C                   ADD       1             §CTRMO
038900010201     C                   IF        errore <> *zeros
039000000801     C                   ADD       1             §CTRNO
039100000801     C                   EVAL      vinflg = '2'
039200000801     C                   ELSE
039300010201     C                   ADD       1             §CTROKVB
039400000801     C                   ENDIF
039500000801     C*
039600000801     C                   ENDSR
039700010201     C*----------------------------------------------------*
039800021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
039900010201     C*----------------------------------------------------*
040000020305     C     WRIVAT        BEGSR
040100010201     C*
040200050926     C* Valorizzo il buffer di scrittura del FIVAT x il "CHI SONO" (barcode)
040300050926     C                   eval      VATTRC = 'E'
040400031117     C                   eval      VATNOT = %triml(%subst(vindta:648:18))
040500021017     C                   write     FIVAT000
040600050926     C*
040700050926     C* Valorizzo il buffer di scrittura del FIVAT x il telefono destinatario
040800050926     C                   eval      VATTRC = 'B'
040900050926     C                   eval      VATNOT = %triml(%subst(vindta:633:15))
041000050926     C                   write     FIVAT000
041100010201     C*
041200010201     C                   ENDSR
041300010202     C*----------------------------------------------------*
041400021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
041500010202     C*----------------------------------------------------*
041600020305     C     PREVAT        BEGSR
041700010202     C*
041800021017     C* Compongo il nome del membro da dare al FIVATWWR
041900010202     C                   eval      parmbr = vlrhdl
042000010202     C                   movel     'M'           parmbr
042100031117     C                   eval      parccm = '01310015'
042200010202     C                   eval      paropz = '1'
042300010202     C* Effettuo la chiamata al CLLE preposto
042400030711     C                   call(e)   'TITVVTC'
042500010202     C                   parm                    parccm
042600010202     C                   parm                    parmbr
042700010202     C                   parm                    paropz
042800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
042900010202     C                   if        %error
043000010202     C                   movel     '1'           chkcall
043100010202     C                   else
043200010202     C                   movel     '0'           chkcall
043300010202     C                   endif
043400010202     C*
043500010202     C                   ENDSR
043600000801     C*----------------------------------------------------*
043700000801     C*  CONTROLLO NUMERICITA' CAMPI
043800000801     C*----------------------------------------------------*
043900000801     C     CHKNUM        BEGSR
044000000801     C*
044100000801     C                   call(e)   'ISNUMERIC'
044200000801     C                   PARM                    PiStr            30
044300030711     C                   PARM      '.'           PiDecChr          1
044400000801     C                   PARM      *ZEROS        PiVal            30 9
044500000801     C                   PARM      '0'           PiInt             1
044600000801     C                   PARM      '0'           PiNum             1
044700000801     C                   IF        %error
044800000801     C                   EVAL      PiInt=*off
044900030107     C                   EVAL      PiNum=*off
045000000801     C                   ENDIF
045100000801     C*
045200000801     C                   ENDSR
045300000801     C***
045400000801
045500011113
045600011113     C*----------------------------------------------------*
045700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
045800011113     C*----------------------------------------------------*
045900011113     C     CHKIMPDIV     BEGSR
046000011113     C*
046100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
046200011113     C                   Z-ADD     *zeros        wrkDec            9 9
046300011113     C*
046400011113     C* Come prima cosa effettuo considerazioni sulla divisa
046500011113     C                   IF        vabIAS > *zeros
046600011113     C                   IF        vabVAS <> 'EUR'
046700011113     C                   EVAL      vabVAS =  'ITL'
046800011113     C                   ENDIF
046900011113     C                   ENDIF
047000011113     C*
047100011113     C                   IF        vabCAS > *zeros
047200011113     C                   IF        vabVCA <> 'EUR'
047300011113     C                   EVAL      vabVCA =  'ITL'
047400011113     C                   ENDIF
047500011113     C                   ENDIF
047600011113     C*
047700011113     C                   IF        vabVMD > *zeros
047800020305     C                   IF        vabVAD <> 'EUR'
047900011113     C                   EVAL      vabVAD =  'ITL'
048000011113     C                   ENDIF
048100011113     C                   ENDIF
048200011113     C*
048300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
048400011113     C                   Z-ADD     vabIAS        wrkDec
048500011113     C                   IF        wrkDec > *zeros
048600011113     C                   IF        vabVAS = 'ITL'
048700011113     C                   EVAL      vabIAS = *zeros
048800011113     C                   ENDIF
048900011113     C                   ENDIF
049000011113     C*
049100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
049200011113     C                   Z-ADD     vabCAS        wrkDec
049300011113     C                   IF        wrkDec > *zeros
049400011113     C                   IF        vabVCA = 'ITL'
049500011113     C                   EVAL      vabCAS = *zeros
049600011113     C                   ENDIF
049700011113     C                   ENDIF
049800011113     C*
049900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
050000011113     C                   Z-ADD     vabVMD        wrkDec
050100011113     C                   IF        wrkDec > *zeros
050200011113     C                   IF        vabVAD = 'ITL'
050300011113     C                   EVAL      vabVMD = *zeros
050400011113     C                   ENDIF
050500011113     C                   ENDIF
050600011113     C*
050700011113     C                   ENDSR
050800011113     C***
050900021017
051000000801
051100000801
051200990920      /TITLE Invio dei dati al punto operativo.
051300010202     C     invio         BEGSR
051400990920     C*
051500021017     C* 1° invio FIVAT
051600010201     C                   reset                   dscmz
051700021017     C                   move      vlrpoi        cmzdst
051800021017     C                   eval      cmzfld = 'FIVATWWR'
051900010201     C                   eval      cmzmbd = vlrhdl
052000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
052100021009     C***                if        prmfir = *blanks
052200021017     C                   eval      cmzfla = 'FIVAT00F'
052300021017     C                   eval      cmzmba = 'FIVAT00F'
052400021009     C***                else
052500021009     C***                eval      cmzfla = prmfir
052600021009     C***                eval      cmzmba = prmfir
052700021009     C***                endif
052800010201     C                   eval      cmznrr = *zeros
052900020305     C                   move      §ctrokvt      cmznrr
053000021018     C                   eval      cmzlba = vlrfl1
053100010201     C                   call(e)   'TIS711C'
053200010201     C                   parm                    dscmz
053300010201     C                   parm      *blanks       esito
053400010205     C                   if        %error
053500010205     C                             or cmzerr = '1'
053600010205     C                             or esito  = '1'
053700010205     C                   eval      wrkesito = '3'
053800010205     C                   else
053900010201     C*
054000021017     C* 2° invio FIVAB
054100010201     C                   reset                   dscmz
054200021017     C                   move      vlrpoi        cmzdst
054300010201     C                   eval      cmzfld = vlrfou
054400010201     C                   eval      cmzmbd = vlrhdl
054500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
054600021009     C***                if        prmfir = *blanks
054700021017     C                   eval      cmzfla = 'FIVAB00F'
054800021017     C                   eval      cmzmba = 'FIVAB00F'
054900021009     C***                else
055000021009     C***                eval      cmzfla = prmfir
055100021009     C***                eval      cmzmba = prmfir
055200021009     C***                endif
055300010201     C                   eval      cmznrr = *zeros
055400010201     C                   move      §ctrokvb      cmznrr
055500021018     C                   eval      cmzlba = vlrfl1
055600010201     C                   call(e)   'TIS711C'
055700010201     C                   parm                    dscmz
055800010201     C                   parm      *blanks       esito
055900010201     C                   if        %error
056000010201     C                             or cmzerr = '1'
056100010201     C                             or esito  = '1'
056200010201     C                   eval      wrkesito = '3'
056300010201     C                   endif
056400010205     C                   endif
056500990920     C*
056600000613     C                   ENDSR
056700000613     C***
056800070411
056900070411     C     *pssr         BEGSR
057000070411     C*
057100070411     C                   if        %open(tivin00r)
057200070411     C                   close     tivin00r
057300070411     C                   endif
057400070411     C                   if        %open(fivabwwr)
057500070411     C                   close     fivabwwr
057600070411     C                   endif
057700070411     C                   if        %open(fivatwwr)
057800070411     C                   close     fivatwwr
057900070411     C                   endif
058000070411     C*
058100070411     C* Effettuo la chiamata al CLLE preposto
058200070411     C                   call(e)   'TITVVTC'
058300070411     C                   parm                    parccm
058400070411     C                   parm                    parmbr
058500070411     C                   parm      '2'           paropz
058600070411     C*
058700070411     C                   eval      wrkesito = '2'
058800070411     C*
058900070411     C                   seton                                        LR
059000070411     C*
059100070411     C                   ENDSR     '*CANCL'
059200070411     C***
059300070411
059400990910
059500000613     C     *inzsr        BEGSR
059600990910     C*
059700990910     C     *entry        plist
059800990920     C                   parm                    tivlrds
059900990921     C                   parm      wrkesito      esito
060000000724     C                   parm                    prmlit
060100000710     C                   parm                    prmfir
060200000613     C*
060300000830     C* CALCOLA LA DATA CORRENTE
060400000830     C                   time                    wn14             14 0
060500000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
060600000830     C                   z-add     wn8           g08dat
060700000830     C                   z-add     *zeros        g08inv
060800000830     C                   movel     '0'           g08err
060900000830     C                   call      'XSRDA8'
061000000830     C                   parm                    wlbda8
061100000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
061200000830     C*
061300000613     C                   ENDSR
061400000613     C***
