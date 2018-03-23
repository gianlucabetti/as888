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
001900990915     D wrkdata         s               d
002000990915     D wrkora          s               t
002100000613     D rrnum           s              6  0 INZ(*zeros)
002200011113     D depcmd          s            150    INZ(*blanks)
002300030114     D depspe          s             10    INZ(*blanks)
002400010213     D depdat          s              8  0 INZ(*zeros)
002500030114     D curspe          s             10    INZ(*blanks)
002600010202     D parccm          s              8    INZ(*blanks)
002700010202     D parmbr          s             10    INZ(*blanks)
002800010202     D paropz          s              1    INZ(*blanks)
002900010202     D chkcall         s              1    INZ(*blanks)
003000010213
003100000801     D  Num5_0         s              5  0
003200030107     D  Num7_0         s              7  0
003300000830
003400000830     D*------------------
003500000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003600000830     D*------------------
003700000830     D WLBDA8          DS                  INZ
003800000830     D  G08DAT                 1      8  0
003900000830     D  G08INV                 9     16  0
004000000830     D  G08ERR                17     17
004100000830     D  G08TGI                18     22  0
004200000830     D*
004300990908
004400010201
004500010201
004600990915     C                   time                    wrkdata
004700990915     C                   time                    wrkora
004800000913     C                   reset                   rrnum
004900990921     C                   reset                   esito
005000990921     C                   reset                   wrkesito
005100000613     C*
005200030114     C                   EXSR      RWHSE                                        LETT/SCR. VAB
005300000613     C*
005400010202     C* Effettuo la chiamata al CLLE preposto
005500050128     C                   call(e)   'TITVVTC'
005600010202     C                   parm                    parccm
005700010202     C                   parm                    parmbr
005800010202     C                   parm      '2'           paropz
005900000616     C*
006000000801     C
006100010201     C                   seton                                        LR
006200990908
006300000801
006400910830     C*--------------------------------------------------------
006500030114     C* RWHSE LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR     *
006600910830     C*--------------------------------------------------------
006700030114     C     RWHSE         BEGSR
006800990910     C*
006900990914     C                   if        not %open(tivin00r)
007000990908     C                   open      tivin00r
007100990914     C                   endif
007200021017     C                   if        not %open(fivabwwr)
007300021017     C                   open      fivabwwr
007400990914     C                   endif
007500021017     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007600020305     C                   exsr      prevat
007700010201     C*
007800010202     C                   if        chkcall = '0'
007900010202     C*
008000021017     C                   if        not %open(fivatwwr)
008100021017     C                   open      fivatwwr
008200010201     C                   endif
008300990910     C*
008400010201     C                   clear                   §CTROKVB          5 0
008500020305     C                   clear                   §CTROKVT          5 0
008600000801     C                   clear                   §CTRMO            5 0
008700000801     C                   clear                   §CTRNO            5 0
008800990910     C*
008900921023     C                   DO        *HIVAL
009000990913     C*
009100990915     C                   READ      tivin00r                               70
009200000905     C                   if        vindta > *blanks
009300000613     C                   add       1             rrnum
009400000801     C*
009500000801     C                   if        *in70 = *off
009600000801     C                             and
009700000801     C                             (vinflg = *blanks
009800000801     C                              or vinflg = '0'
009900000801     C                              or vinflg = '2')
010000000801     C*
010100000801     C                   clear                   vinmsg
010200010305     C*
010300010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
010400030107     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
010500030114     C                   MOVEL(p)  PiStr         curspe
010600010305     C*
010700010305     C                   if        depspe = *blanks                             => 1° giro
010800010305     C                   eval      depspe = curspe                              => memorizz. spediz
010900020305     C                   exsr      impvab
011000020305     C                   exsr      wrivat                                       => carico VAT
011100010305     C                   else
011200010305     C                   if        curspe <> depspe                             => rottura di spediz
011300010305     C                   eval      depspe = curspe                              => memorizz. spediz
011400020305     C                   exsr      wrivab
011500020305     C                   exsr      impvab
011600020305     C                   exsr      wrivat                                       => carico VAT
011700020305     C                   else                                                   => x stessa spediz
011800020305     C                   exsr      impvab
011900020305     C                   exsr      wrivat                                       => carico VAT
012000010305     C                   endif
012100010305     C                   endif
012200010305     C                   endif
012300000905     C*
012400000905     C                   else
012500000905     C                   eval      vinflg = '1'
012600000905     C                   endif
012700000905     C*
012800000905     C  N70              update    tivin000
012900000905     C*
013000991022     C  N70              ENDdo
013100010202     C*
013200020305     C* Scarico i VAB rimasti "in sospeso"
013300020305     C                   exsr      wrivab
013400010202     C*
013500010202     C                   endif
013600990910
013700990910     C* Se non ci sono record con errori ...
013800000710     C                   if        §ctrno = 0
013900990910     C* ... restituisco esito OK.
014000990921     C                   eval      wrkesito = '0'
014100990910     C                   else
014200010201     C                   if        §ctrokvb > 0
014300990921     C                   eval      wrkesito = '1'
014400000710     C                   else
014500000710     C                   eval      wrkesito = '2'
014600990910     C                   endif
014700000710     C                   endif
014800990910     C*
014900990914     C                   if        %open(tivin00r)
015000990908     C                   close     tivin00r
015100990914     C                   endif
015200021017     C                   if        %open(fivabwwr)
015300021017     C                   close     fivabwwr
015400990914     C                   endif
015500021017     C                   if        %open(fivatwwr)
015600021017     C                   close     fivatwwr
015700010201     C                   endif
015800021017     C*
015900021017     C                   if        vlrpoi <> 999
016000021017     C                   eval      vlrpoi = vlrpoi
016100021017     C                   endif
016200990910     C*
016300010201     C                   if        §ctrokvb > 0
016400021017     C                             and vlrpoi > *zeros
016500010202     C                   exsr      invio
016600990920     C                   endif
016700990920     C*
016800910830     C                   ENDSR
016900000613     C***
017000010305
017100010305     C*----------------------------------------------------*
017200020305     C*  SCARICAMENTO BUFFER RECORDS VAB
017300010305     C*----------------------------------------------------*
017400020305     C     WRIVAB        BEGSR
017500010305     C*
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
018700000801     C*
018800000801     C                   ENDSR
018900000801     C*----------------------------------------------------*
019000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
019100000801     C*----------------------------------------------------*
019200000801     C     DEFCAM        BEGSR
019300000801     C*
019400021017     C                   CLEAR                   FIVAB000
019500020619     C* Imposto i valori di default...
019600070605     C                   Z-ADD     2551050       VABCCM
019700070605     C                   Z-ADD     2551050       VATCCM
019800021017     C                   Z-ADD     vlrpoi        VABFGS
019900021017     C                   Z-ADD     vlrpoi        VATFGS
020000070605     C                   Z-ADD     255           VABLNP
020100070605     C                   Z-ADD     255           VATLNP
020200070605     C                   Z-ADD     000           VABCTR
020300021017     C                   MOVEL     '7Q'          VABCTM
020400070605     C                   MOVEL     'C'           VABTSP
020500020305     C                   MOVEL     'E'           VATTRC
020600020619     C* ... e poi verifico se sono stati passati come parametri
020700020619     C                   IF        vlrppt > *blanks
020800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
020900020619     C                   EXSR      CHKNUM
021000020619     C                   IF        PiInt=*on
021100020619     C                   Z-ADD     PiVal         VABCCM
021200020619     C                   Z-ADD     PiVal         VATCCM
021300020619     C                   ENDIF
021400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
021500020619     C                   EXSR      CHKNUM
021600020619     C                   IF        PiInt=*on
021700020619     C                   Z-ADD     PiVal         VABLNP
021800020619     C                   Z-ADD     PiVal         VATLNP
021900020619     C                   ENDIF
022000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
022100020619     C                   EXSR      CHKNUM
022200020619     C                   IF        PiInt=*on
022300020619     C                   Z-ADD     PiVal         VABCTR
022400020619     C                   ENDIF
022500020619     C                   ENDIF
022600000801     C*
022700000801     C                   ENDSR
022800000801     C*----------------------------------------------------*
022900021017     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
023000000801     C*----------------------------------------------------*
023100010201     C     IMPVAB        BEGSR
023200000801     C*
023300020305     C                   EXSR      INZVAR
023400020305     C                   EXSR      DEFCAM
023500010305     C*
023600000801     C                   Z-ADD     *zeros        errore            1 0
023700000830     C                   MOVEL     datcor        VABAAS
023800020305     C                   MOVEL     datcor        VATAAS
023900000830     C                   MOVE      datcor        VABMGS
024000030107     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
024100020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
024200020117     C     '@':'A'       XLATE     VABRSD        VABRSD
024300020117     C* ==
024400030107     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
024500030107     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
024600030107     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
024700030114     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
024800090514     C                   MOVEL     *blanks       wNOTE            80
024900090514     C                   EVAL      wNOTE =%trim(%subst(vindta:197:80))
025000090514     C                   EVAL      VABNOT=%trim(%subst(wNOTE:1:35))
025100090514     C                   EVAL      VABNT2=%trim(%subst(wNOTE:1+35:35))
025200040405     C***                EVAL      VABRMO='Tel:' + %trim(%subst(vindta:92:15))
025300030114     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
025400030107     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
025500030114     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
025600030114     C                   IF        %trim(%subst(vindta:12:1)) = 'D'
025700030107     C                   EVAL      VABTSP='E'
025800030107     C                   ENDIF
025900030114     C                   IF        %trim(%subst(vindta:16:1)) = '1'
026000030114     C                   EVAL      VABFFD='S'
026100030114     C                   ENDIF
026200100705     C                   EVAL      VABGMA=%trim(%subst(vindta:416:2))
026300100713     C*
026400100713     C                   IF        VABGMA = 'RC'
026500121109     C                   IF        VABCCM <> 2552551
026600100713     C                   EVAL      VABGMA = 'R6'
026700121109     C                   ENDIF
026800100713     C                   ENDIF
026900020305     C*
027000030107     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
027100010201     C                   EXSR      CHKNUM
027200010201     C                   IF        PiInt=*on
027300010201     C                   Z-ADD     PiVal         Num5_0
027400021017     C                   MOVEL     Num5_0        VABCAD
027500010201     C                   ELSE
027600010201     C                   ADD       1             errore
027700010201     C                   EVAL      vinmsg = %trimr(vinmsg)
027800021017     C                             + ' ' + 'VABCAD'
027900010201     C                   ENDIF
028000020619     C*
028100030107     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
028200010201     C                   EXSR      CHKNUM
028300010201     C                   IF        PiInt=*on
028400021017     C                   Z-ADD     PiVal         VABRMN
028500030107     C                   Z-ADD     PiVal         Num7_0
028600030107     C                   MOVE(P)   Num7_0        VABNSP
028700030107     C                   MOVE(P)   Num7_0        VATNSP
028800010201     C                   ELSE
028900010201     C                   ADD       1             errore
029000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
029100021017     C                             + ' ' + 'VABNSP VATNSP VABRMN'
029200010201     C                   ENDIF
029300030107     C*
029400030107     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
029500030107     C                   EXSR      CHKNUM
029600030107     C                   IF        PiInt=*on
029700030107     C                   Z-ADD     PiVal         VABNCL
029800030107     C                   ELSE
029900030107     C                   ADD       1             errore
030000030107     C                   EVAL      vinmsg = %trimr(vinmsg)
030100030107     C                             + ' ' + 'VABNCL'
030200030107     C                   ENDIF
030300030107     C*
030400030107     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
030500030114     C                   EVAL      PiDecChr = '.'
030600030107     C                   EXSR      CHKNUM
030700030107     C                   IF        PiNum=*on
030800030114     C                   Z-ADD(H)  PiVal         VABPKB
030900030107     C                   ELSE
031000030107     C                   ADD       1             errore
031100030107     C                   EVAL      vinmsg = %trimr(vinmsg)
031200030107     C                             + ' ' + 'VABPKB'
031300030107     C                   ENDIF
031400030114     C*
031500030114     C                   EVAL      PiStr=%trim(%subst(vindta:308:7))
031600030114     C                   EVAL      PiDecChr = '.'
031700030114     C                   EXSR      CHKNUM
031800030114     C                   IF        PiNum=*on
031900151019     C***                EVAL      PiVal = PiVal / 1000                         * da DM3 a M3
032000030114     C                   Z-ADD(H)  PiVal         VABVLB
032100030114     C                   ELSE
032200030114     C                   ADD       1             errore
032300030114     C                   EVAL      vinmsg = %trimr(vinmsg)
032400030114     C                             + ' ' + 'VABVLB'
032500030114     C                   ENDIF
032600030107     C*
032700030107     C                   IF        %trim(%subst(vindta:280:12)) <> *blanks
032800050614     C                   IF        %trim(%subst(vindta:315:3)) = 'BSC'
032900050614     C                   EVAL      VABTIC=*blanks
033000050614     C                   ENDIF
033100050614     C                   IF        %trim(%subst(vindta:315:3)) = 'CSC'
033200050614     C                   EVAL      VABTIC='C'
033300050614     C                   ENDIF
033400050614     C                   IF        %trim(%subst(vindta:315:3)) = 'CON'
033500091116     C***                EVAL      VABTIC='SC'
033600091116     C                   EVAL      VABTIC=*blanks
033700050614     C                   ENDIF
033800070504     C                   IF        %trim(%subst(vindta:315:3)) = 'ACS'
033900050614     C                   EVAL      VABTIC='C'
034000050614     C                   ENDIF
034100121109     C*
034200121109     C* Forzatura x codice mittente 2552551
034300121109     C                   IF        VABCCM = 2552551
034400121109     C                   EVAL      VABTIC='SC'
034500121109     C                   ENDIF
034600121109     C*
034700030107     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
034800030114     C                   EVAL      PiDecChr = ','
034900030107     C                   EXSR      CHKNUM
035000030107     C                   IF        PiNum=*on
035100030107     C                   Z-ADD     PiVal         VABCAS
035200030107     C                   ELSE
035300030107     C                   ADD       1             errore
035400030107     C                   EVAL      vinmsg = %trimr(vinmsg)
035500030107     C                             + ' ' + 'VABCAS'
035600030107     C                   ENDIF
035700030107     C                   ENDIF
035800030114     C*
035900030114     C                   IF        %trim(%subst(vindta:295:12)) <> *blanks
036000030114     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
036100030114     C                   EVAL      PiDecChr = ','
036200030114     C                   EXSR      CHKNUM
036300030114     C                   IF        PiNum=*on
036400030114     C                   Z-ADD     PiVal         VABIAS
036500030114     C                   ELSE
036600030114     C                   ADD       1             errore
036700030114     C                   EVAL      vinmsg = %trimr(vinmsg)
036800030114     C                             + ' ' + 'VABIAS'
036900030114     C                   ENDIF
037000030114     C                   ENDIF
037100010205     C*
037200010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
037300010205     C                   IF        VABCAS > *zeros
037400010205     C                   EVAL      VABCBO = '4'
037500010205     C                   ELSE
037600010205     C                   EVAL      VABCBO = '1'
037700010205     C                   ENDIF
037800020305     C*
037900011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
038000011113     C                   EXSR      CHKIMPDIV
038100010202     C*
038200000801     C* Ebbene...
038300000801     C                   ADD       1             §CTRMO
038400010201     C                   IF        errore <> *zeros
038500000801     C                   ADD       1             §CTRNO
038600000801     C                   EVAL      vinflg = '2'
038700000801     C                   ELSE
038800010201     C                   ADD       1             §CTROKVB
038900000801     C                   ENDIF
039000000801     C*
039100000801     C                   ENDSR
039200010201     C*----------------------------------------------------*
039300021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
039400010201     C*----------------------------------------------------*
039500020305     C     WRIVAT        BEGSR
039600010201     C*
039700021017     C* Valorizzo l buffer di scrittura del FIVAT
039800070504     C*
039900180222     C                   IF        %triml(%subst(vindta:331:10)) <> *blank
040000180213     C                   EVAL      VATTRC = 'E'
040100180213     C*
040200180213     C* Sviluppati i "CHI SONO" in relazione al numero colli
040300180213     C*
040400180222     C     1             DO        VABNCL        wNumCollo         3 0
040500180213     C*** da analisi in fondo al barcode devo scrivere il numero colli (1 solo char) + '0'
040600180222     C                   EVAL      VATNOT = %triml(%subst(vindta:331:10)) +
040700180213     C                             %editc(wNumCollo:'X') + '0'
040800180213     C                   write     FIVAT000
040900180213     C                   add       1             §CTROKVT
041000180213     C                   ENDDO
041100070504     C                   endif
041200070504     C*
041300070504     C* - telefono destinatario
041400180213     C                   IF        %trim(%subst(vindta:92:15)) <> *blank
041500040224     C                   EVAL      VATTRC = 'B'
041600040224     C                   EVAL      VATNOT=%trim(%subst(vindta:92:15))
041700040224     C                   write     FIVAT000
041800180213     C                   endif
041900010201     C*
042000010201     C                   ENDSR
042100010202     C*----------------------------------------------------*
042200021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
042300010202     C*----------------------------------------------------*
042400020305     C     PREVAT        BEGSR
042500010202     C*
042600021017     C* Compongo il nome del membro da dare al FIVATWWR
042700010202     C                   eval      parmbr = vlrhdl
042800010202     C                   movel     'M'           parmbr
042900030114     C                   eval      parccm = '00632410'
043000010202     C                   eval      paropz = '1'
043100010202     C* Effettuo la chiamata al CLLE preposto
043200050128     C                   call(e)   'TITVVTC'
043300010202     C                   parm                    parccm
043400010202     C                   parm                    parmbr
043500010202     C                   parm                    paropz
043600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
043700010202     C                   if        %error
043800010202     C                   movel     '1'           chkcall
043900010202     C                   else
044000010202     C                   movel     '0'           chkcall
044100010202     C                   endif
044200010202     C*
044300010202     C                   ENDSR
044400000801     C*----------------------------------------------------*
044500000801     C*  CONTROLLO NUMERICITA' CAMPI
044600000801     C*----------------------------------------------------*
044700000801     C     CHKNUM        BEGSR
044800000801     C*
044900030114     C                   IF        PiDecChr = *blanks
045000030114     C                   EVAL      PiDecChr = ','
045100030114     C                   ENDIF
045200030114     C*
045300000801     C                   call(e)   'ISNUMERIC'
045400000801     C                   PARM                    PiStr            30
045500030114     C                   PARM                    PiDecChr          1
045600000801     C                   PARM      *ZEROS        PiVal            30 9
045700000801     C                   PARM      '0'           PiInt             1
045800000801     C                   PARM      '0'           PiNum             1
045900000801     C                   IF        %error
046000000801     C                   EVAL      PiInt=*off
046100030107     C                   EVAL      PiNum=*off
046200000801     C                   ENDIF
046300000801     C*
046400000801     C                   ENDSR
046500000801     C***
046600000801
046700011113
046800011113     C*----------------------------------------------------*
046900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
047000011113     C*----------------------------------------------------*
047100011113     C     CHKIMPDIV     BEGSR
047200011113     C*
047300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
047400011113     C                   Z-ADD     *zeros        wrkDec            9 9
047500011113     C*
047600011113     C* Come prima cosa effettuo considerazioni sulla divisa
047700011113     C                   IF        vabIAS > *zeros
047800011113     C                   IF        vabVAS <> 'EUR'
047900011113     C                   EVAL      vabVAS =  'ITL'
048000011113     C                   ENDIF
048100011113     C                   ENDIF
048200011113     C*
048300011113     C                   IF        vabCAS > *zeros
048400011113     C                   IF        vabVCA <> 'EUR'
048500011113     C                   EVAL      vabVCA =  'ITL'
048600011113     C                   ENDIF
048700011113     C                   ENDIF
048800011113     C*
048900011113     C                   IF        vabVMD > *zeros
049000020305     C                   IF        vabVAD <> 'EUR'
049100011113     C                   EVAL      vabVAD =  'ITL'
049200011113     C                   ENDIF
049300011113     C                   ENDIF
049400011113     C*
049500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
049600011113     C                   Z-ADD     vabIAS        wrkDec
049700011113     C                   IF        wrkDec > *zeros
049800011113     C                   IF        vabVAS = 'ITL'
049900011113     C                   EVAL      vabIAS = *zeros
050000011113     C                   ENDIF
050100011113     C                   ENDIF
050200011113     C*
050300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
050400011113     C                   Z-ADD     vabCAS        wrkDec
050500011113     C                   IF        wrkDec > *zeros
050600011113     C                   IF        vabVCA = 'ITL'
050700011113     C                   EVAL      vabCAS = *zeros
050800011113     C                   ENDIF
050900011113     C                   ENDIF
051000011113     C*
051100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
051200011113     C                   Z-ADD     vabVMD        wrkDec
051300011113     C                   IF        wrkDec > *zeros
051400011113     C                   IF        vabVAD = 'ITL'
051500011113     C                   EVAL      vabVMD = *zeros
051600011113     C                   ENDIF
051700011113     C                   ENDIF
051800011113     C*
051900011113     C                   ENDSR
052000011113     C***
052100021017
052200000801
052300000801
052400990920      /TITLE Invio dei dati al punto operativo.
052500010202     C     invio         BEGSR
052600990920     C*
052700021017     C* 1° invio FIVAT
052800010201     C                   reset                   dscmz
052900021017     C                   move      vlrpoi        cmzdst
053000021017     C                   eval      cmzfld = 'FIVATWWR'
053100010201     C                   eval      cmzmbd = vlrhdl
053200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
053300021009     C***                if        prmfir = *blanks
053400021017     C                   eval      cmzfla = 'FIVAT00F'
053500021017     C                   eval      cmzmba = 'FIVAT00F'
053600021009     C***                else
053700021009     C***                eval      cmzfla = prmfir
053800021009     C***                eval      cmzmba = prmfir
053900021009     C***                endif
054000010201     C                   eval      cmznrr = *zeros
054100020305     C                   move      §ctrokvt      cmznrr
054200021018     C                   eval      cmzlba = vlrfl1
054300010201     C                   call(e)   'TIS711C'
054400010201     C                   parm                    dscmz
054500010201     C                   parm      *blanks       esito
054600010205     C                   if        %error
054700010205     C                             or cmzerr = '1'
054800010205     C                             or esito  = '1'
054900010205     C                   eval      wrkesito = '3'
055000010205     C                   else
055100010201     C*
055200021017     C* 2° invio FIVAB
055300010201     C                   reset                   dscmz
055400021017     C                   move      vlrpoi        cmzdst
055500010201     C                   eval      cmzfld = vlrfou
055600010201     C                   eval      cmzmbd = vlrhdl
055700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055800021009     C***                if        prmfir = *blanks
055900021017     C                   eval      cmzfla = 'FIVAB00F'
056000021017     C                   eval      cmzmba = 'FIVAB00F'
056100021009     C***                else
056200021009     C***                eval      cmzfla = prmfir
056300021009     C***                eval      cmzmba = prmfir
056400021009     C***                endif
056500010201     C                   eval      cmznrr = *zeros
056600010201     C                   move      §ctrokvb      cmznrr
056700021018     C                   eval      cmzlba = vlrfl1
056800010201     C                   call(e)   'TIS711C'
056900010201     C                   parm                    dscmz
057000010201     C                   parm      *blanks       esito
057100010201     C                   if        %error
057200010201     C                             or cmzerr = '1'
057300010201     C                             or esito  = '1'
057400010201     C                   eval      wrkesito = '3'
057500010201     C                   endif
057600010205     C                   endif
057700990920     C*
057800000613     C                   ENDSR
057900000613     C***
058000070411
058100070411     C     *pssr         BEGSR
058200070411     C*
058300070411     C                   if        %open(tivin00r)
058400070411     C                   close     tivin00r
058500070411     C                   endif
058600070411     C                   if        %open(fivabwwr)
058700070411     C                   close     fivabwwr
058800070411     C                   endif
058900070411     C                   if        %open(fivatwwr)
059000070411     C                   close     fivatwwr
059100070411     C                   endif
059200070411     C*
059300070411     C* Effettuo la chiamata al CLLE preposto
059400070411     C                   call(e)   'TITVVTC'
059500070411     C                   parm                    parccm
059600070411     C                   parm                    parmbr
059700070411     C                   parm      '2'           paropz
059800070411     C*
059900070411     C                   eval      wrkesito = '2'
060000070411     C*
060100070411     C                   seton                                        LR
060200070411     C*
060300070411     C                   ENDSR     '*CANCL'
060400070411     C***
060500070411
060600990910
060700000613     C     *inzsr        BEGSR
060800990910     C*
060900990910     C     *entry        plist
061000990920     C                   parm                    tivlrds
061100990921     C                   parm      wrkesito      esito
061200000724     C                   parm                    prmlit
061300000710     C                   parm                    prmfir
061400000613     C*
061500000830     C* CALCOLA LA DATA CORRENTE
061600000830     C                   time                    wn14             14 0
061700000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
061800000830     C                   z-add     wn8           g08dat
061900000830     C                   z-add     *zeros        g08inv
062000000830     C                   movel     '0'           g08err
062100000830     C                   call      'XSRDA8'
062200000830     C                   parm                    wlbda8
062300000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
062400000830     C*
062500000613     C                   ENDSR
