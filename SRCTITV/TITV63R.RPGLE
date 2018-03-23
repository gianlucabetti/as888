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
001500030804     D tisi95ds      e ds
001600990910     D esito           s              1
001700000724     D prmlit          s             10
001800000710     D prmfir          s             10
001900990921     D wrkesito        s                   like(esito)
002000000613     D rrnum           s              6  0 INZ(*zeros)
002100030415     D depspe          s              7    INZ(*blanks)
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
003800070103     D*------------------
003900070103     D* DS REPERIMENTO NUMERATORE
004000070103     D*------------------
004100070103     D trul33ds      e ds                  inz
004200070103     D*------------------
004300070103     D* DS ARCHITETTURA
004400070103     D*------------------
004500070103     D kpjba         e ds                  inz
004600000830     D*
004700990908
004800010201
004900010201
005000000913     C                   reset                   rrnum
005100990921     C                   reset                   esito
005200990921     C                   reset                   wrkesito
005300000613     C*
005400030226     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005500000613     C*
005600010202     C* Effettuo la chiamata al CLLE preposto
005700030730     C                   call(e)   'TITVVTC'
005800010202     C                   parm                    parccm
005900010202     C                   parm                    parmbr
006000010202     C                   parm      '2'           paropz
006100050201     C*
006200050201     C* Effettuo lancio TISI95 solo x chiusura
006300050201     C                   CLEAR                   TISI95DS
006400050201     C                   EVAL      I95TLA = 'C'
006500050201     C                   CALL      'TISI95R'
006600050201     C                   PARM                    TISI95DS
006700000616     C*
006800010201     C                   seton                                        LR
006900990908
007000000801
007100910830     C*--------------------------------------------------------
007200030730     C* RWFILE LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR    *
007300910830     C*--------------------------------------------------------
007400030226     C     RWFILE        BEGSR
007500990910     C*
007600990914     C                   if        not %open(tivin00r)
007700990908     C                   open      tivin00r
007800990914     C                   endif
007900021017     C                   if        not %open(fivabwwr)
008000021017     C                   open      fivabwwr
008100990914     C                   endif
008200021017     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
008300020305     C                   exsr      prevat
008400010201     C*
008500010202     C                   if        chkcall = '0'
008600010202     C*
008700021017     C                   if        not %open(fivatwwr)
008800021017     C                   open      fivatwwr
008900010201     C                   endif
009000990910     C*
009100010201     C                   clear                   §CTROKVB          5 0
009200020305     C                   clear                   §CTROKVT          5 0
009300000801     C                   clear                   §CTRMO            5 0
009400000801     C                   clear                   §CTRNO            5 0
009500990910     C*
009600921023     C                   DO        *HIVAL
009700990913     C*
009800990915     C                   READ      tivin00r                               70
009900000905     C                   if        vindta > *blanks
010000000613     C                   add       1             rrnum
010100000801     C*
010200000801     C                   if        *in70 = *off
010300000801     C                             and
010400000801     C                             (vinflg = *blanks
010500000801     C                              or vinflg = '0'
010600000801     C                              or vinflg = '2')
010700000801     C*
010800000801     C                   clear                   vinmsg
010900030310     C                   eval      vinflg = '1'
011000030310     C*
011100030506     C* Determino se il record corrente è di tipo testata o di tipo dettaglio
011200030804     C                   if        %subst(vindta:1:2) = '11'                    *** TESTATA ***
011300030804     C                   clear                   fivab000
011400030804     C                   exsr      impvab                                       => carico  VAB
011500030804     C                   endif
011600030804     C                   if        %subst(vindta:1:2) = '12'                    *** TESTATA ***
011700030804     C                   exsr      impvab                                       => carico  VAB
011800030804     C                   endif
011900030804     C                   if        %subst(vindta:1:2) = '13'
012000030506     C                   exsr      impvab                                       => carico  VAB
012100030804     C                   exsr      wrivab                                       => scarico VAB
012200030506     C                   endif
012300030804     C                   if        %subst(vindta:1:2) = '14'                    *** DETTAGLIO ***
012400030506     C                   exsr      impvat                                       => carico  VAT
012500030506     C                   exsr      wrivat                                       => scarico VAT
012600030506     C                   endif
012700000905     C*
012800000905     C                   else
012900000905     C                   eval      vinflg = '1'
013000000905     C                   endif
013100030411     C                   endif
013200000905     C*
013300000905     C  N70              update    tivin000
013400000905     C*
013500991022     C  N70              ENDdo
013600010202     C*
013700010202     C                   endif
013800990910
013900990910     C* Se non ci sono record con errori ...
014000000710     C                   if        §ctrno = 0
014100990910     C* ... restituisco esito OK.
014200990921     C                   eval      wrkesito = '0'
014300990910     C                   else
014400010201     C                   if        §ctrokvb > 0
014500990921     C                   eval      wrkesito = '1'
014600000710     C                   else
014700000710     C                   eval      wrkesito = '2'
014800990910     C                   endif
014900000710     C                   endif
015000990910     C*
015100990914     C                   if        %open(tivin00r)
015200990908     C                   close     tivin00r
015300990914     C                   endif
015400021017     C                   if        %open(fivabwwr)
015500021017     C                   close     fivabwwr
015600990914     C                   endif
015700021017     C                   if        %open(fivatwwr)
015800021017     C                   close     fivatwwr
015900010201     C                   endif
016000021017     C*
016100021017     C                   if        vlrpoi <> 999
016200021017     C                   eval      vlrpoi = vlrpoi
016300021017     C                   endif
016400990910     C*
016500010201     C                   if        §ctrokvb > 0
016600021017     C                             and vlrpoi > *zeros
016700010202     C                   exsr      invio
016800990920     C                   endif
016900990920     C*
017000910830     C                   ENDSR
017100000613     C***
017200990920
017300000801     C*----------------------------------------------------*
017400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
017500000801     C*----------------------------------------------------*
017600010201     C     INZVAR        BEGSR
017700000801     C*
017800010201     C                   Z-ADD     *zeros        Num5_0
017900030107     C                   Z-ADD     *zeros        Num7_0
018000030804     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
018100030804     C                   MOVEL     '0'           FlgCAS            1
018200000801     C*
018300000801     C                   ENDSR
018400000801     C*----------------------------------------------------*
018500000801     C*  IMPOSTAZIONE CAMPI COSTANTI
018600000801     C*----------------------------------------------------*
018700000801     C     DEFCAM        BEGSR
018800000801     C*
018900020619     C* Imposto i valori di default...
019000120221     C***                Z-ADD     0013179       VABCCM
019100120221     C***                Z-ADD     0013179       VATCCM
019200120221     C                   Z-ADD     0015896       VABCCM
019300120221     C                   Z-ADD     0015896       VATCCM
019400021017     C                   Z-ADD     vlrpoi        VABFGS
019500021017     C                   Z-ADD     vlrpoi        VATFGS
019600030804     C                   Z-ADD     001           VABLNP
019700030804     C                   Z-ADD     001           VATLNP
019800070312     C                   Z-ADD     300           VABCTR
019900021017     C                   MOVEL     '7Q'          VABCTM
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
022400030730     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
022500000801     C*----------------------------------------------------*
022600010201     C     IMPVAB        BEGSR
022700030730     C*
022800030804     C* Elaboro il primo tipo record d TESTATA
022900030804     C                   IF        %subst(vindta:1:2) = '11'
023000030804     C*
023100020305     C                   EXSR      INZVAR
023200020305     C                   EXSR      DEFCAM
023300010305     C*
023400030226     C                   Z-ADD     *zeros        errore            1 0
023500000830     C                   MOVEL     datcor        VABAAS
023600000830     C                   MOVE      datcor        VABMGS
023700030226     C*
023800030804     C                   EVAL      VABRSD=%trim(%subst(vindta:28:30))
023900020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
024000020117     C     '@':'A'       XLATE     VABRSD        VABRSD
024100020117     C* ==
024200030804     C                   EVAL      VABRD2=%trim(%subst(vindta:58:30))
024300030804     C                   ENDIF
024400030804     C*
024500030804     C* Elaboro il secondo tipo record d TESTATA
024600030804     C                   IF        %subst(vindta:1:2) = '12'
024700030804     C*
024800030804     C                   EVAL      VABIND=%trim(%subst(vindta:57:30))
024900030804     C                   EVAL      VABLOD=%trim(%subst(vindta:100:25))
025000030804     C* CAP
025100030804     C                   EVAL      PiStr=%trim(%subst(vindta:95:5))
025200030804     C                   EXSR      CHKNUM
025300030804     C                   IF        PiInt=*on
025400030804     C                   Z-ADD     PiVal         Num5_0
025500030804     C                   MOVEL     Num5_0        VABCAD
025600030804     C                   ELSE
025700030804     C                   ADD       1             errore
025800030804     C                   EVAL      vinmsg = %trimr(vinmsg)
025900030804     C                             + ' ' + 'VABCAD'
026000030804     C                   ENDIF
026100030804     C* Reperisco la provincia dal CAP e dalla località
026200030804     C                   IF        VABCAD <> *blanks
026300030804     C                   CLEAR                   TISI95DS
026400030804     C                   EVAL      I95TCN = '3'
026500030804     C                   Z-ADD     datcor        I95DAT
026600030804     C                   EVAL      I95CAP = VABCAD
026700030804     C                   EVAL      I95LOC = VABLOD
026800030804     C                   CALL      'TISI95R'
026900030804     C                   PARM                    TISI95DS
027000030804     C                   EVAL      VABPRD = O95PRV
027100030804     C                   ENDIF
027200030804     C                   ENDIF
027300030804     C*
027400030804     C* Elaboro il terzo tipo record d TESTATA
027500030804     C                   IF        %subst(vindta:1:2) = '13'
027600030804     C* NCL
027700030804     C                   EVAL      PiStr=%trim(%subst(vindta:31:3))
027800030804     C                   EXSR      CHKNUM
027900030804     C                   IF        PiInt=*on
028000030804     C                   Z-ADD     PiVal         VABNCL
028100030804     C                   ELSE
028200030804     C                   ADD       1             errore
028300030804     C                   EVAL      vinmsg = %trimr(vinmsg)
028400030804     C                             + ' ' + 'VABNCL'
028500030804     C                   ENDIF
028600030804     C* PKB
028700030804     C                   EVAL      PiStr=%trim(%subst(vindta:34:7))
028800030804     C                   EXSR      CHKNUM
028900030804     C                   IF        PiNum=*on
029000030804     C                   EVAL(H)   VABPKB = PiVal/100
029100030804     C                   ELSE
029200030804     C                   ADD       1             errore
029300030804     C                   EVAL      vinmsg = %trimr(vinmsg)
029400030804     C                             + ' ' + 'VABPKB'
029500030804     C                   ENDIF
029600030804     C* CAS
029700030804     C                   IF        %subst(vindta:57:9) <> *blanks AND
029800030804     C                             %subst(vindta:57:9) <> *zeros
029900030804     C                   MOVEL     '1'           FlgCAS
030000030804     C                   EVAL      PiStr=%trim(%subst(vindta:57:9))
030100030804     C                   EXSR      CHKNUM
030200030804     C                   IF        PiNum=*on
030300030804     C                   EVAL      VABCAS = PiVal/100
030400090312     C                   EVAL      VABTIC='BM'
030500030804     C                   EVAL      VABVCA='EUR'
030600030804     C                   ELSE
030700030804     C                   ADD       1             errore
030800030804     C                   EVAL      vinmsg = %trimr(vinmsg)
030900030804     C                             + ' ' + 'VABCAS'
031000030804     C                   ENDIF
031100030804     C                   ENDIF
031200070103     C* RMA/RMN
031300051213     C                   EVAL      VABRMA=%trim(%subst(vindta:71:15))
031400051213     C                   EVAL      PiStr=%trim(%subst(vindta:71:15))
031500010201     C                   EXSR      CHKNUM
031600010201     C                   IF        PiInt=*on
031700030804     C                   Z-ADD     PiVal         VABRMN
031800010201     C                   ELSE
031900010201     C                   ADD       1             errore
032000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
032100070103     C                             + ' ' + 'VABRMA VABNSP'
032200010201     C                   ENDIF
032300070103     C* NSP => Stacco un numeratore da AZNUM
032400070103     C                   clear                   TRUL33DS
032500070103     C                   eval      I33OPE = *zeros
032600070103     C                   eval      I33CNU = 302
032700070103     C                   eval      I33NUM = 1
032800070103     C                   movel     TRUL33DS      KPJBU
032900070103     C                   call      'TRUL33R'
033000070103     C                   parm                    KPJBA
033100070103     C                   movel     KPJBU         TRUL33DS
033200070103     C                   if        O33ERR = *zeros
033300070103     C                   z-add     O33NRF        VABNSP
033400070103     C                   z-add     O33NRF        VATNSP
033500070103     C                   else
033600070103     C                   Z-ADD     1             errore
033700070103     C                   EVAL      vinmsg = %trimr(vinmsg)
033800070103     C                             + ' ' + 'VABNSP VATNSP'
033900070103     C                   endif
034000030804     C*
034100030804     C* Considerazioni sul contenuto di campi precedentemente valorizzati
034200030804     C                   IF        FlgCAS = '1'
034300030804     C                   EVAL      VABCBO = '4'
034400030804     C                   ELSE
034500030804     C                   EVAL      VABCBO = '1'
034600030804     C                   ENDIF
034700020305     C*
034800011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
034900011113     C                   EXSR      CHKIMPDIV
035000010202     C*
035100000801     C* Ebbene...
035200000801     C                   ADD       1             §CTRMO
035300010201     C                   IF        errore <> *zeros
035400000801     C                   ADD       1             §CTRNO
035500000801     C                   EVAL      vinflg = '2'
035600000801     C                   ELSE
035700010201     C                   ADD       1             §CTROKVB
035800000801     C                   ENDIF
035900030804     C                   ENDIF
036000000801     C*
036100000801     C                   ENDSR
036200030226     C*----------------------------------------------------*
036300030226     C*  SCARICAMENTO BUFFER RECORDS VAB
036400030226     C*----------------------------------------------------*
036500030226     C     WRIVAB        BEGSR
036600030310     C*
036700030310     C                   write     FIVAB000
036800030226     C*
036900030226     C                   ENDSR
037000030226     C*----------------------------------------------------*
037100030226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
037200030226     C*----------------------------------------------------*
037300030226     C     IMPVAT        BEGSR
037400030226     C*
037500030226     C                   MOVEL     datcor        VATAAS
037600030226     C*
037700030415     C* Valorizzo il barcode
037800030804     C                   EVAL      VATNOT=%trim(%subst(vindta:29:24))
037900030226     C*
038000030226     C                   ENDSR
038100010201     C*----------------------------------------------------*
038200021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
038300010201     C*----------------------------------------------------*
038400020305     C     WRIVAT        BEGSR
038500010201     C*
038600021017     C                   write     FIVAT000
038700010201     C*
038800010201     C                   ENDSR
038900010202     C*----------------------------------------------------*
039000021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
039100010202     C*----------------------------------------------------*
039200020305     C     PREVAT        BEGSR
039300010202     C*
039400021017     C* Compongo il nome del membro da dare al FIVATWWR
039500010202     C                   eval      parmbr = vlrhdl
039600010202     C                   movel     'M'           parmbr
039700070103     C                   eval      parccm = '00013179'
039800010202     C                   eval      paropz = '1'
039900010202     C* Effettuo la chiamata al CLLE preposto
040000030730     C                   call(e)   'TITVVTC'
040100010202     C                   parm                    parccm
040200010202     C                   parm                    parmbr
040300010202     C                   parm                    paropz
040400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
040500010202     C                   if        %error
040600010202     C                   movel     '1'           chkcall
040700010202     C                   else
040800010202     C                   movel     '0'           chkcall
040900010202     C                   endif
041000010202     C*
041100010202     C                   ENDSR
041200000801     C*----------------------------------------------------*
041300000801     C*  CONTROLLO NUMERICITA' CAMPI
041400000801     C*----------------------------------------------------*
041500000801     C     CHKNUM        BEGSR
041600000801     C*
041700000801     C                   call(e)   'ISNUMERIC'
041800000801     C                   PARM                    PiStr            30
041900030430     C                   PARM      '.'           PiDecChr          1
042000000801     C                   PARM      *ZEROS        PiVal            30 9
042100000801     C                   PARM      '0'           PiInt             1
042200000801     C                   PARM      '0'           PiNum             1
042300000801     C                   IF        %error
042400000801     C                   EVAL      PiInt=*off
042500030107     C                   EVAL      PiNum=*off
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
048500021017
048600000801
048700000801
048800990920      /TITLE Invio dei dati al punto operativo.
048900010202     C     invio         BEGSR
049000990920     C*
049100021017     C* 1° invio FIVAT
049200010201     C                   reset                   dscmz
049300021017     C                   move      vlrpoi        cmzdst
049400021017     C                   eval      cmzfld = 'FIVATWWR'
049500010201     C                   eval      cmzmbd = vlrhdl
049600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
049700021009     C***                if        prmfir = *blanks
049800021017     C                   eval      cmzfla = 'FIVAT00F'
049900021017     C                   eval      cmzmba = 'FIVAT00F'
050000021009     C***                else
050100021009     C***                eval      cmzfla = prmfir
050200021009     C***                eval      cmzmba = prmfir
050300021009     C***                endif
050400010201     C                   eval      cmznrr = *zeros
050500020305     C                   move      §ctrokvt      cmznrr
050600021018     C                   eval      cmzlba = vlrfl1
050700010201     C                   call(e)   'TIS711C'
050800010201     C                   parm                    dscmz
050900010201     C                   parm      *blanks       esito
051000010205     C                   if        %error
051100010205     C                             or cmzerr = '1'
051200010205     C                             or esito  = '1'
051300010205     C                   eval      wrkesito = '3'
051400010205     C                   else
051500010201     C*
051600021017     C* 2° invio FIVAB
051700010201     C                   reset                   dscmz
051800021017     C                   move      vlrpoi        cmzdst
051900010201     C                   eval      cmzfld = vlrfou
052000010201     C                   eval      cmzmbd = vlrhdl
052100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
052200021009     C***                if        prmfir = *blanks
052300021017     C                   eval      cmzfla = 'FIVAB00F'
052400021017     C                   eval      cmzmba = 'FIVAB00F'
052500021009     C***                else
052600021009     C***                eval      cmzfla = prmfir
052700021009     C***                eval      cmzmba = prmfir
052800021009     C***                endif
052900010201     C                   eval      cmznrr = *zeros
053000010201     C                   move      §ctrokvb      cmznrr
053100021018     C                   eval      cmzlba = vlrfl1
053200010201     C                   call(e)   'TIS711C'
053300010201     C                   parm                    dscmz
053400010201     C                   parm      *blanks       esito
053500010201     C                   if        %error
053600010201     C                             or cmzerr = '1'
053700010201     C                             or esito  = '1'
053800010201     C                   eval      wrkesito = '3'
053900010201     C                   endif
054000010205     C                   endif
054100990920     C*
054200000613     C                   ENDSR
054300000613     C***
054400070411
054500070411     C     *pssr         BEGSR
054600070411     C*
054700070411     C                   if        %open(tivin00r)
054800070411     C                   close     tivin00r
054900070411     C                   endif
055000070411     C                   if        %open(fivabwwr)
055100070411     C                   close     fivabwwr
055200070411     C                   endif
055300070411     C                   if        %open(fivatwwr)
055400070411     C                   close     fivatwwr
055500070411     C                   endif
055600070411     C*
055700070411     C* Effettuo la chiamata al CLLE preposto
055800070411     C                   call(e)   'TITVVTC'
055900070411     C                   parm                    parccm
056000070411     C                   parm                    parmbr
056100070411     C                   parm      '2'           paropz
056200070411     C*
056300070411     C                   eval      wrkesito = '2'
056400070411     C*
056500070411     C                   seton                                        LR
056600070411     C*
056700070411     C                   ENDSR     '*CANCL'
056800070411     C***
056900070411
057000990910
057100000613     C     *inzsr        BEGSR
057200990910     C*
057300990910     C     *entry        plist
057400990920     C                   parm                    tivlrds
057500990921     C                   parm      wrkesito      esito
057600000724     C                   parm                    prmlit
057700000710     C                   parm                    prmfir
057800000613     C*
057900000830     C* CALCOLA LA DATA CORRENTE
058000000830     C                   time                    wn14             14 0
058100000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
058200000830     C                   z-add     wn8           g08dat
058300000830     C                   z-add     *zeros        g08inv
058400000830     C                   movel     '0'           g08err
058500000830     C                   call      'XSRDA8'
058600000830     C                   parm                    wlbda8
058700000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
058800000830     C*
058900000613     C                   ENDSR
059000000613     C***
