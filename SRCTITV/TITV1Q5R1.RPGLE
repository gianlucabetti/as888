000100090114      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130114     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500090114     FFIVABwwr  O    E             DISK    usropn
000600090114     FFIVATwwr  O    E             DISK    usropn
000700990908
000800000801     D*----------------------------------------------------
000900000801     D* DICHIARAZIOINE VARIABILI DI WRK
001000000801     D*----------------------------------------------------
001100990920     D dscmz         e ds                  inz
001200990910     D psds           sds
001300990910     D  procname         *PROC
001400990920     D tivlrds       e ds                  extname(tivlr00f)
001500060307     D tisi95ds      e ds
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
003400090114
003500081217
003600081217     D*------------------
003700081217     D* LINKING A DEFINIZIONI ESTERNE
003800081217     D*------------------
003900081217     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
004000081217     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
004100081217
004200081217
004300010201
004400081217
004500081217     C
004600000913     C                   reset                   rrnum
004700990921     C                   reset                   esito
004800990921     C                   reset                   wrkesito
004900000613     C*
005000040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005100000613     C*
005200010202     C* Effettuo la chiamata al CLLE preposto
005300090114     C                   call(e)   'TITVVTC'
005400010202     C                   parm                    parccm
005500010202     C                   parm                    parmbr
005600010202     C                   parm      '2'           paropz
005700050201     C*
005800050201     C* Effettuo lancio TISI95 solo x chiusura
005900050201     C                   CLEAR                   TISI95DS
006000050201     C                   EVAL      I95TLA = 'C'
006100050201     C                   CALL      'TISI95R'
006200050201     C                   PARM                    TISI95DS
006300000616     C*
006400000801     C
006500010201     C                   seton                                        LR
006600990908
006700000801
006800910830     C*--------------------------------------------------------
006900090114     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
007000910830     C*--------------------------------------------------------
007100040526     C     RWFILE        BEGSR
007200990910     C*
007300990914     C                   if        not %open(tivin00r)
007400990908     C                   open      tivin00r
007500990914     C                   endif
007600090114     C                   if        not %open(fivabwwr)
007700090114     C                   open      fivabwwr
007800990914     C                   endif
007900070103     C*
008000090114     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
008100020305     C                   exsr      prevat
008200010201     C*
008300010202     C                   if        chkcall = '0'
008400010202     C*
008500090114     C                   if        not %open(fivatwwr)
008600090114     C                   open      fivatwwr
008700010201     C                   endif
008800990910     C*
008900010201     C                   clear                   §CTROKVB          5 0
009000020305     C                   clear                   §CTROKVT          5 0
009100000801     C                   clear                   §CTRMO            5 0
009200000801     C                   clear                   §CTRNO            5 0
009300990910     C*
009400921023     C                   DO        *HIVAL
009500990913     C*
009600990915     C                   READ      tivin00r                               70
009700050627     C                   if        vindta > *blanks
009800000613     C                   add       1             rrnum
009900000801     C*
010000000801     C                   if        *in70 = *off
010100000801     C                             and
010200000801     C                             (vinflg = *blanks
010300000801     C                              or vinflg = '0'
010400000801     C                              or vinflg = '2')
010500000801     C*
010600000801     C                   clear                   vinmsg
010700000801     C                   eval      vinflg = '1'
010800070103     C*
010900090309     C                   EXSR      INZVAR
011000090309     C                   EXSR      DEFCAM
011100090527     C*
011200070213     C                   exsr      impvab                                       => carico  VAB
011300090527     C                   exsr      wrivab                                       => scarico VAB
011400000905     C*
011500000905     C                   else
011600000905     C                   eval      vinflg = '1'
011700050628     C                   endif
011800000905     C                   endif
011900000905     C*
012000000905     C  N70              update    tivin000
012100000905     C*
012200991022     C  N70              ENDdo
012300010202     C*
012400010202     C                   endif
012500990910
012600990910     C* Se non ci sono record con errori ...
012700000710     C                   if        §ctrno = 0
012800990910     C* ... restituisco esito OK.
012900990921     C                   eval      wrkesito = '0'
013000990910     C                   else
013100010201     C                   if        §ctrokvb > 0
013200990921     C                   eval      wrkesito = '1'
013300000710     C                   else
013400000710     C                   eval      wrkesito = '2'
013500990910     C                   endif
013600000710     C                   endif
013700990910     C*
013800990914     C                   if        %open(tivin00r)
013900990908     C                   close     tivin00r
014000990914     C                   endif
014100090114     C                   if        %open(fivabwwr)
014200090114     C                   close     fivabwwr
014300990914     C                   endif
014400090114     C                   if        %open(fivatwwr)
014500090114     C                   close     fivatwwr
014600010201     C                   endif
014700990910     C*
014800010201     C                   if        §ctrokvb > 0
014900000724     C                             and vlrpoi <> *zeros
015000010202     C                   exsr      invio
015100990920     C                   endif
015200990920     C*
015300910830     C                   ENDSR
015400000613     C***
015500010305
015600010305     C*----------------------------------------------------*
015700020305     C*  SCARICAMENTO BUFFER RECORDS VAB
015800010305     C*----------------------------------------------------*
015900020305     C     WRIVAB        BEGSR
016000010305     C*
016100060225     C* Quindi scarico il buffer del file d testata
016200090114     C                   write     fivab000                                     => scarico il VAB
016300010305     C*
016400010305     C                   ENDSR
016500990920
016600000801     C*----------------------------------------------------*
016700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
016800000801     C*----------------------------------------------------*
016900010201     C     INZVAR        BEGSR
017000000801     C*
017100090114     C                   CLEAR                   FIVAB000
017200090114     C                   CLEAR                   FIVAT000
017300070213     C*
017400040802     C                   Z-ADD     *zeros        Num5_0            5 0
017500040802     C                   MOVEL     '0'           FlgCAS            1
017600000801     C*
017700000801     C                   ENDSR
017800000801     C*----------------------------------------------------*
017900000801     C*  IMPOSTAZIONE CAMPI COSTANTI
018000000801     C*----------------------------------------------------*
018100000801     C     DEFCAM        BEGSR
018200000801     C*
018300020619     C* Imposto i valori di default...
018400090527     C                   MOVEL     datcor        VABAAS
018500090527     C                   MOVEL     datcor        VATAAS
018600090527     C                   MOVE(P)   vlrpoi        VABFGS
018700090527     C                   MOVE(P)   vlrpoi        VATFGS
018800090527     C*
018900090617     C                   Z-ADD     2751073       VABCCM
019000090617     C                   Z-ADD     2751073       VATCCM
019100090617     C                   Z-ADD     275           VABLNP
019200090617     C                   Z-ADD     275           VATLNP
019300090519     C                   Z-ADD     000           VABCTR
019400040823     C                   MOVEL     '1'           VABCBO
019500020619     C* ... e poi verifico se sono stati passati come parametri
019600020619     C                   IF        vlrppt > *blanks
019700040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
019800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
019900020619     C                   EXSR      CHKNUM
020000020619     C                   IF        PiInt=*on
020100020619     C                   Z-ADD     PiVal         VABCCM
020200020619     C                   Z-ADD     PiVal         VATCCM
020300020619     C                   ENDIF
020400040506     C                   ENDIF
020500040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
020600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
020700020619     C                   EXSR      CHKNUM
020800020619     C                   IF        PiInt=*on
020900020619     C                   Z-ADD     PiVal         VABLNP
021000020619     C                   Z-ADD     PiVal         VATLNP
021100040506     C                   ENDIF
021200020619     C                   ENDIF
021300040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
021400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
021500020619     C                   EXSR      CHKNUM
021600020619     C                   IF        PiInt=*on
021700020619     C                   Z-ADD     PiVal         VABCTR
021800040506     C                   ENDIF
021900020619     C                   ENDIF
022000020619     C                   ENDIF
022100000801     C*
022200000801     C                   ENDSR
022300000801     C*----------------------------------------------------*
022400090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
022500000801     C*----------------------------------------------------*
022600040823     C     IMPVAB        BEGSR
022700010305     C*
022800000801     C                   Z-ADD     *zeros        errore            1 0
022900040526     C                   MOVE      datcor        VABMGS
023000080409     C*
023100080409     C* Reperimento campi ALFA
023200080409     C*
023300080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
023400100531     C                   EVAL      VABRSD=%trim(%subst(vindta:236:35))
023500080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
023600080409     C     '@':'A'       XLATE     VABRSD        VABRSD
023700080409     C* ==
023800100531     C                   EVAL      VABRD2=%trim(%subst(vindta:271:35))
023900100531     C                   EVAL      VABIND=%trim(%subst(vindta:306:50))
024000100531     C                   EVAL      VABLOD=%trim(%subst(vindta:356:40))
024100100531     C                   EVAL      VABPRD=%trim(%subst(vindta:401:2))
024200100531     C                   EVAL      VABNOT=%trim(%subst(vindta:422:35))
024300100531     C                   EVAL      VABNT2=%trim(%subst(vindta:457:35))
024400100531     C                   EVAL      VABRMA=%trim(%subst(vindta:35:7))
024500080409     C*
024600080409     C* Reperimento campi NUMERICI
024700090609     C*
024800090309     C* NSP
024900100531     C                   EVAL      PiStr=%trim(%subst(vindta:35:7))
025000090309     C                   EXSR      CHKNUM
025100090309     C                   IF        PiInt=*on
025200090309     C                   Z-ADD     PiVal         VABNSP
025300090309     C                   Z-ADD     PiVal         VATNSP
025400090309     C                   ELSE
025500090309     C                   EVAL      vinmsg = %trimr(vinmsg)
025600090309     C                             + ' ' + 'VABNSP VATNSP'
025700090309     C                   ENDIF
025800090309     C* RMN
025900100531     C                   EVAL      PiStr=%trim(%subst(vindta:35:7))
026000081217     C                   EXSR      CHKNUM
026100081217     C                   IF        PiInt=*on
026200081217     C                   Z-ADD     PiVal         VABRMN
026300081217     C                   ELSE
026400081217     C                   ADD       1             errore
026500081217     C                   Z-ADD     1             VABRMN
026600081217     C                   EVAL      vinmsg = %trimr(vinmsg)
026700081217     C                             + ' ' + 'VABRMN'
026800081217     C                   ENDIF
026900080409     C* CAD
027000100531     C                   EVAL      PiStr=%trim(%subst(vindta:396:5))
027100090114     C                   EXSR      CHKNUM
027200090114     C                   IF        PiInt=*on
027300090114     C                   Z-ADD     PiVal         Num5_0
027400090114     C                   MOVEL(p)  Num5_0        VABCAD
027500090114     C                   ELSE
027600090114     C                   ADD       1             errore
027700090114     C                   EVAL      VABCAD = *zeros
027800090114     C                   EVAL      vinmsg = %trimr(vinmsg)
027900090114     C                             + ' ' + 'VABCAD'
028000090114     C                   ENDIF
028100081217     C* NCL
028200100531     C                   EVAL      PiStr=%trim(%subst(vindta:406:4))
028300081217     C                   EXSR      CHKNUM
028400081217     C                   IF        PiInt=*on
028500081217     C                   Z-ADD     PiVal         VABNCL
028600081217     C                   ELSE
028700090309     C                   ADD       1             errore
028800081217     C                   EVAL      vinmsg = %trimr(vinmsg)
028900081217     C                             + ' ' + 'VABNCL'
029000081217     C                   ENDIF
029100090312     C* PKB
029200100531     C                   EVAL      PiStr=%trim(%subst(vindta:410:4))
029300090312     C                   EXSR      CHKNUM
029400090312     C                   IF        PiNum=*on
029500090527     C                   Z-ADD     PiVal         VABPKB
029600090312     C                   ELSE
029700090312     C                   ADD       1             errore
029800090312     C                   EVAL      vinmsg = %trimr(vinmsg)
029900090312     C                             + ' ' + 'VABPKB'
030000090312     C                   ENDIF
030100090527     C* CAS
030200100531     C                   IF        %subst(vindta:414:8)        <> *blanks AND
030300100531     C                             %trim(%subst(vindta:414:8)) <> *zeros
030400090609     C                   MOVEL     '1'           FlgCAS
030500090527     C                   EVAL      VABVCA = 'EUR'
030600090907     C                   EVAL      VABTIC = 'BM'
030700100531     C                   EVAL      PiStr=%trim(%subst(vindta:414:8))
030800090428     C                   EXSR      CHKNUM
030900090428     C                   IF        PiNum=*on
031000100531     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
031100090527     C                   Z-ADD     PiVal         VABCAS
031200090428     C                   ELSE
031300090428     C                   ADD       1             errore
031400090428     C                   EVAL      vinmsg = %trimr(vinmsg)
031500090527     C                             + ' ' + 'VABCAS'
031600090428     C                   ENDIF
031700090428     C                   ENDIF
031800010205     C*
031900010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
032000040802     C                   IF        FlgCAS <> '0'
032100090609     C                   IF        VABCBO = '1'
032200010205     C                   EVAL      VABCBO = '4'
032300090609     C                   ENDIF
032400090609     C                   IF        VABCBO = '2'
032500090609     C                   EVAL      VABCBO = '6'
032600090609     C                   ENDIF
032700010205     C                   ENDIF
032800020305     C*
032900011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
033000011113     C                   EXSR      CHKIMPDIV
033100010202     C*
033200000801     C* Ebbene...
033300000801     C                   ADD       1             §CTRMO
033400010201     C                   IF        errore <> *zeros
033500000801     C                   ADD       1             §CTRNO
033600000801     C                   EVAL      vinflg = '2'
033700000801     C                   ELSE
033800010201     C                   ADD       1             §CTROKVB
033900000801     C                   ENDIF
034000000801     C*
034100000801     C                   ENDSR
034200070102     C*----------------------------------------------------*
034300090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
034400070102     C*----------------------------------------------------*
034500070103     C     EXEVATE       BEGSR
034600090527     C*
034700090617     C***                EVAL      VATTRC = 'E'
034800090617     C***                EVAL      VATNOT = %trim(%subst(vindta:495:22))
034900090428     C*
035000090609     C***                exsr      wrivat                                       => scarico VAT
035100070102     C*
035200070102     C                   ENDSR
035300080415     C*----------------------------------------------------*
035400090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
035500080415     C*----------------------------------------------------*
035600080415     C     EXEVATA       BEGSR
035700080415     C*
035800090617     C***                EVAL      VATTRC = 'A'
035900090617     C***                EVAL      VATNOT = %trim(%subst(vindta:495:22))
036000080415     C*
036100090617     C***                exsr      wrivat                                       => scarico VAT
036200080415     C*
036300080415     C                   ENDSR
036400071121     C*----------------------------------------------------*
036500090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
036600071121     C*----------------------------------------------------*
036700071121     C     EXEVATB       BEGSR
036800071121     C*
036900090617     C***                EVAL      VATTRC = 'B'
037000090617     C***                EVAL      VATNOT = %trim(%subst(vindta:517:16))
037100071121     C*
037200090617     C***                exsr      wrivat                                       => scarico VAT
037300071121     C*
037400071121     C                   ENDSR
037500010201     C*----------------------------------------------------*
037600090114     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
037700010201     C*----------------------------------------------------*
037800020305     C     WRIVAT        BEGSR
037900050628     C*
038000060223     C* Scrivo solo se valorizzato qualcosa
038100060223     C                   IF        VATNOT <> *blanks
038200090114     C                   WRITE     FIVAT000
038300060223     C                   ENDIF
038400010201     C*
038500010201     C                   ENDSR
038600010202     C*----------------------------------------------------*
038700090114     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
038800010202     C*----------------------------------------------------*
038900020305     C     PREVAT        BEGSR
039000010202     C*
039100090114     C* Compongo il nome del membro da dare al FIVATWWR
039200010202     C                   eval      parmbr = vlrhdl
039300010202     C                   movel     'M'           parmbr
039400050627     C                   eval      parccm = %subst(vlrKSC:2:7)
039500010202     C                   eval      paropz = '1'
039600010202     C* Effettuo la chiamata al CLLE preposto
039700090114     C                   call(e)   'TITVVTC'
039800010202     C                   parm                    parccm
039900010202     C                   parm                    parmbr
040000010202     C                   parm                    paropz
040100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
040200010202     C                   if        %error
040300010202     C                   movel     '1'           chkcall
040400010202     C                   else
040500010202     C                   movel     '0'           chkcall
040600010202     C                   endif
040700010202     C*
040800010202     C                   ENDSR
040900000801     C*----------------------------------------------------*
041000000801     C*  CONTROLLO NUMERICITA' CAMPI
041100000801     C*----------------------------------------------------*
041200000801     C     CHKNUM        BEGSR
041300081217     C*
041400081217     C                   IF        PiDecChr = *blanks
041500090617     C                   EVAL      PiDecChr = '.'
041600081217     C                   ENDIF
041700081217     C*
041800081217     C                   callp(e)  UBISNUM_Check(PiStr
041900081217     C                                          :PiDecChr
042000081217     C                                          :PiVal
042100081217     C                                          :PiNum
042200081217     C                                          :PiInt)
042300081217     C*
042400081217     C                   IF        %error
042500081217     C                   EVAL      PiInt=*off
042600081217     C                   ENDIF
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
049200090114     C* 1° invio FIVAT
049300010201     C                   reset                   dscmz
049400010201     C                   move      vlrpoi        cmzdst
049500090114     C                   eval      cmzfld = 'FIVATWWR'
049600010201     C                   eval      cmzmbd = vlrhdl
049700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
049800021009     C***                if        prmfir = *blanks
049900090114     C                   eval      cmzfla = 'FIVAT00F'
050000090114     C                   eval      cmzmba = 'FIVAT00F'
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
051700090114     C* 2° invio FIVAB
051800010201     C                   reset                   dscmz
051900010201     C                   move      vlrpoi        cmzdst
052000010201     C                   eval      cmzfld = vlrfou
052100010201     C                   eval      cmzmbd = vlrhdl
052200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
052300021009     C***                if        prmfir = *blanks
052400090114     C                   eval      cmzfla = 'FIVAB00F'
052500090114     C                   eval      cmzmba = 'FIVAB00F'
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
054600090331
054700090331
054800090331
054900070411     C     *pssr         BEGSR
055000070411     C*
055100070411     C                   if        %open(tivin00r)
055200070411     C                   close     tivin00r
055300070411     C                   endif
055400090114     C                   if        %open(fivabwwr)
055500090114     C                   close     fivabwwr
055600070411     C                   endif
055700090114     C                   if        %open(fivatwwr)
055800090114     C                   close     fivatwwr
055900070411     C                   endif
056000070411     C*
056100070411     C* Effettuo la chiamata al CLLE preposto
056200090114     C                   call(e)   'TITVVTC'
056300070411     C                   parm                    parccm
056400070411     C                   parm                    parmbr
056500070411     C                   parm      '2'           paropz
056600070411     C*
056700070411     C                   eval      wrkesito = '2'
056800070411     C*
056900070411     C                   seton                                        LR
057000070411     C*
057100070411     C                   ENDSR     '*CANCL'
057200070411     C***
057300070411
057400090331
057500090331
057600990910
057700000613     C     *inzsr        BEGSR
057800990910     C*
057900990910     C     *entry        plist
058000990920     C                   parm                    tivlrds
058100990921     C                   parm      wrkesito      esito
058200000724     C                   parm                    prmlit
058300000710     C                   parm                    prmfir
058400000613     C*
058500000830     C* CALCOLA LA DATA CORRENTE
058600000830     C                   time                    wn14             14 0
058700000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
058800080512     C                   movel     wn14          oracor            6 0          *ORA
058900000830     C                   z-add     wn8           g08dat
059000000830     C                   z-add     *zeros        g08inv
059100000830     C                   movel     '0'           g08err
059200000830     C                   call      'XSRDA8'
059300000830     C                   parm                    wlbda8
059400000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
059500000830     C*
059600000613     C                   ENDSR
