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
002500100122 xxx D curSped         s              7    INZ(*blanks)
002600100122 xxx D depSped         s              7    INZ(*blanks)
002700000830
002800000830     D*------------------
002900000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003000000830     D*------------------
003100000830     D WLBDA8          DS                  INZ
003200000830     D  G08DAT                 1      8  0
003300000830     D  G08INV                 9     16  0
003400000830     D  G08ERR                17     17
003500000830     D  G08TGI                18     22  0
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
007100070103     C*
007200021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007300020305     C                   exsr      prevat
007400010201     C*
007500010202     C                   if        chkcall = '0'
007600010202     C*
007700021113     C                   if        not %open(fivatwwr)
007800021113     C                   open      fivatwwr
007900010201     C                   endif
008000990910     C*
008100010201     C                   clear                   §CTROKVB          5 0
008200020305     C                   clear                   §CTROKVT          5 0
008300000801     C                   clear                   §CTRMO            5 0
008400000801     C                   clear                   §CTRNO            5 0
008500990910     C*
008600921023     C                   DO        *HIVAL
008700990913     C*
008800990915     C                   READ      tivin00r                               70
008900050627     C                   if        vindta > *blanks
009000000613     C                   add       1             rrnum
009100000801     C*
009200000801     C                   if        *in70 = *off
009300000801     C                             and
009400000801     C                             (vinflg = *blanks
009500000801     C                              or vinflg = '0'
009600000801     C                              or vinflg = '2')
009700000801     C*
009800000801     C                   clear                   vinmsg
009900000801     C                   eval      vinflg = '1'
010000070103     C*
010100070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
010200100122 xxx C                   eval      curSped = %subst(vindta:6:7)
010300070213     C                   if        curSped <> depSped
010400100122 xxx C*
010500100122 xxx C* Forzatura x errore in modifica al file lato cliente
010600100122 xxx C                   Z-ADD     *zeros        wNCLn             3 0
010700100122 xxx C                   MOVEL     *blanks       wNCLa             3
010800100112     C*
010900070213     C* Se prima bolla => importo bolla corrente
011000070213     C                   if        §CTRMO = *zeros
011100070213     C                   exsr      inzvar
011200070213     C                   exsr      defcam
011300070213     C                   exsr      impvab                                       => carico  VAB
011400070213     C                   else
011500100112     C*
011600070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
011700070213     C                   exsr      wrivab                                       => scarico VAB
011800070213     C                   exsr      inzvar
011900070213     C                   exsr      defcam
012000070213     C                   exsr      impvab                                       => carico  VAB
012100070213     C                   endif
012200100122     C                   exsr      exevate                                      => write VAT-E
012300100112     C*
012400070103     C* Salvo il raggruppamento spedizione cliente corrente
012500070103     C                   eval      depSped = curSped
012600070213     C*
012700070213     C* Se collo successivo x stessa bolla
012800070213     C                   else
012900100112     C*
013000070213     C                   exsr      impvab                                       => carico  VAB
013100100122     C                   exsr      exevate                                      => write VAT-E
013200100112     C*
013300070103     C                   endif
013400000905     C*
013500000905     C                   else
013600000905     C                   eval      vinflg = '1'
013700050628     C                   endif
013800000905     C                   endif
013900000905     C*
014000000905     C  N70              update    tivin000
014100000905     C*
014200991022     C  N70              ENDdo
014300070213     C*
014400070213     C* Scarico testata bolla rimasta in sospesa
014500070213     C                   exsr      wrivab                                       => scarico VAB
014600010202     C*
014700010202     C                   endif
014800990910
014900990910     C* Se non ci sono record con errori ...
015000000710     C                   if        §ctrno = 0
015100990910     C* ... restituisco esito OK.
015200990921     C                   eval      wrkesito = '0'
015300990910     C                   else
015400010201     C                   if        §ctrokvb > 0
015500990921     C                   eval      wrkesito = '1'
015600000710     C                   else
015700000710     C                   eval      wrkesito = '2'
015800990910     C                   endif
015900000710     C                   endif
016000990910     C*
016100990914     C                   if        %open(tivin00r)
016200990908     C                   close     tivin00r
016300990914     C                   endif
016400021113     C                   if        %open(fivabwwr)
016500021113     C                   close     fivabwwr
016600990914     C                   endif
016700021113     C                   if        %open(fivatwwr)
016800021113     C                   close     fivatwwr
016900010201     C                   endif
017000990910     C*
017100010201     C                   if        §ctrokvb > 0
017200000724     C                             and vlrpoi <> *zeros
017300010202     C                   exsr      invio
017400990920     C                   endif
017500990920     C*
017600910830     C                   ENDSR
017700000613     C***
017800010305
017900010305     C*----------------------------------------------------*
018000020305     C*  SCARICAMENTO BUFFER RECORDS VAB
018100010305     C*----------------------------------------------------*
018200020305     C     WRIVAB        BEGSR
018300010305     C*
018400060225     C* Quindi scarico il buffer del file d testata
018500021113     C                   write     fivab000                                     => scarico il VAB
018600010305     C*
018700010305     C                   ENDSR
018800990920
018900000801     C*----------------------------------------------------*
019000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019100000801     C*----------------------------------------------------*
019200010201     C     INZVAR        BEGSR
019300000801     C*
019400070213     C                   CLEAR                   FIVAB000
019500070213     C                   CLEAR                   FIVAT000
019600070213     C*
019700040802     C                   Z-ADD     *zeros        Num5_0            5 0
019800040802     C                   MOVEL     '0'           FlgCAS            1
019900000801     C*
020000000801     C                   ENDSR
020100000801     C*----------------------------------------------------*
020200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
020300000801     C*----------------------------------------------------*
020400000801     C     DEFCAM        BEGSR
020500000801     C*
020600020619     C* Imposto i valori di default...
020700100909     C                   Z-ADD     2230012       VABCCM
020800100909     C                   Z-ADD     2230012       VATCCM
020900100909     C                   Z-ADD     223           VABLNP
021000100909     C                   Z-ADD     223           VATLNP
021100070927     C                   Z-ADD     000           VABCTR
021200040823     C                   MOVEL     '1'           VABCBO
021300070928     C                   MOVEL     'E'           VABTSP
021400100126     C                   MOVEL     '7Q'          VABCTM
021500020619     C* ... e poi verifico se sono stati passati come parametri
021600020619     C                   IF        vlrppt > *blanks
021700040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
021800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
021900020619     C                   EXSR      CHKNUM
022000020619     C                   IF        PiInt=*on
022100020619     C                   Z-ADD     PiVal         VABCCM
022200020619     C                   Z-ADD     PiVal         VATCCM
022300020619     C                   ENDIF
022400040506     C                   ENDIF
022500040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
022600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022700020619     C                   EXSR      CHKNUM
022800020619     C                   IF        PiInt=*on
022900020619     C                   Z-ADD     PiVal         VABLNP
023000020619     C                   Z-ADD     PiVal         VATLNP
023100040506     C                   ENDIF
023200020619     C                   ENDIF
023300040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
023400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
023500020619     C                   EXSR      CHKNUM
023600020619     C                   IF        PiInt=*on
023700020619     C                   Z-ADD     PiVal         VABCTR
023800040506     C                   ENDIF
023900020619     C                   ENDIF
024000020619     C                   ENDIF
024100000801     C*
024200000801     C                   ENDSR
024300000801     C*----------------------------------------------------*
024400021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
024500000801     C*----------------------------------------------------*
024600040823     C     IMPVAB        BEGSR
024700100112     C*
024800100122 xxx C                   EVAL      wNCLa=%trim(%subst(vindta:16:3))
024900100122 xxx C                   MOVE(P)   wNCLa         wNCLn
025000100122 xxx C                   IF        VABNCL < wNCLn
025100010305     C*
025200000801     C                   Z-ADD     *zeros        errore            1 0
025300000830     C                   MOVEL     datcor        VABAAS
025400020305     C                   MOVEL     datcor        VATAAS
025500040526     C                   MOVE      datcor        VABMGS
025600040823     C                   MOVE(P)   vlrpoi        VABFGS
025700040823     C                   MOVE(P)   vlrpoi        VATFGS
025800050628     C*
025900100112     C                   EVAL      VABRSD=%trim(%subst(vindta:174:35))
026000020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026100020117     C     '@':'A'       XLATE     VABRSD        VABRSD
026200020117     C* ==
026300100112     C                   EVAL      VABRD2=%trim(%subst(vindta:209:35))
026400100112     C                   EVAL      VABIND=%trim(%subst(vindta:244:35))
026500100112     C                   EVAL      VABLOD=%trim(%subst(vindta:279:35))
026600100112     C                   EVAL      VABPRD=%trim(%subst(vindta:341:2))
026700100112     C                   EVAL      VABRMA=%trim(%subst(vindta:89:6))
026800100112     C                   EVAL      VABNOT=%trim(%subst(vindta:482:35))
026900100112     C                   EVAL      VABNT2=%trim(%subst(vindta:482+35:60-35))
027000050628     C* CAP
027100100112     C                   EVAL      PiStr=%trim(%subst(vindta:334:7))
027200010201     C                   EXSR      CHKNUM
027300010201     C                   IF        PiInt=*on
027400010201     C                   Z-ADD     PiVal         Num5_0
027500040506     C                   MOVEL(P)  Num5_0        VABCAD
027600010201     C                   ELSE
027700040506     C                   ADD       1             errore
027800010201     C                   EVAL      vinmsg = %trimr(vinmsg)
027900040506     C                             + ' ' + 'VABCAD'
028000010201     C                   ENDIF
028100040506     C* Reperisco la provincia dal CAP e dalla località
028200040526     C                   IF        VABCAD <> *blanks AND
028300040526     C                             VABPRD  = *blanks
028400040506     C                   CLEAR                   TISI95DS
028500040506     C                   EVAL      I95TCN = '3'
028600040506     C                   Z-ADD     datcor        I95DAT
028700040506     C                   EVAL      I95CAP = VABCAD
028800040506     C                   EVAL      I95LOC = VABLOD
028900050627     C                   EVAL      I95NAR = VABNZD
029000040506     C                   CALL      'TISI95R'
029100040506     C                   PARM                    TISI95DS
029200040506     C                   EVAL      VABPRD = O95PRV
029300040506     C                   ENDIF
029400100122 xxx C* NCL
029500100122 xxx C                   EVAL      VABNCL = VABNCL + 1
029600040506     C* PKB
029700100112     C                   EVAL      PiStr=%trim(%subst(vindta:378:7))
029800010201     C                   EXSR      CHKNUM
029900010201     C                   IF        PiNum=*on
030000070927     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
030100070927     C                   ADD(H)    PiVal         VABPKB
030200010201     C                   ELSE
030300010201     C                   ADD       1             errore
030400010201     C                   EVAL      vinmsg = %trimr(vinmsg)
030500010201     C                             + ' ' + 'VABPKB'
030600010201     C                   ENDIF
030700080111     C* VLB
030800100112     C                   EVAL      PiStr=%trim(%subst(vindta:403:7))
030900080111     C                   EXSR      CHKNUM
031000080111     C                   IF        PiNum=*on
031100080111     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
031200080111     C                   ADD(H)    PiVal         VABVLB
031300080111     C                   ELSE
031400080111     C                   ADD       1             errore
031500080111     C                   EVAL      vinmsg = %trimr(vinmsg)
031600080111     C                             + ' ' + 'VABVLB'
031700080111     C                   ENDIF
031800071019     C* RMN
031900100112     C*                  EVAL      PiStr=%trim(%subst(vindta:6:7))
032000071019     C*                  EXSR      CHKNUM
032100071019     C*                  IF        PiInt=*on
032200071019     C*                  Z-ADD     PiVal         VABRMN
032300071019     C*                  ELSE
032400071019     C*                  ADD       1             errore
032500071019     C*                  EVAL      vinmsg = %trimr(vinmsg)
032600071019     C*                            + ' ' + 'VABRMN'
032700071019     C*                  ENDIF
032800071019     C* NSP/RMN
032900100122 xxx C                   EVAL      PiStr=%trim(%subst(vindta:101:7))
033000071019     C                   EXSR      CHKNUM
033100071019     C                   IF        PiInt=*on
033200071019     C                   Z-ADD     PiVal         VABNSP
033300071019     C                   Z-ADD     PiVal         VATNSP
033400071019     C                   Z-ADD     PiVal         VABRMN
033500071019     C                   ELSE
033600071019     C                   ADD       1             errore
033700071019     C                   EVAL      vinmsg = %trimr(vinmsg)
033800071019     C                             + ' ' + 'VABNSP VATNSP VABRMN'
033900071019     C                   ENDIF
034000010205     C*
034100010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
034200040802     C                   IF        FlgCAS <> '0'
034300070102     C                   IF        VABCBO = '1'
034400010205     C                   EVAL      VABCBO = '4'
034500010205     C                   ELSE
034600070102     C                   EVAL      VABCBO = '6'
034700070102     C                   ENDIF
034800010205     C                   ENDIF
034900020305     C*
035000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
035100011113     C                   EXSR      CHKIMPDIV
035200100112     C*
035300100122 xxx C                   ENDIF
035400010202     C*
035500000801     C* Ebbene...
035600000801     C                   ADD       1             §CTRMO
035700010201     C                   IF        errore <> *zeros
035800000801     C                   ADD       1             §CTRNO
035900000801     C                   EVAL      vinflg = '2'
036000000801     C                   ELSE
036100010201     C                   ADD       1             §CTROKVB
036200000801     C                   ENDIF
036300000801     C*
036400000801     C                   ENDSR
036500070102     C*----------------------------------------------------*
036600070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
036700070102     C*----------------------------------------------------*
036800070103     C     EXEVATE       BEGSR
036900070102     C*
037000100126     C                   EVAL      VATTRC='E'
037100100126     C***                EVAL      VATNOT = %trim(%subst(vindta:1:19))
037200100126 xxx C                   EVAL      VATNOT = %trim(%subst(vindta:467:15))
037300071019     C* Occorre togliere gli spazi eventualmente presenti all'interno della stringa barcode
037400071019     C                   Z-ADD     2             i                 2 0
037500071019     C                   DOW       i < %size(VATNOT)
037600071019     C                   IF        %subst(VATNOT:i:1) = *blanks
037700071019     C                   EVAL      VATNOT = %subst(VATNOT:1:i-1) +
037800071019     C                                      %subst(VATNOT:i+1)
037900071019     C                   ENDIF
038000071019     C                   ADD       1             i
038100071019     C                   ENDDO
038200071019     C*
038300070102     C                   exsr      wrivat                                       => scarico VAT
038400070102     C*
038500070102     C                   ENDSR
038600010201     C*----------------------------------------------------*
038700040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
038800010201     C*----------------------------------------------------*
038900020305     C     WRIVAT        BEGSR
039000050628     C*
039100060223     C* Scrivo solo se valorizzato qualcosa
039200060223     C                   IF        VATNOT <> *blanks
039300040802     C                   WRITE     FIVAT000
039400060223     C                   ENDIF
039500010201     C*
039600010201     C                   ENDSR
039700010202     C*----------------------------------------------------*
039800021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
039900010202     C*----------------------------------------------------*
040000020305     C     PREVAT        BEGSR
040100010202     C*
040200021113     C* Compongo il nome del membro da dare al FIVATWWR
040300010202     C                   eval      parmbr = vlrhdl
040400010202     C                   movel     'M'           parmbr
040500050627     C                   eval      parccm = %subst(vlrKSC:2:7)
040600010202     C                   eval      paropz = '1'
040700010202     C* Effettuo la chiamata al CLLE preposto
040800040506     C                   call(e)   'TITVVTC'
040900010202     C                   parm                    parccm
041000010202     C                   parm                    parmbr
041100010202     C                   parm                    paropz
041200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
041300010202     C                   if        %error
041400010202     C                   movel     '1'           chkcall
041500010202     C                   else
041600010202     C                   movel     '0'           chkcall
041700010202     C                   endif
041800010202     C*
041900010202     C                   ENDSR
042000000801     C*----------------------------------------------------*
042100000801     C*  CONTROLLO NUMERICITA' CAMPI
042200000801     C*----------------------------------------------------*
042300000801     C     CHKNUM        BEGSR
042400000801     C*
042500000801     C                   call(e)   'ISNUMERIC'
042600000801     C                   PARM                    PiStr            30
042700070213     C                   PARM      ','           PiDecChr          1
042800000801     C                   PARM      *ZEROS        PiVal            30 9
042900000801     C                   PARM      '0'           PiInt             1
043000000801     C                   PARM      '0'           PiNum             1
043100000801     C                   IF        %error
043200000801     C                   EVAL      PiInt=*off
043300000801     C                   ENDIF
043400000801     C*
043500000801     C                   ENDSR
043600000801     C***
043700000801
043800011113
043900011113     C*----------------------------------------------------*
044000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
044100011113     C*----------------------------------------------------*
044200011113     C     CHKIMPDIV     BEGSR
044300011113     C*
044400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
044500011113     C                   Z-ADD     *zeros        wrkDec            9 9
044600011113     C*
044700011113     C* Come prima cosa effettuo considerazioni sulla divisa
044800011113     C                   IF        vabIAS > *zeros
044900011113     C                   IF        vabVAS <> 'EUR'
045000011113     C                   EVAL      vabVAS =  'ITL'
045100011113     C                   ENDIF
045200011113     C                   ENDIF
045300011113     C*
045400011113     C                   IF        vabCAS > *zeros
045500011113     C                   IF        vabVCA <> 'EUR'
045600011113     C                   EVAL      vabVCA =  'ITL'
045700011113     C                   ENDIF
045800011113     C                   ENDIF
045900011113     C*
046000011113     C                   IF        vabVMD > *zeros
046100020305     C                   IF        vabVAD <> 'EUR'
046200011113     C                   EVAL      vabVAD =  'ITL'
046300011113     C                   ENDIF
046400011113     C                   ENDIF
046500011113     C*
046600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
046700011113     C                   Z-ADD     vabIAS        wrkDec
046800011113     C                   IF        wrkDec > *zeros
046900011113     C                   IF        vabVAS = 'ITL'
047000011113     C                   EVAL      vabIAS = *zeros
047100011113     C                   ENDIF
047200011113     C                   ENDIF
047300011113     C*
047400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
047500011113     C                   Z-ADD     vabCAS        wrkDec
047600011113     C                   IF        wrkDec > *zeros
047700011113     C                   IF        vabVCA = 'ITL'
047800011113     C                   EVAL      vabCAS = *zeros
047900011113     C                   ENDIF
048000011113     C                   ENDIF
048100011113     C*
048200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
048300011113     C                   Z-ADD     vabVMD        wrkDec
048400011113     C                   IF        wrkDec > *zeros
048500011113     C                   IF        vabVAD = 'ITL'
048600011113     C                   EVAL      vabVMD = *zeros
048700011113     C                   ENDIF
048800011113     C                   ENDIF
048900011113     C*
049000011113     C                   ENDSR
049100011113     C***
049200011113
049300011113
049400000801
049500000801
049600990920      /TITLE Invio dei dati al punto operativo.
049700010202     C     invio         BEGSR
049800990920     C*
049900021113     C* 1° invio FIVAT
050000010201     C                   reset                   dscmz
050100010201     C                   move      vlrpoi        cmzdst
050200021113     C                   eval      cmzfld = 'FIVATWWR'
050300010201     C                   eval      cmzmbd = vlrhdl
050400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
050500021009     C***                if        prmfir = *blanks
050600021113     C                   eval      cmzfla = 'FIVAT00F'
050700021113     C                   eval      cmzmba = 'FIVAT00F'
050800021009     C***                else
050900021009     C***                eval      cmzfla = prmfir
051000021009     C***                eval      cmzmba = prmfir
051100021009     C***                endif
051200010201     C                   eval      cmznrr = *zeros
051300020305     C                   move      §ctrokvt      cmznrr
051400021018     C                   eval      cmzlba = vlrfl1
051500010201     C                   call(e)   'TIS711C'
051600010201     C                   parm                    dscmz
051700010201     C                   parm      *blanks       esito
051800010205     C                   if        %error
051900010205     C                             or cmzerr = '1'
052000010205     C                             or esito  = '1'
052100010205     C                   eval      wrkesito = '3'
052200010205     C                   else
052300010201     C*
052400021113     C* 2° invio FIVAB
052500010201     C                   reset                   dscmz
052600010201     C                   move      vlrpoi        cmzdst
052700010201     C                   eval      cmzfld = vlrfou
052800010201     C                   eval      cmzmbd = vlrhdl
052900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
053000021009     C***                if        prmfir = *blanks
053100021113     C                   eval      cmzfla = 'FIVAB00F'
053200021113     C                   eval      cmzmba = 'FIVAB00F'
053300021009     C***                else
053400021009     C***                eval      cmzfla = prmfir
053500021009     C***                eval      cmzmba = prmfir
053600021009     C***                endif
053700010201     C                   eval      cmznrr = *zeros
053800010201     C                   move      §ctrokvb      cmznrr
053900021018     C                   eval      cmzlba = vlrfl1
054000010201     C                   call(e)   'TIS711C'
054100010201     C                   parm                    dscmz
054200010201     C                   parm      *blanks       esito
054300010201     C                   if        %error
054400010201     C                             or cmzerr = '1'
054500010201     C                             or esito  = '1'
054600010201     C                   eval      wrkesito = '3'
054700010201     C                   endif
054800010205     C                   endif
054900990920     C*
055000000613     C                   ENDSR
055100000613     C***
055200070411
055300070411     C     *pssr         BEGSR
055400070411     C*
055500070411     C                   if        %open(tivin00r)
055600070411     C                   close     tivin00r
055700070411     C                   endif
055800070411     C                   if        %open(fivabwwr)
055900070411     C                   close     fivabwwr
056000070411     C                   endif
056100070411     C                   if        %open(fivatwwr)
056200070411     C                   close     fivatwwr
056300070411     C                   endif
056400070411     C*
056500070411     C* Effettuo la chiamata al CLLE preposto
056600070411     C                   call(e)   'TITVVTC'
056700070411     C                   parm                    parccm
056800070411     C                   parm                    parmbr
056900070411     C                   parm      '2'           paropz
057000070411     C*
057100070411     C                   eval      wrkesito = '2'
057200070411     C*
057300070411     C                   seton                                        LR
057400070411     C*
057500070411     C                   ENDSR     '*CANCL'
057600070411     C***
057700070411
057800990910
057900000613     C     *inzsr        BEGSR
058000990910     C*
058100990910     C     *entry        plist
058200990920     C                   parm                    tivlrds
058300990921     C                   parm      wrkesito      esito
058400000724     C                   parm                    prmlit
058500000710     C                   parm                    prmfir
058600000613     C*
058700000830     C* CALCOLA LA DATA CORRENTE
058800000830     C                   time                    wn14             14 0
058900000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
059000000830     C                   z-add     wn8           g08dat
059100000830     C                   z-add     *zeros        g08inv
059200000830     C                   movel     '0'           g08err
059300000830     C                   call      'XSRDA8'
059400000830     C                   parm                    wlbda8
059500000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
059600000830     C*
059700000613     C                   ENDSR
059800000613     C***
