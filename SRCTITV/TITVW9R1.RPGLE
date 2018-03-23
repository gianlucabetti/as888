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
002500081114     D curSped         s              6    INZ(*blanks)
002600081114     D depSped         s              6    INZ(*blanks)
002700000830
002800000830     D*------------------
002900000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003000000830     D*------------------
003100000830     D WLBDA8          DS                  INZ
003200000830     D  G08DAT                 1      8  0
003300000830     D  G08INV                 9     16  0
003400000830     D  G08ERR                17     17
003500000830     D  G08TGI                18     22  0
003600070103     D*------------------
003700070103     D* DS REPERIMENTO NUMERATORE
003800070103     D*------------------
003900070103     D trul33ds      e ds                  inz
004000070103     D*------------------
004100070103     D* DS ARCHITETTURA
004200070103     D*------------------
004300070103     D kpjba         e ds                  inz
004400000830     D*
004500010201
004600010201
004700000913     C                   reset                   rrnum
004800990921     C                   reset                   esito
004900120307     C                   reset                   wrkesito
005000000613     C*
005100040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005200000613     C*
005300010202     C* Effettuo la chiamata al CLLE preposto
005400040506     C                   call(e)   'TITVVTC'
005500010202     C                   parm                    parccm
005600010202     C                   parm                    parmbr
005700010202     C                   parm      '2'           paropz
005800050201     C*
005900050201     C* Effettuo lancio TISI95 solo x chiusura
006000050201     C                   CLEAR                   TISI95DS
006100050201     C                   EVAL      I95TLA = 'C'
006200050201     C                   CALL      'TISI95R'
006300050201     C                   PARM                    TISI95DS
006400000616     C*
006500000801     C
006600010201     C                   seton                                        LR
006700990908
006800000801
006900910830     C*--------------------------------------------------------
007000040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
007100910830     C*--------------------------------------------------------
007200040526     C     RWFILE        BEGSR
007300990910     C*
007400990914     C                   if        not %open(tivin00r)
007500990908     C                   open      tivin00r
007600990914     C                   endif
007700021113     C                   if        not %open(fivabwwr)
007800021113     C                   open      fivabwwr
007900990914     C                   endif
008000070103     C*
008100021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
008200020305     C                   exsr      prevat
008300010201     C*
008400010202     C                   if        chkcall = '0'
008500010202     C*
008600021113     C                   if        not %open(fivatwwr)
008700021113     C                   open      fivatwwr
008800010201     C                   endif
008900990910     C*
009000010201     C                   clear                   §CTROKVB          5 0
009100020305     C                   clear                   §CTROKVT          5 0
009200000801     C                   clear                   §CTRMO            5 0
009300000801     C                   clear                   §CTRNO            5 0
009400990910     C*
009500921023     C                   DO        *HIVAL
009600990913     C*
009700990915     C                   READ      tivin00r                               70
009800050627     C                   if        vindta > *blanks
009900000613     C                   add       1             rrnum
010000000801     C*
010100000801     C                   if        *in70 = *off
010200000801     C                             and
010300000801     C                             (vinflg = *blanks
010400000801     C                              or vinflg = '0'
010500000801     C                              or vinflg = '2')
010600000801     C*
010700000801     C                   clear                   vinmsg
010800000801     C                   eval      vinflg = '1'
010900070103     C*
011000070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
011100081114     C                   eval      curSped = %subst(vindta:163:6)
011200070213     C                   if        curSped <> depSped
011300070213     C* Se prima bolla => importo bolla corrente
011400070213     C                   if        §CTRMO = *zeros
011500070213     C                   exsr      inzvar
011600070213     C                   exsr      defcam
011700070213     C                   exsr      repnum
011800070213     C                   exsr      impvab                                       => carico  VAB
011900070213     C                   else
012000070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
012100070213     C                   exsr      wrivab                                       => scarico VAB
012200070213     C                   exsr      inzvar
012300070213     C                   exsr      defcam
012400070213     C                   exsr      repnum
012500070213     C                   exsr      impvab                                       => carico  VAB
012600070213     C                   endif
012700070213     C                   exsr      exevate                                      => write VAT-E
012800070103     C* Salvo il raggruppamento spedizione cliente corrente
012900070103     C                   eval      depSped = curSped
013000070213     C*
013100070213     C* Se collo successivo x stessa bolla
013200070213     C                   else
013300070213     C                   exsr      impvab                                       => carico  VAB
013400070213     C                   exsr      exevate                                      => write VAT-E
013500070103     C                   endif
013600000905     C*
013700000905     C                   else
013800000905     C                   eval      vinflg = '1'
013900050628     C                   endif
014000000905     C                   endif
014100000905     C*
014200000905     C  N70              update    tivin000
014300000905     C*
014400991022     C  N70              ENDdo
014500070213     C*
014600070213     C* Scarico testata bolla rimasta in sospesa
014700070213     C                   exsr      wrivab                                       => scarico VAB
014800010202     C*
014900010202     C                   endif
015000990910
015100990910     C* Se non ci sono record con errori ...
015200000710     C                   if        §ctrno = 0
015300990910     C* ... restituisco esito OK.
015400990921     C                   eval      wrkesito = '0'
015500990910     C                   else
015600010201     C                   if        §ctrokvb > 0
015700990921     C                   eval      wrkesito = '1'
015800000710     C                   else
015900000710     C                   eval      wrkesito = '2'
016000990910     C                   endif
016100000710     C                   endif
016200990910     C*
016300990914     C                   if        %open(tivin00r)
016400990908     C                   close     tivin00r
016500990914     C                   endif
016600021113     C                   if        %open(fivabwwr)
016700021113     C                   close     fivabwwr
016800990914     C                   endif
016900021113     C                   if        %open(fivatwwr)
017000021113     C                   close     fivatwwr
017100010201     C                   endif
017200990910     C*
017300010201     C                   if        §ctrokvb > 0
017400000724     C                             and vlrpoi <> *zeros
017500010202     C                   exsr      invio
017600990920     C                   endif
017700990920     C*
017800910830     C                   ENDSR
017900000613     C***
018000010305
018100010305     C*----------------------------------------------------*
018200020305     C*  SCARICAMENTO BUFFER RECORDS VAB
018300010305     C*----------------------------------------------------*
018400020305     C     WRIVAB        BEGSR
018500010305     C*
018600060225     C* Quindi scarico il buffer del file d testata
018700021113     C                   write     fivab000                                     => scarico il VAB
018800010305     C*
018900010305     C                   ENDSR
019000990920
019100000801     C*----------------------------------------------------*
019200000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019300000801     C*----------------------------------------------------*
019400010201     C     INZVAR        BEGSR
019500000801     C*
019600070213     C                   CLEAR                   FIVAB000
019700070213     C                   CLEAR                   FIVAT000
019800070213     C*
019900040802     C                   Z-ADD     *zeros        Num5_0            5 0
020000040802     C                   MOVEL     '0'           FlgCAS            1
020100000801     C*
020200000801     C                   ENDSR
020300000801     C*----------------------------------------------------*
020400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
020500000801     C*----------------------------------------------------*
020600000801     C     DEFCAM        BEGSR
020700000801     C*
020800020619     C* Imposto i valori di default...
020900070213     C                   Z-ADD     0951267       VABCCM
021000070213     C                   Z-ADD     0951267       VATCCM
021100070213     C                   Z-ADD     050           VABLNP
021200070213     C                   Z-ADD     050           VATLNP
021300070213     C                   Z-ADD     101           VABCTR
021400070103     C                   MOVEL     '7Q'          VABCTM
021500040823     C                   MOVEL     '1'           VABCBO
021600070213     C                   MOVEL     'C'           VABTSP
021700020619     C* ... e poi verifico se sono stati passati come parametri
021800020619     C                   IF        vlrppt > *blanks
021900040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
022000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
022100020619     C                   EXSR      CHKNUM
022200020619     C                   IF        PiInt=*on
022300020619     C                   Z-ADD     PiVal         VABCCM
022400020619     C                   Z-ADD     PiVal         VATCCM
022500020619     C                   ENDIF
022600040506     C                   ENDIF
022700040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
022800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022900020619     C                   EXSR      CHKNUM
023000020619     C                   IF        PiInt=*on
023100020619     C                   Z-ADD     PiVal         VABLNP
023200020619     C                   Z-ADD     PiVal         VATLNP
023300040506     C                   ENDIF
023400020619     C                   ENDIF
023500040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
023600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
023700020619     C                   EXSR      CHKNUM
023800020619     C                   IF        PiInt=*on
023900020619     C                   Z-ADD     PiVal         VABCTR
024000040506     C                   ENDIF
024100020619     C                   ENDIF
024200020619     C                   ENDIF
024300000801     C*
024400000801     C                   ENDSR
024500000801     C*----------------------------------------------------*
024600021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
024700000801     C*----------------------------------------------------*
024800040823     C     IMPVAB        BEGSR
024900010305     C*
025000000801     C                   Z-ADD     *zeros        errore            1 0
025100000830     C                   MOVEL     datcor        VABAAS
025200020305     C                   MOVEL     datcor        VATAAS
025300040526     C                   MOVE      datcor        VABMGS
025400040823     C                   MOVE(P)   vlrpoi        VABFGS
025500040823     C                   MOVE(P)   vlrpoi        VATFGS
025600050628     C*
025700091005     C*
025800091005     C                   EVAL      VABRSD=%trim(%subst(vindta:24:35))
025900091005     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026000091005     C     '@':'A'       XLATE     VABRSD        VABRSD
026100091005     C* ==
026200091005     C                   EVAL      VABRD2=%trim(%subst(vindta:24+35:43-35))
026300091005     C                   EVAL      VABIND=%trim(%subst(vindta:67:50))
026400091005     C                   EVAL      VABLOD=%trim(%subst(vindta:117:36))
026500091005     C                   EVAL      VABPRD=%trim(%subst(vindta:158:2))
026600091005     C                   EVAL      VABRMA=%trim(%subst(vindta:163:6))
026700091005     C                   EVAL      VABTSP=%trim(%subst(vindta:200:1))
026800091005     C                   EVAL      VABNOT=%trim(%subst(vindta:203:35))
026900091005     C                   EVAL      VABNT2=%trim(%subst(vindta:203+35:255-35))
027000091005     C*
027100091005     C* Forzature codice cliente e tariffa
027200091005     C                   SELECT
027300091005     C                   WHEN      %subst(vindta:201:2) = 'I '
027400091005     C                   EVAL      VABCCM = 0951267
027500091005     C                   EVAL      VATCCM = 0951267
027600091005     C                   EVAL      VABCTR = 101
027700091005     C                   WHEN      %subst(vindta:201:2) = 'J '
027800091005     C                   EVAL      VABCCM = 0951586
027900091005     C                   EVAL      VATCCM = 0951586
028000091005     C                   EVAL      VABCTR = 101
028100091005     C                   WHEN      %subst(vindta:201:2) = 'JQ'
028200091005     C                   EVAL      VABCCM = 0951597
028300091005     C                   EVAL      VATCCM = 0951597
028400091005     C                   EVAL      VABCTR = 000
028500091005     C                   ENDSL
028600050628     C* CAP
028700070213     C                   EVAL      PiStr=%trim(%subst(vindta:153:5))
028800010201     C                   EXSR      CHKNUM
028900010201     C                   IF        PiInt=*on
029000010201     C                   Z-ADD     PiVal         Num5_0
029100040506     C                   MOVEL(P)  Num5_0        VABCAD
029200010201     C                   ELSE
029300040506     C                   ADD       1             errore
029400010201     C                   EVAL      vinmsg = %trimr(vinmsg)
029500040506     C                             + ' ' + 'VABCAD'
029600010201     C                   ENDIF
029700040506     C* Reperisco la provincia dal CAP e dalla località
029800040526     C                   IF        VABCAD <> *blanks AND
029900040526     C                             VABPRD  = *blanks
030000040506     C                   CLEAR                   TISI95DS
030100040506     C                   EVAL      I95TCN = '3'
030200040506     C                   Z-ADD     datcor        I95DAT
030300040506     C                   EVAL      I95CAP = VABCAD
030400040506     C                   EVAL      I95LOC = VABLOD
030500050627     C                   EVAL      I95NAR = VABNZD
030600040506     C                   CALL      'TISI95R'
030700040506     C                   PARM                    TISI95DS
030800040506     C                   EVAL      VABPRD = O95PRV
030900040506     C                   ENDIF
031000060307     C* NCL
031100070213     C                   EVAL      VABNCL = VABNCL + 1
031200040506     C* PKB
031300070213     C                   EVAL      PiStr=%trim(%subst(vindta:190:5))
031400010201     C                   EXSR      CHKNUM
031500010201     C                   IF        PiNum=*on
031600070213     C                   ADD       PiVal         VABPKB
031700010201     C                   ELSE
031800010201     C                   ADD       1             errore
031900010201     C                   EVAL      vinmsg = %trimr(vinmsg)
032000010201     C                             + ' ' + 'VABPKB'
032100010201     C                   ENDIF
032200070213     C* VLB
032300070213     C                   EVAL      PiStr=%trim(%subst(vindta:195:5))
032400070213     C                   EXSR      CHKNUM
032500070213     C                   IF        PiNum=*on
032600070213     C                   ADD       PiVal         VABVLB
032700070213     C                   ELSE
032800070213     C                   ADD       1             errore
032900070213     C                   EVAL      vinmsg = %trimr(vinmsg)
033000070213     C                             + ' ' + 'VABVLB'
033100070213     C                   ENDIF
033200070103     C* RMN
033300091006     C                   IF        %subst(vindta:163:1) = '\'
033400091006     C                   EVAL      PiStr='0'+%trim(%subst(vindta:163+1:6-1))
033500091006     C                   ELSE
033600091006     C                   EVAL      PiStr=%trim(%subst(vindta:163:6))
033700091006     C                   ENDIF
033800050928     C                   EXSR      CHKNUM
033900050928     C                   IF        PiInt=*on
034000050928     C                   Z-ADD     PiVal         VABRMN
034100050928     C                   ELSE
034200050928     C                   ADD       1             errore
034300050928     C                   EVAL      vinmsg = %trimr(vinmsg)
034400070103     C                             + ' ' + 'VABRMN'
034500050928     C                   ENDIF
034600010205     C*
034700010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
034800040802     C                   IF        FlgCAS <> '0'
034900070102     C                   IF        VABCBO = '1'
035000010205     C                   EVAL      VABCBO = '4'
035100010205     C                   ELSE
035200070102     C                   EVAL      VABCBO = '6'
035300070102     C                   ENDIF
035400010205     C                   ENDIF
035500020305     C*
035600011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
035700011113     C                   EXSR      CHKIMPDIV
035800010202     C*
035900000801     C* Ebbene...
036000000801     C                   ADD       1             §CTRMO
036100010201     C                   IF        errore <> *zeros
036200000801     C                   ADD       1             §CTRNO
036300000801     C                   EVAL      vinflg = '2'
036400000801     C                   ELSE
036500010201     C                   ADD       1             §CTROKVB
036600000801     C                   ENDIF
036700000801     C*
036800000801     C                   ENDSR
036900070213     C*----------------------------------------------------*
037000070213     C*  REPERIMENTO NUMERATORE X TESTATA BOLLE
037100070213     C*----------------------------------------------------*
037200070213     C     REPNUM        BEGSR
037300070213     C*
037400070213     C* NSP => Stacco un numeratore da AZNUM
037500070213     C                   clear                   TRUL33DS
037600070213     C                   eval      I33OPE = *zeros
037700070213     C                   eval      I33CNU = 302
037800070213     C                   eval      I33NUM = 1
037900070213     C                   movel     TRUL33DS      KPJBU
038000070213     C                   call      'TRUL33R'
038100070213     C                   parm                    KPJBA
038200070213     C                   movel     KPJBU         TRUL33DS
038300070213     C                   if        O33ERR = *zeros
038400070213     C                   z-add     O33NRF        VABNSP
038500070213     C                   z-add     O33NRF        VATNSP
038600070213     C                   else
038700070213     C                   Z-ADD     1             errore
038800070213     C                   EVAL      vinmsg = %trimr(vinmsg)
038900070213     C                             + ' ' + 'VABNSP VATNSP'
039000070213     C                   endif
039100070213     C*
039200070213     C                   ENDSR
039300070102     C*----------------------------------------------------*
039400070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
039500070102     C*----------------------------------------------------*
039600070103     C     EXEVATE       BEGSR
039700070102     C*
039800070103     C                   EVAL      VATTRC='E'
039900070213     C                   EVAL      VATNOT = %trim(%subst(vindta:180:10))
040000070102     C                   exsr      wrivat                                       => scarico VAT
040100070102     C*
040200070102     C                   ENDSR
040300010201     C*----------------------------------------------------*
040400040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
040500010201     C*----------------------------------------------------*
040600020305     C     WRIVAT        BEGSR
040700050628     C*
040800060223     C* Scrivo solo se valorizzato qualcosa
040900060223     C                   IF        VATNOT <> *blanks
041000040802     C                   WRITE     FIVAT000
041100060223     C                   ENDIF
041200010201     C*
041300010201     C                   ENDSR
041400010202     C*----------------------------------------------------*
041500021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
041600010202     C*----------------------------------------------------*
041700020305     C     PREVAT        BEGSR
041800010202     C*
041900021113     C* Compongo il nome del membro da dare al FIVATWWR
042000010202     C                   eval      parmbr = vlrhdl
042100010202     C                   movel     'M'           parmbr
042200050627     C                   eval      parccm = %subst(vlrKSC:2:7)
042300010202     C                   eval      paropz = '1'
042400010202     C* Effettuo la chiamata al CLLE preposto
042500040506     C                   call(e)   'TITVVTC'
042600010202     C                   parm                    parccm
042700010202     C                   parm                    parmbr
042800010202     C                   parm                    paropz
042900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
043000010202     C                   if        %error
043100010202     C                   movel     '1'           chkcall
043200010202     C                   else
043300010202     C                   movel     '0'           chkcall
043400010202     C                   endif
043500010202     C*
043600010202     C                   ENDSR
043700000801     C*----------------------------------------------------*
043800000801     C*  CONTROLLO NUMERICITA' CAMPI
043900000801     C*----------------------------------------------------*
044000000801     C     CHKNUM        BEGSR
044100000801     C*
044200000801     C                   call(e)   'ISNUMERIC'
044300000801     C                   PARM                    PiStr            30
044400070213     C                   PARM      ','           PiDecChr          1
044500000801     C                   PARM      *ZEROS        PiVal            30 9
044600000801     C                   PARM      '0'           PiInt             1
044700000801     C                   PARM      '0'           PiNum             1
044800000801     C                   IF        %error
044900000801     C                   EVAL      PiInt=*off
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
050900011113
051000011113
051100000801
051200000801
051300990920      /TITLE Invio dei dati al punto operativo.
051400010202     C     invio         BEGSR
051500990920     C*
051600021113     C* 1° invio FIVAT
051700010201     C                   reset                   dscmz
051800010201     C                   move      vlrpoi        cmzdst
051900021113     C                   eval      cmzfld = 'FIVATWWR'
052000010201     C                   eval      cmzmbd = vlrhdl
052100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
052200021009     C***                if        prmfir = *blanks
052300021113     C                   eval      cmzfla = 'FIVAT00F'
052400021113     C                   eval      cmzmba = 'FIVAT00F'
052500021009     C***                else
052600021009     C***                eval      cmzfla = prmfir
052700021009     C***                eval      cmzmba = prmfir
052800021009     C***                endif
052900010201     C                   eval      cmznrr = *zeros
053000020305     C                   move      §ctrokvt      cmznrr
053100021018     C                   eval      cmzlba = vlrfl1
053200010201     C                   call(e)   'TIS711C'
053300010201     C                   parm                    dscmz
053400010201     C                   parm      *blanks       esito
053500010205     C                   if        %error
053600010205     C                             or cmzerr = '1'
053700010205     C                             or esito  = '1'
053800010205     C                   eval      wrkesito = '3'
053900010205     C                   else
054000010201     C*
054100021113     C* 2° invio FIVAB
054200010201     C                   reset                   dscmz
054300010201     C                   move      vlrpoi        cmzdst
054400010201     C                   eval      cmzfld = vlrfou
054500010201     C                   eval      cmzmbd = vlrhdl
054600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
054700021009     C***                if        prmfir = *blanks
054800021113     C                   eval      cmzfla = 'FIVAB00F'
054900021113     C                   eval      cmzmba = 'FIVAB00F'
055000021009     C***                else
055100021009     C***                eval      cmzfla = prmfir
055200021009     C***                eval      cmzmba = prmfir
055300021009     C***                endif
055400010201     C                   eval      cmznrr = *zeros
055500010201     C                   move      §ctrokvb      cmznrr
055600021018     C                   eval      cmzlba = vlrfl1
055700010201     C                   call(e)   'TIS711C'
055800010201     C                   parm                    dscmz
055900010201     C                   parm      *blanks       esito
056000010201     C                   if        %error
056100010201     C                             or cmzerr = '1'
056200010201     C                             or esito  = '1'
056300010201     C                   eval      wrkesito = '3'
056400010201     C                   endif
056500010205     C                   endif
056600990920     C*
056700000613     C                   ENDSR
056800000613     C***
056900070411
057000070411     C     *pssr         BEGSR
057100070411     C*
057200070411     C                   if        %open(tivin00r)
057300070411     C                   close     tivin00r
057400070411     C                   endif
057500070411     C                   if        %open(fivabwwr)
057600070411     C                   close     fivabwwr
057700070411     C                   endif
057800070411     C                   if        %open(fivatwwr)
057900070411     C                   close     fivatwwr
058000070411     C                   endif
058100070411     C*
058200070411     C* Effettuo la chiamata al CLLE preposto
058300070411     C                   call(e)   'TITVVTC'
058400070411     C                   parm                    parccm
058500070411     C                   parm                    parmbr
058600070411     C                   parm      '2'           paropz
058700070411     C*
058800070411     C                   eval      wrkesito = '2'
058900070411     C*
059000070411     C                   seton                                        LR
059100070411     C*
059200070411     C                   ENDSR     '*CANCL'
059300070411     C***
059400070411
059500990910
059600000613     C     *inzsr        BEGSR
059700990910     C*
059800990910     C     *entry        plist
059900990920     C                   parm                    tivlrds
060000990921     C                   parm      wrkesito      esito
060100000724     C                   parm                    prmlit
060200000710     C                   parm                    prmfir
060300000613     C*
060400000830     C* CALCOLA LA DATA CORRENTE
060500000830     C                   time                    wn14             14 0
060600000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
060700000830     C                   z-add     wn8           g08dat
060800000830     C                   z-add     *zeros        g08inv
060900000830     C                   movel     '0'           g08err
061000000830     C                   call      'XSRDA8'
061100000830     C                   parm                    wlbda8
061200000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
061300000830     C*
061400000613     C                   ENDSR
061500000613     C***
