000100140408      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200140221      *
000300140221      * PARTICOLARITA':
000400140221      *
000500171023      * copiato dal TITZG3R ma con gestione EDIVAB/VAT
000600171023      *
000700140221     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000800990908
000900990910     Ftivin00r  uF   E             DISK    usropn
001000140408     FEDIVABwr  O    E             DISK    usropn
001100140408     FEDIVATwr  O    E             DISK    usropn
001200990908
001300000801     D*----------------------------------------------------
001400000801     D* DICHIARAZIOINE VARIABILI DI WRK
001500000801     D*----------------------------------------------------
001600990920     D dscmz         e ds                  inz
001700990910     D psds           sds
001800990910     D  procname         *PROC
001900990920     D tivlrds       e ds                  extname(tivlr00f)
002000060307     D tisi95ds      e ds
002100990910     D esito           s              1
002200000724     D prmlit          s             10
002300000710     D prmfir          s             10
002400990921     D wrkesito        s                   like(esito)
002500000613     D rrnum           s              6  0 INZ(*zeros)
002600010202     D parccm          s              8    INZ(*blanks)
002700010202     D parmbr          s             10    INZ(*blanks)
002800010202     D paropz          s              1    INZ(*blanks)
002900010202     D chkcall         s              1    INZ(*blanks)
003000171023     D curSped         s             15    INZ(*blanks)
003100171023     D depSped         s             15    INZ(*blanks)
003200140221     D w70             s             70    INZ(*blanks)
003300150212     D wNomeFile       s             30    INZ(*blanks)
003400100118
003500100118     D*------------------
003600100118     D* DS REPERIMENTO NUMERATORE
003700100118     D*------------------
003800100118     D trul33ds      e ds                  inz
003900100118     D*------------------
004000100118     D* DS ARCHITETTURA
004100100118     D*------------------
004200100118     D kpjba         e ds                  inz
004300100118
004400010201
004500010201
004600090127     C*
004700000913     C                   reset                   rrnum
004800990921     C                   reset                   esito
004900990921     C                   reset                   wrkesito
005000000613     C*
005100040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005200000613     C*
005300010202     C* Effettuo la chiamata al CLLE preposto
005400140408     C                   call(e)   'TITVEVTC'
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
007000140408     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
007100910830     C*--------------------------------------------------------
007200040526     C     RWFILE        BEGSR
007300990910     C*
007400990914     C                   if        not %open(tivin00r)
007500990908     C                   open      tivin00r
007600990914     C                   endif
007700140408     C                   if        not %open(EDIVABwr)
007800140408     C                   open      EDIVABwr
007900140408     C                   endif
008000140408     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
008100020305     C                   exsr      prevat
008200010201     C*
008300010202     C                   if        chkcall = '0'
008400010202     C*
008500140408     C                   if        not %open(EDIVATwr)
008600140408     C                   open      EDIVATwr
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
010800070402     C*
010900070402     C* Verifico la rottura d codice x raggruppamento spedizione cliente
011000171023     C                   eval      curSped = %subst(vindta:80:15)
011100070402     C                   if        curSped <> depSped
011200140408     C                   clear                   edivab00
011300140408     C                   clear                   edivat00
011400050628     C*
011500170720     C                   setoff                                       55
011600060315     C                   exsr      impvab                                       => carico  VAB
011700090127     C*
011800170720     C  N55              exsr      wrivab                                       => scarico VAB
011900070402     C*
012000070402     C* Scrivo anche estensione dettaglio bolle - tipo record "B" => TELEFONO DESTINATARIO
012100171024     C**N55              exsr      exevatb
012200070402     C*
012300070402     C* Salvo il raggruppamento spedizione cliente corrente
012400070402     C                   eval      depSped = curSped
012500070402     C*
012600070402     C                   endif
012700000905     C*
012800000905     C                   else
012900000905     C                   eval      vinflg = '1'
013000050628     C                   endif
013100000905     C                   endif
013200000905     C*
013300000905     C  N70              update    tivin000
013400000905     C*
013500991022     C  N70              ENDdo
013600140408     C*
013700150212     C* Siccome il file in questione non ha più righe per spedizione essendo come un disk A
013800150212     C* questa ulteriore WRITE non serve, in quanto utile se avevo i dati di VAB in memoria
013900150212     C* in attesa di aver scritto tutti i VAT_E
014000150212     C***
014100140408     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
014200150212     C***                MOVE(P)   vlrMSG        wNomeFile
014300170822     C***                EVAL      VABCMR=%char(datcor)+'-'+%char(oracor)
014400150212     C***                EVAL      VABDCM = datcor
014500150212     C***                EVAL      VABDTS = datcor
014600150212     C***                EVAL      VABHMS = oracor
014700150212     C***                EVAL      VABCNT = 1
014800150212     C***
014900150212     C***                WRITE     EDIVAB00
015000010202     C*
015100010202     C                   endif
015200990910
015300990910     C* Se non ci sono record con errori ...
015400000710     C                   if        §ctrno = 0
015500990910     C* ... restituisco esito OK.
015600990921     C                   eval      wrkesito = '0'
015700990910     C                   else
015800010201     C                   if        §ctrokvb > 0
015900990921     C                   eval      wrkesito = '1'
016000000710     C                   else
016100000710     C                   eval      wrkesito = '2'
016200990910     C                   endif
016300000710     C                   endif
016400990910     C*
016500990914     C                   if        %open(tivin00r)
016600990908     C                   close     tivin00r
016700990914     C                   endif
016800140408     C                   if        %open(EDIVABwr)
016900140408     C                   close     EDIVABwr
017000140408     C                   endif
017100140408     C                   if        %open(EDIVATwr)
017200140408     C                   close     EDIVATwr
017300140408     C                   endif
017400990910     C*
017500010201     C                   if        §ctrokvb > 0
017600000724     C                             and vlrpoi <> *zeros
017700010202     C                   exsr      invio
017800990920     C                   endif
017900990920     C*
018000910830     C                   ENDSR
018100000613     C***
018200010305
018300010305     C*----------------------------------------------------*
018400020305     C*  SCARICAMENTO BUFFER RECORDS VAB
018500010305     C*----------------------------------------------------*
018600090127     C     WRIVAB        BEGSR
018700140408     C*
018800140408     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
018900170907     C***                EVAL      VABCMR =%char(datcor)+'-'+%char(oracor)
019000171023     C                   SELECT
019100171023     C                   WHEN      VABCCM = 0015517
019200171023     C                   EVAL      VABCMR ='LIMONI'
019300171023     C                   WHEN      VABCCM = 0018402
019400171023     C                   EVAL      VABCMR ='GARDENIA'
019500171023     C                   OTHER
019600171023     C                   EVAL      VABCMR ='???'
019700171023     C                   ENDSL
019800140408     C                   EVAL      VABDCM = datcor
019900140408     C                   EVAL      VABDTS = datcor
020000140408     C                   EVAL      VABHMS = oracor
020100140408     C                   EVAL      VABCNT = 1
020200090127     C*
020300090127     C* Quindi scarico il buffer del file d testata
020400140408     C                   WRITE     EDIVAB00
020500010305     C*
020600010305     C                   ENDSR
020700990920
020800000801     C*----------------------------------------------------*
020900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021000000801     C*----------------------------------------------------*
021100010201     C     INZVAR        BEGSR
021200000801     C*
021300040802     C                   Z-ADD     *zeros        Num5_0            5 0
021400040802     C                   MOVEL     '0'           FlgCAS            1
021500000801     C*
021600000801     C                   ENDSR
021700000801     C*----------------------------------------------------*
021800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
021900000801     C*----------------------------------------------------*
022000000801     C     DEFCAM        BEGSR
022100070718     C*
022200070718     C* Imposto i valori di default...
022300171023     C                   Z-ADD     0015517       VABCCM
022400171023     C                   Z-ADD     0015517       VATCCM
022500171023     C                   Z-ADD     275           VABLNP
022600171023     C                   Z-ADD     275           VATLNP
022700171023     C                   Z-ADD     000           VABCTR
022800140224     C                   MOVEL     '1'           VABCBO
022900140224     C                   MOVEL     '7Q'          VABCTM
023000020619     C* ... e poi verifico se sono stati passati come parametri
023100020619     C                   IF        vlrppt > *blanks
023200040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
023300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023400020619     C                   EXSR      CHKNUM
023500020619     C                   IF        PiInt=*on
023600020619     C                   Z-ADD     PiVal         VABCCM
023700020619     C                   Z-ADD     PiVal         VATCCM
023800020619     C                   ENDIF
023900040506     C                   ENDIF
024000040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
024100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024200020619     C                   EXSR      CHKNUM
024300020619     C                   IF        PiInt=*on
024400020619     C                   Z-ADD     PiVal         VABLNP
024500020619     C                   Z-ADD     PiVal         VATLNP
024600040506     C                   ENDIF
024700020619     C                   ENDIF
024800040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
024900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
025000020619     C                   EXSR      CHKNUM
025100020619     C                   IF        PiInt=*on
025200020619     C                   Z-ADD     PiVal         VABCTR
025300040506     C                   ENDIF
025400020619     C                   ENDIF
025500140224     C                   IF        %subst(vlrppt:16:2) <> *blanks
025600140224     C                   EVAL      VABCTM = %subst(vlrppt:16:2)
025700140224     C                   ENDIF
025800020619     C                   ENDIF
025900000801     C*
026000000801     C                   ENDSR
026100000801     C*----------------------------------------------------*
026200140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
026300000801     C*----------------------------------------------------*
026400040823     C     IMPVAB        BEGSR
026500040823     C*
026600020305     C                   EXSR      INZVAR
026700020305     C                   EXSR      DEFCAM
026800010305     C*
026900000801     C                   Z-ADD     *zeros        errore            1 0
027000000830     C                   MOVEL     datcor        VABAAS
027100020305     C                   MOVEL     datcor        VATAAS
027200040526     C                   MOVE      datcor        VABMGS
027300040823     C                   MOVE(P)   vlrpoi        VABFGS
027400040823     C                   MOVE(P)   vlrpoi        VATFGS
027500050628     C*
027600171023     C                   EVAL      VABRSD=%trim(%subst(vindta:156:35))
027700171023     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
027800171023     C     '@':'A'       XLATE     VABRSD        VABRSD
027900171023     C* ==
028000171023     C                   EVAL      VABRD2=%trim(%subst(vindta:191:35))
028100171023     C                   EVAL      VABIND=%trim(%subst(vindta:226:35))
028200171023     C                   EVAL      VABLOD=%trim(%subst(vindta:261:35))
028300171023     C                   EVAL      VABCAD=%trim(%subst(vindta:316:7))
028400171023     C                   EVAL      VABPRD=%trim(%subst(vindta:323:2))
028500171023     C                   EVAL      VABNZD=%trim(%subst(vindta:325:4))
028600171023     C                   IF        %trim(VABNZD) = 'I' or
028700171023     C                             %trim(VABNZD) = 'IT' or
028800171023     C                             %trim(VABNZD) = 'ITA'
028900171023     C                   EVAL      VABNZD = *blanks
029000171023     C                   ENDIF
029100171023     C                   EVAL      VABRMA=%trim(%subst(vindta:80:15))
029200171023     C                   EVAL      w70=%trim(%subst(vindta:464:70))
029300171023     C                   EVAL      VABNOT=%subst(w70:1:35)
029400171023     C                   EVAL      VABNT2=%subst(w70:36:35)
029500171023     C*
029600171023     C* CCM
029700171023     C                   EVAL      PiStr=%trim(%subst(vindta:306:7))
029800171023     C                   EXSR      CHKNUM
029900171023     C                   IF        PiInt=*on
030000171023     C                   Z-ADD     PiVal         VABCCM
030100171023     C                   Z-ADD     PiVal         VATCCM
030200171023     C                   ELSE
030300171023     C                   ADD       1             errore
030400171023     C                   EVAL      vinmsg = %trimr(vinmsg)
030500171023     C                             + ' ' + 'VABCCM VATCCM'
030600171023     C                   ENDIF
030700171023     C*
030800171023     C* DCR
030900171023     C                   EVAL      PiStr=%trim(%subst(vindta:46:8))
031000171023     C                   EXSR      CHKNUM
031100171023     C                   IF        PiInt=*on
031200171023     C                   Z-ADD     PiVal         VABDCR
031300171023     C                   ELSE
031400171023     C                   ADD       1             errore
031500171023     C                   EVAL      vinmsg = %trimr(vinmsg)
031600171023     C                             + ' ' + 'VABDCR'
031700171023     C                   ENDIF
031800171023     C* RMN
031900171023     C                   EVAL      PiStr=%trim(%subst(vindta:80:15))
032000171023     C                   EXSR      CHKNUM
032100171023     C                   IF        PiInt=*on
032200171023     C                   Z-ADD     PiVal         VABRMN
032300171023     C                   ELSE
032400171023     C                   ADD       1             errore
032500171023     C                   EVAL      vinmsg = %trimr(vinmsg)
032600171023     C                             + ' ' + 'VABRMN'
032700171023     C                   ENDIF
032800171023     C* NSP
032900171023     C                   EVAL      PiStr=%trim(%subst(vindta:85:7))
033000171023     C                   EXSR      CHKNUM
033100171023     C                   IF        PiInt=*on
033200171023     C                   Z-ADD     PiVal         VABNSP
033300171023     C                   ELSE
033400171023     C                   ADD       1             errore
033500171023     C                   EVAL      vinmsg = %trimr(vinmsg)
033600171023     C                             + ' ' + 'VABNSP'
033700171023     C                   ENDIF
033800171023     C* NCL
033900171023     C                   EVAL      PiStr=%trim(%subst(vindta:355:5))
034000171023     C                   EXSR      CHKNUM
034100171023     C                   IF        PiInt=*on
034200171023     C                   Z-ADD     PiVal         VABNCL
034300171023     C                   ELSE
034400171023     C                   ADD       1             errore
034500171023     C                   EVAL      vinmsg = %trimr(vinmsg)
034600171023     C                             + ' ' + 'VABNCL'
034700171023     C                   ENDIF
034800171023     C* PKB
034900171023     C                   EVAL      PiStr=%trim(%subst(vindta:360:7))
035000171023     C                   EXSR      CHKNUM
035100171023     C                   IF        PiNum=*on
035200171023     C* il dato ha 2 decimali senza separatore
035300171023     C                   EVAL(H)   VABPKB = PiVal/100
035400171023     C                   ELSE
035500171023     C                   ADD       1             errore
035600171023     C                   EVAL      vinmsg = %trimr(vinmsg)
035700171023     C                             + ' ' + 'VABPKB'
035800171023     C                   ENDIF
035900171023     C* CAS
036000171023     C                   IF        %trim(%subst(vindta:396:15)) <> *blanks AND
036100171023     C                             %trim(%subst(vindta:396:15)) <> *zeros
036200171023     C                   MOVEL     '1'           FlgCAS
036300171023     C                   EVAL      VABVCA = 'EUR'
036400171023     C                   EVAL      PiStr=%trim(%subst(vindta:396:15))
036500171023     C                   EXSR      CHKNUM
036600171023     C                   IF        PiNum=*on
036700171023     C                   EVAL      PiVal = PiVal / 1000                         * gest. 3 decimali
036800171023     C                   Z-ADD     PiVal         VABCAS
036900171023     C                   ELSE
037000171023     C                   ADD       1             errore
037100171023     C                   EVAL      vinmsg = %trimr(vinmsg)
037200171023     C                             + ' ' + 'VABCAS'
037300171023     C                   ENDIF
037400171023     C                   ENDIF
037500010205     C*
037600010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
037700040802     C                   IF        FlgCAS <> '0'
037800070102     C                   IF        VABCBO = '1'
037900010205     C                   EVAL      VABCBO = '4'
038000010205     C                   ELSE
038100070102     C                   EVAL      VABCBO = '6'
038200070102     C                   ENDIF
038300010205     C                   ENDIF
038400171023     C*
038500171023     C* Se provincia nn valorizzata la reperisco
038600171023     C* tramite TISI95R a seconda dei dati d instradamento presenti
038700171023     C                   IF        VABPRD =  *blanks AND
038800171023     C                             VABNZD =  *blanks AND
038900171023     C                             VABCAD <> *blanks AND
039000171023     C                             VABLOD <> *blanks
039100171023     C                   CLEAR                   TISI95DS
039200171023     C                   EVAL      I95TCN = '3'
039300171023     C                   Z-ADD     datcor        I95DAT
039400171023     C                   EVAL      I95NAR = VABNZD
039500171023     C                   EVAL      I95CAP = VABCAD
039600171023     C                   EVAL      I95LOC = VABLOD
039700171023     C                   CALL      'TISI95R'
039800171023     C                   PARM                    TISI95DS
039900171023     C                   EVAL      VABPRD = O95PRV
040000171023     C                   ENDIF
040100020305     C*
040200011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
040300011113     C                   EXSR      CHKIMPDIV
040400010202     C*
040500000801     C* Ebbene...
040600000801     C                   ADD       1             §CTRMO
040700010201     C                   IF        errore <> *zeros
040800000801     C                   ADD       1             §CTRNO
040900000801     C                   EVAL      vinflg = '2'
041000000801     C                   ELSE
041100010201     C                   ADD       1             §CTROKVB
041200000801     C                   ENDIF
041300000801     C*
041400000801     C                   ENDSR
041500070102     C*----------------------------------------------------*
041600140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "A"
041700070102     C*----------------------------------------------------*
041800171023     C     EXEVATB       BEGSR
041900070102     C*
042000171023     C                   EVAL      VATTRC='B'
042100171024     C***                EVAL      VATNOT = %trim(%subst(vindta:260:13))
042200070102     C                   exsr      wrivat                                       => scarico VAT
042300070102     C*
042400070102     C                   ENDSR
042500010201     C*----------------------------------------------------*
042600140408     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
042700010201     C*----------------------------------------------------*
042800020305     C     WRIVAT        BEGSR
042900140408     C*
043000140408     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
043100170907     C***                EVAL      VATCMR =%char(datcor)+'-'+%char(oracor)
043200171023     C                   SELECT
043300171023     C                   WHEN      VATCCM = 0015517
043400171023     C                   EVAL      VATCMR ='LIMONI'
043500171023     C                   WHEN      VATCCM = 0018402
043600171023     C                   EVAL      VATCMR ='GARDENIA'
043700171023     C                   OTHER
043800171023     C                   EVAL      VATCMR ='???'
043900171023     C                   ENDSL
044000140408     C                   EVAL      VATCNT = 1
044100050628     C*
044200060223     C* Scrivo solo se valorizzato qualcosa
044300060223     C                   IF        VATNOT <> *blanks
044400140408     C                   WRITE     EDIVAT00
044500060223     C                   ENDIF
044600010201     C*
044700010201     C                   ENDSR
044800010202     C*----------------------------------------------------*
044900140408     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
045000010202     C*----------------------------------------------------*
045100020305     C     PREVAT        BEGSR
045200010202     C*
045300140408     C* Compongo il nome del membro da dare al EDIVATWR
045400010202     C                   eval      parmbr = vlrhdl
045500010202     C                   movel     'M'           parmbr
045600050627     C                   eval      parccm = %subst(vlrKSC:2:7)
045700010202     C                   eval      paropz = '1'
045800010202     C* Effettuo la chiamata al CLLE preposto
045900140408     C                   call(e)   'TITVEVTC'
046000010202     C                   parm                    parccm
046100010202     C                   parm                    parmbr
046200010202     C                   parm                    paropz
046300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046400010202     C                   if        %error
046500010202     C                   movel     '1'           chkcall
046600010202     C                   else
046700010202     C                   movel     '0'           chkcall
046800010202     C                   endif
046900010202     C*
047000010202     C                   ENDSR
047100000801     C*----------------------------------------------------*
047200000801     C*  CONTROLLO NUMERICITA' CAMPI
047300000801     C*----------------------------------------------------*
047400000801     C     CHKNUM        BEGSR
047500000801     C*
047600000801     C                   call(e)   'ISNUMERIC'
047700000801     C                   PARM                    PiStr            30
047800170711     C                   PARM      ','           PiDecChr          1
047900000801     C                   PARM      *ZEROS        PiVal            30 9
048000000801     C                   PARM      '0'           PiInt             1
048100000801     C                   PARM      '0'           PiNum             1
048200000801     C                   IF        %error
048300000801     C                   EVAL      PiInt=*off
048400000801     C                   ENDIF
048500000801     C*
048600000801     C                   ENDSR
048700000801     C***
048800000801
048900011113
049000011113     C*----------------------------------------------------*
049100011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
049200011113     C*----------------------------------------------------*
049300011113     C     CHKIMPDIV     BEGSR
049400011113     C*
049500011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
049600011113     C                   Z-ADD     *zeros        wrkDec            9 9
049700011113     C*
049800011113     C* Come prima cosa effettuo considerazioni sulla divisa
049900011113     C                   IF        vabIAS > *zeros
050000011113     C                   IF        vabVAS <> 'EUR'
050100011113     C                   EVAL      vabVAS =  'ITL'
050200011113     C                   ENDIF
050300011113     C                   ENDIF
050400011113     C*
050500011113     C                   IF        vabCAS > *zeros
050600011113     C                   IF        vabVCA <> 'EUR'
050700011113     C                   EVAL      vabVCA =  'ITL'
050800011113     C                   ENDIF
050900011113     C                   ENDIF
051000011113     C*
051100011113     C                   IF        vabVMD > *zeros
051200020305     C                   IF        vabVAD <> 'EUR'
051300011113     C                   EVAL      vabVAD =  'ITL'
051400011113     C                   ENDIF
051500011113     C                   ENDIF
051600011113     C*
051700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
051800011113     C                   Z-ADD     vabIAS        wrkDec
051900011113     C                   IF        wrkDec > *zeros
052000011113     C                   IF        vabVAS = 'ITL'
052100011113     C                   EVAL      vabIAS = *zeros
052200011113     C                   ENDIF
052300011113     C                   ENDIF
052400011113     C*
052500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
052600011113     C                   Z-ADD     vabCAS        wrkDec
052700011113     C                   IF        wrkDec > *zeros
052800011113     C                   IF        vabVCA = 'ITL'
052900011113     C                   EVAL      vabCAS = *zeros
053000011113     C                   ENDIF
053100011113     C                   ENDIF
053200011113     C*
053300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
053400011113     C                   Z-ADD     vabVMD        wrkDec
053500011113     C                   IF        wrkDec > *zeros
053600011113     C                   IF        vabVAD = 'ITL'
053700011113     C                   EVAL      vabVMD = *zeros
053800011113     C                   ENDIF
053900011113     C                   ENDIF
054000011113     C*
054100011113     C                   ENDSR
054200011113     C***
054300011113
054400011113
054500000801
054600000801
054700990920      /TITLE Invio dei dati al punto operativo.
054800010202     C     invio         BEGSR
054900990920     C*
055000140408     C* 1° invio EDIVAT
055100010201     C                   reset                   dscmz
055200010201     C                   move      vlrpoi        cmzdst
055300140408     C                   eval      cmzfld = 'EDIVATWR'
055400010201     C                   eval      cmzmbd = vlrhdl
055500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055600021009     C***                if        prmfir = *blanks
055700140408     C                   eval      cmzfla = 'EDIVAT0F'
055800140408     C                   eval      cmzmba = 'EDIVAT0F'
055900021009     C***                else
056000021009     C***                eval      cmzfla = prmfir
056100021009     C***                eval      cmzmba = prmfir
056200021009     C***                endif
056300010201     C                   eval      cmznrr = *zeros
056400020305     C                   move      §ctrokvt      cmznrr
056500021018     C                   eval      cmzlba = vlrfl1
056600010201     C                   call(e)   'TIS711C'
056700010201     C                   parm                    dscmz
056800010201     C                   parm      *blanks       esito
056900010205     C                   if        %error
057000010205     C                             or cmzerr = '1'
057100010205     C                             or esito  = '1'
057200010205     C                   eval      wrkesito = '3'
057300010205     C                   else
057400010201     C*
057500140408     C* 2° invio EùDIVAB
057600010201     C                   reset                   dscmz
057700010201     C                   move      vlrpoi        cmzdst
057800010201     C                   eval      cmzfld = vlrfou
057900010201     C                   eval      cmzmbd = vlrhdl
058000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058100021009     C***                if        prmfir = *blanks
058200140408     C                   eval      cmzfla = 'EDIVAB0F'
058300140408     C                   eval      cmzmba = 'EDIVAB0F'
058400021009     C***                else
058500021009     C***                eval      cmzfla = prmfir
058600021009     C***                eval      cmzmba = prmfir
058700021009     C***                endif
058800010201     C                   eval      cmznrr = *zeros
058900010201     C                   move      §ctrokvb      cmznrr
059000021018     C                   eval      cmzlba = vlrfl1
059100010201     C                   call(e)   'TIS711C'
059200010201     C                   parm                    dscmz
059300010201     C                   parm      *blanks       esito
059400010201     C                   if        %error
059500010201     C                             or cmzerr = '1'
059600010201     C                             or esito  = '1'
059700010201     C                   eval      wrkesito = '3'
059800010201     C                   endif
059900010205     C                   endif
060000990920     C*
060100000613     C                   ENDSR
060200000613     C***
060300990910
060400000613     C     *inzsr        BEGSR
060500990910     C*
060600990910     C     *entry        plist
060700990920     C                   parm                    tivlrds
060800990921     C                   parm      wrkesito      esito
060900000724     C                   parm                    prmlit
061000000710     C                   parm                    prmfir
061100000613     C*
061200000830     C* CALCOLA LA DATA CORRENTE
061300140408     C                   time                    wn14             14 0
061400140408     C                   movel     wn14          oracor            6 0          *ORA
061500100118     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
061600100118     C                   eval      datcor = %dec(%date() : *ISO)
061700000830     C*
061800000613     C                   ENDSR
061900000613     C***
