000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200140221      *
000300140221      * PARTICOLARITA':
000400140221      *
000500140221     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000600990908
000700161020     Ftabel00f  if   e           k disk
000800990910     Ftivin00r  uF   E             DISK    usropn
000900021113     FFIVABwwr  O    E             DISK    usropn
001000021113     FFIVATwwr  O    E             DISK    usropn
001100990908
001200000801     D*----------------------------------------------------
001300000801     D* DICHIARAZIOINE VARIABILI DI WRK
001400000801     D*----------------------------------------------------
001500990920     D dscmz         e ds                  inz
001600990910     D psds           sds
001700990910     D  procname         *PROC
001800990920     D tivlrds       e ds                  extname(tivlr00f)
001900060307     D tisi95ds      e ds
002000161020     D ds15          e ds
002100990910     D esito           s              1
002200000724     D prmlit          s             10
002300000710     D prmfir          s             10
002400990921     D wrkesito        s                   like(esito)
002500000613     D rrnum           s              6  0 INZ(*zeros)
002600010202     D parccm          s              8    INZ(*blanks)
002700010202     D parmbr          s             10    INZ(*blanks)
002800010202     D paropz          s              1    INZ(*blanks)
002900010202     D chkcall         s              1    INZ(*blanks)
003000151110     D curSped         s             10    INZ(*blanks)
003100151110     D depSped         s             10    INZ(*blanks)
003200140221     D w70             s             70    INZ(*blanks)
003300100118
003400100118     D*------------------
003500100118     D* DS REPERIMENTO NUMERATORE
003600100118     D*------------------
003700100118     D trul33ds      e ds                  inz
003800100118     D*------------------
003900100118     D* DS ARCHITETTURA
004000100118     D*------------------
004100100118     D kpjba         e ds                  inz
004200100118
004300161020     D*------------
004400161020     D jNAZ            s              5  0 INZ(*zeros)
004500161020     D skNAZISO        S              3    DIM(1000)
004600161020     D skNAZBAR        S              3    DIM(1000)
004700010201
004800010201
004900090127     C*
005000000913     C                   reset                   rrnum
005100990921     C                   reset                   esito
005200990921     C                   reset                   wrkesito
005300000613     C*
005400161020     C                   EXSR      CARTAB                                       CARICA TABELLE
005500040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005600000613     C*
005700010202     C* Effettuo la chiamata al CLLE preposto
005800040506     C                   call(e)   'TITVVTC'
005900010202     C                   parm                    parccm
006000010202     C                   parm                    parmbr
006100010202     C                   parm      '2'           paropz
006200050201     C*
006300050201     C* Effettuo lancio TISI95 solo x chiusura
006400050201     C                   CLEAR                   TISI95DS
006500050201     C                   EVAL      I95TLA = 'C'
006600050201     C                   CALL      'TISI95R'
006700050201     C                   PARM                    TISI95DS
006800000616     C*
006900000801     C
007000010201     C                   seton                                        LR
007100990908
007200000801
007300910830     C*--------------------------------------------------------
007400040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
007500910830     C*--------------------------------------------------------
007600040526     C     RWFILE        BEGSR
007700990910     C*
007800990914     C                   if        not %open(tivin00r)
007900990908     C                   open      tivin00r
008000990914     C                   endif
008100021113     C                   if        not %open(fivabwwr)
008200021113     C                   open      fivabwwr
008300990914     C                   endif
008400021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
008500020305     C                   exsr      prevat
008600010201     C*
008700010202     C                   if        chkcall = '0'
008800010202     C*
008900021113     C                   if        not %open(fivatwwr)
009000021113     C                   open      fivatwwr
009100010201     C                   endif
009200990910     C*
009300010201     C                   clear                   §CTROKVB          5 0
009400020305     C                   clear                   §CTROKVT          5 0
009500000801     C                   clear                   §CTRMO            5 0
009600000801     C                   clear                   §CTRNO            5 0
009700990910     C*
009800921023     C                   DO        *HIVAL
009900990913     C*
010000990915     C                   READ      tivin00r                               70
010100050627     C                   if        vindta > *blanks
010200000613     C                   add       1             rrnum
010300000801     C*
010400000801     C                   if        *in70 = *off
010500000801     C                             and
010600000801     C                             (vinflg = *blanks
010700000801     C                              or vinflg = '0'
010800000801     C                              or vinflg = '2')
010900000801     C*
011000000801     C                   clear                   vinmsg
011100000801     C                   eval      vinflg = '1'
011200070402     C*
011300070402     C* Verifico la rottura d codice x raggruppamento spedizione cliente
011400151110     C                   eval      curSped = %subst(vindta:30:10)
011500070402     C                   if        curSped <> depSped
011600070402     C                   clear                   fivab000
011700070402     C                   clear                   fivat000
011800050628     C*
011900060315     C                   exsr      impvab                                       => carico  VAB
012000090127     C*
012100060315     C                   exsr      wrivab                                       => scarico VAB
012200070402     C*
012300070402     C* Scrivo anche estensione dettaglio bolle - tipo record "A" => REFERENTE CONSEGNA
012400151110     C***                exsr      exevata
012500070402     C*
012600070402     C* Scrivo anche estensione dettaglio bolle - tipo record "B" => TELEFONO DESTINATARIO
012700151110     C***                exsr      exevatb
012800140226     C*
012900140226     C* Indirizzo e-mail
013000140226     C                   exsr      exevati
013100140226     C*
013200140226     C* Cellulare per SMS
013300140226     C                   exsr      exevats
013400070402     C*
013500070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
013600151110     C***                exsr      exevate
013700070402     C*
013800070402     C* Salvo il raggruppamento spedizione cliente corrente
013900070402     C                   eval      depSped = curSped
014000070402     C*
014100070402     C                   else
014200070402     C*
014300070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
014400151110     C***                exsr      exevate
014500070402     C                   endif
014600000905     C*
014700000905     C                   else
014800000905     C                   eval      vinflg = '1'
014900050628     C                   endif
015000000905     C                   endif
015100000905     C*
015200000905     C  N70              update    tivin000
015300000905     C*
015400991022     C  N70              ENDdo
015500010202     C*
015600010202     C                   endif
015700990910
015800990910     C* Se non ci sono record con errori ...
015900000710     C                   if        §ctrno = 0
016000990910     C* ... restituisco esito OK.
016100990921     C                   eval      wrkesito = '0'
016200990910     C                   else
016300010201     C                   if        §ctrokvb > 0
016400990921     C                   eval      wrkesito = '1'
016500000710     C                   else
016600000710     C                   eval      wrkesito = '2'
016700990910     C                   endif
016800000710     C                   endif
016900990910     C*
017000990914     C                   if        %open(tivin00r)
017100990908     C                   close     tivin00r
017200990914     C                   endif
017300021113     C                   if        %open(fivabwwr)
017400021113     C                   close     fivabwwr
017500990914     C                   endif
017600021113     C                   if        %open(fivatwwr)
017700021113     C                   close     fivatwwr
017800010201     C                   endif
017900990910     C*
018000010201     C                   if        §ctrokvb > 0
018100000724     C                             and vlrpoi <> *zeros
018200010202     C                   exsr      invio
018300990920     C                   endif
018400990920     C*
018500910830     C                   ENDSR
018600000613     C***
018700010305
018800010305     C*----------------------------------------------------*
018900020305     C*  SCARICAMENTO BUFFER RECORDS VAB
019000010305     C*----------------------------------------------------*
019100090127     C     WRIVAB        BEGSR
019200090127     C*
019300090127     C* Quindi scarico il buffer del file d testata
019400090127     C                   write     fivab000                                     => scarico il VAB
019500010305     C*
019600010305     C                   ENDSR
019700990920
019800000801     C*----------------------------------------------------*
019900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020000000801     C*----------------------------------------------------*
020100010201     C     INZVAR        BEGSR
020200000801     C*
020300040802     C                   Z-ADD     *zeros        Num5_0            5 0
020400040802     C                   MOVEL     '0'           FlgCAS            1
020500000801     C*
020600000801     C                   ENDSR
020700000801     C*----------------------------------------------------*
020800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
020900000801     C*----------------------------------------------------*
021000000801     C     DEFCAM        BEGSR
021100070718     C*
021200070718     C* Imposto i valori di default...
021300151110     C                   Z-ADD     2220282       VABCCM
021400151110     C                   Z-ADD     2220282       VATCCM
021500151110     C                   Z-ADD     222           VABLNP
021600151110     C                   Z-ADD     222           VATLNP
021700151110     C                   Z-ADD     000           VABCTR
021800140224     C                   Z-ADD     001           VABNCL
021900140224     C                   MOVEL     '1'           VABCBO
022000151110     C                   MOVEL     '7A'          VABCTM
022100020619     C* ... e poi verifico se sono stati passati come parametri
022200020619     C                   IF        vlrppt > *blanks
022300040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
022400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
022500020619     C                   EXSR      CHKNUM
022600020619     C                   IF        PiInt=*on
022700020619     C                   Z-ADD     PiVal         VABCCM
022800020619     C                   Z-ADD     PiVal         VATCCM
022900020619     C                   ENDIF
023000040506     C                   ENDIF
023100040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
023200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023300020619     C                   EXSR      CHKNUM
023400020619     C                   IF        PiInt=*on
023500020619     C                   Z-ADD     PiVal         VABLNP
023600020619     C                   Z-ADD     PiVal         VATLNP
023700040506     C                   ENDIF
023800020619     C                   ENDIF
023900040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
024000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
024100020619     C                   EXSR      CHKNUM
024200020619     C                   IF        PiInt=*on
024300020619     C                   Z-ADD     PiVal         VABCTR
024400040506     C                   ENDIF
024500020619     C                   ENDIF
024600140224     C                   IF        %subst(vlrppt:16:2) <> *blanks
024700140224     C                   EVAL      VABCTM = %subst(vlrppt:16:2)
024800140224     C                   ENDIF
024900020619     C                   ENDIF
025000000801     C*
025100000801     C                   ENDSR
025200000801     C*----------------------------------------------------*
025300021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
025400000801     C*----------------------------------------------------*
025500040823     C     IMPVAB        BEGSR
025600040823     C*
025700020305     C                   EXSR      INZVAR
025800020305     C                   EXSR      DEFCAM
025900010305     C*
026000000801     C                   Z-ADD     *zeros        errore            1 0
026100151110     C                   MOVEL     datcor        VABAAS
026200151110     C                   MOVEL     datcor        VATAAS
026300151110     C                   MOVE      datcor        VABMGS
026400151110     C                   MOVE(P)   vlrpoi        VABFGS
026500151110     C                   MOVE(P)   vlrpoi        VATFGS
026600151110     C*
026700151110     C* Reperimento campi ALFA
026800151110     C*
026900151110     C* Considerazioni sulla ragione sociale del destinatario da indicare
027000151110     C                   EVAL      VABRSD=%trim(%subst(vindta:51:30))
027100151110     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
027200151110     C     '@':'A'       XLATE     VABRSD        VABRSD
027300151110     C* ==
027400151110     C                   EVAL      VABIND=%trim(%subst(vindta:81:30))
027500151110     C                   EVAL      VABLOD=%trim(%subst(vindta:111:30))
027600151110     C                   EVAL      VABPRD=%trim(%subst(vindta:141:2))
027700161020     C                   EVAL      VABNZD=%trim(%subst(vindta:541:3))
027800161020     C* NZD
027900161020     C                   IF        VABNZD = 'I'   OR
028000161020     C                             VABNZD = 'IT'  OR
028100161020     C                             VABNZD = 'ITA'
028200161020     C                   EVAL      VABNZD = *blanks
028300161020     C                   ELSE
028400161020     C                   Z-ADD     1             jNAZ
028500161020     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         13
028600161020     C                   IF        %found
028700161020     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
028800161020     C                   ENDIF
028900161020     C                   ENDIF
029000161020     C*
029100151110     C                   EVAL      VABRMA=%trim(%subst(vindta:30:10))
029200151110     C                   EVAL      w70=%trim(%subst(vindta:230:70))
029300151110     C                   EVAL      VABNOT=%subst(w70:1:35)
029400151110     C                   EVAL      VABNT2=%subst(w70:36:35)
029500160119     C***                IF        %trim(%subst(vindta:18:1)) = '1'
029600160119     C***                EVAL      VABCBO='1'
029700160119     C***                ENDIF
029800160119     C***                IF        %trim(%subst(vindta:18:1)) = '6'
029900160119     C***                EVAL      VABCBO='2'
030000160119     C***                ENDIF
030100160119     C                   EVAL      VABCBO=%trim(%subst(vindta:18:1))
030200160119     C                   EVAL      VABTIC=%trim(%subst(vindta:13:5))
030300151110     C*
030400151110     C* Reperimento campi NUMERICI
030500151110     C*
030600151110     C* CTR
030700151110     C* se diverso da blank testo il dato, altrimenti va bene il default
030800151110     C                   IF        %subst(vindta:1:3) <> *blank
030900151110     C                   EVAL      PiStr=%trim(%subst(vindta:1:3))
031000151110     C                   EXSR      CHKNUM
031100151110     C                   IF        PiInt=*on
031200151110     C                   Z-ADD     PiVal         VABCTR
031300151110     C                   ELSE
031400151110     C                   ADD       1             errore
031500151110     C                   EVAL      vinmsg = %trimr(vinmsg)
031600151110     C                             + ' ' + 'VABCTR'
031700151110     C                   ENDIF
031800151110     C                   ENDIF
031900151110     C* RMN/NSP
032000151110     C                   EVAL      PiStr=%trim(%subst(vindta:30:10))
032100151110     C                   EXSR      CHKNUM
032200151110     C                   IF        PiInt=*on
032300151110     C                   Z-ADD     PiVal         VABRMN
032400151110     C                   Z-ADD     PiVal         VABNSP
032500151110     C                   Z-ADD     PiVal         VATNSP
032600151110     C                   ELSE
032700151110     C                   ADD       1             errore
032800151110     C                   Z-ADD     *zeros        VABRMN
032900151110     C                   EVAL      vinmsg = %trimr(vinmsg)
033000151110     C                             + ' ' + 'VABRMN VABNSP VATNSP'
033100151110     C                   ENDIF
033200151110     C* CAD
033300151110     C                   EVAL      PiStr=%trim(%subst(vindta:143:5))
033400151110     C                   EXSR      CHKNUM
033500151110     C                   IF        PiInt=*on
033600151110     C                   Z-ADD     PiVal         Num5_0
033700151110     C                   MOVEL(p)  Num5_0        VABCAD
033800151110     C                   ELSE
033900151110     C                   ADD       1             errore
034000151110     C                   EVAL      VABCAD = *zeros
034100151110     C                   EVAL      vinmsg = %trimr(vinmsg)
034200151110     C                             + ' ' + 'VABCAD'
034300151110     C                   ENDIF
034400151110     C* NCL
034500151110     C                   EVAL      PiStr=%trim(%subst(vindta:148:5))
034600151110     C                   EXSR      CHKNUM
034700151110     C                   IF        PiInt=*on
034800151110     C                   Z-ADD     PiVal         VABNCL
034900151110     C                   ELSE
035000151110     C                   ADD       1             errore
035100151110     C                   Z-ADD     *zeros        VABNCL
035200151110     C                   EVAL      vinmsg = %trimr(vinmsg)
035300151110     C                             + ' ' + 'VABNCL'
035400151110     C                   ENDIF
035500151110     C* PKB
035600151110     C                   EVAL      PiStr=%trim(%subst(vindta:153:7))
035700151110     C                   EXSR      CHKNUM
035800151110     C                   IF        PiNum=*on
035900151110     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
036000151110     C                   Z-ADD(H)  PiVal         VABPKB
036100151110     C                   ELSE
036200151110     C                   ADD       1             errore
036300151110     C                   Z-ADD     *zeros        VABPKB
036400151110     C                   EVAL      vinmsg = %trimr(vinmsg)
036500151110     C                             + ' ' + 'VABPKB'
036600151110     C                   ENDIF
036700170427     C* VLB
036800170515     C                   IF        %trim(%subst(vindta:19:6)) <> *blanks AND
036900170515     C                             %trim(%subst(vindta:19:6)) <> *zeros
037000170427     C                   EVAL      PiStr=%trim(%subst(vindta:19:6))
037100170427     C                   EXSR      CHKNUM
037200170427     C                   IF        PiNum=*on
037300170427     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
037400170427     C                   Z-ADD(H)  PiVal         VABVLB
037500170427     C                   ELSE
037600170427     C                   ADD       1             errore
037700170427     C                   Z-ADD     *zeros        VABVLB
037800170427     C                   EVAL      vinmsg = %trimr(vinmsg)
037900170427     C                             + ' ' + 'VABVLB'
038000170427     C                   ENDIF
038100170515     C                   ENDIF
038200160119     C* CAS
038300160119     C                   IF        %subst(vindta:4:9)<> *blanks    AND
038400160119     C                             %subst(vindta:4:9)<> *zeros     AND
038500160119     C                             %subst(vindta:4:9)<> '000000,00'
038600160119     C                   EVAL      FlgCAS = '1'
038700160119     C                   EVAL      VABVCA = 'EUR'
038800160119     C                   EVAL      PiStr=%trim(%subst(vindta:4:9))
038900160119     C                   EXSR      CHKNUM
039000160119     C                   IF        PiNum=*on
039100160119     C                   Z-ADD(H)  PiVal         VABCAS
039200160119     C                   ELSE
039300160119     C                   ADD       1             errore
039400160119     C                   Z-ADD     *zeros        VABCAS
039500160119     C                   EVAL      vinmsg = %trimr(vinmsg)
039600160119     C                             + ' ' + 'VABCAS'
039700160119     C                   ENDIF
039800160119     C                   ELSE
039900160119     C                   EVAL      FlgCAS = '0'
040000160119     C                   ENDIF
040100151110     C*
040200151110     C* Reperisco la provincia dal CAP e dalla località
040300151110     C                   IF        VABCAD <> *blanks AND
040400151110     C                             VABPRD  = *blanks
040500151110     C                   CLEAR                   TISI95DS
040600151110     C                   EVAL      I95TCN = '3'
040700151110     C                   Z-ADD     datcor        I95DAT
040800151110     C                   EVAL      I95CAP = VABCAD
040900151110     C                   EVAL      I95LOC = VABLOD
041000151110     C                   EVAL      I95NAR = VABNZD
041100151110     C                   CALL      'TISI95R'
041200151110     C                   PARM                    TISI95DS
041300151110     C                   EVAL      VABPRD = O95PRV
041400151110     C                   ENDIF
041500151110     C*
041600151110     C* Considerazioni finali su CBO/CAS
041700151110     C                   IF        FlgCAS <> '0'
041800151110     C                   IF        VABCBO = '1'
041900151110     C                   EVAL      VABCBO = '4'
042000151110     C                   ENDIF
042100151110     C                   IF        VABCBO = '2'
042200151110     C                   EVAL      VABCBO = '6'
042300151110     C                   ENDIF
042400151110     C                   ENDIF
042500151110     C*
042600151110     C* Eseguo routine finale x considerazioni specifiche su importi/divise
042700151110     C                   EXSR      CHKIMPDIV
042800151110     C*
042900151110     C* Ebbene...
043000151110     C                   ADD       1             §CTRMO
043100151110     C                   IF        errore <> *zeros
043200151110     C                   ADD       1             §CTRNO
043300151110     C                   EVAL      vinflg = '2'
043400151110     C                   ELSE
043500151110     C                   ADD       1             §CTROKVB
043600151110     C                   ENDIF
043700000801     C*
043800000801     C                   ENDSR
043900070102     C*----------------------------------------------------*
044000070102     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
044100070102     C*----------------------------------------------------*
044200070102     C     EXEVATA       BEGSR
044300070102     C*
044400070102     C                   EXSR      INZVAR
044500070102     C                   EXSR      DEFCAM
044600070102     C*
044700070102     C                   EVAL      VATTRC='A'
044800140224     C                   EVAL      VATNOT = %trim(%subst(vindta:376:15))
044900070102     C                   exsr      wrivat                                       => scarico VAT
045000070102     C*
045100070102     C                   ENDSR
045200060307     C*----------------------------------------------------*
045300060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
045400060307     C*----------------------------------------------------*
045500060307     C     EXEVATB       BEGSR
045600060307     C*
045700060307     C                   EXSR      INZVAR
045800060307     C                   EXSR      DEFCAM
045900060307     C*
046000060307     C                   EVAL      VATTRC='B'
046100140304     C                   EVAL      VATNOT = %trim(%subst(vindta:713:14))
046200060307     C                   exsr      wrivat                                       => scarico VAT
046300060307     C*
046400060307     C                   ENDSR
046500070402     C*----------------------------------------------------*
046600070402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
046700070402     C*----------------------------------------------------*
046800070402     C     EXEVATE       BEGSR
046900070402     C*
047000070402     C                   EXSR      INZVAR
047100070402     C                   EXSR      DEFCAM
047200070402     C*
047300070402     C                   EVAL      VATTRC='E'
047400140224     C                   EVAL      VATNOT = %trim(%subst(vindta:46:10))
047500070402     C                   exsr      wrivat                                       => scarico VAT
047600070402     C*
047700070402     C                   ENDSR
047800140224     C*----------------------------------------------------*
047900140224     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I" e "J"
048000140224     C*----------------------------------------------------*
048100140224     C     EXEVATI       BEGSR
048200140224     C*
048300140224     C                   EXSR      INZVAR
048400140224     C                   EXSR      DEFCAM
048500140224     C*
048600140224     C                   EVAL      VATTRC='I'
048700151110     C                   EVAL      VATNOT = %trim(%subst(vindta:180:35))
048800140224     C                   exsr      wrivat                                       => scarico VAT
048900140224     C*
049000140224     C                   EVAL      VATTRC='J'
049100151110     C                   EVAL      VATNOT = %trim(%subst(vindta:180+35:15))
049200140224     C                   exsr      wrivat                                       => scarico VAT
049300140224     C*
049400140224     C                   ENDSR
049500140226     C*----------------------------------------------------*
049600140226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "S"
049700140226     C*----------------------------------------------------*
049800140226     C     EXEVATS       BEGSR
049900140226     C*
050000140226     C                   EXSR      INZVAR
050100140226     C                   EXSR      DEFCAM
050200140226     C*
050300140226     C                   EVAL      VATTRC='S'
050400140226     C                   EVAL      VATNOT=*blank
050500140226     C                   EVAL      %subst(VATNOT:1:14) =
050600151110     C                              %trim(%subst(vindta:160:20))
050700140226     C                   IF        VATNOT <> *blank
050800151110     C                   EVAL      %subst(VATNOT:17:2) = *blank
050900140226     C                   ENDIF
051000140226     C                   exsr      wrivat                                       => scarico VAT
051100140226     C*
051200140226     C                   ENDSR
051300010201     C*----------------------------------------------------*
051400040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
051500010201     C*----------------------------------------------------*
051600020305     C     WRIVAT        BEGSR
051700050628     C*
051800060223     C* Scrivo solo se valorizzato qualcosa
051900060223     C                   IF        VATNOT <> *blanks
052000040802     C                   WRITE     FIVAT000
052100060223     C                   ENDIF
052200010201     C*
052300010201     C                   ENDSR
052400010202     C*----------------------------------------------------*
052500021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
052600010202     C*----------------------------------------------------*
052700020305     C     PREVAT        BEGSR
052800010202     C*
052900021113     C* Compongo il nome del membro da dare al FIVATWWR
053000010202     C                   eval      parmbr = vlrhdl
053100010202     C                   movel     'M'           parmbr
053200050627     C                   eval      parccm = %subst(vlrKSC:2:7)
053300010202     C                   eval      paropz = '1'
053400010202     C* Effettuo la chiamata al CLLE preposto
053500040506     C                   call(e)   'TITVVTC'
053600010202     C                   parm                    parccm
053700010202     C                   parm                    parmbr
053800010202     C                   parm                    paropz
053900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
054000010202     C                   if        %error
054100010202     C                   movel     '1'           chkcall
054200010202     C                   else
054300010202     C                   movel     '0'           chkcall
054400010202     C                   endif
054500010202     C*
054600010202     C                   ENDSR
054700000801     C*----------------------------------------------------*
054800000801     C*  CONTROLLO NUMERICITA' CAMPI
054900000801     C*----------------------------------------------------*
055000000801     C     CHKNUM        BEGSR
055100000801     C*
055200000801     C                   call(e)   'ISNUMERIC'
055300000801     C                   PARM                    PiStr            30
055400160119     C                   PARM      ','           PiDecChr          1
055500000801     C                   PARM      *ZEROS        PiVal            30 9
055600000801     C                   PARM      '0'           PiInt             1
055700000801     C                   PARM      '0'           PiNum             1
055800000801     C                   IF        %error
055900000801     C                   EVAL      PiInt=*off
056000000801     C                   ENDIF
056100000801     C*
056200000801     C                   ENDSR
056300000801     C***
056400000801
056500011113
056600011113     C*----------------------------------------------------*
056700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
056800011113     C*----------------------------------------------------*
056900011113     C     CHKIMPDIV     BEGSR
057000011113     C*
057100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
057200011113     C                   Z-ADD     *zeros        wrkDec            9 9
057300011113     C*
057400011113     C* Come prima cosa effettuo considerazioni sulla divisa
057500011113     C                   IF        vabIAS > *zeros
057600011113     C                   IF        vabVAS <> 'EUR'
057700011113     C                   EVAL      vabVAS =  'ITL'
057800011113     C                   ENDIF
057900011113     C                   ENDIF
058000011113     C*
058100011113     C                   IF        vabCAS > *zeros
058200011113     C                   IF        vabVCA <> 'EUR'
058300011113     C                   EVAL      vabVCA =  'ITL'
058400011113     C                   ENDIF
058500011113     C                   ENDIF
058600011113     C*
058700011113     C                   IF        vabVMD > *zeros
058800020305     C                   IF        vabVAD <> 'EUR'
058900011113     C                   EVAL      vabVAD =  'ITL'
059000011113     C                   ENDIF
059100011113     C                   ENDIF
059200011113     C*
059300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
059400011113     C                   Z-ADD     vabIAS        wrkDec
059500011113     C                   IF        wrkDec > *zeros
059600011113     C                   IF        vabVAS = 'ITL'
059700011113     C                   EVAL      vabIAS = *zeros
059800011113     C                   ENDIF
059900011113     C                   ENDIF
060000011113     C*
060100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
060200011113     C                   Z-ADD     vabCAS        wrkDec
060300011113     C                   IF        wrkDec > *zeros
060400011113     C                   IF        vabVCA = 'ITL'
060500011113     C                   EVAL      vabCAS = *zeros
060600011113     C                   ENDIF
060700011113     C                   ENDIF
060800011113     C*
060900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
061000011113     C                   Z-ADD     vabVMD        wrkDec
061100011113     C                   IF        wrkDec > *zeros
061200011113     C                   IF        vabVAD = 'ITL'
061300011113     C                   EVAL      vabVMD = *zeros
061400011113     C                   ENDIF
061500011113     C                   ENDIF
061600011113     C*
061700011113     C                   ENDSR
061800011113     C***
061900011113
062000011113
062100000801
062200000801
062300990920      /TITLE Invio dei dati al punto operativo.
062400010202     C     invio         BEGSR
062500990920     C*
062600021113     C* 1° invio FIVAT
062700010201     C                   reset                   dscmz
062800010201     C                   move      vlrpoi        cmzdst
062900021113     C                   eval      cmzfld = 'FIVATWWR'
063000010201     C                   eval      cmzmbd = vlrhdl
063100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063200021009     C***                if        prmfir = *blanks
063300021113     C                   eval      cmzfla = 'FIVAT00F'
063400021113     C                   eval      cmzmba = 'FIVAT00F'
063500021009     C***                else
063600021009     C***                eval      cmzfla = prmfir
063700021009     C***                eval      cmzmba = prmfir
063800021009     C***                endif
063900010201     C                   eval      cmznrr = *zeros
064000020305     C                   move      §ctrokvt      cmznrr
064100021018     C                   eval      cmzlba = vlrfl1
064200010201     C                   call(e)   'TIS711C'
064300010201     C                   parm                    dscmz
064400010201     C                   parm      *blanks       esito
064500010205     C                   if        %error
064600010205     C                             or cmzerr = '1'
064700010205     C                             or esito  = '1'
064800010205     C                   eval      wrkesito = '3'
064900010205     C                   else
065000010201     C*
065100021113     C* 2° invio FIVAB
065200010201     C                   reset                   dscmz
065300010201     C                   move      vlrpoi        cmzdst
065400010201     C                   eval      cmzfld = vlrfou
065500010201     C                   eval      cmzmbd = vlrhdl
065600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
065700021009     C***                if        prmfir = *blanks
065800021113     C                   eval      cmzfla = 'FIVAB00F'
065900021113     C                   eval      cmzmba = 'FIVAB00F'
066000021009     C***                else
066100021009     C***                eval      cmzfla = prmfir
066200021009     C***                eval      cmzmba = prmfir
066300021009     C***                endif
066400010201     C                   eval      cmznrr = *zeros
066500010201     C                   move      §ctrokvb      cmznrr
066600021018     C                   eval      cmzlba = vlrfl1
066700010201     C                   call(e)   'TIS711C'
066800010201     C                   parm                    dscmz
066900010201     C                   parm      *blanks       esito
067000010201     C                   if        %error
067100010201     C                             or cmzerr = '1'
067200010201     C                             or esito  = '1'
067300010201     C                   eval      wrkesito = '3'
067400010201     C                   endif
067500010205     C                   endif
067600990920     C*
067700000613     C                   ENDSR
067800000613     C***
067900161020
068000161020
068100161020     C*--------------------------------------------------------
068200161020     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
068300161020     C*--------------------------------------------------------
068400161020     C     CARTAB        BEGSR
068500161020     C*
068600161020     C* TABELLA '15' - NAZIONI
068700161020     C                   clear                   skNAZISO
068800161020     C                   clear                   skNAZBAR
068900161020     C                   eval      tblKUT = 1
069000161020     C                   eval      tblCOD = '15'
069100161020     C     KEYtabP       setll     tabel00f
069200161020     C     KEYtabP       reade     tabel00f
069300161020     C                   dow       not %eof(tabel00f)
069400161020     C                   if        tblFLG = *blanks
069500161020     C                   movel(p)  tblUNI        ds15
069600161020     C                   add       1             jNAZ
069700161020     C                   eval      skNAZBAR(jNAZ) = tblKEY
069800161020     C                   eval      skNAZISO(jNAZ) = §15COD
069900161020     C                   endif
070000161020     C     KEYtabP       reade     tabel00f
070100161020     C                   enddo
070200161020     C*
070300161020     C                   ENDSR
070400161020     C***
070500990910
070600161020
070700161020
070800000613     C     *inzsr        BEGSR
070900990910     C*
071000990910     C     *entry        plist
071100990920     C                   parm                    tivlrds
071200990921     C                   parm      wrkesito      esito
071300000724     C                   parm                    prmlit
071400000710     C                   parm                    prmfir
071500161020     C*
071600161020     C* Chiave su TABEL00F - parziale
071700161020     C     KEYtabP       KLIST
071800161020     C                   KFLD                    tblKUT
071900161020     C                   KFLD                    tblCOD
072000000613     C*
072100000830     C* CALCOLA LA DATA CORRENTE
072200100118     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
072300100118     C                   eval      datcor = %dec(%date() : *ISO)
072400000830     C*
072500000613     C                   ENDSR
072600000613     C***
