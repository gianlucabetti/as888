000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200140221      *
000300140221      * PARTICOLARITA':
000400140221      * il tracciato e le logiche del file sono simili a quelle del traduttore predisposto per il
000500140221      * codice 1160914 (Farmaderbe), con alcune differenze.
000600140221      *
000700140221     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000800990908
000900990910     Ftivin00r  uF   E             DISK    usropn
001000021113     FFIVABwwr  O    E             DISK    usropn
001100021113     FFIVATwwr  O    E             DISK    usropn
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
003000140221     D curSped         s             15    INZ(*blanks)
003100140221     D depSped         s             15    INZ(*blanks)
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
004300010201
004400010201
004500090127     C*
004600000913     C                   reset                   rrnum
004700990921     C                   reset                   esito
004800990921     C                   reset                   wrkesito
004900000613     C*
005000040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005100000613     C*
005200010202     C* Effettuo la chiamata al CLLE preposto
005300040506     C                   call(e)   'TITVVTC'
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
006900040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
007000910830     C*--------------------------------------------------------
007100040526     C     RWFILE        BEGSR
007200990910     C*
007300990914     C                   if        not %open(tivin00r)
007400990908     C                   open      tivin00r
007500990914     C                   endif
007600021113     C                   if        not %open(fivabwwr)
007700021113     C                   open      fivabwwr
007800990914     C                   endif
007900021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
008000020305     C                   exsr      prevat
008100010201     C*
008200010202     C                   if        chkcall = '0'
008300010202     C*
008400021113     C                   if        not %open(fivatwwr)
008500021113     C                   open      fivatwwr
008600010201     C                   endif
008700990910     C*
008800010201     C                   clear                   §CTROKVB          5 0
008900020305     C                   clear                   §CTROKVT          5 0
009000000801     C                   clear                   §CTRMO            5 0
009100000801     C                   clear                   §CTRNO            5 0
009200990910     C*
009300921023     C                   DO        *HIVAL
009400990913     C*
009500990915     C                   READ      tivin00r                               70
009600050627     C                   if        vindta > *blanks
009700000613     C                   add       1             rrnum
009800000801     C*
009900000801     C                   if        *in70 = *off
010000000801     C                             and
010100000801     C                             (vinflg = *blanks
010200000801     C                              or vinflg = '0'
010300000801     C                              or vinflg = '2')
010400000801     C*
010500000801     C                   clear                   vinmsg
010600000801     C                   eval      vinflg = '1'
010700070402     C*
010800070402     C* Verifico la rottura d codice x raggruppamento spedizione cliente
010900140221     C                   eval      curSped = %subst(vindta:46:15)
011000070402     C                   if        curSped <> depSped
011100070402     C                   clear                   fivab000
011200070402     C                   clear                   fivat000
011300050628     C*
011400060315     C                   exsr      impvab                                       => carico  VAB
011500090127     C*
011600060315     C                   exsr      wrivab                                       => scarico VAB
011700070402     C*
011800070402     C* Scrivo anche estensione dettaglio bolle - tipo record "A" => REFERENTE CONSEGNA
011900140221     C***                exsr      exevata
012000070402     C*
012100070402     C* Scrivo anche estensione dettaglio bolle - tipo record "B" => TELEFONO DESTINATARIO
012200070402     C                   exsr      exevatb
012300070402     C*
012400070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
012500070402     C                   exsr      exevate
012600070402     C*
012700070402     C* Salvo il raggruppamento spedizione cliente corrente
012800070402     C                   eval      depSped = curSped
012900070402     C*
013000070402     C                   else
013100070402     C*
013200070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
013300070402     C                   exsr      exevate
013400070402     C                   endif
013500000905     C*
013600000905     C                   else
013700000905     C                   eval      vinflg = '1'
013800050628     C                   endif
013900000905     C                   endif
014000000905     C*
014100000905     C  N70              update    tivin000
014200000905     C*
014300991022     C  N70              ENDdo
014400010202     C*
014500010202     C                   endif
014600990910
014700990910     C* Se non ci sono record con errori ...
014800000710     C                   if        §ctrno = 0
014900990910     C* ... restituisco esito OK.
015000990921     C                   eval      wrkesito = '0'
015100990910     C                   else
015200010201     C                   if        §ctrokvb > 0
015300990921     C                   eval      wrkesito = '1'
015400000710     C                   else
015500000710     C                   eval      wrkesito = '2'
015600990910     C                   endif
015700000710     C                   endif
015800990910     C*
015900990914     C                   if        %open(tivin00r)
016000990908     C                   close     tivin00r
016100990914     C                   endif
016200021113     C                   if        %open(fivabwwr)
016300021113     C                   close     fivabwwr
016400990914     C                   endif
016500021113     C                   if        %open(fivatwwr)
016600021113     C                   close     fivatwwr
016700010201     C                   endif
016800990910     C*
016900010201     C                   if        §ctrokvb > 0
017000000724     C                             and vlrpoi <> *zeros
017100010202     C                   exsr      invio
017200990920     C                   endif
017300990920     C*
017400910830     C                   ENDSR
017500000613     C***
017600010305
017700010305     C*----------------------------------------------------*
017800020305     C*  SCARICAMENTO BUFFER RECORDS VAB
017900010305     C*----------------------------------------------------*
018000090127     C     WRIVAB        BEGSR
018100090127     C*
018200090127     C* Quindi scarico il buffer del file d testata
018300090127     C                   write     fivab000                                     => scarico il VAB
018400010305     C*
018500010305     C                   ENDSR
018600990920
018700000801     C*----------------------------------------------------*
018800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
018900000801     C*----------------------------------------------------*
019000010201     C     INZVAR        BEGSR
019100000801     C*
019200040802     C                   Z-ADD     *zeros        Num5_0            5 0
019300040802     C                   MOVEL     '0'           FlgCAS            1
019400000801     C*
019500000801     C                   ENDSR
019600000801     C*----------------------------------------------------*
019700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
019800000801     C*----------------------------------------------------*
019900000801     C     DEFCAM        BEGSR
020000070718     C*
020100070718     C* Imposto i valori di default...
020200140221     C                   Z-ADD     2140989       VABCCM
020300140221     C                   Z-ADD     2140989       VATCCM
020400140221     C                   Z-ADD     006           VABLNP
020500140221     C                   Z-ADD     006           VATLNP
020600091119     C                   Z-ADD     000           VABCTR
020700070718     C                   MOVEL     '1'           VABCBO
020800070718     C                   MOVEL     '7Q'          VABCTM
020900020619     C* ... e poi verifico se sono stati passati come parametri
021000020619     C                   IF        vlrppt > *blanks
021100040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
021200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
021300020619     C                   EXSR      CHKNUM
021400020619     C                   IF        PiInt=*on
021500020619     C                   Z-ADD     PiVal         VABCCM
021600020619     C                   Z-ADD     PiVal         VATCCM
021700020619     C                   ENDIF
021800040506     C                   ENDIF
021900040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
022000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022100020619     C                   EXSR      CHKNUM
022200020619     C                   IF        PiInt=*on
022300020619     C                   Z-ADD     PiVal         VABLNP
022400020619     C                   Z-ADD     PiVal         VATLNP
022500040506     C                   ENDIF
022600020619     C                   ENDIF
022700040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
022800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
022900020619     C                   EXSR      CHKNUM
023000020619     C                   IF        PiInt=*on
023100020619     C                   Z-ADD     PiVal         VABCTR
023200040506     C                   ENDIF
023300020619     C                   ENDIF
023400020619     C                   ENDIF
023500000801     C*
023600000801     C                   ENDSR
023700000801     C*----------------------------------------------------*
023800021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
023900000801     C*----------------------------------------------------*
024000040823     C     IMPVAB        BEGSR
024100040823     C*
024200020305     C                   EXSR      INZVAR
024300020305     C                   EXSR      DEFCAM
024400010305     C*
024500000801     C                   Z-ADD     *zeros        errore            1 0
024600000830     C                   MOVEL     datcor        VABAAS
024700020305     C                   MOVEL     datcor        VATAAS
024800040526     C                   MOVE      datcor        VABMGS
024900040823     C                   MOVE(P)   vlrpoi        VABFGS
025000040823     C                   MOVE(P)   vlrpoi        VATFGS
025100050628     C*
025200140221     C                   EVAL      VABRSD=%trim(%subst(vindta:276:35))
025300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
025400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
025500020117     C* ==
025600140424     C                   EVAL      VABRD2=%trim(%subst(vindta:316:35))
025700140221     C                   EVAL      VABIND=%trim(%subst(vindta:341:35))
025800070102     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
025900070102     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
026000140221     C                   EVAL      VABRMA=%trim(%subst(vindta:46:15))
026100140221     C                   EVAL      w70=%trim(%subst(vindta:641:70))
026200140221     C                   EVAL      VABNOT=%subst(w70:1:35)
026300140221     C                   EVAL      VABNT2=%subst(w70:36:35)
026400070102     C                   IF        %subst(vindta:43:1) = 'N'
026500070102     C                   EVAL      VABCBO='1'
026600070102     C                   ENDIF
026700070102     C                   IF        %subst(vindta:43:1) = 'P'
026800070102     C                   EVAL      VABCBO='2'
026900070102     C                   ENDIF
027000140321     C                   SELECT
027100140321     C                   WHEN      %subst(vindta:460:1) = 'E' or
027200140321     C                             %subst(vindta:460:1) = 'H'
027300140321     C                   EVAL      VABTSP=%subst(vindta:460:1)
027400140321     C                   OTHER
027500140321     C                   EVAL      VABTSP='C'
027600140321     C                   ENDSL
027700140414     C                   IF        %subst(vindta:461:1) = 'A'
027800140414     C                   EVAL      VABTC1='A'
027900140414     C                   ENDIF
028000050628     C* CAP
028100070719     C                   EVAL      PiStr=%trim(%subst(vindta:391:5))
028200010201     C                   EXSR      CHKNUM
028300010201     C                   IF        PiInt=*on
028400010201     C                   Z-ADD     PiVal         Num5_0
028500040506     C                   MOVEL(P)  Num5_0        VABCAD
028600010201     C                   ELSE
028700040506     C                   ADD       1             errore
028800010201     C                   EVAL      vinmsg = %trimr(vinmsg)
028900040506     C                             + ' ' + 'VABCAD'
029000010201     C                   ENDIF
029100040506     C* Reperisco la provincia dal CAP e dalla località
029200040526     C                   IF        VABCAD <> *blanks AND
029300040526     C                             VABPRD  = *blanks
029400040506     C                   CLEAR                   TISI95DS
029500040506     C                   EVAL      I95TCN = '3'
029600040506     C                   Z-ADD     datcor        I95DAT
029700040506     C                   EVAL      I95CAP = VABCAD
029800040506     C                   EVAL      I95LOC = VABLOD
029900050627     C                   EVAL      I95NAR = VABNZD
030000040506     C                   CALL      'TISI95R'
030100040506     C                   PARM                    TISI95DS
030200040506     C                   EVAL      VABPRD = O95PRV
030300040506     C                   ENDIF
030400060307     C* NCL
030500070102     C                   EVAL      PiStr=%trim(%subst(vindta:432:3))
030600060307     C                   EXSR      CHKNUM
030700060307     C                   IF        PiInt=*on
030800060307     C                   ADD       PiVal         VABNCL
030900060307     C                   ELSE
031000060307     C                   ADD       1             errore
031100060307     C                   EVAL      vinmsg = %trimr(vinmsg)
031200060307     C                             + ' ' + 'VABNCL'
031300060307     C                   ENDIF
031400040506     C* PKB
031500070102     C                   EVAL      PiStr=%trim(%subst(vindta:435:7))
031600010201     C                   EXSR      CHKNUM
031700010201     C                   IF        PiNum=*on
031800070102     C                   EVAL      PiVal=PiVal/1000                             da grammi a kg
031900060316     C                   Z-ADD     PiVal         VABPKB
032000010201     C                   ELSE
032100010201     C                   ADD       1             errore
032200010201     C                   EVAL      vinmsg = %trimr(vinmsg)
032300010201     C                             + ' ' + 'VABPKB'
032400010201     C                   ENDIF
032500100118     C* NSP => Stacco un numeratore da AZNUM
032600100118     C                   clear                   TRUL33DS
032700100118     C                   eval      I33OPE = *zeros
032800100118     C                   eval      I33CNU = 302
032900100118     C                   eval      I33NUM = 1
033000100118     C                   movel     TRUL33DS      KPJBU
033100100118     C                   call      'TRUL33R'
033200100118     C                   parm                    KPJBA
033300100118     C                   movel     KPJBU         TRUL33DS
033400100118     C                   if        O33ERR = *zeros
033500100118     C                   z-add     O33NRF        VABNSP
033600100118     C                   z-add     O33NRF        VATNSP
033700100118     C                   else
033800100118     C                   ADD       1             errore
033900100118     C                   EVAL      vinmsg = %trimr(vinmsg)
034000100118     C                             + ' ' + 'VABNSP VATNSP'
034100100118     C                   endif
034200090511     C* RMN
034300091210     C***                EVAL      PiStr=%trim(%subst(vindta:13:30))
034400140221     C                   EVAL      PiStr=%trim(%subst(vindta:47:14))
034500090511     C                   EXSR      CHKNUM
034600090511     C                   IF        PiInt=*on
034700090511     C                   Z-ADD     PiVal         VABRMN
034800090511     C                   ELSE
034900090511     C                   ADD       1             errore
035000090511     C                   EVAL      vinmsg = %trimr(vinmsg)
035100090511     C                             + ' ' + 'VABRMN'
035200090511     C                   ENDIF
035300060307     C* CAS
035400090127     C                   IF        %trim(%subst(vindta:502:9))<>'000000.00' AND
035500090608     C                             %trim(%subst(vindta:502:9))<>*blanks     AND
035600090608     C                             %trim(%subst(vindta:502:9))<>*zeros
035700070102     C                   EVAL      VABVCA = 'EUR'
035800060307     C                   EVAL      FlgCAS = '1'
035900070102     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
036000060307     C                   EXSR      CHKNUM
036100060307     C                   IF        PiNum=*on
036200060316     C                   Z-ADD     PiVal         VABCAS
036300060307     C                   ELSE
036400060307     C                   ADD       1             errore
036500060307     C                   EVAL      vinmsg = %trimr(vinmsg)
036600060307     C                             + ' ' + 'VABCAS'
036700060307     C                   ENDIF
036800070102     C                   ENDIF
036900060316     C* IAS
037000140221     C                   IF        %trim(%subst(vindta:632:9))<>'000000.00' AND
037100140221     C                             %trim(%subst(vindta:632:9))<>*blanks     AND
037200140221     C                             %trim(%subst(vindta:632:9))<>*zeros
037300070102     C                   EVAL      VABVAS = 'EUR'
037400140221     C                   EVAL      PiStr=%trim(%subst(vindta:632:9))
037500060316     C                   EXSR      CHKNUM
037600060316     C                   IF        PiNum=*on
037700060316     C                   Z-ADD     PiVal         VABIAS
037800060316     C                   ELSE
037900060316     C                   ADD       1             errore
038000060316     C                   EVAL      vinmsg = %trimr(vinmsg)
038100060316     C                             + ' ' + 'VABIAS'
038200070102     C                   ENDIF
038300060316     C                   ENDIF
038400010205     C*
038500010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
038600040802     C                   IF        FlgCAS <> '0'
038700070102     C                   IF        VABCBO = '1'
038800010205     C                   EVAL      VABCBO = '4'
038900010205     C                   ELSE
039000070102     C                   EVAL      VABCBO = '6'
039100070102     C                   ENDIF
039200010205     C                   ENDIF
039300020305     C*
039400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
039500011113     C                   EXSR      CHKIMPDIV
039600010202     C*
039700000801     C* Ebbene...
039800000801     C                   ADD       1             §CTRMO
039900010201     C                   IF        errore <> *zeros
040000000801     C                   ADD       1             §CTRNO
040100000801     C                   EVAL      vinflg = '2'
040200000801     C                   ELSE
040300010201     C                   ADD       1             §CTROKVB
040400000801     C                   ENDIF
040500000801     C*
040600000801     C                   ENDSR
040700070102     C*----------------------------------------------------*
040800070102     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
040900070102     C*----------------------------------------------------*
041000070102     C     EXEVATA       BEGSR
041100070102     C*
041200070102     C                   EXSR      INZVAR
041300070102     C                   EXSR      DEFCAM
041400070102     C*
041500070102     C                   EVAL      VATTRC='A'
041600090127     C                   EVAL      VATNOT = %trim(%subst(vindta:581:30))
041700070102     C                   exsr      wrivat                                       => scarico VAT
041800070102     C*
041900070102     C                   ENDSR
042000060307     C*----------------------------------------------------*
042100060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
042200060307     C*----------------------------------------------------*
042300060307     C     EXEVATB       BEGSR
042400060307     C*
042500060307     C                   EXSR      INZVAR
042600060307     C                   EXSR      DEFCAM
042700060307     C*
042800060307     C                   EVAL      VATTRC='B'
042900070102     C                   EVAL      VATNOT = %trim(%subst(vindta:376:15))
043000060307     C                   exsr      wrivat                                       => scarico VAT
043100060307     C*
043200060307     C                   ENDSR
043300070402     C*----------------------------------------------------*
043400070402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
043500070402     C*----------------------------------------------------*
043600070402     C     EXEVATE       BEGSR
043700070402     C*
043800070402     C                   EXSR      INZVAR
043900070402     C                   EXSR      DEFCAM
044000070402     C*
044100070402     C                   EVAL      VATTRC='E'
044200140221     C                   EVAL      VATNOT = %trim(%subst(vindta:547:25))
044300070402     C                   exsr      wrivat                                       => scarico VAT
044400070402     C*
044500070402     C                   ENDSR
044600010201     C*----------------------------------------------------*
044700040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
044800010201     C*----------------------------------------------------*
044900020305     C     WRIVAT        BEGSR
045000050628     C*
045100060223     C* Scrivo solo se valorizzato qualcosa
045200060223     C                   IF        VATNOT <> *blanks
045300040802     C                   WRITE     FIVAT000
045400060223     C                   ENDIF
045500010201     C*
045600010201     C                   ENDSR
045700010202     C*----------------------------------------------------*
045800021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
045900010202     C*----------------------------------------------------*
046000020305     C     PREVAT        BEGSR
046100010202     C*
046200021113     C* Compongo il nome del membro da dare al FIVATWWR
046300010202     C                   eval      parmbr = vlrhdl
046400010202     C                   movel     'M'           parmbr
046500050627     C                   eval      parccm = %subst(vlrKSC:2:7)
046600010202     C                   eval      paropz = '1'
046700010202     C* Effettuo la chiamata al CLLE preposto
046800040506     C                   call(e)   'TITVVTC'
046900010202     C                   parm                    parccm
047000010202     C                   parm                    parmbr
047100010202     C                   parm                    paropz
047200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047300010202     C                   if        %error
047400010202     C                   movel     '1'           chkcall
047500010202     C                   else
047600010202     C                   movel     '0'           chkcall
047700010202     C                   endif
047800010202     C*
047900010202     C                   ENDSR
048000000801     C*----------------------------------------------------*
048100000801     C*  CONTROLLO NUMERICITA' CAMPI
048200000801     C*----------------------------------------------------*
048300000801     C     CHKNUM        BEGSR
048400000801     C*
048500000801     C                   call(e)   'ISNUMERIC'
048600000801     C                   PARM                    PiStr            30
048700070102     C                   PARM      '.'           PiDecChr          1
048800000801     C                   PARM      *ZEROS        PiVal            30 9
048900000801     C                   PARM      '0'           PiInt             1
049000000801     C                   PARM      '0'           PiNum             1
049100000801     C                   IF        %error
049200000801     C                   EVAL      PiInt=*off
049300000801     C                   ENDIF
049400000801     C*
049500000801     C                   ENDSR
049600000801     C***
049700000801
049800011113
049900011113     C*----------------------------------------------------*
050000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
050100011113     C*----------------------------------------------------*
050200011113     C     CHKIMPDIV     BEGSR
050300011113     C*
050400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
050500011113     C                   Z-ADD     *zeros        wrkDec            9 9
050600011113     C*
050700011113     C* Come prima cosa effettuo considerazioni sulla divisa
050800011113     C                   IF        vabIAS > *zeros
050900011113     C                   IF        vabVAS <> 'EUR'
051000011113     C                   EVAL      vabVAS =  'ITL'
051100011113     C                   ENDIF
051200011113     C                   ENDIF
051300011113     C*
051400011113     C                   IF        vabCAS > *zeros
051500011113     C                   IF        vabVCA <> 'EUR'
051600011113     C                   EVAL      vabVCA =  'ITL'
051700011113     C                   ENDIF
051800011113     C                   ENDIF
051900011113     C*
052000011113     C                   IF        vabVMD > *zeros
052100020305     C                   IF        vabVAD <> 'EUR'
052200011113     C                   EVAL      vabVAD =  'ITL'
052300011113     C                   ENDIF
052400011113     C                   ENDIF
052500011113     C*
052600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
052700011113     C                   Z-ADD     vabIAS        wrkDec
052800011113     C                   IF        wrkDec > *zeros
052900011113     C                   IF        vabVAS = 'ITL'
053000011113     C                   EVAL      vabIAS = *zeros
053100011113     C                   ENDIF
053200011113     C                   ENDIF
053300011113     C*
053400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
053500011113     C                   Z-ADD     vabCAS        wrkDec
053600011113     C                   IF        wrkDec > *zeros
053700011113     C                   IF        vabVCA = 'ITL'
053800011113     C                   EVAL      vabCAS = *zeros
053900011113     C                   ENDIF
054000011113     C                   ENDIF
054100011113     C*
054200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
054300011113     C                   Z-ADD     vabVMD        wrkDec
054400011113     C                   IF        wrkDec > *zeros
054500011113     C                   IF        vabVAD = 'ITL'
054600011113     C                   EVAL      vabVMD = *zeros
054700011113     C                   ENDIF
054800011113     C                   ENDIF
054900011113     C*
055000011113     C                   ENDSR
055100011113     C***
055200011113
055300011113
055400000801
055500000801
055600990920      /TITLE Invio dei dati al punto operativo.
055700010202     C     invio         BEGSR
055800990920     C*
055900021113     C* 1° invio FIVAT
056000010201     C                   reset                   dscmz
056100010201     C                   move      vlrpoi        cmzdst
056200021113     C                   eval      cmzfld = 'FIVATWWR'
056300010201     C                   eval      cmzmbd = vlrhdl
056400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
056500021009     C***                if        prmfir = *blanks
056600021113     C                   eval      cmzfla = 'FIVAT00F'
056700021113     C                   eval      cmzmba = 'FIVAT00F'
056800021009     C***                else
056900021009     C***                eval      cmzfla = prmfir
057000021009     C***                eval      cmzmba = prmfir
057100021009     C***                endif
057200010201     C                   eval      cmznrr = *zeros
057300020305     C                   move      §ctrokvt      cmznrr
057400021018     C                   eval      cmzlba = vlrfl1
057500010201     C                   call(e)   'TIS711C'
057600010201     C                   parm                    dscmz
057700010201     C                   parm      *blanks       esito
057800010205     C                   if        %error
057900010205     C                             or cmzerr = '1'
058000010205     C                             or esito  = '1'
058100010205     C                   eval      wrkesito = '3'
058200010205     C                   else
058300010201     C*
058400021113     C* 2° invio FIVAB
058500010201     C                   reset                   dscmz
058600010201     C                   move      vlrpoi        cmzdst
058700010201     C                   eval      cmzfld = vlrfou
058800010201     C                   eval      cmzmbd = vlrhdl
058900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059000021009     C***                if        prmfir = *blanks
059100021113     C                   eval      cmzfla = 'FIVAB00F'
059200021113     C                   eval      cmzmba = 'FIVAB00F'
059300021009     C***                else
059400021009     C***                eval      cmzfla = prmfir
059500021009     C***                eval      cmzmba = prmfir
059600021009     C***                endif
059700010201     C                   eval      cmznrr = *zeros
059800010201     C                   move      §ctrokvb      cmznrr
059900021018     C                   eval      cmzlba = vlrfl1
060000010201     C                   call(e)   'TIS711C'
060100010201     C                   parm                    dscmz
060200010201     C                   parm      *blanks       esito
060300010201     C                   if        %error
060400010201     C                             or cmzerr = '1'
060500010201     C                             or esito  = '1'
060600010201     C                   eval      wrkesito = '3'
060700010201     C                   endif
060800010205     C                   endif
060900990920     C*
061000000613     C                   ENDSR
061100000613     C***
061200990910
061300000613     C     *inzsr        BEGSR
061400990910     C*
061500990910     C     *entry        plist
061600990920     C                   parm                    tivlrds
061700990921     C                   parm      wrkesito      esito
061800000724     C                   parm                    prmlit
061900000710     C                   parm                    prmfir
062000000613     C*
062100000830     C* CALCOLA LA DATA CORRENTE
062200100118     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
062300100118     C                   eval      datcor = %dec(%date() : *ISO)
062400000830     C*
062500000613     C                   ENDSR
062600000613     C***
