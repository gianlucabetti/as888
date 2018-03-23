000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130116     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
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
002500070402     D curSped         s             25    INZ(*blanks)
002600070402     D depSped         s             25    INZ(*blanks)
002700100118
002800100118     D*------------------
002900100118     D* DS REPERIMENTO NUMERATORE
003000100118     D*------------------
003100100118     D trul33ds      e ds                  inz
003200100118     D*------------------
003300100118     D* DS ARCHITETTURA
003400100118     D*------------------
003500100118     D kpjba         e ds                  inz
003600100118
003700010201
003800010201
003900090127     C*
004000000913     C                   reset                   rrnum
004100990921     C                   reset                   esito
004200990921     C                   reset                   wrkesito
004300000613     C*
004400040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004500000613     C*
004600010202     C* Effettuo la chiamata al CLLE preposto
004700040506     C                   call(e)   'TITVVTC'
004800010202     C                   parm                    parccm
004900010202     C                   parm                    parmbr
005000010202     C                   parm      '2'           paropz
005100050201     C*
005200050201     C* Effettuo lancio TISI95 solo x chiusura
005300050201     C                   CLEAR                   TISI95DS
005400050201     C                   EVAL      I95TLA = 'C'
005500050201     C                   CALL      'TISI95R'
005600050201     C                   PARM                    TISI95DS
005700000616     C*
005800000801     C
005900010201     C                   seton                                        LR
006000990908
006100000801
006200910830     C*--------------------------------------------------------
006300040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
006400910830     C*--------------------------------------------------------
006500040526     C     RWFILE        BEGSR
006600990910     C*
006700990914     C                   if        not %open(tivin00r)
006800990908     C                   open      tivin00r
006900990914     C                   endif
007000021113     C                   if        not %open(fivabwwr)
007100021113     C                   open      fivabwwr
007200990914     C                   endif
007300021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007400020305     C                   exsr      prevat
007500010201     C*
007600010202     C                   if        chkcall = '0'
007700010202     C*
007800021113     C                   if        not %open(fivatwwr)
007900021113     C                   open      fivatwwr
008000010201     C                   endif
008100990910     C*
008200010201     C                   clear                   §CTROKVB          5 0
008300020305     C                   clear                   §CTROKVT          5 0
008400000801     C                   clear                   §CTRMO            5 0
008500000801     C                   clear                   §CTRNO            5 0
008600990910     C*
008700921023     C                   DO        *HIVAL
008800990913     C*
008900990915     C                   READ      tivin00r                               70
009000050627     C                   if        vindta > *blanks
009100000613     C                   add       1             rrnum
009200000801     C*
009300000801     C                   if        *in70 = *off
009400000801     C                             and
009500000801     C                             (vinflg = *blanks
009600000801     C                              or vinflg = '0'
009700000801     C                              or vinflg = '2')
009800000801     C*
009900000801     C                   clear                   vinmsg
010000000801     C                   eval      vinflg = '1'
010100070402     C*
010200070402     C* Verifico la rottura d codice x raggruppamento spedizione cliente
010300070402     C                   eval      curSped = %subst(vindta:46:25)
010400070402     C                   if        curSped <> depSped
010500070402     C                   clear                   fivab000
010600070402     C                   clear                   fivat000
010700050628     C*
010800060315     C                   exsr      impvab                                       => carico  VAB
010900090127     C*
011000060315     C                   exsr      wrivab                                       => scarico VAB
011100070402     C*
011200070402     C* Scrivo anche estensione dettaglio bolle - tipo record "A" => REFERENTE CONSEGNA
011300070402     C                   exsr      exevata
011400070402     C*
011500070402     C* Scrivo anche estensione dettaglio bolle - tipo record "B" => TELEFONO DESTINATARIO
011600070402     C                   exsr      exevatb
011700070402     C*
011800070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
011900070402     C                   exsr      exevate
012000070402     C*
012100070402     C* Salvo il raggruppamento spedizione cliente corrente
012200070402     C                   eval      depSped = curSped
012300070402     C*
012400070402     C                   else
012500070402     C*
012600070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
012700070402     C                   exsr      exevate
012800070402     C                   endif
012900000905     C*
013000000905     C                   else
013100000905     C                   eval      vinflg = '1'
013200050628     C                   endif
013300000905     C                   endif
013400000905     C*
013500000905     C  N70              update    tivin000
013600000905     C*
013700991022     C  N70              ENDdo
013800010202     C*
013900010202     C                   endif
014000990910
014100990910     C* Se non ci sono record con errori ...
014200000710     C                   if        §ctrno = 0
014300990910     C* ... restituisco esito OK.
014400990921     C                   eval      wrkesito = '0'
014500990910     C                   else
014600010201     C                   if        §ctrokvb > 0
014700990921     C                   eval      wrkesito = '1'
014800000710     C                   else
014900000710     C                   eval      wrkesito = '2'
015000990910     C                   endif
015100000710     C                   endif
015200990910     C*
015300990914     C                   if        %open(tivin00r)
015400990908     C                   close     tivin00r
015500990914     C                   endif
015600021113     C                   if        %open(fivabwwr)
015700021113     C                   close     fivabwwr
015800990914     C                   endif
015900021113     C                   if        %open(fivatwwr)
016000021113     C                   close     fivatwwr
016100010201     C                   endif
016200990910     C*
016300010201     C                   if        §ctrokvb > 0
016400000724     C                             and vlrpoi <> *zeros
016500010202     C                   exsr      invio
016600990920     C                   endif
016700990920     C*
016800910830     C                   ENDSR
016900000613     C***
017000010305
017100010305     C*----------------------------------------------------*
017200020305     C*  SCARICAMENTO BUFFER RECORDS VAB
017300010305     C*----------------------------------------------------*
017400090127     C     WRIVAB        BEGSR
017500090127     C*
017600090127     C* Quindi scarico il buffer del file d testata
017700090127     C                   write     fivab000                                     => scarico il VAB
017800010305     C*
017900010305     C                   ENDSR
018000990920
018100000801     C*----------------------------------------------------*
018200000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
018300000801     C*----------------------------------------------------*
018400010201     C     INZVAR        BEGSR
018500000801     C*
018600040802     C                   Z-ADD     *zeros        Num5_0            5 0
018700040802     C                   MOVEL     '0'           FlgCAS            1
018800000801     C*
018900000801     C                   ENDSR
019000000801     C*----------------------------------------------------*
019100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
019200000801     C*----------------------------------------------------*
019300000801     C     DEFCAM        BEGSR
019400070718     C*
019500070718     C* Imposto i valori di default...
019600100525     C                   Z-ADD     1270553       VABCCM
019700100525     C                   Z-ADD     1270553       VATCCM
019800100525     C                   Z-ADD     127           VABLNP
019900100525     C                   Z-ADD     127           VATLNP
020000091119     C                   Z-ADD     000           VABCTR
020100070718     C                   MOVEL     '1'           VABCBO
020200070718     C                   MOVEL     '7Q'          VABCTM
020300020619     C* ... e poi verifico se sono stati passati come parametri
020400020619     C                   IF        vlrppt > *blanks
020500040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
020600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
020700020619     C                   EXSR      CHKNUM
020800020619     C                   IF        PiInt=*on
020900020619     C                   Z-ADD     PiVal         VABCCM
021000020619     C                   Z-ADD     PiVal         VATCCM
021100020619     C                   ENDIF
021200040506     C                   ENDIF
021300040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
021400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
021500020619     C                   EXSR      CHKNUM
021600020619     C                   IF        PiInt=*on
021700020619     C                   Z-ADD     PiVal         VABLNP
021800020619     C                   Z-ADD     PiVal         VATLNP
021900040506     C                   ENDIF
022000020619     C                   ENDIF
022100040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
022200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
022300020619     C                   EXSR      CHKNUM
022400020619     C                   IF        PiInt=*on
022500020619     C                   Z-ADD     PiVal         VABCTR
022600040506     C                   ENDIF
022700020619     C                   ENDIF
022800020619     C                   ENDIF
022900000801     C*
023000000801     C                   ENDSR
023100000801     C*----------------------------------------------------*
023200021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
023300000801     C*----------------------------------------------------*
023400040823     C     IMPVAB        BEGSR
023500040823     C*
023600020305     C                   EXSR      INZVAR
023700020305     C                   EXSR      DEFCAM
023800010305     C*
023900000801     C                   Z-ADD     *zeros        errore            1 0
024000000830     C                   MOVEL     datcor        VABAAS
024100020305     C                   MOVEL     datcor        VATAAS
024200040526     C                   MOVE      datcor        VABMGS
024300040823     C                   MOVE(P)   vlrpoi        VABFGS
024400040823     C                   MOVE(P)   vlrpoi        VATFGS
024500050628     C*
024600070102     C                   EVAL      VABRSD=%trim(%subst(vindta:276:40))
024700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
024800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
024900020117     C* ==
025000070719     C                   EVAL      VABRD2=%trim(%subst(vindta:316:20))
025100090427     C                   EVAL      VABIND=%trim(%trim(%subst(vindta:336:5))+' '+
025200070102     C                                    %trim(%subst(vindta:341:30))+' '+
025300090427     C                                    %trim(%subst(vindta:371:5)))
025400070102     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
025500070102     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
025600090511     C                   EVAL      VABRMA=%trim(%subst(vindta:46:25))
025700070102     C                   EVAL      VABNOT=%trim(%subst(vindta:514:30))
025800070102     C                   IF        %subst(vindta:43:1) = 'N'
025900070102     C                   EVAL      VABCBO='1'
026000070102     C                   ENDIF
026100070102     C                   IF        %subst(vindta:43:1) = 'P'
026200070102     C                   EVAL      VABCBO='2'
026300070102     C                   ENDIF
026400070102     C                   IF        %subst(vindta:511:3) = 'ABM'
026500070102     C                   EVAL      VABTIC='BM'
026600070102     C                   ENDIF
026700070102     C                   IF        %subst(vindta:511:3) = 'ABS'
026800070102     C                   EVAL      VABTIC='B'
026900070102     C                   ENDIF
027000070102     C                   IF        %subst(vindta:511:3) = 'CON'
027100070102     C                   EVAL      VABTIC=*blanks
027200070102     C                   ENDIF
027300070102     C                   IF        %subst(vindta:511:3) = 'ACM'
027400070102     C                   EVAL      VABTIC='CM'
027500070102     C                   ENDIF
027600050628     C* CAP
027700070719     C                   EVAL      PiStr=%trim(%subst(vindta:391:5))
027800010201     C                   EXSR      CHKNUM
027900010201     C                   IF        PiInt=*on
028000010201     C                   Z-ADD     PiVal         Num5_0
028100040506     C                   MOVEL(P)  Num5_0        VABCAD
028200010201     C                   ELSE
028300040506     C                   ADD       1             errore
028400010201     C                   EVAL      vinmsg = %trimr(vinmsg)
028500040506     C                             + ' ' + 'VABCAD'
028600010201     C                   ENDIF
028700040506     C* Reperisco la provincia dal CAP e dalla località
028800040526     C                   IF        VABCAD <> *blanks AND
028900040526     C                             VABPRD  = *blanks
029000040506     C                   CLEAR                   TISI95DS
029100040506     C                   EVAL      I95TCN = '3'
029200040506     C                   Z-ADD     datcor        I95DAT
029300040506     C                   EVAL      I95CAP = VABCAD
029400040506     C                   EVAL      I95LOC = VABLOD
029500050627     C                   EVAL      I95NAR = VABNZD
029600040506     C                   CALL      'TISI95R'
029700040506     C                   PARM                    TISI95DS
029800040506     C                   EVAL      VABPRD = O95PRV
029900040506     C                   ENDIF
030000060307     C* NCL
030100070102     C                   EVAL      PiStr=%trim(%subst(vindta:432:3))
030200060307     C                   EXSR      CHKNUM
030300060307     C                   IF        PiInt=*on
030400060307     C                   ADD       PiVal         VABNCL
030500060307     C                   ELSE
030600060307     C                   ADD       1             errore
030700060307     C                   EVAL      vinmsg = %trimr(vinmsg)
030800060307     C                             + ' ' + 'VABNCL'
030900060307     C                   ENDIF
031000040506     C* PKB
031100070102     C                   EVAL      PiStr=%trim(%subst(vindta:435:7))
031200010201     C                   EXSR      CHKNUM
031300010201     C                   IF        PiNum=*on
031400070102     C                   EVAL      PiVal=PiVal/1000                             da grammi a kg
031500060316     C                   Z-ADD     PiVal         VABPKB
031600010201     C                   ELSE
031700010201     C                   ADD       1             errore
031800010201     C                   EVAL      vinmsg = %trimr(vinmsg)
031900010201     C                             + ' ' + 'VABPKB'
032000010201     C                   ENDIF
032100140317     C* VLB
032200140319     C                   EVAL      PiStr=%trim(%subst(vindta:461:6))
032300140317     C                   EXSR      CHKNUM
032400140317     C                   IF        PiNum=*on
032500140319     C                   EVAL      PiVal=PiVal/1000                             da decm3 a m3
032600140319     C                   Z-ADD     PiVal         VABVLB
032700140317     C                   ELSE
032800140317     C                   ADD       1             errore
032900140317     C                   EVAL      vinmsg = %trimr(vinmsg)
033000140317     C                             + ' ' + 'VABVLB'
033100140317     C                   ENDIF
033200100118     C* NSP => Stacco un numeratore da AZNUM
033300100118     C                   clear                   TRUL33DS
033400100118     C                   eval      I33OPE = *zeros
033500100118     C                   eval      I33CNU = 302
033600100118     C                   eval      I33NUM = 1
033700100118     C                   movel     TRUL33DS      KPJBU
033800100118     C                   call      'TRUL33R'
033900100118     C                   parm                    KPJBA
034000100118     C                   movel     KPJBU         TRUL33DS
034100100118     C                   if        O33ERR = *zeros
034200100118     C                   z-add     O33NRF        VABNSP
034300100118     C                   z-add     O33NRF        VATNSP
034400100118     C                   else
034500100118     C                   ADD       1             errore
034600100118     C                   EVAL      vinmsg = %trimr(vinmsg)
034700100118     C                             + ' ' + 'VABNSP VATNSP'
034800100118     C                   endif
034900090511     C* RMN
035000091210     C***                EVAL      PiStr=%trim(%subst(vindta:13:30))
035100100118     C                   EVAL      PiStr=%trim(%subst(vindta:47:24))
035200090511     C                   EXSR      CHKNUM
035300090511     C                   IF        PiInt=*on
035400090511     C                   Z-ADD     PiVal         VABRMN
035500090511     C                   ELSE
035600090511     C                   ADD       1             errore
035700090511     C                   EVAL      vinmsg = %trimr(vinmsg)
035800090511     C                             + ' ' + 'VABRMN'
035900090511     C                   ENDIF
036000130905     C* CTR
036100130905     C                   EVAL      PiStr=%trim(%subst(vindta:457:3))
036200130905     C                   EXSR      CHKNUM
036300130905     C                   IF        PiInt=*on
036400130905     C                   Z-ADD     PiVal         VABCTR
036500130905     C                   ELSE
036600130905     C                   ADD       1             errore
036700130905     C                   EVAL      vinmsg = %trimr(vinmsg)
036800130905     C                             + ' ' + 'VABCTR'
036900130905     C                   ENDIF
037000060307     C* CAS
037100090127     C                   IF        %trim(%subst(vindta:502:9))<>'000000.00' AND
037200090608     C                             %trim(%subst(vindta:502:9))<>*blanks     AND
037300090608     C                             %trim(%subst(vindta:502:9))<>*zeros
037400070102     C                   EVAL      VABVCA = 'EUR'
037500100702     C                   EVAL      VABTIC='OM'
037600060307     C                   EVAL      FlgCAS = '1'
037700070102     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
037800060307     C                   EXSR      CHKNUM
037900060307     C                   IF        PiNum=*on
038000060316     C                   Z-ADD     PiVal         VABCAS
038100060307     C                   ELSE
038200060307     C                   ADD       1             errore
038300060307     C                   EVAL      vinmsg = %trimr(vinmsg)
038400060307     C                             + ' ' + 'VABCAS'
038500060307     C                   ENDIF
038600070102     C                   ENDIF
038700060316     C* IAS
038800070719     C                   IF        %trim(%subst(vindta:544:10)) <> *blanks
038900070102     C                   EVAL      VABVAS = 'EUR'
039000070102     C                   EVAL      PiStr=%trim(%subst(vindta:544:10))
039100060316     C                   EXSR      CHKNUM
039200060316     C                   IF        PiNum=*on
039300060316     C                   Z-ADD     PiVal         VABIAS
039400060316     C                   ELSE
039500060316     C                   ADD       1             errore
039600060316     C                   EVAL      vinmsg = %trimr(vinmsg)
039700060316     C                             + ' ' + 'VABIAS'
039800070102     C                   ENDIF
039900060316     C                   ENDIF
040000010205     C*
040100010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
040200040802     C                   IF        FlgCAS <> '0'
040300070102     C                   IF        VABCBO = '1'
040400010205     C                   EVAL      VABCBO = '4'
040500010205     C                   ELSE
040600070102     C                   EVAL      VABCBO = '6'
040700070102     C                   ENDIF
040800010205     C                   ENDIF
040900020305     C*
041000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
041100011113     C                   EXSR      CHKIMPDIV
041200010202     C*
041300000801     C* Ebbene...
041400000801     C                   ADD       1             §CTRMO
041500010201     C                   IF        errore <> *zeros
041600000801     C                   ADD       1             §CTRNO
041700000801     C                   EVAL      vinflg = '2'
041800000801     C                   ELSE
041900010201     C                   ADD       1             §CTROKVB
042000000801     C                   ENDIF
042100000801     C*
042200000801     C                   ENDSR
042300070102     C*----------------------------------------------------*
042400070102     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
042500070102     C*----------------------------------------------------*
042600070102     C     EXEVATA       BEGSR
042700070102     C*
042800070102     C                   EXSR      INZVAR
042900070102     C                   EXSR      DEFCAM
043000070102     C*
043100070102     C                   EVAL      VATTRC='A'
043200090127     C                   EVAL      VATNOT = %trim(%subst(vindta:581:30))
043300070102     C                   exsr      wrivat                                       => scarico VAT
043400070102     C*
043500070102     C                   ENDSR
043600060307     C*----------------------------------------------------*
043700060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
043800060307     C*----------------------------------------------------*
043900060307     C     EXEVATB       BEGSR
044000060307     C*
044100060307     C                   EXSR      INZVAR
044200060307     C                   EXSR      DEFCAM
044300060307     C*
044400060307     C                   EVAL      VATTRC='B'
044500070102     C                   EVAL      VATNOT = %trim(%subst(vindta:376:15))
044600060307     C                   exsr      wrivat                                       => scarico VAT
044700060307     C*
044800060307     C                   ENDSR
044900070402     C*----------------------------------------------------*
045000070402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
045100070402     C*----------------------------------------------------*
045200070402     C     EXEVATE       BEGSR
045300070402     C*
045400070402     C                   EXSR      INZVAR
045500070402     C                   EXSR      DEFCAM
045600070402     C*
045700070402     C                   EVAL      VATTRC='E'
045800070402     C                   EVAL      VATNOT = %trim(%subst(vindta:554:25))
045900070402     C                   exsr      wrivat                                       => scarico VAT
046000070402     C*
046100070402     C                   ENDSR
046200010201     C*----------------------------------------------------*
046300040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
046400010201     C*----------------------------------------------------*
046500020305     C     WRIVAT        BEGSR
046600050628     C*
046700060223     C* Scrivo solo se valorizzato qualcosa
046800060223     C                   IF        VATNOT <> *blanks
046900040802     C                   WRITE     FIVAT000
047000060223     C                   ENDIF
047100010201     C*
047200010201     C                   ENDSR
047300010202     C*----------------------------------------------------*
047400021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
047500010202     C*----------------------------------------------------*
047600020305     C     PREVAT        BEGSR
047700010202     C*
047800021113     C* Compongo il nome del membro da dare al FIVATWWR
047900010202     C                   eval      parmbr = vlrhdl
048000010202     C                   movel     'M'           parmbr
048100050627     C                   eval      parccm = %subst(vlrKSC:2:7)
048200010202     C                   eval      paropz = '1'
048300010202     C* Effettuo la chiamata al CLLE preposto
048400040506     C                   call(e)   'TITVVTC'
048500010202     C                   parm                    parccm
048600010202     C                   parm                    parmbr
048700010202     C                   parm                    paropz
048800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048900010202     C                   if        %error
049000010202     C                   movel     '1'           chkcall
049100010202     C                   else
049200010202     C                   movel     '0'           chkcall
049300010202     C                   endif
049400010202     C*
049500010202     C                   ENDSR
049600000801     C*----------------------------------------------------*
049700000801     C*  CONTROLLO NUMERICITA' CAMPI
049800000801     C*----------------------------------------------------*
049900000801     C     CHKNUM        BEGSR
050000000801     C*
050100000801     C                   call(e)   'ISNUMERIC'
050200000801     C                   PARM                    PiStr            30
050300070102     C                   PARM      '.'           PiDecChr          1
050400000801     C                   PARM      *ZEROS        PiVal            30 9
050500000801     C                   PARM      '0'           PiInt             1
050600000801     C                   PARM      '0'           PiNum             1
050700000801     C                   IF        %error
050800000801     C                   EVAL      PiInt=*off
050900000801     C                   ENDIF
051000000801     C*
051100000801     C                   ENDSR
051200000801     C***
051300000801
051400011113
051500011113     C*----------------------------------------------------*
051600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
051700011113     C*----------------------------------------------------*
051800011113     C     CHKIMPDIV     BEGSR
051900011113     C*
052000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
052100011113     C                   Z-ADD     *zeros        wrkDec            9 9
052200011113     C*
052300011113     C* Come prima cosa effettuo considerazioni sulla divisa
052400011113     C                   IF        vabIAS > *zeros
052500011113     C                   IF        vabVAS <> 'EUR'
052600011113     C                   EVAL      vabVAS =  'ITL'
052700011113     C                   ENDIF
052800011113     C                   ENDIF
052900011113     C*
053000011113     C                   IF        vabCAS > *zeros
053100011113     C                   IF        vabVCA <> 'EUR'
053200011113     C                   EVAL      vabVCA =  'ITL'
053300011113     C                   ENDIF
053400011113     C                   ENDIF
053500011113     C*
053600011113     C                   IF        vabVMD > *zeros
053700020305     C                   IF        vabVAD <> 'EUR'
053800011113     C                   EVAL      vabVAD =  'ITL'
053900011113     C                   ENDIF
054000011113     C                   ENDIF
054100011113     C*
054200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
054300011113     C                   Z-ADD     vabIAS        wrkDec
054400011113     C                   IF        wrkDec > *zeros
054500011113     C                   IF        vabVAS = 'ITL'
054600011113     C                   EVAL      vabIAS = *zeros
054700011113     C                   ENDIF
054800011113     C                   ENDIF
054900011113     C*
055000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
055100011113     C                   Z-ADD     vabCAS        wrkDec
055200011113     C                   IF        wrkDec > *zeros
055300011113     C                   IF        vabVCA = 'ITL'
055400011113     C                   EVAL      vabCAS = *zeros
055500011113     C                   ENDIF
055600011113     C                   ENDIF
055700011113     C*
055800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
055900011113     C                   Z-ADD     vabVMD        wrkDec
056000011113     C                   IF        wrkDec > *zeros
056100011113     C                   IF        vabVAD = 'ITL'
056200011113     C                   EVAL      vabVMD = *zeros
056300011113     C                   ENDIF
056400011113     C                   ENDIF
056500011113     C*
056600011113     C                   ENDSR
056700011113     C***
056800011113
056900011113
057000000801
057100000801
057200990920      /TITLE Invio dei dati al punto operativo.
057300010202     C     invio         BEGSR
057400990920     C*
057500021113     C* 1° invio FIVAT
057600010201     C                   reset                   dscmz
057700010201     C                   move      vlrpoi        cmzdst
057800021113     C                   eval      cmzfld = 'FIVATWWR'
057900010201     C                   eval      cmzmbd = vlrhdl
058000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058100021009     C***                if        prmfir = *blanks
058200021113     C                   eval      cmzfla = 'FIVAT00F'
058300021113     C                   eval      cmzmba = 'FIVAT00F'
058400021009     C***                else
058500021009     C***                eval      cmzfla = prmfir
058600021009     C***                eval      cmzmba = prmfir
058700021009     C***                endif
058800010201     C                   eval      cmznrr = *zeros
058900020305     C                   move      §ctrokvt      cmznrr
059000021018     C                   eval      cmzlba = vlrfl1
059100010201     C                   call(e)   'TIS711C'
059200010201     C                   parm                    dscmz
059300010201     C                   parm      *blanks       esito
059400010205     C                   if        %error
059500010205     C                             or cmzerr = '1'
059600010205     C                             or esito  = '1'
059700010205     C                   eval      wrkesito = '3'
059800010205     C                   else
059900010201     C*
060000021113     C* 2° invio FIVAB
060100010201     C                   reset                   dscmz
060200010201     C                   move      vlrpoi        cmzdst
060300010201     C                   eval      cmzfld = vlrfou
060400010201     C                   eval      cmzmbd = vlrhdl
060500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060600021009     C***                if        prmfir = *blanks
060700021113     C                   eval      cmzfla = 'FIVAB00F'
060800021113     C                   eval      cmzmba = 'FIVAB00F'
060900021009     C***                else
061000021009     C***                eval      cmzfla = prmfir
061100021009     C***                eval      cmzmba = prmfir
061200021009     C***                endif
061300010201     C                   eval      cmznrr = *zeros
061400010201     C                   move      §ctrokvb      cmznrr
061500021018     C                   eval      cmzlba = vlrfl1
061600010201     C                   call(e)   'TIS711C'
061700010201     C                   parm                    dscmz
061800010201     C                   parm      *blanks       esito
061900010201     C                   if        %error
062000010201     C                             or cmzerr = '1'
062100010201     C                             or esito  = '1'
062200010201     C                   eval      wrkesito = '3'
062300010201     C                   endif
062400010205     C                   endif
062500990920     C*
062600000613     C                   ENDSR
062700000613     C***
062800990910
062900000613     C     *inzsr        BEGSR
063000990910     C*
063100990910     C     *entry        plist
063200990920     C                   parm                    tivlrds
063300990921     C                   parm      wrkesito      esito
063400000724     C                   parm                    prmlit
063500000710     C                   parm                    prmfir
063600000613     C*
063700000830     C* CALCOLA LA DATA CORRENTE
063800100118     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
063900100118     C                   eval      datcor = %dec(%date() : *ISO)
064000000830     C*
064100000613     C                   ENDSR
064200000613     C***
