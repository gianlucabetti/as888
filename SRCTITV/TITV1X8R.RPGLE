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
032100100118     C* NSP => Stacco un numeratore da AZNUM
032200100118     C                   clear                   TRUL33DS
032300100118     C                   eval      I33OPE = *zeros
032400100118     C                   eval      I33CNU = 302
032500100118     C                   eval      I33NUM = 1
032600100118     C                   movel     TRUL33DS      KPJBU
032700100118     C                   call      'TRUL33R'
032800100118     C                   parm                    KPJBA
032900100118     C                   movel     KPJBU         TRUL33DS
033000100118     C                   if        O33ERR = *zeros
033100100118     C                   z-add     O33NRF        VABNSP
033200100118     C                   z-add     O33NRF        VATNSP
033300100118     C                   else
033400100118     C                   ADD       1             errore
033500100118     C                   EVAL      vinmsg = %trimr(vinmsg)
033600100118     C                             + ' ' + 'VABNSP VATNSP'
033700100118     C                   endif
033800090511     C* RMN
033900091210     C***                EVAL      PiStr=%trim(%subst(vindta:13:30))
034000100118     C                   EVAL      PiStr=%trim(%subst(vindta:47:24))
034100090511     C                   EXSR      CHKNUM
034200090511     C                   IF        PiInt=*on
034300090511     C                   Z-ADD     PiVal         VABRMN
034400090511     C                   ELSE
034500090511     C                   ADD       1             errore
034600090511     C                   EVAL      vinmsg = %trimr(vinmsg)
034700090511     C                             + ' ' + 'VABRMN'
034800090511     C                   ENDIF
034900130905     C* CTR
035000130905     C                   EVAL      PiStr=%trim(%subst(vindta:457:3))
035100130905     C                   EXSR      CHKNUM
035200130905     C                   IF        PiInt=*on
035300130905     C                   Z-ADD     PiVal         VABCTR
035400130905     C                   ELSE
035500130905     C                   ADD       1             errore
035600130905     C                   EVAL      vinmsg = %trimr(vinmsg)
035700130905     C                             + ' ' + 'VABCTR'
035800130905     C                   ENDIF
035900060307     C* CAS
036000090127     C                   IF        %trim(%subst(vindta:502:9))<>'000000.00' AND
036100090608     C                             %trim(%subst(vindta:502:9))<>*blanks     AND
036200090608     C                             %trim(%subst(vindta:502:9))<>*zeros
036300070102     C                   EVAL      VABVCA = 'EUR'
036400100702     C                   EVAL      VABTIC='OM'
036500060307     C                   EVAL      FlgCAS = '1'
036600070102     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
036700060307     C                   EXSR      CHKNUM
036800060307     C                   IF        PiNum=*on
036900060316     C                   Z-ADD     PiVal         VABCAS
037000060307     C                   ELSE
037100060307     C                   ADD       1             errore
037200060307     C                   EVAL      vinmsg = %trimr(vinmsg)
037300060307     C                             + ' ' + 'VABCAS'
037400060307     C                   ENDIF
037500070102     C                   ENDIF
037600060316     C* IAS
037700070719     C                   IF        %trim(%subst(vindta:544:10)) <> *blanks
037800070102     C                   EVAL      VABVAS = 'EUR'
037900070102     C                   EVAL      PiStr=%trim(%subst(vindta:544:10))
038000060316     C                   EXSR      CHKNUM
038100060316     C                   IF        PiNum=*on
038200060316     C                   Z-ADD     PiVal         VABIAS
038300060316     C                   ELSE
038400060316     C                   ADD       1             errore
038500060316     C                   EVAL      vinmsg = %trimr(vinmsg)
038600060316     C                             + ' ' + 'VABIAS'
038700070102     C                   ENDIF
038800060316     C                   ENDIF
038900010205     C*
039000010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
039100040802     C                   IF        FlgCAS <> '0'
039200070102     C                   IF        VABCBO = '1'
039300010205     C                   EVAL      VABCBO = '4'
039400010205     C                   ELSE
039500070102     C                   EVAL      VABCBO = '6'
039600070102     C                   ENDIF
039700010205     C                   ENDIF
039800020305     C*
039900011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
040000011113     C                   EXSR      CHKIMPDIV
040100010202     C*
040200000801     C* Ebbene...
040300000801     C                   ADD       1             §CTRMO
040400010201     C                   IF        errore <> *zeros
040500000801     C                   ADD       1             §CTRNO
040600000801     C                   EVAL      vinflg = '2'
040700000801     C                   ELSE
040800010201     C                   ADD       1             §CTROKVB
040900000801     C                   ENDIF
041000000801     C*
041100000801     C                   ENDSR
041200070102     C*----------------------------------------------------*
041300070102     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
041400070102     C*----------------------------------------------------*
041500070102     C     EXEVATA       BEGSR
041600070102     C*
041700070102     C                   EXSR      INZVAR
041800070102     C                   EXSR      DEFCAM
041900070102     C*
042000070102     C                   EVAL      VATTRC='A'
042100090127     C                   EVAL      VATNOT = %trim(%subst(vindta:581:30))
042200070102     C                   exsr      wrivat                                       => scarico VAT
042300070102     C*
042400070102     C                   ENDSR
042500060307     C*----------------------------------------------------*
042600060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
042700060307     C*----------------------------------------------------*
042800060307     C     EXEVATB       BEGSR
042900060307     C*
043000060307     C                   EXSR      INZVAR
043100060307     C                   EXSR      DEFCAM
043200060307     C*
043300060307     C                   EVAL      VATTRC='B'
043400070102     C                   EVAL      VATNOT = %trim(%subst(vindta:376:15))
043500060307     C                   exsr      wrivat                                       => scarico VAT
043600060307     C*
043700060307     C                   ENDSR
043800070402     C*----------------------------------------------------*
043900070402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
044000070402     C*----------------------------------------------------*
044100070402     C     EXEVATE       BEGSR
044200070402     C*
044300070402     C                   EXSR      INZVAR
044400070402     C                   EXSR      DEFCAM
044500070402     C*
044600070402     C                   EVAL      VATTRC='E'
044700070402     C                   EVAL      VATNOT = %trim(%subst(vindta:554:25))
044800070402     C                   exsr      wrivat                                       => scarico VAT
044900070402     C*
045000070402     C                   ENDSR
045100010201     C*----------------------------------------------------*
045200040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
045300010201     C*----------------------------------------------------*
045400020305     C     WRIVAT        BEGSR
045500050628     C*
045600060223     C* Scrivo solo se valorizzato qualcosa
045700060223     C                   IF        VATNOT <> *blanks
045800040802     C                   WRITE     FIVAT000
045900060223     C                   ENDIF
046000010201     C*
046100010201     C                   ENDSR
046200010202     C*----------------------------------------------------*
046300021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
046400010202     C*----------------------------------------------------*
046500020305     C     PREVAT        BEGSR
046600010202     C*
046700021113     C* Compongo il nome del membro da dare al FIVATWWR
046800010202     C                   eval      parmbr = vlrhdl
046900010202     C                   movel     'M'           parmbr
047000050627     C                   eval      parccm = %subst(vlrKSC:2:7)
047100010202     C                   eval      paropz = '1'
047200010202     C* Effettuo la chiamata al CLLE preposto
047300040506     C                   call(e)   'TITVVTC'
047400010202     C                   parm                    parccm
047500010202     C                   parm                    parmbr
047600010202     C                   parm                    paropz
047700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047800010202     C                   if        %error
047900010202     C                   movel     '1'           chkcall
048000010202     C                   else
048100010202     C                   movel     '0'           chkcall
048200010202     C                   endif
048300010202     C*
048400010202     C                   ENDSR
048500000801     C*----------------------------------------------------*
048600000801     C*  CONTROLLO NUMERICITA' CAMPI
048700000801     C*----------------------------------------------------*
048800000801     C     CHKNUM        BEGSR
048900000801     C*
049000000801     C                   call(e)   'ISNUMERIC'
049100000801     C                   PARM                    PiStr            30
049200070102     C                   PARM      '.'           PiDecChr          1
049300000801     C                   PARM      *ZEROS        PiVal            30 9
049400000801     C                   PARM      '0'           PiInt             1
049500000801     C                   PARM      '0'           PiNum             1
049600000801     C                   IF        %error
049700000801     C                   EVAL      PiInt=*off
049800000801     C                   ENDIF
049900000801     C*
050000000801     C                   ENDSR
050100000801     C***
050200000801
050300011113
050400011113     C*----------------------------------------------------*
050500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
050600011113     C*----------------------------------------------------*
050700011113     C     CHKIMPDIV     BEGSR
050800011113     C*
050900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
051000011113     C                   Z-ADD     *zeros        wrkDec            9 9
051100011113     C*
051200011113     C* Come prima cosa effettuo considerazioni sulla divisa
051300011113     C                   IF        vabIAS > *zeros
051400011113     C                   IF        vabVAS <> 'EUR'
051500011113     C                   EVAL      vabVAS =  'ITL'
051600011113     C                   ENDIF
051700011113     C                   ENDIF
051800011113     C*
051900011113     C                   IF        vabCAS > *zeros
052000011113     C                   IF        vabVCA <> 'EUR'
052100011113     C                   EVAL      vabVCA =  'ITL'
052200011113     C                   ENDIF
052300011113     C                   ENDIF
052400011113     C*
052500011113     C                   IF        vabVMD > *zeros
052600020305     C                   IF        vabVAD <> 'EUR'
052700011113     C                   EVAL      vabVAD =  'ITL'
052800011113     C                   ENDIF
052900011113     C                   ENDIF
053000011113     C*
053100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
053200011113     C                   Z-ADD     vabIAS        wrkDec
053300011113     C                   IF        wrkDec > *zeros
053400011113     C                   IF        vabVAS = 'ITL'
053500011113     C                   EVAL      vabIAS = *zeros
053600011113     C                   ENDIF
053700011113     C                   ENDIF
053800011113     C*
053900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
054000011113     C                   Z-ADD     vabCAS        wrkDec
054100011113     C                   IF        wrkDec > *zeros
054200011113     C                   IF        vabVCA = 'ITL'
054300011113     C                   EVAL      vabCAS = *zeros
054400011113     C                   ENDIF
054500011113     C                   ENDIF
054600011113     C*
054700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
054800011113     C                   Z-ADD     vabVMD        wrkDec
054900011113     C                   IF        wrkDec > *zeros
055000011113     C                   IF        vabVAD = 'ITL'
055100011113     C                   EVAL      vabVMD = *zeros
055200011113     C                   ENDIF
055300011113     C                   ENDIF
055400011113     C*
055500011113     C                   ENDSR
055600011113     C***
055700011113
055800011113
055900000801
056000000801
056100990920      /TITLE Invio dei dati al punto operativo.
056200010202     C     invio         BEGSR
056300990920     C*
056400021113     C* 1° invio FIVAT
056500010201     C                   reset                   dscmz
056600010201     C                   move      vlrpoi        cmzdst
056700021113     C                   eval      cmzfld = 'FIVATWWR'
056800010201     C                   eval      cmzmbd = vlrhdl
056900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057000021009     C***                if        prmfir = *blanks
057100021113     C                   eval      cmzfla = 'FIVAT00F'
057200021113     C                   eval      cmzmba = 'FIVAT00F'
057300021009     C***                else
057400021009     C***                eval      cmzfla = prmfir
057500021009     C***                eval      cmzmba = prmfir
057600021009     C***                endif
057700010201     C                   eval      cmznrr = *zeros
057800020305     C                   move      §ctrokvt      cmznrr
057900021018     C                   eval      cmzlba = vlrfl1
058000010201     C                   call(e)   'TIS711C'
058100010201     C                   parm                    dscmz
058200010201     C                   parm      *blanks       esito
058300010205     C                   if        %error
058400010205     C                             or cmzerr = '1'
058500010205     C                             or esito  = '1'
058600010205     C                   eval      wrkesito = '3'
058700010205     C                   else
058800010201     C*
058900021113     C* 2° invio FIVAB
059000010201     C                   reset                   dscmz
059100010201     C                   move      vlrpoi        cmzdst
059200010201     C                   eval      cmzfld = vlrfou
059300010201     C                   eval      cmzmbd = vlrhdl
059400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059500021009     C***                if        prmfir = *blanks
059600021113     C                   eval      cmzfla = 'FIVAB00F'
059700021113     C                   eval      cmzmba = 'FIVAB00F'
059800021009     C***                else
059900021009     C***                eval      cmzfla = prmfir
060000021009     C***                eval      cmzmba = prmfir
060100021009     C***                endif
060200010201     C                   eval      cmznrr = *zeros
060300010201     C                   move      §ctrokvb      cmznrr
060400021018     C                   eval      cmzlba = vlrfl1
060500010201     C                   call(e)   'TIS711C'
060600010201     C                   parm                    dscmz
060700010201     C                   parm      *blanks       esito
060800010201     C                   if        %error
060900010201     C                             or cmzerr = '1'
061000010201     C                             or esito  = '1'
061100010201     C                   eval      wrkesito = '3'
061200010201     C                   endif
061300010205     C                   endif
061400990920     C*
061500000613     C                   ENDSR
061600000613     C***
061700990910
061800000613     C     *inzsr        BEGSR
061900990910     C*
062000990910     C     *entry        plist
062100990920     C                   parm                    tivlrds
062200990921     C                   parm      wrkesito      esito
062300000724     C                   parm                    prmlit
062400000710     C                   parm                    prmfir
062500000613     C*
062600000830     C* CALCOLA LA DATA CORRENTE
062700100118     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
062800100118     C                   eval      datcor = %dec(%date() : *ISO)
062900000830     C*
063000000613     C                   ENDSR
063100000613     C***
