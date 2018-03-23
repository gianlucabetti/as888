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
019600100722     C                   Z-ADD     0286185       VABCCM
019700100722     C                   Z-ADD     0286185       VATCCM
019800100722     C                   Z-ADD     028           VABLNP
019900100722     C                   Z-ADD     028           VATLNP
020000100722     C                   Z-ADD     300           VABCTR
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
025600100913     C                   EVAL      VABRMA=%trim(%subst(vindta:46:10))
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
027600100622     C                   IF        %subst(vindta:511:3) = 'ACS'
027700100622     C                   EVAL      VABTIC='CB'
027800100622     C                   ENDIF
027900100622     C                   IF        %subst(vindta:511:3) = 'VAR'
028000100622     C                   EVAL      VABTIC=*blanks
028100100622     C                   ENDIF
028200150611     C                   IF        %subst(vindta:660:3) <> *blank and
028300150611     C                             %trim(%subst(vindta:660:3))='E'
028400150611     C                   EVAL      VABTSP='E'
028500150611     C                   ENDIF
028600050628     C* CAP
028700070719     C                   EVAL      PiStr=%trim(%subst(vindta:391:5))
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
031100070102     C                   EVAL      PiStr=%trim(%subst(vindta:432:3))
031200060307     C                   EXSR      CHKNUM
031300060307     C                   IF        PiInt=*on
031400060307     C                   ADD       PiVal         VABNCL
031500060307     C                   ELSE
031600060307     C                   ADD       1             errore
031700060307     C                   EVAL      vinmsg = %trimr(vinmsg)
031800060307     C                             + ' ' + 'VABNCL'
031900060307     C                   ENDIF
032000040506     C* PKB
032100070102     C                   EVAL      PiStr=%trim(%subst(vindta:435:7))
032200010201     C                   EXSR      CHKNUM
032300010201     C                   IF        PiNum=*on
032400070102     C                   EVAL      PiVal=PiVal/1000                             da grammi a kg
032500060316     C                   Z-ADD     PiVal         VABPKB
032600010201     C                   ELSE
032700010201     C                   ADD       1             errore
032800010201     C                   EVAL      vinmsg = %trimr(vinmsg)
032900010201     C                             + ' ' + 'VABPKB'
033000010201     C                   ENDIF
033100100118     C* NSP => Stacco un numeratore da AZNUM
033200100118     C                   clear                   TRUL33DS
033300100118     C                   eval      I33OPE = *zeros
033400100118     C                   eval      I33CNU = 302
033500100118     C                   eval      I33NUM = 1
033600100118     C                   movel     TRUL33DS      KPJBU
033700100118     C                   call      'TRUL33R'
033800100118     C                   parm                    KPJBA
033900100118     C                   movel     KPJBU         TRUL33DS
034000100118     C                   if        O33ERR = *zeros
034100100118     C                   z-add     O33NRF        VABNSP
034200100118     C                   z-add     O33NRF        VATNSP
034300100118     C                   else
034400100118     C                   ADD       1             errore
034500100118     C                   EVAL      vinmsg = %trimr(vinmsg)
034600100118     C                             + ' ' + 'VABNSP VATNSP'
034700100118     C                   endif
034800090511     C* RMN
034900100913     C***                EVAL      VABRMN = VABNSP
035000100913     C                   EVAL      PiStr=%trim(%subst(vindta:46+10:15))
035100091210     C***                EVAL      PiStr=%trim(%subst(vindta:13:30))
035200100726     C***                EVAL      PiStr=%trim(%subst(vindta:47:24))
035300100913     C                   EXSR      CHKNUM
035400100913     C                   IF        PiInt=*on
035500100913     C                   Z-ADD     PiVal         VABRMN
035600100913     C                   ELSE
035700100913     C                   ADD       1             errore
035800100913     C                   EVAL      vinmsg = %trimr(vinmsg)
035900100913     C                             + ' ' + 'VABRMN'
036000100913     C                   ENDIF
036100060307     C* CAS
036200090127     C                   IF        %trim(%subst(vindta:502:9))<>'000000.00' AND
036300090608     C                             %trim(%subst(vindta:502:9))<>*blanks     AND
036400090608     C                             %trim(%subst(vindta:502:9))<>*zeros
036500070102     C                   EVAL      VABVCA = 'EUR'
036600060307     C                   EVAL      FlgCAS = '1'
036700070102     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
036800060307     C                   EXSR      CHKNUM
036900060307     C                   IF        PiNum=*on
037000060316     C                   Z-ADD     PiVal         VABCAS
037100060307     C                   ELSE
037200060307     C                   ADD       1             errore
037300060307     C                   EVAL      vinmsg = %trimr(vinmsg)
037400060307     C                             + ' ' + 'VABCAS'
037500060307     C                   ENDIF
037600070102     C                   ENDIF
037700060316     C* IAS
037800070719     C                   IF        %trim(%subst(vindta:544:10)) <> *blanks
037900070102     C                   EVAL      VABVAS = 'EUR'
038000070102     C                   EVAL      PiStr=%trim(%subst(vindta:544:10))
038100060316     C                   EXSR      CHKNUM
038200060316     C                   IF        PiNum=*on
038300060316     C                   Z-ADD     PiVal         VABIAS
038400060316     C                   ELSE
038500060316     C                   ADD       1             errore
038600060316     C                   EVAL      vinmsg = %trimr(vinmsg)
038700060316     C                             + ' ' + 'VABIAS'
038800070102     C                   ENDIF
038900060316     C                   ENDIF
039000010205     C*
039100010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
039200040802     C                   IF        FlgCAS <> '0'
039300070102     C                   IF        VABCBO = '1'
039400010205     C                   EVAL      VABCBO = '4'
039500010205     C                   ELSE
039600070102     C                   EVAL      VABCBO = '6'
039700070102     C                   ENDIF
039800010205     C                   ENDIF
039900020305     C*
040000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
040100011113     C                   EXSR      CHKIMPDIV
040200010202     C*
040300000801     C* Ebbene...
040400000801     C                   ADD       1             §CTRMO
040500010201     C                   IF        errore <> *zeros
040600000801     C                   ADD       1             §CTRNO
040700000801     C                   EVAL      vinflg = '2'
040800000801     C                   ELSE
040900010201     C                   ADD       1             §CTROKVB
041000000801     C                   ENDIF
041100000801     C*
041200000801     C                   ENDSR
041300070102     C*----------------------------------------------------*
041400070102     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
041500070102     C*----------------------------------------------------*
041600070102     C     EXEVATA       BEGSR
041700070102     C*
041800070102     C                   EXSR      INZVAR
041900070102     C                   EXSR      DEFCAM
042000070102     C*
042100100722     C***                EVAL      VATTRC='A'
042200100722     C***                EVAL      VATNOT = %trim(%subst(vindta:581:30))
042300100722     C***                exsr      wrivat                                       => scarico VAT
042400070102     C*
042500070102     C                   ENDSR
042600060307     C*----------------------------------------------------*
042700060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
042800060307     C*----------------------------------------------------*
042900060307     C     EXEVATB       BEGSR
043000060307     C*
043100060307     C                   EXSR      INZVAR
043200060307     C                   EXSR      DEFCAM
043300060307     C*
043400060307     C                   EVAL      VATTRC='B'
043500070102     C                   EVAL      VATNOT = %trim(%subst(vindta:376:15))
043600060307     C                   exsr      wrivat                                       => scarico VAT
043700060307     C*
043800060307     C                   ENDSR
043900070402     C*----------------------------------------------------*
044000070402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
044100070402     C*----------------------------------------------------*
044200070402     C     EXEVATE       BEGSR
044300070402     C*
044400070402     C                   EXSR      INZVAR
044500070402     C                   EXSR      DEFCAM
044600070402     C*
044700070402     C                   EVAL      VATTRC='E'
044800070402     C                   EVAL      VATNOT = %trim(%subst(vindta:554:25))
044900070402     C                   exsr      wrivat                                       => scarico VAT
045000070402     C*
045100070402     C                   ENDSR
045200010201     C*----------------------------------------------------*
045300040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
045400010201     C*----------------------------------------------------*
045500020305     C     WRIVAT        BEGSR
045600050628     C*
045700060223     C* Scrivo solo se valorizzato qualcosa
045800060223     C                   IF        VATNOT <> *blanks
045900040802     C                   WRITE     FIVAT000
046000060223     C                   ENDIF
046100010201     C*
046200010201     C                   ENDSR
046300010202     C*----------------------------------------------------*
046400021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
046500010202     C*----------------------------------------------------*
046600020305     C     PREVAT        BEGSR
046700010202     C*
046800021113     C* Compongo il nome del membro da dare al FIVATWWR
046900010202     C                   eval      parmbr = vlrhdl
047000010202     C                   movel     'M'           parmbr
047100050627     C                   eval      parccm = %subst(vlrKSC:2:7)
047200010202     C                   eval      paropz = '1'
047300010202     C* Effettuo la chiamata al CLLE preposto
047400040506     C                   call(e)   'TITVVTC'
047500010202     C                   parm                    parccm
047600010202     C                   parm                    parmbr
047700010202     C                   parm                    paropz
047800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047900010202     C                   if        %error
048000010202     C                   movel     '1'           chkcall
048100010202     C                   else
048200010202     C                   movel     '0'           chkcall
048300010202     C                   endif
048400010202     C*
048500010202     C                   ENDSR
048600000801     C*----------------------------------------------------*
048700000801     C*  CONTROLLO NUMERICITA' CAMPI
048800000801     C*----------------------------------------------------*
048900000801     C     CHKNUM        BEGSR
049000000801     C*
049100000801     C                   call(e)   'ISNUMERIC'
049200000801     C                   PARM                    PiStr            30
049300070102     C                   PARM      '.'           PiDecChr          1
049400000801     C                   PARM      *ZEROS        PiVal            30 9
049500000801     C                   PARM      '0'           PiInt             1
049600000801     C                   PARM      '0'           PiNum             1
049700000801     C                   IF        %error
049800000801     C                   EVAL      PiInt=*off
049900000801     C                   ENDIF
050000000801     C*
050100000801     C                   ENDSR
050200000801     C***
050300000801
050400011113
050500011113     C*----------------------------------------------------*
050600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
050700011113     C*----------------------------------------------------*
050800011113     C     CHKIMPDIV     BEGSR
050900011113     C*
051000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
051100011113     C                   Z-ADD     *zeros        wrkDec            9 9
051200011113     C*
051300011113     C* Come prima cosa effettuo considerazioni sulla divisa
051400011113     C                   IF        vabIAS > *zeros
051500011113     C                   IF        vabVAS <> 'EUR'
051600011113     C                   EVAL      vabVAS =  'ITL'
051700011113     C                   ENDIF
051800011113     C                   ENDIF
051900011113     C*
052000011113     C                   IF        vabCAS > *zeros
052100011113     C                   IF        vabVCA <> 'EUR'
052200011113     C                   EVAL      vabVCA =  'ITL'
052300011113     C                   ENDIF
052400011113     C                   ENDIF
052500011113     C*
052600011113     C                   IF        vabVMD > *zeros
052700020305     C                   IF        vabVAD <> 'EUR'
052800011113     C                   EVAL      vabVAD =  'ITL'
052900011113     C                   ENDIF
053000011113     C                   ENDIF
053100011113     C*
053200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
053300011113     C                   Z-ADD     vabIAS        wrkDec
053400011113     C                   IF        wrkDec > *zeros
053500011113     C                   IF        vabVAS = 'ITL'
053600011113     C                   EVAL      vabIAS = *zeros
053700011113     C                   ENDIF
053800011113     C                   ENDIF
053900011113     C*
054000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
054100011113     C                   Z-ADD     vabCAS        wrkDec
054200011113     C                   IF        wrkDec > *zeros
054300011113     C                   IF        vabVCA = 'ITL'
054400011113     C                   EVAL      vabCAS = *zeros
054500011113     C                   ENDIF
054600011113     C                   ENDIF
054700011113     C*
054800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
054900011113     C                   Z-ADD     vabVMD        wrkDec
055000011113     C                   IF        wrkDec > *zeros
055100011113     C                   IF        vabVAD = 'ITL'
055200011113     C                   EVAL      vabVMD = *zeros
055300011113     C                   ENDIF
055400011113     C                   ENDIF
055500011113     C*
055600011113     C                   ENDSR
055700011113     C***
055800011113
055900011113
056000000801
056100000801
056200990920      /TITLE Invio dei dati al punto operativo.
056300010202     C     invio         BEGSR
056400990920     C*
056500021113     C* 1° invio FIVAT
056600010201     C                   reset                   dscmz
056700010201     C                   move      vlrpoi        cmzdst
056800021113     C                   eval      cmzfld = 'FIVATWWR'
056900010201     C                   eval      cmzmbd = vlrhdl
057000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057100021009     C***                if        prmfir = *blanks
057200021113     C                   eval      cmzfla = 'FIVAT00F'
057300021113     C                   eval      cmzmba = 'FIVAT00F'
057400021009     C***                else
057500021009     C***                eval      cmzfla = prmfir
057600021009     C***                eval      cmzmba = prmfir
057700021009     C***                endif
057800010201     C                   eval      cmznrr = *zeros
057900020305     C                   move      §ctrokvt      cmznrr
058000021018     C                   eval      cmzlba = vlrfl1
058100010201     C                   call(e)   'TIS711C'
058200010201     C                   parm                    dscmz
058300010201     C                   parm      *blanks       esito
058400010205     C                   if        %error
058500010205     C                             or cmzerr = '1'
058600010205     C                             or esito  = '1'
058700010205     C                   eval      wrkesito = '3'
058800010205     C                   else
058900010201     C*
059000021113     C* 2° invio FIVAB
059100010201     C                   reset                   dscmz
059200010201     C                   move      vlrpoi        cmzdst
059300010201     C                   eval      cmzfld = vlrfou
059400010201     C                   eval      cmzmbd = vlrhdl
059500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059600021009     C***                if        prmfir = *blanks
059700021113     C                   eval      cmzfla = 'FIVAB00F'
059800021113     C                   eval      cmzmba = 'FIVAB00F'
059900021009     C***                else
060000021009     C***                eval      cmzfla = prmfir
060100021009     C***                eval      cmzmba = prmfir
060200021009     C***                endif
060300010201     C                   eval      cmznrr = *zeros
060400010201     C                   move      §ctrokvb      cmznrr
060500021018     C                   eval      cmzlba = vlrfl1
060600010201     C                   call(e)   'TIS711C'
060700010201     C                   parm                    dscmz
060800010201     C                   parm      *blanks       esito
060900010201     C                   if        %error
061000010201     C                             or cmzerr = '1'
061100010201     C                             or esito  = '1'
061200010201     C                   eval      wrkesito = '3'
061300010201     C                   endif
061400010205     C                   endif
061500990920     C*
061600000613     C                   ENDSR
061700000613     C***
061800990910
061900000613     C     *inzsr        BEGSR
062000990910     C*
062100990910     C     *entry        plist
062200990920     C                   parm                    tivlrds
062300990921     C                   parm      wrkesito      esito
062400000724     C                   parm                    prmlit
062500000710     C                   parm                    prmfir
062600000613     C*
062700000830     C* CALCOLA LA DATA CORRENTE
062800100118     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
062900100118     C                   eval      datcor = %dec(%date() : *ISO)
063000000830     C*
063100000613     C                   ENDSR
063200000613     C***
