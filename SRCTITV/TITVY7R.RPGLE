000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130325     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
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
002700000830
002800000830     D*------------------
002900000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003000000830     D*------------------
003100000830     D WLBDA8          DS                  INZ
003200000830     D  G08DAT                 1      8  0
003300000830     D  G08INV                 9     16  0
003400000830     D  G08ERR                17     17
003500000830     D  G08TGI                18     22  0
003600000830     D*
003700010201
003800010201
003900000913     C                   reset                   rrnum
004000990921     C                   reset                   esito
004100990921     C                   reset                   wrkesito
004200000613     C*
004300040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004400000613     C*
004500010202     C* Effettuo la chiamata al CLLE preposto
004600040506     C                   call(e)   'TITVVTC'
004700010202     C                   parm                    parccm
004800010202     C                   parm                    parmbr
004900010202     C                   parm      '2'           paropz
005000050201     C*
005100050201     C* Effettuo lancio TISI95 solo x chiusura
005200050201     C                   CLEAR                   TISI95DS
005300050201     C                   EVAL      I95TLA = 'C'
005400050201     C                   CALL      'TISI95R'
005500050201     C                   PARM                    TISI95DS
005600000616     C*
005700000801     C
005800010201     C                   seton                                        LR
005900990908
006000000801
006100910830     C*--------------------------------------------------------
006200040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
006300910830     C*--------------------------------------------------------
006400040526     C     RWFILE        BEGSR
006500990910     C*
006600990914     C                   if        not %open(tivin00r)
006700990908     C                   open      tivin00r
006800990914     C                   endif
006900021113     C                   if        not %open(fivabwwr)
007000021113     C                   open      fivabwwr
007100990914     C                   endif
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
010000070402     C*
010100070402     C* Verifico la rottura d codice x raggruppamento spedizione cliente
010200070402     C                   eval      curSped = %subst(vindta:46:25)
010300070402     C                   if        curSped <> depSped
010400070402     C                   clear                   fivab000
010500070402     C                   clear                   fivat000
010600050628     C*
010700060315     C                   exsr      impvab                                       => carico  VAB
010800060315     C                   exsr      wrivab                                       => scarico VAB
010900070402     C*
011000070402     C* Scrivo anche estensione dettaglio bolle - tipo record "A" => REFERENTE CONSEGNA
011100070402     C                   exsr      exevata
011200070402     C*
011300070402     C* Scrivo anche estensione dettaglio bolle - tipo record "B" => TELEFONO DESTINATARIO
011400070402     C                   exsr      exevatb
011500070402     C*
011600070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
011700070402     C                   exsr      exevate
011800070402     C*
011900070402     C* Salvo il raggruppamento spedizione cliente corrente
012000070402     C                   eval      depSped = curSped
012100070402     C*
012200070402     C                   else
012300070402     C*
012400070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
012500070402     C                   exsr      exevate
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
015400021113     C                   if        %open(fivabwwr)
015500021113     C                   close     fivabwwr
015600990914     C                   endif
015700021113     C                   if        %open(fivatwwr)
015800021113     C                   close     fivatwwr
015900010201     C                   endif
016000990910     C*
016100010201     C                   if        §ctrokvb > 0
016200000724     C                             and vlrpoi <> *zeros
016300010202     C                   exsr      invio
016400990920     C                   endif
016500990920     C*
016600910830     C                   ENDSR
016700000613     C***
016800010305
016900010305     C*----------------------------------------------------*
017000020305     C*  SCARICAMENTO BUFFER RECORDS VAB
017100010305     C*----------------------------------------------------*
017200020305     C     WRIVAB        BEGSR
017300010305     C*
017400060225     C* Quindi scarico il buffer del file d testata
017500021113     C                   write     fivab000                                     => scarico il VAB
017600010305     C*
017700010305     C                   ENDSR
017800990920
017900000801     C*----------------------------------------------------*
018000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
018100000801     C*----------------------------------------------------*
018200010201     C     INZVAR        BEGSR
018300000801     C*
018400040802     C                   Z-ADD     *zeros        Num5_0            5 0
018500040802     C                   MOVEL     '0'           FlgCAS            1
018600000801     C*
018700000801     C                   ENDSR
018800000801     C*----------------------------------------------------*
018900000801     C*  IMPOSTAZIONE CAMPI COSTANTI
019000000801     C*----------------------------------------------------*
019100000801     C     DEFCAM        BEGSR
019200000801     C*
019300020619     C* Imposto i valori di default...
019400070402     C                   Z-ADD     1180401       VABCCM
019500070402     C                   Z-ADD     1180401       VATCCM
019600070402     C                   Z-ADD     118           VABLNP
019700070402     C                   Z-ADD     118           VATLNP
019800040714     C                   Z-ADD     000           VABCTR
019900040823     C                   MOVEL     '1'           VABCBO
020000070402     C                   MOVEL     '7Q'          VABCTM
020100020619     C* ... e poi verifico se sono stati passati come parametri
020200020619     C                   IF        vlrppt > *blanks
020300040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
020400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
020500020619     C                   EXSR      CHKNUM
020600020619     C                   IF        PiInt=*on
020700020619     C                   Z-ADD     PiVal         VABCCM
020800020619     C                   Z-ADD     PiVal         VATCCM
020900020619     C                   ENDIF
021000040506     C                   ENDIF
021100040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
021200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
021300020619     C                   EXSR      CHKNUM
021400020619     C                   IF        PiInt=*on
021500020619     C                   Z-ADD     PiVal         VABLNP
021600020619     C                   Z-ADD     PiVal         VATLNP
021700040506     C                   ENDIF
021800020619     C                   ENDIF
021900040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
022000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
022100020619     C                   EXSR      CHKNUM
022200020619     C                   IF        PiInt=*on
022300020619     C                   Z-ADD     PiVal         VABCTR
022400040506     C                   ENDIF
022500020619     C                   ENDIF
022600020619     C                   ENDIF
022700000801     C*
022800000801     C                   ENDSR
022900000801     C*----------------------------------------------------*
023000021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
023100000801     C*----------------------------------------------------*
023200040823     C     IMPVAB        BEGSR
023300040823     C*
023400020305     C                   EXSR      INZVAR
023500020305     C                   EXSR      DEFCAM
023600010305     C*
023700000801     C                   Z-ADD     *zeros        errore            1 0
023800000830     C                   MOVEL     datcor        VABAAS
023900020305     C                   MOVEL     datcor        VATAAS
024000040526     C                   MOVE      datcor        VABMGS
024100040823     C                   MOVE(P)   vlrpoi        VABFGS
024200040823     C                   MOVE(P)   vlrpoi        VATFGS
024300050628     C*
024400070102     C                   EVAL      VABRSD=%trim(%subst(vindta:276:40))
024500020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
024600020117     C     '@':'A'       XLATE     VABRSD        VABRSD
024700020117     C* ==
024800070102     C                   EVAL      VABIND=%trim(%subst(vindta:336:5))+' '+
024900070102     C                                    %trim(%subst(vindta:341:30))+' '+
025000070102     C                                    %trim(%subst(vindta:371:5))
025100070102     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
025200070102     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
025300070102     C                   EVAL      VABRMA=%trim(%subst(vindta:46:25))
025400070102     C                   EVAL      VABNAS=%trim(%subst(vindta:581:30))
025500070102     C                   EVAL      VABNOT=%trim(%subst(vindta:514:30))
025600070102     C                   IF        %subst(vindta:43:1) = 'N'
025700070102     C                   EVAL      VABCBO='1'
025800070102     C                   ENDIF
025900070102     C                   IF        %subst(vindta:43:1) = 'P'
026000070102     C                   EVAL      VABCBO='2'
026100070102     C                   ENDIF
026200070102     C                   IF        %subst(vindta:511:3) = 'ABM'
026300070102     C                   EVAL      VABTIC='BM'
026400070102     C                   ENDIF
026500070102     C                   IF        %subst(vindta:511:3) = 'ABS'
026600070102     C                   EVAL      VABTIC='B'
026700070102     C                   ENDIF
026800070102     C                   IF        %subst(vindta:511:3) = 'CON'
026900070102     C                   EVAL      VABTIC=*blanks
027000070102     C                   ENDIF
027100070102     C                   IF        %subst(vindta:511:3) = 'ACM'
027200070102     C                   EVAL      VABTIC='CM'
027300070102     C                   ENDIF
027400050628     C* CAP
027500070102     C                   EVAL      PiStr=%trim(%subst(vindta:391:9))
027600010201     C                   EXSR      CHKNUM
027700010201     C                   IF        PiInt=*on
027800010201     C                   Z-ADD     PiVal         Num5_0
027900040506     C                   MOVEL(P)  Num5_0        VABCAD
028000010201     C                   ELSE
028100040506     C                   ADD       1             errore
028200010201     C                   EVAL      vinmsg = %trimr(vinmsg)
028300040506     C                             + ' ' + 'VABCAD'
028400010201     C                   ENDIF
028500040506     C* Reperisco la provincia dal CAP e dalla località
028600040526     C                   IF        VABCAD <> *blanks AND
028700040526     C                             VABPRD  = *blanks
028800040506     C                   CLEAR                   TISI95DS
028900040506     C                   EVAL      I95TCN = '3'
029000040506     C                   Z-ADD     datcor        I95DAT
029100040506     C                   EVAL      I95CAP = VABCAD
029200040506     C                   EVAL      I95LOC = VABLOD
029300050627     C                   EVAL      I95NAR = VABNZD
029400040506     C                   CALL      'TISI95R'
029500040506     C                   PARM                    TISI95DS
029600040506     C                   EVAL      VABPRD = O95PRV
029700040506     C                   ENDIF
029800060307     C* NCL
029900070102     C                   EVAL      PiStr=%trim(%subst(vindta:432:3))
030000060307     C                   EXSR      CHKNUM
030100060307     C                   IF        PiInt=*on
030200060307     C                   ADD       PiVal         VABNCL
030300060307     C                   ELSE
030400060307     C                   ADD       1             errore
030500060307     C                   EVAL      vinmsg = %trimr(vinmsg)
030600060307     C                             + ' ' + 'VABNCL'
030700060307     C                   ENDIF
030800040506     C* PKB
030900070102     C                   EVAL      PiStr=%trim(%subst(vindta:435:7))
031000010201     C                   EXSR      CHKNUM
031100010201     C                   IF        PiNum=*on
031200070102     C                   EVAL      PiVal=PiVal/1000                             da grammi a kg
031300060316     C                   Z-ADD     PiVal         VABPKB
031400010201     C                   ELSE
031500010201     C                   ADD       1             errore
031600010201     C                   EVAL      vinmsg = %trimr(vinmsg)
031700010201     C                             + ' ' + 'VABPKB'
031800010201     C                   ENDIF
031900060316     C* NSP/RMN
032000070102     C                   EVAL      PiStr=%trim(%subst(vindta:46:25))
032100050928     C                   EXSR      CHKNUM
032200050928     C                   IF        PiInt=*on
032300060316     C                   Z-ADD     PiVal         VABNSP
032400060316     C                   Z-ADD     PiVal         VATNSP
032500050928     C                   Z-ADD     PiVal         VABRMN
032600050928     C                   ELSE
032700050928     C                   ADD       1             errore
032800050928     C                   EVAL      vinmsg = %trimr(vinmsg)
032900060316     C                             + ' ' + 'VABNSP VATNSP VABRMN'
033000050928     C                   ENDIF
033100060307     C* CAS
033200070402     C                   IF        %trim(%subst(vindta:502:9)) <> '0'
033300070102     C                   EVAL      VABVCA = 'EUR'
033400060307     C                   EVAL      FlgCAS = '1'
033500070102     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
033600060307     C                   EXSR      CHKNUM
033700060307     C                   IF        PiNum=*on
033800060316     C                   Z-ADD     PiVal         VABCAS
033900060307     C                   ELSE
034000060307     C                   ADD       1             errore
034100060307     C                   EVAL      vinmsg = %trimr(vinmsg)
034200060307     C                             + ' ' + 'VABCAS'
034300060307     C                   ENDIF
034400070102     C                   ENDIF
034500060316     C* IAS
034600070402     C                   IF        %trim(%subst(vindta:544:10)) <> '0'
034700070102     C                   EVAL      VABVAS = 'EUR'
034800070102     C                   EVAL      PiStr=%trim(%subst(vindta:544:10))
034900060316     C                   EXSR      CHKNUM
035000060316     C                   IF        PiNum=*on
035100060316     C                   Z-ADD     PiVal         VABIAS
035200060316     C                   ELSE
035300060316     C                   ADD       1             errore
035400060316     C                   EVAL      vinmsg = %trimr(vinmsg)
035500060316     C                             + ' ' + 'VABIAS'
035600070102     C                   ENDIF
035700060316     C                   ENDIF
035800010205     C*
035900010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
036000040802     C                   IF        FlgCAS <> '0'
036100070102     C                   IF        VABCBO = '1'
036200010205     C                   EVAL      VABCBO = '4'
036300010205     C                   ELSE
036400070102     C                   EVAL      VABCBO = '6'
036500070102     C                   ENDIF
036600010205     C                   ENDIF
036700020305     C*
036800011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
036900011113     C                   EXSR      CHKIMPDIV
037000010202     C*
037100000801     C* Ebbene...
037200000801     C                   ADD       1             §CTRMO
037300010201     C                   IF        errore <> *zeros
037400000801     C                   ADD       1             §CTRNO
037500000801     C                   EVAL      vinflg = '2'
037600000801     C                   ELSE
037700010201     C                   ADD       1             §CTROKVB
037800000801     C                   ENDIF
037900000801     C*
038000000801     C                   ENDSR
038100070102     C*----------------------------------------------------*
038200070102     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
038300070102     C*----------------------------------------------------*
038400070102     C     EXEVATA       BEGSR
038500070102     C*
038600070102     C                   EXSR      INZVAR
038700070102     C                   EXSR      DEFCAM
038800070102     C*
038900070102     C                   EVAL      VATTRC='A'
039000070102     C                   EVAL      VATNOT = %trim(%subst(vindta:316:20))
039100070102     C                   exsr      wrivat                                       => scarico VAT
039200070102     C*
039300070102     C                   ENDSR
039400060307     C*----------------------------------------------------*
039500060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
039600060307     C*----------------------------------------------------*
039700060307     C     EXEVATB       BEGSR
039800060307     C*
039900060307     C                   EXSR      INZVAR
040000060307     C                   EXSR      DEFCAM
040100060307     C*
040200060307     C                   EVAL      VATTRC='B'
040300070102     C                   EVAL      VATNOT = %trim(%subst(vindta:376:15))
040400060307     C                   exsr      wrivat                                       => scarico VAT
040500060307     C*
040600060307     C                   ENDSR
040700070402     C*----------------------------------------------------*
040800070402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
040900070402     C*----------------------------------------------------*
041000070402     C     EXEVATE       BEGSR
041100070402     C*
041200070402     C                   EXSR      INZVAR
041300070402     C                   EXSR      DEFCAM
041400070402     C*
041500070402     C                   EVAL      VATTRC='E'
041600070402     C                   EVAL      VATNOT = %trim(%subst(vindta:554:25))
041700070402     C                   exsr      wrivat                                       => scarico VAT
041800070402     C*
041900070402     C                   ENDSR
042000010201     C*----------------------------------------------------*
042100040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
042200010201     C*----------------------------------------------------*
042300020305     C     WRIVAT        BEGSR
042400050628     C*
042500060223     C* Scrivo solo se valorizzato qualcosa
042600060223     C                   IF        VATNOT <> *blanks
042700040802     C                   WRITE     FIVAT000
042800060223     C                   ENDIF
042900010201     C*
043000010201     C                   ENDSR
043100010202     C*----------------------------------------------------*
043200021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043300010202     C*----------------------------------------------------*
043400020305     C     PREVAT        BEGSR
043500010202     C*
043600021113     C* Compongo il nome del membro da dare al FIVATWWR
043700010202     C                   eval      parmbr = vlrhdl
043800010202     C                   movel     'M'           parmbr
043900050627     C                   eval      parccm = %subst(vlrKSC:2:7)
044000010202     C                   eval      paropz = '1'
044100010202     C* Effettuo la chiamata al CLLE preposto
044200040506     C                   call(e)   'TITVVTC'
044300010202     C                   parm                    parccm
044400010202     C                   parm                    parmbr
044500010202     C                   parm                    paropz
044600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
044700010202     C                   if        %error
044800010202     C                   movel     '1'           chkcall
044900010202     C                   else
045000010202     C                   movel     '0'           chkcall
045100010202     C                   endif
045200010202     C*
045300010202     C                   ENDSR
045400000801     C*----------------------------------------------------*
045500000801     C*  CONTROLLO NUMERICITA' CAMPI
045600000801     C*----------------------------------------------------*
045700000801     C     CHKNUM        BEGSR
045800000801     C*
045900000801     C                   call(e)   'ISNUMERIC'
046000000801     C                   PARM                    PiStr            30
046100070102     C                   PARM      '.'           PiDecChr          1
046200000801     C                   PARM      *ZEROS        PiVal            30 9
046300000801     C                   PARM      '0'           PiInt             1
046400000801     C                   PARM      '0'           PiNum             1
046500000801     C                   IF        %error
046600000801     C                   EVAL      PiInt=*off
046700000801     C                   ENDIF
046800000801     C*
046900000801     C                   ENDSR
047000000801     C***
047100000801
047200011113
047300011113     C*----------------------------------------------------*
047400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
047500011113     C*----------------------------------------------------*
047600011113     C     CHKIMPDIV     BEGSR
047700011113     C*
047800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
047900011113     C                   Z-ADD     *zeros        wrkDec            9 9
048000011113     C*
048100011113     C* Come prima cosa effettuo considerazioni sulla divisa
048200011113     C                   IF        vabIAS > *zeros
048300011113     C                   IF        vabVAS <> 'EUR'
048400011113     C                   EVAL      vabVAS =  'ITL'
048500011113     C                   ENDIF
048600011113     C                   ENDIF
048700011113     C*
048800011113     C                   IF        vabCAS > *zeros
048900011113     C                   IF        vabVCA <> 'EUR'
049000011113     C                   EVAL      vabVCA =  'ITL'
049100011113     C                   ENDIF
049200011113     C                   ENDIF
049300011113     C*
049400011113     C                   IF        vabVMD > *zeros
049500020305     C                   IF        vabVAD <> 'EUR'
049600011113     C                   EVAL      vabVAD =  'ITL'
049700011113     C                   ENDIF
049800011113     C                   ENDIF
049900011113     C*
050000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
050100011113     C                   Z-ADD     vabIAS        wrkDec
050200011113     C                   IF        wrkDec > *zeros
050300011113     C                   IF        vabVAS = 'ITL'
050400011113     C                   EVAL      vabIAS = *zeros
050500011113     C                   ENDIF
050600011113     C                   ENDIF
050700011113     C*
050800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
050900011113     C                   Z-ADD     vabCAS        wrkDec
051000011113     C                   IF        wrkDec > *zeros
051100011113     C                   IF        vabVCA = 'ITL'
051200011113     C                   EVAL      vabCAS = *zeros
051300011113     C                   ENDIF
051400011113     C                   ENDIF
051500011113     C*
051600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
051700011113     C                   Z-ADD     vabVMD        wrkDec
051800011113     C                   IF        wrkDec > *zeros
051900011113     C                   IF        vabVAD = 'ITL'
052000011113     C                   EVAL      vabVMD = *zeros
052100011113     C                   ENDIF
052200011113     C                   ENDIF
052300011113     C*
052400011113     C                   ENDSR
052500011113     C***
052600011113
052700011113
052800000801
052900000801
053000990920      /TITLE Invio dei dati al punto operativo.
053100010202     C     invio         BEGSR
053200990920     C*
053300021113     C* 1° invio FIVAT
053400010201     C                   reset                   dscmz
053500010201     C                   move      vlrpoi        cmzdst
053600021113     C                   eval      cmzfld = 'FIVATWWR'
053700010201     C                   eval      cmzmbd = vlrhdl
053800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
053900021009     C***                if        prmfir = *blanks
054000021113     C                   eval      cmzfla = 'FIVAT00F'
054100021113     C                   eval      cmzmba = 'FIVAT00F'
054200021009     C***                else
054300021009     C***                eval      cmzfla = prmfir
054400021009     C***                eval      cmzmba = prmfir
054500021009     C***                endif
054600010201     C                   eval      cmznrr = *zeros
054700020305     C                   move      §ctrokvt      cmznrr
054800021018     C                   eval      cmzlba = vlrfl1
054900010201     C                   call(e)   'TIS711C'
055000010201     C                   parm                    dscmz
055100010201     C                   parm      *blanks       esito
055200010205     C                   if        %error
055300010205     C                             or cmzerr = '1'
055400010205     C                             or esito  = '1'
055500010205     C                   eval      wrkesito = '3'
055600010205     C                   else
055700010201     C*
055800021113     C* 2° invio FIVAB
055900010201     C                   reset                   dscmz
056000010201     C                   move      vlrpoi        cmzdst
056100010201     C                   eval      cmzfld = vlrfou
056200010201     C                   eval      cmzmbd = vlrhdl
056300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
056400021009     C***                if        prmfir = *blanks
056500021113     C                   eval      cmzfla = 'FIVAB00F'
056600021113     C                   eval      cmzmba = 'FIVAB00F'
056700021009     C***                else
056800021009     C***                eval      cmzfla = prmfir
056900021009     C***                eval      cmzmba = prmfir
057000021009     C***                endif
057100010201     C                   eval      cmznrr = *zeros
057200010201     C                   move      §ctrokvb      cmznrr
057300021018     C                   eval      cmzlba = vlrfl1
057400010201     C                   call(e)   'TIS711C'
057500010201     C                   parm                    dscmz
057600010201     C                   parm      *blanks       esito
057700010201     C                   if        %error
057800010201     C                             or cmzerr = '1'
057900010201     C                             or esito  = '1'
058000010201     C                   eval      wrkesito = '3'
058100010201     C                   endif
058200010205     C                   endif
058300990920     C*
058400000613     C                   ENDSR
058500000613     C***
058600990910
058700000613     C     *inzsr        BEGSR
058800990910     C*
058900990910     C     *entry        plist
059000990920     C                   parm                    tivlrds
059100990921     C                   parm      wrkesito      esito
059200000724     C                   parm                    prmlit
059300000710     C                   parm                    prmfir
059400000613     C*
059500000830     C* CALCOLA LA DATA CORRENTE
059600000830     C                   time                    wn14             14 0
059700000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
059800000830     C                   z-add     wn8           g08dat
059900000830     C                   z-add     *zeros        g08inv
060000000830     C                   movel     '0'           g08err
060100000830     C                   call      'XSRDA8'
060200000830     C                   parm                    wlbda8
060300000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
060400000830     C*
060500000613     C                   ENDSR
060600000613     C***
