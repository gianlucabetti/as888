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
002500000830
002600000830     D*------------------
002700000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
002800000830     D*------------------
002900000830     D WLBDA8          DS                  INZ
003000000830     D  G08DAT                 1      8  0
003100000830     D  G08INV                 9     16  0
003200000830     D  G08ERR                17     17
003300000830     D  G08TGI                18     22  0
003400000830     D*
003500010201
003600010201
003700000913     C                   reset                   rrnum
003800990921     C                   reset                   esito
003900990921     C                   reset                   wrkesito
004000000613     C*
004100040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004200000613     C*
004300010202     C* Effettuo la chiamata al CLLE preposto
004400040506     C                   call(e)   'TITVVTC'
004500010202     C                   parm                    parccm
004600010202     C                   parm                    parmbr
004700010202     C                   parm      '2'           paropz
004800050201     C*
004900050201     C* Effettuo lancio TISI95 solo x chiusura
005000050201     C                   CLEAR                   TISI95DS
005100050201     C                   EVAL      I95TLA = 'C'
005200050201     C                   CALL      'TISI95R'
005300050201     C                   PARM                    TISI95DS
005400000616     C*
005500000801     C
005600010201     C                   seton                                        LR
005700990908
005800000801
005900910830     C*--------------------------------------------------------
006000040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
006100910830     C*--------------------------------------------------------
006200040526     C     RWFILE        BEGSR
006300990910     C*
006400990914     C                   if        not %open(tivin00r)
006500990908     C                   open      tivin00r
006600990914     C                   endif
006700021113     C                   if        not %open(fivabwwr)
006800021113     C                   open      fivabwwr
006900990914     C                   endif
007000021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007100020305     C                   exsr      prevat
007200010201     C*
007300010202     C                   if        chkcall = '0'
007400010202     C*
007500021113     C                   if        not %open(fivatwwr)
007600021113     C                   open      fivatwwr
007700010201     C                   endif
007800990910     C*
007900010201     C                   clear                   §CTROKVB          5 0
008000020305     C                   clear                   §CTROKVT          5 0
008100000801     C                   clear                   §CTRMO            5 0
008200000801     C                   clear                   §CTRNO            5 0
008300990910     C*
008400921023     C                   DO        *HIVAL
008500990913     C*
008600990915     C                   READ      tivin00r                               70
008700050627     C                   if        vindta > *blanks
008800000613     C                   add       1             rrnum
008900000801     C*
009000000801     C                   if        *in70 = *off
009100000801     C                             and
009200000801     C                             (vinflg = *blanks
009300000801     C                              or vinflg = '0'
009400000801     C                              or vinflg = '2')
009500000801     C*
009600000801     C                   clear                   vinmsg
009700000801     C                   eval      vinflg = '1'
009800050628     C*
009900050628     C* A rottura inizializzo campi di totalizzazione
010000050628     C                   clear                   fivab000
010100050628     C*
010200060315     C                   exsr      impvab                                       => carico  VAB
010300060315     C                   exsr      wrivab                                       => scarico VAB
010400000905     C*
010500000905     C                   else
010600000905     C                   eval      vinflg = '1'
010700050628     C                   endif
010800000905     C                   endif
010900000905     C*
011000000905     C  N70              update    tivin000
011100000905     C*
011200991022     C  N70              ENDdo
011300010202     C*
011400010202     C                   endif
011500990910
011600990910     C* Se non ci sono record con errori ...
011700000710     C                   if        §ctrno = 0
011800990910     C* ... restituisco esito OK.
011900990921     C                   eval      wrkesito = '0'
012000990910     C                   else
012100010201     C                   if        §ctrokvb > 0
012200990921     C                   eval      wrkesito = '1'
012300000710     C                   else
012400000710     C                   eval      wrkesito = '2'
012500990910     C                   endif
012600000710     C                   endif
012700990910     C*
012800990914     C                   if        %open(tivin00r)
012900990908     C                   close     tivin00r
013000990914     C                   endif
013100021113     C                   if        %open(fivabwwr)
013200021113     C                   close     fivabwwr
013300990914     C                   endif
013400021113     C                   if        %open(fivatwwr)
013500021113     C                   close     fivatwwr
013600010201     C                   endif
013700990910     C*
013800010201     C                   if        §ctrokvb > 0
013900000724     C                             and vlrpoi <> *zeros
014000010202     C                   exsr      invio
014100990920     C                   endif
014200990920     C*
014300910830     C                   ENDSR
014400000613     C***
014500010305
014600010305     C*----------------------------------------------------*
014700020305     C*  SCARICAMENTO BUFFER RECORDS VAB
014800010305     C*----------------------------------------------------*
014900020305     C     WRIVAB        BEGSR
015000010305     C*
015100060225     C* Quindi scarico il buffer del file d testata
015200021113     C                   write     fivab000                                     => scarico il VAB
015300010305     C*
015400010305     C                   ENDSR
015500990920
015600000801     C*----------------------------------------------------*
015700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
015800000801     C*----------------------------------------------------*
015900010201     C     INZVAR        BEGSR
016000000801     C*
016100040802     C                   Z-ADD     *zeros        Num5_0            5 0
016200040802     C                   MOVEL     '0'           FlgCAS            1
016300000801     C*
016400000801     C                   ENDSR
016500000801     C*----------------------------------------------------*
016600000801     C*  IMPOSTAZIONE CAMPI COSTANTI
016700000801     C*----------------------------------------------------*
016800000801     C     DEFCAM        BEGSR
016900000801     C*
017000020619     C* Imposto i valori di default...
017100070102     C                   Z-ADD     0632964       VABCCM
017200070102     C                   Z-ADD     0632964       VATCCM
017300070102     C                   Z-ADD     063           VABLNP
017400070102     C                   Z-ADD     063           VATLNP
017500040714     C                   Z-ADD     000           VABCTR
017600040823     C                   MOVEL     '1'           VABCBO
017700110803     C                   MOVEL     '7Q'          VABCTM
017800020619     C* ... e poi verifico se sono stati passati come parametri
017900020619     C                   IF        vlrppt > *blanks
018000040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
018100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
018200020619     C                   EXSR      CHKNUM
018300020619     C                   IF        PiInt=*on
018400020619     C                   Z-ADD     PiVal         VABCCM
018500020619     C                   Z-ADD     PiVal         VATCCM
018600020619     C                   ENDIF
018700040506     C                   ENDIF
018800040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
018900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
019000020619     C                   EXSR      CHKNUM
019100020619     C                   IF        PiInt=*on
019200020619     C                   Z-ADD     PiVal         VABLNP
019300020619     C                   Z-ADD     PiVal         VATLNP
019400040506     C                   ENDIF
019500020619     C                   ENDIF
019600040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
019700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
019800020619     C                   EXSR      CHKNUM
019900020619     C                   IF        PiInt=*on
020000020619     C                   Z-ADD     PiVal         VABCTR
020100040506     C                   ENDIF
020200020619     C                   ENDIF
020300020619     C                   ENDIF
020400000801     C*
020500000801     C                   ENDSR
020600000801     C*----------------------------------------------------*
020700021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
020800000801     C*----------------------------------------------------*
020900040823     C     IMPVAB        BEGSR
021000040823     C*
021100020305     C                   EXSR      INZVAR
021200020305     C                   EXSR      DEFCAM
021300010305     C*
021400000801     C                   Z-ADD     *zeros        errore            1 0
021500000830     C                   MOVEL     datcor        VABAAS
021600020305     C                   MOVEL     datcor        VATAAS
021700040526     C                   MOVE      datcor        VABMGS
021800040823     C                   MOVE(P)   vlrpoi        VABFGS
021900040823     C                   MOVE(P)   vlrpoi        VATFGS
022000050628     C*
022100070102     C                   EVAL      VABRSD=%trim(%subst(vindta:276:40))
022200020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
022300020117     C     '@':'A'       XLATE     VABRSD        VABRSD
022400020117     C* ==
022500070102     C                   EVAL      VABIND=%trim(%subst(vindta:336:5))+' '+
022600070102     C                                    %trim(%subst(vindta:341:30))+' '+
022700070102     C                                    %trim(%subst(vindta:371:5))
022800070102     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
022900070102     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
023000070102     C                   EVAL      VABRMA=%trim(%subst(vindta:46:25))
023100070102     C                   EVAL      VABNAS=%trim(%subst(vindta:581:30))
023200070102     C                   EVAL      VABNOT=%trim(%subst(vindta:514:30))
023300070102     C                   IF        %subst(vindta:43:1) = 'N'
023400070102     C                   EVAL      VABCBO='1'
023500070102     C                   ENDIF
023600070102     C                   IF        %subst(vindta:43:1) = 'P'
023700070102     C                   EVAL      VABCBO='2'
023800070102     C                   ENDIF
023900070102     C                   IF        %subst(vindta:511:3) = 'ABM'
024000070102     C                   EVAL      VABTIC='BM'
024100070102     C                   ENDIF
024200070102     C                   IF        %subst(vindta:511:3) = 'ABS'
024300070102     C                   EVAL      VABTIC='B'
024400070102     C                   ENDIF
024500070102     C                   IF        %subst(vindta:511:3) = 'CON'
024600070102     C                   EVAL      VABTIC=*blanks
024700070102     C                   ENDIF
024800070102     C                   IF        %subst(vindta:511:3) = 'ACM'
024900070102     C                   EVAL      VABTIC='CM'
025000070102     C                   ENDIF
025100050628     C* CAP
025200070102     C                   EVAL      PiStr=%trim(%subst(vindta:391:9))
025300010201     C                   EXSR      CHKNUM
025400010201     C                   IF        PiInt=*on
025500010201     C                   Z-ADD     PiVal         Num5_0
025600040506     C                   MOVEL(P)  Num5_0        VABCAD
025700010201     C                   ELSE
025800040506     C                   ADD       1             errore
025900010201     C                   EVAL      vinmsg = %trimr(vinmsg)
026000040506     C                             + ' ' + 'VABCAD'
026100010201     C                   ENDIF
026200040506     C* Reperisco la provincia dal CAP e dalla località
026300040526     C                   IF        VABCAD <> *blanks AND
026400040526     C                             VABPRD  = *blanks
026500040506     C                   CLEAR                   TISI95DS
026600040506     C                   EVAL      I95TCN = '3'
026700040506     C                   Z-ADD     datcor        I95DAT
026800040506     C                   EVAL      I95CAP = VABCAD
026900040506     C                   EVAL      I95LOC = VABLOD
027000050627     C                   EVAL      I95NAR = VABNZD
027100040506     C                   CALL      'TISI95R'
027200040506     C                   PARM                    TISI95DS
027300040506     C                   EVAL      VABPRD = O95PRV
027400040506     C                   ENDIF
027500060307     C* NCL
027600070102     C                   EVAL      PiStr=%trim(%subst(vindta:432:3))
027700060307     C                   EXSR      CHKNUM
027800060307     C                   IF        PiInt=*on
027900060307     C                   ADD       PiVal         VABNCL
028000060307     C                   ELSE
028100060307     C                   ADD       1             errore
028200060307     C                   EVAL      vinmsg = %trimr(vinmsg)
028300060307     C                             + ' ' + 'VABNCL'
028400060307     C                   ENDIF
028500040506     C* PKB
028600070102     C                   EVAL      PiStr=%trim(%subst(vindta:435:7))
028700010201     C                   EXSR      CHKNUM
028800010201     C                   IF        PiNum=*on
028900070102     C                   EVAL      PiVal=PiVal/1000                             da grammi a kg
029000060316     C                   Z-ADD     PiVal         VABPKB
029100010201     C                   ELSE
029200010201     C                   ADD       1             errore
029300010201     C                   EVAL      vinmsg = %trimr(vinmsg)
029400010201     C                             + ' ' + 'VABPKB'
029500010201     C                   ENDIF
029600060316     C* NSP/RMN
029700070102     C                   EVAL      PiStr=%trim(%subst(vindta:46:25))
029800050928     C                   EXSR      CHKNUM
029900050928     C                   IF        PiInt=*on
030000060316     C                   Z-ADD     PiVal         VABNSP
030100060316     C                   Z-ADD     PiVal         VATNSP
030200050928     C                   Z-ADD     PiVal         VABRMN
030300050928     C                   ELSE
030400050928     C                   ADD       1             errore
030500050928     C                   EVAL      vinmsg = %trimr(vinmsg)
030600060316     C                             + ' ' + 'VABNSP VATNSP VABRMN'
030700050928     C                   ENDIF
030800060307     C* CAS
030900070102     C                   IF        %subst(vindta:502:9) <> *blanks
031000070102     C                   EVAL      VABVCA = 'EUR'
031100060307     C                   EVAL      FlgCAS = '1'
031200070102     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
031300060307     C                   EXSR      CHKNUM
031400060307     C                   IF        PiNum=*on
031500060316     C                   Z-ADD     PiVal         VABCAS
031600060307     C                   ELSE
031700060307     C                   ADD       1             errore
031800060307     C                   EVAL      vinmsg = %trimr(vinmsg)
031900060307     C                             + ' ' + 'VABCAS'
032000060307     C                   ENDIF
032100070102     C                   ENDIF
032200060316     C* IAS
032300070102     C                   IF        %subst(vindta:544:10) <> *blanks
032400070102     C                   EVAL      VABVAS = 'EUR'
032500070102     C                   EVAL      PiStr=%trim(%subst(vindta:544:10))
032600060316     C                   EXSR      CHKNUM
032700060316     C                   IF        PiNum=*on
032800060316     C                   Z-ADD     PiVal         VABIAS
032900060316     C                   ELSE
033000060316     C                   ADD       1             errore
033100060316     C                   EVAL      vinmsg = %trimr(vinmsg)
033200060316     C                             + ' ' + 'VABIAS'
033300070102     C                   ENDIF
033400060316     C                   ENDIF
033500060316     C*
033600070102     C* Scrivo anche estensione dettaglio bolle - tipo record "A" => REFERENTE CONSEGNA
033700070102     C                   exsr      exevata
033800070102     C*
033900070102     C* Scrivo anche estensione dettaglio bolle - tipo record "B" => TELEFONO DESTINATARIO
034000070102     C                   exsr      exevatb
034100010205     C*
034200010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
034300040802     C                   IF        FlgCAS <> '0'
034400070102     C                   IF        VABCBO = '1'
034500010205     C                   EVAL      VABCBO = '4'
034600010205     C                   ELSE
034700070102     C                   EVAL      VABCBO = '6'
034800070102     C                   ENDIF
034900010205     C                   ENDIF
035000020305     C*
035100011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
035200011113     C                   EXSR      CHKIMPDIV
035300010202     C*
035400000801     C* Ebbene...
035500000801     C                   ADD       1             §CTRMO
035600010201     C                   IF        errore <> *zeros
035700000801     C                   ADD       1             §CTRNO
035800000801     C                   EVAL      vinflg = '2'
035900000801     C                   ELSE
036000010201     C                   ADD       1             §CTROKVB
036100000801     C                   ENDIF
036200000801     C*
036300000801     C                   ENDSR
036400070102     C*----------------------------------------------------*
036500070102     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
036600070102     C*----------------------------------------------------*
036700070102     C     EXEVATA       BEGSR
036800070102     C*
036900070102     C                   EXSR      INZVAR
037000070102     C                   EXSR      DEFCAM
037100070102     C*
037200070102     C                   EVAL      VATTRC='A'
037300070102     C                   EVAL      VATNOT = %trim(%subst(vindta:316:20))
037400070102     C                   exsr      wrivat                                       => scarico VAT
037500070102     C*
037600070102     C                   ENDSR
037700060307     C*----------------------------------------------------*
037800060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
037900060307     C*----------------------------------------------------*
038000060307     C     EXEVATB       BEGSR
038100060307     C*
038200060307     C                   EXSR      INZVAR
038300060307     C                   EXSR      DEFCAM
038400060307     C*
038500060307     C                   EVAL      VATTRC='B'
038600070102     C                   EVAL      VATNOT = %trim(%subst(vindta:376:15))
038700060307     C                   exsr      wrivat                                       => scarico VAT
038800060307     C*
038900060307     C                   ENDSR
039000110803     C*----------------------------------------------------*
039100110803     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
039200110803     C*----------------------------------------------------*
039300110803     C     EXEVATE       BEGSR
039400110803     C*
039500110803     C                   EXSR      INZVAR
039600110803     C                   EXSR      DEFCAM
039700110803     C*
039800110803     C                   EVAL      VATTRC='E'
039900110803     C                   EVAL      VATNOT = %trim(%subst(vindta:554:25))
040000110803     C                   exsr      wrivat                                       => scarico VAT
040100110803     C*
040200110803     C                   ENDSR
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
044400070102     C                   PARM      '.'           PiDecChr          1
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
