000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200140221      *
000300140221      * PARTICOLARITA':
000400140221      *
000500140221     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000600990908
000700990910     Ftivin00r  uF   E             DISK    usropn
000800021113     FFIVABwwr  O    E             DISK    usropn
000900021113     FFIVATwwr  O    E             DISK    usropn
001000990908
001100000801     D*----------------------------------------------------
001200000801     D* DICHIARAZIOINE VARIABILI DI WRK
001300000801     D*----------------------------------------------------
001400990920     D dscmz         e ds                  inz
001500990910     D psds           sds
001600990910     D  procname         *PROC
001700990920     D tivlrds       e ds                  extname(tivlr00f)
001800060307     D tisi95ds      e ds
001900990910     D esito           s              1
002000000724     D prmlit          s             10
002100000710     D prmfir          s             10
002200990921     D wrkesito        s                   like(esito)
002300000613     D rrnum           s              6  0 INZ(*zeros)
002400010202     D parccm          s              8    INZ(*blanks)
002500010202     D parmbr          s             10    INZ(*blanks)
002600010202     D paropz          s              1    INZ(*blanks)
002700010202     D chkcall         s              1    INZ(*blanks)
002800160202     D curSped         s             10    INZ(*blanks)
002900160202     D depSped         s             10    INZ(*blanks)
003000140221     D w70             s             70    INZ(*blanks)
003100160204     D wX              s              3s 0
003200100118
003300100118     D*------------------
003400100118     D* DS REPERIMENTO NUMERATORE
003500100118     D*------------------
003600100118     D trul33ds      e ds                  inz
003700100118     D*------------------
003800100118     D* DS ARCHITETTURA
003900100118     D*------------------
004000100118     D kpjba         e ds                  inz
004100100118
004200010201
004300010201
004400090127     C*
004500000913     C                   reset                   rrnum
004600990921     C                   reset                   esito
004700990921     C                   reset                   wrkesito
004800000613     C*
004900040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005000000613     C*
005100010202     C* Effettuo la chiamata al CLLE preposto
005200040506     C                   call(e)   'TITVVTC'
005300010202     C                   parm                    parccm
005400010202     C                   parm                    parmbr
005500010202     C                   parm      '2'           paropz
005600050201     C*
005700050201     C* Effettuo lancio TISI95 solo x chiusura
005800050201     C                   CLEAR                   TISI95DS
005900050201     C                   EVAL      I95TLA = 'C'
006000050201     C                   CALL      'TISI95R'
006100050201     C                   PARM                    TISI95DS
006200000616     C*
006300000801     C
006400010201     C                   seton                                        LR
006500990908
006600000801
006700910830     C*--------------------------------------------------------
006800040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
006900910830     C*--------------------------------------------------------
007000040526     C     RWFILE        BEGSR
007100990910     C*
007200990914     C                   if        not %open(tivin00r)
007300990908     C                   open      tivin00r
007400990914     C                   endif
007500021113     C                   if        not %open(fivabwwr)
007600021113     C                   open      fivabwwr
007700990914     C                   endif
007800021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007900020305     C                   exsr      prevat
008000010201     C*
008100010202     C                   if        chkcall = '0'
008200010202     C*
008300021113     C                   if        not %open(fivatwwr)
008400021113     C                   open      fivatwwr
008500010201     C                   endif
008600990910     C*
008700010201     C                   clear                   §CTROKVB          5 0
008800020305     C                   clear                   §CTROKVT          5 0
008900000801     C                   clear                   §CTRMO            5 0
009000000801     C                   clear                   §CTRNO            5 0
009100990910     C*
009200921023     C                   DO        *HIVAL
009300990913     C*
009400990915     C                   READ      tivin00r                               70
009500050627     C                   if        vindta > *blanks
009600000613     C                   add       1             rrnum
009700000801     C*
009800000801     C                   if        *in70 = *off
009900000801     C                             and
010000000801     C                             (vinflg = *blanks
010100000801     C                              or vinflg = '0'
010200000801     C                              or vinflg = '2')
010300000801     C*
010400000801     C                   clear                   vinmsg
010500000801     C                   eval      vinflg = '1'
010600070402     C*
010700160202     C* Verifico la rottura d codice x nr. riferimento
010800160202     C                   eval      curSped = %subst(vindta:108:10)
010900070402     C                   if        curSped <> depSped
011000070402     C                   clear                   fivab000
011100070402     C                   clear                   fivat000
011200050628     C*
011300060315     C                   exsr      impvab                                       => carico  VAB
011400090127     C*
011500060315     C                   exsr      wrivab                                       => scarico VAB
011600070402     C*
011700070402     C* Scrivo anche estensione dettaglio bolle - tipo record "A" => REFERENTE CONSEGNA
011800160204     C***                exsr      exevata
011900070402     C*
012000070402     C* Scrivo anche estensione dettaglio bolle - tipo record "B" => TELEFONO DESTINATARIO
012100160204     C***                exsr      exevatb
012200140226     C*
012300140226     C* Indirizzo e-mail
012400160204     C***                exsr      exevati
012500140226     C*
012600140226     C* Cellulare per SMS
012700160204     C***                exsr      exevats
012800070402     C*
012900070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
013000070402     C                   exsr      exevate
013100070402     C*
013200070402     C* Salvo il raggruppamento spedizione cliente corrente
013300070402     C                   eval      depSped = curSped
013400070402     C*
013500070402     C                   else
013600070402     C*
013700070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
013800070402     C                   exsr      exevate
013900070402     C                   endif
014000000905     C*
014100000905     C                   else
014200000905     C                   eval      vinflg = '1'
014300050628     C                   endif
014400000905     C                   endif
014500000905     C*
014600000905     C  N70              update    tivin000
014700000905     C*
014800991022     C  N70              ENDdo
014900010202     C*
015000010202     C                   endif
015100990910
015200990910     C* Se non ci sono record con errori ...
015300000710     C                   if        §ctrno = 0
015400990910     C* ... restituisco esito OK.
015500990921     C                   eval      wrkesito = '0'
015600990910     C                   else
015700010201     C                   if        §ctrokvb > 0
015800990921     C                   eval      wrkesito = '1'
015900000710     C                   else
016000000710     C                   eval      wrkesito = '2'
016100990910     C                   endif
016200000710     C                   endif
016300990910     C*
016400990914     C                   if        %open(tivin00r)
016500990908     C                   close     tivin00r
016600990914     C                   endif
016700021113     C                   if        %open(fivabwwr)
016800021113     C                   close     fivabwwr
016900990914     C                   endif
017000021113     C                   if        %open(fivatwwr)
017100021113     C                   close     fivatwwr
017200010201     C                   endif
017300990910     C*
017400010201     C                   if        §ctrokvb > 0
017500000724     C                             and vlrpoi <> *zeros
017600010202     C                   exsr      invio
017700990920     C                   endif
017800990920     C*
017900910830     C                   ENDSR
018000000613     C***
018100010305
018200010305     C*----------------------------------------------------*
018300020305     C*  SCARICAMENTO BUFFER RECORDS VAB
018400010305     C*----------------------------------------------------*
018500090127     C     WRIVAB        BEGSR
018600090127     C*
018700090127     C* Quindi scarico il buffer del file d testata
018800090127     C                   write     fivab000                                     => scarico il VAB
018900010305     C*
019000010305     C                   ENDSR
019100990920
019200000801     C*----------------------------------------------------*
019300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019400000801     C*----------------------------------------------------*
019500010201     C     INZVAR        BEGSR
019600000801     C*
019700040802     C                   Z-ADD     *zeros        Num5_0            5 0
019800040802     C                   MOVEL     '0'           FlgCAS            1
019900000801     C*
020000000801     C                   ENDSR
020100000801     C*----------------------------------------------------*
020200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
020300000801     C*----------------------------------------------------*
020400000801     C     DEFCAM        BEGSR
020500070718     C*
020600070718     C* Imposto i valori di default...
020700160202     C                   EVAL      VABCCM = 2493089
020800160202     C                   EVAL      VABLNP = 249
020900160202     C                   Z-ADD     VABCCM        VATCCM
021000160202     C                   Z-ADD     VABLNP        VATLNP
021100160202     C                   Z-ADD     000           VABCTR
021200140224     C                   MOVEL     '1'           VABCBO
021300160204     C                   MOVEL     '7Q'          VABCTM
021400020619     C* ... e poi verifico se sono stati passati come parametri
021500020619     C                   IF        vlrppt > *blanks
021600040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
021700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
021800020619     C                   EXSR      CHKNUM
021900020619     C                   IF        PiInt=*on
022000020619     C                   Z-ADD     PiVal         VABCCM
022100020619     C                   Z-ADD     PiVal         VATCCM
022200020619     C                   ENDIF
022300040506     C                   ENDIF
022400040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
022500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022600020619     C                   EXSR      CHKNUM
022700020619     C                   IF        PiInt=*on
022800020619     C                   Z-ADD     PiVal         VABLNP
022900020619     C                   Z-ADD     PiVal         VATLNP
023000040506     C                   ENDIF
023100020619     C                   ENDIF
023200040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
023300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
023400020619     C                   EXSR      CHKNUM
023500020619     C                   IF        PiInt=*on
023600020619     C                   Z-ADD     PiVal         VABCTR
023700040506     C                   ENDIF
023800020619     C                   ENDIF
023900140224     C                   IF        %subst(vlrppt:16:2) <> *blanks
024000140224     C                   EVAL      VABCTM = %subst(vlrppt:16:2)
024100140224     C                   ENDIF
024200020619     C                   ENDIF
024300000801     C*
024400000801     C                   ENDSR
024500000801     C*----------------------------------------------------*
024600021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
024700000801     C*----------------------------------------------------*
024800040823     C     IMPVAB        BEGSR
024900040823     C*
025000020305     C                   EXSR      INZVAR
025100020305     C                   EXSR      DEFCAM
025200010305     C*
025300000801     C                   Z-ADD     *zeros        errore            1 0
025400000830     C                   MOVEL     datcor        VABAAS
025500020305     C                   MOVEL     datcor        VATAAS
025600040526     C                   MOVE      datcor        VABMGS
025700040823     C                   MOVE(P)   vlrpoi        VABFGS
025800040823     C                   MOVE(P)   vlrpoi        VATFGS
025900160202
026000160202     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
026100160202     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026200160202     C     '@':'A'       XLATE     VABRSD        VABRSD
026300160202     C* ==
026400160202     C                   EVAL      VABIND=%trim(%subst(vindta:36:35))
026500160202     C                   EVAL      VABLOD=%trim(%subst(vindta:71:30))
026600160202     C                   EVAL      VABPRD=%trim(%subst(vindta:106:2))
026700160202     C                   IF        %subst(vindta:207:1) = 'A'
026800160202     C                   EVAL      VABCBO='2'
026900160202     C                   ENDIF
027000160202     C***                EVAL      VABRMA=%trim(%subst(vindta:108:10))
027100160202     C                   EVAL      VABRMA=%trim(%subst(vindta:245:20))
027200160202     C                   EVAL      VABNOT=%trim(%subst(vindta:147:35))
027300160202     C                   EVAL      VABNT2=%trim(%subst(vindta:147+35:60-35))
027400160202     C* CAD
027500160202     C                   EVAL      PiStr=%trim(%subst(vindta:101:5))
027600160202     C                   EXSR      CHKNUM
027700160202     C                   IF        PiInt=*on
027800160202     C                   Z-ADD     PiVal         Num5_0
027900160202     C                   MOVEL(p)  Num5_0        VABCAD
028000160202     C                   ELSE
028100160202     C                   SETON                                        32
028200160202     C                   EVAL      VABCAD = *zeros
028300160202     C                   EVAL      vinmsg = %trimr(vinmsg)
028400160202     C                             + ' ' + 'VABCAD'
028500160202     C                   ENDIF
028600160202     C* NSP/RMN
028700160202     C                   EVAL      PiStr=%trim(%subst(vindta:108:10))
028800160202     C                   EXSR      CHKNUM
028900160202     C                   IF        PiInt=*on
029000160202     C                   Z-ADD     PiVal         VABNSP
029100160204     C                   Z-ADD     PiVal         VATNSP
029200160202     C                   Z-ADD     PiVal         VABRMN
029300160202     C                   ELSE
029400160202     C                   SETON                                        31
029500160202     C                   Z-ADD     *zeros        VABNSP
029600160204     C                   Z-ADD     *zeros        VATNSP
029700160202     C                   Z-ADD     1             VABRMN
029800160202     C                   EVAL      vinmsg = %trimr(vinmsg)
029900160202     C                             + ' ' + 'VABNSP VABRMN'
030000160202     C                   ENDIF
030100160202     C* NCL
030200160202     C                   EVAL      PiStr=%trim(%subst(vindta:124:5))
030300160202     C                   EXSR      CHKNUM
030400160202     C                   IF        PiInt=*on
030500160202     C                   Z-ADD     PiVal         VABNCL
030600160202     C                   ELSE
030700160202     C                   SETON                                        32
030800160202     C                   Z-ADD     *zeros        VABNCL
030900160202     C                   EVAL      vinmsg = %trimr(vinmsg)
031000160202     C                             + ' ' + 'VABNCL'
031100160202     C                   ENDIF
031200160202     C* PKB
031300160202     C                   EVAL      PiStr=%trim(%subst(vindta:131:7))
031400160202     C                   EVAL      PiDecChr = ','
031500160202     C                   EXSR      CHKNUM
031600160202     C                   IF        PiNum=*on
031700160202     C                   Z-ADD     PiVal         VABPKB
031800160202     C                   ELSE
031900160202     C                   SETON                                        32
032000160202     C                   Z-ADD     *zeros        VABPKB
032100160202     C                   EVAL      vinmsg = %trimr(vinmsg)
032200160202     C                             + ' ' + 'VABPKB'
032300160202     C                   ENDIF
032400160202     C* CAS
032500160202     C                   IF        %subst(vindta:138:9) <> '000000,00'
032600160202     C                   EVAL      FlgCAS = '1'
032700160202     C                   EVAL      VABVCA =  'EUR'
032800160202     C                   EVAL      PiStr=%trim(%subst(vindta:138:9))
032900160202     C                   EVAL      PiDecChr = ','
033000160202     C                   EXSR      CHKNUM
033100160202     C                   IF        PiNum=*on
033200160202     C                   Z-ADD     PiVal         VABCAS
033300160202     C                   ELSE
033400160202     C                   SETON                                        32
033500160202     C                   Z-ADD     *zeros        VABCAS
033600160202     C                   EVAL      vinmsg = %trimr(vinmsg)
033700160202     C                             + ' ' + 'VABCAS'
033800160202     C                   ENDIF
033900160202     C                   ENDIF
034000050628     C*
034100040506     C* Reperisco la provincia dal CAP e dalla località
034200040526     C                   IF        VABCAD <> *blanks AND
034300040526     C                             VABPRD  = *blanks
034400040506     C                   CLEAR                   TISI95DS
034500040506     C                   EVAL      I95TCN = '3'
034600040506     C                   Z-ADD     datcor        I95DAT
034700040506     C                   EVAL      I95CAP = VABCAD
034800040506     C                   EVAL      I95LOC = VABLOD
034900050627     C                   EVAL      I95NAR = VABNZD
035000040506     C                   CALL      'TISI95R'
035100040506     C                   PARM                    TISI95DS
035200040506     C                   EVAL      VABPRD = O95PRV
035300040506     C                   ENDIF
035400010205     C*
035500010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
035600040802     C                   IF        FlgCAS <> '0'
035700070102     C                   IF        VABCBO = '1'
035800010205     C                   EVAL      VABCBO = '4'
035900010205     C                   ELSE
036000070102     C                   EVAL      VABCBO = '6'
036100070102     C                   ENDIF
036200140224     C                   EVAL      VABTIC = 'BM'
036300010205     C                   ENDIF
036400020305     C*
036500011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
036600011113     C                   EXSR      CHKIMPDIV
036700010202     C*
036800000801     C* Ebbene...
036900000801     C                   ADD       1             §CTRMO
037000010201     C                   IF        errore <> *zeros
037100000801     C                   ADD       1             §CTRNO
037200000801     C                   EVAL      vinflg = '2'
037300000801     C                   ELSE
037400010201     C                   ADD       1             §CTROKVB
037500000801     C                   ENDIF
037600000801     C*
037700000801     C                   ENDSR
037800070102     C*----------------------------------------------------*
037900070102     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
038000070102     C*----------------------------------------------------*
038100070102     C     EXEVATA       BEGSR
038200070102     C*
038300070102     C                   EXSR      INZVAR
038400070102     C                   EXSR      DEFCAM
038500070102     C*
038600070102     C                   EVAL      VATTRC='A'
038700160204     C***                EVAL      VATNOT = %trim(%subst(vindta:376:15))
038800070102     C                   exsr      wrivat                                       => scarico VAT
038900070102     C*
039000070102     C                   ENDSR
039100060307     C*----------------------------------------------------*
039200060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
039300060307     C*----------------------------------------------------*
039400060307     C     EXEVATB       BEGSR
039500060307     C*
039600060307     C                   EXSR      INZVAR
039700060307     C                   EXSR      DEFCAM
039800060307     C*
039900060307     C                   EVAL      VATTRC='B'
040000160204     C***                EVAL      VATNOT = %trim(%subst(vindta:713:14))
040100060307     C                   exsr      wrivat                                       => scarico VAT
040200060307     C*
040300060307     C                   ENDSR
040400070402     C*----------------------------------------------------*
040500070402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
040600070402     C*----------------------------------------------------*
040700070402     C     EXEVATE       BEGSR
040800070402     C*
040900070402     C                   EXSR      INZVAR
041000070402     C                   EXSR      DEFCAM
041100070402     C*
041200160204     C                   EVAL      VATTRC='E'
041300160204     C* sviluppo tanti barcode quanti colli indicati, costruiti in questo modo:
041400160204     C* VABRMN (10,0)
041500160204     C* '-'
041600160204     C* un prog. lungo 3 che va da 1 a VABNCL
041700160204     C     1             do        VABNCL        wX
041800160204     C                   EVAL      VATNOT = %trim(%subst(vindta:108:10))
041900160204     C                                      + '-' + %editc(wX:'X')
042000070402     C                   exsr      wrivat                                       => scarico VAT
042100160204     C                   enddo
042200070402     C*
042300070402     C                   ENDSR
042400140224     C*----------------------------------------------------*
042500140224     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I" e "J"
042600140224     C*----------------------------------------------------*
042700140224     C     EXEVATI       BEGSR
042800140224     C*
042900140224     C                   EXSR      INZVAR
043000140224     C                   EXSR      DEFCAM
043100140224     C*
043200140224     C                   EVAL      VATTRC='I'
043300160204     C***                EVAL      VATNOT = %trim(%subst(vindta:663:35))
043400140224     C                   exsr      wrivat                                       => scarico VAT
043500140224     C*
043600140224     C                   EVAL      VATTRC='J'
043700160204     C***                EVAL      VATNOT = %trim(%subst(vindta:663+35:15))
043800140224     C                   exsr      wrivat                                       => scarico VAT
043900140224     C*
044000140224     C                   ENDSR
044100140226     C*----------------------------------------------------*
044200140226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "S"
044300140226     C*----------------------------------------------------*
044400140226     C     EXEVATS       BEGSR
044500140226     C*
044600140226     C                   EXSR      INZVAR
044700140226     C                   EXSR      DEFCAM
044800140226     C*
044900140226     C                   EVAL      VATTRC='S'
045000140226     C                   EVAL      VATNOT=*blank
045100160204     C***                EVAL      %subst(VATNOT:1:14) =
045200160204     C***                           %trim(%subst(vindta:713:14))
045300140226     C                   IF        VATNOT <> *blank
045400140226     C                   EVAL      %subst(VATNOT:17:1) = 'S'
045500140226     C                   ENDIF
045600140226     C                   exsr      wrivat                                       => scarico VAT
045700140226     C*
045800140226     C                   ENDSR
045900010201     C*----------------------------------------------------*
046000040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
046100010201     C*----------------------------------------------------*
046200020305     C     WRIVAT        BEGSR
046300050628     C*
046400060223     C* Scrivo solo se valorizzato qualcosa
046500060223     C                   IF        VATNOT <> *blanks
046600040802     C                   WRITE     FIVAT000
046700060223     C                   ENDIF
046800010201     C*
046900010201     C                   ENDSR
047000010202     C*----------------------------------------------------*
047100021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
047200010202     C*----------------------------------------------------*
047300020305     C     PREVAT        BEGSR
047400010202     C*
047500021113     C* Compongo il nome del membro da dare al FIVATWWR
047600010202     C                   eval      parmbr = vlrhdl
047700010202     C                   movel     'M'           parmbr
047800050627     C                   eval      parccm = %subst(vlrKSC:2:7)
047900010202     C                   eval      paropz = '1'
048000010202     C* Effettuo la chiamata al CLLE preposto
048100040506     C                   call(e)   'TITVVTC'
048200010202     C                   parm                    parccm
048300010202     C                   parm                    parmbr
048400010202     C                   parm                    paropz
048500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048600010202     C                   if        %error
048700010202     C                   movel     '1'           chkcall
048800010202     C                   else
048900010202     C                   movel     '0'           chkcall
049000010202     C                   endif
049100010202     C*
049200010202     C                   ENDSR
049300000801     C*----------------------------------------------------*
049400000801     C*  CONTROLLO NUMERICITA' CAMPI
049500000801     C*----------------------------------------------------*
049600000801     C     CHKNUM        BEGSR
049700160208     C*
049800160208     C                   IF        PiDecChr = *blanks
049900160208     C                   EVAL      PiDecChr = ','
050000160208     C                   ENDIF
050100160208     C*
050200160208     C                   CALL(e)   'ISNUMERIC'
050300160208     C                   PARM                    PiStr            30
050400160208     C                   PARM                    PiDecChr          1
050500160208     C                   PARM      *ZEROS        PiVal            30 9
050600160208     C                   PARM      '0'           PiInt             1
050700160208     C                   PARM      '0'           PiNum             1
050800160208     C                   IF        %error
050900160208     C                   EVAL      PiNum=*off
051000160208     C                   ENDIF
051100000801     C*
051200000801     C                   ENDSR
051300000801     C***
051400000801
051500011113
051600011113     C*----------------------------------------------------*
051700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
051800011113     C*----------------------------------------------------*
051900011113     C     CHKIMPDIV     BEGSR
052000011113     C*
052100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
052200011113     C                   Z-ADD     *zeros        wrkDec            9 9
052300011113     C*
052400011113     C* Come prima cosa effettuo considerazioni sulla divisa
052500011113     C                   IF        vabIAS > *zeros
052600011113     C                   IF        vabVAS <> 'EUR'
052700011113     C                   EVAL      vabVAS =  'ITL'
052800011113     C                   ENDIF
052900011113     C                   ENDIF
053000011113     C*
053100011113     C                   IF        vabCAS > *zeros
053200011113     C                   IF        vabVCA <> 'EUR'
053300011113     C                   EVAL      vabVCA =  'ITL'
053400011113     C                   ENDIF
053500011113     C                   ENDIF
053600011113     C*
053700011113     C                   IF        vabVMD > *zeros
053800020305     C                   IF        vabVAD <> 'EUR'
053900011113     C                   EVAL      vabVAD =  'ITL'
054000011113     C                   ENDIF
054100011113     C                   ENDIF
054200011113     C*
054300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
054400011113     C                   Z-ADD     vabIAS        wrkDec
054500011113     C                   IF        wrkDec > *zeros
054600011113     C                   IF        vabVAS = 'ITL'
054700011113     C                   EVAL      vabIAS = *zeros
054800011113     C                   ENDIF
054900011113     C                   ENDIF
055000011113     C*
055100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
055200011113     C                   Z-ADD     vabCAS        wrkDec
055300011113     C                   IF        wrkDec > *zeros
055400011113     C                   IF        vabVCA = 'ITL'
055500011113     C                   EVAL      vabCAS = *zeros
055600011113     C                   ENDIF
055700011113     C                   ENDIF
055800011113     C*
055900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
056000011113     C                   Z-ADD     vabVMD        wrkDec
056100011113     C                   IF        wrkDec > *zeros
056200011113     C                   IF        vabVAD = 'ITL'
056300011113     C                   EVAL      vabVMD = *zeros
056400011113     C                   ENDIF
056500011113     C                   ENDIF
056600011113     C*
056700011113     C                   ENDSR
056800011113     C***
056900011113
057000011113
057100000801
057200000801
057300990920      /TITLE Invio dei dati al punto operativo.
057400010202     C     invio         BEGSR
057500990920     C*
057600021113     C* 1° invio FIVAT
057700010201     C                   reset                   dscmz
057800010201     C                   move      vlrpoi        cmzdst
057900021113     C                   eval      cmzfld = 'FIVATWWR'
058000010201     C                   eval      cmzmbd = vlrhdl
058100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058200021009     C***                if        prmfir = *blanks
058300021113     C                   eval      cmzfla = 'FIVAT00F'
058400021113     C                   eval      cmzmba = 'FIVAT00F'
058500021009     C***                else
058600021009     C***                eval      cmzfla = prmfir
058700021009     C***                eval      cmzmba = prmfir
058800021009     C***                endif
058900010201     C                   eval      cmznrr = *zeros
059000020305     C                   move      §ctrokvt      cmznrr
059100021018     C                   eval      cmzlba = vlrfl1
059200010201     C                   call(e)   'TIS711C'
059300010201     C                   parm                    dscmz
059400010201     C                   parm      *blanks       esito
059500010205     C                   if        %error
059600010205     C                             or cmzerr = '1'
059700010205     C                             or esito  = '1'
059800010205     C                   eval      wrkesito = '3'
059900010205     C                   else
060000010201     C*
060100021113     C* 2° invio FIVAB
060200010201     C                   reset                   dscmz
060300010201     C                   move      vlrpoi        cmzdst
060400010201     C                   eval      cmzfld = vlrfou
060500010201     C                   eval      cmzmbd = vlrhdl
060600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060700021009     C***                if        prmfir = *blanks
060800021113     C                   eval      cmzfla = 'FIVAB00F'
060900021113     C                   eval      cmzmba = 'FIVAB00F'
061000021009     C***                else
061100021009     C***                eval      cmzfla = prmfir
061200021009     C***                eval      cmzmba = prmfir
061300021009     C***                endif
061400010201     C                   eval      cmznrr = *zeros
061500010201     C                   move      §ctrokvb      cmznrr
061600021018     C                   eval      cmzlba = vlrfl1
061700010201     C                   call(e)   'TIS711C'
061800010201     C                   parm                    dscmz
061900010201     C                   parm      *blanks       esito
062000010201     C                   if        %error
062100010201     C                             or cmzerr = '1'
062200010201     C                             or esito  = '1'
062300010201     C                   eval      wrkesito = '3'
062400010201     C                   endif
062500010205     C                   endif
062600990920     C*
062700000613     C                   ENDSR
062800000613     C***
062900990910
063000000613     C     *inzsr        BEGSR
063100990910     C*
063200990910     C     *entry        plist
063300990920     C                   parm                    tivlrds
063400990921     C                   parm      wrkesito      esito
063500000724     C                   parm                    prmlit
063600000710     C                   parm                    prmfir
063700000613     C*
063800000830     C* CALCOLA LA DATA CORRENTE
063900100118     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
064000100118     C                   eval      datcor = %dec(%date() : *ISO)
064100000830     C*
064200000613     C                   ENDSR
064300000613     C***
