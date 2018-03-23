000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200140221      *
000300140221      * PARTICOLARITA':
000400150413      * disk A
000500140221      *
000600140221     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000700990908
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
002000990910     D esito           s              1
002100000724     D prmlit          s             10
002200000710     D prmfir          s             10
002300990921     D wrkesito        s                   like(esito)
002400000613     D rrnum           s              6  0 INZ(*zeros)
002500010202     D parccm          s              8    INZ(*blanks)
002600010202     D parmbr          s             10    INZ(*blanks)
002700010202     D paropz          s              1    INZ(*blanks)
002800010202     D chkcall         s              1    INZ(*blanks)
002900150521     D curSped         s              5    INZ(*blanks)
003000150521     D depSped         s              5    INZ(*blanks)
003100140221     D w70             s             70    INZ(*blanks)
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
010700070402     C* Verifico la rottura d codice x raggruppamento spedizione cliente
010800150521     C                   eval      curSped = %subst(vindta:35:5)
010900070402     C                   if        curSped <> depSped
011000070402     C                   clear                   fivab000
011100070402     C                   clear                   fivat000
011200050628     C*
011300060315     C                   exsr      impvab                                       => carico  VAB
011400090127     C*
011500060315     C                   exsr      wrivab                                       => scarico VAB
011600070402     C*
011700070402     C* Scrivo anche estensione dettaglio bolle - tipo record "A" => REFERENTE CONSEGNA
011800150413     C***                exsr      exevata
011900070402     C*
012000070402     C* Scrivo anche estensione dettaglio bolle - tipo record "B" => TELEFONO DESTINATARIO
012100150413     C***                exsr      exevatb
012200140226     C*
012300140226     C* Indirizzo e-mail
012400150413     C***                exsr      exevati
012500140226     C*
012600140226     C* Cellulare per SMS
012700150413     C***                exsr      exevats
012800070402     C*
012900070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
013000150413     C***                exsr      exevate
013100070402     C*
013200070402     C* Salvo il raggruppamento spedizione cliente corrente
013300070402     C                   eval      depSped = curSped
013400070402     C*
013500070402     C                   else
013600070402     C*
013700070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
013800150413     C***                exsr      exevate
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
020700150521     C                   Z-ADD     0057983       VABCCM
020800150521     C                   Z-ADD     0057983       VATCCM
020900150521     C                   Z-ADD     005           VABLNP
021000150521     C                   Z-ADD     005           VATLNP
021100150413     C                   Z-ADD     000           VABCTR
021200150225     C                   MOVEL     '1'           VABCBO
021300020619     C* ... e poi verifico se sono stati passati come parametri
021400020619     C                   IF        vlrppt > *blanks
021500040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
021600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
021700020619     C                   EXSR      CHKNUM
021800020619     C                   IF        PiInt=*on
021900020619     C                   Z-ADD     PiVal         VABCCM
022000020619     C                   Z-ADD     PiVal         VATCCM
022100020619     C                   ENDIF
022200040506     C                   ENDIF
022300040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
022400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022500020619     C                   EXSR      CHKNUM
022600020619     C                   IF        PiInt=*on
022700020619     C                   Z-ADD     PiVal         VABLNP
022800020619     C                   Z-ADD     PiVal         VATLNP
022900040506     C                   ENDIF
023000020619     C                   ENDIF
023100040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
023200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
023300020619     C                   EXSR      CHKNUM
023400020619     C                   IF        PiInt=*on
023500020619     C                   Z-ADD     PiVal         VABCTR
023600040506     C                   ENDIF
023700020619     C                   ENDIF
023800140224     C                   IF        %subst(vlrppt:16:2) <> *blanks
023900140224     C                   EVAL      VABCTM = %subst(vlrppt:16:2)
024000140224     C                   ENDIF
024100020619     C                   ENDIF
024200000801     C*
024300000801     C                   ENDSR
024400000801     C*----------------------------------------------------*
024500021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
024600000801     C*----------------------------------------------------*
024700040823     C     IMPVAB        BEGSR
024800040823     C*
024900020305     C                   EXSR      INZVAR
025000020305     C                   EXSR      DEFCAM
025100010305     C*
025200000801     C                   Z-ADD     *zeros        errore            1 0
025300000830     C                   MOVEL     datcor        VABAAS
025400020305     C                   MOVEL     datcor        VATAAS
025500040526     C                   MOVE      datcor        VABMGS
025600040823     C                   MOVE(P)   vlrpoi        VABFGS
025700040823     C                   MOVE(P)   vlrpoi        VATFGS
025800050628     C*
025900150521     C* Considerazioni sulla ragione sociale del destinatario da indicare
026000150521     C                   EVAL      VABRSD=%trim(%subst(vindta:50:35))
026100150521     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026200150521     C     '@':'A'       XLATE     VABRSD        VABRSD
026300150521     C* ==
026400150521     C                   EVAL      VABIND=%trim(%subst(vindta:85:35))
026500150521     C                   EVAL      VABLOD=%trim(%subst(vindta:120:25))
026600150521     C                   EVAL      VABPRD=%trim(%subst(vindta:154:2))
026700150521     C                   EVAL      VABRMA=%trim(%subst(vindta:35:15))
026800150526     C*
026900150526     C                   SELECT
027000150526     C                   WHEN      %subst(vindta:210:1) = 'C'
027100150526     C                   EVAL      VABTIC = *blank
027200150526     C                   WHEN      %subst(vindta:210:1) = 'M'
027300150526     C                   EVAL      VABTIC = 'BM'
027400150526     C                   WHEN      %subst(vindta:210:1) = 'N'
027500150526     C                   EVAL      VABTIC = 'BM'
027600150526     C                   WHEN      %subst(vindta:210:1) = 'R'
027700150526     C                   EVAL      VABTIC = 'CM'
027800150526     C                   OTHER
027900150526     C                   EVAL      VABTIC = *blank
028000150526     C                   ENDSL
028100150521     C*
028200150521     C* Reperimento campi NUMERICI
028300150521     C*
028400150521     C* NSP/RMN
028500150521     C                   EVAL      PiStr=%trim(%subst(vindta:35:5))
028600150521     C                   EXSR      CHKNUM
028700150521     C                   IF        PiInt=*on
028800150521     C                   Z-ADD     PiVal         VABNSP
028900150521     C                   Z-ADD     PiVal         VABRMN
029000150521     C                   ELSE
029100150521     C                   SETON                                        31
029200150521     C                   Z-ADD     *zeros        VABNSP
029300150521     C                   Z-ADD     1             VABRMN
029400150521     C                   EVAL      vinmsg = %trimr(vinmsg)
029500150521     C                             + ' ' + 'VABNSP VABRMN'
029600150521     C                   ENDIF
029700150521     C* CAD
029800150521     C                   EVAL      PiStr=%trim(%subst(vindta:145:5))
029900150521     C                   EXSR      CHKNUM
030000150521     C                   IF        PiInt=*on
030100150521     C                   Z-ADD     PiVal         Num5_0
030200150521     C                   MOVEL(p)  Num5_0        VABCAD
030300150521     C                   ELSE
030400150521     C                   SETON                                        32
030500150521     C                   EVAL      VABCAD = *zeros
030600150521     C                   EVAL      vinmsg = %trimr(vinmsg)
030700150521     C                             + ' ' + 'VABCAD'
030800150521     C                   ENDIF
030900150521     C* Reperisco la provincia dal CAP e dalla località
031000150521     C                   IF        VABCAD <> *blanks AND
031100150521     C                             VABPRD  = *blanks
031200150521     C                   CLEAR                   TISI95DS
031300150521     C                   EVAL      I95TCN = '3'
031400150521     C                   Z-ADD     datcor        I95DAT
031500150521     C                   EVAL      I95CAP = VABCAD
031600150521     C                   EVAL      I95LOC = VABLOD
031700150521     C                   EVAL      I95NAR = VABNZD
031800150521     C                   CALL      'TISI95R'
031900150521     C                   PARM                    TISI95DS
032000150521     C                   EVAL      VABPRD = O95PRV
032100150521     C                   ENDIF
032200150521     C* NCL
032300150521     C                   EVAL      PiStr=%trim(%subst(vindta:164:5))
032400150521     C                   EXSR      CHKNUM
032500150521     C                   IF        PiInt=*on
032600150521     C                   Z-ADD     PiVal         VABNCL
032700150521     C                   ELSE
032800150521     C                   SETON                                        32
032900150521     C                   Z-ADD     *zeros        VABNCL
033000150521     C                   EVAL      vinmsg = %trimr(vinmsg)
033100150521     C                             + ' ' + 'VABNCL'
033200150521     C                   ENDIF
033300150521     C* PKB
033400150521     C                   EVAL      PiStr=%trim(%subst(vindta:159:5))
033500150521     C                   EVAL      PiDecChr = '.'
033600150521     C                   EXSR      CHKNUM
033700150521     C                   IF        PiNum=*on
033800150521     C                   Z-ADD(H)  PiVal         VABPKB
033900150521     C                   ELSE
034000150521     C                   SETON                                        32
034100150521     C                   Z-ADD     *zeros        VABPKB
034200150521     C                   EVAL      vinmsg = %trimr(vinmsg)
034300150521     C                             + ' ' + 'VABPKB'
034400150521     C                   ENDIF
034500150521     C* VLB
034600150521     C                   EVAL      PiStr=%trim(%subst(vindta:184:6))
034700150521     C                   EVAL      PiDecChr = '.'
034800150521     C                   EXSR      CHKNUM
034900150521     C                   IF        PiNum=*on
035000150521     C                   EVAL(H)   PiVal = PiVal / 1000                         * gest. 3 decimali
035100150521     C                   Z-ADD     PiVal         VABVLB
035200150521     C                   ELSE
035300150521     C                   SETON                                        32
035400150521     C                   Z-ADD     *zeros        VABVLB
035500150521     C                   EVAL      vinmsg = %trimr(vinmsg)
035600150521     C                             + ' ' + 'VABVLB'
035700150521     C                   ENDIF
035800150526     C* CAS
035900150526     C                   IF        %trim(%subst(vindta:196:14)) <> *blanks AND
036000150526     C                             %trim(%subst(vindta:196:14)) <> *zeros
036100150526     C                   MOVEL     '1'           FlgCAS
036200150526     C                   EVAL      VABVCA = 'EUR'
036300150526     C                   EVAL      PiStr=%trim(%subst(vindta:196:14))
036400150526     C                   EXSR      CHKNUM
036500150526     C                   IF        PiNum=*on
036600150526     C                   EVAL      PiVal = PiVal / 100                          * gest. 2 decimali
036700150526     C                   Z-ADD     PiVal         VABCAS
036800150526     C                   ELSE
036900150526     C                   ADD       1             errore
037000150526     C                   Z-ADD     *zeros        VABCAS
037100150526     C                   EVAL      vinmsg = %trimr(vinmsg)
037200150526     C                             + ' ' + 'VABCAS'
037300150526     C                   ENDIF
037400150526     C                   ENDIF
037500010205     C*
037600010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
037700040802     C                   IF        FlgCAS <> '0'
037800150526     C***                MOVEL     'TM'          VABTIC
037900070102     C                   IF        VABCBO = '1'
038000010205     C                   EVAL      VABCBO = '4'
038100010205     C                   ELSE
038200070102     C                   EVAL      VABCBO = '6'
038300070102     C                   ENDIF
038400010205     C                   ENDIF
038500020305     C*
038600011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
038700011113     C                   EXSR      CHKIMPDIV
038800010202     C*
038900000801     C* Ebbene...
039000000801     C                   ADD       1             §CTRMO
039100010201     C                   IF        errore <> *zeros
039200000801     C                   ADD       1             §CTRNO
039300000801     C                   EVAL      vinflg = '2'
039400000801     C                   ELSE
039500010201     C                   ADD       1             §CTROKVB
039600000801     C                   ENDIF
039700000801     C*
039800000801     C                   ENDSR
039900070102     C*----------------------------------------------------*
040000070102     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
040100070102     C*----------------------------------------------------*
040200070102     C     EXEVATA       BEGSR
040300070102     C*
040400070102     C                   EXSR      INZVAR
040500070102     C                   EXSR      DEFCAM
040600070102     C*
040700070102     C                   EVAL      VATTRC='A'
040800150225     C                   EVAL      VATNOT = %trim(%subst(vindta:316:25))
040900070102     C                   exsr      wrivat                                       => scarico VAT
041000070102     C*
041100070102     C                   ENDSR
041200060307     C*----------------------------------------------------*
041300060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
041400060307     C*----------------------------------------------------*
041500060307     C     EXEVATB       BEGSR
041600060307     C*
041700060307     C                   EXSR      INZVAR
041800060307     C                   EXSR      DEFCAM
041900060307     C*
042000060307     C                   EVAL      VATTRC='B'
042100150225     C                   EVAL      VATNOT = %trim(%subst(vindta:376:15))
042200060307     C                   exsr      wrivat                                       => scarico VAT
042300060307     C*
042400060307     C                   ENDSR
042500070402     C*----------------------------------------------------*
042600070402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
042700070402     C*----------------------------------------------------*
042800070402     C     EXEVATE       BEGSR
042900070402     C*
043000070402     C                   EXSR      INZVAR
043100070402     C                   EXSR      DEFCAM
043200070402     C*
043300070402     C                   EVAL      VATTRC='E'
043400150225     C                   EVAL      VATNOT = %trim(%subst(vindta:554:25))
043500070402     C                   exsr      wrivat                                       => scarico VAT
043600070402     C*
043700070402     C                   ENDSR
043800140224     C*----------------------------------------------------*
043900140224     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I" e "J"
044000140224     C*----------------------------------------------------*
044100140224     C     EXEVATI       BEGSR
044200150225     C***
044300150225     C***                EXSR      INZVAR
044400150225     C***                EXSR      DEFCAM
044500150225     C***
044600150225     C***                EVAL      VATTRC='I'
044700150225     C***                EVAL      VATNOT = %trim(%subst(vindta:663:35))
044800150225     C***                exsr      wrivat                                       => scarico VAT
044900150225     C***
045000150225     C***                EVAL      VATTRC='J'
045100150225     C***                EVAL      VATNOT = %trim(%subst(vindta:663+35:15))
045200150225     C***                exsr      wrivat                                       => scarico VAT
045300150225     C***
045400140224     C                   ENDSR
045500140226     C*----------------------------------------------------*
045600140226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "S"
045700140226     C*----------------------------------------------------*
045800140226     C     EXEVATS       BEGSR
045900150225     C***
046000150225     C***                EXSR      INZVAR
046100150225     C***                EXSR      DEFCAM
046200150225     C***
046300150225     C***                EVAL      VATTRC='S'
046400150225     C***                EVAL      VATNOT=*blank
046500150225     C***                EVAL      %subst(VATNOT:1:14) =
046600150225     C***                           %trim(%subst(vindta:713:14))
046700150225     C***                IF        VATNOT <> *blank
046800150225     C***                EVAL      %subst(VATNOT:17:1) = 'S'
046900150225     C***                ENDIF
047000150225     C***                exsr      wrivat                                       => scarico VAT
047100150225     C***
047200140226     C                   ENDSR
047300010201     C*----------------------------------------------------*
047400040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
047500010201     C*----------------------------------------------------*
047600020305     C     WRIVAT        BEGSR
047700050628     C*
047800060223     C* Scrivo solo se valorizzato qualcosa
047900060223     C                   IF        VATNOT <> *blanks
048000040802     C                   WRITE     FIVAT000
048100060223     C                   ENDIF
048200010201     C*
048300010201     C                   ENDSR
048400010202     C*----------------------------------------------------*
048500021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
048600010202     C*----------------------------------------------------*
048700020305     C     PREVAT        BEGSR
048800010202     C*
048900021113     C* Compongo il nome del membro da dare al FIVATWWR
049000010202     C                   eval      parmbr = vlrhdl
049100010202     C                   movel     'M'           parmbr
049200050627     C                   eval      parccm = %subst(vlrKSC:2:7)
049300010202     C                   eval      paropz = '1'
049400010202     C* Effettuo la chiamata al CLLE preposto
049500040506     C                   call(e)   'TITVVTC'
049600010202     C                   parm                    parccm
049700010202     C                   parm                    parmbr
049800010202     C                   parm                    paropz
049900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050000010202     C                   if        %error
050100010202     C                   movel     '1'           chkcall
050200010202     C                   else
050300010202     C                   movel     '0'           chkcall
050400010202     C                   endif
050500010202     C*
050600010202     C                   ENDSR
050700000801     C*----------------------------------------------------*
050800000801     C*  CONTROLLO NUMERICITA' CAMPI
050900000801     C*----------------------------------------------------*
051000000801     C     CHKNUM        BEGSR
051100000801     C*
051200000801     C                   call(e)   'ISNUMERIC'
051300000801     C                   PARM                    PiStr            30
051400140226     C                   PARM      '.'           PiDecChr          1
051500000801     C                   PARM      *ZEROS        PiVal            30 9
051600000801     C                   PARM      '0'           PiInt             1
051700000801     C                   PARM      '0'           PiNum             1
051800000801     C                   IF        %error
051900000801     C                   EVAL      PiInt=*off
052000000801     C                   ENDIF
052100000801     C*
052200000801     C                   ENDSR
052300000801     C***
052400000801
052500011113
052600011113     C*----------------------------------------------------*
052700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
052800011113     C*----------------------------------------------------*
052900011113     C     CHKIMPDIV     BEGSR
053000011113     C*
053100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
053200011113     C                   Z-ADD     *zeros        wrkDec            9 9
053300011113     C*
053400011113     C* Come prima cosa effettuo considerazioni sulla divisa
053500011113     C                   IF        vabIAS > *zeros
053600011113     C                   IF        vabVAS <> 'EUR'
053700011113     C                   EVAL      vabVAS =  'ITL'
053800011113     C                   ENDIF
053900011113     C                   ENDIF
054000011113     C*
054100011113     C                   IF        vabCAS > *zeros
054200011113     C                   IF        vabVCA <> 'EUR'
054300011113     C                   EVAL      vabVCA =  'ITL'
054400011113     C                   ENDIF
054500011113     C                   ENDIF
054600011113     C*
054700011113     C                   IF        vabVMD > *zeros
054800020305     C                   IF        vabVAD <> 'EUR'
054900011113     C                   EVAL      vabVAD =  'ITL'
055000011113     C                   ENDIF
055100011113     C                   ENDIF
055200011113     C*
055300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
055400011113     C                   Z-ADD     vabIAS        wrkDec
055500011113     C                   IF        wrkDec > *zeros
055600011113     C                   IF        vabVAS = 'ITL'
055700011113     C                   EVAL      vabIAS = *zeros
055800011113     C                   ENDIF
055900011113     C                   ENDIF
056000011113     C*
056100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
056200011113     C                   Z-ADD     vabCAS        wrkDec
056300011113     C                   IF        wrkDec > *zeros
056400011113     C                   IF        vabVCA = 'ITL'
056500011113     C                   EVAL      vabCAS = *zeros
056600011113     C                   ENDIF
056700011113     C                   ENDIF
056800011113     C*
056900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
057000011113     C                   Z-ADD     vabVMD        wrkDec
057100011113     C                   IF        wrkDec > *zeros
057200011113     C                   IF        vabVAD = 'ITL'
057300011113     C                   EVAL      vabVMD = *zeros
057400011113     C                   ENDIF
057500011113     C                   ENDIF
057600011113     C*
057700011113     C                   ENDSR
057800011113     C***
057900011113
058000011113
058100000801
058200000801
058300990920      /TITLE Invio dei dati al punto operativo.
058400010202     C     invio         BEGSR
058500990920     C*
058600021113     C* 1° invio FIVAT
058700010201     C                   reset                   dscmz
058800010201     C                   move      vlrpoi        cmzdst
058900021113     C                   eval      cmzfld = 'FIVATWWR'
059000010201     C                   eval      cmzmbd = vlrhdl
059100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059200021009     C***                if        prmfir = *blanks
059300021113     C                   eval      cmzfla = 'FIVAT00F'
059400021113     C                   eval      cmzmba = 'FIVAT00F'
059500021009     C***                else
059600021009     C***                eval      cmzfla = prmfir
059700021009     C***                eval      cmzmba = prmfir
059800021009     C***                endif
059900010201     C                   eval      cmznrr = *zeros
060000020305     C                   move      §ctrokvt      cmznrr
060100021018     C                   eval      cmzlba = vlrfl1
060200010201     C                   call(e)   'TIS711C'
060300010201     C                   parm                    dscmz
060400010201     C                   parm      *blanks       esito
060500010205     C                   if        %error
060600010205     C                             or cmzerr = '1'
060700010205     C                             or esito  = '1'
060800010205     C                   eval      wrkesito = '3'
060900010205     C                   else
061000010201     C*
061100021113     C* 2° invio FIVAB
061200010201     C                   reset                   dscmz
061300010201     C                   move      vlrpoi        cmzdst
061400010201     C                   eval      cmzfld = vlrfou
061500010201     C                   eval      cmzmbd = vlrhdl
061600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
061700021009     C***                if        prmfir = *blanks
061800021113     C                   eval      cmzfla = 'FIVAB00F'
061900021113     C                   eval      cmzmba = 'FIVAB00F'
062000021009     C***                else
062100021009     C***                eval      cmzfla = prmfir
062200021009     C***                eval      cmzmba = prmfir
062300021009     C***                endif
062400010201     C                   eval      cmznrr = *zeros
062500010201     C                   move      §ctrokvb      cmznrr
062600021018     C                   eval      cmzlba = vlrfl1
062700010201     C                   call(e)   'TIS711C'
062800010201     C                   parm                    dscmz
062900010201     C                   parm      *blanks       esito
063000010201     C                   if        %error
063100010201     C                             or cmzerr = '1'
063200010201     C                             or esito  = '1'
063300010201     C                   eval      wrkesito = '3'
063400010201     C                   endif
063500010205     C                   endif
063600990920     C*
063700000613     C                   ENDSR
063800000613     C***
063900990910
064000000613     C     *inzsr        BEGSR
064100990910     C*
064200990910     C     *entry        plist
064300990920     C                   parm                    tivlrds
064400990921     C                   parm      wrkesito      esito
064500000724     C                   parm                    prmlit
064600000710     C                   parm                    prmfir
064700000613     C*
064800000830     C* CALCOLA LA DATA CORRENTE
064900100118     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
065000100118     C                   eval      datcor = %dec(%date() : *ISO)
065100000830     C*
065200000613     C                   ENDSR
065300000613     C***
