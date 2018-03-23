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
011700140403     C*
011800140403     C* Indirizzo e-mail
011900140403     C                   exsr      exevati
012000070402     C*
012100070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
012200070402     C                   exsr      exevate
012300070402     C*
012400070402     C* Salvo il raggruppamento spedizione cliente corrente
012500070402     C                   eval      depSped = curSped
012600070402     C*
012700070402     C                   else
012800070402     C*
012900070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
013000070402     C                   exsr      exevate
013100070402     C                   endif
013200000905     C*
013300000905     C                   else
013400000905     C                   eval      vinflg = '1'
013500050628     C                   endif
013600000905     C                   endif
013700000905     C*
013800000905     C  N70              update    tivin000
013900000905     C*
014000991022     C  N70              ENDdo
014100010202     C*
014200010202     C                   endif
014300990910
014400990910     C* Se non ci sono record con errori ...
014500000710     C                   if        §ctrno = 0
014600990910     C* ... restituisco esito OK.
014700990921     C                   eval      wrkesito = '0'
014800990910     C                   else
014900010201     C                   if        §ctrokvb > 0
015000990921     C                   eval      wrkesito = '1'
015100000710     C                   else
015200000710     C                   eval      wrkesito = '2'
015300990910     C                   endif
015400000710     C                   endif
015500990910     C*
015600990914     C                   if        %open(tivin00r)
015700990908     C                   close     tivin00r
015800990914     C                   endif
015900021113     C                   if        %open(fivabwwr)
016000021113     C                   close     fivabwwr
016100990914     C                   endif
016200021113     C                   if        %open(fivatwwr)
016300021113     C                   close     fivatwwr
016400010201     C                   endif
016500990910     C*
016600010201     C                   if        §ctrokvb > 0
016700000724     C                             and vlrpoi <> *zeros
016800010202     C                   exsr      invio
016900990920     C                   endif
017000990920     C*
017100910830     C                   ENDSR
017200000613     C***
017300010305
017400010305     C*----------------------------------------------------*
017500020305     C*  SCARICAMENTO BUFFER RECORDS VAB
017600010305     C*----------------------------------------------------*
017700090127     C     WRIVAB        BEGSR
017800090127     C*
017900090127     C* Quindi scarico il buffer del file d testata
018000090127     C                   write     fivab000                                     => scarico il VAB
018100010305     C*
018200010305     C                   ENDSR
018300990920
018400000801     C*----------------------------------------------------*
018500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
018600000801     C*----------------------------------------------------*
018700010201     C     INZVAR        BEGSR
018800000801     C*
018900040802     C                   Z-ADD     *zeros        Num5_0            5 0
019000040802     C                   MOVEL     '0'           FlgCAS            1
019100000801     C*
019200000801     C                   ENDSR
019300000801     C*----------------------------------------------------*
019400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
019500000801     C*----------------------------------------------------*
019600000801     C     DEFCAM        BEGSR
019700070718     C*
019800070718     C* Imposto i valori di default...
019900100622     C                   Z-ADD     0400956       VABCCM
020000100622     C                   Z-ADD     0400956       VATCCM
020100100622     C                   Z-ADD     040           VABLNP
020200100622     C                   Z-ADD     040           VATLNP
020300091119     C                   Z-ADD     000           VABCTR
020400070718     C                   MOVEL     '1'           VABCBO
020500070718     C                   MOVEL     '7Q'          VABCTM
020600020619     C* ... e poi verifico se sono stati passati come parametri
020700020619     C                   IF        vlrppt > *blanks
020800040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
020900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
021000020619     C                   EXSR      CHKNUM
021100020619     C                   IF        PiInt=*on
021200020619     C                   Z-ADD     PiVal         VABCCM
021300020619     C                   Z-ADD     PiVal         VATCCM
021400020619     C                   ENDIF
021500040506     C                   ENDIF
021600040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
021700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
021800020619     C                   EXSR      CHKNUM
021900020619     C                   IF        PiInt=*on
022000020619     C                   Z-ADD     PiVal         VABLNP
022100020619     C                   Z-ADD     PiVal         VATLNP
022200040506     C                   ENDIF
022300020619     C                   ENDIF
022400040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
022500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
022600020619     C                   EXSR      CHKNUM
022700020619     C                   IF        PiInt=*on
022800020619     C                   Z-ADD     PiVal         VABCTR
022900040506     C                   ENDIF
023000020619     C                   ENDIF
023100020619     C                   ENDIF
023200000801     C*
023300000801     C                   ENDSR
023400000801     C*----------------------------------------------------*
023500021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
023600000801     C*----------------------------------------------------*
023700040823     C     IMPVAB        BEGSR
023800040823     C*
023900020305     C                   EXSR      INZVAR
024000020305     C                   EXSR      DEFCAM
024100010305     C*
024200000801     C                   Z-ADD     *zeros        errore            1 0
024300000830     C                   MOVEL     datcor        VABAAS
024400020305     C                   MOVEL     datcor        VATAAS
024500040526     C                   MOVE      datcor        VABMGS
024600040823     C                   MOVE(P)   vlrpoi        VABFGS
024700040823     C                   MOVE(P)   vlrpoi        VATFGS
024800050628     C*
024900070102     C                   EVAL      VABRSD=%trim(%subst(vindta:276:40))
025000020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
025100020117     C     '@':'A'       XLATE     VABRSD        VABRSD
025200020117     C* ==
025300130111     C* nei primi 7 byte di ogni record c'è il codice cliente da utilizzare
025400130111     C                   IF        %subst(vindta:1:7) <> *blanks
025500130111     C                   EVAL      PiStr=%trim(%subst(vindta:1:7))
025600130111     C                   EXSR      CHKNUM
025700130111     C                   IF        PiInt=*on
025800130111     C                   Z-ADD     PiVal         VABCCM
025900130111     C                   ENDIF
026000130111     C                   ENDIF
026100130111     C*
026200070719     C                   EVAL      VABRD2=%trim(%subst(vindta:316:20))
026300090427     C                   EVAL      VABIND=%trim(%trim(%subst(vindta:336:5))+' '+
026400070102     C                                    %trim(%subst(vindta:341:30))+' '+
026500090427     C                                    %trim(%subst(vindta:371:5)))
026600100924     C                   EVAL      VABCAD=%trim(%subst(vindta:390:9))
026700070102     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
026800070102     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
026900090511     C                   EVAL      VABRMA=%trim(%subst(vindta:46:25))
027000070102     C                   EVAL      VABNOT=%trim(%subst(vindta:514:30))
027100070102     C                   IF        %subst(vindta:43:1) = 'N'
027200070102     C                   EVAL      VABCBO='1'
027300070102     C                   ENDIF
027400070102     C                   IF        %subst(vindta:43:1) = 'P'
027500070102     C                   EVAL      VABCBO='2'
027600070102     C                   ENDIF
027700120424     C***                IF        %subst(vindta:511:3) = 'ABM'
027800120424     C***                EVAL      VABTIC='BM'
027900120424     C***                ENDIF
028000120424     C***                IF        %subst(vindta:511:3) = 'ABS'
028100120424     C***                EVAL      VABTIC='B'
028200120424     C***                ENDIF
028300120424     C***                IF        %subst(vindta:511:3) = 'CON'
028400120424     C***                EVAL      VABTIC=*blanks
028500120424     C***                ENDIF
028600120424     C***                IF        %subst(vindta:511:3) = 'ACM'
028700120424     C***                EVAL      VABTIC='CM'
028800120424     C***                ENDIF
028900120424     C***                IF        %subst(vindta:511:3) = 'ACS'
029000120424     C***                EVAL      VABTIC='CB'
029100120424     C***                ENDIF
029200120424     C***                IF        %subst(vindta:511:3) = 'VAR'
029300120424     C***                EVAL      VABTIC=*blanks
029400120424     C***                ENDIF
029500040506     C* Reperisco la provincia dal CAP e dalla località
029600040526     C                   IF        VABCAD <> *blanks AND
029700040526     C                             VABPRD  = *blanks
029800040506     C                   CLEAR                   TISI95DS
029900040506     C                   EVAL      I95TCN = '3'
030000040506     C                   Z-ADD     datcor        I95DAT
030100040506     C                   EVAL      I95CAP = VABCAD
030200040506     C                   EVAL      I95LOC = VABLOD
030300050627     C                   EVAL      I95NAR = VABNZD
030400040506     C                   CALL      'TISI95R'
030500040506     C                   PARM                    TISI95DS
030600040506     C                   EVAL      VABPRD = O95PRV
030700040506     C                   ENDIF
030800060307     C* NCL
030900070102     C                   EVAL      PiStr=%trim(%subst(vindta:432:3))
031000060307     C                   EXSR      CHKNUM
031100060307     C                   IF        PiInt=*on
031200060307     C                   ADD       PiVal         VABNCL
031300060307     C                   ELSE
031400060307     C                   ADD       1             errore
031500060307     C                   EVAL      vinmsg = %trimr(vinmsg)
031600060307     C                             + ' ' + 'VABNCL'
031700060307     C                   ENDIF
031800040506     C* PKB
031900110729     C                   EVAL      VABPKB = 2
032000110729     C***                EVAL      PiStr=%trim(%subst(vindta:435:7))
032100110729     C***                EXSR      CHKNUM
032200110729     C***                IF        PiNum=*on
032300110729     C***                EVAL      PiVal=PiVal/1000                             da grammi a kg
032400110729     C***                Z-ADD     PiVal         VABPKB
032500110729     C***                ELSE
032600110729     C***                ADD       1             errore
032700110729     C***                EVAL      vinmsg = %trimr(vinmsg)
032800110729     C***                          + ' ' + 'VABPKB'
032900110729     C***                ENDIF
033000100118     C* NSP => Stacco un numeratore da AZNUM
033100100118     C                   clear                   TRUL33DS
033200100118     C                   eval      I33OPE = *zeros
033300100118     C                   eval      I33CNU = 302
033400100118     C                   eval      I33NUM = 1
033500100118     C                   movel     TRUL33DS      KPJBU
033600100118     C                   call      'TRUL33R'
033700100118     C                   parm                    KPJBA
033800100118     C                   movel     KPJBU         TRUL33DS
033900100118     C                   if        O33ERR = *zeros
034000100118     C                   z-add     O33NRF        VABNSP
034100100118     C                   z-add     O33NRF        VATNSP
034200100118     C                   else
034300100118     C                   ADD       1             errore
034400100118     C                   EVAL      vinmsg = %trimr(vinmsg)
034500100118     C                             + ' ' + 'VABNSP VATNSP'
034600100118     C                   endif
034700090511     C* RMN
034800091210     C***                EVAL      PiStr=%trim(%subst(vindta:13:30))
034900100118     C                   EVAL      PiStr=%trim(%subst(vindta:47:24))
035000090511     C                   EXSR      CHKNUM
035100090511     C                   IF        PiInt=*on
035200090511     C                   Z-ADD     PiVal         VABRMN
035300090511     C                   ELSE
035400090511     C                   ADD       1             errore
035500090511     C                   EVAL      vinmsg = %trimr(vinmsg)
035600090511     C                             + ' ' + 'VABRMN'
035700090511     C                   ENDIF
035800060307     C* CAS
035900090127     C                   IF        %trim(%subst(vindta:502:9))<>'000000.00' AND
036000100921     C                             %trim(%subst(vindta:502:9))<>'0.00'      AND
036100090608     C                             %trim(%subst(vindta:502:9))<>*blanks     AND
036200090608     C                             %trim(%subst(vindta:502:9))<>*zeros
036300070102     C                   EVAL      VABVCA = 'EUR'
036400060307     C                   EVAL      FlgCAS = '1'
036500070102     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
036600060307     C                   EXSR      CHKNUM
036700060307     C                   IF        PiNum=*on
036800060316     C                   Z-ADD     PiVal         VABCAS
036900060307     C                   ELSE
037000060307     C                   ADD       1             errore
037100060307     C                   EVAL      vinmsg = %trimr(vinmsg)
037200060307     C                             + ' ' + 'VABCAS'
037300060307     C                   ENDIF
037400070102     C                   ENDIF
037500060316     C* IAS
037600070719     C                   IF        %trim(%subst(vindta:544:10)) <> *blanks
037700070102     C                   EVAL      VABVAS = 'EUR'
037800070102     C                   EVAL      PiStr=%trim(%subst(vindta:544:10))
037900060316     C                   EXSR      CHKNUM
038000060316     C                   IF        PiNum=*on
038100060316     C                   Z-ADD     PiVal         VABIAS
038200060316     C                   ELSE
038300060316     C                   ADD       1             errore
038400060316     C                   EVAL      vinmsg = %trimr(vinmsg)
038500060316     C                             + ' ' + 'VABIAS'
038600070102     C                   ENDIF
038700060316     C                   ENDIF
038800010205     C*
038900010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
039000040802     C                   IF        FlgCAS <> '0'
039100070102     C                   IF        VABCBO = '1'
039200010205     C                   EVAL      VABCBO = '4'
039300010205     C                   ELSE
039400070102     C                   EVAL      VABCBO = '6'
039500070102     C                   ENDIF
039600010205     C                   ENDIF
039700020305     C*
039800011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
039900011113     C                   EXSR      CHKIMPDIV
040000010202     C*
040100000801     C* Ebbene...
040200000801     C                   ADD       1             §CTRMO
040300010201     C                   IF        errore <> *zeros
040400000801     C                   ADD       1             §CTRNO
040500000801     C                   EVAL      vinflg = '2'
040600000801     C                   ELSE
040700010201     C                   ADD       1             §CTROKVB
040800000801     C                   ENDIF
040900000801     C*
041000000801     C                   ENDSR
041100070102     C*----------------------------------------------------*
041200070102     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
041300070102     C*----------------------------------------------------*
041400070102     C     EXEVATA       BEGSR
041500070102     C*
041600070102     C                   EXSR      INZVAR
041700070102     C                   EXSR      DEFCAM
041800070102     C*
041900130111     C* nei primi 7 byte di ogni record c'è il codice cliente da utilizzare
042000130111     C                   IF        %subst(vindta:1:7) <> *blanks
042100130111     C                   EVAL      PiStr=%trim(%subst(vindta:1:7))
042200130111     C                   EXSR      CHKNUM
042300130111     C                   IF        PiInt=*on
042400130111     C                   Z-ADD     PiVal         VATCCM
042500130111     C                   ENDIF
042600130111     C                   ENDIF
042700070102     C                   EVAL      VATTRC='A'
042800090127     C                   EVAL      VATNOT = %trim(%subst(vindta:581:30))
042900070102     C                   exsr      wrivat                                       => scarico VAT
043000070102     C*
043100070102     C                   ENDSR
043200060307     C*----------------------------------------------------*
043300060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
043400060307     C*----------------------------------------------------*
043500060307     C     EXEVATB       BEGSR
043600060307     C*
043700060307     C                   EXSR      INZVAR
043800060307     C                   EXSR      DEFCAM
043900060307     C*
044000130111     C* nei primi 7 byte di ogni record c'è il codice cliente da utilizzare
044100130111     C                   IF        %subst(vindta:1:7) <> *blanks
044200130111     C                   EVAL      PiStr=%trim(%subst(vindta:1:7))
044300130111     C                   EXSR      CHKNUM
044400130111     C                   IF        PiInt=*on
044500130111     C                   Z-ADD     PiVal         VATCCM
044600130111     C                   ENDIF
044700130111     C                   ENDIF
044800060307     C                   EVAL      VATTRC='B'
044900070102     C                   EVAL      VATNOT = %trim(%subst(vindta:376:15))
045000060307     C                   exsr      wrivat                                       => scarico VAT
045100060307     C*
045200060307     C                   ENDSR
045300140403     C*----------------------------------------------------*
045400140403     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I" e "J"
045500140403     C*----------------------------------------------------*
045600140403     C     EXEVATI       BEGSR
045700140403     C*
045800140403     C                   EXSR      INZVAR
045900140403     C                   EXSR      DEFCAM
046000140403     C*
046100140403     C                   EVAL      VATTRC='I'
046200140403     C                   EVAL      VATNOT = %xlate(',':';':
046300140403     C                                       %trim(%subst(vindta:663:35)))
046400140403     C                   exsr      wrivat                                       => scarico VAT
046500140403     C*
046600140403     C                   EVAL      VATTRC='J'
046700140403     C                   EVAL      VATNOT = %xlate(',':';':
046800140403     C                                       %trim(%subst(vindta:663+35:15)))
046900140403     C                   exsr      wrivat                                       => scarico VAT
047000140403     C*
047100140403     C                   ENDSR
047200070402     C*----------------------------------------------------*
047300070402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
047400070402     C*----------------------------------------------------*
047500070402     C     EXEVATE       BEGSR
047600070402     C*
047700070402     C                   EXSR      INZVAR
047800070402     C                   EXSR      DEFCAM
047900070402     C*
048000130111     C* nei primi 7 byte di ogni record c'è il codice cliente da utilizzare
048100130111     C                   IF        %subst(vindta:1:7) <> *blanks
048200130111     C                   EVAL      PiStr=%trim(%subst(vindta:1:7))
048300130111     C                   EXSR      CHKNUM
048400130111     C                   IF        PiInt=*on
048500130111     C                   Z-ADD     PiVal         VATCCM
048600130111     C                   ENDIF
048700130111     C                   ENDIF
048800070402     C                   EVAL      VATTRC='E'
048900110128     C                   EVAL      VATNOT = %trim(%subst(vindta:554:25))
049000110128     C***                EVAL      VATNOT = %trim(%subst(vindta:554:24))
049100070402     C                   exsr      wrivat                                       => scarico VAT
049200070402     C*
049300070402     C                   ENDSR
049400010201     C*----------------------------------------------------*
049500040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
049600010201     C*----------------------------------------------------*
049700020305     C     WRIVAT        BEGSR
049800050628     C*
049900060223     C* Scrivo solo se valorizzato qualcosa
050000060223     C                   IF        VATNOT <> *blanks
050100040802     C                   WRITE     FIVAT000
050200060223     C                   ENDIF
050300010201     C*
050400010201     C                   ENDSR
050500010202     C*----------------------------------------------------*
050600021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
050700010202     C*----------------------------------------------------*
050800020305     C     PREVAT        BEGSR
050900010202     C*
051000021113     C* Compongo il nome del membro da dare al FIVATWWR
051100010202     C                   eval      parmbr = vlrhdl
051200010202     C                   movel     'M'           parmbr
051300050627     C                   eval      parccm = %subst(vlrKSC:2:7)
051400010202     C                   eval      paropz = '1'
051500010202     C* Effettuo la chiamata al CLLE preposto
051600040506     C                   call(e)   'TITVVTC'
051700010202     C                   parm                    parccm
051800010202     C                   parm                    parmbr
051900010202     C                   parm                    paropz
052000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
052100010202     C                   if        %error
052200010202     C                   movel     '1'           chkcall
052300010202     C                   else
052400010202     C                   movel     '0'           chkcall
052500010202     C                   endif
052600010202     C*
052700010202     C                   ENDSR
052800000801     C*----------------------------------------------------*
052900000801     C*  CONTROLLO NUMERICITA' CAMPI
053000000801     C*----------------------------------------------------*
053100000801     C     CHKNUM        BEGSR
053200000801     C*
053300000801     C                   call(e)   'ISNUMERIC'
053400000801     C                   PARM                    PiStr            30
053500070102     C                   PARM      '.'           PiDecChr          1
053600000801     C                   PARM      *ZEROS        PiVal            30 9
053700000801     C                   PARM      '0'           PiInt             1
053800000801     C                   PARM      '0'           PiNum             1
053900000801     C                   IF        %error
054000000801     C                   EVAL      PiInt=*off
054100000801     C                   ENDIF
054200000801     C*
054300000801     C                   ENDSR
054400000801     C***
054500000801
054600011113
054700011113     C*----------------------------------------------------*
054800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
054900011113     C*----------------------------------------------------*
055000011113     C     CHKIMPDIV     BEGSR
055100011113     C*
055200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
055300011113     C                   Z-ADD     *zeros        wrkDec            9 9
055400011113     C*
055500011113     C* Come prima cosa effettuo considerazioni sulla divisa
055600011113     C                   IF        vabIAS > *zeros
055700011113     C                   IF        vabVAS <> 'EUR'
055800011113     C                   EVAL      vabVAS =  'ITL'
055900011113     C                   ENDIF
056000011113     C                   ENDIF
056100011113     C*
056200011113     C                   IF        vabCAS > *zeros
056300011113     C                   IF        vabVCA <> 'EUR'
056400011113     C                   EVAL      vabVCA =  'ITL'
056500011113     C                   ENDIF
056600011113     C                   ENDIF
056700011113     C*
056800011113     C                   IF        vabVMD > *zeros
056900020305     C                   IF        vabVAD <> 'EUR'
057000011113     C                   EVAL      vabVAD =  'ITL'
057100011113     C                   ENDIF
057200011113     C                   ENDIF
057300011113     C*
057400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
057500011113     C                   Z-ADD     vabIAS        wrkDec
057600011113     C                   IF        wrkDec > *zeros
057700011113     C                   IF        vabVAS = 'ITL'
057800011113     C                   EVAL      vabIAS = *zeros
057900011113     C                   ENDIF
058000011113     C                   ENDIF
058100011113     C*
058200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
058300011113     C                   Z-ADD     vabCAS        wrkDec
058400011113     C                   IF        wrkDec > *zeros
058500011113     C                   IF        vabVCA = 'ITL'
058600011113     C                   EVAL      vabCAS = *zeros
058700011113     C                   ENDIF
058800011113     C                   ENDIF
058900011113     C*
059000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
059100011113     C                   Z-ADD     vabVMD        wrkDec
059200011113     C                   IF        wrkDec > *zeros
059300011113     C                   IF        vabVAD = 'ITL'
059400011113     C                   EVAL      vabVMD = *zeros
059500011113     C                   ENDIF
059600011113     C                   ENDIF
059700011113     C*
059800011113     C                   ENDSR
059900011113     C***
060000011113
060100011113
060200000801
060300000801
060400990920      /TITLE Invio dei dati al punto operativo.
060500010202     C     invio         BEGSR
060600990920     C*
060700021113     C* 1° invio FIVAT
060800010201     C                   reset                   dscmz
060900010201     C                   move      vlrpoi        cmzdst
061000021113     C                   eval      cmzfld = 'FIVATWWR'
061100010201     C                   eval      cmzmbd = vlrhdl
061200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
061300021009     C***                if        prmfir = *blanks
061400021113     C                   eval      cmzfla = 'FIVAT00F'
061500021113     C                   eval      cmzmba = 'FIVAT00F'
061600021009     C***                else
061700021009     C***                eval      cmzfla = prmfir
061800021009     C***                eval      cmzmba = prmfir
061900021009     C***                endif
062000010201     C                   eval      cmznrr = *zeros
062100020305     C                   move      §ctrokvt      cmznrr
062200021018     C                   eval      cmzlba = vlrfl1
062300010201     C                   call(e)   'TIS711C'
062400010201     C                   parm                    dscmz
062500010201     C                   parm      *blanks       esito
062600010205     C                   if        %error
062700010205     C                             or cmzerr = '1'
062800010205     C                             or esito  = '1'
062900010205     C                   eval      wrkesito = '3'
063000010205     C                   else
063100010201     C*
063200021113     C* 2° invio FIVAB
063300010201     C                   reset                   dscmz
063400010201     C                   move      vlrpoi        cmzdst
063500010201     C                   eval      cmzfld = vlrfou
063600010201     C                   eval      cmzmbd = vlrhdl
063700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063800021009     C***                if        prmfir = *blanks
063900021113     C                   eval      cmzfla = 'FIVAB00F'
064000021113     C                   eval      cmzmba = 'FIVAB00F'
064100021009     C***                else
064200021009     C***                eval      cmzfla = prmfir
064300021009     C***                eval      cmzmba = prmfir
064400021009     C***                endif
064500010201     C                   eval      cmznrr = *zeros
064600010201     C                   move      §ctrokvb      cmznrr
064700021018     C                   eval      cmzlba = vlrfl1
064800010201     C                   call(e)   'TIS711C'
064900010201     C                   parm                    dscmz
065000010201     C                   parm      *blanks       esito
065100010201     C                   if        %error
065200010201     C                             or cmzerr = '1'
065300010201     C                             or esito  = '1'
065400010201     C                   eval      wrkesito = '3'
065500010201     C                   endif
065600010205     C                   endif
065700990920     C*
065800000613     C                   ENDSR
065900000613     C***
066000990910
066100000613     C     *inzsr        BEGSR
066200990910     C*
066300990910     C     *entry        plist
066400990920     C                   parm                    tivlrds
066500990921     C                   parm      wrkesito      esito
066600000724     C                   parm                    prmlit
066700000710     C                   parm                    prmfir
066800000613     C*
066900000830     C* CALCOLA LA DATA CORRENTE
067000100118     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
067100100118     C                   eval      datcor = %dec(%date() : *ISO)
067200000830     C*
067300000613     C                   ENDSR
067400000613     C***
