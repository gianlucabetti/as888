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
002700160331     D w70             s             70    INZ(*blanks)
002800100118
002900100118     D*------------------
003000100118     D* DS REPERIMENTO NUMERATORE
003100100118     D*------------------
003200100118     D trul33ds      e ds                  inz
003300100118     D*------------------
003400100118     D* DS ARCHITETTURA
003500100118     D*------------------
003600100118     D kpjba         e ds                  inz
003700100118
003800010201
003900010201
004000090127     C*
004100000913     C                   reset                   rrnum
004200990921     C                   reset                   esito
004300990921     C                   reset                   wrkesito
004400000613     C*
004500040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004600000613     C*
004700010202     C* Effettuo la chiamata al CLLE preposto
004800040506     C                   call(e)   'TITVVTC'
004900010202     C                   parm                    parccm
005000010202     C                   parm                    parmbr
005100010202     C                   parm      '2'           paropz
005200050201     C*
005300050201     C* Effettuo lancio TISI95 solo x chiusura
005400050201     C                   CLEAR                   TISI95DS
005500050201     C                   EVAL      I95TLA = 'C'
005600050201     C                   CALL      'TISI95R'
005700050201     C                   PARM                    TISI95DS
005800000616     C*
005900000801     C
006000010201     C                   seton                                        LR
006100990908
006200000801
006300910830     C*--------------------------------------------------------
006400040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
006500910830     C*--------------------------------------------------------
006600040526     C     RWFILE        BEGSR
006700990910     C*
006800990914     C                   if        not %open(tivin00r)
006900990908     C                   open      tivin00r
007000990914     C                   endif
007100021113     C                   if        not %open(fivabwwr)
007200021113     C                   open      fivabwwr
007300990914     C                   endif
007400021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007500020305     C                   exsr      prevat
007600010201     C*
007700010202     C                   if        chkcall = '0'
007800010202     C*
007900021113     C                   if        not %open(fivatwwr)
008000021113     C                   open      fivatwwr
008100010201     C                   endif
008200990910     C*
008300010201     C                   clear                   §CTROKVB          5 0
008400020305     C                   clear                   §CTROKVT          5 0
008500000801     C                   clear                   §CTRMO            5 0
008600000801     C                   clear                   §CTRNO            5 0
008700990910     C*
008800921023     C                   DO        *HIVAL
008900990913     C*
009000990915     C                   READ      tivin00r                               70
009100050627     C                   if        vindta > *blanks
009200000613     C                   add       1             rrnum
009300000801     C*
009400000801     C                   if        *in70 = *off
009500000801     C                             and
009600000801     C                             (vinflg = *blanks
009700000801     C                              or vinflg = '0'
009800000801     C                              or vinflg = '2')
009900000801     C*
010000000801     C                   clear                   vinmsg
010100000801     C                   eval      vinflg = '1'
010200070402     C*
010300070402     C* Verifico la rottura d codice x raggruppamento spedizione cliente
010400070402     C                   eval      curSped = %subst(vindta:46:25)
010500070402     C                   if        curSped <> depSped
010600070402     C                   clear                   fivab000
010700070402     C                   clear                   fivat000
010800050628     C*
010900060315     C                   exsr      impvab                                       => carico  VAB
011000090127     C*
011100060315     C                   exsr      wrivab                                       => scarico VAB
011200070402     C*
011300070402     C* Scrivo anche estensione dettaglio bolle - tipo record "A" => REFERENTE CONSEGNA
011400070402     C                   exsr      exevata
011500070402     C*
011600070402     C* Scrivo anche estensione dettaglio bolle - tipo record "B" => TELEFONO DESTINATARIO
011700070402     C                   exsr      exevatb
011800070402     C*
011900070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
012000070402     C                   exsr      exevate
012100070402     C*
012200070402     C* Salvo il raggruppamento spedizione cliente corrente
012300070402     C                   eval      depSped = curSped
012400070402     C*
012500070402     C                   else
012600070402     C*
012700070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
012800070402     C                   exsr      exevate
012900070402     C                   endif
013000000905     C*
013100000905     C                   else
013200000905     C                   eval      vinflg = '1'
013300050628     C                   endif
013400000905     C                   endif
013500000905     C*
013600000905     C  N70              update    tivin000
013700000905     C*
013800991022     C  N70              ENDdo
013900010202     C*
014000010202     C                   endif
014100990910
014200990910     C* Se non ci sono record con errori ...
014300000710     C                   if        §ctrno = 0
014400990910     C* ... restituisco esito OK.
014500990921     C                   eval      wrkesito = '0'
014600990910     C                   else
014700010201     C                   if        §ctrokvb > 0
014800990921     C                   eval      wrkesito = '1'
014900000710     C                   else
015000000710     C                   eval      wrkesito = '2'
015100990910     C                   endif
015200000710     C                   endif
015300990910     C*
015400990914     C                   if        %open(tivin00r)
015500990908     C                   close     tivin00r
015600990914     C                   endif
015700021113     C                   if        %open(fivabwwr)
015800021113     C                   close     fivabwwr
015900990914     C                   endif
016000021113     C                   if        %open(fivatwwr)
016100021113     C                   close     fivatwwr
016200010201     C                   endif
016300990910     C*
016400010201     C                   if        §ctrokvb > 0
016500000724     C                             and vlrpoi <> *zeros
016600010202     C                   exsr      invio
016700990920     C                   endif
016800990920     C*
016900910830     C                   ENDSR
017000000613     C***
017100010305
017200010305     C*----------------------------------------------------*
017300020305     C*  SCARICAMENTO BUFFER RECORDS VAB
017400010305     C*----------------------------------------------------*
017500090127     C     WRIVAB        BEGSR
017600090127     C*
017700090127     C* Quindi scarico il buffer del file d testata
017800090127     C                   write     fivab000                                     => scarico il VAB
017900010305     C*
018000010305     C                   ENDSR
018100990920
018200000801     C*----------------------------------------------------*
018300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
018400000801     C*----------------------------------------------------*
018500010201     C     INZVAR        BEGSR
018600000801     C*
018700040802     C                   Z-ADD     *zeros        Num5_0            5 0
018800040802     C                   MOVEL     '0'           FlgCAS            1
018900000801     C*
019000000801     C                   ENDSR
019100000801     C*----------------------------------------------------*
019200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
019300000801     C*----------------------------------------------------*
019400000801     C     DEFCAM        BEGSR
019500070718     C*
019600070718     C* Imposto i valori di default...
019700091119     C                   Z-ADD     1160914       VABCCM
019800091119     C                   Z-ADD     1160914       VATCCM
019900091119     C                   Z-ADD     116           VABLNP
020000091119     C                   Z-ADD     116           VATLNP
020100091119     C                   Z-ADD     000           VABCTR
020200070718     C                   MOVEL     '1'           VABCBO
020300070718     C                   MOVEL     '7Q'          VABCTM
020400020619     C* ... e poi verifico se sono stati passati come parametri
020500020619     C                   IF        vlrppt > *blanks
020600040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
020700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
020800020619     C                   EXSR      CHKNUM
020900020619     C                   IF        PiInt=*on
021000020619     C                   Z-ADD     PiVal         VABCCM
021100020619     C                   Z-ADD     PiVal         VATCCM
021200020619     C                   ENDIF
021300040506     C                   ENDIF
021400040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
021500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
021600020619     C                   EXSR      CHKNUM
021700020619     C                   IF        PiInt=*on
021800020619     C                   Z-ADD     PiVal         VABLNP
021900020619     C                   Z-ADD     PiVal         VATLNP
022000040506     C                   ENDIF
022100020619     C                   ENDIF
022200040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
022300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
022400020619     C                   EXSR      CHKNUM
022500020619     C                   IF        PiInt=*on
022600020619     C                   Z-ADD     PiVal         VABCTR
022700040506     C                   ENDIF
022800020619     C                   ENDIF
022900020619     C                   ENDIF
023000000801     C*
023100000801     C                   ENDSR
023200000801     C*----------------------------------------------------*
023300021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
023400000801     C*----------------------------------------------------*
023500040823     C     IMPVAB        BEGSR
023600040823     C*
023700020305     C                   EXSR      INZVAR
023800020305     C                   EXSR      DEFCAM
023900010305     C*
024000000801     C                   Z-ADD     *zeros        errore            1 0
024100000830     C                   MOVEL     datcor        VABAAS
024200020305     C                   MOVEL     datcor        VATAAS
024300040526     C                   MOVE      datcor        VABMGS
024400040823     C                   MOVE(P)   vlrpoi        VABFGS
024500040823     C                   MOVE(P)   vlrpoi        VATFGS
024600050628     C*
024700070102     C                   EVAL      VABRSD=%trim(%subst(vindta:276:40))
024800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
024900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
025000020117     C* ==
025100070719     C                   EVAL      VABRD2=%trim(%subst(vindta:316:20))
025200090427     C                   EVAL      VABIND=%trim(%trim(%subst(vindta:336:5))+' '+
025300070102     C                                    %trim(%subst(vindta:341:30))+' '+
025400090427     C                                    %trim(%subst(vindta:371:5)))
025500070102     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
025600070102     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
025700090511     C                   EVAL      VABRMA=%trim(%subst(vindta:46:25))
025800160331     C***                EVAL      VABNOT=%trim(%subst(vindta:514:30))
025900160331     C                   EVAL      w70=%trim(%subst(vindta:110:50))
026000160331     C                   EVAL      VABNOT=%subst(w70:1:35)
026100160331     C                   EVAL      VABNT2=%subst(w70:36:15)
026200160331     C                   SELECT
026300160331     C                   WHEN      %subst(vindta:160:1)='A'
026400160331     C                   EVAL      VABTC1='A'
026500160331     C                   ENDSL
026600160331     C                   SELECT
026700160331     C                   WHEN      %subst(vindta:161:1)=*blank
026800160331     C                   EVAL      VABTSP=%subst(vindta:161:1)
026900160331     C                   WHEN      %subst(vindta:161:1)='E'
027000160331     C                   EVAL      VABTSP=%subst(vindta:161:1)
027100160331     C                   WHEN      %subst(vindta:161:1)='H'
027200160331     C                   EVAL      VABTSP=%subst(vindta:161:1)
027300160331     C                   ENDSL
027400070102     C                   IF        %subst(vindta:43:1) = 'N'
027500070102     C                   EVAL      VABCBO='1'
027600070102     C                   ENDIF
027700070102     C                   IF        %subst(vindta:43:1) = 'P'
027800070102     C                   EVAL      VABCBO='2'
027900070102     C                   ENDIF
028000070102     C                   IF        %subst(vindta:511:3) = 'ABM'
028100070102     C                   EVAL      VABTIC='BM'
028200070102     C                   ENDIF
028300070102     C                   IF        %subst(vindta:511:3) = 'ABS'
028400070102     C                   EVAL      VABTIC='B'
028500070102     C                   ENDIF
028600070102     C                   IF        %subst(vindta:511:3) = 'CON'
028700070102     C                   EVAL      VABTIC=*blanks
028800070102     C                   ENDIF
028900070102     C                   IF        %subst(vindta:511:3) = 'ACM'
029000070102     C                   EVAL      VABTIC='CM'
029100070102     C                   ENDIF
029200100610     C                   IF        %subst(vindta:511:3) = 'VAR'
029300100610     C                   EVAL      VABTIC='BP'
029400100610     C                   ENDIF
029500120906     C                   IF        %subst(vindta:511:3) = 'TM '
029600120906     C                   EVAL      VABTIC='TM'
029700120906     C                   ENDIF
029800050628     C* CAP
029900070719     C                   EVAL      PiStr=%trim(%subst(vindta:391:5))
030000010201     C                   EXSR      CHKNUM
030100010201     C                   IF        PiInt=*on
030200010201     C                   Z-ADD     PiVal         Num5_0
030300040506     C                   MOVEL(P)  Num5_0        VABCAD
030400010201     C                   ELSE
030500040506     C                   ADD       1             errore
030600010201     C                   EVAL      vinmsg = %trimr(vinmsg)
030700040506     C                             + ' ' + 'VABCAD'
030800010201     C                   ENDIF
030900040506     C* Reperisco la provincia dal CAP e dalla località
031000040526     C                   IF        VABCAD <> *blanks AND
031100040526     C                             VABPRD  = *blanks
031200040506     C                   CLEAR                   TISI95DS
031300040506     C                   EVAL      I95TCN = '3'
031400040506     C                   Z-ADD     datcor        I95DAT
031500040506     C                   EVAL      I95CAP = VABCAD
031600040506     C                   EVAL      I95LOC = VABLOD
031700050627     C                   EVAL      I95NAR = VABNZD
031800040506     C                   CALL      'TISI95R'
031900040506     C                   PARM                    TISI95DS
032000040506     C                   EVAL      VABPRD = O95PRV
032100040506     C                   ENDIF
032200060307     C* NCL
032300070102     C                   EVAL      PiStr=%trim(%subst(vindta:432:3))
032400060307     C                   EXSR      CHKNUM
032500060307     C                   IF        PiInt=*on
032600060307     C                   ADD       PiVal         VABNCL
032700060307     C                   ELSE
032800060307     C                   ADD       1             errore
032900060307     C                   EVAL      vinmsg = %trimr(vinmsg)
033000060307     C                             + ' ' + 'VABNCL'
033100060307     C                   ENDIF
033200040506     C* PKB
033300070102     C                   EVAL      PiStr=%trim(%subst(vindta:435:7))
033400010201     C                   EXSR      CHKNUM
033500010201     C                   IF        PiNum=*on
033600070102     C                   EVAL      PiVal=PiVal/1000                             da grammi a kg
033700060316     C                   Z-ADD     PiVal         VABPKB
033800010201     C                   ELSE
033900010201     C                   ADD       1             errore
034000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
034100010201     C                             + ' ' + 'VABPKB'
034200010201     C                   ENDIF
034300100118     C* NSP => Stacco un numeratore da AZNUM
034400100118     C                   clear                   TRUL33DS
034500100118     C                   eval      I33OPE = *zeros
034600100118     C                   eval      I33CNU = 302
034700100118     C                   eval      I33NUM = 1
034800100118     C                   movel     TRUL33DS      KPJBU
034900100118     C                   call      'TRUL33R'
035000100118     C                   parm                    KPJBA
035100100118     C                   movel     KPJBU         TRUL33DS
035200100118     C                   if        O33ERR = *zeros
035300100118     C                   z-add     O33NRF        VABNSP
035400100118     C                   z-add     O33NRF        VATNSP
035500100118     C                   else
035600100118     C                   ADD       1             errore
035700100118     C                   EVAL      vinmsg = %trimr(vinmsg)
035800100118     C                             + ' ' + 'VABNSP VATNSP'
035900100118     C                   endif
036000090511     C* RMN
036100091210     C***                EVAL      PiStr=%trim(%subst(vindta:13:30))
036200100118     C                   EVAL      PiStr=%trim(%subst(vindta:47:24))
036300090511     C                   EXSR      CHKNUM
036400090511     C                   IF        PiInt=*on
036500090511     C                   Z-ADD     PiVal         VABRMN
036600090511     C                   ELSE
036700090511     C                   ADD       1             errore
036800090511     C                   EVAL      vinmsg = %trimr(vinmsg)
036900090511     C                             + ' ' + 'VABRMN'
037000090511     C                   ENDIF
037100060307     C* CAS
037200090127     C                   IF        %trim(%subst(vindta:502:9))<>'000000.00' AND
037300090608     C                             %trim(%subst(vindta:502:9))<>*blanks     AND
037400090608     C                             %trim(%subst(vindta:502:9))<>*zeros
037500070102     C                   EVAL      VABVCA = 'EUR'
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
040000160405     C* DCR
040100160405     C                   IF        %trim(%subst(vindta:162:8)) <> *blanks AND
040200160405     C                             %trim(%subst(vindta:162:8)) <> *zeros
040300160331     C                   EVAL      PiStr=%trim(%subst(vindta:162:8))
040400160331     C                   EXSR      CHKNUM
040500160331     C                   IF        PiInt=*on
040600160331     C                   Z-ADD     PiVal         VABDCR
040700160331     C                   ELSE
040800160331     C                   ADD       1             errore
040900160331     C                   Z-ADD     *zeros        VABDCR
041000160331     C                   EVAL      vinmsg = %trimr(vinmsg)
041100160331     C                             + ' ' + 'VABDCR'
041200160331     C                   ENDIF
041300160405     C                   ENDIF
041400010205     C*
041500010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
041600040802     C                   IF        FlgCAS <> '0'
041700070102     C                   IF        VABCBO = '1'
041800010205     C                   EVAL      VABCBO = '4'
041900010205     C                   ELSE
042000070102     C                   EVAL      VABCBO = '6'
042100070102     C                   ENDIF
042200010205     C                   ENDIF
042300020305     C*
042400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
042500011113     C                   EXSR      CHKIMPDIV
042600010202     C*
042700000801     C* Ebbene...
042800000801     C                   ADD       1             §CTRMO
042900010201     C                   IF        errore <> *zeros
043000000801     C                   ADD       1             §CTRNO
043100000801     C                   EVAL      vinflg = '2'
043200000801     C                   ELSE
043300010201     C                   ADD       1             §CTROKVB
043400000801     C                   ENDIF
043500000801     C*
043600000801     C                   ENDSR
043700070102     C*----------------------------------------------------*
043800070102     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
043900070102     C*----------------------------------------------------*
044000070102     C     EXEVATA       BEGSR
044100070102     C*
044200070102     C                   EXSR      INZVAR
044300070102     C                   EXSR      DEFCAM
044400070102     C*
044500070102     C                   EVAL      VATTRC='A'
044600090127     C                   EVAL      VATNOT = %trim(%subst(vindta:581:30))
044700070102     C                   exsr      wrivat                                       => scarico VAT
044800070102     C*
044900070102     C                   ENDSR
045000060307     C*----------------------------------------------------*
045100060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
045200060307     C*----------------------------------------------------*
045300060307     C     EXEVATB       BEGSR
045400060307     C*
045500060307     C                   EXSR      INZVAR
045600060307     C                   EXSR      DEFCAM
045700060307     C*
045800060307     C                   EVAL      VATTRC='B'
045900070102     C                   EVAL      VATNOT = %trim(%subst(vindta:376:15))
046000060307     C                   exsr      wrivat                                       => scarico VAT
046100060307     C*
046200060307     C                   ENDSR
046300070402     C*----------------------------------------------------*
046400070402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
046500070402     C*----------------------------------------------------*
046600070402     C     EXEVATE       BEGSR
046700070402     C*
046800070402     C                   EXSR      INZVAR
046900070402     C                   EXSR      DEFCAM
047000070402     C*
047100070402     C                   EVAL      VATTRC='E'
047200070402     C                   EVAL      VATNOT = %trim(%subst(vindta:554:25))
047300070402     C                   exsr      wrivat                                       => scarico VAT
047400070402     C*
047500070402     C                   ENDSR
047600010201     C*----------------------------------------------------*
047700040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
047800010201     C*----------------------------------------------------*
047900020305     C     WRIVAT        BEGSR
048000050628     C*
048100060223     C* Scrivo solo se valorizzato qualcosa
048200060223     C                   IF        VATNOT <> *blanks
048300040802     C                   WRITE     FIVAT000
048400060223     C                   ENDIF
048500010201     C*
048600010201     C                   ENDSR
048700010202     C*----------------------------------------------------*
048800021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
048900010202     C*----------------------------------------------------*
049000020305     C     PREVAT        BEGSR
049100010202     C*
049200021113     C* Compongo il nome del membro da dare al FIVATWWR
049300010202     C                   eval      parmbr = vlrhdl
049400010202     C                   movel     'M'           parmbr
049500050627     C                   eval      parccm = %subst(vlrKSC:2:7)
049600010202     C                   eval      paropz = '1'
049700010202     C* Effettuo la chiamata al CLLE preposto
049800040506     C                   call(e)   'TITVVTC'
049900010202     C                   parm                    parccm
050000010202     C                   parm                    parmbr
050100010202     C                   parm                    paropz
050200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050300010202     C                   if        %error
050400010202     C                   movel     '1'           chkcall
050500010202     C                   else
050600010202     C                   movel     '0'           chkcall
050700010202     C                   endif
050800010202     C*
050900010202     C                   ENDSR
051000000801     C*----------------------------------------------------*
051100000801     C*  CONTROLLO NUMERICITA' CAMPI
051200000801     C*----------------------------------------------------*
051300000801     C     CHKNUM        BEGSR
051400000801     C*
051500000801     C                   call(e)   'ISNUMERIC'
051600000801     C                   PARM                    PiStr            30
051700070102     C                   PARM      '.'           PiDecChr          1
051800000801     C                   PARM      *ZEROS        PiVal            30 9
051900000801     C                   PARM      '0'           PiInt             1
052000000801     C                   PARM      '0'           PiNum             1
052100000801     C                   IF        %error
052200000801     C                   EVAL      PiInt=*off
052300000801     C                   ENDIF
052400000801     C*
052500000801     C                   ENDSR
052600000801     C***
052700000801
052800011113
052900011113     C*----------------------------------------------------*
053000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
053100011113     C*----------------------------------------------------*
053200011113     C     CHKIMPDIV     BEGSR
053300011113     C*
053400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
053500011113     C                   Z-ADD     *zeros        wrkDec            9 9
053600011113     C*
053700011113     C* Come prima cosa effettuo considerazioni sulla divisa
053800011113     C                   IF        vabIAS > *zeros
053900011113     C                   IF        vabVAS <> 'EUR'
054000011113     C                   EVAL      vabVAS =  'ITL'
054100011113     C                   ENDIF
054200011113     C                   ENDIF
054300011113     C*
054400011113     C                   IF        vabCAS > *zeros
054500011113     C                   IF        vabVCA <> 'EUR'
054600011113     C                   EVAL      vabVCA =  'ITL'
054700011113     C                   ENDIF
054800011113     C                   ENDIF
054900011113     C*
055000011113     C                   IF        vabVMD > *zeros
055100020305     C                   IF        vabVAD <> 'EUR'
055200011113     C                   EVAL      vabVAD =  'ITL'
055300011113     C                   ENDIF
055400011113     C                   ENDIF
055500011113     C*
055600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
055700011113     C                   Z-ADD     vabIAS        wrkDec
055800011113     C                   IF        wrkDec > *zeros
055900011113     C                   IF        vabVAS = 'ITL'
056000011113     C                   EVAL      vabIAS = *zeros
056100011113     C                   ENDIF
056200011113     C                   ENDIF
056300011113     C*
056400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
056500011113     C                   Z-ADD     vabCAS        wrkDec
056600011113     C                   IF        wrkDec > *zeros
056700011113     C                   IF        vabVCA = 'ITL'
056800011113     C                   EVAL      vabCAS = *zeros
056900011113     C                   ENDIF
057000011113     C                   ENDIF
057100011113     C*
057200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
057300011113     C                   Z-ADD     vabVMD        wrkDec
057400011113     C                   IF        wrkDec > *zeros
057500011113     C                   IF        vabVAD = 'ITL'
057600011113     C                   EVAL      vabVMD = *zeros
057700011113     C                   ENDIF
057800011113     C                   ENDIF
057900011113     C*
058000011113     C                   ENDSR
058100011113     C***
058200011113
058300011113
058400000801
058500000801
058600990920      /TITLE Invio dei dati al punto operativo.
058700010202     C     invio         BEGSR
058800990920     C*
058900021113     C* 1° invio FIVAT
059000010201     C                   reset                   dscmz
059100010201     C                   move      vlrpoi        cmzdst
059200021113     C                   eval      cmzfld = 'FIVATWWR'
059300010201     C                   eval      cmzmbd = vlrhdl
059400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059500021009     C***                if        prmfir = *blanks
059600021113     C                   eval      cmzfla = 'FIVAT00F'
059700021113     C                   eval      cmzmba = 'FIVAT00F'
059800021009     C***                else
059900021009     C***                eval      cmzfla = prmfir
060000021009     C***                eval      cmzmba = prmfir
060100021009     C***                endif
060200010201     C                   eval      cmznrr = *zeros
060300020305     C                   move      §ctrokvt      cmznrr
060400021018     C                   eval      cmzlba = vlrfl1
060500010201     C                   call(e)   'TIS711C'
060600010201     C                   parm                    dscmz
060700010201     C                   parm      *blanks       esito
060800010205     C                   if        %error
060900010205     C                             or cmzerr = '1'
061000010205     C                             or esito  = '1'
061100010205     C                   eval      wrkesito = '3'
061200010205     C                   else
061300010201     C*
061400021113     C* 2° invio FIVAB
061500010201     C                   reset                   dscmz
061600010201     C                   move      vlrpoi        cmzdst
061700010201     C                   eval      cmzfld = vlrfou
061800010201     C                   eval      cmzmbd = vlrhdl
061900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062000021009     C***                if        prmfir = *blanks
062100021113     C                   eval      cmzfla = 'FIVAB00F'
062200021113     C                   eval      cmzmba = 'FIVAB00F'
062300021009     C***                else
062400021009     C***                eval      cmzfla = prmfir
062500021009     C***                eval      cmzmba = prmfir
062600021009     C***                endif
062700010201     C                   eval      cmznrr = *zeros
062800010201     C                   move      §ctrokvb      cmznrr
062900021018     C                   eval      cmzlba = vlrfl1
063000010201     C                   call(e)   'TIS711C'
063100010201     C                   parm                    dscmz
063200010201     C                   parm      *blanks       esito
063300010201     C                   if        %error
063400010201     C                             or cmzerr = '1'
063500010201     C                             or esito  = '1'
063600010201     C                   eval      wrkesito = '3'
063700010201     C                   endif
063800010205     C                   endif
063900990920     C*
064000000613     C                   ENDSR
064100000613     C***
064200990910
064300000613     C     *inzsr        BEGSR
064400990910     C*
064500990910     C     *entry        plist
064600990920     C                   parm                    tivlrds
064700990921     C                   parm      wrkesito      esito
064800000724     C                   parm                    prmlit
064900000710     C                   parm                    prmfir
065000000613     C*
065100000830     C* CALCOLA LA DATA CORRENTE
065200100118     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
065300100118     C                   eval      datcor = %dec(%date() : *ISO)
065400000830     C*
065500000613     C                   ENDSR
065600000613     C***
