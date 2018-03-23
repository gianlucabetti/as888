000100090114      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200100114     H DFTACTGRP(*NO) BNDDIR('TRUL':'UBBNDDIR') ACTGRP('BARTVAS')
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500090114     FFIVABwwr  O    E             DISK    usropn
000600090114     FFIVATwwr  O    E             DISK    usropn
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
002500091214     D curSped         s             35    INZ(*blanks)
002600091214     D depSped         s             35    INZ(*blanks)
002700090114
002800091215     D*------------------
002900091215     D* DS REPERIMENTO NUMERATORE
003000091215     D*------------------
003100091215     D trul33ds      e ds                  inz
003200091215     D*------------------
003300091215     D* DS ARCHITETTURA
003400091215     D*------------------
003500091215     D kpjba         e ds                  inz
003600091215
003700081217
003800081217     D*------------------
003900081217     D* LINKING A DEFINIZIONI ESTERNE
004000081217     D*------------------
004100081217     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
004200081217     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
004300100114     D/COPY GAITRASRC/SRCPROTOPR,UBCHKRMN
004400100114     D/COPY GAITRASRC/SRCPROTOPI,UBCHKRMN
004500081217
004600081217
004700010201
004800081217
004900100114     C*
005000100114     C* Metodo apertura UBCHKRMN
005100100114     C                   callp(e)  UBCHKRMN_Open()
005200100114     C*
005300000913     C                   reset                   rrnum
005400990921     C                   reset                   esito
005500990921     C                   reset                   wrkesito
005600000613     C*
005700040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005800000613     C*
005900010202     C* Effettuo la chiamata al CLLE preposto
006000090114     C                   call(e)   'TITVVTC'
006100010202     C                   parm                    parccm
006200010202     C                   parm                    parmbr
006300010202     C                   parm      '2'           paropz
006400050201     C*
006500050201     C* Effettuo lancio TISI95 solo x chiusura
006600050201     C                   CLEAR                   TISI95DS
006700050201     C                   EVAL      I95TLA = 'C'
006800050201     C                   CALL      'TISI95R'
006900050201     C                   PARM                    TISI95DS
007000000616     C*
007100100114     C* Metodo chiusura UBCHKRMN
007200100114     C                   callp(e)  UBCHKRMN_Close()
007300100114     C*
007400000801     C
007500010201     C                   seton                                        LR
007600990908
007700000801
007800910830     C*--------------------------------------------------------
007900090114     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
008000910830     C*--------------------------------------------------------
008100040526     C     RWFILE        BEGSR
008200990910     C*
008300990914     C                   if        not %open(tivin00r)
008400990908     C                   open      tivin00r
008500990914     C                   endif
008600090114     C                   if        not %open(fivabwwr)
008700090114     C                   open      fivabwwr
008800990914     C                   endif
008900070103     C*
009000090114     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
009100020305     C                   exsr      prevat
009200010201     C*
009300010202     C                   if        chkcall = '0'
009400010202     C*
009500090114     C                   if        not %open(fivatwwr)
009600090114     C                   open      fivatwwr
009700010201     C                   endif
009800990910     C*
009900010201     C                   clear                   §CTROKVB          5 0
010000020305     C                   clear                   §CTROKVT          5 0
010100000801     C                   clear                   §CTRMO            5 0
010200000801     C                   clear                   §CTRNO            5 0
010300990910     C*
010400921023     C                   DO        *HIVAL
010500990913     C*
010600990915     C                   READ      tivin00r                               70
010700050627     C                   if        vindta > *blanks
010800000613     C                   add       1             rrnum
010900000801     C*
011000091214     C                   if        *in70 = *off
011100091214 xxx C                             and rrnum > 1
011200000801     C                             and
011300000801     C                             (vinflg = *blanks
011400000801     C                              or vinflg = '0'
011500000801     C                              or vinflg = '2')
011600000801     C*
011700000801     C                   clear                   vinmsg
011800000801     C                   eval      vinflg = '1'
011900070103     C*
012000070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
012100091214     C                   eval      curSped = %subst(vindta:1:35)
012200070213     C                   if        curSped <> depSped
012300070213     C* Se prima bolla => importo bolla corrente
012400070213     C                   if        §CTRMO = *zeros
012500091215     C                   exsr      repNSP
012600070213     C                   exsr      impvab                                       => carico  VAB
012700070213     C                   else
012800070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
012900100120     C  N54              exsr      wrivab                                       => scarico VAB
013000091215     C                   exsr      repNSP
013100070213     C                   exsr      impvab                                       => carico  VAB
013200070213     C                   endif
013300070103     C* Salvo il raggruppamento spedizione cliente corrente
013400070103     C                   eval      depSped = curSped
013500070213     C*
013600070213     C* Se collo successivo x stessa bolla
013700070213     C                   else
013800070213     C                   exsr      impvab                                       => carico  VAB
013900070103     C                   endif
014000091215     C*
014100100114     C  N54              exsr      exevate                                      => write VAT-E
014200000905     C*
014300000905     C                   else
014400000905     C                   eval      vinflg = '1'
014500050628     C                   endif
014600000905     C                   endif
014700000905     C*
014800000905     C  N70              update    tivin000
014900000905     C*
015000991022     C  N70              ENDdo
015100070213     C*
015200070213     C* Scarico testata bolla rimasta in sospesa
015300100120     C  N54              exsr      wrivab                                       => scarico VAB
015400010202     C*
015500010202     C                   endif
015600990910
015700990910     C* Se non ci sono record con errori ...
015800000710     C                   if        §ctrno = 0
015900990910     C* ... restituisco esito OK.
016000990921     C                   eval      wrkesito = '0'
016100990910     C                   else
016200010201     C                   if        §ctrokvb > 0
016300990921     C                   eval      wrkesito = '1'
016400000710     C                   else
016500000710     C                   eval      wrkesito = '2'
016600990910     C                   endif
016700000710     C                   endif
016800990910     C*
016900990914     C                   if        %open(tivin00r)
017000990908     C                   close     tivin00r
017100990914     C                   endif
017200090114     C                   if        %open(fivabwwr)
017300090114     C                   close     fivabwwr
017400990914     C                   endif
017500090114     C                   if        %open(fivatwwr)
017600090114     C                   close     fivatwwr
017700010201     C                   endif
017800990910     C*
017900010201     C                   if        §ctrokvb > 0
018000000724     C                             and vlrpoi <> *zeros
018100010202     C                   exsr      invio
018200990920     C                   endif
018300990920     C*
018400910830     C                   ENDSR
018500000613     C***
018600010305
018700010305     C*----------------------------------------------------*
018800020305     C*  SCARICAMENTO BUFFER RECORDS VAB
018900010305     C*----------------------------------------------------*
019000020305     C     WRIVAB        BEGSR
019100010305     C*
019200060225     C* Quindi scarico il buffer del file d testata
019300090114     C                   write     fivab000                                     => scarico il VAB
019400010305     C*
019500010305     C                   ENDSR
019600990920
019700000801     C*----------------------------------------------------*
019800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019900000801     C*----------------------------------------------------*
020000010201     C     INZVAR        BEGSR
020100000801     C*
020200090114     C                   CLEAR                   FIVAB000
020300090114     C                   CLEAR                   FIVAT000
020400070213     C*
020500040802     C                   Z-ADD     *zeros        Num5_0            5 0
020600040802     C                   MOVEL     '0'           FlgCAS            1
020700100114     C                   setoff                                       54
020800000801     C*
020900000801     C                   ENDSR
021000000801     C*----------------------------------------------------*
021100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
021200000801     C*----------------------------------------------------*
021300000801     C     DEFCAM        BEGSR
021400000801     C*
021500020619     C* Imposto i valori di default...
021600100114     C                   Z-ADD     0895460       VABCCM
021700100114     C                   Z-ADD     0895460       VATCCM
021800091214     C                   Z-ADD     089           VABLNP
021900091214     C                   Z-ADD     089           VATLNP
022000091214     C                   Z-ADD     303           VABCTR
022100040823     C                   MOVEL     '1'           VABCBO
022200090922     C                   MOVEL     '7Q'          VABCTM
022300020619     C* ... e poi verifico se sono stati passati come parametri
022400020619     C                   IF        vlrppt > *blanks
022500040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
022600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
022700020619     C                   EXSR      CHKNUM
022800020619     C                   IF        PiInt=*on
022900020619     C                   Z-ADD     PiVal         VABCCM
023000020619     C                   Z-ADD     PiVal         VATCCM
023100020619     C                   ENDIF
023200040506     C                   ENDIF
023300040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
023400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023500020619     C                   EXSR      CHKNUM
023600020619     C                   IF        PiInt=*on
023700020619     C                   Z-ADD     PiVal         VABLNP
023800020619     C                   Z-ADD     PiVal         VATLNP
023900040506     C                   ENDIF
024000020619     C                   ENDIF
024100040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
024200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
024300020619     C                   EXSR      CHKNUM
024400020619     C                   IF        PiInt=*on
024500020619     C                   Z-ADD     PiVal         VABCTR
024600040506     C                   ENDIF
024700020619     C                   ENDIF
024800020619     C                   ENDIF
024900000801     C*
025000000801     C                   ENDSR
025100091215     C*----------------------------------------------------*
025200091215     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
025300091215     C*----------------------------------------------------*
025400091215     C     REPNSP        BEGSR
025500091215     C*
025600091215     C                   EXSR      INZVAR
025700091215     C                   EXSR      DEFCAM
025800091215     C*
025900091215     C* NSP => Stacco un numeratore da AZNUM
026000091215     C                   clear                   TRUL33DS
026100091215     C                   eval      I33OPE = *zeros
026200091215     C                   eval      I33CNU = 302
026300091215     C                   eval      I33NUM = 1
026400091215     C                   movel     TRUL33DS      KPJBU
026500091215     C                   call      'TRUL33R'
026600091215     C                   parm                    KPJBA
026700091215     C                   movel     KPJBU         TRUL33DS
026800091215     C                   if        O33ERR = *zeros
026900091215     C                   z-add     O33NRF        VABNSP
027000091215     C                   z-add     O33NRF        VATNSP
027100091215     C                   else
027200091215     C                   ADD       1             errore
027300091215     C                   EVAL      vinmsg = %trimr(vinmsg)
027400091215     C                             + ' ' + 'VABNSP VATNSP'
027500091215     C                   endif
027600091215     C*
027700091215     C                   ENDSR
027800000801     C*----------------------------------------------------*
027900090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
028000000801     C*----------------------------------------------------*
028100040823     C     IMPVAB        BEGSR
028200010305     C*
028300000801     C                   Z-ADD     *zeros        errore            1 0
028400000830     C                   MOVEL     datcor        VABAAS
028500020305     C                   MOVEL     datcor        VATAAS
028600040526     C                   MOVE      datcor        VABMGS
028700040823     C                   MOVE(P)   vlrpoi        VABFGS
028800040823     C                   MOVE(P)   vlrpoi        VATFGS
028900080409     C*
029000080409     C* Reperimento campi ALFA
029100080409     C*
029200080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
029300091214     C                   EVAL      VABRSD=%trim(%subst(vindta:201:35))
029400080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
029500080409     C     '@':'A'       XLATE     VABRSD        VABRSD
029600080409     C* ==
029700091214     C                   EVAL      VABRD2=%trim(%subst(vindta:61:35))
029800091214     C                   EVAL      VABIND=%trim(%subst(vindta:96:35))  + ' ' +
029900091214     C                                    %trim(%subst(vindta:131:35)) + ' ' +
030000091214     C                                    %trim(%subst(vindta:166:35))
030100091214     C                   EVAL      VABLOD=%trim(%subst(vindta:281:35))
030200091216     C***                EVAL      VABRMA=%trim(%subst(vindta:955:35))
030300091216     C                   EVAL      VABRMA=%trim(%subst(vindta:1:35))
030400091214     C                   EVAL      VABNZD=%trim(%subst(vindta:371:3))
030500091214     C                   IF        VABNZD='IT' OR
030600091214     C                             VABNZD='VA' OR
030700091214     C                             VABNZD='SM'
030800091214     C                   EVAL      VABNZD=*blanks
030900091214     C                   ENDIF
031000091214     C                   EVAL      VABNAS=%trim(%subst(vindta:1072:35))
031100091214     C                   MOVEL     *blanks       wNOTE            70
031200091214     C                   EVAL      wNOTE =%trim(%subst(vindta:762:35))  + ' ' +
031300091214     C                                    %trim(%subst(vindta:797:35)) + ' ' +
031400091214     C                                    %trim(%subst(vindta:832:35)) + ' ' +
031500091214     C                                    %trim(%subst(vindta:867:35))
031600091214     C                   EVAL      VABNOT=%subst(wNOTE:1:35)
031700091214     C                   EVAL      VABNT2=%subst(wNOTE:35:35)
031800080409     C*
031900080409     C* Reperimento campi NUMERICI
032000090309     C* RMN
032100091215     C                   EVAL      PiStr=%trim(%subst(vindta:1+2:35-2))
032200091215     C                   EXSR      CHKNUM
032300091215     C                   IF        PiInt=*on
032400091215     C                   Z-ADD     PiVal         VABRMN
032500091215     C                   ELSE
032600091215     C                   ADD       1             errore
032700091215     C                   Z-ADD     1             VABRMN
032800091215     C                   EVAL      vinmsg = %trimr(vinmsg)
032900091215     C                             + ' ' + 'VABRMN'
033000091215     C                   ENDIF
033100100114     C*
033200100114     C* Verifico che x il CCM corrente nn esiste già (ovunque) una bolla con medesimo
033300100114     C* riferimento => altrimenti escludo
033400100114     C                   if        VABRMN <> 1
033500100114     C                   callp(e)  UBCHKRMN_Exist(VABCCM
033600100114     C                                           :VABRMN
033700100114     C                                           :pOutFIVAB
033800100114     C                                           :pOutFNBLP
033900100114     C                                           :pOutTITAS)
034000100114     C*
034100100114     C                   if        not %error
034200100114     C                   if        pOutFIVAB = *off AND
034300100114     C                             pOutFNBLP = *off AND
034400100114     C                             pOutTITAS = *off
034500100114     C                   else
034600100114     C                   seton                                        54
034700100114     C                   endif
034800100114     C                   endif
034900100114     C                   endif
035000100114     C*
035100100114     C                   if        not *in54
035200080409     C* CAD
035300091214     C                   EVAL      PiStr=%trim(%subst(vindta:271:10))
035400090114     C                   EXSR      CHKNUM
035500090114     C                   IF        PiInt=*on
035600090114     C                   Z-ADD     PiVal         Num5_0
035700090114     C                   MOVEL(p)  Num5_0        VABCAD
035800090114     C                   ELSE
035900090114     C                   ADD       1             errore
036000090114     C                   EVAL      VABCAD = *zeros
036100090114     C                   EVAL      vinmsg = %trimr(vinmsg)
036200090114     C                             + ' ' + 'VABCAD'
036300090114     C                   ENDIF
036400091214     C*
036500091214     C* Reperisco la provincia dal CAP e dalla località
036600091214     C                   IF        VABCAD <> *blanks AND
036700091214     C                             VABPRD  = *blanks AND
036800091214     C                             VABNZD  = *blanks
036900091214     C                   CLEAR                   TISI95DS
037000091214     C                   EVAL      I95TCN = '3'
037100091214     C                   Z-ADD     datcor        I95DAT
037200091214     C                   EVAL      I95CAP = VABCAD
037300091214     C                   EVAL      I95LOC = VABLOD
037400091214     C                   CALL      'TISI95R'
037500091214     C                   PARM                    TISI95DS
037600091214     C                   EVAL      VABPRD = O95PRV
037700091214     C                   ENDIF
037800091214     C*
037900091214     C* NCL
038000091214     C* ...calcolato in sommatoria dei singoli dettagli
038100091215     C* PKB
038200091215     C* ...calcolato in sommatoria dei singoli dettagli
038300091215     C* IAS
038400091215     C                   IF        %subst(vindta:1006:9) <> *blanks AND
038500091215     C                             %subst(vindta:1006:9) <> *zeros  AND
038600091215     C                             %subst(vindta:1006:9) <> '000000000'
038700091215     C                   EVAL      VABVAS=%trim(%subst(vindta:1015:4))
038800091215     C                   EVAL      PiStr=%trim(%subst(vindta:1006:9))
038900091215     C                   EXSR      CHKNUM
039000091215     C                   IF        PiNum=*on
039100091215     C                   EVAL      PiVal = PiVal / 100                          *da cents a euro
039200091215     C                   Z-ADD     PiVal         VABIAS
039300091215     C                   ELSE
039400091215     C                   ADD       1             errore
039500091215     C                   EVAL      vinmsg = %trimr(vinmsg)
039600091215     C                             + ' ' + 'VABIAS'
039700091215     C                   ENDIF
039800091215     C                   ENDIF
039900090922     C* CAS
040000091214     C                   IF        %subst(vindta:1019:9) <> *blanks AND
040100091214     C                             %subst(vindta:1019:9) <> *zeros  AND
040200091214     C                             %subst(vindta:1019:9) <> '000000000'
040300090922     C                   EVAL      FlgCAS = '1'
040400091214     C                   EVAL      VABVCA=%trim(%subst(vindta:1015:4))
040500091214     C                   EVAL      VABTIC=*blanks
040600091214     C                   EVAL      PiStr=%trim(%subst(vindta:1019:9))
040700090922     C                   EXSR      CHKNUM
040800090922     C                   IF        PiNum=*on
040900091214     C                   EVAL      PiVal = PiVal / 100                          *da cents a euro
041000090922     C                   Z-ADD     PiVal         VABCAS
041100090922     C                   ELSE
041200090922     C                   ADD       1             errore
041300090922     C                   EVAL      vinmsg = %trimr(vinmsg)
041400090922     C                             + ' ' + 'VABCAS'
041500090922     C                   ENDIF
041600090922     C                   ENDIF
041700071121     C*
041800080415     C* Carico l'estensioni A e B del FNVAT
041900080512     C                   exsr      exevata
042000080512     C                   exsr      exevatb
042100010205     C*
042200010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
042300040802     C                   IF        FlgCAS <> '0'
042400090922     C                   IF        VABCBO = '1'
042500010205     C                   EVAL      VABCBO = '4'
042600090922     C                   ELSE
042700090922     C                   EVAL      VABCBO = '6'
042800090922     C                   ENDIF
042900010205     C                   ENDIF
043000020305     C*
043100011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043200011113     C                   EXSR      CHKIMPDIV
043300100114     C*
043400100114     C                   endif
043500010202     C*
043600000801     C* Ebbene...
043700000801     C                   ADD       1             §CTRMO
043800010201     C                   IF        errore <> *zeros
043900000801     C                   ADD       1             §CTRNO
044000000801     C                   EVAL      vinflg = '2'
044100000801     C                   ELSE
044200010201     C                   ADD       1             §CTROKVB
044300000801     C                   ENDIF
044400000801     C*
044500000801     C                   ENDSR
044600070102     C*----------------------------------------------------*
044700090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
044800070102     C*----------------------------------------------------*
044900070103     C     EXEVATE       BEGSR
045000070102     C*
045100091215     C* NCL
045200091214     C                   eval      VABNCL = VABNCL + 1
045300091215     C* PKB
045400091215     C                   EVAL      PiStr=%trim(%subst(vindta:38:8))
045500091215     C                   EXSR      CHKNUM
045600091215     C                   IF        PiNum=*on
045700091215     C                   EVAL      PiVal = PiVal / 100                          *da decagrammi a Kg.
045800091215     C                   ADD       PiVal         VABPKB
045900091215     C                   ELSE
046000091215     C                   ADD       1             errore
046100091215     C                   EVAL      vinmsg = %trimr(vinmsg)
046200091215     C                             + ' ' + 'VABPKB'
046300091215     C                   ENDIF
046400091214     C*
046500090428     C                   EVAL      VATTRC = 'E'
046600091214     C                   EVAL      VATNOT = %trim(%subst(vindta:920:35))
046700090428     C*
046800090922     C                   exsr      wrivat                                       => scarico VAT
046900070102     C*
047000070102     C                   ENDSR
047100080415     C*----------------------------------------------------*
047200090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
047300080415     C*----------------------------------------------------*
047400080415     C     EXEVATA       BEGSR
047500080415     C*
047600091214     C***                EVAL      VATTRC = 'A'
047700091214     C***                EVAL      VATNOT = %trim(%subst(vindta:376:15))
047800080415     C*
047900091215     C***                exsr      wrivat                                       => scarico VAT
048000080415     C*
048100080415     C                   ENDSR
048200071121     C*----------------------------------------------------*
048300090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
048400071121     C*----------------------------------------------------*
048500071121     C     EXEVATB       BEGSR
048600071121     C*
048700090504     C                   EVAL      VATTRC = 'B'
048800091214     C                   EVAL      VATNOT = %trim(%subst(vindta:374:30))
048900071121     C*
049000090922     C                   exsr      wrivat                                       => scarico VAT
049100071121     C*
049200071121     C                   ENDSR
049300010201     C*----------------------------------------------------*
049400090114     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
049500010201     C*----------------------------------------------------*
049600020305     C     WRIVAT        BEGSR
049700050628     C*
049800060223     C* Scrivo solo se valorizzato qualcosa
049900060223     C                   IF        VATNOT <> *blanks
050000090922     C                   WRITE     FIVAT000
050100090922     C                   ADD       1             §ctrokvt
050200060223     C                   ENDIF
050300010201     C*
050400010201     C                   ENDSR
050500010202     C*----------------------------------------------------*
050600090114     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
050700010202     C*----------------------------------------------------*
050800020305     C     PREVAT        BEGSR
050900010202     C*
051000090114     C* Compongo il nome del membro da dare al FIVATWWR
051100010202     C                   eval      parmbr = vlrhdl
051200010202     C                   movel     'M'           parmbr
051300050627     C                   eval      parccm = %subst(vlrKSC:2:7)
051400010202     C                   eval      paropz = '1'
051500010202     C* Effettuo la chiamata al CLLE preposto
051600090114     C                   call(e)   'TITVVTC'
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
053200081217     C*
053300081217     C                   IF        PiDecChr = *blanks
053400090922     C                   EVAL      PiDecChr = '.'
053500081217     C                   ENDIF
053600081217     C*
053700081217     C                   callp(e)  UBISNUM_Check(PiStr
053800081217     C                                          :PiDecChr
053900081217     C                                          :PiVal
054000081217     C                                          :PiNum
054100081217     C                                          :PiInt)
054200081217     C*
054300081217     C                   IF        %error
054400081217     C                   EVAL      PiInt=*off
054500081217     C                   ENDIF
054600000801     C*
054700000801     C                   ENDSR
054800000801     C***
054900000801
055000011113
055100011113     C*----------------------------------------------------*
055200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
055300011113     C*----------------------------------------------------*
055400011113     C     CHKIMPDIV     BEGSR
055500011113     C*
055600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
055700011113     C                   Z-ADD     *zeros        wrkDec            9 9
055800011113     C*
055900011113     C* Come prima cosa effettuo considerazioni sulla divisa
056000011113     C                   IF        vabIAS > *zeros
056100011113     C                   IF        vabVAS <> 'EUR'
056200011113     C                   EVAL      vabVAS =  'ITL'
056300011113     C                   ENDIF
056400011113     C                   ENDIF
056500011113     C*
056600011113     C                   IF        vabCAS > *zeros
056700011113     C                   IF        vabVCA <> 'EUR'
056800011113     C                   EVAL      vabVCA =  'ITL'
056900011113     C                   ENDIF
057000011113     C                   ENDIF
057100011113     C*
057200011113     C                   IF        vabVMD > *zeros
057300020305     C                   IF        vabVAD <> 'EUR'
057400011113     C                   EVAL      vabVAD =  'ITL'
057500011113     C                   ENDIF
057600011113     C                   ENDIF
057700011113     C*
057800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
057900011113     C                   Z-ADD     vabIAS        wrkDec
058000011113     C                   IF        wrkDec > *zeros
058100011113     C                   IF        vabVAS = 'ITL'
058200011113     C                   EVAL      vabIAS = *zeros
058300011113     C                   ENDIF
058400011113     C                   ENDIF
058500011113     C*
058600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
058700011113     C                   Z-ADD     vabCAS        wrkDec
058800011113     C                   IF        wrkDec > *zeros
058900011113     C                   IF        vabVCA = 'ITL'
059000011113     C                   EVAL      vabCAS = *zeros
059100011113     C                   ENDIF
059200011113     C                   ENDIF
059300011113     C*
059400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
059500011113     C                   Z-ADD     vabVMD        wrkDec
059600011113     C                   IF        wrkDec > *zeros
059700011113     C                   IF        vabVAD = 'ITL'
059800011113     C                   EVAL      vabVMD = *zeros
059900011113     C                   ENDIF
060000011113     C                   ENDIF
060100011113     C*
060200011113     C                   ENDSR
060300011113     C***
060400011113
060500011113
060600000801
060700000801
060800990920      /TITLE Invio dei dati al punto operativo.
060900010202     C     invio         BEGSR
061000990920     C*
061100090114     C* 1° invio FIVAT
061200010201     C                   reset                   dscmz
061300010201     C                   move      vlrpoi        cmzdst
061400090114     C                   eval      cmzfld = 'FIVATWWR'
061500010201     C                   eval      cmzmbd = vlrhdl
061600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
061700021009     C***                if        prmfir = *blanks
061800090114     C                   eval      cmzfla = 'FIVAT00F'
061900090114     C                   eval      cmzmba = 'FIVAT00F'
062000021009     C***                else
062100021009     C***                eval      cmzfla = prmfir
062200021009     C***                eval      cmzmba = prmfir
062300021009     C***                endif
062400010201     C                   eval      cmznrr = *zeros
062500020305     C                   move      §ctrokvt      cmznrr
062600021018     C                   eval      cmzlba = vlrfl1
062700010201     C                   call(e)   'TIS711C'
062800010201     C                   parm                    dscmz
062900010201     C                   parm      *blanks       esito
063000010205     C                   if        %error
063100010205     C                             or cmzerr = '1'
063200010205     C                             or esito  = '1'
063300010205     C                   eval      wrkesito = '3'
063400010205     C                   else
063500010201     C*
063600090114     C* 2° invio FIVAB
063700010201     C                   reset                   dscmz
063800010201     C                   move      vlrpoi        cmzdst
063900010201     C                   eval      cmzfld = vlrfou
064000010201     C                   eval      cmzmbd = vlrhdl
064100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
064200021009     C***                if        prmfir = *blanks
064300090114     C                   eval      cmzfla = 'FIVAB00F'
064400090114     C                   eval      cmzmba = 'FIVAB00F'
064500021009     C***                else
064600021009     C***                eval      cmzfla = prmfir
064700021009     C***                eval      cmzmba = prmfir
064800021009     C***                endif
064900010201     C                   eval      cmznrr = *zeros
065000010201     C                   move      §ctrokvb      cmznrr
065100021018     C                   eval      cmzlba = vlrfl1
065200010201     C                   call(e)   'TIS711C'
065300010201     C                   parm                    dscmz
065400010201     C                   parm      *blanks       esito
065500010201     C                   if        %error
065600010201     C                             or cmzerr = '1'
065700010201     C                             or esito  = '1'
065800010201     C                   eval      wrkesito = '3'
065900010201     C                   endif
066000010205     C                   endif
066100990920     C*
066200000613     C                   ENDSR
066300000613     C***
066400070411
066500090331
066600090331
066700090331
066800070411     C     *pssr         BEGSR
066900070411     C*
067000070411     C                   if        %open(tivin00r)
067100070411     C                   close     tivin00r
067200070411     C                   endif
067300090114     C                   if        %open(fivabwwr)
067400090114     C                   close     fivabwwr
067500070411     C                   endif
067600090114     C                   if        %open(fivatwwr)
067700090114     C                   close     fivatwwr
067800070411     C                   endif
067900070411     C*
068000070411     C* Effettuo la chiamata al CLLE preposto
068100090114     C                   call(e)   'TITVVTC'
068200070411     C                   parm                    parccm
068300070411     C                   parm                    parmbr
068400070411     C                   parm      '2'           paropz
068500100114     C*
068600100114     C* Metodo chiusura UBCHKRMN
068700100114     C                   callp(e)  UBCHKRMN_Close()
068800070411     C*
068900070411     C                   eval      wrkesito = '2'
069000070411     C*
069100070411     C                   seton                                        LR
069200070411     C*
069300070411     C                   ENDSR     '*CANCL'
069400070411     C***
069500070411
069600090331
069700090331
069800990910
069900000613     C     *inzsr        BEGSR
070000990910     C*
070100990910     C     *entry        plist
070200990920     C                   parm                    tivlrds
070300990921     C                   parm      wrkesito      esito
070400000724     C                   parm                    prmlit
070500000710     C                   parm                    prmfir
070600000613     C*
070700091214     C                   z-add     *zeros        datcor            8 0
070800091214     C                   eval      datcor = %dec(%date() : *ISO)
070900000830     C*
071000000613     C                   ENDSR
071100000613     C***
