000100090114      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200120614     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300120614     H DECEDIT('0,') DATEDIT(*DMY.)
000400990908
000500990910     Ftivin00r  uF   E             DISK    usropn
000600090114     FFIVABwwr  O    E             DISK    usropn
000700090114     FFIVATwwr  O    E             DISK    usropn
000800120614     FTIVGD00F  O    E             DISK
000900990908
001000000801     D*----------------------------------------------------
001100000801     D* DICHIARAZIOINE VARIABILI DI WRK
001200000801     D*----------------------------------------------------
001300990920     D dscmz         e ds                  inz
001400990910     D psds           sds
001500990910     D  procname         *PROC
001600990920     D tivlrds       e ds                  extname(tivlr00f)
001700060307     D tisi95ds      e ds
001800120614     D fnvab00t      e ds                  inz
001900990910     D esito           s              1
002000000724     D prmlit          s             10
002100000710     D prmfir          s             10
002200990921     D wrkesito        s                   like(esito)
002300000613     D rrnum           s              6  0 INZ(*zeros)
002400010202     D parccm          s              8    INZ(*blanks)
002500010202     D parmbr          s             10    INZ(*blanks)
002600010202     D paropz          s              1    INZ(*blanks)
002700010202     D chkcall         s              1    INZ(*blanks)
002800091214     D curSped         s             35    INZ(*blanks)
002900091214     D depSped         s             35    INZ(*blanks)
003000090114
003100091215     D*------------------
003200091215     D* DS REPERIMENTO NUMERATORE
003300091215     D*------------------
003400091215     D trul33ds      e ds                  inz
003500091215     D*------------------
003600091215     D* DS ARCHITETTURA
003700091215     D*------------------
003800091215     D kpjba         e ds                  inz
003900091215
004000081217
004100081217     D*------------------
004200081217     D* LINKING A DEFINIZIONI ESTERNE
004300081217     D*------------------
004400081217     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
004500081217     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
004600100114     D/COPY GAITRASRC/SRCPROTOPR,UBCHKRMN
004700100114     D/COPY GAITRASRC/SRCPROTOPI,UBCHKRMN
004800081217
004900081217
005000010201
005100081217
005200100114     C*
005300100114     C* Metodo apertura UBCHKRMN
005400100114     C                   callp(e)  UBCHKRMN_Open()
005500100114     C*
005600000913     C                   reset                   rrnum
005700990921     C                   reset                   esito
005800990921     C                   reset                   wrkesito
005900000613     C*
006000040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
006100000613     C*
006200010202     C* Effettuo la chiamata al CLLE preposto
006300090114     C                   call(e)   'TITVVTC'
006400010202     C                   parm                    parccm
006500010202     C                   parm                    parmbr
006600010202     C                   parm      '2'           paropz
006700050201     C*
006800050201     C* Effettuo lancio TISI95 solo x chiusura
006900050201     C                   CLEAR                   TISI95DS
007000050201     C                   EVAL      I95TLA = 'C'
007100050201     C                   CALL      'TISI95R'
007200050201     C                   PARM                    TISI95DS
007300000616     C*
007400100114     C* Metodo chiusura UBCHKRMN
007500100114     C                   callp(e)  UBCHKRMN_Close()
007600100114     C*
007700000801     C
007800010201     C                   seton                                        LR
007900990908
008000000801
008100910830     C*--------------------------------------------------------
008200090114     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
008300910830     C*--------------------------------------------------------
008400040526     C     RWFILE        BEGSR
008500990910     C*
008600990914     C                   if        not %open(tivin00r)
008700990908     C                   open      tivin00r
008800990914     C                   endif
008900090114     C                   if        not %open(fivabwwr)
009000090114     C                   open      fivabwwr
009100990914     C                   endif
009200070103     C*
009300090114     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
009400020305     C                   exsr      prevat
009500010201     C*
009600010202     C                   if        chkcall = '0'
009700010202     C*
009800090114     C                   if        not %open(fivatwwr)
009900090114     C                   open      fivatwwr
010000010201     C                   endif
010100990910     C*
010200010201     C                   clear                   §CTROKVB          5 0
010300020305     C                   clear                   §CTROKVT          5 0
010400000801     C                   clear                   §CTRMO            5 0
010500000801     C                   clear                   §CTRNO            5 0
010600990910     C*
010700921023     C                   DO        *HIVAL
010800990913     C*
010900990915     C                   READ      tivin00r                               70
011000050627     C                   if        vindta > *blanks
011100000613     C                   add       1             rrnum
011200000801     C*
011300091214     C                   if        *in70 = *off
011400091214 xxx C                             and rrnum > 1
011500000801     C                             and
011600000801     C                             (vinflg = *blanks
011700000801     C                              or vinflg = '0'
011800000801     C                              or vinflg = '2')
011900000801     C*
012000000801     C                   clear                   vinmsg
012100000801     C                   eval      vinflg = '1'
012200070103     C*
012300070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
012400091214     C                   eval      curSped = %subst(vindta:1:35)
012500070213     C                   if        curSped <> depSped
012600070213     C* Se prima bolla => importo bolla corrente
012700070213     C                   if        §CTRMO = *zeros
012800091215     C                   exsr      repNSP
012900070213     C                   exsr      impvab                                       => carico  VAB
013000070213     C                   else
013100070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
013200100120     C  N54              exsr      wrivab                                       => scarico VAB
013300091215     C                   exsr      repNSP
013400070213     C                   exsr      impvab                                       => carico  VAB
013500070213     C                   endif
013600070103     C* Salvo il raggruppamento spedizione cliente corrente
013700070103     C                   eval      depSped = curSped
013800070213     C*
013900070213     C* Se collo successivo x stessa bolla
014000070213     C                   else
014100070213     C                   exsr      impvab                                       => carico  VAB
014200070103     C                   endif
014300091215     C*
014400100114     C  N54              exsr      exevate                                      => write VAT-E
014500000905     C*
014600000905     C                   else
014700000905     C                   eval      vinflg = '1'
014800050628     C                   endif
014900000905     C                   endif
015000000905     C*
015100000905     C  N70              update    tivin000
015200000905     C*
015300991022     C  N70              ENDdo
015400070213     C*
015500070213     C* Scarico testata bolla rimasta in sospesa
015600100120     C  N54              exsr      wrivab                                       => scarico VAB
015700010202     C*
015800010202     C                   endif
015900990910
016000990910     C* Se non ci sono record con errori ...
016100000710     C                   if        §ctrno = 0
016200990910     C* ... restituisco esito OK.
016300990921     C                   eval      wrkesito = '0'
016400990910     C                   else
016500010201     C                   if        §ctrokvb > 0
016600990921     C                   eval      wrkesito = '1'
016700000710     C                   else
016800000710     C                   eval      wrkesito = '2'
016900990910     C                   endif
017000000710     C                   endif
017100990910     C*
017200990914     C                   if        %open(tivin00r)
017300990908     C                   close     tivin00r
017400990914     C                   endif
017500090114     C                   if        %open(fivabwwr)
017600090114     C                   close     fivabwwr
017700990914     C                   endif
017800090114     C                   if        %open(fivatwwr)
017900090114     C                   close     fivatwwr
018000010201     C                   endif
018100990910     C*
018200010201     C                   if        §ctrokvb > 0
018300000724     C                             and vlrpoi <> *zeros
018400010202     C                   exsr      invio
018500990920     C                   endif
018600990920     C*
018700910830     C                   ENDSR
018800000613     C***
018900010305
019000010305     C*----------------------------------------------------*
019100020305     C*  SCARICAMENTO BUFFER RECORDS VAB
019200010305     C*----------------------------------------------------*
019300020305     C     WRIVAB        BEGSR
019400010305     C*
019500060225     C* Quindi scarico il buffer del file d testata
019600090114     C                   write     fivab000                                     => scarico il VAB
019700120614     C                   exsr      wrivgd
019800010305     C*
019900010305     C                   ENDSR
020000990920
020100000801     C*----------------------------------------------------*
020200000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020300000801     C*----------------------------------------------------*
020400010201     C     INZVAR        BEGSR
020500000801     C*
020600090114     C                   CLEAR                   FIVAB000
020700090114     C                   CLEAR                   FIVAT000
020800070213     C*
020900040802     C                   Z-ADD     *zeros        Num5_0            5 0
021000040802     C                   MOVEL     '0'           FlgCAS            1
021100100114     C                   setoff                                       54
021200000801     C*
021300000801     C                   ENDSR
021400000801     C*----------------------------------------------------*
021500000801     C*  IMPOSTAZIONE CAMPI COSTANTI
021600000801     C*----------------------------------------------------*
021700000801     C     DEFCAM        BEGSR
021800000801     C*
021900020619     C* Imposto i valori di default...
022000100114     C                   Z-ADD     0895460       VABCCM
022100100114     C                   Z-ADD     0895460       VATCCM
022200091214     C                   Z-ADD     089           VABLNP
022300091214     C                   Z-ADD     089           VATLNP
022400091214     C                   Z-ADD     303           VABCTR
022500040823     C                   MOVEL     '1'           VABCBO
022600090922     C                   MOVEL     '7Q'          VABCTM
022700020619     C* ... e poi verifico se sono stati passati come parametri
022800020619     C                   IF        vlrppt > *blanks
022900040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
023000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023100020619     C                   EXSR      CHKNUM
023200020619     C                   IF        PiInt=*on
023300020619     C                   Z-ADD     PiVal         VABCCM
023400020619     C                   Z-ADD     PiVal         VATCCM
023500020619     C                   ENDIF
023600040506     C                   ENDIF
023700040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
023800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023900020619     C                   EXSR      CHKNUM
024000020619     C                   IF        PiInt=*on
024100020619     C                   Z-ADD     PiVal         VABLNP
024200020619     C                   Z-ADD     PiVal         VATLNP
024300040506     C                   ENDIF
024400020619     C                   ENDIF
024500040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
024600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
024700020619     C                   EXSR      CHKNUM
024800020619     C                   IF        PiInt=*on
024900020619     C                   Z-ADD     PiVal         VABCTR
025000040506     C                   ENDIF
025100020619     C                   ENDIF
025200120614     C*
025300120614     C* Verifico se sui parametri del traduttore è richiesto il feedback dati input
025400120614     C                   if        %subst(vlrppt:14:1) = 'S'
025500120614     C                   seton                                        53
025600120614     C                   else
025700120614     C                   setoff                                       53
025800120614     C                   endif
025900130625     C*
026000130625     C* Verifico se sui parametri del traduttore è richiesto il chack forzatura RMN
026100130625     C                   if        %subst(vlrppt:15:1) = 'S'
026200130625     C                   seton                                        55
026300130625     C                   else
026400130625     C                   setoff                                       55
026500130625     C                   endif
026600120614     C*
026700020619     C                   ENDIF
026800000801     C*
026900000801     C                   ENDSR
027000091215     C*----------------------------------------------------*
027100091215     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
027200091215     C*----------------------------------------------------*
027300091215     C     REPNSP        BEGSR
027400091215     C*
027500091215     C                   EXSR      INZVAR
027600091215     C                   EXSR      DEFCAM
027700091215     C*
027800091215     C* NSP => Stacco un numeratore da AZNUM
027900091215     C                   clear                   TRUL33DS
028000091215     C                   eval      I33OPE = *zeros
028100091215     C                   eval      I33CNU = 302
028200091215     C                   eval      I33NUM = 1
028300091215     C                   movel     TRUL33DS      KPJBU
028400091215     C                   call      'TRUL33R'
028500091215     C                   parm                    KPJBA
028600091215     C                   movel     KPJBU         TRUL33DS
028700091215     C                   if        O33ERR = *zeros
028800091215     C                   z-add     O33NRF        VABNSP
028900091215     C                   z-add     O33NRF        VATNSP
029000091215     C                   else
029100091215     C                   ADD       1             errore
029200091215     C                   EVAL      vinmsg = %trimr(vinmsg)
029300091215     C                             + ' ' + 'VABNSP VATNSP'
029400091215     C                   endif
029500091215     C*
029600091215     C                   ENDSR
029700000801     C*----------------------------------------------------*
029800090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
029900000801     C*----------------------------------------------------*
030000040823     C     IMPVAB        BEGSR
030100010305     C*
030200000801     C                   Z-ADD     *zeros        errore            1 0
030300000830     C                   MOVEL     datcor        VABAAS
030400020305     C                   MOVEL     datcor        VATAAS
030500040526     C                   MOVE      datcor        VABMGS
030600040823     C                   MOVE(P)   vlrpoi        VABFGS
030700040823     C                   MOVE(P)   vlrpoi        VATFGS
030800080409     C*
030900080409     C* Reperimento campi ALFA
031000080409     C*
031100080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
031200091214     C                   EVAL      VABRSD=%trim(%subst(vindta:201:35))
031300080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
031400080409     C     '@':'A'       XLATE     VABRSD        VABRSD
031500080409     C* ==
031600091214     C                   EVAL      VABRD2=%trim(%subst(vindta:61:35))
031700091214     C                   EVAL      VABIND=%trim(%subst(vindta:96:35))  + ' ' +
031800091214     C                                    %trim(%subst(vindta:131:35)) + ' ' +
031900091214     C                                    %trim(%subst(vindta:166:35))
032000091214     C                   EVAL      VABLOD=%trim(%subst(vindta:281:35))
032100091216     C***                EVAL      VABRMA=%trim(%subst(vindta:955:35))
032200091216     C                   EVAL      VABRMA=%trim(%subst(vindta:1:35))
032300091214     C                   EVAL      VABNZD=%trim(%subst(vindta:371:3))
032400091214     C                   IF        VABNZD='IT' OR
032500091214     C                             VABNZD='VA' OR
032600091214     C                             VABNZD='SM'
032700091214     C                   EVAL      VABNZD=*blanks
032800091214     C                   ENDIF
032900091214     C                   EVAL      VABNAS=%trim(%subst(vindta:1072:35))
033000091214     C                   MOVEL     *blanks       wNOTE            70
033100091214     C                   EVAL      wNOTE =%trim(%subst(vindta:762:35))  + ' ' +
033200091214     C                                    %trim(%subst(vindta:797:35)) + ' ' +
033300091214     C                                    %trim(%subst(vindta:832:35)) + ' ' +
033400091214     C                                    %trim(%subst(vindta:867:35))
033500091214     C                   EVAL      VABNOT=%subst(wNOTE:1:35)
033600091214     C                   EVAL      VABNT2=%subst(wNOTE:35:35)
033700080409     C*
033800080409     C* Reperimento campi NUMERICI
033900090309     C* RMN
034000091215     C                   EVAL      PiStr=%trim(%subst(vindta:1+2:35-2))
034100091215     C                   EXSR      CHKNUM
034200091215     C                   IF        PiInt=*on
034300091215     C                   Z-ADD     PiVal         VABRMN
034400091215     C                   ELSE
034500091215     C                   ADD       1             errore
034600091215     C                   Z-ADD     1             VABRMN
034700091215     C                   EVAL      vinmsg = %trimr(vinmsg)
034800091215     C                             + ' ' + 'VABRMN'
034900091215     C                   ENDIF
035000100114     C*
035100100114     C* Verifico che x il CCM corrente nn esiste già (ovunque) una bolla con medesimo
035200100114     C* riferimento => altrimenti escludo
035300130625     C                   if        VABRMN <> 1 and *in55
035400100114     C                   callp(e)  UBCHKRMN_Exist(VABCCM
035500100114     C                                           :VABRMN
035600100114     C                                           :pOutFIVAB
035700100114     C                                           :pOutFNBLP
035800100114     C                                           :pOutTITAS)
035900100114     C*
036000100114     C                   if        not %error
036100100114     C                   if        pOutFIVAB = *off AND
036200100114     C                             pOutFNBLP = *off AND
036300100114     C                             pOutTITAS = *off
036400100114     C                   else
036500100114     C                   seton                                        54
036600100114     C                   endif
036700100114     C                   endif
036800100114     C                   endif
036900100114     C*
037000100114     C                   if        not *in54
037100080409     C* CAD
037200091214     C                   EVAL      PiStr=%trim(%subst(vindta:271:10))
037300090114     C                   EXSR      CHKNUM
037400090114     C                   IF        PiInt=*on
037500090114     C                   Z-ADD     PiVal         Num5_0
037600090114     C                   MOVEL(p)  Num5_0        VABCAD
037700090114     C                   ELSE
037800090114     C                   ADD       1             errore
037900090114     C                   EVAL      VABCAD = *zeros
038000090114     C                   EVAL      vinmsg = %trimr(vinmsg)
038100090114     C                             + ' ' + 'VABCAD'
038200090114     C                   ENDIF
038300091214     C*
038400091214     C* Reperisco la provincia dal CAP e dalla località
038500091214     C                   IF        VABCAD <> *blanks AND
038600091214     C                             VABPRD  = *blanks AND
038700091214     C                             VABNZD  = *blanks
038800091214     C                   CLEAR                   TISI95DS
038900091214     C                   EVAL      I95TCN = '3'
039000091214     C                   Z-ADD     datcor        I95DAT
039100091214     C                   EVAL      I95CAP = VABCAD
039200091214     C                   EVAL      I95LOC = VABLOD
039300091214     C                   CALL      'TISI95R'
039400091214     C                   PARM                    TISI95DS
039500091214     C                   EVAL      VABPRD = O95PRV
039600091214     C                   ENDIF
039700091214     C*
039800091214     C* NCL
039900091214     C* ...calcolato in sommatoria dei singoli dettagli
040000091215     C* PKB
040100091215     C* ...calcolato in sommatoria dei singoli dettagli
040200091215     C* IAS
040300091215     C                   IF        %subst(vindta:1006:9) <> *blanks AND
040400091215     C                             %subst(vindta:1006:9) <> *zeros  AND
040500091215     C                             %subst(vindta:1006:9) <> '000000000'
040600091215     C                   EVAL      VABVAS=%trim(%subst(vindta:1015:4))
040700091215     C                   EVAL      PiStr=%trim(%subst(vindta:1006:9))
040800091215     C                   EXSR      CHKNUM
040900091215     C                   IF        PiNum=*on
041000091215     C                   EVAL      PiVal = PiVal / 100                          *da cents a euro
041100091215     C                   Z-ADD     PiVal         VABIAS
041200091215     C                   ELSE
041300091215     C                   ADD       1             errore
041400091215     C                   EVAL      vinmsg = %trimr(vinmsg)
041500091215     C                             + ' ' + 'VABIAS'
041600091215     C                   ENDIF
041700091215     C                   ENDIF
041800090922     C* CAS
041900091214     C                   IF        %subst(vindta:1019:9) <> *blanks AND
042000091214     C                             %subst(vindta:1019:9) <> *zeros  AND
042100091214     C                             %subst(vindta:1019:9) <> '000000000'
042200090922     C                   EVAL      FlgCAS = '1'
042300091214     C                   EVAL      VABVCA=%trim(%subst(vindta:1015:4))
042400091214     C                   EVAL      VABTIC=*blanks
042500091214     C                   EVAL      PiStr=%trim(%subst(vindta:1019:9))
042600090922     C                   EXSR      CHKNUM
042700090922     C                   IF        PiNum=*on
042800091214     C                   EVAL      PiVal = PiVal / 100                          *da cents a euro
042900090922     C                   Z-ADD     PiVal         VABCAS
043000090922     C                   ELSE
043100090922     C                   ADD       1             errore
043200090922     C                   EVAL      vinmsg = %trimr(vinmsg)
043300090922     C                             + ' ' + 'VABCAS'
043400090922     C                   ENDIF
043500090922     C                   ENDIF
043600071121     C*
043700080415     C* Carico l'estensioni A e B del FNVAT
043800080512     C                   exsr      exevata
043900080512     C                   exsr      exevatb
044000010205     C*
044100010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
044200040802     C                   IF        FlgCAS <> '0'
044300010205     C                   EVAL      VABCBO = '4'
044400010205     C                   ENDIF
044500020305     C*
044600011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
044700011113     C                   EXSR      CHKIMPDIV
044800100114     C*
044900100114     C                   endif
045000010202     C*
045100000801     C* Ebbene...
045200000801     C                   ADD       1             §CTRMO
045300010201     C                   IF        errore <> *zeros
045400000801     C                   ADD       1             §CTRNO
045500000801     C                   EVAL      vinflg = '2'
045600000801     C                   ELSE
045700010201     C                   ADD       1             §CTROKVB
045800000801     C                   ENDIF
045900000801     C*
046000000801     C                   ENDSR
046100070102     C*----------------------------------------------------*
046200090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
046300070102     C*----------------------------------------------------*
046400070103     C     EXEVATE       BEGSR
046500070102     C*
046600091215     C* NCL
046700091214     C                   eval      VABNCL = VABNCL + 1
046800091215     C* PKB
046900091215     C                   EVAL      PiStr=%trim(%subst(vindta:38:8))
047000091215     C                   EXSR      CHKNUM
047100091215     C                   IF        PiNum=*on
047200091215     C                   EVAL      PiVal = PiVal / 100                          *da decagrammi a Kg.
047300091215     C                   ADD       PiVal         VABPKB
047400091215     C                   ELSE
047500091215     C                   ADD       1             errore
047600091215     C                   EVAL      vinmsg = %trimr(vinmsg)
047700091215     C                             + ' ' + 'VABPKB'
047800091215     C                   ENDIF
047900130625     C* CAS
048000130625     C                   IF        %subst(vindta:1019:9) <> *blanks AND
048100130625     C                             %subst(vindta:1019:9) <> *zeros  AND
048200130625     C                             %subst(vindta:1019:9) <> '000000000'
048300130625     C                   EVAL      FlgCAS = '1'
048400130625     C                   EVAL      VABVCA=%trim(%subst(vindta:1015:4))
048500130625     C                   EVAL      VABTIC=*blanks
048600130625     C                   EVAL      PiStr=%trim(%subst(vindta:1019:9))
048700130625     C                   EXSR      CHKNUM
048800130625     C                   IF        PiNum=*on
048900130625     C                   EVAL      PiVal = PiVal / 100                          *da cents a euro
049000130625     C                   ADD       PiVal         VABCAS
049100130625     C                   ELSE
049200130625     C                   ADD       1             errore
049300130625     C                   EVAL      vinmsg = %trimr(vinmsg)
049400130625     C                             + ' ' + 'VABCAS'
049500130625     C                   ENDIF
049600130625     C                   ENDIF
049700091214     C*
049800090428     C                   EVAL      VATTRC = 'E'
049900091214     C                   EVAL      VATNOT = %trim(%subst(vindta:920:35))
050000090428     C*
050100090922     C                   exsr      wrivat                                       => scarico VAT
050200070102     C*
050300070102     C                   ENDSR
050400080415     C*----------------------------------------------------*
050500090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
050600080415     C*----------------------------------------------------*
050700080415     C     EXEVATA       BEGSR
050800080415     C*
050900091214     C***                EVAL      VATTRC = 'A'
051000091214     C***                EVAL      VATNOT = %trim(%subst(vindta:376:15))
051100080415     C*
051200091215     C***                exsr      wrivat                                       => scarico VAT
051300080415     C*
051400080415     C                   ENDSR
051500071121     C*----------------------------------------------------*
051600090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
051700071121     C*----------------------------------------------------*
051800071121     C     EXEVATB       BEGSR
051900071121     C*
052000090504     C                   EVAL      VATTRC = 'B'
052100091214     C                   EVAL      VATNOT = %trim(%subst(vindta:374:30))
052200071121     C*
052300090922     C                   exsr      wrivat                                       => scarico VAT
052400071121     C*
052500071121     C                   ENDSR
052600010201     C*----------------------------------------------------*
052700090114     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
052800010201     C*----------------------------------------------------*
052900020305     C     WRIVAT        BEGSR
053000050628     C*
053100060223     C* Scrivo solo se valorizzato qualcosa
053200060223     C                   IF        VATNOT <> *blanks
053300090922     C                   WRITE     FIVAT000
053400090922     C                   ADD       1             §ctrokvt
053500060223     C                   ENDIF
053600010201     C*
053700010201     C                   ENDSR
053800010202     C*----------------------------------------------------*
053900090114     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
054000010202     C*----------------------------------------------------*
054100020305     C     PREVAT        BEGSR
054200010202     C*
054300090114     C* Compongo il nome del membro da dare al FIVATWWR
054400010202     C                   eval      parmbr = vlrhdl
054500010202     C                   movel     'M'           parmbr
054600050627     C                   eval      parccm = %subst(vlrKSC:2:7)
054700010202     C                   eval      paropz = '1'
054800010202     C* Effettuo la chiamata al CLLE preposto
054900090114     C                   call(e)   'TITVVTC'
055000010202     C                   parm                    parccm
055100010202     C                   parm                    parmbr
055200010202     C                   parm                    paropz
055300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
055400010202     C                   if        %error
055500010202     C                   movel     '1'           chkcall
055600010202     C                   else
055700010202     C                   movel     '0'           chkcall
055800010202     C                   endif
055900010202     C*
056000010202     C                   ENDSR
056100000801     C*----------------------------------------------------*
056200000801     C*  CONTROLLO NUMERICITA' CAMPI
056300000801     C*----------------------------------------------------*
056400000801     C     CHKNUM        BEGSR
056500081217     C*
056600081217     C                   IF        PiDecChr = *blanks
056700090922     C                   EVAL      PiDecChr = '.'
056800081217     C                   ENDIF
056900081217     C*
057000081217     C                   callp(e)  UBISNUM_Check(PiStr
057100081217     C                                          :PiDecChr
057200081217     C                                          :PiVal
057300081217     C                                          :PiNum
057400081217     C                                          :PiInt)
057500081217     C*
057600081217     C                   IF        %error
057700081217     C                   EVAL      PiInt=*off
057800081217     C                   ENDIF
057900000801     C*
058000000801     C                   ENDSR
058100000801     C***
058200000801
058300011113
058400011113     C*----------------------------------------------------*
058500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
058600011113     C*----------------------------------------------------*
058700011113     C     CHKIMPDIV     BEGSR
058800011113     C*
058900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
059000011113     C                   Z-ADD     *zeros        wrkDec            9 9
059100011113     C*
059200011113     C* Come prima cosa effettuo considerazioni sulla divisa
059300011113     C                   IF        vabIAS > *zeros
059400011113     C                   IF        vabVAS <> 'EUR'
059500011113     C                   EVAL      vabVAS =  'ITL'
059600011113     C                   ENDIF
059700011113     C                   ENDIF
059800011113     C*
059900011113     C                   IF        vabCAS > *zeros
060000011113     C                   IF        vabVCA <> 'EUR'
060100011113     C                   EVAL      vabVCA =  'ITL'
060200011113     C                   ENDIF
060300011113     C                   ENDIF
060400011113     C*
060500011113     C                   IF        vabVMD > *zeros
060600020305     C                   IF        vabVAD <> 'EUR'
060700011113     C                   EVAL      vabVAD =  'ITL'
060800011113     C                   ENDIF
060900011113     C                   ENDIF
061000011113     C*
061100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
061200011113     C                   Z-ADD     vabIAS        wrkDec
061300011113     C                   IF        wrkDec > *zeros
061400011113     C                   IF        vabVAS = 'ITL'
061500011113     C                   EVAL      vabIAS = *zeros
061600011113     C                   ENDIF
061700011113     C                   ENDIF
061800011113     C*
061900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
062000011113     C                   Z-ADD     vabCAS        wrkDec
062100011113     C                   IF        wrkDec > *zeros
062200011113     C                   IF        vabVCA = 'ITL'
062300011113     C                   EVAL      vabCAS = *zeros
062400011113     C                   ENDIF
062500011113     C                   ENDIF
062600011113     C*
062700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
062800011113     C                   Z-ADD     vabVMD        wrkDec
062900011113     C                   IF        wrkDec > *zeros
063000011113     C                   IF        vabVAD = 'ITL'
063100011113     C                   EVAL      vabVMD = *zeros
063200011113     C                   ENDIF
063300011113     C                   ENDIF
063400011113     C*
063500011113     C                   ENDSR
063600011113     C***
063700011113
063800011113
063900000801
064000000801
064100990920      /TITLE Invio dei dati al punto operativo.
064200010202     C     invio         BEGSR
064300990920     C*
064400090114     C* 1° invio FIVAT
064500010201     C                   reset                   dscmz
064600010201     C                   move      vlrpoi        cmzdst
064700090114     C                   eval      cmzfld = 'FIVATWWR'
064800010201     C                   eval      cmzmbd = vlrhdl
064900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
065000021009     C***                if        prmfir = *blanks
065100090114     C                   eval      cmzfla = 'FIVAT00F'
065200090114     C                   eval      cmzmba = 'FIVAT00F'
065300021009     C***                else
065400021009     C***                eval      cmzfla = prmfir
065500021009     C***                eval      cmzmba = prmfir
065600021009     C***                endif
065700010201     C                   eval      cmznrr = *zeros
065800020305     C                   move      §ctrokvt      cmznrr
065900021018     C                   eval      cmzlba = vlrfl1
066000010201     C                   call(e)   'TIS711C'
066100010201     C                   parm                    dscmz
066200010201     C                   parm      *blanks       esito
066300010205     C                   if        %error
066400010205     C                             or cmzerr = '1'
066500010205     C                             or esito  = '1'
066600010205     C                   eval      wrkesito = '3'
066700010205     C                   else
066800010201     C*
066900090114     C* 2° invio FIVAB
067000010201     C                   reset                   dscmz
067100010201     C                   move      vlrpoi        cmzdst
067200010201     C                   eval      cmzfld = vlrfou
067300010201     C                   eval      cmzmbd = vlrhdl
067400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
067500021009     C***                if        prmfir = *blanks
067600090114     C                   eval      cmzfla = 'FIVAB00F'
067700090114     C                   eval      cmzmba = 'FIVAB00F'
067800021009     C***                else
067900021009     C***                eval      cmzfla = prmfir
068000021009     C***                eval      cmzmba = prmfir
068100021009     C***                endif
068200010201     C                   eval      cmznrr = *zeros
068300010201     C                   move      §ctrokvb      cmznrr
068400021018     C                   eval      cmzlba = vlrfl1
068500010201     C                   call(e)   'TIS711C'
068600010201     C                   parm                    dscmz
068700010201     C                   parm      *blanks       esito
068800010201     C                   if        %error
068900010201     C                             or cmzerr = '1'
069000010201     C                             or esito  = '1'
069100010201     C                   eval      wrkesito = '3'
069200010201     C                   endif
069300010205     C                   endif
069400990920     C*
069500000613     C                   ENDSR
069600000613     C***
069700070411
069800090331
069900090331
070000090331
070100070411     C     *pssr         BEGSR
070200070411     C*
070300070411     C                   if        %open(tivin00r)
070400070411     C                   close     tivin00r
070500070411     C                   endif
070600090114     C                   if        %open(fivabwwr)
070700090114     C                   close     fivabwwr
070800070411     C                   endif
070900090114     C                   if        %open(fivatwwr)
071000090114     C                   close     fivatwwr
071100070411     C                   endif
071200070411     C*
071300070411     C* Effettuo la chiamata al CLLE preposto
071400090114     C                   call(e)   'TITVVTC'
071500070411     C                   parm                    parccm
071600070411     C                   parm                    parmbr
071700070411     C                   parm      '2'           paropz
071800100114     C*
071900100114     C* Metodo chiusura UBCHKRMN
072000100114     C                   callp(e)  UBCHKRMN_Close()
072100070411     C*
072200070411     C                   eval      wrkesito = '2'
072300070411     C*
072400070411     C                   seton                                        LR
072500070411     C*
072600070411     C                   ENDSR     '*CANCL'
072700070411     C***
072800120614
072900120614
073000120614
073100120614      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
073200120614     C     wriVGD        BEGSR
073300120614     C*
073400120614     C                   clear                   tivgd000
073500120614     C                   eval      vgdDTA = fnvab00t
073600120614     C                   eval      vgdTIP = 'EB'
073700120614     C                   eval      vgdKSU = vlrKSC
073800120614     C                   eval      vgdTSC = 'WW'
073900120614     C                   eval      vgdDAT = datcor
074000120614     C                   eval      vgdPGM = 'TITV1T5R'
074100120614     C                   write     tivgd000
074200120614     C*
074300120614     C                   ENDSR
074400120614     C*------------------------------------------------------------------------*
074500120614
074600070411
074700990910
074800000613     C     *inzsr        BEGSR
074900990910     C*
075000990910     C     *entry        plist
075100990920     C                   parm                    tivlrds
075200990921     C                   parm      wrkesito      esito
075300000724     C                   parm                    prmlit
075400000710     C                   parm                    prmfir
075500000613     C*
075600091214     C                   z-add     *zeros        datcor            8 0
075700091214     C                   eval      datcor = %dec(%date() : *ISO)
075800000830     C*
075900000613     C                   ENDSR
076000000613     C***
