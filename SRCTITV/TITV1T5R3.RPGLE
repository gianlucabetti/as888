000100141024      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200120614     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300120614     H DECEDIT('0,') DATEDIT(*DMY.)
000400990908
000500990910     Ftivin00r  uF   E             DISK    usropn
000600141024     FEDIVABwr  O    E             DISK    usropn
000700141024     FEDIVATwr  O    E             DISK    usropn
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
003000141024     D wNomeFile       s             30    INZ(*blanks)
003100141024     D wCMR            s             35    INZ(*blanks)
003200141024     D wPosDaA         s              2    INZ(*blanks)
003300141024     D wPosDa          s              2  0 INZ(*zeros)
003400141024     D wLungA          s              2    INZ(*blanks)
003500141024     D wLung           s              2  0 INZ(*zeros)
003600090114
003700091215     D*------------------
003800091215     D* DS REPERIMENTO NUMERATORE
003900091215     D*------------------
004000091215     D trul33ds      e ds                  inz
004100091215     D*------------------
004200091215     D* DS ARCHITETTURA
004300091215     D*------------------
004400091215     D kpjba         e ds                  inz
004500091215
004600081217
004700081217     D*------------------
004800081217     D* LINKING A DEFINIZIONI ESTERNE
004900081217     D*------------------
005000081217     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005100081217     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005200100114     D/COPY GAITRASRC/SRCPROTOPR,UBCHKRMN
005300100114     D/COPY GAITRASRC/SRCPROTOPI,UBCHKRMN
005400081217
005500081217
005600010201
005700081217
005800100114     C*
005900100114     C* Metodo apertura UBCHKRMN
006000100114     C                   callp(e)  UBCHKRMN_Open()
006100100114     C*
006200000913     C                   reset                   rrnum
006300990921     C                   reset                   esito
006400990921     C                   reset                   wrkesito
006500000613     C*
006600040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
006700000613     C*
006800010202     C* Effettuo la chiamata al CLLE preposto
006900090114     C                   call(e)   'TITVVTC'
007000010202     C                   parm                    parccm
007100010202     C                   parm                    parmbr
007200010202     C                   parm      '2'           paropz
007300050201     C*
007400050201     C* Effettuo lancio TISI95 solo x chiusura
007500050201     C                   CLEAR                   TISI95DS
007600050201     C                   EVAL      I95TLA = 'C'
007700050201     C                   CALL      'TISI95R'
007800050201     C                   PARM                    TISI95DS
007900000616     C*
008000100114     C* Metodo chiusura UBCHKRMN
008100100114     C                   callp(e)  UBCHKRMN_Close()
008200100114     C*
008300000801     C
008400010201     C                   seton                                        LR
008500990908
008600000801
008700910830     C*--------------------------------------------------------
008800141024     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
008900910830     C*--------------------------------------------------------
009000040526     C     RWFILE        BEGSR
009100990910     C*
009200990914     C                   if        not %open(tivin00r)
009300990908     C                   open      tivin00r
009400990914     C                   endif
009500141024     C                   if        not %open(edivabwr)
009600141024     C                   open      edivabwr
009700990914     C                   endif
009800070103     C*
009900141024     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
010000020305     C                   exsr      prevat
010100010201     C*
010200010202     C                   if        chkcall = '0'
010300010202     C*
010400141024     C                   if        not %open(edivatwr)
010500141024     C                   open      edivatwr
010600010201     C                   endif
010700990910     C*
010800010201     C                   clear                   §CTROKVB          5 0
010900020305     C                   clear                   §CTROKVT          5 0
011000000801     C                   clear                   §CTRMO            5 0
011100000801     C                   clear                   §CTRNO            5 0
011200990910     C*
011300921023     C                   DO        *HIVAL
011400990913     C*
011500990915     C                   READ      tivin00r                               70
011600050627     C                   if        vindta > *blanks
011700000613     C                   add       1             rrnum
011800000801     C*
011900091214     C                   if        *in70 = *off
012000091214 xxx C                             and rrnum > 1
012100000801     C                             and
012200000801     C                             (vinflg = *blanks
012300000801     C                              or vinflg = '0'
012400000801     C                              or vinflg = '2')
012500000801     C*
012600000801     C                   clear                   vinmsg
012700000801     C                   eval      vinflg = '1'
012800070103     C*
012900070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
013000091214     C                   eval      curSped = %subst(vindta:1:35)
013100070213     C                   if        curSped <> depSped
013200070213     C* Se prima bolla => importo bolla corrente
013300070213     C                   if        §CTRMO = *zeros
013400091215     C                   exsr      repNSP
013500070213     C                   exsr      impvab                                       => carico  VAB
013600070213     C                   else
013700070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
013800100120     C  N54              exsr      wrivab                                       => scarico VAB
013900091215     C                   exsr      repNSP
014000070213     C                   exsr      impvab                                       => carico  VAB
014100070213     C                   endif
014200070103     C* Salvo il raggruppamento spedizione cliente corrente
014300070103     C                   eval      depSped = curSped
014400070213     C*
014500070213     C* Se collo successivo x stessa bolla
014600070213     C                   else
014700070213     C                   exsr      impvab                                       => carico  VAB
014800070103     C                   endif
014900091215     C*
015000100114     C  N54              exsr      exevate                                      => write VAT-E
015100000905     C*
015200000905     C                   else
015300000905     C                   eval      vinflg = '1'
015400050628     C                   endif
015500000905     C                   endif
015600000905     C*
015700000905     C  N70              update    tivin000
015800000905     C*
015900991022     C  N70              ENDdo
016000070213     C*
016100070213     C* Scarico testata bolla rimasta in sospesa
016200100120     C  N54              exsr      wrivab                                       => scarico VAB
016300010202     C*
016400010202     C                   endif
016500990910
016600990910     C* Se non ci sono record con errori ...
016700000710     C                   if        §ctrno = 0
016800990910     C* ... restituisco esito OK.
016900990921     C                   eval      wrkesito = '0'
017000990910     C                   else
017100010201     C                   if        §ctrokvb > 0
017200990921     C                   eval      wrkesito = '1'
017300000710     C                   else
017400000710     C                   eval      wrkesito = '2'
017500990910     C                   endif
017600000710     C                   endif
017700990910     C*
017800990914     C                   if        %open(tivin00r)
017900990908     C                   close     tivin00r
018000990914     C                   endif
018100141024     C                   if        %open(edivabwr)
018200141024     C                   close     edivabwr
018300990914     C                   endif
018400141024     C                   if        %open(edivatwr)
018500141024     C                   close     edivatwr
018600010201     C                   endif
018700990910     C*
018800010201     C                   if        §ctrokvb > 0
018900000724     C                             and vlrpoi <> *zeros
019000010202     C                   exsr      invio
019100990920     C                   endif
019200990920     C*
019300910830     C                   ENDSR
019400000613     C***
019500010305
019600010305     C*----------------------------------------------------*
019700020305     C*  SCARICAMENTO BUFFER RECORDS VAB
019800010305     C*----------------------------------------------------*
019900020305     C     WRIVAB        BEGSR
020000141024     C*
020100141024     C* VALORIZZO CAMPI RELATIVI AL "CMR"
020200141024     C                   EVAL      VABCMR = wCMR
020300141024     C                   EVAL      VABDCM = DATCOR
020400141024     C                   EVAL      VABDTS = DATCOR
020500141024     C                   EVAL      VABHMS = ORACOR
020600141024     C                   EVAL      VABCNT = 1
020700010305     C*
020800060225     C* Quindi scarico il buffer del file d testata
020900141024     C                   write     edivab00                                     => scarico il VAB
021000140731     C  N56              exsr      wrivgd
021100010305     C*
021200010305     C                   ENDSR
021300990920
021400000801     C*----------------------------------------------------*
021500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021600000801     C*----------------------------------------------------*
021700010201     C     INZVAR        BEGSR
021800000801     C*
021900141024     C                   CLEAR                   EDIVAB00
022000141024     C                   CLEAR                   EDIVAT00
022100070213     C*
022200040802     C                   Z-ADD     *zeros        Num5_0            5 0
022300040802     C                   MOVEL     '0'           FlgCAS            1
022400100114     C                   setoff                                       54
022500000801     C*
022600000801     C                   ENDSR
022700000801     C*----------------------------------------------------*
022800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
022900000801     C*----------------------------------------------------*
023000000801     C     DEFCAM        BEGSR
023100000801     C*
023200020619     C* Imposto i valori di default...
023300100114     C                   Z-ADD     0895460       VABCCM
023400100114     C                   Z-ADD     0895460       VATCCM
023500091214     C                   Z-ADD     089           VABLNP
023600091214     C                   Z-ADD     089           VATLNP
023700091214     C                   Z-ADD     303           VABCTR
023800040823     C                   MOVEL     '1'           VABCBO
023900090922     C                   MOVEL     '7Q'          VABCTM
024000020619     C* ... e poi verifico se sono stati passati come parametri
024100020619     C                   IF        vlrppt > *blanks
024200040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
024300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
024400020619     C                   EXSR      CHKNUM
024500020619     C                   IF        PiInt=*on
024600020619     C                   Z-ADD     PiVal         VABCCM
024700020619     C                   Z-ADD     PiVal         VATCCM
024800020619     C                   ENDIF
024900040506     C                   ENDIF
025000040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
025100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
025200020619     C                   EXSR      CHKNUM
025300020619     C                   IF        PiInt=*on
025400020619     C                   Z-ADD     PiVal         VABLNP
025500020619     C                   Z-ADD     PiVal         VATLNP
025600040506     C                   ENDIF
025700020619     C                   ENDIF
025800040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
025900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
026000020619     C                   EXSR      CHKNUM
026100020619     C                   IF        PiInt=*on
026200020619     C                   Z-ADD     PiVal         VABCTR
026300040506     C                   ENDIF
026400020619     C                   ENDIF
026500120614     C*
026600120614     C* Verifico se sui parametri del traduttore è richiesto il feedback dati input
026700120614     C                   if        %subst(vlrppt:14:1) = 'S'
026800120614     C                   seton                                        53
026900120614     C                   else
027000120614     C                   setoff                                       53
027100120614     C                   endif
027200130625     C*
027300130625     C* Verifico se sui parametri del traduttore è richiesto il chack forzatura RMN
027400130625     C                   if        %subst(vlrppt:15:1) = 'S'
027500130625     C                   seton                                        55
027600130625     C                   else
027700130625     C                   setoff                                       55
027800130625     C                   endif
027900140731     C*
028000140731     C* Verifico se sui parametri del traduttore è richiesto il feed-back dati ricevuti
028100140731     C                   if        %subst(vlrppt:16:1) = 'N'
028200140731     C                   seton                                        56
028300140731     C                   else
028400140731     C                   setoff                                       56
028500141024 xxx C                   endif
028600120614     C*
028700020619     C                   ENDIF
028800000801     C*
028900000801     C                   ENDSR
029000091215     C*----------------------------------------------------*
029100091215     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
029200091215     C*----------------------------------------------------*
029300091215     C     REPNSP        BEGSR
029400091215     C*
029500091215     C                   EXSR      INZVAR
029600091215     C                   EXSR      DEFCAM
029700091215     C*
029800091215     C* NSP => Stacco un numeratore da AZNUM
029900091215     C                   clear                   TRUL33DS
030000091215     C                   eval      I33OPE = *zeros
030100091215     C                   eval      I33CNU = 302
030200091215     C                   eval      I33NUM = 1
030300091215     C                   movel     TRUL33DS      KPJBU
030400091215     C                   call      'TRUL33R'
030500091215     C                   parm                    KPJBA
030600091215     C                   movel     KPJBU         TRUL33DS
030700091215     C                   if        O33ERR = *zeros
030800091215     C                   z-add     O33NRF        VABNSP
030900091215     C                   z-add     O33NRF        VATNSP
031000091215     C                   else
031100091215     C                   ADD       1             errore
031200091215     C                   EVAL      vinmsg = %trimr(vinmsg)
031300091215     C                             + ' ' + 'VABNSP VATNSP'
031400091215     C                   endif
031500091215     C*
031600091215     C                   ENDSR
031700000801     C*----------------------------------------------------*
031800141024     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
031900000801     C*----------------------------------------------------*
032000040823     C     IMPVAB        BEGSR
032100010305     C*
032200000801     C                   Z-ADD     *zeros        errore            1 0
032300000830     C                   MOVEL     datcor        VABAAS
032400020305     C                   MOVEL     datcor        VATAAS
032500040526     C                   MOVE      datcor        VABMGS
032600040823     C                   MOVE(P)   vlrpoi        VABFGS
032700040823     C                   MOVE(P)   vlrpoi        VATFGS
032800080409     C*
032900080409     C* Reperimento campi ALFA
033000080409     C*
033100080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
033200091214     C                   EVAL      VABRSD=%trim(%subst(vindta:201:35))
033300080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
033400080409     C     '@':'A'       XLATE     VABRSD        VABRSD
033500080409     C* ==
033600091214     C                   EVAL      VABRD2=%trim(%subst(vindta:61:35))
033700091214     C                   EVAL      VABIND=%trim(%subst(vindta:96:35))  + ' ' +
033800091214     C                                    %trim(%subst(vindta:131:35)) + ' ' +
033900091214     C                                    %trim(%subst(vindta:166:35))
034000091214     C                   EVAL      VABLOD=%trim(%subst(vindta:281:35))
034100091216     C***                EVAL      VABRMA=%trim(%subst(vindta:955:35))
034200091216     C                   EVAL      VABRMA=%trim(%subst(vindta:1:35))
034300091214     C                   EVAL      VABNZD=%trim(%subst(vindta:371:3))
034400091214     C                   IF        VABNZD='IT' OR
034500091214     C                             VABNZD='VA' OR
034600091214     C                             VABNZD='SM'
034700091214     C                   EVAL      VABNZD=*blanks
034800091214     C                   ENDIF
034900091214     C                   EVAL      VABNAS=%trim(%subst(vindta:1072:35))
035000091214     C                   MOVEL     *blanks       wNOTE            70
035100091214     C                   EVAL      wNOTE =%trim(%subst(vindta:762:35))  + ' ' +
035200091214     C                                    %trim(%subst(vindta:797:35)) + ' ' +
035300091214     C                                    %trim(%subst(vindta:832:35)) + ' ' +
035400091214     C                                    %trim(%subst(vindta:867:35))
035500091214     C                   EVAL      VABNOT=%subst(wNOTE:1:35)
035600140225     C                   EVAL      VABNT2=%subst(wNOTE:36:35)
035700080409     C*
035800080409     C* Reperimento campi NUMERICI
035900090309     C* RMN
036000091215     C                   EVAL      PiStr=%trim(%subst(vindta:1+2:35-2))
036100091215     C                   EXSR      CHKNUM
036200091215     C                   IF        PiInt=*on
036300091215     C                   Z-ADD     PiVal         VABRMN
036400091215     C                   ELSE
036500091215     C                   ADD       1             errore
036600091215     C                   Z-ADD     1             VABRMN
036700091215     C                   EVAL      vinmsg = %trimr(vinmsg)
036800091215     C                             + ' ' + 'VABRMN'
036900091215     C                   ENDIF
037000100114     C*
037100100114     C* Verifico che x il CCM corrente nn esiste già (ovunque) una bolla con medesimo
037200100114     C* riferimento => altrimenti escludo
037300130625     C                   if        VABRMN <> 1 and *in55
037400100114     C                   callp(e)  UBCHKRMN_Exist(VABCCM
037500100114     C                                           :VABRMN
037600100114     C                                           :pOutFIVAB
037700100114     C                                           :pOutFNBLP
037800100114     C                                           :pOutTITAS)
037900100114     C*
038000100114     C                   if        not %error
038100100114     C                   if        pOutFIVAB = *off AND
038200100114     C                             pOutFNBLP = *off AND
038300100114     C                             pOutTITAS = *off
038400100114     C                   else
038500100114     C                   seton                                        54
038600100114     C                   endif
038700100114     C                   endif
038800100114     C                   endif
038900100114     C*
039000100114     C                   if        not *in54
039100080409     C* CAD
039200091214     C                   EVAL      PiStr=%trim(%subst(vindta:271:10))
039300090114     C                   EXSR      CHKNUM
039400090114     C                   IF        PiInt=*on
039500090114     C                   Z-ADD     PiVal         Num5_0
039600090114     C                   MOVEL(p)  Num5_0        VABCAD
039700090114     C                   ELSE
039800090114     C                   ADD       1             errore
039900090114     C                   EVAL      VABCAD = *zeros
040000090114     C                   EVAL      vinmsg = %trimr(vinmsg)
040100090114     C                             + ' ' + 'VABCAD'
040200090114     C                   ENDIF
040300091214     C*
040400091214     C* Reperisco la provincia dal CAP e dalla località
040500091214     C                   IF        VABCAD <> *blanks AND
040600091214     C                             VABPRD  = *blanks AND
040700091214     C                             VABNZD  = *blanks
040800091214     C                   CLEAR                   TISI95DS
040900091214     C                   EVAL      I95TCN = '3'
041000091214     C                   Z-ADD     datcor        I95DAT
041100091214     C                   EVAL      I95CAP = VABCAD
041200091214     C                   EVAL      I95LOC = VABLOD
041300091214     C                   CALL      'TISI95R'
041400091214     C                   PARM                    TISI95DS
041500091214     C                   EVAL      VABPRD = O95PRV
041600091214     C                   ENDIF
041700091214     C*
041800091214     C* NCL
041900091214     C* ...calcolato in sommatoria dei singoli dettagli
042000091215     C* PKB
042100091215     C* ...calcolato in sommatoria dei singoli dettagli
042200091215     C* IAS
042300091215     C                   IF        %subst(vindta:1006:9) <> *blanks AND
042400091215     C                             %subst(vindta:1006:9) <> *zeros  AND
042500091215     C                             %subst(vindta:1006:9) <> '000000000'
042600091215     C                   EVAL      VABVAS=%trim(%subst(vindta:1015:4))
042700091215     C                   EVAL      PiStr=%trim(%subst(vindta:1006:9))
042800091215     C                   EXSR      CHKNUM
042900091215     C                   IF        PiNum=*on
043000091215     C                   EVAL      PiVal = PiVal / 100                          *da cents a euro
043100091215     C                   Z-ADD     PiVal         VABIAS
043200091215     C                   ELSE
043300091215     C                   ADD       1             errore
043400091215     C                   EVAL      vinmsg = %trimr(vinmsg)
043500091215     C                             + ' ' + 'VABIAS'
043600091215     C                   ENDIF
043700091215     C                   ENDIF
043800090922     C* CAS
043900130627     C***                IF        %subst(vindta:1019:9) <> *blanks AND
044000130627     C***                          %subst(vindta:1019:9) <> *zeros  AND
044100130627     C***                          %subst(vindta:1019:9) <> '000000000'
044200130627     C***                EVAL      FlgCAS = '1'
044300130627     C***                EVAL      VABVCA=%trim(%subst(vindta:1015:4))
044400130627     C***                EVAL      VABTIC=*blanks
044500130627     C***                EVAL      PiStr=%trim(%subst(vindta:1019:9))
044600130627     C***                EXSR      CHKNUM
044700130627     C***                IF        PiNum=*on
044800130627     C***                EVAL      PiVal = PiVal / 100                          *da cents a euro
044900130627     C***                Z-ADD     PiVal         VABCAS
045000130627     C***                ELSE
045100130627     C***                ADD       1             errore
045200130627     C***                EVAL      vinmsg = %trimr(vinmsg)
045300130627     C***                          + ' ' + 'VABCAS'
045400130627     C***                ENDIF
045500130627     C***                ENDIF
045600071121     C*
045700080415     C* Carico l'estensioni A e B del FNVAT
045800080512     C                   exsr      exevata
045900080512     C                   exsr      exevatb
046000140224     C                   exsr      exevatij
046100010205     C*
046200010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
046300130627     C***                IF        FlgCAS <> '0'
046400130627     C***                EVAL      VABCBO = '4'
046500130627     C***                ENDIF
046600020305     C*
046700011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
046800011113     C                   EXSR      CHKIMPDIV
046900100114     C*
047000100114     C                   endif
047100010202     C*
047200000801     C* Ebbene...
047300000801     C                   ADD       1             §CTRMO
047400010201     C                   IF        errore <> *zeros
047500000801     C                   ADD       1             §CTRNO
047600000801     C                   EVAL      vinflg = '2'
047700000801     C                   ELSE
047800010201     C                   ADD       1             §CTROKVB
047900000801     C                   ENDIF
048000000801     C*
048100000801     C                   ENDSR
048200070102     C*----------------------------------------------------*
048300141024     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "E"
048400070102     C*----------------------------------------------------*
048500070103     C     EXEVATE       BEGSR
048600070102     C*
048700091215     C* NCL
048800091214     C                   eval      VABNCL = VABNCL + 1
048900091215     C* PKB
049000091215     C                   EVAL      PiStr=%trim(%subst(vindta:38:8))
049100091215     C                   EXSR      CHKNUM
049200091215     C                   IF        PiNum=*on
049300091215     C                   EVAL      PiVal = PiVal / 100                          *da decagrammi a Kg.
049400091215     C                   ADD       PiVal         VABPKB
049500091215     C                   ELSE
049600091215     C                   ADD       1             errore
049700091215     C                   EVAL      vinmsg = %trimr(vinmsg)
049800091215     C                             + ' ' + 'VABPKB'
049900091215     C                   ENDIF
050000130625     C* CAS
050100130625     C                   IF        %subst(vindta:1019:9) <> *blanks AND
050200130625     C                             %subst(vindta:1019:9) <> *zeros  AND
050300130625     C                             %subst(vindta:1019:9) <> '000000000'
050400130625     C                   EVAL      FlgCAS = '1'
050500130625     C                   EVAL      VABVCA=%trim(%subst(vindta:1015:4))
050600130625     C                   EVAL      VABTIC=*blanks
050700130625     C                   EVAL      PiStr=%trim(%subst(vindta:1019:9))
050800130625     C                   EXSR      CHKNUM
050900130625     C                   IF        PiNum=*on
051000130625     C                   EVAL      PiVal = PiVal / 100                          *da cents a euro
051100130625     C                   ADD       PiVal         VABCAS
051200130625     C                   ELSE
051300130625     C                   ADD       1             errore
051400130625     C                   EVAL      vinmsg = %trimr(vinmsg)
051500130625     C                             + ' ' + 'VABCAS'
051600130625     C                   ENDIF
051700130625     C                   ENDIF
051800130627     C*
051900130627     C* Considerazioni sul contenuto di campi precedentemente valorizzati
052000130627     C                   IF        FlgCAS <> '0'
052100130627     C                   EVAL      VABCBO = '4'
052200130627     C                   ENDIF
052300091214     C*
052400090428     C                   EVAL      VATTRC = 'E'
052500091214     C                   EVAL      VATNOT = %trim(%subst(vindta:920:35))
052600090428     C*
052700090922     C                   exsr      wrivat                                       => scarico VAT
052800070102     C*
052900070102     C                   ENDSR
053000080415     C*----------------------------------------------------*
053100141024     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "A"
053200080415     C*----------------------------------------------------*
053300080415     C     EXEVATA       BEGSR
053400080415     C*
053500091214     C***                EVAL      VATTRC = 'A'
053600091214     C***                EVAL      VATNOT = %trim(%subst(vindta:376:15))
053700080415     C*
053800091215     C***                exsr      wrivat                                       => scarico VAT
053900080415     C*
054000080415     C                   ENDSR
054100071121     C*----------------------------------------------------*
054200141024     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "B"
054300071121     C*----------------------------------------------------*
054400071121     C     EXEVATB       BEGSR
054500071121     C*
054600090504     C                   EVAL      VATTRC = 'B'
054700091214     C                   EVAL      VATNOT = %trim(%subst(vindta:374:30))
054800071121     C*
054900090922     C                   exsr      wrivat                                       => scarico VAT
055000071121     C*
055100071121     C                   ENDSR
055200140224     C*----------------------------------------------------*
055300141024     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "I" e "J"
055400140224     C*----------------------------------------------------*
055500140224     C     EXEVATIJ      BEGSR
055600140224     C*
055700140224     C                   EVAL      VATTRC = 'I'
055800140224     C                   EVAL      VATNOT = %trim(%subst(vindta:1232:35))
055900140224     C*
056000140224     C                   exsr      wrivat                                       => scarico VAT
056100140224     C*
056200140224     C                   EVAL      VATTRC = 'J'
056300140224     C                   EVAL      VATNOT = %trim(%subst(vindta:1232+35:35))
056400140224     C*
056500140224     C                   exsr      wrivat                                       => scarico VAT
056600140224     C*
056700140224     C                   ENDSR
056800010201     C*----------------------------------------------------*
056900141024     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
057000010201     C*----------------------------------------------------*
057100020305     C     WRIVAT        BEGSR
057200141024     C*
057300141024     C* VALORIZZO CAMPI RELATIVI AL "CMR"
057400141024     C                   EVAL      VATCMR = wCMR
057500141024     C                   EVAL      VATCNT = 1
057600050628     C*
057700060223     C* Scrivo solo se valorizzato qualcosa
057800060223     C                   IF        VATNOT <> *blanks
057900141024     C                   WRITE     EDIVAT00
058000090922     C                   ADD       1             §ctrokvt
058100060223     C                   ENDIF
058200010201     C*
058300010201     C                   ENDSR
058400010202     C*----------------------------------------------------*
058500141024     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
058600010202     C*----------------------------------------------------*
058700020305     C     PREVAT        BEGSR
058800010202     C*
058900141024     C* Compongo il nome del membro da dare al EDIVATWR
059000010202     C                   eval      parmbr = vlrhdl
059100010202     C                   movel     'M'           parmbr
059200050627     C                   eval      parccm = %subst(vlrKSC:2:7)
059300010202     C                   eval      paropz = '1'
059400010202     C* Effettuo la chiamata al CLLE preposto
059500090114     C                   call(e)   'TITVVTC'
059600010202     C                   parm                    parccm
059700010202     C                   parm                    parmbr
059800010202     C                   parm                    paropz
059900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
060000010202     C                   if        %error
060100010202     C                   movel     '1'           chkcall
060200010202     C                   else
060300010202     C                   movel     '0'           chkcall
060400010202     C                   endif
060500010202     C*
060600010202     C                   ENDSR
060700000801     C*----------------------------------------------------*
060800000801     C*  CONTROLLO NUMERICITA' CAMPI
060900000801     C*----------------------------------------------------*
061000000801     C     CHKNUM        BEGSR
061100081217     C*
061200081217     C                   IF        PiDecChr = *blanks
061300090922     C                   EVAL      PiDecChr = '.'
061400081217     C                   ENDIF
061500081217     C*
061600081217     C                   callp(e)  UBISNUM_Check(PiStr
061700081217     C                                          :PiDecChr
061800081217     C                                          :PiVal
061900081217     C                                          :PiNum
062000081217     C                                          :PiInt)
062100081217     C*
062200081217     C                   IF        %error
062300081217     C                   EVAL      PiInt=*off
062400081217     C                   ENDIF
062500000801     C*
062600000801     C                   ENDSR
062700000801     C***
062800000801
062900011113
063000011113     C*----------------------------------------------------*
063100011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
063200011113     C*----------------------------------------------------*
063300011113     C     CHKIMPDIV     BEGSR
063400011113     C*
063500011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
063600011113     C                   Z-ADD     *zeros        wrkDec            9 9
063700011113     C*
063800011113     C* Come prima cosa effettuo considerazioni sulla divisa
063900011113     C                   IF        vabIAS > *zeros
064000011113     C                   IF        vabVAS <> 'EUR'
064100011113     C                   EVAL      vabVAS =  'ITL'
064200011113     C                   ENDIF
064300011113     C                   ENDIF
064400011113     C*
064500011113     C                   IF        vabCAS > *zeros
064600011113     C                   IF        vabVCA <> 'EUR'
064700011113     C                   EVAL      vabVCA =  'ITL'
064800011113     C                   ENDIF
064900011113     C                   ENDIF
065000011113     C*
065100011113     C                   IF        vabVMD > *zeros
065200020305     C                   IF        vabVAD <> 'EUR'
065300011113     C                   EVAL      vabVAD =  'ITL'
065400011113     C                   ENDIF
065500011113     C                   ENDIF
065600011113     C*
065700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
065800011113     C                   Z-ADD     vabIAS        wrkDec
065900011113     C                   IF        wrkDec > *zeros
066000011113     C                   IF        vabVAS = 'ITL'
066100011113     C                   EVAL      vabIAS = *zeros
066200011113     C                   ENDIF
066300011113     C                   ENDIF
066400011113     C*
066500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
066600011113     C                   Z-ADD     vabCAS        wrkDec
066700011113     C                   IF        wrkDec > *zeros
066800011113     C                   IF        vabVCA = 'ITL'
066900011113     C                   EVAL      vabCAS = *zeros
067000011113     C                   ENDIF
067100011113     C                   ENDIF
067200011113     C*
067300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
067400011113     C                   Z-ADD     vabVMD        wrkDec
067500011113     C                   IF        wrkDec > *zeros
067600011113     C                   IF        vabVAD = 'ITL'
067700011113     C                   EVAL      vabVMD = *zeros
067800011113     C                   ENDIF
067900011113     C                   ENDIF
068000011113     C*
068100011113     C                   ENDSR
068200011113     C***
068300011113
068400011113
068500000801
068600000801
068700990920      /TITLE Invio dei dati al punto operativo.
068800010202     C     invio         BEGSR
068900990920     C*
069000141024     C* 1° invio EDIVAT
069100010201     C                   reset                   dscmz
069200010201     C                   move      vlrpoi        cmzdst
069300141024     C                   eval      cmzfld = 'EDIVATWR'
069400010201     C                   eval      cmzmbd = vlrhdl
069500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
069600021009     C***                if        prmfir = *blanks
069700141024     C                   eval      cmzfla = 'EDIVAT0F'
069800141024     C                   eval      cmzmba = 'EDIVAT0F'
069900021009     C***                else
070000021009     C***                eval      cmzfla = prmfir
070100021009     C***                eval      cmzmba = prmfir
070200021009     C***                endif
070300010201     C                   eval      cmznrr = *zeros
070400020305     C                   move      §ctrokvt      cmznrr
070500021018     C                   eval      cmzlba = vlrfl1
070600010201     C                   call(e)   'TIS711C'
070700010201     C                   parm                    dscmz
070800010201     C                   parm      *blanks       esito
070900010205     C                   if        %error
071000010205     C                             or cmzerr = '1'
071100010205     C                             or esito  = '1'
071200010205     C                   eval      wrkesito = '3'
071300010205     C                   else
071400010201     C*
071500141024     C* 2° invio EDIVAB
071600010201     C                   reset                   dscmz
071700010201     C                   move      vlrpoi        cmzdst
071800010201     C                   eval      cmzfld = vlrfou
071900010201     C                   eval      cmzmbd = vlrhdl
072000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
072100021009     C***                if        prmfir = *blanks
072200141024     C                   eval      cmzfla = 'EDIVAB0F'
072300141024     C                   eval      cmzmba = 'EDIVAB0F'
072400021009     C***                else
072500021009     C***                eval      cmzfla = prmfir
072600021009     C***                eval      cmzmba = prmfir
072700021009     C***                endif
072800010201     C                   eval      cmznrr = *zeros
072900010201     C                   move      §ctrokvb      cmznrr
073000021018     C                   eval      cmzlba = vlrfl1
073100010201     C                   call(e)   'TIS711C'
073200010201     C                   parm                    dscmz
073300010201     C                   parm      *blanks       esito
073400010201     C                   if        %error
073500010201     C                             or cmzerr = '1'
073600010201     C                             or esito  = '1'
073700010201     C                   eval      wrkesito = '3'
073800010201     C                   endif
073900010205     C                   endif
074000990920     C*
074100000613     C                   ENDSR
074200000613     C***
074300070411
074400090331
074500090331
074600090331
074700070411     C     *pssr         BEGSR
074800070411     C*
074900070411     C                   if        %open(tivin00r)
075000070411     C                   close     tivin00r
075100070411     C                   endif
075200141024     C                   if        %open(edivabwr)
075300141024     C                   close     edivabwr
075400070411     C                   endif
075500141024     C                   if        %open(edivatwr)
075600141024     C                   close     edivatwr
075700070411     C                   endif
075800070411     C*
075900070411     C* Effettuo la chiamata al CLLE preposto
076000090114     C                   call(e)   'TITVVTC'
076100070411     C                   parm                    parccm
076200070411     C                   parm                    parmbr
076300070411     C                   parm      '2'           paropz
076400100114     C*
076500100114     C* Metodo chiusura UBCHKRMN
076600100114     C                   callp(e)  UBCHKRMN_Close()
076700070411     C*
076800070411     C                   eval      wrkesito = '2'
076900070411     C*
077000070411     C                   seton                                        LR
077100070411     C*
077200070411     C                   ENDSR     '*CANCL'
077300070411     C***
077400120614
077500120614
077600120614
077700120614      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
077800120614     C     wriVGD        BEGSR
077900120614     C*
078000120614     C                   clear                   tivgd000
078100120614     C                   eval      vgdDTA = fnvab00t
078200120614     C                   eval      vgdTIP = 'EB'
078300120614     C                   eval      vgdKSU = vlrKSC
078400120614     C                   eval      vgdTSC = 'WW'
078500120614     C                   eval      vgdDAT = datcor
078600120614     C                   eval      vgdPGM = 'TITV1T5R'
078700120614     C                   write     tivgd000
078800120614     C*
078900120614     C                   ENDSR
079000120614     C*------------------------------------------------------------------------*
079100120614
079200070411
079300990910
079400000613     C     *inzsr        BEGSR
079500990910     C*
079600990910     C     *entry        plist
079700990920     C                   parm                    tivlrds
079800990921     C                   parm      wrkesito      esito
079900000724     C                   parm                    prmlit
080000000710     C                   parm                    prmfir
080100141024     C*
080200141024     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
080300141024     C                   move(p)   vlrMSG        wNomeFile
080400141024     C*
080500141024     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
080600141024     C* del codice CMR dal nome del file importato.
080700141024     C                   if        wNomeFile <> *blanks AND
080800141024     C                             vlrppt<>*blanks
080900141024     C                   eval      wPosDaA = %subst(vlrppt:17:2)
081000141024     C                   move(p)   wPosDaA       wPosDa
081100141024     C                   eval      wLungA  = %subst(vlrppt:19:2)
081200141024     C                   move(p)   wLungA        wLung
081300141024     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
081400141024     C                   else
081500141024     C                   eval      wCMR = %char(datcor)+'-'+%char(oracor)
081600141024     C                   endif
081700141024     C*
081800141024     C* CALCOLA LA DATA CORRENTE
081900141024     C                   time                    wn14             14 0
082000141024     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
082100141024     C                   movel     wn14          oracor            6 0          *ORA
082200091214     C                   z-add     *zeros        datcor            8 0
082300091214     C                   eval      datcor = %dec(%date() : *ISO)
082400000830     C*
082500000613     C                   ENDSR
082600000613     C***
