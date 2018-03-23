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
004600130722     D/COPY GAITRASRC/SRCPROTOPR,UBCHKRMN
004700130722     D/COPY GAITRASRC/SRCPROTOPI,UBCHKRMN
004800081217
004900081217
005000010201
005100081217
005200100114     C*
005300100114     C* Metodo apertura UBCHKRMN
005400130722     C                   callp(e)  UBCHKRMN_Open()
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
007500130722     C                   callp(e)  UBCHKRMN_Close()
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
019700140731     C  N56              exsr      wrivgd
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
022000130722     C                   Z-ADD     0495304       VABCCM
022100130722     C                   Z-ADD     0495304       VATCCM
022200130722     C                   Z-ADD     049           VABLNP
022300130722     C                   Z-ADD     049           VATLNP
022400130722     C                   Z-ADD     000           VABCTR
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
026600140731     C*
026700140731     C* Verifico se sui parametri del traduttore è richiesto il feed-back dati ricevuti
026800140731     C                   if        %subst(vlrppt:16:1) = 'N'
026900140731     C                   seton                                        56
027000140731     C                   else
027100140731     C                   setoff                                       56
027200140731     C                   endif
027300120614     C*
027400020619     C                   ENDIF
027500000801     C*
027600000801     C                   ENDSR
027700091215     C*----------------------------------------------------*
027800091215     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
027900091215     C*----------------------------------------------------*
028000091215     C     REPNSP        BEGSR
028100091215     C*
028200091215     C                   EXSR      INZVAR
028300091215     C                   EXSR      DEFCAM
028400091215     C*
028500091215     C* NSP => Stacco un numeratore da AZNUM
028600091215     C                   clear                   TRUL33DS
028700091215     C                   eval      I33OPE = *zeros
028800091215     C                   eval      I33CNU = 302
028900091215     C                   eval      I33NUM = 1
029000091215     C                   movel     TRUL33DS      KPJBU
029100091215     C                   call      'TRUL33R'
029200091215     C                   parm                    KPJBA
029300091215     C                   movel     KPJBU         TRUL33DS
029400091215     C                   if        O33ERR = *zeros
029500091215     C                   z-add     O33NRF        VABNSP
029600091215     C                   z-add     O33NRF        VATNSP
029700091215     C                   else
029800091215     C                   ADD       1             errore
029900091215     C                   EVAL      vinmsg = %trimr(vinmsg)
030000091215     C                             + ' ' + 'VABNSP VATNSP'
030100091215     C                   endif
030200091215     C*
030300091215     C                   ENDSR
030400000801     C*----------------------------------------------------*
030500090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
030600000801     C*----------------------------------------------------*
030700040823     C     IMPVAB        BEGSR
030800010305     C*
030900000801     C                   Z-ADD     *zeros        errore            1 0
031000000830     C                   MOVEL     datcor        VABAAS
031100020305     C                   MOVEL     datcor        VATAAS
031200040526     C                   MOVE      datcor        VABMGS
031300040823     C                   MOVE(P)   vlrpoi        VABFGS
031400040823     C                   MOVE(P)   vlrpoi        VATFGS
031500080409     C*
031600080409     C* Reperimento campi ALFA
031700080409     C*
031800080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
031900091214     C                   EVAL      VABRSD=%trim(%subst(vindta:201:35))
032000080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
032100080409     C     '@':'A'       XLATE     VABRSD        VABRSD
032200080409     C* ==
032300091214     C                   EVAL      VABRD2=%trim(%subst(vindta:61:35))
032400091214     C                   EVAL      VABIND=%trim(%subst(vindta:96:35))  + ' ' +
032500091214     C                                    %trim(%subst(vindta:131:35)) + ' ' +
032600091214     C                                    %trim(%subst(vindta:166:35))
032700091214     C                   EVAL      VABLOD=%trim(%subst(vindta:281:35))
032800091216     C***                EVAL      VABRMA=%trim(%subst(vindta:955:35))
032900091216     C                   EVAL      VABRMA=%trim(%subst(vindta:1:35))
033000091214     C                   EVAL      VABNZD=%trim(%subst(vindta:371:3))
033100091214     C                   IF        VABNZD='IT' OR
033200091214     C                             VABNZD='VA' OR
033300091214     C                             VABNZD='SM'
033400091214     C                   EVAL      VABNZD=*blanks
033500091214     C                   ENDIF
033600091214     C                   EVAL      VABNAS=%trim(%subst(vindta:1072:35))
033700091214     C                   MOVEL     *blanks       wNOTE            70
033800091214     C                   EVAL      wNOTE =%trim(%subst(vindta:762:35))  + ' ' +
033900091214     C                                    %trim(%subst(vindta:797:35)) + ' ' +
034000091214     C                                    %trim(%subst(vindta:832:35)) + ' ' +
034100091214     C                                    %trim(%subst(vindta:867:35))
034200091214     C                   EVAL      VABNOT=%subst(wNOTE:1:35)
034300091214     C                   EVAL      VABNT2=%subst(wNOTE:35:35)
034400080409     C*
034500080409     C* Reperimento campi NUMERICI
034600090309     C* RMN
034700091215     C                   EVAL      PiStr=%trim(%subst(vindta:1+2:35-2))
034800091215     C                   EXSR      CHKNUM
034900091215     C                   IF        PiInt=*on
035000091215     C                   Z-ADD     PiVal         VABRMN
035100091215     C                   ELSE
035200091215     C                   ADD       1             errore
035300091215     C                   Z-ADD     1             VABRMN
035400091215     C                   EVAL      vinmsg = %trimr(vinmsg)
035500091215     C                             + ' ' + 'VABRMN'
035600091215     C                   ENDIF
035700100114     C*
035800100114     C* Verifico che x il CCM corrente nn esiste già (ovunque) una bolla con medesimo
035900100114     C* riferimento => altrimenti escludo
036000130625     C                   if        VABRMN <> 1 and *in55
036100100114     C                   callp(e)  UBCHKRMN_Exist(VABCCM
036200100114     C                                           :VABRMN
036300100114     C                                           :pOutFIVAB
036400100114     C                                           :pOutFNBLP
036500100114     C                                           :pOutTITAS)
036600100114     C*
036700100114     C                   if        not %error
036800100114     C                   if        pOutFIVAB = *off AND
036900100114     C                             pOutFNBLP = *off AND
037000100114     C                             pOutTITAS = *off
037100100114     C                   else
037200100114     C                   seton                                        54
037300100114     C                   endif
037400100114     C                   endif
037500100114     C                   endif
037600100114     C*
037700100114     C                   if        not *in54
037800080409     C* CAD
037900091214     C                   EVAL      PiStr=%trim(%subst(vindta:271:10))
038000090114     C                   EXSR      CHKNUM
038100090114     C                   IF        PiInt=*on
038200090114     C                   Z-ADD     PiVal         Num5_0
038300090114     C                   MOVEL(p)  Num5_0        VABCAD
038400090114     C                   ELSE
038500090114     C                   ADD       1             errore
038600090114     C                   EVAL      VABCAD = *zeros
038700090114     C                   EVAL      vinmsg = %trimr(vinmsg)
038800090114     C                             + ' ' + 'VABCAD'
038900090114     C                   ENDIF
039000091214     C*
039100091214     C* Reperisco la provincia dal CAP e dalla località
039200091214     C                   IF        VABCAD <> *blanks AND
039300091214     C                             VABPRD  = *blanks AND
039400091214     C                             VABNZD  = *blanks
039500091214     C                   CLEAR                   TISI95DS
039600091214     C                   EVAL      I95TCN = '3'
039700091214     C                   Z-ADD     datcor        I95DAT
039800091214     C                   EVAL      I95CAP = VABCAD
039900091214     C                   EVAL      I95LOC = VABLOD
040000091214     C                   CALL      'TISI95R'
040100091214     C                   PARM                    TISI95DS
040200091214     C                   EVAL      VABPRD = O95PRV
040300091214     C                   ENDIF
040400091214     C*
040500091214     C* NCL
040600091214     C* ...calcolato in sommatoria dei singoli dettagli
040700091215     C* PKB
040800091215     C* ...calcolato in sommatoria dei singoli dettagli
040900091215     C* IAS
041000091215     C                   IF        %subst(vindta:1006:9) <> *blanks AND
041100091215     C                             %subst(vindta:1006:9) <> *zeros  AND
041200091215     C                             %subst(vindta:1006:9) <> '000000000'
041300091215     C                   EVAL      VABVAS=%trim(%subst(vindta:1015:4))
041400091215     C                   EVAL      PiStr=%trim(%subst(vindta:1006:9))
041500091215     C                   EXSR      CHKNUM
041600091215     C                   IF        PiNum=*on
041700091215     C                   EVAL      PiVal = PiVal / 100                          *da cents a euro
041800091215     C                   Z-ADD     PiVal         VABIAS
041900091215     C                   ELSE
042000091215     C                   ADD       1             errore
042100091215     C                   EVAL      vinmsg = %trimr(vinmsg)
042200091215     C                             + ' ' + 'VABIAS'
042300091215     C                   ENDIF
042400091215     C                   ENDIF
042500090922     C* CAS
042600130627     C***                IF        %subst(vindta:1019:9) <> *blanks AND
042700130627     C***                          %subst(vindta:1019:9) <> *zeros  AND
042800130627     C***                          %subst(vindta:1019:9) <> '000000000'
042900130627     C***                EVAL      FlgCAS = '1'
043000130627     C***                EVAL      VABVCA=%trim(%subst(vindta:1015:4))
043100130627     C***                EVAL      VABTIC=*blanks
043200130627     C***                EVAL      PiStr=%trim(%subst(vindta:1019:9))
043300130627     C***                EXSR      CHKNUM
043400130627     C***                IF        PiNum=*on
043500130627     C***                EVAL      PiVal = PiVal / 100                          *da cents a euro
043600130627     C***                Z-ADD     PiVal         VABCAS
043700130627     C***                ELSE
043800130627     C***                ADD       1             errore
043900130627     C***                EVAL      vinmsg = %trimr(vinmsg)
044000130627     C***                          + ' ' + 'VABCAS'
044100130627     C***                ENDIF
044200130627     C***                ENDIF
044300071121     C*
044400160412     C* Carico l'estensioni A, B, I/J del FNVAT
044500080512     C                   exsr      exevata
044600080512     C                   exsr      exevatb
044700160412     C                   exsr      exevatij
044800010205     C*
044900010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
045000130627     C***                IF        FlgCAS <> '0'
045100130627     C***                EVAL      VABCBO = '4'
045200130627     C***                ENDIF
045300020305     C*
045400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
045500011113     C                   EXSR      CHKIMPDIV
045600100114     C*
045700100114     C                   endif
045800010202     C*
045900000801     C* Ebbene...
046000000801     C                   ADD       1             §CTRMO
046100010201     C                   IF        errore <> *zeros
046200000801     C                   ADD       1             §CTRNO
046300000801     C                   EVAL      vinflg = '2'
046400000801     C                   ELSE
046500010201     C                   ADD       1             §CTROKVB
046600000801     C                   ENDIF
046700000801     C*
046800000801     C                   ENDSR
046900070102     C*----------------------------------------------------*
047000090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
047100070102     C*----------------------------------------------------*
047200070103     C     EXEVATE       BEGSR
047300070102     C*
047400091215     C* NCL
047500091214     C                   eval      VABNCL = VABNCL + 1
047600091215     C* PKB
047700091215     C                   EVAL      PiStr=%trim(%subst(vindta:38:8))
047800091215     C                   EXSR      CHKNUM
047900091215     C                   IF        PiNum=*on
048000091215     C                   EVAL      PiVal = PiVal / 100                          *da decagrammi a Kg.
048100091215     C                   ADD       PiVal         VABPKB
048200091215     C                   ELSE
048300091215     C                   ADD       1             errore
048400091215     C                   EVAL      vinmsg = %trimr(vinmsg)
048500091215     C                             + ' ' + 'VABPKB'
048600091215     C                   ENDIF
048700130625     C* CAS
048800130625     C                   IF        %subst(vindta:1019:9) <> *blanks AND
048900130625     C                             %subst(vindta:1019:9) <> *zeros  AND
049000130625     C                             %subst(vindta:1019:9) <> '000000000'
049100130625     C                   EVAL      FlgCAS = '1'
049200130625     C                   EVAL      VABVCA=%trim(%subst(vindta:1015:4))
049300130625     C                   EVAL      VABTIC=*blanks
049400130625     C                   EVAL      PiStr=%trim(%subst(vindta:1019:9))
049500130625     C                   EXSR      CHKNUM
049600130625     C                   IF        PiNum=*on
049700130625     C                   EVAL      PiVal = PiVal / 100                          *da cents a euro
049800130625     C                   ADD       PiVal         VABCAS
049900130625     C                   ELSE
050000130625     C                   ADD       1             errore
050100130625     C                   EVAL      vinmsg = %trimr(vinmsg)
050200130625     C                             + ' ' + 'VABCAS'
050300130625     C                   ENDIF
050400130625     C                   ENDIF
050500130627     C*
050600130627     C* Considerazioni sul contenuto di campi precedentemente valorizzati
050700130627     C                   IF        FlgCAS <> '0'
050800130627     C                   EVAL      VABCBO = '4'
050900130627     C                   ENDIF
051000091214     C*
051100090428     C                   EVAL      VATTRC = 'E'
051200091214     C                   EVAL      VATNOT = %trim(%subst(vindta:920:35))
051300090428     C*
051400090922     C                   exsr      wrivat                                       => scarico VAT
051500070102     C*
051600070102     C                   ENDSR
051700080415     C*----------------------------------------------------*
051800090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
051900080415     C*----------------------------------------------------*
052000080415     C     EXEVATA       BEGSR
052100080415     C*
052200091214     C***                EVAL      VATTRC = 'A'
052300091214     C***                EVAL      VATNOT = %trim(%subst(vindta:376:15))
052400080415     C*
052500091215     C***                exsr      wrivat                                       => scarico VAT
052600080415     C*
052700080415     C                   ENDSR
052800071121     C*----------------------------------------------------*
052900090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
053000071121     C*----------------------------------------------------*
053100071121     C     EXEVATB       BEGSR
053200071121     C*
053300090504     C                   EVAL      VATTRC = 'B'
053400091214     C                   EVAL      VATNOT = %trim(%subst(vindta:374:30))
053500071121     C*
053600090922     C                   exsr      wrivat                                       => scarico VAT
053700071121     C*
053800071121     C                   ENDSR
053900160412     C*----------------------------------------------------*
054000160412     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I" e "J"
054100160412     C*----------------------------------------------------*
054200160412     C     EXEVATIJ      BEGSR
054300160412     C*
054400160412     C                   MOVEL     *blanks       wEMAIL           80
054500160412     C                   EVAL      wEMAIL = %trim(%subst(vindta:1232:80))
054600160412     C*
054700160412     C                   EVAL      VATTRC = 'I'
054800160412     C                   EVAL      VATNOT = %subst(wEMAIL:1:35)
054900160412     C                   exsr      wrivat                                       => scarico VAT
055000160412     C*
055100160412     C                   EVAL      VATTRC = 'J'
055200160412     C                   EVAL      VATNOT = %subst(wEMAIL:36:35)
055300160412     C                   exsr      wrivat                                       => scarico VAT
055400160412     C*
055500160412     C                   ENDSR
055600010201     C*----------------------------------------------------*
055700090114     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
055800010201     C*----------------------------------------------------*
055900020305     C     WRIVAT        BEGSR
056000050628     C*
056100060223     C* Scrivo solo se valorizzato qualcosa
056200060223     C                   IF        VATNOT <> *blanks
056300090922     C                   WRITE     FIVAT000
056400090922     C                   ADD       1             §ctrokvt
056500060223     C                   ENDIF
056600010201     C*
056700010201     C                   ENDSR
056800010202     C*----------------------------------------------------*
056900090114     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
057000010202     C*----------------------------------------------------*
057100020305     C     PREVAT        BEGSR
057200010202     C*
057300090114     C* Compongo il nome del membro da dare al FIVATWWR
057400010202     C                   eval      parmbr = vlrhdl
057500010202     C                   movel     'M'           parmbr
057600050627     C                   eval      parccm = %subst(vlrKSC:2:7)
057700010202     C                   eval      paropz = '1'
057800010202     C* Effettuo la chiamata al CLLE preposto
057900090114     C                   call(e)   'TITVVTC'
058000010202     C                   parm                    parccm
058100010202     C                   parm                    parmbr
058200010202     C                   parm                    paropz
058300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
058400010202     C                   if        %error
058500010202     C                   movel     '1'           chkcall
058600010202     C                   else
058700010202     C                   movel     '0'           chkcall
058800010202     C                   endif
058900010202     C*
059000010202     C                   ENDSR
059100000801     C*----------------------------------------------------*
059200000801     C*  CONTROLLO NUMERICITA' CAMPI
059300000801     C*----------------------------------------------------*
059400000801     C     CHKNUM        BEGSR
059500081217     C*
059600081217     C                   IF        PiDecChr = *blanks
059700090922     C                   EVAL      PiDecChr = '.'
059800081217     C                   ENDIF
059900081217     C*
060000081217     C                   callp(e)  UBISNUM_Check(PiStr
060100081217     C                                          :PiDecChr
060200081217     C                                          :PiVal
060300081217     C                                          :PiNum
060400081217     C                                          :PiInt)
060500081217     C*
060600081217     C                   IF        %error
060700081217     C                   EVAL      PiInt=*off
060800081217     C                   ENDIF
060900000801     C*
061000000801     C                   ENDSR
061100000801     C***
061200000801
061300011113
061400011113     C*----------------------------------------------------*
061500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
061600011113     C*----------------------------------------------------*
061700011113     C     CHKIMPDIV     BEGSR
061800011113     C*
061900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
062000011113     C                   Z-ADD     *zeros        wrkDec            9 9
062100011113     C*
062200011113     C* Come prima cosa effettuo considerazioni sulla divisa
062300011113     C                   IF        vabIAS > *zeros
062400011113     C                   IF        vabVAS <> 'EUR'
062500011113     C                   EVAL      vabVAS =  'ITL'
062600011113     C                   ENDIF
062700011113     C                   ENDIF
062800011113     C*
062900011113     C                   IF        vabCAS > *zeros
063000011113     C                   IF        vabVCA <> 'EUR'
063100011113     C                   EVAL      vabVCA =  'ITL'
063200011113     C                   ENDIF
063300011113     C                   ENDIF
063400011113     C*
063500011113     C                   IF        vabVMD > *zeros
063600020305     C                   IF        vabVAD <> 'EUR'
063700011113     C                   EVAL      vabVAD =  'ITL'
063800011113     C                   ENDIF
063900011113     C                   ENDIF
064000011113     C*
064100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
064200011113     C                   Z-ADD     vabIAS        wrkDec
064300011113     C                   IF        wrkDec > *zeros
064400011113     C                   IF        vabVAS = 'ITL'
064500011113     C                   EVAL      vabIAS = *zeros
064600011113     C                   ENDIF
064700011113     C                   ENDIF
064800011113     C*
064900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
065000011113     C                   Z-ADD     vabCAS        wrkDec
065100011113     C                   IF        wrkDec > *zeros
065200011113     C                   IF        vabVCA = 'ITL'
065300011113     C                   EVAL      vabCAS = *zeros
065400011113     C                   ENDIF
065500011113     C                   ENDIF
065600011113     C*
065700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
065800011113     C                   Z-ADD     vabVMD        wrkDec
065900011113     C                   IF        wrkDec > *zeros
066000011113     C                   IF        vabVAD = 'ITL'
066100011113     C                   EVAL      vabVMD = *zeros
066200011113     C                   ENDIF
066300011113     C                   ENDIF
066400011113     C*
066500011113     C                   ENDSR
066600011113     C***
066700011113
066800011113
066900000801
067000000801
067100990920      /TITLE Invio dei dati al punto operativo.
067200010202     C     invio         BEGSR
067300990920     C*
067400090114     C* 1° invio FIVAT
067500010201     C                   reset                   dscmz
067600010201     C                   move      vlrpoi        cmzdst
067700090114     C                   eval      cmzfld = 'FIVATWWR'
067800010201     C                   eval      cmzmbd = vlrhdl
067900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
068000021009     C***                if        prmfir = *blanks
068100090114     C                   eval      cmzfla = 'FIVAT00F'
068200090114     C                   eval      cmzmba = 'FIVAT00F'
068300021009     C***                else
068400021009     C***                eval      cmzfla = prmfir
068500021009     C***                eval      cmzmba = prmfir
068600021009     C***                endif
068700010201     C                   eval      cmznrr = *zeros
068800020305     C                   move      §ctrokvt      cmznrr
068900021018     C                   eval      cmzlba = vlrfl1
069000010201     C                   call(e)   'TIS711C'
069100010201     C                   parm                    dscmz
069200010201     C                   parm      *blanks       esito
069300010205     C                   if        %error
069400010205     C                             or cmzerr = '1'
069500010205     C                             or esito  = '1'
069600010205     C                   eval      wrkesito = '3'
069700010205     C                   else
069800010201     C*
069900090114     C* 2° invio FIVAB
070000010201     C                   reset                   dscmz
070100010201     C                   move      vlrpoi        cmzdst
070200010201     C                   eval      cmzfld = vlrfou
070300010201     C                   eval      cmzmbd = vlrhdl
070400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
070500021009     C***                if        prmfir = *blanks
070600090114     C                   eval      cmzfla = 'FIVAB00F'
070700090114     C                   eval      cmzmba = 'FIVAB00F'
070800021009     C***                else
070900021009     C***                eval      cmzfla = prmfir
071000021009     C***                eval      cmzmba = prmfir
071100021009     C***                endif
071200010201     C                   eval      cmznrr = *zeros
071300010201     C                   move      §ctrokvb      cmznrr
071400021018     C                   eval      cmzlba = vlrfl1
071500010201     C                   call(e)   'TIS711C'
071600010201     C                   parm                    dscmz
071700010201     C                   parm      *blanks       esito
071800010201     C                   if        %error
071900010201     C                             or cmzerr = '1'
072000010201     C                             or esito  = '1'
072100010201     C                   eval      wrkesito = '3'
072200010201     C                   endif
072300010205     C                   endif
072400990920     C*
072500000613     C                   ENDSR
072600000613     C***
072700070411
072800090331
072900090331
073000090331
073100070411     C     *pssr         BEGSR
073200070411     C*
073300070411     C                   if        %open(tivin00r)
073400070411     C                   close     tivin00r
073500070411     C                   endif
073600090114     C                   if        %open(fivabwwr)
073700090114     C                   close     fivabwwr
073800070411     C                   endif
073900090114     C                   if        %open(fivatwwr)
074000090114     C                   close     fivatwwr
074100070411     C                   endif
074200070411     C*
074300070411     C* Effettuo la chiamata al CLLE preposto
074400090114     C                   call(e)   'TITVVTC'
074500070411     C                   parm                    parccm
074600070411     C                   parm                    parmbr
074700070411     C                   parm      '2'           paropz
074800100114     C*
074900100114     C* Metodo chiusura UBCHKRMN
075000130722     C                   callp(e)  UBCHKRMN_Close()
075100070411     C*
075200070411     C                   eval      wrkesito = '2'
075300070411     C*
075400070411     C                   seton                                        LR
075500070411     C*
075600070411     C                   ENDSR     '*CANCL'
075700070411     C***
075800130722
075900120614
076000120614
076100120614      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
076200120614     C     wriVGD        BEGSR
076300120614     C*
076400120614     C                   clear                   tivgd000
076500120614     C                   eval      vgdDTA = fnvab00t
076600120614     C                   eval      vgdTIP = 'EB'
076700120614     C                   eval      vgdKSU = vlrKSC
076800120614     C                   eval      vgdTSC = 'WW'
076900120614     C                   eval      vgdDAT = datcor
077000130722     C                   eval      vgdPGM = 'TITV1T5R2'
077100120614     C                   write     tivgd000
077200120614     C*
077300120614     C                   ENDSR
077400120614     C*------------------------------------------------------------------------*
077500120614
077600070411
077700990910
077800000613     C     *inzsr        BEGSR
077900990910     C*
078000990910     C     *entry        plist
078100990920     C                   parm                    tivlrds
078200990921     C                   parm      wrkesito      esito
078300000724     C                   parm                    prmlit
078400000710     C                   parm                    prmfir
078500000613     C*
078600091214     C                   z-add     *zeros        datcor            8 0
078700091214     C                   eval      datcor = %dec(%date() : *ISO)
078800000830     C*
078900000613     C                   ENDSR
079000000613     C***
