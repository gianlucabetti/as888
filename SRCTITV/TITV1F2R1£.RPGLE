000100140328      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500140328     FEDIVABwr  O    E             DISK    usropn
000600140328     FEDIVATwr  O    E             DISK    usropn
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
002500140709     D curSped         s              7    INZ(*blanks)
002600140709     D depSped         s              7    INZ(*blanks)
002700000830
002800000830     D*------------------
002900000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003000000830     D*------------------
003100000830     D WLBDA8          DS                  INZ
003200000830     D  G08DAT                 1      8  0
003300000830     D  G08INV                 9     16  0
003400000830     D  G08ERR                17     17
003500000830     D  G08TGI                18     22  0
003600010201
003700010201
003800000913     C                   reset                   rrnum
003900990921     C                   reset                   esito
004000990921     C                   reset                   wrkesito
004100000613     C*
004200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004300000613     C*
004400010202     C* Effettuo la chiamata al CLLE preposto
004500140328     C                   call(e)   'TITVEVTC'
004600010202     C                   parm                    parccm
004700010202     C                   parm                    parmbr
004800010202     C                   parm      '2'           paropz
004900050201     C*
005000050201     C* Effettuo lancio TISI95 solo x chiusura
005100050201     C                   CLEAR                   TISI95DS
005200050201     C                   EVAL      I95TLA = 'C'
005300050201     C                   CALL      'TISI95R'
005400050201     C                   PARM                    TISI95DS
005500000616     C*
005600000801     C
005700010201     C                   seton                                        LR
005800990908
005900000801
006000910830     C*--------------------------------------------------------
006100140328     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
006200910830     C*--------------------------------------------------------
006300040526     C     RWFILE        BEGSR
006400990910     C*
006500990914     C                   if        not %open(tivin00r)
006600990908     C                   open      tivin00r
006700990914     C                   endif
006800140328     C                   if        not %open(edivabwr)
006900140328     C                   open      edivabwr
007000990914     C                   endif
007100070103     C*
007200140328     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
007300020305     C                   exsr      prevat
007400010201     C*
007500010202     C                   if        chkcall = '0'
007600010202     C*
007700140328     C                   if        not %open(edivatwr)
007800140328     C                   open      edivatwr
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
010000070103     C*
010100070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
010200140709     C                   eval      curSped = %subst(vindta:23:7)
010300070213     C                   if        curSped <> depSped
010400070213     C* Se prima bolla => importo bolla corrente
010500070213     C                   if        §CTRMO = *zeros
010600070213     C                   exsr      inzvar
010700070213     C                   exsr      defcam
010800070213     C                   exsr      impvab                                       => carico  VAB
010900070213     C                   else
011000070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
011100070213     C                   exsr      wrivab                                       => scarico VAB
011200070213     C                   exsr      inzvar
011300070213     C                   exsr      defcam
011400070213     C                   exsr      impvab                                       => carico  VAB
011500070213     C                   endif
011600071121     C                   exsr      exevate                                      => write VAT-E
011700070103     C* Salvo il raggruppamento spedizione cliente corrente
011800070103     C                   eval      depSped = curSped
011900070213     C*
012000070213     C* Se collo successivo x stessa bolla
012100070213     C                   else
012200070213     C                   exsr      impvab                                       => carico  VAB
012300070213     C                   exsr      exevate                                      => write VAT-E
012400070103     C                   endif
012500000905     C*
012600000905     C                   else
012700000905     C                   eval      vinflg = '1'
012800050628     C                   endif
012900000905     C                   endif
013000000905     C*
013100000905     C  N70              update    tivin000
013200000905     C*
013300991022     C  N70              ENDdo
013400070213     C*
013500070213     C* Scarico testata bolla rimasta in sospesa
013600070213     C                   exsr      wrivab                                       => scarico VAB
013700010202     C*
013800010202     C                   endif
013900990910
014000990910     C* Se non ci sono record con errori ...
014100000710     C                   if        §ctrno = 0
014200990910     C* ... restituisco esito OK.
014300990921     C                   eval      wrkesito = '0'
014400990910     C                   else
014500010201     C                   if        §ctrokvb > 0
014600990921     C                   eval      wrkesito = '1'
014700000710     C                   else
014800000710     C                   eval      wrkesito = '2'
014900990910     C                   endif
015000000710     C                   endif
015100990910     C*
015200990914     C                   if        %open(tivin00r)
015300990908     C                   close     tivin00r
015400990914     C                   endif
015500140328     C                   if        %open(edivabwr)
015600140328     C                   close     edivabwr
015700990914     C                   endif
015800140328     C                   if        %open(edivatwr)
015900140328     C                   close     edivatwr
016000010201     C                   endif
016100990910     C*
016200010201     C                   if        §ctrokvb > 0
016300000724     C                             and vlrpoi <> *zeros
016400010202     C                   exsr      invio
016500990920     C                   endif
016600990920     C*
016700910830     C                   ENDSR
016800000613     C***
016900010305
017000010305     C*----------------------------------------------------*
017100020305     C*  SCARICAMENTO BUFFER RECORDS VAB
017200010305     C*----------------------------------------------------*
017300020305     C     WRIVAB        BEGSR
017400140328     C*
017500140328     C* VALORIZZO CAMPI RELATIVI AL "CMR"
017600140328     C                   EVAL      VABCMR = %char(datcor)+'-'+%char(oracor)
017700140328     C                   EVAL      VABDCM = DATCOR
017800140328     C                   EVAL      VABDTS = DATCOR
017900140328     C                   EVAL      VABHMS = ORACOR
018000140328     C                   EVAL      VABCNT = 1
018100010305     C*
018200060225     C* Quindi scarico il buffer del file d testata
018300140328     C                   write     edivab00                                     => scarico il VAB
018400010305     C*
018500010305     C                   ENDSR
018600990920
018700000801     C*----------------------------------------------------*
018800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
018900000801     C*----------------------------------------------------*
019000010201     C     INZVAR        BEGSR
019100000801     C*
019200140328     C                   CLEAR                   EDIVAB00
019300140328     C                   CLEAR                   EDIVAT00
019400070213     C*
019500040802     C                   Z-ADD     *zeros        Num5_0            5 0
019600040802     C                   MOVEL     '0'           FlgCAS            1
019700000801     C*
019800000801     C                   ENDSR
019900000801     C*----------------------------------------------------*
020000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
020100000801     C*----------------------------------------------------*
020200000801     C     DEFCAM        BEGSR
020300000801     C*
020400020619     C* Imposto i valori di default...
020500080409     C                   Z-ADD     1310268       VABCCM
020600080409     C                   Z-ADD     1310268       VATCCM
020700080409     C                   Z-ADD     131           VABLNP
020800080409     C                   Z-ADD     131           VATLNP
020900070927     C                   Z-ADD     000           VABCTR
021000070103     C                   MOVEL     '7Q'          VABCTM
021100040823     C                   MOVEL     '1'           VABCBO
021200020619     C* ... e poi verifico se sono stati passati come parametri
021300020619     C                   IF        vlrppt > *blanks
021400040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
021500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
021600020619     C                   EXSR      CHKNUM
021700020619     C                   IF        PiInt=*on
021800020619     C                   Z-ADD     PiVal         VABCCM
021900020619     C                   Z-ADD     PiVal         VATCCM
022000020619     C                   ENDIF
022100040506     C                   ENDIF
022200040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
022300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022400020619     C                   EXSR      CHKNUM
022500020619     C                   IF        PiInt=*on
022600020619     C                   Z-ADD     PiVal         VABLNP
022700020619     C                   Z-ADD     PiVal         VATLNP
022800040506     C                   ENDIF
022900020619     C                   ENDIF
023000040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
023100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
023200020619     C                   EXSR      CHKNUM
023300020619     C                   IF        PiInt=*on
023400020619     C                   Z-ADD     PiVal         VABCTR
023500040506     C                   ENDIF
023600020619     C                   ENDIF
023700020619     C                   ENDIF
023800000801     C*
023900000801     C                   ENDSR
024000000801     C*----------------------------------------------------*
024100140328     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
024200000801     C*----------------------------------------------------*
024300040823     C     IMPVAB        BEGSR
024400010305     C*
024500000801     C                   Z-ADD     *zeros        errore            1 0
024600000830     C                   MOVEL     datcor        VABAAS
024700020305     C                   MOVEL     datcor        VATAAS
024800040526     C                   MOVE      datcor        VABMGS
024900040823     C                   MOVE(P)   vlrpoi        VABFGS
025000040823     C                   MOVE(P)   vlrpoi        VATFGS
025100080409     C*
025200080409     C* Reperimento campi ALFA
025300080409     C*
025400080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
025500080409     C                   EVAL      VABRSD=%trim(%subst(vindta:35:35))
025600080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
025700080409     C     '@':'A'       XLATE     VABRSD        VABRSD
025800080409     C* ==
025900080409     C                   EVAL      VABRD2=%trim(%subst(vindta:70:35))
026000080409     C                   EVAL      VABIND=%trim(%subst(vindta:105:35))
026100080409     C                   EVAL      VABLOD=%trim(%subst(vindta:149:35))
026200080409     C                   EVAL      VABPRD=%trim(%subst(vindta:184:2))
026300140709     C                   EVAL      VABRMA=%trim(%subst(vindta:23:7))
026400080409     C                   EVAL      VABNOT=%trim(%subst(vindta:325:34))
026500080409     C                   EVAL      VABNT2=%trim(%subst(vindta:358:34))
026600080409     C                   EVAL      VABCBO=%trim(%subst(vindta:30:1))
026700080409     C                   EVAL      VABVCA=%trim(%subst(vindta:275:3))
026800080409     C                   EVAL      VABTIC=%trim(%subst(vindta:273:2))
026900080409     C                   EVAL      VABTSP=%trim(%subst(vindta:197:1))
027000140411     C                   IF        %subst(vindta:438:1)='A'
027100140411     C                   EVAL      VABTC1='A'
027200140411     C                   ENDIF
027300080409     C*
027400080409     C* Reperimento campi NUMERICI
027500080409     C                   MOVEL     DATCOR        VABAAS
027600080409     C                   MOVE      DATCOR        VABMGS
027700080409     C* NSP/RMN
027800150929     C                   EVAL      PiStr=%trim(%subst(vindta:22:8))
027900080409     C                   EXSR      CHKNUM
028000080409     C                   IF        PiInt=*on
028100080409     C                   Z-ADD     PiVal         VABNSP
028200080409     C                   Z-ADD     PiVal         VATNSP
028300080409     C                   Z-ADD     PiVal         VABRMN
028400080409     C                   ELSE
028500080409     C                   ADD       1             errore
028600080409     C                   Z-ADD     *zeros        VABNSP
028700080409     C                   Z-ADD     1             VABRMN
028800080409     C                   EVAL      vinmsg = %trimr(vinmsg)
028900080409     C                             + ' ' + 'VABNSP VABRMN VATNSP'
029000080409     C                   ENDIF
029100080409     C* CAD
029200080409     C                   EVAL      PiStr=%trim(%subst(vindta:140:5))
029300080409     C                   EXSR      CHKNUM
029400080409     C                   IF        PiInt=*on
029500080409     C                   Z-ADD     PiVal         Num5_0
029600080409     C                   MOVEL(p)  Num5_0        VABCAD
029700080409     C                   ELSE
029800080409     C                   ADD       1             errore
029900080409     C                   EVAL      VABCAD = *zeros
030000080409     C                   EVAL      vinmsg = %trimr(vinmsg)
030100080409     C                             + ' ' + 'VABCAD'
030200080409     C                   ENDIF
030300080409     C* NCL
030400080409     C                   EVAL      PiStr=%trim(%subst(vindta:230:5))
030500080409     C                   EXSR      CHKNUM
030600080409     C                   IF        PiInt=*on
030700080409     C                   Z-ADD     PiVal         VABNCL
030800080409     C                   ELSE
030900080409     C                   ADD       1             errore
031000080409     C                   Z-ADD     *zeros        VABNCL
031100080409     C                   EVAL      vinmsg = %trimr(vinmsg)
031200080409     C                             + ' ' + 'VABNCL'
031300080409     C                   ENDIF
031400080409     C* CCM
031500080409     C                   EVAL      PiStr=%trim(%subst(vindta:2:7))
031600080409     C                   EXSR      CHKNUM
031700080409     C                   IF        PiInt=*on
031800080409     C                   Z-ADD     PiVal         VABCCM
031900080409     C                   ELSE
032000080409     C                   ADD       1             errore
032100080409     C                   EVAL      vinmsg = %trimr(vinmsg)
032200080409     C                             + ' ' + 'VABCCM'
032300080409     C                   ENDIF
032400080409     C* CTR
032500140411     C                   EVAL      PiStr=%trim(%subst(vindta:193:4))
032600080409     C                   EXSR      CHKNUM
032700080409     C                   IF        PiInt=*on
032800080409     C                   Z-ADD     PiVal         VABCTR
032900080409     C                   ELSE
033000080409     C                   ADD       1             errore
033100080409     C                   EVAL      vinmsg = %trimr(vinmsg)
033200080409     C                             + ' ' + 'VABCTR'
033300080409     C                   ENDIF
033400080409     C* PKB
033500080409     C                   EVAL      PiStr=%trim(%subst(vindta:235:7))
033600080409     C                   EXSR      CHKNUM
033700080409     C                   IF        PiNum=*on
033800080409     C                   Z-ADD(H)  PiVal         VABPKB
033900080409     C                   ELSE
034000080409     C                   ADD       1             errore
034100080409     C                   Z-ADD     *zeros        VABPKB
034200080409     C                   EVAL      vinmsg = %trimr(vinmsg)
034300080409     C                             + ' ' + 'VABPKB'
034400080409     C                   ENDIF
034500080409     C* CAS
034600080409     C                   IF        %trim(%subst(vindta:260:13)) <> '0,000'
034700080409     C                   EVAL      FlgCAS = '1'
034800080409     C                   EVAL      PiStr=%trim(%subst(vindta:260:13))
034900080409     C                   EXSR      CHKNUM
035000080409     C                   IF        PiNum=*on
035100080409     C                   Z-ADD     PiVal         VABCAS
035200080409     C                   ELSE
035300080409     C                   ADD       1             errore
035400080409     C                   Z-ADD     *zeros        VABCAS
035500080409     C                   EVAL      vinmsg = %trimr(vinmsg)
035600080409     C                             + ' ' + 'VABCAS'
035700080409     C                   ENDIF
035800080409     C                   ENDIF
035900071121     C*
036000071121     C* Carico l'estensione B del FNVAT
036100071121     C                   exsr      exevatb
036200010205     C*
036300010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
036400040802     C                   IF        FlgCAS <> '0'
036500070102     C                   IF        VABCBO = '1'
036600010205     C                   EVAL      VABCBO = '4'
036700090123     C                   ENDIF
036800090123     C                   IF        VABCBO = '2'
036900070102     C                   EVAL      VABCBO = '6'
037000070102     C                   ENDIF
037100010205     C                   ENDIF
037200020305     C*
037300011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
037400011113     C                   EXSR      CHKIMPDIV
037500010202     C*
037600000801     C* Ebbene...
037700000801     C                   ADD       1             §CTRMO
037800010201     C                   IF        errore <> *zeros
037900000801     C                   ADD       1             §CTRNO
038000000801     C                   EVAL      vinflg = '2'
038100000801     C                   ELSE
038200010201     C                   ADD       1             §CTROKVB
038300000801     C                   ENDIF
038400000801     C*
038500000801     C                   ENDSR
038600070102     C*----------------------------------------------------*
038700140328     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "E"
038800070102     C*----------------------------------------------------*
038900070103     C     EXEVATE       BEGSR
039000070102     C*
039100070103     C                   EVAL      VATTRC='E'
039200111109     C***                EVAL      VATNOT = %trim(%subst(vindta:287:8))
039300111109     C                   EVAL      VATNOT = %trim(%subst(vindta:280:15))
039400071019     C*
039500070102     C                   exsr      wrivat                                       => scarico VAT
039600070102     C*
039700070102     C                   ENDSR
039800071121     C*----------------------------------------------------*
039900140328     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "B"
040000071121     C*----------------------------------------------------*
040100071121     C     EXEVATB       BEGSR
040200071121     C*
040300140411     C                   EVAL      VATTRC='B'
040400140411     C                   EVAL      VATNOT = %trim(%subst(vindta:487:15))
040500071121     C*
040600140411     C                   exsr      wrivat                                       => scarico VAT
040700071121     C*
040800071121     C                   ENDSR
040900010201     C*----------------------------------------------------*
041000140328     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
041100010201     C*----------------------------------------------------*
041200020305     C     WRIVAT        BEGSR
041300140328     C*
041400140328     C* VALORIZZO CAMPI RELATIVI AL "CMR"
041500140328     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
041600140328     C                   EVAL      VATCNT = 1
041700050628     C*
041800060223     C* Scrivo solo se valorizzato qualcosa
041900060223     C                   IF        VATNOT <> *blanks
042000140328     C                   WRITE     EDIVAT00
042100060223     C                   ENDIF
042200010201     C*
042300010201     C                   ENDSR
042400010202     C*----------------------------------------------------*
042500140328     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
042600010202     C*----------------------------------------------------*
042700020305     C     PREVAT        BEGSR
042800010202     C*
042900140328     C* Compongo il nome del membro da dare al EDIVATWR
043000010202     C                   eval      parmbr = vlrhdl
043100010202     C                   movel     'M'           parmbr
043200050627     C                   eval      parccm = %subst(vlrKSC:2:7)
043300010202     C                   eval      paropz = '1'
043400010202     C* Effettuo la chiamata al CLLE preposto
043500140328     C                   call(e)   'TITVEVTC'
043600010202     C                   parm                    parccm
043700010202     C                   parm                    parmbr
043800010202     C                   parm                    paropz
043900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
044000010202     C                   if        %error
044100010202     C                   movel     '1'           chkcall
044200010202     C                   else
044300010202     C                   movel     '0'           chkcall
044400010202     C                   endif
044500010202     C*
044600010202     C                   ENDSR
044700000801     C*----------------------------------------------------*
044800000801     C*  CONTROLLO NUMERICITA' CAMPI
044900000801     C*----------------------------------------------------*
045000000801     C     CHKNUM        BEGSR
045100000801     C*
045200000801     C                   call(e)   'ISNUMERIC'
045300000801     C                   PARM                    PiStr            30
045400070213     C                   PARM      ','           PiDecChr          1
045500000801     C                   PARM      *ZEROS        PiVal            30 9
045600000801     C                   PARM      '0'           PiInt             1
045700000801     C                   PARM      '0'           PiNum             1
045800000801     C                   IF        %error
045900000801     C                   EVAL      PiInt=*off
046000000801     C                   ENDIF
046100000801     C*
046200000801     C                   ENDSR
046300000801     C***
046400000801
046500011113
046600011113     C*----------------------------------------------------*
046700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
046800011113     C*----------------------------------------------------*
046900011113     C     CHKIMPDIV     BEGSR
047000011113     C*
047100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
047200011113     C                   Z-ADD     *zeros        wrkDec            9 9
047300011113     C*
047400011113     C* Come prima cosa effettuo considerazioni sulla divisa
047500011113     C                   IF        vabIAS > *zeros
047600011113     C                   IF        vabVAS <> 'EUR'
047700011113     C                   EVAL      vabVAS =  'ITL'
047800011113     C                   ENDIF
047900011113     C                   ENDIF
048000011113     C*
048100011113     C                   IF        vabCAS > *zeros
048200011113     C                   IF        vabVCA <> 'EUR'
048300011113     C                   EVAL      vabVCA =  'ITL'
048400011113     C                   ENDIF
048500011113     C                   ENDIF
048600011113     C*
048700011113     C                   IF        vabVMD > *zeros
048800020305     C                   IF        vabVAD <> 'EUR'
048900011113     C                   EVAL      vabVAD =  'ITL'
049000011113     C                   ENDIF
049100011113     C                   ENDIF
049200011113     C*
049300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
049400011113     C                   Z-ADD     vabIAS        wrkDec
049500011113     C                   IF        wrkDec > *zeros
049600011113     C                   IF        vabVAS = 'ITL'
049700011113     C                   EVAL      vabIAS = *zeros
049800011113     C                   ENDIF
049900011113     C                   ENDIF
050000011113     C*
050100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
050200011113     C                   Z-ADD     vabCAS        wrkDec
050300011113     C                   IF        wrkDec > *zeros
050400011113     C                   IF        vabVCA = 'ITL'
050500011113     C                   EVAL      vabCAS = *zeros
050600011113     C                   ENDIF
050700011113     C                   ENDIF
050800011113     C*
050900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
051000011113     C                   Z-ADD     vabVMD        wrkDec
051100011113     C                   IF        wrkDec > *zeros
051200011113     C                   IF        vabVAD = 'ITL'
051300011113     C                   EVAL      vabVMD = *zeros
051400011113     C                   ENDIF
051500011113     C                   ENDIF
051600011113     C*
051700011113     C                   ENDSR
051800011113     C***
051900011113
052000011113
052100000801
052200000801
052300990920      /TITLE Invio dei dati al punto operativo.
052400010202     C     invio         BEGSR
052500990920     C*
052600140328     C* 1° invio EDIVAT
052700010201     C                   reset                   dscmz
052800010201     C                   move      vlrpoi        cmzdst
052900140328     C                   eval      cmzfld = 'EDIVATWR'
053000010201     C                   eval      cmzmbd = vlrhdl
053100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
053200021009     C***                if        prmfir = *blanks
053300140328     C                   eval      cmzfla = 'EDIVAT0F'
053400140328     C                   eval      cmzmba = 'EDIVAT0F'
053500021009     C***                else
053600021009     C***                eval      cmzfla = prmfir
053700021009     C***                eval      cmzmba = prmfir
053800021009     C***                endif
053900010201     C                   eval      cmznrr = *zeros
054000020305     C                   move      §ctrokvt      cmznrr
054100021018     C                   eval      cmzlba = vlrfl1
054200010201     C                   call(e)   'TIS711C'
054300010201     C                   parm                    dscmz
054400010201     C                   parm      *blanks       esito
054500010205     C                   if        %error
054600010205     C                             or cmzerr = '1'
054700010205     C                             or esito  = '1'
054800010205     C                   eval      wrkesito = '3'
054900010205     C                   else
055000010201     C*
055100140328     C* 2° invio EDIVAB
055200010201     C                   reset                   dscmz
055300010201     C                   move      vlrpoi        cmzdst
055400010201     C                   eval      cmzfld = vlrfou
055500010201     C                   eval      cmzmbd = vlrhdl
055600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055700021009     C***                if        prmfir = *blanks
055800140328     C                   eval      cmzfla = 'EDIVAB0F'
055900140328     C                   eval      cmzmba = 'EDIVAB0F'
056000021009     C***                else
056100021009     C***                eval      cmzfla = prmfir
056200021009     C***                eval      cmzmba = prmfir
056300021009     C***                endif
056400010201     C                   eval      cmznrr = *zeros
056500010201     C                   move      §ctrokvb      cmznrr
056600021018     C                   eval      cmzlba = vlrfl1
056700010201     C                   call(e)   'TIS711C'
056800010201     C                   parm                    dscmz
056900010201     C                   parm      *blanks       esito
057000010201     C                   if        %error
057100010201     C                             or cmzerr = '1'
057200010201     C                             or esito  = '1'
057300010201     C                   eval      wrkesito = '3'
057400010201     C                   endif
057500010205     C                   endif
057600990920     C*
057700000613     C                   ENDSR
057800000613     C***
057900070411
058000070411     C     *pssr         BEGSR
058100070411     C*
058200070411     C                   if        %open(tivin00r)
058300070411     C                   close     tivin00r
058400070411     C                   endif
058500140328     C                   if        %open(edivabwr)
058600140328     C                   close     edivabwr
058700070411     C                   endif
058800140328     C                   if        %open(edivatwr)
058900140328     C                   close     edivatwr
059000070411     C                   endif
059100070411     C*
059200070411     C* Effettuo la chiamata al CLLE preposto
059300140328     C                   call(e)   'TITVEVTC'
059400070411     C                   parm                    parccm
059500070411     C                   parm                    parmbr
059600070411     C                   parm      '2'           paropz
059700070411     C*
059800070411     C                   eval      wrkesito = '2'
059900070411     C*
060000070411     C                   seton                                        LR
060100070411     C*
060200070411     C                   ENDSR     '*CANCL'
060300070411     C***
060400070411
060500990910
060600000613     C     *inzsr        BEGSR
060700990910     C*
060800990910     C     *entry        plist
060900990920     C                   parm                    tivlrds
061000990921     C                   parm      wrkesito      esito
061100000724     C                   parm                    prmlit
061200000710     C                   parm                    prmfir
061300000613     C*
061400000830     C* CALCOLA LA DATA CORRENTE
061500000830     C                   time                    wn14             14 0
061600140328     C                   movel     wn14          oracor            6 0          *ORA
061700000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
061800000830     C                   z-add     wn8           g08dat
061900000830     C                   z-add     *zeros        g08inv
062000000830     C                   movel     '0'           g08err
062100000830     C                   call      'XSRDA8'
062200000830     C                   parm                    wlbda8
062300000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
062400000830     C*
062500000613     C                   ENDSR
062600000613     C***
