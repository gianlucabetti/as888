000100140117      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200140120     H dftactgrp(*no) actgrp(*caller)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500140117     FEDIVABwr  O    E             DISK    usropn
000600140117     FEDIVATwr  O    E             DISK    usropn
000700990908
000800000801     D*----------------------------------------------------
000900000801     D* DICHIARAZIOINE VARIABILI DI WRK
001000000801     D*----------------------------------------------------
001100990920     D dscmz         e ds                  inz
001200990910     D psds           sds
001300990910     D  procname         *PROC
001400990920     D tivlrds       e ds                  extname(tivlr00f)
001500070719     D tisi95ds      e ds
001600140117     D edivabds      e ds                  extname(edivab0f)
001700990910     D esito           s              1
001800000724     D prmlit          s             10
001900000710     D prmfir          s             10
002000990921     D wrkesito        s                   like(esito)
002100000613     D rrnum           s              6  0 INZ(*zeros)
002200010202     D parccm          s              8    INZ(*blanks)
002300010202     D parmbr          s             10    INZ(*blanks)
002400010202     D paropz          s              1    INZ(*blanks)
002500010202     D chkcall         s              1    INZ(*blanks)
002600140922     D depspe          s             17    INZ(*blanks)
002700000830
002800041025     D*------------------
002900041025     D* DS REPERIMENTO NUMERATORE
003000041025     D*------------------
003100041025     D trul33ds      e ds                  inz
003200041025     D*------------------
003300041025     D* DS ARCHITETTURA
003400041025     D*------------------
003500041025     D kpjba         e ds                  inz
003600041025     D*------------------
003700990908
003800010201
003900010201
004000000913     C                   reset                   rrnum
004100990921     C                   reset                   esito
004200990921     C                   reset                   wrkesito
004300000613     C*
004400150213     C                   EXSR      DEFCAM
004500150213     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004600070719     C*
004700070719     C* Esegue lancio TISI95R solo x chiusura
004800070719     C                   CLEAR                   TISI95DS
004900070719     C                   EVAL      I95TLA = 'C'
005000070719     C                   CALL      'TISI95R'
005100070719     C                   PARM                    TISI95DS
005200000613     C*
005300010202     C* Effettuo la chiamata al CLLE preposto
005400140922     C                   call(e)   'TITVEVTC'
005500140922     C                   parm                    parccm
005600140922     C                   parm                    parmbr
005700140922     C                   parm      '2'           paropz
005800000616     C*
005900010201     C                   seton                                        LR
006000990908
006100000801
006200910830     C*--------------------------------------------------------
006300140117     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
006400910830     C*--------------------------------------------------------
006500040526     C     RWFILE        BEGSR
006600990910     C*
006700990914     C                   if        not %open(tivin00r)
006800990908     C                   open      tivin00r
006900990914     C                   endif
007000140117     C                   if        not %open(edivabwr)
007100140117     C                   open      edivabwr
007200990914     C                   endif
007300140117     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
007400020305     C                   exsr      prevat
007500010201     C*
007600010202     C                   if        chkcall = '0'
007700010202     C*
007800140117     C                   if        not %open(edivatwr)
007900140117     C                   open      edivatwr
008000010201     C                   endif
008100990910     C*
008200010201     C                   clear                   §CTROKVB          5 0
008300020305     C                   clear                   §CTROKVT          5 0
008400000801     C                   clear                   §CTRMO            5 0
008500000801     C                   clear                   §CTRNO            5 0
008600100729     C*
008700100729     C                   z-add     1             wGiro             1 0
008800140117     C*
008900140902     C                   if        *in53
009000140117     C                   call      'TIS7P2SER'
009100140117     C                   parm      'O'           pIn_Opz           1
009200140117     C                   parm                    tivlrds
009300140117     C                   parm                    EDIVABDS
009400140117     C                   parm      *blanks       pIn_CdIdAz        2
009500140117     C                   parm      *blanks       pIn_MaskPDF      50
009600140117     C                   parm      *blanks       pOut_Esito        1
009700140902     C                   endif
009800130708     C*
009900130708     C                   SETOFF                                       31
010000921023     C                   DO        *HIVAL
010100990913     C*
010200990915     C                   READ      tivin00r                               70
010300040910     C                   if        vindta > *blanks
010400000613     C                   add       1             rrnum
010500000801     C*
010600000801     C                   if        *in70 = *off
010700000801     C                             and
010800000801     C                             (vinflg = *blanks
010900000801     C                              or vinflg = '0'
011000000801     C                              or vinflg = '2')
011100000801     C*
011200000801     C                   clear                   vinmsg
011300000801     C                   eval      vinflg = '1'
011400040910     C*
011500040910     C* Eseguo routine d traduzione
011600040910     C                   exsr      impvabvat
011700040802     C*
011800010305     C                   endif
011900000905     C*
012000000905     C                   else
012100000905     C                   eval      vinflg = '1'
012200000905     C                   endif
012300000905     C*
012400000905     C  N70              update    tivin000
012500000905     C*
012600991022     C  N70              ENDdo
012700100722     C*
012800100722     C* Scarico i buffer testata ancora "in canna"
012900140922     C*
013000140922     C  N31              exsr      CHKWRI
013100140117     C  N31              WRITE     EDIVAB00
013200130708     C                   SETOFF                                       31
013300140117     C*
013400140902     C                   if        *in53
013500140117     C                   call      'TIS7P2SER'
013600140117     C                   parm      'C'           pIn_Opz           1
013700140117     C                   parm                    tivlrds
013800140117     C                   parm                    EDIVABDS
013900140117     C                   parm      *blanks       pIn_CdIdAz        2
014000140117     C                   parm      *blanks       pIn_MaskPDF      50
014100140117     C                   parm      *blanks       pOut_Esito        1
014200140902     C                   endif
014300010202     C*
014400010202     C                   endif
014500990910
014600990910     C* Se non ci sono record con errori ...
014700000710     C                   if        §ctrno = 0
014800990910     C* ... restituisco esito OK.
014900990921     C                   eval      wrkesito = '0'
015000990910     C                   else
015100010201     C                   if        §ctrokvb > 0
015200990921     C                   eval      wrkesito = '1'
015300000710     C                   else
015400000710     C                   eval      wrkesito = '2'
015500990910     C                   endif
015600000710     C                   endif
015700990910     C*
015800990914     C                   if        %open(tivin00r)
015900990908     C                   close     tivin00r
016000990914     C                   endif
016100140117     C                   if        %open(edivabwr)
016200140117     C                   close     edivabwr
016300990914     C                   endif
016400140117     C                   if        %open(edivatwr)
016500140117     C                   close     edivatwr
016600010201     C                   endif
016700990910     C*
016800010201     C                   if        §ctrokvb > 0
016900000724     C                             and vlrpoi <> *zeros
017000010202     C                   exsr      invio
017100990920     C                   endif
017200990920     C*
017300910830     C                   ENDSR
017400000613     C***
017500140922
017600140922     C*----------------------------------------------------*
017700140922     C*  FORZATURE E CONSIDERAZIONI PRE-WRITE TESTATA
017800140922     C*----------------------------------------------------*
017900140922     C     CHKWRI        BEGSR
018000140922     C*
018100140922     C                   if        wRMN_2 > *zeros
018200140922     C                   eval      VABRMN = wRMN_2
018300140922     C                   eval      VABRMA = wRMA_2
018400140922     C                   else
018500140922     C                   eval      VABRMN = wRMN_1
018600140922     C                   eval      VABRMA = wRMA_1
018700140922     C                   endif
018800140922     C*
018900140922     C* Considerazioni sul contenuto di campi precedentemente valorizzati
019000140922     C                   IF        FlgCAS <> '0'
019100140922     C                   IF        VABCBO = '1'
019200140922     C                   EVAL      VABCBO = '4'
019300140922     C                   ELSE
019400140922     C                   EVAL      VABCBO = '6'
019500140922     C                   ENDIF
019600140922     C                   ENDIF
019700140922     C*
019800140922     C* Eseguo routine finale x considerazioni specifiche su importi/divise
019900140922     C                   EXSR      CHKIMPDIV
020000140922     C*
020100140922     C                   ENDSR
020200990920
020300000801     C*----------------------------------------------------*
020400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020500000801     C*----------------------------------------------------*
020600010201     C     INZVAR        BEGSR
020700000801     C*
020800040802     C                   Z-ADD     *zeros        Num5_0            5 0
020900040802     C                   MOVEL     '0'           FlgCAS            1
021000000801     C*
021100000801     C                   ENDSR
021200000801     C*----------------------------------------------------*
021300040910     C*  IMPOSTAZIONE CAMPI COSTANTI
021400000801     C*----------------------------------------------------*
021500000801     C     DEFCAM        BEGSR
021600000801     C*
021700140117     C                   CLEAR                   EDIVAB00
021800140117     C                   CLEAR                   EDIVAT00
021900020619     C* Imposto i valori di default...
022000140922     C                   Z-ADD     1662533       VABCCM
022100140922     C                   Z-ADD     1662533       VATCCM
022200140922     C                   Z-ADD     166           VABLNP
022300140922     C                   Z-ADD     166           VATLNP
022400140922     C                   Z-ADD     006           VABCTR
022500140117     C                   MOVEL     '7T'          VABCTM
022600040823     C                   MOVEL     '1'           VABCBO
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
025200060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
025300060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
025400060202     C                   ENDIF
025500140902     C                   IF        %subst(vlrppt:16:1) = 'S'
025600140902     C                   SETON                                        53
025700140902     C                   ELSE
025800140902     C                   SETOFF                                       53
025900140902     C                   ENDIF
026000020619     C                   ENDIF
026100000801     C*
026200000801     C                   ENDSR
026300000801     C*----------------------------------------------------*
026400140117     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
026500000801     C*----------------------------------------------------*
026600040910     C     IMPVABVAT     BEGSR
026700040910     C*
026800040910     C* Traduzione relativa ai tipi record del file del cliente
026900040910     C*
027000071210     C*
027100071210     C***
027200140922     C* ...tipo record 'BGM' (info CMR)
027300140922     C                   IF        %trim(%subst(vindta:1:3)) = 'BGM'
027400140903     C                   MOVEL     *blanks       savCMR           35
027500140922     C                   EVAL      savCMR = %trim(%subst(vindta:17:15)) + ' '+
027600140922     C                                      %char(datcor) + ' ' + %char(oracor)
027700130620     C                   ENDIF
027800140922     C***
027900140922     C* ...tipo record 'CNI' (rottuta di codice spedizione)
028000140922     C                   IF        %trim(%subst(vindta:1:3)) = 'CNI' and
028100140923     C                             (wRMA_2 <> %subst(vindta:921:21) or
028200140926     C                              %subst(vindta:921:21) = *blanks or
028300140923     C                              wGiro = 1 or FlgCAS = '1')
028400100722     C*
028500100722     C* Se nn primo giro => scarico il buffer precedente
028600100722     C                   if        wGiro = 1
028700100722     C                   eval      wGiro = 2
028800100722     C                   else
028900140922     C*
029000140922     C  N31              exsr      CHKWRI
029100140117     C  N31              WRITE     EDIVAB00
029200130708     C                   SETOFF                                       31
029300100722     C                   endif
029400100722     C*
029500071210     C* Resetto indicatore d anomalia sul singolo record
029600071210     C                   eval      vinflg = '1'
029700071210     C* ......inizializzazioni iniziali e formati record file Bartolini
029800071210     C                   EXSR      INZVAR
029900071210     C                   EXSR      DEFCAM
030000071210     C*
030100071210     C                   Z-ADD     *zeros        errore            1 0
030200071210     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
030300071210     C                   MOVEL     datcor        VABAAS
030400071210     C                   MOVEL     datcor        VATAAS
030500071210     C                   MOVE      datcor        VABMGS
030600071210     C                   MOVE(P)   vlrpoi        VABFGS
030700071210     C                   MOVE(P)   vlrpoi        VATFGS
030800140903     C*
030900140903     C* Dai "fissi"
031000140903     C                   EVAL      VABCMR = savCMR
031100140903     C                   EVAL      VABDCM = datcor
031200140903     C                   EVAL      VABDTS = datcor
031300140903     C                   EVAL      VABHMS = oracor
031400140903     C                   EVAL      VABCNT = 1
031500140903     C*
031600071210     C* ......VABNSP/VATNSP
031700071210     C* NSP => Stacco un numeratore da AZNUM
031800071210     C                   clear                   TRUL33DS
031900071210     C                   eval      I33OPE = *zeros
032000071210     C                   eval      I33CNU = 302
032100071210     C                   eval      I33NUM = 1
032200071210     C                   movel     TRUL33DS      KPJBU
032300071210     C                   call      'TRUL33R'
032400071210     C                   parm                    KPJBA
032500071210     C                   movel     KPJBU         TRUL33DS
032600071210     C                   if        O33ERR = *zeros
032700071210     C                   z-add     O33NRF        VABNSP
032800071210     C                   z-add     O33NRF        VATNSP
032900071210     C                   else
033000071210     C                   Z-ADD     1             errore
033100071210     C                   EVAL      vinmsg = %trimr(vinmsg)
033200071210     C                             + ' ' + 'VABNSP VATNSP'
033300071210     C                   endif
033400140922     C*
033500140922     C                   ENDIF
033600140922     C***
033700140922     C* ...tipo record 'CNI' (info testata)
033800140922     C                   IF        %trim(%subst(vindta:1:3)) = 'CNI'
033900140922     C                   z-add     *zeros        wRMN_1           17 0
034000140923     C                   z-add     *zeros        wRMN_2           21 0
034100140922     C                   movel     *blanks       wRMA_1           17
034200140923     C                   movel     *blanks       wRMA_2           21
034300140922     C* ......VABRMN_1
034400140922     C                   EVAL      PiStr=%trim(%subst(vindta:9:35))
034500140922     C                   EXSR      CHKNUM
034600140922     C                   IF        PiInt=*on
034700140922     C                   Z-ADD     PiVal         wRMN_1
034800140922     C                   ELSE
034900140922     C                   ADD       1             errore
035000140922     C                   EVAL      vinmsg = %trimr(vinmsg)
035100140923     C                             + ' ' + 'VABRMN_1'
035200140922     C                   ENDIF
035300140922     C* ......VABRMA_1
035400140922     C                   EVAL      wRMA_1=%trim(%subst(vindta:710:17))
035500141106     C* ......VABRMO / VABNAS
035600140923     C                   EVAL      VABRMO=%trim(%subst(vindta:884:17))
035700141106     C                   EVAL      VABNAS=%trim(%subst(vindta:884:17))
035800140922     C* ......VABNCL
035900140922     C                   EVAL      PiStr=%trim(%subst(vindta:56:5))
036000140922     C                   EXSR      CHKNUM
036100140922     C                   IF        PiInt=*on
036200140922     C                   ADD       PiVal         VABNCL
036300140922     C                   ELSE
036400140922     C                   ADD       1             errore
036500140922     C                   EVAL      vinmsg = %trimr(vinmsg)
036600140922     C                             + ' ' + 'VABNCL'
036700140922     C                   ENDIF
036800140922     C* ......VABPKB
036900140922     C                   EVAL      PiStr=%trim(%subst(vindta:66:12))
037000140922     C                   EXSR      CHKNUM
037100140922     C                   IF        PiNum=*on
037200140922     C                   EVAL      PiVal = PiVAl / 100
037300140922     C                   ADD       PiVal         VABPKB
037400140922     C                   ELSE
037500140922     C                   ADD       1             errore
037600140922     C                   EVAL      vinmsg = %trimr(vinmsg)
037700140922     C                             + ' ' + 'VABPKB'
037800140922     C                   ENDIF
037900140922     C* ......VABVLB
038000140922     C                   EVAL      PiStr=%trim(%subst(vindta:78:12))
038100140922     C                   EXSR      CHKNUM
038200140922     C                   IF        PiNum=*on
038300140922     C                   EVAL      PiVal = PiVAl / 100
038400140922     C                   ADD       PiVal         VABVLB
038500140922     C                   ELSE
038600140922     C                   ADD       1             errore
038700140922     C                   EVAL      vinmsg = %trimr(vinmsg)
038800140922     C                             + ' ' + 'VABVLB'
038900140922     C                   ENDIF
039000140117     C* ......VABCAS
039100140922     C                   IF        %subst(vindta:145:12) <> *all'0'
039200140117     C                   EVAL      FlgCAS = '1'
039300140922     C                   EVAL      VABVCA = 'EUR'
039400140922     C                   EVAL      PiStr=%trim(%subst(vindta:145:12))
039500140117     C                   EXSR      CHKNUM
039600140117     C                   IF        PiNum=*on
039700140922     C                   EVAL      PiVal = PiVAl / 100
039800140922     C                   ADD       PiVal         VABCAS
039900140117     C                   ELSE
040000140117     C                   ADD       1             errore
040100140117     C                   EVAL      vinmsg = %trimr(vinmsg)
040200140117     C                             + ' ' + 'VABCAS'
040300140117     C                   ENDIF
040400140117     C                   ENDIF
040500140922     C* ......VABIAS
040600140922     C                   IF        %subst(vindta:157:12) <> *all'0'
040700140922     C                   EVAL      VABVAS = 'EUR'
040800140922     C                   EVAL      PiStr=%trim(%subst(vindta:157:12))
040900140922     C                   EXSR      CHKNUM
041000140922     C                   IF        PiNum=*on
041100140922     C                   EVAL      PiVal = PiVAl / 100
041200140922     C                   ADD       PiVal         VABIAS
041300140922     C                   ELSE
041400140922     C                   ADD       1             errore
041500140922     C                   EVAL      vinmsg = %trimr(vinmsg)
041600140922     C                             + ' ' + 'VABIAS'
041700140922     C                   ENDIF
041800140922     C                   ENDIF
041900140922     C* ......VABVMD
042000140922     C                   IF        %subst(vindta:169:12) <> *all'0'
042100140922     C                   EVAL      VABVAD = 'EUR'
042200140922     C                   EVAL      PiStr=%trim(%subst(vindta:169:12))
042300140922     C                   EXSR      CHKNUM
042400140922     C                   IF        PiNum=*on
042500140922     C                   EVAL      PiVal = PiVAl / 100
042600140922     C                   ADD       PiVal         VABVMD
042700140922     C                   ELSE
042800140922     C                   ADD       1             errore
042900140922     C                   EVAL      vinmsg = %trimr(vinmsg)
043000140922     C                             + ' ' + 'VABVMD'
043100140922     C                   ENDIF
043200140922     C                   ENDIF
043300140922     C* ......VABRMN_2
043400151027     C                   IF        %subst(vindta:922:20) <> *blanks
043500151027     C                   EVAL      PiStr=%trim(%subst(vindta:922:20))
043600140922     C                   EXSR      CHKNUM
043700140922     C                   IF        PiInt=*on
043800140922     C                   Z-ADD     PiVal         wRMN_2
043900140922     C                   ELSE
044000140922     C                   ADD       1             errore
044100140922     C                   EVAL      vinmsg = %trimr(vinmsg)
044200140923     C                             + ' ' + 'VABRMN_2'
044300140922     C                   ENDIF
044400140922     C                   ENDIF
044500140922     C* ......VABRMA_2
044600140923     C                   EVAL      wRMA_2=%trim(%subst(vindta:921:21))
044700140922     C                   if        FlgCAS = '1'
044800140922     C                   EVAL      wRMA_2 = *blanks
044900140922     C                   endif
045000141002     C* ......VABDCR / VABHCR
045100141009     C                   IF        %trim(%subst(vindta:142:2)) = '2'
045200141002     C                   IF        %trim(%subst(vindta:125:8)) <> *blanks AND
045300141002     C                             %trim(%subst(vindta:125:8)) <> *zeros
045400141002     C                   EVAL      PiStr=%trim(%subst(vindta:125:8))
045500141002     C                   EXSR      CHKNUM
045600141002     C                   IF        PiInt=*on
045700141002     C                   Z-ADD     PiVal         VABDCR
045800141002     C                   ELSE
045900141002     C                   ADD       1             errore
046000141002     C                   EVAL      vinmsg = %trimr(vinmsg)
046100141002     C                             + ' ' + 'VABDCR'
046200141002     C                   ENDIF
046300141002     C                   EVAL      PiStr=%trim(%subst(vindta:133:4))
046400141002     C                   EXSR      CHKNUM
046500141002     C                   IF        PiInt=*on
046600141002     C                   Z-ADD     PiVal         VABHCR
046700141002     C                   ELSE
046800141002     C                   ADD       1             errore
046900141002     C                   EVAL      vinmsg = %trimr(vinmsg)
047000141002     C                             + ' ' + 'VABHCR'
047100141002     C                   ENDIF
047200141002     C                   ENDIF
047300141002     C                   ENDIF
047400141009     C* ......VABDCR / VABHCR
047500141009     C                   IF        %trim(%subst(vindta:142:2)) = '17'
047600141009     C                   IF        %trim(%subst(vindta:125:8)) <> *blanks AND
047700141009     C                             %trim(%subst(vindta:125:8)) <> *zeros
047800141009     C                   EVAL      VABTCR = 'P'
047900141009     C                   EVAL      PiStr=%trim(%subst(vindta:125:8))
048000141009     C                   EXSR      CHKNUM
048100141009     C                   IF        PiInt=*on
048200141009     C                   Z-ADD     PiVal         VABDCR
048300141009     C                   ELSE
048400141009     C                   ADD       1             errore
048500141009     C                   EVAL      vinmsg = %trimr(vinmsg)
048600141009     C                             + ' ' + 'VABDCR'
048700141009     C                   ENDIF
048800141009     C                   EVAL      PiStr=%trim(%subst(vindta:133:4))
048900141009     C                   EXSR      CHKNUM
049000141009     C                   IF        PiInt=*on
049100141009     C                   Z-ADD     PiVal         VABHCR
049200141009     C                   ELSE
049300141009     C                   ADD       1             errore
049400141009     C                   EVAL      vinmsg = %trimr(vinmsg)
049500141009     C                             + ' ' + 'VABHCR'
049600141009     C                   ENDIF
049700141009     C                   ENDIF
049800141009     C                   ENDIF
049900140922     C* ......VATNOT_P
050000140922     C                   if        *in53
050100140922     C*
050200140922     C* Solo se valorizzato RMN
050300140922     C                   if        wRMA_1 <> *blanks
050400140923     C                   eval      pIn_MaskPDF =
050500140923     C                                     %trim(%subst(vindta:1292:100))
050600140922     C                   call      'TIS7P2SER'
050700140922     C                   parm      'W'           pIn_Opz           1
050800140922     C                   parm                    tivlrds
050900140922     C                   parm                    EDIVABDS
051000140922     C                   parm      '10'          pIn_CdIdAz        2
051100140922     C                   parm                    pIn_MaskPDF      50
051200140922     C                   parm      *blanks       pOut_Esito        1
051300140922     C                   endif
051400140922     C                   endif
051500140117     C*
051600140117     C                   ENDIF
051700140117     C***
051800140922     C* ...tipo record 'NA2/ST' (info testata - destinatario)
051900140922     C                   IF        %trim(%subst(vindta:1:3)) = 'NA2'
052000140922     C* Solo sub-types 'ST'
052100140922     C                   IF        %trim(%subst(vindta:4:2)) = 'ST'
052200060202     C* ......VABRSD
052300140922     C                   EVAL      VABRSD=%trim(%subst(vindta:42:35))
052400130620     C* ......VABRD2
052500140922     C                   EVAL      VABRD2=%trim(%subst(vindta:77:35))
052600100714     C* ......VABIND
052700140922     C                   EVAL      VABIND=%trim(%subst(vindta:112:35))
052800130620     C* ......VABCAD
052900140922     C                   EVAL      VABCAD=%trim(%subst(vindta:226:17))
053000060202     C* ......VABLOD
053100140922     C                   EVAL      VABLOD=%trim(%subst(vindta:182:35))
053200140922     C* ......VABPRD
053300140922     C                   EVAL      VABPRD=%trim(%subst(vindta:217:9))
053400140922     C* ......VABNZD
053500140922     C                   EVAL      VABNZD=%trim(%subst(vindta:243:3))
053600130620     C                   IF        VABNZD='IT'
053700130620     C                   EVAL      VABNZD=*blanks
053800130620     C                   ENDIF
053900060202     C* ......VABPRD
054000070719     C* Reperisco la provincia dal CAP e dalla località
054100070719     C                   IF        VABPRD  = *blanks AND
054200130620     C                             VABLOD <> *blanks AND
054300130620     C                             VABCAD <> *blanks AND
054400130620     C                             VABNZD  = *blanks
054500070719     C                   CLEAR                   TISI95DS
054600070719     C                   EVAL      I95TCN = '3'
054700070719     C                   Z-ADD     datcor        I95DAT
054800070719     C                   EVAL      I95CAP = VABCAD
054900070719     C                   EVAL      I95LOC = VABLOD
055000070719     C                   CALL      'TISI95R'
055100070719     C                   PARM                    TISI95DS
055200070719     C                   EVAL      VABPRD = O95PRV
055300070719     C                   ENDIF
055400140922     C* ......VATNOT_A
055500140922     C                   EVAL      VATNOT=%trim(%subst(vindta:371:35))
055600140922     C                   IF        VATNOT <> *blanks
055700140922     C                   EVAL      VATCMR = VABCMR
055800140922     C                   EVAL      VATCNT = VABCNT
055900140922     C                   EVAL      VATTRC = 'A'
056000140922     C                   ADD       1             §CTROKVT
056100140922     C                   WRITE     EDIVAT00
056200140922     C                   ENDIF
056300140922     C* ......VATNOT_B
056400140922     C                   EVAL      VATNOT=%trim(%subst(vindta:406:20))
056500140922     C                   IF        VATNOT <> *blanks
056600140922     C                   EVAL      VATCMR = VABCMR
056700140922     C                   EVAL      VATCNT = VABCNT
056800140922     C                   EVAL      VATTRC = 'B'
056900140922     C                   ADD       1             §CTROKVT
057000140922     C                   WRITE     EDIVAT00
057100140922     C                   ENDIF
057200150216     C* ......VABNOT
057300150216     C                   EVAL      VABNOT=%trim(%subst(vindta:553:35))
057400150216     C* ......VABNT2
057500150216     C                   EVAL      VABNOT=%trim(%subst(vindta:553+35:35))
057600140117     C*
057700140117     C                   ENDIF
057800140117     C*
057900140117     C                   ENDIF
058000140922     C***
058100140922     C* ...tipo record 'GID' (dettaglio segnacolli)
058200140922     C                   IF        %trim(%subst(vindta:1:3)) = 'GID'
058300140922     C* ......VATNOT_E
058400140922     C                   EVAL      VATNOT=%trim(%subst(vindta:118:35))
058500140922     C                   IF        VATNOT <> *blanks
058600140922     C                   EVAL      VATCMR = VABCMR
058700140922     C                   EVAL      VATCNT = VABCNT
058800140922     C                   EVAL      VATTRC = 'E'
058900140922     C                   ADD       1             §CTROKVT
059000140922     C                   WRITE     EDIVAT00
059100140922     C                   ENDIF
059200140922     C*
059300140922     C                   ENDIF
059400010202     C*
059500000801     C* Ebbene...
059600000801     C                   ADD       1             §CTRMO
059700010201     C                   IF        errore <> *zeros
059800000801     C                   ADD       1             §CTRNO
059900000801     C                   EVAL      vinflg = '2'
060000000801     C                   ELSE
060100010201     C                   ADD       1             §CTROKVB
060200000801     C                   ENDIF
060300000801     C*
060400000801     C                   ENDSR
060500010202     C*----------------------------------------------------*
060600140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
060700010202     C*----------------------------------------------------*
060800020305     C     PREVAT        BEGSR
060900010202     C*
061000140117     C* Compongo il nome del membro da dare al EDIVATWR
061100010202     C                   eval      parmbr = vlrhdl
061200010202     C                   movel     'M'           parmbr
061300060113     C                   eval      parccm = vlrksc
061400010202     C                   eval      paropz = '1'
061500010202     C* Effettuo la chiamata al CLLE preposto
061600140117     C                   call(e)   'TITVEVTC'
061700010202     C                   parm                    parccm
061800010202     C                   parm                    parmbr
061900010202     C                   parm                    paropz
062000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
062100010202     C                   if        %error
062200010202     C                   movel     '1'           chkcall
062300010202     C                   else
062400010202     C                   movel     '0'           chkcall
062500010202     C                   endif
062600010202     C*
062700010202     C                   ENDSR
062800000801     C*----------------------------------------------------*
062900000801     C*  CONTROLLO NUMERICITA' CAMPI
063000000801     C*----------------------------------------------------*
063100000801     C     CHKNUM        BEGSR
063200000801     C*
063300000801     C                   call(e)   'ISNUMERIC'
063400000801     C                   PARM                    PiStr            30
063500040714     C                   PARM      ','           PiDecChr          1
063600000801     C                   PARM      *ZEROS        PiVal            30 9
063700000801     C                   PARM      '0'           PiInt             1
063800000801     C                   PARM      '0'           PiNum             1
063900000801     C                   IF        %error
064000000801     C                   EVAL      PiInt=*off
064100000801     C                   ENDIF
064200000801     C*
064300000801     C                   ENDSR
064400000801     C***
064500000801
064600011113
064700011113     C*----------------------------------------------------*
064800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
064900011113     C*----------------------------------------------------*
065000011113     C     CHKIMPDIV     BEGSR
065100011113     C*
065200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
065300011113     C                   Z-ADD     *zeros        wrkDec            9 9
065400011113     C*
065500011113     C* Come prima cosa effettuo considerazioni sulla divisa
065600011113     C                   IF        vabIAS > *zeros
065700011113     C                   IF        vabVAS <> 'EUR'
065800011113     C                   EVAL      vabVAS =  'ITL'
065900011113     C                   ENDIF
066000011113     C                   ENDIF
066100011113     C*
066200011113     C                   IF        vabCAS > *zeros
066300011113     C                   IF        vabVCA <> 'EUR'
066400011113     C                   EVAL      vabVCA =  'ITL'
066500011113     C                   ENDIF
066600011113     C                   ENDIF
066700011113     C*
066800011113     C                   IF        vabVMD > *zeros
066900020305     C                   IF        vabVAD <> 'EUR'
067000011113     C                   EVAL      vabVAD =  'ITL'
067100011113     C                   ENDIF
067200011113     C                   ENDIF
067300011113     C*
067400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
067500011113     C                   Z-ADD     vabIAS        wrkDec
067600011113     C                   IF        wrkDec > *zeros
067700011113     C                   IF        vabVAS = 'ITL'
067800011113     C                   EVAL      vabIAS = *zeros
067900011113     C                   ENDIF
068000011113     C                   ENDIF
068100011113     C*
068200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
068300011113     C                   Z-ADD     vabCAS        wrkDec
068400011113     C                   IF        wrkDec > *zeros
068500011113     C                   IF        vabVCA = 'ITL'
068600011113     C                   EVAL      vabCAS = *zeros
068700011113     C                   ENDIF
068800011113     C                   ENDIF
068900011113     C*
069000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
069100011113     C                   Z-ADD     vabVMD        wrkDec
069200011113     C                   IF        wrkDec > *zeros
069300011113     C                   IF        vabVAD = 'ITL'
069400011113     C                   EVAL      vabVMD = *zeros
069500011113     C                   ENDIF
069600011113     C                   ENDIF
069700011113     C*
069800011113     C                   ENDSR
069900011113     C***
070000011113
070100011113
070200000801
070300000801
070400990920      /TITLE Invio dei dati al punto operativo.
070500010202     C     invio         BEGSR
070600990920     C*
070700140117     C* 1° invio EDIVAT
070800010201     C                   reset                   dscmz
070900010201     C                   move      vlrpoi        cmzdst
071000140117     C                   eval      cmzfld = 'EDIVATWR'
071100010201     C                   eval      cmzmbd = vlrhdl
071200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
071300021009     C***                if        prmfir = *blanks
071400140117     C                   eval      cmzfla = 'EDIVAT0F'
071500140117     C                   eval      cmzmba = 'EDIVAT0F'
071600021009     C***                else
071700021009     C***                eval      cmzfla = prmfir
071800021009     C***                eval      cmzmba = prmfir
071900021009     C***                endif
072000010201     C                   eval      cmznrr = *zeros
072100020305     C                   move      §ctrokvt      cmznrr
072200021018     C                   eval      cmzlba = vlrfl1
072300010201     C                   call(e)   'TIS711C'
072400010201     C                   parm                    dscmz
072500010201     C                   parm      *blanks       esito
072600010205     C                   if        %error
072700010205     C                             or cmzerr = '1'
072800010205     C                             or esito  = '1'
072900010205     C                   eval      wrkesito = '3'
073000010205     C                   else
073100010201     C*
073200140117     C* 2° invio EDIVAB
073300010201     C                   reset                   dscmz
073400010201     C                   move      vlrpoi        cmzdst
073500010201     C                   eval      cmzfld = vlrfou
073600010201     C                   eval      cmzmbd = vlrhdl
073700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
073800021009     C***                if        prmfir = *blanks
073900140117     C                   eval      cmzfla = 'EDIVAB0F'
074000140117     C                   eval      cmzmba = 'EDIVAB0F'
074100021009     C***                else
074200021009     C***                eval      cmzfla = prmfir
074300021009     C***                eval      cmzmba = prmfir
074400021009     C***                endif
074500010201     C                   eval      cmznrr = *zeros
074600010201     C                   move      §ctrokvb      cmznrr
074700021018     C                   eval      cmzlba = vlrfl1
074800010201     C                   call(e)   'TIS711C'
074900010201     C                   parm                    dscmz
075000010201     C                   parm      *blanks       esito
075100010201     C                   if        %error
075200010201     C                             or cmzerr = '1'
075300010201     C                             or esito  = '1'
075400010201     C                   eval      wrkesito = '3'
075500010201     C                   endif
075600010205     C                   endif
075700990920     C*
075800000613     C                   ENDSR
075900000613     C***
076000070411
076100070411     C     *pssr         BEGSR
076200070411     C*
076300070411     C                   if        %open(tivin00r)
076400070411     C                   close     tivin00r
076500070411     C                   endif
076600140117     C                   if        %open(edivabwr)
076700140117     C                   close     edivabwr
076800070411     C                   endif
076900140117     C                   if        %open(edivatwr)
077000140117     C                   close     edivatwr
077100070411     C                   endif
077200070411     C*
077300070411     C* Effettuo la chiamata al CLLE preposto
077400140117     C                   call(e)   'TITVEVTC'
077500070411     C                   parm                    parccm
077600070411     C                   parm                    parmbr
077700070411     C                   parm      '2'           paropz
077800070411     C*
077900070411     C                   eval      wrkesito = '2'
078000070411     C*
078100070411     C                   seton                                        LR
078200070411     C*
078300070411     C                   ENDSR     '*CANCL'
078400070411     C***
078500070411
078600990910
078700000613     C     *inzsr        BEGSR
078800990910     C*
078900990910     C     *entry        plist
079000990920     C                   parm                    tivlrds
079100990921     C                   parm      wrkesito      esito
079200000724     C                   parm                    prmlit
079300000710     C                   parm                    prmfir
079400000613     C*
079500000830     C* CALCOLA LA DATA CORRENTE
079600140117     C                   time                    wn14             14 0
079700140117     C                   movel     wn14          oracor            6 0          *ORA
079800100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
079900100722     C                   eval      datcor = %dec(%date() : *ISO)
080000000830     C*
080100000613     C                   ENDSR
080200000613     C***
