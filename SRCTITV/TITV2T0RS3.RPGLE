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
002600000830
002700041025     D*------------------
002800041025     D* DS REPERIMENTO NUMERATORE
002900041025     D*------------------
003000041025     D trul33ds      e ds                  inz
003100041025     D*------------------
003200041025     D* DS ARCHITETTURA
003300041025     D*------------------
003400041025     D kpjba         e ds                  inz
003500041025     D*------------------
003600990908
003700010201
003800010201
003900000913     C                   reset                   rrnum
004000990921     C                   reset                   esito
004100990921     C                   reset                   wrkesito
004200000613     C*
004300150213     C                   EXSR      DEFCAM
004400150213     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004500070719     C*
004600070719     C* Esegue lancio TISI95R solo x chiusura
004700070719     C                   CLEAR                   TISI95DS
004800070719     C                   EVAL      I95TLA = 'C'
004900070719     C                   CALL      'TISI95R'
005000070719     C                   PARM                    TISI95DS
005100000613     C*
005200010202     C* Effettuo la chiamata al CLLE preposto
005300140922     C                   call(e)   'TITVEVTC'
005400140922     C                   parm                    parccm
005500140922     C                   parm                    parmbr
005600140922     C                   parm      '2'           paropz
005700000616     C*
005800010201     C                   seton                                        LR
005900990908
006000000801
006100910830     C*--------------------------------------------------------
006200140117     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
006300910830     C*--------------------------------------------------------
006400040526     C     RWFILE        BEGSR
006500990910     C*
006600990914     C                   if        not %open(tivin00r)
006700990908     C                   open      tivin00r
006800990914     C                   endif
006900140117     C                   if        not %open(edivabwr)
007000140117     C                   open      edivabwr
007100990914     C                   endif
007200140117     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
007300020305     C                   exsr      prevat
007400010201     C*
007500010202     C                   if        chkcall = '0'
007600010202     C*
007700140117     C                   if        not %open(edivatwr)
007800140117     C                   open      edivatwr
007900010201     C                   endif
008000990910     C*
008100010201     C                   clear                   §CTROKVB          5 0
008200020305     C                   clear                   §CTROKVT          5 0
008300000801     C                   clear                   §CTRMO            5 0
008400000801     C                   clear                   §CTRNO            5 0
008500100729     C*
008600100729     C                   z-add     1             wGiro             1 0
008700140117     C*
008800140902     C                   if        *in53
008900140117     C                   call      'TIS7P2SER'
009000140117     C                   parm      'O'           pIn_Opz           1
009100140117     C                   parm                    tivlrds
009200140117     C                   parm                    EDIVABDS
009300140117     C                   parm      *blanks       pIn_CdIdAz        2
009400140117     C                   parm      *blanks       pIn_MaskPDF      50
009500140117     C                   parm      *blanks       pOut_Esito        1
009600140902     C                   endif
009700130708     C*
009800130708     C                   SETOFF                                       31
009900921023     C                   DO        *HIVAL
010000990913     C*
010100990915     C                   READ      tivin00r                               70
010200040910     C                   if        vindta > *blanks
010300000613     C                   add       1             rrnum
010400000801     C*
010500000801     C                   if        *in70 = *off
010600000801     C                             and
010700000801     C                             (vinflg = *blanks
010800000801     C                              or vinflg = '0'
010900000801     C                              or vinflg = '2')
011000000801     C*
011100000801     C                   clear                   vinmsg
011200000801     C                   eval      vinflg = '1'
011300040910     C*
011400040910     C* Eseguo routine d traduzione
011500040910     C                   exsr      impvabvat
011600040802     C*
011700010305     C                   endif
011800000905     C*
011900000905     C                   else
012000000905     C                   eval      vinflg = '1'
012100000905     C                   endif
012200000905     C*
012300000905     C  N70              update    tivin000
012400000905     C*
012500991022     C  N70              ENDdo
012600100722     C*
012700100722     C* Scarico i buffer testata ancora "in canna"
012800140922     C*
012900140922     C  N31              exsr      CHKWRI
013000140117     C  N31              WRITE     EDIVAB00
013100130708     C                   SETOFF                                       31
013200140117     C*
013300140902     C                   if        *in53
013400140117     C                   call      'TIS7P2SER'
013500140117     C                   parm      'C'           pIn_Opz           1
013600140117     C                   parm                    tivlrds
013700140117     C                   parm                    EDIVABDS
013800140117     C                   parm      *blanks       pIn_CdIdAz        2
013900140117     C                   parm      *blanks       pIn_MaskPDF      50
014000140117     C                   parm      *blanks       pOut_Esito        1
014100140902     C                   endif
014200010202     C*
014300010202     C                   endif
014400990910
014500990910     C* Se non ci sono record con errori ...
014600000710     C                   if        §ctrno = 0
014700990910     C* ... restituisco esito OK.
014800990921     C                   eval      wrkesito = '0'
014900990910     C                   else
015000010201     C                   if        §ctrokvb > 0
015100990921     C                   eval      wrkesito = '1'
015200000710     C                   else
015300000710     C                   eval      wrkesito = '2'
015400990910     C                   endif
015500000710     C                   endif
015600990910     C*
015700990914     C                   if        %open(tivin00r)
015800990908     C                   close     tivin00r
015900990914     C                   endif
016000140117     C                   if        %open(edivabwr)
016100140117     C                   close     edivabwr
016200990914     C                   endif
016300140117     C                   if        %open(edivatwr)
016400140117     C                   close     edivatwr
016500010201     C                   endif
016600990910     C*
016700010201     C                   if        §ctrokvb > 0
016800000724     C                             and vlrpoi <> *zeros
016900010202     C                   exsr      invio
017000990920     C                   endif
017100990920     C*
017200910830     C                   ENDSR
017300000613     C***
017400140922
017500140922     C*----------------------------------------------------*
017600140922     C*  FORZATURE E CONSIDERAZIONI PRE-WRITE TESTATA
017700140922     C*----------------------------------------------------*
017800140922     C     CHKWRI        BEGSR
017900140922     C*
018000140922     C                   if        wRMN_2 > *zeros
018100140922     C                   eval      VABRMN = wRMN_2
018200140922     C                   eval      VABRMA = wRMA_2
018300140922     C                   else
018400140922     C                   eval      VABRMN = wRMN_1
018500140922     C                   eval      VABRMA = wRMA_1
018600140922     C                   endif
018700140922     C*
018800140922     C* Considerazioni sul contenuto di campi precedentemente valorizzati
018900140922     C                   IF        FlgCAS <> '0'
019000140922     C                   IF        VABCBO = '1'
019100140922     C                   EVAL      VABCBO = '4'
019200140922     C                   ELSE
019300140922     C                   EVAL      VABCBO = '6'
019400140922     C                   ENDIF
019500140922     C                   ENDIF
019600140922     C*
019700140922     C* Eseguo routine finale x considerazioni specifiche su importi/divise
019800140922     C                   EXSR      CHKIMPDIV
019900140922     C*
020000140922     C                   ENDSR
020100990920
020200000801     C*----------------------------------------------------*
020300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020400000801     C*----------------------------------------------------*
020500010201     C     INZVAR        BEGSR
020600000801     C*
020700040802     C                   Z-ADD     *zeros        Num5_0            5 0
020800040802     C                   MOVEL     '0'           FlgCAS            1
020900000801     C*
021000000801     C                   ENDSR
021100000801     C*----------------------------------------------------*
021200040910     C*  IMPOSTAZIONE CAMPI COSTANTI
021300000801     C*----------------------------------------------------*
021400000801     C     DEFCAM        BEGSR
021500000801     C*
021600140117     C                   CLEAR                   EDIVAB00
021700140117     C                   CLEAR                   EDIVAT00
021800020619     C* Imposto i valori di default...
021900140922     C                   Z-ADD     1662533       VABCCM
022000140922     C                   Z-ADD     1662533       VATCCM
022100140922     C                   Z-ADD     166           VABLNP
022200140922     C                   Z-ADD     166           VATLNP
022300140922     C                   Z-ADD     006           VABCTR
022400140117     C                   MOVEL     '7T'          VABCTM
022500040823     C                   MOVEL     '1'           VABCBO
022600020619     C* ... e poi verifico se sono stati passati come parametri
022700020619     C                   IF        vlrppt > *blanks
022800040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
022900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023000020619     C                   EXSR      CHKNUM
023100020619     C                   IF        PiInt=*on
023200020619     C                   Z-ADD     PiVal         VABCCM
023300020619     C                   Z-ADD     PiVal         VATCCM
023400020619     C                   ENDIF
023500040506     C                   ENDIF
023600040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
023700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023800020619     C                   EXSR      CHKNUM
023900020619     C                   IF        PiInt=*on
024000020619     C                   Z-ADD     PiVal         VABLNP
024100020619     C                   Z-ADD     PiVal         VATLNP
024200040506     C                   ENDIF
024300020619     C                   ENDIF
024400040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
024500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
024600020619     C                   EXSR      CHKNUM
024700020619     C                   IF        PiInt=*on
024800020619     C                   Z-ADD     PiVal         VABCTR
024900040506     C                   ENDIF
025000020619     C                   ENDIF
025100060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
025200060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
025300060202     C                   ENDIF
025400140902     C                   IF        %subst(vlrppt:16:1) = 'S'
025500140902     C                   SETON                                        53
025600140902     C                   ELSE
025700140902     C                   SETOFF                                       53
025800140902     C                   ENDIF
025900020619     C                   ENDIF
026000000801     C*
026100000801     C                   ENDSR
026200000801     C*----------------------------------------------------*
026300140117     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
026400000801     C*----------------------------------------------------*
026500040910     C     IMPVABVAT     BEGSR
026600040910     C*
026700040910     C* Traduzione relativa ai tipi record del file del cliente
026800040910     C*
026900071210     C*
027000071210     C***
027100140922     C* ...tipo record 'BGM' (info CMR)
027200140922     C                   IF        %trim(%subst(vindta:1:3)) = 'BGM'
027300140903     C                   MOVEL     *blanks       savCMR           35
027400140922     C                   EVAL      savCMR = %trim(%subst(vindta:17:15)) + ' '+
027500140922     C                                      %char(datcor) + ' ' + %char(oracor)
027600130620     C                   ENDIF
027700140922     C***
027800140922     C* ...tipo record 'CNI' (rottuta di codice spedizione)
027900140922     C                   IF        %trim(%subst(vindta:1:3)) = 'CNI' and
028000140923     C                             (wRMA_2 <> %subst(vindta:921:21) or
028100140926     C                              %subst(vindta:921:21) = *blanks or
028200140923     C                              wGiro = 1 or FlgCAS = '1')
028300100722     C*
028400100722     C* Se nn primo giro => scarico il buffer precedente
028500100722     C                   if        wGiro = 1
028600100722     C                   eval      wGiro = 2
028700100722     C                   else
028800140922     C*
028900140922     C  N31              exsr      CHKWRI
029000140117     C  N31              WRITE     EDIVAB00
029100130708     C                   SETOFF                                       31
029200100722     C                   endif
029300100722     C*
029400071210     C* Resetto indicatore d anomalia sul singolo record
029500071210     C                   eval      vinflg = '1'
029600071210     C* ......inizializzazioni iniziali e formati record file Bartolini
029700071210     C                   EXSR      INZVAR
029800071210     C                   EXSR      DEFCAM
029900071210     C*
030000071210     C                   Z-ADD     *zeros        errore            1 0
030100071210     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
030200071210     C                   MOVEL     datcor        VABAAS
030300071210     C                   MOVEL     datcor        VATAAS
030400071210     C                   MOVE      datcor        VABMGS
030500071210     C                   MOVE(P)   vlrpoi        VABFGS
030600071210     C                   MOVE(P)   vlrpoi        VATFGS
030700140903     C*
030800140903     C* Dai "fissi"
030900140903     C                   EVAL      VABCMR = savCMR
031000140903     C                   EVAL      VABDCM = datcor
031100140903     C                   EVAL      VABDTS = datcor
031200140903     C                   EVAL      VABHMS = oracor
031300140903     C                   EVAL      VABCNT = 1
031400140903     C*
031500071210     C* ......VABNSP/VATNSP
031600071210     C* NSP => Stacco un numeratore da AZNUM
031700071210     C                   clear                   TRUL33DS
031800071210     C                   eval      I33OPE = *zeros
031900071210     C                   eval      I33CNU = 302
032000071210     C                   eval      I33NUM = 1
032100071210     C                   movel     TRUL33DS      KPJBU
032200071210     C                   call      'TRUL33R'
032300071210     C                   parm                    KPJBA
032400071210     C                   movel     KPJBU         TRUL33DS
032500071210     C                   if        O33ERR = *zeros
032600071210     C                   z-add     O33NRF        VABNSP
032700071210     C                   z-add     O33NRF        VATNSP
032800071210     C                   else
032900071210     C                   Z-ADD     1             errore
033000071210     C                   EVAL      vinmsg = %trimr(vinmsg)
033100071210     C                             + ' ' + 'VABNSP VATNSP'
033200071210     C                   endif
033300140922     C*
033400140922     C                   ENDIF
033500140922     C***
033600140922     C* ...tipo record 'CNI' (info testata)
033700140922     C                   IF        %trim(%subst(vindta:1:3)) = 'CNI'
033800140922     C                   z-add     *zeros        wRMN_1           17 0
033900140923     C                   z-add     *zeros        wRMN_2           21 0
034000140922     C                   movel     *blanks       wRMA_1           17
034100140923     C                   movel     *blanks       wRMA_2           21
034200140922     C* ......VABRMN_1
034300140922     C                   EVAL      PiStr=%trim(%subst(vindta:9:35))
034400140922     C                   EXSR      CHKNUM
034500140922     C                   IF        PiInt=*on
034600140922     C                   Z-ADD     PiVal         wRMN_1
034700140922     C                   ELSE
034800140922     C                   ADD       1             errore
034900140922     C                   EVAL      vinmsg = %trimr(vinmsg)
035000140923     C                             + ' ' + 'VABRMN_1'
035100140922     C                   ENDIF
035200140922     C* ......VABRMA_1
035300140922     C                   EVAL      wRMA_1=%trim(%subst(vindta:710:17))
035400151112     C* ......VABNAS
035500141106     C                   EVAL      VABNAS=%trim(%subst(vindta:884:17))
035600140922     C* ......VABNCL
035700140922     C                   EVAL      PiStr=%trim(%subst(vindta:56:5))
035800140922     C                   EXSR      CHKNUM
035900140922     C                   IF        PiInt=*on
036000140922     C                   ADD       PiVal         VABNCL
036100140922     C                   ELSE
036200140922     C                   ADD       1             errore
036300140922     C                   EVAL      vinmsg = %trimr(vinmsg)
036400140922     C                             + ' ' + 'VABNCL'
036500140922     C                   ENDIF
036600140922     C* ......VABPKB
036700140922     C                   EVAL      PiStr=%trim(%subst(vindta:66:12))
036800140922     C                   EXSR      CHKNUM
036900140922     C                   IF        PiNum=*on
037000140922     C                   EVAL      PiVal = PiVAl / 100
037100140922     C                   ADD       PiVal         VABPKB
037200140922     C                   ELSE
037300140922     C                   ADD       1             errore
037400140922     C                   EVAL      vinmsg = %trimr(vinmsg)
037500140922     C                             + ' ' + 'VABPKB'
037600140922     C                   ENDIF
037700140922     C* ......VABVLB
037800140922     C                   EVAL      PiStr=%trim(%subst(vindta:78:12))
037900140922     C                   EXSR      CHKNUM
038000140922     C                   IF        PiNum=*on
038100140922     C                   EVAL      PiVal = PiVAl / 100
038200140922     C                   ADD       PiVal         VABVLB
038300140922     C                   ELSE
038400140922     C                   ADD       1             errore
038500140922     C                   EVAL      vinmsg = %trimr(vinmsg)
038600140922     C                             + ' ' + 'VABVLB'
038700140922     C                   ENDIF
038800140117     C* ......VABCAS
038900140922     C                   IF        %subst(vindta:145:12) <> *all'0'
039000140117     C                   EVAL      FlgCAS = '1'
039100140922     C                   EVAL      VABVCA = 'EUR'
039200140922     C                   EVAL      PiStr=%trim(%subst(vindta:145:12))
039300140117     C                   EXSR      CHKNUM
039400140117     C                   IF        PiNum=*on
039500140922     C                   EVAL      PiVal = PiVAl / 100
039600140922     C                   ADD       PiVal         VABCAS
039700140117     C                   ELSE
039800140117     C                   ADD       1             errore
039900140117     C                   EVAL      vinmsg = %trimr(vinmsg)
040000140117     C                             + ' ' + 'VABCAS'
040100140117     C                   ENDIF
040200140117     C                   ENDIF
040300140922     C* ......VABIAS
040400140922     C                   IF        %subst(vindta:157:12) <> *all'0'
040500140922     C                   EVAL      VABVAS = 'EUR'
040600140922     C                   EVAL      PiStr=%trim(%subst(vindta:157:12))
040700140922     C                   EXSR      CHKNUM
040800140922     C                   IF        PiNum=*on
040900140922     C                   EVAL      PiVal = PiVAl / 100
041000140922     C                   ADD       PiVal         VABIAS
041100140922     C                   ELSE
041200140922     C                   ADD       1             errore
041300140922     C                   EVAL      vinmsg = %trimr(vinmsg)
041400140922     C                             + ' ' + 'VABIAS'
041500140922     C                   ENDIF
041600140922     C                   ENDIF
041700140922     C* ......VABVMD
041800140922     C                   IF        %subst(vindta:169:12) <> *all'0'
041900140922     C                   EVAL      VABVAD = 'EUR'
042000140922     C                   EVAL      PiStr=%trim(%subst(vindta:169:12))
042100140922     C                   EXSR      CHKNUM
042200140922     C                   IF        PiNum=*on
042300140922     C                   EVAL      PiVal = PiVAl / 100
042400140922     C                   ADD       PiVal         VABVMD
042500140922     C                   ELSE
042600140922     C                   ADD       1             errore
042700140922     C                   EVAL      vinmsg = %trimr(vinmsg)
042800140922     C                             + ' ' + 'VABVMD'
042900140922     C                   ENDIF
043000140922     C                   ENDIF
043100140922     C* ......VABRMN_2
043200151027     C                   IF        %subst(vindta:922:20) <> *blanks
043300151027     C                   EVAL      PiStr=%trim(%subst(vindta:922:20))
043400140922     C                   EXSR      CHKNUM
043500140922     C                   IF        PiInt=*on
043600140922     C                   Z-ADD     PiVal         wRMN_2
043700140922     C                   ELSE
043800140922     C                   ADD       1             errore
043900140922     C                   EVAL      vinmsg = %trimr(vinmsg)
044000140923     C                             + ' ' + 'VABRMN_2'
044100140922     C                   ENDIF
044200140922     C                   ENDIF
044300140922     C* ......VABRMA_2
044400140923     C                   EVAL      wRMA_2=%trim(%subst(vindta:921:21))
044500140922     C                   if        FlgCAS = '1'
044600140922     C                   EVAL      wRMA_2 = *blanks
044700140922     C                   endif
044800141002     C* ......VABDCR / VABHCR
044900141009     C                   IF        %trim(%subst(vindta:142:2)) = '2'
045000141002     C                   IF        %trim(%subst(vindta:125:8)) <> *blanks AND
045100141002     C                             %trim(%subst(vindta:125:8)) <> *zeros
045200141002     C                   EVAL      PiStr=%trim(%subst(vindta:125:8))
045300141002     C                   EXSR      CHKNUM
045400141002     C                   IF        PiInt=*on
045500141002     C                   Z-ADD     PiVal         VABDCR
045600141002     C                   ELSE
045700141002     C                   ADD       1             errore
045800141002     C                   EVAL      vinmsg = %trimr(vinmsg)
045900141002     C                             + ' ' + 'VABDCR'
046000141002     C                   ENDIF
046100141002     C                   EVAL      PiStr=%trim(%subst(vindta:133:4))
046200141002     C                   EXSR      CHKNUM
046300141002     C                   IF        PiInt=*on
046400141002     C                   Z-ADD     PiVal         VABHCR
046500141002     C                   ELSE
046600141002     C                   ADD       1             errore
046700141002     C                   EVAL      vinmsg = %trimr(vinmsg)
046800141002     C                             + ' ' + 'VABHCR'
046900141002     C                   ENDIF
047000141002     C                   ENDIF
047100141002     C                   ENDIF
047200141009     C* ......VABDCR / VABHCR
047300141009     C                   IF        %trim(%subst(vindta:142:2)) = '17'
047400141009     C                   IF        %trim(%subst(vindta:125:8)) <> *blanks AND
047500141009     C                             %trim(%subst(vindta:125:8)) <> *zeros
047600141009     C                   EVAL      VABTCR = 'P'
047700141009     C                   EVAL      PiStr=%trim(%subst(vindta:125:8))
047800141009     C                   EXSR      CHKNUM
047900141009     C                   IF        PiInt=*on
048000141009     C                   Z-ADD     PiVal         VABDCR
048100141009     C                   ELSE
048200141009     C                   ADD       1             errore
048300141009     C                   EVAL      vinmsg = %trimr(vinmsg)
048400141009     C                             + ' ' + 'VABDCR'
048500141009     C                   ENDIF
048600141009     C                   EVAL      PiStr=%trim(%subst(vindta:133:4))
048700141009     C                   EXSR      CHKNUM
048800141009     C                   IF        PiInt=*on
048900141009     C                   Z-ADD     PiVal         VABHCR
049000141009     C                   ELSE
049100141009     C                   ADD       1             errore
049200141009     C                   EVAL      vinmsg = %trimr(vinmsg)
049300141009     C                             + ' ' + 'VABHCR'
049400141009     C                   ENDIF
049500141009     C                   ENDIF
049600141009     C                   ENDIF
049700140922     C* ......VATNOT_P
049800140922     C                   if        *in53
049900140922     C*
050000140922     C* Solo se valorizzato RMN
050100140922     C                   if        wRMA_1 <> *blanks
050200140923     C                   eval      pIn_MaskPDF =
050300140923     C                                     %trim(%subst(vindta:1292:100))
050400140922     C                   call      'TIS7P2SER'
050500140922     C                   parm      'W'           pIn_Opz           1
050600140922     C                   parm                    tivlrds
050700140922     C                   parm                    EDIVABDS
050800140922     C                   parm      '10'          pIn_CdIdAz        2
050900140922     C                   parm                    pIn_MaskPDF      50
051000140922     C                   parm      *blanks       pOut_Esito        1
051100140922     C                   endif
051200140922     C                   endif
051300140117     C*
051400140117     C                   ENDIF
051500170112     C***
051600170112     C* ...tipo record 'NA2/SF' (info testata - mittente)
051700170112     C                   IF        %trim(%subst(vindta:1:3)) = 'NA2'
051800170112     C* Solo sub-types 'SF'
051900170112     C                   IF        %trim(%subst(vindta:4:2)) = 'SF'
052000170112     C* ......VABRMO
052100170112     C                   EVAL      VABRMO=%trim(%subst(vindta:42:35))
052200170112     C* ......VABCMO
052300170112     C                   EVAL      VABCMO=%trim(%subst(vindta:226:17))
052400170112     C* ......VABNMO
052500170112     C                   EVAL      VABNMO=%trim(%subst(vindta:243:3))
052600170112     C                   IF        VABNMO='IT'
052700170112     C                   EVAL      VABNMO=*blanks
052800170112     C                   ENDIF
052900170112     C* ......VABCCM
053000170112     C                   IF        %trim(%subst(vindta:7:35)) = '40054'
053100170112     C                   EVAL      VABCCM = 1663621
053200170112     C                   EVAL      VATCCM = 1663621
053300170112     C* ......VATNOT_S
053400170112     C                   EVAL      VATNOT=%trim(%subst(vindta:426:20))
053500170112     C                   IF        VATNOT <> *blanks
053600170112     C                   EVAL      VATCMR = VABCMR
053700170112     C                   EVAL      VATCNT = VABCNT
053800170112     C                   EVAL      VATTRC = 'S'
053900170112     C                   ADD       1             §CTROKVT
054000170112     C                   WRITE     EDIVAT00
054100170112     C                   ENDIF
054200170112     C* ......VATNOT_I
054300170112     C                   EVAL      VATNOT=%trim(%subst(vindta:466:35))
054400170112     C                   IF        VATNOT <> *blanks
054500170112     C                   EVAL      VATCMR = VABCMR
054600170112     C                   EVAL      VATCNT = VABCNT
054700170112     C                   EVAL      VATTRC = 'I'
054800170112     C                   ADD       1             §CTROKVT
054900170112     C                   WRITE     EDIVAT00
055000170112     C                   ENDIF
055100170112     C* ......VATNOT_J
055200170112     C                   EVAL      VATNOT=%trim(%subst(vindta:466+35:70-35))
055300170112     C                   IF        VATNOT <> *blanks
055400170112     C                   EVAL      VATCMR = VABCMR
055500170112     C                   EVAL      VATCNT = VABCNT
055600170112     C                   EVAL      VATTRC = 'J'
055700170112     C                   ADD       1             §CTROKVT
055800170112     C                   WRITE     EDIVAT00
055900170112     C                   ENDIF
056000170112     C                   ENDIF
056100170112     C*
056200170112     C                   ENDIF
056300170112     C*
056400170112     C                   ENDIF
056500140117     C***
056600140922     C* ...tipo record 'NA2/ST' (info testata - destinatario)
056700140922     C                   IF        %trim(%subst(vindta:1:3)) = 'NA2'
056800140922     C* Solo sub-types 'ST'
056900140922     C                   IF        %trim(%subst(vindta:4:2)) = 'ST'
057000060202     C* ......VABRSD
057100140922     C                   EVAL      VABRSD=%trim(%subst(vindta:42:35))
057200130620     C* ......VABRD2
057300140922     C                   EVAL      VABRD2=%trim(%subst(vindta:77:35))
057400100714     C* ......VABIND
057500140922     C                   EVAL      VABIND=%trim(%subst(vindta:112:35))
057600130620     C* ......VABCAD
057700140922     C                   EVAL      VABCAD=%trim(%subst(vindta:226:17))
057800060202     C* ......VABLOD
057900140922     C                   EVAL      VABLOD=%trim(%subst(vindta:182:35))
058000140922     C* ......VABPRD
058100140922     C                   EVAL      VABPRD=%trim(%subst(vindta:217:9))
058200140922     C* ......VABNZD
058300140922     C                   EVAL      VABNZD=%trim(%subst(vindta:243:3))
058400130620     C                   IF        VABNZD='IT'
058500130620     C                   EVAL      VABNZD=*blanks
058600130620     C                   ENDIF
058700060202     C* ......VABPRD
058800070719     C* Reperisco la provincia dal CAP e dalla località
058900070719     C                   IF        VABPRD  = *blanks AND
059000130620     C                             VABLOD <> *blanks AND
059100130620     C                             VABCAD <> *blanks AND
059200130620     C                             VABNZD  = *blanks
059300070719     C                   CLEAR                   TISI95DS
059400070719     C                   EVAL      I95TCN = '3'
059500070719     C                   Z-ADD     datcor        I95DAT
059600070719     C                   EVAL      I95CAP = VABCAD
059700070719     C                   EVAL      I95LOC = VABLOD
059800070719     C                   CALL      'TISI95R'
059900070719     C                   PARM                    TISI95DS
060000070719     C                   EVAL      VABPRD = O95PRV
060100070719     C                   ENDIF
060200140922     C* ......VATNOT_A
060300140922     C                   EVAL      VATNOT=%trim(%subst(vindta:371:35))
060400140922     C                   IF        VATNOT <> *blanks
060500140922     C                   EVAL      VATCMR = VABCMR
060600140922     C                   EVAL      VATCNT = VABCNT
060700140922     C                   EVAL      VATTRC = 'A'
060800140922     C                   ADD       1             §CTROKVT
060900140922     C                   WRITE     EDIVAT00
061000140922     C                   ENDIF
061100140922     C* ......VATNOT_B
061200140922     C                   EVAL      VATNOT=%trim(%subst(vindta:406:20))
061300140922     C                   IF        VATNOT <> *blanks
061400140922     C                   EVAL      VATCMR = VABCMR
061500140922     C                   EVAL      VATCNT = VABCNT
061600140922     C                   EVAL      VATTRC = 'B'
061700140922     C                   ADD       1             §CTROKVT
061800140922     C                   WRITE     EDIVAT00
061900140922     C                   ENDIF
062000150216     C* ......VABNOT
062100150216     C                   EVAL      VABNOT=%trim(%subst(vindta:553:35))
062200150216     C* ......VABNT2
062300150216     C                   EVAL      VABNOT=%trim(%subst(vindta:553+35:35))
062400140117     C*
062500140117     C                   ENDIF
062600140117     C*
062700140117     C                   ENDIF
062800140922     C***
062900140922     C* ...tipo record 'GID' (dettaglio segnacolli)
063000140922     C                   IF        %trim(%subst(vindta:1:3)) = 'GID'
063100140922     C* ......VATNOT_E
063200140922     C                   EVAL      VATNOT=%trim(%subst(vindta:118:35))
063300140922     C                   IF        VATNOT <> *blanks
063400140922     C                   EVAL      VATCMR = VABCMR
063500140922     C                   EVAL      VATCNT = VABCNT
063600140922     C                   EVAL      VATTRC = 'E'
063700140922     C                   ADD       1             §CTROKVT
063800140922     C                   WRITE     EDIVAT00
063900140922     C                   ENDIF
064000140922     C*
064100140922     C                   ENDIF
064200010202     C*
064300000801     C* Ebbene...
064400000801     C                   ADD       1             §CTRMO
064500010201     C                   IF        errore <> *zeros
064600000801     C                   ADD       1             §CTRNO
064700000801     C                   EVAL      vinflg = '2'
064800000801     C                   ELSE
064900010201     C                   ADD       1             §CTROKVB
065000000801     C                   ENDIF
065100000801     C*
065200000801     C                   ENDSR
065300010202     C*----------------------------------------------------*
065400140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
065500010202     C*----------------------------------------------------*
065600020305     C     PREVAT        BEGSR
065700010202     C*
065800140117     C* Compongo il nome del membro da dare al EDIVATWR
065900010202     C                   eval      parmbr = vlrhdl
066000010202     C                   movel     'M'           parmbr
066100060113     C                   eval      parccm = vlrksc
066200010202     C                   eval      paropz = '1'
066300010202     C* Effettuo la chiamata al CLLE preposto
066400140117     C                   call(e)   'TITVEVTC'
066500010202     C                   parm                    parccm
066600010202     C                   parm                    parmbr
066700010202     C                   parm                    paropz
066800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
066900010202     C                   if        %error
067000010202     C                   movel     '1'           chkcall
067100010202     C                   else
067200010202     C                   movel     '0'           chkcall
067300010202     C                   endif
067400010202     C*
067500010202     C                   ENDSR
067600000801     C*----------------------------------------------------*
067700000801     C*  CONTROLLO NUMERICITA' CAMPI
067800000801     C*----------------------------------------------------*
067900000801     C     CHKNUM        BEGSR
068000000801     C*
068100000801     C                   call(e)   'ISNUMERIC'
068200000801     C                   PARM                    PiStr            30
068300040714     C                   PARM      ','           PiDecChr          1
068400000801     C                   PARM      *ZEROS        PiVal            30 9
068500000801     C                   PARM      '0'           PiInt             1
068600000801     C                   PARM      '0'           PiNum             1
068700000801     C                   IF        %error
068800000801     C                   EVAL      PiInt=*off
068900000801     C                   ENDIF
069000000801     C*
069100000801     C                   ENDSR
069200000801     C***
069300000801
069400011113
069500011113     C*----------------------------------------------------*
069600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
069700011113     C*----------------------------------------------------*
069800011113     C     CHKIMPDIV     BEGSR
069900011113     C*
070000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
070100011113     C                   Z-ADD     *zeros        wrkDec            9 9
070200011113     C*
070300011113     C* Come prima cosa effettuo considerazioni sulla divisa
070400011113     C                   IF        vabIAS > *zeros
070500011113     C                   IF        vabVAS <> 'EUR'
070600011113     C                   EVAL      vabVAS =  'ITL'
070700011113     C                   ENDIF
070800011113     C                   ENDIF
070900011113     C*
071000011113     C                   IF        vabCAS > *zeros
071100011113     C                   IF        vabVCA <> 'EUR'
071200011113     C                   EVAL      vabVCA =  'ITL'
071300011113     C                   ENDIF
071400011113     C                   ENDIF
071500011113     C*
071600011113     C                   IF        vabVMD > *zeros
071700020305     C                   IF        vabVAD <> 'EUR'
071800011113     C                   EVAL      vabVAD =  'ITL'
071900011113     C                   ENDIF
072000011113     C                   ENDIF
072100011113     C*
072200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
072300011113     C                   Z-ADD     vabIAS        wrkDec
072400011113     C                   IF        wrkDec > *zeros
072500011113     C                   IF        vabVAS = 'ITL'
072600011113     C                   EVAL      vabIAS = *zeros
072700011113     C                   ENDIF
072800011113     C                   ENDIF
072900011113     C*
073000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
073100011113     C                   Z-ADD     vabCAS        wrkDec
073200011113     C                   IF        wrkDec > *zeros
073300011113     C                   IF        vabVCA = 'ITL'
073400011113     C                   EVAL      vabCAS = *zeros
073500011113     C                   ENDIF
073600011113     C                   ENDIF
073700011113     C*
073800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
073900011113     C                   Z-ADD     vabVMD        wrkDec
074000011113     C                   IF        wrkDec > *zeros
074100011113     C                   IF        vabVAD = 'ITL'
074200011113     C                   EVAL      vabVMD = *zeros
074300011113     C                   ENDIF
074400011113     C                   ENDIF
074500011113     C*
074600011113     C                   ENDSR
074700011113     C***
074800011113
074900011113
075000000801
075100000801
075200990920      /TITLE Invio dei dati al punto operativo.
075300010202     C     invio         BEGSR
075400990920     C*
075500140117     C* 1° invio EDIVAT
075600010201     C                   reset                   dscmz
075700010201     C                   move      vlrpoi        cmzdst
075800140117     C                   eval      cmzfld = 'EDIVATWR'
075900010201     C                   eval      cmzmbd = vlrhdl
076000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
076100021009     C***                if        prmfir = *blanks
076200140117     C                   eval      cmzfla = 'EDIVAT0F'
076300140117     C                   eval      cmzmba = 'EDIVAT0F'
076400021009     C***                else
076500021009     C***                eval      cmzfla = prmfir
076600021009     C***                eval      cmzmba = prmfir
076700021009     C***                endif
076800010201     C                   eval      cmznrr = *zeros
076900020305     C                   move      §ctrokvt      cmznrr
077000021018     C                   eval      cmzlba = vlrfl1
077100010201     C                   call(e)   'TIS711C'
077200010201     C                   parm                    dscmz
077300010201     C                   parm      *blanks       esito
077400010205     C                   if        %error
077500010205     C                             or cmzerr = '1'
077600010205     C                             or esito  = '1'
077700010205     C                   eval      wrkesito = '3'
077800010205     C                   else
077900010201     C*
078000140117     C* 2° invio EDIVAB
078100010201     C                   reset                   dscmz
078200010201     C                   move      vlrpoi        cmzdst
078300010201     C                   eval      cmzfld = vlrfou
078400010201     C                   eval      cmzmbd = vlrhdl
078500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
078600021009     C***                if        prmfir = *blanks
078700140117     C                   eval      cmzfla = 'EDIVAB0F'
078800140117     C                   eval      cmzmba = 'EDIVAB0F'
078900021009     C***                else
079000021009     C***                eval      cmzfla = prmfir
079100021009     C***                eval      cmzmba = prmfir
079200021009     C***                endif
079300010201     C                   eval      cmznrr = *zeros
079400010201     C                   move      §ctrokvb      cmznrr
079500021018     C                   eval      cmzlba = vlrfl1
079600010201     C                   call(e)   'TIS711C'
079700010201     C                   parm                    dscmz
079800010201     C                   parm      *blanks       esito
079900010201     C                   if        %error
080000010201     C                             or cmzerr = '1'
080100010201     C                             or esito  = '1'
080200010201     C                   eval      wrkesito = '3'
080300010201     C                   endif
080400010205     C                   endif
080500990920     C*
080600000613     C                   ENDSR
080700000613     C***
080800070411
080900070411     C     *pssr         BEGSR
081000070411     C*
081100070411     C                   if        %open(tivin00r)
081200070411     C                   close     tivin00r
081300070411     C                   endif
081400140117     C                   if        %open(edivabwr)
081500140117     C                   close     edivabwr
081600070411     C                   endif
081700140117     C                   if        %open(edivatwr)
081800140117     C                   close     edivatwr
081900070411     C                   endif
082000070411     C*
082100070411     C* Effettuo la chiamata al CLLE preposto
082200140117     C                   call(e)   'TITVEVTC'
082300070411     C                   parm                    parccm
082400070411     C                   parm                    parmbr
082500070411     C                   parm      '2'           paropz
082600070411     C*
082700070411     C                   eval      wrkesito = '2'
082800070411     C*
082900070411     C                   seton                                        LR
083000070411     C*
083100070411     C                   ENDSR     '*CANCL'
083200070411     C***
083300070411
083400990910
083500000613     C     *inzsr        BEGSR
083600990910     C*
083700990910     C     *entry        plist
083800990920     C                   parm                    tivlrds
083900990921     C                   parm      wrkesito      esito
084000000724     C                   parm                    prmlit
084100000710     C                   parm                    prmfir
084200000613     C*
084300000830     C* CALCOLA LA DATA CORRENTE
084400140117     C                   time                    wn14             14 0
084500140117     C                   movel     wn14          oracor            6 0          *ORA
084600100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
084700100722     C                   eval      datcor = %dec(%date() : *ISO)
084800000830     C*
084900000613     C                   ENDSR
085000000613     C***
