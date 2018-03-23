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
021900170224     C                   Z-ADD     1662533       VABCCM
022000170224     C                   Z-ADD     1662533       VATCCM
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
027700170224     C***
027800170224     C* ...tipo record 'NAD' (info cliente)
027900170224     C                   IF        %trim(%subst(vindta:1:3)) = 'NAD'
028000170224     C                   Z-ADD     *zeros        savCCM            7 0
028100170224     C                   IF        %trim(%subst(vindta:491:35)) = '40054'
028200170224     C                   EVAL      savCCM = 1663621
028300170224     C                   ENDIF
028400170224     C                   ENDIF
028500140922     C***
028600140922     C* ...tipo record 'CNI' (rottuta di codice spedizione)
028700140922     C                   IF        %trim(%subst(vindta:1:3)) = 'CNI' and
028800140923     C                             (wRMA_2 <> %subst(vindta:921:21) or
028900140926     C                              %subst(vindta:921:21) = *blanks or
029000140923     C                              wGiro = 1 or FlgCAS = '1')
029100100722     C*
029200100722     C* Se nn primo giro => scarico il buffer precedente
029300100722     C                   if        wGiro = 1
029400100722     C                   eval      wGiro = 2
029500100722     C                   else
029600140922     C*
029700140922     C  N31              exsr      CHKWRI
029800140117     C  N31              WRITE     EDIVAB00
029900130708     C                   SETOFF                                       31
030000100722     C                   endif
030100100722     C*
030200071210     C* Resetto indicatore d anomalia sul singolo record
030300071210     C                   eval      vinflg = '1'
030400071210     C* ......inizializzazioni iniziali e formati record file Bartolini
030500071210     C                   EXSR      INZVAR
030600071210     C                   EXSR      DEFCAM
030700170224     C*
030800170224     C                   IF        savCCM > *zeros
030900170224     C                   EVAL      VABCCM = savCCM
031000170224     C                   EVAL      VATCCM = savCCM
031100170224     C                   ENDIF
031200071210     C*
031300071210     C                   Z-ADD     *zeros        errore            1 0
031400071210     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
031500071210     C                   MOVEL     datcor        VABAAS
031600071210     C                   MOVEL     datcor        VATAAS
031700071210     C                   MOVE      datcor        VABMGS
031800071210     C                   MOVE(P)   vlrpoi        VABFGS
031900071210     C                   MOVE(P)   vlrpoi        VATFGS
032000140903     C*
032100140903     C* Dai "fissi"
032200140903     C                   EVAL      VABCMR = savCMR
032300140903     C                   EVAL      VABDCM = datcor
032400140903     C                   EVAL      VABDTS = datcor
032500140903     C                   EVAL      VABHMS = oracor
032600140903     C                   EVAL      VABCNT = 1
032700140903     C*
032800071210     C* ......VABNSP/VATNSP
032900071210     C* NSP => Stacco un numeratore da AZNUM
033000071210     C                   clear                   TRUL33DS
033100071210     C                   eval      I33OPE = *zeros
033200071210     C                   eval      I33CNU = 302
033300071210     C                   eval      I33NUM = 1
033400071210     C                   movel     TRUL33DS      KPJBU
033500071210     C                   call      'TRUL33R'
033600071210     C                   parm                    KPJBA
033700071210     C                   movel     KPJBU         TRUL33DS
033800071210     C                   if        O33ERR = *zeros
033900071210     C                   z-add     O33NRF        VABNSP
034000071210     C                   z-add     O33NRF        VATNSP
034100071210     C                   else
034200071210     C                   Z-ADD     1             errore
034300071210     C                   EVAL      vinmsg = %trimr(vinmsg)
034400071210     C                             + ' ' + 'VABNSP VATNSP'
034500071210     C                   endif
034600140922     C*
034700140922     C                   ENDIF
034800140922     C***
034900140922     C* ...tipo record 'CNI' (info testata)
035000140922     C                   IF        %trim(%subst(vindta:1:3)) = 'CNI'
035100140922     C                   z-add     *zeros        wRMN_1           17 0
035200140923     C                   z-add     *zeros        wRMN_2           21 0
035300140922     C                   movel     *blanks       wRMA_1           17
035400140923     C                   movel     *blanks       wRMA_2           21
035500140922     C* ......VABRMN_1
035600140922     C                   EVAL      PiStr=%trim(%subst(vindta:9:35))
035700140922     C                   EXSR      CHKNUM
035800140922     C                   IF        PiInt=*on
035900140922     C                   Z-ADD     PiVal         wRMN_1
036000140922     C                   ELSE
036100140922     C                   ADD       1             errore
036200140922     C                   EVAL      vinmsg = %trimr(vinmsg)
036300140923     C                             + ' ' + 'VABRMN_1'
036400140922     C                   ENDIF
036500140922     C* ......VABRMA_1
036600140922     C                   EVAL      wRMA_1=%trim(%subst(vindta:710:17))
036700151112     C* ......VABNAS
036800141106     C                   EVAL      VABNAS=%trim(%subst(vindta:884:17))
036900140922     C* ......VABNCL
037000140922     C                   EVAL      PiStr=%trim(%subst(vindta:56:5))
037100140922     C                   EXSR      CHKNUM
037200140922     C                   IF        PiInt=*on
037300140922     C                   ADD       PiVal         VABNCL
037400140922     C                   ELSE
037500140922     C                   ADD       1             errore
037600140922     C                   EVAL      vinmsg = %trimr(vinmsg)
037700140922     C                             + ' ' + 'VABNCL'
037800140922     C                   ENDIF
037900140922     C* ......VABPKB
038000140922     C                   EVAL      PiStr=%trim(%subst(vindta:66:12))
038100140922     C                   EXSR      CHKNUM
038200140922     C                   IF        PiNum=*on
038300140922     C                   EVAL      PiVal = PiVAl / 100
038400140922     C                   ADD       PiVal         VABPKB
038500140922     C                   ELSE
038600140922     C                   ADD       1             errore
038700140922     C                   EVAL      vinmsg = %trimr(vinmsg)
038800140922     C                             + ' ' + 'VABPKB'
038900140922     C                   ENDIF
039000140922     C* ......VABVLB
039100140922     C                   EVAL      PiStr=%trim(%subst(vindta:78:12))
039200140922     C                   EXSR      CHKNUM
039300140922     C                   IF        PiNum=*on
039400140922     C                   EVAL      PiVal = PiVAl / 100
039500140922     C                   ADD       PiVal         VABVLB
039600140922     C                   ELSE
039700140922     C                   ADD       1             errore
039800140922     C                   EVAL      vinmsg = %trimr(vinmsg)
039900140922     C                             + ' ' + 'VABVLB'
040000140922     C                   ENDIF
040100140117     C* ......VABCAS
040200140922     C                   IF        %subst(vindta:145:12) <> *all'0'
040300140117     C                   EVAL      FlgCAS = '1'
040400140922     C                   EVAL      VABVCA = 'EUR'
040500140922     C                   EVAL      PiStr=%trim(%subst(vindta:145:12))
040600140117     C                   EXSR      CHKNUM
040700140117     C                   IF        PiNum=*on
040800140922     C                   EVAL      PiVal = PiVAl / 100
040900140922     C                   ADD       PiVal         VABCAS
041000140117     C                   ELSE
041100140117     C                   ADD       1             errore
041200140117     C                   EVAL      vinmsg = %trimr(vinmsg)
041300140117     C                             + ' ' + 'VABCAS'
041400140117     C                   ENDIF
041500140117     C                   ENDIF
041600140922     C* ......VABIAS
041700140922     C                   IF        %subst(vindta:157:12) <> *all'0'
041800140922     C                   EVAL      VABVAS = 'EUR'
041900140922     C                   EVAL      PiStr=%trim(%subst(vindta:157:12))
042000140922     C                   EXSR      CHKNUM
042100140922     C                   IF        PiNum=*on
042200140922     C                   EVAL      PiVal = PiVAl / 100
042300140922     C                   ADD       PiVal         VABIAS
042400140922     C                   ELSE
042500140922     C                   ADD       1             errore
042600140922     C                   EVAL      vinmsg = %trimr(vinmsg)
042700140922     C                             + ' ' + 'VABIAS'
042800140922     C                   ENDIF
042900140922     C                   ENDIF
043000140922     C* ......VABVMD
043100140922     C                   IF        %subst(vindta:169:12) <> *all'0'
043200140922     C                   EVAL      VABVAD = 'EUR'
043300140922     C                   EVAL      PiStr=%trim(%subst(vindta:169:12))
043400140922     C                   EXSR      CHKNUM
043500140922     C                   IF        PiNum=*on
043600140922     C                   EVAL      PiVal = PiVAl / 100
043700140922     C                   ADD       PiVal         VABVMD
043800140922     C                   ELSE
043900140922     C                   ADD       1             errore
044000140922     C                   EVAL      vinmsg = %trimr(vinmsg)
044100140922     C                             + ' ' + 'VABVMD'
044200140922     C                   ENDIF
044300140922     C                   ENDIF
044400140922     C* ......VABRMN_2
044500151027     C                   IF        %subst(vindta:922:20) <> *blanks
044600151027     C                   EVAL      PiStr=%trim(%subst(vindta:922:20))
044700140922     C                   EXSR      CHKNUM
044800140922     C                   IF        PiInt=*on
044900140922     C                   Z-ADD     PiVal         wRMN_2
045000140922     C                   ELSE
045100140922     C                   ADD       1             errore
045200140922     C                   EVAL      vinmsg = %trimr(vinmsg)
045300140923     C                             + ' ' + 'VABRMN_2'
045400140922     C                   ENDIF
045500140922     C                   ENDIF
045600140922     C* ......VABRMA_2
045700140923     C                   EVAL      wRMA_2=%trim(%subst(vindta:921:21))
045800140922     C                   if        FlgCAS = '1'
045900140922     C                   EVAL      wRMA_2 = *blanks
046000140922     C                   endif
046100141002     C* ......VABDCR / VABHCR
046200141009     C                   IF        %trim(%subst(vindta:142:2)) = '2'
046300141002     C                   IF        %trim(%subst(vindta:125:8)) <> *blanks AND
046400141002     C                             %trim(%subst(vindta:125:8)) <> *zeros
046500141002     C                   EVAL      PiStr=%trim(%subst(vindta:125:8))
046600141002     C                   EXSR      CHKNUM
046700141002     C                   IF        PiInt=*on
046800141002     C                   Z-ADD     PiVal         VABDCR
046900141002     C                   ELSE
047000141002     C                   ADD       1             errore
047100141002     C                   EVAL      vinmsg = %trimr(vinmsg)
047200141002     C                             + ' ' + 'VABDCR'
047300141002     C                   ENDIF
047400141002     C                   EVAL      PiStr=%trim(%subst(vindta:133:4))
047500141002     C                   EXSR      CHKNUM
047600141002     C                   IF        PiInt=*on
047700141002     C                   Z-ADD     PiVal         VABHCR
047800141002     C                   ELSE
047900141002     C                   ADD       1             errore
048000141002     C                   EVAL      vinmsg = %trimr(vinmsg)
048100141002     C                             + ' ' + 'VABHCR'
048200141002     C                   ENDIF
048300141002     C                   ENDIF
048400141002     C                   ENDIF
048500141009     C* ......VABDCR / VABHCR
048600141009     C                   IF        %trim(%subst(vindta:142:2)) = '17'
048700141009     C                   IF        %trim(%subst(vindta:125:8)) <> *blanks AND
048800141009     C                             %trim(%subst(vindta:125:8)) <> *zeros
048900141009     C                   EVAL      VABTCR = 'P'
049000141009     C                   EVAL      PiStr=%trim(%subst(vindta:125:8))
049100141009     C                   EXSR      CHKNUM
049200141009     C                   IF        PiInt=*on
049300141009     C                   Z-ADD     PiVal         VABDCR
049400141009     C                   ELSE
049500141009     C                   ADD       1             errore
049600141009     C                   EVAL      vinmsg = %trimr(vinmsg)
049700141009     C                             + ' ' + 'VABDCR'
049800141009     C                   ENDIF
049900141009     C                   EVAL      PiStr=%trim(%subst(vindta:133:4))
050000141009     C                   EXSR      CHKNUM
050100141009     C                   IF        PiInt=*on
050200141009     C                   Z-ADD     PiVal         VABHCR
050300141009     C                   ELSE
050400141009     C                   ADD       1             errore
050500141009     C                   EVAL      vinmsg = %trimr(vinmsg)
050600141009     C                             + ' ' + 'VABHCR'
050700141009     C                   ENDIF
050800141009     C                   ENDIF
050900141009     C                   ENDIF
051000140922     C* ......VATNOT_P
051100140922     C                   if        *in53
051200140922     C*
051300140922     C* Solo se valorizzato RMN
051400140922     C                   if        wRMA_1 <> *blanks
051500140923     C                   eval      pIn_MaskPDF =
051600140923     C                                     %trim(%subst(vindta:1292:100))
051700140922     C                   call      'TIS7P2SER'
051800140922     C                   parm      'W'           pIn_Opz           1
051900140922     C                   parm                    tivlrds
052000140922     C                   parm                    EDIVABDS
052100140922     C                   parm      '10'          pIn_CdIdAz        2
052200140922     C                   parm                    pIn_MaskPDF      50
052300140922     C                   parm      *blanks       pOut_Esito        1
052400140922     C                   endif
052500140922     C                   endif
052600140117     C*
052700140117     C                   ENDIF
052800170112     C***
052900170112     C* ...tipo record 'NA2/SF' (info testata - mittente)
053000170112     C                   IF        %trim(%subst(vindta:1:3)) = 'NA2'
053100170112     C* Solo sub-types 'SF'
053200170112     C                   IF        %trim(%subst(vindta:4:2)) = 'SF'
053300170112     C* ......VABRMO
053400170112     C                   EVAL      VABRMO=%trim(%subst(vindta:42:35))
053500170112     C* ......VABCMO
053600170112     C                   EVAL      VABCMO=%trim(%subst(vindta:226:17))
053700170112     C* ......VABNMO
053800170112     C                   EVAL      VABNMO=%trim(%subst(vindta:243:3))
053900170112     C                   IF        VABNMO='IT'
054000170112     C                   EVAL      VABNMO=*blanks
054100170112     C                   ENDIF
054200170224     C*
054300170224     C                   IF        VABCCM = 1663621
054400170112     C* ......VATNOT_S
054500170112     C                   EVAL      VATNOT=%trim(%subst(vindta:426:20))
054600170112     C                   IF        VATNOT <> *blanks
054700170112     C                   EVAL      VATCMR = VABCMR
054800170112     C                   EVAL      VATCNT = VABCNT
054900170112     C                   EVAL      VATTRC = 'S'
055000170112     C                   ADD       1             §CTROKVT
055100170112     C                   WRITE     EDIVAT00
055200170112     C                   ENDIF
055300170112     C* ......VATNOT_I
055400170112     C                   EVAL      VATNOT=%trim(%subst(vindta:466:35))
055500170112     C                   IF        VATNOT <> *blanks
055600170112     C                   EVAL      VATCMR = VABCMR
055700170112     C                   EVAL      VATCNT = VABCNT
055800170112     C                   EVAL      VATTRC = 'I'
055900170112     C                   ADD       1             §CTROKVT
056000170112     C                   WRITE     EDIVAT00
056100170112     C                   ENDIF
056200170112     C* ......VATNOT_J
056300170112     C                   EVAL      VATNOT=%trim(%subst(vindta:466+35:70-35))
056400170112     C                   IF        VATNOT <> *blanks
056500170112     C                   EVAL      VATCMR = VABCMR
056600170112     C                   EVAL      VATCNT = VABCNT
056700170112     C                   EVAL      VATTRC = 'J'
056800170112     C                   ADD       1             §CTROKVT
056900170112     C                   WRITE     EDIVAT00
057000170112     C                   ENDIF
057100170112     C                   ENDIF
057200170112     C*
057300170112     C                   ENDIF
057400170112     C*
057500170112     C                   ENDIF
057600140117     C***
057700140922     C* ...tipo record 'NA2/ST' (info testata - destinatario)
057800140922     C                   IF        %trim(%subst(vindta:1:3)) = 'NA2'
057900140922     C* Solo sub-types 'ST'
058000140922     C                   IF        %trim(%subst(vindta:4:2)) = 'ST'
058100060202     C* ......VABRSD
058200140922     C                   EVAL      VABRSD=%trim(%subst(vindta:42:35))
058300130620     C* ......VABRD2
058400140922     C                   EVAL      VABRD2=%trim(%subst(vindta:77:35))
058500100714     C* ......VABIND
058600140922     C                   EVAL      VABIND=%trim(%subst(vindta:112:35))
058700130620     C* ......VABCAD
058800140922     C                   EVAL      VABCAD=%trim(%subst(vindta:226:17))
058900060202     C* ......VABLOD
059000140922     C                   EVAL      VABLOD=%trim(%subst(vindta:182:35))
059100140922     C* ......VABPRD
059200140922     C                   EVAL      VABPRD=%trim(%subst(vindta:217:9))
059300140922     C* ......VABNZD
059400140922     C                   EVAL      VABNZD=%trim(%subst(vindta:243:3))
059500130620     C                   IF        VABNZD='IT'
059600130620     C                   EVAL      VABNZD=*blanks
059700130620     C                   ENDIF
059800060202     C* ......VABPRD
059900070719     C* Reperisco la provincia dal CAP e dalla località
060000070719     C                   IF        VABPRD  = *blanks AND
060100130620     C                             VABLOD <> *blanks AND
060200130620     C                             VABCAD <> *blanks AND
060300130620     C                             VABNZD  = *blanks
060400070719     C                   CLEAR                   TISI95DS
060500070719     C                   EVAL      I95TCN = '3'
060600070719     C                   Z-ADD     datcor        I95DAT
060700070719     C                   EVAL      I95CAP = VABCAD
060800070719     C                   EVAL      I95LOC = VABLOD
060900070719     C                   CALL      'TISI95R'
061000070719     C                   PARM                    TISI95DS
061100070719     C                   EVAL      VABPRD = O95PRV
061200070719     C                   ENDIF
061300140922     C* ......VATNOT_A
061400140922     C                   EVAL      VATNOT=%trim(%subst(vindta:371:35))
061500140922     C                   IF        VATNOT <> *blanks
061600140922     C                   EVAL      VATCMR = VABCMR
061700140922     C                   EVAL      VATCNT = VABCNT
061800140922     C                   EVAL      VATTRC = 'A'
061900140922     C                   ADD       1             §CTROKVT
062000140922     C                   WRITE     EDIVAT00
062100140922     C                   ENDIF
062200140922     C* ......VATNOT_B
062300140922     C                   EVAL      VATNOT=%trim(%subst(vindta:406:20))
062400140922     C                   IF        VATNOT <> *blanks
062500140922     C                   EVAL      VATCMR = VABCMR
062600140922     C                   EVAL      VATCNT = VABCNT
062700140922     C                   EVAL      VATTRC = 'B'
062800140922     C                   ADD       1             §CTROKVT
062900140922     C                   WRITE     EDIVAT00
063000140922     C                   ENDIF
063100150216     C* ......VABNOT
063200150216     C                   EVAL      VABNOT=%trim(%subst(vindta:553:35))
063300150216     C* ......VABNT2
063400150216     C                   EVAL      VABNOT=%trim(%subst(vindta:553+35:35))
063500140117     C*
063600140117     C                   ENDIF
063700140117     C*
063800140117     C                   ENDIF
063900140922     C***
064000140922     C* ...tipo record 'GID' (dettaglio segnacolli)
064100140922     C                   IF        %trim(%subst(vindta:1:3)) = 'GID'
064200140922     C* ......VATNOT_E
064300140922     C                   EVAL      VATNOT=%trim(%subst(vindta:118:35))
064400140922     C                   IF        VATNOT <> *blanks
064500140922     C                   EVAL      VATCMR = VABCMR
064600140922     C                   EVAL      VATCNT = VABCNT
064700140922     C                   EVAL      VATTRC = 'E'
064800140922     C                   ADD       1             §CTROKVT
064900140922     C                   WRITE     EDIVAT00
065000140922     C                   ENDIF
065100140922     C*
065200140922     C                   ENDIF
065300010202     C*
065400000801     C* Ebbene...
065500000801     C                   ADD       1             §CTRMO
065600010201     C                   IF        errore <> *zeros
065700000801     C                   ADD       1             §CTRNO
065800000801     C                   EVAL      vinflg = '2'
065900000801     C                   ELSE
066000010201     C                   ADD       1             §CTROKVB
066100000801     C                   ENDIF
066200000801     C*
066300000801     C                   ENDSR
066400010202     C*----------------------------------------------------*
066500140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
066600010202     C*----------------------------------------------------*
066700020305     C     PREVAT        BEGSR
066800010202     C*
066900140117     C* Compongo il nome del membro da dare al EDIVATWR
067000010202     C                   eval      parmbr = vlrhdl
067100010202     C                   movel     'M'           parmbr
067200060113     C                   eval      parccm = vlrksc
067300010202     C                   eval      paropz = '1'
067400010202     C* Effettuo la chiamata al CLLE preposto
067500140117     C                   call(e)   'TITVEVTC'
067600010202     C                   parm                    parccm
067700010202     C                   parm                    parmbr
067800010202     C                   parm                    paropz
067900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
068000010202     C                   if        %error
068100010202     C                   movel     '1'           chkcall
068200010202     C                   else
068300010202     C                   movel     '0'           chkcall
068400010202     C                   endif
068500010202     C*
068600010202     C                   ENDSR
068700000801     C*----------------------------------------------------*
068800000801     C*  CONTROLLO NUMERICITA' CAMPI
068900000801     C*----------------------------------------------------*
069000000801     C     CHKNUM        BEGSR
069100000801     C*
069200000801     C                   call(e)   'ISNUMERIC'
069300000801     C                   PARM                    PiStr            30
069400040714     C                   PARM      ','           PiDecChr          1
069500000801     C                   PARM      *ZEROS        PiVal            30 9
069600000801     C                   PARM      '0'           PiInt             1
069700000801     C                   PARM      '0'           PiNum             1
069800000801     C                   IF        %error
069900000801     C                   EVAL      PiInt=*off
070000000801     C                   ENDIF
070100000801     C*
070200000801     C                   ENDSR
070300000801     C***
070400000801
070500011113
070600011113     C*----------------------------------------------------*
070700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
070800011113     C*----------------------------------------------------*
070900011113     C     CHKIMPDIV     BEGSR
071000011113     C*
071100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
071200011113     C                   Z-ADD     *zeros        wrkDec            9 9
071300011113     C*
071400011113     C* Come prima cosa effettuo considerazioni sulla divisa
071500011113     C                   IF        vabIAS > *zeros
071600011113     C                   IF        vabVAS <> 'EUR'
071700011113     C                   EVAL      vabVAS =  'ITL'
071800011113     C                   ENDIF
071900011113     C                   ENDIF
072000011113     C*
072100011113     C                   IF        vabCAS > *zeros
072200011113     C                   IF        vabVCA <> 'EUR'
072300011113     C                   EVAL      vabVCA =  'ITL'
072400011113     C                   ENDIF
072500011113     C                   ENDIF
072600011113     C*
072700011113     C                   IF        vabVMD > *zeros
072800020305     C                   IF        vabVAD <> 'EUR'
072900011113     C                   EVAL      vabVAD =  'ITL'
073000011113     C                   ENDIF
073100011113     C                   ENDIF
073200011113     C*
073300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
073400011113     C                   Z-ADD     vabIAS        wrkDec
073500011113     C                   IF        wrkDec > *zeros
073600011113     C                   IF        vabVAS = 'ITL'
073700011113     C                   EVAL      vabIAS = *zeros
073800011113     C                   ENDIF
073900011113     C                   ENDIF
074000011113     C*
074100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
074200011113     C                   Z-ADD     vabCAS        wrkDec
074300011113     C                   IF        wrkDec > *zeros
074400011113     C                   IF        vabVCA = 'ITL'
074500011113     C                   EVAL      vabCAS = *zeros
074600011113     C                   ENDIF
074700011113     C                   ENDIF
074800011113     C*
074900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
075000011113     C                   Z-ADD     vabVMD        wrkDec
075100011113     C                   IF        wrkDec > *zeros
075200011113     C                   IF        vabVAD = 'ITL'
075300011113     C                   EVAL      vabVMD = *zeros
075400011113     C                   ENDIF
075500011113     C                   ENDIF
075600011113     C*
075700011113     C                   ENDSR
075800011113     C***
075900011113
076000011113
076100000801
076200000801
076300990920      /TITLE Invio dei dati al punto operativo.
076400010202     C     invio         BEGSR
076500990920     C*
076600140117     C* 1° invio EDIVAT
076700010201     C                   reset                   dscmz
076800010201     C                   move      vlrpoi        cmzdst
076900140117     C                   eval      cmzfld = 'EDIVATWR'
077000010201     C                   eval      cmzmbd = vlrhdl
077100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
077200021009     C***                if        prmfir = *blanks
077300140117     C                   eval      cmzfla = 'EDIVAT0F'
077400140117     C                   eval      cmzmba = 'EDIVAT0F'
077500021009     C***                else
077600021009     C***                eval      cmzfla = prmfir
077700021009     C***                eval      cmzmba = prmfir
077800021009     C***                endif
077900010201     C                   eval      cmznrr = *zeros
078000020305     C                   move      §ctrokvt      cmznrr
078100021018     C                   eval      cmzlba = vlrfl1
078200010201     C                   call(e)   'TIS711C'
078300010201     C                   parm                    dscmz
078400010201     C                   parm      *blanks       esito
078500010205     C                   if        %error
078600010205     C                             or cmzerr = '1'
078700010205     C                             or esito  = '1'
078800010205     C                   eval      wrkesito = '3'
078900010205     C                   else
079000010201     C*
079100140117     C* 2° invio EDIVAB
079200010201     C                   reset                   dscmz
079300010201     C                   move      vlrpoi        cmzdst
079400010201     C                   eval      cmzfld = vlrfou
079500010201     C                   eval      cmzmbd = vlrhdl
079600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
079700021009     C***                if        prmfir = *blanks
079800140117     C                   eval      cmzfla = 'EDIVAB0F'
079900140117     C                   eval      cmzmba = 'EDIVAB0F'
080000021009     C***                else
080100021009     C***                eval      cmzfla = prmfir
080200021009     C***                eval      cmzmba = prmfir
080300021009     C***                endif
080400010201     C                   eval      cmznrr = *zeros
080500010201     C                   move      §ctrokvb      cmznrr
080600021018     C                   eval      cmzlba = vlrfl1
080700010201     C                   call(e)   'TIS711C'
080800010201     C                   parm                    dscmz
080900010201     C                   parm      *blanks       esito
081000010201     C                   if        %error
081100010201     C                             or cmzerr = '1'
081200010201     C                             or esito  = '1'
081300010201     C                   eval      wrkesito = '3'
081400010201     C                   endif
081500010205     C                   endif
081600990920     C*
081700000613     C                   ENDSR
081800000613     C***
081900070411
082000070411     C     *pssr         BEGSR
082100070411     C*
082200070411     C                   if        %open(tivin00r)
082300070411     C                   close     tivin00r
082400070411     C                   endif
082500140117     C                   if        %open(edivabwr)
082600140117     C                   close     edivabwr
082700070411     C                   endif
082800140117     C                   if        %open(edivatwr)
082900140117     C                   close     edivatwr
083000070411     C                   endif
083100070411     C*
083200070411     C* Effettuo la chiamata al CLLE preposto
083300140117     C                   call(e)   'TITVEVTC'
083400070411     C                   parm                    parccm
083500070411     C                   parm                    parmbr
083600070411     C                   parm      '2'           paropz
083700070411     C*
083800070411     C                   eval      wrkesito = '2'
083900070411     C*
084000070411     C                   seton                                        LR
084100070411     C*
084200070411     C                   ENDSR     '*CANCL'
084300070411     C***
084400070411
084500990910
084600000613     C     *inzsr        BEGSR
084700990910     C*
084800990910     C     *entry        plist
084900990920     C                   parm                    tivlrds
085000990921     C                   parm      wrkesito      esito
085100000724     C                   parm                    prmlit
085200000710     C                   parm                    prmfir
085300000613     C*
085400000830     C* CALCOLA LA DATA CORRENTE
085500140117     C                   time                    wn14             14 0
085600140117     C                   movel     wn14          oracor            6 0          *ORA
085700100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
085800100722     C                   eval      datcor = %dec(%date() : *ISO)
085900000830     C*
086000000613     C                   ENDSR
086100000613     C***
