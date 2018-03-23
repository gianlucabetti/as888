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
005700170731     C*
005800170731     C                   if        *in53
005900170731     C                   call      'TIS7P2SER'
006000170731     C                   parm      'C'           pIn_Opz           1
006100170731     C                   parm                    tivlrds
006200170731     C                   parm                    EDIVABDS
006300170731     C                   parm      *blanks       pIn_CdIdAz        2
006400170731     C                   parm      *blanks       pIn_MaskPDF      50
006500170731     C                   parm      *blanks       pOut_Esito        1
006600170731     C                   endif
006700000616     C*
006800010201     C                   seton                                        LR
006900990908
007000000801
007100910830     C*--------------------------------------------------------
007200140117     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
007300910830     C*--------------------------------------------------------
007400040526     C     RWFILE        BEGSR
007500990910     C*
007600990914     C                   if        not %open(tivin00r)
007700990908     C                   open      tivin00r
007800990914     C                   endif
007900140117     C                   if        not %open(edivabwr)
008000140117     C                   open      edivabwr
008100990914     C                   endif
008200140117     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
008300020305     C                   exsr      prevat
008400010201     C*
008500010202     C                   if        chkcall = '0'
008600010202     C*
008700140117     C                   if        not %open(edivatwr)
008800140117     C                   open      edivatwr
008900010201     C                   endif
009000990910     C*
009100010201     C                   clear                   §CTROKVB          5 0
009200020305     C                   clear                   §CTROKVT          5 0
009300000801     C                   clear                   §CTRMO            5 0
009400000801     C                   clear                   §CTRNO            5 0
009500100729     C*
009600100729     C                   z-add     1             wGiro             1 0
009700140117     C*
009800140902     C                   if        *in53
009900140117     C                   call      'TIS7P2SER'
010000140117     C                   parm      'O'           pIn_Opz           1
010100140117     C                   parm                    tivlrds
010200140117     C                   parm                    EDIVABDS
010300140117     C                   parm      *blanks       pIn_CdIdAz        2
010400140117     C                   parm      *blanks       pIn_MaskPDF      50
010500140117     C                   parm      *blanks       pOut_Esito        1
010600140902     C                   endif
010700130708     C*
010800130708     C                   SETOFF                                       31
010900921023     C                   DO        *HIVAL
011000990913     C*
011100990915     C                   READ      tivin00r                               70
011200040910     C                   if        vindta > *blanks
011300000613     C                   add       1             rrnum
011400000801     C*
011500000801     C                   if        *in70 = *off
011600000801     C                             and
011700000801     C                             (vinflg = *blanks
011800000801     C                              or vinflg = '0'
011900000801     C                              or vinflg = '2')
012000000801     C*
012100000801     C                   clear                   vinmsg
012200000801     C                   eval      vinflg = '1'
012300040910     C*
012400040910     C* Eseguo routine d traduzione
012500040910     C                   exsr      impvabvat
012600040802     C*
012700010305     C                   endif
012800000905     C*
012900000905     C                   else
013000000905     C                   eval      vinflg = '1'
013100000905     C                   endif
013200000905     C*
013300000905     C  N70              update    tivin000
013400000905     C*
013500991022     C  N70              ENDdo
013600100722     C*
013700100722     C* Scarico i buffer testata ancora "in canna"
013800140922     C*
013900140922     C  N31              exsr      CHKWRI
014000140117     C  N31              WRITE     EDIVAB00
014100130708     C                   SETOFF                                       31
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
018000140922     C* Considerazioni sul contenuto di campi precedentemente valorizzati
018100140922     C                   IF        FlgCAS <> '0'
018200140922     C                   IF        VABCBO = '1'
018300140922     C                   EVAL      VABCBO = '4'
018400140922     C                   ELSE
018500140922     C                   EVAL      VABCBO = '6'
018600140922     C                   ENDIF
018700140922     C                   ENDIF
018800140922     C*
018900140922     C* Eseguo routine finale x considerazioni specifiche su importi/divise
019000140922     C                   EXSR      CHKIMPDIV
019100140922     C*
019200140922     C                   ENDSR
019300990920
019400000801     C*----------------------------------------------------*
019500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019600000801     C*----------------------------------------------------*
019700010201     C     INZVAR        BEGSR
019800000801     C*
019900040802     C                   Z-ADD     *zeros        Num5_0            5 0
020000040802     C                   MOVEL     '0'           FlgCAS            1
020100000801     C*
020200000801     C                   ENDSR
020300000801     C*----------------------------------------------------*
020400040910     C*  IMPOSTAZIONE CAMPI COSTANTI
020500000801     C*----------------------------------------------------*
020600000801     C     DEFCAM        BEGSR
020700000801     C*
020800140117     C                   CLEAR                   EDIVAB00
020900140117     C                   CLEAR                   EDIVAT00
021000020619     C* Imposto i valori di default...
021100170224     C                   Z-ADD     1662533       VABCCM
021200170224     C                   Z-ADD     1662533       VATCCM
021300140922     C                   Z-ADD     166           VABLNP
021400140922     C                   Z-ADD     166           VATLNP
021500140922     C                   Z-ADD     006           VABCTR
021600140117     C                   MOVEL     '7T'          VABCTM
021700040823     C                   MOVEL     '1'           VABCBO
021800020619     C* ... e poi verifico se sono stati passati come parametri
021900020619     C                   IF        vlrppt > *blanks
022000040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
022100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
022200020619     C                   EXSR      CHKNUM
022300020619     C                   IF        PiInt=*on
022400020619     C                   Z-ADD     PiVal         VABCCM
022500020619     C                   Z-ADD     PiVal         VATCCM
022600020619     C                   ENDIF
022700040506     C                   ENDIF
022800040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
022900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023000020619     C                   EXSR      CHKNUM
023100020619     C                   IF        PiInt=*on
023200020619     C                   Z-ADD     PiVal         VABLNP
023300020619     C                   Z-ADD     PiVal         VATLNP
023400040506     C                   ENDIF
023500020619     C                   ENDIF
023600040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
023700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
023800020619     C                   EXSR      CHKNUM
023900020619     C                   IF        PiInt=*on
024000020619     C                   Z-ADD     PiVal         VABCTR
024100040506     C                   ENDIF
024200020619     C                   ENDIF
024300060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
024400060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
024500060202     C                   ENDIF
024600140902     C                   IF        %subst(vlrppt:16:1) = 'S'
024700140902     C                   SETON                                        53
024800140902     C                   ELSE
024900140902     C                   SETOFF                                       53
025000140902     C                   ENDIF
025100020619     C                   ENDIF
025200000801     C*
025300000801     C                   ENDSR
025400000801     C*----------------------------------------------------*
025500140117     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
025600000801     C*----------------------------------------------------*
025700040910     C     IMPVABVAT     BEGSR
025800040910     C*
025900040910     C* Traduzione relativa ai tipi record del file del cliente
026000040910     C*
026100071210     C*
026200071210     C***
026300140922     C* ...tipo record 'BGM' (info CMR)
026400140922     C                   IF        %trim(%subst(vindta:1:3)) = 'BGM'
026500140903     C                   MOVEL     *blanks       savCMR           35
026600140922     C                   EVAL      savCMR = %trim(%subst(vindta:17:15)) + ' '+
026700140922     C                                      %char(datcor) + ' ' + %char(oracor)
026800130620     C                   ENDIF
026900170224     C***
027000170224     C* ...tipo record 'NAD' (info cliente)
027100170224     C                   IF        %trim(%subst(vindta:1:3)) = 'NAD'
027200170224     C                   Z-ADD     *zeros        savCCM            7 0
027300171027     C                   Z-ADD     *zeros        savLNP            3 0
027400171027     C                   SELECT
027500171027     C                   WHEN      %trim(%subst(vindta:491:35)) = '40054'
027600170224     C                   EVAL      savCCM = 1663621
027700171027     C                   WHEN      %trim(%subst(vindta:491:35)) = '40042'
027800171031     C                   EVAL      savCCM = 1662533
027900171027     C                   EVAL      savLNP = 220
028000171027     C                   WHEN      %trim(%subst(vindta:491:35)) = '80052'
028100171027     C                   EVAL      savCCM = 0435707
028200171027     C                   EVAL      savLNP = 043
028300171110     C                   WHEN      %trim(%subst(vindta:491:35)) = '80066'
028400171110     C                   EVAL      savCCM = 0430001
028500171110     C                   EVAL      savLNP = 043
028600180112     C                   WHEN      %trim(%subst(vindta:491:35)) = '88020'
028700180112     C                   EVAL      savCCM = 0435916
028800180112     C                   EVAL      savLNP = 043
028900180124     C                   WHEN      %trim(%subst(vindta:491:35)) = '80078'
029000180124     C                   EVAL      savCCM = 0437380
029100180124     C                   EVAL      savLNP = 043
029200180130     C                   WHEN      %trim(%subst(vindta:491:35)) = '80079'
029300180130     C                   EVAL      savCCM = 0437796
029400180130     C                   EVAL      savLNP = 043
029500171027     C                   ENDSL
029600170224     C                   ENDIF
029700140922     C***
029800140922     C* ...tipo record 'CNI' (rottuta di codice spedizione)
029900140922     C                   IF        %trim(%subst(vindta:1:3)) = 'CNI' and
030000170516     C                             (VABNAS <> %subst(vindta:884:17) or
030100170504     C                              %subst(vindta:884:17) = *blanks or
030200140923     C                              wGiro = 1 or FlgCAS = '1')
030300100722     C*
030400100722     C* Se nn primo giro => scarico il buffer precedente
030500100722     C                   if        wGiro = 1
030600100722     C                   eval      wGiro = 2
030700100722     C                   else
030800140922     C*
030900140922     C  N31              exsr      CHKWRI
031000140117     C  N31              WRITE     EDIVAB00
031100130708     C                   SETOFF                                       31
031200100722     C                   endif
031300100722     C*
031400071210     C* Resetto indicatore d anomalia sul singolo record
031500071210     C                   eval      vinflg = '1'
031600071210     C* ......inizializzazioni iniziali e formati record file Bartolini
031700071210     C                   EXSR      INZVAR
031800071210     C                   EXSR      DEFCAM
031900170224     C*
032000170224     C                   IF        savCCM > *zeros
032100170224     C                   EVAL      VABCCM = savCCM
032200170224     C                   EVAL      VATCCM = savCCM
032300171027     C                   EVAL      VABLNP = savLNP
032400171027     C                   EVAL      VATLNP = savLNP
032500171110     C                   SELECT
032600171110     C                   WHEN      VABCCM = 0435707
032700171109     C                   IF        datcor < 20171113
032800171030     C                   EVAL      VABCTR = 301
032900171109     C                   ELSE
033000171109     C                   EVAL      VABCTR = 000
033100171109     C                   ENDIF
033200171030     C                   EVAL      VABCTM = '7Q'
033300171110     C                   WHEN      VABCCM = 0430001
033400171110     C                   EVAL      VABCTR = 000
033500171110     C                   EVAL      VABCTM = '7Q'
033600180112     C                   WHEN      VABCCM = 0435916
033700180112     C                   EVAL      VABCTR = 000
033800180112     C                   EVAL      VABCTM = '7Q'
033900180124     C                   WHEN      VABCCM = 0437380
034000180124     C                   EVAL      VABCTR = 301
034100180124     C                   EVAL      VABCTM = '7Q'
034200180130     C                   WHEN      VABCCM = 0437796
034300180130     C                   EVAL      VABCTR = 301
034400180130     C                   EVAL      VABCTM = '7Q'
034500171110     C                   ENDSL
034600171030     C                   ENDIF
034700071210     C*
034800071210     C                   Z-ADD     *zeros        errore            1 0
034900071210     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
035000071210     C                   MOVEL     datcor        VABAAS
035100071210     C                   MOVEL     datcor        VATAAS
035200071210     C                   MOVE      datcor        VABMGS
035300071210     C                   MOVE(P)   vlrpoi        VABFGS
035400071210     C                   MOVE(P)   vlrpoi        VATFGS
035500140903     C*
035600140903     C* Dai "fissi"
035700140903     C                   EVAL      VABCMR = savCMR
035800140903     C                   EVAL      VABDCM = datcor
035900140903     C                   EVAL      VABDTS = datcor
036000140903     C                   EVAL      VABHMS = oracor
036100140903     C                   EVAL      VABCNT = 1
036200140903     C*
036300071210     C* ......VABNSP/VATNSP
036400071210     C* NSP => Stacco un numeratore da AZNUM
036500071210     C                   clear                   TRUL33DS
036600071210     C                   eval      I33OPE = *zeros
036700071210     C                   eval      I33CNU = 302
036800071210     C                   eval      I33NUM = 1
036900071210     C                   movel     TRUL33DS      KPJBU
037000071210     C                   call      'TRUL33R'
037100071210     C                   parm                    KPJBA
037200071210     C                   movel     KPJBU         TRUL33DS
037300071210     C                   if        O33ERR = *zeros
037400071210     C                   z-add     O33NRF        VABNSP
037500071210     C                   z-add     O33NRF        VATNSP
037600071210     C                   else
037700071210     C                   Z-ADD     1             errore
037800071210     C                   EVAL      vinmsg = %trimr(vinmsg)
037900071210     C                             + ' ' + 'VABNSP VATNSP'
038000071210     C                   endif
038100140922     C*
038200140922     C                   ENDIF
038300140922     C***
038400140922     C* ...tipo record 'CNI' (info testata)
038500140922     C                   IF        %trim(%subst(vindta:1:3)) = 'CNI'
038600170516     C* ......VABRMN
038700140922     C                   EVAL      PiStr=%trim(%subst(vindta:9:35))
038800140922     C                   EXSR      CHKNUM
038900140922     C                   IF        PiInt=*on
039000170516     C                   Z-ADD     PiVal         VABRMN
039100140922     C                   ELSE
039200140922     C                   ADD       1             errore
039300140922     C                   EVAL      vinmsg = %trimr(vinmsg)
039400170516     C                             + ' ' + 'VABRMN'
039500140922     C                   ENDIF
039600170516     C* ......VABRMA
039700170516     C                   EVAL      VABRMA=%trim(%subst(vindta:710:17))
039800151112     C* ......VABNAS
039900141106     C                   EVAL      VABNAS=%trim(%subst(vindta:884:17))
040000140922     C* ......VABNCL
040100140922     C                   EVAL      PiStr=%trim(%subst(vindta:56:5))
040200140922     C                   EXSR      CHKNUM
040300140922     C                   IF        PiInt=*on
040400140922     C                   ADD       PiVal         VABNCL
040500140922     C                   ELSE
040600140922     C                   ADD       1             errore
040700140922     C                   EVAL      vinmsg = %trimr(vinmsg)
040800140922     C                             + ' ' + 'VABNCL'
040900140922     C                   ENDIF
041000140922     C* ......VABPKB
041100140922     C                   EVAL      PiStr=%trim(%subst(vindta:66:12))
041200140922     C                   EXSR      CHKNUM
041300140922     C                   IF        PiNum=*on
041400140922     C                   EVAL      PiVal = PiVAl / 100
041500140922     C                   ADD       PiVal         VABPKB
041600140922     C                   ELSE
041700140922     C                   ADD       1             errore
041800140922     C                   EVAL      vinmsg = %trimr(vinmsg)
041900140922     C                             + ' ' + 'VABPKB'
042000140922     C                   ENDIF
042100140922     C* ......VABVLB
042200140922     C                   EVAL      PiStr=%trim(%subst(vindta:78:12))
042300140922     C                   EXSR      CHKNUM
042400140922     C                   IF        PiNum=*on
042500140922     C                   EVAL      PiVal = PiVAl / 100
042600140922     C                   ADD       PiVal         VABVLB
042700140922     C                   ELSE
042800140922     C                   ADD       1             errore
042900140922     C                   EVAL      vinmsg = %trimr(vinmsg)
043000140922     C                             + ' ' + 'VABVLB'
043100140922     C                   ENDIF
043200140117     C* ......VABCAS
043300140922     C                   IF        %subst(vindta:145:12) <> *all'0'
043400140117     C                   EVAL      FlgCAS = '1'
043500140922     C                   EVAL      VABVCA = 'EUR'
043600140922     C                   EVAL      PiStr=%trim(%subst(vindta:145:12))
043700140117     C                   EXSR      CHKNUM
043800140117     C                   IF        PiNum=*on
043900140922     C                   EVAL      PiVal = PiVAl / 100
044000140922     C                   ADD       PiVal         VABCAS
044100140117     C                   ELSE
044200140117     C                   ADD       1             errore
044300140117     C                   EVAL      vinmsg = %trimr(vinmsg)
044400140117     C                             + ' ' + 'VABCAS'
044500140117     C                   ENDIF
044600140117     C                   ENDIF
044700140922     C* ......VABIAS
044800140922     C                   IF        %subst(vindta:157:12) <> *all'0'
044900140922     C                   EVAL      VABVAS = 'EUR'
045000140922     C                   EVAL      PiStr=%trim(%subst(vindta:157:12))
045100140922     C                   EXSR      CHKNUM
045200140922     C                   IF        PiNum=*on
045300140922     C                   EVAL      PiVal = PiVAl / 100
045400140922     C                   ADD       PiVal         VABIAS
045500140922     C                   ELSE
045600140922     C                   ADD       1             errore
045700140922     C                   EVAL      vinmsg = %trimr(vinmsg)
045800140922     C                             + ' ' + 'VABIAS'
045900140922     C                   ENDIF
046000140922     C                   ENDIF
046100140922     C* ......VABVMD
046200140922     C                   IF        %subst(vindta:169:12) <> *all'0'
046300140922     C                   EVAL      VABVAD = 'EUR'
046400140922     C                   EVAL      PiStr=%trim(%subst(vindta:169:12))
046500140922     C                   EXSR      CHKNUM
046600140922     C                   IF        PiNum=*on
046700140922     C                   EVAL      PiVal = PiVAl / 100
046800140922     C                   ADD       PiVal         VABVMD
046900140922     C                   ELSE
047000140922     C                   ADD       1             errore
047100140922     C                   EVAL      vinmsg = %trimr(vinmsg)
047200140922     C                             + ' ' + 'VABVMD'
047300140922     C                   ENDIF
047400140922     C                   ENDIF
047500141002     C* ......VABDCR / VABHCR
047600141009     C                   IF        %trim(%subst(vindta:142:2)) = '2'
047700141002     C                   IF        %trim(%subst(vindta:125:8)) <> *blanks AND
047800141002     C                             %trim(%subst(vindta:125:8)) <> *zeros
047900141002     C                   EVAL      PiStr=%trim(%subst(vindta:125:8))
048000141002     C                   EXSR      CHKNUM
048100141002     C                   IF        PiInt=*on
048200141002     C                   Z-ADD     PiVal         VABDCR
048300141002     C                   ELSE
048400141002     C                   ADD       1             errore
048500141002     C                   EVAL      vinmsg = %trimr(vinmsg)
048600141002     C                             + ' ' + 'VABDCR'
048700141002     C                   ENDIF
048800141002     C                   EVAL      PiStr=%trim(%subst(vindta:133:4))
048900141002     C                   EXSR      CHKNUM
049000141002     C                   IF        PiInt=*on
049100141002     C                   Z-ADD     PiVal         VABHCR
049200141002     C                   ELSE
049300141002     C                   ADD       1             errore
049400141002     C                   EVAL      vinmsg = %trimr(vinmsg)
049500141002     C                             + ' ' + 'VABHCR'
049600141002     C                   ENDIF
049700141002     C                   ENDIF
049800141002     C                   ENDIF
049900141009     C* ......VABDCR / VABHCR
050000141009     C                   IF        %trim(%subst(vindta:142:2)) = '17'
050100141009     C                   IF        %trim(%subst(vindta:125:8)) <> *blanks AND
050200141009     C                             %trim(%subst(vindta:125:8)) <> *zeros
050300141009     C                   EVAL      VABTCR = 'P'
050400141009     C                   EVAL      PiStr=%trim(%subst(vindta:125:8))
050500141009     C                   EXSR      CHKNUM
050600141009     C                   IF        PiInt=*on
050700141009     C                   Z-ADD     PiVal         VABDCR
050800141009     C                   ELSE
050900141009     C                   ADD       1             errore
051000141009     C                   EVAL      vinmsg = %trimr(vinmsg)
051100141009     C                             + ' ' + 'VABDCR'
051200141009     C                   ENDIF
051300141009     C                   EVAL      PiStr=%trim(%subst(vindta:133:4))
051400141009     C                   EXSR      CHKNUM
051500141009     C                   IF        PiInt=*on
051600141009     C                   Z-ADD     PiVal         VABHCR
051700141009     C                   ELSE
051800141009     C                   ADD       1             errore
051900141009     C                   EVAL      vinmsg = %trimr(vinmsg)
052000141009     C                             + ' ' + 'VABHCR'
052100141009     C                   ENDIF
052200141009     C                   ENDIF
052300141009     C                   ENDIF
052400140922     C* ......VATNOT_P
052500140922     C                   if        *in53
052600140923     C                   eval      pIn_MaskPDF =
052700140923     C                                     %trim(%subst(vindta:1292:100))
052800170516     C                   call      'TIS7P2SER'
052900140922     C                   parm      'W'           pIn_Opz           1
053000140922     C                   parm                    tivlrds
053100140922     C                   parm                    EDIVABDS
053200140922     C                   parm      '10'          pIn_CdIdAz        2
053300140922     C                   parm                    pIn_MaskPDF      50
053400140922     C                   parm      *blanks       pOut_Esito        1
053500140922     C                   endif
053600140117     C*
053700140117     C                   ENDIF
053800170112     C***
053900170112     C* ...tipo record 'NA2/SF' (info testata - mittente)
054000170112     C                   IF        %trim(%subst(vindta:1:3)) = 'NA2'
054100170112     C* Solo sub-types 'SF'
054200170112     C                   IF        %trim(%subst(vindta:4:2)) = 'SF'
054300170112     C* ......VABRMO
054400170112     C                   EVAL      VABRMO=%trim(%subst(vindta:42:35))
054500170112     C* ......VABCMO
054600170112     C                   EVAL      VABCMO=%trim(%subst(vindta:226:17))
054700170112     C* ......VABNMO
054800170112     C                   EVAL      VABNMO=%trim(%subst(vindta:243:3))
054900170112     C                   IF        VABNMO='IT'
055000170112     C                   EVAL      VABNMO=*blanks
055100170112     C                   ENDIF
055200170112     C*
055300170112     C                   ENDIF
055400170112     C*
055500170112     C                   ENDIF
055600140117     C***
055700140922     C* ...tipo record 'NA2/ST' (info testata - destinatario)
055800140922     C                   IF        %trim(%subst(vindta:1:3)) = 'NA2'
055900140922     C* Solo sub-types 'ST'
056000140922     C                   IF        %trim(%subst(vindta:4:2)) = 'ST'
056100060202     C* ......VABRSD
056200140922     C                   EVAL      VABRSD=%trim(%subst(vindta:42:35))
056300130620     C* ......VABRD2
056400140922     C                   EVAL      VABRD2=%trim(%subst(vindta:77:35))
056500100714     C* ......VABIND
056600140922     C                   EVAL      VABIND=%trim(%subst(vindta:112:35))
056700130620     C* ......VABCAD
056800140922     C                   EVAL      VABCAD=%trim(%subst(vindta:226:17))
056900060202     C* ......VABLOD
057000140922     C                   EVAL      VABLOD=%trim(%subst(vindta:182:35))
057100140922     C* ......VABPRD
057200140922     C                   EVAL      VABPRD=%trim(%subst(vindta:217:9))
057300140922     C* ......VABNZD
057400140922     C                   EVAL      VABNZD=%trim(%subst(vindta:243:3))
057500130620     C                   IF        VABNZD='IT'
057600130620     C                   EVAL      VABNZD=*blanks
057700130620     C                   ENDIF
057800060202     C* ......VABPRD
057900070719     C* Reperisco la provincia dal CAP e dalla località
058000070719     C                   IF        VABPRD  = *blanks AND
058100130620     C                             VABLOD <> *blanks AND
058200130620     C                             VABCAD <> *blanks AND
058300130620     C                             VABNZD  = *blanks
058400070719     C                   CLEAR                   TISI95DS
058500070719     C                   EVAL      I95TCN = '3'
058600070719     C                   Z-ADD     datcor        I95DAT
058700070719     C                   EVAL      I95CAP = VABCAD
058800070719     C                   EVAL      I95LOC = VABLOD
058900070719     C                   CALL      'TISI95R'
059000070719     C                   PARM                    TISI95DS
059100070719     C                   EVAL      VABPRD = O95PRV
059200070719     C                   ENDIF
059300140922     C* ......VATNOT_A
059400140922     C                   EVAL      VATNOT=%trim(%subst(vindta:371:35))
059500140922     C                   IF        VATNOT <> *blanks
059600140922     C                   EVAL      VATCMR = VABCMR
059700140922     C                   EVAL      VATCNT = VABCNT
059800140922     C                   EVAL      VATTRC = 'A'
059900140922     C                   ADD       1             §CTROKVT
060000140922     C                   WRITE     EDIVAT00
060100140922     C                   ENDIF
060200140922     C* ......VATNOT_B
060300140922     C                   EVAL      VATNOT=%trim(%subst(vindta:406:20))
060400140922     C                   IF        VATNOT <> *blanks
060500140922     C                   EVAL      VATCMR = VABCMR
060600140922     C                   EVAL      VATCNT = VABCNT
060700140922     C                   EVAL      VATTRC = 'B'
060800140922     C                   ADD       1             §CTROKVT
060900140922     C                   WRITE     EDIVAT00
061000140922     C                   ENDIF
061100180119     C*
061200180119     C                   IF        VABCCM = 1663621 or
061300180119     C                             VABCCM = 0435707 or
061400180119     C                             VABCCM = 0430001 or
061500180119     C                             VABCCM = 0435916
061600180119     C* ......VATNOT_S
061700180119     C                   EVAL      VATNOT=%trim(%subst(vindta:426:20))
061800180119     C                   IF        VATNOT <> *blanks
061900180119     C                   EVAL      VATCMR = VABCMR
062000180119     C                   EVAL      VATCNT = VABCNT
062100180119     C                   EVAL      VATTRC = 'S'
062200180119     C                   ADD       1             §CTROKVT
062300180119     C                   WRITE     EDIVAT00
062400180119     C                   ENDIF
062500180119     C* ......VATNOT_I
062600180119     C                   EVAL      VATNOT=%trim(%subst(vindta:466:35))
062700180119     C                   IF        VATNOT <> *blanks
062800180119     C                   EVAL      VATCMR = VABCMR
062900180119     C                   EVAL      VATCNT = VABCNT
063000180119     C                   EVAL      VATTRC = 'I'
063100180119     C                   ADD       1             §CTROKVT
063200180119     C                   WRITE     EDIVAT00
063300180119     C                   ENDIF
063400180119     C* ......VATNOT_J
063500180119     C                   EVAL      VATNOT=%trim(%subst(vindta:466+35:70-35))
063600180119     C                   IF        VATNOT <> *blanks
063700180119     C                   EVAL      VATCMR = VABCMR
063800180119     C                   EVAL      VATCNT = VABCNT
063900180119     C                   EVAL      VATTRC = 'J'
064000180119     C                   ADD       1             §CTROKVT
064100180119     C                   WRITE     EDIVAT00
064200180119     C                   ENDIF
064300180119     C                   ENDIF
064400150216     C* ......VABNOT
064500150216     C                   EVAL      VABNOT=%trim(%subst(vindta:553:35))
064600150216     C* ......VABNT2
064700150216     C                   EVAL      VABNOT=%trim(%subst(vindta:553+35:35))
064800140117     C*
064900140117     C                   ENDIF
065000140117     C*
065100140117     C                   ENDIF
065200140922     C***
065300140922     C* ...tipo record 'GID' (dettaglio segnacolli)
065400140922     C                   IF        %trim(%subst(vindta:1:3)) = 'GID'
065500140922     C* ......VATNOT_E
065600140922     C                   EVAL      VATNOT=%trim(%subst(vindta:118:35))
065700140922     C                   IF        VATNOT <> *blanks
065800140922     C                   EVAL      VATCMR = VABCMR
065900140922     C                   EVAL      VATCNT = VABCNT
066000140922     C                   EVAL      VATTRC = 'E'
066100140922     C                   ADD       1             §CTROKVT
066200140922     C                   WRITE     EDIVAT00
066300140922     C                   ENDIF
066400140922     C*
066500140922     C                   ENDIF
066600010202     C*
066700000801     C* Ebbene...
066800000801     C                   ADD       1             §CTRMO
066900010201     C                   IF        errore <> *zeros
067000000801     C                   ADD       1             §CTRNO
067100000801     C                   EVAL      vinflg = '2'
067200000801     C                   ELSE
067300010201     C                   ADD       1             §CTROKVB
067400000801     C                   ENDIF
067500000801     C*
067600000801     C                   ENDSR
067700010202     C*----------------------------------------------------*
067800140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
067900010202     C*----------------------------------------------------*
068000020305     C     PREVAT        BEGSR
068100010202     C*
068200140117     C* Compongo il nome del membro da dare al EDIVATWR
068300010202     C                   eval      parmbr = vlrhdl
068400010202     C                   movel     'M'           parmbr
068500060113     C                   eval      parccm = vlrksc
068600010202     C                   eval      paropz = '1'
068700010202     C* Effettuo la chiamata al CLLE preposto
068800140117     C                   call(e)   'TITVEVTC'
068900010202     C                   parm                    parccm
069000010202     C                   parm                    parmbr
069100010202     C                   parm                    paropz
069200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
069300010202     C                   if        %error
069400010202     C                   movel     '1'           chkcall
069500010202     C                   else
069600010202     C                   movel     '0'           chkcall
069700010202     C                   endif
069800010202     C*
069900010202     C                   ENDSR
070000000801     C*----------------------------------------------------*
070100000801     C*  CONTROLLO NUMERICITA' CAMPI
070200000801     C*----------------------------------------------------*
070300000801     C     CHKNUM        BEGSR
070400000801     C*
070500000801     C                   call(e)   'ISNUMERIC'
070600000801     C                   PARM                    PiStr            30
070700040714     C                   PARM      ','           PiDecChr          1
070800000801     C                   PARM      *ZEROS        PiVal            30 9
070900000801     C                   PARM      '0'           PiInt             1
071000000801     C                   PARM      '0'           PiNum             1
071100000801     C                   IF        %error
071200000801     C                   EVAL      PiInt=*off
071300000801     C                   ENDIF
071400000801     C*
071500000801     C                   ENDSR
071600000801     C***
071700000801
071800011113
071900011113     C*----------------------------------------------------*
072000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
072100011113     C*----------------------------------------------------*
072200011113     C     CHKIMPDIV     BEGSR
072300011113     C*
072400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
072500011113     C                   Z-ADD     *zeros        wrkDec            9 9
072600011113     C*
072700011113     C* Come prima cosa effettuo considerazioni sulla divisa
072800011113     C                   IF        vabIAS > *zeros
072900011113     C                   IF        vabVAS <> 'EUR'
073000011113     C                   EVAL      vabVAS =  'ITL'
073100011113     C                   ENDIF
073200011113     C                   ENDIF
073300011113     C*
073400011113     C                   IF        vabCAS > *zeros
073500011113     C                   IF        vabVCA <> 'EUR'
073600011113     C                   EVAL      vabVCA =  'ITL'
073700011113     C                   ENDIF
073800011113     C                   ENDIF
073900011113     C*
074000011113     C                   IF        vabVMD > *zeros
074100020305     C                   IF        vabVAD <> 'EUR'
074200011113     C                   EVAL      vabVAD =  'ITL'
074300011113     C                   ENDIF
074400011113     C                   ENDIF
074500011113     C*
074600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
074700011113     C                   Z-ADD     vabIAS        wrkDec
074800011113     C                   IF        wrkDec > *zeros
074900011113     C                   IF        vabVAS = 'ITL'
075000011113     C                   EVAL      vabIAS = *zeros
075100011113     C                   ENDIF
075200011113     C                   ENDIF
075300011113     C*
075400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
075500011113     C                   Z-ADD     vabCAS        wrkDec
075600011113     C                   IF        wrkDec > *zeros
075700011113     C                   IF        vabVCA = 'ITL'
075800011113     C                   EVAL      vabCAS = *zeros
075900011113     C                   ENDIF
076000011113     C                   ENDIF
076100011113     C*
076200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
076300011113     C                   Z-ADD     vabVMD        wrkDec
076400011113     C                   IF        wrkDec > *zeros
076500011113     C                   IF        vabVAD = 'ITL'
076600011113     C                   EVAL      vabVMD = *zeros
076700011113     C                   ENDIF
076800011113     C                   ENDIF
076900011113     C*
077000011113     C                   ENDSR
077100011113     C***
077200011113
077300011113
077400000801
077500000801
077600990920      /TITLE Invio dei dati al punto operativo.
077700010202     C     invio         BEGSR
077800990920     C*
077900140117     C* 1° invio EDIVAT
078000010201     C                   reset                   dscmz
078100010201     C                   move      vlrpoi        cmzdst
078200140117     C                   eval      cmzfld = 'EDIVATWR'
078300010201     C                   eval      cmzmbd = vlrhdl
078400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
078500021009     C***                if        prmfir = *blanks
078600140117     C                   eval      cmzfla = 'EDIVAT0F'
078700140117     C                   eval      cmzmba = 'EDIVAT0F'
078800021009     C***                else
078900021009     C***                eval      cmzfla = prmfir
079000021009     C***                eval      cmzmba = prmfir
079100021009     C***                endif
079200010201     C                   eval      cmznrr = *zeros
079300020305     C                   move      §ctrokvt      cmznrr
079400021018     C                   eval      cmzlba = vlrfl1
079500010201     C                   call(e)   'TIS711C'
079600010201     C                   parm                    dscmz
079700010201     C                   parm      *blanks       esito
079800010205     C                   if        %error
079900010205     C                             or cmzerr = '1'
080000010205     C                             or esito  = '1'
080100010205     C                   eval      wrkesito = '3'
080200010205     C                   else
080300010201     C*
080400140117     C* 2° invio EDIVAB
080500010201     C                   reset                   dscmz
080600010201     C                   move      vlrpoi        cmzdst
080700010201     C                   eval      cmzfld = vlrfou
080800010201     C                   eval      cmzmbd = vlrhdl
080900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
081000021009     C***                if        prmfir = *blanks
081100140117     C                   eval      cmzfla = 'EDIVAB0F'
081200140117     C                   eval      cmzmba = 'EDIVAB0F'
081300021009     C***                else
081400021009     C***                eval      cmzfla = prmfir
081500021009     C***                eval      cmzmba = prmfir
081600021009     C***                endif
081700010201     C                   eval      cmznrr = *zeros
081800010201     C                   move      §ctrokvb      cmznrr
081900021018     C                   eval      cmzlba = vlrfl1
082000010201     C                   call(e)   'TIS711C'
082100010201     C                   parm                    dscmz
082200010201     C                   parm      *blanks       esito
082300010201     C                   if        %error
082400010201     C                             or cmzerr = '1'
082500010201     C                             or esito  = '1'
082600010201     C                   eval      wrkesito = '3'
082700010201     C                   endif
082800010205     C                   endif
082900990920     C*
083000000613     C                   ENDSR
083100000613     C***
083200070411
083300070411     C     *pssr         BEGSR
083400070411     C*
083500070411     C                   if        %open(tivin00r)
083600070411     C                   close     tivin00r
083700070411     C                   endif
083800140117     C                   if        %open(edivabwr)
083900140117     C                   close     edivabwr
084000070411     C                   endif
084100140117     C                   if        %open(edivatwr)
084200140117     C                   close     edivatwr
084300070411     C                   endif
084400070411     C*
084500070411     C* Effettuo la chiamata al CLLE preposto
084600140117     C                   call(e)   'TITVEVTC'
084700070411     C                   parm                    parccm
084800070411     C                   parm                    parmbr
084900070411     C                   parm      '2'           paropz
085000070411     C*
085100070411     C                   eval      wrkesito = '2'
085200070411     C*
085300070411     C                   seton                                        LR
085400070411     C*
085500070411     C                   ENDSR     '*CANCL'
085600070411     C***
085700070411
085800990910
085900000613     C     *inzsr        BEGSR
086000990910     C*
086100990910     C     *entry        plist
086200990920     C                   parm                    tivlrds
086300990921     C                   parm      wrkesito      esito
086400000724     C                   parm                    prmlit
086500000710     C                   parm                    prmfir
086600000613     C*
086700000830     C* CALCOLA LA DATA CORRENTE
086800140117     C                   time                    wn14             14 0
086900140117     C                   movel     wn14          oracor            6 0          *ORA
087000100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
087100100722     C                   eval      datcor = %dec(%date() : *ISO)
087200000830     C*
087300000613     C                   ENDSR
087400000613     C***
