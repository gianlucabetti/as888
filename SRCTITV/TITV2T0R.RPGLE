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
029500180208     C                   WHEN      %trim(%subst(vindta:491:35)) = '25103'
029600180208     C                   EVAL      savCCM = 1161727
029700180208     C                   EVAL      savLNP = 116
029800180213     C                   WHEN      %trim(%subst(vindta:491:35)) = '40090'
029900180213     C                   EVAL      savCCM = 1601003
030000180213     C                   EVAL      savLNP = 160
030100171027     C                   ENDSL
030200170224     C                   ENDIF
030300140922     C***
030400140922     C* ...tipo record 'CNI' (rottuta di codice spedizione)
030500180208     C                   IF        %trim(%subst(vindta:1:3)) = 'CNI' and
030600170516     C                             (VABNAS <> %subst(vindta:884:17) or
030700170504     C                              %subst(vindta:884:17) = *blanks or
030800140923     C                              wGiro = 1 or FlgCAS = '1')
030900100722     C*
031000100722     C* Se nn primo giro => scarico il buffer precedente
031100100722     C                   if        wGiro = 1
031200100722     C                   eval      wGiro = 2
031300100722     C                   else
031400140922     C*
031500140922     C  N31              exsr      CHKWRI
031600140117     C  N31              WRITE     EDIVAB00
031700130708     C                   SETOFF                                       31
031800100722     C                   endif
031900100722     C*
032000071210     C* Resetto indicatore d anomalia sul singolo record
032100071210     C                   eval      vinflg = '1'
032200071210     C* ......inizializzazioni iniziali e formati record file Bartolini
032300071210     C                   EXSR      INZVAR
032400071210     C                   EXSR      DEFCAM
032500170224     C*
032600180208     C                   IF        savCCM > *zeros
032700170224     C                   EVAL      VABCCM = savCCM
032800170224     C                   EVAL      VATCCM = savCCM
032900171027     C                   EVAL      VABLNP = savLNP
033000171027     C                   EVAL      VATLNP = savLNP
033100180208     C                   SELECT
033200171110     C                   WHEN      VABCCM = 0435707
033300171109     C                   IF        datcor < 20171113
033400171030     C                   EVAL      VABCTR = 301
033500171109     C                   ELSE
033600171109     C                   EVAL      VABCTR = 000
033700171109     C                   ENDIF
033800171030     C                   EVAL      VABCTM = '7Q'
033900171110     C                   WHEN      VABCCM = 0430001
034000171110     C                   EVAL      VABCTR = 000
034100171110     C                   EVAL      VABCTM = '7Q'
034200180112     C                   WHEN      VABCCM = 0435916
034300180112     C                   EVAL      VABCTR = 000
034400180112     C                   EVAL      VABCTM = '7Q'
034500180124     C                   WHEN      VABCCM = 0437380
034600180124     C                   EVAL      VABCTR = 301
034700180124     C                   EVAL      VABCTM = '7Q'
034800180130     C                   WHEN      VABCCM = 0437796
034900180130     C                   EVAL      VABCTR = 301
035000180130     C                   EVAL      VABCTM = '7Q'
035100180208     C                   WHEN      VABCCM = 1161727
035200180208     C                   EVAL      VABCTR = 000
035300180208     C                   EVAL      VABCTM = '7Q'
035400180213     C                   WHEN      VABCCM = 1601003
035500180213     C                   EVAL      VABCTR = 000
035600180213     C                   EVAL      VABCTM = '7Q'
035700171110     C                   ENDSL
035800171030     C                   ENDIF
035900071210     C*
036000071210     C                   Z-ADD     *zeros        errore            1 0
036100071210     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
036200071210     C                   MOVEL     datcor        VABAAS
036300071210     C                   MOVEL     datcor        VATAAS
036400071210     C                   MOVE      datcor        VABMGS
036500071210     C                   MOVE(P)   vlrpoi        VABFGS
036600071210     C                   MOVE(P)   vlrpoi        VATFGS
036700140903     C*
036800140903     C* Dai "fissi"
036900140903     C                   EVAL      VABCMR = savCMR
037000140903     C                   EVAL      VABDCM = datcor
037100140903     C                   EVAL      VABDTS = datcor
037200140903     C                   EVAL      VABHMS = oracor
037300140903     C                   EVAL      VABCNT = 1
037400140903     C*
037500071210     C* ......VABNSP/VATNSP
037600071210     C* NSP => Stacco un numeratore da AZNUM
037700071210     C                   clear                   TRUL33DS
037800071210     C                   eval      I33OPE = *zeros
037900071210     C                   eval      I33CNU = 302
038000071210     C                   eval      I33NUM = 1
038100071210     C                   movel     TRUL33DS      KPJBU
038200071210     C                   call      'TRUL33R'
038300071210     C                   parm                    KPJBA
038400071210     C                   movel     KPJBU         TRUL33DS
038500071210     C                   if        O33ERR = *zeros
038600071210     C                   z-add     O33NRF        VABNSP
038700071210     C                   z-add     O33NRF        VATNSP
038800071210     C                   else
038900071210     C                   Z-ADD     1             errore
039000071210     C                   EVAL      vinmsg = %trimr(vinmsg)
039100071210     C                             + ' ' + 'VABNSP VATNSP'
039200071210     C                   endif
039300140922     C*
039400140922     C                   ENDIF
039500140922     C***
039600140922     C* ...tipo record 'CNI' (info testata)
0397001802081    C                   IF        %trim(%subst(vindta:1:3)) = 'CNI'
039800170516     C* ......VABRMN
039900140922     C                   EVAL      PiStr=%trim(%subst(vindta:9:35))
040000140922     C                   EXSR      CHKNUM
040100140922     C                   IF        PiInt=*on
040200170516     C                   Z-ADD     PiVal         VABRMN
040300140922     C                   ELSE
040400140922     C                   ADD       1             errore
040500140922     C                   EVAL      vinmsg = %trimr(vinmsg)
040600170516     C                             + ' ' + 'VABRMN'
040700140922     C                   ENDIF
040800170516     C* ......VABRMA
040900170516     C                   EVAL      VABRMA=%trim(%subst(vindta:710:17))
041000151112     C* ......VABNAS
041100141106     C                   EVAL      VABNAS=%trim(%subst(vindta:884:17))
041200140922     C* ......VABNCL
041300140922     C                   EVAL      PiStr=%trim(%subst(vindta:56:5))
041400140922     C                   EXSR      CHKNUM
041500140922     C                   IF        PiInt=*on
041600140922     C                   ADD       PiVal         VABNCL
041700140922     C                   ELSE
041800140922     C                   ADD       1             errore
041900140922     C                   EVAL      vinmsg = %trimr(vinmsg)
042000140922     C                             + ' ' + 'VABNCL'
042100140922     C                   ENDIF
042200140922     C* ......VABPKB
042300140922     C                   EVAL      PiStr=%trim(%subst(vindta:66:12))
042400140922     C                   EXSR      CHKNUM
042500140922     C                   IF        PiNum=*on
042600140922     C                   EVAL      PiVal = PiVAl / 100
042700140922     C                   ADD       PiVal         VABPKB
042800140922     C                   ELSE
042900140922     C                   ADD       1             errore
043000140922     C                   EVAL      vinmsg = %trimr(vinmsg)
043100140922     C                             + ' ' + 'VABPKB'
043200140922     C                   ENDIF
043300140922     C* ......VABVLB
043400140922     C                   EVAL      PiStr=%trim(%subst(vindta:78:12))
043500140922     C                   EXSR      CHKNUM
043600140922     C                   IF        PiNum=*on
043700140922     C                   EVAL      PiVal = PiVAl / 100
043800140922     C                   ADD       PiVal         VABVLB
043900140922     C                   ELSE
044000140922     C                   ADD       1             errore
044100140922     C                   EVAL      vinmsg = %trimr(vinmsg)
044200140922     C                             + ' ' + 'VABVLB'
044300140922     C                   ENDIF
044400140117     C* ......VABCAS
044500140922     C                   IF        %subst(vindta:145:12) <> *all'0'
044600140117     C                   EVAL      FlgCAS = '1'
044700140922     C                   EVAL      VABVCA = 'EUR'
044800140922     C                   EVAL      PiStr=%trim(%subst(vindta:145:12))
044900140117     C                   EXSR      CHKNUM
045000140117     C                   IF        PiNum=*on
045100140922     C                   EVAL      PiVal = PiVAl / 100
045200140922     C                   ADD       PiVal         VABCAS
045300140117     C                   ELSE
045400140117     C                   ADD       1             errore
045500140117     C                   EVAL      vinmsg = %trimr(vinmsg)
045600140117     C                             + ' ' + 'VABCAS'
045700140117     C                   ENDIF
045800140117     C                   ENDIF
045900140922     C* ......VABIAS
046000140922     C                   IF        %subst(vindta:157:12) <> *all'0'
046100140922     C                   EVAL      VABVAS = 'EUR'
046200140922     C                   EVAL      PiStr=%trim(%subst(vindta:157:12))
046300140922     C                   EXSR      CHKNUM
046400140922     C                   IF        PiNum=*on
046500140922     C                   EVAL      PiVal = PiVAl / 100
046600140922     C                   ADD       PiVal         VABIAS
046700140922     C                   ELSE
046800140922     C                   ADD       1             errore
046900140922     C                   EVAL      vinmsg = %trimr(vinmsg)
047000140922     C                             + ' ' + 'VABIAS'
047100140922     C                   ENDIF
047200140922     C                   ENDIF
047300140922     C* ......VABVMD
047400140922     C                   IF        %subst(vindta:169:12) <> *all'0'
047500140922     C                   EVAL      VABVAD = 'EUR'
047600140922     C                   EVAL      PiStr=%trim(%subst(vindta:169:12))
047700140922     C                   EXSR      CHKNUM
047800140922     C                   IF        PiNum=*on
047900140922     C                   EVAL      PiVal = PiVAl / 100
048000140922     C                   ADD       PiVal         VABVMD
048100140922     C                   ELSE
048200140922     C                   ADD       1             errore
048300140922     C                   EVAL      vinmsg = %trimr(vinmsg)
048400140922     C                             + ' ' + 'VABVMD'
048500140922     C                   ENDIF
048600140922     C                   ENDIF
048700141002     C* ......VABDCR / VABHCR
0488001802082    C                   IF        %trim(%subst(vindta:142:2)) = '2'
0489001802083    C                   IF        %trim(%subst(vindta:125:8)) <> *blanks AND
049000141002     C                             %trim(%subst(vindta:125:8)) <> *zeros
049100141002     C                   EVAL      PiStr=%trim(%subst(vindta:125:8))
049200141002     C                   EXSR      CHKNUM
049300141002     C                   IF        PiInt=*on
049400141002     C                   Z-ADD     PiVal         VABDCR
049500141002     C                   ELSE
049600141002     C                   ADD       1             errore
049700141002     C                   EVAL      vinmsg = %trimr(vinmsg)
049800141002     C                             + ' ' + 'VABDCR'
049900141002     C                   ENDIF
050000141002     C                   EVAL      PiStr=%trim(%subst(vindta:133:4))
050100141002     C                   EXSR      CHKNUM
050200141002     C                   IF        PiInt=*on
050300141002     C                   Z-ADD     PiVal         VABHCR
050400141002     C                   ELSE
050500141002     C                   ADD       1             errore
050600141002     C                   EVAL      vinmsg = %trimr(vinmsg)
050700141002     C                             + ' ' + 'VABHCR'
050800141002     C                   ENDIF
0509001802083-   C                   ENDIF
0510001802082-   C                   ENDIF
051100141009     C* ......VABDCR / VABHCR
051200141009     C                   IF        %trim(%subst(vindta:142:2)) = '17'
051300141009     C                   IF        %trim(%subst(vindta:125:8)) <> *blanks AND
051400141009     C                             %trim(%subst(vindta:125:8)) <> *zeros
051500141009     C                   EVAL      VABTCR = 'P'
051600141009     C                   EVAL      PiStr=%trim(%subst(vindta:125:8))
051700141009     C                   EXSR      CHKNUM
051800141009     C                   IF        PiInt=*on
051900141009     C                   Z-ADD     PiVal         VABDCR
052000141009     C                   ELSE
052100141009     C                   ADD       1             errore
052200141009     C                   EVAL      vinmsg = %trimr(vinmsg)
052300141009     C                             + ' ' + 'VABDCR'
052400141009     C                   ENDIF
052500141009     C                   EVAL      PiStr=%trim(%subst(vindta:133:4))
052600141009     C                   EXSR      CHKNUM
052700141009     C                   IF        PiInt=*on
052800141009     C                   Z-ADD     PiVal         VABHCR
052900141009     C                   ELSE
053000141009     C                   ADD       1             errore
053100141009     C                   EVAL      vinmsg = %trimr(vinmsg)
053200141009     C                             + ' ' + 'VABHCR'
053300141009     C                   ENDIF
053400141009     C                   ENDIF
053500141009     C                   ENDIF
053600140922     C* ......VATNOT_P
053700140922     C                   if        *in53
053800140923     C                   eval      pIn_MaskPDF =
053900140923     C                                     %trim(%subst(vindta:1292:100))
054000170516     C                   call      'TIS7P2SER'
054100140922     C                   parm      'W'           pIn_Opz           1
054200140922     C                   parm                    tivlrds
054300140922     C                   parm                    EDIVABDS
054400140922     C                   parm      '10'          pIn_CdIdAz        2
054500140922     C                   parm                    pIn_MaskPDF      50
054600140922     C                   parm      *blanks       pOut_Esito        1
054700140922     C                   endif
054800140117     C*
054900180208     C* solo per determinati clienti
055000180208     C                   select
055100180209     C                   when      VABCCM = 1161727
055200180208     C*
055300180208     C                   select
055400180208     C* ......VABTSP
055500180208     C                   when      %trim(%subst(vindta:855:17))='7'
055600180208     C                   eval      VABTSP = 'E'
055700180208     C* ......VABTC1
055800180208     C                   when      %trim(%subst(vindta:855:17))='8'
055900180208     C                   eval      VABTC1 = 'A'
056000180208     C                   endsl
056100180208     C*
056200180208     C                   endsl
056300180208     C*
0564001802081-   C                   ENDIF
056500170112     C***
056600170112     C* ...tipo record 'NA2/SF' (info testata - mittente)
056700170112     C                   IF        %trim(%subst(vindta:1:3)) = 'NA2'
056800170112     C* Solo sub-types 'SF'
056900170112     C                   IF        %trim(%subst(vindta:4:2)) = 'SF'
057000170112     C* ......VABRMO
057100170112     C                   EVAL      VABRMO=%trim(%subst(vindta:42:35))
057200170112     C* ......VABCMO
057300170112     C                   EVAL      VABCMO=%trim(%subst(vindta:226:17))
057400170112     C* ......VABNMO
057500170112     C                   EVAL      VABNMO=%trim(%subst(vindta:243:3))
057600170112     C                   IF        VABNMO='IT'
057700170112     C                   EVAL      VABNMO=*blanks
057800170112     C                   ENDIF
057900170112     C*
058000170112     C                   ENDIF
058100170112     C*
058200170112     C                   ENDIF
058300140117     C***
058400140922     C* ...tipo record 'NA2/ST' (info testata - destinatario)
058500140922     C                   IF        %trim(%subst(vindta:1:3)) = 'NA2'
058600140922     C* Solo sub-types 'ST'
058700140922     C                   IF        %trim(%subst(vindta:4:2)) = 'ST'
058800060202     C* ......VABRSD
058900140922     C                   EVAL      VABRSD=%trim(%subst(vindta:42:35))
059000130620     C* ......VABRD2
059100140922     C                   EVAL      VABRD2=%trim(%subst(vindta:77:35))
059200100714     C* ......VABIND
059300140922     C                   EVAL      VABIND=%trim(%subst(vindta:112:35))
059400130620     C* ......VABCAD
059500140922     C                   EVAL      VABCAD=%trim(%subst(vindta:226:17))
059600060202     C* ......VABLOD
059700140922     C                   EVAL      VABLOD=%trim(%subst(vindta:182:35))
059800140922     C* ......VABPRD
059900140922     C                   EVAL      VABPRD=%trim(%subst(vindta:217:9))
060000140922     C* ......VABNZD
060100140922     C                   EVAL      VABNZD=%trim(%subst(vindta:243:3))
060200130620     C                   IF        VABNZD='IT'
060300130620     C                   EVAL      VABNZD=*blanks
060400130620     C                   ENDIF
060500060202     C* ......VABPRD
060600070719     C* Reperisco la provincia dal CAP e dalla località
060700070719     C                   IF        VABPRD  = *blanks AND
060800130620     C                             VABLOD <> *blanks AND
060900130620     C                             VABCAD <> *blanks AND
061000130620     C                             VABNZD  = *blanks
061100070719     C                   CLEAR                   TISI95DS
061200070719     C                   EVAL      I95TCN = '3'
061300070719     C                   Z-ADD     datcor        I95DAT
061400070719     C                   EVAL      I95CAP = VABCAD
061500070719     C                   EVAL      I95LOC = VABLOD
061600070719     C                   CALL      'TISI95R'
061700070719     C                   PARM                    TISI95DS
061800070719     C                   EVAL      VABPRD = O95PRV
061900070719     C                   ENDIF
062000140922     C* ......VATNOT_A
062100140922     C                   EVAL      VATNOT=%trim(%subst(vindta:371:35))
062200140922     C                   IF        VATNOT <> *blanks
062300140922     C                   EVAL      VATCMR = VABCMR
062400140922     C                   EVAL      VATCNT = VABCNT
062500140922     C                   EVAL      VATTRC = 'A'
062600140922     C                   ADD       1             §CTROKVT
062700140922     C                   WRITE     EDIVAT00
062800140922     C                   ENDIF
062900140922     C* ......VATNOT_B
063000140922     C                   EVAL      VATNOT=%trim(%subst(vindta:406:20))
063100140922     C                   IF        VATNOT <> *blanks
063200140922     C                   EVAL      VATCMR = VABCMR
063300140922     C                   EVAL      VATCNT = VABCNT
063400140922     C                   EVAL      VATTRC = 'B'
063500140922     C                   ADD       1             §CTROKVT
063600140922     C                   WRITE     EDIVAT00
063700140922     C                   ENDIF
063800180119     C*
063900180119     C                   IF        VABCCM = 1663621 or
064000180208     C                             VABCCM = 1161727 or
064100180208     C                             VABCCM = 0435707 or
064200180119     C                             VABCCM = 0430001 or
064300180213     C                             VABCCM = 0435916 or
064400180213     C                             VABCCM = 1601003
064500180119     C* ......VATNOT_S
064600180119     C                   EVAL      VATNOT=%trim(%subst(vindta:426:20))
064700180119     C                   IF        VATNOT <> *blanks
064800180119     C                   EVAL      VATCMR = VABCMR
064900180119     C                   EVAL      VATCNT = VABCNT
065000180119     C                   EVAL      VATTRC = 'S'
065100180119     C                   ADD       1             §CTROKVT
065200180119     C                   WRITE     EDIVAT00
065300180119     C                   ENDIF
065400180119     C* ......VATNOT_I
065500180119     C                   EVAL      VATNOT=%trim(%subst(vindta:466:35))
065600180119     C                   IF        VATNOT <> *blanks
065700180119     C                   EVAL      VATCMR = VABCMR
065800180119     C                   EVAL      VATCNT = VABCNT
065900180119     C                   EVAL      VATTRC = 'I'
066000180119     C                   ADD       1             §CTROKVT
066100180119     C                   WRITE     EDIVAT00
066200180119     C                   ENDIF
066300180119     C* ......VATNOT_J
066400180119     C                   EVAL      VATNOT=%trim(%subst(vindta:466+35:70-35))
066500180119     C                   IF        VATNOT <> *blanks
066600180119     C                   EVAL      VATCMR = VABCMR
066700180119     C                   EVAL      VATCNT = VABCNT
066800180119     C                   EVAL      VATTRC = 'J'
066900180119     C                   ADD       1             §CTROKVT
067000180119     C                   WRITE     EDIVAT00
067100180119     C                   ENDIF
067200180119     C                   ENDIF
067300150216     C* ......VABNOT
067400150216     C                   EVAL      VABNOT=%trim(%subst(vindta:553:35))
067500150216     C* ......VABNT2
067600150216     C                   EVAL      VABNOT=%trim(%subst(vindta:553+35:35))
067700140117     C*
067800140117     C                   ENDIF
067900140117     C*
068000140117     C                   ENDIF
068100140922     C***
068200140922     C* ...tipo record 'GID' (dettaglio segnacolli)
068300140922     C                   IF        %trim(%subst(vindta:1:3)) = 'GID'
068400140922     C* ......VATNOT_E
068500140922     C                   EVAL      VATNOT=%trim(%subst(vindta:118:35))
068600140922     C                   IF        VATNOT <> *blanks
068700140922     C                   EVAL      VATCMR = VABCMR
068800140922     C                   EVAL      VATCNT = VABCNT
068900140922     C                   EVAL      VATTRC = 'E'
069000140922     C                   ADD       1             §CTROKVT
069100140922     C                   WRITE     EDIVAT00
069200140922     C                   ENDIF
069300140922     C*
069400140922     C                   ENDIF
069500010202     C*
069600000801     C* Ebbene...
069700000801     C                   ADD       1             §CTRMO
069800010201     C                   IF        errore <> *zeros
069900000801     C                   ADD       1             §CTRNO
070000000801     C                   EVAL      vinflg = '2'
070100000801     C                   ELSE
070200010201     C                   ADD       1             §CTROKVB
070300000801     C                   ENDIF
070400000801     C*
070500000801     C                   ENDSR
070600010202     C*----------------------------------------------------*
070700140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
070800010202     C*----------------------------------------------------*
070900020305     C     PREVAT        BEGSR
071000010202     C*
071100140117     C* Compongo il nome del membro da dare al EDIVATWR
071200010202     C                   eval      parmbr = vlrhdl
071300010202     C                   movel     'M'           parmbr
071400060113     C                   eval      parccm = vlrksc
071500010202     C                   eval      paropz = '1'
071600010202     C* Effettuo la chiamata al CLLE preposto
071700140117     C                   call(e)   'TITVEVTC'
071800010202     C                   parm                    parccm
071900010202     C                   parm                    parmbr
072000010202     C                   parm                    paropz
072100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
072200010202     C                   if        %error
072300010202     C                   movel     '1'           chkcall
072400010202     C                   else
072500010202     C                   movel     '0'           chkcall
072600010202     C                   endif
072700010202     C*
072800010202     C                   ENDSR
072900000801     C*----------------------------------------------------*
073000000801     C*  CONTROLLO NUMERICITA' CAMPI
073100000801     C*----------------------------------------------------*
073200000801     C     CHKNUM        BEGSR
073300000801     C*
073400000801     C                   call(e)   'ISNUMERIC'
073500000801     C                   PARM                    PiStr            30
073600040714     C                   PARM      ','           PiDecChr          1
073700000801     C                   PARM      *ZEROS        PiVal            30 9
073800000801     C                   PARM      '0'           PiInt             1
073900000801     C                   PARM      '0'           PiNum             1
074000000801     C                   IF        %error
074100000801     C                   EVAL      PiInt=*off
074200000801     C                   ENDIF
074300000801     C*
074400000801     C                   ENDSR
074500000801     C***
074600000801
074700011113
074800011113     C*----------------------------------------------------*
074900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
075000011113     C*----------------------------------------------------*
075100011113     C     CHKIMPDIV     BEGSR
075200011113     C*
075300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
075400011113     C                   Z-ADD     *zeros        wrkDec            9 9
075500011113     C*
075600011113     C* Come prima cosa effettuo considerazioni sulla divisa
075700011113     C                   IF        vabIAS > *zeros
075800011113     C                   IF        vabVAS <> 'EUR'
075900011113     C                   EVAL      vabVAS =  'ITL'
076000011113     C                   ENDIF
076100011113     C                   ENDIF
076200011113     C*
076300011113     C                   IF        vabCAS > *zeros
076400011113     C                   IF        vabVCA <> 'EUR'
076500011113     C                   EVAL      vabVCA =  'ITL'
076600011113     C                   ENDIF
076700011113     C                   ENDIF
076800011113     C*
076900011113     C                   IF        vabVMD > *zeros
077000020305     C                   IF        vabVAD <> 'EUR'
077100011113     C                   EVAL      vabVAD =  'ITL'
077200011113     C                   ENDIF
077300011113     C                   ENDIF
077400011113     C*
077500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
077600011113     C                   Z-ADD     vabIAS        wrkDec
077700011113     C                   IF        wrkDec > *zeros
077800011113     C                   IF        vabVAS = 'ITL'
077900011113     C                   EVAL      vabIAS = *zeros
078000011113     C                   ENDIF
078100011113     C                   ENDIF
078200011113     C*
078300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
078400011113     C                   Z-ADD     vabCAS        wrkDec
078500011113     C                   IF        wrkDec > *zeros
078600011113     C                   IF        vabVCA = 'ITL'
078700011113     C                   EVAL      vabCAS = *zeros
078800011113     C                   ENDIF
078900011113     C                   ENDIF
079000011113     C*
079100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
079200011113     C                   Z-ADD     vabVMD        wrkDec
079300011113     C                   IF        wrkDec > *zeros
079400011113     C                   IF        vabVAD = 'ITL'
079500011113     C                   EVAL      vabVMD = *zeros
079600011113     C                   ENDIF
079700011113     C                   ENDIF
079800011113     C*
079900011113     C                   ENDSR
080000011113     C***
080100011113
080200011113
080300000801
080400000801
080500990920      /TITLE Invio dei dati al punto operativo.
080600010202     C     invio         BEGSR
080700990920     C*
080800140117     C* 1° invio EDIVAT
080900010201     C                   reset                   dscmz
081000010201     C                   move      vlrpoi        cmzdst
081100140117     C                   eval      cmzfld = 'EDIVATWR'
081200010201     C                   eval      cmzmbd = vlrhdl
081300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
081400021009     C***                if        prmfir = *blanks
081500140117     C                   eval      cmzfla = 'EDIVAT0F'
081600140117     C                   eval      cmzmba = 'EDIVAT0F'
081700021009     C***                else
081800021009     C***                eval      cmzfla = prmfir
081900021009     C***                eval      cmzmba = prmfir
082000021009     C***                endif
082100010201     C                   eval      cmznrr = *zeros
082200020305     C                   move      §ctrokvt      cmznrr
082300021018     C                   eval      cmzlba = vlrfl1
082400010201     C                   call(e)   'TIS711C'
082500010201     C                   parm                    dscmz
082600010201     C                   parm      *blanks       esito
082700010205     C                   if        %error
082800010205     C                             or cmzerr = '1'
082900010205     C                             or esito  = '1'
083000010205     C                   eval      wrkesito = '3'
083100010205     C                   else
083200010201     C*
083300140117     C* 2° invio EDIVAB
083400010201     C                   reset                   dscmz
083500010201     C                   move      vlrpoi        cmzdst
083600010201     C                   eval      cmzfld = vlrfou
083700010201     C                   eval      cmzmbd = vlrhdl
083800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
083900021009     C***                if        prmfir = *blanks
084000140117     C                   eval      cmzfla = 'EDIVAB0F'
084100140117     C                   eval      cmzmba = 'EDIVAB0F'
084200021009     C***                else
084300021009     C***                eval      cmzfla = prmfir
084400021009     C***                eval      cmzmba = prmfir
084500021009     C***                endif
084600010201     C                   eval      cmznrr = *zeros
084700010201     C                   move      §ctrokvb      cmznrr
084800021018     C                   eval      cmzlba = vlrfl1
084900010201     C                   call(e)   'TIS711C'
085000010201     C                   parm                    dscmz
085100010201     C                   parm      *blanks       esito
085200010201     C                   if        %error
085300010201     C                             or cmzerr = '1'
085400010201     C                             or esito  = '1'
085500010201     C                   eval      wrkesito = '3'
085600010201     C                   endif
085700010205     C                   endif
085800990920     C*
085900000613     C                   ENDSR
086000000613     C***
086100070411
086200070411     C     *pssr         BEGSR
086300070411     C*
086400070411     C                   if        %open(tivin00r)
086500070411     C                   close     tivin00r
086600070411     C                   endif
086700140117     C                   if        %open(edivabwr)
086800140117     C                   close     edivabwr
086900070411     C                   endif
087000140117     C                   if        %open(edivatwr)
087100140117     C                   close     edivatwr
087200070411     C                   endif
087300070411     C*
087400070411     C* Effettuo la chiamata al CLLE preposto
087500140117     C                   call(e)   'TITVEVTC'
087600070411     C                   parm                    parccm
087700070411     C                   parm                    parmbr
087800070411     C                   parm      '2'           paropz
087900070411     C*
088000070411     C                   eval      wrkesito = '2'
088100070411     C*
088200070411     C                   seton                                        LR
088300070411     C*
088400070411     C                   ENDSR     '*CANCL'
088500070411     C***
088600070411
088700990910
088800000613     C     *inzsr        BEGSR
088900990910     C*
089000990910     C     *entry        plist
089100990920     C                   parm                    tivlrds
089200990921     C                   parm      wrkesito      esito
089300000724     C                   parm                    prmlit
089400000710     C                   parm                    prmfir
089500000613     C*
089600000830     C* CALCOLA LA DATA CORRENTE
089700140117     C                   time                    wn14             14 0
089800140117     C                   movel     wn14          oracor            6 0          *ORA
089900100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
090000100722     C                   eval      datcor = %dec(%date() : *ISO)
090100000830     C*
090200000613     C                   ENDSR
090300000613     C***
